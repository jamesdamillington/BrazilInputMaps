#script classifies MapBiomas maps (possibly output from ResampleMosaic.r) 
#then optionally uses planted area data to improve classification ('disaggregation')

##load libraries
rm(list=ls())
library(raster)
library(tidyverse)
library(readxl)


################
##Script settings
input_path <- "C:/Users/k1076631/Google Drive/Shared/Crafty Telecoupling/Data/LandCover/MapBiomas4/BrazilInputMaps/"

#Classification from Excel
cname <- "PastureB"
classification <- read_excel(paste0(input_path,"Data/MapBiomas_CRAFTY_classifications_v4.xlsx"), sheet = cname, range="B2:C28", col_names=F)  

#classify for the following years
yrs <- seq(2001, 2018, 1)   

#map for our study area, cell values are municipality IDs
munis.r <- raster(paste0(input_path,"Data/BaseMaps/sim10_BRmunis_latlon_5km.asc")) 

#indicate whether to improve classification by disaggregating some classes using planted area data
disaggregate <- T

#indicate whether to create summary tables or not (these are needed for disaggregation but take some time to create so pre-created might be used)
sumTab <- T


################
##Classify 

if(!dir.exists(paste0(input_path,"Data/Classified"))){
  dir.create(paste0(input_path,"Data/Classified"))
}


for(yr in seq_along(yrs)){
  
  map <- raster(paste0(input_path,"Data/Unclassified/Brazil_",yrs[yr],"_5km.asc"))  #read pre-classified data
  map <- reclassify(map, rcl=as.matrix(classification))                 #classify
  writeRaster(map, paste0(input_path,"Data/Classified/LandCover",yrs[yr],"_",cname,".asc"), format = 'ascii', overwrite=T)  #output
}





################
##Functions for disagregation

#function converts raster to xyz  (with help from https://stackoverflow.com/a/19847419)
#specify input raster, whether nodata cells should be output, whether a unique cell ID should be added
#return is a matrix. note format is row (Y) then col (X)
extractXYZ <- function(raster, nodata = FALSE, addCellID = TRUE){
  
  vals <- raster::extract(raster, 1:ncell(raster))   #specify raster otherwise dplyr used
  xys <- rowColFromCell(raster,1:ncell(raster))
  combine <- cbind(xys,vals)
  
  if(addCellID){
    combine <- cbind(1:length(combine[,1]), combine)
  }
  
  if(!nodata){
    combine <- combine[!rowSums(!is.finite(combine)),]  #from https://stackoverflow.com/a/15773560
  }
  
  return(combine)
}

#function to calculate proportion of each LC in a muni (ignoring NAs, help from https://stackoverflow.com/a/44290753)
getLCs <- function(data)
{
  
  data %>%
    group_by(muniID) %>%
    dplyr::summarise(LC1 = round(sum(lcMap == 1, na.rm = T) / sum(!is.na(lcMap)), 3),
                     LC2 = round(sum(lcMap == 2, na.rm = T) / sum(!is.na(lcMap)), 3),
                     LC3 = round(sum(lcMap == 3, na.rm = T) / sum(!is.na(lcMap)), 3),
                     LC4 = round(sum(lcMap == 4, na.rm = T) / sum(!is.na(lcMap)), 3),
                     LC5 = round(sum(lcMap == 5, na.rm = T) / sum(!is.na(lcMap)), 3),
                     NonNAs = sum(!is.na(lcMap)),
                     NAs = sum(is.na(lcMap))
    ) -> LCs

  return(LCs)
}


#function converts data in CRAFTY output file for a single variable and creates a raster
outputRaster <- function(data, variable){
  
  out <- data %>%
    dplyr::select(X, Y, !!variable)
  
  ras <- rasterFromXYZ(out)
  
  return(ras)
}


#function to Calculate number of OAgri, Agri and Pasture cells needed to match OA_plant and A_plant
calcDiffcs <- function(tbl) {
  
   # Calculate number of OAgri, Agri and Pasture cells needed to match OA_plant and A_plant:
  #Overall A_final + OA_final + P_final must equal A_mapped + OA_mapped
  #So:
  #case 1
  #if OA_mapped > OA_planted AND A_mapped < A_planted
    #then take enough from difference so A_mapped == A_planted, any remainder is pasture 
  #case 2
  #if OA_mapped > OA_planted AND A_mapped >= A_planted
    #then OA is planted value and remainder is pasture, A_mapped does not change
  #case 3
  #if OA_mapped == OA_planted AND A_mapped < A_planted
    #then nothing changes
  #case 4
  #if OA_mapped == OA_planted AND A_mapped >= A_planted
    #then nothing changes
  #case 5
  #if OA_mapped < OA_planted AND A_mapped <= A_planted
    #then nothing changes
  #case 6
  #if OA_mapped < OA_planted AND A_mapped > A_planted
    #then add difference from A to OA (so that OA_M == OA_p, or until A_m == A_p) 
  
  
  #calculate differences (used in cases below)
  tbl <- tbl %>%
    dplyr::select(muniID, A_mapped_cells, OA_mapped_cells, A_plant_cells, OA_plant_cells) %>%
    mutate(A_diffc = A_mapped_cells - A_plant_cells) %>%
    mutate(OA_diffc = OA_mapped_cells - OA_plant_cells)
  
  #case 1
  #if OA_mapped > OA_planted AND A_mapped < A_planted
    #then take enough from difference so A_mapped == A_planted, any remainder is pasture
  tbl <- tbl %>%
    mutate(OA_final = 
      if_else(OA_diffc > 0 & A_diffc < 0, OA_plant_cells,99))  %>%  
    mutate(A_final = 
      if_else(OA_diffc > 0 & A_diffc < 0, 
        if_else(OA_diffc >= abs(A_diffc), A_plant_cells, A_mapped_cells + OA_diffc),
        99)) %>%
    mutate(P_final = 
      if_else(OA_diffc > 0 & A_diffc < 0,
        if_else(OA_diffc >= abs(A_diffc), OA_mapped_cells - OA_plant_cells - abs(A_diffc), 0),
        99))
  
  #case 2
  #if OA_mapped > OA_planted AND A_mapped >= A_planted
    #then OA is planted value and remainder is pasture, A_mapped does not change
  tbl <- tbl %>%
    mutate(OA_final = 
      if_else(OA_diffc > 0 & A_diffc >= 0, OA_plant_cells, OA_final)) %>%
    mutate(A_final = 
        if_else(OA_diffc > 0 & A_diffc >= 0, A_plant_cells, A_final)) %>%
    mutate(P_final = 
      if_else(OA_diffc > 0 & A_diffc >= 0, OA_diffc, P_final))
  
  #case 6
  #if OA_mapped < OA_planted AND A_mapped > A_planted
    #then add difference from A to OA:
      #if A_diffc <= abs(OA_diffc) OA_final is max possible, otherwise OA_plant (A_final is always A_mapped - A_diffc)
      #nothing goes to P_final
  tbl <- tbl %>%
    mutate(OA_final = 
      if_else(OA_diffc < 0 & A_diffc > 0,
        if_else(A_diffc <= abs(OA_diffc), OA_mapped_cells + A_diffc, OA_mapped_cells + abs(OA_diffc)),
        OA_final)) %>%
    mutate(A_final = 
      if_else(OA_diffc < 0 & A_diffc > 0, 
        if_else(A_diffc <= abs(OA_diffc), A_mapped_cells - A_diffc, A_mapped_cells - abs(OA_diffc)),
        A_final)) %>%
    mutate(P_final = 
      if_else(OA_diffc < 0 & A_diffc > 0, 0, P_final))
  
  
  #case 3
  tbl <- tbl %>%
    mutate(OA_final = 
      if_else(OA_diffc == 0 & A_diffc < 0, 0, OA_final)) %>%
    mutate(A_final = 
      if_else(OA_diffc == 0 & A_diffc < 0, 0, A_final)) %>%
    mutate(P_final = 
      if_else(OA_diffc == 0 & A_diffc < 0, 0, P_final))
  
  #case 4
  tbl <- tbl %>%
    mutate(OA_final = 
      if_else(OA_diffc == 0 & A_diffc >= 0, 0, OA_final)) %>%
    mutate(A_final = 
      if_else(OA_diffc == 0 & A_diffc >= 0, 0, A_final)) %>%
    mutate(P_final = 
      if_else(OA_diffc == 0 & A_diffc >= 0, 0, P_final))
  
  #case 5
  tbl <- tbl %>%
    mutate(OA_final = 
      if_else(OA_diffc < 0 & A_diffc <= 0, 0, OA_final)) %>%
    mutate(A_final = 
      if_else(OA_diffc < 0 & A_diffc <= 0, 0, A_final)) %>%
    mutate(P_final = 
      if_else(OA_diffc < 0 & A_diffc <= 0, 0, P_final))
  
  tbl <- tbl %>%
    mutate(Agri_final = A_final + OA_final)
 
  return(tbl)
  
  #check all cells have changed
  #k <- j %>%
  #  filter(OA_final == 99)
}


#function for each municipality:
#- compare A obs and desired; change if necessary (increase from OA, decrease to OA)
#- compare OA obs and desired; change if necessary (increase from Pas, decrease to Pas)
#convs is the conversions calculated in calcDiffcs, lcs is the lc_munis df
#both should already have been subst to a single muni
convertLCs <- function(convs, lcs) {

  #print(convs[,8:11])
  #print(lcs)
  
  #subset data
  NA_lcs <- filter(lcs, lc == 1 | lc == 4)  #not Agri or Pas
  Agri_lcs <- filter(lcs, lc == 2 | lc == 3)   #Agri (not Pas or Nat)
  P_lcs  <- filter(lcs, lc == 5)  #Pas
  A_lcs <- filter(lcs, lc == 3)   #soy/maize
  OA_lcs <- filter(lcs, lc == 2)  #OA

  #calc how many conversions needed (if negative move to pasture) 
  Agri_obs <- length(Agri_lcs$lc)        
  Agri_diffc <- Agri_obs - convs$Agri_final

  # print(convs)
  # print(Agri_obs)
  # print(convs$Agri_final)
  # print(Agri_diffc)
  # print(is.na(Agri_diffc))
  # print(length(convs$Agri_final))
  
  #print(OA_lcs)
  ctr <- 1 #counter to ensure we don't try to access beyond length of tables below

  #if(length(OA_lcs$lc) == 0) print("length(OA_lcs$lc) == 0")
  
  #if we observe more Agri than we want, move OA to pasture
  #try to move sufficient amount to get the right amount of Agri, otherwise all OA
  if(length(OA_lcs$lc) > 0)  #if there are some Agri pixels
  {
    while(Agri_diffc > 0) {
      if(!any(2 %in% OA_lcs$lc)) break   #if there are no OA values to convert, break
      if(ctr > length(OA_lcs$lc)) break  #should never happen but check if we are trying to convert more values than available, break
      
      OA_lcs[ctr,4] <- 5                 #do the conversion to Pas
      #print("update: 2 -> 5")
      
      ctr <- ctr + 1                     #update counter
      Agri_diffc <- Agri_diffc - 1       #update counter
    }
  }
  
  #print(OA_lcs)
  #update data as we may have made some conversions that have changed OA_lcs 
  nlcs <- bind_rows(NA_lcs, A_lcs, OA_lcs, P_lcs)
  
  #split out again for later
  NA_lcs <- filter(nlcs, lc == 1 | lc == 4)  #not Agri or Pas
  Agri_lcs <- filter(nlcs, lc == 2 | lc == 3)   #Agri (not Pas or Nat)
  P_lcs  <- filter(nlcs, lc == 5)  #Pas
  A_lcs <- filter(nlcs, lc == 3)   #soy/maize
  OA_lcs <- filter(nlcs, lc == 2)  #OA
  
  #print("nlcs")
  #print(nlcs)
  #if(length(Agri_lcs$lc) == 0) print("length(Agri_lcs$lc) == 0")
  
  #now distribute amongst soy/maize (A) vs OA (only if there are any Agri cells)
  if(length(Agri_lcs$lc) > 0) {
    
    #calculate desired soy/maize prop
    Agri_sum <- convs$A_final + convs$OA_final  #total number of desired Agri cells, used to calc prop
    A_prop <- NA
    
    #if both A and OA have a value we need to calc prop, otherwise we can assign based on 0s
    if(convs$A_final > 0 & convs$OA_final > 0) { 
      A_prop <- convs$A_final / Agri_sum
    } else if(convs$A_final > 0) {
      A_prop <- 1.0
    } else { 
      A_prop <- 0
    }
  
    #print(paste0("A_prop: ", A_prop))
      
    #use desired prop to calc desired soy/maize cells
    A_obs <- length(A_lcs$lc) 
    OA_obs <- length(OA_lcs$lc) 
    
    if(is.na(length(A_lcs$lc))) A_obs <- 0
    if(is.na(length(OA_lcs$lc))) OA_obs <- 0
    
    Agri_obs <- A_obs + OA_obs

    #desired number of soy/maize cells
    A_dobs <- round(Agri_obs * A_prop, 0)
    
    #check difference between desired and observed
    A_diffc <- A_dobs - A_obs

    #print(paste0("A_diffc: ", A_diffc))
    
    ctr <- 1 #counter to ensure we don't try to access beyond length of tables below
    
    #print(OA_lcs)
    
    #if desired soy/maize is greater than observed, take some from OA
    while(A_diffc > 0) {
      if(!any(2 %in% OA_lcs$lc)) break   #if there are no OA values to convert (because we already changed them all), break
      if(ctr > length(OA_lcs$lc)) break  #should never happen but check if we are trying to convert more values than available, break
      
      OA_lcs[ctr,4] <- 3                 #do the conversion to soy/maize
      #print("update: 2 -> 3")
      
      ctr <- ctr + 1               #update counter
      A_diffc <- A_diffc - 1       #update counter
    }
    
    #print(OA_lcs)
    
    ctr <- 1 #counter to ensure we don't try to access beyond length of tables below
    
    #print(A_lcs)
    
    #if desired soy/maize is less than observed, give some to OA
    while(A_diffc < 0) {
      if(!any(3 %in% A_lcs$lc)) break   #if there are no A values to convert (because we already changed them all), break
      if(ctr > length(A_lcs$lc)) break  #should never happen but check if we are trying to convert more values than available, break
      
      A_lcs[ctr,4] <- 2                 #do the conversion to other agri
      #print("update: 3 -> 2")
      
      ctr <- ctr + 1               #update counter
      A_diffc <- A_diffc + 1       #update counter
    }
    
    #print(A_lcs)
  }
  
  #update data as we may have made some conversions  
  nlcs <- bind_rows(NA_lcs, A_lcs, OA_lcs, P_lcs)
  
  return(nlcs)
  
}



#function to create summary table for each pre-classified map for comparison below
#output tables contain proportions of LCs and count of data and NA cells for each muni (munis are rows)
createSummaryTables <- function(munisMap, yrs, disagg){

  for(yr in seq_along(yrs)){

    if(!disagg){
      lcname <- paste0("LandCover",yrs[yr],"_",cname,".asc")
      print(paste0("Creating Summary Table from: ", lcname))
      outname <- paste0("Data/Classified/SummaryTable",yrs[yr],"_",cname,".csv")
    }
    
    if(disagg){
      lcname <- paste0("LandCover",yrs[yr],"_",cname,"_Disagg.asc")
      print(paste0("Creating Summary Table from: ", lcname))
      outname <- paste0("Data/Classified/SummaryTable",yrs[yr],"_",cname,"_Disagg.csv")
    }
    
    inname <- paste0("Data/Classified/",lcname) 
    lcMap <- raster(inname)
    
    
    #extract cell values to table format
    munis.t <- extractXYZ(munisMap, addCellID = F)
    lcMap.t <- extractXYZ(lcMap, addCellID = F)
    
    munis.t <- as.data.frame(munis.t)
    munis.t <- plyr::rename(munis.t, c("vals" = "muniID"))
    
    
    lcMap.t <- as.data.frame(lcMap.t)
    lcMap.t <- plyr::rename(lcMap.t, c("vals" = "lcMap"))
    
    
    #set NA in both rasters
    lcMap[is.na(munisMap)] <- NA
    munisMap[is.na(lcMap)] <- NA
    
    #then check what setting NA does....
    munis.t2 <- extractXYZ(munisMap, addCellID = F)
    lcMap.t2 <- extractXYZ(lcMap, addCellID = F)
    
    munis.t2 <- as.data.frame(munis.t2)
    munis.t2 <- plyr::rename(munis.t2, c("vals" = "muniID"))
    
    
    lcMap.t2 <- as.data.frame(lcMap.t2)
    lcMap.t2 <- plyr::rename(lcMap.t2, c("vals" = "lcMap"))
  
    #so need to join 
    lcMap_munis <- left_join(as.data.frame(munis.t), as.data.frame(lcMap.t), by = c("row" = "row", "col" = "col"))
    
    #now summarise by muniID
    lcs <- getLCs(lcMap_munis)
    
    #head(lcs)
    #summary(lcs)
    
    #write to file
    write.csv(lcs, outname, row.names = F)
  
  }
}





################
##Disaggregate 

if(disaggregate){

  #may need to create summary tables of pre-classified maps first 
  if(sumTab){
    createSummaryTables(munis.r, yrs, disagg=FALSE)
  }
  
  
  #loop through years disaggregating
  for(yr in seq_along(yrs)){
  
    #yr <- 1 #for testing
    
    print(paste0("Disaggregating, year: ", yrs[yr]))
  
    #read Summary table for this year - this contains number of cells in each muni (and proportions in each LC)
    mapped <- read_csv(paste0(input_path,"Data/Classified/SummaryTable",yrs[yr],"_",cname,".csv"))
    
    # From mapbiomas data calculate number of cells for:
    # - agriculture
    # - OAgri
    mapped <- mapped %>%
      mutate(A_mapped_cells = round(LC3 * NonNAs,0)) %>%
      mutate(OA_mapped_cells = round(LC2 * NonNAs,0))
    
    
    #muni 5006275 was only created in 2013, partitioned from 5000203
    #so add values from 5006275 to 5000203
    old <- mapped$A_mapped_cells[mapped$muniID == 5000203]
    new <- mapped$A_mapped_cells[mapped$muniID == 5006275] + old
    mapped$A_mapped_cells[mapped$muniID == 5000203] <- new
    
    old <- mapped$OA_mapped_cells[mapped$muniID == 5000203]
    new <- mapped$OA_mapped_cells[mapped$muniID == 5006275] + old
    mapped$OA_mapped_cells[mapped$muniID == 5000203] <- new 
    
    #mapped %>%
    #  filter(A_mapped_cells > 0) %>%
    #  ggplot(aes(x = A_mapped_cells)) +
    #  geom_histogram(binwidth=5)
      
    
    #read planted area data (from IBGE)
    planted <- read_excel(paste0(input_path,"Data/PlantedAreas/PlantedArea_",yrs[yr],".xlsx"), sheet = paste0(yrs[yr]), col_names=T)  
    #  planted <- read_csv("Data/ObservedLCmaps/PlantedArea_2000-2003.csv")
    
    # #From planted area data calculate number of cells for:
    # - Soybean + Maize  [A_plant]
    # - Cotton + Rice + Sugar_Cane + Bean + Sorghum + Wheat [OA_plant]
    
    #no data for first_crop maize in 2001 and 2002 so use 2003 data
    if(yrs[yr] == 2001 | yrs[yr] == 2002) {
      planted <- planted %>%
        mutate(A_plant_ha = first_crop_2003 + soybean)
    } else {
      planted <- planted %>%
        mutate(A_plant_ha = first_crop + soybean) 
    }
    
    planted <- planted %>%
      mutate(OA_plant_ha = cotton + rice + sugarcane + bean + sorghum + wheat) %>%
      mutate(A_plant_cells = round(A_plant_ha / 2500, 0)) %>%
      mutate(OA_plant_cells = round(OA_plant_ha / 2500, 0))  #one cell = 2500ha
  
    
    #join the data
    joined <- left_join(mapped, planted, by = c("muniID" = "IBGE_CODE"))
    
    #previously used to check the join 
    #(this is where issue with muni 5006275 was discovered
    #munis 4300001 and 4300002 are also missing, but these are large lakes with minimal agriculture
    #missing <- joined %>% 
    #  filter(is.na(A_plant_cells))
    
    #calculate differences between mapped and planted areas (in cells)
    diffs <- calcDiffcs(joined)
    diffs <- diffs %>% filter(!is.na(A_plant_cells))  #drop NA
    
    #now update map
    #read muniID map -> get x,y,z
    #load the rasters
    #munis.r <- raster(munis.r)
    lc.r <- raster(paste0(input_path,"Data/Classified/LandCover",yrs[yr],"_",cname,".asc"))
    
    munis.t <- extractXYZ(munis.r, addCellID = F)
    lc.t <- extractXYZ(lc.r, addCellID = F)
    
    munis.t <- as.data.frame(munis.t)
    munis.t <- plyr::rename(munis.t, c("vals" = "muniID"))
      
    lc.t <- as.data.frame(lc.t)
    lc.t <- plyr::rename(lc.t, c("vals" = "lc"))
     
    
    #join observed land cover map (so have x,y,muniID,original LC
    lc_munis <- left_join(as.data.frame(munis.t), as.data.frame(lc.t), by = c("row" = "row", "col" = "col"))
    
    #note: missing cells after join 
    #lcNA <- lc_munis %>% filter(is.na(lc)) 
    
    #for testing
    #this.muniID <- 4202073
    #lcs <- filter(lc_munis, muniID == this.muniID)
    #convs <- filter(j, muniID == this.muniID)
    #convertLCs(convs, lcs)
    
    final <- data.frame() 
  
    #for testing
    #dummy <- c(3527603,3527603,3527504,3527504,3528205)
      
    #loop through all munis to update https://stackoverflow.com/a/13916342/10219907
    for(i in 1:length(unique(diffs$muniID))) {
    
    #for(i in 1:length(unique(dummy))) {  
      
      #i <- 1 # for testing
      #this.muniID <- unique(dummy)[i]
  
      this.muniID <- unique(diffs$muniID)[i]
      
      lcm <- filter(lc_munis, muniID == this.muniID)
      js <- filter(diffs, muniID == this.muniID)
       
      this.conv <- convertLCs(js, lcm)
      
      #print(this.muniID)
      #print(lcm)
      #print(this.conv)
      
      if(i == 1) final <- this.conv
      else final <- bind_rows(final, this.conv)
  
    }
    
    #set final to a raster with same extent as inputs (to the same)with help from https://gis.stackexchange.com/questions/250149/assign-values-to-a-subset-of-cells-of-a-raster)
    final.r <- raster(munis.r)
    final.r[] <- NA_real_
    cells <- cellFromRowCol(final.r, final$row, final$col)
    final.r[cells] <- final$lc
  
    #becasue there are a few munis with no planted data we end up with some 'holes' in the data
    #fill those holes with the original lc data
    final.cov <- cover(final.r, lc.r)
    final.r <- mask(final.cov, munis.r) 
    
    #protected and pasture, change to nature
    Lprotect <- raster(paste0(input_path,"Data/All_ProtectionMap.asc")) #read protected map #land protection is intially identical for all services
    final.r[final.r == 5 & Lprotect < 1] <- 1   #protected and pasture, change to nature
  
    writeRaster(final.r, paste0(input_path,"Data/Classified/LandCover",yrs[yr],"_",cname,"_Disagg.asc"), format = 'ascii', overwrite=T)
  }
  
  if(sumTab){
    createSummaryTables(munis.r, yrs, disagg=TRUE)
  }
}

