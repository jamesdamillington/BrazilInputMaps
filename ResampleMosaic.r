#Script to resample and mosaic mapbiomas data

rm(list=ls())

library(raster)

inpath <- "Data/MapBiomas/"
outpath <- "Data/Unclassified/"

#set years for analysis
startyr <- 2000
endyr <- 2018
stepyr <- 1
yrs <- seq(startyr,endyr,stepyr)
yrs <- yrs - 1984   #because 1985 is in band 1 of the MapBiomas tif files
labelyr <- paste0(startyr,"-",endyr,"-",stepyr)

#set biomes for analysis
biomes <- list("PAMPA", "PANTANAL","MATAATLANTICA","CAATINGA","CERRADO","AMAZONIA") 

print(paste0("Start: ",Sys.time()))

#loop by biome to aggregate to 1km
for(b in biomes){

  print(b)
  sraw <- stack(paste0(inpath,b,".tif"))  #read 30m file to a stack
  sraw <- sraw[[yrs]]                   #select only required years
  
  #resample to 5km in two steps
  map1km <- aggregate(sraw, fact=33, fun=modal, expand=TRUE, 
                      filename=paste0(inpath,b,"_",labelyr,"_1km.tif", overwrite=TRUE))
  
  #write to file as multi-layer tif
  writeRaster(map1km, paste0(inpath,b,"_",labelyr,"_1km.tif"), format="GTiff", bylayer=FALSE)
}

#reset years as the 1km tif layers may differ from the 30m tif 
yrs <- seq(startyr,endyr,stepyr)
yrs <- yrs - startyr + 1   #+1 because raster reads multi-layer files with 1 index file

#loop by year to create Brazil mosaic and aggregate to 5km
for(y in yrs){
  
  year = y+startyr-1
  print(year)
  
  #for each biome read the 1km tif layer for this year
  P <- raster(paste0(inpath,"PAMPA","_",labelyr,"_1km.tif"),band=y)
  M <- raster(paste0(inpath,"MATAATLANTICA","_",labelyr,"_1km.tif"),band=y)
  C <- raster(paste0(inpath,"CAATINGA","_",labelyr,"_1km.tif"),band=y)
  O <- raster(paste0(inpath,"CERRADO","_",labelyr,"_1km.tif"),band=y)
  A <- raster(paste0(inpath,"AMAZONIA","_",labelyr,"_1km.tif"),band=y)
  L <- raster(paste0(inpath,"PANTANAL","_",labelyr,"_1km.tif"),band=y)
  
  #specify (in MapBiomas data) to NoData
  P <- reclassify(P, cbind(0, NA))
  M <- reclassify(M, cbind(0, NA))
  C <- reclassify(C, cbind(0, NA))
  O <- reclassify(O, cbind(0, NA))
  A <- reclassify(A, cbind(0, NA))
  L <- reclassify(L, cbind(0, NA))
  
  #1km base used to resample biomes to standard origin
  base1km <- raster(ncols=4505,
                    nrows=4455,
                    xmn=-75.00007,
                    ymn=-34.01103,
                    ymx=6.053681,
                    xmx=-34.48571,
                    crs=crs(P),
                    resolution=0.008993,
                    vals=NULL)
  
  #resample biomes to standard origin for mosaic below
  Pbase <- resample(P,base1km,method="ngb")
  Mbase <- resample(M,base1km,method="ngb")
  Cbase <- resample(C,base1km,method="ngb")
  Obase <- resample(O,base1km,method="ngb")
  Abase <- resample(A,base1km,method="ngb")
  Lbase <- resample(L,base1km,method="ngb")
  
  #mosaic the 1km map for all of Brazil
  mos1km <- mosaic(Pbase,Mbase,Cbase,Obase,Abase,Lbase, fun=max)
  
  #aggregate the 1km map to 5km
  mos5km <- aggregate(mos1km, fact=5, fun=modal, na.rm=T, expand=TRUE)
  
  #5km base for use to ensure identical origin to 1km
  base5km <- raster(ncols=901,
                    nrows=891,
                    xmn=-75.00007,
                    ymn=-34.01103,
                    ymx=6.053681,
                    xmx=-34.48571,
                    crs=crs(P),
                    resolution=0.044966,
                    vals=NULL)
  
  #resample to ensure identical origin to 1km
  mos5km_res <- resample(mos5km,base5km,method="ngb")
  
  #write Brazil 5km map for this year to file
  writeRaster(mos5km_res,paste0(outpath,"Brazil_",year,"_5km.asc"),format="ascii", overwrite=T)

}

print(paste0("End: ",Sys.time()))


