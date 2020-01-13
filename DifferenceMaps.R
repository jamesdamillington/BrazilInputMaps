#DifferenceMaps - create an output raster from two input rasters to show locations of differences 
#between the two inputs for a given cell class ('from' is target in first raster, 'to' is target in second)

##load libraries
rm(list=ls())
library(raster)

input_path <- "C:/Users/k1076631/Google Drive/Shared/Crafty Telecoupling/Data/LandCover/MapBiomas4/BrazilInputMaps/Data/Classified/"

#classification base
cls <- "_PastureB_Disagg.asc"

#compare the following years (number of output maps will be n - 1)
yrs <- seq(2001, 2018, 1)   

#set changeTO TRUE of we want to check change for target land cover in the second raster (i.e. cells that have changed TO to_target in the later year)
#else set FALSE if we check change for target land cover in the firstraster (i.e. cells that have changed FROM from_target in the earlier year)
changeTO <- TRUE

#target land cover in the FIRST raster to assess change of (used when changeTO is FALSE)
from_target <- 3

#target land cover in the SECOND raster to assess change of (used when changeTO is TRUE)
to_target <- 3


for(i in seq(2,length(yrs),1)){
  
  r1name <- paste0("LandCover", yrs[i-1], cls)
  r2name <- paste0("LandCover", yrs[i], cls)
  
  print(r1name)
  print(r2name)
  
  r1 <- raster(paste0(input_path,r1name))
  r2 <- raster(paste0(input_path,r2name))
  
  r3 <- r2
  r3[!is.na(r3)] <- 0

  if(changeTO){
    r3[r2 == to_target & r1 != to_target] <- 1
    
    writeRaster(r3, paste0(input_path,"Diffc",cls,"_",yrs[i-1],"-",yrs[i],"_toLC",to_target,".asc"), 
      format="ascii", overwrite=T)
  }
  else{
    r3[r1 == from_target & r2 != from_target] <- 1
    
    writeRaster(r3, paste0(input_path,"Diffc_",cls,"_",yrs[i-1],"-",yrs[i],"_fromLC",from_target,".asc"), 
      format="ascii", overwrite=T)
  }
}



#proof  
# r1 <- raster(nrow=10, ncol=10)
# r1[] <- 2
# r1[25] <- 4
# r1[45] <- 3
# 
# r2 <- raster(nrow=10, ncol=10)
# r2[] <- 2
# r2[5] <- 3
# r2[25] <- 4
# r2[45] <- 3
# plot(r2)
# 
# target <- 3
# 
# r3 <- r2
# r3[] <- 0
# 
# r3[r2 == target & r1 != target] <- 1
# plot(r3)
