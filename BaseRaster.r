#Create a base raster file from a shapefile


rm(list=ls())

library(sf)
library(raster)
library(tidyverse)

belmontDataPath <- "C:/Users/k1076631/Google Drive/Research/Projects/Belmont/Data/"
setwd(paste(belmontDataPath, "Brazil/AdminBoundaries/IBGE", sep=""))

#read shape file 
BRadmin = st_read("BRadmin.shp")  


#5km base for use to ensure identical origin to 1km
BRadmin_latlon <- raster(ncols=901,
                    nrows=891,
                    xmn=-75.00007,
                    ymn=-34.01103,
                    ymx=6.053681,
                    xmx=-34.48571,
                    resolution=0.044966,
                    vals=NULL)


#create rasters from the shp using the empty raster
BRmunis_r_latlon <- rasterize(as(BRadmin, "Spatial"), y = BRadmin_latlon, field = "CD_GEOCMUn")
BRstates_r_latlon <- rasterize(as(BRadmin, "Spatial"), y = BRadmin_latlon, field = "State")


#remove non-simulated states
simStates <- c(29,31,35,41,42,43,50,51,52,17)                   #ids of states to simulate
simdf <- as.data.frame(cbind(simStates,simStates))           #create a df for next two functions
sim_BRstates_r_latlon <- subs(BRstates_r_latlon, simdf)            #retain simStates values, otherwise set to NA
sim_BRmunis_r_latlon <- mask(BRmunis_r_latlon,sim_BRstates_r_latlon)  #mask munis raster using NAs in simStates raster

#trim the extent of the raster (on NAs as default)
#sim_BRstates_r_latlon <- trim(sim_BRstates_r_latlon)  
#sim_BRmunis_r_latlon <- trim(sim_BRmunis_r_latlon)    

#check
#plot(sim_BRmunis_r_latlon)
#plot(sim_BRstates_r_latlon)

#writing to file
writeRaster(sim_BRmunis_r_latlon, "sim10_BRmunis_latlon_5km.asc", "ascii", overwrite = T, NAflag = -9999)
writeRaster(sim_BRstates_r_latlon, "sim10_BRstates_latlon_5km.asc", "ascii", overwrite = T, NAflag = -9999)

writeRaster(BRmunis_r_latlon, "BRmunis_latlon_5km.asc", "ascii", overwrite = T, NAflag = -9999)
writeRaster(BRstates_r_latlon, "BRstates_latlon_5km.asc", "ascii", overwrite = T, NAflag = -9999)
