# BrazilInputMaps
[![DOI](https://zenodo.org/badge/203189584.svg)](https://zenodo.org/badge/latestdoi/203189584)

Manipulating [MapBiomas](https://mapbiomas.org/en/) (version 4) data for land cover maps to use in the [CRAFTY Brazil](https://github.com/jamesdamillington/CRAFTY_Brazil) ABM simulation model.

Scripts should be run in the following order:
- [BaseRaster.r](BaseRaster.r) used to create a raster file from a shapefile used in other scripts
- [ResampleMosaic.r](ResampleMosaic.r) resamples (to 5km) and mosaics (multiple biomes) Mapbiomas data for later classification
- [ClassifyDisaggregate.r](ClassifyDisaggregate.r) classifies MapBiomas maps (possibly output from [ResampleMosaic.r](ResampleMosaic.r)) then optionally uses planted area data to improve classification
- [ClassificationComparison.Rmd](ClassificationComparison.Rmd) present code and analyses of pasture areas, meat production and pasture yield for all states 2001-2018, for three different classifications of the MapBiomas data both with and without disaggregation using planted area data. Analysis is best viewed online through the [markdown version](ClassificationComparison.md)

Ancillary data are in the Data directory. 

## Notes on [ClassifyDisaggregate.r](ClassifyDisaggregate.r)

- See the included Excel spreadsheet [MapBiomas_CRAFTY_classifications_v4.xlsx](Data/MapBiomas_CRAFTY_classifications_v4.xlsx) for example classifications (and those examined in [ClassificationComparison.Rmd](ClassificationComparison.Rmd)
- In all classifications, Pasture in areas of land protection is designated as ‘Nature’
