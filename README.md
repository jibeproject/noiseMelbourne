# noiseMelbourne
This repository was set up to contain code for preparing Melbourne data inputs required for noise model

# Overview
There are three main scripts:
- `landcover.R`, which adds G-values to the landcover raster
- `boundaries.R`, which produces a file of SA3 boundaries for Melbourne.
- `buildings.R`, which adds purpose and height values to building footprints.

# Input files
The code requires the following input files, which are available [to authorised users] at [*insert location when known*].  The code assumes that the input files are located in a `data` directory ("../data/") which sits beside the directory in which the script files are located.

### `landcover.R`

| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*data/original/VIC_LANDCOVER_TS.zip* | Victorian Land Cover Time Series data, 2015-19 |
|*data/processed/region.sqlite* | Region: the study area for the JIBE project    |


### `boundaries.R`

| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*data/original/1270055001_mb_2016_vic_shape.zip* | ABS 2016 census meshblocks for Victoria |


### `buildings.R`

| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*data/original/melbourne_buildings.zip* | Melbourne building footprints from Overture Maps |
|*data/processed/poi.gpkg* | Locations of points of interest                    |
|*data/original/1270055001_mb_2016_vic_shape.zip* | ABS 2016 census meshblocks for Victoria |
|*data/original/2016 census mesh block counts.csv* | ABS 2016 census meshblock population and dwelling counts |
|*data/original/LGAs.zip* | Local government areas for Victoria                 | 
|*data/original/2018-building-footprints.zip* | Footprints for buildings in the City of Melbourne |
|*data/processed/yarra_buildings/yarra_building_bed_elevation.tif* | Elevation of buildings in the City of Yarra |
|*data/original/yarra_buildings/mga55_yarra_1m_dtm.tif* | Digital elevation model for the City of Yarra |
|*data/original/manningham_building_footprints.zip* | Footprints for buildings in the City of Manningham |



# Output files
The code produces the following output files, which are saved to the `data\processed` directory.

### `landcover.R`
| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*landcover.zip*     |Landcover polygons, including G values                    |

### `boundaries.R`
| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*melb_sa3.zip*      |SA3 polygons for Greater Melbourne                        |

### `buildings.R`
| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*melbourne_buildings_processed.sqlite* |Building footprints, including purpose and height values |

