# noiseMelbourne
This repository was set up to contain code for preparing Melbourne data inputs required for noise model

# Overview
There are two main scripts:
- `landcover.R`, which adds G-values to the landcover raster
- `boundaries.R`, which produces a file of SA3 boundaries for Melbourne.

# Input files
The code requires the following input files, which are available [to authorised users] at [*insert location when known*].  The code assumes that the input files are located in a `data` directory ("../data/") which sits beside the directory in which the script files are located.

### `landcover.R`

| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*data/original/VIC_LANDCOVER_TS.zip* | Victorian Land Cover Time Series data, 2015-19 |
|*data/processed/region.sqlite* |Region: the study area for the JIBE project    |

### `boundaries.R`

| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*data/original/1270055001_mb_2016_vic_shape.zip* | ABS 2016 census meshblocks for Victoria |

# Output files
The code produces the following output files, which are saved to the `data\processed` directory.

#### `landcover.R`
| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*landcover.zip*     |Landcover polygons, including G values                    |

#### `boundaries.R`
| File               | Content                                                  |
|--------------------|----------------------------------------------------------|
|*melb_sa3.zip*      |SA3 polygons for Greater Melbourne                        |

