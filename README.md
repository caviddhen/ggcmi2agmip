Processing GGCMI ISIMIP3b outputs for AgMIP
================
David M Chen (<david.chen@pik-potsdam.de>)
21 January, 2021

  - [ggcmi2agmip](#ggcmi2agmip)
      - [Installation](#installation)
      - [Processing](#processing)
          - [Parameters](#parameters)
          - [Default Cluster Processing](#default-cluster-processing)
          - [Files required for local
            processing](#files-required-for-local-processing)
      - [Output Files](#output-files)

# ggcmi2agmip

ggcmi2agmip bundles the functions required to process GGCMI outputs from
ISIMIP3b, located either on the PIK/ISIMIP cluster or locally. If run
locally, note that the path structure for the GGCMI data needs to be set
as on cluster, i.e. starting with the crop model name as so:
“/LPJmL/phase3b/mri-esm2-0/ssp585/mai” (/model name/phase3b/climate
model/ssp scenario/crop) . Spam files do not require specific folder
structure as they are single files.

## Installation

Install the development version from
[GitHub](https://github.com/caviddhen) with:

``` r
#install package
devtools::install_github("caviddhen/ggcmi2agmip")
library(ggcmi2agmip)
```

## Processing

Tne processing is run using the processIsimip() function, with preferred
paths and parameters. See default configuration below for running on
cluster. Parameters that end in “.path” are paths where the spam files
are found on cluster. If processing is done locally, please see Files
required… below

### Parameters

**general.path**: general path where the crop model outputs are stored,
described as above

**models**: chooses the crop model to process

**rcpso**: rcp scenarios

**gcms**: climate model

**co2\_scen**: “2015co2” (co2 effect off) or “default” (co2 effect on)

**time**: time scale to process (comparing 30 year average centred
around 2030,2050, or 2080.)

### Default Cluster Processing

``` r
# run processing
processIsimip(general.path = "/p/projects/macmit/data/GGCMI/AgMIP.output/",
                         models = c("LPJmL","EPIC-IIASA","pDSSAT","GEPIC","LPJ-GUESS"),
                         rcpso = c("historical","picontrol","ssp126","ssp585"),
                         gcms = c("gfdl-esm4","mpi-esm1-2-hr","mri-esm2-0","ukesm1-0-ll"),
                         co2_scen = c("2015co2","default"),
                         time = c("2030s","2050s","2085s", "2100"),
                         lpj.spam.path = "/p/projects/macmit/data/GGCMI/fast-track/yields/yield_shifter_econ_models/some_spam/",
              adm.path= "/p/projects/macmit/data/GGCMI/AgMIP.output/processed/masks/aggr/",
                         rice.path = "/p/projects/macmit/data/GGCMI/AgMIP.input/phase3/crop_calendar/",
                         wheat.path = "/p/projects/macmit/data/GGCMI/AgMIP.input/phase3/landuse/winter_spring_wheat_separation/",
                          output.path = "/p/projects/landuse/users/davidch/AgMIP_impacts/ISIMIP3b")
```

### Files required for local processing

If processing on a local computer, then spam files are required from
cluster, with the path to these files listed in the default settings
above.

**lpjml spam for Irrigated and Rainfed cropping area**:
lpj.spam.path/*crop*“.R\_physical\_area” and “.I\_physical\_area” for
all crops

**adm file for mapping country names and codes**:
adm.path/“gadm0.meta.csv”

**rice spam for first and second cropping of rice**:
rice.path/"\_rf\_ggcmi\_crop\_calendar\_phase3\_v1.01.nc4" and
"\_ir\_ggcmi\_crop\_calendar\_phase3\_v1.01.nc4

**wheat spam for spring and winter wheat **:
wheat.path/winter\_and\_spring\_wheat\_areas\_phase3.nc4

## Output Files

processIsimip() saves the processed output files in the output.path
given. All values are in calories dry matter. Currently, the function
produces, for each time period, for co2 scenario, ssp scenario, and for
irrigated and rainfed crops, a table of crop model and crop by country:

**ISI-MIP3\_production\_changes**: total amount of calories produced in
future time vs 2000s

**ISI-MIP3\_percent\_changes\_30-year\_average**: Average percent change
over 30 years for time step t vs 2000s

**ISI-MIP3\_percent\_changes\_30-year\_annual**: Annual percent change
over 30 years for time step t vs 2000s

**ISI-MIP3\_growth\_rates\_30-year\_average**: Average growth rates by
crop model and crop for time step t vs 2000s

**ISI-MIP3\_percent\_shocks\_30-year\_annual**: The change from one year
to the previous year in the 30-year time step t
