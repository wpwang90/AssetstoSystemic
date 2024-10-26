# Railway system climate risk management

This repository provides the code to perform a national railway system risk analysis for floods and landslides. 

These codes for the work titled ""

## Data requirements
The data sources utilized in this study encompass various domains related to railway operations, topography, economic data, hydrological projections, and climate-related risks:

* Railway Operation Data: Information regarding train operation plans for the railway system is obtained from the Chinese online train booking platform (http://www.12306.cn).
* Topographic Data: Digital Elevation Model (DEM) data at a resolution of 3 arc-seconds is acquired from HydroSHEDS Core Data (https://www.hydrosheds.org/hydrosheds-core-downloads).
* Economic Data: Historical GDP data is sourced from gridded datasets for population and economic activity, available at https://www.scidb.cn/en/detail?dataSetId=73c1ddbd79e54638bd0ca2a6bd48e3ff. *Projections for GDP in the late 21st century are accessed through a global dataset of gridded population and GDP scenarios (http://www.cger.nies.go.jp/gcp/population-and-gdp.html).
* Hydrological Data: Daily runoff data are extracted from the World Climate Research Programme (WCRP CMIP6), which can be accessed at https://esgf-node.llnl.gov/search/cmip6/.
* Coastal Flooding Data: Data inputs for modeling coastal flooding are provided by the repository at https://zenodo.org/records/6969115.
* Landslide Data under Climate Change: Data related to landslides in the context of climate change are available in the public repository at https://doi.org/10.6084/m9.figshare.23694501.v1.

## Prepare data and results paths

    "data_path": "../Data/",
    "result_path": "../res/",

## R packages requirements


```bash

library(raster)
library(rgdal)
library(ggplot2)
library(maptools)  
library(stars)
library(sf)
library(dplyr)
library(tidyr)
library(car)
library(reshape2)
library(data.table)
library(ggthemes)
library(RColorBrewer)
library(purrr)
library(proj4)
library(rineq)
library(ncdf4)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(ggExtra)
library(raster)
library(igraph)
library(scales)
library(EnvStats)

### License
Copyright (C) 2024 Weiping Wang. All versions released under the [GNU Affero General Public License v3.0 license](LICENSE).
