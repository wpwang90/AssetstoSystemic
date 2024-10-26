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

## Python requirements

Recommended option is to use a [miniconda](https://conda.io/miniconda.html)
environment to work in for this project, relying on conda to handle some of the
trickier library dependencies.

```bash

# Add conda-forge channel for extra packages
conda config --add channels conda-forge

# Create a conda environment for the project and install packages
conda env create -f environment.yml
activate AssetstoSystemic


