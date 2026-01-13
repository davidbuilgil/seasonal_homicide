# Climate Change and Crime: Shifts in Seasonal Violence Patterns — Replication Materials

This repository contains the code and data to reproduce the analyses in:

*Climate Change and Crime: Shifts in Seasonal Violence Patterns* (manuscript under review).

## Repository structure

- Scripts:
  - `script_colombia.R` Loads raw data for Colombia, creates monthly anomaly series, runs fixed-effects and robustness models and saves results, and generates figures used in the manuscript.
  - `script_mexico.R` Loads raw data for Mexico, creates monthly anomaly series, runs fixed-effects and robustness models and saves results, and generates figures used in the manuscript.
  -  `script_gainesville.R` Loads raw data for Gainesville, creates monthly anomaly series, runs fixed-effects and robustness models and saves results, and generates figures used in the manuscript.
- `data/`  Raw data used in the study, including shapefiles.
- `output/` Regression outputs, summary tables, and figures exported for the paper.

## Data access (how to obtain the raw data)

The raw datasets are public and can be downloaded from the following sources (accessed **November 10, 2025**):

### Colombia
- Homicides (Ministry of National Defense Open Data Portal):  
  https://www.datos.gov.co/Seguridad-y-Defensa/HOMICIDIO/m8fd-ahd9/about_data
- Rainfall (OCHA / Climate Hazards Center):  
  https://data.humdata.org/dataset/col-rainfall-subnational
- Temperature (World Bank Climate Change Knowledge Portal; CMIP6-based):  
  https://climateknowledgeportal.worldbank.org/download-data

### Mexico
- Homicides (Executive Secretariat of the National Public Security System):  
  https://www.gob.mx/sesnsp/acciones-y-programas/datos-abiertos-de-incidencia-delictiva
- Rainfall (OCHA / Climate Hazards Center):  
  https://data.humdata.org/dataset/mex-rainfall-subnational
- Temperature (World Bank Climate Change Knowledge Portal; CMIP6-based):  
  https://climateknowledgeportal.worldbank.org/download-data

### Gainesville, Florida
- Crime incidents (City of Gainesville Open Data Portal):  
  https://data.cityofgainesville.org/Public-Safety/Crime-Responses/gvua-xt9q/about_data
- Rainfall (Florida State University Climate Center):  
  https://climatecenter.fsu.edu/products-services/data/precipitation/gainesville
- Temperature (World Bank Climate Change Knowledge Portal; CMIP6-based):  
  https://climateknowledgeportal.worldbank.org/download-data

### Where to save downloaded data
After downloading, place the raw files in:

- `data/`

## Software requirements

- R (recommended: R >= 4.2)
- R packages used in the analysis include:
  - `here`, `ggplot2`, `plm`, `nmle`, `sf`, `spdep`, `stringr`, `ggpubr`, `lubridate`, `purrr`, `tidyr`
