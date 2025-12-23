# Kelpbass_SMURF
Analysis of larval supply to kelp bass populations in California

The code in this repo comprises the analyses presented in White et al., "Spatiotemporal variation in larval supply and kelp habitat explain trends in population dynamics of fish populations in and out of marine protected areas", currently submitted to Ecology Letters.

The data formatting, analysis, and creation of all figures is found in the Rmarkdown file MPA_recruitment_v1.Rmd. This file also locally installs the PISCO.file.helpers library, which loads and manipulates the datasets found in /data.

Prior to running this code, you will need to download the PISCO subtidal fish survey data, as the file is too large to be hosted on Github. This dataset is found in PISCO, Mark Carr, & Jenn Caselle. (2009). PISCO: Subtidal: Community Surveys: Fish Survey. PISCO MN. doi:10.6085/AA/pisco_subtidal.150.2. 

The file pisco_fish_data.csv should be placed in the ../data folder on your local device.
