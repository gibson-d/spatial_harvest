# Covariate Data

This repository hosts the raw covariate data for the multispecies direct recovery model.

north_america_tmax.csv consists of the 60-year (1960 - 2019) average summer-fall mean maximum temperature for each recovery region (State, Province, Mexico) considered as a potential location harvest occurred. Data were accquired from TerraClimate and post-processed through R-scripts to correspond to each recovery region.

north_america_wdef.csv consists of the annual (1960 - 2019) estimates of summer-fall water deficits for each recovery region (State, Province, Mexico) considered as a potential location harvest occurred. Data were accquired from TerraClimate and post-processed through R-scripts to correspond to each recovery region.

wetland_habitat.csv: The amount of wetland habitat (km^2) that remained in each recovery region (State, Province, Mexico) for each decade from 1960 - 2020. Data were acquired from https://zenodo.org/records/7293597 as part of https://www.nature.com/articles/s41586-022-05572-6. Data occur at the decade-scale and were down-scaled through R-scripts to correspond to wetland habitat in each recovery region.

delta_wetland_habitat.csv: The change in the amount of wetland habitat (km^2) since the last decade in each recovery region (State, Province, Mexico) for each decade from 1960 - 2020. Data were acquired from https://zenodo.org/records/7293597 as part of https://www.nature.com/articles/s41586-022-05572-6. Data occur at the decade-scale and were down-scaled through R-scripts to correspond to wetland habitat in each recovery region.
