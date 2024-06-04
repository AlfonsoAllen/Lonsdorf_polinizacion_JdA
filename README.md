# Lonsdorf_polinizacion_JdA

## Overview

This repository contains the code to obtain the results of the model by Lonsdorf et al. (2009) for the technical report entitled "EVALUATION AND ASSESSMENT OF ECOSYSTEM SERVICES FOR POLLINATION" by Ignasi Bartomeus, Carlos Martinez-Nuñez, Alfonso Allen-Perkins, and Elena Velado-Alonso.

## Data Sources

The data used includes:
- Validation point coordinates: `Results/fixed_Junta_validation_data.csv`
- Coordinates for 10,280 points in Andalusia: `Results/sites_AAP2_refined.csv`
- Expert tables: `Results/expert_table_JdA_V1.csv`
- Results of all machine learning models and Lonsdorf at the validation points: `Results/Junta_validation_data_outputs.csv`

*Note: To compute the Lonsdorf scores, the raster of Land covers provided by the Junta de Andalucía was used (not included due to storage limitations).*

## Generating Lonsdorf's Service Map Scores

To generate predictions at the validation points using the Lonsdorf model, run the following script: `R-scripts/compute_Lonsdorf_ecosistemas2023_JdA.R`

Set the following variables:
```r
compute_validation_points <- TRUE
compute_other_points <- FALSE
```

To generate predictions at the 10,280 points across Andalusia, run the same script but set:

```r
compute_validation_points <- FALSE
compute_other_points <- TRUE
```

To explore the validation process of the results obtained from the machine learning and Lonsdorf models, run: `R-scripts/validation_AAP.R`

## Visualization

To visualize the prediction results in R for both the validation points and all of Andalusia, run: `R-scripts/plot_lonsdorf_refined_results.R`

## References

Lonsdorf, E., Kremen, C., Ricketts, T., Winfree, R., Williams, N., & Greenleaf, S., 2009. "Modelling pollination services across agricultural landscapes," *Annals of Botany*, 103(9), pp. 1589–1600. [https://doi.org/10.1093/aob/mcp069](https://doi.org/10.1093/aob/mcp069).
