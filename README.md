# Curve Head-On Collision Analysis

## Overview

This project aims to reduce head-on collision casualties by analyzing the relationship between head-on collisions and road curvature in Japan. The analysis includes geospatial cluster analysis, map visualization, and factor analysis using decision trees.

This project analyzes actual traffic accident data recorded between 2019 and 2023, provided by the National Police Agency of Japan.


## Scripts

The `/R` directory contains the following scripts:

- **01-fetch-accident-data.R**: Downloads traffic accident data for the years 2019 to 2023.
- **02-clean-accident-data.R**: Cleans the downloaded accident data and saves it for further analysis.
- **03-extract-headon-collisions.R**: Extracts head-on collision accidents from the cleaned accident data.
- **04-identify-collision-clusters.R**: Identifies spatial clusters of head-on collision accidents using the DBSCAN algorithm.
- **05-identify-curve-collision-clusters.R**: Identifies clusters of head-on collision accidents on curves using the DBSCAN algorithm.
- **06-visualize-collision-clusters.R**: Visualizes the clusters of head-on collisions.
- **07-visualize-cluster-details.R**: Visualizes the details of clusters where head-on collisions frequently occur, creating detailed maps for the top accident clusters in Osaka and Kanagawa.
- **08-fetch-road-data.R**: Fetches road data around curve accident locations using OpenStreetMap.
- **09-calculate-road-curvature.R**: Calculates road curvature for accident locations.
- **10-analyze-road-curvature.r**: Analyzes road curvature in relation to head-on collisions, creating visualizations and performing statistical analysis.
- **11-curvature-dicision-tree.R**: Creates a decision tree model to predict head-on collisions based on road curvature.

Each script contains detailed documentation at the beginning of the file describing its purpose, inputs, and outputs.


## Installation

This project's scripts use the following packages. Please install them as needed:

- `tidyverse`
- `sf`
- `dbscan`
- `mapgl`
- `rpart`
- `rpart.plot`
- `cli`
- `fs`
- `NONONOexe/jpaccidents`
- `NONONOexe/pavement`

```
install.packages("pak")  # if not installed
pak::pak("tidyverse")    # install package (i.e. tidyverse)
```


## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.


## Contact

For questions or feedback, please contact me at [ando@maslab.aitech.ac.jp](mailto:ando@maslab.aitech.ac.jp) or open an issue.
