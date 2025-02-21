# This script identifies clusters of head-on collision accidents on curves
# using DBSCAN algorithm.

# Load sf package to handle spatial vector data classes
library(sf)

# Get the directory for intermediate data from the environment variable
intermediate_dir <- Sys.getenv("INTERMEDIATE_DATA_DIR")

# Read the head-on collision accident data
headon_data <- readr::read_rds(
  file.path(intermediate_dir, "03-headon_collisions.rds")
)

# Extract accident coordinates occurring on curves
curve_accident_points <- headon_data |>
  jpaccidents::filter_road_shape("curve") |>
  purrr::pluck("accident_info")

# Transform coordinates to the Web Mercator projection
curve_accident_coords <- curve_accident_points |>
  sf::st_transform(crs = 3395) |>
  sf::st_coordinates()

# Perform DBSCAN clustering
curve_collion_clusters <- dbscan::dbscan(
  curve_accident_coords, eps = 10, minPts = 2
)

# Assign cluster labels and remove noise points (cluster 0)
curve_clustered_points <- curve_accident_points |>
  dplyr::mutate(cluster = curve_collion_clusters$cluster) |>
  dplyr::filter(cluster != 0)

# Save the clustering results
readr::write_rds(
  curve_clustered_points,
  file.path(intermediate_dir, "05-curve_clustered_points.rds")
)
