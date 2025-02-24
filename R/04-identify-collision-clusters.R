# This script identifies spatial clusters of head-on collision accidents
# using DBSCAN algorithm.
#
# INPUTS:
#   {INTERMEDIATE_DATA_DIR}/headon_collisions.rds: Head-on collision accidents
#
# OUTPUTS:
#   {INTERMEDIATE_DATA_DIR}/clustered_points.rds: Clustered points
#

# Load sf package to handle spatial vector data classes
library(sf)

# Get the directory for intermediate data from the environment variable
intermediate_dir <- Sys.getenv("INTERMEDIATE_DATA_DIR")

# Read the head-on collision accident data
headon_data <- readr::read_rds(
  file.path(intermediate_dir, "headon_collisions.rds")
)

# Extract accident coordinates
accident_points <- headon_data |>
  purrr::pluck("accident_info")

# Transform coordinates to the Web Mercator projection
accident_coords <- accident_points |>
  sf::st_transform(crs = 3395) |>
  sf::st_coordinates()

# Define parameter grid for DBSCAN
dbscan_params <- tidyr::expand_grid(
  eps = c(50, 30, 10), # Neighborhood radius
  minpts = c(3, 4, 5, 6) # Minimum points in a cluster
)

# Perform DBSCAN clustering for each parameter combination
results <- dbscan_params |>
  dplyr::mutate(
    clusters = purrr::map2(eps, minpts, dbscan::dbscan, x = accident_coords),
    n_clusters = purrr::map_int(clusters, ~ length(unique(.x$cluster)))
  )

# Show the results
print(results)

# Select interest clustering parameters
eps_selected <- 10
minpts_selected <- 4

# Extract the clustering result corresponding to the selected parameters
selected_cluster <- results |>
  dplyr::filter(eps == eps_selected, minpts == minpts_selected) |>
  dplyr::pull("clusters") |>
  purrr::pluck(1)

# Assign cluster labels and remove noise points (cluster 0)
clustered_points <- accident_points |>
  dplyr::mutate(cluster = selected_cluster$cluster) |>
  dplyr::filter(cluster != 0)

# Save the clustering results
readr::write_rds(
  clustered_points,
  file.path(intermediate_dir, "clustered_points.rds")
)
