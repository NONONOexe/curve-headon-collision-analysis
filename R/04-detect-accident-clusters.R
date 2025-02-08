# Load necessary packages
# install.package("dbscan")

# Get the directory for intermediate data from the environment variable
intermediate_dir <- Sys.getenv("INTERMEDIATE_DATA_DIR")

# Define the path to the accident data and read the data
headon_collisions <- readRDS(file.path(intermediate_dir, "03-headon_collisions.rds"))

# Extract accident coordinates and transform to the Web Mercator projection
accident_coords <- headon_collisions |>
  purrr::pluck("accident_info") |>
  sf::st_transform(crs = 3395) |>
  sf::st_coordinates()

# Define DBSCAN parameter grid
dbscan_params <- tidyr::expand_grid(
  eps    = c(50, 30, 10), # Neighborhood radius
  minpts = c(3, 4, 5, 6)  # Minimum points in a cluster
)

# Perform DBSCAN clustering for each parameter combination
results <- dbscan_params |>
  dplyr::mutate(
    clusters = purrr::map2(eps, minpts, dbscan::dbscan, x = accident_coords),
    n_clusters = purrr::map_int(clusters, ~length(unique(.x$cluster)))
  )

# Show the results
print(results)

# Select interest clustering parameters
eps_selected    <- 10
minpts_selected <- 4

# Extract the clustering result corresponding to the selected parameters
selected_cluster <- results |>
  dplyr::filter(eps == eps_selected, minpts == minpts_selected) |>
  dplyr::pull("clusters") |>
  purrr::pluck(1)

# Save the selected clustering results
saveRDS(
  selected_cluster,
  file.path(intermediate_dir, "04-collision_clusters.rds")
)
