# This script visualizes the details of clusters where head-on collisions
# frequently occur. It creates detailed maps for the top accident clusters in
# Osaka and Kanagawa.
#
# INPUTS:
#   - {INTERMEDIATE_DATA_DIR}/cluster_centroids.rds: Centroids and summary
#     statistics of head-on collision clusters
#   - {INTERMEDIATE_DATA_DIR}/clustered_points.rds: Clustered points
#   - {INTERMEDIATE_DATA_DIR}/curve_clustered_points.rds: Clustered points
#
# OUTPUTS:
#   - {PLOT_DIR}/osaka_cluster.pdf: Static visualization of head-on collision
#     cluster locations in Osaka
#   - {PLOT_DIR}/kanagawa_cluster.pdf: Static visualization of head-on collision
#     cluster locations in Kanagawa
#

# Load sf package to handle spatial vector data classes
library(sf)

# Load rlang for .data pronoun
library(rlang)

# Fetch and categorize roads for a cluster
fetch_cluster_roads <- function(centroid_point) {
  nearby_roads <- pavement::fetch_roads(
    centroid_point,
    radius = 100,
    crop = TRUE
  )

  main_road_types <- c("trunk", "motorway", "motorway_link")

  list(
    main_roads = nearby_roads |>
      dplyr::filter(.data$highway %in% main_road_types),
    secondary_roads = nearby_roads |>
      dplyr::filter(!.data$highway %in% main_road_types)
  )
}

# Create cluster detail map
create_cluster_map <- function(centroid_point, cluster_id, clustered_points) {
  # Get road data
  roads <- fetch_cluster_roads(centroid_point)

  # Filter accident points for the specific cluster
  cluster_accidents <- clustered_points |>
    dplyr::filter(.data$cluster == cluster_id)

  # Create visualization
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = roads$secondary_roads,
      linewidth = 2.5,
      colour = "#CDCDCD"
    ) +
    ggplot2::geom_sf(
      data = roads$main_roads,
      linewidth = 3.5,
      colour = "#fbd789"
    ) +
    ggplot2::geom_sf(
      data = cluster_accidents,
      colour = "red",
      size = 3
    ) +
    ggplot2::theme_void()
}

# Save cluster map
save_cluster_map <- function(plot, filename, plot_dir) {
  ggplot2::ggsave(
    plot = plot,
    filename = file.path(plot_dir, filename),
    width = 500,
    height = 400,
    scale = 5,
    units = "px"
  )
}

# Get data directories from environment variables
intermediate_dir <- Sys.getenv("INTERMEDIATE_DATA_DIR")
plot_dir <- Sys.getenv("PLOT_DIR")

# Load cluster data
cluster_centroids <- readr::read_rds(
  file.path(intermediate_dir, "cluster_centroids.rds")
)
clustered_points <- readr::read_rds(
  file.path(intermediate_dir, "clustered_points.rds")
)

# Select top clusters by number of accidents
top_clusters <- cluster_centroids |>
  dplyr::slice_max(n_accidents, n = 2)

# Create and save Osaka cluster map
# Location: https://maps.app.goo.gl/ojsAbtpmxAXNux1x9
osaka_map <- create_cluster_map(
  centroid_point = top_clusters$centroid[1],
  cluster_id = top_clusters$cluster[1],
  clustered_points = clustered_points
)
save_cluster_map(osaka_map, "osaka_cluster.pdf", plot_dir)

# Create and save Kanagawa cluster map
# Location: https://maps.app.goo.gl/RQiggBbCbLJhacAH6
kanagawa_map <- create_cluster_map(
  centroid_point = top_clusters$centroid[2],
  cluster_id = top_clusters$cluster[2],
  clustered_points = clustered_points
)
save_cluster_map(kanagawa_map, "kanagawa_cluster.pdf", plot_dir)
