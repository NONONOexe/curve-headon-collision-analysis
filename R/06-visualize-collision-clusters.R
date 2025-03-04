# This script visualizes the clusters of head-on collisions.
#
# INPUTS:
#   - {INTERMEDIATE_DATA_DIR}/clustered_points.rds: Clustered points
#   - {INTERMEDIATE_DATA_DIR}/curve_clustered_points.rds: Clustered points
#
# OUTPUTS:
#   - {INTERIM_DATA_DIR}/cluster_centroids.rds: Centroids and summary
#     statistics of head-on collision clusters
#   - {PLOT_DIR}/headon_collision_clusters.pdf: Static visualization of head-on
#     collision cluster locations
#   - {PLOT_DIR}/curve_headon_collision_clusters.pdf: Static visualization of
#     head-on collision clusters at curved roads
#

# Load sf package to handle spatial vector data classes
library(sf)

# Load rlang for .data pronoun
library(rlang)

# Compute cluster centroids and summary statistics
compute_cluster_centroids <- function(points) {
  points |>
    dplyr::group_by(.data$cluster) |>
    dplyr::summarise(
      centroid = sf::st_centroid(sf::st_union(.data$geometry)),
      n_accidents = dplyr::n(),
      prefecture_code = names(which.max(table(.data$prefecture)))
    ) |>
    sf::st_as_sf()
}

# Load and preprocess Japan's prefectural boundaries
get_prefecture_boundaries <- function() {
  # Temporarily disable S2 geometry to avoid potential issues with coordinate
  # transforms
  sf::sf_use_s2(FALSE)

  # Create prefecture boundaries
  library(jpndistrict)
  prefectures <- 1:47 |>
    purrr::map(jpndistrict::jpn_pref, district = FALSE) |>
    purrr::reduce(dplyr::bind_rows) |>
    sf::st_transform(crs = 3395) |>
    sf::st_simplify(dTolerance = 3000)

  # Re-enable S2 geometry
  sf::sf_use_s2(TRUE)

  prefectures
}

# Generate cluster visualization maps
create_cluster_visualizations <- function(cluster_centroids) {
  interactive_map <- mapgl::maplibre(bounds = cluster_centroids) |>
    mapgl::add_circle_layer(
      id = "cluster-centroids",
      source = cluster_centroids,
      circle_radius = 3,
      circle_color = "#db0a00",
      circle_opacity = 0.5
    )

  static_map <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = get_prefecture_boundaries(),
      fill = "gray",
      colour = "white",
      linewidth = 0.3
    ) +
    ggplot2::geom_sf(
      data = cluster_centroids,
      colour = "#db0a00"
    ) +
    ggplot2::theme_void()

  list(
    interactive_map = interactive_map,
    static_map = static_map
  )
}

# Save ggplot visualization to file
save_cluster_plot <- function(plot, filename) {
  plot_dir <- Sys.getenv("PLOT_DIR")
  ggplot2::ggsave(
    plot = plot,
    filename = file.path(plot_dir, filename),
    width = 1000,
    height = 800,
    scale = 4,
    units = "px"
  )
}

# Tabulate the number of clusters by prefecture
tabulate_clusters <- function(cluster_centroids) {
  cluster_centroids |>
    dplyr::group_by(.data$cluster) |>
    dplyr::pull("prefecture_code") |>
    table() |>
    as.data.frame() |>
    dplyr::rename(prefecture = "Var1", count = "Freq") |>
    dplyr::arrange(dplyr::desc(.data$count))
}

# Get the directory for intermediate data from the environment variable
intermediate_dir <- Sys.getenv("INTERMEDIATE_DATA_DIR")

# Load clustered points data
clustered_points <- readr::read_rds(
  file.path(intermediate_dir, "clustered_points.rds")
)
curve_clustered_points <- readr::read_rds(
  file.path(intermediate_dir, "curve_clustered_points.rds")
)

# Compute centroids for each cluster
cluster_centroids <- compute_cluster_centroids(clustered_points)
curve_cluster_centroids <- compute_cluster_centroids(curve_clustered_points)

# Tabulate results
head(tabulate_clusters(cluster_centroids), n = 10)
head(tabulate_clusters(curve_cluster_centroids), n = 10)

# Create visualization maps
cluster_vis <- create_cluster_visualizations(cluster_centroids)
curve_cluster_vis <- create_cluster_visualizations(curve_cluster_centroids)

# Save cluster centroids data
readr::write_rds(
  cluster_centroids,
  file.path(intermediate_dir, "cluster_centroids.rds")
)

# Save static visualizations
save_cluster_plot(
  cluster_vis$static_map,
  "headon_collision_clusters.pdf"
)
save_cluster_plot(
  curve_cluster_vis$static_map,
  "curve_headon_collision_clusters.pdf"
)
