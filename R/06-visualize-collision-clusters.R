# This script visualizes the clusters of head-on collisions.

# Load sf package to handle spatial vector data classes
library(sf)

# Compute cluster centroids and summary statistics
compute_cluster_centroids <- function(points) {
  points |>
    dplyr::group_by(cluster) |>
    dplyr::summarise(
      centroid        = sf::st_centroid(sf::st_union(geometry)),
      n_accidents     = dplyr::n(),
      prefecture_code = names(which.max(table(prefecture)))
    ) |>
    sf::st_as_sf()
}

# Generate cluster visualization maps
create_cluster_visualizations <- function(cluster_centroids, japan_prefectures) {
  interactive_map <- tmap::tm_view() +
    tmap::tm_shape(cluster_centroids) +
    tmap::tm_dots(size = 1, fill_alpha = 0.5, fill = "red")

  static_map <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data      = japan_prefectures,
      fill      = "gray",
      colour    = "white",
      linewidth = 0.3
    ) +
    ggplot2::geom_sf(
      data      = cluster_centroids,
      colour    = "#db0a00"
    ) +
    ggplot2::theme_void()

  list(
    interactive_map   = interactive_map,
    static_map        = static_map
  )
}

# Save ggplot visualization to file
save_cluster_plot <- function(plot, filename) {
  plot_dir <- Sys.getenv("PLOT_DIR")
  ggplot2::ggsave(
    plot     = plot,
    filename = file.path(plot_dir, filename),
    width    = 1000,
    height   = 800,
    scale    = 4,
    units    = "px"
  )
}

# Tabulate the number of clusters by prefecture
tabulate_clusters <- function(cluster_centroids) {
  cluster_centroids |>
    dplyr::group_by(cluster) |>
    dplyr::pull("prefecture_code") |>
    table() |>
    as.data.frame() |>
    dplyr::rename(prefecture = Var1, count = Freq) |>
    dplyr::arrange(dplyr::desc(count))
}

# Get the directory for intermediate data from the environment variable
intermediate_dir <- Sys.getenv("INTERMEDIATE_DATA_DIR")

# Load clustered points data
clustered_points <- readr::read_rds(
  file.path(intermediate_dir, "04-clustered_points.rds")
)
curve_clustered_points <- readr::read_rds(
  file.path(intermediate_dir, "05-curve_clustered_points.rds")
)

# Load and preprocess Japan's prefectural boundaries
library(jpndistrict)
sf::sf_use_s2(FALSE)
japan_prefectures <- 1:47 |>
  purrr::map(jpndistrict::jpn_pref, district = FALSE) |>
  purrr::reduce(dplyr::bind_rows) |>
  sf::st_transform(crs = 3395) |>
  sf::st_simplify(dTolerance = 3000)
sf::sf_use_s2(TRUE)

# Compute centroids for each cluster
cluster_centroids <- compute_cluster_centroids(
  clustered_points
)
curve_cluster_centroids <- compute_cluster_centroids(
  curve_clustered_points
)

# Create visualization maps
cluster_vis <- create_cluster_visualizations(
  cluster_centroids, japan_prefectures
)
curve_cluster_vis <- create_cluster_visualizations(
  curve_cluster_centroids, japan_prefectures
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

# Tabulate results
head(tabulate_clusters(cluster_centroids), n = 10)
head(tabulate_clusters(curve_cluster_centroids), n = 10)
