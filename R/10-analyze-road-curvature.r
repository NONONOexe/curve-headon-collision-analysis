# This script analyzes road curvature in relation to head-on collisions.
# It processes curvature data, creates visualization, and performs statistical analysis.
#
# INPUTS:
#   - {INTERMEDIATE_DATA_DIR}/road_curvatures.rds: Road curvature calculations
#   - {INTERMEDIATE_DATA_DIR}/headon_collisions.rds: Head-on collision data
#
# OUTPUTS:
#   - {PLOT_DIR}/road_curvature_density.pdf: Density plot of road curvature
#

# Load sf package to handle spatial vector data classes
library(sf)

# Process curvature data for analysis
prepare_curvature_for_analysis <- function(headon_collisions, road_curvatures) {
  # Filter out infinite curvature values
  curve_roads <- road_curvatures |>
    dplyr::filter(curvature != Inf)
  
  # Define key columns for joining datasets
  accident_data_key_columns <- c(
    "document_type",
    "report_year",
    "prefecture",
    "police_code",
    "report_number"
  )

  # Extract key information from head-on collisions
  headon_collisions_keyset <- headon_collisions |>
    purrr::pluck("accident_info") |>
    dplyr::select(dplyr::all_of(accident_data_key_columns))

  # Identify roads with head-on collisions
  curve_roads_headon <- curve_roads |>
    tibble::as_tibble() |>
    dplyr::semi_join(
      headon_collisions_keyset,
      by = accident_data_key_columns
    ) |>
    dplyr::mutate(headon = 1L)

  # Identify roads with other types of collisions
  curve_roads_others <- curve_roads |>
    tibble::as_tibble() |>
    dplyr::anti_join(
      headon_collisions_keyset,
      by = accident_data_key_columns
    ) |>
    dplyr::mutate(headon = 0L)
  
  # Combine and select relevant columns
  headon_labeled_curvatures <- dplyr::bind_rows(
    curve_roads_headon,
    curve_roads_others
  ) |>
  dplyr::select(
    dplyr::all_of(accident_data_key_columns),
    curvature,
    headon,
    nearest_road,
    accident_point
  )

  return(headon_labeled_curvatures)
}

# Create density plot of curvature data
create_curvature_plot <- function(curvature_data) {
  ggplot2::ggplot() +
    ggplot2::geom_density(
      data = curvature_data,
      mapping = ggplot2::aes(x = curvature, fill = factor(headon)),
      alpha = .5
    ) +
    ggplot2::scale_fill_manual(
      values = c("#999999", "#db0900"),
      labels = c("Other", "Head-on")
    ) +
    ggplot2::geom_vline(
      data = curvature_data |>
          dplyr::group_by(headon = factor(headon)) |>
          dplyr::summarize(median = median(curvature)),
      mapping = ggplot2::aes(xintercept = median),
      colour = "black",
      linetype = "dashed"
    ) +
    ggplot2::theme_bw(base_size = ggplot2::unit(8.5, "pt")) +
    ggplot2::scale_x_sqrt() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        linetype = "dashed",
        colour = "black",
        linewidth = .2
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", linewidth = .5),
      axis.line = ggplot2::element_line(color = "black", linewidth = .5)
    )
}

# Perform statistical analysis on curvature data
analyze_curvature_data <- function(curvature_data) {
  # Extract and summarize head-on collision curvature data
  cat("Head-on collisions curvature summary:\n")
  headon_curvature <- curvature_data |>
    dplyr::filter(headon == 1L) |>
    dplyr::pull(curvature)
  print(summary(headon_curvature))
  cat("SD:", stats::sd(headon_curvature), "\n\n")
  
  # Extract and summarize other collision curvature data
  cat("Other collisions curvature summary:\n")
  others_curvature <- curvature_data |>
    dplyr::filter(headon == 0L) |>
    dplyr::pull(curvature)
  print(summary(others_curvature))
  cat("SD:", stats::sd(others_curvature), "\n\n")
  
  # Perform Wilcoxon rank sum test
  result <- stats::wilcox.test(headon_curvature, others_curvature)
  print(result)
  
  # Calculate effect size
  n_headon <- length(headon_curvature)
  n_others <- length(others_curvature)
  r <- sqrt(result$statistic / (n_headon * n_others))
  cat("Effect size (r):", round(r, 3), "\n")
}

# Get data directories from environment variables
intermediate_dir <- Sys.getenv("INTERMEDIATE_DATA_DIR")
plot_dir <- Sys.getenv("PLOT_DIR")

# Load required data
road_curvatures <- readr::read_rds(file.path(intermediate_dir, "road_curvatures.rds"))
headon_collisions <- readr::read_rds(file.path(intermediate_dir, "headon_collisions.rds"))

# Process curvature data
curvature_data <- prepare_curvature_for_analysis(headon_collisions, road_curvatures)

# Create and save plot
plot <- create_curvature_plot(curvature_data)
ggplot2::ggsave(
  plot = plot,
  filename = file.path(plot_dir, "road_curvature_density.pdf"),
  width = 1618,
  height = 1000,
  scale = 1,
  units = "px"
)

# Perform statistical analysis
analyze_curvature_data(curvature_data)
