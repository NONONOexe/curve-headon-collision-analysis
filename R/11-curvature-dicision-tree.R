# This script creates a decision tree model to predict head-on collisions
# based on road curvature.
#
# INPUTS:
#   - {INTERMEDIATE_DATA_DIR}/accident_data.rds: Accident data
#   - {INTERMEDIATE_DATA_DIR}/curvature_data.rds: Curvature data
#
# OUTPUTS:
#   - {PLOT_DIR}/variable_importance.pdf: Variable importance plot
#   - {PLOT_DIR}/high_curvature_road.pdf: Road with curvature 0.03986
#   - {PLOT_DIR}/low_curvature_road.pdf: Road with curvature 0.0104
#

# Load sf package to handle spatial vector data classes
library(sf)

# Load rlang for .data pronoun
library(rlang)

# Prepare data for modeling
prepare_model_data <- function(curvature_data, accident_data) {
  # Define key columns for joining datasets
  accident_key_columns <- c(
    "document_type",
    "report_year",
    "prefecture",
    "police_code",
    "report_number"
  )

  # Join datasets and select variables
  curvature_data |>
    dplyr::inner_join(
      accident_data$accident_info,
      by = accident_key_columns
    ) |>
    dplyr::inner_join(
      accident_data$person_info,
      by = accident_key_columns
    ) |>
    dplyr::filter(party_order == 1) |>
    dplyr::select(
      headon,
      day_night,
      weather,
      region_type,
      center_divider,
      road_verge,
      age,
      day_of_week,
      holiday,
      curvature,
      car_type
    ) |>
    dplyr::mutate(
      headon = factor(
        headon,
        levels = c("0", "1"),
        labels = c("not-headon", "headon")
      ),
      dplyr::across(where(is.character), as.factor)
    ) |>
    sf::st_drop_geometry()
}

# Create variable importance plot
create_importance_plot <- function(decision_tree) {
  # Calculate variable importance
  variable_importance <- tibble::tibble(
    variable = names(decision_tree$variable.importance) |>
      forcats::fct_reorder(decision_tree$variable.importance),
    importance = decision_tree$variable.importance
  )

  # Create plot
  ggplot2::ggplot(variable_importance) +
    ggplot2::geom_bar(
      ggplot2::aes(x = importance, y = variable),
      stat = "identity",
      width = 0.6,
      fill = "#101010",
      linewidth = 0.35
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        linetype = "dashed",
        colour = "black",
        linewidth = 0.2
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(
        colour = "black",
        linewidth = 0.2
      ),
      axis.ticks.x = ggplot2::element_line(
        colour = "black",
        linewidth = 0.2
      ),
      axis.ticks.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        colour = "black",
        linewidth = 0.2
      )
    )
}

# Function to get sampling points along the road
get_sampling_points <- function(road, point, closest_point, path_distance) {
  # Split the road at the nearest point
  split_roads <- pavement::split_linestring(road, closest_point)

  # Calculate distances for sampling along the road
  to_closest_point <- as.double(sf::st_length(split_roads[1, ]))
  road_length <- as.double(sf::st_length(road))

  # Define sampling offsets
  sample_offsets <- c(path_distance, 0, -path_distance)
  sample_ratio <- (sample_offsets + to_closest_point) / road_length

  # Sample points along the road at the computed positions
  road |>
    sf::st_transform(crs = 3395) |>
    sf::st_line_sample(sample = sample_ratio) |>
    sf::st_transform(crs = 4326) |>
    sf::st_cast("POINT")
}

# Function to crop the road curve
crop_curve <- function(road, point, path_distance = 50) {
  # Find the nearest point on the road from the given point
  closest_point <- sf::st_nearest_points(point, road) |>
    sf::st_cast("POINT") |>
    purrr::pluck(2) |>
    sf::st_sfc(crs = 4326)

  # Get sampling points along the road
  sampled_points <- get_sampling_points(
    road,
    point,
    closest_point,
    path_distance
  )

  # Split the road at the sampled points
  split_roads <- pavement::split_linestring(road, sampled_points[c(1, 3)])

  # Return the middle segment if available, otherwise return the original road
  if (length(split_roads) == 3) {
    split_roads[2]
  } else {
    road
  }
}

# Extract road data based on curvature threshold
extract_road_data <- function(curvature_data, threshold, slice_num) {
  nearest_data <- curvature_data |>
    dplyr::filter(
      headon == 1,
      threshold < curvature
    ) |>
    dplyr::arrange(abs(threshold - curvature)) |>
    dplyr::slice(slice_num)

  nearest_road <- nearest_data$nearest_road[[1]]
  accident_point <- nearest_data$accident_point[[1]]
  cropped_road <- crop_curve(nearest_road, accident_point)

  list(
    road = nearest_road,
    point = accident_point,
    cropped_road = cropped_road,
    curvature = nearest_data$curvature[[1]]
  )
}

# Create road plot
create_road_plot <- function(road_data) {
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = road_data$cropped_road,
      colour = "#aeaeae",
      linewidth = 5
    ) +
    ggplot2::geom_sf(
      data = road_data$point,
      colour = "#ff3333",
      size = 3
    ) +
    ggplot2::theme_void()
}

# Get directories from environment variables
intermediate_dir <- Sys.getenv("INTERMEDIATE_DATA_DIR")
plot_dir <- Sys.getenv("PLOT_DIR")

# Load data
accident_data <- readr::read_rds(
  file.path(intermediate_dir, "accident_data.rds")
)
curvature_data <- readr::read_rds(
  file.path(intermediate_dir, "curvature_data.rds")
)

# Prepare model data
model_data <- prepare_model_data(curvature_data, accident_data)

# Build decision tree model
decision_tree <- rpart::rpart(
  headon ~ .,
  data = model_data,
  method = "class",
  control = rpart::rpart.control(cp = 0.0015)
)

# Plot decision tree
rpart.plot::rpart.plot(
  decision_tree,
  extra = 2,
  type = 4,
  cex = 0.5,
  digit = 4
)

# Create and save variable importance plot
importance_plot <- create_importance_plot(decision_tree)
ggplot2::ggsave(
  plot = importance_plot,
  filename = file.path(plot_dir, "variable_importance.pdf"),
  width = 1618,
  height = 1000,
  scale = 1,
  units = "px"
)

# Create and save road plots for different curvatures
road_data_high <- extract_road_data(curvature_data, 0.03986, 4)
road_plot_high <- create_road_plot(road_data_high)
ggplot2::ggsave(
  plot = road_plot_high,
  filename = file.path(plot_dir, "high_curvature_road.pdf"),
  width = 500,
  height = 500,
  scale = 2,
  units = "px"
)

road_data_low <- extract_road_data(curvature_data, 0.0104, 1)
road_plot_low <- create_road_plot(road_data_low)
ggplot2::ggsave(
  plot = road_plot_low,
  filename = file.path(plot_dir, "low_curvature_road.pdf"),
  width = 500,
  height = 500,
  scale = 2,
  units = "px"
)
