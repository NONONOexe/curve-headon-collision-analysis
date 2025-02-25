# This script calculates road curvature for accident locations.
# It processes nearest roads to accident points and computes curvature metrics.
#
# INPUTS:
#   {INTERMEDIATE_DATA_DIR}/around_roads.rds: Road data around accidents
#
# OUTPUTS:
#   {INTERMEDIATE_DATA_DIR}/road_curvatures.rds: Road curvature calculations
#

# Load sf package to handle spatial vector data classes
library(sf)

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

# Function to crop road curve
crop_curve <- function(road, point, path_distance = 50) {
  # Find the nearest point on the road from the given point
  closest_point <- sf::st_nearest_points(point, road) |>
    sf::st_cast("POINT") |>
    purrr::pluck(2) |>
    sf::st_sfc(crs = 4326)
  
  # Get sampling points along the road
  sampled_points <- get_sampling_points(road, point, closest_point, path_distance)
  
  # Split the road at the sampled points
  split_roads <- pavement::split_linestring(road, sampled_points[c(1, 3)])
  
  # Return the middle segment if available, otherwise return the original road
  if (length(split_roads) == 3) {
    return(split_roads[2])
  } else {
    return(road)
  }
}

# Function to calculate curvature
calc_curvature <- function(road, point, path_distance = 50) {
  # Find the nearest point on the road from the given point
  closest_point <- sf::st_nearest_points(point, road) |>
    sf::st_cast("POINT") |>
    purrr::pluck(2) |>
    sf::st_sfc(crs = 4326)
  
  # Get sampling points along the road
  sampled_points <- get_sampling_points(road, point, closest_point, path_distance)
  
  # Create a convex hull from the sampled points to form a triangle
  triangle <- sampled_points |>
    sf::st_union() |>
    sf::st_convex_hull()
  
  # Calculate the area of the triangle
  area <- sf::st_area(triangle)
  
  # Compute pairwise distances between the sampled points
  pairwise_distances <- sf::st_distance(sampled_points)
  
  # Calculate curvature using the triangle's area and distances
  curvature <- 4 *
    area /
    (pairwise_distances[1, 2] *
      pairwise_distances[2, 3] *
      pairwise_distances[3, 1])
  
  return(curvature)
}

# Process nearest roads from around_roads data
process_nearest_roads <- function(around_roads) {
  around_roads |>
    dplyr::mutate(
      accident_point = purrr::map(geometry, sf::st_sfc, crs = 4326),
      nearest_road = purrr::map2(
        accident_point,
        road_data,
        \(point, roads) {
          point_sfc <- sf::st_sfc(point, crs = 4326)
          nearest_road_idx <- sf::st_nearest_feature(point_sfc, roads)
          roads[nearest_road_idx, ]$geometry
        }
      ),
      road_length = purrr::map(nearest_road, sf::st_length)
    )
}

# Calculate road curvatures
calculate_road_curvatures <- function(nearest_roads) {
  safe_calc_curvature <- purrr::possibly(calc_curvature, otherwise = NA_real_)
  
  nearest_roads |>
    dplyr::mutate(
      curvature = purrr::map2_dbl(
        nearest_road,
        accident_point,
        safe_calc_curvature,
        .progress = TRUE
      )
    )
}

# Get the directory for intermediate data from the environment variable
intermediate_dir <- Sys.getenv("INTERMEDIATE_DATA_DIR")

# Read road data
around_roads <- readr::read_rds(file.path(intermediate_dir, "around_roads.rds"))

# Process nearest roads
nearest_roads <- process_nearest_roads(around_roads)

# Calculate road curvatures
road_curvatures <- calculate_road_curvatures(nearest_roads)

# Save road curvatures
readr::write_rds(road_curvatures, file.path(intermediate_dir, "road_curvatures.rds"))
