# This script extracts head-on collision accidents from the accident data.
#
# INPUTS:
#   {INTERMEDIATE_DATA_DIR}/accident_data.rds: Cleaned accident data
#
# OUTPUTS:
#   {INTERMEDIATE_DATA_DIR}/headon_collisions.rds: Head-on collision accidents
#

# Get the directory for intermediate data from the environment variable
intermediate_dir <- Sys.getenv("INTERMEDIATE_DATA_DIR")

# Define the path to the accident data and read the data
accident_data <- readRDS(file.path(intermediate_dir, "accident_data.rds"))

# Filter only vehicle-to-vehicle accidents
vehicle_accidents <- accident_data |>
  jpaccidents::filter_accident_type("vehicle_to_vehicle")

# Define head-on collision patterns
collision_patterns <- list(
  c("front", "front"),
  c("front_right", "front"),
  c("front", "front_right"),
  c("front_right", "front_right")
)

# Extract head-on collisions based on patterns
headon_collisions <- collision_patterns |>
  purrr::map(
    jpaccidents::filter_collision_pattern,
    data = vehicle_accidents
  ) |>
  jpaccidents::merge_accident_data()

# Save the filtered data
saveRDS(
  headon_collisions,
  file = file.path(intermediate_dir, "headon_collisions.rds")
)
