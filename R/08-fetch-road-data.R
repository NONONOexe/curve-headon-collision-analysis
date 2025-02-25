# This script fetches road data around curve accident locations using OpenStreetMap.
# The data is processed in batches to allow partial re-execution in case of I/O errors
# due to the use of network-based data. This script is also time-consuming to execute.
#
# INPUTS:
#   - {INTERMEDIATE_DATA_DIR}/accident_data.rds: Accident data
#
# OUTPUTS:
#   - {INTERMEDIATE_DATA_DIR}/around_roads.rds: Road data around curve accidents
#

# Load sf package to handle spatial vector data classes
library(sf)

# Fetch road data for a batch of accident points
fetch_road_data <- function(batch) {
  batch |>
    dplyr::mutate(
      road_data = purrr::map(
        geometry,
        pavement::fetch_roads,
        radius = 20,
        .progress = TRUE
      )
    )
}

# Combine batch results and filter valid records
combine_road_data <- function(data_paths) {
  data_paths |>
    purrr::map(readr::read_rds) |>
    purrr::reduce(dplyr::bind_rows) |>
    dplyr::select(
      document_type,
      report_year,
      prefecture,
      police_code,
      report_number,
      road_data
    ) |>
    dplyr::mutate(road_count = purrr::map_int(road_data, nrow)) |>
    dplyr::filter(0 < road_count)
}

# Get the directory for intermediate data
intermediate_dir <- Sys.getenv("INTERMEDIATE_DATA_DIR")

# Read and filter accident data for curves
accident_data <- readr::read_rds(file.path(intermediate_dir, "accident_data.rds"))
curve_accidents <- accident_data |>
  jpaccidents::filter_accident_type("vehicle_to_vehicle") |>
  jpaccidents::filter_road_shape("curve")

# Extract accident points and split into batches
accident_points <- curve_accidents |>
  purrr::pluck("accident_info")

split_data <- accident_points |>
  # dplyr::mutate(group = (dplyr::row_number() - 1) %/% 1000) |>
  dplyr::mutate(group = (dplyr::row_number() - 1) %/% 1) |>
  dplyr::group_split(group, .keep = FALSE)

# Process data batches using purrr
split_data |>
  purrr::iwalk(function(batch, idx) {
    cli::cli_alert_info("Processing group: {idx}/{length(split_data)}")

    result <- try({
      around_roads <- fetch_road_data(batch)
      file_path <- file.path(intermediate_dir, sprintf("around_roads-%02d.rds", idx))
      readr::write_rds(around_roads, file_path)
    }, silent = TRUE)

    if (inherits(result, "try-error")) {
      cli::cli_alert_warning("Error in group {idx}, skipping...")
    }
  })

# Combine and save final results
data_paths <- fs::dir_ls(intermediate_dir, type = "file", regexp = "around_roads-\\d{2}[.]rds") |>
  sort()
around_roads <- combine_road_data(data_paths)
readr::write_rds(around_roads, file.path(intermediate_dir, "around_roads.rds"))

# Clean up temporary files
fs::dir_ls(intermediate_dir, regexp = "around_roads-\\d{2}[.]rds$") |>
  fs::file_delete()
