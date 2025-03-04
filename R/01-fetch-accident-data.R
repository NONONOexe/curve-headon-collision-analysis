# This script downloads traffic accident data for the years 2019 to 2023.
#
# OUTPUTS:
#   {INTERMEDIATE_DATA_DIR}/accident_data_path.rds: File paths to the downloaded
#   traffic accident data files
#

# Get the directory for raw data from the environment variable
download_dir <- Sys.getenv("RAW_DATA_DIR")

# Download traffic accident data the years 2019 to 2023
# The data is saved in the directory specified `download_dir`
accident_data_path <- jpaccidents::download_accident_data(
  type = "main",
  download_dir = download_dir,
  year = 2019:2023
)

# Get the directory for intermediate data from the environment variable
intermediate_dir <- Sys.getenv("INTERMEDIATE_DATA_DIR")

# Save the path to the downloaded accident data as an RDS file
saveRDS(
  accident_data_path,
  file = file.path(intermediate_dir, "accident_data_path.rds")
)
