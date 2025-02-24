# This script cleans the accident data and saves it as an RDS file.
#
# INPUTS:
#   {INTERMEDIATE_DATA_DIR}/accident_data_path.rds: File paths to the downloaded
#   traffic accident data files
#
# OUTPUTS:
#   {INTERMEDIATE_DATA_DIR}/accident_data.rds: Cleaned accident data
#

# Get the directory for intermediate data from the environment variable
intermediate_dir <- Sys.getenv("INTERMEDIATE_DATA_DIR")

# Read the accident data path
accident_data_path <- readRDS(
  file.path(intermediate_dir, "accident_data_path.rds")
)

# Load accident data using the jpaccidents package function
accident_data <- jpaccidents::read_accident_data(accident_data_path)

# Save the processed accident data for further use
saveRDS(
  accident_data,
  file = file.path(intermediate_dir, "accident_data.rds")
)
