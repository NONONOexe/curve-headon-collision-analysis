# Get the directory for intermediate data from the environment variable
intermediate_dir <- Sys.getenv("INTERMEDIATE_DATA_DIR")

# Define the path to the accident data and read the data
accident_data_path <- readRDS(file.path(intermediate_dir, "01-accident_data_path.rds"))

# Load accident data using the jpaccidents package function
accident_data <- jpaccidents::read_accident_data(accident_data_path)

# Save the processed accident data for further use
saveRDS(
  accident_data,
  file = file.path(intermediate_dir, "02-accident_data.rds")
)
