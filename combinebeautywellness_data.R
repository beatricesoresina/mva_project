# Load necessary libraries
install.packages("readxl")
install.packages("writexl")
library(readxl)
library(writexl) # optional, mainly for writing back to Excel if needed

# CSV Set the working directory to the folder containing your Excel files
setwd("/Users/beatricesoresina/Desktop/mva_project/beauty")

# List all Excel files (.xlsx or .xls)
files <- list.files(pattern = "\\.xlsx?$")

# Create output folder if it doesn't exist
output_dir <- "/Users/beatricesoresina/Desktop/mva_project/beauty_csv"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Loop through each file
for (file in files) {
  # Read the Excel file (assuming the first sheet)
  data <- read_excel(file)
  
  # Extract the first 2 characters from the filename
  new_name_base <- substr(file, 1, 2)
  
  # Create the new filename
  new_filename <- paste0(new_name_base, "_Hair.csv")
  
  # Save the data as CSV into the output folder
  write.csv(data, file = file.path(output_dir, new_filename), row.names = FALSE)
  
  cat("Converted:", file, "to", new_filename, "\n")
}

cat("All files converted successfully into beauty_csv folder!")

# CSV WELLNESS  Set the working directory to the 'wellness' folder
setwd("/Users/beatricesoresina/Desktop/mva_project/wellness")

# List all Excel files (.xlsx or .xls)
files <- list.files(pattern = "\\.xlsx?$")

# Create output folder if it doesn't exist
output_dir <- "/Users/beatricesoresina/Desktop/mva_project/wellness_csv"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Loop through each file
for (file in files) {
  # Read the Excel file (assuming the first sheet)
  data <- read_excel(file)
  
  # Extract the first 2 characters from the filename
  new_name_base <- substr(file, 1, 2)
  
  # Create the new filename
  new_filename <- paste0(new_name_base, "_Wellness.csv")
  
  # Save the data as CSV into the output folder
  write.csv(data, file = file.path(output_dir, new_filename), row.names = FALSE)
  
  cat("Converted:", file, "to", new_filename, "\n")
}

cat("All files converted successfully into wellness_csv folder!")

# MERGE INTO CSV FILE INDUSTRY Load necessary libraries
install.packages("dplyr")
install.packages("readr")
install.packages("janitor")


library(dplyr)
library(readr)
library(janitor)  # NEW to clean column names easily!


# Define input folders
beauty_folder <- "/Users/beatricesoresina/Desktop/mva_project/beauty_csv"
wellness_folder <- "/Users/beatricesoresina/Desktop/mva_project/wellness_csv"

# List all files
beauty_files <- list.files(beauty_folder, pattern = "\\.csv$", full.names = TRUE)
wellness_files <- list.files(wellness_folder, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty dataframe to collect everything
combined_data <- data.frame()

# Loop through each Beauty file
for (beauty_file in beauty_files) {
  
  # Extract the state code from the filename (first two letters)
  state <- substr(basename(beauty_file), 1, 2)
  
  # Find the matching Wellness file
  matching_wellness_file <- file.path(wellness_folder, paste0(state, "_Wellness.csv"))
  
  # If matching wellness file exists
  if (file.exists(matching_wellness_file)) {
    
    # Read and clean both files
    beauty_data <- read_csv(beauty_file) %>% janitor::clean_names()
    wellness_data <- read_csv(matching_wellness_file) %>% janitor::clean_names()
    
    # Now column names are standardized: revenue_m, establishments_units, etc.
    
    # Rename beauty columns
    beauty_data <- beauty_data %>%
      rename(
        beauty_rev = revenue_m,
        beauty_est = establishments_units,
        beauty_ent = enterprises_units,
        beauty_emp = employment_units,
        beauty_wag = wages_m
      )
    
    # Rename wellness columns
    wellness_data <- wellness_data %>%
      rename(
        wellness_rev = revenue_m,
        wellness_est = establishments_units,
        wellness_ent = enterprises_units,
        wellness_emp = employment_units,
        wellness_wag = wages_m
      )
    
    # Merge beauty and wellness by year
    merged_data <- beauty_data %>%
      inner_join(wellness_data, by = "year")
    
    # Add state column
    merged_data <- merged_data %>%
      mutate(state = state) %>%
      select(state, everything())  # put state first
  }
  
  # Add to the master combined dataframe
  combined_data <- bind_rows(combined_data, merged_data)
}

# Save the final result
write_csv(combined_data, "/Users/beatricesoresina/Desktop/mva_project/beautywellness_combined_data.csv")

cat("Master CSV created successfully at /Users/beatricesoresina/Desktop/mva_project/master_combined_data.csv!")

# FILTER OUT PREDICTIONS Filter out the prediction years
combined_data_nopred <- combined_data %>%
  filter(!(year %in% c(2025, 2026, 2027, 2028, 2029)))

# Save the new filtered data
write_csv(combined_data_nopred, "/Users/beatricesoresina/Desktop/mva_project/beautywellness_combined_data_nopred.csv")

cat("Filtered dataset saved successfully at /Users/beatricesoresina/Desktop/mva_project/beautywellness_combined_data_nopred.csv!")
