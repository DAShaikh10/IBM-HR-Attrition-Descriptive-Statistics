# ------------------------------------------------------------------------------
# IPL Data Loading Script
# Data Source: Kaggle - IPL Dataset (2008 - 2025)
# URL: https://www.kaggle.com/datasets/chaitu20/ipl-dataset2008-2025/data
# ------------------------------------------------------------------------------

# Clean the session state.
rm(list = ls(all.names = TRUE))

# ------------------------------------------------------------------------------
#                                 LOAD DATASET
# ------------------------------------------------------------------------------

# For ease, we expect the data to be present at the same hierarcy as the script. 
file_path <- 'IPL.csv'
ipl_data <- read.csv(file_path)

# As mentioned in the document we exclude certain columns.
excluded_col_names = c(
  "event_name",
  "gender",
  "match_number",
  "match_type",
  "team_type")

# Column exclusion ref.: https://stackoverflow.com/a/34427840
ipl_data <- ipl_data[, !(names(ipl_data) %in% excluded_col_names)]

# ------------------------------------------------------------------------------
#                              BASIC INFORMATION                       
# ------------------------------------------------------------------------------

# Data dimensions.
print(paste("Total number of observations:", nrow(ipl_data)))
print(paste("Total number of columns:", ncol(ipl_data)))

# Print the first few rows. 
print(head(ipl_data))

# Print the dataset summary.
print(summary(ipl_data))
