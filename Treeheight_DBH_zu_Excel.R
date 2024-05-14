# Load the necessary packages
library(readxl)
library(dplyr)


# Specify the path to the folder containing the Excel files
folder_path <- "M:/Masterarbeit/TLS/3DFIN_xlsx/"

# Get the list of all Excel files in the folder
file_list <- list.files(path = folder_path, pattern = "*.xlsx", full.names = TRUE)

# Initialize an empty list to store data frames
all_data <- list()

# Loop through each file and read the data
for (file_path in file_list) {
  # Read a larger range that includes potential headers and data
  raw_data <- read_excel(file_path, sheet = "Plot Metrics", range = cell_limits(c(1, 2), c(NA, NA)))
  
  # Find the first row that contains the actual headers (usually row 2 in this case)
  header_row <- which(!is.na(raw_data[, 1]))[1]
  
  # Read the data again starting from the header row
  data <- read_excel(file_path, sheet = "Plot Metrics", range = cell_limits(c(header_row, 2), c(NA, NA)))
  
  # Append the data frame to the list
  all_data <- append(all_data, list(data))
}

# Combine all data frames into one large data frame
combined_data <- bind_rows(all_data)

cleaned_data <- combined_data %>%
  filter(!is.na(DBH) & DBH != 0)

# Display the first few rows of the combined data frame
head(cleaned_data)

# Print the number of trees in the combined data frame
paste("There is data for:", nrow(cleaned_data), "trees")


#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________

library(ggplot2)
library(openxlsx)


# Create a scatter plot of DBH against TH
ggplot(cleaned_data, aes(x = DBH, y = TH)) +
  geom_point(color = "darkgreen") +
  labs(title = "Scatter Plot of DBH vs. TH",
       x = "Diameter at Breast Height (DBH)",
       y = "Total Height (TH)") +
  theme_minimal()


summary(combined_data)


output_file_path <- "M:/Masterarbeit/TLS/3DFIN_xlsx/combined.xlsx"
write.xlsx(combined_data, output_file_path)


