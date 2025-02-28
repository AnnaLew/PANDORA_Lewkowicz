# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Read elements file to get column names
elements_file <- "elements_final.csv"
elements_data <- read.csv(elements_file, header = TRUE)

# Extract element names (all columns except RefNo)
element_names <- setdiff(colnames(elements_data), "RefNo")

# Read the RefNoOoLCategoryDocType.csv file with ; as delimiter
ref_file <- "RefNoOoLCategoryDocType.csv"
ref_data <- read.csv(ref_file, sep = ";", header = TRUE)

# Merge datasets using RefNo
merged_data <- ref_data %>%
  inner_join(elements_data, by = "RefNo")  # Merge elements based on RefNo

# Convert wide to long format for element counts (now using merged data)
long_data <- merged_data %>%
  pivot_longer(cols = all_of(element_names), names_to = "Element", values_to = "Count")

# Remove rows where Count is NA or zero to clean the dataset
long_data <- long_data %>%
  filter(!is.na(Count) & Count > 0)

# Expand multiple categories (split by commas into separate rows)
expanded_data <- long_data %>%
  filter(!is.na(OoLCategory)) %>%
  mutate(OoLCategory = strsplit(as.character(OoLCategory), ", ")) %>%
  unnest(OoLCategory)  # Creates separate rows for each category

# Aggregate counts per Element and OoLCategory
category_counts <- expanded_data %>%
  group_by(Element, OoLCategory) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop")

# Calculate percentage share for each category in each element
category_percentages <- category_counts %>%
  group_by(Element) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  ungroup()

# Create and save separate plots for each element
output_dir <- "element_charts"  # Folder to store charts
dir.create(output_dir, showWarnings = FALSE)  # Create folder if it doesn't exist

for (elem in unique(category_percentages$Element)) {
  element_data <- category_percentages %>% filter(Element == elem)
  
  # Generate plot
  plot <- ggplot(element_data, aes(x = OoLCategory, y = Percentage, fill = OoLCategory)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = paste("Percentage Share of OoL Categories in", elem),
         x = "OoL Category",
         y = "Percentage",
         fill = "OoL Category") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for readability
  
  # Save plot
  filename <- paste0(output_dir, "/", elem, "_OoL_percentage.png")
  ggsave(filename, plot, width = 8, height = 6, dpi = 300)
  
  print(paste("Saved:", filename))  # Confirmation message
}



















# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Read elements file to get column names
elements_file <- "elements_final.csv"
elements_data <- read.csv(elements_file, header = TRUE)

# Extract element names (all columns except RefNo)
element_names <- setdiff(colnames(elements_data), "RefNo")

# Read the RefNoOoLCategoryDocType.csv file with ; as delimiter
ref_file <- "RefNoOoLCategoryDocType.csv"
ref_data <- read.csv(ref_file, sep = ";", header = TRUE)

# Merge datasets using RefNo
merged_data <- ref_data %>%
  inner_join(elements_data, by = "RefNo")  # Merge elements based on RefNo

# Convert wide to long format for element counts (now using merged data)
long_data <- merged_data %>%
  pivot_longer(cols = all_of(element_names), names_to = "Element", values_to = "Count")

# Remove rows where Count is NA or zero to clean the dataset
long_data <- long_data %>%
  filter(!is.na(Count) & Count > 0)

# Expand multiple categories (split by commas into separate rows)
expanded_data <- long_data %>%
  filter(!is.na(OoLCategory)) %>%
  mutate(OoLCategory = strsplit(as.character(OoLCategory), ", ")) %>%
  unnest(OoLCategory)  # Creates separate rows for each category

# Aggregate counts per Element and OoLCategory
category_counts <- expanded_data %>%
  group_by(Element, OoLCategory) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop")

# Calculate percentage share for each category in each element
category_percentages <- category_counts %>%
  group_by(Element) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  ungroup()

# Create and save stacked bar chart for each element
output_dir <- "element_charts"  # Folder to store charts
dir.create(output_dir, showWarnings = FALSE)  # Create folder if it doesn't exist

for (elem in unique(category_percentages$Element)) {
  element_data <- category_percentages %>% filter(Element == elem)
  
  # Generate **stacked** bar chart
  plot <- ggplot(element_data, aes(x = Element, y = Percentage, fill = OoLCategory)) +
    geom_bar(stat = "identity", position = "stack") +  # **Stacked bars**
    theme_minimal() +
    labs(title = paste("Stacked Percentage Share of OoL Categories in", elem),
         x = "Element",
         y = "Percentage",
         fill = "OoL Category") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability
  
  # Save plot
  filename <- paste0(output_dir, "/", elem, "_stacked_OoL_percentage.png")
  ggsave(filename, plot, width = 8, height = 6, dpi = 300)
  
  print(paste("Saved:", filename))  # Confirmation message
}














# Select the elements to include in the final plot
selected_elements <- c("O", "Si", "Fe", "Mg", "Ca", "H", "Al", "C", "Na", "K", "S", "P", "Ni")

# Filter data for the selected elements
filtered_data <- category_percentages %>% filter(Element %in% selected_elements)

# Generate **one stacked bar chart** with all selected elements
plot <- ggplot(filtered_data, aes(x = Element, y = Percentage, fill = OoLCategory)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bars for elements
  theme_minimal() +
  labs(title = "Stacked Percentage Share of OoL Categories for Selected Elements",
       x = "Element",
       y = "Percentage",
       fill = "OoL Category") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),  # Increase x-axis text size
        axis.text.y = element_text(size = 14),  # Increase y-axis text size
        axis.title.x = element_text(size = 16),  # Increase x-axis title size
        axis.title.y = element_text(size = 16),  # Increase y-axis title size
        legend.title = element_text(size = 14),  # Increase legend title size
        legend.text = element_text(size = 12),  # Increase legend text size
        plot.title = element_text(size = 18, face = "bold"))  # Increase title size
# Save the figure
filename <- "stacked_OoL_all_selected_elements.png"
ggsave(filename, plot, width = 12, height = 8, dpi = 300)

print(paste("Saved:", filename)) 
