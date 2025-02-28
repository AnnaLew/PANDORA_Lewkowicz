# Load necessary libraries
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ggrepel)

# Load the first file
elements_refno <- read.csv2("ElementsRefNo.csv", stringsAsFactors = FALSE)

# Load the second file
refno_ool_doctype <- read.csv2("RefNoOoLCategoryDocType.csv", stringsAsFactors = FALSE)

# View the first few rows
head(elements_refno)
head(refno_ool_doctype)

# Convert Elements column to character type
elements_refno$Elements <- as.character(elements_refno$Elements)

# Split elements into separate rows
elements_refno_long <- elements_refno %>%
  separate_rows(Elements, sep = ", ") %>%  # Split elements by comma
  mutate(Elements = trimws(Elements))      # Remove leading/trailing spaces

# Convert Elements column to character type to avoid type mismatch
elements_refno_long$Elements <- as.character(elements_refno_long$Elements)

# Count occurrences of each element per RefNo
element_counts <- elements_refno_long %>%
  group_by(RefNo, Elements) %>%
  summarise(Count = n(), .groups = "drop")

# Convert to wide format (pivot table)
elements_final <- element_counts %>%
  pivot_wider(names_from = Elements, values_from = Count, values_fill = 0)  # Fill missing counts with 0

# Save the table
write.csv(elements_final, "elements_final.csv", row.names = FALSE)

# Set RefNo as row names
elements_final <- elements_final %>%
  column_to_rownames(var = "RefNo")

# View the result
head(elements_final)






### ALL ELEMENTS PIECHART ###

# Calculate the total sum for each element (excluding RefNo)
total_elements <- colSums(elements_final)

# Convert the result to a data frame for better readability
total_elements_df <- as.data.frame(total_elements)

# Move rownames into a new column called "Element"
total_elements_df <- total_elements_df %>%
  rownames_to_column(var = "Element")

# Rename the column to "Total_Count"
colnames(total_elements_df) <- c("Element", "Total_Count")

# Create a new column for percentage
total_elements_df$Percentage <- (total_elements_df$Total_Count / sum(total_elements_df$Total_Count)) * 100

# Sort elements by Total_Count in descending order
total_elements_df <- total_elements_df %>%
  arrange(desc(Total_Count))

# Select top 10 elements
top_10 <- total_elements_df[1:10, ]

# Sum the remaining elements into "Other Elements"
other_elements <- sum(total_elements_df$Total_Count[11:nrow(total_elements_df)])

# Create a new dataframe for "Other Elements" with the correct columns
other_elements_df <- data.frame(
  Element = "Other Elements",
  Total_Count = other_elements,
  Percentage = (other_elements / sum(total_elements_df$Total_Count)) * 100
)

# Bind the data correctly
top_10 <- rbind(top_10, other_elements_df)

# Recalculate percentages to ensure everything sums to 100%
top_10$Percentage <- (top_10$Total_Count / sum(top_10$Total_Count)) * 100

# Sort the data by Percentage in descending order
top_10 <- top_10 %>%
  arrange(desc(Percentage)) %>%
  mutate(Element = factor(Element, levels = Element)) 

# # Create the pie chart
# ggplot(top_10, aes(x = "", y = Percentage, fill = Element)) +
#   geom_bar(stat = "identity", width = 1) +
#   coord_polar("y", start = 0) + 
#   theme_void() +  # Removes background and axis for a clean pie chart
#   labs(title = "Top 10 Elements + Other Elements") +
#   theme(legend.title = element_blank())

# Define specific colors for each element
element_colors <- c(
  "O" = "#0197F6",   
  "Si" = "#E0E0E2",  
  "Fe" = "#B7410E",  
  "Mg" = "#FF0000",  
  "Ca" = "#FAF2DF",  
  "H" = "#EC9A29",  
  "Al" = "#999999",  
  "C" = "#636363",  
  "Na" = "#EFFD5F",  
  "K" = "#32CD32",  
  "S" = "#FFD700",  
  "P" = "#008080",  
  "Ni" = "#8B0000",  
  "F" = "#00FF7F",  
  "Ti" = "#4682B4",  
  "Cl" = "#6A3D9A", 
  "Ba" = "#FF6347",  
  "Cr" = "#800080",  
  "Mn" = "#708090",  
  "Th" = "#B22222",  
  "U" = "#FF69B4",   
  "Co" = "#DC143C",  
  "Zn" = "#00CED1",  
  "Ce" = "#FFDAB9",  
  "F" = "#FF00FF",  
  "La" = "#9370DB",  
  "Nd" = "#DAA520",  
  "Sr" = "#ADFF2F",  
  "Zr" = "#D2691E",  
  "Cu" = "#CD5C5C",  
  "Other Elements" = "#893168"  
)

# Create a pie chart with fixed colors
ggplot(top_10, aes(x = "", y = Percentage, fill = Element)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +  
  theme_void() +  
  labs(title = "Top 10 Elements + Other Elements") + 
  theme(legend.title = element_blank()) +
  
  # Add repelled labels for better readability
  geom_label_repel(aes(label = paste0(Element, ": ", round(Percentage, 1), "%")), 
                   position = position_stack(vjust = 0.5), 
                   size = 5, box.padding = 0.5) +
  scale_fill_manual(values = element_colors) +
  guides(fill = guide_legend(title = "Element", override.aes = list(label = "")))



# Save as PNG
ggsave("top_10.png", width = 8, height = 6, dpi = 300)







### ALL METALS PIECHART ###

# Remove non-metal columns
metals_final <- elements_final %>%
  select(-C, -O, -H)

# Calculate the total sum for each element (excluding RefNo)
total_metals <- colSums(metals_final)

# Convert the result to a data frame for better readability
total_metals_df <- as.data.frame(total_metals)

# Move rownames into a new column called "Element"
total_metals_df <- total_metals_df %>%
  rownames_to_column(var = "Element")

# Rename the column to "Total_Count"
colnames(total_metals_df) <- c("Element", "Total_Count")

# Create a new column for percentage
total_metals_df$Percentage <- (total_metals_df$Total_Count / sum(total_metals_df$Total_Count)) * 100

# Sort elements by Total_Count in descending order
total_metals_df <- total_metals_df %>%
  arrange(desc(Total_Count))

# Select top 10 elements
top_10_metals <- total_metals_df[1:10, ]

# Sum the remaining elements into "Other Elements"
other_metals <- sum(total_metals_df$Total_Count[11:nrow(total_metals_df)])

# Create a new dataframe for "Other Elements" with the correct columns
other_metals_df <- data.frame(
  Element = "Other Elements",
  Total_Count = other_metals,
  Percentage = (other_metals / sum(total_metals_df$Total_Count)) * 100
)

# Bind the data correctly
top_10_metals <- rbind(top_10_metals, other_metals_df)

# Recalculate percentages to ensure everything sums to 100%
top_10_metals$Percentage <- (top_10_metals$Total_Count / sum(top_10_metals$Total_Count)) * 100

# Sort the data by Percentage in descending order
top_10_metals <- top_10_metals %>%
  arrange(desc(Percentage)) %>%
  mutate(Element = factor(Element, levels = Element)) 

# # Create the pie chart
# ggplot(top_10_metals, aes(x = "", y = Percentage, fill = Element)) +
#   geom_bar(stat = "identity", width = 1) +
#   coord_polar("y", start = 0) + 
#   theme_void() +  # Removes background and axis for a clean pie chart
#   labs(title = "Top 10 Elements (C, O, H excluded) + Other Elements") +
#   theme(legend.title = element_blank()) 

top_10_metals$Element <- as.character(top_10_metals$Element)

# Pie chart with labels
ggplot(top_10_metals, aes(x = "", y = Percentage, fill = Element)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +  
  theme_void() +  
  labs(title = "Top 10 Elements + Other Elements") + 
  theme(legend.title = element_blank()) +
  
  # Add repelled labels for better readability
  geom_label_repel(aes(label = paste0(Element, ": ", round(Percentage, 1), "%")), 
                   position = position_stack(vjust = 0.5), 
                   size = 5, box.padding = 0.5) +
  scale_fill_manual(values = element_colors) +
  guides(fill = guide_legend(title = "Element", override.aes = list(label = "")))


# Save as PNG
ggsave("top_10_metals.png", width = 8, height = 6, dpi = 300)






## TODO

# Plots of contribution of each element per category

