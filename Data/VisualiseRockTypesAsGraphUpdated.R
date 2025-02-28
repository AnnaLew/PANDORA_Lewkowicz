library(DiagrammeR)
library(dplyr)
library(readr)
library(rsvg)

# Load data
data <- read_delim("All_rocks.csv", delim=';', col_types = cols(.default = "c"))

# Function to sanitize names and remove quotes
sanitize_name <- function(name) {
  name <- gsub('"', '', name)  # Remove quotes
  name <- gsub('\\s+', ' ', name)  # Replace multiple spaces with a single space
  trimws(name)  # Trim leading and trailing spaces
}

# Apply sanitization to all data
colnames(data) <- sanitize_name(colnames(data))
data <- data %>% mutate(across(everything(), ~sanitize_name(.)))

# Define colors for different columns
column_colors <- c("#ff9999", "#66b3ff", "#99ff99", "#ffcc99", "#c2c2f0", "#FFFF00")  # Assign colors to layers

# Function to generate graph script with color-coding
generate_graph_script <- function(rock_type, data) {
  filtered_data <- data %>% filter(.data[[colnames(data)[1]]] == rock_type)
  edges <- c()
  nodes <- c()
  
  for (row in 1:nrow(filtered_data)) {
    current_row <- filtered_data[row, ]
    
    for (i in 1:(ncol(filtered_data) - 1)) {
      parent <- current_row[[i]]
      
      if (!is.na(parent)) {
        # Assign color based on column index
        nodes <- c(nodes, paste0('"', parent, '" [style=filled, fillcolor="', column_colors[i], '"];'))
        
        for (j in (i + 1):ncol(filtered_data)) {
          child <- current_row[[j]]
          if (!is.na(child)) {
            nodes <- c(nodes, paste0('"', child, '" [style=filled, fillcolor="', column_colors[j], '"];'))
            edges <- c(edges, paste0('"', parent, '" -> "', child, '";'))
            break  # Ensure the first available non-NA column is linked correctly
          }
        }
      }
    }
  }
  
  paste("digraph rock_chart {", 
        "node [shape=box];", 
        "rankdir=LR;", 
        paste(nodes, collapse="\n"),
        paste(edges, collapse="\n"), 
        "}", sep="\n")
}

# Generate separate graphs for each rock type
igneous_graph <- generate_graph_script("Igneous", data)
sedimentary_graph <- generate_graph_script("Sedimentary", data)
metamorphic_graph <- generate_graph_script("Metamorphic", data)

# Render the flowcharts
cat("Igneous Rocks:\n")
DiagrammeR::grViz(igneous_graph)

cat("Sedimentary Rocks:\n")
DiagrammeR::grViz(sedimentary_graph)

cat("Metamorphic Rocks:\n")
DiagrammeR::grViz(metamorphic_graph)

# Convert and save Igneous Rocks graph
svg_igneous <- export_svg(DiagrammeR::grViz(igneous_graph))
rsvg_png(charToRaw(svg_igneous), "igneous_rocks.png")

# Convert and save Sedimentary Rocks graph
svg_sedimentary <- export_svg(DiagrammeR::grViz(sedimentary_graph))
rsvg_png(charToRaw(svg_sedimentary), "sedimentary_rocks.png")

# Convert and save Metamorphic Rocks graph
svg_metamorphic <- export_svg(DiagrammeR::grViz(metamorphic_graph))
rsvg_png(charToRaw(svg_metamorphic), "metamorphic_rocks.png")

