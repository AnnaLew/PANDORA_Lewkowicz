# install.packages("V8", type = "binary")
library(DiagrammeR)
library(V8)
library(DiagrammeRsvg)
library(rsvg)

data <- read.csv("All_rocks_v1.csv", sep=";", stringsAsFactors = FALSE)

data <- na.omit(data)  # Remove rows with missing values



## ALL TYPES IN ONE IMAGE ##

types <- unique(data$Rock.type)
subtypes <- unique(data$Rock.subtype)
nodes <- unique(data$Rock)

graph_script <- "digraph rocks {\n  rankdir=LR;\n  node [shape = box, style = filled, fillcolor = lightblue, fontname = Helvetica]\n  edge [arrowhead = normal]\n"

graph_script <- paste(graph_script, paste(types, "[fillcolor = lightcoral];", collapse="\n"), "\n")

for (i in 1:nrow(data)) {
  if (data$Rock.subtype[i] != "" && data$Rock.type[i] != "") {
    graph_script <- paste(graph_script, paste0("\"", data$Rock.type[i], "\" -> \"", data$Rock.subtype[i], "\";\n"))
  }
  if (data$Rock[i] != "" && data$Rock.subtype[i] != "") {
    graph_script <- paste(graph_script, paste0("\"", data$Rock.subtype[i], "\" -> \"", data$Rock[i], "\";\n"))
  }
}

graph_script <- paste(graph_script, "}")

grViz(graph_script)

# Convert graph to SVG
svg_code <- export_svg(grViz(graph_script))

# Save as PNG
rsvg_png(charToRaw(svg_code), file = "rock_flowchart.png")



## EACH TYPE SEPARATELY ##

rock_types <- unique(data$Rock.type)

for (rock_type in rock_types) {
  subset_data <- data[data$Rock.type == rock_type, ]
  
  graph_script <- "digraph rocks {\n  rankdir=LR;\n  node [shape = box, style = filled, fillcolor = lightblue, fontname = Helvetica]\n  edge [arrowhead = normal]\n"
  
  graph_script <- paste(graph_script, paste0("\"", rock_type, "\" [fillcolor = lightcoral];\n"))
  
  for (i in 1:nrow(subset_data)) {
    if (subset_data$Rock.subtype[i] != "" && subset_data$Rock.type[i] != "") {
      graph_script <- paste(graph_script, paste0("\"", subset_data$Rock.type[i], "\" -> \"", subset_data$Rock.subtype[i], "\";\n"))
    }
    if (subset_data$Rock[i] != "" && subset_data$Rock.subtype[i] != "") {
      graph_script <- paste(graph_script, paste0("\"", subset_data$Rock.subtype[i], "\" -> \"", subset_data$Rock[i], "\";\n"))
    }
  }
  
  graph_script <- paste(graph_script, "}")
  
  graph <- grViz(graph_script)
  
  svg_code <- export_svg(graph)
  png_filename <- paste0(tolower(rock_type), "_flowchart.png")
  rsvg_png(charToRaw(svg_code), file = png_filename)
}
