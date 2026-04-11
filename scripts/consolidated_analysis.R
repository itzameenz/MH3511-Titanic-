# Consolidated R Code for Generating Statistics and Images

# Load required libraries
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)

# Load the processed dataset
data_path <- file.path("data", "processed", "billionaires_analysis_ready.csv")
data <- read.csv(data_path, stringsAsFactors = FALSE)

# 1. Generate Histograms
source("scripts/process_billionaires_data.R")

# 2. Generate Boxplots
source("scripts/plot_top_categorical_boxplots.R")

# 3. Analyze Final Worth Factors
source("scripts/analyze_final_worth_factors.R")

# 4. Analyze Recommended Factors
source("scripts/analyze_recommended_factors.R")

# 5. Test All Boxplot Groups
source("scripts/test_all_boxplot_groups.R")

# 6. Generate Geospatial Clustering Graphs
source("scripts/geospatial_clustering.R")

cat("All analyses and visualizations have been consolidated and executed.\n")
