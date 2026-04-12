library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

data_path <- file.path("data", "processed", "billionaires_analysis_ready.csv")
data <- read.csv(data_path, stringsAsFactors = FALSE)

country_counts <- data %>%
    group_by(country) %>%
    summarise(count = n(), .groups = "drop")

coords <- data %>%
    select(country, latitude_country, longitude_country) %>%
    distinct() %>%
    filter(!is.na(latitude_country) & !is.na(longitude_country))

country_counts <- country_counts %>%
    left_join(coords, by = "country") %>%
    filter(!is.na(latitude_country)) %>%
    rename(lat = latitude_country, lon = longitude_country)

world_map <- ne_countries(scale = "medium", returnclass = "sf")

p <- ggplot() +
    geom_sf(data = world_map, fill = "gray80", colour = "gray50", linewidth = 0.2) +
    geom_point(data = country_counts,
               aes(x = lon, y = lat, color = count, size = count),
               alpha = 0.8) +
    scale_color_gradient(low = "#fee08b", high = "#d73027", name = "Billionaires") +
    scale_size_continuous(range = c(2, 12), name = "Billionaires") +
    guides(size = "none") +
    labs(title = "Billionaire Density by Country of Residence", x = "Longitude", y = "Latitude") +
    theme_minimal()

output_path <- file.path("output", "geospatial_cluster_country.png")
ggsave(output_path, plot = p, width = 12, height = 7)
cat("Saved:", output_path, "\n")
