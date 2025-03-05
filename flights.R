# load libraries
library(ggplot2)
library(gganimate)
library(maps)
library(geosphere)
library(dplyr)
library(tools)
library(ggimage)
library(av)

# Define locations (longitude, latitude)
locations <- list(
  istanbul = c(lon = 28.9784, lat = 41.0082),
  lisbon = c(lon = -9.1399, lat = 38.7169),
  belgrade = c(lon = 20.4601, lat = 44.7866),
  malmo = c(lon = 13.0038, lat = 55.6049),
  paris = c(lon = 2.3522, lat = 48.8566),
  madrid = c(lon = -3.7038, lat = 40.4168),
  geneva = c(lon = 6.1432, lat = 46.2044),
  vilnius = c(lon = 25.2797, lat = 54.6872),
  copenhagen = c(lon = 12.5655, lat = 55.6761),
  porto = c(lon = -8.6110, lat = 41.1496),
  thisted = c(lon = 8.5690, lat = 56.9550),
  riga = c(lon = 24.1052, lat = 56.9496)
)

# Define flight paths and corresponding years
flight_segments <- list(
  c("istanbul", "lisbon", 2013),
  c("lisbon", "belgrade", 2014),
  c("belgrade", "malmo", 2015),
  c("malmo", "paris", 2016),
  c("paris", "madrid", 2017),
  c("madrid", "geneva", 2018),
  c("geneva", "vilnius", 2019),
  c("vilnius", "copenhagen", 2022),
  c("copenhagen", "porto", 2023),
  c("porto", "thisted", 2024),
  c("thisted", "riga", 2025)
)

# Number of points for animation
n_points <- 50

# Generate flight paths and combine into a single data frame
all_flights <- data.frame()
segment_index <- 1

for (segment in flight_segments) {
  start <- locations[[segment[1]]]
  end <- locations[[segment[2]]]
  year <- segment[3]
  
  path <- as.data.frame(geosphere::gcIntermediate(start, end, n = n_points, addStartEnd = TRUE))
  path$segment <- factor(segment_index)  
  path$frame <- seq_len(nrow(path)) + (segment_index - 1) * n_points  
  path$year <- year  # Assign year
  path$start <- segment[1]
  path$end <- segment[2]
  
  all_flights <- bind_rows(all_flights, path)
  segment_index <- segment_index + 1
}

# Get world map data
world_map <- map_data("world")

# get plane image
plane_image <- "plane.png"

# Generate plots for each row
for (i in seq_len(nrow(all_flights))) {
  # Subset data up to current row
  current_data <- all_flights[i:i, ]
  
  # get destination city name
  city_name <- current_data$end
  
  # Access the correct location based on city_name
  location_coords <- locations[[city_name]]
  
  # get lon and lat for destination city
  lon <- location_coords['lon']
  lat <- location_coords['lat']
  
  # plot for each frame
  p <- ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                 fill = alpha("gray90", 0.6), color = "lightgray") +  
    geom_image(data = current_data, aes(x = lon, y = lat, image = plane_image), size = 0.08) +  
    geom_point(aes(x = lon, y = lat), color = "red", size = 5) +  # Mark city
    geom_text(aes(x = lon, y = lat, label = toTitleCase(city_name)), color = "darkblue", vjust = -1.5) +
    coord_fixed(xlim = c(-20, 35), ylim = c(35, 60)) + 
    labs(
      title = 'Anniversary Trips', 
      subtitle = paste0("Going to ", toTitleCase(current_data$end)  ," in Year ", current_data$year)) +  
    theme_minimal() +
    theme(
      axis.text = element_blank(), 
      axis.ticks = element_blank(), 
      axis.title = element_blank(),
      panel.grid = element_blank(), 
      panel.border = element_blank()
    )
  
  
  # Save each frame as jpg
  ggsave(filename = sprintf("frames/frame_%03d.jpg", i), plot = p, width = 8, height = 6, dpi = 300)
}

# using the animation method found here : https://stackoverflow.com/a/73376411/10710995
filenames <- paste0("frames/frame_", sprintf("%03d", 1:572), ".jpg")

av::av_encode_video(filenames, framerate = 20,
                    output = "anniversary_trips.mp4")