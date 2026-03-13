# data 
source("Scripts/Api_Key.R") # for google maos
register_google(key = API_KEY) # saved in a separate file, not saved to Git

pacman::p_load(splines, ggmap)

gps_data <- fread(file.path("Data/RawData/BigCahuna", "Artemis_GPS.csv"))

# crop out the dat I want
gps_data$Date <- as.Date(gps_data$gps_timestamp)
gps_data <- gps_data[gps_data$Date < "2025-06-03", ]

# smooth it
gps_smooth <- gps_data %>%
  arrange(gps_timestamp) %>%
  mutate(t = as.numeric(gps_timestamp - first(gps_timestamp), units = "secs")) %>%
  group_modify(~ {
    df <- .x
    if (nrow(df) < 5) return(df)
    
    lat_lm <- lm(lat  ~ bs(t, df = min(30, nrow(df)-1), degree = 3), data = df)
    lon_lm <- lm(lon  ~ bs(t, df = min(30, nrow(df)-1), degree = 3), data = df)
    
    df %>%
      mutate(
        Lat_smooth = predict(lat_lm),
        Lon_smooth = predict(lon_lm)
      )
  }) %>%
  ungroup()


# Plot it -----------------------------------------------------------------
center_lon <- mean(gps_smooth$Lon_smooth, na.rm = TRUE)
center_lat <- mean(gps_smooth$Lat_smooth, na.rm = TRUE)

map_sat <- get_googlemap(
  center = c(lon = center_lon, lat = center_lat),
  maptype = "satellite",
  zoom = 13
)

ggmap(map_sat) +
  geom_path(data = gps_smooth, aes(Lon_smooth, Lat_smooth, colour = gps_timestamp),
            linewidth = 2) +
  my_theme() +
  scale_colour_gradient(low = "blue", high = "red") +
  labs(x = "Longitude", y = "Latitude", colour = "Time")+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

