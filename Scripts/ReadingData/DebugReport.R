# Debug report: looking for possible issues in the data -------------------
# This was necessitated by me finding so many inconsistencies in the raw data the downstream analysis didn't work
# Created to be used for assisting with the debugging

# Making visualisations of the raw data -----------------------------------
vis_dir <- file.path(collar, "QualityChecks")
if(!dir.exists(vis_dir)){
  dir.create(vis_dir, showWarnings = FALSE)
}

load(file.path(collar, "Artemis_Accel.RDA")) # comes in as accel_data
accel_data$rtcDate <- gsub("/", "-", accel_data$rtcDate)
accel_list <- split(accel_data, accel_data$rtcDate) # split into a list per day

# Make a plot for each of those days
lapply(names(accel_list), function(date) {
  plot_day_from_list(accel_list[[date]], date)
})

# Making plots from the gps data ------------------------------------------
gps_data <- fread(file.path(collar, "Artemis_GPS.csv"))
gps_plot <- ggplot(gps_data, aes(x = lon, y = lat, colour = gps_timestamp)) +
  geom_path(linewidth = 1.5) +
  my_theme() +
  labs(x = "Longitude", y = "Latiutude", colour = "Time")
ggsave(file.path(vis_dir, "gps_plot.png"),
       gps_plot, width = 9, height = 6, units = "in", dpi = 100, bg = "white")

# Checking the continuousness of the times --------------------------------
## Accel ------------------------------------------------------------------
setDT(accel_data)[, `:=`(
  internal_diff = as.numeric(rtc_datetime  - shift(rtc_datetime), units = "secs")
)]

maxincaccel <- max(accel_data$internal_diff, na.rm = TRUE)
meanincaccel <- mean(accel_data$internal_diff, na.rm = TRUE)
minincaccel <- min(accel_data$internal_diff, na.rm = TRUE)

## GPS --------------------------------------------------------------------
setDT(gps_data)[, `:=`(
  internal_diff = as.numeric(internal_timestamp - shift(internal_timestamp), units = "mins"),
  gps_diff      = as.numeric(gps_timestamp - shift(gps_timestamp), units = "mins")
)]

maxincintgps <- max(gps_data$internal_diff, na.rm = TRUE)
meanincintgps <- mean(gps_data$internal_diff, na.rm = TRUE)
minincintgps <- min(gps_data$internal_diff, na.rm = TRUE)
maxincextgps <- max(gps_data$gps_diff, na.rm = TRUE)
meanincextgps <- mean(gps_data$gps_diff, na.rm = TRUE)
minincextgps <- min(gps_data$gps_diff, na.rm = TRUE)

ggplot(gps_data, aes(x = seq(1:nrow(gps_data)), y = internal_timestamp)) + geom_point()







# reinterpolate internal_timestamp where internal_diff < 1 
# (this is just an error threshold... it shouldnt be sampling more than once in the same sec)
gps_data[, internal_timestamp := {
  for (i in seq_len(.N)) {
    if (!is.na(internal_diff[i]) && internal_diff[i] < 1) {
      internal_timestamp[i] <- internal_timestamp[i - 1] + gps_diff[i] * 60
    }
  }
  internal_timestamp
}]