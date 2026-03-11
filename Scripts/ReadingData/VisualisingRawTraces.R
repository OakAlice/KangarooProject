# Creating basic plots of each day's data ---------------------------------
# All the code in the world is not as good as a decent eyeballing
# looking for flat spots, misreads, time-skips, etc.

vis_dir <- file.path(collar, "Visuals")
if(!dir.exists(vis_dir)){
  dir.create(vis_dir, showWarnings = FALSE)
}

all_files <- list.files(file.path(collar, "ArtemisAlignedChunked"), 
                        full.names = TRUE, recursive = TRUE)

plot_day <- function(x) {
  load(x)  # loads day_data
  
  date <- str_split(tools::file_path_sans_ext(basename(x)), "_", simplify = TRUE)[3]
  
  # Downsample accel (high freq, dont need every point for vis purposes)
  n <- max(1, nrow(day_data) %/% 5000)
  accel_sub <- day_data[seq(1, nrow(day_data), by = n), ]
  
  accel_plot <- ggplot(accel_sub, aes(x = gps_time_est)) +
    geom_line(aes(y = RawAX), colour = "coral",        alpha = 0.6) +
    geom_line(aes(y = RawAY), colour = "deepskyblue3", alpha = 0.6) +
    geom_line(aes(y = RawAZ), colour = "gold1",        alpha = 0.6) +
    my_theme() +
    labs(x = "Time", y = "Acceleration")
  
  ggsave(file.path(vis_dir, paste0(date, "_accel_plot.png")),
         accel_plot, width = 9, height = 3, units = "in", dpi = 100, bg = "white")
  
  gps_data <- day_data[!is.na(day_data$gps_lon), ]
  
  gps_plot <- ggplot(gps_data, aes(x = gps_lat, y = gps_lon, colour = gps_timestamp)) +
    geom_path(linewidth = 2) +
    my_theme() +
    labs(x = "Latitude", y = "Longitude", colour = "Timestamp")
  
  ggsave(file.path(vis_dir, paste0(date, "_gps_plot.png")),
         gps_plot, width = 9, height = 9, units = "in", dpi = 100, bg = "white")
  
  invisible(NULL)
}

# Parallel across files (faster)
n_cores <- max(1, detectCores() - 2)
cl <- makeCluster(n_cores)
clusterExport(cl, c("vis_dir", "my_theme"))
clusterEvalQ(cl, { library(ggplot2); library(stringr); library(dplyr) })

parLapply(cl, all_files, plot_day)
stopCluster(cl)
