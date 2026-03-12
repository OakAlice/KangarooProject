# Creating basic plots of each day's data ---------------------------------
# All the code in the world is not as good as a decent eyeballing
# looking for flat spots, misreads, time-skips, etc.
plot_day_from_list <- function(day_data, date) {
  
  n <- max(1, nrow(day_data) %/% 5000)
  accel_sub <- day_data[seq(1, nrow(day_data), by = n), ]
  
  accel_plot <- ggplot(accel_sub, aes(x = rtc_datetime)) +
    geom_line(aes(y = RawAX), colour = "coral",        alpha = 0.6) +
    geom_line(aes(y = RawAY), colour = "deepskyblue3", alpha = 0.6) +
    geom_line(aes(y = RawAZ), colour = "gold1",        alpha = 0.6) +
    my_theme() +
    labs(x = "Time", y = "Acceleration")
  
  ggsave(file.path(vis_dir, paste0(date, "_accel_plot.png")),
         accel_plot, width = 9, height = 3, units = "in", dpi = 100, bg = "white")
  
  invisible(NULL)
}
