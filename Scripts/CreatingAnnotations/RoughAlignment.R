# Manually Assessing Alignment / Determining Delay ------------------------
# the reality of working with technology is that some of the clocks drifted
# we need to determine the amount by which it drifted and it is easiest to do this manually
# use the following script to explore and play around with the different files

# Interactive plot for finding the delay between video and accel ------------
# you'll need to do this manually
# save results in an excel sheet - in this case I saved it to the RawData folder under Video_info.csv
# Manulaly define the video and date you want to work on ------------------

accel_files <- list.files(file.path(collar, "ArtemisAlignedChunked"), full.names = TRUE, pattern = ".RDA")
video_dir <- file.path(collar, "Videos")
video_metadata <- fread(file.path(collar, "Video_metadata.csv"))

videos <- list.files(video_dir, full.names = TRUE, recursive = TRUE, pattern = "\\.MP4$")
# video number selection
vid_number_in_list <- 23

{
  video_name <- basename(videos[vid_number_in_list]) # "DJI_20240702082054_0038_D.MP4"
  
  # get the times
  video_start <- as.POSIXct(
    str_split(video_name, "_", simplify = TRUE)[2], 
    format = "%Y%m%d%H%M%S", tz = "Australia/Sydney") ## Note timezone where the data was collected
  video_duration <- video_metadata[filename == video_name, duration_sec]
  video_end <- video_start + seconds(video_duration)
  
  # now convert to UTC (which is what the accel boards are in)
  video_start_UTC <- as.POSIXct(video_start, tz = "UTC")
  video_end_UTC <- as.POSIXct(video_end, tz = "UTC")
  
  # Load in the data --------------------------------------------------------
  # Load in the relevant accelerometer]
  date <- as.Date(video_start)
  load(accel_files[grep(date, accel_files)]) # comes in as day_data
  if (!inherits(day_data$gps_time_est, "POSIXct")) {
    day_data$gps_time_est <- as.POSIXct(day_data$gps_time_est, tz="UTC")
  }
  setDT(day_data)
}

# make sure everything is in UTC
plot_segment_app(day_data, video_start_UTC, video_end_UTC, date, save_dir = collar)


# Set the variables -----------------------------------------------------------
# Delay
# this value is derived from trial and error
# once it is determined for a single day, it can be used for all videos on that date
# this little shiny app allows you to scroll up to a minute in either direction to find the match
# watch the video and try to align it.
# when you determine the delay, save that number.
plot_segment_app <- function(day_data, video_start, video_end, date, x = 5, save_dir) {
  ui <- fluidPage(
    sliderInput("delay", "Drone delay (seconds):",
                min = -240, max = 240, value = 0, step = 1),
    actionButton("save", "Save clipped accel segment"),
    plotOutput("accelPlot")
  )
  server <- function(input, output, session) {
    # reactive expression to compute accel_segment based on current delay
    accel_segment_reactive <- reactive({
      Drone_delay <- input$delay
      video_start_updated <- video_start + seconds(Drone_delay)
      video_end_updated   <- video_end + seconds(Drone_delay)
      accel_segment <- day_data[gps_time_est >= video_start_updated & gps_time_est <= video_end_updated]
      accel_segment[, X := RawAX / 8192]
      accel_segment[, Y := RawAY / 8192]
      accel_segment[, Z := RawAZ / 8192]
      accel_segment[, t_sec := as.numeric(gps_time_est - video_start_updated)]
      accel_segment[, t_minsec := sprintf("%d:%02d",
                                          as.integer(t_sec %/% 60),   # minutes
                                          as.integer(t_sec %% 60))]   # seconds
      accel_segment
    })
    # plot based on reactive accel_segment
    output$accelPlot <- renderPlot({
      accel_segment <- accel_segment_reactive()
      plot_data <- tidyr::pivot_longer(
        accel_segment,
        cols = c(X, Y, Z),
        names_to = "Axis",
        values_to = "Accel_g"
      )
      ggplot(plot_data, aes(x = t_sec, y = Accel_g, color = Axis)) +
        geom_line(alpha = 0.7) +
        labs(x = "Minutes since video start", y = "Acceleration (g)",
             title = paste0("Accelerometer Data (delay = ", input$delay, "s)")) +
        theme_minimal() +
        scale_x_continuous(
          labels = function(s) sprintf("%d:%02d", s %/% 60, s %% 60),
          breaks = scales::breaks_extended(8)
        )
    })
    # save when button is clicked
    observeEvent(input$save, {
      accel_segment <- accel_segment_reactive()
      # convert to MATLAB time
      matlab_origin <- 719529  # MATLAB datenum for 1970-01-01
      accel_segment[, time_matlab := as.numeric(gps_time_est) / 86400 + matlab_origin]
      out <- accel_segment[, .(time_matlab, X, Y, Z)]
      # make the directory
      clipped_dir_path <- file.path(save_dir, "Clipped")
      if (!dir.exists(clipped_dir_path)) {
        dir.create(clipped_dir_path, recursive = TRUE)
      }
      # save with delay in filename
      vid_save_name <- tools::file_path_sans_ext(video_name)
      out_file <- file.path(
        clipped_dir_path,
        paste0(vid_save_name, "_delay", input$delay, "_clipped.csv")
      )
      fwrite(out, out_file)
      showNotification(paste("Saved:", out_file))
    })
  }
  shinyApp(ui, server)
}







