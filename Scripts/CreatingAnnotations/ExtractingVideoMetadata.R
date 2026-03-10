# Extracting video metadata -----------------------------------------------

if(file.exists(file.path(collar, "Video_metadata.csv"))){
  print("metadata already extracted")
} else {
  # Extracting video information
  videos_list <- list.files(file.path(collar, "Videos"), 
                            pattern = "MP4", 
                            ignore.case = TRUE, 
                            full.names = TRUE, 
                            recursive = TRUE)
  
  # Extract the base metadata
  video_info <- data.frame()  # Reset for each
  for (video in videos_list) {
    filename <- basename(video)
    dirname <- basename(dirname(video))
    
    Video_mtime <- file.info(video)$mtime
    Dur_video_sec <- av_media_info(video)$duration
    
    if (grepl("DJI", filename)) {
      # strip the video time out of the name
      time_string <- str_split(filename, "_", simplify = TRUE)[2]
      start_time <- as.POSIXct(time_string, format = "%Y%m%d%H%M%S", tz = "Australia/Brisbane")
    } else {
      print("havent figured out time comnversion for non-DJI videos yet")
    }
    
    # Create temporary dataframe for this video
    temp_video_info <- data.frame(
      individual = basename(collar),
      date = dirname,
      filename = filename,
      mtime = Video_mtime,
      start_time = start_time,
      duration_sec = Dur_video_sec,
      stringsAsFactors = FALSE
    )
    
    # Append to this video info
    video_info <- rbind(video_info, temp_video_info)
  }
  # save results
  fwrite(video_info, file.path(collar, "Video_metadata.csv"))
}