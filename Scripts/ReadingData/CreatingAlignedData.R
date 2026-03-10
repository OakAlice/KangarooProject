# Combing the accelerometer and GPS data ----------------------------------
# This requires reading the data in, reformatting, and cleaning

setDTthreads(0L) # make the fread function faster

# define the path to the files
data_dir <- file.path(path_to_data, "Raw")
accel_files <- list.files(data_dir, pattern = "dataLog", full.names = TRUE)
gps_files <- list.files(data_dir, pattern = "serialLog", full.names = TRUE)

# Read artemis accel files together ---------------------------------------
if(!file.exists(file.path(path_to_data, "Artemis_Accel.RDA"))){
  accel_data <- stitch_artemis_accels(accel_files)
  setDT(accel_data)
  # convert the units of acceleration
  accel_data[, c("RawAX", "RawAY", "RawAZ")] <- accel_data[, c("RawAX", "RawAY", "RawAZ")] / 2048
  # remove the emoty column
  accel_data[, V17 := NULL]
 
  ###NOTE: Because our study was international, we set up the collars in Australia, tested them several times, and then flew them to South Africa
  # This created a fair bit of chaos for our collars with the time resetting continuously
  # therefore we have to account for the fact that collars will revert to 0 multiple times
  # DO NOT sort by rtc_datetime until you've dealt with the duplication (reset events)
  
  # convert the internal timestamp
  accel_data[, rtc_datetime :=
                as.POSIXct(paste(rtcDate, rtcTime), format = "%m/%d/%Y %H:%M:%OS", tz = "UTC")
  ]
  
  # Find whenever the device resets and label those as separate sampling events
  accel_data[, time_diff :=  c(NA_real_, diff(unclass(rtc_datetime)))]
  accel_data[, reset := as.integer(time_diff < 0)]
  accel_data[is.na(reset), reset := 0L]
  accel_data[, reset_events := cumsum(reset)]
  
  # clean it up
  accel_data[, c("time_diff", "reset", "output_Hz") := NULL]
  
  # save as an RDA file
  save(accel_data, file = file.path(path_to_data, "Artemis_Accel.RDA"), compress = FALSE) # make it faster to read and write
  
} else {
  print("just loading in the premade accel data")
  load(file.path(path_to_data, "Artemis_Accel.RDA")) # comes in as accel_data
}

# Read the GPS files together ---------------------------------------------
if(!file.exists(file.path(path_to_data, "Artemis_GPS.csv"))){
  gps_data <- stitch_artemis_gps(gps_files)
  gps_data <- gps_data[complete.cases(gps_data), ]
  
  ##NOTE: Because our study was international, we set up the collars in Australia, tested them several times, and then flew them to South Africa
  # This created a fair bit of chaos for our collars with the time resetting continuously
  # I have to remove everything that occurred prior to Africa (without removing the calibration event either)
  # therefore we have to account for the fact that collars will revert to 0 multiple times
  gps_data[, time_diff :=  c(NA_real_, diff(unclass(internal_timestamp)))]
  gps_data[, reset := as.integer(time_diff < 0)]
  gps_data[is.na(reset), reset := 0L]
  gps_data[, reset_events := cumsum(reset)]
  
  # clean it up
  gps_data[, c("time_diff", "reset") := NULL]
  
  fwrite(gps_data, file.path(path_to_data, "Artemis_GPS.csv"))
} else {
  print("just loading in what was made before")
  gps_data <- fread(file.path(path_to_data, "Artemis_GPS.csv"))
}

if(!file.exists(file.path(path_to_data, "Artemis_Aligned.RDA"))){
  # Combing the GPS and Accel files based on timestamp ----------------------
  # saves all aligned data as a single RDA with aligned timestamps
  accel_data <- combine_accel_GPS(path_to_data, accel_data, gps_data)
  
  save(accel_data, file = file.path(path_to_data, "Artemis_Aligned.RDA"), compress = FALSE) # make it faster to read and write

} else { # else load it in
  print("just loading in all the aligned data that was made before")
  load(file.path(path_to_data, "Artemis_Aligned.RDA")) # this will come in as accel_data
}

# now split the data into 24 hr periods and save each of those independently
# Extract date from estimated GPS time
setDT(accel_data)
accel_data[, date := as.Date(gps_time_est)]
unique(accel_data$date)

if(!file.exists(file.path(chunked_dir_path, paste0("Board_Aligned_", unique(testdata$date)[1], ".RDA")))){
  if (!dir.exists(chunked_dir_path)) {
    dir.create(chunked_dir_path, recursive = TRUE)
  }
  
  # Split by date
  data_list <- split(accel_data, by = "date", keep.by = TRUE)
  
  # Save each day to a separate RDA file in the chunked folder
  lapply(names(data_list), function(d) {
    day_data <- data_list[[d]]
    fwrite(day_data, file = file.path(chunked_dir_path, paste0("Board_Aligned_", d, ".csv")))
    save(day_data, file = file.path(chunked_dir_path, paste0("Board_Aligned_", d, ".RDA")))
  })
} else {
  print("the data has already been split into days")
}
 
# clean the workspace
if (exists("testdata")) rm(testdata)
if (exists("accel_data")) rm(accel_data)
if (exists("data")) rm(data)
if (exists("data_list")) rm(data_list)
