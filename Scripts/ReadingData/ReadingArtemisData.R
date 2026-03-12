# Reading off the board into standardised formats -------------------------

setDTthreads(0L) # make the fread function faster

source("Scripts/ReadingData/Custom_Functions.R")

path_to_data <- file.path(collar, "Board")
accel_files <- list.files(path_to_data, pattern = "dataLog", full.names = TRUE)
gps_files <- list.files(path_to_data, pattern = "serialLog", full.names = TRUE)

# Read artemis accel files together ---------------------------------------
if(!file.exists(file.path(collar, "Artemis_Accel.csv"))){
  accel_data <- stitch_artemis_accels(accel_files)
  setDT(accel_data)
  # convert the units of acceleration
  accel_data[, c("RawAX", "RawAY", "RawAZ")] <- accel_data[, c("RawAX", "RawAY", "RawAZ")] / 2048
  # remove the emoty column
  accel_data[, V17 := NULL]
  
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
  save(accel_data, file = file.path(collar, "Artemis_Accel.RDA"), compress = FALSE) # make it faster to read and write
  fwrite(accel_data, file = file.path(collar, "Artemis_Accel.csv"))
} else {
  print("already made the accel data")
}

# Read the GPS files together ---------------------------------------------
if(!file.exists(file.path(collar, "Artemis_GPS.csv"))){
  gps_data <- stitch_artemis_gps(gps_files)
  
  if(nrow(gps_data) < 1){
    print("no suffessful hits :( sad")
    next
  }
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
  
  fwrite(gps_data, file.path(collar, "Artemis_GPS.csv"))
} else {
  print("aloready made the gps before")
}