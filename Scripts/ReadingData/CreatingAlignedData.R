# Combing the accelerometer and GPS data ----------------------------------
# Adding the two different data sources together

source("Scripts/ReadingData/Custom_Functions.R")

if(!file.exists(file.path(collar, "Artemis_Aligned.RDA"))){
  
  # load in what we created earlier
  load(file.path(collar, "Artemis_Accel.RDA")) # comes in as accel_data
  gps_data <- fread(file.path(collar, "Artemis_GPS.csv"))
  
  # Combing the GPS and Accel files based on timestamp ----------------------
  # saves all aligned data as a single RDA with aligned timestamps
  accel_data <- combine_accel_GPS(collar, accel_data, gps_data)
  
  save(accel_data, file = file.path(collar, "Artemis_Aligned.RDA"), compress = FALSE) # make it faster to read and write

} else { # else load it in
  print("just loading in all the aligned data that was made before")
  load(file.path(collar, "Artemis_Aligned.RDA")) # this will come in as accel_data
}

# now split the data into 24 hr periods and save each of those independently
# Extract date from estimated GPS time
setDT(accel_data)
accel_data[, date := as.Date(gps_time_est)]
unique(accel_data$date)

chunked_dir_path <- file.path(collar, "ArtemisAlignedChunked")

if(!file.exists(file.path(chunked_dir_path, paste0("Board_Aligned_", unique(accel_data$date)[1], ".RDA")))){
  if (!dir.exists(chunked_dir_path)) {
    dir.create(chunked_dir_path, recursive = TRUE)
  }
  
  # Split by date
  data_list <- split(accel_data, by = "date", keep.by = TRUE)
  
  # Save each day to a separate RDA file in the chunked folder
  lapply(names(data_list), function(d) {
    day_data <- data_list[[d]]
    save(day_data, file = file.path(chunked_dir_path, paste0("Board_Aligned_", d, ".RDA")))
  })
} else {
  print("the data has already been split into days")
}
 
# clean the workspace
if (exists("accel_data")) rm(accel_data)
if (exists("data")) rm(data)
if (exists("data_list")) rm(data_list)
if (exists("gps_data")) rm(gps_data)
gc()

