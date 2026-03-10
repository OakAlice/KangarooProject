# Function to combine the artemis accelerometer files ---------------------
# adapted from Chris Clemente -> https://github.com/cclemente/Collar_data_extraction/blob/main/Step1a_Read_in_raw_accel_files.R
stitch_artemis_accels <- function(accel_files){
  
  file_nums <- as.integer(gsub("\\D", "", basename(accel_files))) # order them approporiately
  accel_files <- accel_files[order(file_nums)]
  
  bad_files <- c()  # store skipped files
  
  accel_data <- rbindlist(
    lapply(accel_files, function(f) {
      if (file.size(f) == 0) {
        message("Skipping empty file: ", f)
        bad_files <<- c(bad_files, f)
        return(NULL)
      }
      dt <- tryCatch(
        fread(f, skip = 0),
        error = function(e) {
          message("Skipping unreadable file: ", f, " (", e$message, ")")
          bad_files <<- c(bad_files, f)
          return(NULL)
        }
      )
      dt
    }),
    use.names = TRUE,
    fill = TRUE
  )
  
  return(accel_data)
}

# Combining the GPS files -------------------------------------------------
# code from Chris
clean_artemis_GPS <- function(path) {
  # read in the raw data
  raw <- readBin(path, what = "raw", n = file.info(path)$size)
  raw[raw == as.raw(0)] <- as.raw(32)                       # NUL -> space
  keep <- (raw >= as.raw(32) & raw <= as.raw(126)) | raw %in% c(as.raw(10), as.raw(13))
  txt  <- rawToChar(raw[keep])
  txt  <- gsub("\r\n?", "\n", txt, useBytes = TRUE)
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE), use.names = FALSE)
  
  # grep patterns for the specific data we are trying to extract
  re_lat_only <- "^\\s*Lat:([+-]?\\d+(?:\\.\\d+)?)\\s*$"
  re_lon_line <- "^\\s*(\\d{2}/\\d{2}/\\d{4}\\s+\\d{2}:\\d{2}:\\d{2})\\s*-\\s*Lon:([+-]?\\d+(?:\\.\\d+)?),?\\s*(?:Lat:([+-]?\\d+(?:\\.\\d+)?))?\\s*$"
  re_rtc_line <- "^\\s*\\^\\s*(\\d{2}/\\d{2}/\\d{4})\\s*,\\s*(\\d{2}:\\d{2}:\\d{2}(?:\\.\\d{1,2})?)\\s*$"
  
  out <- vector("list", length(lines))
  k <- 0
  
  # extract lat from nearby lines (prev/next up to 2)
  find_neighbor_lat <- function(idx) {
    # prefer previous line, then next lines (skip blank/rtc)
    # search order: i-1, i-2, i+1, i+2
    ord <- c(idx-1L, idx-2L, idx+1L, idx+2L)
    ord <- ord[ord >= 1 & ord <= length(lines)]
    for (j in ord) {
      lj <- lines[j]
      mlat <- str_match(lj, re_lat_only)
      if (!is.na(mlat[1,1])) return(as.numeric(mlat[1,2]))
      # also tolerate a "Lat:" trailing after a comma-only line, e.g. "...,", then "Lat:..."
    }
    return(NA_real_)
  }
  
  i <- 1L
  while (i <= length(lines)) {
    li <- lines[i]
    
    # Case A: the standard lon line (may or may not include Lat)
    m <- str_match(li, re_lon_line)
    if (!is.na(m[1,1])) {
      gps_ts_str <- m[1,2]
      lon_val    <- as.numeric(m[1,3])
      lat_val    <- if (!is.na(m[1,4])) as.numeric(m[1,4]) else NA_real_
      
      # If Lat wasn't on the same line, try nearby lines (handles your Lat-then-Lon case)
      if (is.na(lat_val)) lat_val <- find_neighbor_lat(i)
      
      # Look ahead for RTC (up to 3 lines, but stop if we hit next lon record)
      rtc_str <- NA_character_
      for (j in seq.int(i+1L, min(i+3L, length(lines)))) {
        if (j > length(lines)) break
        lj <- lines[j]
        if (nzchar(lj)) {
          # Stop early if a new GPS block appears
          if (!is.na(str_match(lj, re_lon_line)[1,1])) break
          mrtc <- str_match(lj, re_rtc_line)
          if (!is.na(mrtc[1,1])) {
            rtc_str <- paste(mrtc[1,2], mrtc[1,3])
            break
          }
        }
      }
      
      # Record if we have enough fields
      if (!is.na(lat_val) && !is.na(rtc_str)) {
        k <- k + 1
        out[[k]] <- list(
          # note that the dates are in DIFFERENT formats
          # there is american AND normal formats in the same txt file!!
          internal_timestamp_raw = rtc_str,          # mm/dd/yyyy hh:mm:ss.s
          gps_timestamp_raw      = gps_ts_str,       # dd/mm/yyyy hh:mm:ss
          lon = lon_val,
          lat = lat_val
        )
      }
      
      i <- i + 1L
      next
    }
    
    # Case B: early-file pattern may start with a Lat-only line; just move on.
    # We'll bind it when we encounter the Lon line that follows.
    i <- i + 1L
  }
  
  if (k == 0) return(NULL)
  dt <- rbindlist(out[seq_len(k)])
  
  # Parse timestamps (note the different formats)
  dt[, internal_timestamp := as.POSIXct(internal_timestamp_raw, format = "%m/%d/%Y %H:%M:%OS", tz = "UTC")]
  dt[, gps_timestamp      := as.POSIXct(gps_timestamp_raw,      format = "%d/%m/%Y %H:%M:%S",  tz = "UTC")]
  
  dt[, .(internal_timestamp, gps_timestamp, lon, lat)]
}

stitch_artemis_gps <- function(gps_files){
  gps_data <- lapply(gps_files, clean_artemis_GPS)
  gps_data <- rbindlist(gps_data, use.names = TRUE, fill = TRUE)
  return(gps_data)
}

# Combine gps and accel together ------------------------------------------

combine_accel_GPS <- function(example_dir, accel_data, gps_data){
  
  setDT(accel_data)
  setDT(gps_data)
  
  # convert the times to POSIXct again (just in case) # set them all to UTC
  gps_data[, internal_timestamp := as.POSIXct(internal_timestamp, tz = "UTC")]
  gps_data[, gps_timestamp := as.POSIXct(gps_timestamp, tz = "UTC")]
  
  # check the reset events
  if (max(accel_data$reset_events) != max(gps_data$reset_events)){
    print("stop, there's soemthing wrong - the devices have a different number of resets")
  } else {
    print("devices were reset the same number of times")
  }
  
  # Match timestamps in the accelerometer and GPS ---------------------------
  setkey(accel_data, reset_events, rtc_datetime)
  setkey(gps_data, reset_events, internal_timestamp)
  
  # check whether there are matches and print if there arent # debugging step
  bounds <- range(accel_data$rtc_datetime, na.rm = TRUE)
  
  any_in_range <- any(
    gps_data$internal_timestamp >= bounds[1] &
      gps_data$internal_timestamp <= bounds[2],
    na.rm = TRUE
  )
  
  if (any_in_range){
    accel_data[, gps_flag := FALSE]
    accel_data[
      gps_data,
      on = .(
        reset_events, # ensure that only matches within the reset event
        rtc_datetime = internal_timestamp
      ),
      roll = "nearest",
      mult = "first",
      `:=`(
        gps_timestamp = i.gps_timestamp,
        gps_lon       = i.lon,
        gps_lat       = i.lat,
        gps_flag      = TRUE
      )
    ]
  } else {
    print("these dont match or they dont overlap")
  }
  
  # Should be the same as the number of GPS hits, check whether that's the case
  if (sum(accel_data$gps_flag) != nrow(gps_data)){
    print("there is something funny going on... not all the GPS hits have a match")
  }

  # arrange by the reset_events and then the rtc_datetime 
  setorder(accel_data, reset_events, rtc_datetime)
  
  # Convert GPS times to numeric seconds
  accel_data[, gps_time_sec := as.numeric(gps_timestamp)]
  # Interpolate GPS times linearly
  accel_data[, gps_time_est_sec := na.approx(gps_time_sec, na.rm = FALSE)]
  
  ##NOTE: However, because of the multiple reset, the times prior to the first clean hit will be wrong
  # We can't just delete these times because often the calibration event will be occuring in this window
  # Therefore, a bit of messy math...
  
  # set the correct order
  setorder(accel_data, reset_events, rtc_datetime)
  
  # get the sample rate moving forward after the first genuine hit
  accel_data[, dt_est := {
    
    first_idx <- which(!is.na(gps_timestamp))[1]

    valid_after <- gps_time_est_sec[first_idx:.N]
    if (length(valid_after) > 1)
      mean(diff(valid_after[1:min(1000, length(valid_after))]))
    else
      NA_real_
    
  }, by = reset_events]
  
  # find the first genione time
  accel_data[, first_idx := which(!is.na(gps_timestamp))[1],
    by = reset_events]
  
  # back proagate from that first valid hit to the beginning of the day
  accel_data[first_idx > 1 & !is.na(dt_est),
    gps_time_est_sec := {
      
      x <- gps_time_est_sec
      fi <- first_idx[1]
      dt <- dt_est[1]
      
      # cumulative backward offsets
      offsets <- rev(cumsum(rep(dt, fi - 1)))
      
      x[1:(fi - 1)] <- x[fi] - offsets
      x
      
    },
    by = reset_events]
  
  # Convert back to POSIXct
  accel_data[, gps_time_est := as.POSIXct(gps_time_est_sec, origin = "1970-01-01", tz = "UTC")]
  # clean
  accel_data[, c("gps_time_sec", "gps_time_est_sec", "first_idx", "dt_est") := NULL]

  # return it. Though warning, this can be massive
  return(accel_data)
}

# Looking for time skips in the GPS data ----------------------------------
gps_diagnostics <- function(gps_data){
  
  gps_jumps <- gps_data %>%
    arrange(gps_timestamp) %>%
    mutate(time_diff = difftime(internal_timestamp, lag(internal_timestamp), units = "min"), # had to define package or errored
           break_point = ifelse(as.numeric(time_diff) > 6 | as.numeric(time_diff) < 0 , 1, 0), # if bigger than 6 minutes or -ve... then count as break
           break_point = replace_na(break_point, 0),
           sequence = cumsum(break_point)) %>%
    select(-time_diff) %>%
    ungroup() %>%
    mutate(difference_gps = round(as.numeric(
      difftime(gps_timestamp, lag(gps_timestamp), units = "mins")
    ), 1)) %>%
    mutate(difference_internal = round(as.numeric(
      difftime(internal_timestamp, lag(internal_timestamp), units = "mins")
    ), 1)) %>%
    mutate(read_error = difference_gps - difference_internal)
  
  return(gps_jumps)
}
