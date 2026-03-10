# Main reading the data together ------------------------------------------
# Reading data off the Artemis board is not straight forward

# Set up ------------------------------------------------------------------
  setwd("C:/Users/PC/OneDrive - University of the Sunshine Coast/DeadReckoning")
  
  pacman::p_load(
    tidyverse,
    data.table,
    plotly,
    rgl,
    lubridate,
    zoo
  )
  
  # load in custom functions (helpers)
  source("Scripts/ReadingData/Custom_Functions.R")
  
  # set some variables and paths
  path_to_data <- "Data/Impala/Collar8" # set the collar we're working on
  CollarNum <- basename(path_to_data)

# Read the data together  and aline the data sources ---------------------
chunked_dir_path <- file.path(path_to_data, "ArtemisChunked")
source("Scripts/ReadingData/CreatingAlignedData.R")
