# Main file for the analysis of the kangaroo data -------------------------

# Set up ------------------------------------------------------------------
setwd("~/KangarooProject")

pacman::p_load(
  tidyverse,
  data.table,
  zoo,
  av,
  stringr,
  shiny,
  patchwork,
  parallel
)

sample_rate <- 50

source("Scripts/Plotting_Functions.R")

# meaningful days
sampling_times <- fread("Metadata/Sampling_Times.csv") %>%
  mutate(StartDate = as.Date(as.character(CollarDate), format = "%Y%m%d"),
         EndDate = as.Date(as.character(DropOffDate), format = "%Y%m%d"))

collars <- list.dirs("Data/RawData", recursive = FALSE) # all the ones we want to do
for (collar in collars){

  # Reading the data together -----------------------------------------------
  source("Scripts/ReadingData/ReadingArtemisData.R")
  
  
} 
  # Data cleaning and bug report --------------------------------------
  # Visualising the data directly ( for eyeballing purposes)
  source("Scripts/ReadingData/VisualisingRawTraces.R")
  # debug statements helping us find error and clean the data
  # source("Scripts/ReadingData/DegugReport.R")
  
  # Read the data together  and aline the data sources ------------------
  source("Scripts/ReadingData/CreatingAlignedData.R")
  
  # delete the files that don't matter (outside of the sampling days of interest)
  all_files <- list.files(file.path(collar, "ArtemisAlignedChunked"), full.names = TRUE, recursive = TRUE)
  start_date <- sampling_times %>% filter(Name == basename(collar)) %>% pull(StartDate)
  end_date <- sampling_times %>% filter(Name == basename(collar)) %>% pull(EndDate)
  file_dates <- as.Date(stringr::str_extract(all_files, "\\d{4}-\\d{2}-\\d{2}"))
  selected_files <- all_files[file_dates >= start_date & file_dates <= end_date]
  file.remove(setdiff(all_files, selected_files))
  
  
  # get the metadata for each of the videos -----------------------------
  source("Scripts/CreatingAnnotations/ExtractingVideoMetadata.R")
  
  # now align with the videos roughly... this is quite manual -----------
  # source("Scripts/CreatingAnnotations/RoughAlignment.R")
  
  # make the 

}
