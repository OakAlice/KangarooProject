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
  patchwork
)

sample_rate <- 50

# meaningful days
sampling_times <- fread("Metadata/Sampling_Times.csv")

collars <- list.dirs("Data/RawData", recursive = FALSE) # all the ones we want to do
for (collar in collars){
  # Read the data together  and aline the data sources ------------------
  source("Scripts/ReadingData/CreatingAlignedData.R")
  
  # delete the files that don't matter (outside of the sampling days of interest)
  # sampling_start <- sampling_times %>% dplyr::filter(Name == basename(collar)) %>% mutate(Start = as.Date(Start)) %>% pull(Start)
  # sampling_end <- sampling_times %>% dplyr::filter(Name == basename(collar)) %>% mutate(Start = as.Date(End)) %>% pull(Start)
  # unnecesssary_files <- list.files(file.path(base_path, "Output"), pattern = "_3", full.names = TRUE, recursive = TRUE)
  # file.remove(unnecesssary_files)
  
  # get the metadata for each of the videos -----------------------------
  source("Scripts/CreatingAnnotations/ExtractingVideoMetadata.R")
  
  # now align with the videos roughly... this is quite manual -----------
  # source("Scripts/CreatingAnnotations/RoughAlignment.R")
  
  # make the 

}
