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

# Read the data together  and aline the data sources ---------------------
source("Scripts/ReadingData/CreatingAlignedData.R")

# Extract sections relevant to each video ---------------------------------
# get the metadata for each of the videos # for now just works with the drone data
source("Scripts/CreatingAnnotations/ExtractingVideoMetadata.R")
# now align with the videos roughly... this is quite manual
source("Scripts/CreatingAnnotations/RoughAlignment.R")
