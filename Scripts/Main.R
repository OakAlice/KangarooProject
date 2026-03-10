# Main file for the analysis of the kangaroo data -------------------------

# Set up ------------------------------------------------------------------
setwd("~/KangarooProject")

pacman::p_load(
  tidyverse,
  data.table,
  zoo,
  patchwork
)

sample_rate <- 50

# Reading in the data -----------------------------------------------------
# load in custom functions (helpers)


# Read the data together  and aline the data sources ---------------------
source("Scripts/ReadingData/CreatingAlignedData.R")

# Extract sections relevant to each video ---------------------------------
# for annotating

