# READ ME
# DO NOT DELETE OR EDIT THIS FILE!
# run this script at the beginning of all subsequent analyses to input data
# to this effect, use the 'source' function at the start of the script contatining your analysis

# Load packages

library(abcdee)
library(readxl)

# Input LAM data

path <- "./data/Activity/R02/"

R02M11 <- load.LAM(file = paste0(path, "R02M11.txt"), name = "R02M11")
R02M12 <- load.LAM(file = paste0(path, "R02M12.txt"), name = "R02M12")

LAMs <- list(M11 = R02M11,
             M12 = R02M12)

## Input Metadata

Metadata <- read_excel("data/Activity/R02/Metadata/Metadata_R02_Nutrition.xlsx")

saveRDS(object = Metadata, file = paste0(path, "Metadata.rds"))

## Consolidate LAM data with Metadata

source("./src/fun/consolidate.LAMs.R") # this function will be included in 'abcdee' in due course

activity <- consolidate.LAMs(LAMs, Metadata)

saveRDS(object = activity, file = paste0(path, "Activity.rds"))

# Clean .GlobalEnv

rm(R02M11); rm(R02M12)

rm(path)
