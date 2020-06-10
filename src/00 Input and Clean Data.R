# READ ME
# DO NOT DELETE OR EDIT THIS FILE!
# run this script at the beginning of all subsequent analyses to input data
# to this effect, use the 'source' function at the start of the script contatining your analysis

# Load packages

library(abcdee)
library(readxl)

# Input LAM data

path <- "./data/Activity/R01/"

R01M11 <- load.LAM(file = paste0(path, "R01M11.txt"), name = "R01M11")
R01M12 <- load.LAM(file = paste0(path, "R01M12.txt"), name = "R01M12")

LAMs <- list(M11 = R01M11,
             M12 = R01M12)

## Input Metadata

Metadata <- read_excel("data/Activity/R01/Metadata/Metadata_R01_Nutition.xlsx")

saveRDS(object = Metadata, file = paste0(path, "Metadata.rds"))

## Consolidate LAM data with Metadata

source("./src/fun/consolidate.LAMs.R") # this function will be included in 'abcdee' in due course

activity <- consolidate.LAMs(LAMs, Metadata)

saveRDS(object = activity, file = paste0(path, "Activity.rds"))

# Clean .GlobalEnv

rm(R01M11); rm(R01M12)

rm(path)
