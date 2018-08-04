# Note: This code is adapted from https://stackoverflow.com/questions/41954183/how-can-i-extract-multiple-zip-files-and-read-those-csvs-in-r

# Load packages to extend base R
library("plyr")

# Select zip files to be extracted
zipF <- list.files(path = choose.dir(), pattern = "*.zip", full.names = TRUE)

# Extract zip files to directory of your choice
ldply(.data = zipF, .fun = unzip, exdir = choose.dir())
