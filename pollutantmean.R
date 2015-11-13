# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# R Programming
# Coursera; Johns Hopkins Bloomberg School of Public Health
#           Baltimore, Maryland, U.S.
#
# Week 02 Assignment: Air Pollution, Part 1: Calculates mean of a pollutant
#     Submission Due: 15-Nov-2015 1630 ET
#
# Template: Johns Hopkins University
#   Author: Peter Blackmore
#    Maxim: lathe biosas
#
# -----------------------------------------------------------------------------
# FUNCITON: pollutantmean
#
# Reads pollution monitors' particulate matter data from the directory
# specified and calculates the mean of a pollutant (sulfate or nitrate) across
# all monitors. Ignores any missing values coded as NA.
#
# Args:
#   directory: String; File system folder name that contains references source
#              data files for the pollution monitors. The forward slash 
#              separator is not required at the end of the directory string.
#   pollutant: String; Name of a pollutant read from the source data files
#          id: Vector; Monitor identifier(s)
#
# Returns:
#   The mean of the specified pollutant across all requested monitors,
#   ignoring missing values indicators, coded as NA.
#
pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    extension <- ".csv"
    directory <- paste(directory, "/", sep = "")
    
    for (i in id) {
        if (i < 10) {
            id_padding <- "00"
        }
        else if (i < 100) {
            id_padding <- "0"
        }
        else {
            id_padding <- ""
        }
        filename <- paste(directory, id_padding, i, extension, sep = "")
        
        data <- read.csv(filename)
        if (i == id[1]) {
            all_data <- data
        }
        else {
            all_data <- rbind(all_data, data)
        }
    } 
    mean(all_data[[pollutant]], na.rm = TRUE)
}
