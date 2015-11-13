# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# R Programming
# Coursera; Johns Hopkins Bloomberg School of Public Health
#           Baltimore, Maryland, U.S.
#
# Week 02 Assignment: Air Pollution, Part 3, reports the number of completely
#                     observed cases in each data file
#     Submission Due: 15-Nov-2015 1630 ET
#
# Template: Johns Hopkins University
#   Author: Peter Blackmore
#    Maxim: lathe biosas
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

source("complete.R")
corr <- function(directory, threshold = 0) {
    # Takes a directory of data files and a threshold for complete cases and
    # calculates the correlation between sulfate and nitrate for monitor locations
    # where the number of completely observed cases (on all variables) is GREATER
    # THAN the threshold.
    #
    # Args:
    #   directory: String; File system folder name that contains references source
    #              data files for the pollution monitors. The forward slash 
    #              separator is not required at the end of the directory string.
    #   threshold: Vector; Numeric vector of length 1 indicating the number of
    #              completely observed observations (on all variables) required
    #              to compute the correlation between nitrate and sulfate; 
    #              the default is 0.
    # Returns:
    #   A numeric vector of correlations.
    #
    so4nit.cor <- numeric(0)
    mon.data   <- complete(directory)
    mon.data   <- subset(mon.data, mon.data[,"nobs"] > threshold)

    if (nrow(mon.data) != 0) {
        mon.data   <- mon.data[,"id"]
        f.ext      <- ".csv"
        for (i in mon.data) {
            
            if (i < 10) {
                id.padding <- "00"
            }
            else if (i < 100) {
                id.padding <- "0"
            }
            else {
                id.padding <- ""
            }
            
            src.file <- paste(id.padding, toString(i), f.ext, sep = "")
            filename <- file.path(directory, src.file)
            
            if (file.exists(filename)) {
                p.data <- read.csv(filename)
                p.data <- na.omit(p.data)
                so4nit.cor <- c(so4nit.cor, cor(p.data[,"sulfate"], p.data[,"nitrate"]))
            }
        }
    }

    so4nit.cor
}
