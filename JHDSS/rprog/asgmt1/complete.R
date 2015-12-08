# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# R Programming
# Coursera; Johns Hopkins Bloomberg School of Public Health
#           Baltimore, Maryland, U.S.
#
# Week 02 Assignment: Air Pollution, Part 2, reports the number of completely
#                     observed cases in each data file
#     Submission Due: 15-Nov-2015 1630 ET
#
# Template: Johns Hopkins University
#   Author: Peter Blackmore
#    Maxim: lathe biosas
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

complete <- function(directory, id = 1:332) {
    # Reads a directory full of files and reports the number of completely
    # observed cases of pollution monitors' particulate matter data in each file.
    # A "complete case" is defined as the condition where there are no missing
    # value indicators, coded as NA, in any column.
    #
    # Args:
    #   directory: String; File system folder name that contains references source
    #              data files for the pollution monitors. The forward slash 
    #              separator is not required at the end of the directory string.
    #          id: Vector; Monitor identifier(s)
    #
    # Returns:
    #   A data frame of the form:
    #       id nobs
    #       1  117
    #       2  1041
    #        ...
    #   where 'id' is the monitor ID number and 'nobs' is the number
    #   of complete cases.
    #
    extension <- ".csv"
    cnames    <- c("id", "nobs")
    obs_data  <- c()

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
        
        src_file <- paste(id_padding, toString(i), extension, sep = "")
        filename <- file.path(directory, src_file)
        
        if (file.exists(filename)) {
            mtr_data <- read.csv(filename)
            mtr_data <- matrix(c(i, nrow(na.omit(mtr_data))), 1, 2)
            colnames(mtr_data) <- cnames
        }
        
        if (i == id[1]) {
            obs_data <- data.frame(mtr_data)
        }
        else {
            obs_data <- rbind(obs_data, mtr_data)
        }
    }

    obs_data
}
