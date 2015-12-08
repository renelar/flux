# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# R Programming
# Coursera; Johns Hopkins Bloomberg School of Public Health
#           Baltimore, Maryland, U.S.
#
# Programming Assignment 3: Hospital Quality
#           Submission Due: 29-Nov-2015 1930 EST
#
#      Author: Peter Blackmore
#       Maxim: lathe biosas
# Style Guide: https://google.github.io/styleguide/Rguide.xml
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# *************************************************
#               !!! REMINDER !!!                  *
# setwd("C:/Users/Kinective/DSS-repo/asgmt3data") *
#                                                 *    
# *************************************************

makeDataCache <- function(filename) {
    
    ## Read the outcome of care measures data from file
    raw.data <- read.csv(filename, colClasses = "character")
    o.data   <- NULL
    
    # Create the subset of data required for this function.
    # Numeric data is in character format and has to be converted.
#     ha.rate <- suppressWarnings(lapply(raw.data[11], as.numeric))
#     hf.rate <- suppressWarnings(lapply(raw.data[17], as.numeric))
#     pn.rate <- suppressWarnings(lapply(raw.data[23], as.numeric))
#     o.data  <- data.frame(raw.data[2], raw.data[7],
#                           ha.rate, hf.rate, pn.rate)
#     rm(ha.rate, hf.rate, pn.rate)
#     
#     # Simplified column names for the subset of outcome data
#     col.names <- c("Hosptial.Name", "State", "HA.Rate", "HF.Rate", "Pn.Rate")
#     colnames(o.data) <- col.names
#     
#     o.data

#     set <- function() {
#         print("CC")
        # Create the subset of data required for this function.
        # Numeric data is in character format and has to be converted.
#         ha.rate <- suppressWarnings(lapply(raw.data[11], as.numeric))
#         hf.rate <- suppressWarnings(lapply(raw.data[17], as.numeric))
#         pn.rate <- suppressWarnings(lapply(raw.data[23], as.numeric))
#         o.data  <- data.frame(raw.data[2], raw.data[7],
#                               ha.rate, hf.rate, pn.rate)
#         rm(ha.rate, hf.rate, pn.rate)
#         
#         # Simplified column names for the subset of outcome data
#         col.names <- c("Hosptial.Name", "State", "HA.Rate", "HF.Rate", "Pn.Rate")
#         colnames(o.data) <- col.names
# 
#         o.data
#     }

    get <- function() {raw.data}
    
    list(get = get)
}

best <- function(state, outcome) {
    # Reads the outcome-of-care-measures.csv file and returns a character
    # vector with the name of the hospital that has the best (i.e. lowest)
    # 30-day mortality for the specifed outcome in that state.
    #
    # The hospital name is the name provided in the Hospital.Name column of
    # the outcome-of-care-measures.csv file. Hospitals that do not have data
    # on a particular outcome are be excluded from the set of hospitals when
    # deciding the rankings.
    #
    # If there is a tie for the best hospital for a given outcome, the
    # hospital names are be sorted in alphabetical order and the first hospital
    # in that set is chosen (i.e. if hospitals B, C, and F are tied for best,
    # then hospital B is returned).
    #
    # Args:
    #     state: Character vector; 2-character state code in which the
    #            hospital is geographically located. Column 7 in the 
    #            outcome-of-care-measures.csv file.
    #   outcome: Clinical condition: The values are one of:
    #             - Heart Attack
    #             - Heart Failure
    #             - Pneumonia
    # Returns:
    #   Single character vector with the name of a hospital that has the 
    #   lowest 30-day mortality rate for the given clinical condition.
    
    # Character vector with Valid names of conditions that can
    # be used as outcome parameter to the function.
    condition <- c("heart attack", "heart failure", "pneumonia")
    
    ## Read the outcome of care measures data from file
    raw.data <- read.csv("outcome-of-care-measures.csv",
                         colClasses = "character")

    # Create the subset of data required for this function.
    # Numeric data is in character format and has to be converted.
    h.name  <- lapply(raw.data[2], as.character)
    st.name <- lapply(raw.data[7], as.character)
    ha.rate <- suppressWarnings(lapply(raw.data[11], as.numeric))
    hf.rate <- suppressWarnings(lapply(raw.data[17], as.numeric))
    pn.rate <- suppressWarnings(lapply(raw.data[23], as.numeric))
    o.data  <- data.frame(h.name, st.name, ha.rate, hf.rate, pn.rate)
    
    # Simplified column names for the subset of outcome data
    col.names <- c("Hospital.Name", "State", "HA.Rate", "HF.Rate", "Pn.Rate")
    colnames(o.data) <- col.names

    # Check that state and outcome parameters are valid.
    if (nrow(o.data[o.data$State == state, ]) == 0) {
        stop("invalid state")
    } else if (length(condition[condition == outcome]) == 0) {
        stop("invalid outcome")
    }

    # Select a state outcomes subset of outcome data
    # based on a state designator.
    so.data <- o.data[o.data$State == state, ]

    # Get the index value of the condition parameter from the list
    # of valid clinical conditions. Use this index to find the
    # name of the mortality rate data column related to the requested
    # condition.
    # Sort the state outcomes data subset in ascending order by (1) state code
    # and (2) mortality rate of the clinical condition given in the function's
    # outcome parameter. Use the index vector to further subset the state
    # outcomes data to only rows with that state code passed to the function.
    i <- which(condition == outcome)
    so.data <- so.data[, c(col.names[1], col.names[2], col.names[i + 2])]
    csort <- col.names[i + 2]
    index <- order(so.data[[csort]], so.data$Hospital.Name)
    so.data <- so.data[index, ]
    rm(i, index)

    if (is.na(so.data[[1, csort]])) {
        rtn <- "No data"
    } else {
        ## Return hospital name in that state with lowest 30-day death rate
        as.character(so.data[1,col.names[1]])
    }
}

# *****************************************************************************
# TEST SCRIPT(s)
#
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
# > best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome

# --< end of file >------------------------------------------------------------
