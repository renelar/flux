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

makeDataCache <- function() {
    
    CONDITIONS <- c("heart attack", "heart failure", "pneumonia")
    COL.NAMES  <- c("Hospital", "State", "HA.Rate", "HF.Rate", "Pn.Rate")
    
    ## Read the outcome of care measures data from file
    setwd("C:/Users/Kinective/DSS-repo/asgmt3data")
    raw.data <- read.csv("outcome-of-care-measures.csv",
                         colClasses = "character")
    
    h.name  <- lapply(raw.data[2], as.character)
    st.code <- lapply(raw.data[7], as.character)
    # Numeric data is in character format and has to be converted.
    ha.rate <- suppressWarnings(lapply(raw.data[11], as.numeric))
    hf.rate <- suppressWarnings(lapply(raw.data[17], as.numeric))
    pn.rate <- suppressWarnings(lapply(raw.data[23], as.numeric))
    # Create a data frame of the working data set
    wk.set  <- data.frame(h.name, st.code, ha.rate, hf.rate, pn.rate)
    
    colnames(wk.set) <- COL.NAMES
    rm(h.name, st.code, ha.rate, hf.rate, pn.rate)
    d.cached <- TRUE
    
    isCached <- function() {d.cached}
    
    getRates <- function(cond) {
        
        # Check that the outcome parameter is valid.
        if (!(cond %in% CONDITIONS)) {stop("invalid outcome")}
        
        i <- which(CONDITIONS == cond)
        wk.set <- wk.set[c(1:2, i + 2)]
        wk.set <- wk.set[complete.cases(wk.set[3]), ]
        index  <- order(wk.set[[COL.NAMES[2]]], 
                        wk.set[[COL.NAMES[i + 2]]],
                        wk.set[[COL.NAMES[1]]])
        wk.set <- wk.set[index, ]
        rm(index)
        wk.set
    }
    
    list(isCached = isCached, getRates = getRates)
}

rankhospital <- function(state, outcome, num = "best") {
    # Reads the outcome-of-care-measures.csv file and returns a character
    # vector with the name of the hospital that has the best (i.e. lowest)
    # 30-day mortality for the specifed outcome in that state.
    #
    # The hospital name is the name provided in the Hospital.Name column of
    # the outcome-of-care-measures.csv file. Hospitals that do not have data
    # on a particular outcome are be excluded from the set of hospitals when
    # deciding the rankings.
    #
    # If there is a tie for the hospital for a given outcome and ranking, the
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
    #       num: Character vector; ranking of the hospital in the state. The
    #            parameter can take values "best", "worst", or an integer
    #            indicating the ranking (smaller numbers are better).
    # Returns:
    #   Single character vector with the name of a hospital that has the 
    #   ranking specified by the num argument. If the number given by num
    #   parameter is larger than the number of hospitals in that state, then
    #   the function should return NA.

    # Character vector with Valid names of conditions that can
    # be used as outcome parameter to the function.
    # Character vectors with all Valid state codes and names of conditions
    # that can be used as parameters to the function.
    STATES     <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC",
                    "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY",
                    "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT",
                    "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH",
                    "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX",
                    "UT", "VT", "VI", "VA", "WA", "WV", "WI", "WY", "GU")
    CONDITIONS <- c("heart attack", "heart failure", "pneumonia")
    
    COL.NAMES <- c("", "Hospital", "State", "HA.Rate", "HF.Rate", "Pn.Rate")
    
    # Check that the outcome parameter is valid.
    if (!(outcome %in% CONDITIONS)) {stop("invalid outcome")}
    # Check that the rank parameter is valid.
    if (num != "best" & num != "worst") {
        if (!is.numeric(num)) {
            stop("invalid rank")
        }
    }

    ## Read the outcome of care measures data from file
    raw.data <- read.csv("outcome-of-care-measures.csv",
                         colClasses = "character")
    
    # Create the subset of data required for this function.
    # -------------------------------------------------------------------
    st.code <- lapply(raw.data[7], as.character)
    # Check that state parameters is valid.
    if (!(state %in% st.code[[1]])) {stop("invalid state")}
    
    h.name  <- lapply(raw.data[2], as.character)

    # Get the index value of the condition parameter from the list
    # of valid clinical conditions. Use this index to find the
    # name of the mortality rate data column related to the requested
    # condition.
    i <- which(CONDITIONS == outcome)
    # Numeric data is in character format and has to be converted.
    om.rate <- suppressWarnings(lapply(raw.data[5 + (i * 6)], as.numeric))
    wk.set <- data.frame(st.code, h.name, st.code, om.rate)
    # Simplified column names for the subset of outcome data
    COL.NAMES <- c("", "Hospital", "State", "Rate")
    colnames(wk.set) <- COL.NAMES
    
    # Select a state outcomes subset of outcome data
    # based on a state code paramater.
    so.data <- wk.set[wk.set$State == state, ]
    rm(st.code, h.name, om.rate, wk.set)
    
    # Sort the state outcomes data subset in ascending order by (1) state code
    # and (2) mortality rate of the clinical condition given in the function's
    # outcome parameter.
    so.data <- so.data[complete.cases(so.data[4]), ]
    index <- order(so.data[[COL.NAMES[4]]], so.data[[COL.NAMES[2]]])
    so.data <- so.data[index, ]
    rm(index)

    # Return hospital name in that state with the requested
    # ranking on 30-day mortality rate
    if (nrow(so.data) == 0) {
        as.character(NA)
    } else if (num == "best") {
        as.character(so.data[1, COL.NAMES[2]]) 
    } else if (num == "worst") {
        as.character(tail(so.data[[COL.NAMES[2]]], n = 1))
        #as.character(so.data[nrow(so.data), COL.NAMES[2]])
    } else if (num > nrow(so.data)) {
        as.character(NA)
    } else {
        as.character(so.data[num, COL.NAMES[2]])
    }
}

# *****************************************************************************
# TEST SCRIPT(s)
#
# > rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
# > rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"
# > rankhospital("MN", "heart attack", 5000)
# [1] NA

# --< end of file >------------------------------------------------------------
