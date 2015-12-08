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

makeOutcomesCache <- function() {
    # Creates and caches a subset of the outcome of care measures required
    # by the rankhospital function.
    #
    # Returns:
    #   Number of rows in the working set

    COL.NAMES  <- c("Hospital", "State", "HA.Rate", "HF.Rate", "Pn.Rate")
    CONDITIONS <- c("heart attack", "heart failure", "pneumonia")
    
    # Read the outcome of care measures data from file
    # Col  Name/Value
    # -------------------------------------------------------------------------
    #   2  Hospital Name: varchar (50) Lists the name of the hospital.
    #   7  State: varchar (2) Lists the 2 letter State code in which
    #      the hospital is located.
    #  11  Hospital 30-Day Death (Mortality) Rates from Heart Attack: Lists
    #      the risk adjusted rate (percentage) for each hospital.
    #  17  Hospital 30-Day Death (Mortality) Rates from Heart Failure: Lists
    #      the risk adjusted rate (percentage) for each hospital.
    #  23  Hospital 30-Day Death (Mortality) Rates from Pneumonia: Lists the
    #      risk adjusted rate (percentage) for each hospital.

    cwd <- getwd()
    setwd("C:/Users/Kinective/DSS-repo/asgmt3data")
    raw.data <- read.csv("outcome-of-care-measures.csv",
                         colClasses = "character")
    setwd(cwd)
    rm(cwd)
    
    # Create a data frame of the working data set
    wkg.set <- data.frame(raw.data[ 2], raw.data[ 7], raw.data[11],
                          raw.data[17], raw.data[23])
    rm(raw.data)
    colnames(wkg.set) <- COL.NAMES
    # Numeric data is in character format and has to be converted.
    wkg.set[3:5] <- suppressWarnings(lapply(wkg.set[3:5], as.numeric))
    # Remove rows where all rate values are NA
    wkg.set <- wkg.set[-which(apply(wkg.set[,3:5],1,
                                    function(x) all(is.na(x)))),]
    d.cached <- TRUE
    nrow(wkg.set)
    
    isCached <- function() { d.cached }
    
    getRates <- function(cond) {
        # Subsets the outcome by measures data (wkg.set) by condition,
        # removing NA from the numeric values and sorting the resulting
        # data frame in ascending order by State, Rate, and Hospital.
        
        # Check that the outcome parameter is valid.
        if (!(cond %in% CONDITIONS)) {stop("invalid outcome")}
        
        i <- which(CONDITIONS == cond)
        con.set <- wkg.set[c(1:2, i + 2)]
        con.set <- con.set[complete.cases(con.set[3]), ]
        index   <- order(con.set[[COL.NAMES[2]]], 
                         con.set[[COL.NAMES[i + 2]]],
                         con.set[[COL.NAMES[1]]])
        con.set <- con.set[index, ]
        rm(index)
        con.set
    }
    
    list(isCached = isCached, getRates = getRates)
}

outcomes <- makeOutcomesCache()

rankhosp <- function(state, outcome, num = "best") {
    CONDITIONS <- c("heart attack", "heart failure", "pneumonia")
    COL.NAMES  <- c("Hospital", "State", "HA.Rate", "HF.Rate", "Pn.Rate")
    
    # Check that the outcome parameter is valid.
    if (!(outcome %in% CONDITIONS)) {stop("invalid outcome")}
    # Check that the rank parameter is valid.
    if (num != "best" & num != "worst") {
        if (!is.numeric(num)) {
            stop("invalid rank")
        }
    }

    cwd <- getwd()
    setwd("C:/Users/Kinective/DSS-repo/asgmt3data")
    raw.data <- read.csv("outcome-of-care-measures.csv",
                         colClasses = "character")
    setwd(cwd)
    rm(cwd)
    
    # Create a data frame of the working data set
    wkg.set <- data.frame(raw.data[ 2], raw.data[ 7], raw.data[11],
                          raw.data[17], raw.data[23])
    rm(raw.data)
    colnames(wkg.set) <- COL.NAMES
    # Numeric data is in character format and has to be converted.
    wkg.set[3:5] <- suppressWarnings(lapply(wkg.set[3:5], as.numeric))
    # Remove rows where all rate values are NA
    wkg.set <- wkg.set[-which(apply(wkg.set[,3:5],1,
                                    function(x) all(is.na(x)))),]
    
    # Retrieve outcome data for the stated condition
    i <- which(CONDITIONS == outcome)
    con.set <- wkg.set[c(1:2, i + 2)]
    con.set <- con.set[complete.cases(con.set[3]), ]
    index   <- order(con.set[[COL.NAMES[2]]], 
                     con.set[[COL.NAMES[i + 2]]],
                     con.set[[COL.NAMES[1]]])
    so.data <- con.set[index, ]
    rm(index, con.set)

    so.data <- so.data[so.data$State == state, ]
    
    # Return hospital name in that state with the requested
    # ranking on 30-day mortality rate
    if (nrow(so.data) == 0) {
        data.frame("hospital" = as.character(NA),
                   "state" = state, row.names = state)
    } else if (num == "best") {
        data.frame("hospital" = as.character(so.data[1, COL.NAMES[1]]),
                   "state" = state, row.names = state)
    } else if (num == "worst") {
        data.frame("hospital" = as.character(tail(so.data[[COL.NAMES[1]]],
                                                  n = 1)),
                   "state" = state, row.names = state)
    } else if (num > nrow(so.data)) {
        data.frame("hospital" = as.character(NA),
                   "state" = state, row.names = state)
    } else {
        data.frame("hospital" = as.character(so.data[num, COL.NAMES[1]]),
                   "state" = state, row.names = state)
    }
    
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
    COL.NAMES <- c("Hospital", "State", "Rate")
    
    # Check that the outcome parameter is valid.
    if (!(outcome %in% CONDITIONS)) {stop("invalid outcome")}
    # Check that the rank parameter is valid.
    if (num != "best" & num != "worst") {
        if (!is.numeric(num)) {
            stop("invalid rank")
        }
    }
    
    # Retrieve outcome data for the stated condition
    so.data <- outcomes$getRates(outcome)
    so.data <- so.data[so.data$State == state, ]
    
    # Return hospital name in that state with the requested
    # ranking on 30-day mortality rate
    if (nrow(so.data) == 0) {
        data.frame("hospital" = as.character(NA),
                   "state" = state, row.names = state)
    } else if (num == "best") {
        data.frame("hospital" = as.character(so.data[1, COL.NAMES[1]]),
                   "state" = state, row.names = state)
    } else if (num == "worst") {
        data.frame("hospital" = as.character(tail(so.data[[COL.NAMES[1]]],
                                                  n = 1)),
                   "state" = state, row.names = state)
    } else if (num > nrow(so.data)) {
        data.frame("hospital" = as.character(NA),
                   "state" = state, row.names = state)
    } else {
        data.frame("hospital" = as.character(so.data[num, COL.NAMES[1]]),
                   "state" = state, row.names = state)
    }
    
}

rankall <- function(outcome, num = "best") {
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
    #   outcome: Clinical condition: The values are one of:
    #             - Heart Attack
    #             - Heart Failure
    #             - Pneumonia
    #       num: Character vector; ranking of the hospital in the state. The
    #            parameter can take values "best", "worst", or an integer
    #            indicating the ranking (smaller numbers are better).
    # Returns:
    #   returns a 2-column data frame containing the hospital in each state
    #   that has the ranking specified in num.. If the number given by num
    #   parameter is larger than the number of hospitals in that state, then
    #   the function should return NA.

    CONDITIONS <- c("heart attack", "heart failure", "pneumonia")

    # Check that the outcome parameter is valid.
    if (!(outcome %in% CONDITIONS)) { stop("invalid outcome") }
    # Check that the rank parameter is valid.
    if (num != "best" & num != "worst") {
        if (!is.numeric(num)) {
            stop("invalid rank")
        }
    }
    rslt <- data.frame("hospital"=character(), "state"=character())
    
    # Retrieve outcome data for the stated condition
    s.data <- outcomes$getRates(outcome)
    s.data <- unique(s.data$State)
    for (st in s.data) {
        r <- rankhospital(st, outcome, num)
        rslt <- rbind(rslt, r)
    }
    rslt
}

# *****************************************************************************
# TEST SCRIPT(s)
#
# > head(rankall("heart attack", 20), 10)
#                               hospital state
# AK                                <NA>    AK
# AL      D W MCMILLAN MEMORIAL HOSPITAL    AL
# AR  ARKANSAS METHODIST MEDICAL CENTER     AR
# AZ JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
# CA               SHERMAN OAKS HOSPITAL    CA
# CO            SKY RIDGE MEDICAL CENTER    CO
# CT             MIDSTATE MEDICAL CENTER    CT
# DC                                <NA>    DC
# DE                                 <NA>   DE
# FL       SOUTH FLORIDA BAPTIST HOSPITAL   FL
# > tail(rankall("pneumonia", "worst"), 3)
#                                      hospital state
# WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
# WV                     PLATEAU MEDICAL CENTER    WV
# WY           NORTH BIG HORN HOSPITAL DISTRICT    WY
# > tail(rankall("heart failure"), 10)
#                                                             hospital state
# TN                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
# TX                                        FORT DUNCAN MEDICAL CENTER    TX
# UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
# VA                                          SENTARA POTOMAC HOSPITAL    VA
# VI                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
# VT                                              SPRINGFIELD HOSPITAL    VT
# WA                                         HARBORVIEW MEDICAL CENTER    WA
# WI                                    AURORA ST LUKES MEDICAL CENTER    WI
# WV                                         FAIRMONT GENERAL HOSPITAL    WV
# WY                                        CHEYENNE VA MEDICAL CENTER    WY

# --< end of file >------------------------------------------------------------
