# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Getting and Cleaning Data
# Coursera; Johns Hopkins Bloomberg School of Public Health
#           Baltimore, Maryland, U.S.
#
#        Project: Wearable Computing Dataset (http://bit.ly/1mEvWTG)
#                 (Ref: http://bit.ly/1BQZaFw)
# Submission Due: 2015-DEC-27 7:30 PM EST
#         Author: Peter Blackmore
#          Maxim: lathe biosas
#    Style Guide: https://google.github.io/styleguide/Rguide.xml
# -----------------------------------------------------------------------------
#  Environment
#      CPU: Intel i5-650 @ 3.20-MHz (dual core)
#      RAM: 8.00-GB
#       OS: Windows 7 64-bit SP1
#        R: 3.2.2, 64-bit
#   Editor: RStudio 0.99.486
# Packages: lubridate 1.5.0, dplyr 0.4.3, graphics 3.2.2
# -----------------------------------------------------------------------------
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# ***********************************************************************
#                               NOTE                                    *    
#       Change the DATA.DIR constant to your working directory          *
#                   prior to running this script.                       *
# ***********************************************************************
DATA.DIR  <- paste0("C:\\Users\\Kinective\\RProjects\\JHDSS\\03-clean\\",
                    "project\\data")

ACTIVITY.LABELS <- "UCI HAR Dataset\\activity_labels.txt"
FEATURES.LABELS <- "UCI HAR Dataset\\features.txt"

TESTDATA.LABELS  <- "UCI HAR Dataset\\test\\y_test.txt"
TESTDATA.SUBJECT <- "UCI HAR Dataset\\test\\subject_test.txt"
TESTDATA.SET     <- "UCI HAR Dataset\\test\\x_test.txt"

current.wd <- getwd()
setwd(DATA.DIR)

if (!file.exists(DATA.FILE)) {
    
    file.url <- paste0("https://d396qusza40orc.cloudfront.net/",
                       "getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")
    
    # Include download time stamp in destination filename 
    require(lubridate)
    instant    <- with_tz(now(), tzone = "GMT")
    time.stamp <- format(instant, "%Y%m%b%d-%H%M%Z")
    zip.file   <- paste0("Dataset-", time.stamp, ".zip")
    download.file(file.url, destfile = zip.file)
    unzip(zip.file)
    
    # Clean up memory
    rm(instant, file.url, zip.file)
}

# Get column header label names
LABEL.COL.NAMES = c("index", "label")
act.labels <- read.table(ACTIVITY.LABELS, header = FALSE,
                         col.names = LABEL.COL.NAMES,
                         colClasses = "character",
                         fill=TRUE, strip.white=TRUE)
ftr.labels <- read.table(FEATURES.LABELS, header = FALSE, 
                         col.names = LABEL.COL.NAMES,
                         colClasses = "character",
                         fill=TRUE, strip.white=TRUE)

# label.ord  <- unlist(lapply(ac.labels[,1], substr, start = 1, stop = 1))
# label.ord  <- as.integer(label.ord)
# label.name <- unlist(lapply(ac.labels[,1], 
#                             function(x) {substr(x, 3, nchar(x))}))
# require(data.table)
# activity.labels <- data.table(label.ord, label.name)
# activity.labels

rm(label.name, label.ord, ac.labels)
test.data <- read.table(TESTDATA.SET, header = FALSE, 
                        colClasses = "character", nrows = 1,
                        fill=TRUE, strip.white=TRUE)


# Reset the working directory
setwd(current.wd)

# --< end of file >------------------------------------------------------------
