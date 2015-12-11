# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Exploratory Data Analysis
# Coursera; Johns Hopkins Bloomberg School of Public Health
#           Baltimore, Maryland, U.S.
#
#        Project: Project #1, Making Plots, Plot 1
# Submission Due: 2015-DEC-13 7:30 PM EST
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
# Packages: lubridate 1.5.0, dplyr 0.4.3
# -----------------------------------------------------------------------------
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# ***********************************************************************
#                               NOTE                                    *    
#       Change the DATA.DIR constant to your working directory          *
#                   prior to running this script.                       *
# ***********************************************************************
DATA.DIR  <- "C:\\Users\\Kinective\\RProjects\\JHDSS\\04-explor\\data"


DATA.FILE <- "household_power_consumption.txt"

current.wd <- getwd()
setwd(DATA.DIR)

require(data.table)

if (!file.exists(DATA.FILE)) {

    require(lubridate)

    file.url <- paste0("https://d396qusza40orc.cloudfront.net/",
                       "exdata%2Fdata%2Fhousehold_power_consumption.zip")
                       
    # Include download time stamp in destination filename 
    instant    <- with_tz(now(), tzone = "GMT")
    time.stamp <- format(instant, "%Y%m%b%d-%H%M%Z")
    zip.file <- paste0("exdata_Fdata_Fhousehold_power_consumption-",
                       time.stamp, ".zip")
    download.file(file.url, destfile = zip.file)
    # Ref: Read Compressed Zip Files in R
    # See http://bit.ly/1IUEiSe
    unzip(zip.file)
    
    # Clean up memory
    rm(instant, file.url, zip.file)
}

require(dplyr)
require(data.table)

COL_NAMES <- c("Date", "Time", "Global_active_power", "Global_reactive_power",
               "Voltage", "Global_intensity", "Sub_metering_1",
               "Sub_metering_2", "Sub_metering_3")

# This produces a raw data frame, with all columns as character vectors
# Result = 2075259 obs. of  9 variables
raw.df <- tbl_df(read.table(DATA.FILE, header = TRUE, sep=";", 
                            col.names = COL_NAMES, na.strings = "?",
                            colClasses = "character",
                            fill=TRUE, strip.white=TRUE))

# NOTE: To clean up Time data, use...
# require(chron)
# mutate(Time = chron(time = Time))

# Clean up the data and create a data table, GAP (Global Active Power), with
# only the Date and Global_active_power columns; removed 25,979 NAs from
# Global_active_power column
# Result = 2049280 obs. of  2 variables
GAP <-
    raw.df %>%
        mutate(Date = as.Date(Date, "%d/%m/%Y")) %>%
        mutate(Global_active_power = as.numeric(Global_active_power)) %>%
        subset.data.frame(!is.na(Global_active_power), 
                          select = c(Date, Global_active_power)) %>%
        data.table()

# Select only February 1st and 2nd, 2007
# Result = 2880 obs. of  2 variables
GAP <- GAP[GAP[,(Date >= "2007-02-01" & Date <= "2007-02-02")]]

# Create a a histogram of Global_active_power values & save to .png file
png(filename = "plot1.png")
hist(GAP$Global_active_power, col = "red",
     main = "Global Active Power", xlab = "Global Active Power (kilowats)")
dev.off()

# Reset the working directory
setwd(current.wd)

# --< end of file >------------------------------------------------------------
