# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Exploratory Data Analysis
# Coursera; Johns Hopkins Bloomberg School of Public Health
#           Baltimore, Maryland, U.S.
#
#        Project: Project #1, Making Plots
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
# Packages: data.table 1.9.6, lubridate 1.5.0, chron 1.17.1, dplyr 0.4.3
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

COL_NAMES <- c("Date", "Time", "Global_active_power", "Global_reactive_power",
               "Voltage", "Global_intensity", "Sub_metering_1",
               "Sub_metering_2", "Sub_metering_3")
col.headers <- as.vector(read.table(DATA.FILE, header = FALSE, sep=";", 
                                    colClasses = "character", nrows = 1,
                                    fill=TRUE, strip.white=TRUE))

# Create a raw data frame, with all column data as character vectors
# Result = 2075259 obs. of  9 variables
require(dplyr)
raw.df <- tbl_df(read.table(DATA.FILE, header = TRUE, sep=";", 
                            col.names = col.headers, na.strings = "?",
                            colClasses = "character",
                            fill=TRUE, strip.white=TRUE))
x <- raw.df %>%
    subset.data.frame(Date == "1/2/2007" | Date == "2/2/2007",
                      select = c(Date, Time, Global_active_power)) %>%
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>%
    mutate(Date = ymd_hms(paste0(Date, "-", Time))) %>%
    mutate(Global_active_power = as.numeric(Global_active_power))

require(lubridate)
x <- mutate(x, Day = as.character(wday(Date, label = TRUE)))
require(graphics)
plot(x$Date, x$Global_active_power, type = "l", 
     ylab = "Global Active Power (kilowats)", xlab = "")
#axis(1, labels = x$Day)
#plot(x$Global_active_power, type = "l")


