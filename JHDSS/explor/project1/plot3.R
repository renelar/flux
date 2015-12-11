# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Exploratory Data Analysis
# Coursera; Johns Hopkins Bloomberg School of Public Health
#           Baltimore, Maryland, U.S.
#
#        Project: Project #1, Making Plots, Plot 3
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
# Packages: lubridate 1.5.0, dplyr 0.4.3, graphics 3.2.2
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

if (!file.exists(DATA.FILE)) {
    
    file.url <- paste0("https://d396qusza40orc.cloudfront.net/",
                       "exdata%2Fdata%2Fhousehold_power_consumption.zip")
    
    # Include download time stamp in destination filename 
    require(lubridate)
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

col.headers <- as.vector(read.table(DATA.FILE, header = FALSE, sep=";", 
                                    colClasses = "character", nrows = 1,
                                    fill=TRUE, strip.white=TRUE))

# This produces a raw data frame, with all columns as character vectors
# Result = 2075259 obs. of  9 variables
require(dplyr)
raw.df <- tbl_df(read.table(DATA.FILE, header = TRUE, sep=";", 
                            col.names = col.headers, na.strings = "?",
                            colClasses = "character",
                            fill=TRUE, strip.white=TRUE))

    # NOTE: To clean up Time data, use...
    # require(chron)
    # mutate(Time = chron(time = Time))

# Selecting only observations from February 1st and 2nd, 2007
# (Result = 2880 obs.). Clean up the data and create a data frame that has only
# Date, Time, and Sub_metering columns
# (Sub_metering_1, Sub_metering_2, Sub_metering_3).
require(lubridate)
GAP <-
    raw.df %>%
    subset.data.frame(Date == "1/2/2007" | Date == "2/2/2007",
                      select = c(Date, Time,
                                 Sub_metering_1, 
                                 Sub_metering_2, 
                                 Sub_metering_3)) %>%
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>%
    mutate(Date = ymd_hms(paste0(Date, "-", Time))) %>%
    mutate(Sub_metering_1 = as.numeric(Sub_metering_1)) %>%
    mutate(Sub_metering_2 = as.numeric(Sub_metering_2)) %>%
    mutate(Sub_metering_3 = as.numeric(Sub_metering_3))

# Create a line plot of Sub_metering values & save to .png file
require(graphics)
png(filename = "plot3.png")
yrange <- range(c(GAP$Sub_metering_1,GAP$Sub_metering_2,GAP$Sub_metering_3))
line.col <- c("black", "red", "blue")
plot(GAP$Date, GAP$Sub_metering_1, type = "l", col = line.col[1], 
     ylim = yrange, ylab = "Energy sub metering", xlab = "")
par(new = TRUE)
plot(GAP$Date, GAP$Sub_metering_2, type = "l", col = line.col[2],
     ylim = yrange, ylab = "Energy sub metering", xlab = "")
par(new = TRUE)
plot(GAP$Date, GAP$Sub_metering_3, type = "l", col = line.col[3], 
     ylim = yrange, ylab = "Energy sub metering", xlab = "")
legend("topright", legend = colnames(GAP)[3:5], 
       col = line.col, lwd = 2, cex=0.9)
par(new = FALSE)
dev.off()

# Reset the working directory
setwd(current.wd)

# --< end of file >------------------------------------------------------------
