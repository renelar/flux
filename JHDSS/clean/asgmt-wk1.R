# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Getting and Cleaning Data
# Coursera; Johns Hopkins Bloomberg School of Public Health
#           Baltimore, Maryland, U.S.
#
#    Quiz Week 1: Data collection
# Submission Due: 2015-DEC-13 7:30 PM EST
#
#      Author: Peter Blackmore
#       Maxim: lathe biosas
# Style Guide: https://google.github.io/styleguide/Rguide.xml
# -----------------------------------------------------------------------------
#  Environment
#      CPU: Intel i5-650 @ 3.20-MHz (dual core)
#      RAM: 8.00-GB
#       OS: Windows 7 64-bit SP1
#        R: 3.2.2, 64-bit
#   Editor: RStudio 0.99.486
# Packages: data.table 1.9.6, lubridate 1.5.0, xlsx 0.5.7
#           RCurl 1.95-4.7, XML 3.98-1.3
# -----------------------------------------------------------------------------
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# Question 1
# ----------
# In the 2006 American Community Survey (ACS) microdata survey about housing
# for the state of Idaho, how many properties are worth $1,000,000 or more?
# Data dictionary: column VAL = Property value

require(lubridate)
require(data.table)

current.wd <- getwd()
setwd("C:\\Users\\Kinective\\RProjects\\JHDSS\\03-clean")

file.url <- paste0("https://d396qusza40orc.cloudfront.net/",
                   "getdata%2Fdata%2Fss06hid.csv")
# Include download time stamp in destination filename 
instant    <- with_tz(now(), tzone = "GMT")
time.stamp <- format(instant, "%Y%m%b%d-%H%M%Z")
csv.file <- paste0("./data/getdata_Fdata_Fss06hid-", time.stamp, ".csv")
download.file(file.url, destfile = csv.file)

ACSdata <- read.csv(csv.file)
setwd(current.wd)

ACSsurvey <- as.data.table(ACSdata)   # Convert to data.table
ACSsurvey[,.N, by = VAL][order(VAL)]  #

# Question 3
# ----------
require(xlsx)

current.wd <- getwd()
setwd("C:\\Users\\Kinective\\DSS-repo\\JHDSS\\clean")

# Natural Gas Aquisition Program
file.url <- paste0("https://d396qusza40orc.cloudfront.net/",
                   "getdata%2Fdata%2FDATA.gov_NGAP.xlsx") 
time.stamp <- with_tz(now(), tzone = "GMT")
data.downloaded <- format(time.stamp, "%Y%m%b%d-%H%M%Z")
csv.file <- paste0("./data/getdata_Fdata_FDATA.gov_NGAP-", 
                   data.downloaded, ".xlsx")
# !! See http://bit.ly/1NStnA8
download.file(file.url, csv.file, mode = "wb")

dat <- read.xlsx(csv.file, sheetIndex = 1, 
                 rowIndex = c(18:23), colIndex = c(7:15))
setwd(current.wd)

# What is the value of: sum(dat$Zip*dat$Ext,na.rm=T)
sum(dat$Zip*dat$Ext,na.rm=T)

# Question 4
# ----------
require(RCurl)
require(XML)

current.wd <- getwd()
setwd("C:\\Users\\Kinective\\DSS-repo\\JHDSS\\clean")
file.url <- paste0("https://d396qusza40orc.cloudfront.net/",
                   "getdata%2Fdata%2Frestaurants.xml") 

# !! See http://bit.ly/1lpuU4i
temp <- getURL(file.url, ssl.verifyPeer=FALSE)
setwd(current.wd)

doc <- xmlTreeParse(temp, useInternalNodes = TRUE)
rootNode <- xmlRoot(doc)

# How many restaurants have zipcode 21231? 
x <- xpathSApply(rootNode, "//zipcode", xmlValue)

# Question 5
# ----------
require(lubridate)
require(data.table)

current.wd <- getwd()
setwd("C:\\Users\\Kinective\\DSS-repo\\JHDSS\\clean")

# American Community Survey, 2006 microdata survey about housing
# for the state of Idaho
fileURL <- paste0("https://d396qusza40orc.cloudfront.net/",
                  "getdata%2Fdata%2Fss06pid.csv")

instant    <- with_tz(now(), tzone = "GMT")
time.stamp <- format(instant, "%Y%m%b%d-%H%M%Z")
csv.file   <- paste0("./data/getdata_Fdata_Fss06pid-", time.stamp, ".csv")
download.file(fileURL, csv.file)

DT <- fread(csv.file)
setwd(current.wd)

# Which of the following is the fastest way to calculate the average value of
# the variable pwgtp15 broken down by sex using the data.table package? 
system.time(for (i in 1:100) {
    tapply(DT$pwgtp15,DT$SEX,mean)
})
system.time(for (i in 1:100) {
    mean(DT$pwgtp15,by=DT$SEX)
})
# rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
system.time(for (i in 1:100) {
    mean(DT[DT$SEX==1,]$pwgtp15)
    mean(DT[DT$SEX==2,]$pwgtp15)
})
system.time(for (i in 1:100) {
    sapply(split(DT$pwgtp15,DT$SEX),mean)
})
system.time(for (i in 1:100) {
    DT[,mean(pwgtp15),by=SEX]
})

# --< end of file >------------------------------------------------------------
