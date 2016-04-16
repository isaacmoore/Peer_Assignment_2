# Loading libraries --------------------
library(dplyr)
library(ggplot2)
library(tidyr)
# -------------------- Data Processing --------------------
# Set Working Directory --------------------
wd <- setwd("~/Google Drive/Data Science/Coursera - John Hopkins University/Course 5 - Reproducible Research/Week 4/Peer Assignment 2")
# URL of the file to download --------------------
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
# download the file to working directory and name the file "stormdata.csv.bz2" (same file extension as the original)
download.file(fileURL, destfile = paste0(wd, "/stormdata.csv.bz2"), method = "curl")
# read the downloaded file in the enviroment and use the variable "data"
data <- tbl_df(read.csv("stormdata.csv.bz2", header = TRUE, sep = ",", stringsAsFactors = FALSE))
# View to see what the data looks like --------------------
str(data)
View(data)
# -------------------- Data Processing --------------------
# Conversion of the date and time variables to change the class from a factor to date/time. 
data$BGN_DATE <- as.Date(data$BGN_DATE, "%m/%d/%Y")
# Double-check class for Date --------------------
class(data$BGN_DATE)
# Thousands, Millions, Billions
data[data$PROPDMGEXP == "K", ]$PROPDMG <- data[data$PROPDMGEXP == "K", ]$PROPDMG * 1000
data[data$PROPDMGEXP == "M", ]$PROPDMG <- data[data$PROPDMGEXP == "M", ]$PROPDMG * 1000000
data[data$PROPDMGEXP == "m", ]$PROPDMG <- data[data$PROPDMGEXP == "m", ]$PROPDMG * 1000000
data[data$PROPDMGEXP == "B", ]$PROPDMG <- data[data$PROPDMGEXP == "B", ]$PROPDMG * 1000000000
data[data$CROPDMGEXP == "K", ]$CROPDMG <- data[data$CROPDMGEXP == "K", ]$CROPDMG * 1000
data[data$CROPDMGEXP == "k", ]$CROPDMG <- data[data$CROPDMGEXP == "k", ]$CROPDMG * 1000
data[data$CROPDMGEXP == "M", ]$CROPDMG <- data[data$CROPDMGEXP == "M", ]$CROPDMG * 1000000
data[data$CROPDMGEXP == "m", ]$CROPDMG <- data[data$CROPDMGEXP == "m", ]$CROPDMG * 1000000
data[data$CROPDMGEXP == "B", ]$CROPDMG <- data[data$CROPDMGEXP == "B", ]$CROPDMG * 1000000000

# -------------------- Results --------------------
fatalities <- data %>% select(EVTYPE, FATALITIES) %>%
        group_by(EVTYPE) %>%
        mutate(sum_fatalities = (sum(FATALITIES))) %>%
        filter(sum_fatalities >= 1)
fatalities <- distinct(fatalities) %>%
        arrange(desc(sum_fatalities))
# -------------------- Alternate --------------------
fatalities <- data %>% group_by(EVTYPE) %>%
        summarise(total_fatalities = sum(FATALITIES)) %>%
        arrange(desc(total_fatalities)) %>%
        filter(total_fatalities >= 1)
fatalities

injuries <- data %>% group_by(EVTYPE) %>%
        summarise(total_injuries = sum(INJURIES)) %>%
        arrange(desc(total_injuries)) %>%
        filter(total_injuries >= 1)
injuries

prop_dmg <- data %>% group_by(EVTYPE) %>%
        summarise(total_propdmg = sum(PROPDMG)) %>%
        arrange(desc(total_propdmg)) %>%
        filter(total_propdmg >= 1)
prop_dmg

crop_dmg <- data %>% group_by(EVTYPE) %>%
        summarise(total_cropdmg = sum(CROPDMG)) %>%
        arrange(desc(total_cropdmg)) %>%
        filter(total_cropdmg >= 1)
crop_dmg
# Create plot data --------------------
plot_data <- data %>% select(EVTYPE, FATALITIES, INJURIES, BGN_DATE) %>%
		filter(EVTYPE == "TORNADO")
# Fatalities Plot --------------------
ggplot(plot_data, aes(BGN_DATE, FATALITIES))+
        geom_line()+
        ggtitle("Tornado Fatalities by Year")+
        xlab("Year")+
        ylab("Number of Fatalities")
# Injuries Plot --------------------
ggplot(plot_data, aes(BGN_DATE, INJURIES))+
        geom_line()+
        ggtitle("Tornado Injuries by Year")+
        xlab("Year")+
        ylab("Number of Injuries")


