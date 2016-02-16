######################################################
#    Line 8 (Shelikof Strait) Zooplankton Data       #
#                   Cleaning Script                  #
#  Script by Colette Ward (ward at nceas.ucsb.edu)   #
######################################################

## load packages
library(dplyr)
library(httr)
library(lubridate)


# load the data
URL_l8zoop <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7ublpmc2dkQ3J1eVU"
l8zoopGet <- GET(URL_l8zoop)
l8zoop1 <- content(l8zoopGet, as='text')
l8zoop <- read.csv(file=textConnection(l8zoop1), stringsAsFactors=F, na.strings = "")


names(l8zoop) <- tolower(names(l8zoop)) # convert column names to lower case


l8zoop2 <- l8zoop %>%
  mutate(gmt_date_time = gsub(" at", "", gmt_date_time)) %>% # use gsub to remove " at" from time stamps
  mutate(gmt_date_time1 = parse_date_time(gmt_date_time, c('%m/%d/%Y %I:%M:%OS %p'), exact = T, tz = "GMT")) %>% # convert to POSIXct format, 24-hr time, and 
  # (cont) and tell R that dates/times are reported for GMT time zone
  mutate(akst_date_time = with_tz(gmt_date_time1, "Etc/GMT+9")) %>% # convert GMT to Alaska Standard Time
  mutate(time=strsplit(as.character(akst_date_time),split=" ") %>%
           sapply(function(x) x[2])) %>%
  mutate(date=strsplit(as.character(akst_date_time),split=" ") %>%
           sapply(function(x) x[1])) %>%
  mutate(julianDay = yday(as.Date(date, '%Y-%m-%d'))) %>% # create column of julian day
  select(-month, -day) %>% # remove current month and day vectors, because these are for dates in GMT (some dates are inaccurate after accounting for time zone change)
  mutate(month1=strsplit(date,split="-") %>% # next 3 lines: create new month vector for dates in AKST
           sapply(function(x) x[2])) %>%
  mutate(month=as.numeric(month1)) %>%
  mutate(day1=strsplit(date,split="-") %>% # next 3 lines: create new day vector for dates in AKST
           sapply(function(x) x[3])) %>%
  mutate(day=as.numeric(day1)) %>%
  mutate(gearDistFromBottom = bottom_depth - max_gear_depth) %>%
  select(-haul_id, -gmt_date_time, -gmt_date_time1, -akst_date_time, -date, -month1, -day1)
