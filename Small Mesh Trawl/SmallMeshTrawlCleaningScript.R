######################################################################
#####            Small Mesh Trawl Cleaning Script                #####
#####     Script by Colette Ward (ward@nceas.ucsb.edu)           #####
#####                       October 2015                         #####
######################################################################

# Load packages
library(httr)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)


# 1. Manipulate Metadata table to extract samples for Wide Bay:

# Load metadata ("adfgSmallmeshHaul.csv")
URL_SMTh <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7ud09TSDdMeWV3TGs"
SMThGet <- GET(URL_SMTh)
SMTh1 <- content(SMThGet, as='text')
SMTh <- read.csv(file=textConnection(SMTh1),stringsAsFactors=F)
head(SMTh)
str(SMTh)


# Split date information into month, day, year; clean up column names
SMTmetadata=SMTh %>%
  mutate(Date=as.Date(fish_date, "%m/%d/%y")) %>%   # output is reordered as yyyy-mm-dd
  mutate(year1=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[1])) %>%
  mutate(year=as.numeric(year1)) %>%
  mutate(month1=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[2])) %>%
  mutate(month=as.numeric(month1)) %>%
  mutate(day1=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[3])) %>%
  mutate(day=as.numeric(day1)) %>%
  rename(lat = lat_start) %>%
  rename(lon = lon_start) %>%
  rename(startHr = start_hour) %>%
  rename(duration = duration.hr.) %>%
  rename(distance = distance.km.) %>%
  rename(bottomDepth = bottom_depth.m.) %>%
  rename(gearTemp = gear_temp.c.) %>%
  select(-fish_date, -Date, -year1, -month1, -day1, -lat_end, -lon_end)
View(SMTmetadata)
str(SMTmetadata)



