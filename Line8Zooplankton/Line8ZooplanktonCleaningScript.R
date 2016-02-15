######################################################
#    Line 8 (Shelikof Strait) Zooplankton Data       #
#                   Cleaning Script                  #
#  Script by Colette Ward (ward at nceas.ucsb.edu)   #
######################################################

## load packages
library(dplyr)
library(httr)
library(lubridate)
library(taxize)


# load the data
URL_l8zoop <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7ublpmc2dkQ3J1eVU"
l8zoopGet <- GET(URL_l8zoop)
l8zoop1 <- content(l8zoopGet, as='text')
l8zoop <- read.csv(file=textConnection(l8zoop1), stringsAsFactors=F, na.strings = "")


names(l8zoop) <- tolower(names(l8zoop)) # convert column names to lower case


l8zoop2 <- l8zoop %>%
  mutate(time=strsplit(gmt_date_time,split=" at ") %>%
           sapply(function(x) x[2])) %>%
  # convert time to 24 hr time (use later for Euphausiid catchability)
  mutate(date=strsplit(gmt_date_time,split=" at ") %>%
           sapply(function(x) x[1])) %>%
  mutate(julianDay = yday(as.Date(date, '%m/%d/%Y'))) %>% # create column of julian day
  mutate(gearDistFromBottom = bottom_depth - max_gear_depth) %>%
  select(-haul_id, -gmt_date_time, -date)
head(l8zoop2)


# Issues to deal with:
# figure out if there are 2 nets (1 & 2) for each haul - why are 1 and 2 noted, 
# and past issue that Janet mentioned re nets being combined already in the database?

# clean up taxon names
# create decision tree re which species / sizes / sexes to use from which mesh size
# EST_NUM_PERM2 and EST_NUM_PERM3 are both  vectors of characters because of "Present"

