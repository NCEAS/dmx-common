#######################################################################
#####   Seward Line Small Zooplankton (<0.3mg) Cleaning Script   ######
#####     Script by Colette Ward (ward at nceas.ucsb.edu)        ######
#####                      August 2015                           ######
#######################################################################

## load packages
library(dplyr)
library(httr)
library(ggplot2)

# A. Load datasets

# A.i  Load CalVET net (meshSize = 149 um) 1998-2011 from NCEAS' AOOS site:
#URL_SCZo <- "url here"
#SCZoGet <- GET(URL_SCZo)
#SCZo1 <- content(SCZoGet, as='text')
#SCZo <- read.csv(file=textConnection(SCZo1),stringsAsFactors=F, strip.white=TRUE)
#head(SCZo)


# problem with dates in online file (compared with https://www.sfos.uaf.edu/sewardline/results/DataByCruise2.html)
# Temporarily use locally-stored file with corrected dates until changes are verified by Russ Hopcroft
setwd("~/Google Drive/GoA project/Data/Datasets/Data Packages/AOOS NCEAS Packages/Seward Line Zooplankton 97-11")
SCZo.a=read.csv('df35b.56.4-ltopZooplanktonData_CW corrected TXF06 date.csv', header=T, stringsAsFactors=F, strip.white=TRUE)
head(SCZo.a)


# Pull 2012 data from AOOS Ocean Workspace (requires log-in here: https://workspace.aoos.org/login).
# Files online are .xls, so I'm using a .csv saved locally (upload it to our server?)
# Load CalVET net (meshSize = 149 um), 2010-2012:
setwd("~/Google Drive/GoA project/Data/Datasets/Data Packages/AOOS Ocean Workspace Packages/Seward Line Zooplankton 2010-12")
SCZo.b=read.csv('Seward_Zoodata_Calvet_2010_2012_update.csv', header=T, stringsAsFactors=F, strip.white=TRUE)
head(SCZo.b)
str(SCZo.b)
names(SCZo.b)

# 1997-2011 dataset: extract year, month, day, time:
SCZo.a1=SCZo.a %>%
  mutate(time=strsplit(as.character(dateTime),split=" ") %>%
           sapply(function(x) x[2])) %>%
  mutate(date=strsplit(as.character(dateTime),split=" ") %>%
           sapply(function(x) x[1])) %>%
  mutate(Date=as.Date(date, "%m/%d/%y")) %>%   # NB output is reordered as yyyy-mm-dd
  mutate(year=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[1])) %>%
  mutate(Year=as.numeric(year)) %>%
  mutate(month=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[2])) %>%
  mutate(Month=as.numeric(month)) %>%
  mutate(day=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[3])) %>%
  mutate(Day=as.numeric(day)) %>%
  select(-dateTime, -date, -Date, -year, -month, -day, -ship, -notes, -consecStationNum)
head(SCZo.a1)


# Convert variable names in 2010-2012 dataset to those used in 1998-2011 dataset
# and extract samples from 2012
SCZo.b1 = SCZo.b %>%
  rename(cruiseID=Cruise) %>%
  rename(time=Time) %>%
  rename(stationID=Station) %>%
  rename(towDepth=Tow.Depth..m.) %>%
  rename(sonicDepth=Sonic.Depth..m.) %>%
  rename(lat=Latitude..N.) %>%
  rename(lon=Longitude..W.) %>%
  rename(gear=Gear.Type) %>%
  rename(ringDiam=Ring.Diameter..cm.) %>%
  rename(meshSize=Mesh.Size.um) %>%
  rename(towType=Tow.Type) %>%
  rename(sampleVol=Sample.Volume..m3.) %>%
  rename(sciName=Current.Name) %>%
  rename(stage=Life.Stage) %>%
  rename(abundance=Abundance..no.m.3.) %>%
  rename(biomass=Biomass..g.m.3.) %>%
  rename(kingdom=Kingdom) %>%
  rename(phylum=Phylum) %>%
  rename(subphylum=Subphylum) %>%
  rename(class=Class) %>%
  rename(subclass=Subclass) %>%
  rename(infraclass=Infraclass) %>%
  rename(order=Order) %>%
  rename(suborder=Suborder) %>%
  rename(infraorder=Infraorder) %>%
  rename(family=Family) %>%
  rename(genus=Genus) %>%
  rename(species=Species.Name) %>%
  filter(Year==2012)
head(SCZo.b1)


# Bind 1998-2011 and 2012 datasets:
SCZo1 = bind_rows(SCZo.a1, SCZo.b1)
head(SCZo1)
dim(SCZo1) # combined dataset should have dim 37543 31