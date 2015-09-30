#######################################################################
#####   Seward Line Large Zooplankton (>0.3mg) Cleaning Script   ######
#####        Script by Colette Ward (ward@nceas.ucsb.edu)        ######
#####                     September 2015                         ######
#######################################################################

## load packages
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(httr)
library(ggplot2)
library(taxize)

# Load data: MOCNESS & MultiNet (meshSize = 500 um), 1998-2011:
URL_SMZo <- "http://gulfwatch.nceas.ucsb.edu/goa/d1/mn/v1/object/df35b.61.4"
SMZoGet <- GET(URL_SMZo)
SMZo1 <- content(SMZoGet, as='text')
SMZo <- read.csv(file=textConnection(SMZo1),stringsAsFactors=F, na.strings = c("NA", " ", ""))  # turns empty spots into NA; consider also strip.white
head(SMZo)
str(SMZo)
dim(SMZo)
View(SMZo)


# Check for NAs:
for(i in 1:ncol(SMZo)){
  print(any(is.na(SMZo[,i])))
}

sum(is.na(SMZo$stationID)) # there are 1531 rows with missing stationID

# ------------------------------

# Clean up missing stationID
# create table of unique date/time and stationIDs
dts = SMZo %>%
  select(startDateTime, stationID) %>%
  filter(stationID != "NA") %>%
  distinct()
#View(dts)
any(is.na(dts$stationID)) # test for presence of NA in stationID; should be FALSE

# merge with main file; this replaces missing stationIDs in main file
SMZo1 = merge(dts, SMZo, all.x=T)
#View(SMZo1)
any(is.na(SMZo1$stationID)) # test for presence of NA in stationID; should be FALSE

# ------------------------------

# extract time, day, month, year  and remove empty taxonomic columns
SMZo2 = SMZo %>%
  rename(sciName = specimen) %>%
  mutate(abundance = as.numeric(abundance)) %>% 
  mutate(time=strsplit(as.character(startDateTime),split=" ") %>%
           sapply(function(x) x[2])) %>%
  mutate(date=strsplit(as.character(startDateTime),split=" ") %>%
           sapply(function(x) x[1])) %>%
  mutate(year=strsplit(date,split="-") %>%
           sapply(function(x) x[1])) %>%
  mutate(Year=as.numeric(year)) %>%
  mutate(month=strsplit(date,split="-") %>%
           sapply(function(x) x[2])) %>%
  mutate(Month=as.numeric(month)) %>%
  mutate(day=strsplit(date,split="-") %>%
           sapply(function(x) x[3])) %>%
  mutate(Day=as.numeric(day)) %>%
  select(-ship, -tow, -notes, -kingdom, -Phylum, -Subphylum, - Class, - Subclass, -Infraclass, - Order, 
         -Suborder, -Infraorder, -Family, -Genus, -Species, -startDateTime, -endDateTime, -date, 
         -year, -month, -day) 
# Error "In eval(substitute(expr), envir, enclos) : NAs introduced by coercion"
# This refers to 2 rows in dataset that are mostly NA - ie not in fact an error in code
str(SMZo2)

# ------------------------------

# Clean up the species names
unique(sort(SMZo2$sciName))
SMZo2.c = SMZo2 %>%
  filter(sciName != "NA") %>% # remove rows for which sciName == NA (2 rows) until dataset is checked for source of NA
  mutate(sciName = gsub(" $", "", sciName, perl=T)) %>%  #remove trailing white spaces
  mutate(sciName = gsub("_sp.$", "", sciName)) %>% 
  mutate(sciName = gsub(" sp.$", "", sciName)) %>%
  mutate(sciName = gsub("_", " ", sciName)) %>%
  mutate(sciName = gsub("Metridia ochotensis", "Metridia okhotensis", sciName)) %>% # correct spelling error
  mutate(sciName = gsub("Gaetanus intermedius", "Gaetanus brevispinus", sciName)) %>% # G. brevispinus is the accepted name
  mutate(sciName = gsub("Gaidius", "Gaetanus", sciName)) %>% # Gaetanus is the accepted name
  mutate(sciName = gsub("Fish", "Osteichthyes", sciName)) %>%
  mutate(sciName = gsub("Squid", "Teuthida", sciName)) %>%
  mutate(sciName = gsub("Flatfish", "Osteichthyes", sciName)) %>%
  mutate(sciName = gsub("Parasitic copepoda", "Copepoda", sciName)) %>%
  mutate(sciName = gsub(" copepodids", "", sciName)) %>%
  mutate(stage = gsub("mealopa", "megalopa", stage)) %>% # correct spelling error
  mutate(stage = gsub("glocothoe", "glaucothoe", stage)) %>% # correct spelling error
  mutate(sciName = gsub(" zoea,", "", sciName)) %>% # this information is in stage vector
  mutate(sciName = gsub(" zoea", "", sciName)) %>% # this information is in stage vector
  mutate(sciName = gsub(" i$", "", sciName)) %>% # this information is in stage vector
  mutate(sciName = gsub(" ii$", "", sciName)) %>% # this information is in stage vector
  mutate(sciName = gsub(" iii$", "", sciName)) %>% # this information is in stage vector
  mutate(sciName = gsub(" iv$", "", sciName)) %>% # this information is in stage vector
  mutate(sciName = gsub(" v$", "", sciName)) %>% # this information is in stage vector
  mutate(sciName = gsub("\\(|\\)", "", sciName)) %>% # remove brackets
  mutate(sciName = gsub(" small$", "", sciName)) %>% # this information is in stage vector
  mutate(sciName = gsub(" large$", "", sciName)) %>% # this information is in stage vector
  mutate(sciName = gsub(" echinospira$", "", sciName)) %>%
  mutate(sciName = gsub(" $", "", sciName, perl=T))
unique(sort(SMZo2.c$sciName))
View(SMZo2.c)

# ------------------------------

# Add taxonomic information:

# Query ITIS for higher order taxonomic information:
#SMZo2.c.specieslist <- unique(SMZo2.c$sciName)
#tax.info = tax_name(query = SMZo2.c.specieslist, 
#                    get = c("kingdom", "phylum", "subphylum", "class", "subclass", "infraclass", 
#                            "order", "suborder", "infraorder", "suborder", "infraorder", "family", 
#                            "genus", "species"), 
#                    db = "itis")
#tax.info1 = tax.info %>%
#  mutate(sciName = query) %>%
#  select(-db, -query)
#View(tax.info1)
# The above code to create tax.info takes 16min to run, with sporadic inputs needed
# you can upload the resulting file from here instead:
URL_tax.info1 <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7ucEJFVlV5VnBhVjA"
tax.info1Get <- GET(URL_tax.info1)
tax.info2 <- content(tax.info1Get, as='text')
tax.info1 <- read.csv(file=textConnection(tax.info2),stringsAsFactors=F)
View(tax.info1)

# Merge [dataset with cleaned species names] and [taxonomic information]
SMZo2.d <- merge(SMZo2.c,tax.info1,all.x=T)
View(SMZo2.d)
dim(SMZo2.d) # should be 163391     33
str(SMZo2.d)

