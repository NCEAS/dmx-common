# ADF&G Large Mesh Trawl Cleaning Script
# Colette Ward, Nov 2015
#########################################


# load packages
library(httr)
library(dplyr)
library(taxize)

setwd("~/Google Drive/GoA project/Data/Datasets/Data Packages/AOOS NCEAS Packages/Trawl Packages/ADFG Large mesh trawl/adfgTrawlData/largeMeshTrawl")


# load catch data for 1988-1999
adfgLMTa <- read.csv('ALLtrawlCatch1988_1999.csv',header=T,stringsAsFactors=F) 
# convert character vectors to numeric to facilitate binding files together
adfgLMTa1 <- adfgLMTa %>%
  mutate(start_lat = gsub("null", NA, start_lat)) %>% mutate(start_lat = as.numeric(start_lat)) %>%
  mutate(start_lon = gsub("null", NA, start_lon)) %>% mutate(start_lon = as.numeric(start_lon)) %>%
  mutate(end_lat = gsub("null", NA, end_lat)) %>% mutate(end_lat = as.numeric(end_lat)) %>%
  mutate(end_lon = gsub("null", NA, end_lon)) %>% mutate(end_lon = as.numeric(end_lon)) %>%
  mutate(bottom_temp.c. = gsub("null", NA, bottom_temp.c.)) %>% mutate(bottom_temp.c. = as.numeric(bottom_temp.c.)) %>%
  mutate(avg_depth.fm. = gsub("null", NA, avg_depth.fm.)) %>% mutate(avg_depth.fm. = as.numeric(avg_depth.fm.))


# load catch data for 2000-2014
adfgLMTb <- read.csv('ALLtrawlCatch2000_2014.csv',header=T,stringsAsFactors=F) 
# convert character vectors to numeric to facilitate binding files together
adfgLMTb1 <- adfgLMTb %>%
  mutate(bottom_temp.c. = gsub("null", NA, bottom_temp.c.)) %>% mutate(bottom_temp.c. = as.numeric(bottom_temp.c.)) %>%
  mutate(avg_depth.fm. = gsub("null", NA, avg_depth.fm.)) %>% mutate(avg_depth.fm. = as.numeric(avg_depth.fm.)) %>%
  mutate(total_weight.kg. = gsub("null", NA, total_weight.kg.)) %>% mutate(total_weight.kg. = as.numeric(total_weight.kg.))


# load catch data for Marmot 1998-1999
adfgLMTc <- read.csv('ALLtrawlCatchMarmot1998_1999.csv',header=T,stringsAsFactors=F) 
# convert character vectors to numeric to facilitate binding files together
adfgLMTc1 <- adfgLMTc %>%
  mutate(end_lat = gsub("null", NA, end_lat)) %>% mutate(end_lat = as.numeric(end_lat)) %>%
  mutate(end_lon = gsub("null", NA, end_lon)) %>% mutate(end_lon = as.numeric(end_lon)) %>%
  mutate(bottom_temp.c. = gsub("null", NA, bottom_temp.c.)) %>% mutate(bottom_temp.c. = as.numeric(bottom_temp.c.))


# bind all 3 catch files
adfgLMTcatch <- bind_rows(adfgLMTa1, adfgLMTb1, adfgLMTc1)
dim(adfgLMTcatch)  # check: length should be 3651 + 129282 + 64465  [1] 197398


# Clean up the dataset
adfgLMTcatch1 <- adfgLMTcatch %>%
  mutate(Date=as.Date(haul_date, "%m/%d/%Y")) %>%   # output is reordered as yyyy-mm-dd
  mutate(year1=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[1])) %>%
  mutate(year=as.numeric(year1)) %>%
  mutate(month1=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[2])) %>%
  mutate(month=as.numeric(month1)) %>%
  mutate(day1=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[3])) %>%
  mutate(day=as.numeric(day1)) %>%
  rename(survey = survey_name, area = sq_miles, lat = start_lat, lon = start_lon, 
         meanDepth = avg_depth.fm., bottomTemp = bottom_temp.c., raceCode = race_code, 
         catchKg = total_weight.kg.) %>%
  mutate(meta=paste(survey, haul, station, Date)) %>%# create vector of unique identifying info to merge with metadata table
  select(-haul_date, -Date, -year1, -month1, -day1, -end_lat, -end_lon, -tow_distance.nm., -common_name)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Load look-up table of scientific names
setwd("~/Google Drive/GoA project/Data/Datasets/Data Packages/AOOS NCEAS Packages/Trawl Packages")
# load table of NOAA raceCode definitions
spCodes <- read.csv('RACEspCookbook2015.csv',header=T,stringsAsFactors=F)


# go back to current working directory:
setwd("~/Google Drive/GoA project/Data/Datasets/Data Packages/AOOS NCEAS Packages/Trawl Packages/ADFG Large mesh trawl/adfgTrawlData/largeMeshTrawl")


# merge spCodes onto catch data by raceCode
adfgLMTcatch2 <- left_join(adfgLMTcatch1, spCodes, by = "raceCode")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Add higher order taxonomic information

# Filter out natural debris, Clean up Scientific Names
adfgLMTcatch3 = adfgLMTcatch2 %>%
  filter(raceCode != -9) %>% # remove 2 cases of unrecorded taxonomic information
  filter(raceCode != 1) %>% # remove fish eggs unid.
  filter(raceCode != 401) %>% # remove skate egg case unid.
  filter(raceCode != 71001) %>% # remove gastropod eggs (many entries)
  filter(raceCode != 99990) %>% # remove 2 cases of invertebrate unid.
  filter(raceCode != 99993) %>% # remove empty bivalve shells
  filter(raceCode != 99994) %>% # remove empty gastropod shells
  filter(raceCode != 99998) %>% # remove polychaete tubes
  filter(raceCode != 99999) %>% # remove unsorted shab
  mutate(sciName = gsub("\\(|\\)", "", sciName)) %>% # remove parentheses
  mutate(sciName = gsub("Aplidium sp. A Clark 2006 ", "Aplidium", sciName)) %>% 
  mutate(sciName = gsub("juvenile", "", sciName)) %>%  # leaves a trailing space for cod & pollock
  mutate(sciName = gsub("adult", "", sciName)) %>%
  mutate(sciName = gsub(" $", "", sciName, perl=T)) %>%  #remove trailing white spaces
  mutate(sciName = gsub("Platichthys stellatus X Pleuronectes quadrituberculatus hybrid", "Platichthys", sciName)) %>%
  mutate(sciName = gsub("Decapodiformes", "Teuthida", sciName)) %>% # assign Decapodiformes (squid unident.) to Teuthida
  mutate(sciName = gsub("Sebastes melanostictus", "Sebastes aleutianus", sciName)) %>% # ***CAREFUL*** assign Sebastes melanostictus (blackspotted rockfish) to Sebastes aleutianus. See* below
  mutate(sciName = gsub("Selachii", "Euselachii", sciName)) %>% # update for invalid name
  mutate(sciName = gsub("Atheresthes stomias", "Reinhardtius stomias", sciName)) %>% # update to valid name for Arrowtooth flounder
  mutate(sciName = gsub(" sp.$", "", sciName))

# *no record of Sebastes melanostictus in ITIS, probably because it was described in 2008 (split from Rougheye rockfish, Sebastes aleutianus):
#Orr, James W. and Hawkins, Sharon (2008) Species of the rougheye rockfish complex: resurrection of 
#Sebastes melanostictus (Matsubara, 1934) and a redescription of Sebastes aleutianus (Jordan and Evermann, 1898)
#(Teleostei: Scorpaeniformes). Fishery Bulletin, 106(2), pp. 111-134.)
# In pre-2009 samples, Rougheye & Blackspotted are called Sebastes aleutianus, 
#therefore consider calling all corresponding raceCode (30050, 30051, 30052) the same thing post-2008.

# For dusky & dark rockfishes (raceCode 30150, 30151, 30152) Sebastes ciliatus & S. variabilis exist for all years 1988 - 2014; 
# the combined dusky & dark group exists in dataset from 1988 - 2002

# fill in empty species names
for(j in 1:nrow(adfgLMTcatch3)) {
  if(adfgLMTcatch3$raceCode[j] == 74981) {adfgLMTcatch3$sciName[j] <- "Cardiidae"} # cockle unid.
  if(adfgLMTcatch3$raceCode[j] == 59000) {adfgLMTcatch3$sciName[j] <- "Polychaeta"} # bristle worm unid.
  if(adfgLMTcatch3$raceCode[j] == 50010) {adfgLMTcatch3$sciName[j] <- "Sedentaria"} # tube worm unid.
  if(adfgLMTcatch3$raceCode[j] == 50001) {adfgLMTcatch3$sciName[j] <- "Polychaeta"} # worm unid.
  if(adfgLMTcatch3$raceCode[j] == 82730) {adfgLMTcatch3$sciName[j] <- "Clypeasteroida"} # sand dollar unid.
  if(adfgLMTcatch3$raceCode[j] == 66000) {adfgLMTcatch3$sciName[j] <- "Caridea"} # shrimp unid.
  if(adfgLMTcatch3$raceCode[j] == 30150) {adfgLMTcatch3$sciName[j] <- "Sebastes"} # ***CAREFUL*** see note above. assign 3150 (dusky and dark rockfishes unid.) to Sebastes
  if(adfgLMTcatch3$raceCode[j] == 30050) {adfgLMTcatch3$sciName[j] <- "Sebastes aleutianus"} # ***CAREFUL*** see note above. assign 30050 (rougheye and blackspotted rockfish unid.) to Sebastes aleutianus
}



# Query ITIS for higher order taxonomic information:
adfgLMTsp <- unique(sort(adfgLMTcatch3$sciName))
#adfgLMTsp
#tax.info = tax_name(query = adfgLMTsp, 
#                    get = c("phylum", "subphylum", "class", "subclass", "infraclass", 
#                            "order", "suborder", "infraorder", "suborder", "infraorder", "family", 
#                            "genus", "species"), 
#                    db = "itis")

# Aplidium choose 2, Colus choose 76, Ctenophora choose 4, Cyanea choose 39, Echinacea choose 6 (Tax. Serial No. 157891), 
# Liparidae choose 2, Lumpenus fabricii  choose 2, Polychaeta choose 15 (Tax. Serial No. 914166), Suberites ficus choose 2

#adfgLMTtaxinfo = tax.info %>%
#  mutate(sciName = query) %>%
#  select(-db, -query)
#setwd("~/Google Drive/GoA project/dmx-common/ADFG_LargeMeshTrawl")
#write.csv(adfgLMTtaxinfo, file = "adfgLMTtaxonomicInfo.csv", row.names=F) # save the dataframe to csv


# The above code to create tax.info takes 12min to run with sporadic inputs needed
# upload the resulting file from here instead:
setwd("~/Google Drive/GoA project/dmx-common/ADFG_LargeMeshTrawl")
adfgLMTtaxinfo1 <- read.csv('adfgLMTtaxonomicInfo.csv',header=T,stringsAsFactors=F)

# merge taxonomic info onto catch dataframe by sciName
adfgLMTcatch4 <- left_join(adfgLMTcatch3, adfgLMTtaxinfo1, by = "sciName")
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Load metadata
setwd("~/Google Drive/GoA project/Data/Datasets/Data Packages/AOOS NCEAS Packages/Trawl Packages/ADFG Large mesh trawl/adfgTrawlData/largeMeshTrawl")
adfgLMTmeta <- read.csv('ALLhaulSummary.csv',header=T,stringsAsFactors=F)
# Clean up & extract desired info not already in catch files (km towed, duration, performance)
adfgLMTmeta1 <- adfgLMTmeta %>%
  mutate(Date=as.Date(Haul_Date, "%m/%d/%Y")) %>%   # output is reordered as yyyy-mm-dd
  rename(survey = survey_name, haul = Haul, station = Station, 
         distance = Km_Towed, duration = Duration, performance = Performance) %>%
  mutate(meta=paste(survey, haul, station, Date)) %>% # create vector of unique identifying info to merge with metadata table
  select(meta, distance, duration, performance)

# merge metadata onto catch data by meta (survey, haul, station, Date)
adfgLMT <- left_join(adfgLMTcatch4, adfgLMTmeta1, by = "meta")
head(adfgLMT)
str(adfgLMT)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


# to create empty vector:
adfgLMT$stage = vector(mode = "logical", length = nrow(adfgLMT))
#adfgLMT$stage = c(1:nrow(adfgLMT)
#adfgLMT$stage = rep(0,nrow(adfgLMT))
                  
# create column to indicate juveniles
for(i in 1:nrow(adfgLMT)) {
  if(adfgLMT$commonName[i] == "Juvenile Cod") {adfgLMT$stage[i] <- "juvenile"}
  else if(adfgLMT$commonName[i] == "Juvenile pollock") {adfgLMT$stage[i] <- "juvenile"} 
  else {adfgLMT$stage[i] <- NA}
  }
View(adfgLMT)