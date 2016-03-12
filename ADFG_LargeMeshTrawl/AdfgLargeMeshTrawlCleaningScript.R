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


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


# Assign site names to match those in Small Mesh Trawl dataset
unique(sort(adfgLMT$station))

adfgLMT1 <- adfgLMT %>%
  mutate(site = ifelse((station %in% c("KZA","KZB", "KZC", "KZD", "KZE", "KZF", "KZG", 
                                       "KZH", "KZI", "KZJ", "KZK", "KZL", "KZM", "KZN", # no KZH, KZI, KZL, KZM, KZN, kzp, kzq in dataset?
                                       "KZO", "KZP", "KZQ", "KZR", "KZS", "MONX", "MOS")), 
                                       'Marmot Bay inshore',
                       
                ifelse((station %in% c("MOEX","MOT","MOPX", "MOXX", "MOX", "MOLX", "MOQ", # careful, Morzhovoi Bay sites have similar names
                                       "MOGX", "255X", "255", "256", "257", "283X", "283", 
                                       "284", "285", "313", "314")), 
                                       'Marmot Gully offshore',
                       
                ifelse((station %in% c("CHA", "CHB", "CHC", "CHD", "CHE", "CHF", "CHG", "CHI", "CHJ", "CHK", "CHL")), 
                                       'Chiniak Bay inshore', # consider also including CHM, CHU, CHN, CHQ, CHP, CHO, CHR, CHS, CHT, which are all north of the other Chiniak inshore sites
                 
                ifelse((station %in% c("369X", "395", "420", "421", "442", "443", "444")), 
                                       'Chiniak Bay offshore', 
                                       # where is site 368A?  consider also including 371-373, 394-398, 418-422, 440-445, which are present in dataset
                                       # these sites are called Chiniak Bay offshore by SMT; Chiniak Gully by LMT
               
                ifelse((station %in% c("UGA", "UGAA", "UGAB", "UGAC", "UGB", "UGC", "UGD", 
                                       "UGE", "UGF", "UGG", "UGH", "UGI", "UGJ", "UGK", "UGM")),
                                       'Ugak Bay',    
                       
                ifelse((station %in% c("KLA", "KLB", "KLC", "KLD", "KLE", "KLF", "KLG", "KLH", "KLI", "KLJ", "KLL",
                                       "533A", "533B")), # consider calling these Barnabas Gully instead
                                       'Kiliuda Bay',  
                       
                ifelse((station %in% c("486", "486A", "486B", "510", "510A", "510B", "510C", "511", "511A", "511B", 
                                       "534", "534A", "534B", "534C", "534D", "535", "535A", "535B", "535C", "535D", #consider including "533A", "533B"
                                       "558", "558B", "559", "560", "561", "585X", "586", "587", "588", "589", 
                                       "618A", "619", "620", "621", "654", "655", "656", "695", "696")), 
                                       'Barnabas Gully', 
                       
                ifelse((station %in% c("THA", "THB", "THC", "THD", "THF", "THG", "THH", "THI", "THJ", "THK", "THL", "THM", "THN",  
                                       "614", "615", "650", "651")),
                                       'Two-Headed Gully',
                                       # looks like SMT includes Horse's Head Area with Two-Headed Gully ...
                       
                ifelse((station %in% c("688", "689", "725", "726", "727", "728", "729", "759", "760", "761")), # also include "794", "795", "796"? (immediately south of 759, 760, 761)
                                       'Horses Head Area',

                ifelse((station %in% c("ALA", "ALB", "ALC", "ALD", "ALF", "ALG", "ALH", "ALI", 
                                       "ALJ", "ALK", "ALL", "ALM", "ALO", "ALP", "ALQ", "ALR")),
                                       'Alitak Bay',
                                       
                ifelse((station %in% c("645", "645B", "646A", "646B", "646C", "646D", 
                                       "682B", "682D", "683A", "683B", "683C", "683D", "684A", "684B", "684C", "684D")),
                                       'Alitak Flats',
                       
                ifelse((station %in% c("UYBX", "UYEX", "UYFX", "UYHX", "UYKX", "UYMX", "UYO", "UYQX", "UYSS", 
                                       "UYSX", "UYT")),
                                       'Uyak Bay',
                       
                ifelse((station %in% c("KUN", "KUNX", "KUO", "KUP", "KUQ", "KUR", "KUS", "KUT", "KUU", "KUV", "KUW", "KUX", "KUXX", "KUY", "KUYX")),
                                       'Uganik Bay',
                       
                ifelse((station %in% c("KUD", "KUE", "KUF", "KUG", "KUH", "KUI", "KUJ", "KUK", "KUL", "KULX", "KUM")),
                                       'Viekoda Bay',
                       
                ifelse((station %in% c("MAA")), 'Malina Bay',
                       
                ifelse((station %in% c("2", "3", "31", "60", "61", "90", "91", "117", "118", "119", 
                                       "120", "121", "144", "145", "146", "147")),
                                       'North Shelikof Strait',
                                       # not sure where to put 172, 173, 174, 198, 199, 200, 222, 223, 224 (North or Central?)
                       
                ifelse((station %in% c("171", "171X", "171Y")), 'Kukak Bay',
                       
                ifelse((station %in% c("MOA", "MOB", "MOC", "MOD", "MOE", "MOF", "MOG", "MOH", "MOI", 
                                       "MOJ", "MOK", "MOL", "MOM", "MON", "MOO", "MOOX", "MOP", "MOQ", 
                                       "MOR", "MORX", "MOS", "MOSX", "MOT", "MOU", "MOV", "MOW", "MOX", 
                                       "87", "87A", "87AX", "87B", "87C", "87D", "87E")), 'Morzhovoi Bay',
                       
                 ifelse((station %in% c("COB", "COC", "COE", "COF", "COG", "COGA", "COGB", "COH", "COJ",
                                        "COL", "COM", "CON", "COO", "COP", "COQ", "132A")), 'Cold Bay',
                 
                 ifelse((station %in% c("BEB", "BEBX", "BEC", "BECX", "BED", "BEE", "BEF", "BEG")), 'Belkofski Bay', 
                        
                 ifelse((station %in% c("113", "125", "126", "137", "138", "138A", "138B", "138C")), 'Sanak Island',
                        
                 ifelse((station %in% c("PAA", "PAB", "PABX", "PAC", "PAD", "PAE", "PAEX", "PAF", "PAG", "PAH",
                                        "PAI", "PAIX", "PAJ", "PAL", "PALX", "PAM", "PAN", "PAO", "PAOA", "PAOB",
                                        "PAP", "PAQ", "PAR", "PARA", "PARB", "PAS", "PAT", "PAU", "PAV", "PAWA", 
                                        "VOA", "VOB", "VOBX", "VOC", "VOD", "VOE", "VOF", "VOFA", "VOFB", "VOG",
                                        "VOH", "VOI", "VOJ", "VOK", "VOL", "VOLX", "VOM", "VOMA", "VOMB", "VON",
                                        "VOO", "VOP", "VOQ", "VOR", "VOT")), 'Pavlof Bay',
                        
                 ifelse((station %in% c("BVA", "BVB", "BVC", "BVD", "278")), 'Beaver Bay', # consider whether to include 228, 245, 261, 262
                        
                 ifelse((station %in% c("BAA", "BAC", "BAD", "BAE", "BAF", "311", "311A", "311B", "311C", "312A", 
                                        "329", "329B", "329C", "348", "368A")), 'Balboa-Unga Strait', 
                        
                 ifelse((station %in% c("332", "332B", "334", "335", "353", "354", "371", "373A", "373B", "393")), 'West Nagai Strait',
                        
                 ifelse((station %in% c("STA", "STB", "STD", "STE", "409", "410")), 'Stepovak Bay', # SMT includes much larger area ...
                        
                 ifelse((station %in% c("4900", "4915", "400X", "4000", "4007", "4008", "4009")), 'Ivanof Bay', # SMT includes larger area ...
                        
                 ifelse((station %in% c("4024", "4025", "4026", "4035", "4036", "4037", "4038", "4039A", "4039B", 
                                        "4043", "4048", "4049", "4053", "4063", "4064", "4065", "4066", "4067",
                                        "4068", "4095")), 'Mitrofania Island', 
                        
                 ifelse((station %in% c("4964", "4256", "4260", "4262", "4264", "4265", "4266", "4267", "4270", 
                                        "4271", "4272", "4274", "4277", "4278", "4279", "4282", "4286", "4287")), 'Chignik-Castle Bays',
                               
                 ifelse((station %in% c("4290", "4296", "4298", "4301", "4302", "4304", "4308", "4312")), 'Kujulik Bay',
                               
                             )))

# Barnabas Gully )),                
                
                     
dba <- adfgLMT %>%
  mutate(site = ifelse((station %in% (starts_with("CH"))), 'Chiniak Bay inshore'))                     

