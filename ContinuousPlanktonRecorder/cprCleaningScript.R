#############################################################
#####   Continuous Plankton Recorder Cleaning Script   ######
#####  Script by Colette Ward (ward at nceas.ucsb.edu) ######
#############################################################

## load packages
library(dplyr)
library(httr)
library(taxize)

# Load 2000-2012 data
setwd("~/Google Drive/GoA project/Data/Datasets/Data Packages/AOOS NCEAS Packages/Continuous Plankton Recorder_Zoop, Phyto, NPacific_97-12/resourceMap_df35d_195_12/data")
cpr1 <- read.table("df35d.193.9-timeSeries.txt", sep="\t", header=T, stringsAsFactors = F)
View(cpr1)
str(cpr1)

#URL_cpr <- "http://gulfwatch.nceas.ucsb.edu/goa/d1/mn/v1/object/df35d.193.9"
#cprGet <- GET(URL_cpr)
#cpr1 <- content(cprGet, as='text')
#cpr <- read.table(file=textConnection(cprGet), sep="\t", header=T, stringsAsFactors=F) # Error in textConnection(cprGet) : invalid 'text' argument


# split date & time
cpr2 <- cpr1 %>%
  mutate(time=strsplit(as.character(date),split=" ") %>%
         sapply(function(x) x[2])) %>%
  mutate(date1=strsplit(as.character(date),split=" ") %>%
           sapply(function(x) x[1])) %>%
  mutate(Date=as.Date(date1, "%Y-%m-%d")) %>%
  mutate(Year=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[1])) %>%
  mutate(year=as.numeric(Year)) %>%
  mutate(Month=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[2])) %>%
  mutate(month=as.numeric(Month)) %>%
  mutate(Day=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[3])) %>%
  mutate(day=as.numeric(Day)) %>%
  select(-date, -date1, -Date, -Year, -Month, -Day)

# ------------------------------------------------------

# Look for missing metadata:

# look for NAs:
sum(is.na(cpr2$lat)) # 897
sum(is.na(cpr2$long)) # 897
sum(is.na(cpr2$year)) # 1196
sum(is.na(cpr2$month)) # 1196
sum(is.na(cpr2$day)) # 1196

cpr4 <- cpr2 %>%
  filter(is.na(lat))
View(cpr4)
# date & time info are also missing for these samples, but sample is recorded
unique(sort(cpr4$sample)) # [1] "78AC52" "78AC53" "78AC54"

cpr5 <- cpr2 %>%
  filter(is.na(year))
View(cpr5)
# date & time info are also missing for these samples, but sample, lat, long are recorded
unique(sort(cpr5$sample)) # [1] "1VJ2"   "78AC52" "78AC53" "78AC54"

# is there metadata for these 4 samples elsewhere in the file?  No.
cpr6 <- cpr2 %>%
  filter(sample %in% c("78AC52", "78AC53", "78AC54", "1VJ2")) %>%
  mutate(uni=paste(sample,lat,long,year,month,day)) %>%
  filter(!duplicated(uni)) %>%
  select(sample,lat,long,year,month,day)
View(cpr6)

# Guess where & when 78AC52-54 were collected based on adjacent numbered samples?
cpr7 <- cpr2 %>%
  filter(sample %in% c("78AC48", "78AC49", "78AC50", "78AC51", "78AC55", "78AC56", "78AC57", "78AC58")) %>%
  mutate(uni=paste(sample,lat,long,year,month,day)) %>%
  filter(!duplicated(uni)) %>%
  select(sample,lat,long,year,month,day)
View(cpr7)
# 78AC49: May 25, 2003 (in ACC)
# 78AC51: May 25, 2003, almost same location as above (in ACC)
# all samples beginning with 78AC appear to have been collected in 2003
# and given trend in lat & long with sample numbering, 78AC52-54 are almost certainly in the Alaskan shelf area
# IVJ2 is not in ACC (off NW coast of Vancouver Island)


# ----------------------------------------------------------

# Filter out only entries from the Gulf & shelf, because it takes too long to run code on the entire dataset.

# Visualize sampling locations

## load mapping packages
library(rworldmap)
library(rworldxtra)
library(rgdal)
library(ggplot2)
library(grid)


# extract unique sampling events
CPRSites=cpr2 %>%
  filter(lat != "NA") %>%
  mutate(uni=paste(lat,long,year,month,day)) %>%
  filter(!duplicated(uni)) %>%
  select(lat,long,year,month,day)
View(CPRSites)

# Inititate a blank map
world=getMap('low',projection=NA)
worldB=world[!is.na(world$continent),]
world2=worldB[worldB$continent=='North America' & worldB$LON<0,]
fWorld=fortify(world2)
colMap=c('dimgrey','black')


# Map all stations, all years, all locations
ggplot(data=fWorld) +
  geom_map(map=fWorld,aes(x=long,y=lat,map_id=id)) +
  #coord_map(xlim = c(-182, -118),ylim = c(33, 62)) + #for all sampling locations
  coord_map(xlim = c(-170, -130),ylim = c(52, 62)) + 
  scale_fill_manual(values=colMap) +
  geom_point(data=CPRSites,mapping=aes(x=long, y=lat, color=year),size=3,alpha=0.5, shape=20) +
  ggtitle('Continuous Plankton Recorder Sampling Locations') +
  theme(axis.line=element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position='right',
        axis.text=element_text(size=8),
        title=element_text(size=12,face="bold"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  guides(colour = guide_legend(override.aes = list(size=6)))

# Map all stations, all years, locations in/near GoA shelf
ggplot(data=fWorld) +
  geom_map(map=fWorld,aes(x=long,y=lat,map_id=id)) +
  coord_map(xlim = c(-155, -140),ylim = c(56, 62)) + 
  scale_fill_manual(values=colMap) +
  geom_point(data=CPRSites,mapping=aes(x=long, y=lat, color=year),size=3,alpha=0.5, shape=20) +
  ggtitle('Gulf of Alaska Sites') +
  theme(axis.line=element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position='right',
        axis.text=element_text(size=8),
        title=element_text(size=12,face="bold"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  guides(colour = guide_legend(override.aes = list(size=6)))


# ----------------------------------------------------------

# select only samples collected on GoA shelf
# for samples 2003 & earlier (ie outside Prince William Sound): latitude 59.3 to 60.1, long -144.5 to -146.7
# for samples collected 2004 & after (ie approaching & in Cook Inlet): Lat 58.3 to 58.7, Long -148.3 to -151.5
# exclude PWS & Cook Inlet for now 

# select stations outside Prince William Sound (needs adjustment):
CprPws <- cpr2 %>%
  filter(lat > 59.3) %>% filter(lat < 60.1) %>%
  filter(long < -144.5) %>% filter(long > -146.7)


# select stations outside Cook Inlet (needs adjustment):
CprCook <- cpr2 %>%
  filter(lat > 58.3) %>% filter(lat < 58.7) %>%
  filter(long < -148.3) %>% filter(long > -151.5)


# create dataframe with only GoA shelf sites:
cpr3 <- bind_rows(CprPws, CprCook)

# look at map to check:
ggplot(data=fWorld) +
  geom_map(map=fWorld,aes(x=long,y=lat,map_id=id)) +
  coord_map(xlim = c(-155, -140),ylim = c(56, 62)) + 
  scale_fill_manual(values=colMap) +
  geom_point(data=cpr3,mapping=aes(x=long, y=lat, color=year),size=3,alpha=0.5, shape=20) +
  ggtitle('Gulf of Alaska Shelf Sites') +
  theme(axis.line=element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position='right',
        axis.text=element_text(size=8),
        title=element_text(size=12,face="bold"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  guides(colour = guide_legend(override.aes = list(size=6)))

# ----------------------------------------------------------

# merge new and old species names into a single vector of accepted species names
cpr3$AccSpeciesName <- vector(mode="character", length=nrow(cpr3)) # create an empty vector for accepted scientific name
for(j in 1:nrow(cpr3)) {
  if(!is.na(cpr3$NEWSpeciesName[j])) {cpr3$AccSpeciesName[j] <- cpr3$NEWSpeciesName[j]} # if there is a value in NEWSpeciesName, write this into AccSpeciesName
  else{cpr3$AccSpeciesName[j] <- cpr3$OLDSpeciesName[j]} # otherwise, write OLDSpeciesName into AccSpeciesName
}
View(cpr3)
dim(cpr3) # should be 127280     15


# Note to self: result for entire dataset (entire North Pacific) is stored here on my computer (takes 2 hrs to run the above code)
#setwd("~/Google Drive/GoA project/Data/Datasets/Data Packages/AOOS NCEAS Packages/Continuous Plankton Recorder_Zoop, Phyto, NPacific_97-12")
#cpr3 <- read.csv('cpr2_AcceptedNames.csv')
#dim(cpr3) # 1366932      15


# ----------------------------------------------------------

# work with just Phytoplankton data for now
# for Phytoplankton taxa, I think analysisStage should always be fields of view - check this.

cprPhyto <- cpr3 %>% filter(planktonType == "phytoplankton")

# Clean up species names
cprPhyto1 = cprPhyto %>%
  mutate(AccSpeciesName = gsub("\\(|\\)", "", AccSpeciesName)) %>% # remove brackets
  mutate(AccSpeciesName = gsub(" unidentified$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub("Unidentified ", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" Total$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" spp.$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" spp $", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" sp.$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" sp..$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" sp.. $", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" tissue", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" V_VI$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" spp. V-VI", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" I_IV$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" III$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" IV$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" V$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" V 3.4-3.9 from 2001 only", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" VI F$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub("Pacific Calanus spp V-VI", "Calanus", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" juvenile$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" nauplii$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" spp. developing asexual", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" Eyecount$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub("Clione pacific/Thliptodon", "Clione", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub("D. tripos", "Dinophysis tripos", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub("Detonula/Neodenticula temporary", "Detonula", AccSpeciesName)) %>% # consider changing this later
  mutate(AccSpeciesName = gsub(" post-larvae$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub("Prorocentrum spp. Exuviaella type", "Prorocentrum", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" type$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub("fragilariopsis", "Fragilariopsis", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" resting spore$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub("Nitzschia bicapitata\xe6", "Nitzschia bicapitata", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub("Parasitic ", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" larvae$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" cysts$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" complex$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub("Pterosperma \xefblocks IV\xcd", "Pterosperma", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub(" blocks$", "", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub("Rhizomonas setigera = Solenicola setigera +", "Solenicola setigera", AccSpeciesName)) %>% #neither are found in ITIS
  mutate(AccSpeciesName = gsub("\\+", "", AccSpeciesName)) %>% # remove "+"
  mutate(AccSpeciesName = gsub("Rhizosolenia imbrica. shrubsolei", "Rhizosolenia imbricata var. shrubsolei", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub("Pontellid", "Pontellidae", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub("Podosira stelligera", "Hyalodiscus stelliger", AccSpeciesName)) %>% # update to accepted name
  mutate(AccSpeciesName = gsub("Podosira stelliger", "Hyalodiscus stelliger", AccSpeciesName)) %>% # update to accepted name
  mutate(AccSpeciesName = gsub("Rhizosolenia alata alata", "Proboscia alata", AccSpeciesName)) %>% # update to accepted name
  mutate(AccSpeciesName = gsub("Rhisosolenia pungens", "Rhizosolenia pugens", AccSpeciesName)) %>% # update mis-spelling (z) - but should pungens be pugens? not ITIS hits for pungens
  mutate(AccSpeciesName = gsub("Rhizosolenia alata curvirostris", "Proboscia truncata", AccSpeciesName)) %>% # update to accepted name
  mutate(AccSpeciesName = gsub("Rhizosolenia alata indica", "Rhizosolenia alata f. indica", AccSpeciesName)) %>% # update to accepted name
  mutate(AccSpeciesName = gsub("Rhizosolenia alata inermis", "Proboscia inermis", AccSpeciesName)) %>% # update to accepted name
  mutate(AccSpeciesName = gsub("Rhizosolenia hebetata hiemalis", "Rhizosolenia hebetata var. hiemalis", AccSpeciesName)) %>% # update to accepted name
  mutate(AccSpeciesName = gsub("Rhizosolenia hebetata semispina", "Rhizosolenia hebetata var. semispina", AccSpeciesName)) %>% # update to accepted name
  mutate(AccSpeciesName = gsub("Small centric diatom\xe6\xe6\xe6\xe6\xe6\xe6\xe6\xe6\xe6\xe6", "Small centric diatom", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub("echinoderm", "Echinodermata", AccSpeciesName)) %>% 
  mutate(AccSpeciesName = gsub("Fish", "Osteichthyes", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub("  $", "", AccSpeciesName)) %>% # remove trailing double white space (must come before next line)
  mutate(AccSpeciesName = gsub(" $", "", AccSpeciesName)) %>%
  # consider changing "Silicoflagellatae"?
  mutate(AccSpeciesName = gsub("Chaetoceros Phaeoceros", "Chaetoceros (Phaeoceros)", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub("Chaetoceros Hyalochaete", "Chaetoceros (Hyalochaete)", AccSpeciesName)) %>%
  mutate(AccSpeciesName = gsub("Hyalochaete$", "Chaetoceros (Hyalochaete)", AccSpeciesName))
#unique(sort(cprPhyto1$AccSpeciesName))

# ----------------------------------------------------------

# Add higher order taxonomic information
# Query ITIS for higher order taxonomic information:
cprPhyto1.specieslist <- unique(cprPhyto1$AccSpeciesName)
cpr.tax.info = tax_name(query = cprPhyto1.specieslist, 
                    get = c("kingdom", "phylum", "subphylum", "class", "subclass", "infraclass", 
                            "order", "suborder", "infraorder", "family", "genus", "subgenus", 
                            "species"), 
                    db = "both", pref = "itis")

cpr.tax.info.itis = cpr.tax.info %>%
  mutate(sciName = query) %>%
  select(-db, -query)

write.csv(cpr.tax.info.itis, "itis.phyto.names.csv", row.names = F)
# see also ncbi.phyto.names.csv

# The above code to create tax.info takes 16min to run, with sporadic inputs needed
# upload the resulting file from here instead:
setwd("~/Google Drive/GoA project/Data/Datasets/Data Packages/AOOS NCEAS Packages/Continuous Plankton Recorder_Zoop, Phyto, NPacific_97-12/resourceMap_df35d_195_12/data")
itis.phyto.names <- read.csv("itis.phyto.names.csv", header=T)
View(itis.phyto.names)

# NCBI misses more taxa than ITIS, therefore use ITIS result
# manually add information for a few taxa that NCBI found that ITIS didn't
