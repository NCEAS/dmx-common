######################################################
#    Line 8 (Shelikof Strait) Zooplankton Data       #
#                  Processing Script                 #
#  Script by Colette Ward (ward at nceas.ucsb.edu)   #
######################################################


## load packages
library(dplyr)
library(lubridate)


# call output dataframe from Cleaning Script
source('Line8ZooplanktonCleaningScript.R') # does not work?  tell R to look in this subfolder of the repo?
head(l8zoop2)



######################################################################


# create columns for metadata visualizations:
l8zoop3 <- l8zoop2 %>%
  mutate(gearDistFromBottom = bottomDepth - maxGearDepth) %>%
  mutate(julianDay = yday(akst_date_time)) # create vector of julian days


######################################################################

# Visualize sampling locations

## load mapping packages
library(rworldmap)
library(rworldxtra)
library(rgdal)
library(ggplot2)
library(grid)

# extract unique sampling events
l8Sites=l8zoop %>%
  mutate(uni=paste(cruise_name, station_name, gmt_date_time)) %>% # create column of cruise_name, station_name, gmt_date_time
  filter(!duplicated(uni)) %>% 
  select(lat,lon,year,month,day)
head(l8Sites)

# Inititate a blank map
world=getMap('low',projection=NA)
worldB=world[!is.na(world$continent),]
world2=worldB[worldB$continent=='North America' & worldB$LON<0,]
fWorld=fortify(world2)
colMap=c('dimgrey','black')

# Map all stations, all years, all locations
ggplot(data=fWorld) +
  geom_map(map=fWorld,aes(x=long,y=lat,map_id=id)) +
  coord_map(xlim = c(-162, -150),ylim = c(55, 59.5)) + 
  scale_fill_manual(values=colMap) +
  geom_point(data=l8Sites,mapping=aes(x=lon, y=lat, color=year),size=3,alpha=0.5, shape=20) +
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

######################################################################


# Visualize sampling months

plot(l8zoop3$month ~ l8zoop3$year, pch=16)
plot(l8zoop3$julianDay ~ l8zoop3$year, pch=16)
# May 1 = 121, May 31 = 151
# April 25 = 115, June 8 = 159



######################################################################


# Visualize net depths
plot(l8zoop3$gearDistFromBottom ~ l8zoop3$net, pch=16)
plot(l8zoop3$gearDistFromBottom ~ l8zoop3$year, pch=16)


# Visualize which nets are in the database for which years
plot(l8zoop3$net ~ l8zoop3$year, pch=16)
plot(l8zoop3$mesh ~ l8zoop3$year, pch=16)


# trying to figure out net 1 vs net 2:
# only net 1 for all years (excpet 1995, 1997, 1999, which have no data)
# both nets for 1998, 2004-2012
# have both 20BON and 60BON nets for 2004 - 2012

# for all years:
# net 2 is always 60BON, net 1 is both
# 60BON is always 333um mesh, 20BON is always 153um mesh

# have 333um mesh (60BON) for 1989-1994, 1996, 1998, 2000, 2001, 2004-2012
# have 153um mesh (20BON) for 2002-2012

# see cruise metadata here: http://access.afsc.noaa.gov/icc/selectionframe.php



######################################################################

# Calculate annual mean abundances for Spring for each species:

# subset the data
l8zoop4 <- l8zoop3 %>%
  filter(julianDay > 114, julianDay < 160) %>% # select samples collected between April 25 and June 8
  filter(mesh == 333) %>% # select only data from 333um mesh net, because the 153um mesh net was only deployed starting in 2002.
  filter(volumetricAbund != "Present") %>% # for now, remove "Present" until we know how to deal with it
  mutate(volumetricAbund = as.numeric(volumetricAbund)) # convert to a numeric vector
head(l8zoop4)  

# # create dataframe with samples collected at night (use this for Euphausiids and Mysids)
# define night as 22:00 to 05:59 (took times from Seward Line MOCNESS / Multinet collections) ... Anchorage sunrise/sunset times on May 15 are 05:10 and 22:40
night <- l8zoop4 %>%
  filter(hour(akst_date_time) >= 22 | hour(akst_date_time) < 6)
head(night)

#######################################################################

# trying to create a function:
# Create a table of data for species and stages quantitatively sampled by the 333um mesh net:

# create filter conditions:
condition1 <- list(taxonName = "Neocalanus cristatus", stageVector=c("adult", "c - 5 (copepodite v)", "c - 3 (copepodite iii)"))
condition2 <- list(taxonName = "Eucalanus bungii", stageVector=c("adult", "c - 5 (copepodite v)", "c - 4 (copepodite iv)", "c - 3 (copepodite iii)"))
conditionList <- list(condition1, condition2)

# Define the filtering function
filterStages <- function(condition, l8zoop4) {
  subset.data <- l8zoop4 %>%
    filter(taxonName == condition$taxonName) %>%
    filter(stage %in% condition$stageVector)
  return(subset.data)
}

# check that the filter function works for N. cristatus:
a <- filterStages(condition1, l8zoop4) # a is a list and a dataframe
b <- filterStages(condition2, l8zoop4) 


# apply the function to the dataframe
resultDataList <- lapply(conditionList, filterStages, l8zoop4)
resultDataList # this is a list
b <- as.data.frame(resultDataList) # nope.

unique(resultDataList$taxonName)


# now calculate annual mean abundances for each species.
# problem: how to work with output from above function that is a list instead of a dataframe?


#abund = function(LgStages) {
#  group_by(year, station, taxonName) %>%
#    summarise(AetideidaeSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
#    ungroup %>%
#    group_by(year, taxonName) %>%
#    summarise(Aetideidae=mean(AetideidaeSite)) %>%  # then calculate mean abundance across all stations, for each year
#    ungroup
#}


#######################################################################


# temporary fix: group needs these data asap, so do the calculations one-at-a-time until I can figure out the function

Ncristatus = l8zoop4 %>%
  filter(taxonName == "Neocalanus cristatus") %>%
  filter(stage %in% c("adult", "c - 5 (copepodite v)", "c - 3 (copepodite iii)")) %>%
  group_by(year, station) %>%
  summarise(NcristatusSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
  ungroup %>%
  group_by(year) %>%
  summarise(Ncristatus=mean(NcristatusSite)) %>% # then calculate mean abundance across all stations, for each year
  ungroup
#View(Ncristatus)


# "Neocalanus flemingeri" and "Neocalanus flemingeri / plumchrus" 
# use Adult (M/F), CV, CIV, CIII
# only observed in 2012 and only in the 153um net, stages C1 - C4


Ebungii = l8zoop4 %>%
  filter(taxonName == "Eucalanus bungii") %>%
  filter(stage %in% c("adult", "c - 5 (copepodite v)", "c - 4 (copepodite iv)", "c - 3 (copepodite iii)")) %>%
  group_by(year, station) %>%
  summarise(EbungiiSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
  ungroup %>%
  group_by(year) %>%
  summarise(Ebungii=mean(EbungiiSite)) %>% # then calculate mean abundance across all stations, for each year
  ungroup
#View(Ebungii)


Cmarshallae = l8zoop4 %>%
  filter(taxonName == "Calanus marshallae") %>%
  filter(stage %in% c("adult", "c - 5 (copepodite v)", "c - 4 (copepodite iv)", "c - 3 (copepodite iii)")) %>%
  group_by(year, station) %>%
  summarise(CmarshallaeSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
  ungroup %>%
  group_by(year) %>%
  summarise(Cmarshallae=mean(CmarshallaeSite)) %>% # then calculate mean abundance across all stations, for each year
  ungroup


Cpacificus = l8zoop4 %>%
  filter(taxonName == "Calanus pacificus") %>%
  filter(stage %in% c("adult", "c - 5 (copepodite v)", "c - 4 (copepodite iv)"))%>%
  group_by(year, station) %>%
  summarise(CpacificusSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
  ungroup %>%
  group_by(year) %>%
  summarise(Cpacificus=mean(CpacificusSite)) %>% # then calculate mean abundance across all stations, for each year
  ungroup


Mpacificalucens = l8zoop4 %>%
  filter(taxonName == "Metridia pacifica/lucens") %>%
  filter(stage %in% c("C6 + C5", "c - 5 (copepodite v)")) %>%
  group_by(year, station) %>%
  summarise(MpacificalucensSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
  ungroup %>%
  group_by(year) %>%
  summarise(Mpacificalucens=mean(MpacificalucensSite)) %>% # then calculate mean abundance across all stations, for each year
  ungroup


Metridia = l8zoop4 %>%
  filter(taxonName == "Metridia spp.") %>%
  filter(stage %in% c("C6 + C5", "c - 5 (copepodite v)")) %>%
  group_by(year, station) %>%
  summarise(MetridiaSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
  ungroup %>%
  group_by(year) %>%
  summarise(Metridia=mean(MetridiaSite)) %>% # then calculate mean abundance across all stations, for each year
  ungroup


CalanidsUnid = l8zoop4 %>%
  filter(taxonName == "Unidentified Calanids") %>%
  filter(stage %in% c("c - 3 (copepodite iii)")) %>%
  group_by(year, station) %>%
  summarise(CalanidsUnidSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
  ungroup %>%
  group_by(year) %>%
  summarise(CalanidsUnid=mean(CalanidsUnidSite)) %>% # then calculate mean abundance across all stations, for each year
  ungroup


#Unidentified Calanids (Damaged; all sizes) # none in these data


#Other copepoda (Damaged, adults, all sizes) # none in these data


EuphausiidAdJuv = night %>%
  filter(taxonName %in% c("Tessarabrachion oculatum", "Thysanoessa raschii", "Thysanoessa inermis", "Thysanoessa spinifera",
                          "Thysanoessa longipes", "Thysanoessa inspinata", "Euphasia pacifica", "Euphausiid")) %>%
  filter(stage %in% c("a + j (adult/juvenile)")) %>%
  group_by(year, station) %>%
  summarise(EuphausiidAdJuvSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
  ungroup %>%
  group_by(year) %>%
  summarise(EuphausiidAdJuv=mean(EuphausiidAdJuvSite)) %>% # then calculate mean abundance across all stations, for each year
  ungroup


EuphausiidFurcillia = night %>%
  filter(taxonName == "Euphausiid") %>%
  filter(stage %in% c("furcilia")) %>%
  group_by(year, station) %>%
  summarise(EuphausiidFurcilliaSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
  ungroup %>%
  group_by(year) %>%
  summarise(EuphausiidFurcillia=mean(EuphausiidFurcilliaSite)) %>% # then calculate mean abundance across all stations, for each year
  ungroup


Chaetognatha = l8zoop4 %>%
  filter(taxonName %in% c("Chaetognatha", "Chaetognatha (other)")) %>%
  filter(size %in% c(">= 5 and < 20 mm", ">= 20 mm")) %>%
  group_by(year, station) %>%
  summarise(ChaetognathaSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
  ungroup %>%
  group_by(year) %>%
  summarise(Chaetognatha=mean(ChaetognathaSite)) %>% # then calculate mean abundance across all stations, for each year
  ungroup


NatantiaAdJuv = l8zoop4 %>%
  filter(taxonName == "Natantia") %>%
  filter(stage %in% c("a + j (adult/juvenile)")) %>%
  filter(size == ">= 5 mm") %>%
  group_by(year, station) %>%
  summarise(NatantiaAdJuvSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
  ungroup %>%
  group_by(year) %>%
  summarise(NatantiaAdJuv=mean(NatantiaAdJuvSite)) %>% # then calculate mean abundance across all stations, for each year
  ungroup


InvertLarvae = l8zoop4 %>%
  filter(taxonName %in% c("Natantia", "Anomura, larvae", "Brachyura, larvae")) %>%
  filter(stage %in% c("larva")) %>%
  group_by(year, station) %>%
  summarise(InvertLarvaeSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
  ungroup %>%
  group_by(year) %>%
  summarise(InvertLarvae=mean(InvertLarvaeSite)) %>% # then calculate mean abundance across all stations, for each year
  ungroup


# NB ignoring "Teleost (Fish) Larvae" (only in the dataset for 2012)


CnidarianMedusae = l8zoop4 %>%
  filter(taxonName %in% c("Cnidaria")) %>%
  filter(size %in% c(">= 5 mm")) %>%
  group_by(year, station) %>%
  summarise(CnidarianMedusaeSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
  ungroup %>%
  group_by(year) %>%
  summarise(CnidarianMedusae=mean(CnidarianMedusaeSite)) %>% # then calculate mean abundance across all stations, for each year
  ungroup


Ctenophora = l8zoop4 %>%
  filter(taxonName %in% c("Ctenophora")) %>%
  filter(size %in% c(">= 5 mm")) %>%
  group_by(year, station) %>%
  summarise(CtenophoraSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
  ungroup %>%
  group_by(year) %>%
  summarise(Ctenophora=mean(CtenophoraSite)) %>% # then calculate mean abundance across all stations, for each year
  ungroup


Gammaridea = l8zoop4 %>%
  filter(taxonName %in% c("Gammaridea", "Gammaridea (Unidentifiable)")) %>%
  filter(size %in% c(">= 5 mm")) %>%
  group_by(year, station) %>%
  summarise(GammarideaSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
  ungroup %>%
  group_by(year) %>%
  summarise(Gammaridea=mean(GammarideaSite)) %>% # then calculate mean abundance across all stations, for each year
  ungroup


Hyperiidea = l8zoop4 %>%
  filter(taxonName == "Hyperiidea") %>%
  filter(size %in% c(">= 5 mm")) %>%
  group_by(year, station) %>%
  summarise(HyperiideaSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
  ungroup %>%
  group_by(year) %>%
  summarise(Hyperiidea=mean(HyperiideaSite)) %>% # then calculate mean abundance across all stations, for each year
  ungroup


Mysidacea = night %>%
  filter(taxonName == "Mysidacea") %>%
  filter(size %in% c(">= 5 mm")) %>%
  group_by(year, station) %>%
  summarise(MysidaceaSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
  ungroup %>%
  group_by(year) %>%
  summarise(Mysidacea=mean(MysidaceaSite)) %>% # then calculate mean abundance across all stations, for each year
  ungroup


Siphonophora = l8zoop4 %>%
  filter(taxonName == "Siphonophora") %>%
  filter(size %in% c(">= 5 mm")) %>%
  group_by(year, station) %>%
  summarise(SiphonophoraSite=sum(volumetricAbund)) %>% # sum abundance for all these stages, for each station
  ungroup %>%
  group_by(year) %>%
  summarise(Siphonophora=mean(SiphonophoraSite)) %>% # then calculate mean abundance across all stations, for each year
  ungroup



######################################################################

# Compile these into a single dataframe:

# problem with missing zeros, but can't simply change blanks to zeros, because we do not have any data for some years
# therefore, merge the above vectors onto a data frame of years for which we have data. 
# after doing this, any blanks in the data can be changed to zero.

SpringZoopAbund <- data.frame('year'=c(1989:1993, 1996, 1998, 2000, 2001, 2004:2011)) # create dataframe with years
# do not include 2012 for now because these data do not reflect mesh change to 505um

# Merge in the taxon-specific data:
SpringZoopAbund <- merge(SpringZoopAbund,Ncristatus,all.x=T)
SpringZoopAbund <- merge(SpringZoopAbund,Ebungii,all.x=T)
SpringZoopAbund <- merge(SpringZoopAbund,Cmarshallae,all.x=T)
SpringZoopAbund <- merge(SpringZoopAbund,Cpacificus,all.x=T)
SpringZoopAbund <- merge(SpringZoopAbund,Mpacificalucens,all.x=T)
SpringZoopAbund <- merge(SpringZoopAbund,Metridia,all.x=T)
SpringZoopAbund <- merge(SpringZoopAbund,CalanidsUnid,all.x=T)
SpringZoopAbund <- merge(SpringZoopAbund,Chaetognatha,all.x=T)
SpringZoopAbund <- merge(SpringZoopAbund,NatantiaAdJuv,all.x=T)
SpringZoopAbund <- merge(SpringZoopAbund,InvertLarvae,all.x=T)
SpringZoopAbund <- merge(SpringZoopAbund,CnidarianMedusae,all.x=T)
SpringZoopAbund <- merge(SpringZoopAbund,Ctenophora,all.x=T)
SpringZoopAbund <- merge(SpringZoopAbund,Gammaridea,all.x=T)
SpringZoopAbund <- merge(SpringZoopAbund,Hyperiidea,all.x=T)
SpringZoopAbund <- merge(SpringZoopAbund,Siphonophora,all.x=T)
# now change all NA to zero
SpringZoopAbund[is.na(SpringZoopAbund)] <- 0
#View(SpringZoopAbund)



# do the same for years in which samples were collected at night:
unique(sort(night$year))  # nighttime samples were collected in these years: 1989 1990 1996 1998 2000 2001 2005 2006 2007 2008 2010 2012
NightSpringZoopAbund <- data.frame('year'=c(1989, 1990, 1996, 1998, 2000, 2001, 2005:2008, 2010)) 

NightSpringZoopAbund <- merge(NightSpringZoopAbund,EuphausiidAdJuv,all.x=T)
NightSpringZoopAbund <- merge(NightSpringZoopAbund,EuphausiidFurcillia,all.x=T)
NightSpringZoopAbund <- merge(NightSpringZoopAbund,Mysidacea,all.x=T)
#View(NightSpringZoopAbund)
# there are no NAs in the dataset, but we would fill them in here if needed



#Now merge Nighttime samples into Spring dataset:
SpringZoopAbund <- merge(SpringZoopAbund,NightSpringZoopAbund,all.x=T)
#View(SpringZoopAbund)


#write.csv(SpringZoopAbund, file = "Line8SpringZoop.csv", row.names = F) # create .csv file of output

######################################################################


# Issues to deal with:

# 2. discuss with Janet how to quantitatively interpret "Present" (e.g. 0.0028, which is smallest observed unit?)

# 3. in main dataset (under cleaning script) need to specify zeros for years where species were not observed vs years with no data collection. 
# for now, I've done a temporary fix (see lines 399-401; after removing years for which I know that no samples were collected, changed blanks to zeros)