#######################################################################
#####  Seward Line Large Zooplankton (>0.3mg) Processing Script  ######
#####        Script by Colette Ward (ward@nceas.ucsb.edu)        ######
#####                        September 2015                      ######
#######################################################################

# Dataset is from Waite, J.N. & Mueter, F.J. (2013) 
# Spatial and temporal variability of chlorophyll-a concentrations in the
# coastal Gulf of Alaska, 1998–2011, using cloud-free reconstructions of
# SeaWiFS and MODIS-Aqua data. Prog. Oceanography, 116, 179-192.


## load packages
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)

# A. Load data:
# we do not have the authors' permission (we have not requested it) to share this data beyond the Dynamics working group
# therefore data is stored locally on our machines (should figure out if we can we call it from Redmine)
setwd("~/Google Drive/GoA project/Data/Datasets")
Chla <- read.csv('Waite & Mueter 2013_Time-Series_1998-2011_lnChl_GOAclusters.csv', header=T, stringsAsFactors=F, na.strings = c("NA", " ", "")) # consider also strip.white=TRUE
head(Chla)
str(Chla)
dim(Chla) # 630  11
names(Chla)

# data are 8-day mean log chl-a levels and interannual anomalies
# regions: se = southeast, woff = western offshelf, won = western onshelf, cen = central
# units = mg/m3

# measurements are available for weeks 7-38 (approx mid-Feb to Oct 31)

# Western Gulf, On-shelf:
won = Chla %>%
  filter(chl.won != "NA") %>%
  group_by(year) %>%
  summarise(won.mean=mean(chl.won)) %>% 
  ungroup
View(won)

ggplot(data=won, aes(y=won.mean, x=year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  ylab("Mean Annual Chl a, Western Gulf On-Shelf (log mg/m3)") +
  xlab("Year")



# Western Gulf, Off-shelf:
woff = Chla %>%
  filter(chl.woff != "NA") %>%
  group_by(year) %>%
  summarise(woff.mean=mean(chl.woff)) %>% 
  ungroup
View(woff)

ggplot(data=woff, aes(y=woff.mean, x=year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  ylab("Mean Annual Chl a, Western Gulf Off-Shelf (log mg/m3)") +
  xlab("Year")



# Central Gulf:
cen = Chla %>%
  filter(chl.cen != "NA") %>%
  group_by(year) %>%
  summarise(cen.mean=mean(chl.cen)) %>% 
  ungroup
View(cen)

ggplot(data=cen, aes(y=cen.mean, x=year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  ylab("Mean Annual Chl a, Central Gulf (log mg/m3)") +
  xlab("Year")


# SouthEast Alaska:
se = Chla %>%
  filter(chl.se != "NA") %>%
  group_by(year) %>%
  summarise(se.mean=mean(chl.se)) %>% 
  ungroup
View(se)

ggplot(data=se, aes(y=se.mean, x=year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  ylab("Mean Annual Chl a, SouthEast Alaska (log mg/m3)") +
  xlab("Year")



# Create dataframe with years:
AnnChlA=data.frame('year'=c(1998:2011))

# Merge log annual means calculated above:
AnnChlA <- merge(AnnChlA,won,all.x=T)
AnnChlA <- merge(AnnChlA,woff,all.x=T)
AnnChlA <- merge(AnnChlA,cen,all.x=T)
AnnChlA <- merge(AnnChlA,se,all.x=T)
View(AnnChlA)

#setwd()
write.csv(AnnChlA, file = "AnnChlA_WaiteMueter2013.csv", row.names=F)
