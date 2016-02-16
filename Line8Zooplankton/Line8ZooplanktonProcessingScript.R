######################################################
#    Line 8 (Shelikof Strait) Zooplankton Data       #
#                  Processing Script                 #
#  Script by Colette Ward (ward at nceas.ucsb.edu)   #
######################################################

# call output dataframe from Cleaning Script
source('Line8ZooplanktonCleaningScript.R') # does not work?  tell R to look in this subfolder of the repo?
head(l8zoop2)


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

plot(l8zoop2$month ~ l8zoop2$year, pch=16, cex=2.5)
plot(l8zoop2$julianDay ~ l8zoop2$year, pch=16, cex=2.5)
# May 1 = 121, May 31 = 151
# April 25 = 115, June 8 = 159


######################################################################


# Visualize net depths
plot(l8zoop2$gearDistFromBottom ~ l8zoop2$net, pch=16)
plot(l8zoop2$gearDistFromBottom ~ l8zoop2$year, pch=16)


######################################################################


# Issues to deal with:
# figure out if there are 2 nets (1 & 2) for each haul - why are 1 and 2 noted, 
# and past issue that Janet mentioned re nets being combined already in the database?

# clean up taxon names
# create decision tree re which species / sizes / sexes to use from which mesh size
# EST_NUM_PERM2 and EST_NUM_PERM3 are both  vectors of characters because of "Present"

