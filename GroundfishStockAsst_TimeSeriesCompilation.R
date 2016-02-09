# Compile time series from 2015 SAFE report (downloaded Feb 4, 2016)

library(dplyr)

# set working directory
setwd("~/Google Drive/GoA project/Data/Datasets/2015 Stock Assessment Reports/extracted data")

# 1. load adult arrowtooth data
atf <- read.csv('Arrowtooth_SA2015_Table1.csv', header=T)
atfAdult <- atf %>%
  rename(ArrowtoothAge3Plus = BiomassAge3Plus) %>%
  select(Year, ArrowtoothAge3Plus)
plot(log(atfAdult$ArrowtoothAge3Plus) ~ atfAdult$Year, pch=16, type="b", cex=1.5)


# 2. load adult pollock data
pollock <- read.csv('Pollock_SA2015_Table1.22.csv', header=T)
pollockAdult <- pollock %>%
  mutate(PollockAge3Plus = BiomassAge3Plusx1000*1000) %>%
  select(Year, PollockAge3Plus)
plot(log(pollockAdult$PollockAge3Plus) ~ pollockAdult$Year, pch=16, type="b", cex=1.5)


# 3. load Pacific cod data (all lengths)
pcod <- read.csv('PacificCod_SA2015_Table2.12.csv', header=T)
pcodAll <- pcod %>%
  rename(PCod = AllLengths.Biomass) %>%
  select(Year, PCod)
plot(log(pcodAll$PCod) ~ pcodAll$Year, pch=16, type="b", cex=1.5)


# 4. Sablefish
sablefish <- read.csv('Sablefish_SA2015_Table3.14.csv', header=T)
sablefishAdult <- sablefish %>%
  mutate(SablefishAge2Plus = BiomassMeanAge2PlusKilotons*1000) %>%
  select(Year, SablefishAge2Plus)


# 5. Shallow-water Flatfish
shallowFlatfish <- read.csv('ShallowFlatfish_SA2015_Table4.9.csv', header=T)
shFlatfish <- shallowFlatfish %>%
  select(-NorthernRockSole, -SouthernRockSole) # remove these because Northern & Southern rock sole are combined as Total Rock fish before 1996


# 6. Deep-water Flatfish ( Dover sole, Greenland turbot, Deepsea sole)
deepFlatfish <- read.csv('DeepwaterFlatfish_SA2015_Table14and15.csv', header=T)
dpFlatfishAdult <- deepFlatfish %>%
  rename(dpFlatfishAge3Plus = BiomassAge3Plus2015SA) %>%
  select(Year, dpFlatfishAge3Plus) # remove these because Northern & Southern rock sole are combined as Total Rock fish before 1996


# 7. Rex sole
rex <- read.csv('RexSole_SA2015_Table14and15.csv', header=T, na.strings="")
rexSoleAdult <- rex %>%
  rename(rexSoleAge3Plus = BiomassAge3Plus.2015SA) %>%
  select(Year, rexSoleAge3Plus)
  

# 8. Flathead sole
flathead <- read.csv('FlatheadSole_SA2015_Tables13,14.csv', header=T, na.strings="")
flatheadSoleAdult <- flathead %>%
  rename(flatheadSoleAge3Plus = BiomassAge3Plus.2015SA) %>%
  select(Year, flatheadSoleAge3Plus)


# 9. Pacific ocean perch
pop <- read.csv('PacificOceanPerch_SA2015_Table9.15.csv', header=T)
poPerchAdult <- pop %>%
  rename(poPerchAge2Plus = BiomassAge2Plus) %>%
  select(Year, poPerchAge2Plus)


# 10. Northern rockfish
nRock <- read.csv('NorthernRockfish_SA2015_Table10.14.csv', header=T)
nrockAll <- nRock %>%
  rename(nRockfish = TotalBiomass) %>%
  select(Year, nRockfish)


# 11. Shortraker rockfish 
# NB 2001 estimate is partly calculated from mean of 1993, 1996, 1999 data
# Relative population weight is also available from Longline survey, on annual basis
shRock <- read.csv('ShortrakerRockfish_SA2015_Table11.6.csv', header=T)
shortRockfishAll <- shRock %>%
  rename(shortRockfish = BiomassGulfwide) %>%
  select(Year, shortRockfish)


# 12. Dusky rockfish
dusky <- read.csv('DuskyRockfish_SA2015_Table12.15.csv', header=T)
duskyRockfishAdult <- dusky %>%
  rename(duskyRockfishAge4Plus = BiomassAge4Plus) %>%
  select(Year, duskyRockfishAge4Plus)


# 13. Rougheye / Blackspotted rockfish
reye <- read.csv('RougheyeRockfish_SA2015_Table13.19.csv', header=T)
rougheyeAdult <- reye %>%
  rename(rougheyeRockfishAdult = BiomassAge3Plus) %>%
  select(Year, rougheyeRockfishAdult)


# 14. Demersal shelf rockfish
# data are density / km2 and only for SEAK. Therefore ignore for now.
# file is DemersalShelfRockfish_SA2015_TablesA1,A2,A4.csv


# 15. Thornyheads
# from SA report: use only 1999, 2005, 2007, 2009, 2015
thornys <- read.csv('Thornyhead_SA2015_Table15.6.csv', na.strings=c("-- ", ""))
thornyheadAll <- thornys %>%
  filter(Area == "GoA.all") %>% # select only gulf-wide biomass totals
  filter(Year %in% c(1999, 2005, 2007, 2009, 2015)) %>% # select only 1999, 2005, 2007, 2009, 2015
  group_by(Year) %>%
  summarise(Thornyhead=sum(BiomassTotal)) %>% # I have checked sums
  ungroup()
# consider keeping Shortspine and Longspine thornyhead separate


# 16. Other Rockfish (slope)
# nb biomass estimates from bottom trawls are available on a species basis, by regulatory area, for 6 species, but only on a tri- or bi-ennial basis
# data used here are Random Effects model output
# 16a. Sharpchin Rockfish
sharpchin <- read.csv('OtherRockfish_Sharpchin_SA2015_Table16.13.csv', header=T)
sharpchinAll <- sharpchin %>%
  rename(SharpchinRockfish = GulfwideTotal) %>%
  select(Year, SharpchinRockfish)
# 16b. 16 other rockfish species which are 'well-sampled' by bottom trawls
moreRocks <- read.csv('OtherRockfish_16species_SA2015_Table16.14.csv')
otherRockfishAll <- moreRocks %>%
  rename(OtherRockfish16sp = GulfwideTotal) %>%
  select(Year, OtherRockfish16sp)




# Create empty data frame with Year column
Groundfish <- data.frame('Year'=c(1961:2015))

# Merge all data frames into one large data frame
Groundfish <- merge(Groundfish,atfAdult,all.x=T) # Arrowtooth Flounder Biomass (t)
Groundfish <- merge(Groundfish,pollockAdult,all.x=T) # Biomass (t)
Groundfish <- merge(Groundfish,pcodAll,all.x=T)
Groundfish <- merge(Groundfish,sablefishAdult,all.x=T)
Groundfish <- merge(Groundfish,shFlatfish,all.x=T)
Groundfish <- merge(Groundfish,dpFlatfishAdult,all.x=T)
Groundfish <- merge(Groundfish,rexSoleAdult,all.x=T)
Groundfish <- merge(Groundfish,flatheadSoleAdult,all.x=T)
Groundfish <- merge(Groundfish,poPerchAdult,all.x=T)
Groundfish <- merge(Groundfish,nrockAll,all.x=T)
Groundfish <- merge(Groundfish,shortRockfishAll,all.x=T)
Groundfish <- merge(Groundfish,duskyRockfishAdult,all.x=T)
Groundfish <- merge(Groundfish,rougheyeAdult,all.x=T)
Groundfish <- merge(Groundfish,thornyheadAll,all.x=T)
Groundfish <- merge(Groundfish,sharpchinAll,all.x=T)
Groundfish <- merge(Groundfish,otherRockfishAll,all.x=T)



View(Groundfish)
#write.csv(Groundfish, file = "Groundfish.csv", row.names=FALSE)