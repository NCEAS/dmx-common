# Compile time series from 2015 SAFE report
# which is available at http://www.afsc.noaa.gov/REFM/stocks/assessments.htm (Downloaded Feb 4, 2016)

library(dplyr)

# set working directory
setwd("~/Google Drive/GoA project/Data/Datasets/2015 Stock Assessment Reports/extracted data")


# 1. load adult arrowtooth data
atf <- read.csv('Arrowtooth_SA2015_Table1.csv', header=T)
atfAdult <- atf %>%
  rename(ArrowtoothAge3Plus = BiomassAge3Plus) %>%
  select(Year, ArrowtoothAge3Plus)


# 2. load adult pollock data
pollock <- read.csv('Pollock_SA2015_Table1.22.csv', header=T)
pollockAdult <- pollock %>%
  mutate(PollockAge3Plus = BiomassAge3Plusx1000*1000) %>%
  select(Year, PollockAge3Plus)


# 3. load Pacific cod data (all lengths)
pcod <- read.csv('PacificCod_SA2015_Table2.12.csv', header=T)
pcodAll <- pcod %>%
  rename(PCod = AllLengths.Biomass) %>%
  select(Year, PCod)


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
thornys <- read.csv('Thornyhead_SA2015_Table15.6.csv', header=T, na.strings=c("-- ", ""))
thornyheadAll <- thornys %>%
  filter(Area == "GoA.all") %>% # select only gulf-wide biomass totals
  filter(Year %in% c(1999, 2005, 2007, 2009, 2015)) %>% # select only 1999, 2005, 2007, 2009, 2015
  group_by(Year) %>%
  summarise(Thornyhead=sum(BiomassTotal)) %>% # I have checked sums
  ungroup()
# consider keeping Shortspine and Longspine thornyhead separate ...


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


# 17. Atka mackerel
# do not use these data. From stock assessment report (p10):
# "What can be concluded from this is that the general groundfish GOA bottom trawl survey, 
# as it has been designed and used since 1984, does not assess GOA Atka mackerel well, 
# and the resulting biomass estimates are not considered consistent reliable indicators of 
# absolute abundance or indices of trend."


# 18. Skates
# nb biomass estimates from bottom trawls are available on a species basis, by regulatory area, for 10 species, but only on a tri- or bi-ennial basis
# data used here are Random Effects model output
# 18a. Big skates
bigSkates <- read.csv('Skate_BigSkate_SA2015_Table9a.csv', header=T, na.strings="")
bigSkatesAll <- bigSkates %>%
  mutate(bigSkates = WestGoA.REModelEst + CentralGoA.REModelEst + EastGoA.REModelEst) %>% # add regional biomass totals to get Gulfwide biomass totals; I have checked the sums
  select(Year, bigSkates)
# 18b. Longnose skates
longnoseSkates <- read.csv('Skate_Longnose_SA2015_Table9b.csv', header=T, na.strings="")
longnoseSkatesAll <- longnoseSkates %>%
  mutate(longnoseSkates = WestGoA.REModelEst + CentralGoA.REModelEst + EastGoA.REModelEst) %>% # add regional biomass totals to get Gulfwide biomass totals
  select(Year, longnoseSkates)
# 18c. Other skates (including Bering, Mud, Roughtail, Alaska, Aleutian, Whiteblotched, Whitebrow)
otherSkates <- read.csv('Skate_OtherAggregate_SA2015_Table9c.csv', header=T, na.strings="")
otherSkatesAll <- otherSkates %>%
  mutate(otherSkates = WestGoA.REModelEst + CentralGoA.REModelEst + EastGoA.REModelEst) %>% # add regional biomass totals to get Gulfwide biomass totals
  select(Year, otherSkates)
  

# 19. Sculpin
# 2001 survey did not sample eastern GoA, therefore do not use?
sculpin <- read.csv('Sculpin_SA2015_Table7.csv', header=T, na.strings=c("- ", "-  "))

# set "<1" to "1"
# can't get any of the following to work:
for(j in 1:ncol(sculpin)) { 
  for(i in nrow(sculpin)) {
    if(sculpin[i,j] == "<1 ")  {sculpin[i,j] <- "1"}
  }}
View(sculpin)
rm(sculpin)
str(sculpin)

sculpin[sculpin == c("<1 ")] <- "a" # does not work; turns some to NA and others unchanged
sculpin[sculpin == "\\<1 "] <- 9999999
sculpin[sculpin == paste(expression("<"),1)] <- 666666

replace(sculpin,"<1 ",999999)

paste(expression("<"),1)

paste(expression(u,v,1+ 0:9))

# not trying gsub because that requires doing it for each column

    
# Ignore script between ####
#################################
# first remember the names
n <- df.aree$name

# transpose all but the first column (name)
df.aree <- as.data.frame(t(df.aree[,-1]))
colnames(df.aree) <- n
df.aree$myfactor <- factor(row.names(df.aree))

str(df.aree)


sculpin1 <- t(sculpin) # transpose the table
colnames(sculpin1) <- sculpin1[1,] # make first row into column names
sculpin1 = sculpin1[-1,] # remove first row of table
# sculpin1 <- sculpin1[2:nrow(sculpin1),] # another way of doing it
View(sculpin1)
rm(sculpin1)

sculpin2 <- as.data.frame(sculpin1) # make it a dataframe
# note all columns are factors
View(sculpin2)
rm(sculpin2)

# make rownames into a column
# name the Year column
# remove X from in front of years
# remove * from 2001
# what to do about "<"?
##########################################


# 20. Sharks
# from SA report p1582: interannual variation in trawl survey depths probably does not impact shark biomass estimates, excpet for 2001
# therefore only remove 2001 data, and only when using biomass estimates derived solely from bottom trawls

# 20a. Spiny dogfish (data are output of random effects model; units = Biomass(t))
dogfish <- read.csv('Sharks_Dogfish_SA2015_Table20.14.csv', header=T)
dogfishAll <- dogfish %>%
  rename(Dogfish = REModelBiomass) %>%
  select(Year, Dogfish)

# 20b. Pacific sleeper shark (data are bi- and tri-ennial biomass estimates from bottom trawls)
# 20c. Salmon shark (data are bi- and tri-ennial biomass estimates from bottom trawls)
moreSharks <- read.csv('Sharks_SA2015_Table20.12.csv', header=T)
moreSharksAll <- moreSharks %>%
  rename(SleeperShark = SleeperShark.BiomassEst, SalmonShark = SalmonShark.BiomassEst) %>%
  filter(Year != 2001) %>% # remove 2001 estimates
  select(Year, SleeperShark, SalmonShark)


# 21. Squid
squid <- read.csv('Squid_AllSp_SA2015_Table8.csv', header=T, na.strings="")



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
Groundfish <- merge(Groundfish,bigSkatesAll,all.x=T)
Groundfish <- merge(Groundfish,longnoseSkatesAll,all.x=T)
Groundfish <- merge(Groundfish,otherSkatesAll,all.x=T)
Groundfish <- merge(Groundfish,dogfishAll,all.x=T)
Groundfish <- merge(Groundfish,moreSharksAll,all.x=T)




View(Groundfish)
#write.csv(Groundfish, file = "Groundfish.csv", row.names=FALSE)