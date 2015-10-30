#######################################################################
#####        Seward Line Total Zooplankton Analysis Script       ######
#####       Script by Colette Ward (ward at nceas.ucsb.edu)      ######
#####                         October 2015                       ######
#######################################################################

# Call data from source files
source('SewardLineSmallZoopProcessingScript.R')
View(MaySmallZoop) # these are biomass data; prefix 'sm' indicates Small stages sample with smaller net

source('SewardLineLgZoopProcessingScript.R') # does not work???
View(MayLgZoBiomass)
View(MayLgZoAbund)


# merge these datasets (*need to replace NA with 0 (zero))
MayZoop = full_join(MaySmallZoop, MayLgZoBiomass, by = "Year") # merge Small & Large May Zooplankton datasets
str(MayZoop)
#View(MayZoop)

FallZoop = full_join(FallSmallZoop, FallLgZoBiomass, by = "Year") # merge Small & Large Fall Zoop datasets
str(FallZoop)


# -------------------------------------------------------------------------
# May Zooplankton

# May Copepods
MayCopepods <- MayZoop %>%
  select(-SmPodonidae, -Mysids, -Euphausiids, -Salps, -Chaetnognatha, -Zoea, -Gastropoda)
#View(MayCopepods)

# sum May total copepod biomass
MayTotCopepodBiomass = MayCopepods %>%
  mutate(MayTotCopepodBiomass = rowSums(MayCopepods[,2:47], na.rm=T)) %>%
  select(Year, MayTotCopepodBiomass)
View(MayTotCopepodBiomass)


# Plot time series

# Copepods
ggplot(data=MayTotCopepodBiomass, aes(y=MayTotCopepodBiomass, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012)) +
  ylab("Mean May Total Copepod Biomass across GAK sites (g WW / m3)") +
  xlab("Year")


# Podonidae (Cladocerans)
ggplot(data=MayZoop, aes(y=SmPodonidae, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012)) +
  ylab("Mean May Podonidae Biomass across GAK sites (g WW / m3)") +
  xlab("Year")


# Euphausiids
ggplot(data=MayZoop, aes(y=Euphausiids, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012)) +
  ylab("Mean May Euphausiid Biomass across GAK sites (g WW / m3)") +
  xlab("Year")


# Mysids
ggplot(data=MayZoop, aes(y=Mysids, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012)) +
  ylab("Mean May Mysid Biomass across GAK sites (g WW / m3)") +
  xlab("Year")


# Salps: none

# Chaetnognaths: none


# Crab & Shrimp Zoea
ggplot(data=MayZoop, aes(y=Zoea, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012)) +
  ylab("Mean May Zoea Biomass across GAK sites (g WW / m3)") +
  xlab("Year")


# Gastropoda
ggplot(data=MayZoop, aes(y=Gastropoda, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012)) +
  ylab("Mean May Gastropoda Biomass across GAK sites (g WW / m3)") +
  xlab("Year")

# -------------------------------------------------------------------------
# Fall Zooplankton

# Fall Copepods
FallCopepods <- FallZoop %>%
  select(-SmPodonidae, -Mysids, -Euphausiids, -Salps, -Chaetnognatha, -Zoea, -Gastropoda, -Month.y)
View(FallCopepods)
dim(FallCopepods)

# sum May total copepod biomass PROBLEM WITH MONTH BEING INCLUDED IN SUM???
FallTotCopepodBiomass = FallCopepods %>%
  mutate(FallTotCopepodBiomass = rowSums(FallCopepods[,2:48], na.rm=T)) %>%
  select(Year, FallTotCopepodBiomass)
View(FallTotCopepodBiomass)


# Plot time series

# Copepods
ggplot(data=FallTotCopepodBiomass, aes(y=MayTotCopepodBiomass, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012)) +
  ylab("Mean May Total Copepod Biomass across GAK sites (g WW / m3)") +
  xlab("Year")


# Podonidae (Cladocerans)
ggplot(data=MayZoop, aes(y=SmPodonidae, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012)) +
  ylab("Mean May Podonidae Biomass across GAK sites (g WW / m3)") +
  xlab("Year")


# Euphausiids
ggplot(data=MayZoop, aes(y=Euphausiids, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012)) +
  ylab("Mean May Euphausiid Biomass across GAK sites (g WW / m3)") +
  xlab("Year")


# Mysids
ggplot(data=MayZoop, aes(y=Mysids, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012)) +
  ylab("Mean May Mysid Biomass across GAK sites (g WW / m3)") +
  xlab("Year")


# Salps: none

# Chaetnognaths: none


# Crab & Shrimp Zoea
ggplot(data=MayZoop, aes(y=Zoea, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012)) +
  ylab("Mean May Zoea Biomass across GAK sites (g WW / m3)") +
  xlab("Year")


# Gastropoda
ggplot(data=MayZoop, aes(y=Gastropoda, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012)) +
  ylab("Mean May Gastropoda Biomass across GAK sites (g WW / m3)") +
  xlab("Year")


# -------------------------------------------------------------------------

# sum small + large species-specific biomass