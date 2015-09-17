#######################################################################
#####  Seward Line Large Zooplankton (>0.3mg) Processing Script  ######
#####     Script by Colette Ward (ward@nceas.ucsb.edu)           ######
#####                     September 2015                         ######
#######################################################################

# Caution: Gear change in 2005 (335um mesh to 500um mesh) is not yet accounted for! (Not yet sure how to do this)

## load packages
library(plyr)
library(dplyr)
library(stringr)
library(httr)
library(ggplot2)
library(taxize)

# A. Load data: MOCNESS & MultiNet (meshSize = 335 & 500 um), 1998-2011:
URL_SMZo <- "http://gulfwatch.nceas.ucsb.edu/goa/d1/mn/v1/object/df35b.61.3"
SMZoGet <- GET(URL_SMZo)
SMZo1 <- content(SMZoGet, as='text')
SMZo <- read.csv(file=textConnection(SMZo1),stringsAsFactors=F)
head(SMZo)
str(SMZo)

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
         -year, -month, -day, -gearSize, -meshSize) 
# Error "In eval(substitute(expr), envir, enclos) : NAs introduced by coercion"
# This refers to 2 rows in dataset that are mostly NA - ie not in fact an error in code
str(SMZo2)

# -------------------------------------------------------------------------

# B. Clean up the species names

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

# -------------------------------------------------------------------------

# C. Add taxonomic information:

# Query ITIS for higher order taxonomic information:
SMZo2.c.specieslist <- unique(SMZo2.c$sciName)
tax.info = tax_name(query = SMZo2.c.specieslist, 
                    get = c("kingdom", "phylum", "subphylum", "class", "subclass", "infraclass", 
                            "order", "suborder", "infraorder", "suborder", "infraorder", "family", 
                            "genus", "species"), 
                    db = "itis")
tax.info1 = tax.info %>%
  mutate(sciName = query) %>%
  select(-db, -query)
View(tax.info1)
# code to create tax.info takes 16min to run, with sporadic inputs needed
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

# -------------------------------------------------------------------------

# D. Extract Seward Line (GAK) sites
SMZo2.d.GAK = SMZo2.d %>%
  filter(stationID %in% c("GAK1", "GAK2", "GAK3", "GAK4", "GAK5", "GAK6", "GAK7", "GAK8", "GAK9", "GAK10", "GAK11", "GAK12", "GAK13"))
head(SMZo2.d.GAK)

# View temporal distribution of samples:
plot(SMZo2.d.GAK$Year ~ SMZo2.d.GAK$Month, pch=16) # MOCNESS & Multi net

# -------------------------------------------------------------------------

# E. Integrate samples over 100m depth
# MOCNESS & MultiNets were collected for every 20m depth increment over the 100m depth of the water column
# to render these comparable to CalVET net, sum densities and biomass for all 20m depth increments for each sampling occasion

DepthIntGAK = aggregate(SMZo2.d.GAK$biomass, by = list(
  cruiseID = SMZo2.d.GAK$cruiseID,
  Year = SMZo2.d.GAK$Year,
  Month = SMZo2.d.GAK$Month,
  Day = SMZo2.d.GAK$Day,
  stationID = SMZo2.d.GAK$stationID,
  gear = SMZo2.d.GAK$gear,
  sciName = SMZo2.d.GAK$sciName,
  stage = SMZo2.d.GAK$stage),
  sum)
DepthIntGAK = DepthIntGAK %>%
  rename(biomass = x) # assign name to new column
View(DepthIntGAK)


# -------------------------------------------------------------------------

# F. Create May large zooplankton biomass

# From Hopcroft 2014 (unpubl data; oral presentation at Ocean Sciences Symposium):
# Large zooplankton: NMDS suggests that GAK 1-4 (sites in ACC) have different community structure than GAK 5-13 (shelf & offshore sites)
#       correlated with distance from shore, salinity, temperature
#       lg zoop biomass varies year-to-year; community struture varies less from year-to-year
# Small zooplankton: there appears to be a gradient in community structure across the shelf, but structure does not shift as strongly as lg zoop
#       sm zoop community structure is more variable from year-to-year


# Which net to use for which taxa?
# Coyle et al. 2013 (citing Coyle & Pinchuk 2003, 2005): 
#         see Table 2 in Coyle & Pinchuk 2003:
#         for large zooplankton (i.e. > 0.3mg) use MOCNESS & Multinet samples (335 & 500um mesh); 
#         for small zoop (<0.3mg) use CalVET net (150um mesh)


# NB copepod stages I, II, III, etc are the same as stages C1, C2, C3, etc ... (shift in nomenclature in the database in 2005)


# Messy script - clean this up!
# Merge again with taxonomic info 
DepthIntGAK.taxinfo <- merge(DepthIntGAK,tax.info1,all.x=T)
View(DepthIntGAK.taxinfo)


# Extract May samples:
May = DepthIntGAK.taxinfo %>%
  filter(Month == 5)

# Create dataframe with years:
MayLargeZoop=data.frame('Year'=c(1998:2010))


# F.i Copepod size / net classifications from Coyle & Pinchuk 2003:

# Aetideidae, stages V, Adults
Aetideidae = May %>%
  filter(family == "Aetideidae") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AetideidaeSite=sum(biomass)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(Aetideidae=mean(AetideidaeSite)) %>% 
  ungroup
#View(Aetideidae)
MayLargeZoop <- merge(MayLargeZoop,Aetideidae,all.x=T)
#View(MayLargeZoop)


# Calanus marshallae, stages IV, V, adults
Cmarshallae = May %>%
  filter(sciName == "Calanus marshallae") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(CmarshallaeSite=sum(biomass)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(Cmarshallae=mean(CmarshallaeSite)) %>%
  ungroup
#View(Cmarshallae)
MayLargeZoop <- merge(MayLargeZoop,Cmarshallae,all.x=T)
#View(MayLargeZoop)

# Calanus pacificus, stages V, adults
Cpacificus = May %>%
  filter(sciName == "Calanus pacificus") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(CpacificusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Cpacificus=mean(CpacificusSite)) %>% 
  ungroup
#View(Cpacificus)
MayLargeZoop <- merge(MayLargeZoop,Cpacificus,all.x=T)
#View(MayLargeZoop)

# Candacia, stages IV, V, adults
Candacia = May %>%
  filter(genus == "Candacia") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(CcolumbiaeSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Ccolumbiae=mean(CcolumbiaeSite)) %>% 
  ungroup
#View(Ccolumbiae) # none at GAK sites in May
MayLargeZoop <- merge(MayLargeZoop,Ccolumbiae,all.x=T)
#View(MayLargeZoop)


# Epilabidocera amphitrites, stages IV, V, adults;  add in Apr 30 sample from GAK2 in 2002 (cruiseID == hx258)
Eamphitrites = May %>%
  filter(species == "Epilabidocera amphitrites") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(EamphitritesSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Eamphitrites=mean(EamphitritesSite)) %>% 
  ungroup
#View(Eamphitrites) # none in May GAK samples


# Eucalanus bungii, stages IV, V, adults
Ebungii = May %>%
  filter(sciName == "Eucalanus bungii") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(EbungiiSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Ebungii=mean(EbungiiSite)) %>% 
  ungroup
#View(Ebungii)
MayLargeZoop <- merge(MayLargeZoop,Ebungii,all.x=T)
#View(MayLargeZoop)

# Euchaeta elongata, stages III-V, adults
# renamed to Paraeuchaeta elongata?
# Use ITIS to query this:
elongata = c("Euchaeta elongata", "Paraeuchaeta elongata")
tsn = get_tsn(elongata)
View(tsn) # Yes, E. elongata was renamed to P. elongata
Pelongata = May %>%
  filter(species %in% c("Paraeuchaeta elongata", "Elongata")) %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(PelongataSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Pelongata=mean(PelongataSite)) %>% 
  ungroup
#View(Pelongata)
MayLargeZoop <- merge(MayLargeZoop,Pelongata,all.x=T)
#View(MayLargeZoop)

# Heterorhabdus spp., stages IV, V, adults
Heterorhabdus = May %>%
  filter(genus == "Heterorhabdus") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(HeterorhabdusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Heterorhabdus=mean(HeterorhabdusSite)) %>% 
  ungroup
#View(Heterorhabdus)
MayLargeZoop <- merge(MayLargeZoop,Heterorhabdus,all.x=T)
#View(MayLargeZoop)

# Heterostylites spp., stages V, adults; none in May GAK samples
Heterostylites = May %>%
  filter(genus == "Heterostylites") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(HeterostylitesSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Heterostylites=mean(HeterostylitesSite)) %>% 
  ungroup
#View(Heterostylites)


# Lucicutia spp., stages V, adults
Lucicutia = May %>%
  filter(genus == "Lucicutia") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LucicutiaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Lucicutia=mean(LucicutiaSite)) %>% 
  ungroup
#View(Lucicutia)
MayLargeZoop <- merge(MayLargeZoop,Lucicutia,all.x=T)
#View(MayLargeZoop)


# Metridia okhotensis, stages V, adults
Mokhotensis = May %>%
  filter(sciName == "Metridia okhotensis") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(MokhotensisSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Mokhotensis=mean(MokhotensisSite)) %>% 
  ungroup
#View(Mokhotensis)
MayLargeZoop <- merge(MayLargeZoop,Mokhotensis,all.x=T)
#View(MayLargeZoop)

# Metridia pacifica, stages Females
Mpacifica = May %>%
  filter(sciName == "Metridia pacifica") %>% 
  filter(stage == "AF") %>%
  group_by(Year, stationID) %>%
  summarise(MpacificaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Mpacifica=mean(MpacificaSite)) %>% 
  ungroup
#View(Mpacifica)
MayLargeZoop <- merge(MayLargeZoop,Mpacifica,all.x=T)
#View(MayLargeZoop)

# Neocalanus cristatus, stages III-V, adults
Ncristatus = May %>%
  filter(sciName == "Neocalanus cristatus") %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(NcristatusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Ncristatus=mean(NcristatusSite)) %>% 
  ungroup
#View(Ncristatus)
MayLargeZoop <- merge(MayLargeZoop,Ncristatus,all.x=T)
#View(MayLargeZoop)

# Neocalanus plumchrus-flemingeri, stages IV, V, adults
Npflemingeri = May %>%
  filter(sciName %in% c("Neocalanus plumchrus", "Neocalanus flemingeri")) %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "CV_large", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(NpflemingeriSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Npflemingeri=mean(NpflemingeriSite)) %>% 
  ungroup
#View(Npflemingeri)
MayLargeZoop <- merge(MayLargeZoop,Npflemingeri,all.x=T)
#View(MayLargeZoop)

# Pleuromamma spp., stages V, adults 
Pleuromamma = May %>%
  filter(genus == "Pleuromamma") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(PleuromammaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Pleuromamma=mean(PleuromammaSite)) %>% 
  ungroup
#View(Pleuromamma)
MayLargeZoop <- merge(MayLargeZoop,Pleuromamma,all.x=T)
#View(MayLargeZoop)

# -----------------------

# F.ii Other copepod taxa present in CalVET nets but not assigned to size / net group by Coyle & Pinchuk 2003:

# Gaussia princeps   Follow Coyle & Pinchuk's classification for congenors Pleuromamma & Metridia
# None in GAK May samples
Gprinceps = May %>%
  filter(sciName == "Gaussia princeps") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(GprincepsSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Gprinceps=mean(GprincepsSite)) %>% 
  ungroup
#View(Gprinceps)
#MayLargeZoop <- merge(MayLargeZoop,Gprinceps,all.x=T)
#View(MayLargeZoop)


# Metridia sp. Follow Coyle & Pinchuk's 2003 classification for M. okhotensis
# ie take stages V, adults from large nets
Metridia = May %>%
  filter(sciName == "Metridia") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(MetridiaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Metridia=mean(MetridiaSite)) %>% 
  ungroup
#View(Metridia)
MayLargeZoop <- merge(MayLargeZoop,Metridia,all.x=T)
#View(MayLargeZoop)

# Monstrilla sp. (infraclass Neocopepod).  Not captured in large zoop nets


# Neocalanus sp.  Follow Coyle & Pinchuk's 2003 classification for N. cristatus
# ie take III-V, adults from large nets
Neocalanus = May %>%
  filter(sciName == "Neocalanus") %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(NeocalanusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Neocalanus=mean(NeocalanusSite)) %>% 
  ungroup
#View(Neocalanus)
MayLargeZoop <- merge(MayLargeZoop,Neocalanus,all.x=T)
#View(MayLargeZoop)


# -----------------------

# F.iii. Other non-copepod crustacean zooplankton present in CalVET nets

# Euphausiids all stages?
Euphausiids = May %>%
  filter(order == "Euphausiacea") %>% 
  group_by(Year, stationID) %>%
  summarise(EuphausiidsSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Euphausiids=mean(EuphausiidsSite)) %>% 
  ungroup
View(Euphausiids)
MayLargeZoop <- merge(MayLargeZoop,Euphausiids,all.x=T)
#View(MayLargeZoop)

ggplot(data=Euphausiids, aes(y=Euphausiids, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + scale_y_log10() + 
  ylab("Mean May Euphausiid Biomass across GAK sites (g WW / m3)") +
  xlab("Year")

# Mysids (all stages?)
Mysids = May %>%
  filter(order == "Mysida") %>% 
  group_by(Year, stationID) %>%
  summarise(MysidsSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Mysids=mean(MysidsSite)) %>% 
  ungroup
#View(Mysids)
MayLargeZoop <- merge(MayLargeZoop,Mysids,all.x=T)
#View(MayLargeZoop)

# Cnidaria
Cnidaria = May %>%
  filter(phylum == "Cnidaria") %>% 
  group_by(Year, stationID) %>%
  summarise(CnidariaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Mysids=mean(CnidariaSite)) %>% 
  ungroup
#View(Cnidaria)
MayLargeZoop <- merge(MayLargeZoop,Cnidaria,all.x=T)
#View(MayLargeZoop)

#Salps; none in May GAK samples
Salps = May %>%
  filter(family == "Salpidae") %>% 
  group_by(Year, stationID) %>%
  summarise(SalpsSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Salps=mean(SalpsSite)) %>% 
  ungroup
#View(Salps)
#MayLargeZoop <- merge(MayLargeZoop,Salps,all.x=T)
#View(MayLargeZoop)

# Chaetnognaths; none in May GAK samples
Chaetnognatha = May %>%
  filter(phylum == "Chaetnognatha") %>% 
  group_by(Year, stationID) %>%
  summarise(ChaetnognathaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Chaetnognatha=mean(ChaetnognathaSite)) %>% 
  ungroup
#View(Chaetnognatha)
#MayLargeZoop <- merge(MayLargeZoop,Chaetnognatha,all.x=T)
#View(MayLargeZoop)

#Crab & shrimp zoea
Zoea = May %>%
  filter(stage %in% c("zoea", "zoea_stage_1")) %>% 
  group_by(Year, stationID) %>%
  summarise(ZoeaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Zoea=mean(ZoeaSite)) %>% 
  ungroup
#View(Zoea)
MayLargeZoop <- merge(MayLargeZoop,Zoea,all.x=T)
#View(MayLargeZoop)

# Gastropods
Gastropoda = May %>%
  filter(class == "Gastropoda") %>% 
  group_by(Year, stationID) %>%
  summarise(GastropodaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Gastropoda=mean(GastropodaSite)) %>% 
  ungroup
#View(Gastropoda)
MayLargeZoop <- merge(MayLargeZoop,Gastropoda,all.x=T)
#View(MayLargeZoop)

# end product:
View(MayLargeZoop)

