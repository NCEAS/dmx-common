#######################################################################
#####  Seward Line Large Zooplankton (>0.3mg) Processing Script  ######
#####     Script by Colette Ward (ward@nceas.ucsb.edu)           ######
#####                     September 2015                         ######
#######################################################################

# Call data from source file
source('SewardLineLgZoopCleaningScript.R')
View(SMZo4)

# Integrate over 100m depth
# MOCNESS & MultiNets were collected for every 20m depth increment over the 100m depth of the water column
# to render these comparable to CalVET net, sum densities and biomass for all 20m depth increments for each sampling occasion

# Biomass:
DepthInt.b = aggregate(SMZo4$biomass, by = list(
  cruiseID = SMZo4$cruiseID,
  Year = SMZo4$Year,
  Month = SMZo4$Month,
  Day = SMZo4$Day,
  stationID = SMZo4$stationID,
  gear = SMZo4$gear,
  sciName = SMZo4$sciName,
  stage = SMZo4$stage),
  sum)
DepthInt.b = DepthInt.b %>%
  rename(biomass = x) # assign name to new column
View(DepthInt.b)
dim(DepthInt.b) # 50561     9


# Abundance:
DepthInt.a = aggregate(SMZo4$abundance, by = list(
  cruiseID = SMZo4$cruiseID,
  Year = SMZo4$Year,
  Month = SMZo4$Month,
  Day = SMZo4$Day,
  stationID = SMZo4$stationID,
  gear = SMZo4$gear,
  sciName = SMZo4$sciName,
  stage = SMZo4$stage),
  sum)
DepthInt.a = DepthInt.a %>%
  rename(abundance = x) # assign name to new column
View(DepthInt.a)
dim(DepthInt.a) # 50561     9

# Merge depth-integrated biomass and abundance:
DepthInt<- merge(DepthInt.a,DepthInt.b,all.x=T)
View(DepthInt)
dim(DepthInt) # should be   50561    10

# Merge with taxonomic info 
DepthInt.taxinfo <- merge(DepthInt,tax.info1,all.x=T)
View(DepthInt.taxinfo)
dim(DepthInt.taxinfo) # should be 50561    22


# -------------------------------------------------------------------------
# D. Extract Seward Line (GAK) sites
GAK = DepthInt.taxinfo %>%
  filter(stationID %in% c("GAK1", "GAK2", "GAK3", "GAK4", "GAK5", "GAK6", "GAK7", "GAK8", "GAK9", "GAK10", "GAK11", "GAK12", "GAK13"))
head(GAK)

# View temporal distribution of samples:
plot(GAK$Year ~ GAK$Month, pch=16) # MOCNESS & Multi net


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

# Extract May samples:
May = GAK %>%
  filter(Month == 5)

# Create dataframe with years:
MayLgZoBiomass=data.frame('Year'=c(1998:2010))

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
MayLgZoBiomass <- merge(MayLgZoBiomass,Aetideidae,all.x=T)
#View(MayLgZoBiomass)


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
MayLgZoBiomass <- merge(MayLgZoBiomass,Cmarshallae,all.x=T)
#View(MayLgZoBiomass)

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
MayLgZoBiomass <- merge(MayLgZoBiomass,Cpacificus,all.x=T)
#View(MayLgZoBiomass)

# Candacia, stages IV, V, adults
Candacia = May %>%
  filter(genus == "Candacia") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(CandaciaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Candacia=mean(CandaciaSite)) %>% 
  ungroup
#View(Candacia) # none at GAK sites in May
MayLgZoBiomass <- merge(MayLgZoBiomass,Candacia,all.x=T)
#View(MayLgZoBiomass)


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
MayLgZoBiomass <- merge(MayLgZoBiomass,Ebungii,all.x=T)
#View(MayLgZoBiomass)

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
MayLgZoBiomass <- merge(MayLgZoBiomass,Pelongata,all.x=T)
#View(MayLgZoBiomass)

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
MayLgZoBiomass <- merge(MayLgZoBiomass,Heterorhabdus,all.x=T)
#View(MayLgZoBiomass)

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
MayLgZoBiomass <- merge(MayLgZoBiomass,Lucicutia,all.x=T)
#View(MayLgZoBiomass)


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
MayLgZoBiomass <- merge(MayLgZoBiomass,Mokhotensis,all.x=T)
#View(MayLgZoBiomass)

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
MayLgZoBiomass <- merge(MayLgZoBiomass,Mpacifica,all.x=T)
#View(MayLgZoBiomass)

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
MayLgZoBiomass <- merge(MayLgZoBiomass,Ncristatus,all.x=T)
#View(MayLgZoBiomass)

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
MayLgZoBiomass <- merge(MayLgZoBiomass,Npflemingeri,all.x=T)
#View(MayLgZoBiomass)

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
MayLgZoBiomass <- merge(MayLgZoBiomass,Pleuromamma,all.x=T)
#View(MayLgZoBiomass)

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
#MayLgZoBiomass <- merge(MayLgZoBiomass,Gprinceps,all.x=T)
#View(MayLgZoBiomass)


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
MayLgZoBiomass <- merge(MayLgZoBiomass,Metridia,all.x=T)
#View(MayLgZoBiomass)

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
MayLgZoBiomass <- merge(MayLgZoBiomass,Neocalanus,all.x=T)
#View(MayLgZoBiomass)


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
MayLgZoBiomass <- merge(MayLgZoBiomass,Euphausiids,all.x=T)
#View(MayLgZoBiomass)

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
MayLgZoBiomass <- merge(MayLgZoBiomass,Mysids,all.x=T)
#View(MayLgZoBiomass)

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
MayLgZoBiomass <- merge(MayLgZoBiomass,Cnidaria,all.x=T)
#View(MayLgZoBiomass)

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
#MayLgZoBiomass <- merge(MayLgZoBiomass,Chaetnognatha,all.x=T)
#View(MayLgZoBiomass)

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
MayLgZoBiomass <- merge(MayLgZoBiomass,Zoea,all.x=T)
#View(MayLgZoBiomass)

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
MayLgZoBiomass <- merge(MayLgZoBiomass,Gastropoda,all.x=T)
#View(MayLgZoBiomass)

# end product:
View(MayLgZoBiomass)


# -------------------------------------------------------------------------

# G. Create May large zooplankton abundance

# Extract May samples:
May = DepthIntGAK.taxinfo %>%
  filter(Month == 5)

# Create dataframe with years:
MayLgZoAbund=data.frame('Year'=c(1998:2010))

# G.i Copepod size / net classifications from Coyle & Pinchuk 2003:

# Aetideidae, stages V, Adults
Aetideidae = May %>%
  filter(family == "Aetideidae") %>%
  #filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  filter(stage %in% c("AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AetideidaeSite=sum(abundance)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(Aetideidae=mean(AetideidaeSite)) %>% 
  ungroup
#View(Aetideidae)
MayLgZoAbund <- merge(MayLgZoAbund,Aetideidae,all.x=T)
#View(MayLgZoAbund)


# Calanus marshallae, stages IV, V, adults
Cmarshallae = May %>%
  filter(sciName == "Calanus marshallae") %>% 
  #filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  filter(stage %in% c("AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(CmarshallaeSite=sum(abundance)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(Cmarshallae=mean(CmarshallaeSite)) %>%
  ungroup
#View(Cmarshallae)
MayLgZoAbund <- merge(MayLgZoAbund,Cmarshallae,all.x=T)
#View(MayLgZoAbund)

# Calanus pacificus, stages V, adults
Cpacificus = May %>%
  filter(sciName == "Calanus pacificus") %>%
  #filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  filter(stage %in% c("AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(CpacificusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Cpacificus=mean(CpacificusSite)) %>% 
  ungroup
#View(Cpacificus)
MayLgZoAbund <- merge(MayLgZoAbund,Cpacificus,all.x=T)
#View(MayLgZoAbund)

# Candacia, stages IV, V, adults
Candacia = May %>%
  filter(genus == "Candacia") %>% 
  #filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  filter(stage %in% c("AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(CandaciaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Candacia=mean(CandaciaSite)) %>% 
  ungroup
#View(Candacia) # none at GAK sites in May
MayLgZoAbund <- merge(MayLgZoAbund,Candacia,all.x=T)
#View(MayLgZoAbund)


# Epilabidocera amphitrites, stages IV, V, adults;  add in Apr 30 sample from GAK2 in 2002 (cruiseID == hx258)
Eamphitrites = May %>%
  filter(species == "Epilabidocera amphitrites") %>% 
  #filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  filter(stage %in% c("AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(EamphitritesSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Eamphitrites=mean(EamphitritesSite)) %>% 
  ungroup
#View(Eamphitrites) # none in May GAK samples


# Eucalanus bungii, stages IV, V, adults
Ebungii = May %>%
  filter(sciName == "Eucalanus bungii") %>% 
  #filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  filter(stage %in% c("AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(EbungiiSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Ebungii=mean(EbungiiSite)) %>% 
  ungroup
#View(Ebungii)
MayLgZoAbund <- merge(MayLgZoAbund,Ebungii,all.x=T)
#View(MayLgZoAbund)

ggplot(data=Ebungii, aes(y=Ebungii, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + scale_y_log10() + 
  ylab("Mean May Eucalanus bungii Abundance across GAK sites (g WW / m3)") +
  xlab("Year")


# Euchaeta elongata, stages III-V, adults
# renamed to Paraeuchaeta elongata?
# Use ITIS to query this:
elongata = c("Euchaeta elongata", "Paraeuchaeta elongata")
tsn = get_tsn(elongata)
View(tsn) # Yes, E. elongata was renamed to P. elongata
Pelongata = May %>%
  filter(species %in% c("Paraeuchaeta elongata", "Elongata")) %>% 
  #filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  filter(stage %in% c("AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(PelongataSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Pelongata=mean(PelongataSite)) %>% 
  ungroup
#View(Pelongata)
MayLgZoAbund <- merge(MayLgZoAbund,Pelongata,all.x=T)
#View(MayLgZoAbund)

# Heterorhabdus spp., stages IV, V, adults
Heterorhabdus = May %>%
  filter(genus == "Heterorhabdus") %>% 
  #filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  filter(stage %in% c("AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(HeterorhabdusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Heterorhabdus=mean(HeterorhabdusSite)) %>% 
  ungroup
#View(Heterorhabdus)
MayLgZoAbund <- merge(MayLgZoAbund,Heterorhabdus,all.x=T)
#View(MayLgZoAbund)

# Heterostylites spp., stages V, adults; none in May GAK samples
Heterostylites = May %>%
  filter(genus == "Heterostylites") %>% 
  #filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  filter(stage %in% c("AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(HeterostylitesSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Heterostylites=mean(HeterostylitesSite)) %>% 
  ungroup
#View(Heterostylites)


# Lucicutia spp., stages V, adults
Lucicutia = May %>%
  filter(genus == "Lucicutia") %>% 
  #filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  filter(stage %in% c("AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LucicutiaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Lucicutia=mean(LucicutiaSite)) %>% 
  ungroup
#View(Lucicutia)
MayLgZoAbund <- merge(MayLgZoAbund,Lucicutia,all.x=T)
#View(MayLgZoAbund)


# Metridia okhotensis, stages V, adults
Mokhotensis = May %>%
  filter(sciName == "Metridia okhotensis") %>% 
  #filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  filter(stage %in% c("AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(MokhotensisSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Mokhotensis=mean(MokhotensisSite)) %>% 
  ungroup
#View(Mokhotensis)
MayLgZoAbund <- merge(MayLgZoAbund,Mokhotensis,all.x=T)
#View(MayLgZoAbund)

# Metridia pacifica, stages Females
Mpacifica = May %>%
  filter(sciName == "Metridia pacifica") %>% 
  filter(stage == "AF") %>%
  group_by(Year, stationID) %>%
  summarise(MpacificaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Mpacifica=mean(MpacificaSite)) %>% 
  ungroup
#View(Mpacifica)
MayLgZoAbund <- merge(MayLgZoAbund,Mpacifica,all.x=T)
#View(MayLgZoAbund)

ggplot(data=Mpacifica, aes(y=Mpacifica, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + scale_y_log10() + 
  ylab("Mean May Metridia pacifica Abundance across GAK sites (g WW / m3)") +
  xlab("Year")


# Neocalanus cristatus, stages III-V, adults
Ncristatus = May %>%
  filter(sciName == "Neocalanus cristatus") %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  #filter(stage %in% c("AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(NcristatusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Ncristatus=mean(NcristatusSite)) %>% 
  ungroup
View(Ncristatus)
MayLgZoAbund <- merge(MayLgZoAbund,Ncristatus,all.x=T)
#View(MayLgZoAbund)

ggplot(data=Ncristatus, aes(y=Ncristatus, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + scale_y_log10() + 
  ylab("Mean May Neocalanus cristatus Abundance across GAK sites (g WW / m3)") +
  xlab("Year")


# Neocalanus plumchrus-flemingeri, stages IV, V, adults
Npflemingeri = May %>%
  filter(sciName %in% c("Neocalanus plumchrus", "Neocalanus flemingeri")) %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "CV_large", "AF", "AM")) %>%
  #filter(stage %in% c("AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(NpflemingeriSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Npflemingeri=mean(NpflemingeriSite)) %>% 
  ungroup
View(Npflemingeri)
MayLgZoAbund <- merge(MayLgZoAbund,Npflemingeri,all.x=T)
#View(MayLgZoAbund)

ggplot(data=Npflemingeri, aes(y=Npflemingeri, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + scale_y_log10() + 
  ylab("Mean May Neocalanus flemingeri / plumchurus Abundance across GAK sites (g WW / m3)") +
  xlab("Year")

# Pleuromamma spp., stages V, adults 
Pleuromamma = May %>%
  filter(genus == "Pleuromamma") %>% 
  #filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  filter(stage %in% c("AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(PleuromammaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Pleuromamma=mean(PleuromammaSite)) %>% 
  ungroup
#View(Pleuromamma)
MayLgZoAbund <- merge(MayLgZoAbund,Pleuromamma,all.x=T)
#View(MayLgZoAbund)

# -----------------------

# G.ii Other copepod taxa present in CalVET nets but not assigned to size / net group by Coyle & Pinchuk 2003:

# Gaussia princeps   Follow Coyle & Pinchuk's classification for congenors Pleuromamma & Metridia
# None in GAK May samples
Gprinceps = May %>%
  filter(sciName == "Gaussia princeps") %>%
  #filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  filter(stage %in% c("AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(GprincepsSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Gprinceps=mean(GprincepsSite)) %>% 
  ungroup
#View(Gprinceps)
#MayLgZoAbund <- merge(MayLgZoAbund,Gprinceps,all.x=T)
#View(MayLgZoAbund)


# Metridia sp. Follow Coyle & Pinchuk's 2003 classification for M. okhotensis
# ie take stages V, adults from large nets
Metridia = May %>%
  filter(sciName == "Metridia") %>%
  #filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  filter(stage %in% c("AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(MetridiaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Metridia=mean(MetridiaSite)) %>% 
  ungroup
#View(Metridia)
MayLgZoAbund <- merge(MayLgZoAbund,Metridia,all.x=T)
#View(MayLgZoAbund)

# Monstrilla sp. (infraclass Neocopepod).  Not captured in large zoop nets


# Neocalanus sp.  Follow Coyle & Pinchuk's 2003 classification for N. cristatus
# ie take III-V, adults from large nets
Neocalanus = May %>%
  filter(sciName == "Neocalanus") %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  #filter(stage %in% c("AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(NeocalanusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Neocalanus=mean(NeocalanusSite)) %>% 
  ungroup
View(Neocalanus)
MayLgZoAbund <- merge(MayLgZoAbund,Neocalanus,all.x=T)
#View(MayLgZoAbund)

ggplot(data=Neocalanus, aes(y=Neocalanus, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + scale_y_log10() + 
  ylab("Mean May Neocalanus Abundance across GAK sites (g WW / m3)") +
  xlab("Year")


# -----------------------

# G.iii. Other non-copepod crustacean zooplankton present in CalVET nets

# Euphausiids all stages?
Euphausiids = May %>%
  filter(order == "Euphausiacea") %>% 
  group_by(Year, stationID) %>%
  summarise(EuphausiidsSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Euphausiids=mean(EuphausiidsSite)) %>% 
  ungroup
View(Euphausiids)
MayLgZoAbund <- merge(MayLgZoAbund,Euphausiids,all.x=T)
#View(MayLgZoAbund)

ggplot(data=Euphausiids, aes(y=Euphausiids, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + scale_y_log10() + 
  ylab("Mean May Euphausiid Abundance across GAK sites (g WW / m3)") +
  xlab("Year")

# Mysids (all stages?)
Mysids = May %>%
  filter(order == "Mysida") %>% 
  group_by(Year, stationID) %>%
  summarise(MysidsSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Mysids=mean(MysidsSite)) %>% 
  ungroup
#View(Mysids)
MayLgZoAbund <- merge(MayLgZoAbund,Mysids,all.x=T)
#View(MayLgZoAbund)

# Cnidaria
Cnidaria = May %>%
  filter(phylum == "Cnidaria") %>% 
  group_by(Year, stationID) %>%
  summarise(CnidariaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Mysids=mean(CnidariaSite)) %>% 
  ungroup
#View(Cnidaria)
MayLgZoAbund <- merge(MayLgZoAbund,Cnidaria,all.x=T)
#View(MayLgZoAbund)

#Salps; none in May GAK samples
Salps = May %>%
  filter(family == "Salpidae") %>% 
  group_by(Year, stationID) %>%
  summarise(SalpsSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Salps=mean(SalpsSite)) %>% 
  ungroup
#View(Salps)
#MayLgZoAbund <- merge(MayLgZoAbund,Salps,all.x=T)
#View(MayLgZoAbund)

# Chaetnognaths; none in May GAK samples
Chaetnognatha = May %>%
  filter(phylum == "Chaetnognatha") %>% 
  group_by(Year, stationID) %>%
  summarise(ChaetnognathaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Chaetnognatha=mean(ChaetnognathaSite)) %>% 
  ungroup
#View(Chaetnognatha)
#MayLgZoAbund <- merge(MayLgZoAbund,Chaetnognatha,all.x=T)
#View(MayLgZoAbund)

#Crab & shrimp zoea
Zoea = May %>%
  filter(stage %in% c("zoea", "zoea_stage_1")) %>% 
  group_by(Year, stationID) %>%
  summarise(ZoeaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Zoea=mean(ZoeaSite)) %>% 
  ungroup
#View(Zoea)
MayLgZoAbund <- merge(MayLgZoAbund,Zoea,all.x=T)
#View(MayLgZoAbund)

# Gastropods
Gastropoda = May %>%
  filter(class == "Gastropoda") %>% 
  group_by(Year, stationID) %>%
  summarise(GastropodaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Gastropoda=mean(GastropodaSite)) %>% 
  ungroup
#View(Gastropoda)
MayLgZoAbund <- merge(MayLgZoAbund,Gastropoda,all.x=T)
#View(MayLgZoAbund)

# end product:
View(MayLgZoAbund)

