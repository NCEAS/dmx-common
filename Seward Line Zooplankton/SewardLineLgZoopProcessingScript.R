#######################################################################
#####  Seward Line Large Zooplankton (>0.3mg) Processing Script  ######
#####     Script by Colette Ward (ward@nceas.ucsb.edu)           ######
#####                     September 2015                         ######
#######################################################################

# Call data from source file
source('SewardLineLgZoopCleaningScript.R')
#View(SMZo4)

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
#View(DepthInt.b)
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
#View(DepthInt.a)
dim(DepthInt.a) # 50561     9

# Merge depth-integrated biomass and abundance:
DepthInt<- merge(DepthInt.a,DepthInt.b,all.x=T)
#View(DepthInt)
dim(DepthInt) # should be   50561    10

# Merge with taxonomic info 
DepthInt.taxinfo <- merge(DepthInt,tax.info1,all.x=T)
#View(DepthInt.taxinfo)
dim(DepthInt.taxinfo) # should be 50561    22


# -------------------------------------------------------------------------

# Extract Seward Line (GAK) sites
GAK = DepthInt.taxinfo %>%
  filter(stationID %in% c("GAK1", "GAK2", "GAK3", "GAK4", "GAK5", "GAK6", "GAK7", "GAK8", "GAK9", "GAK10", "GAK11", "GAK12", "GAK13"))
head(GAK)

# View temporal distribution of samples:
plot(GAK$Year ~ GAK$Month, pch=16) # MOCNESS & Multi net


# -------------------------------------------------------------------------

# Create May large zooplankton biomass

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

# 1. Copepod size / net classifications from Coyle & Pinchuk 2003:

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


# -----------------------

# 2. Other copepod taxa present but not assigned to size / net group by Coyle & Pinchuk 2003:

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


# -----------------------

# 3. Other non-copepod crustacean zooplankton

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


# -----------------------

# Create dataframe with years:
MayLgZoBiomass=data.frame('Year'=c(1998:2010))

# Merge in the taxon-specific biomass data
MayLgZoBiomass <- merge(MayLgZoBiomass,Aetideidae,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Cmarshallae,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Cpacificus,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Candacia,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Eamphitrites,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Ebungii,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Pelongata,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Heterorhabdus,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Heterostylites,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Lucicutia,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Mokhotensis,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Mpacifica,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Ncristatus,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Npflemingeri,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Pleuromamma,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Gprinceps,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Metridia,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Neocalanus,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Euphausiids,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Mysids,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Cnidaria,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Salps,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Chaetnognatha,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Zoea,all.x=T)
MayLgZoBiomass <- merge(MayLgZoBiomass,Gastropoda,all.x=T)

#View(MayLgZoBiomass)


# ---------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------

# Extract May samples:
Fall = GAK %>%
  filter(Month >= 9) # select Sept & Oct samples
unique(sort(Fall$Month))

# 1. Copepod size / net classifications from Coyle & Pinchuk 2003:

# Aetideidae, stages V, Adults
Aetideidae = Fall %>%
  filter(family == "Aetideidae") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AetideidaeSite=sum(biomass)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(Aetideidae=mean(AetideidaeSite)) %>% 
  ungroup
#View(Aetideidae)


# Calanus marshallae, stages IV, V, adults
Cmarshallae = Fall %>%
  filter(sciName == "Calanus marshallae") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(CmarshallaeSite=sum(biomass)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(Cmarshallae=mean(CmarshallaeSite)) %>%
  ungroup
#View(Cmarshallae)


# Calanus pacificus, stages V, adults
Cpacificus = Fall %>%
  filter(sciName == "Calanus pacificus") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(CpacificusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Cpacificus=mean(CpacificusSite)) %>% 
  ungroup
#View(Cpacificus)


# Candacia, stages IV, V, adults
Candacia = Fall %>%
  filter(genus == "Candacia") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(CandaciaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Candacia=mean(CandaciaSite)) %>% 
  ungroup
#View(Candacia) # none at GAK sites in May


# Epilabidocera amphitrites, stages IV, V, adults;  add in Apr 30 sample from GAK2 in 2002 (cruiseID == hx258)
Eamphitrites = Fall %>%
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
Ebungii = Fall %>%
  filter(sciName == "Eucalanus bungii") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(EbungiiSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Ebungii=mean(EbungiiSite)) %>% 
  ungroup
#View(Ebungii)


# Euchaeta elongata, stages III-V, adults
Pelongata = Fall %>%
  filter(species %in% c("Paraeuchaeta elongata", "Elongata")) %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(PelongataSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Pelongata=mean(PelongataSite)) %>% 
  ungroup
#View(Pelongata)


# Heterorhabdus spp., stages IV, V, adults
Heterorhabdus = Fall %>%
  filter(genus == "Heterorhabdus") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(HeterorhabdusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Heterorhabdus=mean(HeterorhabdusSite)) %>% 
  ungroup
#View(Heterorhabdus)


# Heterostylites spp., stages V, adults; none in May GAK samples
Heterostylites = Fall %>%
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
Lucicutia = Fall %>%
  filter(genus == "Lucicutia") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LucicutiaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Lucicutia=mean(LucicutiaSite)) %>% 
  ungroup
#View(Lucicutia)


# Metridia okhotensis, stages V, adults
Mokhotensis = Fall %>%
  filter(sciName == "Metridia okhotensis") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(MokhotensisSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Mokhotensis=mean(MokhotensisSite)) %>% 
  ungroup
#View(Mokhotensis)


# Metridia pacifica, stages Females
Mpacifica = Fall %>%
  filter(sciName == "Metridia pacifica") %>% 
  filter(stage == "AF") %>%
  group_by(Year, stationID) %>%
  summarise(MpacificaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Mpacifica=mean(MpacificaSite)) %>% 
  ungroup
#View(Mpacifica)


# Neocalanus cristatus, stages III-V, adults
Ncristatus = Fall %>%
  filter(sciName == "Neocalanus cristatus") %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(NcristatusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Ncristatus=mean(NcristatusSite)) %>% 
  ungroup
#View(Ncristatus)


# Neocalanus plumchrus-flemingeri, stages IV, V, adults
Npflemingeri = Fall %>%
  filter(sciName %in% c("Neocalanus plumchrus", "Neocalanus flemingeri")) %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "CV_large", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(NpflemingeriSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Npflemingeri=mean(NpflemingeriSite)) %>% 
  ungroup
#View(Npflemingeri)


# Pleuromamma spp., stages V, adults 
Pleuromamma = Fall %>%
  filter(genus == "Pleuromamma") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(PleuromammaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Pleuromamma=mean(PleuromammaSite)) %>% 
  ungroup
#View(Pleuromamma)


# -----------------------

# 2. Other copepod taxa present but not assigned to size / net group by Coyle & Pinchuk 2003:

# Gaussia princeps   Follow Coyle & Pinchuk's classification for congenors Pleuromamma & Metridia
# None in GAK May samples
Gprinceps = Fall %>%
  filter(sciName == "Gaussia princeps") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(GprincepsSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Gprinceps=mean(GprincepsSite)) %>% 
  ungroup
#View(Gprinceps)


# Metridia sp. Follow Coyle & Pinchuk's 2003 classification for M. okhotensis
# ie take stages V, adults from large nets
Metridia = Fall %>%
  filter(sciName == "Metridia") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(MetridiaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Metridia=mean(MetridiaSite)) %>% 
  ungroup
#View(Metridia)


# Monstrilla sp. (infraclass Neocopepod).  Not captured in large zoop nets


# Neocalanus sp.  Follow Coyle & Pinchuk's 2003 classification for N. cristatus
# ie take III-V, adults from large nets
Neocalanus = Fall %>%
  filter(sciName == "Neocalanus") %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(NeocalanusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Neocalanus=mean(NeocalanusSite)) %>% 
  ungroup
#View(Neocalanus)


# -----------------------

# 3. Other non-copepod crustacean zooplankton

# Euphausiids all stages?
Euphausiids = Fall %>%
  filter(order == "Euphausiacea") %>% 
  group_by(Year, stationID) %>%
  summarise(EuphausiidsSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Euphausiids=mean(EuphausiidsSite)) %>% 
  ungroup
View(Euphausiids)

ggplot(data=Euphausiids, aes(y=Euphausiids, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + scale_y_log10() + 
  ylab("Mean May Euphausiid Biomass across GAK sites (g WW / m3)") +
  xlab("Year")

# Mysids (all stages?)
Mysids = Fall %>%
  filter(order == "Mysida") %>% 
  group_by(Year, stationID) %>%
  summarise(MysidsSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Mysids=mean(MysidsSite)) %>% 
  ungroup
#View(Mysids)


# Cnidaria
Cnidaria = Fall %>%
  filter(phylum == "Cnidaria") %>% 
  group_by(Year, stationID) %>%
  summarise(CnidariaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Mysids=mean(CnidariaSite)) %>% 
  ungroup
#View(Cnidaria)


#Salps; none in May GAK samples
Salps = Fall %>%
  filter(family == "Salpidae") %>% 
  group_by(Year, stationID) %>%
  summarise(SalpsSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Salps=mean(SalpsSite)) %>% 
  ungroup
#View(Salps)


# Chaetnognaths; none in May GAK samples
Chaetnognatha = Fall %>%
  filter(phylum == "Chaetnognatha") %>% 
  group_by(Year, stationID) %>%
  summarise(ChaetnognathaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Chaetnognatha=mean(ChaetnognathaSite)) %>% 
  ungroup
#View(Chaetnognatha)


#Crab & shrimp zoea
Zoea = Fall %>%
  filter(stage %in% c("zoea", "zoea_stage_1")) %>% 
  group_by(Year, stationID) %>%
  summarise(ZoeaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Zoea=mean(ZoeaSite)) %>% 
  ungroup
#View(Zoea)


# Gastropods
Gastropoda = Fall %>%
  filter(class == "Gastropoda") %>% 
  group_by(Year, stationID) %>%
  summarise(GastropodaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Gastropoda=mean(GastropodaSite)) %>% 
  ungroup
#View(Gastropoda)


# -----------------------

# Create dataframe with years:
FallLgZoBiomass=data.frame('Year'=c(1998:2010))

# Merge in the taxon-specific biomass data
FallLgZoBiomass <- merge(FallLgZoBiomass,Aetideidae,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Cmarshallae,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Cpacificus,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Candacia,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Eamphitrites,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Ebungii,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Pelongata,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Heterorhabdus,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Heterostylites,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Lucicutia,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Mokhotensis,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Mpacifica,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Ncristatus,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Npflemingeri,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Pleuromamma,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Gprinceps,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Metridia,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Neocalanus,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Euphausiids,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Mysids,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Cnidaria,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Salps,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Chaetnognatha,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Zoea,all.x=T)
FallLgZoBiomass <- merge(FallLgZoBiomass,Gastropoda,all.x=T)

View(FallLgZoBiomass)

FallLgZoBiomass = FallLgZoBiomass %>%
  mutate(Month = as.numeric(c(10, 10, 10, 10, 10, 10, 10, 9, 9, 9, 9, 9, 9)))
  
FallLgCopepods <- FallLgZoBiomass %>%
  select(-Mysids, -Euphausiids, -Salps, -Chaetnognatha, -Zoea, -Gastropoda)
View(FallLgCopepods)
unique(sort(FallLgCopepods$Year))
# sum Fall total large copepod biomass
FallTotLgCopepodBiomass = FallLgCopepods %>%
  mutate(FallTotLgCopepodBiomass = rowSums(FallLgCopepods[,2:19], na.rm=T)) %>%
  select(Year, Month, FallTotLgCopepodBiomass)
View(FallTotLgCopepodBiomass)

# Plot Large Copepod Biomass:
ggplot(data=FallTotLgCopepodBiomass, aes(y=FallTotLgCopepodBiomass, x=Year, colour=as.factor(Month))) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012)) + coord_cartesian(ylim = c(0.04, 0.2)) +
  ylab("Mean Fall Total Large Copepod Biomass across GAK sites (g WW / m3)") +
  xlab("Year")

# Plot Euphausiid Biomass:
ggplot(data=FallLgZoBiomass, aes(y=Euphausiids, x=Year, colour=as.factor(Month))) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012)) + coord_cartesian(ylim = c(0.01, 0.7)) +
  ylab("Mean Fall Euphausiid Biomass across GAK sites (g WW / m3)") +
  xlab("Year")



AugLg <- FallLgZoBiomass
SeptLg <- FallLgZoBiomass
OctLg <- FallLgZoBiomass

rm(FallLgZoBiomass)

# Aug LgCopepods
AugLgCopepods <- AugLg %>%
  select(-Mysids, -Euphausiids, -Salps, -Chaetnognatha, -Zoea, -Gastropoda)
View(AugLgCopepods)
# sum Aug total lg copepod biomass
AugTotLgCopepodBiomass = AugLgCopepods %>%
  mutate(AugTotLgCopepodBiomass = rowSums(AugLgCopepods[,2:19], na.rm=T)) %>%
  select(Year, AugTotLgCopepodBiomass)
View(AugTotLgCopepodBiomass)

# Sept LgCopepods
SeptLgCopepods <- SeptLg %>%
  select(-Mysids, -Euphausiids, -Salps, -Chaetnognatha, -Zoea, -Gastropoda)
View(SeptLgCopepods)
# sum Aug total lg copepod biomass
SeptTotLgCopepodBiomass = SeptLgCopepods %>%
  mutate(SeptTotLgCopepodBiomass = rowSums(SeptLgCopepods[,2:19], na.rm=T)) %>%
  select(Year, SeptTotLgCopepodBiomass)
View(SeptTotLgCopepodBiomass)

# Oct LgCopepods
OctLgCopepods <- OctLg %>%
  select(-Mysids, -Euphausiids, -Salps, -Chaetnognatha, -Zoea, -Gastropoda)
View(OctLgCopepods)
# sum Aug total lg copepod biomass
OctTotLgCopepodBiomass = OctLgCopepods %>%
  mutate(OctTotLgCopepodBiomass = rowSums(OctLgCopepods[,2:19], na.rm=T)) %>%
  select(Year, OctTotLgCopepodBiomass)
View(OctTotLgCopepodBiomass)

ggplot(AugTotLgCopepodBiomass, aes(y=AugTotLgCopepodBiomass, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012), ylim = c(0.01, 0.4)) +
  ylab("Mean Aug Total Lg Copepod Biomass across GAK sites (g WW / m3)") +
  xlab("Year")

ggplot(SeptTotLgCopepodBiomass, aes(y=SeptTotLgCopepodBiomass, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012), ylim = c(0.01, 0.4)) +
  ylab("Mean Sept Total Lg Copepod Biomass across GAK sites (g WW / m3)") +
  xlab("Year")

ggplot(OctTotLgCopepodBiomass, aes(y=OctTotLgCopepodBiomass, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012), ylim = c(0.01, 0.4)) +
  ylab("Mean Oct Total Lg Copepod Biomass across GAK sites (g WW / m3)") +
  xlab("Year")


# Euphausiids; look similar across months:
ggplot(AugLg, aes(y=Euphausiids, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012), ylim = c(0.01, 0.7)) +
  ylab("Mean Aug Euphausiid Biomass across GAK sites (g WW / m3)") +
  xlab("Year")

ggplot(SeptLg, aes(y=Euphausiids, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012), ylim = c(0.01, 0.7)) +
  ylab("Mean Sept Euphausiid Biomass across GAK sites (g WW / m3)") +
  xlab("Year")

ggplot(OctLg, aes(y=Euphausiids, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + 
  scale_y_log10() + coord_cartesian(xlim = c(1996, 2012), ylim = c(0.01, 0.7)) +
  ylab("Mean Oct Euphausiid Biomass across GAK sites (g WW / m3)") +
  xlab("Year")



# ---------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------

# Create May large zooplankton abundance

# 1. Copepod size / net classifications from Coyle & Pinchuk 2003:

# Aetideidae, stages V, Adults
AetideidaeAb = May %>%
  filter(family == "Aetideidae") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AetideidaeAbSite=sum(abundance)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(AetideidaeAb=mean(AetideidaeAbSite)) %>% 
  ungroup


# Calanus marshallae, stages IV, V, adults
CmarshallaeAb = May %>%
  filter(sciName == "Calanus marshallae") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(CmarshallaeAbSite=sum(abundance)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(CmarshallaeAb=mean(CmarshallaeAbSite)) %>%
  ungroup


# Calanus pacificus, stages V, adults
CpacificusAb = May %>%
  filter(sciName == "Calanus pacificus") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(CpacificusAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(CpacificusAb=mean(CpacificusAbSite)) %>% 
  ungroup


# Candacia, stages IV, V, adults
CandaciaAb = May %>%
  filter(genus == "Candacia") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(CandaciaAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(CandaciaAb=mean(CandaciaAbSite)) %>% 
  ungroup


# Epilabidocera amphitrites, stages IV, V, adults;  add in Apr 30 sample from GAK2 in 2002 (cruiseID == hx258)
EamphitritesAb = May %>%
  filter(species == "Epilabidocera amphitrites") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(EamphitritesAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(EamphitritesAb=mean(EamphitritesAbSite)) %>% 
  ungroup


# Eucalanus bungii, stages IV, V, adults
EbungiiAb = May %>%
  filter(sciName == "Eucalanus bungii") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(EbungiiAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(EbungiiAb=mean(EbungiiAbSite)) %>% 
  ungroup


# Euchaeta elongata, stages III-V, adults
PelongataAb = May %>%
  filter(species %in% c("Paraeuchaeta elongata", "Elongata")) %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(PelongataAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(PelongataAb=mean(PelongataAbSite)) %>% 
  ungroup


# Heterorhabdus spp., stages IV, V, adults
HeterorhabdusAb = May %>%
  filter(genus == "Heterorhabdus") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(HeterorhabdusAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(HeterorhabdusAb=mean(HeterorhabdusAbSite)) %>% 
  ungroup


# Heterostylites spp., stages V, adults; none in May GAK samples
HeterostylitesAb = May %>%
  filter(genus == "Heterostylites") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(HeterostylitesAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(HeterostylitesAb=mean(HeterostylitesAbSite)) %>% 
  ungroup


# Lucicutia spp., stages V, adults
LucicutiaAb = May %>%
  filter(genus == "Lucicutia") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LucicutiaAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(LucicutiaAb=mean(LucicutiaAbSite)) %>% 
  ungroup


# Metridia okhotensis, stages V, adults
MokhotensisAb = May %>%
  filter(sciName == "Metridia okhotensis") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(MokhotensisAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(MokhotensisAb=mean(MokhotensisAbSite)) %>% 
  ungroup


# Metridia pacifica, stages Females
MpacificaAb = May %>%
  filter(sciName == "Metridia pacifica") %>% 
  filter(stage == "AF") %>%
  group_by(Year, stationID) %>%
  summarise(MpacificaAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(MpacificaAb=mean(MpacificaAbSite)) %>% 
  ungroup


# Neocalanus cristatus, stages III-V, adults
NcristatusAb = May %>%
  filter(sciName == "Neocalanus cristatus") %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(NcristatusAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(NcristatusAb=mean(NcristatusAbSite)) %>% 
  ungroup


# Neocalanus plumchrus-flemingeri, stages IV, V, adults
NpflemingeriAb = May %>%
  filter(sciName %in% c("Neocalanus plumchrus", "Neocalanus flemingeri")) %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "CV_large", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(NpflemingeriAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(NpflemingeriAb=mean(NpflemingeriAbSite)) %>% 
  ungroup


# Pleuromamma spp., stages V, adults 
PleuromammaAb = May %>%
  filter(genus == "Pleuromamma") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(PleuromammaAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(PleuromammaAb=mean(PleuromammaAbSite)) %>% 
  ungroup

# -----------------------

# 2. Other copepod taxa present but not assigned to size / net group by Coyle & Pinchuk 2003:

# Gaussia princeps   Follow Coyle & Pinchuk's classification for congenors Pleuromamma & Metridia
# None in GAK May samples
GprincepsAb = May %>%
  filter(sciName == "Gaussia princeps") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(GprincepsAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(GprincepsAb=mean(GprincepsAbSite)) %>% 
  ungroup


# Metridia sp. Follow Coyle & Pinchuk's 2003 classification for M. okhotensis
# ie take stages V, adults from large nets
MetridiaAb = May %>%
  filter(sciName == "Metridia") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(MetridiaAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(MetridiaAb=mean(MetridiaAbSite)) %>% 
  ungroup


# Monstrilla sp. (infraclass Neocopepod).  Not captured in large zoop nets


# Neocalanus sp.  Follow Coyle & Pinchuk's 2003 classification for N. cristatus
# ie take III-V, adults from large nets
NeocalanusAb = May %>%
  filter(sciName == "Neocalanus") %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(NeocalanusAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(NeocalanusAb=mean(NeocalanusAbSite)) %>% 
  ungroup


# -----------------------

# 3. Other non-copepod crustacean zooplankton present in CalVET nets

# Euphausiids all stages?
EuphausiidsAb = May %>%
  filter(order == "Euphausiacea") %>% 
  group_by(Year, stationID) %>%
  summarise(EuphausiidsAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(EuphausiidsAb=mean(EuphausiidsAbSite)) %>% 
  ungroup


# Mysids (all stages?)
MysidsAb = May %>%
  filter(order == "Mysida") %>% 
  group_by(Year, stationID) %>%
  summarise(MysidsAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(MysidsAb=mean(MysidsAbSite)) %>% 
  ungroup


# Cnidaria
CnidariaAb = May %>%
  filter(phylum == "Cnidaria") %>% 
  group_by(Year, stationID) %>%
  summarise(CnidariaAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(MysidsAb=mean(CnidariaAbSite)) %>% 
  ungroup


#Salps; none in May GAK samples
SalpsAb = May %>%
  filter(family == "Salpidae") %>% 
  group_by(Year, stationID) %>%
  summarise(SalpsAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SalpsAb=mean(SalpsAbSite)) %>% 
  ungroup


# Chaetnognaths; none in May GAK samples
ChaetnognathaAb = May %>%
  filter(phylum == "Chaetnognatha") %>% 
  group_by(Year, stationID) %>%
  summarise(ChaetnognathaAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ChaetnognathaAb=mean(ChaetnognathaAbSite)) %>% 
  ungroup


#Crab & shrimp zoea
ZoeaAb = May %>%
  filter(stage %in% c("zoea", "zoea_stage_1")) %>% 
  group_by(Year, stationID) %>%
  summarise(ZoeaAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ZoeaAb=mean(ZoeaAbSite)) %>% 
  ungroup


# Gastropods
GastropodaAb = May %>%
  filter(class == "Gastropoda") %>% 
  group_by(Year, stationID) %>%
  summarise(GastropodaAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(GastropodaAb=mean(GastropodaAbSite)) %>% 
  ungroup


# -----------------------

# Create dataframe with years:
MayLgZoAbund=data.frame('Year'=c(1998:2010))

# Merge in the taxon-specific abundance data
MayLgZoAbund <- merge(MayLgZoAbund,AetideidaeAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,CmarshallaeAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,CpacificusAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,CandaciaAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,EamphitritesAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,EbungiiAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,PelongataAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,HeterorhabdusAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,HeterostylitesAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,LucicutiaAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,MokhotensisAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,MpacificaAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,NcristatusAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,NpflemingeriAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,PleuromammaAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,GprincepsAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,MetridiaAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,NeocalanusAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,EuphausiidsAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,MysidsAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,CnidariaAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,SalpsAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,ChaetnognathaAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,ZoeaAb,all.x=T)
MayLgZoAbund <- merge(MayLgZoAbund,GastropodaAb,all.x=T)

#View(MayLgZoAbund)
