#######################################################################
#####  Seward Line Small Zooplankton (<0.3mg) Processing Script  ######
#####     Script by Colette Ward (ward at nceas.ucsb.edu)        ######
#####                      August 2015                           ######
#######################################################################

# Call data from source file
source('SewardLineSmallZoopCleaningScript.R')
View(SCZo1)


# extract GAK sites:
SCZo1GAK = SCZo1 %>%
  filter(stationID %in% c("GAK1", "GAK2", "GAK3", "GAK4", "GAK5", "GAK6", "GAK7", "GAK8", "GAK9", "GAK10", "GAK11", "GAK12", "GAK13"))  # select only GAK sites
head(SCZo1GAK)

# -----------------------------------------------------------------------------------

# Some sampling visualizations:
# Site depths across Seward Line:
SCZo1GAK$Site <- factor(SCZo1GAK$stationID, levels=c("GAK1", "GAK2", "GAK3", "GAK4", "GAK5", "GAK6", "GAK7", "GAK8", "GAK9", "GAK10", "GAK11", "GAK12", "GAK13"))
ggplot(data=SCZo1GAK, aes(y=sonicDepth, x=Site)) +
  geom_point() + theme_bw() + scale_y_log10() + ylab("Bottom Depth (m)")

# Temporal distribution of sampling
# also check whether there are problems with month & day being mixed up (compare against https://www.sfos.uaf.edu/sewardline/results/DataByCruise2.html:
plot(SCZo1$Year ~ SCZo1$Month, pch=16)


# -----------------------------------------------------------------------------------

# Create May small zooplankton biomass

# From Hopcroft 2014 (unpubl data; oral presentation at Ocean Sciences Symposium):
# Large zooplankton: NMDS suggests that GAK 1-4 (sites in ACC) have different community structure than GAK 5-13 (shelf & offshore sites)
#       correlated with distance from shore, salinity, temperature
#       lg zoop biomass varies year-to-year; community struture varies less from year-to-year
# Small zooplankton: there appears to be a gradient in community structure across the shelf, but structure does not shift as strongly as lg zoop
#       sm zoop community structure is more variable from year-to-year
#       therefore use all GAK sites for small zoop


# Which net to use for which taxa?
# Coyle et al. 2013 (citing Coyle & Pinchuk 2003, 2005): 
#         see Table 2 in Coyle & Pinchuk 2003:
#         for large zooplankton (i.e. > 0.3mg) use MOCNESS & Multinet samples (335 & 500um mesh); 
#         for small zoop (<0.3mg) use CalVET net (150um mesh)


# NB copepod stages I, II, III, etc are the same as stages C1, C2, C3, etc ... (shift in nomenclature in the database in 2005)

# NB I need to add nauplii to small zooplankton code below ***








# 1. Copepod size / net classifications from Coyle & Pinchuk 2003:
# Acartia, all life stages
SmAcartia = SCZo1GAK %>%
  filter(family == "Acartiidae") %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(AcartiaSite=sum(biomass)) %>% # find total biomass of all Acartia life stages for each GAK site
  ungroup %>%
  group_by(Year) %>%
  summarise(SmAcartia=mean(AcartiaSite)) %>% # take mean Acartia biomass across all GAK sites
  ungroup


# Aetideidae, stages I-IV # NB there are none in GAK samples
SmAetideidae = SCZo1GAK %>%
  filter(family == "Aetideidae") %>%
  filter(stage %in% c("I", "II", "III", "IV", "C1", "C2", "C3", "C4")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(AetideidaeSite=sum(biomass)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(SmAetideidae=mean(AetideidaeSite)) %>% 
  ungroup

# Calanus marshallae, stages I - III
SmCmarshallae = SCZo1GAK %>%
  filter(species %in% c("Calanus_marshallae", "marshallae")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(CmarshallaeSite=sum(biomass)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(SmCmarshallae=mean(CmarshallaeSite)) %>%
  ungroup


# Calanus pacificus, stages I - IV
SmCpacificus = SCZo1GAK %>%
  filter(species %in% c("Calanus_pacificus", "pacificus")) %>%  # does not include Aetideus pacificus
  filter(stage %in% c("I", "II", "III", "IV", "C1", "C2", "C3", "C4")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(CpacificusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmCpacificus=mean(CpacificusSite)) %>% 
  ungroup


# Candacia columbiae, stages I - III # none at GAK sites in May
SmCcolumbiae = SCZo1GAK %>%
  filter(species %in% c("Candacia_columbiae", "columbiae")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(CcolumbiaeSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmCcolumbiae=mean(CcolumbiaeSite)) %>% 
  ungroup


# Centropages abdominalis, all stages
SmCabdominalis = SCZo1GAK %>%
  filter(species %in% c("Centropages_abdominalis", "abdominalis")) %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(CabdominalisSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmCabdominalis=mean(CabdominalisSite)) %>% 
  ungroup


# Clausocalanus spp., all stages
SmClausocalanus = SCZo1GAK %>%
  filter(species %in% c("Clausocalanus_sp.", "Clausocalanus_parapergens", "Clausocalanus_lividus") | genus == "Clausocalanus") %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(ClausocalanusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmClausocalanus=mean(ClausocalanusSite)) %>% 
  ungroup


# Epilabidocera amphitrites, stages I - III; NB none in May GAK samples - but add in Apr 30 sample from GAK2 in 2002 (cruiseID == hx258)
SmEamphitrites = SCZo1GAK %>%
  filter(species %in% c("Epilabidocera_amphitrites", "amphitrites")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(EamphitritesSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmEamphitrites=mean(EamphitritesSite)) %>% 
  ungroup


# Eucalanus bungii, stages I - III
SmEbungii = SCZo1GAK %>%
  filter(species %in% c("Eucalanus_bungii", "bungii")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(EbungiiSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmEbungii=mean(EbungiiSite)) %>% 
  ungroup


# Euchaeta elongata, stages I - II # renamed to Paraeuchaeta elongata?
SmPelongata = SCZo1GAK %>%
  filter(species %in% c("Paraeuchaeta_elongata", "elongata")) %>% 
  filter(stage %in% c("I", "II", "C1", "C2")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(PelongataSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmPelongata=mean(PelongataSite)) %>% 
  ungroup


# Eurytemora, all life stages; NB doesn't appear at GAK sites
SmEurytemora = SCZo1GAK %>%
  filter(species == "Eurytemora_sp.") %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(EurytemoraSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmEurytemora=mean(EurytemoraSite)) %>%
  ungroup

# Heterorhabdus spp., stages I - III  NB none in May GAK samples
SmHeterorhabdus = SCZo1GAK %>%
  filter(species %in% c("Heterorhabdus_sp.", "Heterorhabdus_tanneri", "tanneri")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(HeterorhabdusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmHeterorhabdus=mean(HeterorhabdusSite)) %>% 
  ungroup

# Heterostylites spp., stages I - IV # not in CalVET nets at all?


# Lucicutia spp., stages I - IV; none in May GAK samples
SmLucicutia = SCZo1GAK %>%
  filter(species %in% c("Lucicutia_sp.", "Lucicutia_flavicornis")) %>% 
  filter(stage %in% c("I", "II", "III", "IV", "C1", "C2", "C3", "C4")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(LucicutiaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmLucicutia=mean(LucicutiaSite)) %>% 
  ungroup

# Mesocalanus tenuicornis, all stages (include Mesocalanus sp. here as well, there are only 5 entries, all in 1998)
SmMesocalanus = SCZo1GAK %>%
  filter(species %in% c("Mesocalanus_tenuicornis", "Mesocalanus_sp.", "tenuicornis")) %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(MesocalanusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmMesocalanus=mean(MesocalanusSite)) %>% 
  ungroup


# Metridia okhotensis, stages I - IV
SmMokhotensis = SCZo1GAK %>%
  filter(species %in% c("Metridia_okhotensis", "okhotensis")) %>% 
  filter(stage %in% c("I", "II", "III", "IV", "C1", "C2", "C3", "C4")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(MokhotensisSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmMokhotensis=mean(MokhotensisSite)) %>% 
  ungroup


# Metridia pacifica, stages I - V & Males
SmMpacifica = SCZo1GAK %>%
  filter(species == "Metridia_pacifica" | sciName =="Metridia pacifica") %>% 
  filter(stage %in% c("I", "II", "III", "IV", "V", "C1", "C2", "C3", "C4", "C5", "AM", "Male", "males")) %>%  # don't need to look at NAs, there are none for M pacifica
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(MpacificaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmMpacifica=mean(MpacificaSite)) %>% 
  ungroup


# Microcalanus spp., all stages
SmMicrocalanus = SCZo1GAK %>%
  filter(species == "Microcalanus_sp." | sciName == "Microcalanus_sp.") %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(MicrocalanusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmMicrocalanus=mean(MicrocalanusSite)) %>% 
  ungroup


# Neocalanus cristatus, stages I - II
SmNcristatus = SCZo1GAK %>%
  filter(species %in% c("Neocalanus_cristatus", "cristatus")) %>% 
  filter(stage %in% c("I", "II", "C1", "C2")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(NcristatusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmNcristatus=mean(NcristatusSite)) %>% 
  ungroup


# Neocalanus plumchrus-flemingeri, stages I - III # NB none in May GAK samples
SmNpflemingeri = SCZo1GAK %>%
  filter(species %in% c("Neocalanus_plumchrus", "Neocalanus_flemingeri", "plumchrus", "flemingeri")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(NpflemingeriSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmNpflemingeri=mean(NpflemingeriSite)) %>% 
  ungroup

# Oithona spp., all stages
SmOithona = SCZo1GAK %>%
  filter(species %in% c("Oithona_similis", "Oithona_spinirostris", "Oithona_sp.", "similis", "spinirostris") | sciName == "Oithona_sp.") %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(OithonaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmOithona=mean(OithonaSite)) %>% 
  ungroup


# Oncaea spp., all stages
SmOncaea = SCZo1GAK %>%
  filter(species == "Oncaea_sp." | sciName == "Oncaea_sp.") %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(OncaeaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmOncaea=mean(OncaeaSite)) %>% 
  ungroup


# Paracalanus spp., all stages; NB none in May GAK samples
SmParacalanus = SCZo1GAK %>%
  filter(species %in% c("Paracalanus_parvus", "Paracalanus_sp.", "parvus") | sciName == "Paracalanus_sp.") %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(ParacalanusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmParacalanus=mean(ParacalanusSite)) %>% 
  ungroup

# Pleuromamma spp., stgbes I-IV (family Metridinidae, genus Pleuromamma)  NB None in this dataset ... ?

# Pseudocalanus spp., all stages
SmPseudocalanus = SCZo1GAK %>%
  filter(species == "Pseudocalanus_sp." | sciName == "Pseudocalanus_sp.") %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(PseudocalanusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmPseudocalanus=mean(PseudocalanusSite)) %>% 
  ungroup


# Racovitzanus antarcticus, all stages
SmRantacrticus = SCZo1GAK %>%
  filter(species %in% c("Racovitzanus_antarcticus", "antarcticus")) %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(RantacrticusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmRantacrticus=mean(RantacrticusSite)) %>% 
  ungroup


# Scolecithricella spp., all stages
SmScolecithricella = SCZo1GAK %>%
  filter(species %in% c("Scolecithricella_minor", "Scolecithricella_sp.", "Pseudoamallothrix_ovata", "Pseudoamallothrix_sp.", "minor") | sciName == "Scolecithricella_sp.") %>%  # "ovata" not in species column
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(ScolecithricellaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmScolecithricella=mean(ScolecithricellaSite)) %>% 
  ungroup


# Tortanus discaudata, all stages; NB none in May GAK samples (only appeared once in sampling record)
SmTdiscaudata = SCZo1GAK %>%
  filter(species %in% c("Tortanus_discaudatus", "discaudatus")) %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(TdiscaudataSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmTdiscaudata=mean(SmTdiscaudataSite)) %>% 
  ungroup

# -----------------------

# 2. Other copepod taxa present in CalVET nets but not assigned to size / net group by Coyle & Pinchuk 2003:

# Metridia sp. Follow Coyle & Pinchuk's 2003 classification for M. okhotensis
SmMetridia = SCZo1GAK %>%
  filter(species == "Metridia_sp." | sciName %in% c("Metridia sp.", "Metridia spp.")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(MetridiaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmMetridia=mean(MetridiaSite)) %>% 
  ungroup


# Microsetella sp. only found in 2004.  Adult females are 0.02mg. Therefore take all stages from CalVET nets
SmMicrosetella = SCZo1GAK %>%
  filter(species == "Microsetella_sp." | sciName == "Microsetella sp.") %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(MicrosetellaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmMicrosetella=mean(MicrosetellaSite)) %>% 
  ungroup


# Monstrilla sp. (infraclass Neocopepod).  Not sure, and only approx 20 entries across entire dataset, therefore leave this for now
#Parasitic on marine benthic inverts (polychaetes, gastropods); only the first nauplius & adult stages are free-swimming (adults don't feed)

# Neocalanus sp.  Follow Coyle & Pinchuk's 2003 classification for N. cristatus; NB none of these stages are in May GAK samples
SmNeocalanus = SCZo1GAK %>%
  filter(species == "Neocalanus_sp." | sciName == "Neocalanus_sp.") %>% 
  filter(stage %in% c("I", "II", "C1", "C2")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(NeocalanusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmNeocalanus=mean(NeocalanusSite)) %>% 
  ungroup


# -----------------------

# 3. Other non-copepod crustacean zooplankton present in CalVET nets

# class Branchiopoda, family Podonidae (Cladocerans: Evadne sp.  & Podon sp.)  
# Judging from length comparisons with Neocalanus sp (these are much smaller in length) on Seward Line website, all stages of Branchiopods should come from CalVET
# NB None in May GAK samples
SmPodonidae = SCZo1GAK %>%
  filter(family == "Podonidae") %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(PodonidaeSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SmPodonidae=mean(PodonidaeSite)) %>% 
  ungroup


# -----------------------------------------------------------------------------------

# Create dataframe with years:
MaySmallZoop=data.frame('Year'=c(1998:2010))

# Merge in the taxon-specific biomass data
MaySmallZoop <- merge(MaySmallZoop,SmAcartia,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmAetideidae,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmCmarshallae,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmCpacificus,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmCcolumbiae,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmCabdominalis,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmClausocalanus,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmEamphitrites,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmEbungii,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmPelongata,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmEurytemora,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmHeterorhabdus,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmLucicutia,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmMesocalanus,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmMokhotensis,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmMpacifica,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmMicrocalanus,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmNcristatus,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmNpflemingeri,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmOithona,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmOncaea,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmParacalanus,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmPseudocalanus,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmRantacrticus,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmScolecithricella,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmMetridia,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmMicrosetella,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmNeocalanus,all.x=T)
MaySmallZoop <- merge(MaySmallZoop,SmPodonidae,all.x=T)