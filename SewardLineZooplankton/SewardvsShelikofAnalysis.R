#######################################################################
##### Seward Line / Shelikof Line 8 Zooplankton Comparison Script #####
#####     Script by Colette Ward (ward@nceas.ucsb.edu)           ######
#####                       October 2015                         ######
#######################################################################


# Run SewardLineSmZoopCleaningScript and SewardLineLgZoopCleaningScript
# Run SewardLineLgZoopProcessingScript up to line 54



# Create small zooplankton (from 150um mesh CalVET net)
# Extract Seward Line (GAK) sites from Small Zoop file
May.s = SCZo1 %>%
  filter(stationID %in% c("GAK1", "GAK2", "GAK3", "GAK4", "GAK5", "GAK6", "GAK7", "GAK8", "GAK9", "GAK10", "GAK11", "GAK12", "GAK13")) %>%
  filter(Month == 5)


# 1. Small Copepods (from CalVET net)

# Copepod stage size / net classifications from Coyle & Pinchuk 2003:
# Acartia, all life stages
SmAcartia = May.s %>%
  filter(family == "Acartiidae") %>%
  group_by(Year, stationID) %>%
  summarise(SmAcartia=sum(abundance)) %>%
  ungroup

# Aetideidae, stages I-IV # NB there are none in GAK samples
SmAetideidae = May.s %>%
  filter(family == "Aetideidae") %>%
  filter(stage %in% c("I", "II", "III", "IV", "C1", "C2", "C3", "C4")) %>%
  group_by(Year, stationID) %>%
  summarise(SmAetideidae=sum(abundance)) %>%
  ungroup

# Calanus marshallae, stages I - III
SmCmarshallae = May.s %>%
  filter(species %in% c("Calanus_marshallae", "marshallae")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  group_by(Year, stationID) %>%
  summarise(SmCmarshallae=sum(abundance)) %>%
  ungroup

# Calanus pacificus, stages I - IV
SmCpacificus = May.s %>%
  filter(species %in% c("Calanus_pacificus", "pacificus")) %>%  # does not include Aetideus pacificus
  filter(stage %in% c("I", "II", "III", "IV", "C1", "C2", "C3", "C4")) %>%
  group_by(Year, stationID) %>%
  summarise(SmCpacificus=sum(abundance)) %>% 
  ungroup


# Candacia columbiae, stages I - III # none at GAK sites in May
SmCcolumbiae = May.s %>%
  filter(species %in% c("Candacia_columbiae", "columbiae")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  group_by(Year, stationID) %>%
  summarise(SmCcolumbiae=sum(abundance)) %>% 
  ungroup


# Centropages abdominalis, all stages
SmCabdominalis = May.s %>%
  filter(species %in% c("Centropages_abdominalis", "abdominalis")) %>% 
  group_by(Year, stationID) %>%
  summarise(SmCabdominalis=sum(abundance)) %>% 
  ungroup


# Clausocalanus spp., all stages
SmClausocalanus = May.s %>%
  filter(species %in% c("Clausocalanus_sp.", "Clausocalanus_parapergens", "Clausocalanus_lividus") | genus == "Clausocalanus") %>%
  group_by(Year, stationID) %>%
  summarise(SmClausocalanus=sum(abundance)) %>% 
  ungroup


# Epilabidocera amphitrites, stages I - III; NB none in May GAK samples - but add in Apr 30 sample from GAK2 in 2002 (cruiseID == hx258)
SmEamphitrites = May.s %>%
  filter(species %in% c("Epilabidocera_amphitrites", "amphitrites")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  group_by(Year, stationID) %>%
  summarise(SmEamphitrites=sum(abundance)) %>% 
  ungroup


# Eucalanus bungii, stages I - III
SmEbungii = May.s %>%
  filter(species %in% c("Eucalanus_bungii", "bungii")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  group_by(Year, stationID) %>%
  summarise(SmEbungii=sum(abundance)) %>% 
  ungroup

# Euchaeta elongata, stages I - II # renamed to Paraeuchaeta elongata
SmPelongata = May.s %>%
  filter(species %in% c("Paraeuchaeta_elongata", "elongata")) %>% 
  filter(stage %in% c("I", "II", "C1", "C2")) %>%
  group_by(Year, stationID) %>%
  summarise(SmPelongata=sum(abundance)) %>% 
  ungroup


# Eurytemora, all life stages; NB doesn't appear at GAK sites
SmEurytemora = May.s %>%
  filter(species == "Eurytemora_sp.") %>%
  group_by(Year, stationID) %>%
  summarise(SmEurytemora=sum(abundance)) %>% 
  ungroup

# Heterorhabdus spp., stages I - III  NB none in May GAK samples
SmHeterorhabdus = May.s %>%
  filter(species %in% c("Heterorhabdus_sp.", "Heterorhabdus_tanneri", "tanneri")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  group_by(Year, stationID) %>%
  summarise(SmHeterorhabdus=sum(abundance)) %>% 
  ungroup

# Heterostylites spp., stages I - IV # not in CalVET nets at all?


# Lucicutia spp., stages I - IV; none in May GAK samples
SmLucicutia = May.s %>%
  filter(species %in% c("Lucicutia_sp.", "Lucicutia_flavicornis")) %>% 
  filter(stage %in% c("I", "II", "III", "IV", "C1", "C2", "C3", "C4")) %>%
  group_by(Year, stationID) %>%
  summarise(SmLucicutia=sum(abundance)) %>% 
  ungroup

# Mesocalanus tenuicornis, all stages (include Mesocalanus sp. here as well, there are only 5 entries, all in 1998)
SmMesocalanus = May.s %>%
  filter(species %in% c("Mesocalanus_tenuicornis", "Mesocalanus_sp.", "tenuicornis")) %>% 
  group_by(Year, stationID) %>%
  summarise(SmMesocalanus=sum(abundance)) %>% 
  ungroup

# Metridia okhotensis, stages I - IV
SmMokhotensis = May.s %>%
  filter(species %in% c("Metridia_okhotensis", "okhotensis")) %>% 
  filter(stage %in% c("I", "II", "III", "IV", "C1", "C2", "C3", "C4")) %>%
  group_by(Year, stationID) %>%
  summarise(SmMokhotensis=sum(abundance)) %>% 
  ungroup


# Metridia pacifica, stages I - V & Males
SmMpacifica = May.s %>%
  filter(species == "Metridia_pacifica" | sciName =="Metridia pacifica") %>% 
  filter(stage %in% c("I", "II", "III", "IV", "V", "C1", "C2", "C3", "C4", "C5", "AM", "Male", "males")) %>%  # don't need to look at NAs, there are none for M pacifica
  group_by(Year, stationID) %>%
  summarise(SmMpacifica=sum(abundance)) %>% 
  ungroup

# Metrididae, I-III
# Family Metridinidae includes Gaussia, Metridia, Pleuromamma.  
# EcoFOCI has already specified Metridia spp. as per above, 
# Gaussia princpeps was observed only once, at GAK12 in Sept 2007
# therefore include only Pleuromamma, P. abdominalis, P. scutullata, P. xiphias in this category
SmMetridinidae = May.s %>%
  filter(genus == "Pleuromamma") %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  group_by(Year, stationID) %>%
  summarise(SmMetridinidae=sum(abundance)) %>% 
  ungroup


# Microcalanus spp., all stages
SmMicrocalanus = May.s %>%
  filter(species == "Microcalanus_sp." | sciName == "Microcalanus_sp.") %>% 
  group_by(Year, stationID) %>%
  summarise(SmMicrocalanus=sum(abundance)) %>% 
  ungroup


# Neocalanus cristatus, stages I - II
SmNcristatus = May.s %>%
  filter(species %in% c("Neocalanus_cristatus", "cristatus")) %>% 
  filter(stage %in% c("I", "II", "C1", "C2")) %>%
  group_by(Year, stationID) %>%
  summarise(SmNcristatus=sum(abundance)) %>% 
  ungroup

# Neocalanus plumchrus-flemingeri, stages I - III # NB none in May GAK samples
SmNpflemingeri = May.s %>%
  filter(species %in% c("Neocalanus_plumchrus", "Neocalanus_flemingeri", "plumchrus", "flemingeri")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  group_by(Year, stationID) %>%
  summarise(SmNpflemingeri=sum(abundance)) %>% 
  ungroup

# Oithona spp., all stages
SmOithona = May.s %>%
  filter(species %in% c("Oithona_similis", "Oithona_spinirostris", "Oithona_sp.", "similis", "spinirostris") | sciName == "Oithona_sp.") %>% 
  group_by(Year, stationID) %>%
  summarise(SmOithona=sum(abundance)) %>% 
  ungroup


# Oncaea spp., all stages
SmOncaea = May.s %>%
  filter(species == "Oncaea_sp." | sciName == "Oncaea_sp.") %>% 
  group_by(Year, stationID) %>%
  summarise(SmOncaea=sum(abundance)) %>% 
  ungroup


# Paracalanus spp., all stages; NB none in May GAK samples
SmParacalanus = May.s %>%
  filter(species %in% c("Paracalanus_parvus", "Paracalanus_sp.", "parvus") | sciName == "Paracalanus_sp.") %>% 
  group_by(Year, stationID) %>%
  summarise(SmParacalanus=sum(abundance)) %>% 
  ungroup


# Pseudocalanus spp., all stages
SmPseudocalanus = May.s %>%
  filter(species == "Pseudocalanus_sp." | sciName == "Pseudocalanus_sp.") %>% 
  group_by(Year, stationID) %>%
  summarise(SmPseudocalanus=sum(abundance)) %>% 
  ungroup


# Racovitzanus antarcticus, all stages
SmRantacrticus = May.s %>%
  filter(species %in% c("Racovitzanus_antarcticus", "antarcticus")) %>% 
  group_by(Year, stationID) %>%
  summarise(SmRantacrticus=sum(abundance)) %>% 
  ungroup


# Scolecithricella spp., all stages
SmScolecithricella = May.s %>%
  filter(species %in% c("Scolecithricella_minor", "Scolecithricella_sp.", "Pseudoamallothrix_ovata", "Pseudoamallothrix_sp.", "minor") | sciName == "Scolecithricella_sp.") %>%  # "ovata" not in species column
  group_by(Year, stationID) %>%
  summarise(SmScolecithricella=sum(abundance)) %>% 
  ungroup

# Tortanus discaudata, all stages; NB none in May GAK samples (only appeared once in sampling record)
SmTdiscaudata = May.s %>%
  filter(species %in% c("Tortanus_discaudatus", "discaudatus")) %>% 
  group_by(Year, stationID) %>%
  summarise(SmTdiscaudata=sum(abundance)) %>% 
  ungroup
# -----------------------

# Other copepod taxa present in CalVET nets but not assigned to size / net group by Coyle & Pinchuk 2003:

# Metridia sp. Follow Coyle & Pinchuk's 2003 classification for M. okhotensis
SmMetridia = May.s %>%
  filter(species == "Metridia_sp." | sciName %in% c("Metridia sp.", "Metridia spp.")) %>%
  group_by(Year, stationID) %>%
  summarise(SmMetridia=sum(abundance)) %>% 
  ungroup


# Microsetella sp. only found in 2004.  Adult females are 0.02mg. Therefore take all stages from CalVET nets
SmMicrosetella = May.s %>%
  filter(species == "Microsetella_sp." | sciName == "Microsetella sp.") %>%
  group_by(Year, stationID) %>%
  summarise(SmMicrosetella=sum(abundance)) %>% 
  ungroup

# Monstrilla sp. (infraclass Neocopepod).  Not sure, and only approx 20 entries across entire dataset, therefore leave this for now
#Parasitic on marine benthic inverts (polychaetes, gastropods); only the first nauplius & adult stages are free-swimming (adults don't feed)

# Neocalanus sp.  Follow Coyle & Pinchuk's 2003 classification for N. cristatus; NB none of these stages are in May GAK samples
SmNeocalanus = May.s %>%
  filter(species == "Neocalanus_sp." | sciName == "Neocalanus_sp.") %>% 
  filter(stage %in% c("I", "II", "C1", "C2")) %>%
  group_by(Year, stationID) %>%
  summarise(SmNeocalanus=sum(abundance)) %>% 
  ungroup

# Other unidentified Calanids, I-III (use stage designation of Calanus, Mesocalanus, Neocalanus) - ALL SHOULD COME FROM SMALL MESH NETS
# Unidentified Calanids, damaged I-II and undetermined stages
SmUnidCalanids = May.s %>%
  filter(family == "Calanidae" & species %in% c(NA, "N/A")) %>%  # works as desired
  group_by(Year, stationID) %>%
  summarise(SmUnidCalanids=sum(abundance)) %>% 
  ungroup
#View(SmUnidCalanids)


# -----------------------

# Other non-copepod crustacean zooplankton present in CalVET nets

# class Branchiopoda, family Podonidae (Cladocerans: Evadne sp.  & Podon sp.)  
# Judging from length comparisons with Neocalanus sp (these are much smaller in length) on Seward Line website, all stages of Branchiopods should come from CalVET
# NB None in May GAK samples
SmPodonidae = May.s %>%
  filter(family == "Podonidae") %>% 
  group_by(Year, stationID) %>%
  summarise(SmPodonidae=sum(abundance)) %>% 
  ungroup

# ----------------

# Create dataframe with years; please excuse the ugly code
Year <- c(1998:2010, 1998:2010, 1998:2010, 1998:2010, 1998:2010, 1998:2010, 1998:2010, 1998:2010, 1998:2010, 1998:2010, 1998:2010, 1998:2010, 1998:2010)
stationID <- c("GAK1", "GAK1", "GAK1", "GAK1", "GAK1", "GAK1", "GAK1", "GAK1", "GAK1", "GAK1", "GAK1", "GAK1", "GAK1",
               "GAK2", "GAK2", "GAK2", "GAK2", "GAK2", "GAK2", "GAK2", "GAK2", "GAK2", "GAK2", "GAK2", "GAK2", "GAK2", 
               "GAK3", "GAK3", "GAK3", "GAK3", "GAK3", "GAK3", "GAK3", "GAK3", "GAK3", "GAK3", "GAK3", "GAK3", "GAK3", 
               "GAK4", "GAK4", "GAK4", "GAK4", "GAK4", "GAK4", "GAK4", "GAK4", "GAK4", "GAK4", "GAK4", "GAK4", "GAK4",
               "GAK5", "GAK5", "GAK5", "GAK5", "GAK5", "GAK5", "GAK5", "GAK5", "GAK5", "GAK5", "GAK5", "GAK5", "GAK5", 
               "GAK6", "GAK6", "GAK6", "GAK6", "GAK6", "GAK6", "GAK6", "GAK6", "GAK6", "GAK6", "GAK6", "GAK6", "GAK6",
               "GAK7", "GAK7", "GAK7", "GAK7", "GAK7", "GAK7", "GAK7", "GAK7", "GAK7", "GAK7", "GAK7", "GAK7", "GAK7",
               "GAK8", "GAK8", "GAK8", "GAK8", "GAK8", "GAK8", "GAK8", "GAK8", "GAK8", "GAK8", "GAK8", "GAK8", "GAK8",
               "GAK9", "GAK9", "GAK9", "GAK9", "GAK9", "GAK9", "GAK9", "GAK9", "GAK9", "GAK9", "GAK9", "GAK9", "GAK9",
               "GAK10", "GAK10", "GAK10", "GAK10", "GAK10", "GAK10", "GAK10", "GAK10", "GAK10", "GAK10", "GAK10", "GAK10", "GAK10",
               "GAK11", "GAK11", "GAK11", "GAK11", "GAK11", "GAK11", "GAK11", "GAK11", "GAK11", "GAK11", "GAK11", "GAK11", "GAK11",
               "GAK12", "GAK12", "GAK12", "GAK12", "GAK12", "GAK12", "GAK12", "GAK12", "GAK12", "GAK12", "GAK12", "GAK12", "GAK12",
               "GAK13", "GAK13", "GAK13", "GAK13", "GAK13", "GAK13", "GAK13", "GAK13", "GAK13", "GAK13", "GAK13", "GAK13", "GAK13")
ACCMaySmZoAbund <- data.frame(Year, stationID)
#View(ACCMaySmZoAbund)
str(ACCMaySmZoAbund)


# Merge in the taxon-specific biomass data
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmAcartia,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmAetideidae,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmCmarshallae,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmCpacificus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmCcolumbiae,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmCabdominalis,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmClausocalanus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmEamphitrites,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmEbungii,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmPelongata,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmEurytemora,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmHeterorhabdus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmLucicutia,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmMesocalanus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmMokhotensis,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmMpacifica,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmMetridinidae,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmMicrocalanus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmNcristatus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmNpflemingeri,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmOithona,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmOncaea,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmParacalanus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmPseudocalanus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmRantacrticus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmScolecithricella,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmMetridia,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmMicrosetella,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmNeocalanus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmUnidCalanids,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,SmPodonidae,all.x=T)

#View(ACCMaySmZoAbund)


#####################################################################################
#####################################################################################
#####################################################################################

# Create large zooplankton (from 500um mesh MOCNESS net)

# Extract Seward Line (GAK) sites from Large Zoop file
MayACC = DepthInt.taxinfo %>%
  filter(stationID %in% c("GAK1", "GAK2", "GAK3", "GAK4", "GAK5", "GAK6", "GAK7", "GAK8", "GAK9", "GAK10", "GAK11", "GAK12", "GAK13")) %>%
  filter(Month == 5)
str(MayACC)

# Copepod stage size / net classifications from Coyle & Pinchuk 2003:
# Aetideidae, stages V, Adults
LgAetideidae = MayACC %>%
  filter(family == "Aetideidae") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LgAetideidae=sum(abundance)) %>%
  ungroup


# Calanus marshallae, stages IV, V, adults
LgCmarshallae = MayACC %>%
  filter(sciName == "Calanus marshallae") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LgCmarshallae=sum(abundance)) %>%
  ungroup


# Calanus pacificus, stages V, adults
LgCpacificus = MayACC %>%
  filter(sciName == "Calanus pacificus") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LgCpacificus=sum(abundance)) %>% 
  ungroup


# Candacia, stages IV, V, adults
LgCandacia = MayACC %>%
  filter(genus == "Candacia") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LgCandacia=sum(abundance)) %>% 
  ungroup


# Epilabidocera amphitrites, stages IV, V, adults;  add in Apr 30 sample from GAK2 in 2002 (cruiseID == hx258)
LgEamphitrites = MayACC %>%
  filter(species == "Epilabidocera amphitrites") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LgEamphitrites=sum(abundance)) %>% 
  ungroup


# Eucalanus bungii, stages IV, V, adults
LgEbungii = MayACC %>%
  filter(sciName == "Eucalanus bungii") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LgEbungii=sum(abundance)) %>% 
  ungroup


# Euchaeta elongata, stages III-V, adults
LgPelongata = MayACC %>%
  filter(species %in% c("Paraeuchaeta elongata", "Elongata")) %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LgPelongata=sum(abundance)) %>% 
  ungroup


# Heterorhabdus spp., stages IV, V, adults
LgHeterorhabdus = MayACC %>%
  filter(genus == "Heterorhabdus") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LgHeterorhabdus=sum(abundance)) %>% 
  ungroup


# Heterostylites spp., stages V, adults; none in May GAK samples
LgHeterostylites = MayACC %>%
  filter(genus == "Heterostylites") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LgHeterostylites=sum(abundance)) %>% 
  ungroup


# Lucicutia spp., stages V, adults
LgLucicutia = MayACC %>%
  filter(genus == "Lucicutia") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LgLucicutia=sum(abundance)) %>% 
  ungroup


# Metridia okhotensis, stages V, adults
LgMokhotensis = MayACC %>%
  filter(sciName == "Metridia okhotensis") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LgMokhotensis=sum(abundance)) %>% 
  ungroup

# Metridia pacifica, stages Females
LgMpacifica = MayACC %>%
  filter(sciName == "Metridia pacifica") %>% 
  filter(stage == "AF") %>%
  group_by(Year, stationID) %>%
  summarise(LgMpacifica=sum(abundance)) %>% 
  ungroup


# Neocalanus cristatus, stages III-V, adults
LgNcristatus = MayACC %>%
  filter(sciName == "Neocalanus cristatus") %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LgNcristatus=sum(abundance)) %>% 
  ungroup


# Neocalanus plumchrus-flemingeri, stages IV, V, adults
LgNpflemingeri = MayACC %>%
  filter(sciName %in% c("Neocalanus plumchrus", "Neocalanus flemingeri")) %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "CV_large", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LgNpflemingeri=sum(abundance)) %>% 
  ungroup


# Pleuromamma spp., stages V, adults 
LgPleuromamma = MayACC %>%
  filter(genus == "Pleuromamma") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LgPleuromamma=sum(abundance)) %>% 
  ungroup
# -----------------------

# Other copepod taxa present but not assigned to size / net group by Coyle & Pinchuk 2003:

# Gaussia princeps   Follow Coyle & Pinchuk's classification for congenors Pleuromamma & Metridia
# None in GAK May samples
LgGprinceps = MayACC %>%
  filter(sciName == "Gaussia princeps") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LgGprinceps=sum(abundance)) %>% 
  ungroup


# Metridia sp. Follow Coyle & Pinchuk's 2003 classification for M. okhotensis
# ie take stages V, adults from large nets
LgMetridia = MayACC %>%
  filter(sciName == "Metridia") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LgMetridia=sum(abundance)) %>% 
  ungroup


# Monstrilla sp. (infraclass Neocopepod).  Not captured in large zoop nets


# Neocalanus sp.  Follow Coyle & Pinchuk's 2003 classification for N. cristatus
# ie take III-V, adults from large nets
LgNeocalanus = MayACC %>%
  filter(sciName == "Neocalanus") %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(LgNeocalanus=sum(abundance)) %>% 
  ungroup

# -----------------------

# Euphausiids, all stages (for total zooplankton dataset)
Euphausiids = MayACC %>%
  filter(order == "Euphausiacea") %>% 
  group_by(Year, stationID) %>%
  summarise(Euphausiids=sum(abundance)) %>% 
  ungroup
#View(AEuphausiids)

# -----------------------

# Euphausiids, for Line 8 subset calculation

SThysanoessa = MayACC %>% # "Thysanoessa" plus T. raschii, T. inermis, T. spinifera, T. longipes, T. inspinata, Adults + Juveniles
  filter(genus == "Thysanoessa") %>% 
  group_by(Year, stationID) %>%
  summarise(SThysanoessa=sum(abundance)) %>% 
  ungroup

# Tessarabrachion oculatum, Adult + Juvenile (ITIS valid name = Tessarabrachion oculatus)
SToculatum = MayACC %>%
  filter(sciName == "Tessarabrachion oculatus") %>% 
  group_by(Year, stationID) %>%
  summarise(SToculatum=sum(abundance)) %>% 
  ungroup

# pre-Juvenile Euphausiids
SEuphJuv = MayACC %>%
  filter(sciName == "Euphausiacea") %>% 
  filter(stage %in% c("furcilia", "calyptopis", "nauplii", "metanauplii")) %>% #consider also using "juvenile" and "juveniles"?
  group_by(Year, stationID) %>%
  summarise(SEuphJuv=sum(abundance)) %>% 
  ungroup


# -----------------------

# "Other" category for total zooplankton dataset (everything in the large net that is not a Copepod nor Euphausiid; 
# includes Mysids, Pteropods, Decapod zoea, Ichthyoplankton, Cnidaria, Amphipods, Chaetnognaths)

Other = MayACC %>%
  filter(subclass != "Copepoda") %>% # exclude Copepods
  filter(order != "Euphausiacea") %>% # exclude Euphausiids
  filter(subphylum != "Vertebrata") %>% # exclude ichthyoplankton & juv fish
  group_by(Year, stationID) %>%
  summarise(Other=sum(abundance)) %>% 
  ungroup

# -----------------------

# Other category for Line 8 subset

# Cirripedia, nauplii, cyprid, medusae (<5mm)
# nb medusae are not in Seward dataset
SCirripedia = MayACC %>%
  filter(sciName == "Cirripedia") %>% 
  group_by(Year, stationID) %>%
  summarise(SCirripedia=sum(abundance)) %>% 
  ungroup


# Chaetnognatha, all sizes
SChaetnognatha = MayACC %>%
  filter(phylum == "Chaetnognatha") %>% 
  group_by(Year, stationID) %>%
  summarise(SChaetnognatha=sum(abundance)) %>% 
  ungroup



# Natantia, larvae, adults, juveniles 
#(decapod families that swim, ie shrimp (infraorder == c(Caridea, Procarididea)), prawns (suborder == Dendrobranchiata), boxer shrimp (infraorder == Stenopodidea))
SNatantia = MayACC %>%
  filter(suborder == "Dendrobranchiata" | infraorder =="Caridea") %>%
  group_by(Year, stationID) %>%
  summarise(SNatantia=sum(abundance)) %>% 
  ungroup



# Cnidarian medusae >5mm
SCnidaria = MayACC %>%
  filter(phylum == "Cnidaria") %>%
  group_by(Year, stationID) %>%
  summarise(SCnidaria=sum(abundance)) %>% 
  ungroup


# Ctenophora, all sizes  # not recorded at Seward Line


# Gammaridea, all sizes (order = Amphipoda)
# Hyperiidea, all sizes (order = Amphipoda)
SAmphipoda = MayACC %>%
  filter(order == "Amphipoda") %>%
  group_by(Year, stationID) %>%
  summarise(SAmphipoda=sum(abundance)) %>% 
  ungroup

# Mysidacea, all sizes
SMysidacea = MayACC %>%
  filter(order == "Mysida") %>%
  group_by(Year, stationID) %>%
  summarise(SMysidacea=sum(abundance)) %>% 
  ungroup

# Siphonophora, all sizes # already included in Cnidaria above

# Ostracoda <5mm
SOstracoda = MayACC %>%
  filter(class == "Ostracoda") %>%
  group_by(Year, stationID) %>%
  summarise(SOstracoda=sum(abundance)) %>% 
  ungroup


# Thecosomata <5mm (Pteropods)
SThecosomata = MayACC %>%
  filter(order == "Thecosomata") %>%
  group_by(Year, stationID) %>%
  summarise(SThecosomata=sum(abundance)) %>% 
  ungroup


# Anomura larvae
SAnomura = MayACC %>%
  filter(infraorder == "Anomura") %>%
  filter(stage == "zoea") %>%
  group_by(Year, stationID) %>%
  summarise(SAnomura=sum(abundance)) %>% 
  ungroup

# Brachyura larvae
SBrachyura = MayACC %>%
  filter(infraorder == "Brachyura") %>%
  filter(stage == "zoea") %>%
  group_by(Year, stationID) %>%
  summarise(SBrachyura=sum(abundance)) %>% 
  ungroup

# Fish larvae
# probably not retained at Seward as much as at Shelikof?
SIchthyoplankton = MayACC %>%
  filter(subphylum == "Vertebrata") %>%
  filter(stage == "larvae") %>%
  group_by(Year, stationID) %>%
  summarise(SIchthyoplankton=sum(abundance)) %>% 
  ungroup

# -----------------------

# Create dataframe with years, please excuse the ugly code
Year <- c(1998:2010, 1998:2010, 1998:2010, 1998:2010, 1998:2010, 1998:2010, 1998:2010, 1998:2010, 1998:2010, 1998:2010, 1998:2010, 1998:2010, 1998:2010)
stationID <- c("GAK1", "GAK1", "GAK1", "GAK1", "GAK1", "GAK1", "GAK1", "GAK1", "GAK1", "GAK1", "GAK1", "GAK1", "GAK1",
               "GAK2", "GAK2", "GAK2", "GAK2", "GAK2", "GAK2", "GAK2", "GAK2", "GAK2", "GAK2", "GAK2", "GAK2", "GAK2", 
               "GAK3", "GAK3", "GAK3", "GAK3", "GAK3", "GAK3", "GAK3", "GAK3", "GAK3", "GAK3", "GAK3", "GAK3", "GAK3", 
               "GAK4", "GAK4", "GAK4", "GAK4", "GAK4", "GAK4", "GAK4", "GAK4", "GAK4", "GAK4", "GAK4", "GAK4", "GAK4",
               "GAK5", "GAK5", "GAK5", "GAK5", "GAK5", "GAK5", "GAK5", "GAK5", "GAK5", "GAK5", "GAK5", "GAK5", "GAK5", 
               "GAK6", "GAK6", "GAK6", "GAK6", "GAK6", "GAK6", "GAK6", "GAK6", "GAK6", "GAK6", "GAK6", "GAK6", "GAK6",
               "GAK7", "GAK7", "GAK7", "GAK7", "GAK7", "GAK7", "GAK7", "GAK7", "GAK7", "GAK7", "GAK7", "GAK7", "GAK7",
               "GAK8", "GAK8", "GAK8", "GAK8", "GAK8", "GAK8", "GAK8", "GAK8", "GAK8", "GAK8", "GAK8", "GAK8", "GAK8",
               "GAK9", "GAK9", "GAK9", "GAK9", "GAK9", "GAK9", "GAK9", "GAK9", "GAK9", "GAK9", "GAK9", "GAK9", "GAK9",
               "GAK10", "GAK10", "GAK10", "GAK10", "GAK10", "GAK10", "GAK10", "GAK10", "GAK10", "GAK10", "GAK10", "GAK10", "GAK10",
               "GAK11", "GAK11", "GAK11", "GAK11", "GAK11", "GAK11", "GAK11", "GAK11", "GAK11", "GAK11", "GAK11", "GAK11", "GAK11",
               "GAK12", "GAK12", "GAK12", "GAK12", "GAK12", "GAK12", "GAK12", "GAK12", "GAK12", "GAK12", "GAK12", "GAK12", "GAK12",
               "GAK13", "GAK13", "GAK13", "GAK13", "GAK13", "GAK13", "GAK13", "GAK13", "GAK13", "GAK13", "GAK13", "GAK13", "GAK13")
ACCMayLgZoAbund <- data.frame(Year, stationID)

# Merge in the taxon-specific abundance data
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,LgAetideidae,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,LgCmarshallae,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,LgCpacificus,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,LgCandacia,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,LgEamphitrites,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,LgEbungii,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,LgPelongata,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,LgHeterorhabdus,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,LgHeterostylites,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,LgLucicutia,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,LgMokhotensis,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,LgMpacifica,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,LgNcristatus,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,LgNpflemingeri,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,LgPleuromamma,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,LgGprinceps,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,LgMetridia,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,LgNeocalanus,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,Euphausiids,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,SThysanoessa,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,SToculatum,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,SEuphJuv,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,Other,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,SCirripedia,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,SChaetnognatha,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,SNatantia,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,SCnidaria,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,SAmphipoda,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,SMysidacea,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,SOstracoda,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,SThecosomata,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,SAnomura,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,SBrachyura,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,SIchthyoplankton,all.x=T)

View(ACCMayLgZoAbund)


#####################################################################################
#####################################################################################
#####################################################################################

# Calculate sums & means for zooplankton

# Merge Small & Large Zooplankton abundance datasets
ACCMayZoop <- full_join(ACCMaySmZoAbund, ACCMayLgZoAbund, by = c("Year", "stationID"))
ACCMayZoop[is.na(ACCMayZoop)] <- 0 # replace NA with 0
str(ACCMayZoop)
#View(ACCMayZoop)
names(ACCMayZoop)

#########################################

# Create summary dataframe for all taxa collected at GAK1-4 sites
ACCMayZoop1 <- ACCMayZoop %>%
  mutate(ATotCopepods = SmAcartia + SmAetideidae + SmCmarshallae + SmCpacificus + SmCcolumbiae + SmCabdominalis + 
           SmClausocalanus + SmEamphitrites + SmEbungii + SmPelongata + SmEurytemora + SmHeterorhabdus + SmLucicutia + 
           SmMesocalanus + SmMokhotensis + SmMpacifica + SmMetridinidae + SmMicrocalanus + SmNcristatus + SmNpflemingeri + SmOithona + 
           SmOncaea + SmParacalanus + SmPseudocalanus + SmRantacrticus + SmScolecithricella + SmMetridia + SmMicrosetella + 
           SmNeocalanus + SmUnidCalanids + LgAetideidae + LgCmarshallae + LgCpacificus + LgCandacia + LgEamphitrites + LgEbungii + LgPelongata + 
           LgHeterorhabdus + LgHeterostylites + LgLucicutia + LgMokhotensis + LgMpacifica + LgNcristatus + LgNpflemingeri + 
           LgPleuromamma + LgGprinceps + LgMetridia + LgNeocalanus) %>%
  rename(APodonidae = SmPodonidae, ATotEuphausiids = Euphausiids) %>%
  mutate(ATotOther = Other + SIchthyoplankton) %>%
  select(Year, stationID, ATotCopepods, APodonidae, ATotEuphausiids, ATotOther)
#View(ACCMayZoop1)

# Calculate annual means across all 4 sites, for all taxa collected at GAK1-4 sites
AnnualMeansAll = ACCMayZoop1 %>%
  group_by(Year) %>%
  summarise_each(funs(mean)) %>% # warning message is not a problem (indicates that it's not possible to find means of stationID)
  ungroup %>%
  select(Year, ATotCopepods, APodonidae, ATotEuphausiids, ATotOther)
#View(AnnualMeansAll)


#########################################

# Create summary dataframe for subset of taxa collected at Line 8:
SubsetACCMayZoop <- ACCMayZoop %>%
  mutate(STotCopepods = SmNcristatus + LgNcristatus + SmNpflemingeri + LgNpflemingeri + 
           SmEbungii + LgEbungii + SmCmarshallae + LgCmarshallae + SmCpacificus + LgCpacificus + 
           SmMpacifica + LgMpacifica + SmMetridia + LgMetridia + SmMetridinidae + 
           SmPseudocalanus + SmAcartia + SmOithona + SmUnidCalanids) %>%
  rename(SPodonidae = SmPodonidae) %>%
  mutate(STotEuphausiids = SThysanoessa + SToculatum + SEuphJuv) %>%
  mutate(STotOther = SCirripedia + SChaetnognatha + SNatantia + SCnidaria + SAmphipoda + 
           SMysidacea + SOstracoda + SThecosomata + SAnomura + SBrachyura + SIchthyoplankton) %>%
  select(Year, stationID, STotCopepods, SPodonidae, STotEuphausiids, STotOther)
#View(SubsetACCMayZoop)

# Calculate annual means across all 4 sites, for all taxa collected at GAK1-4 sites
AnnualMeansSubset = SubsetACCMayZoop %>%
  group_by(Year) %>%
  summarise_each(funs(mean)) %>% # warning message is not a problem (indicates that it's not possible to find means of stationID)
  ungroup %>%
  select(Year, STotCopepods, SPodonidae, STotEuphausiids, STotOther)
#View(AnnualMeansSubset)


#####################################################################################
#####################################################################################
#####################################################################################

# Plots & Stats

totals = full_join(AnnualMeansAll, AnnualMeansSubset, by = "Year")
#View(totals)
str(totals)

############################

# Stuff for poster:

# Using abundance data from Seward Line (where all zooplankton species were sampled),
# Is the subset of taxa collected at Line 8 (Shelikof) representative of total zooplankton abundance?

totalZoop <- totals %>%
  mutate(totalAll = ATotCopepods + APodonidae + ATotEuphausiids + ATotOther) %>%
  mutate(totalSubset = STotCopepods + SPodonidae + STotEuphausiids + STotOther) %>%
  select(Year, totalAll, totalSubset)
View(totalZoop)

# Correlation:
cor.test(log(totalZoop$totalAll), log(totalZoop$totalSubset))
# Pearson's product-moment correlation
# data:  log(totalZoop$totalAll) and log(totalZoop$totalSubset)
# t = 19.918, df = 11, p-value = 5.585e-10
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.9538557 0.9960490
# sample estimates:
# cor 
# 0.9864184 



# Plot time series

library(grid)
library(scales)

# Load Rachael's plot theme:
theme_boxplot <- function(base_size = 12){
  theme_bw(base_size) %+replace%
    theme(legend.key.size=unit(15,"points"),
          legend.text=element_text(size=I(13)),
          legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position="none",
          plot.margin=unit(c(0.25,2,0.25,2), "lines"), # respectively: top, right, bottom, left; refers to margin *outside* labels; default is c(1,1,0.5,0.5)
          panel.border=element_rect(colour='black', fill = NA),
          panel.margin=unit(0,"lines"),
          axis.ticks.length=unit(1,"mm"),
          axis.ticks.margin = unit(0, "lines"),
          axis.text=element_text(size=15),
          axis.title.x=element_text(hjust=.55, vjust=-.01, size=17),
          axis.title.y=element_text(size=17, angle=90, hjust=.56, vjust=-.001),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          strip.text.x=element_text(size=14),
          strip.background=element_rect(colour='black', fill='white'))
} 

# Total Zooplankton
ggplot(data=totalZoop, aes(x=Year, y = value)) + 
  geom_point(aes(y = totalAll), col = rgb(155, 255, 255, maxColorValue=255), size=3) + # LTOP
  geom_point(aes(y = totalSubset), col = rgb(107, 107, 255, maxColorValue = 255), size=3) + # subset sampeld by FOCI
  geom_line(aes(y = totalAll), col = rgb(155, 255, 255, maxColorValue=255), size=3) + # LTOP
  geom_line(aes(y = totalSubset), col = rgb(107, 107, 255, maxColorValue = 255), size=3) + # subset sampeld by FOCI
  theme_boxplot() +
  theme(axis.line=element_line('black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_y_log10() +
  coord_cartesian(ylim = c(350, 11000)) +
  theme(axis.text.x = element_text(angle=90))+
  theme(axis.text.y = element_text(size=22))+
  scale_x_continuous(breaks=c(seq(1996,2012,2)), labels=c(seq(1996,2012,2))) +
  ylab("Mean Abundance, Total Zooplankton (inidv / m3)") +
  xlab("Year") #+
#theme(legend.title=element_blank()) # turns off legend title
#ggsave("TotalZoopTimeSeries.png", width = 8, height = 5)


############################
# Other plots, not for poster

# Copepods
ggplot(data=totals, aes(x=Year, y = value, color = variable)) + 
  geom_point(aes(y = ATotCopepods), col = rgb(155, 255, 255, maxColorValue=255), size=2) + # LTOP
  geom_point(aes(y = STotCopepods), col = rgb(107, 107, 255, maxColorValue = 255), size=2) + # subset sampeld by FOCI
  geom_line(aes(y = ATotCopepods), col = rgb(155, 255, 255, maxColorValue=255), size=2) + # LTOP
  geom_line(aes(y = STotCopepods), col = rgb(107, 107, 255, maxColorValue = 255), size=2) + # subset sampeld by FOCI
  theme_boxplot() +
  theme(axis.line=element_line('black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_y_log10() +
  theme(axis.text.x = element_text(angle=90))+
  scale_x_continuous(breaks=c(seq(1996,2012,2)), labels=c(seq(1996,2012,2))) +
  ylab("Mean Copepod Abundance (inidv / m3)") +
  xlab("Year") +
  theme(legend.title=element_blank()) # turns off legend title
#ggsave("CopepodTimeSeries.png", width = 8, height = 5)


# Euphausiids
ggplot(data=totals, aes(x=Year, y = value, color = variable)) + 
  geom_point(aes(y = ATotEuphausiids), col = rgb(155, 255, 255, maxColorValue=255), size=2) + # LTOP
  geom_point(aes(y = STotEuphausiids), col = rgb(107, 107, 255, maxColorValue = 255), size=2) + # subset sampeld by FOCI
  geom_line(aes(y = ATotEuphausiids), col = rgb(155, 255, 255, maxColorValue=255), size=2) + # LTOP
  geom_line(aes(y = STotEuphausiids), col = rgb(107, 107, 255, maxColorValue = 255), size=2) + # subset sampeld by FOCI
  theme_boxplot() +
  theme(axis.line=element_line('black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_y_log10() +
  theme(axis.text.x = element_text(angle=90))+
  scale_x_continuous(breaks=c(seq(1996,2012,2)), labels=c(seq(1996,2012,2))) +
  ylab("Mean Euphausiid Abundance (inidv / m3)") +
  xlab("Year") #+
  #theme(legend.title=element_blank()) # turns off legend title
#ggsave("EuphausiidTimeSeries.png", width = 8, height = 5)


# Other Stuff
ggplot(data=totals, aes(x=Year, y = value, color = variable)) + 
  geom_point(aes(y = ATotOther), col = rgb(155, 255, 255, maxColorValue=255), size=2) + # LTOP
  geom_point(aes(y = STotOther), col = rgb(107, 107, 255, maxColorValue = 255), size=2) + # subset sampeld by FOCI
  geom_line(aes(y = ATotOther), col = rgb(155, 255, 255, maxColorValue=255), size=2) + # LTOP
  geom_line(aes(y = STotOther), col = rgb(107, 107, 255, maxColorValue = 255), size=2) + # subset sampeld by FOCI
  theme_boxplot() +
  theme(axis.line=element_line('black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_y_log10() +
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_continuous(breaks=c(seq(1996,2012,2)), labels=c(seq(1996,2012,2))) +
  ylab("Mean Abundance, Other Zooplankton (inidv / m3)") +
  xlab("Year") +
  theme(legend.title=element_blank()) # turns off legend title



# Copepods: Are the subset and totals correlated?
cor.test(log(totals$ATotCopepods), log(totals$STotCopepods))
# Pearson's product-moment correlation
# data:  log(totals$ATotCopepods) and log(totals$STotCopepods)
# t = 21.972, df = 11, p-value = 1.945e-10
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.9618314 0.9967441
# sample estimates:
# cor 
# 0.9887982


# Euphausiids: Are the subset and totals correlated?
cor.test(log(totals$ATotEuphausiids), log(totals$STotEuphausiids))
# For GAK1-4:
# Pearson's product-moment correlation
# data:  log(totals$ATotEuphausiids) and log(totals$STotEuphausiids)
# t = 4.3503, df = 11, p-value = 0.001155
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.4347578 0.9360731
# sample estimates:
# cor 
# 0.7952458


# For GAK1-13:
# Pearson's product-moment correlation
# t = 6.5872, df = 11, p-value = 3.931e-05
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.6737716 0.9678542
# sample estimates:
# cor 
# 0.8931759 



# Other Zooplankton: Are the subset and totals correlated?
cor.test(log(totals$ATotOther), log(totals$STotOther))
# For GAK1-4:
# Pearson's product-moment correlation
# data:  log(totals$ATotOther) and log(totals$STotOther)
# t = 2.5378, df = 11, p-value = 0.02759
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.08523391 0.86802969
# sample estimates:
# cor 
# 0.6076809

# For GAK1-13:
# Pearson's product-moment correlation
# data:  log(totals$ATotOther) and log(totals$STotOther)
# t = 4.8469, df = 11, p-value = 0.0005133
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.5030388 0.9460717
# sample estimates:
# cor 
# 0.8252829 


