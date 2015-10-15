#######################################################################
##### Seward Line / Shelikof Line 8 Zooplankton Comparison Script #####
#####     Script by Colette Ward (ward@nceas.ucsb.edu)           ######
#####                       October 2015                         ######
#######################################################################


# Call data from Small Zooplankton Cleaning script
source('SewardLineSmallZoopCleaningScript.R')
View(SCZo1)

# Call data from Large Zooplankton Processing script
source('SewardLineLgZoopProcessingScript.R') # does not work???
View(DepthInt.taxinfo)

# Purpose: Is the subset of copepod species sampled at Shelikof Strait representative of total copepod abundance?
# Use Seward Line data to analyze

# use ACC (Alaska Coastal Current) sites. For Seward Line, ACC sites are GAK 1-4

# use only: 
# (i) size classes focused on (ie attempt to count 30) as per Siefert & Incze 1991 p4 (1989 Sampling report)
# and (ii) which were quantitatively retained by nets (re body size & lack of net avoidance)
# Calanus marshallae (C4 - adults)
# C. pacificus (C4 - adults)
# Eucalanus bungii (C1 - adults)
# Metridia spp. (No, not counted to 30)
# M. pacifica (C4 - adults)
# Neocalanus cristatus (C4, C5)
# N. plumchrus-flemingeri (C4, C5)
# Pseudocalanus (C4 - adults)


# ---------------------------------------------------------------------------------------------------

# For the subset of copepod species & stages quantitatively sampled at Shelikof St, create May Small Zooplankton Abundance for GAK 1-4

# Extract Seward Line (GAK) sites from Small Zoop file
May.s = SCZo1 %>%
  filter(stationID %in% c("GAK1", "GAK2", "GAK3", "GAK4")) %>%
  filter(Month == 5)

# Calanus pacificus, stage IV
SSmCpacificus = May.s %>%
  filter(sciName == "Calanus pacificus") %>%
  filter(stage %in% c("IV", "C4")) %>%
  group_by(Year, stationID) %>%
  summarise(SSmCpacificusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SSmCpacificus=mean(SSmCpacificusSite)) %>% 
  ungroup
#View(SSmCpacificus)


# Eucalanus bungii, stages I-III
SSmEbungii = May.s %>%
  filter(sciName == "Eucalanus bungii") %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  group_by(Year, stationID) %>%
  summarise(SSmEbungiiSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SSmEbungii=mean(SSmEbungiiSite)) %>% 
  ungroup
#View(SSmEbungii)


# Metridia pacifica, stages IV, V, Adult Males
SSmMpacifica = May.s %>%
  filter(sciName == "Metridia pacifica") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(SSmMpacificaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SSmMpacifica=mean(SSmMpacificaSite)) %>% 
  ungroup
#View(SSmMpacifica)


# Pseudocalanus spp., stages IV - adult
SSmPseudocalanus = May.s %>%
  filter(species == "Pseudocalanus_sp." | sciName == "Pseudocalanus_sp.") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AM", "AF")) %>%
  group_by(Year, stationID) %>%
  summarise(SPseudocalanusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SSmPseudocalanus=mean(SPseudocalanusSite)) %>% 
  ungroup
#View(SSmPseudocalanus)

# -----------------------

# Create dataframe with years:
SACCMaySmZoAbund=data.frame('Year'=c(1998:2010))

# Merge in the taxon-specific biomass data
SACCMaySmZoAbund <- merge(SACCMaySmZoAbund,SSmCpacificus,all.x=T)
SACCMaySmZoAbund <- merge(SACCMaySmZoAbund,SSmEbungii,all.x=T)
SACCMaySmZoAbund <- merge(SACCMaySmZoAbund,SSmMpacifica,all.x=T)
SACCMaySmZoAbund <- merge(SACCMaySmZoAbund,SSmPseudocalanus,all.x=T)

#View(SACCMaySmZoAbund)


# ---------------------------------------------------------------------------------------------------

# For the subset of copepod species & stages quantitatively sampled at Shelikof St, create May Large Zooplankton Abundance for GAK 1-4

# Extract Seward Line (GAK) sites from Large Zoop file
MayACC = DepthInt.taxinfo %>%
  filter(stationID %in% c("GAK1", "GAK2", "GAK3", "GAK4")) %>%
  filter(Month == 5)

# Calanus marshallae, stages IV, V
SLgCmarshallae = MayACC %>%
  filter(sciName == "Calanus marshallae") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(SLgCmarshallaeSite=sum(abundance)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(SLgCmarshallae=mean(SLgCmarshallaeSite)) %>%
  ungroup
#View(SLgCmarshallae)


# Calanus pacificus, stages V, adults
SLgCpacificus = MayACC %>%
  filter(sciName == "Calanus pacificus") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(SLgCpacificusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SLgCpacificus=mean(SLgCpacificusSite)) %>% 
  ungroup
#View(SLgCpacificus)


# Eucalanus bungii, stages IV, V, adults
SLgEbungii = MayACC %>%
  filter(sciName == "Eucalanus bungii") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(SLgEbungiiSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SLgEbungii=mean(SLgEbungiiSite)) %>% 
  ungroup
#View(SLgEbungii)


# Metridia pacifica, stages Females
SLgMpacifica = MayACC %>%
  filter(sciName == "Metridia pacifica") %>% 
  filter(stage == "AF") %>%
  group_by(Year, stationID) %>%
  summarise(SLgMpacificaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SLgMpacifica=mean(SLgMpacificaSite)) %>% 
  ungroup
#View(SLgMpacifica)


# Neocalanus cristatus, stages IV, V
SLgNcristatus = MayACC %>%
  filter(sciName == "Neocalanus cristatus") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5")) %>%
  group_by(Year, stationID) %>%
  summarise(SLgNcristatusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SLgNcristatus=mean(SLgNcristatusSite)) %>% 
  ungroup
#View(SLgNcristatus)


# Neocalanus plumchrus-flemingeri, stages IV, V
SLgNpflemingeri = MayACC %>%
  filter(sciName %in% c("Neocalanus plumchrus", "Neocalanus flemingeri")) %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "CV_large")) %>%
  group_by(Year, stationID) %>%
  summarise(SLgNpflemingeriSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SLgNpflemingeri=mean(SLgNpflemingeriSite)) %>% 
  ungroup
#View(SLgNpflemingeri)


# -----------------------

# Create dataframe with years:
SACCMayLgZoAbund=data.frame('Year'=c(1998:2010))

# Merge in the taxon-specific biomass data
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SLgCmarshallae,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SLgCpacificus,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SLgEbungii,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SLgMpacifica,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SLgNcristatus,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SLgNpflemingeri,all.x=T)

#View(SACCMayLgZoAbund)

# ---------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------

# Merge Small & Large Zooplankton abundance datasets (Shelikof sp subset)
SACCMayZoop = full_join(SACCMaySmZoAbund, SACCMayLgZoAbund, by = "Year")
SACCMayZoop[is.na(SACCMayZoop)] <- 0 # replace NA with 0
str(SACCMayZoop)
#View(SACCMayZoop)

# Sum large & small zooplankton abundance by species
STACCMayZoop = SACCMayZoop %>%
  rename(SCmarshallae = SLgCmarshallae) %>%
  mutate(SCpacificus = SSmCpacificus + SLgCpacificus) %>%
  mutate(SEbungii = SSmEbungii + SLgEbungii) %>%
  mutate(SMpacifica = SSmMpacifica + SLgMpacifica) %>%
  rename(SNcristatus = SLgNcristatus) %>%
  rename(SNpflemingeri = SLgNpflemingeri) %>%
  rename(SPseudocalanus = SSmPseudocalanus) %>%
  select(Year, SCmarshallae, SCpacificus, SEbungii, SMpacifica, SNcristatus, SNpflemingeri, SPseudocalanus)
STACCMayZoop = STACCMayZoop %>%
  mutate(SACCTotCopepods = rowSums(STACCMayZoop[,2:8]))
View(STACCMayZoop)


# Plot data
ggplot(data=STACCMayZoop, aes(Year, y = value, color = variable)) + 
  geom_point(aes(y = SCmarshallae, col = "C. marshallae")) +
  geom_point(aes(y = SCpacificus, col = "C. pacificus")) +
  geom_point(aes(y = SEbungii, col = "E. bungii")) +
  geom_point(aes(y = SMpacifica, col = "M. pacifica")) +
  geom_point(aes(y = SNcristatus, col = "N. cristatus")) +
  geom_point(aes(y = SNpflemingeri, col = "N. plumchurus-flemingeri")) +
  geom_point(aes(y = SPseudocalanus, col = "Pseudocalanus")) +
  geom_point(aes(y = SACCTotCopepods, col = "Total")) +
  geom_line(aes(y = SCmarshallae, col = "C. marshallae")) +
  geom_line(aes(y = SCpacificus, col = "C. pacificus")) +
  geom_line(aes(y = SEbungii, col = "E. bungii")) +
  geom_line(aes(y = SMpacifica, col = "M. pacifica")) +
  geom_line(aes(y = SNcristatus, col = "N. cristatus")) +
  geom_line(aes(y = SNpflemingeri, col = "N. plumchurus-flemingeri")) +
  geom_line(aes(y = SPseudocalanus, col = "Pseudocalanus")) +
  geom_line(aes(y = SACCTotCopepods, col = "Total")) +
  theme_bw() +
  scale_y_log10() +
  coord_cartesian(xlim = c(1996, 2012)) +
  ylab("Mean May Copepod Abundance, GAK1-4 (inidv/m3)") +
  xlab("Year") +
  theme(legend.title=element_blank()) # turns off legend title


# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------

# Now create Total Copepod Abundance for total ACC Seward Samples (small & large)

# 1. Small Copepods (from CalVET net)

# Copepod stage size / net classifications from Coyle & Pinchuk 2003:
# Acartia, all life stages
ASmAcartia = May.s %>%
  filter(family == "Acartiidae") %>%
  group_by(Year, stationID) %>%
  summarise(AAcartiaSite=sum(abundance)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmAcartia=mean(AAcartiaSite)) %>% 
  ungroup


# Aetideidae, stages I-IV # NB there are none in GAK samples
ASmAetideidae = May.s %>%
  filter(family == "Aetideidae") %>%
  filter(stage %in% c("I", "II", "III", "IV", "C1", "C2", "C3", "C4")) %>%
  group_by(Year, stationID) %>%
  summarise(AAetideidaeSite=sum(abundance)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmAetideidae=mean(AAetideidaeSite)) %>% 
  ungroup

# Calanus marshallae, stages I - III
ASmCmarshallae = May.s %>%
  filter(species %in% c("Calanus_marshallae", "marshallae")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  group_by(Year, stationID) %>%
  summarise(ACmarshallaeSite=sum(abundance)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmCmarshallae=mean(ACmarshallaeSite)) %>%
  ungroup


# Calanus pacificus, stages I - IV
ASmCpacificus = May.s %>%
  filter(species %in% c("Calanus_pacificus", "pacificus")) %>%  # does not include Aetideus pacificus
  filter(stage %in% c("I", "II", "III", "IV", "C1", "C2", "C3", "C4")) %>%
  group_by(Year, stationID) %>%
  summarise(ACpacificusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmCpacificus=mean(ACpacificusSite)) %>% 
  ungroup


# Candacia columbiae, stages I - III # none at GAK sites in May
ASmCcolumbiae = May.s %>%
  filter(species %in% c("Candacia_columbiae", "columbiae")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  group_by(Year, stationID) %>%
  summarise(ACcolumbiaeSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmCcolumbiae=mean(ACcolumbiaeSite)) %>% 
  ungroup


# Centropages abdominalis, all stages
ASmCabdominalis = May.s %>%
  filter(species %in% c("Centropages_abdominalis", "abdominalis")) %>% 
  group_by(Year, stationID) %>%
  summarise(ACabdominalisSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmCabdominalis=mean(ACabdominalisSite)) %>% 
  ungroup


# Clausocalanus spp., all stages
ASmClausocalanus = May.s %>%
  filter(species %in% c("Clausocalanus_sp.", "Clausocalanus_parapergens", "Clausocalanus_lividus") | genus == "Clausocalanus") %>%
  group_by(Year, stationID) %>%
  summarise(AClausocalanusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmClausocalanus=mean(AClausocalanusSite)) %>% 
  ungroup


# Epilabidocera amphitrites, stages I - III; NB none in May GAK samples - but add in Apr 30 sample from GAK2 in 2002 (cruiseID == hx258)
ASmEamphitrites = May.s %>%
  filter(species %in% c("Epilabidocera_amphitrites", "amphitrites")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  group_by(Year, stationID) %>%
  summarise(AEamphitritesSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmEamphitrites=mean(AEamphitritesSite)) %>% 
  ungroup


# Eucalanus bungii, stages I - III
ASmEbungii = May.s %>%
  filter(species %in% c("Eucalanus_bungii", "bungii")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  group_by(Year, stationID) %>%
  summarise(AEbungiiSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmEbungii=mean(AEbungiiSite)) %>% 
  ungroup


# Euchaeta elongata, stages I - II # renamed to Paraeuchaeta elongata
ASmPelongata = May.s %>%
  filter(species %in% c("Paraeuchaeta_elongata", "elongata")) %>% 
  filter(stage %in% c("I", "II", "C1", "C2")) %>%
  group_by(Year, stationID) %>%
  summarise(APelongataSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmPelongata=mean(APelongataSite)) %>% 
  ungroup


# Eurytemora, all life stages; NB doesn't appear at GAK sites
ASmEurytemora = May.s %>%
  filter(species == "Eurytemora_sp.") %>%
  group_by(Year, stationID) %>%
  summarise(AEurytemoraSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmEurytemora=mean(AEurytemoraSite)) %>%
  ungroup

# Heterorhabdus spp., stages I - III  NB none in May GAK samples
ASmHeterorhabdus = May.s %>%
  filter(species %in% c("Heterorhabdus_sp.", "Heterorhabdus_tanneri", "tanneri")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  group_by(Year, stationID) %>%
  summarise(AHeterorhabdusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmHeterorhabdus=mean(AHeterorhabdusSite)) %>% 
  ungroup

# Heterostylites spp., stages I - IV # not in CalVET nets at all?


# Lucicutia spp., stages I - IV; none in May GAK samples
ASmLucicutia = May.s %>%
  filter(species %in% c("Lucicutia_sp.", "Lucicutia_flavicornis")) %>% 
  filter(stage %in% c("I", "II", "III", "IV", "C1", "C2", "C3", "C4")) %>%
  group_by(Year, stationID) %>%
  summarise(ALucicutiaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmLucicutia=mean(ALucicutiaSite)) %>% 
  ungroup

# Mesocalanus tenuicornis, all stages (include Mesocalanus sp. here as well, there are only 5 entries, all in 1998)
ASmMesocalanus = May.s %>%
  filter(species %in% c("Mesocalanus_tenuicornis", "Mesocalanus_sp.", "tenuicornis")) %>% 
  group_by(Year, stationID) %>%
  summarise(AMesocalanusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmMesocalanus=mean(AMesocalanusSite)) %>% 
  ungroup


# Metridia okhotensis, stages I - IV
ASmMokhotensis = May.s %>%
  filter(species %in% c("Metridia_okhotensis", "okhotensis")) %>% 
  filter(stage %in% c("I", "II", "III", "IV", "C1", "C2", "C3", "C4")) %>%
  group_by(Year, stationID) %>%
  summarise(AMokhotensisSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmMokhotensis=mean(AMokhotensisSite)) %>% 
  ungroup


# Metridia pacifica, stages I - V & Males
ASmMpacifica = May.s %>%
  filter(species == "Metridia_pacifica" | sciName =="Metridia pacifica") %>% 
  filter(stage %in% c("I", "II", "III", "IV", "V", "C1", "C2", "C3", "C4", "C5", "AM", "Male", "males")) %>%  # don't need to look at NAs, there are none for M pacifica
  group_by(Year, stationID) %>%
  summarise(AMpacificaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmMpacifica=mean(AMpacificaSite)) %>% 
  ungroup


# Microcalanus spp., all stages
ASmMicrocalanus = May.s %>%
  filter(species == "Microcalanus_sp." | sciName == "Microcalanus_sp.") %>% 
  group_by(Year, stationID) %>%
  summarise(AMicrocalanusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmMicrocalanus=mean(AMicrocalanusSite)) %>% 
  ungroup


# Neocalanus cristatus, stages I - II
ASmNcristatus = May.s %>%
  filter(species %in% c("Neocalanus_cristatus", "cristatus")) %>% 
  filter(stage %in% c("I", "II", "C1", "C2")) %>%
  group_by(Year, stationID) %>%
  summarise(ANcristatusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmNcristatus=mean(ANcristatusSite)) %>% 
  ungroup


# Neocalanus plumchrus-flemingeri, stages I - III # NB none in May GAK samples
ASmNpflemingeri = May.s %>%
  filter(species %in% c("Neocalanus_plumchrus", "Neocalanus_flemingeri", "plumchrus", "flemingeri")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  group_by(Year, stationID) %>%
  summarise(ANpflemingeriSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmNpflemingeri=mean(ANpflemingeriSite)) %>% 
  ungroup

# Oithona spp., all stages
ASmOithona = May.s %>%
  filter(species %in% c("Oithona_similis", "Oithona_spinirostris", "Oithona_sp.", "similis", "spinirostris") | sciName == "Oithona_sp.") %>% 
  group_by(Year, stationID) %>%
  summarise(AOithonaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmOithona=mean(AOithonaSite)) %>% 
  ungroup


# Oncaea spp., all stages
ASmOncaea = May.s %>%
  filter(species == "Oncaea_sp." | sciName == "Oncaea_sp.") %>% 
  group_by(Year, stationID) %>%
  summarise(AOncaeaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmOncaea=mean(AOncaeaSite)) %>% 
  ungroup


# Paracalanus spp., all stages; NB none in May GAK samples
ASmParacalanus = May.s %>%
  filter(species %in% c("Paracalanus_parvus", "Paracalanus_sp.", "parvus") | sciName == "Paracalanus_sp.") %>% 
  group_by(Year, stationID) %>%
  summarise(AParacalanusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmParacalanus=mean(AParacalanusSite)) %>% 
  ungroup

# Pleuromamma spp., stgbes I-IV (family Metridinidae, genus Pleuromamma)  NB None in this dataset ... ?

# Pseudocalanus spp., all stages
ASmPseudocalanus = May.s %>%
  filter(species == "Pseudocalanus_sp." | sciName == "Pseudocalanus_sp.") %>% 
  group_by(Year, stationID) %>%
  summarise(APseudocalanusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmPseudocalanus=mean(APseudocalanusSite)) %>% 
  ungroup


# Racovitzanus antarcticus, all stages
ASmRantacrticus = May.s %>%
  filter(species %in% c("Racovitzanus_antarcticus", "antarcticus")) %>% 
  group_by(Year, stationID) %>%
  summarise(ARantacrticusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmRantacrticus=mean(ARantacrticusSite)) %>% 
  ungroup


# Scolecithricella spp., all stages
ASmScolecithricella = May.s %>%
  filter(species %in% c("Scolecithricella_minor", "Scolecithricella_sp.", "Pseudoamallothrix_ovata", "Pseudoamallothrix_sp.", "minor") | sciName == "Scolecithricella_sp.") %>%  # "ovata" not in species column
  group_by(Year, stationID) %>%
  summarise(AScolecithricellaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmScolecithricella=mean(AScolecithricellaSite)) %>% 
  ungroup


# Tortanus discaudata, all stages; NB none in May GAK samples (only appeared once in sampling record)
ASmTdiscaudata = May.s %>%
  filter(species %in% c("Tortanus_discaudatus", "discaudatus")) %>% 
  group_by(Year, stationID) %>%
  summarise(ATdiscaudataSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmTdiscaudata=mean(ASmTdiscaudataSite)) %>% 
  ungroup

# -----------------------

# Other copepod taxa present in CalVET nets but not assigned to size / net group by Coyle & Pinchuk 2003:

# Metridia sp. Follow Coyle & Pinchuk's 2003 classification for M. okhotensis
ASmMetridia = May.s %>%
  filter(species == "Metridia_sp." | sciName %in% c("Metridia sp.", "Metridia spp.")) %>%
  group_by(Year, stationID) %>%
  summarise(AMetridiaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmMetridia=mean(AMetridiaSite)) %>% 
  ungroup


# Microsetella sp. only found in 2004.  Adult females are 0.02mg. Therefore take all stages from CalVET nets
ASmMicrosetella = May.s %>%
  filter(species == "Microsetella_sp." | sciName == "Microsetella sp.") %>%
  group_by(Year, stationID) %>%
  summarise(AMicrosetellaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmMicrosetella=mean(AMicrosetellaSite)) %>% 
  ungroup


# Monstrilla sp. (infraclass Neocopepod).  Not sure, and only approx 20 entries across entire dataset, therefore leave this for now
#Parasitic on marine benthic inverts (polychaetes, gastropods); only the first nauplius & adult stages are free-swimming (adults don't feed)

# Neocalanus sp.  Follow Coyle & Pinchuk's 2003 classification for N. cristatus; NB none of these stages are in May GAK samples
ASmNeocalanus = May.s %>%
  filter(species == "Neocalanus_sp." | sciName == "Neocalanus_sp.") %>% 
  filter(stage %in% c("I", "II", "C1", "C2")) %>%
  group_by(Year, stationID) %>%
  summarise(ANeocalanusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmNeocalanus=mean(ANeocalanusSite)) %>% 
  ungroup


# -----------------------

# Other non-copepod crustacean zooplankton present in CalVET nets

# class Branchiopoda, family Podonidae (Cladocerans: Evadne sp.  & Podon sp.)  
# Judging from length comparisons with Neocalanus sp (these are much smaller in length) on Seward Line website, all stages of Branchiopods should come from CalVET
# NB None in May GAK samples
ASmPodonidae = May.s %>%
  filter(family == "Podonidae") %>% 
  group_by(Year, stationID) %>%
  summarise(APodonidaeSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ASmPodonidae=mean(APodonidaeSite)) %>% 
  ungroup


# ----------------

# Create dataframe with years:
ACCMaySmZoAbund=data.frame('Year'=c(1998:2010))


# Merge in the taxon-specific biomass data
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmAcartia,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmAetideidae,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmCmarshallae,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmCpacificus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmCcolumbiae,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmCabdominalis,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmClausocalanus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmEamphitrites,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmEbungii,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmPelongata,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmEurytemora,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmHeterorhabdus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmLucicutia,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmMesocalanus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmMokhotensis,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmMpacifica,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmMicrocalanus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmNcristatus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmNpflemingeri,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmOithona,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmOncaea,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmParacalanus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmPseudocalanus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmRantacrticus,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmScolecithricella,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmMetridia,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmMicrosetella,all.x=T)
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmNeocalanus,all.x=T)
#ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmPodonidae,all.x=T) # Cladoceran

View(ACCMaySmZoAbund)


# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------

# Large zooplankton (from MOCNESS net)

# Copepod stage size / net classifications from Coyle & Pinchuk 2003:
# Aetideidae, stages V, Adults
AAetideidaeAb = MayACC %>%
  filter(family == "Aetideidae") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AAetideidaeAbSite=sum(abundance)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(AAetideidaeAb=mean(AAetideidaeAbSite)) %>% 
  ungroup


# Calanus marshallae, stages IV, V, adults
ACmarshallaeAb = MayACC %>%
  filter(sciName == "Calanus marshallae") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(ACmarshallaeAbSite=sum(abundance)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(ACmarshallaeAb=mean(ACmarshallaeAbSite)) %>%
  ungroup


# Calanus pacificus, stages V, adults
ACpacificusAb = MayACC %>%
  filter(sciName == "Calanus pacificus") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(ACpacificusAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ACpacificusAb=mean(ACpacificusAbSite)) %>% 
  ungroup


# Candacia, stages IV, V, adults
ACandaciaAb = MayACC %>%
  filter(genus == "Candacia") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(ACandaciaAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ACandaciaAb=mean(ACandaciaAbSite)) %>% 
  ungroup


# Epilabidocera amphitrites, stages IV, V, adults;  add in Apr 30 sample from GAK2 in 2002 (cruiseID == hx258)
AEamphitritesAb = MayACC %>%
  filter(species == "Epilabidocera amphitrites") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AEamphitritesAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(AEamphitritesAb=mean(AEamphitritesAbSite)) %>% 
  ungroup


# Eucalanus bungii, stages IV, V, adults
AEbungiiAb = MayACC %>%
  filter(sciName == "Eucalanus bungii") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AEbungiiAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(AEbungiiAb=mean(AEbungiiAbSite)) %>% 
  ungroup


# Euchaeta elongata, stages III-V, adults
APelongataAb = MayACC %>%
  filter(species %in% c("Paraeuchaeta elongata", "Elongata")) %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(APelongataAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(APelongataAb=mean(APelongataAbSite)) %>% 
  ungroup


# Heterorhabdus spp., stages IV, V, adults
AHeterorhabdusAb = MayACC %>%
  filter(genus == "Heterorhabdus") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AHeterorhabdusAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(AHeterorhabdusAb=mean(AHeterorhabdusAbSite)) %>% 
  ungroup


# Heterostylites spp., stages V, adults; none in May GAK samples
AHeterostylitesAb = MayACC %>%
  filter(genus == "Heterostylites") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AHeterostylitesAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(AHeterostylitesAb=mean(AHeterostylitesAbSite)) %>% 
  ungroup


# Lucicutia spp., stages V, adults
ALucicutiaAb = MayACC %>%
  filter(genus == "Lucicutia") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(ALucicutiaAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ALucicutiaAb=mean(ALucicutiaAbSite)) %>% 
  ungroup


# Metridia okhotensis, stages V, adults
AMokhotensisAb = MayACC %>%
  filter(sciName == "Metridia okhotensis") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AMokhotensisAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(AMokhotensisAb=mean(AMokhotensisAbSite)) %>% 
  ungroup


# Metridia pacifica, stages Females
AMpacificaAb = MayACC %>%
  filter(sciName == "Metridia pacifica") %>% 
  filter(stage == "AF") %>%
  group_by(Year, stationID) %>%
  summarise(AMpacificaAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(AMpacificaAb=mean(AMpacificaAbSite)) %>% 
  ungroup


# Neocalanus cristatus, stages III-V, adults
ANcristatusAb = MayACC %>%
  filter(sciName == "Neocalanus cristatus") %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(ANcristatusAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ANcristatusAb=mean(ANcristatusAbSite)) %>% 
  ungroup


# Neocalanus plumchrus-flemingeri, stages IV, V, adults
ANpflemingeriAb = MayACC %>%
  filter(sciName %in% c("Neocalanus plumchrus", "Neocalanus flemingeri")) %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "CV_large", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(ANpflemingeriAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ANpflemingeriAb=mean(ANpflemingeriAbSite)) %>% 
  ungroup


# Pleuromamma spp., stages V, adults 
APleuromammaAb = MayACC %>%
  filter(genus == "Pleuromamma") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(APleuromammaAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(APleuromammaAb=mean(APleuromammaAbSite)) %>% 
  ungroup

# -----------------------

# Other copepod taxa present but not assigned to size / net group by Coyle & Pinchuk 2003:

# Gaussia princeps   Follow Coyle & Pinchuk's classification for congenors Pleuromamma & Metridia
# None in GAK May samples
AGprincepsAb = MayACC %>%
  filter(sciName == "Gaussia princeps") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AGprincepsAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(AGprincepsAb=mean(AGprincepsAbSite)) %>% 
  ungroup


# Metridia sp. Follow Coyle & Pinchuk's 2003 classification for M. okhotensis
# ie take stages V, adults from large nets
AMetridiaAb = MayACC %>%
  filter(sciName == "Metridia") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AMetridiaAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(AMetridiaAb=mean(AMetridiaAbSite)) %>% 
  ungroup


# Monstrilla sp. (infraclass Neocopepod).  Not captured in large zoop nets


# Neocalanus sp.  Follow Coyle & Pinchuk's 2003 classification for N. cristatus
# ie take III-V, adults from large nets
ANeocalanusAb = MayACC %>%
  filter(sciName == "Neocalanus") %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(ANeocalanusAbSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ANeocalanusAb=mean(ANeocalanusAbSite)) %>% 
  ungroup


# -----------------------

# Create dataframe with years:
ACCMayLgZoAbund=data.frame('Year'=c(1998:2010))

# Merge in the taxon-specific abundance data
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,AAetideidaeAb,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ACmarshallaeAb,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ACpacificusAb,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ACandaciaAb,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,AEamphitritesAb,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,AEbungiiAb,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,APelongataAb,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,AHeterorhabdusAb,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,AHeterostylitesAb,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALucicutiaAb,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,AMokhotensisAb,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,AMpacificaAb,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ANcristatusAb,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ANpflemingeriAb,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,APleuromammaAb,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,AGprincepsAb,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,AMetridiaAb,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ANeocalanusAb,all.x=T)

View(ACCMayLgZoAbund)

# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------


# Merge Small & Large Zooplankton abundance datasets
ACCMayZoop = full_join(ACCMaySmZoAbund, ACCMayLgZoAbund, by = "Year")
ACCMayZoop[is.na(ACCMayZoop)] <- 0 # replace NA with 0
str(ACCMayZoop)
View(ACCMayZoop)

# Calculate May Total Copepod Abundance for GAK1-4 sites
TACCMayZoop = ACCMayZoop %>%
  mutate(ACCTotCopepods = rowSums(ACCMayZoop[,2:47]))
View(TACCMayZoop)


# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------

# Using abundance data from Seward Line (where all zooplankton species were sampled),
# Is the subset of copepod species sampled at Shelikof Strait representative of total copepod abundance?

totals = full_join(TACCMayZoop, STACCMayZoop, by = "Year") %>%
  select(Year, ACCTotCopepods, SACCTotCopepods)
View(totals)

# Plot time series
ggplot(data=totals, aes(x=Year, y = value, color = variable)) + 
  geom_point(aes(y = ACCTotCopepods, col = "Total Copepod Abundance")) +
  geom_point(aes(y = SACCTotCopepods, col = "Subset Copepod Abundance")) +
  geom_line(aes(y = ACCTotCopepods, col = "Total Copepod Abundance")) +
  geom_line(aes(y = SACCTotCopepods, col = "Subset Copepod Abundance")) +
  theme_bw() +
  scale_y_log10() +
  coord_cartesian(xlim = c(1996, 2012)) +
  ylab("Abundance (inidv / m3)") +
  xlab("Year") +
  theme(legend.title=element_blank()) # turns off legend title
ggsave("TimeSeries.png", width = 8, height = 5)


# Are the subset and totals correlated?
cor.test(log(totals$ACCTotCopepods), log(totals$SACCTotCopepods))
#t = 5.6906, df = 11, p-value = 0.0001401
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.5973415 0.9586196
#sample estimates:
#  cor 
#0.8639711 

# Plot correlation
ggplot(data=totals) + 
  geom_point(aes(x = ACCTotCopepods, y = SACCTotCopepods, size = 6)) +
  theme_bw() +
  scale_y_log10() +
  scale_x_log10() +
  coord_cartesian(ylim = c(100, 2200)) +
  ylab("Subset Copepod Abundance (inidv / m3)") +
  xlab("Total Copepod Abundance (indiv / m3)") 
ggsave("Correlation.png", width = 6, height = 5)
