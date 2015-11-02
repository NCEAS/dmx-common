#######################################################################
##### Seward Line / Shelikof Line 8 Zooplankton Comparison Script #####
#####     Script by Colette Ward (ward@nceas.ucsb.edu)           ######
#####                       October 2015                         ######
#######################################################################


# Run SewardLineSmZoopCleaningScript and SewardLineLgZoopCleaningScript
# Run SewardLineLgZoopProcessingScript up to line 54

#sourceDir <- function(path, trace=TRUE) {
#  for (nm in list.files(path, pattern = "[.][Rr]$")) {
#    if(trace) cat(nm,":")
#    source(file.path(path, nm))
#    if(trace) cat("\n")
#  }
#}
#sourceDir("SewardLineZooplankton")
#sourceDir("Seward Line Zooplankton/Cleaning_Scripts")



# Purpose: Is the subset of copepod species sampled at Shelikof Strait representative of total copepod abundance?
# Use Seward Line data to analyze

# use ACC (Alaska Coastal Current) sites. For Seward Line, ACC sites are GAK 1-4


# ---------------------------------------------------------------------------------------------------

# For the subset of copepod species & stages quantitatively sampled at Shelikof St, create May Small Zooplankton Abundance for GAK 1-4

# Extract Seward Line (GAK) sites from Small Zoop file
May.s = SCZo1 %>%
  filter(stationID %in% c("GAK1", "GAK2", "GAK3", "GAK4")) %>%
  filter(Month == 5)


# Neocalanus cristatus, stage II
SSmNcristatus = May.s %>%
  filter(species %in% c("Neocalanus_cristatus", "cristatus")) %>% 
  filter(stage %in% c("II", "C2")) %>%
  group_by(Year, stationID) %>%
  summarise(NcristatusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SSmNcristatus=mean(NcristatusSite)) %>% 
  ungroup

# Neocalanus spp (N. flemingeri + N. plumchurus), stages II, III  # NB none in May GAK samples
SSmNpflemingeri = May.s %>%
  filter(species %in% c("Neocalanus_plumchrus", "Neocalanus_flemingeri", "plumchrus", "flemingeri")) %>% 
  filter(stage %in% c("II", "III", "C2", "C3")) %>%
  group_by(Year, stationID) %>%
  summarise(NpflemingeriSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SSmNpflemingeri=mean(NpflemingeriSite)) %>% 
  ungroup


# Eucalanus bungii, stages I - III
SSmEbungii = May.s %>%
  filter(species %in% c("Eucalanus_bungii", "bungii")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  group_by(Year) %>% #, stationID) %>%
  summarise(EbungiiSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SSmEbungii=mean(EbungiiSite)) %>% 
  ungroup


# Calanus marshallae, II-III
SSmCmarshallae = May.s %>%
  filter(species %in% c("Calanus_marshallae", "marshallae")) %>% 
  filter(stage %in% c("II", "III", "C2", "C3")) %>%
  group_by(Year, stationID) %>%
  summarise(CmarshallaeSite=sum(abundance)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(SSmCmarshallae=mean(CmarshallaeSite)) %>%
  ungroup


# Calanus pacificus, IV
SSmCpacificus = May.s %>%
  filter(species %in% c("Calanus_pacificus", "pacificus")) %>%  # does not include Aetideus pacificus
  filter(stage %in% c("IV", "C4")) %>%
  group_by(Year, stationID) %>%
  summarise(CpacificusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SSmCpacificus=mean(CpacificusSite)) %>% 
  ungroup


# Metridia pacifica/lucens, IV, V, AM (is there a stage VI?)
SSmMpacifica = May.s %>%
  filter(species == "Metridia_pacifica" | sciName =="Metridia pacifica") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AM", "Male", "males")) %>%  # don't need to look at NAs, there are none for M pacifica
  group_by(Year, stationID) %>%
  summarise(MpacificaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SSmMpacifica=mean(MpacificaSite)) %>% 
  ungroup




# Metridia spp., stage IV 
# Metridia sp. Follow Coyle & Pinchuk's 2003 classification for M. okhotensis
SSmMetridia = May.s %>%
  filter(species == "Metridia_sp." | sciName %in% c("Metridia sp.", "Metridia spp.") | species %in% c("Metridia_okhotensis", "okhotensis")) %>%
  filter(stage %in% c("IV", "C4")) %>%
  group_by(Year, stationID) %>%
  summarise(MetridiaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SSmMetridia=mean(MetridiaSite)) %>% 
  ungroup



# Metrididae, I-III
# Family Metridinidae includes Gaussia, Metridia, Pleuromamma.  
# EcoFOCI has already specified Metridia spp. as per above, 
# Gaussia princpeps was observed only once, at GAK12 in Sept 2007
# therefore include only Pleuromamma, P. abdominalis, P. scutullata, P. xiphias in this category
SMetridinidae = May.s %>%
  filter(genus == "Pleuromamma") %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  group_by(Year, stationID) %>%
  summarise(SMetridinidaeSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SMetridinidae=mean(SMetridinidaeSite)) %>% 
  ungroup


# Pseudocalanus spp, I-V, AM, AF (all stages)
SPseudocalanus = May.s %>%
  filter(species == "Pseudocalanus_sp." | sciName == "Pseudocalanus_sp.") %>% 
  group_by(Year, stationID) %>%
  summarise(PseudocalanusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SPseudocalanus=mean(PseudocalanusSite)) %>% 
  ungroup


# Acartia spp., Adults
SAcartia = May.s %>%
  filter(family == "Acartiidae") %>%
  filter(stage %in% c("AM", "AF")) %>%
  group_by(Year, stationID) %>%
  summarise(AcartiaSite=sum(abundance)) %>% # find total biomass of all Acartia life stages for each GAK site
  ungroup %>%
  group_by(Year) %>%
  summarise(SAcartia=mean(AcartiaSite)) %>% # take mean Acartia biomass across all GAK sites
  ungroup



# Oithona spp, stage V, AM, AF
SOithona = May.s %>%
  filter(species %in% c("Oithona_similis", "Oithona_spinirostris", "Oithona_sp.", "similis", "spinirostris") | sciName == "Oithona_sp.") %>%
  filter(stage %in% c("V", "C5", "AM", "AF")) %>%
  group_by(Year, stationID) %>%
  summarise(OithonaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SOithona=mean(OithonaSite)) %>% 
  ungroup


# Unidentified Calanids, I-III (use stage designation of Calanus, Mesocalanus, Neocalanus) - ALL SHOULD COME FROM SMALL MESH NETS
# Unidentified Calanids, damaged I-II and undetermined stages
SCalanids = May.s %>%
  filter(family == "Calanidae" & species %in% c(NA, "N/A")) %>%  # works as desired
  group_by(Year, stationID) %>%
  summarise(CalanidsSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SCalanids=mean(CalanidsSite)) %>% 
  ungroup

# Other copepoda (damaged), Adults

# Cladocerans <5mm (153um mesh) (order)  ALL FROM SMALL MESH NET
SPodonidae = May.s %>%
  filter(family == "Podonidae") %>% 
  group_by(Year, stationID) %>%
  summarise(PodonidaeSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SPodonidae=mean(PodonidaeSite)) %>% 
  ungroup


# -----------------------

# Create dataframe with years:
SACCMaySmZoAbund=data.frame('Year'=c(1998:2010))

# Merge in the taxon-specific biomass data
SACCMaySmZoAbund <- merge(SACCMaySmZoAbund,SSmNcristatus,all.x=T)
SACCMaySmZoAbund <- merge(SACCMaySmZoAbund,SSmNpflemingeri ,all.x=T)
SACCMaySmZoAbund <- merge(SACCMaySmZoAbund,SSmEbungii,all.x=T)
SACCMaySmZoAbund <- merge(SACCMaySmZoAbund,SSmCmarshallae,all.x=T)
SACCMaySmZoAbund <- merge(SACCMaySmZoAbund,SSmCpacificus,all.x=T)
SACCMaySmZoAbund <- merge(SACCMaySmZoAbund,SSmMpacifica,all.x=T)
SACCMaySmZoAbund <- merge(SACCMaySmZoAbund,SSmMetridia,all.x=T)
SACCMaySmZoAbund <- merge(SACCMaySmZoAbund,SMetridinidae,all.x=T)
SACCMaySmZoAbund <- merge(SACCMaySmZoAbund,SPseudocalanus,all.x=T)
SACCMaySmZoAbund <- merge(SACCMaySmZoAbund,SAcartia,all.x=T)
SACCMaySmZoAbund <- merge(SACCMaySmZoAbund,SOithona,all.x=T)
SACCMaySmZoAbund <- merge(SACCMaySmZoAbund,SCalanids,all.x=T)
SACCMaySmZoAbund <- merge(SACCMaySmZoAbund,SPodonidae,all.x=T)

View(SACCMaySmZoAbund)


# ---------------------------------------------------------------------------------------------------

# For the subset of copepod species & stages quantitatively sampled at Shelikof St, create May Large Zooplankton Abundance for GAK 1-4

# Extract Seward Line (GAK) sites from Large Zoop file
MayACC = DepthInt.taxinfo %>%
  filter(stationID %in% c("GAK1", "GAK2", "GAK3", "GAK4")) %>%
  filter(Month == 5)


# Neocalanus cristatus, stages III, V, adults (no IV?)
SLgNcristatus = MayACC %>%
  filter(sciName == "Neocalanus cristatus") %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(NcristatusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SLgNcristatus=mean(NcristatusSite)) %>% 
  ungroup


# Neocalanus spp (N. flemingeri + N. plumchurus), stages IV, V, adults
SLgNpflemingeri = MayACC %>%
  filter(sciName %in% c("Neocalanus plumchrus", "Neocalanus flemingeri")) %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "CV_large", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(NpflemingeriSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SLgNpflemingeri=mean(NpflemingeriSite)) %>% 
  ungroup


# Eucalanus bungii, stages IV, V, adults
SLgEbungii = MayACC %>%
  filter(sciName == "Eucalanus bungii") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(EbungiiSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SLgEbungii=mean(EbungiiSite)) %>% 
  ungroup


# Calanus marshallae, stages IV, V, adults
SLgCmarshallae = MayACC %>%
  filter(sciName == "Calanus marshallae") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(CmarshallaeSite=sum(abundance)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(SLgCmarshallae=mean(CmarshallaeSite)) %>%
  ungroup


# Calanus pacificus, stages V, adults
SLgCpacificus = MayACC %>%
  filter(sciName == "Calanus pacificus") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(CpacificusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SLgCpacificus=mean(CpacificusSite)) %>% 
  ungroup


# Metridia pacifica/lucens, Females
SLgMpacifica = MayACC %>%
  filter(sciName == "Metridia pacifica") %>% 
  filter(stage == "AF") %>%
  group_by(Year, stationID) %>%
  summarise(MpacificaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SLgMpacifica=mean(MpacificaSite)) %>% 
  ungroup


# Metridia spp., stages V, adults
SLgMetridia = MayACC %>%
  filter(sciName == c("Metridia", "Metridia okhotensis")) %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(MetridiaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SLgMetridia=mean(MetridiaSite)) %>% 
  ungroup


# Other copepoda (damaged), Adults


# Euphausiids:

# Thysanoessa: in Seward Line dataset, this includes the 5 species listed here plus "Thysanoessa" (which looks like mostly juvenile stages not id'd to species)
# Thysanoessa raschii, Adult + Juvenile
# Thysanoessa inermis, Adult + Juvenile
# Thysanoessa spinifera, Adult + Juvenile
# Thysanoessa longipes, Adult + Juvenile
# Thysanoessa inspinata, Adult + Juvenile
SThysanoessa = MayACC %>%
  filter(genus == "Thysanoessa") %>% 
  group_by(Year, stationID) %>%
  summarise(ThysanoessaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SThysanoessa=mean(ThysanoessaSite)) %>% 
  ungroup


# Tessarabrachion oculatum, Adult + Juvenile (ITIS valid name = Tessarabrachion oculatus)
SToculatum = MayACC %>%
  filter(sciName == "Tessarabrachion oculatus") %>% 
  group_by(Year, stationID) %>%
  summarise(ToculatumSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SToculatum=mean(ToculatumSite)) %>% 
  ungroup


# Other euphausiids (damaged), Adult + Juvenile


# Euphausiid, stage = furcillia
# Euphausiid, stage = calyptopis 1-3
# Euphausiid, stage = nauplii
SEuphJuv = MayACC %>%
  filter(sciName == "Euphausiacea") %>% 
  filter(stage %in% c("furcilia", "calyptopis", "nauplii", "metanauplii")) %>% #consider also using "juvenile" and "juveniles"?
  group_by(Year, stationID) %>%
  summarise(EuphJuvSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SEuphJuv=mean(EuphJuvSite)) %>% 
  ungroup



# Cirripedia, nauplii, cyprid, medusae (<5mm)
# nb medusae are not in Seward dataset
SCirripedia = MayACC %>%
  filter(sciName == "Cirripedia") %>% 
  group_by(Year, stationID) %>%
  summarise(CirripediaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SCirripedia=mean(CirripediaSite)) %>% 
  ungroup


# Chaetnognatha, all sizes
SChaetnognatha = MayACC %>%
  filter(phylum == "Chaetnognatha") %>% 
  group_by(Year, stationID) %>%
  summarise(ChaetnognathaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SChaetnognatha=mean(ChaetnognathaSite)) %>% 
  ungroup



# Natantia, larvae, adults, juveniles 
#(decapod families that swim, ie shrimp (infraorder == c(Caridea, Procarididea)), prawns (suborder == Dendrobranchiata), boxer shrimp (infraorder == Stenopodidea))
SNatantia = MayACC %>%
  filter(suborder == "Dendrobranchiata" | infraorder =="Caridea") %>%
  group_by(Year, stationID) %>%
  summarise(NatantiaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SNatantia=mean(NatantiaSite)) %>% 
  ungroup



# Cnidarian medusae >5mm
SCnidaria = MayACC %>%
  filter(phylum == "Cnidaria") %>%
  group_by(Year, stationID) %>%
  summarise(CnidariaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SCnidaria=mean(CnidariaSite)) %>% 
  ungroup


# Ctenophora, all sizes  # not recorded at Seward Line


# Gammaridea, all sizes (order = Amphipoda)
# Hyperiidea, all sizes (order = Amphipoda)
SAmphipoda = MayACC %>%
  filter(order == "Amphipoda") %>%
  group_by(Year, stationID) %>%
  summarise(AmphipodaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SAmphipoda=mean(AmphipodaSite)) %>% 
  ungroup

# Mysidacea, all sizes
SMysidacea = MayACC %>%
  filter(order == "Mysida") %>%
  group_by(Year, stationID) %>%
  summarise(MysidaceaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SMysidacea=mean(MysidaceaSite)) %>% 
  ungroup


# Siphonophora, all sizes # already included in Cnidaria above

# Ostracoda <5mm
SOstracoda = MayACC %>%
  filter(class == "Ostracoda") %>%
  group_by(Year, stationID) %>%
  summarise(OstracodaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SOstracoda=mean(OstracodaSite)) %>% 
  ungroup


# Thecosomata <5mm (Pteropods)
SThecosomata = MayACC %>%
  filter(order == "Thecosomata") %>%
  group_by(Year, stationID) %>%
  summarise(ThecosomataSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SThecosomata=mean(ThecosomataSite)) %>% 
  ungroup


# Anomura larvae
SAnomura = MayACC %>%
  filter(infraorder == "Anomura") %>%
  filter(stage == "zoea") %>%
  group_by(Year, stationID) %>%
  summarise(AnomuraSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SAnomura=mean(AnomuraSite)) %>% 
  ungroup

# Brachyura larvae
SBrachyura = MayACC %>%
  filter(infraorder == "Brachyura") %>%
  filter(stage == "zoea") %>%
  group_by(Year, stationID) %>%
  summarise(BrachyuraSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SBrachyura=mean(BrachyuraSite)) %>% 
  ungroup


# Fish larvae
# probably not retained at Seward as much as at Shelikof?
SIchthyoplankton = MayACC %>%
  filter(subphylum == "Vertebrata") %>%
  filter(stage == "larvae") %>%
  group_by(Year, stationID) %>%
  summarise(IchthyoplanktonSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(SIchthyoplankton=mean(IchthyoplanktonSite)) %>% 
  ungroup

# -----------------------

# Create dataframe with years:
SACCMayLgZoAbund=data.frame('Year'=c(1998:2010))

# Merge in the taxon-specific biomass data
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SLgNcristatus,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SLgNpflemingeri,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SLgEbungii,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SLgCmarshallae,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SLgCpacificus,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SLgMpacifica,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SLgMetridia,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SThysanoessa,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SToculatum,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SEuphJuv,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SCirripedia,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SChaetnognatha,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SNatantia,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SCnidaria,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SAmphipoda,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SMysidacea,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SOstracoda,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SThecosomata,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SAnomura,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SBrachyura,all.x=T)
SACCMayLgZoAbund <- merge(SACCMayLgZoAbund,SIchthyoplankton,all.x=T)

View(SACCMayLgZoAbund)

# ---------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------

# Merge Small & Large Zooplankton abundance datasets (Shelikof sp subset)
SACCMayZoop <- full_join(SACCMaySmZoAbund, SACCMayLgZoAbund, by = "Year")
SACCMayZoop[is.na(SACCMayZoop)] <- 0 # replace NA with 0
str(SACCMayZoop)
names(SACCMayZoop)


# Create summary dataframe
SACCMayZoop1 <- SACCMayZoop %>%
  mutate(STotCopepods = SSmNcristatus + SLgNcristatus + SSmNpflemingeri + SLgNpflemingeri + 
           SSmEbungii + SLgEbungii + SSmCmarshallae + SLgCmarshallae + SSmCpacificus + SLgCpacificus + 
           SSmMpacifica + SLgMpacifica + SSmMetridia + SLgMetridia + SMetridinidae + 
           SPseudocalanus + SAcartia + SOithona + SCalanids) %>%
  mutate(STotEuphausiids = SThysanoessa + SToculatum + SEuphJuv) %>%
  mutate(STotOther = SCirripedia + SChaetnognatha + SNatantia + SCnidaria + SAmphipoda + 
           SMysidacea + SOstracoda + SThecosomata + SAnomura + SBrachyura + SIchthyoplankton) %>%
  select(Year, STotCopepods, SPodonidae, STotEuphausiids, STotOther)
View(SACCMayZoop1)



# Create copepod dataframe by species
#STACCMayCopepods <- SACCMayZoop %>%
#  mutate(SNcristatus = SSmNcristatus + SLgNcristatus) %>%
#  mutate(SNpflemingeriAb = SSmNpflemingeri + SLgNpflemingeri) %>%
#  mutate(SEbungii = SSmEbungii + SLgEbungii) %>%
#  mutate(SCmarshallae = SSmCmarshallae + SLgCmarshallae) %>%
#  mutate(SCpacificusAb = SSmCpacificusAb + SLgCpacificus) %>%
#  mutate(SMpacificaAb = SSmMpacificaAb + SLgMpacifica) %>%
#  mutate(SMetridia = SSmMetridia + SLgMetridia) %>%
#  select(Year, SNcristatus, SNpflemingeri, SEbungii, SCmarshallae, SCpacificus, SMpacifica, SMetridia, SMetridinidae, SPseudocalanus, SAcartia, SOithona, SCalanids)
#STACCMayCopepods = STACCMayCopepods %>%
#  mutate(SACCTotCopepods = rowSums(STACCMayZoop[,2:13]))
#View(STACCMayCopepods)



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
ACCMaySmZoAbund <- merge(ACCMaySmZoAbund,ASmPodonidae,all.x=T)

View(ACCMaySmZoAbund)


# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------

# Large zooplankton (from MOCNESS net)

# Copepod stage size / net classifications from Coyle & Pinchuk 2003:
# Aetideidae, stages V, Adults
ALgAetideidae = MayACC %>%
  filter(family == "Aetideidae") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AAetideidaeSite=sum(abundance)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(ALgAetideidae=mean(AAetideidaeSite)) %>% 
  ungroup


# Calanus marshallae, stages IV, V, adults
ALgCmarshallae = MayACC %>%
  filter(sciName == "Calanus marshallae") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(ACmarshallaeSite=sum(abundance)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(ALgCmarshallae=mean(ACmarshallaeSite)) %>%
  ungroup


# Calanus pacificus, stages V, adults
ALgCpacificus = MayACC %>%
  filter(sciName == "Calanus pacificus") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(ACpacificusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ALgCpacificus=mean(ACpacificusSite)) %>% 
  ungroup


# Candacia, stages IV, V, adults
ALgCandacia = MayACC %>%
  filter(genus == "Candacia") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(ACandaciaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ALgCandacia=mean(ACandaciaSite)) %>% 
  ungroup


# Epilabidocera amphitrites, stages IV, V, adults;  add in Apr 30 sample from GAK2 in 2002 (cruiseID == hx258)
ALgEamphitrites = MayACC %>%
  filter(species == "Epilabidocera amphitrites") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AEamphitritesSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ALgEamphitrites=mean(AEamphitritesSite)) %>% 
  ungroup


# Eucalanus bungii, stages IV, V, adults
ALgEbungii = MayACC %>%
  filter(sciName == "Eucalanus bungii") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AEbungiiSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ALgEbungii=mean(AEbungiiSite)) %>% 
  ungroup


# Euchaeta elongata, stages III-V, adults
ALgPelongata = MayACC %>%
  filter(species %in% c("Paraeuchaeta elongata", "Elongata")) %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(APelongataSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ALgPelongata=mean(APelongataSite)) %>% 
  ungroup


# Heterorhabdus spp., stages IV, V, adults
ALgHeterorhabdus = MayACC %>%
  filter(genus == "Heterorhabdus") %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AHeterorhabdusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ALgHeterorhabdus=mean(AHeterorhabdusSite)) %>% 
  ungroup


# Heterostylites spp., stages V, adults; none in May GAK samples
ALgHeterostylites = MayACC %>%
  filter(genus == "Heterostylites") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AHeterostylitesSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ALgHeterostylites=mean(AHeterostylitesSite)) %>% 
  ungroup


# Lucicutia spp., stages V, adults
ALgLucicutia = MayACC %>%
  filter(genus == "Lucicutia") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(ALucicutiaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ALgLucicutia=mean(ALucicutiaSite)) %>% 
  ungroup


# Metridia okhotensis, stages V, adults
ALgMokhotensis = MayACC %>%
  filter(sciName == "Metridia okhotensis") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AMokhotensisSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ALgMokhotensis=mean(AMokhotensisSite)) %>% 
  ungroup


# Metridia pacifica, stages Females
ALgMpacifica = MayACC %>%
  filter(sciName == "Metridia pacifica") %>% 
  filter(stage == "AF") %>%
  group_by(Year, stationID) %>%
  summarise(AMpacificaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ALgMpacifica=mean(AMpacificaSite)) %>% 
  ungroup


# Neocalanus cristatus, stages III-V, adults
ALgNcristatus = MayACC %>%
  filter(sciName == "Neocalanus cristatus") %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(ANcristatusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ALgNcristatus=mean(ANcristatusSite)) %>% 
  ungroup


# Neocalanus plumchrus-flemingeri, stages IV, V, adults
ALgNpflemingeri = MayACC %>%
  filter(sciName %in% c("Neocalanus plumchrus", "Neocalanus flemingeri")) %>% 
  filter(stage %in% c("IV", "V", "C4", "C5", "CV_large", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(ANpflemingeriSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ALgNpflemingeri=mean(ANpflemingeriSite)) %>% 
  ungroup


# Pleuromamma spp., stages V, adults 
ALgPleuromamma = MayACC %>%
  filter(genus == "Pleuromamma") %>% 
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(APleuromammaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ALgPleuromamma=mean(APleuromammaSite)) %>% 
  ungroup

# -----------------------

# Other copepod taxa present but not assigned to size / net group by Coyle & Pinchuk 2003:

# Gaussia princeps   Follow Coyle & Pinchuk's classification for congenors Pleuromamma & Metridia
# None in GAK May samples
ALgGprinceps = MayACC %>%
  filter(sciName == "Gaussia princeps") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AGprincepsSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ALgGprinceps=mean(AGprincepsSite)) %>% 
  ungroup


# Metridia sp. Follow Coyle & Pinchuk's 2003 classification for M. okhotensis
# ie take stages V, adults from large nets
ALgMetridia = MayACC %>%
  filter(sciName == "Metridia") %>%
  filter(stage %in% c("V", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(AMetridiaSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ALgMetridia=mean(AMetridiaSite)) %>% 
  ungroup


# Monstrilla sp. (infraclass Neocopepod).  Not captured in large zoop nets


# Neocalanus sp.  Follow Coyle & Pinchuk's 2003 classification for N. cristatus
# ie take III-V, adults from large nets
ALgNeocalanus = MayACC %>%
  filter(sciName == "Neocalanus") %>% 
  filter(stage %in% c("III", "IV", "V", "C3", "C4", "C5", "AF", "AM")) %>%
  group_by(Year, stationID) %>%
  summarise(ANeocalanusSite=sum(abundance)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(ALgNeocalanus=mean(ANeocalanusSite)) %>% 
  ungroup

# -----------------------

# Euphausiids, all stages
AEuphausiids = MayACC %>%
  filter(order == "Euphausiacea") %>% 
  group_by(Year, stationID) %>%
  summarise(EuphausiidsSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(AEuphausiids=mean(EuphausiidsSite)) %>% 
  ungroup
#View(AEuphausiids)

# -----------------------

# "Other" category (everything in the large net that is not a Copepod nor Euphausiid; 
# includes Mysids, Pteropods, Decapod zoea, Ichthyoplankton, Cnidaria, Amphipods, Chaetnognaths)

AOther = MayACC %>%
  filter(subclass != "Copepoda") %>% # exclude Copepods
  filter(order != "Euphausiacea") %>% # exclude Euphausiids
  #filter(!subphylum == "Vertebrata" & !stage %in% c("juvenile", "juveniles")) %>% # exclude juvenile fish (but retain fish eggs & larvae)
  group_by(Year, stationID) %>%
  summarise(OtherSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(AOther=mean(OtherSite)) %>% 
  ungroup
View(AOther)

# -----------------------

# Create dataframe with years:
ACCMayLgZoAbund=data.frame('Year'=c(1998:2010))

# Merge in the taxon-specific abundance data
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALgAetideidae,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALgCmarshallae,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALgCpacificus,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALgCandacia,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALgEamphitrites,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALgEbungii,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALgPelongata,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALgHeterorhabdus,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALgHeterostylites,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALgLucicutia,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALgMokhotensis,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALgMpacifica,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALgNcristatus,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALgNpflemingeri,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALgPleuromamma,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALgGprinceps,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALgMetridia,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,ALgNeocalanus,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,AEuphausiids,all.x=T)
ACCMayLgZoAbund <- merge(ACCMayLgZoAbund,AOther,all.x=T)

View(ACCMayLgZoAbund)


# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------


# Merge Small & Large Zooplankton abundance datasets
AACCMayZoop = full_join(ACCMaySmZoAbund, ACCMayLgZoAbund, by = "Year")
AACCMayZoop[is.na(ACCMayZoop)] <- 0 # replace NA with 0
str(AACCMayZoop)
View(AACCMayZoop)
names(AACCMayZoop)

# Calculate May Total Copepod Abundance for GAK1-4 sites
#TACCMayZoop = ACCMayZoop %>%
#  mutate(ACCTotCopepods = rowSums(ACCMayZoop[,2:47]))
#View(TACCMayZoop)


# Create summary dataframe for GAK1-4 sites
AACCMayZoop1 <- AACCMayZoop %>%
  mutate(ATotCopepods = ASmAcartia + ASmAetideidae + ASmCmarshallae + ASmCpacificus + ASmCcolumbiae + ASmCabdominalis + 
           ASmClausocalanus + ASmEamphitrites + ASmEbungii + ASmPelongata + ASmEurytemora + ASmHeterorhabdus + ASmLucicutia + 
           ASmMesocalanus + ASmMokhotensis + ASmMpacifica + ASmMicrocalanus + ASmNcristatus + ASmNpflemingeri + ASmOithona + 
           ASmOncaea + ASmParacalanus + ASmPseudocalanus + ASmRantacrticus + ASmScolecithricella + ASmMetridia + ASmMicrosetella + 
           ASmNeocalanus + ALgAetideidae + ALgCmarshallae + ALgCpacificus + ALgCandacia + ALgEamphitrites + ALgEbungii + ALgPelongata + 
           ALgHeterorhabdus + ALgHeterostylites + ALgLucicutia + ALgMokhotensis + ALgMpacifica + ALgNcristatus + ALgNpflemingeri + 
           ALgPleuromamma + ALgGprinceps + ALgMetridia + ALgNeocalanus) %>%
  rename(APodonidae = ASmPodonidae, ATotEuphausiids = AEuphausiids, ATotOther = AOther) %>%
  select(Year, ATotCopepods, APodonidae, ATotEuphausiids, ATotOther)
View(AACCMayZoop1)


# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------

# Using abundance data from Seward Line (where all zooplankton species were sampled),
# Is the subset of copepod species sampled at Shelikof Strait representative of total copepod abundance?

totals = full_join(SACCMayZoop1, AACCMayZoop1, by = "Year")#S prefix indicates subset sampled at Line 8; A prefix indicicates total zoop sampled at Seward Line
View(totals)
names(totals)

# Plot time series
ggplot(data=totals, aes(x=Year, y = value, color = variable)) + 
  geom_point(aes(y = ATotCopepods, col = "Total Copepod Abundance")) +
  geom_point(aes(y = STotCopepods, col = "Subset Copepod Abundance")) +
  geom_line(aes(y = ATotCopepods, col = "Total Copepod Abundance")) +
  geom_line(aes(y = STotCopepods, col = "Subset Copepod Abundance")) +
  theme_bw() +
  scale_y_log10() +
  coord_cartesian(xlim = c(1996, 2012)) +
  ylab("Abundance (inidv / m3)") +
  xlab("Year") +
  theme(legend.title=element_blank()) # turns off legend title
#ggsave("CopepodTimeSeries.png", width = 8, height = 5)

# Euphausiids
ggplot(data=totals, aes(x=Year, y = value, color = variable)) + 
  geom_point(aes(y = ATotEuphausiids, col = "Total Euphausiid Abundance")) +
  geom_point(aes(y = STotEuphausiids, col = "Subset Euphausiid Abundance")) +
  geom_line(aes(y = ATotEuphausiids, col = "Total Euphausiid Abundance")) +
  geom_line(aes(y = STotEuphausiids, col = "Subset Euphausiid Abundance")) +
  theme_bw() +
  scale_y_log10() +
  coord_cartesian(xlim = c(1996, 2012)) +
  ylab("Abundance (inidv / m3)") +
  xlab("Year") +
  theme(legend.title=element_blank()) # turns off legend title
#ggsave("EuphausiidTimeSeries.png", width = 8, height = 5)


# Other Stuff
ggplot(data=totals, aes(x=Year, y = value, color = variable)) + 
  geom_point(aes(y = ATotOther, col = "Total Other Abundance")) +
  geom_point(aes(y = STotOther, col = "Subset Other Abundance")) +
  geom_line(aes(y = ATotOther, col = "Total Other Abundance")) +
  geom_line(aes(y = STotOther, col = "Subset Other Abundance")) +
  theme_bw() +
  scale_y_log10() +
  coord_cartesian(xlim = c(1996, 2012)) +
  ylab("Abundance (inidv / m3)") +
  xlab("Year") +
  theme(legend.title=element_blank()) # turns off legend title


# Total Zooplankton
ggplot(data=totals, aes(x=Year, y = value, color = variable)) + 
  geom_point(aes(y = ATotCopepods + APodonidae + ATotEuphausiids + ATotOther, col = "Total Zooplankton Abundance")) +
  geom_point(aes(y = STotCopepods + SPodonidae + STotEuphausiids + STotOther, col = "Subset Zooplankton Abundance")) +
  geom_line(aes(y = ATotCopepods + APodonidae + ATotEuphausiids + ATotOther, col = "Total Zooplankton Abundance")) +
  geom_line(aes(y = STotCopepods + SPodonidae + STotEuphausiids + STotOther, col = "Subset Zooplankton Abundance")) +
  theme_bw() +
  scale_y_log10() +
  coord_cartesian(xlim = c(1996, 2012)) +
  ylab("Abundance (inidv / m3)") +
  xlab("Year") +
  theme(legend.title=element_blank()) # turns off legend title


# Copepods: Are the subset and totals correlated?
cor.test(log(totals$ATotCopepods), log(totals$STotCopepods))
#Pearson's product-moment correlation
#data:  log(totals$ATotCopepods) and log(totals$STotCopepods)
#t = 17.323, df = 11, p-value = 2.481e-09
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#0.9397033 0.9948029
#sample estimates:
#cor 
#0.9821618 


# Euphausiids: Are the subset and totals correlated?
cor.test(log(totals$ATotEuphausiids), log(totals$STotEuphausiids))
#Pearson's product-moment correlation
#data:  log(totals$ATotEuphausiids) and log(totals$STotEuphausiids)
#t = 2.3889, df = 11, p-value = 0.03593
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#0.0493697 0.8588644
#sample estimates:
#cor 
#0.5844566


# Plot correlation
ggplot(data=totals) + 
  geom_point(aes(x = ATotCopepods, y = STotCopepods, size = 6)) +
  theme_bw() +
  scale_y_log10() +
  scale_x_log10() +
  #coord_cartesian(ylim = c(100, 2200)) +
  ylab("Subset Copepod Abundance (inidv / m3)") +
  xlab("Total Copepod Abundance (indiv / m3)") 
#ggsave("CopepodCorrelation.png", width = 6, height = 5)
