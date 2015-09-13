#######################################################################
#####  Seward Line Small Zooplankton (<0.3mg) Processing Script  ######
#####     Script by Colette Ward (ward@nceas.ucsb.edu)           ######
#####                      August 2015                           ######
#######################################################################

## load packages
library(dplyr)
library(httr)
library(ggplot2)

# A. Load and merge datasets

# A.i  Load CalVET net (meshSize = 149 um), 1998-2011:
#URL_SCZo <- "http://gulfwatch.nceas.ucsb.edu/goa/d1/mn/v1/object/df35b.56.3"
#SCZoGet <- GET(URL_SCZo)
#SCZo1 <- content(SCZoGet, as='text')
#SCZo <- read.csv(file=textConnection(SCZo1),stringsAsFactors=F, strip.white=TRUE)
#head(SCZo)


# problem with dates in online file (compared with https://www.sfos.uaf.edu/sewardline/results/DataByCruise2.html)
# Temporarily use locally-stored file with corrected dates until changes are verified by Russ Hopcroft
# In the following file I (Colette) have corrected sampling dates for cruise TXF06, station MS2: changed date from June 15 2009 to Sept 15 2006
# and renamed variable "inraforder" to "infraorder"
setwd("~/Google Drive/GoA project/Data/Datasets/Data Packages/AOOS NCEAS Packages/Seward Line Zooplankton 97-11")
SCZo.a=read.csv('df35b.56.4-ltopZooplanktonData_CW corrected TXF06 date.csv', header=T, stringsAsFactors=F, strip.white=TRUE)
head(SCZo.a)


# Pull 2012 data from AOOS Ocean Workspace (requires log-in here: https://workspace.aoos.org/login).
# Files online are .xls, so I'll use .csv saved locally on my machine (can I upload these to our server?)
# Load CalVET net (meshSize = 149 um), 2010-2012:
setwd("~/Google Drive/GoA project/Data/Datasets/Data Packages/AOOS Ocean Workspace Packages/Seward Line Zooplankton 2010-12")
SCZo.b=read.csv('Seward_Zoodata_Calvet_2010_2012_update.csv', header=T, stringsAsFactors=F, strip.white=TRUE)
head(SCZo.b)


# 1997-2011 dataset: extract year, month, day, time:
SCZo.a1=SCZo.a %>%
  mutate(time=strsplit(as.character(dateTime),split=" ") %>%
           sapply(function(x) x[2])) %>%
  #mutate(Time=as.numeric(time)) %>%  #creates a vector of NA, probably due to ":"
  mutate(date=strsplit(as.character(dateTime),split=" ") %>%
           sapply(function(x) x[1])) %>%
  mutate(Date=as.Date(date, "%m/%d/%y")) %>%   # NB output is reordered as yyyy-mm-dd
  mutate(year=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[1])) %>%
  mutate(Year=as.numeric(year)) %>%
  mutate(month=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[2])) %>%
  mutate(Month=as.numeric(month)) %>%
  mutate(day=strsplit(as.character(Date),split="-") %>%
           sapply(function(x) x[3])) %>%
  mutate(Day=as.numeric(day)) %>%
  #gsub("_"," ",species) %>%  #Error in gsub(., "_", " ", species) : object 'species' not found  # tried to replace "_" with " " in species names
  select(-dateTime, -date, -Date, -year, -month, -day, -ship, -notes, -consecStationNum)
View(SCZo.a1)
head(SCZo.a1)


# Convert variable names in 2010-2012 dataset to those used in 1998-2011 dataset
# and extract samples from 2012
SCZo.b1 = SCZo.b %>%
  mutate(cruiseID=Cruise) %>%
  mutate(time=Time) %>%
  mutate(stationID=Station) %>%
  mutate(towDepth=Tow.Depth..m.) %>%
  mutate(sonicDepth=Sonic.Depth..m.) %>%
  mutate(lat=Latitude..N.) %>%
  mutate(lon=Longitude..W.) %>%
  mutate(gear=Gear.Type) %>%
  mutate(ringDiam=Ring.Diameter..cm.) %>%
  mutate(meshSize=Mesh.Size.um) %>%
  mutate(towType=Tow.Type) %>%
  mutate(sampleVol=Sample.Volume..m3.) %>%
  mutate(sciName=Current.Name) %>%
  mutate(stage=Life.Stage) %>%
  mutate(abundance=Abundance..no.m.3.) %>%
  mutate(biomass=Biomass..g.m.3.) %>%
  mutate(kingdom=Kingdom) %>%
  mutate(phylum=Phylum) %>%
  mutate(subphylum=Subphylum) %>%
  mutate(class=Class) %>%
  mutate(subclass=Subclass) %>%
  mutate(infraclass=Infraclass) %>%
  mutate(order=Order) %>%
  mutate(suborder=Suborder) %>%
  mutate(infraorder=Infraorder) %>%
  mutate(family=Family) %>%
  mutate(genus=Genus) %>%
  mutate(species=Species.Name) %>%
  filter(Year==2012) %>%
  select(-Cruise, -Date.Time, -Time, -Station, -Tow.Depth..m., -Sonic.Depth..m., -Latitude..N., -Longitude..W., 
         -Gear.Type, -Ring.Diameter..cm., -Mesh.Size.um, -Tow.Type, -Sample.Volume..m3., -Current.Name, -Life.Stage, 
         -Abundance..no.m.3., -Biomass..g.m.3., -Kingdom, -Phylum, -Subphylum, -Class, -Subclass, -Infraclass, -Order, 
         -Suborder, -Infraorder, -Family, -Genus, -Species.Name)
head(SCZo.b1)
dim(SCZo.b1) # len should be 2620


# Bind together 1998-2011 and 2012 datasets:
SCZo1 = bind_rows(SCZo.a1, SCZo.b1)
head(SCZo1)
dim(SCZo.a1) # 34923    31
dim(SCZo.b1) # 2620   31
dim(SCZo1) # combined dataset should have dim 37543 31


# --------------------------------------------------------------------------------------------------------------

# B. Look at data for GAK sites on Seward Line

# extract GAK sites for CalVET net:
SCZo1GAK = SCZo1 %>%
  filter(stationID %in% c("GAK1", "GAK2", "GAK3", "GAK4", "GAK5", "GAK6", "GAK7", "GAK8", "GAK9", "GAK10", "GAK11", "GAK12", "GAK13"))  # select only GAK sites
head(SCZo1GAK)


# Some sampling visualizations

# Site depths across Seward Line:
SCZo1GAK$Site <- factor(SCZo1GAK$stationID, levels=c("GAK1", "GAK2", "GAK3", "GAK4", "GAK5", "GAK6", "GAK7", "GAK8", "GAK9", "GAK10", "GAK11", "GAK12", "GAK13"))
ggplot(data=SCZo1GAK, aes(y=sonicDepth, x=Site)) +
  geom_point() + theme_bw() + scale_y_log10() + ylab("Bottom Depth (m)")

# Temporal distribution of sampling
# also check whether there are problems with month & day being mixed up (compare against https://www.sfos.uaf.edu/sewardline/results/DataByCruise2.html:
plot(SCZo1$Year ~ SCZo1$Month, pch=16)


# --------------------------------------------------------------------------------------------------------------

# C. Create May small zooplankton biomass

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

# Small zooplankton:
# Hopcroft 2014 conference talk: small zooplankton do not show spatial structure across the GAK line
# therefore use all GAK sites for small zoop

# NB copepod stages I, II, III, etc are the same as stages C1, C2, C3, etc ... (shift in nomenclature in the database in 2005)
# my assumption: when stage is blank ("NA") assume the individuals are adults.  Check this with Russ Hopcroft. (*be careful with this when doing adult data for Large Zoop dataset*)

# ***NB Coyle & Pinchuk 2003 do not include nauplii in their size classification ... I need to add them here ***

unique(sort(SCZo1GAK$species))
[1] ""                             "abdominalis"                  "Acartia_longiremis"           "Acartia_sp."                 
[5] "Acartia_tumida"               "Aegina_rosea"                 "Aequorea_sp."                 "Aetideus_pacificus"          
[9] "Aglantha_digitale"            "Ammodytes_hexapterus"         "amphitrites"                  "antarcticus"                 
[13] "arctica"                      "Autolytus_sp."                "Bradyidius_saanichi"          "bungii"                      
[17] "Calanus_marshallae"           "Calanus_pacificus"            "Calocalanus_styliremis"       "Calycopsis_nematophora"      
[21] "Candacia_bipinnata"           "Candacia_columbiae"           "Centropages_abdominalis"      "challengeri"                 
[25] "Chionoecetes_sp."             "Clausocalanus_lividus"        "Clausocalanus_parapergens"    "Clausocalanus_sp."           
[29] "Clio_sp."                     "Clione_limacina"              "columbiae"                    "Conchoecia_sp."              
[33] "Coryne_principes"             "cristatus"                    "Cyphocaris_challengeri"       "digitale"                    
[37] "Dimophyes_arctica"            "dispar"                       "Eirene_indicans"              "elegans"                     
[41] "elongata"                     "Epilabidocera_amphitrites"    "Eucalanus_bungii"             "Eukrohnia_hamata"            
[45] "Euphausia_pacifica"           "Euphysa_flammea"              "Evadne_sp."                   "flavicirrata"                
[49] "flemingeri"                   "Fritillaria_sp."              "Gadus_macrocephalus"          "hamata"                      
[53] "helicina"                     "Heterorhabdus_sp."            "Heterorhabdus_tanneri"        "Hyas_sp."                    
[57] "Hyperia_sp."                  "Hyperoche_medusarum"          "indicans"                     "inermis"                     
[61] "inspinata"                    "Leuroglossus_stilbius"        "libellula"                    "limacina"                    
[65] "Limacina_helicina"            "longipes"                     "longiremis"                   "longissima"                  
[69] "Lucicutia_flavicornis"        "Lucicutia_sp."                "macropa"                      "marshallae"                  
[73] "medusarum"                    "Mesocalanus_sp."              "Mesocalanus_tenuicornis"      "Metridia_okhotensis"         
[77] "Metridia_pacifica"            "Metridia_sp."                 "Microcalanus_sp."             "Microsetella_sp."            
[81] "minor"                        "Monstrilla_sp."               "muelleri"                     "N/A"                         
[85] "nd_superciliaris"             "Neocalanus_cristatus"         "Neocalanus_flemingeri"        "Neocalanus_plumchrus"        
[89] "Neocalanus_sp."               "Obelia_longissima"            "octocostatum"                 "Oikopleura_sp."              
[93] "Oithona_similis"              "Oithona_sp."                  "Oithona_spinirostris"         "Oncaea_sp."                  
[97] "pacifica"                     "pacificus"                    "Pandalopsis_dispar"           "Pandalus_platyceros"         
[101] "Paracalanus_parvus"           "Paracalanus_sp."              "Paraeuchaeta_elongata"        "Paralithodes_camtschaticus"  
[105] "Parasagitta_elegans"          "parvus"                       "Pasiphaea_pacifica"           "Phronima_sp."                
[109] "platyceros"                   "plumchrus"                    "Podon_sp."                    "Primno_macropa"              
[113] "principes"                    "Proboscidactyla_flavicirrata" "Pseudoamallothrix_ovata"      "Pseudoamallothrix_sp."       
[117] "Pseudocalanus_sp."            "Pseudosagitta_scrippsae"      "Racovitzanus_antarcticus"     "rosea"                       
[121] "Salpa_sp."                    "Scina_borealis"               "Scolecithricella_minor"       "Scolecithricella_sp."        
[125] "scrippsae"                    "similis"                      "Solmissus_sp."                "spinifera"                   
[129] "spinirostris"                 "superciliaris"                "tanneri"                      "tenuicornis"                 
[133] "Tessarabrachion_oculatus"     "Themisto_libellula"           "Themisto_pacifica"            "Themisto_sp."                
[137] "Theragra_chalcogramma"        "Thysanoessa_inermis"          "Thysanoessa_inspinata"        "Thysanoessa_longipes"        
[141] "Thysanoessa_raschii"          "Thysanoessa_sp."              "Thysanoessa_spinifera"        "Tomopteris_sp."              
[145] "Tortanus_discaudatus"         "tumida"                       "Typhloscolex_muelleri"       



# Create dataframe with years:
MaySmallZoop=data.frame('Year'=c(1998:2012))

# 1. Copepod size / net classifications from Coyle & Pinchuk 2003:
# Acartia, all life stages
Acartia = SCZo1GAK %>%
  filter(family == "Acartiidae") %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(AcartiaSite=sum(biomass)) %>% # find total biomass of all Acartia life stages for each GAK site
  ungroup %>%
  group_by(Year) %>%
  summarise(Acartia=mean(AcartiaSite)) %>% # take mean Acartia biomass across all GAK sites
  ungroup
#View(Acartia)
MaySmallZoop <- merge(MaySmallZoop,Acartia,all.x=T)

# Aetideidae, stages I-IV
Aetideidae = SCZo1GAK %>%
  filter(family == "Aetideidae") %>%
  filter(stage %in% c("I", "II", "III", "IV", "C1", "C2", "C3", "C4")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(AetideidaeSite=sum(biomass)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(Aetideidae=mean(AetideidaeSite)) %>% 
  ungroup
#View(Aetideidae) # NB there are none in GAK samples

# Calanus marshallae, stages I - III
Cmarshallae = SCZo1GAK %>%
  filter(species %in% c("Calanus_marshallae", "marshallae")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(CmarshallaeSite=sum(biomass)) %>%
  ungroup %>%
  group_by(Year) %>%
  summarise(Cmarshallae=mean(CmarshallaeSite)) %>%
  ungroup
#View(Cmarshallae)
MaySmallZoop <- merge(MaySmallZoop,Cmarshallae,all.x=T)

# Calanus pacificus, stages I - IV
Cpacificus = SCZo1GAK %>%
  filter(species %in% c("Calanus_pacificus", "pacificus")) %>%  # does not include Aetideus pacificus
  filter(stage %in% c("I", "II", "III", "IV", "C1", "C2", "C3", "C4")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(CpacificusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Cpacificus=mean(CpacificusSite)) %>% 
  ungroup
#View(Cpacificus)
MaySmallZoop <- merge(MaySmallZoop,Cpacificus,all.x=T)

# Candacia columbiae, stages I - III
Ccolumbiae = SCZo1GAK %>%
  filter(species %in% c("Candacia_columbiae", "columbiae")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(CcolumbiaeSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Ccolumbiae=mean(CcolumbiaeSite)) %>% 
  ungroup
#View(Ccolumbiae) # none at GAK sites in May

# Centropages abdominalis, all stages
Cabdominalis = SCZo1GAK %>%
  filter(species %in% c("Centropages_abdominalis", "abdominalis")) %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(CabdominalisSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Cabdominalis=mean(CabdominalisSite)) %>% 
  ungroup
#View(Cabdominalis)
MaySmallZoop <- merge(MaySmallZoop,Cabdominalis,all.x=T)

# Clausocalanus spp., all stages
Clausocalanus = SCZo1GAK %>%
  filter(species %in% c("Clausocalanus_sp.", "Clausocalanus_parapergens", "Clausocalanus_lividus") | genus == "Clausocalanus") %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(ClausocalanusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Clausocalanus=mean(ClausocalanusSite)) %>% 
  ungroup
#View(Clausocalanus)
MaySmallZoop <- merge(MaySmallZoop,Clausocalanus,all.x=T)

# Epilabidocera amphitrites, stages I - III; NB none in May GAK samples - but add in Apr 30 sample from GAK2 in 2002 (cruiseID == hx258)
Eamphitrites = SCZo1GAK %>%
  filter(species %in% c("Epilabidocera_amphitrites", "amphitrites")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(EamphitritesSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Eamphitrites=mean(EamphitritesSite)) %>% 
  ungroup
#View(Eamphitrites) 


# Eucalanus bungii, stages I - III
Ebungii = SCZo1GAK %>%
  filter(species %in% c("Eucalanus_bungii", "bungii")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(EbungiiSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Ebungii=mean(EbungiiSite)) %>% 
  ungroup
#View(Ebungii)
MaySmallZoop <- merge(MaySmallZoop,Ebungii,all.x=T)

# Euchaeta elongata, stages I - II # renamed to Paraeuchaeta elongata?
Pelongata = SCZo1GAK %>%
  filter(species %in% c("Paraeuchaeta_elongata", "elongata")) %>% 
  filter(stage %in% c("I", "II", "C1", "C2")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(PelongataSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Pelongata=mean(PelongataSite)) %>% 
  ungroup
#View(Pelongata)
MaySmallZoop <- merge(MaySmallZoop,Pelongata,all.x=T)

# Eurytemora, all life stages; NB doesn't appear at GAK sites
Eurytemora = SCZo1GAK %>%
  filter(species == "Eurytemora_sp.") %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(EurytemoraSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Eurytemora=mean(EurytemoraSite)) %>%
  ungroup
#View(Eurytemora)

# Heterorhabdus spp., stages I - III  NB none in May GAK samples
Heterorhabdus = SCZo1GAK %>%
  filter(species %in% c("Heterorhabdus_sp.", "Heterorhabdus_tanneri", "tanneri")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(HeterorhabdusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Heterorhabdus=mean(HeterorhabdusSite)) %>% 
  ungroup
#View(Heterorhabdus)

# Heterostylites spp., stages I - IV # not in CalVET nets at all?


# Lucicutia spp., stages I - IV; none in May GAK samples
Lucicutia = SCZo1GAK %>%
  filter(species %in% c("Lucicutia_sp.", "Lucicutia_flavicornis")) %>% 
  filter(stage %in% c("I", "II", "III", "IV", "C1", "C2", "C3", "C4")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(LucicutiaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Lucicutia=mean(LucicutiaSite)) %>% 
  ungroup
#View(Lucicutia)

# Mesocalanus tenuicornis, all stages (include Mesocalanus sp. here as well, there are only 5 entries, all in 1998)
Mesocalanus = SCZo1GAK %>%
  filter(species %in% c("Mesocalanus_tenuicornis", "Mesocalanus_sp.", "tenuicornis")) %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(MesocalanusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Mesocalanus=mean(MesocalanusSite)) %>% 
  ungroup
#View(Mesocalanus)
MaySmallZoop <- merge(MaySmallZoop,Mesocalanus,all.x=T)

# Metridia okhotensis, stages I - IV
Mokhotensis = SCZo1GAK %>%
  filter(species %in% c("Metridia_okhotensis", "okhotensis")) %>% 
  filter(stage %in% c("I", "II", "III", "IV", "C1", "C2", "C3", "C4")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(MokhotensisSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Mokhotensis=mean(MokhotensisSite)) %>% 
  ungroup
#View(Mokhotensis)
MaySmallZoop <- merge(MaySmallZoop,Mokhotensis,all.x=T)

# Metridia pacifica, stages I - V & Males
Mpacifica = SCZo1GAK %>%
  filter(species == "Metridia_pacifica" | sciName =="Metridia pacifica") %>% 
  filter(stage %in% c("I", "II", "III", "IV", "V", "C1", "C2", "C3", "C4", "C5", "AM", "Male", "males")) %>%  # don't need to look at NAs, there are none for M pacifica
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(MpacificaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Mpacifica=mean(MpacificaSite)) %>% 
  ungroup
#View(Mpacifica)
MaySmallZoop <- merge(MaySmallZoop,Mpacifica,all.x=T)

# Microcalanus spp., all stages
Microcalanus = SCZo1GAK %>%
  filter(species == "Microcalanus_sp." | sciName == "Microcalanus_sp.") %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(MicrocalanusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Microcalanus=mean(MicrocalanusSite)) %>% 
  ungroup
#View(Microcalanus)
MaySmallZoop <- merge(MaySmallZoop,Microcalanus,all.x=T)

# Neocalanus cristatus, stages I - II
Ncristatus = SCZo1GAK %>%
  filter(species %in% c("Neocalanus_cristatus", "cristatus")) %>% 
  filter(stage %in% c("I", "II", "C1", "C2")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(NcristatusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Ncristatus=mean(NcristatusSite)) %>% 
  ungroup
#View(Ncristatus)
MaySmallZoop <- merge(MaySmallZoop,Ncristatus,all.x=T)

# Neocalanus plumchrus-flemingeri, stages I - III # NB none in May GAK samples
Npflemingeri = SCZo1GAK %>%
  filter(species %in% c("Neocalanus_plumchrus", "Neocalanus_flemingeri", "plumchrus", "flemingeri")) %>% 
  filter(stage %in% c("I", "II", "III", "C1", "C2", "C3")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(NpflemingeriSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Npflemingeri=mean(NpflemingeriSite)) %>% 
  ungroup
#View(Npflemingeri)

# Oithona spp., all stages
Oithona = SCZo1GAK %>%
  filter(species %in% c("Oithona_similis", "Oithona_spinirostris", "Oithona_sp.", "similis", "spinirostris") | sciName == "Oithona_sp.") %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(OithonaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Oithona=mean(OithonaSite)) %>% 
  ungroup
#View(Oithona)
MaySmallZoop <- merge(MaySmallZoop,Oithona,all.x=T)

# Oncaea spp., all stages
Oncaea = SCZo1GAK %>%
  filter(species == "Oncaea_sp." | sciName == "Oncaea_sp.") %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(OncaeaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Oncaea=mean(OncaeaSite)) %>% 
  ungroup
#View(Oncaea)
MaySmallZoop <- merge(MaySmallZoop,Oncaea,all.x=T)

# Paracalanus spp., all stages; NB none in May GAK samples
Paracalanus = SCZo1GAK %>%
  filter(species %in% c("Paracalanus_parvus", "Paracalanus_sp.", "parvus") | sciName == "Paracalanus_sp.") %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(ParacalanusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Paracalanus=mean(ParacalanusSite)) %>% 
  ungroup
#View(Paracalanus)

# Pleuromamma spp., stgbes I-IV (family Metridinidae, genus Pleuromamma)  NB None in this dataset ... ?

# Pseudocalanus spp., all stages
Pseudocalanus = SCZo1GAK %>%
  filter(species == "Pseudocalanus_sp." | sciName == "Pseudocalanus_sp.") %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(PseudocalanusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Pseudocalanus=mean(PseudocalanusSite)) %>% 
  ungroup
#View(Pseudocalanus)
MaySmallZoop <- merge(MaySmallZoop,Pseudocalanus,all.x=T)

# Racovitzanus antarcticus, all stages
Rantacrticus = SCZo1GAK %>%
  filter(species %in% c("Racovitzanus_antarcticus", "antarcticus")) %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(RantacrticusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Rantacrticus=mean(RantacrticusSite)) %>% 
  ungroup
#View(Rantacrticus)
MaySmallZoop <- merge(MaySmallZoop,Rantacrticus,all.x=T)

# Scolecithricella spp., all stages
Scolecithricella = SCZo1GAK %>%
  filter(species %in% c("Scolecithricella_minor", "Scolecithricella_sp.", "Pseudoamallothrix_ovata", "Pseudoamallothrix_sp.", "minor") | sciName == "Scolecithricella_sp.") %>%  # "ovata" not in species column
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(ScolecithricellaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Scolecithricella=mean(ScolecithricellaSite)) %>% 
  ungroup
#View(Scolecithricella)
MaySmallZoop <- merge(MaySmallZoop,Scolecithricella,all.x=T)

# Tortanus discaudata, all stages; NB none in May GAK samples (only appeared once in sampling record)
Tdiscaudata = SCZo1GAK %>%
  filter(species %in% c("Tortanus_discaudatus", "discaudatus")) %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(TdiscaudataSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Tdiscaudata=mean(TdiscaudataSite)) %>% 
  ungroup
#View(Tdiscaudata)

# -----------------------

# 2. Other copepod taxa present in CalVET nets but not assigned to size / net group by Coyle & Pinchuk 2003:

# Metridia sp. Follow Coyle & Pinchuk's 2003 classification for M. okhotensis
Metridia = SCZo1GAK %>%
  filter(species == "Metridia_sp." | sciName %in% c("Metridia sp.", "Metridia spp.")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(MetridiaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Metridia=mean(MetridiaSite)) %>% 
  ungroup
#View(Metridia)
MaySmallZoop <- merge(MaySmallZoop,Metridia,all.x=T)

# Microsetella sp. only found in 2004.  Adult females are 0.02mg. Therefore take all stages from CalVET nets
Microsetella = SCZo1GAK %>%
  filter(species == "Microsetella_sp." | sciName == "Microsetella sp.") %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(MicrosetellaSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Microsetella=mean(MicrosetellaSite)) %>% 
  ungroup
#View(Microsetella)
MaySmallZoop <- merge(MaySmallZoop,Microsetella,all.x=T)

# Monstrilla sp. (infraclass Neocopepod).  Not sure, and only approx 20 entries across entire dataset, therefore leave this for now
#Parasitic on marine benthic inverts (polychaetes, gastropods); only the first nauplius & adult stages are free-swimming (adults don't feed)

# Neocalanus sp.  Follow Coyle & Pinchuk's 2003 classification for N. cristatus; NB none of these stages are in May GAK samples
Neocalanus = SCZo1GAK %>%
  filter(species == "Neocalanus_sp." | sciName == "Neocalanus_sp.") %>% 
  filter(stage %in% c("I", "II", "C1", "C2")) %>%
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(NeocalanusSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Neocalanus=mean(NeocalanusSite)) %>% 
  ungroup
#View(Neocalanus)


# -----------------------

# 3. Other non-copepod crustacean zooplankton present in CalVET nets

# class Branchiopoda, family Podonidae (Cladocerans: Evadne sp.  & Podon sp.)  
# Judging from length comparisons with Neocalanus sp (these are much smaller in length) on Seward Line website, all stages of Branchiopods should come from CalVET
# NB None in May GAK samples
Podonidae = SCZo1GAK %>%
  filter(family == "Podonidae") %>% 
  filter(Month == 5) %>%
  group_by(Year, stationID) %>%
  summarise(PodonidaeSite=sum(biomass)) %>% 
  ungroup %>%
  group_by(Year) %>%
  summarise(Podonidae=mean(PodonidaeSite)) %>% 
  ungroup
#View(Podonidae)

# Add Gastropoda?

# --------------------------------------------------------------------------------------------------------------

# Total biomass of small zooplankton in May at GAK sites:
View(MaySmallZoop)
MaySmallZoop$tot <- rowSums(MaySmallZoop[,2:20], na.rm=T)
# check sum of first row; should be  0.07037575   0.070375753
0.0005089881 + 0.0013066137 + 0.002122075 + 0.000822537 + 0.0006138843 + 0.0004410340 +0.0009865588 + 0.0020116515 + 0.0091233778 +0.0002815145 + 0.04214510 + 0.0011274123 + 0.0088338052 + 0.0000512000
View(MaySmallZoop)

ggplot(data=MaySmallZoop, aes(y=tot, x=Year)) +
  geom_point(size=4) + geom_line() + theme_bw() + scale_y_log10() + ylab("May Small Zooplankton Total Biomass (g WW / m3)") + xlab("Year")

# trend does not reflect abundance trends posted on Seward Line website.  Make sure data have not gone missing from the dataset post-2004-ish
