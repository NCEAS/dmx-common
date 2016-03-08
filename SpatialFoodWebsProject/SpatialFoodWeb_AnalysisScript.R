# Spatial Food Webs Project Analysis Script
# Colette Ward, March 2016
################################################

# call output from Small Mesh Trawl cleaning script (need SMT dataframe)
# View(SMT)

# Calculate CUE (kg/km2) for each haul:
SMT1 <- SMT %>%
  mutate(effort = distance*0.0098) %>% # create effort column (area swept (km2); see notes below)
  select(-catchNum) %>%
  mutate(cue = catchKg/effort)
head(SMT1)
# check: cruise 152, haul 1, CUE should be:
2.7/0.01451968 # 185.9545. Yes.

# Effort
# Standardized to area towed (vs temporal duration) because sampling protocol aims to trawl over a standard distance, and temporal duration can reflect things that affect tow speed
# Sampling protocol is to tow at 3.7 km / hr for a distance of 1.85 km (Jackson 2003, ADFG Small mesh trawl protocol), ie 30 min
# Net opening is 9.8m wide, 4.0m high, 3.1cm (1.2 inch) stretched mesh (Jackson 2003, ADFG Small mesh trawl protocol)
# Therefore area trawled = 1.85km (or whatever distance is recorded in distance column) x 0.0098km 
# 1.85km x 0.0098km = 0.01813 km2


### Which depths to use?  
# Anderson & Piatt 1996 used depths >55m
# Litzow 2006 used all depths
# See plots of percent catch by depth for each year in CUEbyDepth script
# From this, use samples collected at ...



# Calculate annual mean CUEs (kg/km2) for July - Oct samples, for each taxa at each site
# group by site name (note that some bays do not have site names, but I'm not using those sites)
SMT2 <- SMT1 %>%
  filter(month %in% c(7,8,9,10)) %>% # select July - Oct
  #filter(bottom_depth.m.>= | bottom_depth.m.<= ) %>% # select depth range (see notes above)
  group_by(site, year, Sample, family, sciName) %>%
  summarise(cueByHaul=sum(cue)) %>% # sum cue for all entries of each species collected in each haul (sometimes there are multiple values for a given taxa)
  ungroup %>%
  group_by(site, year, family, sciName) %>%
  summarise(annCue = mean(cueByHaul, na.rm=T)) %>% # calculate mean annual CUEs of non-zero catches across hauls
  ungroup
head(SMT2)
unique(sort(SMT2$sciName))


####################################################
# To do: review table of higher-order taxonomic info
# also: can I substitue all NAs in annCue with 0?
####################################################


# create taxonomic grouping categories for Forage Fish, Pandalid Shrimp, and Non Pelagic Shrimp
for(i in 1:nrow(SMT2)) {
  if(SMT2$sciName[i] %in% c("Thaleichthys pacificus", "Mallotus villosus", "Ammodytes hexapterus", 
                            "Trichodon trichodon", "Clupea pallasii", "Microgadus proximus")) {SMT2$taxaGroup[i] <- "ForageFish"}
  
  else{if(SMT2$family[i] %in% c("Pandalidae")) {SMT2$taxaGroup[i] <- "Pandalids"} # (from list at http://www.afsc.noaa.gov/REFM/REEM/DietData/PreyCatTree.htm )
    
    else{if(SMT2$family[i] %in% c("Hippolytidae", "Crangonidae", "Pasiphaeidae")) {SMT2$taxaGroup[i] <- "NonPelagicShrimp"} # (from list at http://www.afsc.noaa.gov/REFM/REEM/DietData/PreyCatTree.htm )
      
      else{SMT2$taxaGroup[i] <- NA}
      
}}} 
#View(SMT2)


# for each site, sum the [annual means of non-zero cues of each species] for each taxonomic group created just above
SMTgroupsCUE <- SMT2 %>%
  filter(!is.na(taxaGroup)) %>%
  group_by(site, year, taxaGroup) %>%
  summarise(groupCUE = sum(annCue)) %>%
  ungroup()
#View(SMTgroupsCUE)