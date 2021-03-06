# ADFG & NOAA Small Mesh Trawl Processing Script
# Colette Ward, Oct 2015
################################################


# call output from Small Mesh Trawl cleaning script
sourceDir <- function(path, trace=TRUE) {
  for (nm in list.files(path, pattern = "[.][Rr]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm))
    if(trace) cat("\n")
  }
}
sourceDir("ADFG_NOAA_SmallMeshTrawl")
#View(SMT)



# Select hauls to use:
SMT1 = SMT %>%
  filter(month %in% c(7,8,9,10)) # select July - Oct
  #filter(bottom_depth.m.>= ) %>% # select depth range (see notes below)
  #filter(bottom_depth.m.<= ) # select depth range
#View(SMT1)

### Which depths to use?  
# Anderson & Piatt 1996 used depths >55m
# Litzow 2006 used all depths
# See plots of percent catch by depth for each year in CUEbyDepth script
# From this, use samples collected at ...



# Calculate CUE (kg/km2):
SMT2=SMT1 %>%
  mutate(effort = distance*0.0098) %>% # create effort column (area swept (km2); see notes below)
  select(-catchNum) %>%
  mutate(cue = catchKg/effort)
#(SMT2)
# check: cruise 152, haul 1, CUE should be:
2.7/0.01451968 # 185.9545. Yes.


# Effort
# Standardized to area towed (vs temporal duration) because sampling protocol aims to trawl over a standard distance, and temporal duration can reflect things that affect tow speed
# Sampling protocol is to tow at 3.7 km / hr for a distance of 1.85 km (Jackson 2003, ADFG Small mesh trawl protocol), ie 30 min
# Net opening is 9.8m wide, 4.0m high, 3.1cm (1.2 inch) stretched mesh (Jackson 2003, ADFG Small mesh trawl protocol)
# Therefore area trawled = 1.85km (or whatever distance is recorded in distance column) x 0.0098km 
# 1.85km x 0.0098km = 0.01813 km2