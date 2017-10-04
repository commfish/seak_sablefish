# Clean data - processing script for all incoming data
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2017-10-03

# Fishery cpue ----

cpue <- read.csv("data/fishery/raw_data/fishery_cpue_1997_2015.csv")

# rename, define factors, remove mixed hook sizes; calculate stanardized no. of 
# hooks and cpue
cpue <- cpue %>% 
  mutate(Year = factor(YEAR), #factor
         year = YEAR, #numeric
         Adfg = factor(ADFG_NO),
         Spp_cde = factor(TRIP_TARGET),
         date = dmy(SELL_DATE), #ISO 8601 format
         julian_day = yday(date),
         Gear = factor(LONGLINE_SYSTEM_CODE),
         Hook_size = HOOK_SIZE,
         Size = factor(as.numeric(gsub("[^0-9]","",Hook_size))),
         hook_space = HOOK_SPACING, #*FLAG*-check that this is in inches
         hooks_per_skate = HOOKS_PER_SKATE,
         Stat = factor(G_STAT_AREA),
         depth = AVERAGE_DEPTH_METERS,
         sets = EFFORT_NO, #number of sets
         no_hooks = NUMBER_OF_HOOKS,         
         std_hooks = 2.2*no_hooks*(1-exp(-0.57*(2.54*hook_space/100))), #standardize hook spacing (Sigler & Lunsford 2001, CJFAS)
         sable_wt_set = SABLE_LBS_PER_SET, 
         std_cpue = sable_wt_set/std_hooks) %>% #standardized cpue
  filter(!is.na(date) & !is.na(hook_space) & 
           !is.na(hooks_per_skate) & 
           !is.na(sable_wt_set) &
           Spp_cde == "710" & #if there are any other spp (710=sablefish)
           julian_day > 226 & #if there were special projects before the fishery opened
           no_hooks < 15000) %>%  #*FLAG* where does 15000 come from? 14370 is the 75th percentile
  select(Year, year, Adfg, Spp_cde, date, julian_day, Gear, Hook_size,
         Size, hook_space, hooks_per_skate, Stat, depth, sets, no_hooks, 
         std_hooks, sable_wt_set, std_cpue)

write_rds(cpue, "data/fishery/fishery_cpue_1997_2015.rds")

# Fishery harvest, catch time series ----

# in progress...

# Survey cpue ----

# ALEX QUERY CRITERIA FOR CPUE_SRV: *FLAG* where's depth? sets?
# SURVEY >> LONGLINE SURVEY - CATCH AND HOOK ACCOUNTING
# Base Table	out_g_sur_longline_hooks_catch																				
# Select Clause	*																				
#   Where Clause	project_code = '03' AND year IN (1988:present)																				
# Group By Clause																					
# Order By Clause	year, project_code, trip_no,effort_no,subset_no		

srv_cpue <- read.csv("data/survey/raw_data/survey_cpue_1988_2016.csv")
str(srv_cpue)
srv_cpue <- srv_cpue %>% 
  mutate(year = Year, #numeric
         Year = as.factor(Year),
         Stat = as.factor(G.Stat.Area),
         station = Station.No,
         subset_no = Subset.No, #*FLAG* no idea what this is
         subset_condition = Subset.Condition #*FLAG* no idea what this is
  ) %>%  
  select(Year, year, trip_no = Trip.No, Project = Project.1, set = Set.No,
         Stat, station, subset_condition, no_hooks = `Hooks...Total`, 
         hooks_bait = `Hooks...Baited`, hooks_bare = `Hooks...Bare`, 
         hooks_invalid = `Hooks...Invalid`, hooks_unknown = `Hooks...Uknown`,
         no_sablefish = Sablefish, sable_per_hook = Sablefish.per.Hook
  ) %>% 
  filter(subset_condition == "Valid", #doesn't do anything
         station < 100, # omit samping stations with 3 digits *FLAG* no documentation
         !is.na(no_hooks) #doesn't do anything
  ) %>% 
  #standardize hook spacing (Sigler & Lunsford 2001, CJFAS)
  #there was a change in hook spacing in 1997 from 2 to 3 m. *FLAG* check
  mutate(std_hooks = ifelse(year > 1996, 2.2*no_hooks*(1-exp(-0.57*2)),
                            2.2*no_hooks*(1-exp(-0.57*3))),
         no_sablefish = ifelse(is.na(no_sablefish), 0, no_sablefish), # make any NAs 0 values
         std_cpue = no_sablefish/std_hooks, #*FLAG* this is NPUE, the fishery is a WPUE
         Station = as.factor(station)
  )
  
write_rds(srv_cpue, "data/survey/survey_cpue_1988_2016.rds")

# Survey biological ----
srv_bio <- read.csv("data/survey/raw_data/survey_bio_1988_2016.csv")
