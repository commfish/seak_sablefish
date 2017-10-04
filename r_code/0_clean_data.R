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
         hook_space = HOOK_SPACING,
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

write.csv(cpue, "data/fishery/fishery_cpue_1997_2015.csv")

# Fishery harvest, catch time series----

# in progress...

#
