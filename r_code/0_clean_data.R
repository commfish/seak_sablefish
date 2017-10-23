# Clean data - processing script for all incoming data
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2017-10-05

# *in progress* All columns formatted as follows unless otherwise specified:
# `length` - cm fork length 
# `weight` - kg round, use `dressed_weight` otherwise
# `dates` - ISO 8601 YYYY/MM/DD
# `depth` - currently have both meters and fathoms...
# characters & factors - first letter capitilized (e.g. 'Sex'), otherwise lowercase

# load ----
source("r_code/helper.R")

# Fishery cpue ----

read_csv("data/fishery/raw_data/fishery_cpue_1997_2015.csv") %>% 

  # rename, define factors, remove mixed hook sizes; calculate stanardized no. of 
  # hooks and cpue
  mutate(date = dmy(SELL_DATE), #ISO 8601 format
         julian_day = yday(date),
         Gear = factor(LONGLINE_SYSTEM_CODE),
         Hook_size = HOOK_SIZE, 
         hook_space = HOOK_SPACING, #*FLAG*-check that hook_space is in inches
         Size = factor(as.numeric(gsub("[^0-9]", "", Hook_size))),
         no_hooks = NUMBER_OF_HOOKS,
         hooks_per_skate = HOOKS_PER_SKATE,
         sable_wt_set = SABLE_LBS_PER_SET) %>% 
  filter(!is.na(date) & !is.na(hook_space) & 
           !is.na(hooks_per_skate) &
           !is.na(sable_wt_set) &
           TRIP_TARGET == "710" & #if there are any other spp (710=sablefish)
           julian_day > 226 & #if there were special projects before the fishery opened
           no_hooks < 15000) %>%  #*FLAG* where does 15000 come from? 14370 is the 75th percentile
  select(year = YEAR, Adfg = ADFG_NO, Spp_cde = TRIP_TARGET, date, julian_day, 
         Gear = LONGLINE_SYSTEM_CODE, Hook_size, Size, hooks_per_skate, 
         hook_space, Stat = G_STAT_AREA, no_hooks, depth = AVERAGE_DEPTH_METERS, 
         sets = EFFORT_NO, sable_wt_set) %>% 
  write_csv("data/fishery/fishery_cpue_1997_2015.csv")

# Fishery harvest, catch time series ----

# in progress...

# Fishery biological ----

# *FLAG* Need an explanation of G_STAT_AREA_GROUP (e.g. NSEI 22, NSEI 15),
# SPECIMEN_NO, SAMPLE_TYPE_CODE, WEIGHT_KILOGRAMS (round? ice/slime? some of
# these are really heavy, e.g. 17 kg), SEX_CODE (females = 1 or 2?)

# ALEX QUERY CRITERIA FOR FISH_BIO_DATA
# Year BETWEEN 1980 AND 2015 AND species_code = '710' AND 
# g_manAgement_area_code = 'NSEI' AND project_code = '02'
# NOTE: when running, always make sure that all data have been read 
# and uploaded into IFDB by the ADU

read_csv("data/fishery/raw_data/fishery_bio_2000_2016.csv") %>% 
  mutate(date = mdy(SELL_DATE), #ISO 8601 format
         julian_day = yday(date),
         Sex_cde = SEX_CODE,
         length = LENGTH_MILLIMETERS / 10,
         Sex = ifelse(Sex_cde == 1, "Male", 
                      ifelse(Sex_cde == 2, "Female", "Unknown"))) %>% 
  select(year = YEAR, Project_cde = PROJECT_CODE, trip_no = TRIP_NO, 
         Adfg = ADFG_NO, Vessel = VESSEL_NAME, date, julian_day,
         Stat = G_STAT_AREA, Mgmt_area = G_MANAGEMENT_AREA_CODE,
         Sample_type = SAMPLE_TYPE, Spp_cde = SPECIES_CODE, 
         length, weight = WEIGHT_KILOGRAMS,
         age = AGE, Sex_cde, Sex, Maturity_cde = MATURITY_CODE,
         Maturity = MATURITY) %>% 
  write_csv("data/fishery/fishery_bio_2000_2016.csv")

# Longline survey cpue ----

# ALEX QUERY CRITERIA FOR CPUE_SRV: *FLAG* where's depth? sets?
# SURVEY >> LONGLINE SURVEY - CATCH AND HOOK ACCOUNTING
# Base Table	out_g_sur_longline_hooks_catch																				
# Select Clause	*																				
#   Where Clause	project_code = '03' AND year IN (1988:present)																				
# Group By Clause																					
# Order By Clause	year, project_code, trip_no,effort_no,subset_no		

read_csv("data/survey/raw_data/llsurvey_cpue_1988_2016.csv") %>% 
  mutate(year = Year, #numeric
         subset_no = `Subset No`, #*FLAG* no idea what this is
         subset_condition = `Subset Condition`) %>%  #*FLAG* no idea what this is
  select(year,  trip_no = `Trip No`, Project = Project, 
         set = `Set No`, Stat = `G Stat Area`, Station = `Station No`, 
         subset_condition, no_hooks = `Hooks - Total`, 
         hooks_bait = `Hooks - Baited`, hooks_bare = `Hooks - Bare`, 
         hooks_invalid = `Hooks - Invalid`, hooks_unknown = `Hooks - Uknown`,
         no_sablefish = Sablefish, sable_per_hook = `Sablefish per Hook`) %>% 
  filter(subset_condition == "Valid", #doesn't do anything
         Station < 100, # omit samping stations with 3 digits *FLAG* no documentation
         !is.na(no_hooks)) %>% #doesn't do anything
  write_csv("data/survey/llsurvey_cpue_1988_2016.csv")

# Longline survey biological ----

# *FLAG* - What does Effort No mean in this context? What day and depth were these collected? Date collected?

# ALEX QUERY CRITERIA FOR SRV_BIO_DATA -  obsolete
# BIOLOGICAL DATA >> Age Sex Size Sampled at Sea
#  Base Table	out_g_bio_effort_Age_sex_size							
#  Select Clause	*							
# Where Clause	Year BETWEEN 1988 AND 2015 AND 
#                   species_code = '710' AND 
#                   project_code = '03'																	


# OceanAK QUERY CRITERIA FOR SRV_BIO_DATA
# Commercial Fisheries / Region I / Groundfish / Age-Sex-Size Sampled at Sea
#
# Criteria     	    Year is between 1988 AND Present AND 
#                   Species = 'Sablefish' AND 
#                   Project = 'Chatham Sablefish LL Survey'																	

read_csv("data/survey/raw_data/llsurvey_bio_1988_2016.csv") %>% 
  mutate(length = `Length Millimeters` / 10) %>% 
  select(year = Year,  trip_no = `Trip No`, Project = Project, 
         sets = `Effort No`, Stat = `G Stat Area`, Station = `Station No`, 
         sets = `Effort No`, Sex, Maturity, Maturity_cde = maturity_Code,
         length, weight = `Weight Kilograms`,
         age = Age, Age_method = `Age Type`, Age_readability = `Age Readability`) %>% 
  write_csv("data/survey/llsurvey_bio_1988_2016.csv")

# Pot survey biological ----

# The pot survey is a mark-recapture survey. Fish are

# ALEX QUERY CRITERIA - legacy - keep
# Subsample of those fish on the pot marking survey sacrificed to obtain
# sex data. Note that this table is distinct from POT_LENGTH above, as 
# this one selects from Age-Sex-Size *Sampled* at Sea, not *Tagged*
#   
# *FLAG* join pot survey and ll survey biological data?
#
# BIOLOGICAL DATA >> Age-Sex-Size Sampled at Sea
# Base Table	out_g_bio_eff_Age_sex_size							
# Select Clause	*							
#   Where Clause	Year BETWEEN 2009 AND 2015 AND
#                   species_code = '710' AND
#                   gear_code = '91' AND
#                   project_code = '11'

# *FLAG* this query currently only has age-sex-size data in 2009.

read_csv("data/survey/raw_data/potsurvey_bio_2009_2015.csv") %>% 
  # split date column to convert to proper format
  tidyr::separate(TIME_SECOND_ANCHOR_ONBOARD, c("date", "time"), sep = " ", extra = "merge") %>% 
  mutate(length = LENGTH_MILLIMETERS / 10,# convert all lengths to cm
         date = mdy(date), 
         julian_day = yday(date)) %>% 
  select(year = YEAR, Project_cde = PROJECT_CODE, Project = PROJECT, 
         trip_no = TRIP_NO, Spp_cde = TARGET_SPECIES_CODE, 
         Vessel = VESSEL_NAME, Stat = G_STAT_AREA, Mgmt_area = MANAGEMENT_AREA, 
         Station = STATION_NO, date, julian_day, depth = AVG_DEPTH_FATHOMS, Sex = SEX,
         age = AGE, length, weight = WEIGHT_KILOGRAMS, Maturity_cde = MATURITY_CODE, 
         Discard_status = DISCARD_STATUS, Discard_status_cde = DISCARD_STATUS_CODE) -> pot

pot %>% mutate(Length = as.character(length)) %>% 
  # filter(grep('\\..{2}$', Length))
  filter(grepl('\\..$', Length)) -> sub

str(sub$length)
# filter(!grepl(".", Length)) 
  write_csv("data/survey/potsurvey_bio_2009_2015.csv")



