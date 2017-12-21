# Query and clean data - processing script for all incoming data
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2017-12-19

# *in progress* All columns formatted as follows unless otherwise specified:
# `length` - cm fork length 
# `weight` - kg round, use `dressed_weight` otherwise
# `dates` - ISO 8601 YYYY/MM/DD
# `depth` - currently have both meters and fathoms...
# characters & factors - first letter capitilized (e.g. 'Sex'), otherwise lowercase

# year of the assessment 
YEAR <- 2018

# load ----
source("r_code/helper.R")

# Oracle connections ----

# Database usernames and passwords (user-specific, ignored)
ora <- read_csv("database.csv") 

# Connection strings in T:/Toolbox/TNS/tnsnames.ora

# IFDB aka ALEX. Region I database, ultimately will be replaced by Zander
ifdb <- "(DESCRIPTION =
     (ADDRESS = (PROTOCOL = TCP)(HOST = db-ifdb.dfg.alaska.local)(PORT = 1521))
   (CONNECT_DATA = (SERVER = DEDICATED)
     (SERVICE_NAME = ifdb.dfg.alaska.local)))"

ifdb_channel <- dbConnect(drv = dbDriver('Oracle'), 
                          username = ora$ifdb_user, 
                          password = ora$ifdb_pw, 
                          dbname = ifdb)

# Zander. Region I database
zprod <- "(DESCRIPTION =
    (ADDRESS = (PROTOCOL = TCP)(HOST = db-zprod.dfg.alaska.local)(PORT = 1521))
    (CONNECT_DATA = (SERVER = DEDICATED)
      (SERVICE_NAME = zprod.dfg.alaska.local)))"

zprod_channel <- dbConnect(drv = dbDriver('Oracle'), 
                          username = ora$zprod_user, 
                          password = ora$zprod_pw, 
                          dbname = zprod)

# Fish tickets
adfgcf <- "(DESCRIPTION =
    (ADDRESS = (PROTOCOL = TCP)(HOST = db-padfgcf.dfg.alaska.local)(PORT = 1521))
(CONNECT_DATA = (SID = PADFGCF)))"

adfgcf_channel <- dbConnect(drv = dbDriver('Oracle'), 
                          username = ora$adfgcf_user, 
                          password = ora$adfgcf_pw, 
                          dbname = adfgcf)

# Data warehouse. eLandings, fish tickets, maybe tag lab data too?
dwprod <- "(DESCRIPTION =
(ADDRESS = (PROTOCOL = TCP)(HOST = db-dwprod.dfg.alaska.local)(PORT = 1521))
    (CONNECT_DATA = (SERVER = DEDICATED)
      (SERVICE_NAME = dwprod.dfg.alaska.local)))"

dwprod_channel <- dbConnect(drv = dbDriver('Oracle'), 
                          username = ora$dwprod_user, 
                          password = ora$dwprod_pw, 
                          dbname = dwprod)

# Fishery harvest, catch time series ----

# g_cfec_fishery_group_code = 'C' -- Sablefish

query <- 
" select  year, adfg_no, vessel_name, port_code, gear,
          catch_date, sell_date, harvest_code, harvest, g_stat_area,
          g_management_area_code, species_code, pounds, round_pounds,
          delivery_code, delivery

  from    out_g_cat_ticket

  where   g_cfec_fishery_group_code = 'C' and
          species_code = '710' and
          g_management_area_code = 'NSEI'  "

dbGetQuery(ifdb_channel, query) -> annual_catch

annual_catch %>% 
  group_by(YEAR) %>% 
  summarise(sum(POUNDS),
            sum(ROUND_POUNDS)) %>% 
  View()

write_csv(paste0("data/fishery/raw_data/fishery_catch_fishery_cpue_", 
                 min(annual_catch$YEAR), "_", max(annual_catch$YEAR), ".csv"))

# Chatham Strait Longline Survey biological data. originally stored in ifdb
# under out_g_bio_effort_age_sex_size but since the development of ACES (the
# mobile app used on the survey), which was not compatible with ALEX, all bio
# data has migrated to zprod under output.out_g_sur_longline_specimen

query <-
" select  trip_id, year, time_first_buoy_onboard as fishing_date, project_code, trip_no, 
          target_species_code, adfg_no, vessel_name, number_of_stations, sample_type_code,
          hooks_per_set, hook_size, hook_spacing_inches, bait_code, 
          effort_no, g_stat_area, station_no, start_latitude_decimal_degrees,
          start_longitude_decimal_degree, end_latitude_decimal_degrees, 
          end_longitude_decimal_degrees, longline_system_code, start_depth_fathoms,
          end_depth_fathoms, avg_depth_fathoms, species_code, length_millimeters, 
          length_type_code, weight_kilograms, age, age_type_code, age_readability_code,
          sex_code, maturity_code, otolith_condition_code

  from    output.out_g_sur_longline_specimen

  where   species_code = '710' and 
          project_code = '603' "

dbGetQuery(zprod_channel, query) -> srv_bio

srv_bio %>% 
  group_by(YEAR) %>% 
  summarise(n()) %>% 
  View()

write_csv(paste0("data/survey/raw_data/llsurvey_bio_", 
                 min(srv_bio$YEAR), "_", max(srv_bio$YEAR), ".csv"))

# Groundfish project codes (yes, they are different for OceanAK and IFDB)

# OceanAK
query <-
" select  project_code, project
  from    lookup.project
  where   category_code = 'g' "

dbGetQuery(zprod_channel, query)

# IFDB
query <-
" select  project_code, project
  from    project
  where   category_code = 'g' "

dbGetQuery(ifdb_channel, query)


# Fishery biological data. project codes: 17 = commercial pot, 02 = commercial
# longline

query <-
" select  year, project_code, trip_no, adfg_no, vessel_name, sell_date, g_stat_area,
          g_management_area_code, g_stat_area_group, sample_type_code, sample_type, 
          species_code, length_type_code, length_type, length_millimeters, weight_kilograms,
          age, age_readability_code, age_readability, sex_code, maturity_code, maturity, 
          gear_code, gear

  from    out_g_bio_age_sex_size

  where   species_code = '710' and
          project_code in ('02', '17') and
          g_management_area_code in ('NSEO', 'NSEI') "

dbGetQuery(ifdb_channel, query) -> fsh_bio

fsh_bio %>% 
  group_by(YEAR, PROJECT_CODE, G_MANAGEMENT_AREA_CODE) %>% 
  summarise(n()) %>% 
  View()

write_csv(paste0("data/survey/raw_data/llsurvey_bio_", 
                 min(srv_bio$YEAR), "_", max(srv_bio$YEAR), ".csv"))

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
         Discard_status = DISCARD_STATUS, Discard_status_cde = DISCARD_STATUS_CODE) -> check_error

# Error: In biological.r when we read_csv(), you get parsing failures warning
# due to entries that have decimal points. read_csv turns these into NAs b/c it
# has not identified this col as a dbl (only looks at first 1000 rows). Increase
# guess_max in order to read col properly. It doesn't matter for age comps b/c
# they get filtered out due to no age/sex data. Should they be removed? Check
# with Aaron Baldwin about how fish are measured on the survey - should we
# expect to the exact mm or rounded to the nearest cm?
check_error %>% mutate(Length = as.character(length)) %>% 
  filter(grepl('\\..$', Length)) %>% View()

check_error %>% write_csv("data/survey/potsurvey_bio_2009_2015.csv")
