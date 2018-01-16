# Query and clean data - processing script for all incoming data
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2018-01-12

# All final columns formatted as follows unless otherwise specified:
# length: fork length, cm
# weight: kg
# Maturity: "0" = immature, "1" = mature
# depth: average depth, meters
# date: ISO 8601 YYYY/MM/DD
# characters & factors - first letter capitilized (e.g. 'Sex'), otherwise lowercase
# lat and lon are in decimal degrees

# project codes: query zprod: "select new_project_code, project_code, project from
# lookup.project_conversion where category_code = 'g'"

# new (Zander) = old (IFDB) = description
# 601 = 01 = Clarence sablefish longline survey
# 602 = 02 = commercial longline trip
# 603 = 03 = Chatham sablefish longline survey
# 607 = 07 = atypical sample (unknown gear)
# 608 = 08 = atypical longline sample
# 617 = 17 = commercial pot trip
# 610 = 10 = Clarence sablefish pot survey
# 611 = 11 = Chatham sabelfish pot survey
# 623 = 23 = Canadian commercial longline
# 624 = 24 = Canadian commercial pot
# 625 = 25 = Canadian commercial trawl
# 626 = 26 = Canadian scientific survey

# most recent year of data
YEAR <- 2017

# Load ----
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

# Fishery removals ----

# gef = gross earnings file. This was pitched to me by J. Shriver as the
# definitive source for historical landings records. The detailed file includes
# all sablefish (species code 710) landings, including as bycatch. If you're
# interested in which fishery landings came from, use the CFEC permit code (cfec
# group code = 'C' = Sablefish, code descriptions at
# https://www.cfec.state.ak.us/misc/FshyDesC.htm

query <-
" select  adfg_b_batch_year as year, adfg_h_date_landed as date_landed,
          adfg_h_gear_code as gear_code, adfg_h_port as port_code, 
          cfec_harvest_area, adfg_h_permit_fishery as cfec_permit, 
          adfg_i_delivery_code as delivery_code, adfg_i_harvest_code as harvest_code, 
          adfg_h_mgt_program_id as mgt_program_code, adfg_i_species_code as species_code, 
          adfg_i_stat_area as stat_area, adfg_h_stat_area_type as stat_area_type, 
          cfec_landing_status as landing_status, adfg_i_pounds as pounds, 
          adfg_i_whole_pounds as whole_pounds

  from    dwgross.ge_gross_earnings 

  where   adfg_i_species_code = '710' "

system.time(dbGetQuery(dwprod_channel, query) -> gef ) 

# Stat area information by database type (includes additional info like state vs
# fed waters, effective dates for stat area codes, ADFG/NMFS mgt areas, etc.)
query <- 
" select  stat_area, database_type as stat_area_type, region, waters, 
          start_date, end_date, g_mgt_area_district, g_nmfs_area 

  from    dwgross.ge_stat_area "

dbGetQuery(dwprod_channel, query) -> stat_areas

merge(gef, stat_areas, by = c("STAT_AREA", "STAT_AREA_TYPE")) %>% 
  filter(G_MGT_AREA_DISTRICT == "NSEI") -> gef

write_csv(gef, paste0("data/fishery/raw_data/nseiharvest_gef_",
                          min(gef$YEAR), "_", max(gef$YEAR), ".csv"))

# This summary will include IFQ caught in state waters. Can filter
# that using the management program code field.
read_csv(paste0("data/fishery/raw_data/nseiharvest_gef_",
                min(gef$YEAR), "_", max(gef$YEAR), ".csv"), 
         guess_max = 50000) %>% 
  filter(G_MGT_AREA_DISTRICT == "NSEI") %>% 
  droplevels() %>% 
  mutate(
    date = ymd(as.Date(DATE_LANDED)), #ISO 8601 format
    julian_day = yday(date),
    # BEWARE: Landings are always entered as POUNDS and then converted to
    # WHOLE_POUNDS using a conversion factor related to the disposition code.
    # Prior to 1985 there was no disposition code for landings, which is why
    # WHOLE_POUNDS is not populated prior to 1985. Here we assume fish were
    # delivered whole prior to 1985, because that's the best we have.
    whole_pounds = ifelse(WHOLE_POUNDS == 0, POUNDS, WHOLE_POUNDS),
    # Aggregate gear field codes (lookup table in GEF_Variable_Descriptions....xls)
    Gear = derivedFactor("HAL" = GEAR_CODE %in% c("61", "06"), #hook-and-line aka longline
                              "POT" = GEAR_CODE %in% c("91", "09"),
                              "TRW" = GEAR_CODE %in% c("07", "47", "27", "17"), # non-pelagic/bottom trawl, pelagic/mid-water trawl, otter trawl, or beam trawl
                              "JIG" = GEAR_CODE %in% c("26"), # mechanical jigs
                              "TRL" = GEAR_CODE %in% c("05", "25", "15"), # hand troll, dinglebar troll
                              "GNT" = GEAR_CODE %in% c("03", "04", "41"), # set, drift, or sunken gillnet
                              "UNK" = GEAR_CODE %in% c("99"))) %>% # unknown
  select(year = YEAR, date, julian_day, Mgmt_area = G_MGT_AREA_DISTRICT, Stat = STAT_AREA,
         Waters = WATERS, Port = PORT_CODE,  Cfec_permit = CFEC_PERMIT, Mgmt_cde = MGT_PROGRAM_CODE, 
         Delivery_cde = DELIVERY_CODE, Harvest_cde = HARVEST_CODE, Landing_cde = LANDING_STATUS, 
         Spp_cde = SPECIES_CODE, whole_pounds, pounds = POUNDS) -> gef

write_csv(gef, paste0("data/fishery/nseiharvest_gef_",
                      min(gef$year), "_", max(gef$year), ".csv"))

# Alternative query using IFDB
query <-
" select  year, adfg_no, vessel_name, port_code, gear,
          catch_date, sell_date, harvest_code, harvest, g_stat_area,
          g_management_area_code, species_code, pounds, round_pounds,
          delivery_code, g_cfec_fishery_group, g_cfec_fishery

  from    out_g_cat_ticket

  where   species_code = '710' and
          g_management_area_code = 'NSEI' "

dbGetQuery(ifdb_channel, query) -> ifdb_catch

write_csv(ifdb_catch, paste0("data/fishery/raw_data/nseiharvest_ifdb_",
                      min(ifdb_catch$YEAR), "_", max(ifdb_catch$YEAR), ".csv"))

read_csv(paste0("data/fishery/raw_data/nseiharvest_ifdb_",
                min(ifdb_catch$YEAR), "_", max(ifdb_catch$YEAR), ".csv"), 
         guess_max = 50000) %>% 
  mutate(date = ymd(as.Date(CATCH_DATE)), #ISO 8601 format
         julian_day = yday(date),
         # BEWARE: Landings are always entered as POUNDS and then converted to
         # WHOLE_POUNDS using a conversion factor related to the disposition code.
         # Prior to 1985 there was no disposition code for landings, which is why
         # WHOLE_POUNDS is not populated prior to 1985. Here we assume fish were
         # delivered whole prior to 1985, because that's the best we have.
         whole_pounds = ifelse(ROUND_POUNDS == 0, POUNDS, ROUND_POUNDS)) %>% 
  select(year = YEAR, date, julian_day, Mgmt_area = G_MANAGEMENT_AREA_CODE, Stat = G_STAT_AREA,
         Adfg = ADFG_NO, Vessel = VESSEL_NAME, Port = PORT_CODE,  
         Cfec_permit = G_CFEC_FISHERY, Delivery_cde = DELIVERY_CODE, 
         Harvest = HARVEST, Harvest_cde = HARVEST_CODE, Spp_cde = SPECIES_CODE, 
         whole_pounds, pounds = POUNDS) -> ifdb_catch

write_csv(ifdb_catch, paste0("data/fishery/nseiharvest_ifdb_",
                             min(ifdb_catch$year), "_", max(ifdb_catch$year), ".csv"))

# catch %>% 
#   filter(G_CFEC_FISHERY %in% 
#            c(#'C09B', 'C91A', 'Z99B', 'M61B', 'C61C', 'B06B', 
#              'C06B', 'C06A', 'B61B', '0000', 'C61B', 'C61A')) %>%
#   group_by(YEAR, G_CFEC_FISHERY) %>% 
#   summarise(pounds = sum(POUNDS),
#             round_pounds = sum(ROUND_POUNDS)) %>%
#   group_by(YEAR) %>% 
#   mutate(tot_pounds = sum(pounds),
#          tot_round = sum(round_pounds)) -> annual_catch
# 
# ggplot(annual_catch) + 
#   geom_line(aes(x = YEAR, y = pounds, 
#                 colour = G_CFEC_FISHERY), size = 2) +
#   geom_line(aes(YEAR, tot_pounds), colour = "black", size = 1, linetype = 4)

# Y61A	DEMERSAL SHELF ROCKFISH , LONGLINE VESSEL OVER 5 TON, SOUTHEAST	1990 - 1995
# Y61A	DEMERSAL SHELF ROCKFISH , LONGLINE VESSEL 60' OR OVER, SOUTHEAST	1996 - 2017
# K09A	KING CRAB , POT GEAR VESSEL 50' OR LESS, SOUTHEAST	1974 - 1984
# M99B	MISC. SALTWATER FINFISH , OTHER GEAR, STATEWIDE	1975 - 2017
# M17B	MISC. SALTWATER FINFISH , BEAM TRAWL, STATEWIDE	1975 - 2017
# M06B	MISC. SALTWATER FINFISH , LONGLINE VESSEL UNDER 5 TON, STATEWIDE	1974 - 1995
# M06B	MISC. SALTWATER FINFISH , LONGLINE VESSEL UNDER 60', STATEWIDE	1996 - 2017
# S05B	SALMON , HAND TROLL, STATEWIDE	1974 - 2017
# S15B	SALMON , POWER TROLL, STATEWIDE	1974 - 2017
# C09B	SABLEFISH , POT GEAR VESSEL 50' OR LESS, STATEWIDE	1975 - 1995
# C09B	SABLEFISH , POT GEAR VESSEL UNDER 60', STATEWIDE	1996 - 2017
# C91A	SABLEFISH , POT GEAR, NORTHERN SE-TEST ADF&G ONLY	2004 - 2017
# Z99B	MISC. MARINE INVERTEBRATES , OTHER GEAR, STATEWIDE	1974 - 2017
# M61B	MISC. SALTWATER FINFISH , LONGLINE VESSEL OVER 5 TON, STATEWIDE	1975 - 1995
# M61B	MISC. SALTWATER FINFISH , LONGLINE VESSEL OVER 60', STATEWIDE	1996 - 2005
# C61C	SABLEFISH , LONGLINE, SOUTHERN SOUTHEAST	1986 - 2017
# B06B	HALIBUT , LONGLINE VESSEL UNDER 5 TON, STATEWIDE	1974 - 1995
# B06B	HALIBUT , LONGLINE VESSEL UNDER 60', STATEWIDE	1996 - 2017
# C06B	SABLEFISH , LONGLINE VESSEL UNDER 5 TON, STATEWIDE	1975 - 1994
# C06B	SABLEFISH , LONGLINE VESSEL UNDER 60', STATEWIDE	1995 - 2017
# C06A	SABLEFISH , LONGLINE VESSEL UNDER 5 TON, NORTHERN SOUTHEAST	1985 - 1987
# B61B	HALIBUT , LONGLINE VESSEL OVER 5 TON, STATEWIDE	1974 - 1995
# B61B	HALIBUT , LONGLINE VESSEL 60' OR OVER, STATEWIDE	1996 - 2017
# NA-0000		
# C61B	SABLEFISH , LONGLINE VESSEL OVER 5 TON, STATEWIDE	1975 - 1995
# C61B	SABLEFISH , LONGLINE VESSEL 60' OR OVER, STATEWIDE	1996 - 2017
# C61A	SABLEFISH , LONGLINE, NORTHERN SOUTHEAST	1985 - 2017

# Fishery cpue ----

# Kamala Carroll pulls the IFDB view out_g_log_longline_c_e, populates new
# columns effort_target_species_code and effort_comments, and sends to Scott
# Johnson. Scott runs a series of Martina Kallenburger's sql scripts (which I
# don't have access to) that match fish tickets pounds back to set based on
# Kamala's set target designation based on some undocumented methodology
# (proportional to numbers/pounds in logbook?). It lives in a view called
# sablefish_cpue_final_view in scottj's IFDB schema, though he has made a public
# synonym for it. This output doesn't contain information on sets designated as
# halibut targets. Note that it is missing effort_no's (effort_no's = individual
# sets).

query <- 
  " select  year, project_code, trip_no, adfg_no, longline_system_code, sell_date, 
            hook_size, hook_spacing, hooks_per_skate, number_of_skates, number_of_hooks,
            average_depth_meters, g_management_area_code, g_stat_area, trip_target, set_target,
            effort_no, sable_lbs_per_set, time_set, time_hauled, 
            start_latitude_decimal_degrees, start_longitude_decimal_degree

    from    sablefish_cpue_final_view

    where   g_management_area_code = 'NSEI' "

dbGetQuery(ifdb_channel, query) -> fsh_eff

write_csv(fsh_eff, paste0("data/fishery/raw_data/fishery_cpue_",
                          min(fsh_eff$YEAR), "_", max(fsh_eff$YEAR), ".csv"))

read_csv(paste0("data/fishery/raw_data/fishery_cpue_",
                min(fsh_eff$YEAR), "_", max(fsh_eff$YEAR), ".csv"), 
         guess_max = 50000) %>% 
  # rename, define factors, remove mixed hook sizes; calculate stanardized no. of 
  # hooks and cpue
  mutate(date = ymd(as.Date(TIME_SET)), #ISO 8601 format
         julian_day = yday(date),
         time_fished = difftime(TIME_HAULED, TIME_SET, units = "hours"),
         Gear = factor(LONGLINE_SYSTEM_CODE),
         Hook_size = HOOK_SIZE, 
         hook_space = HOOK_SPACING, #*FLAG* - check that hook_space is in inches
         Size = factor(as.numeric(gsub("[^0-9]", "", Hook_size))),
         no_hooks = NUMBER_OF_HOOKS,
         hooks_per_skate = HOOKS_PER_SKATE,
         sable_lbs_set = SABLE_LBS_PER_SET) %>% 
  filter(!is.na(date) & !is.na(hook_space) &
           !is.na(hooks_per_skate) &
           !is.na(sable_lbs_set) &
           julian_day > 226 ) %>% # if there were special projects before the fishery opened
           # no_hooks < 15000) %>%  #*FLAG* where does 15000 come from? 14370 is the 75th percentile
  select(year = YEAR, trip_no = TRIP_NO, Adfg = ADFG_NO, Spp_cde = TRIP_TARGET, date, julian_day, 
         time_fished, Gear = LONGLINE_SYSTEM_CODE, Hook_size, Size, hooks_per_skate, 
         hook_space, Stat = G_STAT_AREA, no_hooks, depth = AVERAGE_DEPTH_METERS, 
         sets = EFFORT_NO, sable_lbs_set) -> fsh_eff

write_csv(fsh_eff, paste0("data/fishery/fishery_cpue_",
                   min(fsh_eff$year), "_", max(fsh_eff$year), ".csv"))

# Fishery biological ----

# Fishery and pot survey bio data still come from IFDB, ZPROD official source
# for longline survey data

query <-
" select  year, project_code, trip_no, adfg_no, vessel_name, sell_date, g_stat_area,
          g_management_area_code, sample_type, species_code, length_type_code, 
          length_type, length_millimeters / 10 as length, weight_kilograms,
          age, age_readability_code, age_readability, sex_code, 
          maturity_code, maturity, gear_code, gear

  from    out_g_bio_age_sex_size

  where   species_code = '710' and
          project_code in ('02', '17') and
          g_management_area_code = 'NSEI' "

dbGetQuery(ifdb_channel, query) -> fsh_bio

write_csv(fsh_bio, paste0("data/fishery/raw_data/fishery_bio_", 
                 min(fsh_bio$YEAR), "_", max(fsh_bio$YEAR), ".csv"))

read_csv(paste0("data/fishery/raw_data/fishery_bio_", 
                min(fsh_bio$YEAR), "_", max(fsh_bio$YEAR), ".csv"), 
         guess_max = 50000) %>% 
  mutate(date = ymd(as.Date(SELL_DATE)), #ISO 8601 format
         julian_day = yday(date),
         Sex = derivedFactor("Male" = SEX_CODE == "01",
                             "Female" = SEX_CODE == "02",
                             .default = NA),
         Maturity = derivedFactor("0" = MATURITY_CODE %in% c("01", "02"), 
                                  "1" = MATURITY_CODE %in% c("03", "04", "05", "06", "07"),
                                  .default = NA),
         Gear = derivedFactor("Pot" = PROJECT_CODE == "17",
                              "Longline" = PROJECT_CODE == "02")) %>% 
  select(year = YEAR, Project_cde = PROJECT_CODE, trip_no = TRIP_NO, 
         Adfg = ADFG_NO, Vessel = VESSEL_NAME, date, julian_day,
         Stat = G_STAT_AREA, Mgmt_area = G_MANAGEMENT_AREA_CODE,
         Sample_type = SAMPLE_TYPE, Spp_cde = SPECIES_CODE, 
         length, weight = WEIGHT_KILOGRAMS,
         age = AGE, Sex, Maturity) -> fsh_bio

write_csv(fsh_bio, paste0("data/fishery/fishery_bio_", 
                          min(fsh_bio$YEAR), "_", max(fsh_bio$YEAR), ".csv"))

# Longline survey cpue ----

query <- 
" select  year, project_code, trip_no, target_species_code, adfg_no, vessel_name, 
          time_first_buoy_onboard, number_of_stations, hooks_per_set, hook_size, 
          hook_spacing_inches, sample_freq, last_skate_sampled, effort_no, station_no, species_code, 
          g_stat_area as stat, start_latitude_decimal_degrees as start_lat,
          start_longitude_decimal_degree as start_lon, end_latitude_decimal_degrees as end_lat,
          end_longitude_decimal_degrees as end_lon, avg_depth_fathoms * 1.8288 as depth_meters, 
          number_hooks, bare, bait, invalid, hagfish_slime, unknown, numbers

  from    output.out_g_sur_longline_catch_bi
  
  where   species_code = '710' and
          project_code in ('603', '03')"
          
dbGetQuery(zprod_channel, query) -> srv_eff

# View doesn't have management areas, join with stat_area look up as a check
# (the project code should be enough because they're area-specific)

query <- 
  " select  g_stat_area as stat, g_management_area_code
    from    lookup.g_stat_area"

dbGetQuery(zprod_channel, query) -> stat_areas

merge(srv_eff, stat_areas, by = "STAT") -> srv_eff

write_csv(srv_eff, paste0("data/survey/raw_data/llsrv_cpue_",
                          min(srv_eff$YEAR), "_", YEAR, ".csv"))

read_csv(paste0("data/survey/raw_data/llsrv_cpue_",
                min(srv_eff$YEAR), "_", YEAR, ".csv"), 
         guess_max = 50000) %>% 
  filter(YEAR <= 2017) %>% #the programmers have some dummy data in the db for the upcoming year
  mutate(date = ymd(as.Date(TIME_FIRST_BUOY_ONBOARD)), #ISO 8601 format
         julian_day = yday(date)) %>% 
  select(year = YEAR, Mgmt_area = G_MANAGEMENT_AREA_CODE, Project_cde = PROJECT_CODE, 
         trip_no = TRIP_NO, Adfg = ADFG_NO, Vessel = VESSEL_NAME, date, julian_day,
         Stat = STAT, Mgmt_area = G_MANAGEMENT_AREA_CODE, Spp_cde = SPECIES_CODE, 
         set = EFFORT_NO, start_lat = START_LAT, start_lon = START_LON, end_lat = END_LAT,
         end_lon = END_LON, depth = DEPTH_METERS, no_hooks = NUMBER_HOOKS, hooks_bare = BARE,
         hooks_bait = BAIT, hook_invalid = INVALID, hooks_sablefish = NUMBERS) -> srv_eff

write_csv(srv_eff, paste0("data/survey/llsrv_cpue_",
                          min(srv_eff$year), "_", YEAR, ".csv"))

# Longline survey biological ----

# Chatham Strait Longline Survey biological data. originally stored in ifdb
# under out_g_bio_effort_age_sex_size but since the development of ACES (the
# mobile app used on the survey), which was not compatible with ALEX, all bio
# data has migrated to zprod under output.out_g_sur_longline_specimen


# These are stored in the modern database, Zander (aka ZPROD)

query <-
" select  year, project_code, trip_no, target_species_code, adfg_no, vessel_name, 
          time_first_buoy_onboard, number_of_stations, hooks_per_set, hook_size, 
          hook_spacing_inches, sample_freq, last_skate_sampled, effort_no, station_no, species_code, 
          g_stat_area as stat, start_latitude_decimal_degrees as start_lat,
          start_longitude_decimal_degree as start_lon, end_latitude_decimal_degrees as end_lat,
          end_longitude_decimal_degrees as end_lon, avg_depth_fathoms * 1.8288 as depth_meters, 
          length_millimeters / 10 as length, weight_kilograms as weight, 
          age, age_type_code, age_readability_code, sex_code, maturity_code, otolith_condition_code

  from    output.out_g_sur_longline_specimen

  where   species_code = '710' and
          project_code in ('603', '03')"

dbGetQuery(zprod_channel, query) -> srv_bio

# View doesn't have management areas, join with stat_area look up as a check
# (the project code should be enough because they're area-specific)

merge(srv_bio, stat_areas, by = "STAT") -> srv_bio

write_csv(srv_bio, paste0("data/survey/raw_data/llsrv_bio_",
                          min(srv_bio$YEAR), "_", YEAR, ".csv"))

read_csv(paste0("data/survey/raw_data/llsrv_bio_",
                min(srv_bio$YEAR), "_", max(srv_bio$YEAR), ".csv"), 
         guess_max = 50000) %>% 
  mutate(date = ymd(as.Date(TIME_FIRST_BUOY_ONBOARD)), #ISO 8601 format
         julian_day = yday(date),Sex = derivedFactor("Male" = SEX_CODE == "01",
                                                     "Female" = SEX_CODE == "02",
                                                     .default = NA),
         Maturity = derivedFactor("0" = MATURITY_CODE %in% c("01", "02"), 
                                  "1" = MATURITY_CODE %in% c("03", "04", "05", "06", "07"),
                                  .default = NA)) %>% 
  select(year = YEAR, Mgmt_area = G_MANAGEMENT_AREA_CODE, Project_cde = PROJECT_CODE, 
         trip_no = TRIP_NO, Adfg = ADFG_NO, Vessel = VESSEL_NAME, date, julian_day,
         Stat = STAT, Spp_cde = SPECIES_CODE, set = EFFORT_NO, start_lat = START_LAT, 
         start_lon = START_LON, end_lat = END_LAT, end_lon = END_LON, depth = DEPTH_METERS, 
         length = LENGTH, weight = WEIGHT, age = AGE, Sex, Maturity, age_type_code = AGE_TYPE_CODE, 
         age_readability = AGE_READABILITY_CODE, otolith_condition = OTOLITH_CONDITION_CODE )  %>% 
  filter(Mgmt_area == 'NSEI') -> srv_bio

write_csv(srv_bio, paste0("data/survey/llsrv_bio_",
                          min(srv_bio$year), "_", max(srv_bio$year), ".csv"))

# Pot survey biological ----

# The pot survey is a mark-recapture survey. Limitted bio data exists.

# *FLAG* this query currently only has age-sex-size data in 2009.

query <- 
" select  year, project_code, trip_no, target_species_code, adfg_no, vessel_name, 
          time_first_buoy_onboard, effort_no, station_no, species_code, 
          g_stat_area as stat, management_area, start_latitude_decimal_degrees as start_lat,
          start_longitude_decimal_degree as start_lon, end_latitude_decimal_degrees as end_lat,
          end_longitude_decimal_degrees as end_lon, avg_depth_fathoms * 1.8288 as depth_meters, 
          length_millimeters / 10 as length, weight_kilograms as weight, 
          age, age_type_code, age_readability_code, sex_code, maturity_code
  
  from    out_g_bio_effort_age_sex_size

  where   species_code = '710' and
          gear_code = '91' and
          project_code in ('11', '611') "

dbGetQuery(ifdb_channel, query) -> pot_bio

write_csv(pot_bio, paste0("data/survey/raw_data/potsrv_bio_",
                          min(pot_bio$YEAR), "_", max(pot_bio$YEAR), ".csv"))

read_csv(paste0("data/survey/raw_data/potsrv_bio_",
                min(pot_bio$YEAR), "_", max(pot_bio$YEAR), ".csv"), 
         guess_max = 50000) %>% 
  mutate(date = ymd(as.Date(TIME_FIRST_BUOY_ONBOARD)), #ISO 8601 format
         julian_day = yday(date),Sex = derivedFactor("Male" = SEX_CODE == "01",
                                                     "Female" = SEX_CODE == "02",
                                                     .default = NA),
         Maturity = derivedFactor("0" = MATURITY_CODE %in% c("01", "02"), 
                                  "1" = MATURITY_CODE %in% c("03", "04", "05", "06", "07"),
                                  .default = NA)) %>% 
  select(year = YEAR, Mgmt_area = MANAGEMENT_AREA, Project_cde = PROJECT_CODE, 
         trip_no = TRIP_NO, Adfg = ADFG_NO, Vessel = VESSEL_NAME, date, julian_day,
         Stat = STAT, Spp_cde = SPECIES_CODE, set = EFFORT_NO, 
         start_lat = START_LAT, start_lon = START_LON, end_lat = END_LAT,
         end_lon = END_LON, depth = DEPTH_METERS, length = LENGTH, weight = WEIGHT, 
         age = AGE, Sex, Maturity, age_type_code = AGE_TYPE_CODE, 
         age_readability = AGE_READABILITY_CODE)  -> pot_bio

write_csv(pot_bio, paste0("data/survey/potsrv_bio_",
                          min(pot_bio$year), "_", max(pot_bio$year), ".csv"))

# Tagging data ----

# Wont be finalized until the end of January 2018.