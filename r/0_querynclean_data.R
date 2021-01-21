# Query and clean data - processing script for all incoming data
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: June 2020

# NOTE: The last time queries were run using ROracle was using R version 3.5.3
# 64-bit. This package has NOT been tested using the current 3.6.3 64-bit
# version

# All final columns formatted as follows unless otherwise specified:
# length: fork length, cm
# weight: kg
# Maturity: "0" = immature, "1" = mature
# depth: average depth, meters
# date: ISO 8601 YYYY/MM/DD
# characters & factors - first letter capitilized (e.g. 'Sex'), otherwise lowercase
# lat and lon are in decimal degrees
# catch - varies between whole_pounds or whole_kg depending on what its being used for

# project codes: query zprod: "select new_project_code, project_code, project from
# lookup.project_conversion where category_code = 'g'"

# new (Zander) = old (IFDB) = description
# 601 = 01 = Clarence Sablefish LL Survey
# 602 = 02 = Commercial Longline Trip
# 603 = 03 = Chatham Sablefish LL Survey
# 604 = 04 = Commercial Jig Trip
# 605 = 05 = Longline Survey **NMFS survey
# 606 = 06 = Jig Survey
# 607 = 07 = Atypical Sample (unknown gear)
# 608 = 08 = Atypical Longline Sample
# 609 = 09 = Atypical Jig Sample
# 610 = 10 = Clarence Sablefish Pot Survey
# 611 = 11 = Chatham Sablefish Pot Survey
# 612 = 12 = Sitka Harbor Sablefish Survey
# 613 = 13 = Kodiak Trawl Sablefish Survey
# 614 = 14 = 1979 NSEI Crab Survey
# 615 = 15 = IPHC Annual Survey
# 616 = 16 = NMFS Coop Tagging Survey
# 617 = 17 = Commercial Pot Trip
# 618 = 18 = Lingcod Stock Assessment
# 619 = 19 = Black Rockfish Stock Assessment
# 620 = 20 = Commercial Troll
# 621 = 21 = Commercial Halibut Longline
# 622 = 22 = Atypical Trawl Sample
# 623 = 23 = Canadian Commercial Longline
# 624 = 24 = Canadian Commercial Pot
# 625 = 25 = Canadian Commercial Trawl
# 626 = 26 = Canadian Scientific Survey
# 627 = 27 = Subsistence/Personal Use
# 628 = 28 = Sport-caught Sample
# --- = 61 = Longline
# --- = 91 = Pot

# most recent year of data
YEAR <- 2019

# Load ----
source("r/helper.R")

# Oracle connections ----

# Database usernames and passwords (user-specific, ignored)
ora <- read_csv("database.csv") 

# *** Connection strings in T:/Toolbox/TNS/tnsnames.ora ***

# IFDB aka ALEX. Region I database, ultimately will be replaced by Zander
ifdb <- "(DESCRIPTION =
     (ADDRESS = (PROTOCOL = TCP)(HOST = 10.209.0.83)(PORT = 1521))
   (CONNECT_DATA = (SERVER = DEDICATED)
     (SERVICE_NAME = DFGCFR1P.500040564.us1.internal)))"

ifdb_channel <- dbConnect(drv = dbDriver('Oracle'), 
                          username = ora$ifdb_user, 
                          password = ora$ifdb_pw, 
                          dbname = ifdb)

# Zander. Region I database. Zander and IFDB are now merged into one db but are
# stored in separate schemas (they use the same username and password)
zprod <- "(DESCRIPTION =
    (ADDRESS = (PROTOCOL = TCP)(HOST = 10.209.0.83)(PORT = 1521))
    (CONNECT_DATA = (SERVER = DEDICATED)
      (SERVICE_NAME = DFGCFR1P.500040564.us1.internal)))"

zprod_channel <- dbConnect(drv = dbDriver('Oracle'), 
                          username = ora$zprod_user, 
                          password = ora$zprod_pw, 
                          dbname = zprod)



# Fish tickets
adfgcf <- "(DESCRIPTION =
    (ADDRESS = (PROTOCOL = TCP)(HOST = 10.209.2.34)(PORT = 1521))
    (CONNECT_DATA = (SERVER = DEDICATED)
      (SERVICE_NAME = DFGFTDBP.us1.ocm.s7134325.oraclecloudatcustomer.com)))"

adfgcf_channel <- dbConnect(drv = dbDriver('Oracle'), 
                          username = ora$adfgcf_user,
                          password = ora$adfgcf_pw,
                          # username = ora$dwprod_user, 
                          # password = ora$dwprod_pw, 
                          dbname = adfgcf)

# Data warehouse. eLandings, fish tickets, maybe tag lab data too
dwprod <- "(DESCRIPTION =
(ADDRESS = (PROTOCOL = TCP)(HOST = 10.209.2.34)(PORT = 1521))
    (CONNECT_DATA = (SERVER = DEDICATED)
      (SERVICE_NAME = DFGDWP.us1.ocm.s7134325.oraclecloudatcustomer.com)))"

dwprod_channel <- dbConnect(drv = dbDriver('Oracle'), 
                          username = ora$dwprod_user, 
                          password = ora$dwprod_pw, 
                          dbname = dwprod)

# Stat areas ----

# Stat area information by database type (includes additional info like state vs
# fed waters, effective dates for stat area codes, ADFG/NMFS mgt areas, etc.)
query <-
  " select  stat_area, database_type as stat_area_type, region, waters,
          start_date, end_date, g_mgt_area_district, g_nmfs_area

  from    dwgross.ge_stat_area "

dbGetQuery(dwprod_channel, query) -> stat_areas 

stat_areas %>% rename(STAT = STAT_AREA) -> stat_areas

# Fishery removals ----

# gef = gross earnings file. This was pitched to me by J. Shriver as the
# definitive source for historical landings records. The detailed file includes
# all sablefish (species code 710) landings, including as bycatch. If you're
# interested in which fishery landings came from, use the CFEC permit code (cfec
# group code = 'C' = Sablefish, code descriptions at
# https://www.cfec.state.ak.us/misc/FshyDesC.htm

# query <-
# " select  adfg_b_batch_year as year, adfg_h_date_landed as date_landed,
#           adfg_h_gear_code as gear_code, adfg_h_port as port_code, 
#           cfec_harvest_area, adfg_h_permit_fishery as cfec_permit, 
#           adfg_i_delivery_code as delivery_code, adfg_i_harvest_code as harvest_code, 
#           adfg_h_mgt_program_id as mgt_program_code, adfg_i_species_code as species_code, 
#           adfg_i_stat_area as stat_area, adfg_h_stat_area_type as stat_area_type, 
#           cfec_landing_status as landing_status, adfg_i_pounds as pounds, 
#           adfg_i_whole_pounds as whole_pounds
# 
#   from    dwgross.ge_gross_earnings 
# 
#   where   adfg_i_species_code = '710' "
# 
# system.time(dbGetQuery(dwprod_channel, query) -> gef ) 
# 
# 
# merge(gef, stat_areas, by = c("STAT_AREA", "STAT_AREA_TYPE")) %>% 
#   filter(G_MGT_AREA_DISTRICT == "NSEI") -> gef
# 
# write_csv(gef, paste0("data/fishery/raw_data/nseiharvest_gef_",
#                           min(gef$YEAR), "_", max(gef$YEAR), ".csv"))
# 
# # This summary will include IFQ caught in state waters. Can filter
# # that using the management program code field.
# read_csv(paste0("data/fishery/raw_data/nseiharvest_gef_",
#                 min(gef$YEAR), "_", max(gef$YEAR), ".csv"), 
#          guess_max = 50000) %>% 
#   filter(G_MGT_AREA_DISTRICT == "NSEI") %>% 
#   droplevels() %>% 
#   mutate(
#     date = ymd(as.Date(DATE_LANDED)), #ISO 8601 format
#     julian_day = yday(date),
#     # BEWARE: Landings are always entered as POUNDS and then converted to
#     # WHOLE_POUNDS using a conversion factor related to the disposition code.
#     # Prior to 1985 there was no disposition code for landings, which is why
#     # WHOLE_POUNDS is not populated prior to 1985. Here we assume fish were
#     # delivered whole prior to 1985, because that's the best we have.
#     whole_pounds = ifelse(WHOLE_POUNDS == 0, POUNDS, WHOLE_POUNDS),
#     # Aggregate gear field codes (lookup table in GEF_Variable_Descriptions....xls)
#     Gear = derivedFactor("HAL" = GEAR_CODE %in% c("61", "06"), #hook-and-line aka longline
#                               "POT" = GEAR_CODE %in% c("91", "09"),
#                               "TRW" = GEAR_CODE %in% c("07", "47", "27", "17"), # non-pelagic/bottom trawl, pelagic/mid-water trawl, otter trawl, or beam trawl
#                               "JIG" = GEAR_CODE %in% c("26"), # mechanical jigs
#                               "TRL" = GEAR_CODE %in% c("05", "25", "15"), # hand troll, dinglebar troll
#                               "GNT" = GEAR_CODE %in% c("03", "04", "41"), # set, drift, or sunken gillnet
#                               "UNK" = GEAR_CODE %in% c("99"))) %>% # unknown
#   select(year = YEAR, date, julian_day, Mgmt_area = G_MGT_AREA_DISTRICT, Stat = STAT_AREA,
#          Waters = WATERS, Port = PORT_CODE,  Cfec_permit = CFEC_PERMIT, Mgmt_cde = MGT_PROGRAM_CODE, 
#          Delivery_cde = DELIVERY_CODE, Harvest_cde = HARVEST_CODE, Landing_cde = LANDING_STATUS, 
#          Spp_cde = SPECIES_CODE, whole_pounds, pounds = POUNDS) -> gef
# 
# write_csv(gef, paste0("data/fishery/nseiharvest_gef_",
#                       min(gef$year), "_", max(gef$year), ".csv"))

# Harvest from IFDB - what managers are using. This only includes directed NSEI
# harvest (harvest_code = 43 is test fish)
query <-
  paste0(" select  year, adfg_no, trip_no, vessel_name, port_code, gear,
          catch_date, sell_date, harvest_code, harvest, g_stat_area,
          g_management_area_code, species_code, pounds, round_pounds,
          delivery_code, g_cfec_fishery_group, g_cfec_fishery

  from    out_g_cat_ticket

  where   species_code = '710' and
          harvest_code NOT IN ('43', '42') and
          g_cfec_fishery = 'C61A' and
          g_management_area_code = 'NSEI' and year = ", YEAR)

dbGetQuery(ifdb_channel, query) -> ifdb_catch

# ifdb_catch %>% group_by(YEAR) %>% dplyr::summarize(sum(ROUND_POUNDS)) %>% View()
# unique(ifdb_catch$HARVEST_CODE)

write_csv(ifdb_catch, paste0("data/fishery/raw_data/nseiharvest_ifdb_",
                      max(ifdb_catch$YEAR), ".csv"))

read_csv(paste0("data/fishery/raw_data/nseiharvest_ifdb_",
                max(ifdb_catch$YEAR), ".csv"), 
         guess_max = 50000) %>% 
  mutate(date = ymd(as.Date(CATCH_DATE)), #ISO 8601 format
         julian_day = yday(date),
         sell_date = ymd(as.Date(SELL_DATE)),
         # BEWARE: Landings are always entered as POUNDS and then converted to
         # WHOLE_POUNDS using a conversion factor related to the disposition code.
         # Prior to 1985 there was no disposition code for landings, which is why
         # WHOLE_POUNDS is not populated prior to 1985. Here we assume fish were
         # delivered whole prior to 1985, because that's the best we have.
         whole_pounds = ifelse(ROUND_POUNDS == 0, POUNDS, ROUND_POUNDS)) %>% 
  select(year = YEAR, date, julian_day, Mgmt_area = G_MANAGEMENT_AREA_CODE, Stat = G_STAT_AREA,
         Adfg = ADFG_NO, trip_no = TRIP_NO, sell_date, Vessel = VESSEL_NAME, Port = PORT_CODE,  
         Cfec_permit = G_CFEC_FISHERY, Delivery_cde = DELIVERY_CODE, 
         Harvest = HARVEST, Harvest_cde = HARVEST_CODE, Spp_cde = SPECIES_CODE, 
         whole_pounds, pounds = POUNDS) %>% 
  mutate(Stat = as.character(Stat),
         Harvest_cde = as.character(Harvest_cde)) -> ifdb_catch

# Data quieried before (that way you're using the same data that was used for
# the assessment, starting in 2017)
read_csv(paste0("data/fishery/nseiharvest_ifdb_1969_", YEAR-1, ".csv"),
         guess_max = 50000) -> past_catch

bind_rows(past_catch, ifdb_catch) -> ifdb_catch

write_csv(ifdb_catch, paste0("data/fishery/nseiharvest_ifdb_",
                             min(ifdb_catch$year), "_", max(ifdb_catch$year), ".csv"))

# catch %>% 
#   filter(G_CFEC_FISHERY %in% 
#            c(#'C09B', 'C91A', 'Z99B', 'M61B', 'C61C', 'B06B', 
#              'C06B', 'C06A', 'B61B', '0000', 'C61B', 'C61A')) %>%
#   group_by(YEAR, G_CFEC_FISHERY) %>% 
#   dplyr::summarise(pounds = sum(POUNDS),
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
# Johnson. Scott runs a series of Martina Kallenburger's sql scripts
# (https://github.com/commfish/seak_sablefish/issues/7) that match fish
# tickets pounds back to set based on Kamala's set target designation based on
# some undocumented methodology (proportional to numbers/pounds in logbook?). It
# lives in a view called sablefish_cpue_final_view in scottj's IFDB schema,
# though he has made a public synonym for it. This output doesn't contain
# information on sets designated as halibut targets. Note that it is missing
# effort_no's (effort_no's = individual sets).

query <- 
  paste0(" select  year, project_code, trip_no, adfg_no, longline_system_code, sell_date, 
            hook_size, hook_spacing, number_of_skates, number_of_hooks,
            average_depth_meters, g_management_area_code, g_stat_area, trip_target, set_target,
            effort_no, sable_lbs_per_set, time_set, time_hauled, 
            start_latitude_decimal_degrees, start_longitude_decimal_degree

    from    sablefish_cpue_final_view

    where   g_management_area_code = 'NSEI' and year = ", YEAR)

# FYI, the final view is produced by the following query:
# query <- 
#   paste0(" select   s.*, e.time_set, e.time_hauled, e.start_latitude_decimal_degrees, e.start_longitude_decimal_degree
#            from     scottj.out_g_log_longline_prop_54 s, ifdb.g_log_longline_effort e   
#            where    s.year = 2019 and s.set_target = '710' and s.year = e.year
#                     and s.project_code = e.project_code and s.trip_no = e.trip_no and s.effort_no = e.effort_no")


dbGetQuery(ifdb_channel, query) -> fsh_eff

write_csv(fsh_eff, paste0("data/fishery/raw_data/fishery_cpue_",
                          max(fsh_eff$YEAR), ".csv"))

read_csv(paste0("data/fishery/raw_data/fishery_cpue_",
                max(fsh_eff$YEAR), ".csv"), 
         guess_max = 50000) %>% 
  # rename, define factors, remove mixed hook sizes; calculate stanardized no. of 
  # hooks and cpue
  mutate(date = ymd(as.Date(TIME_SET)), #ISO 8601 format
         julian_day = yday(date),
         soak = difftime(TIME_HAULED, TIME_SET, units = "hours"),
         Gear = factor(LONGLINE_SYSTEM_CODE),
         Hook_size = as.character(HOOK_SIZE), 
         hook_space = HOOK_SPACING, #*FLAG* - check that hook_space is in inches
         Size = factor(as.numeric(gsub("[^0-9]", "", Hook_size))),
         no_hooks = NUMBER_OF_HOOKS,
         sable_lbs_set = SABLE_LBS_PER_SET) %>% 
  select(year = YEAR, trip_no = TRIP_NO, Adfg = ADFG_NO, Spp_cde = TRIP_TARGET, date, julian_day, 
         soak, Gear = LONGLINE_SYSTEM_CODE, Hook_size, Size, 
         hook_space, Stat = G_STAT_AREA, no_hooks, depth = AVERAGE_DEPTH_METERS, 
         sets = EFFORT_NO, sable_lbs_set, start_lat = START_LATITUDE_DECIMAL_DEGREES,
         start_lon = START_LONGITUDE_DECIMAL_DEGREE) -> fsh_eff

# Data quieried before (that way you're using the same data that was used for
# the assessment, starting in 2017)
read_csv(paste0("data/fishery/fishery_cpue_1997_", YEAR-1, ".csv"), 
         guess_max = 50000) %>% 
  mutate(Size = as.character(Size)) -> past_fsh_eff

bind_rows(past_fsh_eff, fsh_eff) -> fsh_eff

write_csv(fsh_eff, paste0("data/fishery/fishery_cpue_",
                   min(fsh_eff$year), "_", max(fsh_eff$year), ".csv"))

# Fishery biological ----

# Fishery and pot survey bio data still come from IFDB, ZPROD official source
# for longline survey data

# Before 2019 I was accidentally including age readability codes > 3. I repulled
# all the data in 2019. Kevin McNeel (Age Determination Unit) 20190116: only
# code 1-3 should be used for analyses (see Issue #33)

query <-
  paste0(" select  year, project_code, trip_no, adfg_no, vessel_name, sell_date, g_stat_area,
          g_management_area_code, sample_type, species_code, length_type_code, 
          length_type, length_millimeters / 10 as length, weight_kilograms,
          age, age_readability_code, age_readability, sex_code, 
          maturity_code, maturity, gear_code, gear

  from    out_g_bio_age_sex_size

  where   species_code = '710' and
          project_code in ('02', '17') and
          age_readability_code in ('01', '02', '03') and
          g_management_area_code = 'NSEI'and year = ", YEAR)

dbGetQuery(ifdb_channel, query) -> fsh_bio

write_csv(fsh_bio, paste0("data/fishery/raw_data/fishery_bio_", 
                 max(fsh_bio$YEAR), ".csv"))

read_csv(paste0("data/fishery/raw_data/fishery_bio_",
                max(fsh_bio$YEAR), ".csv"), 
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
         length = LENGTH, weight = WEIGHT_KILOGRAMS,
         age = AGE, Sex, Maturity) %>% 
  mutate(Adfg = as.character(Adfg)) -> fsh_bio

# Data quieried before (that way you're using the same data that was used for
# the assessment, starting in 2017). This was updated again in 2019 due to the
# age readability code issue.
read_csv(paste0("data/fishery/fishery_bio_2000_", YEAR-1, ".csv"), 
         guess_max = 50000) %>% 
  mutate(Maturity = as.character(Maturity)) -> past_fsh_bio

bind_rows(past_fsh_bio, fsh_bio) -> fsh_bio

write_csv(fsh_bio, paste0("data/fishery/fishery_bio_", 
                          min(fsh_bio$year), "_", max(fsh_bio$year), ".csv"))

# Longline survey cpue ----

# There are two longline survey CPUE views: For CPUE you want
# output.out_g_sur_longline_hook_acc_bi. out_g_sur_longline_hook_acc_bi sums the
# total number of sablefish while out_g_sur_longline_catch_bi splits out by
# retained, lost, discard, etc. For the 2017 assessment (2018 forecast) I used
# the correct view for CPUE calculation but the incorrect one in the
# mark-recapture models #3 and #4. These were not used for management, and the
# code has been corrected for the 2018 assessments. The other view is used for
# mark-recapture purposes because all retained fish are checked for marks/tags.

query <- 
  paste0(" select  year, project_code, trip_no, target_species_code, adfg_no, vessel_name, 
          time_second_anchor_overboard as time_set, time_first_buoy_onboard as time_hauled,
          number_of_stations, hooks_per_set, hook_size, 
          hook_spacing_inches, sample_freq, last_skate_sampled, effort_no, station_no,
          g_stat_area as stat, start_latitude_decimal_degrees as start_lat,
          start_longitude_decimal_degree as start_lon, end_latitude_decimal_degrees as end_lat,
          end_longitude_decimal_degrees as end_lon, avg_depth_fathoms * 1.8288 as depth_meters, 
          number_hooks, bare, bait, invalid, sablefish, 
          subset_condition_code

  from    output.out_g_sur_longline_hook_acc_bi
  
  where   project_code in ('603', '03') and year = ", YEAR)


dbGetQuery(zprod_channel, query) -> srv_eff

# srv_eff %>% filter(is.na(SABLEFISH)) %>% View()

write_csv(srv_eff, paste0("data/survey/raw_data/llsrv_cpue_", YEAR, ".csv"))

read_csv(paste0("data/survey/raw_data/llsrv_cpue_", YEAR, ".csv"), 
         guess_max = 50000) %>% 
  filter(YEAR <= YEAR) %>% #the programmers have some dummy data in the db for the upcoming year
  mutate(date = ymd(as.Date(TIME_SET)), #ISO 8601 format
         julian_day = yday(date)) %>% 
  select(year = YEAR, Project_cde = PROJECT_CODE, Station_no = STATION_NO,
         trip_no = TRIP_NO, Adfg = ADFG_NO, Vessel = VESSEL_NAME, date, julian_day,
         Stat = STAT,  set = EFFORT_NO, start_lat = START_LAT, start_lon = START_LON, end_lat = END_LAT,
         end_lon = END_LON, depth = DEPTH_METERS, no_hooks = NUMBER_HOOKS, hooks_bare = BARE,
         hooks_bait = BAIT, hook_invalid = INVALID, hooks_sablefish = SABLEFISH,
         subset_condition_cde = SUBSET_CONDITION_CODE) -> srv_eff

# Past finalize data
read_csv(paste0("data/survey/llsrv_cpue_1985_", YEAR-1, ".csv"), 
         guess_max = 50000) -> past_srv_eff

bind_rows(past_srv_eff, srv_eff) -> srv_eff

write_csv(srv_eff, paste0("data/survey/llsrv_cpue_", min(srv_eff$year), "_", max(srv_eff$year), ".csv"))

# LL srv CPUE v2 ----

# 2020-01-28: Attempt to do a better job cleaning up the data based on comments.
# Invalidate: skates with >= 12 invalid hooks (should be done already), large
# snarls, clothelined skates, control for bycatch, sleeper sharks

# year, project_code, trip_no, target_species_code, adfg_no, vessel_name, 
# time_second_anchor_overboard as time_set, time_first_buoy_onboard as time_hauled,
# number_of_stations, hooks_per_set, hook_size, 
# hook_spacing_inches, sample_freq, last_skate_sampled, effort_no, station_no,
# g_stat_area as stat, start_latitude_decimal_degrees as start_lat,
# start_longitude_decimal_degree as start_lon, end_latitude_decimal_degrees as end_lat,
# end_longitude_decimal_degrees as end_lon, avg_depth_fathoms * 1.8288 as depth_meters, 
# number_hooks, bare, bait, invalid, sablefish, 
# subset_condition_code, trip_comments, trip_design_comment, effort_comment, subset_comments

query <- 
  paste0(" select  *

  from    output.out_g_sur_longline_hook_acc_bi
  
  where   project_code in ('603', '03')")

dbGetQuery(zprod_channel, query) -> srv_eff

# names(srv_eff)
write_csv(srv_eff, paste0("data/survey/raw_data/llsrv_cpue_v2_", min(srv_eff$YEAR), "_",
                          max(srv_eff$YEAR), ".csv"))

read_csv(paste0("data/survey/raw_data/llsrv_cpue_v2_", min(srv_eff$YEAR), "_",
                          max(srv_eff$YEAR), ".csv"), 
         guess_max = 50000) %>% 
  filter(YEAR <= YEAR) %>% #the programmers have some dummy data in the db for the upcoming year
  mutate(date = ymd(as.Date(TIME_SECOND_ANCHOR_OVERBOARD)), #ISO 8601 format
         julian_day = yday(date),
         soak = difftime(TIME_FIRST_ANCHOR_ONBOARD, TIME_SECOND_ANCHOR_OVERBOARD, units = "hours"),
         slope = abs(START_DEPTH_FATHOMS - END_DEPTH_FATHOMS) * 1.8288, # slope of the set
         depth = AVG_DEPTH_FATHOMS * 1.8288) %>% # depth in meters
  select(year = YEAR, Project_cde = PROJECT_CODE, Adfg = ADFG_NO, Vessel = VESSEL_NAME, 
         Station_no = STATION_NO, trip_no = TRIP_NO, set = EFFORT_NO, skate = SUBSET_NO,
         date, julian_day, soak, depth, slope,
         Stat = G_STAT_AREA, area_description = AREA_DESCRIPTION,
         start_lat = START_LATITUDE_DECIMAL_DEGREES, start_lon = START_LONGITUDE_DECIMAL_DEGREE, 
         end_lat = END_LATITUDE_DECIMAL_DEGREES, end_lon = END_LONGITUDE_DECIMAL_DEGREES,
         skate_condition_cde = SUBSET_CONDITION_CODE, bait_cde = BAIT_CODE, 
         trip_comments = TRIP_COMMENTS, set_comments = EFFORT_COMMENT, skate_comments = SUBSET_COMMENTS,
         no_hooks = NUMBER_HOOKS, bare = BARE, bait = BAIT, invalid = INVALID, 
         sablefish = SABLEFISH, halibut = HALIBUT, idiot = IDIOT, 
         shortraker = SHORTRAKER, rougheye = ROUGHEYE, skate_general = SKATE_GENERAL,
         longnose_skate = SKATE_LONGNOSE, big_skate = SKATE_BIG, sleeper_shark = SLEEPER_SHARK) -> srv_eff

write_csv(srv_eff, paste0("data/survey/llsrv_cpue_v2_", min(srv_eff$year), "_",
                          max(srv_eff$year), ".csv"))

# Longline survey catch ----

# There is no countback for each fish on the longline survey to check for marks.
# Only tags are pulled. However, prior to 2019, it was assumed that all fish
# were checked (only discard status "01" for retained fish) and this is the view
# that was used. Jane found out about this false assumption during the 2019 longline
# survey.

query <- 
  " select  year, project_code, trip_no, target_species_code, adfg_no, vessel_name, 
time_first_buoy_onboard, number_of_stations, hooks_per_set, hook_size, 
hook_spacing_inches, sample_freq, last_skate_sampled, effort_no, station_no, species_code, 
g_stat_area as stat, number_hooks, bare, bait, invalid, hagfish_slime, unknown, numbers, discard_status_code, 
subset_condition_code

from    output.out_g_sur_longline_catch_bi

where   species_code = '710' and
project_code in ('603', '03')"
          
dbGetQuery(zprod_channel, query) -> srv_eff

# Merge in discard status codes
query <- "select * from discard_status"
dbGetQuery(ifdb_channel, query) -> discard_codes

merge(srv_eff, discard_codes, by = "DISCARD_STATUS_CODE") -> srv_eff

write_csv(srv_eff, paste0("data/survey/raw_data/llsrv_by_condition_",
                          min(srv_eff$YEAR), "_", YEAR, ".csv"))

read_csv(paste0("data/survey/raw_data/llsrv_by_condition_",
                min(srv_eff$YEAR), "_", YEAR, ".csv"), 
         guess_max = 50000) %>% 
  filter(YEAR <= YEAR) %>% #the programmers have some dummy data in the db for the upcoming year
  mutate(date = ymd(as.Date(TIME_FIRST_BUOY_ONBOARD)), #ISO 8601 format
         julian_day = yday(date)) %>% 
  select(year = YEAR, Project_cde = PROJECT_CODE, 
         trip_no = TRIP_NO, Adfg = ADFG_NO, Vessel = VESSEL_NAME, date, 
         julian_day, Stat = STAT, Spp_cde = SPECIES_CODE, 
         set = EFFORT_NO, no_hooks = NUMBER_HOOKS, hooks_bare = BARE,
         hooks_bait = BAIT, hook_invalid = INVALID, hooks_sablefish = NUMBERS,
         discard_status_cde = DISCARD_STATUS_CODE, discard_status = DISCARD_STATUS) -> srv_eff

write_csv(srv_eff, paste0("data/survey/llsrv_by_condition_",
                          min(srv_eff$year), "_", YEAR, ".csv"))

# Longline survey biological ----

# Chatham Strait Longline Survey biological data. originally stored in ifdb
# under out_g_bio_effort_age_sex_size but since the development of ACES (the
# mobile app used on the survey), which was not compatible with ALEX, all bio
# data has migrated to zprod under output.out_g_sur_longline_specimen. These are
# now stored in the modern database, Zander (aka ZPROD)

# Same issue as Fishery Biological data with the age readability codes (see
# description above). Repulled all data in 2019 to strip out age readability
# codes > 3.

query <-
  paste0(" select  year, project_code, trip_no, target_species_code, adfg_no, vessel_name, 
          time_first_buoy_onboard, number_of_stations, hooks_per_set, hook_size, 
          hook_spacing_inches, sample_freq, last_skate_sampled, effort_no, station_no, species_code, 
          g_stat_area as stat, start_latitude_decimal_degrees as start_lat,
          start_longitude_decimal_degree as start_lon, end_latitude_decimal_degrees as end_lat,
          end_longitude_decimal_degrees as end_lon, avg_depth_fathoms * 1.8288 as depth_meters, 
          length_millimeters / 10 as length, weight_kilograms as weight, 
          age, age_type_code, age_readability_code, sex_code, maturity_code, otolith_condition_code

  from    output.out_g_sur_longline_specimen

  where   species_code = '710' and
          age_readability_code in ('01', '02', '03') and
          project_code in ('603', '03') ) and year = ", YEAR)

dbGetQuery(zprod_channel, query) -> srv_bio

# View doesn't have management areas, join with stat_area look up as a check
# (the project code should be enough because they're area-specific)

merge(srv_bio, stat_areas, by = "STAT") -> srv_bio

write_csv(srv_bio, paste0("data/survey/raw_data/llsrv_bio_", #min(srv_bio$YEAR), "_",
                          max(srv_bio$YEAR), ".csv"))

read_csv(paste0("data/survey/raw_data/llsrv_bio_", #min(srv_bio$YEAR), "_",
                max(srv_bio$YEAR), ".csv"), 
         guess_max = 50000) %>% 
  mutate(date = ymd(as.Date(TIME_FIRST_BUOY_ONBOARD)), #ISO 8601 format
         julian_day = yday(date),
         Sex = derivedFactor("Male" = SEX_CODE == "01",
                                                     "Female" = SEX_CODE == "02",
                                                     .default = NA),
         Maturity = derivedFactor("0" = MATURITY_CODE %in% c("01", "02"), 
                                  "1" = MATURITY_CODE %in% c("03", "04", "05", "06", "07"),
                                  .default = NA)) %>% 
  select(year = YEAR, Mgmt_area = G_MGT_AREA_DISTRICT, Project_cde = PROJECT_CODE, 
         trip_no = TRIP_NO, Adfg = ADFG_NO, Vessel = VESSEL_NAME, date, julian_day,
         Stat = STAT, Spp_cde = SPECIES_CODE, set = EFFORT_NO, start_lat = START_LAT, 
         start_lon = START_LON, end_lat = END_LAT, end_lon = END_LON, depth = DEPTH_METERS, 
         length = LENGTH, weight = WEIGHT, age = AGE, Sex, Maturity, age_type_code = AGE_TYPE_CODE, 
         age_readability = AGE_READABILITY_CODE, otolith_condition = OTOLITH_CONDITION_CODE )  %>% 
  filter(Mgmt_area == 'NSEI') -> srv_bio

read_csv(paste0("data/survey/llsrv_bio_1988_", YEAR-1, ".csv"), 
         guess_max = 50000) %>% 
  mutate(Maturity = as.character(Maturity)) -> past_srv_bio

bind_rows(past_srv_bio, srv_bio) -> srv_bio

write_csv(srv_bio, paste0("data/survey/llsrv_bio_",
                          min(srv_bio$year), "_", max(srv_bio$year), ".csv"))

# Pot survey biological ----

# The pot survey is a mark-recapture survey. Limitted bio data exists. Use this
# data for two purposes: 1) the bio data, and 2) determining the number of marks
# deployed into the fishery in a given year and which batch_no the tags are
# from.

# *FLAG* this query currently only has age-sex-size data in 2009. There's also
# an IFDB view called out_g_bio_eff_age_sex_size_tag. Asked S. Johnson
# 2018-02-16 what the difference between the two views is. He said "These two
# views are almost identical. out_g_bio_eff_age_sex_size_tag is created by the
# following SQL: 'CREATE OR REPLACE VIEW OUT_G_BIO_EFF_AGE_SEX_SIZE_TAG AS
# select *  from out_g_bio_effort_age_sex_size where length(tag_no) > 2'. This
# omits the "T-" entries which are fish that were sampled but not tagged. The
# _tag view only includes fish that were tagged.

# The out_g_bio_effort_age_sex_size view has all the biological samples for the
# pot surveys, and includes fish that were not tagged (tag_no = 'T-').

# Updated query 20200124 to include project code = 66, the experimental code
# used for the 2019 (and 2020) escape ring studies

query <- 
  paste0(" select  year, project_code, trip_no, target_species_code, adfg_no, vessel_name,
          time_first_buoy_onboard, effort_no, station_no, species_code, 
          g_stat_area as stat, management_area, start_latitude_decimal_degrees as start_lat,
          start_longitude_decimal_degree as start_lon, end_latitude_decimal_degrees as end_lat,
          end_longitude_decimal_degrees as end_lon, avg_depth_fathoms * 1.8288 as depth_meters, 
          length_millimeters / 10 as length, weight_kilograms as weight, 
          age, age_type_code, age_readability_code, sex_code, maturity_code, 
          tag_no, tag_batch_no, discard_status, release_condition_code
  
  from    out_g_bio_effort_age_sex_size

  where   species_code = '710' and
          project_code in ('11', '611', '66') and year = ", YEAR)


dbGetQuery(ifdb_channel, query) -> pot_bio

write_csv(pot_bio, paste0("data/survey/raw_data/potsrv_bio_", max(pot_bio$YEAR), ".csv"))

read_csv(paste0("data/survey/raw_data/potsrv_bio_", max(pot_bio$YEAR), ".csv"), 
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
         age_readability = AGE_READABILITY_CODE, tag_no = TAG_NO, 
         discard_status = DISCARD_STATUS, release_condition_cde = RELEASE_CONDITION_CODE )  -> pot_bio

read_csv(paste0("data/survey/potsrv_bio_1981_", YEAR-1, ".csv"), 
         guess_max = 50000) %>% 
  mutate(Maturity = as.character(Maturity)) -> past_pot_bio

bind_rows(past_pot_bio, pot_bio) -> pot_bio

# Ages for the pot data are sparse anyway but removie any age readability codes
# that aren't 01, 02, or 03 (same as llsrv and llfsh 20200124 #33)
filter(pot_bio, is.na(age_readability) | age_readability %in% c('01', '02', '03')) -> pot_bio

write_csv(pot_bio, paste0("data/survey/potsrv_bio_",
                          min(pot_bio$year), "_", max(pot_bio$year), ".csv"))

# Tag releases ----

# From the pot marking survey, includes length

# The out_g_bio_eff_age_sex_size_tag view is almost the same as the
# out_g_bio_effort_age_sex_size view, except it only stores tagged fish from the
# pot marking survey. Use this to get the number of fish marked, double check
# the release condition codes, get length frequencies of tagged fish, and cross
# reference the batch_no's with fish recovered outside of the directed fishery.
# Note that each year has a unique tag_batch_no

# Updated query 20200124 to include project code = 66, the experimental code
# used for the 2019 (and 2020) escape ring studies

# Normal query
query <- paste0(
  " select  year, project_code, trip_no, time_second_anchor_overboard, species_code, 
          g_stat_area as stat, management_area, length_millimeters / 10 as length, 
          tag_no, tag_batch_no, discard_status, release_condition_code, comments

  from    out_g_bio_eff_age_sex_size_tag

  where   species_code = '710' and project_code in ('11', '611', '66') and year = ", YEAR)


# queries used in 2019 to merge date, time, and stat area information from the
# survey effort table
query <- paste0(
  " select year, project_code, trip_no, effort_no, species_code, 
          management_area, length_millimeters / 10 as length, 
          tag_no, tag_batch_no, discard_status, release_condition_code, comments

  from    out_g_bio_eff_age_sex_size_tag

  where   species_code = '710' and project_code in ('11', '611', '66') and year = ", YEAR)

dbGetQuery(ifdb_channel, query) -> tag_releases

query <-
  paste0(
    " select distinct year, trip_no, effort_no, time_second_anchor_overboard, species_code, 
          g_stat_area as stat

  from out_g_sur_pot

  where species_code = '710' and
        year = ", YEAR)

dbGetQuery(zprod_channel, query) -> pot_effort

tag_releases %>% left_join(pot_effort, by = c('YEAR', 'TRIP_NO', 'EFFORT_NO', 'SPECIES_CODE')) -> tag_releases

# Lookup table for release condition codes "select * from g_bio_release_condition" **not sure if this column is useful
# RELEASE_CONDITION_CODE - RELEASE_CONDITION
# 00 - Unknown
# 01 - Presumed healthy
# 02 - Torn mouth
# 03 - Flea bitten
# 04 - Old injury
# 05 - Presumed dead
# 06 - No clip
# 07 - Appears undamaged but shows signs of stress (NMFS 2)
# 08 - Lacks vigor, shows ill effects from capture and handling (NMFS 3)

write_csv(tag_releases, paste0("data/survey/raw_data/tag_releases_",
                          max(tag_releases$YEAR), ".csv"))

read_csv(paste0("data/survey/raw_data/tag_releases_",
                       max(tag_releases$YEAR), ".csv"), 
         guess_max = 50000) %>% 
  mutate(date = ymd(as.Date(TIME_SECOND_ANCHOR_OVERBOARD)), #ISO 8601 format
         julian_day = yday(date)) %>% 
  select(year = YEAR, Project_cde = PROJECT_CODE, trip_no = TRIP_NO, date, julian_day,
         Stat = STAT, Mgmt_area = MANAGEMENT_AREA, length = LENGTH, tag_no = TAG_NO, tag_batch_no = TAG_BATCH_NO, 
         release_condition_cde = RELEASE_CONDITION_CODE, discard_status = DISCARD_STATUS,
         comments = COMMENTS) -> tag_releases


# Data quieried before (that way you're using the same data that was used for
# the assessment, starting in 2017)
read_csv(paste0("data/survey/tag_releases_2003_", YEAR-1, ".csv"), 
         guess_max = 50000) -> past_releases

bind_rows(past_releases, tag_releases) -> tag_releases

write_csv(tag_releases, paste0("data/survey/tag_releases_",
                               min(tag_releases$year), "_", max(tag_releases$year), ".csv"))

# Exploratory total number of marks
tag_releases %>% group_by(year, tag_batch_no) %>% dplyr::summarise(n_distinct(tag_no))

# Exploratory release code that fish "lacks vigor", but is tagged and released any way?
tag_releases %>% group_by(year, release_condition_cde, discard_status) %>% 
  dplyr::summarise(n_distinct(tag_no)) %>% filter(release_condition_cde == '08')

# Few recaptures
tag_releases %>% group_by(year, discard_status) %>% dplyr::summarise(n_distinct(tag_no))

# Tag recoveries ----

# This is the batch report that Mike Vaughn does (how we determine how many tags
# lost/not available to the directed NSEI sablefish fishery). Match up
# batch_no's to the tag_releases. Also includes recapture lengths (careful to
# only use sampler lengths)

query <- paste0(
  " select  tag_no, tag_batch_no, tag_event_code, tag_event, year, project_code, 
          trip_no, species_code, landing_date, catch_date, g_management_area_code, 
          g_stat_area, g_stat_area_group, vessel_type, length_millimeters / 10 as length, measurer_type, 
          information_source, tag_returned_by_type, comments

  from    out_g_bio_tag_recovery

  where   species_code = '710' and year = ", YEAR)

dbGetQuery(ifdb_channel, query) -> tag_recoveries

write_csv(tag_recoveries, paste0("data/fishery/raw_data/tag_recoveries_",
                               max(tag_recoveries$YEAR), ".csv"))

read_csv(paste0("data/fishery/raw_data/tag_recoveries_",
                max(tag_recoveries$YEAR), ".csv"), 
         guess_max = 50000) %>% 
  mutate(landing_date = ymd(as.Date(LANDING_DATE)), #ISO 8601 format
         landing_julian_day = yday(landing_date),
         catch_date = ymd(as.Date(CATCH_DATE)), #ISO 8601 format
         catch_julian_day = yday(catch_date)) %>% 
  select(year = YEAR, Project_cde = PROJECT_CODE, trip_no = TRIP_NO, landing_date, landing_julian_day,
         catch_date, catch_julian_day, Stat = G_STAT_AREA, Mgmt_area = G_MANAGEMENT_AREA_CODE,
         Stat = G_STAT_AREA, length = LENGTH, tag_no = TAG_NO, tag_batch_no = TAG_BATCH_NO, 
         measurer_type = MEASURER_TYPE, info_source = INFORMATION_SOURCE, returned_by = TAG_RETURNED_BY_TYPE,
         comments = COMMENTS) -> tag_recoveries

# Data quieried before (that way you're using the same data that was used for
# the assessment, starting in 2017)
read_csv(paste0("data/fishery/tag_recoveries_2003_", YEAR-1, ".csv"), 
         guess_max = 50000) -> past_recoveries

bind_rows(past_recoveries, tag_recoveries) -> tag_recoveries

write_csv(tag_recoveries, paste0("data/fishery/tag_recoveries_",
                               min(tag_recoveries$year), "_", max(tag_recoveries$year), ".csv"))

tag_recoveries %>% 
  group_by(year, info_source) %>% 
  dplyr::summarize(n_distinct(tag_no)) %>% 
  print(n = Inf)

# Fishery countbacks ----

# Daily accounting of observed fish and tag recoveries in the NSEI fishery.

# These data are not stored in the database. It's currently in heavily formatted
# spreadsheets with inconsistent data types (one column can have empty cells,
# Yes, yes, N/A, NA, etc.) in M:/SABLEFISH/CHATHAM/<year>/<year> port daily
# summary.xlxs. There are equivalent 2003 data but I don't know where they're
# stored. Asked Mike Vaughn if he'd look for them on 2018-02-16. It has been
# confirmed that these data are lost. I worked with Amy Jo Linsley in PB to
# clean these up and convert them to csvs, but there were lots of errors (e.g.
# missing values that could have been filled in using fish ticket or logbook
# data, incorrect trip_nos, or duplicate records). These were all fixed by hand.
# It's also impossible to tell which records were omitted for analysis in a
# given year, which is probably why KVK's numbers don't match up with past
# numbers and won't match up with mine either.

# Follow up Feb 2018: The spreadsheet was in poor shape again this year. I have
# followed up several times about 2003 data and can confirm now that it is lost.
# I've impressed on A. Olson the importance that these data be entered and
# stored in a safer way.

# Follow up Dec 2019 with Rhea Ehresmann (new GFP leader): Goal to move these to
# Zander in 2020. M. Vaughn also indicated there are countback data going back
# to 1997.

# Now that these data are finalized, add on each year:
read_csv(paste0("data/fishery/raw_data/nsei_daily_tag_accounting_", YEAR, ".csv"),
         guess_max = 50000) %>% 
    mutate(date = ymd(as.Date(date, "%m/%d/%Y")),
           year = year(date),
           julian_day = yday(date),
           total_obs = unmarked + marked,
           whole_kg = round_lbs * 0.453592) -> counts

read_csv(paste0("data/fishery/nsei_daily_tag_accounting_2004_", YEAR-1, ".csv"),
         guess_max = 50000) -> past_counts

bind_rows(counts, past_counts) -> counts

write_csv(counts, paste0("data/fishery/nsei_daily_tag_accounting_2004_", YEAR, ".csv"))

# Historical tagging ----

# Not used, just an fyi

query <-
" select  *
  from    out_g_bio_tag_rel_rec
  where   rel_species_code = '710'"

dbGetQuery(ifdb_channel, query) -> historical

table(historical$REL_YEAR,
      historical$REL_GEAR_CODE, 
      historical$REL_MANAGEMENT_AREA)
