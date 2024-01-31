# Clean data 
# Author: Jane Sullivan
# Contact: jane.sullivan@noaa.gov
# Last edited: Feb 2021

# R version 3.6.3 (2020-02-29)
# Platform: x86_64-w64-mingw32/x64 (64-bit)

# R version 4.0.2 (2020-06-22) 
# Platform: x86_64-w64-mingw32/x64 (64-bit)

# In 2022 and 2023 we moved to querying data directly from OceanAK.  In 2022 I (phil)
# reworked code and was provided the data by Justin Dailt and based on Jane's querries.
# In 2023 Justin set up the OceanAK querries so that the user (me) has 
# access and can modify (update year) and download.  This should vastly speed up
# this whole process.  

# OceanAK: Chatham Sablefish Assessment
# https://oceanak.dfg.alaska.local/analytics/saw.dll?Dashboard&PortalPath=%2Fshared%2FCommercial%20Fisheries%2FRegion%20I%2FGroundFish%2FUser%20Reports%2FQueries%20for%20Chatham%20Sablefish%20Assessment%2FDDR%2FChatham%20Sablefish%20Assessment&page=page%201
# This will match up with data pulls for every section below EXCEPT #2 which we
# developed our own OceanAK querry and script to deal with.  See details below.

# Note 20210127: I had to modify code to bring date/time fields in, i.e.
# ymd(parse_date_time(CATCH_DATE, c("%m/%d/%Y %H:%M:%S %p"))). If this ever
# happens again, which it probably will because date/times are notoriously
# annoying in R, keep in mind that your end goal is to get the date into ISO
# 8601 format or YEAR-MONTH-DAY (e.g. 2021-01-27).

# All final columns formatted as follows unless otherwise specified:
# length: fork length, cm
# weight: kg
# Maturity: "0" = immature, "1" = mature
# depth: average depth, meters
# date: ISO 8601 YYYY/MM/DD
# characters & factors - first letter capitilized (e.g. 'Sex'), otherwise lowercase
# lat and lon are in decimal degrees
# catch - varies between whole_pounds or whole_kg depending on what its being used for

# most recent year of data
YEAR <- 2023

# Load ----
source("r_helper/helper.r")
source("r_helper/functions.r")

# 1. Fishery harvest ----

# Harvest from IFDB - what managers are using. This only includes directed NSEI
# harvest (harvest_code = 43 is test fish)

read.csv(paste0(YEAR+1,"/data/fishery/raw_data/nseiharvest_ifdb_",
                     YEAR, ".csv")) %>%
#str(Dat)
#ifdb_catch<-Dat %>% 
  mutate(date = ymd(parse_date_time(CATCH_DATE, c("%Y-%m-%d %H:%M:%S"))), #ISO 8601 format
         julian_day = yday(date),
         sell_date = ymd(parse_date_time(SELL_DATE, c("%Y-%m-%d %H:%M:%S"))),
         whole_pounds = ifelse(ROUND_POUNDS == 0, POUNDS, ROUND_POUNDS)) %>% 
  select(year = YEAR, date, julian_day, Mgmt_area = G_MANAGEMENT_AREA_CODE, Stat = G_STAT_AREA,
         Adfg = ADFG_NO, trip_no = TRIP_NO, sell_date, Vessel = VESSEL_NAME, Port = PORT_CODE,  
         Cfec_permit = G_CFEC_FISHERY, Delivery_cde = DELIVERY_CODE, 
         Harvest = HARVEST, Harvest_cde = HARVEST_CODE, Spp_cde = SPECIES_CODE, 
         whole_pounds, pounds = POUNDS) %>% 
  mutate(Stat = as.character(Stat),
         Harvest_cde = as.character(Harvest_cde),
         Adfg = as.character(Adfg),
         Delivery_cde = as.character(Delivery_cde)) -> ifdb_catch

# Data quieried before (that way you're using the same data that was used for
# the assessment, starting in 2017)
read_csv(paste0("legacy_data/fishery/nseiharvest_ifdb_1969_", YEAR-1, ".csv"),
         guess_max = 50000) -> past_catch

bind_rows(past_catch, ifdb_catch) -> ifdb_catch
unique(ifdb_catch$year)
#save full '69 through this year for SCAA
write_csv(ifdb_catch, paste0(YEAR+1,"/data/fishery/nseiharvest_ifdb_",
                             min(ifdb_catch$year), "_", max(ifdb_catch$year), ".csv"))

write_csv(ifdb_catch, paste0("legacy_data/fishery/nseiharvest_ifdb_",
                             min(ifdb_catch$year), "_", max(ifdb_catch$year), ".csv"))

# only use this for year >= 1985 (see fishery_catch_cpue.R for more documentation)
ifdb_catch <- ifdb_catch %>% filter(year >= 1985)
write_csv(ifdb_catch, paste0(YEAR+1,"/data/fishery/nseiharvest_ifdb_",
                             min(ifdb_catch$year), "_", max(ifdb_catch$year), ".csv"))

write_csv(ifdb_catch, paste0("legacy_data/fishery/nseiharvest_ifdb_",
                             min(ifdb_catch$year), "_", max(ifdb_catch$year), ".csv"))

#======================================================================================
# 2. Fishery cpue ----

# For past iterations of the Fishery CPUE please see the 2022 folder or the original
# analysis in branch: seak_sablefish_thru2021_original_JS

# Starting in 2023 we set up the CPUE calculations and data using OceanAK queries
# detailed in script: fishery_cpue_fr_OceanAK_ftx_lb_dat.R.  The query criteria
# is described in that script and running that code will provide the CPUE data
# for 1997 through today.  That data is stored in the legacy data folder and will be
# pulled here and cleaned up for use in the assessment. 
#
# This CPUE calculation has replaced the CPUE data that was being used prior to 
# 2021 and involved an undocumented series of transformations that was meant to deal
# with inaccurate recording of what fishermen were targeting (halibut and sablefish).
# The old methods ascribed target based on relative catch of species.  Rhea Ehresmann 
# (groundfish biologist) reworked ad entered the logbooks to accurately ascribe 
# target so the new methods should provide a clearer and more accurate description 
# of fishery CPUE that will also allow for a more detailed analysis in 2023 and 
# going forward as the fishery transitions from longline to pots.  For more 
# information please see the notes at the head of fishery_cpue_fr_OceanAK_ftx_lb_dat.R.
#
# There are now two CPUE files to work with and examine: 
# 2.a will be the longline data and these CPUE values are used in the model
# 2.b will be the pot data.  2022 was the first year boats were allowed to fish this
#     gear, so 2023 will be a "first look" and comparison with longline gear.
#     As the fleet transitions to pot gear (as it's expected to do) there will be 
#     a need to use a conversion and create a single Fishery CPUE time series.
#     This will also be necessary if the ADF&G surveys also transition to pot surveys.

# 2.a Longline Fishery CPUE 

 read_csv(paste0("legacy_data/fishery/raw_data/fishery_ll_cpue_vers23_1997-",
                 YEAR,".csv",sep=""), 
                guess_max = 50000) %>% #nrow(unique(ugh))
   mutate(year = Year,
          set_date = as.Date(Time.Set, c("%Y-%m-%d %H:%M:%S"), tz="UTC"), #ISO 8601 format; tz="America/Anchorage"
          sell_date = as.Date(Sell.Date, c("%Y-%m-%d %H:%M:%S"), tz="UTC"),
          julian_day_set = yday(set_date),
          julian_day_sell = yday(sell_date),
          tm_hauled = parse_date_time(Time.Hauled, c("%Y-%m-%d %H:%M:%S")),
          t_set = parse_date_time(Time.Set, c("%Y-%m-%d %H:%M:%S")),
          set_soak = soak_time_hrs,
          set_length = set_length_km,
          
          trip_recorded_releases = ifelse(grepl("Released", trip_recorded_releases, ignore.case = TRUE),
                                          "logged_releases","no_logged_releases"),
          avg_set_depth_m = 1.8288*Average.Depth.Fathoms,
          avg_trip_depth_m = 1.8288*mean_trip_depth_fm,
          
          gear = factor(Longline.System.Code),
          gear_name = factor(Gear),
          
          #primary configuration
          hook_size = as.character(hook_size1), 
          mean_hook_size = as.numeric(mean_hook_size),
          hook_space = spacing1, #*FLAG* - check that hook_space is in inches
          size = factor(as.numeric(gsub("[^0-9]", "", hook_size1))),
          mean_size = mean_hook_size,
          no_hooks_fished_on_trip = total_hooks_all,
          no_hooks_exact = total_hooks_exact,
          no_hooks_est = total_hooks_est,
          no_hooks_p_set = total_hooks_all/set.count,
          
          no_skates_fished_on_trip = total_skate_count,
          
          #secondary configuration
          hook_size_2 = as.character(hook_size2), 
          hook_space_2 = spacing2, #*FLAG* - check that hook_space is in inches
          size_2 = factor(as.numeric(gsub("[^0-9]", "", hook_size_2))),
          no_hooks_2 = total_hooks_est,
          
          sable_lbs_set = lbs_p_set,
          sable_lbs_hook = lbs_p_hk_all,
          sable_lbs_hook_exact = lbs_p_hk_exact,
          sable_lbs_hook_est = lbs_p_hk_est) %>% 
   select(year, sell_date, set_date, julian_day_set, julian_day_sell,
          trip_no = Trip.Number.log,  #or Trip.Number_ftx?? 
          Adfg = ADFG.Number, Spp_cde = Trip.Primary.Target.Species.Code, 
          set_soak, trip_soak = total_soak_time,
          set_length, total_km_fished, 
          gear, gear_name,
          hook_size, mean_hook_size, 
          hook_space, 
          mean_hook_spacing,
          size, mean_size,
          no_hooks_set = num_of_hooks1,
          no_hooks_fished_on_trip, no_hooks_exact, no_hooks_est,
          no_hooks_p_set,
          hook_size_2, hook_space_2, size_2, no_hooks_2,
          no_skates_fished_on_trip,
          #no_skates_exact = num_of_skates1,
          #no_skates_est = num_of_skates1_calc,
          Stat = Groundfish.Stat.Area, 
          
          set_depth = avg_set_depth_m,
          trip_depth = avg_trip_depth_m,
          
          mean_trip_depth_fm,
          set_no = Effort.Number, 
          no_sets = set.count, 
          multigear_trip,
          trip_set_targets, trip_recorded_releases, multi_gear_config, 
          disposition = Disposition, set_depredation = Depredation, 
          trip_depredation = trip_depr_desc,
          p_sets_depredated = p_sets_depr, 
          catch,
          logged_no = Numbers_log, 
          logged_lbs = Pounds_log,
          
          sable_lbs_set, sable_lbs_hook, sable_lbs_hook_exact, sable_lbs_hook_est,
          lbs_p_set_km, lbs_p_set_hr, lbs_p_set_km_hr, 
          lbs_p_hk_exact, lbs_p_hk_km_exact,lbs_p_hk_hr_exact,                        
          lbs_p_hk_km_hr_exact, lbs_p_hk_est, lbs_p_hk_km_est, lbs_p_hk_hr_est, 
          lbs_p_hk_km_hr_est, lbs_p_hk_all, lbs_p_hk_km_all, lbs_p_hk_hr_all, 
          lbs_p_hk_km_hr_all, lbs_p_hk_exact2, lbs_p_hk_km_exact2, lbs_p_hk_hr_exact2, 
          lbs_p_hk_km_hr_exact2, 
          
          start_lat = Start.Latitude.Decimal.Degrees,
          start_lon = Start.Longitude.Decimal.Degrees, 
          set_target = Effort.Primary.Target.Species,
          set_target_2 = Effort.Secondary.Target.Species,
          trip_target = Trip.Primary.Target.Species,
          trip_target_2 = Trip.Secondary.Target.Species) -> ll_eff

ll_eff<-unique(ll_eff)

 #note some of the dat time stuff will have warnings because of incomplete data
 # will have to deal with NA's in analysis
 
  #Get new data from "this" year and add
 #read.csv(paste0(YEAR+1,"/data/fishery/raw_data/fishery_CPUE_", YEAR, ".csv")) -> new_fish_cpue
 
head(ll_eff)

 #bind the old and new... 
 #bind_rows(fsh_eff_new22, new_fish_cpue) -> fsh_eff_thru_now
 
 #save for 2023 assessment...
 write_csv(ll_eff, paste0(YEAR+1,"/data/fishery/fishery_ll_cpue_",
                           min(ll_eff$year), "_", max(ll_eff$year), ".csv"))
 
 #save for legacy for 2024 and forward
 write_csv(ll_eff, paste0("legacy_data/fishery/fishery_ll_cpue_",
                                 min(ll_eff$year), "_", max(ll_eff$year), ".csv"))
 
 # 2.b Pot Fishery CPUE 
 
 read_csv(paste0("legacy_data/fishery/raw_data/fishery_pot_cpue_vers23_2022-",
                 YEAR,".csv",sep=""), 
          guess_max = 50000) %>% 
    mutate(year = Year,
           set_date = as.Date(Time.Set, c("%Y-%m-%d %H:%M:%S"), tz="UTC"), #ISO 8601 format
           sell_date = as.Date(Sell.Date, c("%Y-%m-%d %H:%M:%S"), tz="UTC"),
           julian_day_set = yday(set_date),
           julian_day_sell = yday(sell_date),
           tm_hauled = parse_date_time(Time.Hauled, c("%Y-%m-%d %H:%M:%S")),
           t_set = parse_date_time(Time.Set, c("%Y-%m-%d %H:%M:%S")),
        date = as.Date(Time.Set, c("%Y-%m-%d %H:%M:%S"), tz="America/Anchorage"), #ISO 8601 format
        #julian_day = yday(date),
        tm_hauled = parse_date_time(Time.Hauled, c("%Y-%m-%d %H:%M:%S")),
        t_set = parse_date_time(Time.Set, c("%Y-%m-%d %H:%M:%S")),
        
        set_soak = soak_time_hrs,
        set_length = set_length_km,
        
        #avg_depth_m = 1.8288*Average.Depth.Fathoms,
        avg_set_depth_m = 1.8288*Average.Depth.Fathoms,
        avg_trip_depth_m = 1.8288*mean_trip_depth_fm,
        #gear = factor(Longline.System.Code),
        gear_name = factor(Gear),
        
        #primary configuration
        pot_space = 0.3048*Pot.Spacing.Feet,  #(convert to meters)
        line_diam = 2.54*Groundline.Diameter.Inches, # (convert to cm)
        pot_dim = factor(Pot.Dimensions.raw),
        pot_type = factor(Pot.Type),
        
        no_pots_fished_on_trip = total_pots,
        no_pots_p_set = total_pots/set.count,
        
        sable_lbs_set = lbs_p_set,
        sable_lbs_pot = lbs_p_pot) %>% 
   select(year, sell_date, set_date, julian_day_set, julian_day_sell,
          trip_no = Trip.Number.log,  #or Trip.Number_ftx?? 
          Adfg = ADFG.Number, 
          Spp_cde = Trip.Target.Species.Code, #date, #julian_day, 
          set_soak, trip_soak = total_soak_time,
          set_length, total_km_fished, 
          #gear, 
          gear_name, pot_data = pot_data_status,
          pot_space, line_diam, pot_dim, pot_type, 
          pot_len, pot_diam, pot_volume_m3 = Pot_Volume_m3,
          no_pots_fished_on_trip,
          no_pots_p_set,
          Stat = Groundfish.Stat.Area, 
          set_depth = avg_set_depth_m,
          trip_depth = avg_trip_depth_m,
          set_no = Effort.Number, 
          no_sets = set.count, 
          multigear_trip,
          trip_set_targets, trip_recorded_releases,  
          disposition = Disposition, # depredation = Depredation, 
          catch,
          logged_no = Numbers_log, 
          logged_lbs = Pounds_log,
          sable_lbs_set, sable_lbs_pot, 
          lbs_p_set_km, lbs_p_set_hr, lbs_p_set_km_hr, 
          lbs_p_pot, lbs_p_pot_km,lbs_p_pot_hr,                        
          lbs_p_pot_km_hr, 
          
          start_lat = Start.Latitude.Decimal.Degrees,
          start_lon = Start.Longitude.Decimal.Degrees, 
          set_target = Effort.Primary.Target.Species,
          trip_target = Trip.Target.Species) -> pot_eff
 
 nrow(pot_eff)
 nrow(unique(pot_eff))
 
pot_eff<-unique(pot_eff)

 #save for 2023 assessment...
 write_csv(pot_eff, paste0(YEAR+1,"/data/fishery/fishery_pot_cpue_",
                                 min(pot_eff$year), "_", max(pot_eff$year), ".csv"))
 
 #save for legacy for 2024 and forward
 write_csv(pot_eff, paste0("legacy_data/fishery/fishery_pot_cpue_",
                                 min(pot_eff$year), "_", max(pot_eff$year), ".csv"))
 #=====================================================================================
# 3. Fishery biological ----

# Fishery and pot survey bio data still come from IFDB, ZPROD official source
# for longline survey data

# Past documentation: Before 2019 I was accidentally including age readability
# codes > 3. I repulled all the data in 2019. Kevin McNeel (Age Determination
# Unit) 20190116: only code 1-3 should be used for analyses (see Issue #33)

##also stupid dates... had to go about it a bit differently
# OceanAK current stupidery is that exporting the file to a csv imprints a UTC time
# stamp on the date and so it thinks its in another time zone.  "Lie" to R and use
# UTC.  

read_csv(paste0(YEAR+1,"/data/fishery/raw_data/fishery_bio_", 
                YEAR, ".csv"), 
         guess_max = 50000)  %>% 
  mutate(#date = as.Date(format(parse_date_time(SELL_DATE, c("%m/%d/%Y %H:%M")),"%Y-%m-%d")), #ISO 8601 format
         date = as.Date(SELL_DATE, c("%Y-%m-%d"), tz="UTC"), #, tz="America/Anchorage"),
         julian_day = yday(date),
         Sex = derivedFactor("Male" = SEX_CODE == "01",#"01",
                             "Female" = SEX_CODE == "02",#"02",
                             .default = NA),
         Maturity = derivedFactor("0" = MATURITY_CODE %in% c("01", "02"), 
                                  "1" = MATURITY_CODE %in% c("03", "04", "05", "06", "07"),
                                  .default = NA),
         PROJECT_CODE = "02",   #new format has code as 602, but older data and legacy code ref's 02, so change here from
                                #2022 onward
         Gear = as.factor(GEAR)) %>% #derivedFactor(#"Pot" = PROJECT_CODE == "17", !!!***!!! <-add in in 2023 because now have pot gear out there!!! 
                      #        "Longline" = PROJECT_CODE == "02")) %>%  #, Should be 02 for Longline, 602 is for the survey
  select(year = YEAR, Project_cde = PROJECT_CODE, trip_no = TRIP_NO, Gear,
         Adfg = ADFG_NO, Vessel = VESSEL_NAME, date, julian_day,
         Stat = G_STAT_AREA, Mgmt_area = G_MANAGEMENT_AREA_CODE,
         Sample_type = SAMPLE_TYPE, Spp_cde = SPECIES_CODE, 
         length = LENGTH, weight = WEIGHT_KILOGRAMS,
         age = AGE, Sex, Maturity) %>%   #note; Jane not factoring gear into this piece of data... 
  mutate(Adfg = as.character(Adfg),
         Sex = as.character(Sex),
         Maturity = as.character(Maturity),
         Project_cde = as.character(Project_cde)) -> fsh_bio

 histogram(fsh_bio$length, breaks=50)
 histogram(fsh_bio$length[fsh_bio$length<120], breaks=50)
 
# Data quieried before (that way you're using the same data that was used for
# the assessment, starting in 2017). This was updated again in 2019 due to the
# age readability code issue.
read_csv(paste0("legacy_data/fishery/fishery_bio_2000_", YEAR-1, ".csv"), 
         guess_max = 50000) %>% 
  mutate(Maturity = as.character(Maturity),
         #Gear = "Longline"   #Cross this out in 2024 because data will already be formatted with mixed gear in 2023
         ) -> past_fsh_bio

bind_rows(past_fsh_bio, fsh_bio) -> fsh_bio

write_csv(fsh_bio, paste0(YEAR+1,"/data/fishery/fishery_bio_", 
                          min(fsh_bio$year), "_", max(fsh_bio$year), ".csv"))
write_csv(fsh_bio, paste0("legacy_data/fishery/fishery_bio_", 
                          min(fsh_bio$year), "_", max(fsh_bio$year), ".csv"))

#======================================================================================
# 4. Longline survey cpue ----

# 2020-01-28: (i.e. "v2") Attempt to do a better job cleaning up the data based
# on comments. Invalidate: skates with >= 12 invalid hooks (should be done
# already), large snarls, clothelined skates, control for bycatch, sleeper
# sharks. This was an ongoing effort,
# so it's worth pulling the full data set. The file is saved as "v2" because the data
# are different than past years, which were not QAQCed as thoroughly, but that can and
# probably should be changed in the future. Once these data are stable, you can modify the 
# query and R code to only pull the current year of data and merge it to past years.

# Other documentation: There are two longline survey CPUE views: For CPUE you want
# output.out_g_sur_longline_hook_acc_bi. out_g_sur_longline_hook_acc_bi sums the
# total number of sablefish while out_g_sur_longline_catch_bi splits out by
# retained, lost, discard, etc. For the 2017 assessment (2018 forecast) I used
# the correct view for CPUE calculation but the incorrect one in the
# mark-recapture models #3 and #4. These were not used for management, and the
# code has been corrected for the 2018 assessments. The other view is used for
# mark-recapture purposes because all retained fish are checked for marks/tags.
# year, project_code, trip_no, target_species_code, adfg_no, vessel_name, 
# time_second_anchor_overboard as time_set, time_first_buoy_onboard as time_hauled,
# number_of_stations, hooks_per_set, hook_size, 
# hook_spacing_inches, sample_freq, last_skate_sampled, effort_no, station_no,
# g_stat_area as stat, start_latitude_decimal_degrees as start_lat,
# start_longitude_decimal_degree as start_lon, end_latitude_decimal_degrees as end_lat,
# end_longitude_decimal_degrees as end_lon, avg_depth_fathoms * 1.8288 as depth_meters, 
# number_hooks, bare, bait, invalid, sablefish, 
# subset_condition_code, trip_comments, trip_design_comment, effort_comment, subset_comments

read_csv(paste0(YEAR+1,"/data/survey/raw_data/llsrv_cpue_1985_",
                YEAR, ".csv"), 
         guess_max = 50000) %>% 
  filter(YEAR <= YEAR) %>%
  mutate(date = as.Date(format(parse_date_time(TIME_SECOND_ANCHOR_OVERBOARD, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d")), #ISO 8601 format
         julian_day = yday(date),
         time1 = ymd_hms(parse_date_time(TIME_FIRST_ANCHOR_ONBOARD, c("%Y-%m-%d %H:%M:%S"))),
         time2 = ymd_hms(parse_date_time(TIME_SECOND_ANCHOR_OVERBOARD, c("%Y-%m-%d %H:%M:%S"))),
         soak = difftime(time1, time2,
                         units = "hours"),
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

str(srv_eff)

write_csv(srv_eff, paste0(YEAR+1,"/data/survey/llsrv_cpue_", min(srv_eff$year), "_",
                          max(srv_eff$year), ".csv"))
#write_csv(srv_eff, paste0("legacy_data/data/survey/llsrv_cpue_v2_", min(srv_eff$year), "_",
#                          max(srv_eff$year), ".csv"))
write_csv(srv_eff, paste0("legacy_data/survey/llsrv_cpue_", min(srv_eff$year), "_",
                          max(srv_eff$year), ".csv"))

#=======================================================================================
# 5. Longline survey catch ----

# There is no countback for each fish on the longline survey to check for marks.
# Only tags are pulled. However, prior to 2019, it was assumed that all fish
# were checked (only discard status "01" for retained fish) and this is the view
# that was used. Jane found out about this false assumption during the 2019 longline
# survey.

#JANE's 2020 with PHIL's 2021 MODCODE
read_csv(paste0(YEAR+1,"/data/survey/raw_data/llsrv_by_condition_1988_", YEAR, ".csv"), 
         guess_max = 50000) %>% 
  filter(YEAR <= YEAR) %>%
  mutate(date = as.Date(format(parse_date_time(TIME_FIRST_BUOY_ONBOARD, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d")),
#  mutate(date = ymd(as.Date(parse_date_time(TIME_FIRST_BUOY_ONBOARD, c("%m/%d/%Y %H:%M:%S %p")))), # ISO 8601 format
         julian_day = yday(date)) %>% 
  select(year = YEAR, Project_cde = PROJECT_CODE, 
         trip_no = TRIP_NO, Adfg = ADFG_NO, Vessel = VESSEL_NAME, date, 
         julian_day, Stat = STAT, Spp_cde = SPECIES_CODE, 
         set = EFFORT_NO, no_hooks = NUMBER_HOOKS, hooks_bare = BARE,
         hooks_bait = BAIT, hook_invalid = INVALID, hooks_sablefish = NUMBERS,
         discard_status_cde = DISCARD_STATUS_CODE, discard_status = DISCARD_STATUS) -> srv_ctc

max(srv_ctc$year); unique(srv_ctc$year)
view(srv_ctc %>% filter(year == 2022))
 
write_csv(srv_ctc, paste0(YEAR+1,"/data/survey/llsrv_by_condition_",
                          min(srv_ctc$year), "_", YEAR, ".csv"))
write_csv(srv_ctc, paste0("legacy_data/survey/llsrv_by_condition_", min(srv_ctc$year), "_",
                          max(srv_ctc$year), ".csv"))

#==============================================================================================
# 6. Longline survey biological ----

# Chatham Strait Longline Survey biological data. originally stored in ifdb
# under out_g_bio_effort_age_sex_size but since the development of ACES (the
# mobile app used on the survey), which was not compatible with ALEX, all bio
# data has migrated to zprod under output.out_g_sur_longline_specimen. These are
# now stored in the modern database, Zander (aka ZPROD)

# Same issue as Fishery Biological data with the age readability codes (see
# description above). Repulled all data in 2019 to strip out age readability
# codes > 3.

read_csv(paste0(YEAR+1,"/data/survey/raw_data/llsrv_bio_", 
                YEAR, ".csv"), 
         guess_max = 50000) %>% 
  mutate(date = date(parse_date_time(TIME_FIRST_BUOY_ONBOARD, c("%Y-%m-%d %H:%M:%S"))), #ISO 8601 format
#  mutate(date = date(parse_date_time(TIME_FIRST_BUOY_ONBOARD, c("%m/%d/%Y %H:%M:%S %p"))), #ISO 8601 format 
         julian_day = yday(date),
         Sex = derivedFactor("Male" = SEX_CODE == "01",
                             "Female" = SEX_CODE == "02",
                             .default = NA),
         Maturity = derivedFactor("0" = MATURITY_CODE %in% c("01", "02"), 
                                  "1" = MATURITY_CODE %in% c("03", "04", "05", "06", "07"),
                                  .default = NA),
         Mgmt_area = "NSEI") %>% 
  select(year = YEAR, Mgmt_area, Project_cde = PROJECT_CODE, 
         trip_no = TRIP_NO, Adfg = ADFG_NO, Vessel = VESSEL_NAME, date, julian_day,
         Stat = STAT, Spp_cde = SPECIES_CODE, set = EFFORT_NO, start_lat = START_LAT, 
         start_lon = START_LON, end_lat = END_LAT, end_lon = END_LON, depth = DEPTH_METERS, 
         length = LENGTH, weight = WEIGHT, age = AGE, Sex, Maturity, age_type_code = AGE_TYPE_CODE, 
         age_readability = AGE_READABILITY_CODE, otolith_condition = OTOLITH_CONDITION_CODE )  %>% 
  filter(Mgmt_area == 'NSEI') -> srv_bio

read_csv(paste0("legacy_data/survey/llsrv_bio_1988_", YEAR-1, ".csv"), 
         guess_max = 50000) %>% 
  mutate(Maturity = as.character(Maturity)) -> past_srv_bio

bind_rows(past_srv_bio, srv_bio) -> srv_bio

write_csv(srv_bio, paste0(YEAR+1,"/data/survey/llsrv_bio_",
                          min(srv_bio$year), "_", max(srv_bio$year), ".csv"))
write_csv(srv_bio, paste0("legacy_data/survey/llsrv_bio_",
                          min(srv_bio$year), "_", max(srv_bio$year), ".csv"))

#================================================================================================
# 7. Pot survey biological ----

# The pot survey is a mark-recapture survey. Limited bio data exists. Use this
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

#Codes

#PJ 2023: Some of the date data was corrupted so we repulled all the pot survey data
#         from OceanAK.  Will compare to the archived data when its time to analyse... 

read_csv(paste0(YEAR+1,"/data/survey/raw_data/potsrv_bio_", YEAR, ".csv"), 
         guess_max = 50000) %>% #filter(!is.na(TIME_FIRST_BUOY_ONBOARD)) %>% pull(TIME_FIRST_BUOY_ONBOARD)
  mutate(date = ymd(as.Date(TIME_FIRST_BUOY_ONBOARD)), #ISO 8601 format
         julian_day = yday(date),
         Sex = derivedFactor("Male" = SEX_CODE == "01",
                                                     "Female" = SEX_CODE == "02",
                                                     .default = NA),
         Maturity = derivedFactor("0" = MATURITY_CODE %in% c("01", "02"), 
                                  "1" = MATURITY_CODE %in% c("03", "04", "05", "06", "07"),
                                  .default = NA),
         tag_no = as.character(TAG_NO)) %>% 
  select(year = YEAR, Mgmt_area = MANAGEMENT_AREA, Project_cde = PROJECT_CODE, 
         trip_no = TRIP_NO, Adfg = ADFG_NO, Vessel = VESSEL_NAME, date, julian_day,
         Stat = STAT, Spp_cde = SPECIES_CODE, set = EFFORT_NO, 
         start_lat = START_LAT, start_lon = START_LON, end_lat = END_LAT,
         end_lon = END_LON, depth = DEPTH_METERS, length = LENGTH, weight = WEIGHT, 
         age = AGE, Sex, Maturity, age_type_code = AGE_TYPE_CODE, 
         age_readability = AGE_READABILITY_CODE, tag_no, 
         discard_status = DISCARD_STATUS, release_condition_cde = RELEASE_CONDITION_CODE )  -> pot_bio
str(pot_bio)
unique(as.numeric(pot_bio$year))  

#PJ NOTE: Date column corrupted.  Not used in analysis and just need year? 
read_csv(paste0("legacy_data/survey/potsrv_bio_1981_", YEAR-2, ".csv"), 
         guess_max = 50000) %>% 
  mutate(#Maturity = as.character(Maturity),
         date = ymd(as.Date(date)),
         Sex = as.factor(Sex),
         Maturity = as.factor(Maturity),
         age_type_code = as.logical(age_type_code),
         age_readability = as.logical(age_readability)) -> past_pot_bio_archived

str(past_pot_bio_archived)
#Given the corruption of the past data, tried pulling the old data from OceanAK
{read_csv(paste0(YEAR+1,"/data/survey/raw_data/potsrv_bio_1981_2020.csv"), 
         guess_max = 50000) %>%  #filter(!is.na(TIME_FIRST_BUOY_ONBOARD)) %>% pull(TIME_FIRST_BUOY_ONBOARD)
  filter(is.numeric(YEAR)) %>% 
  mutate(year = as.numeric(year),
         date = ymd(as.Date(TIME_FIRST_BUOY_ONBOARD)), #ISO 8601 format
         julian_day = yday(date),
         Sex = derivedFactor("Male" = SEX_CODE == "01",
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
         discard_status = DISCARD_STATUS, release_condition_cde = RELEASE_CONDITION_CODE )  -> past_pot_bio_OAk

unique(past_pot_bio_OAk$year) 

past_pot_bio_OAk<-past_pot_bio_OAk %>% 
  filter(!(year %in% c(unique(past_pot_bio_OAk$year)[15:21])))
unique(past_pot_bio_OAk$year)
unique(past_pot_bio_archived$year)}
#Something wrong in data querry and many years missing.  Stick with the archived data.  

bind_rows(past_pot_bio_archived, pot_bio) -> pot_bio   #no new data in 21 so skipped in 22 analysis; unblock in 23 when new 22 data

# Ages for the pot data are sparse anyway but remove any age readability codes
# that aren't 01, 02, or 03 (same as llsrv and llfsh 20200124 #33)
filter(pot_bio, is.na(age_readability) | age_readability %in% c('01', '02', '03')) -> pot_bio

write_csv(pot_bio, paste0(YEAR+1,"/data/survey/potsrv_bio_",
                          min(pot_bio$year), "_", max(pot_bio$year), ".csv"))
write_csv(pot_bio, paste0("legacy_data/survey/potsrv_bio_",
                          min(pot_bio$year), "_", max(pot_bio$year), ".csv"))


#===================================================================================================
# 8. Tag releases ----

# From the pot marking survey, includes length

# The out_g_bio_eff_age_sex_size_tag view is almost the same as the
# out_g_bio_effort_age_sex_size view, except it only stores tagged fish from the
# pot marking survey. Use this to get the number of fish marked, double check
# the release condition codes, get length frequencies of tagged fish, and cross
# reference the batch_no's with fish recovered outside of the directed fishery.
# Note that each year has a unique tag_batch_no

# Updated query 20200124 to include project code = 66, the experimental code
# used for the 2019 (and 2020) escape ring studies

#PJ: no new tag releases in 2021 so this step skipped in '22 analysis (thru '21 data)
# open this up in 23 when new 22 data entered ... 
library(janitor)
read_csv(paste0(YEAR+1,"/data/survey/raw_data/tag_releases_",
                       YEAR, ".csv"), 
         guess_max = 50000)  %>%
  clean_names() %>% 
 # may need to manually change date type in Excel depending on how it gets
 # exported from database
  mutate(date = ymd(as.Date(time_second_anchor_onboard)), # ISO 8601 format
         #date = ymd(as.Date(TIME_SECOND_ANCHOR_OVERBOARD)), #ISO 8601 format
         length = length_millimeters/10, #beginning in 2022 lengths in this query are recorded in mm...
         julian_day = yday(date),
         tag_number = as.character(tag_number)) %>% 
  select(year , Project_cde = project_code, trip_no = trip_number, date, julian_day,
         Stat = groundfish_stat_area, Mgmt_area = groundfish_management_area_code, 
         length, tag_no = tag_number, tag_batch_no = tag_batch_number, 
         release_condition_cde = release_condition_code, discard_status = discard_status,
         comments = specimen_comments) -> tag_releases


# Data queried before (that way you're using the same data that was used for
# the assessment, starting in 2017)
str(past_releases); str(tag_releases)

#when was last marking survey?  In 2022 it was two years age
last_surv <- 2

read_csv(paste0("legacy_data/survey/tag_releases_2003_", YEAR-last_surv, ".csv"), 
         guess_max = 50000) -> past_releases
#for 2022 analysis

bind_rows(past_releases, tag_releases) -> tag_releases

write_csv(tag_releases, paste0(YEAR+1,"/data/survey/tag_releases_",
                               min(tag_releases$year), "_", max(tag_releases$year), ".csv"))
write_csv(tag_releases, paste0("legacy_data/survey/tag_releases_",
                               min(tag_releases$year), "_", max(tag_releases$year), ".csv"))

#this line just for 22 analysis bc no new 21 data
#tag_releases<-past_releases

# Exploratory total number of marks
tag_releases %>% group_by(year, tag_batch_no) %>% dplyr::summarise(n_distinct(tag_no))

# Exploratory release code that fish "lacks vigor", but is tagged and released any way?
tag_releases %>% group_by(year, release_condition_cde, discard_status) %>% 
  dplyr::summarise(n_distinct(tag_no)) %>% filter(release_condition_cde == '08')

# Few recaptures
tag_releases %>% group_by(year, discard_status) %>% dplyr::summarise(n_distinct(tag_no))

#==========================================================================================
# 9. Tag recoveries ----

# This is the batch report that Mike Vaughn does (how we determine how many tags
# lost/not available to the directed NSEI sablefish fishery). Match up
# batch_no's to the tag_releases. Also includes recapture lengths (careful to
# only use sampler lengths)

Dat<-read.csv(paste0(YEAR+1,"/data/fishery/raw_data/tag_recoveries_",
                     YEAR, ".csv")) %>%   
  mutate(landing_date = as.Date(format(parse_date_time(LANDING_DATE, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d")),
         landing_julian_day = yday(landing_date),
         catch_date = as.Date(format(parse_date_time(CATCH_DATE, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d")), #ISO 8601 format 
         catch_julian_day = yday(catch_date),
         PROJECT_CODE = as.character(PROJECT_CODE))%>%
  select(year = YEAR, Project_cde = PROJECT_CODE, trip_no = TRIP_NO, landing_date, landing_julian_day,
         catch_date, catch_julian_day, Stat = G_STAT_AREA, Mgmt_area = G_MANAGEMENT_AREA_CODE,
         Stat = G_STAT_AREA, length = LENGTH, tag_no = TAG_NO, tag_batch_no = TAG_BATCH_NO, 
         measurer_type = MEASURER_TYPE, info_source = INFORMATION_SOURCE, returned_by = TAG_RETURNED_BY_TYPE,
         comments = COMMENTS)

# Data quieried before (that way you're using the same data that was used for
# the assessment, starting in 2017)
read_csv(paste0("legacy_data/fishery/tag_recoveries_2003_", YEAR-1, ".csv"), 
         guess_max = 50000) -> past_recoveries

bind_rows(past_recoveries, tag_recoveries) -> tag_recoveries

write_csv(tag_recoveries, paste0(YEAR+1,"/data/fishery/tag_recoveries_",
                               min(tag_recoveries$year), "_", max(tag_recoveries$year), ".csv"))
write_csv(tag_recoveries, paste0("legacy_data/fishery/tag_recoveries_",
                                 min(tag_recoveries$year), "_", max(tag_recoveries$year), ".csv"))

tag_recoveries %>% 
  group_by(year, info_source) %>% 
  dplyr::summarize(n_distinct(tag_no)) %>% 
  print(n = Inf)

#========================================================================================
# 10. Fishery countbacks ----

# Daily accounting of observed fish and tag recoveries in the NSEI fishery.

# These data are not stored in the database. It's currently in heavily formatted
# spreadsheets with inconsistent data types (one column can have empty cells,
# Yes, yes, N/A, NA, etc.) in M:/SABLEFISH/CHATHAM/<year>/Port Sampling port daily
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
# PJ22: no countbacks in 2021... need to check on this -pj22

# For 2023 will get this data from Groundfish staff in a xlxs worksheet called
# "YEAR NSEI Daily Accounting Form by port_FINAL.xlsx" or on the shared drive
# M:\SABLEFISH\CHATHAM\2022\Port Sampling\22 NSEI Daily Accounting Form by port_FINAL.xlsx
# Will need to make scv files that match below...


read_csv(paste0(YEAR+1,"/data/fishery/raw_data/nsei_daily_tag_accounting_", YEAR, ".csv"),
         guess_max = 50000) %>% 
    mutate(unmarked = as.numeric(unmarked),
           marked = as.numeric(marked),
           trip_no = as.character(trip_no),
           tags_recovered = as.numeric(tags_recovered),
           date = as.Date(date, "%m/%d/%Y"),
           year = year(date),
           julian_day = yday(date),
           total_obs = unmarked + marked,
           whole_kg = round_lbs * 0.453592) -> fsh_counts
#view(fsh_counts)

#fsh_counts %>% filter(stringr::str_count(trip_no, "/") == 1) -> dangit
#view(dangit)

#fsh_bio %>% filter(trip_no %in% c(dangit$trip_no))
#unique(fsh_bio$trip_no[year(fsh_bio$date) == 2022])
#unique(dangit$trip_no)

read_csv(paste0(YEAR+1,"/data/survey/raw_data/llsurv_daily_tag_accounting_", YEAR, ".csv"),
         guess_max = 50000) %>% 
  mutate(unmarked = as.numeric(unmarked),
         marked = as.numeric(marked),
         trip_no = as.character(trip_no),
         tags_recovered = as.numeric(tags_recovered),
         date = as.Date(date, "%m/%d/%Y"),
         year = year(date),
         julian_day = yday(date),
         total_obs = unmarked + marked,
         whole_kg = round_lbs * 0.453592) -> srv_counts

read_csv(paste0("legacy_data/fishery/nsei_daily_tag_accounting_2004_", YEAR-2, ".csv"),
         guess_max = 50000) %>% 
  mutate(tags_recovered = NA,
         trip_no = as.character(trip_no),
         gear = "LL"   #turn this off in 2024 because archived data will be set... 
         ) -> past_counts

str(fsh_counts); str(srv_counts)

bind_rows(fsh_counts, srv_counts, past_counts) -> counts

write_csv(counts, paste0(YEAR+1,"/data/fishery/nsei_daily_tag_accounting_2004_", YEAR, ".csv"))
write_csv(counts, paste0("legacy_data/fishery/nsei_daily_tag_accounting_2004_", YEAR, ".csv"))

Project_cde %in% c("03" , "603")#while we're add it, lets take care of the survey countback data... 
srv_marks<-counts %>% filter (date < "2022-08-15" & year == YEAR) %>% 
   select(year,number_unmarked = unmarked, number_marked = marked, 
          fully_observed = observed_flag, gear, comments)
view(srv_marks)

#read_csv(paste0(YEAR+1,"/data/survey/nsei_sable_llsurvey_countbacks.csv")) -> past_srv_marks
read_csv(paste0("legacy_data/survey/nsei_sable_llsurvey_countbacks_thru_",YEAR-1,".csv")) %>%
  mutate(gear = "LL")-> past_srv_marks

bind_rows(past_srv_marks, srv_marks) -> srv_marks

write_csv(srv_marks, paste0(YEAR+1,"/data/survey/nsei_sable_llsurvey_countbacks_thru_", YEAR, ".csv"))
write_csv(srv_marks, paste0("legacy_data/survey/nsei_sable_llsurvey_countbacks_thru_", YEAR, ".csv"))

# Historical tagging ----

# Not used, just an fyi

# query <-
# " select  *
#   from    out_g_bio_tag_rel_rec
#   where   rel_species_code = '710'"
# 
# dbGetQuery(ifdb_channel, query) -> historical
# 
# table(historical$REL_YEAR,
#       historical$REL_GEAR_CODE, 
#       historical$REL_MANAGEMENT_AREA)
