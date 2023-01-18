#################################################################################
## January 2022
## Phil's scrap code for sussing out 0_clean_data.R
#################################################################################
#1) 
Dat<-read.csv(paste0("data/fishery/raw_data/nseiharvest_ifdb_",
                     YEAR, ".csv"))
head(Dat)

Dat<-Dat %>% 
  mutate(date = ymd(parse_date_time(CATCH_DATE, c("%Y-%m-%d %H:%M:%S"))), #ISO 8601 format
         julian_day = yday(date),
         sell_date = ymd(parse_date_time(SELL_DATE, c("%Y-%m-%d %H:%M:%S"))),
         whole_pounds = ifelse(ROUND_POUNDS == 0, POUNDS, ROUND_POUNDS)) %>% 
  select(year = ï..YEAR, date, julian_day, Mgmt_area = G_MANAGEMENT_AREA_CODE, Stat = G_STAT_AREA,
                Adfg = ADFG_NO, trip_no = TRIP_NO, sell_date, Vessel = VESSEL_NAME, Port = PORT_CODE,  
                Cfec_permit = G_CFEC_FISHERY, Delivery_cde = DELIVERY_CODE, 
                Harvest = HARVEST, Harvest_cde = HARVEST_CODE, Spp_cde = SPECIES_CODE, 
                whole_pounds, pounds = POUNDS) %>% 
           mutate(Stat = as.character(Stat),
                  Harvest_cde = as.character(Harvest_cde))

read_csv(paste0("data/fishery/raw_data/nseiharvest_ifdb_",
                YEAR, ".csv"), 
        guess_max = 500000) %>% 
  mutate(date = ymd(parse_date_time(CATCH_DATE, c("%Y-%m-%d %H:%M:%S"))), #ISO 8601 format
         julian_day = yday(date),
         sell_date = ymd(parse_date_time(SELL_DATE, c("%Y-%m-%d %H:%M:%S"))), #
         # BEWARE: Landings are always entered as POUNDS and then converted to
         # WHOLE_POUNDS using a conversion factor related to the disposition code.
         # Prior to 1985 there was no disposition code for landings, which is why
         # WHOLE_POUNDS is not populated prior to 1985. Here we assume fish were
         # delivered whole prior to 1985, because that's the best we have.
         whole_pounds = ifelse(ROUND_POUNDS == 0, POUNDS, ROUND_POUNDS)) %>% 
  select(year = YEAR,#ï..YEAR, #YEAR, 
         date, julian_day, Mgmt_area = G_MANAGEMENT_AREA_CODE, Stat = G_STAT_AREA,
         Adfg = ADFG_NO, trip_no = TRIP_NO, sell_date, Vessel = VESSEL_NAME, Port = PORT_CODE,  
         Cfec_permit = G_CFEC_FISHERY, Delivery_cde = DELIVERY_CODE, 
         Harvest = HARVEST, Harvest_cde = HARVEST_CODE, Spp_cde = SPECIES_CODE, 
         whole_pounds, 
         pounds = POUNDS) %>% 
  mutate(Stat = as.character(Stat),
         Harvest_cde = as.character(Harvest_cde)) -> ifdb_catch

str(past_catch,5)
str(ifdb_catch,5)
#=============================================================================================
#2) 
 read_csv(paste0("data/fishery/raw_data/fishery_cpue_",
                 YEAR, ".csv"), 
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
str(fsh_eff); unique(fsh_eff$year)
str(past_fsh_eff)
#================================================================================================
#3)
Dat<-read.csv(paste0("data/fishery/raw_data/fishery_bio_", 
                     YEAR, ".csv"))
#
{str(Dat)
unique(Dat$PROJECT_CODE)
Dat<-Dat %>% 
  mutate(date = ymd(parse_date_time(SELL_DATE, c("%Y/%m/%d %H:%M"))), #ISO 8601 format
         julian_day = yday(date))

ex<-Dat$SELL_DATE[1]; str(ex)

exp<-parse_date_time(ex, c("%m/%d/%Y %H:%M"))

ymd(exp)

ymd(as.Date(ex, format="%Y/%m/%d %H:%M"))

as.Date(format(parse_date_time(ex, c("%Y/%m/%d %H:%M")),"%Y/%m/%d"))
ymd(parse_date_time(ex, c("%m/%d/%Y %H:%M")))

parse_date_time(ex, c("%m/%d/%Y %H:%M"))


str(exp)
ymd(exp, truncated=2)

yday(exp)

year(ex)
day(exp)
month(exp)

exd<-as.Date(exp,"%Y-%m-%d")

expf<-format(exp,"%Y-%m-%d")
str(expf)

expfd<-as.Date(expf)
str(expfd)

Dat<-Dat %>% 
  mutate(date = as.Date(format(parse_date_time(SELL_DATE, c("%m/%d/%Y %H:%M")),"%Y-%m-%d")), #ISO 8601 format
         julian_day = yday(date))
}

unique(Dat$SEX_CODE)
with(Dat,table(SEX_CODE))
Dat<-Dat %>% 
  mutate(date = format(parse_date_time(SELL_DATE, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d"), #ISO 8601 format
         julian_day = yday(date),
         Sex = derivedFactor("Male" = SEX_CODE == "01",
                             "Female" = SEX_CODE == "02",
                             .default = NA),
         Maturity = derivedFactor("0" = MATURITY_CODE %in% c("01", "02"), 
                                  "1" = MATURITY_CODE %in% c("03", "04", "05", "06", "07"),
                                  .default = NA),
         Gear = "Longline") %>% #,
  select(year = ï..YEAR, Project_cde = PROJECT_CODE, trip_no = TRIP_NO, 
            Adfg = ADFG_NO, Vessel = VESSEL_NAME, date, julian_day,
            Stat = G_STAT_AREA, Mgmt_area = G_MANAGEMENT_AREA_CODE,
            Sample_type = SAMPLE_TYPE, Spp_cde = SPECIES_CODE, 
            length = LENGTH, weight = WEIGHT_KILOGRAMS,
            age = AGE, Sex, Maturity) %>% 
  mutate(Adfg = as.character(Adfg))

str(Dat); unique(Dat$PROJECT_CODE)

ymd(exp)

ymd(parse_date_time(ex, c("%Y-%m-%d %H:%M:%S")))

read_csv(paste0("data/fishery/raw_data/fishery_bio_", 
                YEAR, ".csv"), 
         guess_max = 50000) %>% 
  mutate(date = format(as.Date(parse_date_time(SELL_DATE, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d")), #ISO 8601 format
         julian_day = yday(date),
         Sex = derivedFactor("Male" = SEX_CODE == "01",
                             "Female" = SEX_CODE == "02",
                             .default = NA),
         Maturity = derivedFactor("0" = MATURITY_CODE %in% c("01", "02"), 
                                  "1" = MATURITY_CODE %in% c("03", "04", "05", "06", "07"),
                                  .default = NA),
         Gear = "Longline") %>% #,
  select(year = YEAR, Project_cde = PROJECT_CODE, trip_no = TRIP_NO, 
         Adfg = ADFG_NO, Vessel = VESSEL_NAME, date, julian_day,
         Stat = G_STAT_AREA, Mgmt_area = G_MANAGEMENT_AREA_CODE,
         Sample_type = SAMPLE_TYPE, Spp_cde = SPECIES_CODE, 
         length = LENGTH, weight = WEIGHT_KILOGRAMS,
         age = AGE, Sex, Maturity) %>% 
  mutate(Adfg = as.character(Adfg)) -> fsh_bio

str(fsh_bio)
fsh_bio$date<-as.Date(fsh_bio$date)
fsh_bio$Sex<-as.character(fsh_bio$Sex)
fsh_bio$Maturity<-as.character(fsh_bio$Maturity)
fsh_bio$Project_cde<-as.character(fsh_bio$Project_cde)
str(past_fsh_bio)

tst<-bind_rows(past_fsh_bio, fsh_bio)

Dat<-read.csv(paste0("data/fishery/fishery_bio_2000_2021.csv"))
str(Dat)
unique(Dat$Project_cde)
nrow(Dat[Dat$Project_cde==2,]); nrow(Dat[Dat$Project_cde==602,])
unique(Dat$Gear)
#================================================================================

Dat<-read.csv(paste0("data/survey/raw_data/llsrv_cpue_v2_1985_",
                     YEAR, ".csv"))
str(Dat)

Dat<-Dat %>% 
  filter(YEAR <= YEAR) %>%
  mutate(date = as.Date(format(parse_date_time(ex, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d")), #ISO 8601 format
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
         longnose_skate = SKATE_LONGNOSE, big_skate = SKATE_BIG, sleeper_shark = SLEEPER_SHARK)

ex<-Dat$TIME_FIRST_ANCHOR_ONBOARD[1]
exp<-parse_date_time(ex, c("%Y-%m-%d %H:%M:%S"))

str(exp)
ymd_hms(exp)
yday(exp)
year(exp)
day(exp)
month(exp)

format(exp,"%Y-%m-%d"); str(format(exp,"%Y-%m-%d"))
expd<-as.Date(format(parse_date_time(ex, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d"))
str(expd)

Dat$soak[1:20]

read_csv(paste0("data/survey/raw_data/llsrv_cpue_v2_1985_",
                YEAR, ".csv"), 
         guess_max = 50000) %>% 
  filter(YEAR <= YEAR) %>%
  mutate(date = as.Date(format(parse_date_time(ex, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d")), #ISO 8601 format
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

#================================================================================================
#PJ: new data set (1/22) is 01-21, will need to download two data sets, get rid of redundancies 
# and bind together to get 88-21...
#CORRECTION -> Top date is 2001 but all data through '88 is there so no need to import 2 files
# and bind them together
Dat<-read.csv(paste0("data/survey/raw_data/llsrv_by_condition_1988_", YEAR, ".csv")); str(Dat)

ex<-Dat$TIME_FIRST_BUOY_ONBOARD[1]
exp<-parse_date_time(ex, c("%Y-%m-%d %H:%M:%S"))
format(exp,"%Y-%m-%d"); str(format(exp,"%Y-%m-%d"))
expd<-as.Date(format(parse_date_time(ex, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d"))
str(expd)

read_csv(paste0("data/survey/raw_data/llsrv_by_condition_1988_", YEAR, ".csv"), 
         guess_max = 50000) %>% 
  filter(YEAR <= YEAR) %>%
  mutate(date = as.Date(format(parse_date_time(ex, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d")), # ISO 8601 format
         julian_day = yday(date)) %>% 
  select(year = YEAR, Project_cde = PROJECT_CODE, 
         trip_no = TRIP_NO, Adfg = ADFG_NO, Vessel = VESSEL_NAME, date, 
         julian_day, Stat = STAT, Spp_cde = SPECIES_CODE, 
         set = EFFORT_NO, no_hooks = NUMBER_HOOKS, hooks_bare = BARE,
         hooks_bait = BAIT, hook_invalid = INVALID, hooks_sablefish = NUMBERS,
         discard_status_cde = DISCARD_STATUS_CODE, discard_status = DISCARD_STATUS) -> srv_eff

#=====================================================================================
Dat<-read.csv(paste0("data/survey/raw_data/llsrv_bio_", YEAR, ".csv")); str(Dat)

ex<-Dat$TIME_FIRST_BUOY_ONBOARD[1]
exp<-parse_date_time(ex, c("%Y-%m-%d %H:%M:%S"))
format(exp,"%Y-%m-%d"); str(format(exp,"%Y-%m-%d"))
expd<-as.Date(format(parse_date_time(ex, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d"))
str(expd)

exd<-date(exp); str(exd)

read_csv(paste0("data/survey/raw_data/llsrv_bio_", 
                YEAR, ".csv"), 
         guess_max = 50000) %>% 
  mutate(date = date(parse_date_time(TIME_FIRST_BUOY_ONBOARD, c("%Y-%m-%d %H:%M:%S"))), #ISO 8601 format 
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

#=================================================================================================
read_csv(paste0("data/survey/potsrv_bio_1981_", YEAR-1, ".csv"), 
         guess_max = 50000) %>% 
  mutate(Maturity = as.character(Maturity)) -> past_pot_bio
str(past_pot_bio)
str(past_pot_bio$date)
past_pot_bio$date[100000]

unique(past_pot_bio$year)
unique(past_pot_bio$date)

dat_noNA<-past_pot_bio %>% drop_na(date)

nrow(past_pot_bio)
nrow(dat_noNA)
unique(dat_noNA$date)

read_csv(paste0("data/survey/raw_data/potsrv_bio_1981_2017.csv"), 
         guess_max = 50000) %>% #filter(!is.na(TIME_FIRST_BUOY_ONBOARD)) %>% pull(TIME_FIRST_BUOY_ONBOARD)
  mutate(date = ymd(as.Date(TIME_FIRST_BUOY_ONBOARD)), #ISO 8601 format
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
         discard_status = DISCARD_STATUS, release_condition_cde = RELEASE_CONDITION_CODE )  -> pot_bio_thr17
str(pot_bio_thr17)

Dat<-read.csv("data/survey/raw_data/potsrv_bio_1981_2017.csv")
str(Dat$TIME_FIRST_BUOY_ONBOARD)
unique(Dat$YEAR)
unique(Dat$TIME_FIRST_BUOY_ONBOARD)

#===========================================================================
Dat<-read.csv(paste0("data/fishery/raw_data/tag_recoveries_",
                     YEAR, ".csv"))
str(Dat)
Dat$LANDING_DATE
ex<-Dat$LANDING_DATE[10]
exp<-parse_date_time(ex, c("%Y-%m-%d %H:%M:%S"))
date(exp)
format(exp,"%Y-%m-%d"); str(format(exp,"%Y-%m-%d"))
expd<-as.Date(format(parse_date_time(ex, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d"))
str(expd)


Dat<-Dat %>%   
  mutate(landing_date = ifelse(!is.na(LANDING_DATE),
                               as.Date(format(parse_date_time(LANDING_DATE, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d")),
                               NA))
Dat$landing_date

Dat<-Dat %>%   
  mutate(landing_date = as.Date(format(parse_date_time(LANDING_DATE, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d")),
         landing_julian_day = yday(landing_date),
         catch_date = as.Date(format(parse_date_time(CATCH_DATE, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d")), #ISO 8601 format 
         catch_julian_day = yday(catch_date),
         TAG_NO=ï..TAG_NO)%>%
  select(year = YEAR, Project_cde = PROJECT_CODE, trip_no = TRIP_NO, landing_date, landing_julian_day,
         catch_date, catch_julian_day, Stat = G_STAT_AREA, Mgmt_area = G_MANAGEMENT_AREA_CODE,
         Stat = G_STAT_AREA, length = LENGTH, tag_no = TAG_NO, tag_batch_no = TAG_BATCH_NO, 
         measurer_type = MEASURER_TYPE, info_source = INFORMATION_SOURCE, returned_by = TAG_RETURNED_BY_TYPE,
         comments = COMMENTS)

Dat$landing_date
Dat$catch_date

read_csv(paste0("data/fishery/raw_data/tag_recoveries_",
                YEAR, ".csv"), 
         guess_max = 50000) %>% #pull(CATCH_DATE)
#  mutate(landing_date = date(parse_date_time(LANDING_DATE, c("%Y-%m-%d %H:%M:%S"))), #ISO 8601 format 
  mutate(landing_date = as.Date(format(parse_date_time(LANDING_DATE, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d")),
         landing_julian_day = yday(landing_date),
         catch_date = as.Date(format(parse_date_time(CATCH_DATE, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d")), #ISO 8601 format 
         catch_julian_day = yday(catch_date)) %>%
  select(year = YEAR, Project_cde = PROJECT_CODE, trip_no = TRIP_NO, landing_date, landing_julian_day,
         catch_date, catch_julian_day, Stat = G_STAT_AREA, Mgmt_area = G_MANAGEMENT_AREA_CODE,
         Stat = G_STAT_AREA, length = LENGTH, tag_no = TAG_NO, tag_batch_no = TAG_BATCH_NO, 
         measurer_type = MEASURER_TYPE, info_source = INFORMATION_SOURCE, returned_by = TAG_RETURNED_BY_TYPE,
         comments = COMMENTS) -> tag_recoveries

str(tag_recoveries)
tag_recoveries$landing_date

str(past_recoveries)
tag_recoveries$Project_cde<-as.character(tag_recoveries$Project_cde)
bind_rows(past_recoveries, tag_recoveries) -> tag_recoveries

#=============================================================================================
# more fishery cpue exam scrap

read_csv(paste0("data/fishery/fishery_cpue_1997_", fsh_lyr,".csv"), 
         guess_max = 50000) %>% 
  filter(!is.na(hook_space) & !is.na(sable_lbs_set) & !is.na(no_hooks) &
           julian_day > 226) %>%  # if there were special projects before the fishery opened
  mutate(# standardize hook spacing (Sigler & Lunsford 2001, CJFAS), 1 m = 39.37 in
    std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (hook_space / 39.37))), 
    # convert lbs to kg
    std_cpue_kg = (sable_lbs_set * 0.453592) / std_hooks) -> fsh_cpue  
view(fsh_cpue)
str(fsh_cpue)

fsh_cpue<-read.csv(paste0("data/fishery/fishery_cpue_1997_", fsh_lyr,".csv"))

C19<-fsh_cpue[fsh_cpue$year == 2019,]
C20<-fsh_cpue[fsh_cpue$year == 2020,]
C21<-fsh_cpue[fsh_cpue$year == 2021,]

nrow(C21[is.na(C21$no_hooks),])

C19<-C19 %>% 
  filter(!is.na(hook_space) & !is.na(sable_lbs_set) &
           julian_day > 226) %>%  # if there were special projects before the fishery opened
  mutate(# standardize hook spacing (Sigler & Lunsford 2001, CJFAS), 1 m = 39.37 in
    std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (hook_space / 39.37))), 
    # convert lbs to kg
    std_cpue_kg = (sable_lbs_set * 0.453592) / std_hooks) 
nrow(C19)

C20<-C20 %>% 
  filter(!is.na(hook_space) & !is.na(sable_lbs_set) &
           julian_day > 226) %>%  # if there were special projects before the fishery opened
  mutate(# standardize hook spacing (Sigler & Lunsford 2001, CJFAS), 1 m = 39.37 in
    std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (hook_space / 39.37))), 
    # convert lbs to kg
    std_cpue_kg = (sable_lbs_set * 0.453592) / std_hooks) 
nrow(C20)

C21<-C21 %>% 
  filter(!is.na(hook_space) & !is.na(sable_lbs_set) &
           julian_day > 226) %>%  # if there were special projects before the fishery opened
  mutate(# standardize hook spacing (Sigler & Lunsford 2001, CJFAS), 1 m = 39.37 in
    std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (hook_space / 39.37))), 
    # convert lbs to kg
    std_cpue_kg = (sable_lbs_set * 0.453592) / std_hooks) 
nrow(C21)

par(mfrow=c(3,1))
hist(C19$std_cpue_kg, breaks=25, xlim=c(0,3))
hist(C20$std_cpue_kg, breaks=25, xlim=c(0,3))
hist(C21$std_cpue_kg, breaks=25, xlim=c(0,3))

mean(C19$std_cpue_kg); mean(C20$std_cpue_kg); mean(C21$std_cpue_kg)
sd(C19$std_cpue_kg)^2; sd(C20$std_cpue_kg)^2; sd(C21$std_cpue_kg)^2
se(C19$std_cpue_kg); se(C20$std_cpue_kg); se(C21$std_cpue_kg)
nrow(C21[is.na(C21$std_cpue_kg),])
nrow(C21)
C21[]

# Nominal CPUE 
C21 %>% 
  group_by(year) %>% 
  dplyr::summarise(fsh_cpue = mean(std_cpue_kg),
                   n = n(),
                   sd = sd(std_cpue_kg),
                   se = sd / sqrt(n),
                   # sigma_fsh_cpue = se / fsh_cpue, # relative standard error too low
                   # sigma_fsh_cpue = sd / fsh_cpue#, # CV too high
                   # *FLAG* currently just assume cv=0.05 for new ts, 0.1 for old
                   sigma_fsh_cpue = 0.08,  #why assume sigma when you have sd measurements which are much larger?
                   truesigma_fsh_cpue = sd^2/fsh_cpue
  ) #-> fsh_cpue 

#================
#more fishery CPUE crap - Justin's big query of 1997-2021
check<-read.csv("data/fishery/raw_data/fishery_cpue_2021.csv")
just<-read.csv("data/fishery/raw_data/fishery_cpue_1997_2021_update2022.csv")
str(just)
unique(just$YEAR)
unique(just$PROJECT_CODE)
unique(just$TRIP_NO)
unique(just$ADFG_NO)
unique(just$LONGLINE_SYSTEM)
unique(just$DATE_LEFT_PORT)
unique(just$SELL_DATE)
unique(just$HOOK_SIZE)
unique(just$HOOK_SPACING)
unique(just$HOOKS_PER_SKATE)
unique(just$AVERAGE_DEPTH_METERS)
unique(just$G_MANAGEMENT_AREA_CODE) #*** need to filter NSEI
unique(just$G_STAT_AREA)
unique(just$TRIP_TARGET)  #***need to filter for 710 sablefish for CPUE
unique(just$SET_TARGET)
unique(just$EFFORT_NO)
unique(just$SABLE_LBS_PER_SET)
unique(just$TIME_SET)
unique(just$TIME_HAULED)
unique(just$START_LATITUDE_DECIMAL_DEGREES)
unique(just$START_LONGITUDE_DECIMAL_DEGREE)

just %>% mutate(date = as.Date(TIME_SET, c("%m/%d/%Y"))) -> just
yda

unique(just$date)

just$TIME_SET<-as.Date(just$TIME_SET)

just$date

str(just)
str(just$SELL_DATE)

just$TIME_HAULED

ex<-just$TIME_SET[1]; str(ex)
unique(just$TIME_SET)

exp<-parse_date_time(ex, c("%m/%d/%Y"))
ymd(exp)

exf<-as.Date(just$TIME_SET, c("%m/%d/%Y"))
str(ymd(exf))

exf2<-parse_date_time(just$TIME_HAULED, c("%m/%d/%Y"))

ymd(as.Date(ex, format="%Y/%m/%d %H:%M"))

as.Date(format(parse_date_time(ex, c("%Y/%m/%d %H:%M")),"%Y/%m/%d"))
ymd(parse_date_time(ex, c("%m/%d/%Y %H:%M")))

parse_date_time(ex, c("%m/%d/%Y %H:%M"))

#=======

fsh_eff_new22<-read_csv(paste0("data/fishery/raw_data/fishery_cpue_1997_2021_update2022.csv"), 
                        guess_max = 50000)
fsh_eff_new22 %>% mutate(date = as.Date(TIME_SET, c("%m/%d/%Y"))) -> fsh_eff_new22

fsh_eff_new22$date
nrow(fsh_eff_new22[is.na(fsh_eff_new22$date),])

fsh_eff_new22 %>% mutate(julian_day = yday(date)) -> fsh_eff_new22

fsh_eff_new22 %>% mutate(time_hld = parse_date_time(TIME_HAULED, c("%m/%d/%Y %H:%M %p"))) -> fsh_eff_new22
nrow(fsh_eff_new22[is.na(fsh_eff_new22$time_hld),])

fsh_eff_new22 %>% mutate(t_set = parse_date_time(TIME_SET, c("%m/%d/%Y %H:%M %p"))) -> fsh_eff_new22
nrow(fsh_eff_new22[is.na(fsh_eff_new22$time_hld),])

fsh_eff_new22 %>% mutate(soak = as.numeric(difftime(time_hld, t_set, units = "hours"))) -> fsh_eff_new22
nrow(fsh_eff_new22[is.na(fsh_eff_new22$soak),])
hist(fsh_eff_new22$soak, breaks=100)
range(fsh_eff_new22$soak, na.rm=T)

nrow(fsh_eff_new22[fsh_eff_new22$soak <= 0,])
negsoak<-fsh_eff_new22[fsh_eff_new22$soak <= 0,]
head(negsoak)
view(negsoak)
ex<-negsoak[3,]

fsh_eff_new22 %>% mutate(year = YEAR) -> fsh_eff_new22
fsh_soaks<-fsh_eff_new22[!is.na(fsh_eff_new22$soak),]
str(fsh_soaks)
histos(fsh_soaks,"soak")

fsh_eff_new22 %>% mutate(Gear = factor(LONGLINE_SYSTEM_CODE)) -> fsh_eff_new22
unique(fsh_eff_new22$Gear)
warnings()

fsh_eff_new22 %>% mutate(Hook_size = as.character(HOOK_SIZE), 
                         hook_space = HOOK_SPACING, #*FLAG* - check that hook_space is in inches
                         Size = factor(as.numeric(gsub("[^0-9]", "", Hook_size))),
                         no_hooks = NUMBER_OF_HOOKS,
                         sable_lbs_set = SABLE_LBS_PER_SET) -> fsh_eff_new22

histos<-function(dat,var){    #dat<-fsh_soaks    var<-"soak"
  ys<-sort(unique(dat$year))
  coln<-3 #round(sqrt(length(ys)))
  rown<-3 #ceiling(length(ys)/coln)
  ref<-which(colnames(dat)==var)
  par(mfrow=c(rown,coln))
  for (y in ys){   #y<-ys[2]
    daty<-dat[dat$year == y,]
    nums<-as.numeric(unlist(daty[,ref]))
    if (is.na(unique(daty[,ref]))) {} else {
      hist(nums, main=y, na.rm=T)
    }
  }
}

fsh_eff_new22[is.na(fsh_eff_new22$year),]

histos(fsh_eff_new22, "depth")
