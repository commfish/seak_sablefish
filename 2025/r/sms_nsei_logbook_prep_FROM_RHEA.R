# This script is to join catch, gear, and fish ticket reports from OceanAK into the necessary formats and files
# for the 2024 NSEI stock assessment run CPUE data, as well as clean up column names, do minor calculations, and overall 
# prepare datasets for CPUE analyses 

# Author: Rhea Ehresmann
# Updated: 3/19/24

# Packages
library(pacman)
p_load(dplyr, ggplot2, tidyverse, sf, lubridate, tibble, janitor, stringr, readr, data.table)

YEAR <- 2023
#-------------------------------------------------------------------------------------------------------------
# Notes on Data and Metadata
#-------------------------------------------------------------------------------------------------------------
# Source data from OceanAK:
# The 3 logbook outputs are saved in OceanAK: /Shared Folders/Commercial Fisheries/Region 1/GroundFish/User Reports/Queries for Sablefish Assessment/DDR/Chatham Sablefish Assessment 
# and include 2. Fishery Logbooks for CPUE (longline and pot) -- fishticket_report, catch_report, and gear_report. This link should take you to it: 
## https://oceanak.dfg.alaska.local/analytics/saw.dll?Portal&PortalPath=%2Fshared%2FCommercial%20Fisheries%2FRegion%20I%2FGroundFish%2FUser%20Reports%2FQueries%20for%20Sablefish%20Assessment%2FDDR%2FChatham%20Sablefish%20Assessment

#### ATTENTION!!!!!!
## TO AVOID ISSUES WITH DATES, YOU MUST SAVE THE CSV FILES ABOVE AS "CSV (Comma delimited)" NOT "CSV UTF-8 (Comma delimited)" WHICH IS 
##  HOW THEY WILL DOWNLOAD FROM OCEANAK!!


### NOTE!!! catch_report and gear_report outputs **DO NOT** have the correct permits because the
# database is not set up to show multiple permits for those subject areas (i.e., both B halibut and C sablefish permits), so 
# only pull permit information from the fishticket_report. Basically, avoid using any fish ticket related 
# information from the Logbook-Catch and Logbook-Gear subject areas in OceanAK because it's potentially wrong or incomplete. Get 
# that info from the Logbook-Fish Ticket subject area instead. This includes sell date, ADFG, vessel name, elandings #, 
# cfec fishery codes and permit numbers, permit holder names, harvest codes, delivery codes, and disposition codes.  

### Projects in these reports are 602-Commercial LL trip, 617-Commercial Pot trip; 631-Commercial Mixed Pot/LL trip 
# use these to separate the trips by gear for CPUE analysis instead of any of the gear columns 

### Harvest codes are 11-State managed fishery, 16-Federally managed (groundfish), 18-Confiscated, 19-Forfeited catch-overage, 
# 20-Forfeited catch-trip/season limit overage, 28-Forfeited catch-bycatch overage-state managed, 40-Forfeited catch-IFQ, 
# 50-IFQ fisheries, 80-State managed groundfish, 81-Allowable overage (5% or less), 
# 82-Allowable overage transferred to another permit, 83-Allowable overage received from another permit 

### Delivery codes are 1-whole fish, 2-whole bait, 3-bled only, 4-gutted only, 5-headed & gutted, 7-headed & gutted/western cut, 
# 8-headed & gutted/eastern cut, 10-headed & gutted/tail removed, 20-fillets with skin and ribs, 32-fish meal, 
# 95-personal use/not sold, 98-discarded at sea, 99-landed discard 

### Disposition codes are 47-ticket not located, 60-sold, 61-sold for bait, 62-overage, 63-confiscated or seized, 87-retained for future sale, 
# 92-retained for bait/not sold, 95-personal use/not sold, 98-discarded at sea, 99-discard onshore/not sold, and some blanks 
# as disposition wasn't always a required field 

#-------------------------------------------------------------------------------------------------------------
# fish ticket data by trip level 
#-------------------------------------------------------------------------------------------------------------
# Shows fish ticket data joined with appropriate year, trip, and project from OceanAK report
# Notes: we don't have halibut fish tickets prior to 2006!
# Do not use STAT AREA from this fish ticket data -- always use the STAT AREA info from the gear_report from the logbook data directly

ftx <- read.csv(paste0(YEAR+1,"/data/fishery/raw_data/CPUE_dat_24_revisions/fishticket_report.csv")) %>% 
  clean_names() %>%  
  mutate(date_left_port = as.Date(date_left_port, format = "%m/%d/%Y"),
         ft_sell_date = as.Date(sell_date, format = "%m/%d/%Y"),
         year = as.numeric(year), 
         elandings_trip_num = as.character(e_landings_trip_number))%>% 
  filter(harvest_code != 43,
         stat_area %in% c("345603", "345631", "345702", # removed in 2024 because filtered out already:
                          "335701", "345701", "345731", "345803")) %>% # remove survey harvest fish tickets
  select(year, trip_number, project_code, ft_adfg_number = adfg_number, vessel_name, elandings_trip_num, ticket_number, cfec_fishery_code, 
         cfec_permit_number, permit_holder, ft_sell_date, harvest_code, delivery_code, ft_dispo_code = disposition_code,
         ft_species = species, ft_rnd_lbs = round_pounds, ft_stat = stat_area) ## do not use stat_area from fish ticket data -- it's incomplete and wrong! 

unique(ftx$ft_stat)

## longline trip fish tickets for cpue join later 
## NOTE: pulling fish tickets with sablefish only and getting rid of harvest code, delivery code, and dispo fields
## feel free to add those in or apply filters here for what you want -- I'm keeping it simple because once you add in those
## fields, your lines for each trip start stacking up and creates a mess when you join with the tidy logbook data 
ftx_ll_cpue <- ftx %>% 
  filter(ft_species == "Sablefish", 
         project_code == 602) %>% 
  select(year, trip_number, project_code, ft_adfg_number, ft_stat, vessel_name, ticket_number, cfec_fishery_code, cfec_permit_number, 
         ft_sell_date, ft_sable_rnd_lbs = ft_rnd_lbs, ft_dispo_code, harvest_code) %>%
  group_by(year,trip_number,ft_sell_date) %>%
  mutate(trip_landing = sum(ft_sable_rnd_lbs))

data.frame(ftx_ll_cpue)
## pot trip fish tickets for cpue join later 
ftx_pot_cpue <- ftx %>% 
  filter(ft_species == "Sablefish", 
         project_code == 617) %>% 
  select(year, trip_number, project_code, ft_adfg_number, ft_stat, vessel_name, ticket_number, cfec_fishery_code, cfec_permit_number, 
         ft_sell_date, ft_sable_rnd_lbs = ft_rnd_lbs, ft_dispo_code, harvest_code) 

## mixed trip fish tickets for cpue join later 
ftx_mixed_cpue <- ftx %>% 
  filter(ft_species == "Sablefish", 
         project_code == 631) %>% 
  select(year, trip_number, project_code, ft_adfg_number, ft_stat, vessel_name, ticket_number, cfec_fishery_code, cfec_permit_number, 
         ft_sell_date, ft_sable_rnd_lbs = ft_rnd_lbs, ft_dispo_code, harvest_code) 


#-------------------------------------------------------------------------------------------------------------
# catch data by effort level 
#-------------------------------------------------------------------------------------------------------------
#Unique row for each effort that includes depredation and catch for all species  

catch <- read.csv(paste0(YEAR+1,"/data/fishery/raw_data/CPUE_dat_24_revisions/catch_report.csv")) %>% 
  clean_names() %>% 
  filter(groundfish_management_area_code %in% c("345603", "345631", "345702", # removed in 2024 because filtered out already:
                                                "335701", "345701", "345731", "345803")) %>%
  select(year, trip_number, project_code, effort_number, depredation_code, depredation, number_of_skates_impacted,  
         log_species = species, log_dispo = disposition, log_numbers = numbers, log_rnd_lbs = pounds)  %>% 
  mutate(log_species = case_when(log_species == "Sablefish" ~ "sablefish", 
                                 log_species == "Halibut" ~ "halibut",
                                 log_species == "General groundfish" & log_numbers == 0 ~ "no_catch", # no catch in logbook data is entered as general groundfish 0 numbers
                                 TRUE ~ "bycatch"), # including bycatch  allows for us to keep efforts that were 0s for sablefish
         log_dispo = case_when(log_dispo == "Retained" ~ "retained",
                               log_dispo == "Released" ~ "released",
                               log_dispo == "" ~ "no_catch", 
                               TRUE ~ "released_quota_limit"),
         number_of_skates_impacted = ifelse(is.na(number_of_skates_impacted),0,number_of_skates_impacted)) %>% 
  group_by(across(c(year:log_dispo))) %>% 
  ## add up numbers and round lbs for each species/group by set
  summarise(log_numbers = sum(log_numbers), 
            log_rnd_lbs = sum(log_rnd_lbs)) %>% 
  ## spread columns to make it easier to see so each effort is one row 
  pivot_wider(names_from = c(log_species, log_dispo), values_from = c(log_numbers, log_rnd_lbs)) 

colnames(catch)
#-------------------------------------------------------------------------------------------------------------
# gear configuration data by effort level 
#-------------------------------------------------------------------------------------------------------------
# Unique row for each effort that includes gear configurations for each set as well as set/haul data and target species 

gear <- read.csv(paste0(YEAR+1,"/data/fishery/raw_data/CPUE_dat_24_revisions/gear_report.csv"), na.strings = c("", " ", "NA")) %>% 
  clean_names() %>% 
  filter(groundfish_stat_area %in% c("345603", "345631", "345702", # removed in 2024 because filtered out already:
                                     "335701", "345701", "345731", "345803")) %>%
  mutate(date_left_port = as.Date(date_left_port, format = "%m/%d/%Y"),
         year = as.numeric(year), 
         trip_target_species_code_1 = as.numeric(trip_target_species_code_1), 
         trip_target_species_code_2 = as.numeric(trip_target_species_code_2), 
         gear_target_species_code = as.character(gear_target_species_code)) %>% 
  select(year, trip_number, project_code, adfg_number, date_left_port, log_tickets = ticket, trip_target_species_code_1, trip_target_species_code_2, 
         bait, port, trip_gear, trip_gear_code, effort_number, effort_target_species_1, effort_target_species_code_1, 
         effort_target_species_code_2, effort_target_species_2, time_set, time_hauled, soak_time_hrs = soak_time_hours, 
         start_latitude_decimal_degrees, start_longitude_decimal_degrees, end_latitude_decimal_degrees, end_longitude_decimal_degrees,
         average_depth_fathoms, set_length_mi, set_length_km, log_stat_area = groundfish_stat_area, effort_gear_code, effort_gear, 
         gear_target_species_code, longline_system_code, longline_system, longline_hook_size, longline_hook_spacing_inches, 
         longline_hooks_per_skate, longline_skate_length_feet, longline_number_of_skates_set, longline_number_of_skates_lost, longline_number_of_hooks, 
         pot_groundline_diameter_inches, pot_spacing_feet, pot_number_of_pots_set, pot_number_of_pots_lost, pot_type, 
         pot_dimensions) %>%
  rename(hook_size = longline_hook_size, spacing = longline_hook_spacing_inches, hooks_per_skate = longline_hooks_per_skate,
         skate_length = longline_skate_length_feet, num_of_skates_set = longline_number_of_skates_set,
         num_of_skates_lost = longline_number_of_skates_lost, num_of_hooks = longline_number_of_hooks,
         pot_line_diam = pot_groundline_diameter_inches, pot_spacing = pot_spacing_feet, num_of_pots_set = pot_number_of_pots_set,
         num_of_pots_lost = pot_number_of_pots_lost) %>%
  ## create a column called trip_target_species to combine dual trip target species for the gear target column 
  unite(trip_target_species_combined, trip_target_species_code_1, trip_target_species_code_2, sep= ",", remove = FALSE, na.rm = TRUE) %>%  
  ## assign gear target species 1 and 2 based on trip level target species in code above because the logbook app asks for the target species of each gear 
  ## configuration unless different than at the trip level - this should probably be auto-filled in the future in OceanAK but doing manually for now    
  mutate(gear_target_species_code = case_when(is.na(gear_target_species_code) ~ trip_target_species_combined, 
                                              TRUE ~ gear_target_species_code)) %>% 
  group_by(across(c(year:effort_gear))) %>% 
  ## collapse effort data from multiple rows into one row with semi-colons separating data
  summarise(across(c(gear_target_species_code:pot_dimensions), ~ paste(.x, collapse = ";"), .names ="{.col}")) %>% 
  ## separate those data into new columns labeled _1 and _2 for all the different gear configs (there are at most 2 configs per effort) 
  separate_wider_delim(cols = c(gear_target_species_code:pot_dimensions), delim = ";", names_sep = "_", too_few = "align_start") 

colnames(gear)
unique(gear$log_stat_area)
#-------------------------------------------------------------------------------------------------------------
# join gear config and catch data by effort level so every effort is its own row -- this all logbook data now!
#-------------------------------------------------------------------------------------------------------------
# join the effort catch and effort gear dfs so that now every effort of every trip has its own row - this is all trips
all_effort_data <- gear %>% 
  left_join(catch) %>%
  group_by(year, trip_number, project_code) %>% 
  ## add column for number of sets to join with fish ticket data later 
  mutate(set.count = n_distinct(effort_number)) %>% ungroup() 

## longline trip logbook data only for cpue   
ll_log <- all_effort_data %>% 
  filter(project_code == 602) %>% 
  ## remove pot gear columns 
  select(-c(pot_line_diam_1:pot_dimensions_2)) 

## pot trip logbook data only for cpue 
pot_log <- all_effort_data %>%
  filter(project_code == 617) %>% 
  ## remove longline gear columns
  select(-c(longline_system_code_1:num_of_hooks_2))

## mixed trip logbook data only for cpue 
mixed_log <- all_effort_data %>%
  filter(project_code == 631) 
  
#-------------------------------------------------------------------------------------------------------------
# Longline logbook prep for CPUE -- right now each row = one unique effort and this code manipulates logbooks
# data based on fishery_cpue_fr_OceanAK_ftx_lb_dat.R file step #13 lines 377-421 
#-------------------------------------------------------------------------------------------------------------

log_ll_cpue <- ll_log %>% 
  ## Get numbers of hooks when num_of_hooks not available...
  ## Also calculate mean hook size for multiple hook size listings within same config (not sure if this will be useful, but...)
  ## Add in new columns for mean hooks per skate, mean hook size, and hook spacing by gear config 1 or 2 when there are multiples in each cell (e.g., 36,42)
  mutate(mean_hook_p_skate1 = sapply(strsplit(hooks_per_skate_1, ","), function(z) mean(as.numeric(z))), 
         mean_hook_p_skate2 = sapply(strsplit(hooks_per_skate_2, ","), function(z) mean(as.numeric(z))),
         mean_hook_size1 = sapply(strsplit(hook_size_1, ","), function(z) mean(as.numeric(z))),
         mean_hook_size2 = sapply(strsplit(hook_size_2, ","), function(z) mean(as.numeric(z))),
         mean_hook_spacing1 =  sapply(strsplit(spacing_1, ","), function(z) mean(as.numeric(z))),  
         mean_hook_spacing2 =  sapply(strsplit(spacing_2, ","), function(z) mean(as.numeric(z))),
         across(c("mean_hook_p_skate1", "mean_hook_size1", "mean_hook_spacing1", "mean_hook_p_skate2", "mean_hook_size2", 
                  "mean_hook_spacing2"), ~ifelse(is.nan(.), NA, .)), 
  ## If number of skates lost is NA then return the number of skates set, otherwise subtract number lost from number set to get those retrieved
         num_of_skates1 = case_when(is.na(as.numeric(num_of_skates_lost_1)) ~ as.numeric(num_of_skates_set_1),
                         TRUE ~ as.numeric(num_of_skates_set_1)-as.numeric(num_of_skates_lost_1)),
         num_of_skates2 = case_when(is.na(as.numeric(num_of_skates_lost_2)) ~ as.numeric(num_of_skates_set_2),
                         TRUE ~ as.numeric(num_of_skates_set_2)-as.numeric(num_of_skates_lost_2)),  
  ## If number of skates retrieved is not NA then return the number of skates, otherwise return number of hooks/hooks per skate
         num_of_skates1_calc = case_when(!is.na(as.numeric(num_of_skates1)) ~ as.numeric(num_of_skates1),
                               .default = as.numeric(num_of_hooks_1)/as.numeric(hooks_per_skate_1)),
         num_of_skates2_calc = case_when(!is.na(as.numeric(num_of_skates2)) ~ as.numeric(num_of_skates2),
                                .default = as.numeric(num_of_hooks_2)/as.numeric(hooks_per_skate_2)), 
  ## If number of hooks is not NA, then return the exact number of hooks, otherwise return calculated product of the number of skates * hooks per skate 
         num_of_hooks1_calc = case_when(!is.na(as.numeric(num_of_hooks_1)) ~ as.numeric(num_of_hooks_1), 
                                .default = as.numeric(num_of_skates1_calc)*as.numeric(hooks_per_skate_1)), 
         num_of_hooks2_calc = case_when(!is.na(as.numeric(num_of_hooks_2)) ~ as.numeric(num_of_hooks_2), 
                                .default = as.numeric(num_of_skates2_calc)*as.numeric(hooks_per_skate_2)),  
  ## If number of hooks is not NA, then return the exact number of hooks, otherwise return the exact number of skates * hooks per skate                             
         num_of_hooks1_exact = case_when(!is.na(as.numeric(num_of_hooks_1)) ~ as.numeric(num_of_hooks_1),
                                .default = as.numeric(num_of_skates1)*as.numeric(hooks_per_skate_1)),
         num_of_hooks2_exact = case_when(!is.na(as.numeric(num_of_hooks_2)) ~ as.numeric(num_of_hooks_2),
                                .default = as.numeric(num_of_skates2)*as.numeric(hooks_per_skate_2)),
  ## Find estimate of numbers of hooks by taking mean hooks per skate * number of skates retrieved 
         num_of_hooks1_est = as.numeric(mean_hook_p_skate1)*as.numeric(num_of_skates1), 
         num_of_hooks2_est = as.numeric(mean_hook_p_skate2)*as.numeric(num_of_skates2),
  ## If exact number of hooks is not NA, then return the exact number of hooks, otherwise return the estimated number of hooks 
         num_hooks1_cpue = case_when(!is.na(as.numeric(num_of_hooks1_exact)) ~ as.numeric(num_of_hooks1_exact),
                                .default = as.numeric(num_of_hooks1_est)),
         num_hooks2_cpue = case_when(!is.na(as.numeric(num_of_hooks2_exact)) ~ as.numeric(num_of_hooks2_exact),
                                .default = as.numeric(num_of_hooks2_est)),
  ## If exact number of hooks is not NA, then return exact, if estimated number of hooks is not NA then return estimated, otherwise return not available 
    ## There are currently 8 efforts that are missing number of hooks or hooks per skate resulting in not_available for hook_count1_qual - logbooks are missing info
         hook_count1_qual = case_when(!is.na(num_of_hooks1_exact) ~ "exact",
                                  !is.na(num_of_hooks1_est) ~ "estimated", 
                                .default = "not_available"), 
         hook_count2_qual = case_when(!is.na(num_of_hooks2_exact) ~ "exact",
                               !is.na(num_of_hooks2_est) ~ "estimated", 
                               .default = "not_available"),
  ## Gear configs now calculated or estimated, moving on to depredation and more calculations in Phil's original code lines 379-420 
  ## FLAG FOR PHIL -- we now have a field for number of skates impacted by depredation so that might be better used here as opposed to n() -- I removed n() from these 
  ## since each row is an effort now and we can return the number of skates impacted by depredation, if known. If not, then I'm not sure how you want to calculate that...
  ## all of the skates in the set? 
         dep_shar_count = ifelse(depredation == "Sleeper shark", number_of_skates_impacted, 0), 
         dep_lion_count = ifelse(depredation == "Sea lion", number_of_skates_impacted, 0),  
         dep_orca_count = ifelse(depredation == "Orca", number_of_skates_impacted, 0), 
         dep_whale_count = ifelse(depredation == "Sperm whale", number_of_skates_impacted, 0),
         dep_unk_count = ifelse(depredation == "Unknown species", number_of_skates_impacted, 0)) %>%
  group_by(year, trip_number, effort_number) %>% ## if you want to add in ADFG # or stat area, 
                ## you can do that here but I wasn't sure of purpose of this partial code -- this is still focusing on each trip/effort
  mutate(uniques = n(), 
         set_soak_time = soak_time_hrs/n(),
         set_km_fished = set_length_km/n(), 
         set_skate_count = as.numeric(num_of_skates1)/n(),
         set_hook_count_exact = as.numeric(num_of_hooks1_exact)/n(),
         set_hook_count_est = as.numeric(num_of_hooks1_est)/n(),
         set_hook_count_best_available = as.numeric(num_hooks1_cpue)/n(),
         set_hook_count1 = as.numeric(num_of_hooks1_calc)/n(),
         set_depr_count = dep_shar_count+dep_lion_count+dep_orca_count+dep_whale_count+dep_unk_count, 
         set_targets = case_when(effort_target_species_code_1 %in% c("200", "710") & effort_target_species_code_2 %in% c("200", "710") ~ "hal_&_sable_mix",
                               effort_target_species_code_1 %in% c("710", "100", "152", "169") & effort_target_species_code_2 %in% c("710", "100", "152", "169") ~ "sable_&_other_mix", 
                               .default = paste0("",unique(effort_target_species_1)))) %>% ungroup () %>% 
  group_by(year, trip_number) %>% 
  ## if depredation column is NA or blank, it means we don't have the data available in those older logbooks or the fisherman did not fill it out, 
  ## so blank or NA means "incomplete" not that there was truly no depredation - changed to return "No depredation data" 
  mutate(Depredation = ifelse(is.na(depredation),"No depredation data", depredation),
         Depredation = ifelse(depredation == "","No depredation data", depredation),   
  ## Trip set targets are now assigned in the effort_target_species_1 and effort_target_species_2 columns 
         trip_set_targets = ifelse(length(unique(set_targets))>1,  #column that can use to filter for multiple targets in a trip
                                   "multiple_targets",
                                   paste0("all_",unique(set_targets))),
         trip_recorded_releases = case_when(!is.na(log_numbers_sablefish_released) | !is.na(log_numbers_sablefish_released_quota_limit) | 
                                            !is.na(log_rnd_lbs_sablefish_released) | !is.na(log_rnd_lbs_sablefish_released_quota_limit) ~ "some_released", 
                                            .default = "all_retained"),  #column for identifying when skippers recorded their releases
         p_sets_depr = sum(set_depr_count)/n(),
         trip_depr_desc = ifelse(length(unique(Depredation))>1,             
                                 paste0("some ",unique(Depredation[Depredation != ""] ), collapse =", "),
                                 paste0(unique(Depredation)," all sets")),
         trip_depr_desc = ifelse(grepl("some No depredation, ", trip_depr_desc, ignore.case = TRUE),
                                 gsub("some No depredation, ","",trip_depr_desc),
                                 ifelse(grepl(", some No depredation", trip_depr_desc, ignore.case = TRUE),
                                        gsub(", some No depredation","",trip_depr_desc),
                                        trip_depr_desc)),
         multi_gear_config = ifelse(is.na(gear_target_species_code_2),"single_config","multi_config"), #column to flag when more than one gear configuration
         #for analyzing CPUE versus things like hook size will want
         #to filter out multi_config
         mean_trip_depth_fm = mean(average_depth_fathoms)) 
## Next line of code in Phil's data pulls in whole weights from FT data, so merge FT data next 
unique()
#-------------------------------------------------------------------------------------------------------------
# Longline logbook join with fish ticket data (you can merge this earlier but it will change 1 unique effort/row!) 
# Refer to Line 422 in fishery_cpue_fr_OceanAK_ftx_lb_dat.R file from here onward 
#-------------------------------------------------------------------------------------------------------------
# problem with not using stat area in join. Examining here:
colnames(log_ll_cpue)


## Join logbook data with fish ticket data 
ll_cpue_log_ftx <- log_ll_cpue %>% 
  full_join(ftx_ll_cpue, by = c("year", "trip_number", "project_code")) %>% 
  relocate(c(114:120), .before = trip_target_species_combined)

#ll_cpue_log_ftx2 <- log_ll_cpue %>% mutate(stat_area_agr = log_stat_area) %>%
#  full_join(ftx_ll_cpue %>% mutate(stat_area_agr = ft_stat),
#            by = c("year", "trip_number", "project_code","stat_area_agr")) %>% 
#  relocate(c(114:120), .before = trip_target_species_combined)

unique(ll_cpue_log_ftx$ft_dispo_code)
with(ll_cpue_log_ftx,table(year,ft_dispo_code))

View(ll_cpue_log_ftx)
colnames(ll_cpue_log_ftx)
colnames(ftx_ll_cpue)
year
log_stat_area
ft_adfg_number
ft_sell_date

unique(ftx_ll_cpue$ft_dispo_code)
unique(ftx_ll_cpue$harvest_code)

unique(ll_cpue_log_ftx$ft_dispo_code)
unique(ll_cpue_log_ftx$harvest_code)

random_check<-function(data){ #data<-mixed_targets
  #length(unique(Sable_ll_CPUE$Year))
  data<-data  #data<-whaled
  
  year_list<-unique(data$year)
  #year_check<-sample(year_list,1)
  year_check<-year_list[sample(length(year_list),1)]
  
  data<-data %>% filter(year == year_check)
  
  adfg_list<-unique(data$ft_adfg_number)
  adfg_check<-adfg_list[sample(length(adfg_list),1)]
  
  data<-data %>% filter(ft_adfg_number == adfg_check)
  
  sd_list<-unique(data$ft_sell_date)
  sd_check<-sd_list[sample(length(sd_list),1)]
  
  data<-data %>% filter(ft_sell_date == sd_check)
  
  #gsa_list<-unique(data$log_stat_area)
  #gsa_check<-gsa_list[sample(length(gsa_list),1)]
  return<-list(year_check,adfg_check,sd_check) #rands<-return
  #return<-list(year_check,adfg_check,sd_check,gsa_check) #rands<-return
}

rands <- random_check(ll_cpue_log_ftx)

check<-as.data.frame(ll_cpue_log_ftx %>% filter(ft_sell_date == rands[[3]][1],
                                                log_stat_area == rands[[4]][1],
                                   year == rands[[1]][1], #unique(Sable_ll_CPUE$Year)[rands[[1]][1]],
                                   ft_adfg_number == rands[[2]][1])); nrow(check)
str(check)
check$num_of_hooks_1

check %>% select()

ll_cpue_log_ftx %>% select(is.na(num_of_hooks_1))

hist(as.numeric(ll_cpue_log_ftx$num_of_hooks_1), breaks = 100)
hist(as.numeric(ll_cpue_log_ftx$num_hooks1_cpue), breaks = 100)
hist(as.numeric(ll_cpue_log_ftx$num_hooks2_cpue), breaks = 100)
nrow(data.frame(ll_cpue_log_ftx)[is.na(ll_cpue_log_ftx$num_hooks1_cpue),])/nrow(data.frame(ll_cpue_log_ftx))
#OK, Rhea seems to have the hook counts in there...
# Lets check the other key calculations that we need
nrow(data.frame(ll_cpue_log_ftx)[is.na(ll_cpue_log_ftx$p_sets_depr),])/nrow(data.frame(ll_cpue_log_ftx))
partial_depr_count
nrow(data.frame(ll_cpue_log_ftx)[is.na(ll_cpue_log_ftx$partial_depr_count),])/nrow(data.frame(ll_cpue_log_ftx))
dep_shar_count
nrow(data.frame(ll_cpue_log_ftx)[is.na(ll_cpue_log_ftx$dep_shar_count),])/nrow(data.frame(ll_cpue_log_ftx))
nrow(data.frame(ll_cpue_log_ftx)[is.na(ll_cpue_log_ftx$depredation),])/nrow(data.frame(ll_cpue_log_ftx))
unique(ll_cpue_log_ftx$depredation)
number_of_skates_impacted
nrow(data.frame(ll_cpue_log_ftx)[is.na(ll_cpue_log_ftx$number_of_skates_impacted),])/nrow(data.frame(ll_cpue_log_ftx))
ll_cpue_log_ftx$total
nrow(data.frame(ll_cpue_log_ftx)[is.na(ll_cpue_log_ftx$uniques),])/nrow(data.frame(ll_cpue_log_ftx))

nrow(ll_cpue_log_ftx %>% filter(is.na(log_rnd_lbs_sablefish_retained)))/nrow(ll_cpue_log_ftx)
nrow(ll_cpue_log_ftx %>% filter(is.na(log_numbers_sablefish_retained)))/nrow(ll_cpue_log_ftx)
nrow(ll_cpue_log_ftx %>% filter(is.na(log_rnd_lbs_sablefish_retained) &
                                  is.na(log_numbers_sablefish_retained)))/nrow(ll_cpue_log_ftx) #Exciting! Rhea's new data seems to indicate that 98.4% of log book data has either numbers or lbs recorded... 
#-------------------------------------------------------------------------------
# Adding in initial cpue calculations from trip level data:
unique(ll_cpue_log_ftx$trip_target_species_code_1)

#calculate trip level cpue
ll_cpue_log_ftx %>% filter(effort_target_species_code_1 == 710) %>% #only sets that were targetting sablefish
  mutate() %>% 
  group_by(year,trip_number,adfg_number,
           #log_stat_area,
           ft_sell_date) %>%
  mutate(harvest_code = list(unique(harvest_code)), #need to get rid of harvest code so that we combine all catches from that trip, even if they were overages... 
         ft_dispo_code = list(unique(ft_dispo_code)), #need to combine fish when more than one disposition recorded for the trip
         ticket_number = list(unique(ticket_number)), #sometimes a trip has more than one ticket so need to combined... 
         cfec_permit_number = list(unique(cfec_permit_number)), #sometimes a trip has more than one permit number so need to combined...
         stat_area_count_log = length(unique(log_stat_area)),
         stat_area_count_ft = length(unique(ft_stat)),
         set_count = length(unique(effort_number)),
         set_count_match = ifelse(set_count == set.count,"Match","Mismatch"),
         trip_catch = trip_landing, #sum(unique(ft_sable_rnd_lbs)),
         trip_soak_time = sum(set_soak_time),
         trip_km_fished = sum(set_km_fished),
         trip_hooks_exact = sum(set_hook_count_exact),
         trip_hooks_est = sum(set_hook_count_best_available),
         trip_sable_lbs_set = sum(unique(trip_catch))/set_count,
         #sable_lbs_set = sum(unique(trip_catch))/set_count,
         lbs_p_hk_exact = sum(unique(trip_catch))/trip_hooks_exact,
         lbs_p_hk_all = sum(unique(trip_catch))/trip_hooks_est,
         multigear_trip = ifelse(multi_gear_config == "single_config","No","Yes"),
         skip_acc = ifelse(!is.na(log_rnd_lbs_sablefish_retained),
                           (sum(log_rnd_lbs_sablefish_retained )/ unique(trip_catch)),NA)) %>%
  select(-c(ft_sable_rnd_lbs,ft_stat,log_stat_area)) %>% unique() -> trip_cpue

# calculate set level cpue
set_cpue <- trip_cpue %>% group_by(year,trip_number,adfg_number,
                                   #log_stat_area,
                                   ft_sell_date#,
                                   #effort_number
                                   ) %>%
  mutate(#stat_area_match = ifelse(ft_stat == log_stat_area,"Match","Mismatch"),
         target_match = ifelse(effort_target_species_code_1 == trip_target_species_code_1,"Match","Mismatch"),
         lbs_sablefish_released = ifelse(is.na(log_rnd_lbs_sablefish_released),0,log_rnd_lbs_sablefish_released),
         lbs_sablefish_released_quota_limit = ifelse(is.na(log_rnd_lbs_sablefish_released_quota_limit),0,log_rnd_lbs_sablefish_released_quota_limit),
         no_sablefish_released = ifelse(is.na(log_numbers_sablefish_released),0,log_numbers_sablefish_released),
         no_sablefish_released_quota_limit = ifelse(is.na(log_numbers_sablefish_released_quota_limit),0,log_numbers_sablefish_released_quota_limit),
         set_catch = ifelse(!is.na(log_rnd_lbs_sablefish_retained), #if skipper recorded catch in lbs
                       trip_catch * log_rnd_lbs_sablefish_retained / sum(log_rnd_lbs_sablefish_retained) +
                         lbs_sablefish_released + lbs_sablefish_released_quota_limit, #est catch based on landings and logbook recordings
                       ifelse(!is.na(log_numbers_sablefish_retained), #if skipper recorded catch in no
                              trip_catch * log_numbers_sablefish_retained / sum(log_numbers_sablefish_retained) +
                                trip_catch * (no_sablefish_released+no_sablefish_released_quota_limit) / sum(log_numbers_sablefish_retained),
                              NA)),
         sable_lbs_set = set_catch, 
          skipper_lbsper_fish = ifelse(!is.na(log_numbers_sablefish_retained),
                          set_catch / log_numbers_sablefish_retained,
                          NA)
  )

## Format and save for cpue exam and calculation: 

ll_cpue_log_ftx %>% filter(effort_target_species_code_1 == 710) %>% 
  group_by(year,trip_number,adfg_number,
           #log_stat_area,
           ft_sell_date) %>%
  mutate(
         harvest_code = list(unique(harvest_code)), #need to get rid of harvest code so that we combine all catches from that trip, even if they were overages... 
         ft_dispo_code = list(unique(ft_dispo_code)), #need to combine fish when more than one disposition recorded for the trip
         ticket_number = list(unique(ticket_number)), #sometimes a trip has more than one ticket so need to combined... 
         cfec_permit_number = list(unique(cfec_permit_number)), #sometimes a trip has more than one permit number so need to combined...
         stat_area_count_log = length(unique(log_stat_area)),
         stat_area_count_ft = length(unique(ft_stat)),
         set_count = length(unique(effort_number)),
         set_count_match = ifelse(set_count == set.count,"Match","Mismatch"),
         trip_catch = trip_landing #sum(unique(ft_sable_rnd_lbs)),
         #success_sets = length(log_rnd_lbs_sablefish_retained[!is.na(log_rnd_lbs_sablefish_retained)]),
         #set_record_mismatch = ifelse(set_count == success_sets,"Match","Mismatch")
         ) %>%
  select(-c(ft_sable_rnd_lbs,ft_stat,log_stat_area)) %>% unique() %>% #-> trip_cpue

# calculate set level cpue
#set_cpue <- trip_cpue %>% group_by(year,trip_number,adfg_number,
#                                   #log_stat_area,
#                                   ft_sell_date#,
#                                   #effort_number
#) %>%
  mutate(trip_soak_time = sum(set_soak_time),
         trip_km_fished = sum(set_km_fished),
         trip_hooks_exact = sum(set_hook_count_exact),
         trip_hooks_est = sum(set_hook_count_best_available),
         trip_sable_lbs_set = sum(unique(trip_catch))/set_count, #otherwise, count it using the number of sets that recorded a landing... 
         #sable_lbs_set = sum(unique(trip_catch))/set_count,
         lbs_p_hk_exact = sum(unique(trip_catch))/trip_hooks_exact,
         lbs_p_hk_all = sum(unique(trip_catch))/trip_hooks_est,
         multigear_trip = ifelse(multi_gear_config == "single_config","No","Yes"),
         skip_acc = ifelse(!is.na(log_rnd_lbs_sablefish_retained),
                           (sum(log_rnd_lbs_sablefish_retained )/ unique(trip_catch)),NA),#stat_area_match = ifelse(ft_stat == log_stat_area,"Match","Mismatch"),
    target_match = ifelse(effort_target_species_code_1 == trip_target_species_code_1,"Match","Mismatch"),
    lbs_sablefish_retained = ifelse(is.na(log_rnd_lbs_sablefish_retained),0,log_rnd_lbs_sablefish_retained),
    numbers_sablefish_retained = ifelse(is.na(log_numbers_sablefish_retained),0,log_numbers_sablefish_retained),
    lbs_sablefish_released = ifelse(is.na(log_rnd_lbs_sablefish_released),0,log_rnd_lbs_sablefish_released),
    lbs_sablefish_released_quota_limit = ifelse(is.na(log_rnd_lbs_sablefish_released_quota_limit),0,log_rnd_lbs_sablefish_released_quota_limit),
    no_sablefish_released = ifelse(is.na(log_numbers_sablefish_released),0,log_numbers_sablefish_released),
    no_sablefish_released_quota_limit = ifelse(is.na(log_numbers_sablefish_released_quota_limit),0,log_numbers_sablefish_released_quota_limit),
    
    tot_sable_lbs = lbs_sablefish_retained + lbs_sablefish_released + lbs_sablefish_released_quota_limit,
    tot_sable_no = numbers_sablefish_retained + no_sablefish_released +no_sablefish_released_quota_limit,
    
    set_catch = ifelse(!is.na(log_rnd_lbs_sablefish_retained), #if skipper recorded catch in lbs
                       trip_catch * lbs_sablefish_retained / sum(lbs_sablefish_retained) +
                         lbs_sablefish_released + lbs_sablefish_released_quota_limit, #est catch based on landings and logbook recordings
                       ifelse(!is.na(log_numbers_sablefish_retained), #if skipper recorded catch in no
                              trip_catch * numbers_sablefish_retained / sum(numbers_sablefish_retained) +
                                trip_catch * (no_sablefish_released+no_sablefish_released_quota_limit) / sum(numbers_sablefish_retained),
                              NA)),
    #Still some weird results with releases and such. This is my attempts to fix, but left off because I was running out of time
    # Less that 0.5% of set cpue is NA right now, so just chuck it. 
    #set_catch2 = ifelse(!is.na(log_rnd_lbs_sablefish_retained) |
    #                      !is.na(log_rnd_lbs_sablefish_released) |
    #                      !is.na(log_rnd_lbs_sablefish_released_quota_limit), #if skipper recorded catch in lbs
    #                   trip_catch * lbs_sablefish_retained / sum(lbs_sablefish_retained, na.rm=T) +
    #                     lbs_sablefish_released + lbs_sablefish_released_quota_limit, #est catch based on landings and logbook recordings
    #                   ifelse(!is.na(log_numbers_sablefish_retained) |
    #                            !is.na(log_numbers_sablefish_released) |
    #                            !is.na(log_numbers_sablefish_released_quota_limit), #if skipper recorded catch in no
    #                          trip_catch * numbers_sablefish_retained / sum(numbers_sablefish_retained, na.rm=T) +
    #                            trip_catch * (no_sablefish_released+no_sablefish_released_quota_limit) / sum(numbers_sablefish_retained, na.rm=T),
    #                          NA)),
    sable_lbs_set = set_catch, 
    skipper_lbsper_fish = ifelse(!is.na(log_numbers_sablefish_retained),
                                 set_catch / log_numbers_sablefish_retained,
                                 NA),
    meanset_vs_trip_cpue_ratio = mean(sable_lbs_set, na.rm=T) / trip_sable_lbs_set
  ) -> ll_cpue

colnames(ll_cpue)

# format for existing cpue scrips
ll_cpue %>% 
  mutate(set_date = as.Date(time_set, c("%m/%d/%Y %H:%M"), tz="UTC"),
         sell_date = ft_sell_date,
         julian_day_set = yday(set_date),
         julian_day_sell = yday(sell_date),
         tm_hauled = parse_date_time(time_hauled, c("%m/%d/%Y %H:%M")),
         t_set = parse_date_time(time_set, c("%m/%d/%Y %H:%M")),
         set_soak = set_soak_time,
         set_length = set_km_fished,
         
         trip_recorded_releases = trip_recorded_releases,
         avg_set_depth_m = 1.8288*average_depth_fathoms,
         avg_trip_depth_m = 1.8288*mean_trip_depth_fm,
         
         gear = factor(longline_system_code_1),
         gear_name = factor(effort_gear),
         
         #primary configuration
         hook_size = as.character(hook_size_1), 
         mean_hook_size = as.numeric(mean_hook_size1),
         hook_space = spacing_1, #*FLAG* - check that hook_space is in inches
         size = factor(as.numeric(gsub("[^0-9]", "", hook_size_1))),
         mean_size = mean_hook_size1,
         no_hooks_fished_on_trip = trip_hooks_est,
         no_hooks_exact_trip = trip_hooks_exact,
         no_hooks_est_trip = trip_hooks_est,
         no_hooks_p_set_trip = trip_hooks_est/set_count,
         
         
         #no_skates_fished_on_trip = total_skate_count,
         
         #secondary configuration
         hook_size_2 = as.character(hook_size_2), 
         hook_space_2 = spacing_2, #*FLAG* - check that hook_space is in inches
         size_2 = factor(as.numeric(gsub("[^0-9]", "", hook_size_2))),
         no_hooks_2 = num_hooks2_cpue,
         
         sable_lbs_set = sable_lbs_set,
         trip_sable_lbs_set  = trip_sable_lbs_set) %>%
  select(year, sell_date, set_date, julian_day_set, julian_day_sell,
         trip_no = trip_number,  #or Trip.Number_ftx?? 
         Adfg = adfg_number, Spp_cde_trip = trip_set_targets, 
         Spp_cde_set = effort_target_species_code_1,
         set_soak, trip_soak = trip_soak_time,
         set_length, total_km_fished = trip_km_fished, 
         gear, gear_name,
         hook_size, mean_hook_size, 
         hook_space, 
         mean_hook_spacing = mean_hook_spacing1,
         size, mean_size,
         no_hooks_set = set_hook_count1,
         no_hooks_fished_on_trip, 
         no_hooks_exact_trip, 
         no_hooks_est_trip,
         no_hooks_p_set_trip,
         hook_size_2, hook_space_2, size_2, no_hooks_2,
         set_hook_count_exact,
         set_hook_count_est,
         set_hook_count_best_available,
         #no_skates_fished_on_trip,
         #no_skates_exact = num_of_skates1,
         #no_skates_est = num_of_skates1_calc,
         #Stat = Groundfish.Stat.Area, 
         
         set_depth = avg_set_depth_m,
         trip_depth = avg_trip_depth_m,
         
         #mean_trip_depth_fm,
         set_no = effort_number, 
         no_sets = set_count, 
         multigear_trip,
         trip_set_targets, trip_recorded_releases, multi_gear_config, 
         disposition = ft_dispo_code, set_depredation = Depredation, 
         trip_depredation = trip_depr_desc,
         p_sets_depredated = p_sets_depr, 
         #catch,
         #logged_no = Numbers_log, 
         #logged_lbs = Pounds_log,
         
         sable_lbs_set, trip_sable_lbs_set,
         #sable_lbs_hook, sable_lbs_hook_exact, sable_lbs_hook_est,
         #lbs_p_set_km, lbs_p_set_hr, lbs_p_set_km_hr, 
         #lbs_p_hk_exact, lbs_p_hk_km_exact,lbs_p_hk_hr_exact,                        
         #lbs_p_hk_km_hr_exact, lbs_p_hk_est, lbs_p_hk_km_est, lbs_p_hk_hr_est, 
         #lbs_p_hk_km_hr_est, lbs_p_hk_all, lbs_p_hk_km_all, lbs_p_hk_hr_all, 
         #lbs_p_hk_km_hr_all, lbs_p_hk_exact2, lbs_p_hk_km_exact2, lbs_p_hk_hr_exact2, 
         #lbs_p_hk_km_hr_exact2, 
         
         start_lat = start_latitude_decimal_degrees,
         start_lon = start_longitude_decimal_degrees, 
         set_target = effort_target_species_1,
         set_target_2 = effort_target_species_2,
         trip_target = trip_target_species_code_1,
         trip_target_2 = trip_target_species_code_2,
         meanset_vs_trip_cpue_ratio) -> cpue_for_analysis

write_csv(cpue_for_analysis, paste0(YEAR+1,"/data/fishery/fishery_ll_cpue_",
                         min(cpue_for_analysis$year), "_", max(cpue_for_analysis$year), ".csv"))

#save for legacy for 2025 and forward
write_csv(cpue_for_analysis, paste0("legacy_data/fishery/fishery_ll_cpue_",
                         min(cpue_for_analysis$year), "_", max(cpue_for_analysis$year), ".csv"))

#-------------------------------------------------------------------------------
# checking on some weirdos:
nrow(ll_cpue %>% filter(is.na(set_catch))) / nrow(ll_cpue)
nrow(ll_cpue %>% filter(is.na(meanset_vs_trip_cpue_ratio))) / nrow(ll_cpue)

hist(ll_cpue$meanset_vs_trip_cpue_ratio, breaks = 100)




unique(cpue$hook_size_1)
unique(cpue$spacing_1)

unique(cpue$mean_hook_size1)
unique(cpue$mean_hook_spacing1)

nrow(ll_cpue[is.na(ll_cpue$set_catch),])/nrow(ll_cpue)

set_record_mismatch
nrow(ll_cpue %>% filter(set_record_mismatch == "Mismatch"))/nrow(ll_cpue)

ll_cpue %>% filter(is.na(set_catch2)) -> bad
bad$year
#---------------------------------------------------------------------------------
# Code development checks:
nrow(set_cpue %>% filter(stat_area_match == "Mismatch")) / nrow(set_cpue)
nrow(set_cpue %>% filter(target_match == "Mismatch")) / nrow(set_cpue) #targets usually match, but on occasion a halibut trip had some efforts targeting sablefish
#nrow(set_cpue %>% filter(set_count_match == "Mismatch")) / nrow(set_cpue) #targets usually match, but want to clean up

nrow(set_cpue %>% filter(is.na(start_latitude_decimal_degrees)))/nrow(set_cpue) # right on.. now we have lat long data... 

with(set_cpue,table(year,effort_target_species_code_1))
with(set_cpue,table(year,trip_target_species_code_1))
with(set_cpue,table(year,target_match))

rands <- random_check(ll_cpue)
rands <- list(1997,51950,as.Date("1997/09/22", format = "%Y/%m/%d"))
rands <- list(1997,66183,as.Date("1997/09/03", format = "%Y/%m/%d"))
#rands <- list(1999,35632,as.Date("1999/10/23", format = "%Y/%m/%d"),345603)

check<-as.data.frame(ll_cpue %>% filter(ft_sell_date == rands[[3]][1],
                                                #log_stat_area == rands[[4]][1],
                                                year == rands[[1]][1], #unique(Sable_ll_CPUE$Year)[rands[[1]][1]],
                                                ft_adfg_number == rands[[2]][1]))

check$set_catch / check$trip_sable_lbs_set

difference <- setdiff(as.vector(check[1,]),as.vector(check[2,]))
print(difference)

check$trip_catch
check$set_catch
sum(check$set_catch, na.rm=T)
check$set_catch2
sum(check$set_catch2)

mean(check$sable_lbs_set, na.rm=T)
unique(check$trip_sable_lbs_set)

check$ft_dispo_code
check$harvest_code
check$ticket_number
check$cfec_permit_number

check$trip_km_fished
sum(check$set_length_km)

check$trip_soak_time
sum(check$set_soak_time)

check$trip_hooks_exact
sum(check$set_hook_count_exact)
check$trip_hooks_est
sum(check$set_hook_count_est)

check$hook_size_1
check$spacing_1

check$mean_hook_size1
check$mean_hook_spacing1

check$lbs_sablefish_retained
check$numbers_sablefish_retained
check$log_rnd_lbs_sablefish_retained
check$log_numbers_sablefish_retained

check$no_sablefish_released
check$no_sablefish_released_quota_limit
check$log_numbers_sablefish_released
check$log_numbers_sablefish_released_quota_limit

check$lbs_sablefish_released
check$lbs_sablefish_released_quota_limit
check$log_rnd_lbs_sablefish_released
check$log_rnd_lbs_sablefish_released_quota_limit

nrow(check)
nrow(unique(check))
check[,c(1:16,21:61,78:98,105:136)]
check$ft_dispo_code
check$harvest_code




length(unique(check$effort_number))
check$set.count
check$set_count

check2$set.count
check2$set_count

check$effort_target_species_code_1
check$trip_target_species_code_1
check$gear_target_species_code_1

ll_cpue_log_ftx$total_hooks

check[1:10,]
unique(check$effort_number); length(unique(check$effort_number))
check$set.count
unique(check$trip_number)
unique(check$ft_sable_rnd_lbs)

check2<- as.data.frame(try %>% filter(ft_sell_date == rands[[3]][1],
                                       log_stat_area == rands[[4]][1],
                                       year == rands[[1]][1], #unique(Sable_ll_CPUE$Year)[rands[[1]][1]],
                                       ft_adfg_number == rands[[2]][1]))
check2$ft_dispo_code
check2$harvest_code
nrow(check2)
nrow(unique(check2))
unique(check2$trip_number)
View(check2)
unique(check2$ticket_number)
unique(check2$ft_sell_date)
unique(check2$ft_sable_rnd_lbs)
length(unique(check2$effort_number))
length(unique(check2$soak_time_hrs))
length(unique(check2$start_latitude_decimal_degrees))

check3 <- as.data.frame(ll_cpue_log_ftx %>% filter(ft_sell_date == rands[[3]][1],
                                       log_stat_area == rands[[4]][1],
                                       year == rands[[1]][1], #unique(Sable_ll_CPUE$Year)[rands[[1]][1]],
                                       ft_adfg_number == rands[[2]][1]))
check3$ft_dispo_code
check3$harvest_code

nrow(check3)
nrow(unique(distinct(check3)))
nrow(check3 %>% distinct())
unique(check3$trip_number)
View(check3)
unique(check3$ticket_number)
unique(check3$ft_sell_date)
unique(check3$ft_sable_rnd_lbs)
length(unique(check3$effort_number))
length(unique(check3$soak_time_hrs))
length(unique(check3$start_latitude_decimal_degrees))
length(unique(check3$time_set))

unique(check3$trip_target_species_code_1)
unique(check3$effort_target_species_code_1)
unique(check3$gear_target_species_code_1)
#-------------------------------------------------------------------------------------------------------------
# FLAGS FOR PHIL 
#-------------------------------------------------------------------------------------------------------------

## pre.lbs_per_set should be split by gear proportional to set instead of split evenly by set count? 

## need to subtract gear affected by depredation from that which was retrieved in some calculation above, if you care about this 

## STEP 10 in fishery_cpue_fr_OceanAK_ftx_lb_dat.R is to separate logbook data out by halibut and sablefish species
## but should this be target species? Otherwise this code would just pull out rows with any halibut or any sablefish 
## caught regardless of the target species 
## "10) Separate the logbook data out by halibut and sablefish
## hal_ll_log<-ll_log %>% filter(Species_log == "Halibut")
## sable_ll_log <- ll_log %>% filter(Species_log == "Sablefish")"

## For the gear configs above, do you want to merge/average gear configurations above? Everything is currently 
## split out with gear configurations 1 and 2 (when applicable) but it seems like it would be appropriate to add those 
## and/or average those (e.g., gear config 1 = 10 skates, gear config 2 = 12 skates so a total of 22 skates were set for that effort, 
## but things like hook size, hook spacing, etc. could just be averaged. I'm not sure how you want to handle these different 
## gear configurations especially when diff target species. See your notes below on this. 

## Notes from Phil in lines 508-519 in fishery_cpue_fr_OceanAK_ftx_lb_dat.R 
## "Note regarding two gear configurations.  When calculating final CPUE in analysis
## you will need to be mindful of gear_target_1 and _2.  When there are two sets of
## gear targeting different species (halibut and sablefish) the CPUE will NOT be
## useful for analysis because there is no way to separate out the catch by target 
## if they are both on the same ticket. :(  Filter out multi_gear_config = "multi_config""
# 
## "Will also need to be mindful of disposition.  Releases are recorded in some logbooks.
## When this happens looking up a particular landing will show two entries; one retained
## and one released (or some combo depending on sets).  Catch will reflect the landing
## but release data on the Release logbook may be in Pounds_log and Numbers_log.  In 
## the analysis will want to examine this data to see if there is any information 
## to be gleaned on release behavior in the fishery."





###################################################################################################
### QAQC Code that checks for fish ticket trips to logbook trips 
###################################################################################################
# look for fish tickets that don't have corresponding logbooks and logbooks that report fish tickets but the fish tickets are missing 
unique_ftx <- ftx %>% 
  group_by(year, trip_number, project_code, ticket_number) %>%
  select(year, trip_number, project_code, ticket_number) %>% 
  mutate(ticket_number = as.character(ticket_number), 
         fishticket_database = "ftx") %>% 
  distinct() 

unique_gear <- gear %>% 
  group_by(year, trip_number, project_code, log_tickets) %>% 
  select(year, trip_number, project_code, log_tickets) %>% 
  separate_longer_delim(log_tickets, delim = ", ") %>% 
  mutate(ticket_number = as.character(log_tickets), 
         logbook_database = "logs") %>% ungroup () %>% 
  select(year, trip_number, project_code, ticket_number, logbook_database) %>%
  distinct() %>%
  full_join(unique_ftx, by = c("year", "trip_number", "project_code", "ticket_number")) %>% 
  filter(is.na(fishticket_database)) 
## as of 3.20.2024 there are ZERO fish tickets missing that are reported on logbooks and 
## ZERO fish tickets that aren't reported on logbooks! 


