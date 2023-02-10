# 2. Fishery cpue ---- 2023

# This script will replace past (2022 and earlier) CPUE calculations that relied
# on a long and mysterious sql script that was not documented in written format.  
# When Rhea and staff reworked the logbook data that script became even more 
# problematic and required programmer Justin Daily to run the scripts on the 
# revamped logbook data.  Rather than continue this undocumented and "black box" 
# calculation of CPUE this script will utilize data pulled directly from OceanAK.  

#-----Pre-2023-HISTORY------------------------------------------------------------------
# HISTORY details from Jane Sullivan's original 2021 analysis:
# from JANE SULLIVAN in 2021 Current documentation: In 2020, Rhea Ehresmann 
# and GF project staff worked
# with Karl Wood to create a new cpue data input application. As part, they
# redefined how set targets are defined. This means Kallenburger's scripts that
# match fish ticket poundage data to logbook data are likely depricated and will
# need a revamp. If this is the case, my recommendation is to work with Justin
# Daily and the new biometrician to develop new methods (ideally in R). In the
# mean time, the statistical catch-at-age model will accept NULLS for this data
# component. This part of the model likelihood receives a relatively low weight,
# so I anticipate this will have little influence on model results. The full
# time series will need to be redeveloped once this has happened.

# Past documentation: Kamala Carroll pulls the IFDB view out_g_log_longline_c_e,
# populates new columns effort_target_species_code and effort_comments, and
# sends to Scott Johnson. Scott runs a series of Martina Kallenburger's sql
# scripts (https://github.com/commfish/seak_sablefish/issues/7) that match fish
# tickets pounds back to set based on Kamala's set target designation based on
# some undocumented methodology (proportional to numbers/pounds in logbook?). It
# lives in a view called sablefish_cpue_final_view in scottj's IFDB schema,
# though he has made a public synonym for it. This output doesn't contain
# information on sets designated as halibut targets. Note that it is missing
# effort_no's (effort_no's = individual sets).
#----------------------------------------------------------------------------------
# most recent year of data
YEAR <- 2022

# Load ----
source("r_helper/helper.r")
source("r_helper/functions.r")

#---------------------------------------------------------------------------------
# Ocean AK filters and data sources:
# 1) Logbook Data
#   A) Longline :
#     i) Year >= 1997
#     ii) Trip primary target = sablefish & halibut
#     iii) Groundfish Management Area Code = NSEI
#   B) Pot: Pot_logbook_sable_and_halibut_target_2006-now_for_CPUE.csv
#     i) Year >= 2006
#     ii) Trip primary target = sablefish & halibut
#     iii) Groundfish Management Area Code = NSEI
# 2) V1 Fish ticket data: NSEI_ftx_sablefish_halibut_pot_longline_1997-now.csv
#     i) Year >=1997
#     ii) Gear = longline or pot
#     iii) species = halibut or sablefish
#     iv) Mgt Area District = NSEI
#     v) CFEC Code = B05B;B06B;B06Z;B06ZL;B09B;B25B;B26B;B61B;B61ZL;B91B;BAKE;BO6B;C61A; 0000, 9998, 9999
#         *0000, 9998 & 9999 for missing values
#         *halibut must be landed on a "B" permit, and Chatham sablefish must be 
#          landed on "C61A" permit. Otherwise any other C-permit is IFQ or Clarence 
#          sablefish, I-permits = directed lingcod, M-permits = misc finfish like 
#          Pcod and older rockfish landings, hagfish, etc., S-permits = salmon, 
#          Y-permits = directed DSR.
#         *!!!! FLAG !!! This is broken.  There are cases where fish tickets show C61A
#                        permits but they are not in the logbook entries.  
#                        Example 1: Year = 2013 and ADFG_No = 65119.  Fish tickets show
#                        C and B permits, but only C permits in logbooks... 
#
# NSEI stat areas = 325700; 335631, 335634, 335701, 335702, 335703, 335704, 335705
#                     335731, 335732, 335733, 335734, 335735, 345534, 345603, 345605
#                     345631, 345632, 345701, 345702, 345703, 345704, 345705, 345706
#                     345731, 345732, 345801, 345802, 345803, 345830, 355707, 355731
#                     355732, 355733, 355801, 355802, 355830, 355900, 365804, 365830
# 3
# Other shit notes from this shit show:
# 1) Sequence number in ticket sometimes matches "Ticket" in logbook data... and sometimes not. 
#    See same example of Year = 2013 and ADFG_No = 65119


#ll_log<-rbind(read.csv(paste0(YEAR+1,"/data/fishery/raw_data/Longline_logbook_sable_and_halibut_target_1997-2005_for_CPUE.csv")),
#             read.csv(paste0(YEAR+1,"/data/fishery/raw_data/Longline_logbook_sable_and_halibut_target_2006-now_for_CPUE.csv")))

ll_log<-read.csv(paste0(YEAR+1,"/data/fishery/raw_data/Longline logbook sable and halibut target 1997-now for CPUE.csv"))
#str(ll_log)
#colnames(ll_log)

pot_log<-read.csv(paste0(YEAR+1,"/data/fishery/raw_data/Pot_logbook_sable_and_halibut_target_2006-now_for_CPUE.csv"))

ftx<-read.csv(paste0(YEAR+1,"/data/fishery/raw_data/NSEI_ftx_sablefish_halibut_pot_longline_1997-now.csv"))
#str(ftx)

#byc.ftx<-read.csv(paste0(YEAR+1,"/data/fishery/raw_data/NSEI_ftx_bycatch_pot_longline_1997-now.csv"))

#alltx<-rbind(ftx,byc.ftx)
#----------------------------------------------------------------------------------

#1) separate out multiple gear configurations
gear_config<-max(stringr::str_count(ll_log$Config.List, "---"))+1
ll_log %>% separate(Config.List, into = paste0("gear_config",1:gear_config),sep = "---") -> ll_log
#2) get rid of parenthesis...
ll_log %>% 
  mutate(gear_config1 = gsub(")","",sub(".","",gear_config1))) %>%   #junk1 and junk2
  mutate(gear_config2 = gsub(")","",sub(".","",gear_config2))) %>%
  mutate(gear_config2 = sub(".","",gear_config2)) -> ll_log
#3) divide out target species into its own column before separating the rest of 
#   the gear configuration into its own rows
ll_log<-ll_log %>% 
  mutate(split_gf1 = gear_config1,
         split_gf2 = gear_config2) %>% 
  separate(split_gf1,into = paste0("gear_target_",1),sep = ",") %>%
  separate(split_gf2,into = paste0("gear_target_",2),sep = ",") %>%
  mutate(gear_target_1 = ifelse(grepl("Target Species", gear_target_1, fixed = TRUE),
                                as.numeric(gsub(".*?([0-9]+).*", "\\1", gear_target_1)),NA),
         gear_target_2 = ifelse(grepl("Target Species", gear_target_2, fixed = TRUE),
                                as.numeric(gsub(".*?([0-9]+).*", "\\1", gear_target_2)),NA)) #-> ll_log
#colnames(ll_log)
# 4) now, separate gear config column into multiple rows and get rid of rows with
#    target species
ll_log <- separate_rows(ll_log,"gear_config1",sep=", ") %>% 
  filter(!grepl("Target Species", gear_config1, fixed = TRUE)) 
ll_log <-  separate_rows(ll_log,"gear_config2",sep=", ") %>%
  filter(!grepl("Target Species", gear_config2, fixed = TRUE))

# 5) separate the gear configuration columns...
ll_log<-ll_log %>% separate(gear_config1,into = paste0(c("gear_sub1","gear_spec1"),""),sep = ":") %>%
  separate(gear_config2,into = paste0(c("gear_sub2","gear_spec2"),""),sep = ":") %>%
  mutate(gear_sub1 = str_replace(gear_sub1, " ","_"),
         gear_sub1 = str_replace(gear_sub1, "/","_per_"),
         gear_sub2 = str_replace(gear_sub2, " ","_"),
         gear_sub2 = str_replace(gear_sub2, "/","_per_")) %>%
  pivot_wider(names_from = gear_sub1, values_from = gear_spec1) %>% 
  mutate(hook_size1 = gsub("[ (]","",Hook_Size),
         spacing1 = gsub("[ (]","",Spacing),
         hooks_per_skate1 = gsub("[ (]","",Hooks_per_Skate),
         num_of_skates1 = gsub("[ (]","",Num_Skates),
         num_of_hooks1 = gsub("[ (]","",Num_Hooks)) %>%
  select(-Hook_Size,-Spacing,-Hooks_per_Skate,-Num_Skates) %>% #,Num_Hooks) #!!! Flag!!! Add back in when doing the whole shebang!!
  pivot_wider(names_from = gear_sub2, values_from = gear_spec2) %>%
  mutate(hook_size2 = gsub("[ (]","",Hook_Size),
         spacing2 = gsub("[ (]","",Spacing),
         hooks_per_skate2 = gsub("[ (]","",Hooks_per_Skate),
         num_of_skates2 = gsub("[ (]","",Num_Skates),
         num_of_hooks1 = gsub("[ (]","",Num_Hooks)) %>%
  select(-Hook_Size,-Spacing,-Hooks_per_Skate,-Num_Skates,-Num_Hooks) # %>% #,Num_Hooks)

#6) #Separate out multiple tickets
ll_log <- separate_rows(ll_log,"Ticket",sep=", ") 

#7) Take a piece of the "Ticket" that might match sequential number in fish ticket data base
ll_log <- ll_log %>% mutate(Ticket_subref = str_remove(str_sub(Ticket,nchar(Ticket)-2,nchar(Ticket)), "^0+"),
                          Trip.Number.log = Trip.Number)

# 8) Get number of sets for each ticket...
ll_log <- ll_log %>% group_by(Ticket, Year, ADFG.Number, Groundfish.Stat.Area) %>%
  mutate(set.count = n_distinct(Effort.Number))%>% ungroup()

# 9) Get numbers of hooks when num_of_hooks not available...
# Lots of error warnings from this bit because of incomplete data series.  That's OK.  Its working
hps<-max(stringr::str_count(ll_log$hooks_per_skate1, ","),na.rm=T)+1 #<-separate out multiple hooks-per-skate 

ll_log <-ll_log %>% 
  mutate(hps_orig = hooks_per_skate1) %>%
  separate(hps_orig, into = paste0("hps_",1:hps),sep = ",") %>%
  mutate(hps_1 = as.numeric(hps_1),
         hps_2 = as.numeric(hps_2),
         hps_3 = as.numeric(hps_3)) %>% 
  rowwise() %>%
  mutate(mean_hps = mean(c(hps_1,hps_2,hps_3),na.rm=TRUE)) %>% data.frame() %>%
  mutate(num_of_hooks1_exact = ifelse(!is.na(num_of_hooks1),num_of_hooks1,
                                                     as.numeric(num_of_skates1)*as.numeric(hooks_per_skate1)),
         num_of_hooks1_est = mean_hps*as.numeric(num_of_skates1),
         num_hooks_cpue = ifelse(!is.na(num_of_hooks1_exact),num_of_hooks1_exact,num_of_hooks1_est),
         hook_count_qual = ifelse(!is.na(num_of_hooks1_exact),"exact",
                                  ifelse(!is.na(num_of_hooks1_est),"estimated","not_available")),
         num_of_skates1_calc = ifelse(!is.na(num_of_skates1),num_of_skates1,
                                 as.numeric(num_of_hooks1)/as.numeric(hooks_per_skate1)), 
         num_of_hooks2_calc = as.numeric(num_of_skates2)*as.numeric(hooks_per_skate2),
         set_length_km = 1.609344*Set.Length..mi.,
         soak_time_hrs = Soak.Time.Hours,
         Ticket_log = Ticket,
         Species_log = Species,
         Pounds_log = Pounds,
         Numbers_log = Numbers) %>%
   select(-Ticket,-Pounds,-Species,-Numbers,-hps_1,-hps_2,-hps_3,)

### If there are issues you can skip down below to section labelled:
#   DEVELOPMENT CODE FOR FULL JOIN to see if you can resolve it... 
#   Otherwise it's time to join the fish ticket and logbook data.

# 10) Separate the logbook data out by halibut and sablefish
hal_ll_log<-ll_log %>% filter(Species_log == "Halibut")
sable_ll_log <- ll_log %>% filter(Species_log == "Sablefish")

# 11) Separate the fish tickets out by fishery
ll_ftx<-ftx %>% filter(Harvest.Code != 43,
                       Gear.Name == "Longline") %>% #get rid of survey fishtickets... 
  mutate(Ticket_subref = str_remove(str_sub(Sequential.Number,nchar(Sequential.Number)-2,nchar(Sequential.Number)), "^0+"),
         Trip.Number.FTX = Trip.Number)


hal_ll_ftx<-ll_ftx %>% filter(Fishery.Name %in% unique(ftx$Fishery.Name)[2:4])
sable_ll_ftx<-ll_ftx %>% filter(Fishery.Name %in% unique(ftx$Fishery.Name)[1])

# 12) Join the sablefish fishticket and logbook data! 
# 99% of the joins work using step 1, but 1% the sell and landing don't match so 
# ??? need two joins
#   12.Step1: Join using sell and landing date
for_ll_CPUE<-full_join(sable_ll_log, sable_ll_ftx, by = c("Year", "ADFG.Number" = "ADFG",
                                                   "Sell.Date" = "Date.of.Landing",
                                                   "Groundfish.Stat.Area" = "Stat.Area"),
                   multiple="all",
                   suffix = c("_log","_ftx")) %>% filter(!is.na(Fishery.Name)) #%>% #this gets rid of logbook entries with no matching fish tickets

nrow(for_ll_CPUE)

# identify fish ticket data with no matching logbooks.  Means that sell and landing dates don't match
# or there is just missing or misentered data f@!#$g this s$%t up... 
missing_ll_lb<-for_ll_CPUE %>% filter(is.na(Species_log)); nrow(missing_ll_lb)

ftx_not_in_ll_logs <- missing_ll_lb %>% select(Year, Sell.Date, Species.Code_ftx,Harvest.Code,Harvest.Name,
                                            Whole.Weight..detail.,Gear.Name,Port.Name,
                                            Fishery.Name,Batch.Number,Trip.Number_ftx,
                                            Sequential.Number,Record.ID, Ticket_subref_ftx,                       
                                            Trip.Number.FTX)
# 12.Step2: get the unmatched tickets...
unlogged_tix<-sable_ll_ftx %>% filter(#Ticket_subref %in% missing_lb$Ticket_subref_ftx,
                                   Year %in% missing_ll_lb$Year,
                                   ADFG %in% missing_ll_lb$ADFG,
                                   Stat.Area %in% missing_ll_lb$Groundfish.Stat.Area)
nrow(unlogged_tix)
# ...and join the unmatched tickets to the logbook data again using the 
#   Ticket_subref INSTEAD of sell and landing date
for_ll_CPUE_aux<-full_join(sable_ll_log, unlogged_tix, by = c("Year", "ADFG.Number" = "ADFG",
                                                       "Groundfish.Stat.Area" = "Stat.Area",
                                                       "Ticket_subref"),
                       multiple="all",
                       suffix = c("_log","_ftx")) #%>% filter(!is.na(Fishery.Name))

nrow(for_ll_CPUE_aux) #; colnames(for_ll_CPUE_aux)

#get rid of logbooks with no matching tickets in this one
for_ll_CPUE_aux<-for_ll_CPUE_aux %>% filter(!is.na(Species.Code_ftx)); nrow(for_ll_CPUE_aux)

#unique(for_ll_CPUE_aux$Species.Code_ftx)

#are there still fish ticket data missing logbooks? 
missing_ll_lb2<-for_ll_CPUE_aux %>% filter(is.na(Species_log)); nrow(missing_ll_lb2)
# of f$%&^*g course there are! 
missing_ll_lb2$LD<-as.Date(missing_ll_lb2$Date.of.Landing)
missing_ll_lb2$land_jday<-as.POSIXlt(missing_ll_lb2$LD)$yday
hist(missing_ll_lb2$land_jday, breaks=20)
abline(v=as.POSIXlt("2000-08-01")$yday, col="red")

hist(missing_ll_lb2$Year)

eg<-missing_ll_lb2[sample(nrow(missing_ll_lb2),1),]  #eg1 is row148 with no logbook anywhere...
                                                #row 257 with no logbook
eg
#is the missing record dealt with in the first join?
for_ll_CPUE %>% filter(!is.na(Species.Code_log),
                   Sell.Date == eg$Date.of.Landing, Year == eg$Year, 
                   ADFG.Number == eg$ADFG.Number, 
                   Groundfish.Stat.Area == eg$Groundfish.Stat.Area)
for_ll_CPUE %>% filter(!is.na(Species.Code_log),
                   Ticket_subref_ftx == eg$Ticket_subref, 
                   Year == eg$Year, 
                   ADFG.Number == eg$ADFG.Number, 
                   Groundfish.Stat.Area == eg$Groundfish.Stat.Area)
#no? is it anywhere in the logbook data
ll_log %>% filter(Sell.Date == eg$Date.of.Landing, 
                  Groundfish.Stat.Area == eg$Groundfish.Stat.Area,
                  ADFG.Number == eg$ADFG.Number)
ll_log %>% filter(Ticket_subref == eg$Ticket_subref)
ll_log %>% filter(Year == eg$Year, ADFG.Number == eg$ADFG.Number, Groundfish.Stat.Area == eg$Groundfish.Stat.Area)
sable_ll_log %>% filter(Year == eg$Year, ADFG.Number == eg$ADFG.Number)
#row number list of checking missing_lb2 that had no associated logbook data...
#148, 257, 96, 207, 
#row 156 was landed on 7-26-2015 which is prior to the sablefish fishery???  survey trip that wasn't properly recorded?

#******FLAG!!! Giving up on the missing/mismatched.  Groundfish biologists can try
#* to identify where this missing data is.  This represents <1% of the records as
#* far as I can tell.  Better quality control on data entry, storage and curration
#* would go a long way to alleviating these issue. -PJ

# 12.Step3: Lets try binding the two join data sets, get rid of duplicates and move
#           on to CPUE calculations... 
setdiff(colnames(for_ll_CPUE),colnames(for_ll_CPUE_aux))

for_ll_CPUE_aux %>% rename(Ticket_subref_log = Ticket_subref) %>%  #this works because aux join based on Ticket_subref
  mutate(Ticket_subref_ftx = Ticket_subref_log) -> for_ll_CPUE_aux

for_ll_CPUE %>% mutate(Date.of.Landing = Sell.Date)-> for_ll_CPUE  #

ncol(for_ll_CPUE); ncol(for_ll_CPUE_aux)
setdiff(colnames(for_ll_CPUE),colnames(for_ll_CPUE_aux))
setdiff(colnames(for_ll_CPUE_aux),colnames(for_ll_CPUE))

for_ll_CPUE<-unique(rbind(for_ll_CPUE,for_ll_CPUE_aux))
nrow(for_ll_CPUE)
# 13) Calculate CPUE for the logbook data

for_ll_CPUE %>% group_by(Year, ADFG.Number, Sell.Date, Groundfish.Stat.Area, Effort.Number) %>% 
  mutate(uniques = n(),
         partial_soak_time = soak_time_hrs/n(),
         partial_km_fished = set_length_km/n(),
         partial_hook_count_exact = as.numeric(num_of_hooks1_exact)/n(),
         partial_hook_count_est = as.numeric(num_of_hooks1_est)/n(),
         partial_hook_count_best_available = as.numeric(num_hooks_cpue)/n(),
         partial_hook_count2 = as.numeric(num_of_hooks2_calc)/n()) %>% 
  ungroup %>%
  group_by(Year, ADFG.Number, Sell.Date, Groundfish.Stat.Area) %>%
  mutate(set.count = set.count,
         trip_set_targets = ifelse(length(unique(Effort.Primary.Target.Species))>1,  #column that can use to filter for multiple targets in a trip
                               "hal_&_sable_mix",
                               paste0("all_",unique(Effort.Primary.Target.Species))),
         trip_recorded_releases = ifelse(length(unique(Disposition))>1,             #column for identifying when skippers recorded their releases
                                         paste0(unique(Disposition)[1],"_&_",unique(Disposition)[2]),
                                         paste0("all_",unique(Disposition))),
         
         multi_gear_config = ifelse(is.na(gear_target_2),"single_config","multi_config"), #column to flag when more than one gear configuration
                                                                                          #for analyzing CPUE versus things like hook size will want
                                                                                          #to filter out multi_config
         mean_trip_depth_fm = mean(Average.Depth.Fathoms),
         
         tix.ref.catch = Whole.Weight..sum.,
         catch = sum(unique(Whole.Weight..sum.)),
         pre.lbs_per_set = catch/set.count,
         
         total_soak_time = sum(partial_soak_time),
         total_km_fished = sum(partial_km_fished),
         
         total_hooks_exact = sum(partial_hook_count_exact),
         total_hooks_est = sum(partial_hook_count_est),
         total_hooks_all = sum(partial_hook_count_best_available),
         total_hooks_exact2 = sum(partial_hook_count2),
         
         lbs_p_set = sum(unique(catch))/set.count,
         lbs_p_set_km = sum(unique(catch))/set.count/total_km_fished,
         lbs_p_set_hr = sum(unique(catch))/set.count/total_soak_time,
         lbs_p_set_km_hr = sum(unique(catch))/set.count/total_km_fished/total_soak_time,
         
         lbs_p_hk_exact = sum(unique(catch))/total_hooks_exact,
         lbs_p_hk_km_exact = sum(unique(catch))/total_hooks_exact/total_km_fished,
         lbs_p_hk_hr_exact = sum(unique(catch))/total_hooks_exact/total_soak_time,
         lbs_p_hk_km_hr_exact = sum(unique(catch))/total_hooks_exact/total_km_fished/total_soak_time,
         
         lbs_p_hk_est = sum(unique(catch))/total_hooks_est,
         lbs_p_hk_km_est = sum(unique(catch))/total_hooks_est/total_km_fished,
         lbs_p_hk_hr_est = sum(unique(catch))/total_hooks_est/total_soak_time,
         lbs_p_hk_km_hr_est = sum(unique(catch))/total_hooks_est/total_km_fished/total_soak_time,
         
         lbs_p_hk_all = sum(unique(catch))/total_hooks_all,
         lbs_p_hk_km_all = sum(unique(catch))/total_hooks_all/total_km_fished,
         lbs_p_hk_hr_all = sum(unique(catch))/total_hooks_all/total_soak_time,
         lbs_p_hk_km_hr_all = sum(unique(catch))/total_hooks_all/total_km_fished/total_soak_time,
         
         lbs_p_hk_exact2 = sum(unique(catch))/total_hooks_exact2,
         lbs_p_hk_km_exact2 = sum(unique(catch))/total_hooks_exact2/total_km_fished,
         lbs_p_hk_hr_exact2 = sum(unique(catch))/total_hooks_exact2/total_soak_time,
         lbs_p_hk_km_hr_exact2 = sum(unique(catch))/total_hooks_exact2/total_km_fished/total_soak_time,
  ) -> Sable_ll_CPUE  #don't change! This attaches to joined

colnames(Sable_ll_CPUE)
#Quick look
QL<- Sable_ll_CPUE %>% select(Ticket_subref_log,Ticket_subref_ftx,
                           uniques,Year, Species_log, Species_tx = Species.Name,
                           Disposition, trip_recorded_releases,
                           Groundfish.Stat.Area, Effort.Primary.Target.Species,
                           Trip.Primary.Target.Species, trip_set_targets,
                           multi_gear_config,
                           Set.Length..mi.,Average.Depth.Fathoms, mean_trip_depth_fm,
                           Soak.Time.Hours,
                           Effort.Number,Trip.Primary.Target.Species,
                           Port.Code,ADFG.Number,
                           CFEC.Permit.Number,CFEC_tix=CFEC,
                           Sell.Date,
                           Trip.Number_log, gear_target_1, gear_target_2, hook_size1,
                           spacing1, hooks_per_skate1, num_of_skates1, num_of_hooks1,
                           hook_size2, spacing2, hooks_per_skate2, num_of_skates2,
                            
                           Trip.Number.log, set.count, mean_hps,
                           num_of_hooks1_exact, num_of_hooks1_est, num_hooks_cpue, hook_count_qual,
                           num_of_skates1_calc,
                           num_of_hooks2_calc, set_length_km, soak_time_hrs, Ticket_log,
                           Species_log, Pounds_log, Numbers_log,
                           Whole.Weight..sum.,Disposition.Name, 
                           Harvest.Name, Fishery.Name,
                           partial_soak_time, partial_km_fished, 
                           hook_count_qual,
                           partial_hook_count_exact, partial_hook_count_est,
                           partial_hook_count_best_available,partial_hook_count2,
                           tix.ref.catch,   catch, pre.lbs_per_set,
                           total_soak_time, total_km_fished, 
                           total_hooks_exact,total_hooks_est,total_hooks_all,total_hooks_exact2,
                           lbs_p_set,lbs_p_set_km,lbs_p_set_hr,lbs_p_set_km_hr,
                           lbs_p_hk_exact,lbs_p_hk_km_exact,lbs_p_hk_hr_exact,lbs_p_hk_km_hr_exact,
                           lbs_p_hk_est,lbs_p_hk_km_est,lbs_p_hk_hr_est,lbs_p_hk_km_hr_est,
                           lbs_p_hk_all,lbs_p_hk_km_all,lbs_p_hk_hr_all,lbs_p_hk_km_hr_all,
                           lbs_p_hk_exact2
                           )

# Note regarding two gear configurations.  When calculating final CPUE in analysis
# you will need to be mindful of gear_target_1 and _2.  When there are two sets of
# gear targeting different species (halibut and sablefish) the CPUE will NOT be
# useful for analysis because there is no way to separate out the catch by target 
# if they are both on the same ticket. :(  Filter out multi_gear_config = "multi_config"

# Will also need to be mindful of disposition.  Releases are recorded in some logbooks.
# When this happens looking up a particular landing will show two entries; one retained
# and one released (or some combo depending on sets).  Catch will reflect the landing
# but release data on the Release logbook may be in Pounds_log and Numbers_log,  

# 14) Spot checking to make sure things look good... 
# This step will allow you to grab a random year and then grab a boat, logbook ticket, 
# and management area to check that the calculations make sense.

#function to grab random year, stat area, ADFG.no and sell date... 
random_check<-function(data){ #data<-mixed_targets
  #length(unique(Sable_ll_CPUE$Year))
  data<-data
  
  year_list<-unique(data$Year)
  year_check<-sample(year_list,1)
  
  data<-data %>% filter(Year == year_check)
  
  adfg_list<-unique(data$ADFG.Number)
  adfg_check<-adfg_list[sample(length(adfg_list),1)]
  
  data<-data %>% filter(ADFG.Number == adfg_check)
  
  sd_list<-unique(data$Sell.Date)
  sd_check<-sd_list[sample(length(sd_list),1)]
  
  data<-data %>% filter(Sell.Date == sd_check)
  
  gsa_list<-unique(data$Groundfish.Stat.Area)
  gsa_check<-gsa_list[sample(length(gsa_list),1)]
  
  return<-list(year_check,adfg_check,sd_check,gsa_check) #rands<-return
}

#missing logbooks... 
nrow(QL)
nrow(QL %>% filter(is.na(Species_log)))  
missing_lb<-QL %>% filter(is.na(Species_log))

#released records
releases<-QL %>% filter(Disposition == "Released"); nrow(releases)

#mixed trips
mixed_targets<-QL %>% filter(trip_set_targets == "hal_&_sable_mix"); nrow(mixed_targets)

#Get random landing and check it out... 
rands<-random_check(data=QL) #can run this on QL, Sable_ll_CPUE, missing_lb, releases, etc... 
#saverands<-rands #if you want to save a particular set to come back to

check<-as.data.frame(QL %>% filter(Sell.Date == rands[[3]][1],
                                  Groundfish.Stat.Area == rands[[4]][1],
                                  Year == rands[[1]][1], #unique(Sable_ll_CPUE$Year)[rands[[1]][1]],
                                  ADFG.Number == rands[[2]][1])); check
check %>% select(Effort.Number,total_km_fished,set_length_km,
                 total_soak_time,soak_time_hrs,
                 total_hooks_exact,num_of_hooks1_exact)

unique(check$Effort.Number)
unique(check$total_km_fished)
unique(check$set_length_km)
check$Set.Length..mi.
sum(unique(check$set_length_km)) #this check won't always match if skipper recorded same
                                 # trip length on different sets.  

unique(check$total_soak_time)
unique(check$soak_time_hrs)
sum(unique(check$soak_time_hrs)) #may not match if skipper recorded same soak times on multiple sets... 

unique(check$num_of_hooks1_exact) #wont match because same number of hooks on multiple sets
unique(check$total_hooks_exact)


ll_log %>% filter(ADFG.Number==20105, Year == 2002, Groundfish.Stat.Area == 345631,
                  Sell.Date == "2002-09-14 00:00:00")
ll_log %>% filter(ADFG.Number==20105, Year == 2002, Groundfish.Stat.Area == 345631,
                  Sell.Date == "2002-09-15 00:00:00")

ftx %>% filter(ADFG == 20105, Year == 2002, Stat.Area == 345631)
missing_tix<-ftx %>% filter(ADFG == 20105, Year == 2002, Stat.Area == 345631, Date.of.Landing == "2002-09-14 00:00:00")

ll_log %>% filter(ADFG.Number==20105, Year == 2002, Groundfish.Stat.Area == 345631,
                  Ticket_subref == missing_tix$Ticket_subref)
#THIS IS THE SABLEFISH CPUE FOR THIS TICKET!! 
# This is how things will be grouped in the analysis to get specific CPUE values
#-----
unique(as.data.frame(Sable_ll_CPUE %>% filter(Sell.Date == rands[[3]][1],
                                           Groundfish.Stat.Area == rands[[4]][1],
                                           Year == rands[[1]][1],
                                           ADFG.Number == rands[[2]][1]))$lbs_p_set)
unique(as.data.frame(Sable_ll_CPUE %>% filter(Sell.Date == rands[[3]][1],
                                           Groundfish.Stat.Area == rands[[4]][1],
                                           Year == rands[[1]][1],
                                           ADFG.Number == rands[[2]][1]))$lbs_p_set_km)
unique(as.data.frame(Sable_ll_CPUE %>% filter(Sell.Date == rands[[3]][1],
                                           Groundfish.Stat.Area == rands[[4]][1],
                                           Year == rands[[1]][1],
                                           ADFG.Number == rands[[2]][1]))$lbs_p_set_hr)
unique(as.data.frame(Sable_ll_CPUE %>% filter(Sell.Date == rands[[3]][1],
                                           Groundfish.Stat.Area == rands[[4]][1],
                                           Year == rands[[1]][1],
                                           ADFG.Number == rands[[2]][1]))$lbs_p_set_km_hr)
unique(as.data.frame(Sable_ll_CPUE %>% filter(Sell.Date == rands[[3]][1],
                                           Groundfish.Stat.Area == rands[[4]][1],
                                           Year == rands[[1]][1],
                                           ADFG.Number == rands[[2]][1]))$lbs_p_hk_exact)
unique(as.data.frame(Sable_ll_CPUE %>% filter(Sell.Date == rands[[3]][1],
                                           Groundfish.Stat.Area == rands[[4]][1],
                                           Year == rands[[1]][1],
                                           ADFG.Number == rands[[2]][1]))$lbs_p_hk_km_exact)
unique(as.data.frame(Sable_ll_CPUE %>% filter(Sell.Date == rands[[3]][1],
                                           Groundfish.Stat.Area == rands[[4]][1],
                                           Year == rands[[1]][1],
                                           ADFG.Number == rands[[2]][1]))$lbs_p_hk_hr_exact)
unique(as.data.frame(Sable_ll_CPUE %>% filter(Sell.Date == rands[[3]][1],
                                           Groundfish.Stat.Area == rands[[4]][1],
                                           Year == rands[[1]][1],
                                           ADFG.Number == rands[[2]][1]))$lbs_p_hk_km_hr_exact)

# 15) Save the raw data a
# 2023 will save to legacy folder for use going forward
# in 2024 you will pull the legacy data, add the new year to it and resave if you
# choose to just pull the new year of data.  It may be better to just pull the same
# OceanAK queries set up in my folder as they are set up to pull every year from 
# 1997 until today...

write_csv(Sable_ll_CPUE, paste0("legacy_data/fishery/raw_data/fishery_ll_cpue_vers23_",
                             min(Sable_ll_CPUE$Year),"-",YEAR,".csv",sep=""))

#check some final things out...
{colnames(Sable_ll_CPUE)

unique(Sable_ll_CPUE$Disposition) #Logbook Data
nrow(Sable_ll_CPUE %>% filter (Disposition == "Released")) #955!!! 
as.data.frame(head(Sable_ll_CPUE %>% filter (Disposition == "Released"),10))  #CHECK!!

unique(Sable_ll_CPUE$Depredation)
nrow(Sable_ll_CPUE %>% filter (Depredation == "Orca"))
as.data.frame(head(Sable_ll_CPUE %>% filter (Depredation == "Orca"),10))  #OK ... but need to include to exclude for CPUE... 

unique(Sable_ll_CPUE$Sablefish.Discards)

unique(Sable_ll_CPUE$Longline.System)

unique(Sable_ll_CPUE$Gear)

unique(Sable_ll_CPUE$Disposition.Name) #Fish ticket data
nrow(Sable_ll_CPUE %>% filter (Disposition.Name == "Disc at sea"))  #CHECK
nrow(Sable_ll_CPUE %>% filter (Disposition.Name == "Disc dock"))  #CHECK

unique(Sable_ll_CPUE$Discard.at.Sea)
nrow(Sable_ll_CPUE %>% filter (Discard.at.Sea == "Yes")) #matches Disposition.Name == "Disc at sea"
nrow(Sable_ll_CPUE %>% filter (Discard.at.Sea == "No")) #matches Disposition.Name == "Disc at sea"
as.data.frame(head(Sable_ll_CPUE %>% filter (Discard.at.Sea == "Yes"),10)) 

unique(Sable_ll_CPUE$Harvest.Name)

unique(Sable_ll_CPUE$Fishery.Name)

unique(Sable_ll_CPUE$Depredation)
nrow(Sable_ll_CPUE %>% filter (Depredation == "Orca"))
as.data.frame(head(Sable_ll_CPUE %>% filter (Depredation == "Orca"),10)) 

unique(Sable_ll_CPUE$Depredation)
nrow(Sable_ll_CPUE %>% filter (Depredation == "Orca"))
as.data.frame(head(Sable_ll_CPUE %>% filter (Depredation == "Orca"),10)) }
#################################################################################
##*******************************************************************************
##* POT CPUE
##* *****************************************************************************
#################################################################################
str(pot_log)
unique(pot_log$Pot.Dimensions) #[1]
unique(pot_log$Pot.Type)
unique(pot_log$Number.of.Pots)
unique(pot_log$Pot.Spacing.Feet)
unique(pot_log$Groundline.Diameter.Inches)
unique(pot_log$First.Ticket)
unique(pot_log$All.Tickets)

#Pot gear configuration looks MUCH EASIER than longline configurations!!! Hallelujah!
# can skip to step 6 equivalent of longline logbook shitshow...
#1) #Separate out multiple tickets
pot_log <- separate_rows(pot_log,"All.Tickets",sep=", ") 

#2) Take a piece of the "Ticket" that might match sequential number in fish ticket data base
pot_log <- pot_log %>% mutate(Ticket_subref = str_remove(str_sub(All.Tickets,nchar(All.Tickets)-2,nchar(All.Tickets)), "^0+"),
                            Trip.Number.log = Trip.Number,
                            mixed_flag = ifelse(grepl("Mixed Gear Trip", Trip.Comments, ignore.case = TRUE),
                                                "MIXED_GEAR!","no_flag"))

# 3) Get number of sets for each ticket...
pot_log <- pot_log %>% group_by(All.Tickets, Year, ADFG.Number, Groundfish.Stat.Area) %>%
  mutate(set.count = n_distinct(Effort.Number))%>% ungroup()

# 4) A little light cleaning...

pot_log <-pot_log %>% 
  mutate(set_length_km = 1.609344*Set.Length..mi.,
         soak_time_hrs = Soak.Time.Hours,
         Ticket_log = All.Tickets,
         Species_log = Species,
         Pounds_log = Pounds,
         Numbers_log = Numbers) %>%
  select(-All.Tickets,-Pounds,-Species,-Numbers)

# 5) Separate the logbook data out by halibut and sablefish
hal_pot_log<-pot_log %>% filter(Species_log == "Halibut")
sable_pot_log <- pot_log %>% filter(Species_log == "Sablefish")

# 6) Separate the fish tickets out by fishery
pot_ftx<-ftx %>% filter(Harvest.Code != 43,
                        Gear.Name == "Pot") %>% #get rid of survey fishtickets... 
  mutate(Ticket_subref = str_remove(str_sub(Sequential.Number,nchar(Sequential.Number)-2,nchar(Sequential.Number)), "^0+"),
         Trip.Number.FTX = Trip.Number)


unique(ftx$Fishery.Name)  #FLAG!!! There is no sablefish pot gear in fishery name!!!!!! only long line!!! 
# 2-8-23 going to start by using all the fish tickets and see whay it looks like... 

hal_pot_ftx<-pot_ftx %>% filter(Fishery.Name %in% unique(ftx$Fishery.Name)[2:4])
sable_pot_ftx<-pot_ftx %>% filter(Fishery.Name %in% unique(ftx$Fishery.Name)[1])

# 7) Join the sablefish fishticket and logbook data! 

#   7.Step1: Join using sell and landing date
for_pot_CPUE<-full_join(sable_pot_log, pot_ftx, by = c("Year", "ADFG.Number" = "ADFG",
                                                   "Sell.Date" = "Date.of.Landing",
                                                   "Groundfish.Stat.Area" = "Stat.Area"),
                   multiple="all",
                   suffix = c("_log","_ftx"))# %>% filter(!is.na(Fishery.Name)) #%>% #this gets rid of logbook entries with no matching fish tickets

nrow(for_pot_CPUE)

# identify fish ticket data with no matching logbooks.  
missing_pot_lb<-for_pot_CPUE %>% filter(is.na(Species_log)); nrow(missing_pot_lb)  

"https://media1.tenor.com/images/e986f3cda38e718a181ce57cfad77fe4/tenor.gif?itemid=4953579"

#!!! FLAG!!!!! 0 unmatched tickets!!! wow!! Way to go Rhea and groundfish team!!!! 

complete_pot<-for_pot_CPUE %>% filter(!is.na(Species_log)); nrow(complete_pot)

head(ftx_not_in_ll_logs)

pot_sn<-unique(complete_pot$Sequential.Number)
ll_missing_sn<- unique(ftx_not_in_ll_logs$Sequential.Number)

intersect_all <- function(a,b,...){
  Reduce(intersect, list(a,b,...))
}

intersect_all(pot_sn,ll_missing_sn)

#looks like 2 of the missing logbooks from the longline exam are in the pot logbooks
#... but not all... Just some of the missing 2022 data... 
pot_rid<-unique(complete_pot$Record.ID)
ll_missing_rid<- unique(ftx_not_in_ll_logs$Record.ID)

intersect_all(pot_rid,ll_missing_rid)

head(as.data.frame(for_pot_CPUE))

# 7.Step2: get the unmatched tickets...
# Since the groundfish team crushed it in 2022 and there are no unmatched pot fish tickets
# we can skip this step.Keep it up in '23!!! <<thmbs up emoji>>
{colnames(missing_pot_lb)
unlogged_pot_tix<-sable_ftx %>% filter(#Ticket_subref %in% missing_lb$Ticket_subref_ftx,
  Year %in% missing_pot_lb$Year,
  ADFG %in% missing_pot_lb$ADFG.Number,
  Stat.Area %in% missing_pot_lb$Groundfish.Stat.Area)
nrow(unlogged_pot_tix)
# ...and join the unmatched tickets to the logbook data again using the 
#   Ticket_subref INSTEAD of sell and landing date
forpotCPUE_aux<-full_join(sable_pot_log, unlogged_pot_tix, by = c("Year", "ADFG.Number" = "ADFG",
                                                          "Groundfish.Stat.Area" = "Stat.Area",
                                                          "Ticket_subref"),
                       multiple="all",
                       suffix = c("_log","_ftx")) #%>% filter(!is.na(Fishery.Name))

nrow(forpotCPUE_aux) #; colnames(for_ll_CPUE_aux)

#get rid of logbooks with no matching tickets in this one
forpotCPUE_aux<-forpotCPUE_aux %>% filter(!is.na(Species.Code_ftx)); nrow(forpotCPUE_aux)

unique(forpotCPUE_aux$Species.Code_ftx)

#are there still fish ticket data missing logbooks? 
missing_pot_lb2<-forpotCPUE_aux %>% filter(is.na(Species_log)); nrow(missing_pot_lb2)
# of course there are! 
missing_pot_lb2$LD<-as.Date(missing_pot_lb2$Date.of.Landing)
missing_pot_lb2$land_jday<-as.POSIXlt(missing_pot_lb2$LD)$yday
hist(missing_pot_lb2$land_jday, breaks=20)
abline(v=as.POSIXlt("2000-08-01")$yday, col="red")

hist(missing_pot_lb2$Year)

# 7.Step3: Lets try binding the two join data sets, get rid of duplicates and move
#           on to CPUE calculations... 
setdiff(colnames(for_pot_CPUE),colnames(forpotCPUE_aux))

forpotCPUE_aux %>% rename(Ticket_subref_log = Ticket_subref) %>%  #this works because aux join based on Ticket_subref
  mutate(Ticket_subref_ftx = Ticket_subref_log) -> forpotCPUE_aux

for_pot_CPUE %>% mutate(Date.of.Landing = Sell.Date)-> for_pot_CPUE  #

ncol(for_pot_CPUE); ncol(forpotCPUE_aux)
setdiff(colnames(for_pot_CPUE),colnames(forpotCPUE_aux))
setdiff(colnames(forpotCPUE_aux),colnames(for_pot_CPUE))

for_pot_CPUE<-unique(rbind(for_pot_CPUE,forpotCPUE_aux))
nrow(for_pot_CPUE)

#lots of unmatched tickets, mostly because its all fish ticket data and pot fishing
# just started in 2022

for_pot_CPUE %>% filter(!is.na(Species.Code_log)) -> for_pot_CPUE
nrow(for_pot_CPUE)
}

# 8) Calculate CPUE for the logbook data

# **** FLAG **** at some point we need to factor in boats that fished both longline
#                and pot gear in the same trip. First I'm just going to run the pot 
#                CPUE estimates blindly to and then go through again and combine the
#                2022 longline and pot trips to identify mixed gear trips... 
colnames(for_pot_CPUE)

for_pot_CPUE %>% group_by(Year, ADFG.Number, Sell.Date, Groundfish.Stat.Area, Effort.Number) %>% 
  mutate(uniques = n(),
         partial_soak_time = soak_time_hrs/n(),
         partial_km_fished = set_length_km/n(),
         partial_pot_count = as.numeric(Number.of.Pots)/n()) %>% 
  ungroup %>%
  group_by(Year, ADFG.Number, Sell.Date, Groundfish.Stat.Area) %>%
  mutate(set.count = set.count,  #for_pot_CPUE$set.count
         trip_set_targets = ifelse(length(unique(Effort.Primary.Target.Species))>1,  #column that can use to filter for multiple targets in a trip
                                   "hal_&_sable_mix",
                                   paste0("all_",unique(Effort.Primary.Target.Species))),
         trip_recorded_releases = ifelse(length(unique(Disposition))>1,             #column for identifying when skippers recorded their releases
                                         paste0(unique(Disposition)[1],"_&_",unique(Disposition)[2]),
                                         paste0("all_",unique(Disposition))),
         
         mean_trip_depth_fm = mean(Average.Depth.Fathoms),
         
         tix.ref.catch = Whole.Weight..sum.,
         catch = sum(unique(Whole.Weight..sum.)),
         pre.lbs_per_set = catch/set.count,
         
         total_soak_time = sum(partial_soak_time),
         total_km_fished = sum(partial_km_fished),
         
         total_pots = sum(partial_pot_count),
         
         lbs_p_set = sum(unique(catch))/set.count,
         lbs_p_set_km = sum(unique(catch))/set.count/total_km_fished,
         lbs_p_set_hr = sum(unique(catch))/set.count/total_soak_time,
         lbs_p_set_km_hr = sum(unique(catch))/set.count/total_km_fished/total_soak_time,
         
         lbs_p_pot = sum(unique(catch))/total_pots,
         lbs_p_pot_km = sum(unique(catch))/total_pots/total_km_fished,
         lbs_p_pot_hr = sum(unique(catch))/total_pots/total_soak_time,
         lbs_p_pot_km_hr = sum(unique(catch))/total_pots/total_km_fished/total_soak_time,
         
  ) -> Sable_pot_CPUE  #don't change! This attaches to joined

colnames(Sable_pot_CPUE)
colnames(pot_log)
#Quick look
QL<- Sable_pot_CPUE %>% select(Ticket_subref_log,Ticket_subref_ftx,
                           uniques,Year, Species_log, Species_tx = Species.Name,
                           Disposition, trip_recorded_releases,
                           Groundfish.Stat.Area, Effort.Primary.Target.Species,
                           Trip.Target.Species, trip_set_targets,
                           
                           Set.Length..mi.,set_length_km,
                           Average.Depth.Fathoms, mean_trip_depth_fm,
                           Soak.Time.Hours,
                           Effort.Number,
                           Port.Code,ADFG.Number,
                           CFEC.Permit.Number,CFEC_tix=CFEC,
                           Sell.Date,
                           Trip.Number_log, 
                           
                           Pot.Dimensions, Pot.Type, Number.of.Pots,                      
                           
                           Trip.Number.log, set.count, soak_time_hrs, Ticket_log,
                           Species_log, Pounds_log, Numbers_log,
                           Whole.Weight..sum.,Disposition.Name, 
                           Harvest.Name, Fishery.Name,
                           partial_soak_time, partial_km_fished, 
                           
                           partial_pot_count, catch, pre.lbs_per_set,
                           total_soak_time, total_km_fished,
                           total_pots,
                           lbs_p_set, lbs_p_set_km, lbs_p_set_hr, lbs_p_set_km_hr,
                           lbs_p_pot, lbs_p_pot_km, lbs_p_pot_hr, lbs_p_pot_km_hr
)

# Note regarding two gear configurations.  When calculating final CPUE in analysis
# you will need to be mindful of gear_target_1 and _2.  When there are two sets of
# gear targeting different species (halibut and sablefish) the CPUE will NOT be
# useful for analysis because there is no way to separate out the catch by target 
# if they are both on the same ticket. :(  Filter out multi_gear_config = "multi_config"

# Will also need to be mindful of disposition.  Releases are recorded in some logbooks.
# When this happens looking up a particular landing will show two entries; one retained
# and one released (or some combo depending on sets).  Catch will reflect the landing
# but release data on the Release logbook may be in Pounds_log and Numbers_log,  

# 14) Spot checking to make sure things look good... 
# This step will allow you to grab a random year and then grab a boat, logbook ticket, 
# and management area to check that the calculations make sense.

#function to grab random year, stat area, ADFG.no and sell date... 
random_check<-function(data){ #data<-QL
  #length(unique(Sable_ll_CPUE$Year))
  data<-data
  
  year_list<-unique(data$Year)
  year_check<-sample(c(year_list,year_list),1)
  
  data<-data %>% filter(Year == year_check)
  
  adfg_list<-unique(data$ADFG.Number)
  adfg_check<-adfg_list[sample(length(adfg_list),1)]
  
  data<-data %>% filter(ADFG.Number == adfg_check)
  
  sd_list<-unique(data$Sell.Date)
  sd_check<-sd_list[sample(length(sd_list),1)]
  
  data<-data %>% filter(Sell.Date == sd_check)
  
  gsa_list<-unique(data$Groundfish.Stat.Area)
  gsa_check<-gsa_list[sample(length(gsa_list),1)]
  
  return<-list(year_check,adfg_check,sd_check,gsa_check) #rands<-return
}

#missing logbooks... 
nrow(QL)
nrow(QL %>% filter(is.na(Species_log)))  
missing_lb<-QL %>% filter(is.na(Species_log))

#released records
releases<-QL %>% filter(Disposition == "Released"); nrow(releases)

#mixed trips
mixed_targets<-QL %>% filter(trip_set_targets == "hal_&_sable_mix"); nrow(mixed_targets)

#Get random landing and check it out... 
rands<-random_check(data=QL) #can run this on QL, Sable_ll_CPUE, missing_lb, releases, etc... 
#saverands<-rands #if you want to save a particular set to come back to

check<-as.data.frame(QL %>% filter(Sell.Date == rands[[3]][1],
                                   Groundfish.Stat.Area == rands[[4]][1],
                                   Year == rands[[1]][1], #unique(Sable_ll_CPUE$Year)[rands[[1]][1]],
                                   ADFG.Number == rands[[2]][1])); check
unique(check)
check %>% select(Effort.Number, set.count,
                 total_km_fished,set_length_km,
                 total_soak_time,soak_time_hrs,
                 total_pots)

unique(check$Effort.Number)
unique(check$total_km_fished)
unique(check$set_length_km)
check$Set.Length..mi.
sum(unique(check$set_length_km)) #this check won't always match if skipper recorded same
# trip length on different sets.  

unique(check$total_soak_time)
unique(check$soak_time_hrs)
sum(unique(check$soak_time_hrs)) #may not match if skipper recorded same soak times on multiple sets... 

unique(check$Number.of.Pots) #wont match because same number of hooks on multiple sets
unique(check$total_pots)


{ll_log %>% filter(ADFG.Number==20105, Year == 2002, Groundfish.Stat.Area == 345631,
                  Sell.Date == "2002-09-14 00:00:00")
ll_log %>% filter(ADFG.Number==20105, Year == 2002, Groundfish.Stat.Area == 345631,
                  Sell.Date == "2002-09-15 00:00:00")

ftx %>% filter(ADFG == 20105, Year == 2002, Stat.Area == 345631)
missing_tix<-ftx %>% filter(ADFG == 20105, Year == 2002, Stat.Area == 345631, Date.of.Landing == "2002-09-14 00:00:00")

ll_log %>% filter(ADFG.Number==20105, Year == 2002, Groundfish.Stat.Area == 345631,
                  Ticket_subref == missing_tix$Ticket_subref)}
#THIS IS THE SABLEFISH CPUE FOR THIS TICKET!! 
# This is how things will be grouped in the analysis to get specific CPUE values
#-----
unique(as.data.frame(Sable_pot_CPUE %>% filter(Sell.Date == rands[[3]][1],
                                           Groundfish.Stat.Area == rands[[4]][1],
                                           Year == rands[[1]][1],
                                           ADFG.Number == rands[[2]][1]))$lbs_p_set)
unique(as.data.frame(Sable_pot_CPUE %>% filter(Sell.Date == rands[[3]][1],
                                           Groundfish.Stat.Area == rands[[4]][1],
                                           Year == rands[[1]][1],
                                           ADFG.Number == rands[[2]][1]))$lbs_p_set_km)
unique(as.data.frame(Sable_pot_CPUE %>% filter(Sell.Date == rands[[3]][1],
                                           Groundfish.Stat.Area == rands[[4]][1],
                                           Year == rands[[1]][1],
                                           ADFG.Number == rands[[2]][1]))$lbs_p_set_hr)
unique(as.data.frame(Sable_pot_CPUE %>% filter(Sell.Date == rands[[3]][1],
                                           Groundfish.Stat.Area == rands[[4]][1],
                                           Year == rands[[1]][1],
                                           ADFG.Number == rands[[2]][1]))$lbs_p_set_km_hr)
unique(as.data.frame(Sable_pot_CPUE %>% filter(Sell.Date == rands[[3]][1],
                                           Groundfish.Stat.Area == rands[[4]][1],
                                           Year == rands[[1]][1],
                                           ADFG.Number == rands[[2]][1]))$lbs_p_pot)
unique(as.data.frame(Sable_pot_CPUE %>% filter(Sell.Date == rands[[3]][1],
                                           Groundfish.Stat.Area == rands[[4]][1],
                                           Year == rands[[1]][1],
                                           ADFG.Number == rands[[2]][1]))$lbs_p_pot_km)
unique(as.data.frame(Sable_pot_CPUE %>% filter(Sell.Date == rands[[3]][1],
                                           Groundfish.Stat.Area == rands[[4]][1],
                                           Year == rands[[1]][1],
                                           ADFG.Number == rands[[2]][1]))$lbs_p_pot_hr)
unique(as.data.frame(Sable_pot_CPUE %>% filter(Sell.Date == rands[[3]][1],
                                           Groundfish.Stat.Area == rands[[4]][1],
                                           Year == rands[[1]][1],
                                           ADFG.Number == rands[[2]][1]))$lbs_p_pot_km_hr)

# 15) Save the raw data a
# 2023 will save to legacy folder for use going forward
# in 2024 you will pull the legacy data, add the new year to it and resave if you
# choose to just pull the new year of data.  It may be better to just pull the same
# OceanAK queries set up in my folder as they are set up to pull every year from 
# 1997 until today...

write_csv(Sable_pot_CPUE, paste0("legacy_data/fishery/raw_data/fishery_pot_cpue_vers23_",
                             min(Sable_pot_CPUE$Year),"-",YEAR,".csv",sep=""))

##################################################################################
#*********************************************************************************
#* Pot fishing for sablefish started in 2022 and some skippers fished both pot and
#*  longline gear on the same trips.  Because landings are not separated by gear
#*  we will need to identify those trips that fished mixed gear and remove them 
#*  from both the Sable_ll_CPUE and Sable_pot_CPUE data frames and resave.  
#*  Then we will need to create a new data frame called Sable_mix_CPUE for the
#*  mixed gear trips
#*  *****************************************************************************


colnames(ll_log)
sable_ll_log %>% filter (Year >= 2022) -> ll_log_22on
sable_pot_log %>% filter (Year >= 2022)  -> pot_log_22on 

rbind(sable_ll_ftx %>% filter (Year >= 2022),
      sable_pot_ftx %>% filter (Year >=2022)) -> sable_ftx_22on

#get a sample row from the pot ll book
pot_samp<-sample(pot_log_22on,1)
saved_pot_samp<-pot_samp

#get all matching rows from that sample
pot_samp<-pot_log_22on %>% filter(ADFG.Number == pot_samp$ADFG.Number,
                             Sell.Date == pot_samp$Sell.Date,
                             Groundfish.Stat.Area == pot_samp$Groundfish.Stat.Area)

data.frame(pot_samp)
#is there a match for this one in the ll logs?
ll_samp <- ll_log_22on %>% filter(ADFG.Number %in% pot_samp$ADFG.Number,
                            Sell.Date %in% pot_samp$Sell.Date,
                            Groundfish.Stat.Area %in% pot_samp$Groundfish.Stat.Area)
data.frame(ll_samp)

pot_samp$Effort.Number; pot_samp$set.count
ll_samp$Effort.Number; ll_samp$set.count

tx_samp <- sable_ftx_22on %>% filter(ADFG %in% pot_samp$ADFG.Number,
                                     Date.of.Landing %in% pot_samp$Sell.Date,
                                     Stat.Area %in% pot_samp$Groundfish.Stat.Area)
 tx_samp
 tx_samp$Gear.Name
 #Example 1: ADFG 45454, stat area 345702 landed on "2022-10-13 00:00:00" - main target with ll was halibut
 #     and fished a 5 pot sets
 
 #!!! Effort number are continuous... this might be useful in joining or binding... 
 #Lets look at the ticket for this one...
 #Woohoo!!! two tickets ... on with pot and one with longline!!!  Of course, in this instant the 
 # longline landings were bycatch from halibut targets... 
 
 ## Some test driving seems to indicate that the trip comments in the pot longline book
 ## may be enough to find the mixed trips
 # So... lets see if there are any matches for pot_logs that aren't flagged as a 
 # mixed gear trip...
 
 purepot <-pot_log_22on %>% filter(mixed_flag == "no_flag")
 
 nrow(purepot)
 
 ll_samp <- ll_log_22on %>% filter(ADFG.Number %in% purepot$ADFG.Number &
                                   Sell.Date %in% purepot$Sell.Date &
                                   Groundfish.Stat.Area %in% purepot$Groundfish.Stat.Area)
 data.frame(ll_samp)  #get 32 records that match so the flags from the comments aren't perfect... 
 
 for (i in 1:nrow(ll_samp)) {
   potcheck<- purepot %>% filter (ADFG.Number %in% ll_samp$ADFG.Number[i] &
                                    Sell.Date %in% ll_samp$Sell.Date[i] &
                                    Groundfish.Stat.Area %in% ll_samp$Groundfish.Stat.Area[i])
   ll_match <- ll_samp %>% filter(ADFG.Number %in% ll_samp$ADFG.Number[i] &
                                    Sell.Date %in% ll_samp$Sell.Date[i] &
                                    Groundfish.Stat.Area %in% ll_samp$Groundfish.Stat.Area[i])
   tx_samp <- sable_ftx_22on %>% filter(ADFG %in% ll_match$ADFG.Number,
                                        Date.of.Landing %in% ll_match$Sell.Date,
                                        Stat.Area %in% ll_match$Groundfish.Stat.Area)
   print(paste0(i," ",tx_samp$Gear.Name))
 }
 
 rec<-10
 potcheck<- purepot %>% filter (ADFG.Number %in% ll_samp$ADFG.Number[rec] &
                                  Sell.Date %in% ll_samp$Sell.Date[rec] &
                                  Groundfish.Stat.Area %in% ll_samp$Groundfish.Stat.Area[rec])
 
data.frame(potcheck)            
ll_samp[rec,]

ll_match <- ll_samp %>% filter(ADFG.Number %in% ll_samp$ADFG.Number[rec] &
                                    Sell.Date %in% ll_samp$Sell.Date[rec] &
                                    Groundfish.Stat.Area %in% ll_samp$Groundfish.Stat.Area[rec])
ll_match

tx_samp <- sable_ftx_22on %>% filter(ADFG %in% ll_match$ADFG.Number,
                                     Date.of.Landing %in% ll_match$Sell.Date,
                                     Stat.Area %in% ll_match$Groundfish.Stat.Area)
tx_samp
tx_samp$Gear.Name
 
#however, after looking through this I don't seem any evidence that landings weren't
#separated by gear.  So no need to have a mixed_gear_CPUE.  The mixed trips are of
# corse dependent on skippers making an honest effort to wuantify what theyve landed 
# from the different gear type, but thats the best we can do.  

# For analysis it might be worthwhile to separate the one-gear trips from multi-gear trips
# which will, of course, be a pain in the ass. :(
colnames(Sable_ll_CPUE)
colnames(Sable_pot_CPUE)

sable_ll_ftx %>% filter(Trip.Number.FTX == 6511920220290,
                        Date.of.Landing == "2022-10-01 00:00:00")  #nope, has multipple landing dates... 
sable_pot_ftx %>% filter(Trip.Number.FTX == 6511920220290,
                         Date.of.Landing == "2022-10-01 00:00:00")

Sable_ll_CPUE %>% 
  rowwise() %>% 
  mutate(multigear_trip = ifelse(nrow(as.data.frame(Sable_pot_CPUE[Sable_pot_CPUE$Trip.Number.FTX == Trip.Number.FTX &
                                                  Sable_pot_CPUE$Sell.Date == Sell.Date,]))>0,
                                 "pot_&_ll_trip","ll_trip")) %>%
  data.frame()

###FLAG for 2-10!!! make sure this worked!!! and then do it for the pot gear and resave those csv files.!!! 

ex<-Sable_ll_CPUE %>% filter(Year == 1997,ADFG.Number == 7317, 
                             Sell.Date == "1997-09-04 00:00:00",
                             Groundfish.Stat.Area == 345701)
ex<-ex[c(1:3),]

nrow(Sable_pot_CPUE %>% filter(Trip.Number.FTX == ex$Trip.Number.FTX &
                            Sell.Date == ex$Sell.Date))

ex<-ex %>% 
  rowwise() %>%
  mutate(multigear_trip = ifelse(nrow(as.data.frame(Sable_pot_CPUE[Sable_pot_CPUE$Trip.Number.FTX == Trip.Number.FTX &
                                                                     Sable_pot_CPUE$Sell.Date == Sell.Date,]))>0,
                                 "pot_&_ll_trip","ll_trip")) %>%
  data.frame()

data.frame(ex)
ex
Sable_pot_CPUE[Sable_pot_CPUE$Trip.Number.FTX == ex$Trip.Number.FTX &
                 Sable_pot_CPUE$Sell.Date == ex$Sell.Date,]
##################################################################################
##*****************************************************************************###
##*   DEVELOPMENT CODE FOR FULL JOIN... This starts after Step 9 above... 
##*  This code breaks out pieces of the logbook and fishticket data and 
##*  walks through the process of joining them, calculating CPUE and making
##*  sure everything makes sense.
##******************************************************************************##
#################################################################################
ll_lb<-ll_log
colnames(ll_lb)

unique(ll_lb$num_of_hooks1)
nrow(ll_lb[is.na(ll_lb$num_of_hooks1),])/nrow(ll_lb)
nrow(ll_lb[is.na(ll_lb$num_of_hooks2),])/nrow(ll_lb)
nrow(ll_lb[is.na(ll_lb$num_of_skates1),])/nrow(ll_lb)

nh_na<-which(is.na(ll_lb$num_of_hooks1))
nh_num<-which(!is.na(ll_lb$num_of_hooks1))  

samp<-c(sample(nh_na,10),sample(nh_num,10))

str(for_ll_CPUE)
str(sable_ll_lb)
unique(ll_lb$Disposition)

eg<-ll_lb[samp,]

eg1<-eg %>% mutate(num_of_hooks1_calc = ifelse(!is.na(num_of_hooks1),num_of_hooks1,
                                     as.numeric(num_of_skates1)*as.numeric(hooks_per_skate1)),
                   num_of_skates1_calc = ifelse(!is.na(num_of_skates1),num_of_skates1,
                                                as.numeric(num_of_hooks1)/as.numeric(hooks_per_skate1)),
                   num_of_hooks2_calc = as.numeric(num_of_skates2)*as.numeric(hooks_per_skate2))

as.data.frame(eg1); 
cbind(eg1$num_of_hooks1, eg1$num_of_hooks1_calc)
cbind(cbind(eg1$num_of_skates1,eg1$hooks_per_skate1), cbind(eg1$num_of_hooks1,eg1$num_of_skates1_calc))
cbind(eg1$num_of_hooks2, eg1$num_of_hooks2_calc)

# examine some lines to see if this worked...
nrow(ll_lb)
nrow(unique(ll_lb))
unique(ll_lb$Species)
#***FLAG!!!: When analysing this data there will be a lot of duplicates when 
#* there is a second gear configuration.  These will need to be dealt with at 
#* the analysis phase by getting rid of that column

CFEC.exam<-as.data.frame(with(ll_lb,table(CFEC.Permit.Number,Year)))
view(CFEC.exam %>% filter(Year == 2013))

#STARTER CASE
#lb_samp<-ll_lb %>% filter(Year == 2013 & ADFG.Number == 65119 & Species == "Sablefish" |
#                        Year == 2013 & ADFG.Number == 65119 & Species == "Halibut")

#Trials...
Y <- sample(unique(ll_lb$Year),1)
AN <- sample(unique(ll_lb$ADFG.Number[ll_lb$Year == Y]),10)

lb_samp<-ll_lb %>% filter(Year %in% Y & ADFG.Number %in% AN & Species == "Sablefish" |
                          Year %in% Y & ADFG.Number %in% AN & Species == "Halibut")

#examine when two gear configurations are specified... 
{two_config<-which(!is.na(ll_lb$hook_size2)) 
Y <- sample(unique(ll_lb$Year[two_config]),1)
sub<- slice(ll_lb,two_config) %>% filter(Year == Y)
ans<-unique(sub$ADFG.Number)
if (length(ans < 2)) {ans <- rep(ans,2)} else {}
AN<-sample(ans,1)}

lb_samp<-ll_lb %>% filter(Year == Y & ADFG.Number == AN & Species == "Sablefish" |
                            Year == Y & ADFG.Number == AN & Species == "Halibut")

#SA<-sample(unique(lb_samp$Groundfish.Stat.Area),1)  

#lb_samp<-ll_lb %>% filter(Year == Y & ADFG.Number == AN )
#colnames(lb_samp)
lb_sampx<-lb_samp %>% #filter(Groundfish.Stat.Area == SA) %>%
  #adding in set details to calculate other CPUE
  select(Year, Pounds, Species, Ticket_subref, 
         Groundfish.Stat.Area, Effort.Primary.Target.Species,
         Effort.Number, Trip.Primary.Target.Species, Port, Ticket, CFEC.Permit.Number,
         ADFG.Number,Sell.Date,Trip.Number.LB, gear_target_1, gear_target_2, 
         hook_size1, spacing1, num_of_skates1_calc, num_of_hooks1_calc, hooks_per_skate1, 
         set_length_km, soak_time_hrs,
         hook_size2, spacing2, num_of_skates2, num_of_hooks2_calc, hooks_per_skate2, 
         set.count)
#nrow(unique(lb_sampx))
as.data.frame(lb_sampx)
unique(lb_sampx$Sell.Date)
unique(lb_sampx$Ticket[lb_sampx$Species == "Sablefish"])
unique(lb_sampx$Ticket[lb_sampx$Species == "Halibut"])
#as.data.frame(ll_lb %>% filter(Year == Y, ADFG.Number == AN & Species == "Sablefish"))
#unique(as.data.frame(ll_lb %>% filter(Year == Y, ADFG.Number == AN & Species == "Sablefish"))$Sell.Date)
#colnames(ftx)
#unique(ftx$Fishery.Name)
ftx_samp<-ftx %>% filter(Year %in% Y & ADFG %in% AN & Harvest.Code != 43
                         #& Stat.Area == SA
                         ) %>% 
  mutate(Ticket_subref = str_remove(str_sub(Sequential.Number,nchar(Sequential.Number)-2,nchar(Sequential.Number)), "^0+"),
         Trip.Number.FTX = Trip.Number) %>%
  select(Year, Date.of.Landing, Stat.Area, Sequential.Number, Ticket_subref, CFEC, 
         Year.Office.BN,Whole.Weight..sum.,
         Ticket.Count, Harvest.Code, Harvest.Name,
         Port, ADFG, Fishery.Name, Expanded.Fishery.Code,
         CFEC.1, Office.Code, Batch.Number, Trip.Number.FTX)

(ftx_samp %>% filter(Fishery.Name %in% unique(ftx$Fishery.Name)[2:4]))$Sequential.Number
(ftx_samp %>% filter(Fishery.Name %in% unique(ftx$Fishery.Name)[1]))$Sequential.Number
#ftx_samp<-alltx %>% filter(Year == Y & ADFG == AN & Harvest.Code != 43
                         #& Stat.Area == SA
#) %>% 
#  mutate(#Ticket_subref = str_sub(Sequential.Number,nchar(Sequential.Number)-2,nchar(Sequential.Number)),
#    Ticket_subref = str_remove(str_sub(Sequential.Number,nchar(Sequential.Number)-2,nchar(Sequential.Number)), "^0+"),
#    Trip.Number.FTX = Trip.Number) %>%
#  select(Year, Species.Name, Date.of.Landing, Stat.Area, Sequential.Number, Ticket_subref, CFEC, 
#         Year.Office.BN,Whole.Weight..sum.,
#         Ticket.Count, Harvest.Code, Harvest.Name,
#         Port, ADFG, Fishery.Name, Expanded.Fishery.Code,
#         CFEC.1, Office.Code, Batch.Number, Trip.Number.FTX)

#unique(lb_sampx$Sell.Date); unique(ftx_samp$Date.of.Landing)

#try<-full_join(lb_sampx, ftx_samp, by = c("Year", "ADFG.Number" = "ADFG",
#                                  "Sell.Date" = "Date.of.Landing",
#                                  "Groundfish.Stat.Area" = "Stat.Area",
#                                  "Ticket_subref"))
#as.data.frame(try)
#as.data.frame(unique(try[,!names(try)%in% c("CFEC")]))

{lb_sampx_but<-lb_sampx %>% filter(Species == "Halibut"); as.data.frame(lb_sampx_but)
lb_sampx_sf <- lb_sampx %>% filter(Species == "Sablefish"); as.data.frame(lb_sampx_sf)

ftx_samp_but<-ftx_samp %>% filter(Fishery.Name %in% unique(ftx$Fishery.Name)[2:4])
ftx_samp_sf<-ftx_samp %>% filter(Fishery.Name %in% unique(ftx$Fishery.Name)[1])

#get number of sets per fish Ticket... 
lb_sampx_sf <- lb_sampx_sf %>% group_by(Ticket, Groundfish.Stat.Area) %>%
  mutate(set.count.check = n_distinct(Effort.Number))%>% ungroup()
lb_sampx_but <- lb_sampx_but %>% group_by(Ticket, Groundfish.Stat.Area) %>%
  mutate(set.count.check = n_distinct(Effort.Number))%>% ungroup()}

#try the join
sf_try<-full_join(lb_sampx_sf, ftx_samp_sf, by = c("Year", "ADFG.Number" = "ADFG",
                                     "Sell.Date" = "Date.of.Landing",
                                     "Groundfish.Stat.Area" = "Stat.Area",
                                     #"Species" = "Species.Name",
                                     "Ticket_subref"),multiple="all") %>% filter(!is.na(Fishery.Name)) #%>% #this gets rid of logbook entries with no matching fish tickets
  
as.data.frame(sf_try[order(sf_try$Ticket),]) 
unique(as.data.frame(sf_try[order(sf_try$Ticket),]))
colnames(sf_try)
#ftx with no logbook...
#{nolb<-as.data.frame(sf_try[is.na(sf_try$Species),])
#nrow(nolb)
#ch1<-ll_lb %>% filter(Ticket == sf_try[is.na(sf_try$Species),]$Sequential.Number)
#colnames(ch1)
   #is it the bycatch species? 
#bycatch<-unique(ch1$Species)[unique(ch1$Species)!="Sablefish"]
#missinglb<-ll_lb %>% filter(Sell.Date == nolb$Date.of.Landing,
#                            ADFG.Number == AN)
#as.data.frame(missinglb)

#ll_lb %>% filter(Year == Y, ADFG.Number == AN,
 #                Groundfish.Stat.Area == sf_try[is.na(sf_try$Species),]$Groundfish.Stat.Area)}

#logbooks with no tickets...

#CPUE try... 
#a little tricky because need to add the overages to the allowable catch... 
sf_try %>% group_by(Ticket, Groundfish.Stat.Area, Effort.Number) %>% 
  mutate(partial_soak_time = soak_time_hrs/n(),
         partial_km_fished = set_length_km/n(),
         partial_hook_count1 = as.numeric(num_of_hooks1_calc)/n(),
         partial_hook_count2 = as.numeric(num_of_hooks2_calc)/n()) %>% ungroup %>%
  group_by(Ticket, Groundfish.Stat.Area) %>%
  mutate(set.count = set.count,
            catch = Whole.Weight..sum.,
            pre.lbs_per_set = catch/set.count,
         total_soak_time = sum(partial_soak_time),
         total_km_fished = sum(partial_km_fished),
         total_hook_count1 = sum(partial_hook_count1),
         total_hook_count2 = sum(partial_hook_count2),
         lbs_per_set = sum(unique(catch))/set.count,
         lbs_per_set_per_km = sum(unique(catch))/set.count/total_km_fished,
         lbs_per_set_per_hour = sum(unique(catch))/set.count/total_soak_time,
         lbs_per_set_per_km_per_hour = sum(unique(catch))/set.count/total_km_fished/total_soak_time,
         lbs_per_hook1 = sum(unique(catch))/total_hook_count1,
         lbs_per_hook_per_km1 = sum(unique(catch))/total_hook_count1/total_km_fished,
         lbs_per_hook_per_hour1 = sum(unique(catch))/total_hook_count1/total_soak_time,
         lbs_per_hook_per_km_per_hour1 = sum(unique(catch))/total_hook_count1/total_km_fished/total_soak_time,
         lbs_per_hook2 = sum(unique(catch))/total_hook_count2,
         lbs_per_hook_per_km2 = sum(unique(catch))/total_hook_count2/total_km_fished,
         lbs_per_hook_per_hour2 = sum(unique(catch))/total_hook_count2/total_soak_time,
         lbs_per_hook_per_km_per_hour2 = sum(unique(catch))/total_hook_count2/total_km_fished/total_soak_time
           ) ->sf_cpue2  #don't change! This attaches to joined
#Note regarding two gear configurations.  When calculating final CPUE in analysis
# you will need to be mindful of gear_target_1 and _2.  When there are two sets of
# gear targeting different species (halibut and sablefish) the CPUE will NOT be
# useful for analysis because there is no way to separate out the catch by target 
# if they are both on the same ticket. :(

as.data.frame(unique(sf_cpue2))                
nrow(sf_cpue2)
nrow(unique(sf_cpue2))

length(unique(sf_try$Year))
year_check<-1
length(unique(sf_try$ADFG.Number))
adfg_check<-3

tix_list<-
  unique(sf_try$Ticket[which(sf_try$Year == unique(sf_try$Year)[year_check] & 
                               sf_try$ADFG.Number == unique(sf_try$ADFG.Number)[adfg_check])])
length(tix_list)
tix_list
tix_check<-tix_list[1]

gsa_list<-
  unique(sf_try$Groundfish.Stat.Area[which(sf_try$Year == unique(sf_try$Year)[year_check] & 
                               sf_try$ADFG.Number == unique(sf_try$ADFG.Number)[adfg_check] &
                               sf_try$Ticket == tix_check)])
length(gsa_list)
gsa_check<-gsa_list[1]


#as.data.frame(sf_try %>% filter(Ticket == unique(sf_try$Ticket)[check],
#                                Groundfish.Stat.Area == unique(sf_try$Groundfish.Stat.Area)[check2]))
as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                  Groundfish.Stat.Area == gsa_check,
                                  Year == unique(sf_cpue2$Year)[year_check],
                                  ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))
#THIS IS THE SABLEFISH CPUE FOR THIS TICKET!! 

unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_set)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_set_per_km)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_set_per_hour)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_set_per_km_per_hour)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_hook)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_hook_per_km)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_hook_per_hour)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_hook_per_km_per_hour)

##Halibut CPUE in sablefish sets... 
but_try<-full_join(lb_sampx_but, ftx_samp_but, by = c("Year", "ADFG.Number" = "ADFG",
                                           "Sell.Date" = "Date.of.Landing",
                                           "Groundfish.Stat.Area" = "Stat.Area",
                                           "Ticket_subref")) %>%  filter(!is.na(Fishery.Name))
as.data.frame(but_try[order(but_try$Ticket),]) 
unique(as.data.frame(but_try[order(but_try$Ticket),]))

but_try %>% group_by(Ticket, Groundfish.Stat.Area, Effort.Number) %>% 
  mutate(partial_soak_time = soak_time_hrs/n(),
         partial_km_fished = set_length_km/n(),
         partial_hook_count1 = as.numeric(num_of_hooks1_calc)/n(),
         partial_hook_count2 = as.numeric(num_of_hooks2_calc)/n()) %>% ungroup %>%
  group_by(Ticket, Groundfish.Stat.Area) %>%
  mutate(set.count = set.count,
         catch = Whole.Weight..sum.,
         pre.lbs_per_set = catch/set.count,
         total_soak_time = sum(partial_soak_time),
         total_km_fished = sum(partial_km_fished),
         total_hook_count1 = sum(partial_hook_count1),
         total_hook_count2 = sum(partial_hook_count2),
         lbs_per_set = sum(unique(catch))/set.count,
         lbs_per_set_per_km = sum(unique(catch))/set.count/total_km_fished,
         lbs_per_set_per_hour = sum(unique(catch))/set.count/total_soak_time,
         lbs_per_set_per_km_per_hour = sum(unique(catch))/set.count/total_km_fished/total_soak_time,
         lbs_per_hook1 = sum(unique(catch))/total_hook_count1,
         lbs_per_hook_per_km1 = sum(unique(catch))/total_hook_count1/total_km_fished,
         lbs_per_hook_per_hour1 = sum(unique(catch))/total_hook_count1/total_soak_time,
         lbs_per_hook_per_km_per_hour1 = sum(unique(catch))/total_hook_count1/total_km_fished/total_soak_time,
         lbs_per_hook2 = sum(unique(catch))/total_hook_count2,
         lbs_per_hook_per_km2 = sum(unique(catch))/total_hook_count2/total_km_fished,
         lbs_per_hook_per_hour2 = sum(unique(catch))/total_hook_count2/total_soak_time,
         lbs_per_hook_per_km_per_hour2 = sum(unique(catch))/total_hook_count2/total_km_fished/total_soak_time
  ) ->but_cpue2  #don't change! This attaches to joined
as.data.frame(unique(but_cpue2)) 

length(unique(but_try$Year))
year_check<-1
length(unique(but_try$ADFG.Number))
adfg_check<-1

tix_list<-
  unique(but_try$Ticket[which(but_try$Year == unique(but_try$Year)[year_check] & 
                               but_try$ADFG.Number == unique(but_try$ADFG.Number)[adfg_check])])
length(tix_list)
tix_list
tix_check<-tix_list[1]

gsa_list<-
  unique(but_try$Groundfish.Stat.Area[which(but_try$Year == unique(but_try$Year)[year_check] & 
                                             but_try$ADFG.Number == unique(but_try$ADFG.Number)[adfg_check] &
                                             but_try$Ticket == tix_check)])
length(gsa_list)
gsa_check<-gsa_list[1]


#as.data.frame(but_try %>% filter(Ticket == unique(but_try$Ticket)[check],
#                                Groundfish.Stat.Area == unique(but_try$Groundfish.Stat.Area)[check2]))
as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                  Groundfish.Stat.Area == gsa_check,
                                  Year == unique(sf_cpue2$Year)[year_check],
                                  ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))
#THIS IS THE SABLEFISH CPUE FOR THIS TICKET!! 

unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_set)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_set_per_km)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_set_per_hour)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_set_per_km_per_hour)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_hook)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_hook_per_km)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_hook_per_hour)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_hook_per_km_per_hour)
#Do we need to check if there are sablefish harvests associated with halibut logbooks?

#Shit example 1: so, on Sept 10, 2013 boat 65119 fishing in stat.area 345701 landed sablefish and
# halibut on two tickets ...821 and ...823.  
# Fish tickets allocate sablefish to sequ # 821 and halibut to #823.
# Logbook allocates both species to 3000821 and 3000823.
# Fishticket recognizes correct permits C61A and B06B but logbook only has B06B permits.

#Shit example 2: sometimes there is a fish ticket that is not matched by a logbook:
# see ADFG.No 18402 in 1997 stat.area = 345631
#
#Shit example 3: sometimes there are apparent duplicate tickets that only vary in 
# the amount of fish landed, but harvest.name does not specify an overage (although thats
# what it looks like).  See Sequntial.Number 130451, in year 1998 boat ADFG 10127


#7) Whittle the data down to what's needed for RAW file
colnames(ll_lb)
ll_lb<-ll_lb %>% 
  mutate(Date = as.Date(Time.Hauled),
         Set_Length = Set.Length..mi.,
         Sell_Date = as.Date(Sell.Date),
         Max.Sell.Date = as.Date(Max.Sell.Date),
         DATE_LEFT_PORT = as.Date(Date.Left.Port),
         CFEC_CODE = str_sub(CFEC.Permit.Number,1,4),
         AVERAGE_DEPTH_METERS = 0.54680665*Average.Depth.Fathoms) %>% 
  select(YEAR = Year, PROJECT_CODE = Project.Code, TRIP_NO = Trip.Number, 
         ADFG_NO = ADFG.Number, 
         CFEC.Permit.Number=CFEC.Permit.Number,
         CFEC_CODE,
         TICKET = Ticket,
         SPECIES = Species,
         SPECIES_CODE = Species.Code,
         LONGLINE_SYSTEM = Longline.System, 
         DATE_LEFT_PORT,
         SELL_DATE = Sell_Date, MAX_SELL_DATE = Max.Sell.Date,
         LONGLINE_SYSTEM_CODE = Longline.System.Code,
         SOAK_TIME = Soak.Time.Hours,
         
         CONFIG_TARGET_SP_CODE = gear_target_1,
         HOOK_SIZE = hook_size1, 
         HOOK_SPACING = spacing1, 
         HOOKS_PER_SKATE = hooks_per_skate1, 
         NUMBER_OF_SKATES = num_of_skates1, 
         NUMBER_OF_HOOKS = num_of_hooks1, 
         
         CONFIG_TARGET_SP_CODE_2 = gear_target_2,
         HOOK_SIZE_2 = hook_size2, 
         HOOK_SPACING_2 = spacing2, 
         HOOKS_PER_SKATE_2 = hooks_per_skate2, 
         NUMBER_OF_SKATES_2 = num_of_skates2, 
         #NUMBER_OF_HOOKS_2 = num_of_hooks2,
         
         AVERAGE_DEPTH_METERS, 
         G_MANAGEMENT_AREA_CODE = Groundfish.Management.Area.Code,
         G_STAT_AREA = Groundfish.Stat.Area, 
         TRIP_TARGET = Trip.Primary.Target.Species,
         TRIP_TARGET2 = Trip.Secondary.Target.Species,
         SET_TARGET = Effort.Primary.Target.Species,
         SET_TARGET2 = Effort.Secondary.Target.Species,
         
         TRIP_TARGET_CODE = Trip.Primary.Target.Species.Code,
         TRIP_TARGET2_CODE = Trip.Secondary.Target.Species.Code,
         SET_TARGET_CODE = Effort.Primary.Target.Species.Code,
         SET_TARGET2_CODE = Effort.Secondary.Target.Species.Code,
         
         EFFORT_NO = Effort.Number, 
         
         SET_COMMENT = Effort.Comments, 
         #TRIP_COMMENT = NA, 
         TIME_SET = Time.Set,
         TIME_HAULED = Time.Hauled,
         START_LATITUDE_DECIMAL_DEGREES = Start.Latitude.Decimal.Degrees,
         START_LONGITUDE_DECIMAL_DEGREE = Start.Longitude.Decimal.Degrees) 

nrow(ll_lb)
nrow(unique(ll_lb))
ll_lb<-unique(ll_lb)


#------------------------------------------------------------------------------
# ftx
str(ftx)
unique(ftx$Trip.Number)
unique(ftx$Gear.Code)
unique(ftx$Gear.Name)

nrow(ftx[is.na(ftx$Trip.Number),])/nrow(ftx)
length(unique(ftx$Year.Office.BN))/nrow(ftx)
length(unique(ftx$CFEC))/nrow(ftx)
length(unique(ftx$Ticket.Count))/nrow(ftx)
unique(ftx$Ticket.Count)
length(unique(ftx$CFEC.1))/nrow(ftx)
length(unique(ftx$Batch.Number))/nrow(ftx)
length(unique(ftx$Trip.Number))/nrow(ftx)
length(unique(ftx$Sequential.Number))/nrow(ftx)
length(unique(ftx$Record.ID))/nrow(ftx)

unique(ftx$Year)
unique(ftx$Permit.Year)
unique(ftx$CFEC.Fishery.Code)
unique(ftx$Disposition.Name)
unique(ftx$Delivery.Condition.Name)
unique(ftx$Species.Name)
unique(ftx$Harvest.Name)
unique(ftx$Sold)
unique(ftx$Discard.at.Sea)
unique(ftx$Gear.Name)
unique(ftx$Port)
unique(ftx$Processor.Type.Name)
unique(ftx$Office.Name)

unique(ftx$Region)
unique(ftx$Mgt.Area.District)
unique(ftx$Regulation.Mgt.Area)
unique(ftx$FMP.Area)
unique(ftx$Stat.Area)

min(nchar(ftx$Record.ID))
max(nchar(ftx$Record.ID))

colnames(ftx)

eg<-ftx$Date.of.Landing..MM.DD.

ll_ftx<-ftx %>% 
  filter(Gear.Name == "Longline",
         Harvest.Code != 43) %>%          ##!! FLAG <- get rid of longline survey data!
  mutate(DATE_FISHING_BEGAN = as.Date(Date.Fishing.Began),
         DATE_LANDING = as.Date(Date.of.Landing),
         PERMIT_START_DATE = as.Date(Start.Date),
         PERMIT_END_DATE = as.Date(End.Date),
         TICKET_x = paste0(Office.Code,
                         str_pad(Sequential.Number,
                                 8-nchar(Office.Code),side=c("left"),pad=0))
         #TICKET = 
         ) %>%
  select(YEAR = Year,     #**Join
         DATE_FISHING_BEGAN, DATE_LANDING,
         PERMIT_START_DATE, PERMIT_END_DATE,
         #PROJECT_CODE = NA,  #from lb
         #identifiers:
         TRIP_NO = Trip.Number, #**Join
         ADFG_NO = ADFG,        #**Join
         SEQU_NO = Sequential.Number,
         RECORD_ID = Record.ID,
         CFEC = CFEC,
         CFEC_CODE = CFEC.Fishery.Code,
         OFFICE_CODE = Office.Code,
         OFFICE_NAME = Office.Name,
         TICKET_x,
         
         G_STAT_AREA = Stat.Area,
         DATE_FISHING_BEGAN, DATE_LANDING,
         
         SPECIES = Species.Name,
         SPECIES_CODE = Species.Code,
         HARVEST_NAME = Harvest.Name,
         HARVEST_CODE = Harvest.Code,
         
         DISPOSITION_NAME = Disposition.Name,
         DISPOSITION_CODE = Disposition.Code,
         
         DELIVERY_COND = Delivery.Condition.Name,
         DELIVERY_COND_CODE = Delivery.Condition.Code,
         
         DISCARD_AT_SEA = Discard.at.Sea,
         
         LANDED_WEIGHT_SUM_lbs = Landed.Weight..sum.,
         WHOLE_WEIGHT_SUM_lbs = Whole.Weight..sum.,
         NUMBER_OF_FISH = Number.of.Animals..sum.
         ) 

#fish tickets broken down by Species
sab_ll_ftx<-ll_ftx %>% filter (SPECIES_CODE == 710)
hal_ll_ftx<-ll_ftx %>% filter(SPECIES_CODE == 200)

#logbook broken down by TRIP TARGET
sab_ll_lb<-ll_lb %>% filter(TRIP_TARGET_CODE == 710)
hal_ll_lb<-ll_lb %>% filter(TRIP_TARGET_CODE == 200)
#subset small chunk to play with joins...

sa<-sample(unique(ll_lb$G_STAT_AREA),1)

{Y<-sample(unique(ll_lb$YEAR),1) #2005
an<-sample(unique(ll_lb$ADFG_NO[!is.na(ll_lb$ADFG_NO) & ll_lb$YEAR == Y]),1)

tix_eg<-ll_ftx %>% filter (YEAR == Y & ADFG_NO == an); nrow(tix_eg)
s_tix_eg<-sab_ll_ftx %>% filter (YEAR == Y & ADFG_NO == an); nrow(s_tix_eg)
h_tix_eg<-hal_ll_ftx %>% filter (YEAR == Y & ADFG_NO == an); nrow(h_tix_eg)

lb_eg<-ll_lb %>% filter (YEAR == Y & ADFG_NO == an); nrow(lb_eg)
s_lb_eg<-sab_ll_lb %>% filter (YEAR == Y & ADFG_NO == an); nrow(s_lb_eg)
h_lb_eg<-hal_ll_lb %>% filter (YEAR == Y & ADFG_NO == an); nrow(h_lb_eg)}

head(tix_eg)
#head(lb_eg)
unique(lb_eg$TRIP_NO) #does the number of unique trips match the number of unique
                      #landing dates
length(unique(lb_eg$TRIP_NO)); length(unique(tix_eg$DATE_LANDING))
length(unique(s_tix_eg$DATE_LANDING)); length(unique(h_tix_eg$DATE_LANDING))
length(unique(s_lb_eg$TRIP_NO));length(unique(h_lb_eg$TRIP_NO))

sort(unique(lb_eg$SELL_DATE)); sort(unique(tix_eg$DATE_LANDING))   #should match DATE_LANDING
sort(unique(s_tix_eg$DATE_LANDING));sort(unique(h_tix_eg$DATE_LANDING))
sort(unique(s_lb_eg$SELL_DATE));sort(unique(h_lb_eg$SELL_DATE))

sort(unique(lb_eg$G_STAT_AREA)); sort(unique(tix_eg$G_STAT_AREA)) #do they match?
sort(unique(s_tix_eg$G_STAT_AREA)); sort(unique(h_tix_eg$G_STAT_AREA))
sort(unique(s_lb_eg$G_STAT_AREA)); sort(unique(h_lb_eg$G_STAT_AREA))

unique(lb_eg$TRIP_TARGET)
unique(lb_eg$TRIP_TARGET2)
unique(lb_eg$SET_TARGET)
unique(lb_eg$SET_TARGET2)
unique(lb_eg$CONFIG_TARGET_SP_CODE)
unique(lb_eg$CONFIG_TARGET_SP_CODE_2)

tix_eg$SPECIES_CODE
lb_eg$TRIP_TARGET_CODE
lb_eg$TRIP_TARGET2_CODE
lb_eg$SET_TARGET_CODE
lb_eg$SET_TARGET2_CODE
lb_eg$CONFIG_TARGET_SP_CODE
lb_eg$CONFIG_TARGET_SP_CODE_2

#OK... now to see how many sets per trip... 
sort(unique(lb_eg$EFFORT_NO))

lb_eg[lb_eg$EFFORT_NO]

lb_eg$trip_effort<-paste0(lb_eg$TRIP_NO,"-",lb_eg$EFFORT_NO)
unique(lb_eg[lb_eg$TRIP_NO == 4072,])

unique(lb_eg)

#-------------------------------------------------------------------------------
# JOIN PLAN and NOTES...
## I think we can leave ftx with sab and hal combined for now and then calculate
## species specific stuff once they are joined... need to be careful about labels
## and not getting things confused... 

## OK, need to fish ticket species code = 710? do not want or need fish tickets from 
## halibut landings... OR maybe not? 

# Step 1) Split ftx up by SPECIES 
# Step 2) rename split ftx columns to be species specific
# Step 3) join both ftx into lb data frame... then you will have the amount landed
#         for each species in each trip
# Step 4) At that point it will be time to look at TARGET and relative numbers
#         caught by species and make call on which to include in calculating
#         CPUE statistics

# So the Join outline:
# 1) lb$SELL_DATE = ftx$DATE_LANDING
# 2) lb$G_STAT_AREA = ftx$G_STAT_AREA
# 3) YEAR = YEAR
# 4) ADFG_NO
#fish tickets broken down by Species
ll_lb<-ll_lb %>% mutate(JOIN_DATE = SELL_DATE,
                        JOIN_TIX = as.integer(TICKET))
colnames(ll_lb)
colnames(ll_ftx)

ll_ftx<-ll_ftx %>% mutate(JOIN_TIX = as.integer(SEQU_NO),
                          JOIN_DATE = DATE_LANDING)

sab_ll_ftx<-ll_ftx %>% filter (SPECIES_CODE == 710) 

hal_ll_ftx<-ll_ftx %>% filter (SPECIES_CODE == 200) 
#hal_labs<-paste0("hal_",colnames(hal_ll_ftx)[-c(1,7,12,25)], sep = "")
#colnames(hal_ll_ftx)<-c(colnames(hal_ll_ftx)[c(1,7,12,25)],hal_labs)

#Try joining a small piece... 
unique(ll_lb$SPECIES)

Y<-sample(unique(ll_lb$YEAR),1)  
an<-sample(unique(ll_lb$ADFG_NO[!is.na(ll_lb$ADFG_NO) & ll_lb$YEAR == Y]),1)

#Problem 1: multiple fish tickets on same trip... some are bycatch species 
# so you will see logbook entries with no fish tickets because fish tickets
# have been filtered for just halibut and sablefish.  In this example you will 
# see a logbook entry for fish ticket 152806 that has permit number M06B31083X.
# That permit is for the rockfish bycatch... 
Y<-1998
an<-53570

tix_eg<-ll_ftx %>% filter (YEAR == Y & ADFG_NO == an); nrow(tix_eg)
s_tix_eg<-sab_ll_ftx %>% filter (YEAR == Y & ADFG_NO == an); nrow(s_tix_eg)
h_tix_eg<-hal_ll_ftx %>% filter (YEAR == Y & ADFG_NO == an); nrow(h_tix_eg)

lb_eg<-ll_lb %>% filter (YEAR == Y & ADFG_NO == an & 
                           SPECIES_CODE == 710 | 
                           YEAR == Y & ADFG_NO == an & SPECIES_CODE ==200); nrow(lb_eg)
tix_eg; as.data.frame(lb_eg)
view(lb_eg)

lb_eg2<-ll_lb %>% filter(YEAR == Y & ADFG_NO == an)
view(lb_eg2)
#lb_eg<-ll_lb %>% filter (YEAR == Y & ADFG_NO == an); nrow(lb_eg)

TEST2<-full_join(lb_eg,tix_eg, by=c("JOIN_TIX",
                                      "JOIN_DATE",
                                      #"CFEC.Permit.Number"="CFEC",
                                      "G_STAT_AREA"),
                 suffix = c("_logbook",""))
nrow(TEST2[is.na(TEST2$WHOLE_WEIGHT_SUM_lbs),]); nrow(TEST2[is.na(TEST2$TICKET_logbook),])
view(TEST2)
colnames(TEST2)
unique(TEST2$TICKET)
unique(TEST2$SEQU_NO)
unique(TEST2$TRIP_NO_logbook)
unique(TEST2$CFEC.Permit.Number)
unique(TEST2$SPECIES_logbook)
unique(TEST2$TRIP_TARGET)
unique(TEST2$SET_TARGET)
unique(TEST2$EFFORT_NO)
unique(TEST2$CFEC_CODE)
unique(TEST2$CFEC)
unique(TEST2$TICKET_x)
unique(TEST2$SPECIES)
unique(TEST2$WHOLE_WEIGHT_SUM_lbs)

view(unique(TEST2 %>% filter(TICKET == 3000821 | is.na(TICKET))))
as.data.frame(unique(TEST2 %>% filter(TICKET == 3000821 | is.na(TICKET))))
as.data.frame(TEST2)
as.data.frame(TEST2[is.na(TEST2$WHOLE_WEIGHT_SUM_lbs),])
as.data.frame(TEST2 %>% filter(CFEC.Permit.Number == "M06B31083X"))

test.tix<-unique(TEST2 %>% filter(TICKET == 153046))
unique(test.tix$TICKET)
unique(test.tix$TRIP_NO_logbook)
unique(test.tix$CFEC.Permit.Number)
unique(test.tix$SPECIES_logbook)
unique(test.tix$TRIP_TARGET)
unique(test.tix$SET_TARGET)
unique(test.tix$EFFORT_NO)
unique(test.tix$CFEC_CODE)
unique(test.tix$CFEC)
unique(test.tix$TICKET_x)
unique(test.tix$SPECIES)
unique(test.tix$WHOLE_WEIGHT_SUM_lbs)

s.test.tix<-test.tix %>% filter(SPECIES_logbook == "Sablefish")
view(s.test.tix)
as.data.frame(s.test.tix)
nrow(unique(s.test.tix))
unique(s.test.tix$G_STAT_AREA)
unique(s.test.tix$G_STAT_AREA) ##This should get you to sablefish landings and effort

as.data.frame(TEST2 %>% filter(TICKET == 153046)) 

#need to do the same thing for halibut... 


###*** 1-27-23 FRIDAY LEAVE OFF; EXAMINE THOSE TICKETS THAT DON'T MERGE DATA
###*   CHECK ABOUT SPECIES IN LOGBOOK DEFINITION... I THINK WE SHOULD FILTER THAT
###*   FOR THE JUST HALIBUT AND SABLEFISH? 

BIGTEST<-full_join(TEST2,h_tix_eg, by=c("JOIN_TIX","JOIN_DATE"),
                 suffix = c("_sabtx","_haltx"))

QL<-TEST2 %>% select(YEAR,TRIP_NO_logbook,ADFG_NO_logbook,CFEC.Permit.Number,
                     TICKET_logbook, SELL_DATE, MAX_SELL_DATE, G_STAT_AREA_logbook,
                     TRIP_TARGET,TRIP_TARGET2, SET_TARGET, SET_TARGET2, EFFORT_NO,
                     JOIN_DATE_logbook, JOIN_TIX, DATE_LANDING_sabtx,DATE_LANDING_haltx,  
                     ADFG_NO_sabtx, ADFG_NO_haltx, SEQU_NO_sabtx,SEQU_NO_haltx,
                     RECORD_ID_sabtx, RECORD_ID_haltx,CFEC_CODE_sabtx, CFEC_CODE_haltx,
                     TICKET_sabtx,TICKET_haltx, G_STAT_AREA_sabtx, SPECIES_sabtx,SPECIES_haltx )
as.data.frame(QL)
as.data.frame(TEST2[is.na(TEST2$WHOLE_WEIGHT_SUM_lbs),])
colnames(TEST2)
nrow(TEST2[is.na(TEST2$WHOLE_WEIGHT_SUM_lbs_sabtx),])
nrow(TEST2[is.na(TEST2$WHOLE_WEIGHT_SUM_lbs_haltx),])
nrow(TEST2[is.na(TEST2$WHOLE_WEIGHT_SUM_lbs_haltx) & is.na(TEST2$WHOLE_WEIGHT_SUM_lbs_sabtx),])

as.data.frame(TEST2[is.na(TEST2$WHOLE_WEIGHT_SUM_lbs_haltx) & is.na(TEST2$WHOLE_WEIGHT_SUM_lbs_sabtx),])
as.data.frame(TEST2[is.na(TEST2$WHOLE_WEIGHT_SUM_lbs_haltx) & 
                      is.na(TEST2$WHOLE_WEIGHT_SUM_lbs_sabtx),])$CFEC.Permit.Number


###BREAK ####

TEST<-full_join(lb_eg,s_tix_eg, by=c("YEAR","JOIN_DATE","G_STAT_AREA","ADFG_NO","JOIN_TIX"),
                suffix = c("_logbook",""))
nrow(TEST[is.na(TEST$WHOLE_WEIGHT_SUM_lbs),]); nrow(TEST[is.na(TEST$TICKET_logbook),])
as.data.frame(TEST)

TEST1<-full_join(lb_eg,s_tix_eg, by=c("JOIN_DATE","G_STAT_AREA"),
                suffix = c("_logbook",""))
nrow(TEST1[is.na(TEST1$WHOLE_WEIGHT_SUM_lbs),]); nrow(TEST1[is.na(TEST1$TICKET_logbook),])
as.data.frame(TEST1)

TEST2<-full_join(lb_eg,s_tix_eg, by=c("JOIN_TIX"),
                 suffix = c("_logbook",""))
nrow(TEST2[is.na(TEST2$WHOLE_WEIGHT_SUM_lbs),]); nrow(TEST2[is.na(TEST2$TICKET_logbook),])
as.data.frame(TEST2)
as.data.frame(TEST2[is.na(TEST2$WHOLE_WEIGHT_SUM_lbs),])

TEST2<-full_join(TEST2,h_tix_eg, by=c("JOIN_TIX"),
                suffix = c("_sabtx","_haltx"))
view(TEST)
nrow(TEST[is.na(TEST$WHOLE_WEIGHT_SUM_lbs),])
colnames(TEST)
unique(TEST$JOIN_TIX); unique(TEST$TICKET_logbook); unique(TEST$JOIN_TIX_logbook)

TEST$WHOLE_WEIGHT_SUM_lbs
colnames(TEST)
head(as.data.frame(lb_eg),20)
head(s_tix_eg)

unique(lb_eg$JOIN_TIX)
unique(s_tix_eg$JOIN_TIX)
setdiff(lb_eg$JOIN_TIX,s_tix_eg$JOIN_TIX)
prob<-as.data.frame(TEST[TEST$JOIN_TIX == setdiff(lb_eg$JOIN_TIX,s_tix_eg$JOIN_TIX),])
s_tix_eg[s_tix_eg$JOIN_DATE == prob$JOIN_DATE,]


unique(TEST$SELL_DATE)
unique(TEST$JOIN_DATE)
unique(TEST$DATE_LANDING_sabtx)
unique(TEST$DATE_LANDING_haltx)

nrow(TEST)
nrow(unique(TEST))

s_tix_eg[s_tix_eg$JOIN_DATE == "2000-09-10",]
TEST[TEST$JOIN_DATE == "2000-09-10",]

#! 2008, ADFG_NO 50721 missing logbook data for sablefish landing...?
lbx<-ll_lb %>% filter(YEAR == 2008 & ADFG_NO == 50721)
txx<-sab_ll_ftx %>% filter (YEAR == 2008 & ADFG_NO == 50721)

unique(lbx$JOIN_DATE); unique(lbx$SELL_DATE); unique(lbx$MAX_SELL_DATE);unique(txx$DATE_LANDING)
#---------------------------------------------------------------------------------
# Next step is to calculate CPUE as lbs/set!!!!
# 1) Group by JOIN_DATE and G_STAT_AREA and ADFG_NO and TARGET
#    i.e., for each landing, stat area and boat we want lbs/set
#    *** need to think about how to figure the target part?
#        maybe just calculate CPUE for all and then consider on the back end... 
# 2) Get number of sets for each date, area and boat
# 3) Calculate CPUE for both species = 
#    a) sablefish CPUE = WHOLE_WEIGHT_SUM_lbs_sabtx/no.sets
#    b) halibut CPUE = WHOLE_WEIGHT_SUM_lbs_haltx/no.sets
#    c)!!! for trips that landed both halibut and sablefish calculate the ratio
#         of the two species in the landings
# 4) Examine CPUE for different SET_TARGET and TRIP_TARGET
# 5) Examine ratios and see if there are trips/sets that landed more of species
#    NOT set as target.
# 6) MAke final call on what data to be included in CPUE calculations... 

#################################################################################
#--------------------------------------------------------------------------------  
  select(Date, Year, Trip.Number, Effort_no = Effort.Number, Ticket, CFEC.Permit.Number, ADFG.Number,
         Set_Length, Depth = Average.Depth.Fathoms, Soak = Soak.Time.Hours,
         Species, Species.Code, Groundfish.Stat.Area, Groundfish.Management.Area.Code, 
         Disposition, Pounds, Numbers, 
         Effort.Primary.Target.Species, Effort.Secondary.Target.Species, 
         Sablefish.Discards, 
         Depredation, Number.of.Skates.Impacted.by.Depredation, 
         Sell_Date,
         Port, Vessel.Name, 
         Longline.System.Code, Longline.System, 
         Gear.Code, Gear.Class.Code, Gear, 
         Project, Project.Code, 
         Start.Longitude.Decimal.Degrees, Start.Latitude.Decimal.Degrees,
         End.Longitude.Decimal.Degrees, End.Latitude.Decimal.Degrees )

str(ll_lb)

eg<-ll_lb$Time.Set[1:4]

as.

################################################################################
## Appendix: Long winded loop to separate out gear configuration... 
## Replaced with nicer code above, but saved here for references...
################################################################################
ll_lb<-ll_lb %>% 
  mutate(CONFIG_TARGET_SP_CODE=NA, HOOK_SIZE=NA, HOOK_SPACING=NA, 
         HOOKS_PER_SKATE = NA, NUMBER_OF_SKATES = NA, NUMBER_OF_HOOKS = NA,
         CONFIG_TARGET_SP_CODE_2=NA, HOOK_SIZE_2=NA, HOOK_SPACING_2=NA, 
         HOOKS_PER_SKATE_2 = NA, NUMBER_OF_SKATES_2 = NA, NUMBER_OF_HOOKS_2 = NA)

# 1) Split up ll_lb$Config.List into Target Species, Hook Size,Spacing, Hooks/Skate, Num Skates, Spacing
# The column Config.List has all the information on hook size, spacing, # of skates
# etc.  All in one clunky, inconsistent column.  A better coder could likey use 
# dplyr::pivot_whatever to get this split out.  But not being a better coder
# I would up witing a loop that takes the data frame apart line by line to separate
# out all the data in that column.  It takes a couple of minutes to run (embarrassing) 
# but that's what I have for now.  The loop is at least annotated and recorded now... 

#Prep ll_lb to rip apart the Config.List column
{n_col<-max(stringr::str_count(ll_lb$Config.List, ","))+1      #Config.List replaced Shit from development code
  n_precol<-max(stringr::str_count(ll_lb$Config.List, "---"))+1
  
  ll_lb %>% separate(Config.List, into = paste0("precol",1:n_precol),sep = "---") -> ll_lb #prestep
  n_col1<-max(stringr::str_count(ll_lb$precol1, ":"))+1
  n_col2<-max(stringr::str_count(ll_lb$precol2[!is.na(ll_lb$precol2)], ":"))+1
  
  ll_lb %>% mutate(junk1 = gsub(")","",sub(".","",precol1))) %>%   #junk1 and junk2
    mutate(junk2 = gsub(")","",sub(".","",precol2))) %>%
    mutate(junk2 = sub(".","",junk2)) %>%
    select(-precol1, -precol2) -> ll_lb #prestep
  
  junk1cols<-max(stringr::str_count(ll_lb$junk1, ":"), na.rm=T)+1
  junk2cols<-max(stringr::str_count(ll_lb$junk2, ":"), na.rm=T)+1
  
  ll_lb %>%        
    separate(junk1, into = paste0("col", 1:(junk1cols)),sep = ",") %>%
    separate(junk2, into = paste0("col", (1+junk1cols):(1+junk1cols+junk2cols)),sep = ",") -> ll_lb}

# This loop will pull apart the Config.List column and break it up into the different
# pieces of longline gear info... 
for (i in 1:nrow(ll_lb)) {  #i<-21
  #1st Config list
  vals<-vector()
  #need to identify when it switches to second set of set configuration...???
  #da<-Step0[i,]
  da<-select(ll_lb[i,],contains("col"))
  cats<-sub("(.*?)[\\.|:].*", "\\1", da)
  targ_refs<-which(cats=="Target Species")  #column numbers 
  
  if (length(targ_refs) > 1) {
    second.config <- targ_refs[2]
  } else {
    second.config <- length(cats[!is.na(cats)])+1
  }
  
  #1st Gear Configuration Breakdown
  for (j in 1:(second.config-1)){   #j<-3
    val<-ll_lb[i,which(colnames(ll_lb)==paste0("col",j))]
    vals[j]<-val
    #val reference if it's just a number so we know where to add it... 
    if (!is.na(as.numeric(val))) {
      if (is.na(as.numeric(vals[j-1]))) {
        ref<-sub("(.*?)[\\.|:].*", "\\1", vals[j-1])
      } else {ref<-sub("(.*?)[\\.|:].*", "\\1", vals[j-2])}
      
    } else {ref<-NA}
    
    #Target
    if (grepl("Target Species",val) & !is.na(val)) {
      ll_lb[i,"CONFIG_TARGET_SP_CODE"] = as.numeric(gsub("[^0-9]","",val))
    } else {}
    
    #Hook Size 
    if (grepl("Hook Size",val) & !is.na(val)) {
      ll_lb[i,"HOOK_SIZE"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Hook Size",ref)) {
        ll_lb[i,"HOOK_SIZE"] = paste0(ll_lb[i,"HOOK_SIZE"],",",val)
      } else {}
    }
    
    # Spacing
    if (grepl("Spacing",val) & !is.na(val)) {
      ll_lb[i,"HOOK_SPACING"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Spacing",ref)) {
        ll_lb[i,"HOOK_SPACING"] = paste0(ll_lb[i,"HOOK_SPACING"],",",val)
      } else {}
    }  
    
    #Hooks per skate
    if (grepl("Hooks/Skate",val) & !is.na(val)) {
      ll_lb[i,"HOOKS_PER_SKATE"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Hooks/Skate",ref)) {
        ll_lb[i,"HOOKS_PER_SKATE"] = paste0(ll_lb[i,"HOOKS_PER_SKATE"],",",val)
      } else {}
    } 
    
    #number of hooks
    if (grepl("Num Hooks",val) & !is.na(val)) {
      ll_lb[i,"NUMBER_OF_HOOKS"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Num Skates",ref)) {
        ll_lb[i,"NUMBER_OF_HOOKS"] = paste0(ll_lb[i,"NUMBER_OF_HOOKS"],",",val)
      } else {}
    }  
    
    #number of skates
    if (grepl("Num Skates",val) & !is.na(val)) {
      ll_lb[i,"NUMBER_OF_SKATES"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Num Skates",ref)) {
        ll_lb[i,"NUMBER_OF_SKATES"] = paste0(ll_lb[i,"NUMBER_OF_SKATES"],",",val)
      } else {}
    }   
  }
  
  #2nd Gear Configuration list
  if (length(targ_refs) > 1) {
    for (j in second.config:(length(cats))){   #j<-1
      val<-ll_lb[i,which(colnames(ll_lb)==paste0("col",j))]
      vals[j]<-val
      
      if (!is.na(as.numeric(val))) {
        if (is.na(as.numeric(vals[j-1]))) {
          ref<-sub("(.*?)[\\.|:].*", "\\1", vals[j-1])
        } else {ref<-sub("(.*?)[\\.|:].*", "\\1", vals[j-2])}
        
      } else {ref<-NA}
      
      #Target
      if (grepl("Target Species",val) & !is.na(val)) {
        ll_lb[i,"CONFIG_TARGET_SP_CODE_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {}
      #Hook Size 
      if (grepl("Hook Size",val) & !is.na(val)) {
        ll_lb[i,"HOOK_SIZE_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Hook Size",ref)) {
          ll_lb[i,"HOOK_SIZE_2"] = paste0(ll_lb[i,"HOOK_SIZE_2"],",",val)
        } else {}
      }
      
      # Spacing
      if (grepl("Spacing",val) & !is.na(val)) {
        ll_lb[i,"HOOK_SPACING_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Spacing",ref)) {
          ll_lb[i,"HOOK_SPACING_2"] = paste0(ll_lb[i,"HOOK_SPACING_2"],",",val)
        } else {}
      }  
      
      #Hooks per skate
      if (grepl("Hooks/Skate",val) & !is.na(val)) {
        ll_lb[i,"HOOKS_PER_SKATE_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Hooks/Skate",ref)) {
          ll_lb[i,"HOOKS_PER_SKATE_2"] = paste0(ll_lb[i,"HOOKS_PER_SKATE_2"],",",val)
        } else {}
      } 
      
      #number of hooks
      if (grepl("Num Hooks",val) & !is.na(val)) {
        ll_lb[i,"NUMBER_OF_HOOKS2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Num Skates",ref)) {
          ll_lb[i,"NUMBER_OF_HOOKS2"] = paste0(ll_lb[i,"NUMBER_OF_HOOKS2"],",",val)
        } else {}
      }  
      
      #number of skates
      if (grepl("Num Skates",val) & !is.na(val)) {
        ll_lb[i,"NUMBER_OF_SKATES_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Num Skates",ref)) {
          ll_lb[i,"NUMBER_OF_SKATES_2"] = paste0(ll_lb[i,"NUMBER_OF_SKATES_2"],",",val)
        } else {}
      }   
    }
  } else {}
  
}

##################################################################################
## SCRAP SCRAP
##################################################################################
# Code development for splitting apart gear configuration column

# 1) Split up ll_lb$Config.List into Target Species, Hook Size,Spacing, Hooks/Skate, Num Skates, Spacing
grab6<-round(runif(6,1,nrow(ll_lb)),0)
sam<-as.data.frame(cbind(ll_lb$Year[c(grab6)],   #sam will get replaced by ll_lb
                         ll_lb$Config.List[c(grab6)]))
colnames(sam)<-c("Year","Shit")

ridiculous8<-ll_lb[stringr::str_count(ll_lb$Config.List, ",") == 8,]
#ridiculous8$Config.List
grab3<-round(runif(3,1,nrow(ridiculous8)),0)
ridiculous8<-as.data.frame(cbind(ridiculous8$Year[c(grab3)],
                                 ridiculous8$Config.List[c(grab3)]))
colnames(ridiculous8)<-c("Year","Shit")

ridiculous9<-ll_lb[stringr::str_count(ll_lb$Config.List, ",") == 9,]
ridiculous9$Config.List
grab3<-round(runif(3,1,nrow(ridiculous9)),0)
ridiculous9<-as.data.frame(cbind(ridiculous9$Year[c(grab3)],ridiculous9$Config.List[c(grab3)]))
colnames(ridiculous9)<-c("Year","Shit")

ridiculous12<-ll_lb[stringr::str_count(ll_lb$Config.List, ",") == 12,]
ridiculous12$Config.List
ridiculous12<-as.data.frame(cbind(ridiculous12$Year[c(1:2)],ridiculous12$Config.List[c(1:2)]))
colnames(ridiculous12)<-c("Year","Shit")

sam<-rbind(sam, ridiculous8,ridiculous9,ridiculous12)

#for trouble shooting...
ll_lb<-read.csv(paste0(YEAR+1,"/data/fishery/raw_data/Longline logbook sable and halibut target 1997-now for CPUE.csv"))
{sam<-ll_lb[24189,c("Year","Config.List")]
colnames(sam)<-c("Year","junk")
#Argh... need to divid up around "---" and deal with two configuration lists... 

n_col<-max(stringr::str_count(sam$junk, ","))+1
n_precol<-max(stringr::str_count(sam$junk, "---"))+1

sam %>% separate(junk, into = paste0("precol",1:n_precol),sep = "---") -> prestep
n_col1<-max(stringr::str_count(prestep$precol1, ":"))+1
n_col2<-max(stringr::str_count(prestep$precol2[!is.na(prestep$precol2)], ":"))+1

prestep %>% mutate(junk1 = gsub(")","",sub(".","",precol1))) %>%
  mutate(junk2 = gsub(")","",sub(".","",precol2))) %>%
  mutate(junk2 = sub(".","",junk2)) %>%
  select(Year, junk1, junk2) -> prestep

junk1cols<-max(stringr::str_count(prestep$junk1, ":"), na.rm=T)+1
junk2cols<-max(stringr::str_count(prestep$junk2, ":"), na.rm=T)+1

prestep %>%        
  separate(junk1, into = paste0("col", 1:(junk1cols)),sep = ",") %>%
  separate(junk2, into = paste0("col", (1+junk1cols):(1+junk1cols+junk2cols)),sep = ",") -> Step0
#1-7 = first config
#8-14 = second config
#  mutate(across(everything(),~dplyr::na_if(.,""))) -> Step0 #%>%

Step0<-Step0 %>% 
  mutate(CONFIG_TARGET_SP_CODE=NA, HOOK_SIZE=NA, HOOK_SPACING=NA, 
         HOOKS_PER_SKATE = NA, NUMBER_OF_SKATES = NA, NUMBER_OF_HOOKS = NA,
         CONFIG_TARGET_SP_CODE_2=NA, HOOK_SIZE_2=NA, HOOK_SPACING_2=NA, 
         HOOKS_PER_SKATE_2 = NA, NUMBER_OF_SKATES_2 = NA, NUMBER_OF_HOOKS_2 = NA)
}

for (i in 1:nrow(Step0)) {  #i<-21
  #1st Config list
  vals<-vector()
  #need to identify when it switches to second set of set configuration...???
  #da<-Step0[i,]
  da<-select(Step0[i,],contains("col"))
  cats<-sub("(.*?)[\\.|:].*", "\\1", da)
  targ_refs<-which(cats=="Target Species")  #column numbers 
  
  if (length(targ_refs) > 1) {
    second.config <- targ_refs[2]
  } else {
    second.config <- length(cats[!is.na(cats)])+1
  }
  
  #1st Gear Configuration Breakdown
  for (j in 1:(second.config-1)){   #j<-3
    val<-Step0[i,which(colnames(Step0)==paste0("col",j))]
    vals[j]<-val
    #val reference if it's just a number so we know where to add it... 
    if (!is.na(as.numeric(val))) {
      if (is.na(as.numeric(vals[j-1]))) {
        ref<-sub("(.*?)[\\.|:].*", "\\1", vals[j-1])
      } else {ref<-sub("(.*?)[\\.|:].*", "\\1", vals[j-2])}
      
    } else {ref<-NA}
    
    #Target
    if (grepl("Target Species",val) & !is.na(val)) {
      Step0[i,"CONFIG_TARGET_SP_CODE"] = as.numeric(gsub("[^0-9]","",val))
    } else {}
    
    #Hook Size 
    if (grepl("Hook Size",val) & !is.na(val)) {
      Step0[i,"HOOK_SIZE"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Hook Size",ref)) {
        Step0[i,"HOOK_SIZE"] = paste0(Step0[i,"HOOK_SIZE"],",",val)
      } else {}
    }
    
    # Spacing
    if (grepl("Spacing",val) & !is.na(val)) {
      Step0[i,"HOOK_SPACING"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Spacing",ref)) {
        Step0[i,"HOOK_SPACING"] = paste0(Step0[i,"HOOK_SPACING"],",",val)
      } else {}
    }  
    
    #Hooks per skate
    if (grepl("Hooks/Skate",val) & !is.na(val)) {
      Step0[i,"HOOKS_PER_SKATE"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Hooks/Skate",ref)) {
        Step0[i,"HOOKS_PER_SKATE"] = paste0(Step0[i,"HOOKS_PER_SKATE"],",",val)
      } else {}
    } 
    
    #number of hooks
    if (grepl("Num Hooks",val) & !is.na(val)) {
      Step0[i,"NUMBER_OF_HOOKS"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Num Skates",ref)) {
        Step0[i,"NUMBER_OF_HOOKS"] = paste0(Step0[i,"NUMBER_OF_HOOKS"],",",val)
      } else {}
    }  
    
    #number of skates
    if (grepl("Num Skates",val) & !is.na(val)) {
      Step0[i,"NUMBER_OF_SKATES"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Num Skates",ref)) {
        Step0[i,"NUMBER_OF_SKATES"] = paste0(Step0[i,"NUMBER_OF_SKATES"],",",val)
      } else {}
    }   
  }
  
  #2nd Gear Configuration list
  if (length(targ_refs) > 1) {
    
    for (j in second.config:(length(cats))){   #j<-second.config+2
      
      val<-Step0[i,which(colnames(Step0)==paste0("col",j))]
      vals[j]<-val
      
      if (!is.na(as.numeric(val))) {
        if (is.na(as.numeric(vals[j-1]))) {
          ref<-sub("(.*?)[\\.|:].*", "\\1", vals[j-1])
        } else {ref<-sub("(.*?)[\\.|:].*", "\\1", vals[j-2])}
        
      } else {ref<-NA}
      #Target
      if (grepl("Target Species",val) & !is.na(val)) {
        Step0[i,"CONFIG_TARGET_SP_CODE_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {}
      #Hook Size 
      if (grepl("Hook Size",val) & !is.na(val)) {
        Step0[i,"HOOK_SIZE_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Hook Size",ref)) {
          Step0[i,"HOOK_SIZE_2"] = paste0(Step0[i,"HOOK_SIZE_2"],",",val)
        } else {}
      }
      
      # Spacing
      if (grepl("Spacing",val) & !is.na(val)) {
        Step0[i,"HOOK_SPACING_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Spacing",ref)) {
          Step0[i,"HOOK_SPACING_2"] = paste0(Step0[i,"HOOK_SPACING_2"],",",val)
        } else {}
      }  
      
      #Hooks per skate
      if (grepl("Hooks/Skate",val) & !is.na(val)) {
        Step0[i,"HOOKS_PER_SKATE_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Hooks/Skate",ref)) {
          Step0[i,"HOOKS_PER_SKATE_2"] = paste0(Step0[i,"HOOKS_PER_SKATE_2"],",",val)
        } else {}
      } 
      
      #number of hooks
      if (grepl("Num Hooks",val) & !is.na(val)) {
        Step0[i,"NUMBER_OF_HOOKS2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Num Skates",ref)) {
          Step0[i,"NUMBER_OF_HOOKS2"] = paste0(Step0[i,"NUMBER_OF_HOOKS2"],",",val)
        } else {}
      }  
      
      #number of skates
      if (grepl("Num Skates",val) & !is.na(val)) {
        Step0[i,"NUMBER_OF_SKATES_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Num Skates",ref)) {
          Step0[i,"NUMBER_OF_SKATES_2"] = paste0(Step0[i,"NUMBER_OF_SKATES_2"],",",val)
        } else {}
      }   
    }
  } else {}
  
}


#--------------------------------------------------------------------------------
grepl("Hook Size",Step0[i,which(colnames(Step0)==paste0("col",j))])


Step0 %>% filter(sub("(.*?)[\\.|:].*", "\\1", col1) == "Hook Size") %>%
  mutate(HOOK_SIZE = gsub("[^0-9]","",col1)) %>% unfilter()

Step0 %>% mutate(col1_txt = sub("(.*?)[\\.|:].*", "\\1", col1),
                 col1_no = gsub("[^0-9]","",col1)) -> Step0.1

Step0.1 %>% pivot_wider(names_from=col1_txt,values_from=col1_no)

#----
varNames <- c("var1", "var2", "var3", "var4", "var5")
df <- data.frame(VariableID = rep(varNames, 5),
                 Serial = rep(1:5, 5),
                 Response = runif(25, 1, 10))

df
newdf <- df %>% 
  arrange(Serial) %>%
  group_by(Serial) %>% mutate(id=row_number()) %>% 
  pivot_wider(names_from=VariableID,values_from=Response) %>% select(-id)
#-----

Step0 %>% mutate(LL_gear = as.factor(sub("(.*?)[\\.|:].*", "\\1", value)))

Step0 %>% pivot_longer(-Year) %>%
  mutate(LL_gear = as.factor(sub("(.*?)[\\.|:].*", "\\1", value))) %>%
  arrange(LL_gear) %>%
  group_by(LL_gear) %>% mutate(id=row_number()) %>%
  pivot_wider(names_from=LL_gear, values_from=value) -> Step0.1

str(Step0.1)
unique(Step0.1)

pivot_wider(Year = Year, names_from = LL_gear, values_from = gsub("[^0-9]","",value))

Step0 %>% pivot_wider(Year = Year, names_from = value) %>%
  pivot_

Step0 %>% mutate(HOOK_SIZE = ifelse(sub("(.*?)[\\.|:].*", "\\1", col1) %in% c("Hook Size"),
                                    gsub("[^0-9]","",col1),
                                    ifelse(sub("(.*?)[\\.|:].*", "\\1", col2) %in% c("Hook Size"),
                                           gsub("[^0-9]","",col2),NA
                                    )))

Step0 %>% mutate(Ex_txt = sub("(.*?)[\\.|:].*", "\\1", col1),
                 Ex_no = gsub("[^0-9]","",col1))

select(Step0,contains("Hook Size"))

mutate(Hook_size = select(contains("Hook Size")))

mutate(Hook_size = gsub("[^0-9]","",select(contains("Hook Size"))))

mutate(Hook_size = gsub("[^0-9]","",col1))




pivot_longer(-Year) %>% 
  select(-name) %>% 
  drop_na() -> step1

step1<-as.data.frame(step1)
n_col2<-max(stringr::str_count(step1[,2], ":"))+1  
colnames(step1)<-c("Year","Shit") 

step1 %>% separate(Shit, into = paste0("col", 1:n_col),sep = ":") %>%
  mutate(across(everything(),~dplyr::na_if(.,""))) %>%
  pivot_longer(-Year) %>% 
  select(-name) %>% 
  drop_na() -> step2  


pivot_wider(Year = Year, names_from = hooks)


pivot_wider

ll_lb$Config.List

ll_lb<-separate_rows(ll_lb,"TICKET",sep=",")

eights<-which(ll_lb$Config.List %in% ll_lb$Config.List[stringr::str_count(ll_lb$Config.List, ",") == 8])
nines<-which(ll_lb$Config.List %in% ll_lb$Config.List[stringr::str_count(ll_lb$Config.List, ",") == 9])
twelves<-which(ll_lb$Config.List %in% ll_lb$Config.List[stringr::str_count(ll_lb$Config.List, ",") == 12])
simps<-which(ll_lb$Config.List %in% ll_lb$Config.List[stringr::str_count(ll_lb$Config.List, ",") <=7 ])
?sample
play<-c(sample(simps,3,replace=FALSE),
        sample(eights,3,replace=FALSE),
        sample(nines,3,replace=FALSE),
        sample(twelves,3,replace=FALSE))

view(ll_lb[play,])
lb_samp<-ll_lb[play,]

#1) separate out multiple gear configurations
gear_config<-max(stringr::str_count(lb_samp$Config.List, "---"))+1
lb_samp %>% separate(Config.List, into = paste0("gear_config",1:gear_config),sep = "---") -> lb_samp
#2) get rid of parenthesis...
lb_samp %>% mutate(gear_config1 = gsub(")","",sub(".","",gear_config1))) %>%   #junk1 and junk2
  mutate(gear_config2 = gsub(")","",sub(".","",gear_config2))) %>%
  mutate(gear_config2 = sub(".","",gear_config2)) -> lb_samp
#3) divide out target species into its own column before separating the rest of 
#   the gear configuration into its own rows
lb_samp<-lb_samp %>% mutate(split_gf1 = gear_config1,
                   split_gf2 = gear_config2) %>% 
  separate(split_gf1,into = paste0("gear_target_",1),sep = ",") %>%
  separate(split_gf2,into = paste0("gear_target_",2),sep = ",") %>%
  mutate(gear_target_1 = ifelse(grepl("Target Species", gear_target_1, fixed = TRUE),
                                as.numeric(gsub(".*?([0-9]+).*", "\\1", gear_target_1)),NA),
         gear_target_2 = ifelse(grepl("Target Species", gear_target_2, fixed = TRUE),
                                as.numeric(gsub(".*?([0-9]+).*", "\\1", gear_target_2)),NA))

# 4) now, separate gear config column into multiple rows and get rid of rows with
#    target species
lb_samp <- separate_rows(lb_samp,"gear_config1",sep=", ") %>% 
  filter(!grepl("Target Species", gear_config1, fixed = TRUE)) 
lb_samp <-  separate_rows(lb_samp,"gear_config2",sep=", ") %>%
  filter(!grepl("Target Species", gear_config2, fixed = TRUE))

# 5) separate the gear configuration columns...
lb_samp<-lb_samp %>% separate(gear_config1,into = paste0(c("gear_sub1","gear_spec1"),""),sep = ":") %>%
  separate(gear_config2,into = paste0(c("gear_sub2","gear_spec2"),""),sep = ":") %>%
  mutate(gear_sub1 = str_replace(gear_sub1, " ","_"),
         gear_sub1 = str_replace(gear_sub1, "/","_per_"),
         gear_sub2 = str_replace(gear_sub2, " ","_"),
         gear_sub2 = str_replace(gear_sub2, "/","_per_")) %>%
  pivot_wider(names_from = gear_sub1, values_from = gear_spec1) %>% 
  mutate(hook_size1 = gsub("[ (]","",Hook_Size),
         spacing1 = gsub("[ (]","",Spacing),
         hooks_per_skate1 = gsub("[ (]","",Hooks_per_Skate),
         num_of_skates1 = gsub("[ (]","",Num_Skates)) %>% #,
        # num_of_hooks1 = Num_Hooks) %>%
  select(-Hook_Size,-Spacing,-Hooks_per_Skate,-Num_Skates) %>% #,Num_Hooks)
  pivot_wider(names_from = gear_sub2, values_from = gear_spec2) %>%
  mutate(hook_size2 = gsub("[ (]","",Hook_Size),
         spacing2 = gsub("[ (]","",Spacing),
         hooks_per_skate2 = gsub("[ (]","",Hooks_per_Skate),
         num_of_skates2 = gsub("[ (]","",Num_Skates)) %>% #,
  # num_of_hooks1 = Num_Hooks) %>%
  select(-Hook_Size,-Spacing,-Hooks_per_Skate,-Num_Skates) # %>% #,Num_Hooks)

#6) #Separate out multiple tickets
lb_samp <- separate_rows(lb_samp,"Ticket",sep=", ") 
  
data.frame(lb_samp)
colnames(lb_samp)

logs<-separate_rows(lb_samp,"Config.List", sep=",")
viewsamp<-data.frame(lb_sampx)
view(logs)

pivot_wider(names_from = gear_sub1, values_from = gear_spec1,names_glue = "{Phase}_{.value}") #

#n_col<-max(stringr::str_count(ll_lb$Config.List, ","))+1      #Config.List replaced Shit from development code
#  n_precol<-max(stringr::str_count(ll_lb$Config.List, "---"))+1
  
#  ll_lb %>% separate(Config.List, into = paste0("precol",1:n_precol),sep = "---") -> ll_lb #prestep
  n_col1<-max(stringr::str_count(ll_lb$precol1, ":"))+1
  n_col2<-max(stringr::str_count(ll_lb$precol2[!is.na(ll_lb$precol2)], ":"))+1
  
  ll_lb %>% mutate(junk1 = gsub(")","",sub(".","",precol1))) %>%   #junk1 and junk2
    mutate(junk2 = gsub(")","",sub(".","",precol2))) %>%
    mutate(junk2 = sub(".","",junk2)) %>%
    select(-precol1, -precol2) -> ll_lb #prestep
  
  junk1cols<-max(stringr::str_count(ll_lb$junk1, ":"), na.rm=T)+1
  junk2cols<-max(stringr::str_count(ll_lb$junk2, ":"), na.rm=T)+1
  
  ll_lb %>%        
    separate(junk1, into = paste0("col", 1:(junk1cols)),sep = ",") %>%
    separate(junk2, into = paste0("col", (1+junk1cols):(1+junk1cols+junk2cols)),sep = ",") -> ll_lb

  df<-structure(list(`number` = c(51, 57, 57), abilities = c("b1261", 
                                                         "d710", "b1301; d550")), .Names = c("number", "abilities"
                                                         ), row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"
                                                         ))
  df %>%
    mutate(unpacked = str_split(abilities, ";")) %>%
    unnest(cols = unpacked) %>%
    mutate(abilities = str_trim(unpacked))
  