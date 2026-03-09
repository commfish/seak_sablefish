
# Fishery catch 1985-present, fishery CPUE 1997-present
# Author: Aaron Lambert
# Contact: aaron.lambert@alaska.gov
# Last edited: Feb 2026

# Starting in 2023 this code replaces the old fishery CPUE calculations including
# scripts titled fishery_catch_cpue.R (Jane's original) and fishery_catch_cpue_2022reboot.R
# which was the script I used in 2022 as a patch from the old CPUE data.  This
# script uses the CPUE data processed directly from OceanAK logbook and fish ticket
# data.  The HARVEST in the first section still comes from the data querry set
# up by Jane and Justin Priest.  

# To get the OceanAK data and format it for this analysis use script fishery_cpue_prep.R

# 2024 update: There was a big change in the format of the OceanAK output this year
# which necessitated totally reworking the code for combining the fish ticket and 
# logbook data. The good news is that the data set is much more complete and CPUE 
# can be examined at the set level rather than at the trip level as was done in the 
# past. The bad news is that we have temporarilly dropped stat area from the list
# of things we're considering. Rhea doesn't trust the fishticket data regarding
# stat area and the data was not made available to me until late March. Personally
# I think that the fishticket data on stat area could be used for the join and calculations
# made using the logbook data, but there was not enough time to explore that. This
# should be examined and reevaluated going forward. #AGR- hmm did we do that in the data prep code in 2025??  

# Also note that as of 2024, the cpue data is separated into ll trips, pot trips
# and mixed trips. Right now the cpue is from the purely longline trips. 

# Also note that cpue uses the longline gear. In 2023 2/3 of the catch was still
# coming from longline gear and thus the sample size for the index is still a
# adequate. #AGR WHAT IS GOING ON IN 2024 THO?? However, if the pot fishery overtakes the lonline fishery as has
# ocurred in the federal fishery it may be necessary to derive a different index.
# One could have separate indices for the pot and longline fishery but the feds
# are currently using a combined index formulated by Matt Cheng in Curry's lab. 
# Please see his paper for more information: 
# Cheng, M. L. H., C. J. Rodgvellar, J. A. Langan, and C. J. Cunningham. 2023. 
#   Standardizing fishery-dependent catch-rate information across gears and data
#   collection programs for Alaska sablefish (Anaplopoma fimbria). ICES Journal of
#   Marine Science 80, 1028-1042.

source("r_helper/helper.r")
source("r_helper/functions.r")

# Load packages
library(rms)
library(viridis)
if(!require("GGally"))   install.packages("GGally") 
if(!require("mgcViz"))   install.packages("mgcViz") 
if(!require("mgcv"))   install.packages("mgcv") 
if(!require("rms"))   install.packages("rms") # simple bootstrap confidence intervals

# Most recent year of data
YEAR <- 2025


#_______________________________________________________________________________
#****************************************************************************
# Functions -------------------------------------------------------------------
#****************************************************************************
#' Randomly Sample and Filter Fisheries Data
#'
#' This function performs a random hierarchical sampling of fisheries catch data
#' by sequentially filtering through year, ADFG stat area, and sell date to 
#' extract a random subset of observations.
#'
#' @param data A data frame containing fisheries data with columns: year, Adfg, 
#'   and sell_date
#'
#' @return A data frame containing all records that match the randomly selected
#'   year, ADFG stat area, and sell date combination
#'
#' @details The function uses a three-step random sampling process:
#'   \itemize{
#'     \item Step 1: Randomly selects one year from all available years
#'     \item Step 2: Randomly selects one ADFG statistical area from those 
#'       available in the selected year
#'     \item Step 3: Randomly selects one sell date from those available in 
#'       the selected year/ADFG combination
#'   }
#'   
#'   The function then returns all records matching this randomly selected 
#'   year/ADFG/date combination. This is useful for quality control checks or 
#'   random auditing of fisheries data.

random_check <- function(data) {
  
  # Step 1: Randomly select one year
  year_list <- unique(data$year)
  year_check <- sample(year_list, 1)
  
  data1 <- data %>% filter(year == year_check)
  
  # Step 2: Randomly select one ADFG area from the selected year
  adfg_list <- unique(data1$Adfg)
  adfg_check <- adfg_list[sample(length(adfg_list), 1)]
  
  data1 <- data1 %>% filter(Adfg == adfg_check)
  
  # Step 3: Randomly select one sell date from the selected year/ADFG
  d_list <- unique(data1$sell_date)
  d_check <- d_list[sample(length(d_list), 1)]
  
  data1 <- data1 %>% filter(sell_date == d_check)
  
  # Return all records matching the randomly selected criteria
  check <- as.data.frame(data %>% 
                           filter(sell_date == d_check,
                                  year == year_check,
                                  Adfg == adfg_check))
  
  return(check)
}

#****************************************************************************
# LONGLINE Logbook/CPUE data  -------------------------------------------------
#****************************************************************************
# Read in data
ll_cpue <- read_csv(paste0(YEAR+1,"/data/fishery/fishery_ll_cpue_1997_", YEAR,".csv"), 
                    guess_max = 50000)

# Keep unique observations
ll_cpue <- unique(ll_cpue)

# Run random check
random_check(data = ll_cpue)

#
length(is.na(ll_cpue$no_hooks_set))

# Look at unique depredation events
unique(ll_cpue$set_depredation)

# Data will still be stratified by set and some other variables so need to consolidate
# data by year, sell_date, Adfg, Stat, 

# Aaron : These are checks that I dont think are used anymore, but I am retaining them
# until I am sure...

# # 1) Look at cpue based on fish ticket landings... 
# ll_cpue_ftx <- unique(ll_cpue %>% 
#   group_by(year, sell_date, Adfg, trip_no) %>% 
#   #select(-set_date,-julian_day_set,-set_soak,-set_length,-set_depth,-set_no,
#   #       -disposition,-set_depredation)) %>%
#   mutate(no_hooks_p_set = set_hook_count_best_available)) 
#   #AGR added
# 
# random_check(ll_cpue_ftx) #should just be one row for each trip... 
# histogram(ll_cpue_ftx$p_sets_depredated[ll_cpue_ftx$p_sets_depredated > 0])
# 
# #unique(ll_cpue_ftx$trip_set_targets) ARG turned off. Idk, this doesn't exist in the df
# 
# colnames(ll_cpue)
# nrow(ll_cpue)
# nrow(unique(ll_cpue))
# 
# unique(ll_cpue$multi_gear_config); with(ll_cpue, table(multi_gear_config))
# unique(ll_cpue$multigear_trip)
# unique(ll_cpue$trip_recorded_releases); with(ll_cpue, table(year, trip_recorded_releases))
# unique(ll_cpue$set_depredation); unique(ll_cpue$trip_depredation)
# unique(ll_cpue$no_hooks_p_set_trip)
# unique(ll_cpue$gear); str(ll_cpue$gear)
# 
# with(ll_cpue, table(hook_size)); nrow(ll_cpue %>% filter (is.na(trip_soak)))
# with(ll_cpue, table(log_stat_area))
# hist(ll_cpue$julian_day_sell); abline(v=226, col="red")
# hist(ll_cpue$no_hooks_fished_on_trip); abline(v=15000, col="blue")
# table(ll_cpue$hook_space)
# colnames(ll_cpue_ftx)
# unique(ll_cpue_ftx$multi_gear_config)

# }

# Add new column with new name
# ll_cpue_filter1 <- ll_cpue %>% 
#   mutate(no_hooks_p_set = set_hook_count_best_available) 

# Filter out any sets with depredation
# ll_cpue_depr_filter <- filter(ll_cpue, set_depredation %in% c("No depredation", "No depredation data")) # Changed 2025 - kept only entries that had no depredation or no depredation data

# Keep trips that have the necessary information (i.e., single gear trips, non-depredated sets)
# ll_cpue_ftx <- ll_cpue_depr_filter %>% 
# 
ll_cpue_ftx <- ll_cpue %>% 
  filter(set_depredation %in% c("No depredation", "No depredation data"),
         log_stat_area %in% c(345631,345603,345702,345803,345701,345731,335701)) %>% 
  mutate(no_hooks_p_set = set_hook_count_best_available) %>% 
  filter(multi_gear_config == "single_config" &   #get rid of trips that reported 2 gear configurations
           #trip_set_targets == "all_Sablefish",   # only use trips that were dedicated to sablefish... but not yet...
           p_sets_depredated == 0 &
           !is.na(sell_date) & 
           !is.na(mean_hook_spacing) & 
           #!is.na(hook_space) & 
           !is.na(sable_lbs_set) &
           # !is.na(start_lon) & 
           # !is.na(start_lon) & #remove this because this is at the set level... 
           !is.na(trip_soak) & 
           !is.na(trip_depth) &
           !is.na(mean_hook_size) &  
           #!is.na(hook_size) &
           hook_size != "MIX" &
           trip_soak > 0 & !is.na(trip_soak) & # soak time in hrs
           julian_day_sell > 226 # if there were special projects before the fishery opened
         # limit analysis to Chatham Strait and Frederick Sounds where the
         # majority of fishing occurs
         # target = 710 &
         #Stat %in% c("345603", "345631", "345702", # removed in 2024 because filtered out already:
         #             "335701", "345701", "345731", "345803")
  ) %>% 
  
  mutate(Year = factor(year), 
         Gear = factor(gear),
         Adfg = factor(Adfg),
         Trip = factor(trip_no),
         StatArea = factor(log_stat_area),
         #Stat = factor(Stat),
         StatArea = fct_relevel(StatArea,
                            c("345702", "335701", # Frederick Sound
         # Chatham south to north
                            "345603", "345631", "345701", "345731", "345803")),
         Depr_sum = ifelse(p_sets_depredated == 0, "none",
                           ifelse(p_sets_depredated == 1, "all sets", 
                                  ifelse(p_sets_depredated > 0 & p_sets_depredated <= 0.25,
                                         "0-25% of sets",
                                         ifelse(p_sets_depredated > 0.25 & p_sets_depredated <= 0.5,
                                                "25-50% of sets",
                                                ifelse(p_sets_depredated > 0.5 & p_sets_depredated <= 0.75,
                                                       "50-75% of sets","75-100%"))))),
         # 01=Conventional, 02=Snap On, 05=Mixed, 06=Autobaiter -> 01, 02, 05
         # show no strong differences. main difference with autobaiters, which
         # have lwr cpue than conventional gear
         Gear = derivedFactor("AB" = Gear == "6",
                              "CS" = Gear %in% c("1","2","5")),
         Hook_size = factor(hook_size),  #might be worth treating as numeric? 
         Mean_hook_size = mean_size, 
         # standardize hook spacing (Sigler & Lunsford 2001, CJFAS), 1 m = 39.37 in
         #std_hooks = 2.2 * no_hooks_p_set * (1 - exp(-0.57 * (hook_space / 39.37))), 
         std_hooks = 2.2 * no_hooks_set * (1 - exp(-0.57 * (mean_hook_spacing / 39.37))),
         std_cpue_trip = trip_sable_lbs_set / std_hooks,
         std_cpue = sable_lbs_set / std_hooks,
         # dummy varbs, for prediction with random effects
         dum = 1, 
         dumstat = 1) %>% 
  #"sets" (aka effort_no) is the set identifier. Currently Martina's scripts
  #filter out sets that Kamala identifies as halibut target sets. Create a new
  #column that is the total number of sablefish target sets in a trip (trip_no's
  #only unique within a year)
  #group_by(year, trip_no) %>%              # XXX!!! already done in raw processing... 
  #mutate(no_sets = n_distinct(sets)) %>% 
  group_by(year) %>% 
  mutate(
    #The number of vessels participating in the fishery has descreased by 50% from
    #1997-2015. create new column is the total number of active vessels
    #participating in a given year
    total_vessels = n_distinct(Adfg),
    # Total unique trips per year
    total_trips = n_distinct(trip_no)) %>% 
  ungroup()

ll_cpue_ftx %>% 
  select(year, Vessels = total_vessels, Trips = total_trips) %>% 
  gather(Variable, Count, -year) %>% 
  distinct() %>%
  ggplot(aes(x = year, y = Count)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~ Variable, ncol = 1, scales = "free") +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "") +
  ylim(0, NA) -> trips_vessels

trips_vessels
ggsave(plot = trips_vessels, paste0(YEAR+1,"/figures/fishery_tripandvessel_trends_1997_", YEAR, ".png"), 
       dpi=300, height=6, width=5, units="in")


# Calculate proportions of all_retained by year and Adfg
retention_props <- ll_cpue_ftx %>%
  group_by(Year) %>%
  summarise(
    total = n(),
    all_retained = sum(trip_recorded_releases == "all_retained"),
    some_released = sum(trip_recorded_releases == "some_released"),
    prop_all_retained = all_retained / total,
    .groups = "drop"
  )

ggplot(retention_props, aes(x = Year, y = prop_all_retained)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Proportion All Retained",
       x = "Year") +
  coord_cartesian(ylim = c(0,1))+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 90))

#****************************************************************************
# Bootstrap ------------------------------------------------------------------
#****************************************************************************
# Simple bootstrap confidence intervals (smean.cl.boot from rms) 
plot_boot1 <- ll_cpue_ftx %>%
  group_by(year) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue))))

# Plot the 95% CI
ggplot(plot_boot1) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper), 
               alpha = 0.1) +
  geom_point(aes(x = year, y = Mean), size = 1) +
  geom_line(aes(x = year, y = Mean)) +
  labs(x = "", y = "Sablefish CPUE (round lb per hook)\n") +
  #lims(y = c(0, 1.5))  + #AGR had to adjust lims- turned this off to not miss data
 # scale_color_viridis_d(name = "Set targets of trip",
#                    labels = c("all halibut","all sablefish","sablefish and halibut mix"),
#                    option = "C", begin=0,end=0.85) +
#  scale_fill_viridis_d(name = "Set targets of trip",
#                       labels = c("all halibut","all sablefish","sablefish and halibut mix"),
#                     option = "C", begin=0,end=0.85) +
  theme(legend.position = "bottom") #, + #c(0.2,0.8),
  
ggsave(paste0(YEAR+1,"/figures/llcpue_ftx_bootCI_bytarget_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")


# Look at CPUE by recorded releases 
plot_boot2 <- ll_cpue_ftx %>%
  #filter(trip_set_targets == "all_Sablefish") %>%
  group_by(year,trip_recorded_releases) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue))))

# Plot this results
ggplot(plot_boot2) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper, fill = trip_recorded_releases), 
              alpha = 0.1) +
  geom_point(aes(x = year, y = Mean, col = trip_recorded_releases), size = 1) +
  geom_line(aes(x = year, y = Mean, col = trip_recorded_releases)) +
  labs(x = "", y = "Sablefish CPUE (round lb per hook)\n") +
  #lims(y = c(0, 7)) + #agr deactivated for better graph
  scale_color_viridis_d(name = "",
                        labels = c("Trip logged releases","Trip logged NO releases"),
                        option = "C", begin=0,end=0.65) +
  scale_fill_viridis_d(name = "",
                       labels = c("Trip logged releases","Trip logged NO releases"),
                       option = "C", begin=0,end=0.65) +
  theme(legend.position = "bottom") #, + #c(0.2,0.8),

ggsave(paste0(YEAR+1,"/figures/llcpue_ftx_bootCI_byrelease_1997_", YEAR, ".png"),
       dpi=300, height=4, width=6, units="in") 



# ll_cpue_ftx %>%
#   #filter(trip_set_targets == "all_Sablefish") %>%
#   group_by(year,Depr_sum) %>%
#   do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> plot_boot3 #view(plot_boot2)
# 
# ggplot(plot_boot3) +
#   geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper, fill = Depr_sum),
#               #             alpha = 0.1, fill = "grey55") +
#               alpha = 0.1) +
#   geom_point(aes(x = year, y = Mean, col = Depr_sum), size = 1) +
#   geom_line(aes(x = year, y = Mean, col = Depr_sum)) +
#   # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
#   labs(x = "", y = "Fishery CPUE (round lb per hook)\n") #
#   #lims(y = c(0, 2))

#ggsave(paste0(YEAR+1,"/figures/fshcpue_ftx_bootCI_bydepr_1997_", YEAR, ".png"),
#       dpi=300, height=4, width=7, units="in")

# ll_cpue_ftx %>%
#   #filter(trip_set_targets == "all_Sablefish") %>%
#   group_by(year,multigear_trip) %>%
#   do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> plot_boot4 #view(plot_boot4)
# 
# ggplot(plot_boot4) +
#   geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper, fill = multigear_trip), 
#               #             alpha = 0.1, fill = "grey55") +
#               alpha = 0.1) +
#   geom_point(aes(x = year, y = Mean, col = multigear_trip), size = 1) +
#   geom_line(aes(x = year, y = Mean, col = multigear_trip)) +
#   geom_errorbar(aes(x=year, y=Mean,ymin=Lower,ymax=Upper, col = multigear_trip),
#                 position=position_dodge(width=0), width=0.5, size=0.2) +
#   # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
#   labs(x = "", y = "Fishery CPUE (round lb per hook)\n") +
#   #lims(y = c(0, 1.5)) +  #AGR turned this off
#   scale_color_viridis_d(name = "",
#                         labels = c("Longline trip","Mixed longline & pot trip"),
#                         option = "A", begin=0,end=0.65) +
#   scale_fill_viridis_d(name = "",
#                        labels = c("Longline trip","Mixed longline & pot trip"),
#                        option = "A", begin=0,end=0.65) +
#   theme(legend.position = "bottom")

# ggsave(paste0(YEAR+1,"/figures/llcpue_ftx_bootCI_bygeartrip_1997_", YEAR, ".png"),
#        dpi=300, height=4, width=6, units="in")

#****************************************************************************
# Normality --------------------------------------------------------------------
#****************************************************************************

#for analysis we will use CPUE from trips that targeted only sablefish and that
# experienced no depredation... 
ll_cpue_ftx_clean <-ll_cpue_ftx %>% 
  filter(#trip_set_targets == "all_Sablefish",
         Depr_sum == "none")
# Long right tail
ggplot(ll_cpue_ftx_clean, aes(std_cpue)) + 
  geom_density(alpha = 0.4, fill = 4)

# Better, but still not normal with log transformation
ggplot(ll_cpue_ftx_clean, aes(log(std_cpue + 1))) + 
  geom_density(alpha = 0.4, fill = 4)

# Following Jenny Stahl and Ben Williams' work in the SSEI, increase CPUE by 10%
# of the mean per Cambell et al 1996 and Cambell 2004. Back-transform with
# exp(cpue - mean(fsh_cpue$std_cpue) * 0.1)
ll_cpue_ftx_clean <- ll_cpue_ftx_clean %>% 
  mutate(cpue = log(std_cpue + (mean(ll_cpue_ftx_clean$std_cpue, na.rm=T) * 0.1)))

# Plot with mean corrected CPUE. Normally distributed
ggplot(ll_cpue_ftx_clean, aes(cpue)) + 
  geom_density(alpha = 0.4, fill = 4) 

# Trends over time
ggplot(ll_cpue_ftx_clean, aes(Year, std_cpue)) + 
  geom_boxplot()


#****************************************************************************
# CPUE Standardization: Variable Exploration --------------------------------------
#****************************************************************************
# HOOK SIZE performance - hook size 11 should be removed due to sample size and
# infrequency of use. Probably size 7 too - 4 vessels fished size 7 hooks in
# 1997, and only 1 vessel fished it until 2004
# 2022: also size 6 hooks ... half as many as size 7
fsh_cpue_ll_ft<-ll_cpue_ftx_clean

# Note 2025 - set depredation includes only entries with "no depredation" or "no depredation data." 
# Trip depredation includes entries where depredation occurred. Confirm that these are fine to include?

table(fsh_cpue_ll_ft$Hook_size)
#new filter includes mixed hook size sets... need to cull those for analysis if
# hook size is included... 
str(fsh_cpue_ll_ft$Hook_size)
fsh_cpue_hooks<-fsh_cpue_ll_ft %>% 
  filter(Hook_size %in% c("11","12","13","14","15","16")) %>%
  mutate(Hook_size = fct_relevel(Hook_size,c("11","12","13","14","15","16")))

fsh_cpue_hooks %>% filter(Hook_size == "7") %>% distinct(Adfg, Year, trip_no)
fsh_cpue_hooks %>% filter(Hook_size == "6") %>% distinct(Adfg, Year, trip_no)

# Not much contrast in CPUE between remaining hook sizes... maybe some
# difference in performance between years/areas
ggplot(fsh_cpue_hooks, aes(Hook_size, cpue)) + geom_boxplot()
ggplot(fsh_cpue_hooks, aes(Year, cpue, fill = Hook_size)) + geom_boxplot()+
  theme(axis.text.x = element_text(size = 14, angle = 90, h = 1)) +
  labs(x = "", y = "Fishery CPUE\n")

#ggplot(fsh_cpue_hooks, aes(Stat, cpue, fill = Hook_size)) + geom_boxplot()+
#  labs(x = "\nStat area", y = "Fishery CPUE\n")
# New hook size 6 in 2019, vessel look up:

#**2022: I think the best way to get CPUE is cull 6,7, and 11 (<100 samples) and
# then ignore hook size in estimating CPUE since it doesn't make a difference
# (even if model fits better)
#AGR - hook size and year appear to have an interaction, at least for size 12. 
#But this may be insignificant in the grand scheme of things

fsh_cpue_cl<-fsh_cpue_hooks %>%
  filter(!Hook_size %in% c("6","7","11")) %>%
  mutate(soak_p_set = set_soak)

# Depth - clear increasing trend, asymptotes ~ 450 m
##AGR - I dont agree that increasing... it's mostly linear 300-900 (meters?? what units?)
###AGR- I would exclude this from the standardization, but that is me.
### SYW - I also don't agree that this is increasing
ggplot(fsh_cpue_cl, aes(set_depth, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 2, se = FALSE) 

# Soak time - cut off at 40 hrs b/c it looks like there's a slight outlier
# effect
# hmmm ... debatable.  Outliers, but legit?  probably long soaks do to weather or other issues?

ggplot(fsh_cpue_cl %>% 
         filter(!is.na(soak_p_set) & soak_p_set > 0), 
       aes(soak_p_set, cpue)) + 
  geom_point(shape = 20) + 
  geom_smooth(size = 2, se = FALSE) 

#fsh_cpue_cl %>% filter(soak_p_set < 40) -> fsh_cpue_cl

#Total km fished 
ggplot(fsh_cpue_cl, aes(total_km_fished, cpue)) + 
  geom_point(shape = 20) + 
  geom_smooth(size = 2, se = FALSE)

fsh_cpue_cl <-fsh_cpue_cl %>%
  filter(total_km_fished < 200 & total_km_fished > 0) 

ggplot(fsh_cpue_cl, aes(total_km_fished, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 2, se = FALSE)

#individual sets length: 
ggplot(fsh_cpue_cl %>% 
         filter(!is.na(set_length) & set_length > 0), 
       aes(set_length, cpue)) +
  geom_point(shape = 20) + 
  geom_smooth(size = 2, se = FALSE)

# similar to soak time trends... Jane is right about the scavengers down there! 

# Inconsistent and very slight latitudinal effect #!!! NEED TO GET LAT LONG DATA  ???
# 2023 change: with fish tickets will ignore lat long and just use stat area as spatial variable

# Inconsistent and very slight seasonal effect
ggplot(fsh_cpue_cl, aes(julian_day_sell, cpue, group = Year, colour = Year)) +
  geom_smooth(method = 'loess', span = 1, se = FALSE) 

# By stat area: Clear differences in stat area and sample size by stat area
fsh_cpue_cl %>% 
ggplot(aes(x = StatArea, cpue)) +
  geom_boxplot()+
  labs(x = "\nStat area", y = "Fishery CPUE\n")

#****************************************************************************
# CPUE std with GAM ------------------------------------------------------------
#****************************************************************************
# Potential variables influencing CPUE (ultimately interested in estimating a
# Year effect):
# Depth -      Increase in CPUE up to ~ 450 m, then asymptote. Very clear and
#              consistent trend between years.
# Julian_day - decrease towards the end of the season? EDA suggested there is no
#              consistent seasonal trend. If there is a trend, its slightly 
#              decreasing over the season.  
#              PJ22: I agree with slight decreasing trend through season.
#                    soak time and total km fished have similar patterns... 
# Adfg -       Vessel effect, some are better fishermen than others. Routinely
#              improves model fit and doesn't grossly violate assumptions.
# Gear -       Higher for conventional gear (01) over autobaiter (06) consistently
#              between years, although this becomes dampened with the inclusion on the vessel
#              effect.
# Hook_size -  Optimal hook size? No consistent trend between years. Treat as random
#              effect (Pj22: not sure I agree with random effect designation?  )
# start_lat -  Is there some consistent trend in Chatham Strait going north into
#              Chatham? Not one that is consistent between years. If one exists it tends to
#              be decreasing with latitude. There was no spatial autocorrelation detected
#              (done in previous analysis).
# start_lat/start_lon - spatial autocorrelation  (need to check on this - pj22)
# StatArea -   Variation between stat areas

# Look at correlations and distributions of predictors
fsh_cpue_cl %>% 
  select(Gear, 
         hook_size,
         set_depth,
         soak_p_set, 
         #Adfg, 
         julian_day_sell, 
         set_length, 
         StatArea) %>% #cardinality_threshold=NULL
  GGally::ggpairs(cardinality_threshold = 19) # cut off anything with a correlation less than 0.05 (just shark flag) # Changed 2025 - removed lat/long, added stat area

# Check for factors and NA's
unique(fsh_cpue_cl$hook_size)
nrow(fsh_cpue_cl %>% filter(is.na(Gear)))
nrow(fsh_cpue_cl %>% filter(is.na(hook_size)))
nrow(fsh_cpue_cl %>% filter(is.na(set_depth) | set_depth == 0))
nrow(fsh_cpue_cl %>% filter(is.na(soak_p_set) | soak_p_set == 0)) / nrow(fsh_cpue_cl)
nrow(fsh_cpue_cl %>% filter(is.na(Adfg))) 
nrow(fsh_cpue_cl %>% filter(is.na(start_lon)))
nrow(fsh_cpue_cl %>% filter(is.na(start_lat)))
nrow(fsh_cpue_cl %>% filter(is.na(julian_day_sell)))
nrow(fsh_cpue_cl %>% filter(is.na(set_length) | set_length == 0)) / nrow(fsh_cpue_cl)

# Data for fitting models
cpue_exam <- fsh_cpue_cl %>% 
  select(Year,
         cpue,
         std_cpue,
         Gear,
         hook_size,
         set_depth,
         soak_p_set,
         Adfg,
         julian_day_sell,
         set_length,dum, 
         StatArea) %>% 
  filter(soak_p_set > 0, !is.na(soak_p_set),
         set_length > 0, !is.na(set_length)) %>%
  mutate(hook_size = as.factor(hook_size))

nrow(cpue_exam %>% filter(is.na(set_length))) / nrow(cpue_exam)
cpue_exam <- cpue_exam[complete.cases(cpue_exam),]
nrow(cpue_exam %>% filter(is.na(set_length))) / nrow(cpue_exam)

# Fit models. gamma = 1.4 to avoid overfitting
# s(factor, bs = "re") fits a random effect. This is correct and not a smoothed linear term....
m0 <- bam(cpue ~ Year + Gear, data=cpue_exam, gamma=1.4)
m0.hook <- bam(cpue ~ Year + Gear + hook_size, data=cpue_exam, gamma=1.4)
m0.depth <- bam(cpue ~ Year + Gear + s(set_depth, k=4), data=cpue_exam, gamma=1.4)
m0.soak <- bam(cpue ~ Year + Gear + s(soak_p_set, k=4) , data=cpue_exam, gamma=1.4)
m0.stat <- bam(cpue ~ Year + Gear + s(StatArea, bs='re', by = dum), data=fsh_cpue_cl, gamma=1.4) 
m0.adfg <- bam(cpue ~ Year + Gear + s(Adfg, bs='re', by=dum), data=cpue_exam, gamma=1.4)
#m0.lat_lon <- bam(cpue ~ Year + Gear + te(start_lon, start_lat), data=cpue_exam, gamma=1.4)
#m0.lat <- bam(cpue ~ Year + Gear + s(start_lat), data=cpue_exam, gamma=1.4) #so do we want stat area instead of lat long? AGR
#m0.lon <- bam(cpue ~ Year + Gear + s(start_lon), data=cpue_exam, gamma=1.4) #AGR I turned lat long models off
m0.jday <- bam(cpue ~ Year + Gear + s(julian_day_sell, k=4), data=cpue_exam, gamma=1.4)
m0.length <- bam(cpue ~ Year + Gear + s(set_length), data=cpue_exam, gamma=1.4)

model.list<-list(m0,m0.hook,m0.depth,m0.soak,m0.stat,
                 m0.adfg,
                 m0.jday,m0.length)
names(model.list)<-c("m0","hook","depth","soak","stat",
                     "adfg",
                     "jday","length")
modsum0<-data.frame(); j<-1
for (i in model.list) {
  #mod<-i
  modsum0[j,"model"]<-names(model.list[j])
  modsum0[j,"aic"]<-AIC(i)
  modsum0[j,"dev"]<-summary(i)$dev.expl
  modsum0[j,"rsq"]<-summary(i)$r.sq
  modsum0[j,"dev_exp"]<-summary(i)$dev.expl-summary(m0)$dev.expl
  j<-j+1
}

modsum0 %>% arrange(aic)  
modsum0 %>% arrange(-dev)  
modsum0 %>% arrange(-rsq) 

# 2023: dominant variable is adfg followed by depth and then stat
# but all variables better than the null... 
# AGR 2024 most recent data (in 2025) - adfg then depth, then soak, then stat, then length...so a little different
# 2025: ADFG, depth, soak, hook, length, jday, m0, Stat, so including stat may not be waranted.

#****************************************************************************
# AGR analysis --------------------------------------------------------------
#****************************************************************************
#AGR straight up revises things:
# # cpue_exam$STAT_A <- factor(cpue_exam$log_stat_area)
# 
# #global fixed - not dealing with interaction effects right now but noting that they may be there
# global_fixed <- bam(cpue ~ Year + Gear + hook_size + StatArea + Adfg + s(set_depth, k=4) + s(soak_p_set, k=4) + #fixed effect global model
#                 s(julian_day_sell, k=4) + s(set_length),
#               data=cpue_exam, gamma=1.4) 
# 
# #select ranef #method should be.... ML or REML?? I think REML for ranef sleection, ML for fixed selection, REML for model estimation
# global_ran <-bam(cpue ~ Year + Gear + hook_size + s(set_depth, k=4) + s(soak_p_set, k=4) + #all random effects 
#               s(Adfg, bs='re') + s(StatArea, bs='re')+
#               s(julian_day_sell, k=4) + s(set_length),
#             data=cpue_exam, gamma=1.4) #fREML is the method default, good enough I think- at least for model selection
# 
# ran_1 <- bam(cpue ~ Year + Gear + hook_size + Adfg + s(set_depth, k=4) + s(soak_p_set, k=4) + #stat area is a ranef
#                       s(StatArea, bs='re')+
#                       s(julian_day_sell, k=4) + s(set_length),
#                     data=cpue_exam, gamma=1.4)
# 
# ran_2 <- bam(cpue ~ Year + Gear + hook_size + StatArea + s(set_depth, k=4) + s(soak_p_set, k=4) +  #ADFG is a ranef
#                       s(Adfg, bs='re')+
#                       s(julian_day_sell, k=4) + s(set_length),
#                     data=cpue_exam, gamma=1.4)
# 
# #futher work can test interaction effects
# 
# AIC(global_fixed, global_ran, ran_1, ran_2)
# BIC(global_fixed, global_ran, ran_1, ran_2) #well these two give different answers, isn;t that fun
# #selecting with AIC for consistency with the rest of what Jane/Phil did.
# ##read up on BIC selection later tho.
# ##anyway, AIC says the global fixed model wins (no random effects)
# 
# #select fixef - using ML to select the fixed effects
# global_fixed <- bam(cpue ~ Year + Gear + hook_size + STAT_A + Adfg + s(set_depth, k=4) + s(soak_p_set, k=4) + #fixed effect global model
#                       s(julian_day_sell, k=4) + s(set_length),
#                     data=cpue_exam, gamma=1.4, method= "ML") 
# summary(global_fixed)
# #plot
# mAR1 <- bam(cpue ~ Year + Gear + hook_size + STAT_A + Adfg + s(set_depth, k=4) + s(soak_p_set, k=4) + #fixed effect global model
#               s(julian_day_sell, k=4) + s(set_length),
#             data=cpue_exam, gamma=1.4, method= "ML") 

#****************************************************************************
# End of AR Analysis ------------------------------------------------------------
#****************************************************************************
# Model fit with all variables
# global<-bam(cpue ~ Year + Gear + hook_size + s(StatArea,bs="re", by = dum) + s(set_depth, k=4) + s(soak_p_set, k=4) + 
#               s(Adfg, bs='re', by = dum) +
#               s(julian_day_sell, k=4) + s(set_length),
#             data=cpue_exam, gamma=1.4)

global <- bam(cpue ~ Year + Gear + hook_size + StatArea + s(set_depth, k=4) + s(soak_p_set, k=4) + 
                 Adfg +
                 s(julian_day_sell, k=4) + s(set_length),
               data=cpue_exam, gamma=1.4)
# AIC chooses global in 2025 and 2026
AIC(global)

plot(global, page = 1, shade = TRUE, resid = TRUE, all = TRUE)
summary(global)
summary(global)$s.table

# No residual patterns, but may be some outliers
plot(fitted(global), resid(global))
abline(h = 0, col = "red", lty = 2)

# 14 outliers, get rid of them and refit models with new data set
which(fitted(global) < -1.5)   #6 outliers in 2023 #AGR I dont see any outliers in 2025
not_outliers <- which(fitted(global) >= -1.5)
cpue_exam <- cpue_exam %>% 
  slice(not_outliers)

vcov.gam(global)

# Determine if random variables should be included (Stat and Adfg)
# 
# Model with Vessel as RE
m1 <- bam(cpue ~ Year + Gear + hook_size + s(set_depth, k=4) + s(soak_p_set, k=4) + 
            StatArea +  # Changed 2025
            s(Adfg, bs='re', by=dum), data=cpue_exam, gamma=1.4)

# Model with Stat area as RE
m2 <- bam(cpue ~ Year + Gear + hook_size + Adfg + s(set_depth, k=4) +
            s(soak_p_set, k=4) + s(StatArea, bs='re', by=dum), data=cpue_exam, gamma=1.4)

# Model with both as factors
m3 <- bam(cpue ~ Year + Gear + hook_size + StatArea + Adfg + s(set_depth, k=4) + s(soak_p_set, k=4),
          data=cpue_exam, gamma=1.4)

# Model with both as RE
m4 <- bam(cpue ~ Year + Gear + hook_size + s(Adfg, bs='re', by=dum)+  s(StatArea, bs='re', by=dum)+
            s(set_depth, k=4) + s(soak_p_set, k=4), 
          data=cpue_exam, gamma=1.4) #so m3 and m4 are the same?? AGR

AIC(m1, m2, m3, m4) 

# Better, AIC still likes hook size in there...
plot(fitted(m1), resid(m1))
abline(h = 0, col = "red", lty = 2)

plot(m1, page = 1, shade = TRUE, resid = TRUE, all = TRUE)
summary(m1)

plot(m3, page = 1, shade = TRUE, resid = TRUE, all = TRUE)
summary(m3)

# CPUE increases with depth, then asymptotes ~ 450 m. CPUE is constant and then
# drops off ~ 10 hr soak time, but the overall effect is weaker than depth
# Conventional gear performs slightly better than autobaiter gear.

# Determine whether to keep hook size or keep it as a random effect
# PJ22: not crazy about hook size as a random effect - it should always be a fixed affect because
# it would directly affect cpue... as opposed to year or vessel random effect where there is 
# random noise associated with the variable, hook size has a plausible effect on the response variable? #AGR I agree with phil
# Aaron: Hooks size should not be RE. They are not really similar but different. They are mostly different...
#        I will treat Stat area and vessel as a random effect, because this makes the most sense, and the AIC is only 2 points
#        different than fitting them as factors.
# 
# m5 <- bam(cpue ~ Year + Gear + s(set_depth, k=4) + s(soak_p_set, k=4) +    #no hook size
#             StatArea+ 
#             s(Adfg, bs='re', by=dum), data=cpue_exam, gamma=1.4)
# m6 <- bam(cpue ~ Year +  Gear + s(set_depth, k=4) + s(soak_p_set, k=4) +    #hook size as a random variable
#             s(hook_size, bs='re', by=dum) + StatArea+ # Changed 2025 - removed lat/long, added stat area
#             s(Adfg, bs='re', by=dum), data=cpue_exam, gamma=1.4)

AIC(global, m1, m2, m3, m4)
BIC(global, m1, m3, m4)
#library(performance)#agr add
#model_performance(m5,m6)
# 
# plot(m6, page = 1, shade = TRUE, resid = TRUE, all = TRUE)
# summary(m5)
# summary(m6)

# By AIC, treating Hooksize as a factor has the best predictive pwr, but the 
# model treating it as a random effect is a close second. Inclusion of hook size
# as a factor or re results in no change in the deviance explained. Because
# there's no strong trend or difference between hook sizes and it seems just to
# account up some of the random variation, I'm going carry m6 forward (the model
# with the re for hook size). ##hmm ok. AGR
# 2022: OK, same results.  get Jane's point about no strong trend with hook size
#       but am disinclined to treat it as a random effect.  HS is not a random category
#       or group... there is an effect of HS on catchability.... 
#       I will carry model 1 forward instead of model 6.
# 2023: Now that we are just using fishticket data it probably doesn't make sense
#       to schwag the lat-long data into this and just use the stat area as our 
#       spatial variable.  For now models m7-m10 will be blocked out... 
# 2024: Stat area is out as per Rhea (not sure I agree) but lat on is back in because
#       the new data format has us working at the set level:
#       In this formulation the model likes hook size as a factor (not random effect)
# 2025: AGR - the global model wins, according to AIC. We added stat area back in and 
#       removed lat/long (well, Spencer did) global wins and then m6, according to AIC 
#       but should make a better decision than that
# 2026: Stat Area is included i the final std model fit      

#{
#Determine whether to include lat and long #AGR 2025- nope. I turned this whole section off. no lat/long in 2025, yes stat area
# #AGR - ok I changed this to stat area- do we include?
# m7 <- bam(cpue ~ Year + Gear + s(set_depth, k=4) + s(soak_p_set, k=4) + 
#             hook_size + STAT_A + #s(Hook_size, bs='re', by=dum) + 
#             s(Adfg, bs='re', by=dum), data=cpue_exam, gamma=1.4)
# m8 <- bam(cpue ~ Year + Gear + s(set_depth, k=4) + s(soak_p_set) + 
#             hook_size + STAT_A + #s(Hook_size, bs='re', by=dum) + 
#             s(Adfg, bs='re', by=dum), data=cpue_exam, gamma=1.4)
# m9 <- bam(cpue ~ Year + Gear + s(set_depth, k=4) + s(soak_p_set, k=4) + 
#             hook_size + STAT_A +  #s(Hook_size, bs='re', by=dum) + 
#             s(Adfg, bs='re', by=dum), data=cpue_exam, gamma=1.4)
# 
# AIC(global, m1, m6, m7, m8, m9) #AIC(m6, m7, m8, m9)
# BIC(global, m1, m6, m7, m8, m9)
# 
# summary(m7)
# summary(m8)
# summary(m9)

# m9, the model with the latitudinal effect, performs best by AIC, but only
# results in a slight improvement in the dev explained. Try limiting the number
# of knots to guard against overfitting... but m9 still performs best by AIC.
# Phil note: knots = k
# Phil note; tensor smoother allows integration of two variables (lat and lon here)
# same in 2022
# m10 <- bam(cpue ~ Year + Gear + s(set_depth, k=4) + s(soak_p_set, k=4) + 
#              hook_size + STAT_A + #s(Hook_size, bs='re', by=dum) +  
#              s(Adfg, bs='re', by=dum), data=cpue_exam, gamma=1.4) #AGR - stat area as a smoothed numeric again. I feel like that should be a factor
# AIC(global,m1, m6, m10) #m7, m8, m9  AGR removed

#plot(m9, page = 1, shade = TRUE, all = TRUE) #resid = TRUE, #AGR turned off
# plot(m10, page = 1, shade = TRUE, all = TRUE) #resid = TRUE,

# m7 with both lat and lon with a tensor smoother has the second best
# performance. red/orange is higher CPUE, green average and blue lower; can
# change "too.far" values to change what shows on graph. Highest cpue in the
# north, south and central chatham
#vis.gam(m7, c('log_stat_area'), type='response', plot.type='contour', color='topo', too.far=0.1) #AGR turned off the lat/long relevant code
#}
# The inclusion of a seasonal effect  improves model fit - there is a
# slightly decreasing trend in cpue on average over the course of the season.
m11 <- bam(cpue ~ Year + Gear + s(julian_day_sell, k=4) + s(set_depth, k=4) + 
             s(soak_p_set, k=4)+ StatArea + #s(start_lat) + 
             hook_size + 
             s(Adfg, bs='re', by=dum), data=cpue_exam, gamma=1.4)
AIC(global,m1,m6, m10, m11) #AIC(m1, m7, m9, m11)  m7,m9, #global does best AIC 2015 AGR
BIC(global,m1,m6, m10, m11) #AIC(m1, m7, m9, m11) m7,m9, #m11 does best BIC 2025 AGR
summary(m11)
plot(m11, page = 1, shade = TRUE, all = TRUE) #resid = TRUE,
plot(global, page = 1, shade = TRUE, all = TRUE) #resid = TRUE,
# 2024: model prefers to have set_length dropped (model 11)
# 2025: global or M11 would work

# # Relationship between depth and soak time - highest cpue in > 450 m
# # and ~ 10 hr soak time
vis.gam(global, c('set_depth', 'soak_p_set'), plot.type='contour', type='response', color='topo', too.far=0.1)

#global<-bam(cpue ~ Year + Gear + Hook_size + s(trip_depth, k=4) + 
#              s(soak_p_set, k=4) + s(total_km_fished) + 
#              s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum) +
#              s(julian_day_sell, k=4) ,
#            data=fsh_cpue_cl, gamma=1.4)

# AIC(global, m1, m11) #AIC(m1, m7, m9, m11)
# BIC(global, m1, m11) #agr added

# AIC(global_fixed, global, m1,m2,m3,m4,m5,m6, m7, m11) # 
# BIC(global_fixed, global, m1,m2,m3,m4,m5,m6, m7, m11) #AGR BIC chooses M11 tho. AIC chooses global

summary(global)
plot(global, page = 1, shade = TRUE, all = TRUE) #resid = TRUE,
vis.gam(global, c('set_depth', 'soak_p_set'), plot.type='contour', type='response', color='topo', too.far=0.1)
vis.gam(global, c('set_depth', 'set_length'), plot.type='contour', type='response', color='topo', too.far=0.1)
vis.gam(global, c('soak_p_set', 'set_length'), plot.type='contour', type='response', color='topo', too.far=0.1)

plot.gam(global)

str(diag(vcov.gam(global)))

# Compare the models
model.list<-list(global,m1,m2,m3,m4)
names(model.list)<-c("global","m1","m2","m3","m4")
names(model.list[1])
modsum<-data.frame(); j<-1
for (i in model.list) {
  #mod<-i
  modsum[j,"model"]<-names(model.list[j])
  modsum[j,"aic"]<-AIC(i)
  modsum[j,"bic"]<-BIC(i) #AGR added
  modsum[j,"dev"]<-summary(i)$dev.expl
  modsum[j,"rsq"]<-summary(i)$r.sq
  j<-j+1
}

modsum %>% arrange(aic)  
modsum %>% arrange(bic) 
modsum %>% arrange(-dev)  
modsum %>% arrange(-rsq) 

#****************************************************************************
# GAM summary ----
#****************************************************************************
# Final model structure (m12) (* = random effect): #AGR WHATS UP WITH THIS BLOCK??
# CPUE ~ Year + Gear + s(julian_day_sell, k=4) + s(trip_depth, k=4) + 
#         s(soak_p_set, k=4) + s(total_km_fished) + 
#        Hook_size + 
#  s(Stat, bs='re', by=dumstat) + s(Adfg, bs='re', by=dum)

# 36.7% deviance explained in 2021
# 33.9% in 2022 with pj mods.  With Jane's original code get 34.9%
# 44.9% in 2023

# CPUE decreases throughout the season. CPUE increases with depth, then
# asymptotes ~ 450 m. CPUE is constant and then drops off ~ 10 hr soak time,

# The overall effect of julian day, soak time, and latitude is weaker than
# depth. Conventional gear performs slightly better than autobaiter gear. 

#****************************************************************************
# Predictions to get Standardized CPUE --------------------------------------
#****************************************************************************
#Create standard dataset to get standardized CPUE for each year

std_dat <- expand.grid(year = unique(cpue_exam$Year),
                       Gear = 'CS',
                       set_depth = mean(cpue_exam$set_depth), 
                       soak_p_set = mean(cpue_exam$soak_p_set), 
                       julian_day_sell = median(cpue_exam$julian_day_sell),
                       # log_stat_area = median(cpue_exam$log_stat_area), # Note 2025 - added this but not certain it makes the most sense
                       set_length = median(cpue_exam$set_length),
                       StatArea = "345701",
                       hook_size = "14",
                       Adfg = "35491",
                       dum = 0,
                       dumstat = 0) %>% 
  mutate(Year = factor(year))

#checking my code with Jane's... checks out :)
pred_cpue<-predict.bam(global, type="response", std_dat, se = TRUE)

#Put the standardized CPUE and SE into the data frame and convert to
#backtransformed (bt) CPUE
std_dat <- std_dat %>% 
  mutate(fit = pred_cpue$fit,
         se = pred_cpue$se.fit,
         upper = fit + (2 * se),
         lower = fit - (2 * se),
         bt_cpue = exp(fit) - (mean(cpue_exam$cpue) * 0.1),
         bt_upper = exp(upper) - (mean(cpue_exam$cpue) * 0.1),
         bt_lower = exp(lower) - (mean(cpue_exam$cpue) * 0.1),
         bt_se = (bt_upper - bt_cpue) / 2  #,
         #bt_cv = bt_se/bt_cpue
  ) 


# Nominal CPUE ----
fsh_sum <- cpue_exam %>% mutate(year = Year) %>%
  group_by(year) %>% 
  dplyr::summarise(fsh_cpue = mean(std_cpue),
                   sd = sd(std_cpue),
                   n = length(std_cpue),
                   se = sd / (n ^ (1/2)),
                   var = var(std_cpue),
                   cv = sd / fsh_cpue,
                   upper = fsh_cpue + (2 * se),
                   lower = fsh_cpue - (2 * se)) 

# Compare predicted cpue from gam to nominal cpue
fsh_sum %>%
  select(year, cpue = fsh_cpue, upper, lower) %>% 
  mutate(CPUE = "Nominal") %>%
  bind_rows(std_dat %>% 
              select(year, cpue = bt_cpue, upper = bt_upper, lower = bt_lower) %>% 
              mutate(CPUE = "Fully Standardized")) %>% #data.frame() %>%
  mutate(year = as.numeric(as.character(year))) %>%
  ggplot() + #lims(y = c(0, 3)) +
  geom_ribbon(aes(year, ymin = lower, ymax = upper, fill = CPUE), 
              colour = "white", alpha = 0.2) +
  #geom_ribbon(aes(year, ymin = lower, ymax = upper), 
  #            alpha = 1) +
  geom_point(aes(year, cpue, colour = CPUE, shape = CPUE), size = 2) +
  geom_line(aes(year, cpue, colour = CPUE, group = CPUE), size = 1) +
  # scale_colour_grey(name = "Standardized CPUE") +
  # scale_fill_grey(name = "Standardized CPUE") +
  scale_colour_manual(values = c("darkcyan", "goldenrod"), name = "Standardized CPUE") +
  scale_fill_manual(values = c("darkcyan", "goldenrod"), name = "Standardized CPUE") +
  scale_shape_manual(values = c(19, 17), name = "Standardized CPUE") +
  #scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  labs(x = "", y = "Fishery CPUE (round lb/hook)\n") +
  theme(legend.position = c(0.8, 0.2)) +
  expand_limits(y = 0)

ggsave(paste0(YEAR+1,"/figures/compare_stdcpue_llfsh_", YEAR, ".png"), dpi=300, height=4, width=7, units="in")

#compare to the old methods... 

old_cpue<-read_csv("2022/output/fshcpue_1997_2021_nominal_for_fut.csv", 
                   guess_max = 50000) 
cpue_23 <- read_csv("2023/output/ll_cpue_fullstand_1980_2022.csv", 
                    guess_max = 50000)

#compared to nominal unstandardized indices used pre-2023:
fsh_sum %>%
  select(year, cpue = fsh_cpue, upper, lower) %>% 
  mutate(CPUE = "Nominal") %>% 
  bind_rows(std_dat %>% 
              select(year, cpue = bt_cpue, upper = bt_upper, lower = bt_lower) %>% 
              mutate(CPUE = "Fully Standardized")) %>% 
  bind_rows(old_cpue %>% 
              select(year, cpue = fsh_cpue, upper, lower) %>% 
              mutate(CPUE = "Pre-2023 Nominal",year = as.factor(year))) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  ggplot() +
  geom_ribbon(aes(year, ymin = lower, ymax = upper, fill = CPUE), 
              colour = NA, alpha = 0.2) +
  geom_point(aes(year, cpue, colour = CPUE, shape = CPUE), size = 2) +
  geom_line(aes(year, cpue, colour = CPUE, group = CPUE), size = 1) +
  # scale_colour_grey(name = "Standardized CPUE") +
  # scale_fill_grey(name = "Standardized CPUE") +
  scale_colour_manual(values = c("darkcyan", "goldenrod", "coral"), name = "") +
  scale_fill_manual(values = c("darkcyan", "goldenrod", "coral"), name = "") +
  scale_shape_manual(values = c(19, 17, 19), name = "") +
  #scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  labs(x = "", y = "Fishery CPUE (round lb/hook)\n") +
  # theme(legend.position = c(0.3, .9)) +
  theme(legend.position = "top")+
  expand_limits(y = 0)

ggsave(paste0(YEAR+1,"/figures/compare_OLD_stdcpue_llfsh_", YEAR, ".png"), dpi=300, height=4, width=7, units="in")

# compared to last year's standardization: #AGR not sure if this graph is current (to 2024)
fsh_sum %>%
  select(year, cpue = fsh_cpue, upper, lower) %>% 
  mutate(CPUE = "Nominal") %>% 
  bind_rows(std_dat %>% 
              select(year, cpue = bt_cpue, upper = bt_upper, lower = bt_lower) %>% 
              mutate(CPUE = "Fully Standardized")) %>% 
  bind_rows(cpue_23 %>% filter(year > 1996) %>%
              select(year, cpue = fsh_cpue, upper, lower) %>% 
              mutate(CPUE = "2023 Standardized",year = as.factor(year))) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  ggplot() +
  geom_ribbon(aes(year, ymin = lower, ymax = upper, fill = CPUE), 
              colour = NA, alpha = 0.2) +
  geom_point(aes(year, cpue, colour = CPUE, shape = CPUE), size = 2) +
  geom_line(aes(year, cpue, colour = CPUE, group = CPUE), size = 1) +
  # scale_colour_grey(name = "Standardized CPUE") +
  # scale_fill_grey(name = "Standardized CPUE") +
  scale_colour_manual(values = c("darkcyan", "goldenrod", "coral"), name = "Standardized CPUE") +
  scale_fill_manual(values = c("darkcyan", "goldenrod", "coral"), name = "Standardized CPUE") +
  scale_shape_manual(values = c(19, 17, 19), name = "Standardized CPUE") +
  #scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  labs(x = "", y = "Fishery CPUE (round lb/hook)\n") +
  theme(legend.position = c(0.8, 0.2)) +
  expand_limits(y = 0)

ggsave(paste0(YEAR+1,"/figures/compare_23_&_24_stdcpue_llfsh_", YEAR, ".png"), dpi=300, height=4, width=7, units="in")

# Percent change in fishery nominal cpue compared to a ten year rolling average
fsh_sum %>% mutate(year = as.numeric(as.character(year))) %>%
  filter(year > YEAR - 10) %>% 
  mutate(lt_mean = mean(fsh_cpue),
         perc_change_lt = (fsh_cpue - lt_mean) / lt_mean * 100) 

std_dat %>% mutate(year = as.numeric(as.character(year))) %>%
  filter(year > YEAR - 10) %>% 
  mutate(lt_mean = mean(bt_cpue),
         perc_change_lt = (bt_cpue - lt_mean) / lt_mean * 100) 

# Percent change in fishery nominal cpue from last year
#not relevant since no CPUE in 2020 due to stupid covid
fsh_sum %>% mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= YEAR - 1) %>%
  select(year, fsh_cpue) %>% 
  reshape2::dcast("fsh_cpue" ~ year) -> perc_ch; as.numeric(100*(perc_ch[3]-perc_ch[2])/perc_ch[2])

std_dat %>% mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= YEAR - 1) %>%
  select(year, bt_cpue) %>% 
  reshape2::dcast("bt_cpue" ~ year) -> std_perc_ch; as.numeric(100*(std_perc_ch[3]-std_perc_ch[2])/std_perc_ch[2])

names(perc_ch) <- c("cpue", "last_year", "this_year") 
perc_ch %>% mutate(perc_change_ly = (`this_year` - `last_year`) / `last_year` * 100)

names(std_perc_ch) <- c("cpue", "last_year", "this_year") 
std_perc_ch %>% mutate(perc_change_ly = (`this_year` - `last_year`) / `last_year` * 100)

# Historical CPUE ----

# From KVK: Logbooks were not included in IFDB until 1997. Commercial fishery
# CPUE values prior to 1997, for use in the ASA or other medium, are LEGACY
# VALUES. Jane updates: I don't have any source information from these numbers
# other than this. Kray kept them in a csv file called
# data/legacy_fishery_cpue.csv. Similarly, I moved and renamed the same file as
# data/fishery/legacy_fisherycpue_1980_1996.csv

read_csv("legacy_data/legacy_fisherycpue_1980_1996.csv", 
         col_names = FALSE) %>% as.numeric() -> hist_cpue

# Because the variance is easier to interpret and the point estimates from the
# GAM are extremely similar to nominal CPUE, use nominal CPUE. Use the mean CV
# from 1997-present to estimate the variance for legacy CPUE values, following
# KVK.
nom_cpue_ts <- data.frame(year = 1980:1996,
                          fsh_cpue = hist_cpue) %>% 
  mutate(var = (fsh_cpue * mean(fsh_sum$cv)) ^ 2,
         se = mean(fsh_sum$se),
         upper = hist_cpue+(2*se),
         lower = hist_cpue-(2*se),
         CPUE = "Nominal") %>% 
  bind_rows(fsh_sum %>% mutate(CPUE = "Nominal") %>%
              select(year, fsh_cpue, var, se, upper, lower, CPUE) %>%
              mutate(year = as.integer(as.character(year)))) %>% 
  mutate(cpue = round(fsh_cpue, 3),
         var = round(var, 3)) 

glob_cpue_ts <- data.frame(year = 1980:1996,
                           fsh_cpue = hist_cpue) %>% 
  mutate(var = sqrt(mean(std_dat$bt_se)),
         se = mean(std_dat$bt_se),
         upper = hist_cpue+(2*se),
         lower = hist_cpue-(2*se),
         CPUE = "Fully Standardized") %>% 
  bind_rows(std_dat %>% 
              mutate(var = sqrt(bt_se)) %>% 
              select(year, fsh_cpue = bt_cpue, se = bt_se, var,
                     upper = bt_upper, lower = bt_lower) %>% 
              mutate(CPUE = "Fully Standardized",
                     year = as.integer(as.character(year)))) %>% 
  mutate(cpue = round(fsh_cpue, 3),
         var = round(var, 3)) %>% data.frame() 


cpue_ts_multi<-rbind(nom_cpue_ts,glob_cpue_ts)

cpue_ts_short <- glob_cpue_ts %>% 
         filter(year >= 1997)

ggplot(nom_cpue_ts) +
  geom_point(aes(year, cpue)) +
  geom_line(aes(year, cpue)) +
  geom_ribbon(aes(year, ymin = cpue - sqrt(var), ymax = cpue + sqrt(var)),  #CI's for data
              alpha = 0.2,  fill = "grey") +
  geom_ribbon(aes(year, ymin = lower, ymax = upper),  #CI's for the mean
  # geom_ribbon(aes(year, ymin = cpue - var, ymax = cpue + var),
              alpha = 0.4,  fill = "grey") +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  #lims(y = c(0, 1.5)) +
  #lims(y = c(-0.5, 1.5)) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") 

ggsave(paste0(YEAR+1,"/figures/ll_cpue_nom_1980_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

fig.9 <- ggplot(cpue_ts_multi) +
  geom_point(aes(year, cpue, col = CPUE)) +
  geom_line(aes(year, cpue, col=CPUE)) +
  geom_ribbon(aes(year, ymin = cpue - sqrt(var), ymax = cpue + sqrt(var), fill = CPUE),
              alpha = 0.2) +
  geom_ribbon(aes(year, ymin = lower, ymax = upper, fill = CPUE),
              # geom_ribbon(aes(year, ymin = cpue - var, ymax = cpue + var),
              alpha = 0.4) +
  scale_fill_colorblind()+
  scale_color_colorblind()+
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  #lims(y = c(0, 1.5)) +
  #lims(y = c(-0.5, 1.5)) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") +
  theme(legend.position = "top")

fig.9

#NOTE2022: much more variance in data now... could easily run a straight line
# through error polygon = lack of information in this data!!! 

ggsave(paste0(YEAR+1,"/figures/ll_cpue_nom_1980_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

# Write to file
write_csv(nom_cpue_ts, paste0(YEAR+1,"/output/ll_cpue_nom_", min(nom_cpue_ts$year), "_", YEAR, ".csv"))

glob_cpue_ts<-as.matrix(glob_cpue_ts)
glob_cpue_ts[,c(1,2,3,4,5,6,8)]<-as.numeric(glob_cpue_ts[,c(1,2,3,4,5,6,8)])
glob_cpue_ts<-as.data.frame(glob_cpue_ts)

#****************************************************************************
# Save The Chosen standardized CPUE for SCAA model here ---------------------
write_csv(glob_cpue_ts, paste0(YEAR+1,"/output/ll_cpue_fullstand_", min(nom_cpue_ts$year), "_", YEAR, ".csv"))

