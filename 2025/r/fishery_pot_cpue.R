
# Fishery catch 1985-present, fishery CPUE 1997-present
# Author: Phil Joy
# Contact: philip.joy@alaska.gov
# Last edited: April 2024

# Starting in 2023 this code replaces the old fishery CPUE calculations including
# scripts titled fishery_catch_cpue.R (Jane's original) and fishery_catch_cpue_2022reboot.R
# which was the script I used in 2022 as a patch from the old CPUE data.  This
# script uses the CPUE data processed directly from OceanAK logbook and fish ticket
# data.  The HARVEST in the first section still comes from the data querry set
# up by Jane and Justin Priest.  

source("r_helper/helper.r")
source("r_helper/functions.r")

# if(!require("rms"))   install.packages("rms") # simple bootstrap confidence intervals

# Most recent year of data
YEAR <- 2023

# POT Logbook/CPUE data  ----
random_check<-function(data){ #data<-pot_cpue
  #length(unique(Sable_ll_CPUE$Year))
  data<-data  #data<-ll_cpue
  
  year_list<-unique(data$year)
  if(length(unique(data$year))==1) {
    year_check<-unique(data$year)
  } else {
    year_check<-sample(year_list,1)
  }
  
  data1<-data %>% filter(year == year_check)
  
  adfg_list<-unique(data1$Adfg)
  adfg_check<-adfg_list[sample(length(adfg_list),1)]
  
  data1<-data1 %>% filter(Adfg == adfg_check)
  
  d_list<-unique(data1$sell_date)
  d_check<-d_list[sample(length(d_list),1)]
  
  data1<-data1 %>% filter(sell_date == d_check)
  
  stat_list<-unique(data1$Stat)
  stat_check<-stat_list[sample(length(stat_list),1)]
  
  check<-as.data.frame(data %>% filter(sell_date == d_check,
                                     Stat == stat_check,
                                     year == year_check, #unique(Sable_ll_CPUE$Year)[rands[[1]][1]],
                                     Adfg == adfg_check))
  
  return(check)
}
# Read in data, standardize cpue, etc.
read_csv(paste0(YEAR+1,"/data/fishery/fishery_pot_cpue_2022_", YEAR,".csv"), 
         guess_max = 50000) -> pot_cpue #%>% 
colnames(pot_cpue)
#pot_cpue<-unique(pot_cpue)
random_check(pot_cpue)

#data will still be stratified by set and some other variables so need to consolidate
# data by year, sell_date, Adfg, Stat, 

# 1) Look at cpue based on fish ticket landings... 
#pot_cpue_ftx <- unique(pot_cpue %>% 
#  group_by(year, sell_date, Adfg, Stat) %>% 
  #select(-set_date,-julian_day_set,-set_soak,-set_length,-set_depth,-set_no,
#         -logged_no,-logged_lbs,-disposition,
         #-set_depredation, #depredation not in 2022 pot logbooks... 
#         -start_lat,-start_lon))
#random_check(pot_cpue_ftx) #should just be one row for each trip... 
#histogram(ll_cpue_ftx$p_sets_depredated[ll_cpue_ftx$p_sets_depredated > 0])

unique(pot_cpue_ftx$trip_set_targets)

colnames(pot_cpue)
nrow(pot_cpue)
nrow(unique(pot_cpue))

unique(pot_cpue$Stat); with(pot_cpue, table(Stat))
unique(pot_cpue$multigear_trip)
unique(pot_cpue$trip_recorded_releases); with(pot_cpue, table(year, trip_recorded_releases))
unique(pot_cpue$depredation)
unique(pot_cpue$pot_data)
unique(pot_cpue$pot_space)
unique(pot_cpue$line_diam)
unique(pot_cpue$pot_dim)
unique(pot_cpue$pot_type)
unique(pot_cpue$pot_volume_m3)
unique(pot_cpue$gear_name); str(pot_cpue$gear_name)

with(pot_cpue, table(pot_volume_m3)) #; nrow(pot_cpue %>% filter (is.na(trip_soak)))
with(pot_cpue, table(Stat))
hist(pot_cpue$julian_day_sell); abline(v=226, col="red")
hist(pot_cpue$no_pots_fished_on_trip)#; abline(v=15000, col="blue")
hist(pot_cpue$no_pots_p_set) #; abline(v=15000, col="blue")
nrow(pot_cpue)
table(pot_cpue$pot_space)

colnames(pot_cpue)

nrow(pot_cpue_ftx %>% filter(Stat == "345803"))

pot_cpue %>% 
  #filter(depredation == "No depredation" | is.na(depredation)) %>% 
  filter(#trip_set_targets == "all_Sablefish",   # only use trips that were dedicated to sablefish... but not yet...
           !is.na(sell_date) & 
           #!is.na(mean_hook_spacing) & #!is.na(hook_space) & 
           !is.na(sable_lbs_set) &
          # !is.na(start_lon) & !is.na(start_lon) & #remove this because this is at the set level... 
           !is.na(set_soak) & !is.na(set_depth) &
           #!is.na(mean_hook_size) &   #!is.na(hook_size) &
           #hook_size != "MIX" &
             set_soak > 0 & #!is.na(set_soak) & # soak time in hrs
           julian_day_sell > 226 #& # if there were special projects before the fishery opened
           #no_hooks_p_set < 15000 & # 15000 in Kray's scripts - 14370 is the 75th percentile
           # limit analysis to Chatham Strait and Frederick Sounds where the
           # majority of fishing occurs
           # target = 710 &
            #Stat areas not used in 2024: but you might bring this back!?!
           #Stat %in% c("345603", "345631", "345702",
          #             "335701", "345701", "345731", "345803")
          ) %>% 
  
  mutate(Year = factor(year), 
         #Gear = factor(gear),
         Adfg = factor(Adfg),
         #Stat = factor(Stat),
         #Stat = fct_relevel(Stat,
        #                    c("345702", "335701", # Frederick Sound
                            # Chatham south to north
        #                    "345603", "345631", "345701", "345731", "345803")),
         #Depr_sum = ifelse(p_sets_depredated == 0, "none",
        #                   ifelse(p_sets_depredated == 1, "all sets", 
        #                          ifelse(p_sets_depredated > 0 & p_sets_depredated <= 0.25,
        #                                 "0-25% of sets",
         #                                ifelse(p_sets_depredated > 0.25 & p_sets_depredated <= 0.5,
          #                                      "25-50% of sets",
           #                                     ifelse(p_sets_depredated > 0.5 & p_sets_depredated <= 0.75,
            #                                           "50-75% of sets","75-100%"))))),
         
         #Hook_size = factor(hook_size),  #might be worth treating as numeric? 
         #Mean_hook_size = mean_size, 
         # standardize for pot spacing???  
         #  std_pots = 2.2 * no_hooks_p_set * (1 - exp(-0.57 * (mean_hook_spacing / 39.37))),
         std_cpue = sable_lbs_set / pots_p_set,
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
  ungroup() -> pot_cpue_ftx

random_check(pot_cpue_ftx)
nrow(unique(pot_cpue_ftx))
view(pot_cpue_ftx)

pot_cpue_ftx %>% 
  select(year, Vessels = total_vessels, Trips = total_trips) %>% 
  gather(Variable, Count, -year) %>% 
  distinct() -> trips_and_vessels; trips_and_vessels
trips_and_vessels %>%
  ggplot(aes(x = year, y = Count)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~ Variable, ncol = 1, scales = "free") +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "") +
  ylim(0, NA) -> trips_vessels

trips_vessels
ggsave(plot = trips_vessels, paste0(YEAR+1,"/figures/potfishery_tripandvessel_trends_2022_", YEAR, ".png"), 
       dpi=300, height=6, width=5, units="in")

#-------------------------------------------------------------------------------
# Quick look:unique(pot_cpue$year)
colnames(pot_cpue)
hist(pot_cpue$sable_lbs_set)
hist(pot_cpue$sable_lbs_pot)
hist(pot_cpue$pots_p_set)

hist(pot_cpue_ftx$sable_lbs_pot, breaks = 100)
hist(pot_cpue_ftx$std_cpue, breaks = 100)

nrow(pot_cpue_ftx %>% filter(year == 2022))
nrow(pot_cpue_ftx %>% filter(year == 2023))


# Bootstrap ----

# axis <- tickr(fsh_cpue, year, 5)

# Simple bootstrap confidence intervals (smean.cl.boot from rms) ??rms
library(rms)
pot_cpue_ftx %>% #filter(year == 2022) %>%
  group_by(year) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> plot_boot1

ggplot(plot_boot1) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper), 
 #             alpha = 0.1, fill = "grey55") +
              alpha = 0.1) +
  geom_point(aes(x = year, y = Mean), size = 1) +
  geom_line(aes(x = year, y = Mean)) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Fishery CPUE (round lb per pot)\n") +
  lims(y = c(0, 70)); view(plot_boot1)
  
ggsave(paste0(YEAR+1,"/figures/pot_cpue_ftx_bootCI_bytarget_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")
#----
pot_cpue_ftx %>%
  #filter(trip_set_targets == "all_Sablefish") %>%
  group_by(year,trip_recorded_releases) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> plot_boot2 #view(plot_boot2)

ggplot(plot_boot2) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper, fill = trip_recorded_releases), 
              #             alpha = 0.1, fill = "grey55") +
              alpha = 0.1) +
  geom_point(aes(x = year, y = Mean, col = trip_recorded_releases), size = 1) +
  geom_line(aes(x = year, y = Mean, col = trip_recorded_releases)) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Fishery CPUE (round lb per pot)\n") +
  lims(y = c(0, 125))

ggsave(paste0(YEAR+1,"/figures/pot_cpue_ftx_bootCI_byrelease_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

#---- Depredation... not recorded in 2022
{depr_eff<-lm(data=ll_cpue_ftx, std_cpue ~ p_sets_depredated)
summary(depr_eff); plot(depr_eff)
plot(data=ll_cpue_ftx, std_cpue ~ p_sets_depredated)
abline(depr_eff)

ll_cpue_ftx %>%
  filter(trip_set_targets == "all_Sablefish") %>%
  group_by(year,Depr_sum) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> plot_boot3 #view(plot_boot2)

ggplot(plot_boot3) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper, fill = Depr_sum), 
              #             alpha = 0.1, fill = "grey55") +
              alpha = 0.1) +
  geom_point(aes(x = year, y = Mean, col = Depr_sum), size = 1) +
  geom_line(aes(x = year, y = Mean, col = Depr_sum)) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") +
  lims(y = c(0, 2))

ggsave(paste0(YEAR+1,"/figures/fshcpue_ftx_bootCI_bydepr_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")}

pot_cpue_ftx %>%
  #filter(trip_set_targets == "all_Sablefish") %>%
  group_by(year,multigear_trip) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> plot_boot4 #view(plot_boot4)

ggplot(plot_boot4) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper, fill = multigear_trip), 
              #             alpha = 0.1, fill = "grey55") +
              alpha = 0.1) +
  geom_point(aes(x = year, y = Mean, col = multigear_trip), size = 1) +
  geom_line(aes(x = year, y = Mean, col = multigear_trip)) +
  geom_errorbar(aes(x=year, y=Mean,ymin=Lower,ymax=Upper, col = multigear_trip),
                position=position_dodge(width=0), width=0.1, size=0.2) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Fishery CPUE (round lb per pot)\n") +
  lims(y = c(0, 55))

ggsave(paste0(YEAR+1,"/figures/pot_cpue_ftx_bootCI_bygeartrip_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")
# Prelim works towards CPUE analysis for NSEI, mirroring what was done by Jenny
# Stahl and Ben Williams in SSEI

#time plots not necessary for first year, so we'll just save this as a table...
plot_boot1; plot_boot2; plot_boot4

Sum2023_ftx<-rbind(as.data.frame(plot_boot1) %>% mutate(category = "Overall total lbs/pot") %>% 
        dplyr::select(year,category, Mean, Lower_CI = Lower, Upper_CI = Upper),
      as.data.frame(plot_boot2) %>% mutate(category = trip_recorded_releases) %>% 
        select(year,category, Mean, Lower_CI = Lower, Upper_CI = Upper)#,
      #as.data.frame(plot_boot4) %>% mutate(category = multigear_trip) %>% 
      #  select(category, Mean, Lower_CI = Lower, Upper_CI = Upper)
      )

write_csv(Sum2023_ftx, paste0(YEAR+1,"/output/pot_nom_cpue_summary_",
                              max(pot_cpue_ftx$year), ".csv"))
#--------------------------------------------------------------------------------
# Normality

#for analysis we will use CPUE from trips that targetted only sablefish and that
# experienced no depredation... 
pot_cpue_ftx_clean <-pot_cpue_ftx %>% 
  filter(trip_set_targets == "all_Sablefish" #,
         #Depr_sum == "none"
         ) 
# Long right tail
ggplot(pot_cpue_ftx_clean, aes(std_cpue)) + geom_density(alpha = 0.6, fill = 4)
ggplot(pot_cpue_ftx_clean, aes(std_cpue)) + geom_histogram(alpha = 0.6, fill = 4)
# Better, but still not normal with log transformation
ggplot(pot_cpue_ftx_clean, aes(log(std_cpue + 0.1))) + geom_density(alpha = 0.4, fill = 4)
ggplot(pot_cpue_ftx_clean, aes(log(std_cpue + 0.1))) + geom_histogram(alpha = 0.4, fill = 4)
# Following Jenny Stahl and Ben Williams' work in the SSEI, increase CPUE by 10%
# of the mean per Cambell et al 1996 and Cambell 2004. Back-transform with
# exp(cpue - mean(fsh_cpue$std_cpue) * 0.1)
pot_cpue_ftx_clean %>% 
  mutate(cpue = log(std_cpue + (mean(pot_cpue_ftx_clean$std_cpue, na.rm=T) * 0.1))) -> pot_cpue_ftx_clean

ggplot(pot_cpue_ftx_clean, aes(cpue)) + geom_density(alpha = 0.4, fill = 4)
ggplot(pot_cpue_ftx_clean, aes(cpue)) + geom_histogram(alpha = 0.4, fill = 4)

view(pot_cpue_ftx_clean)
# We can roll with the fully transformed for now... not much data at this point
# so small sample sizes will curtail any true analysis at this phase... 

# Trends over time
ggplot(pot_cpue_ftx_clean, aes(Year, std_cpue)) + geom_boxplot()

# Trends over time by area
ggplot(pot_cpue_ftx_clean %>% 
         filter(Stat %in% c("345603", "345631", "345701", "345731")), aes(Stat, std_cpue, fill = Year)) + 
  geom_boxplot() +
  scale_fill_manual(values = rev(colorspace::sequential_hcl(c = 0, l = c(30, 90), 
                                                            power = c(1/5, 1.3), 
                                                            n_distinct(pot_cpue$year))),
                    #guide = FALSE
                    guide = "none") +
  labs(x = NULL, y = "Fishery CPUE (round pounds per pot)\n")

ggsave(paste0(YEAR+1,"/figures/pot_nom_cpue_trendsbyStat_",min(pot_cpue$year), "_", YEAR, ".png"), 
       dpi=400, height=4, width=7.5, units="in")

# No one fished in Fred. Sound in 2018, 2 in 2019, 4 in 2021
pot_cpue_ftx_clean %>% filter(Stat %in% c("345702", "335701") & year == YEAR) %>% distinct(Adfg)
# Activity in N Chatham 
pot_cpue_ftx_clean %>% filter(Stat %in% c("345731", "345803") & year == YEAR) %>% distinct(Adfg)
# Activity in S Chatham
pot_cpue_ftx_clean %>% filter(Stat %in% c("345603")) %>% group_by(year) %>%  dplyr::summarize(n_distinct(Adfg)) %>% View()
pot_cpue_ftx_clean %>% filter(Stat %in% c("335701")) %>% group_by(year) %>%  dplyr::summarize(n_distinct(Adfg)) %>% View()

pot_cpue_ftx_clean %>% 
  group_by(year, Stat) %>% 
  dplyr::summarize(trips = n_distinct(trip_no),
            vessels = n_distinct(Adfg)) -> stat_sum

# Gear performance by Stat
# right now there isn't any "Gear" like autobaiter in ll analysis.  Leaving this
# bracketed off for future use as the fishery develops.  
{ggplot(pot_cpue_ftx_clean, aes(Stat, cpue, fill = Gear)) + geom_boxplot()

# Gear performance over time
ggplot(ll_cpue_ftx_clean, aes(Year, cpue, fill = Gear)) + geom_boxplot() +
  theme(axis.text.x = element_text(size = 14, angle = 90, h = 1)) +
  labs(x = "", y = "Fishery CPUE\n")

# Only a handful of vessels with autobaiter gear
ggplot(ll_cpue_ftx_clean, aes(Adfg, cpue, color = Gear)) + geom_jitter(alpha=.4) +
  theme(axis.text.x = element_text(colour = "white"))}

#*******************************************************************************
#* STANDARDIZING CPUE by boat, stat area, gear spec, etc. 
#* This code was set up in 2023. Not modified in 2024 due to late arrival of data
#* This will be something to pursue as the pot fishery develops
#* *****************************************************************************
colnames(pot_cpue_ftx_clean)

pot_cpue_ftx_clean <- pot_cpue_ftx_clean %>%
  mutate(soak_p_set = trip_soak/no_sets)

# depth
ggplot(pot_cpue_ftx_clean, aes(trip_depth, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 1, se = FALSE) + 
  geom_smooth(size = 1, se = FALSE, method="gam", col="purple", 
              formula = y ~ poly(x,2)) + 
  geom_smooth(size = 1, se = FALSE, method="gam", col="violet", 
              formula = y ~ log(x))

#soak time
ggplot(pot_cpue_ftx_clean, aes(trip_soak, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 1, se = FALSE) + 
  geom_smooth(size = 1, se = FALSE, method="gam", col="purple", 
              formula = y ~ poly(x,2)) + 
  geom_smooth(size = 1, se = FALSE, method="gam", col="violet", 
              formula = y ~ log(x))
ggplot(pot_cpue_ftx_clean, aes(soak_p_set, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 1, se = FALSE) + 
  geom_smooth(size = 1, se = FALSE, method="gam", col="purple", 
              formula = y ~ poly(x,2)) + 
  geom_smooth(size = 1, se = FALSE, method="gam", col="violet", 
              formula = y ~ log(x))

#km fished (length)
ggplot(pot_cpue_ftx_clean, aes(total_km_fished, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 1, se = FALSE) + 
  geom_smooth(size = 1, se = FALSE, method="gam", col="purple", 
              formula = y ~ poly(x,2)) + 
  geom_smooth(size = 1, se = FALSE, method="gam", col="violet", 
              formula = y ~ log(x))
#pot spacing
ggplot(pot_cpue_ftx_clean, aes(pot_space, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 1, se = FALSE) + 
  geom_smooth(size = 1, se = FALSE, method="gam", col="purple", 
              formula = y ~ poly(x,2)) + 
  geom_smooth(size = 1, se = FALSE, method="gam", col="violet", 
              formula = y ~ log(x))
ggplot(pot_cpue_ftx_clean, aes(as.factor(pot_space), cpue)) + geom_boxplot()
ggplot(pot_cpue_ftx_clean, aes(Year, cpue, fill = as.factor(pot_space))) + geom_boxplot()+
  theme(axis.text.x = element_text(size = 14, angle = 90, h = 1)) +
  labs(x = "", y = "Fishery CPUE\n")
ggplot(pot_cpue_ftx_clean, aes(Stat, cpue, fill = as.factor(pot_space))) + geom_boxplot()+
  labs(x = "\nStat area", y = "Fishery CPUE\n")

#ground line diameter
ggplot(pot_cpue_ftx_clean, aes(line_diam, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 1, se = FALSE) + 
  geom_smooth(size = 1, se = FALSE, method="gam", col="purple", 
              formula = y ~ poly(x,2)) + 
  geom_smooth(size = 1, se = FALSE, method="gam", col="violet", 
              formula = y ~ log(x))
ggplot(pot_cpue_ftx_clean, aes(as.factor(line_diam), cpue)) + geom_boxplot()
ggplot(pot_cpue_ftx_clean, aes(Year, cpue, fill = as.factor(line_diam))) + geom_boxplot()+
  theme(axis.text.x = element_text(size = 14, angle = 90, h = 1)) +
  labs(x = "", y = "Fishery CPUE\n")
ggplot(pot_cpue_ftx_clean, aes(Stat, cpue, fill = as.factor(line_diam))) + geom_boxplot()+
  labs(x = "\nStat area", y = "Fishery CPUE\n")

#pot dimensions as factor
ggplot(pot_cpue_ftx_clean, aes(as.factor(pot_dim), cpue)) + geom_boxplot()
ggplot(pot_cpue_ftx_clean, aes(Year, cpue, fill = as.factor(pot_dim))) + geom_boxplot()+
  theme(axis.text.x = element_text(size = 14, angle = 90, h = 1)) +
  labs(x = "", y = "Fishery CPUE\n")
ggplot(pot_cpue_ftx_clean, aes(Stat, cpue, fill = as.factor(pot_dim))) + geom_boxplot()+
  labs(x = "\nStat area", y = "Fishery CPUE\n")

# pot type... if more than slinky
unique(pot_cpue_ftx_clean$pot_type)

# pot length
ggplot(pot_cpue_ftx_clean, aes(pot_len, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 1, se = FALSE) + 
  geom_smooth(size = 1, se = FALSE, method="gam", col="purple", 
              formula = y ~ poly(x,2)) + 
  geom_smooth(size = 1, se = FALSE, method="gam", col="violet", 
              formula = y ~ log(x))
# pot diameter
ggplot(pot_cpue_ftx_clean, aes(pot_diam, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 1, se = FALSE) + 
  geom_smooth(size = 1, se = FALSE, method="gam", col="purple", 
              formula = y ~ poly(x,2)) + 
  geom_smooth(size = 1, se = FALSE, method="gam", col="violet", 
              formula = y ~ log(x))
# pot volume
ggplot(pot_cpue_ftx_clean, aes(pot_volume_m3, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 1, se = FALSE) + 
  geom_smooth(size = 1, se = FALSE, method="gam", col="purple", 
              formula = y ~ poly(x,2)) + 
  geom_smooth(size = 1, se = FALSE, method="gam", col="violet", 
              formula = y ~ log(x))

# 2023: First look, so not much to see.  We'll set up the gams for standardizing 
# in the future as the fishery develops... 

#Cant' 

m0 <- bam(cpue ~ year , data=pot_cpue_ftx_clean, gamma=1.4)

m0.depth <- bam(cpue ~ year + s(trip_depth, k=4), data=pot_cpue_ftx_clean, gamma=1.4)
m0.soak <- bam(cpue ~ year + s(soak_p_set, k=4) , data=pot_cpue_ftx_clean, gamma=1.4)
m0.stat <- bam(cpue ~ year + s(Stat, bs='re', by=dumstat), data=pot_cpue_ftx_clean, gamma=1.4)
m0.adfg <- bam(cpue ~ year + s(Adfg, bs='re', by=dum), data=pot_cpue_ftx_clean, gamma=1.4)
m0.jday <- bam(cpue ~ year + s(julian_day_sell, k=4), data=pot_cpue_ftx_clean, gamma=1.4)
m0.length <- bam(cpue ~ year + s(total_km_fished), data=pot_cpue_ftx_clean, gamma=1.4)
m0.pot_space <- bam(cpue ~ year + s(total_km_fished), data=pot_cpue_ftx_clean, gamma=1.4)
m0.line_diam <- bam(cpue ~ year + s(line_diam, k=4), data=pot_cpue_ftx_clean, gamma=1.4)
m0.pot_dim <- bam(cpue ~ year + pot_dim, data=pot_cpue_ftx_clean, gamma=1.4)
m0.pot_len <- bam(cpue ~ year + s(pot_len, k=4), data=pot_cpue_ftx_clean, gamma=1.4)
m0.pot_diam <- bam(cpue ~ year + s(pot_diam, k=4), data=pot_cpue_ftx_clean, gamma=1.4)
m0.pot_vol <- bam(cpue ~ year + s(pot_volume_m3, k=4), data=pot_cpue_ftx_clean, gamma=1.4)


model.list<-list(m0,m0.depth,m0.soak,m0.stat,m0.adfg,
                 m0.jday,m0.length, m0.pot_space, m0.line_diam, m0.pot_dim, 
                 m0.pot_len, m0.pot_diam, m0.pot_vol)
names(model.list)<-c("m0","depth","soak","stat","adfg",#
                     "jday","length","spacing","line_diam","pot_dim",
                     "pot_len","pot_diam","pot_vol")
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

#Too few data points to run a "global" model, so stopping here for 2023
# AS fishery develops and need to standardize comes about we can develop this... 
# Develop the bigger models following the ll blueprint... 

global<-bam(cpue ~ year + s(trip_depth, k=4) + s(soak_p_set, k=4) + 
              s(Stat, bs='re', by=dumstat) + s(Adfg, bs='re', by=dum) + 
              s(julian_day_sell, k=4) + s(total_km_fished, k=4) + s(total_km_fished) + 
              s(line_diam, k=4) + pot_dim + s(pot_len, k=4) + s(pot_diam, k=4) + 
              s(pot_volume_m3, k=4), data=pot_cpue_ftx_clean, gamma=1.4)

AIC(global) #way better than simple models

plot(global, page = 1, shade = TRUE, resid = TRUE, all = TRUE)
summary(global)

# No residual patterns, but may be some outliers
plot(fitted(global), resid(global))
abline(h = 0, col = "red", lty = 2)

# 14 outliers, get rid of them and refit models with new data set
which(fitted(global) < -1.5)   #6 outliers in 2023
not_outliers <- which(fitted(global) >= -1.5)
fsh_cpue_cl <- fsh_cpue_cl %>% 
  slice(not_outliers)

vcov.gam(global)

#-------------------------------------------------------------------------------
# Get data for the model... if it every goes in the model before I retire...

pot_cpue_ftx_clean %>% 
  group_by(year) %>% 
  dplyr::summarise(fsh_cpue = mean(std_cpue),
                   sd = sd(std_cpue),
                   n = length(std_cpue),
                   se = sd / (n ^ (1/2)),
                   var = var(std_cpue),
                   cv = sd / fsh_cpue,
                   upper = fsh_cpue + (2 * se),
                   lower = fsh_cpue - (2 * se)) -> fsh_sum 

# Compare predicted cpue from gam to nominal cpue
fsh_sum %>%
  select(year, cpue = fsh_cpue, upper, lower) %>% 
  mutate(CPUE = "Nominal") %>% 
  bind_rows(std_dat %>% 
              select(year, cpue = bt_cpue, upper = bt_upper, lower = bt_lower) %>% 
              mutate(CPUE = "Fully Standardized")) %>% 
  ggplot() +
  geom_ribbon(aes(year, ymin = lower, ymax = upper, fill = CPUE), 
              colour = "white", alpha = 0.2) +
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


# Percent change in fishery nominal cpue compared to a ten year rolling average
fsh_sum %>% 
  filter(year > YEAR - 10) %>% 
  mutate(lt_mean = mean(fsh_cpue),
         perc_change_lt = (fsh_cpue - lt_mean) / lt_mean * 100) 

std_dat %>% 
  filter(year > YEAR - 10) %>% 
  mutate(lt_mean = mean(bt_cpue),
         perc_change_lt = (bt_cpue - lt_mean) / lt_mean * 100) 

# Percent change in fishery nominal cpue from last year
#not relevant since no CPUE in 2020 due to stupid covid
fsh_sum %>% 
  filter(year >= YEAR - 1) %>%
  select(year, fsh_cpue) %>% 
  reshape2::dcast("fsh_cpue" ~ year) -> perc_ch; as.numeric(100*(perc_ch[3]-perc_ch[2])/perc_ch[2])

std_dat %>% 
  filter(year >= YEAR - 1) %>%
  select(year, bt_cpue) %>% 
  reshape2::dcast("bt_cpue" ~ year) -> std_perc_ch; as.numeric(100*(std_perc_ch[3]-std_perc_ch[2])/std_perc_ch[2])

names(perc_ch) <- c("cpue", "last_year", "this_year") 
perc_ch %>% mutate(perc_change_ly = (`this_year` - `last_year`) / `last_year` * 100)

names(std_perc_ch) <- c("cpue", "last_year", "this_year") 
std_perc_ch %>% mutate(perc_change_ly = (`this_year` - `last_year`) / `last_year` * 100)

#data.frame(year = 1980:1996,
#           fsh_cpue = hist_cpue) %>% 
#  mutate(var = (fsh_cpue * mean(fsh_sum$cv)) ^ 2,
#         se = mean(fsh_sum$se),
#         upper = hist_cpue+(2*se),
#         lower = hist_cpue-(2*se),
#         CPUE = "Nominal") %>% 
fsh_sum %>% mutate(CPUE = "Nominal") %>%
              select(year, fsh_cpue, var, se, upper, lower, CPUE) %>% 
  mutate(cpue = round(fsh_cpue, 3),
         var = round(var, 3)) -> nom_cpue_ts

#data.frame(year = 1980:1996,
#           fsh_cpue = hist_cpue) %>% 
#  mutate(var = sqrt(mean(std_dat$bt_se)),
#         se = mean(std_dat$bt_se),
#         upper = hist_cpue+(2*se),
#         lower = hist_cpue-(2*se),
#         CPUE = "Fully Standardized") %>% 
#  bind_rows(std_dat %>% 
#              mutate(var = sqrt(bt_se)) %>% 
#              select(year, fsh_cpue = bt_cpue, se = bt_se, var,
#                     upper = bt_upper, lower = bt_lower) %>% 
#              mutate(CPUE = "Fully Standardized")) %>% 
#  mutate(cpue = round(fsh_cpue, 3),
#         var = round(var, 3)) %>% data.frame() -> glob_cpue_ts

#cpue_ts_multi<-rbind(nom_cpue_ts,glob_cpue_ts)

#cpue_ts_short <- glob_cpue_ts %>% 
#  filter(year >= 1997)

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

ggplot(cpue_ts_multi) +
  geom_point(aes(year, cpue, col = CPUE)) +
  geom_line(aes(year, cpue, col=CPUE)) +
  geom_ribbon(aes(year, ymin = cpue - sqrt(var), ymax = cpue + sqrt(var), fill = CPUE),
              alpha = 0.2) +
  geom_ribbon(aes(year, ymin = lower, ymax = upper, fill = CPUE),
              # geom_ribbon(aes(year, ymin = cpue - var, ymax = cpue + var),
              alpha = 0.4) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  #lims(y = c(0, 1.5)) +
  #lims(y = c(-0.5, 1.5)) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") 

#NOTE2022: much more variance in data now... could easily run a straight line
# through error polygon = lack of information in this data!!! 

ggsave(paste0(YEAR+1,"/figures/ll_cpue_nom_m12_1980_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")
# Write to file

write_csv(nom_cpue_ts, paste0(YEAR+1,"/output/pot_cpue_nom_", min(nom_cpue_ts$year), "_", YEAR, ".csv"))

glob_cpue_ts<-as.matrix(m12_cpue_ts)
glob_cpue_ts[,c(1,2,3,4,5,6,8)]<-as.numeric(glob_cpue_ts[,c(1,2,3,4,5,6,8)])
glob_cpue_ts<-as.data.frame(m12_cpue_ts)

write_csv(glob_cpue_ts, paste0(YEAR+1,"/output/ll_cpue_m12_", min(nom_cpue_ts$year), "_", YEAR, ".csv"))


#--------------------------------------------------------------------------------
# CPUE from LOGBOOKS:
#*******************************************************************************
#*FLAG!!!!!!!!
#* The code below that calculates CPUE from logbooks still has a minor bug in it
#* and as of 2023 CPUE should be calculated based on fish tickets.  The problem is that
#* logbook CPUE is slightly lower than fish tickets even though I tried to add
#* in release information to calculate a more accurate cpue.  The code is accurate 
#* EXCEPT when skippers released an entire set and/or recorded these releases in 
#* different units (nos/lbs) from those they kept.  I had planned to try yet another
#* change to the code, but am giving up for now in the interest in completing the
#* assessment.  This is another example of poor data collection and curration that
#* makes this assessment extremely tedious and difficult.  Until clean and readily
#* accessible data is available for this fishery this will project will be a slog.

# CPUE based on skipper's LOGBOOK data...
# Fishticket data is at the trip level and thus averages out the set specific performance.
# This is generally necessary because logbook entries are estimates 
#*******************************************************************************
#--------------------------------------------------------------------------------
# CPUE based on skipper's logbook data...
# Fishticket data is at the trip level and thus averages out the set specific performance.
# This is generally necessary because logbook entries are estimates 
nrow(unique(pot_cpue))

pot_cpue_logged <-pot_cpue
nrow(pot_cpue_logged)
colnames(pot_cpue_logged)

nrow(pot_cpue_logged %>% filter (!is.na(logged_no)))
nrow(pot_cpue_logged %>% filter (!is.na(logged_lbs)))

releases<-pot_cpue_logged %>% filter(trip_recorded_releases == "Retained_&_Released")
random_check(releases)
unique(pot_cpue_logged$disposition)

old <- Sys.time()
pot_cpue_logged %>% group_by(year, sell_date, Adfg, Stat, disposition) %>%
  #filter (disposition == "no_logged_releases" | is.na(disposition)) %>% #need to exclude the releases... will look at later?
  mutate(logged_and_landed = case_when((disposition == "Retained"| is.na(disposition)) ~ sum(logged_lbs)),
         logged_and_landed_nos = case_when((disposition == "Retained"| is.na(disposition)) ~ sum(logged_no)),
         logged_and_released = case_when((disposition == "Released (Quota Limit)"| disposition == "Released") ~ sum(logged_lbs)),
         logged_and_released_nos = case_when((disposition == "Released (Quota Limit)"| disposition == "Released") ~ sum(logged_no))) %>%
  #mutate(logged_and_landed = if((disposition == "Retained"| is.na(disposition)) sum(logged_lbs))) %>% 
  ungroup () %>% 
  group_by(year, sell_date, Adfg, Stat) %>%
  mutate(adj_logged_lbs = case_when((disposition == "Retained"| is.na(disposition)) ~ 
                                      (logged_lbs/logged_and_landed)*catch),
         est_released_lbs = case_when((disposition == "Released (Quota Limit)"| disposition == "Released") ~ 
                                      (logged_lbs/sum(unique(logged_and_landed), na.rm=T))*unique(catch)),
         #skipper_bias_bytrip = ,
         #skipper_bias_bytrip = sum(unique(logged_and_landed_nos), na.rm=T)-unique(catch), 
         adj_logged_nos = case_when((disposition == "Retained"| is.na(disposition)) ~ 
                                      (logged_no/logged_and_landed_nos)*catch),
         est_rel_lbs_nos = case_when((disposition == "Released (Quota Limit)"| disposition == "Released") ~ 
                                      (logged_no/sum(unique(logged_and_landed_nos), na.rm=T))*unique(catch))
         ) %>% 
  ungroup() -> test; Sys.time() - old #%>%  #12 minutes for this piece!!!


old <- Sys.time()
test %>% group_by(year, sell_date, Adfg, Stat, set_no) %>%
  
  filter(#multi_gear_config == "single_config" &   #get rid of trips that reported 2 gear configurations
           trip_set_targets == "all_Sablefish",   # only use trips that were dedicated to sablefish... above anlysis says this is smart
           !is.na(sell_date) & 
           #!is.na(mean_hook_spacing) & #!is.na(hook_space) & 
           !is.na(sable_lbs_set) &
           # !is.na(start_lon) & !is.na(start_lon) & #remove this because this is at the set level... 
           !is.na(trip_soak) & !is.na(trip_depth) &
           #!is.na(mean_hook_size) &   #!is.na(hook_size) &
           #hook_size != "MIX" &
           trip_soak > 0 & !is.na(trip_soak) & # soak time in hrs
           julian_day_sell > 226 & # if there were special projects before the fishery opened
           #no_hooks_p_set < 15000 & # 15000 in Kray's scripts - 14370 is the 75th percentile
           Stat %in% c("345603", "345631", "345702",
                       "335701", "345701", "345731", "345803")) %>% 
  
  mutate(Year = factor(year), 
         #Gear = factor(gear),
         Adfg = factor(Adfg),
         Stat = factor(Stat),
         Stat = fct_relevel(Stat,
                            c("345702", "335701", # Frederick Sound
                              # Chatham south to north
                              "345603", "345631", "345701", "345731", "345803")),
         #Depr_sum = ifelse(p_sets_depredated == 0, "none",
        #                   ifelse(p_sets_depredated == 1, "all sets", 
        #                          ifelse(p_sets_depredated > 0 & p_sets_depredated <= 0.25,
        #                                 "0-25% of sets",
        #                                 ifelse(p_sets_depredated > 0.25 & p_sets_depredated <= 0.5,
        #                                        "25-50% of sets",
        #                                        ifelse(p_sets_depredated > 0.5 & p_sets_depredated <= 0.75,
        #                                               "50-75% of sets","75-100%"))))),
         # 01=Conventional, 02=Snap On, 05=Mixed, 06=Autobaiter -> 01, 02, 05
         # show no strong differences. main difference with autobaiters, which
         # have lwr cpue than conventional gear
         #Gear = derivedFactor("AB" = Gear == "6",
        #                    "CS" = Gear %in% c("1","2","5")),
         #Hook_size = factor(hook_size),  #might be worth treating as numeric? 
         #Mean_hook_size = mean_size, 
         #Need to calculate corrected set landing and flag those sets where skippers
         #recorded releases and retentions in different formats (ie, numbers for one 
         # lbs for the other)
         set_catch_flag = ifelse(Inf %in% est_released_lbs,
                                 "CPUE underestimated",
                                 ifelse(Inf %in% est_rel_lbs_nos,
                                 "CPUE underestimated",
                                 "good")),
         
         #log_data = "fuckitfornow",
         log_data_form = ifelse(length(unique(is.na(logged_no))) > 1,
                           "mixed",
                           ifelse(unique(is.na(logged_no)),"lbs","nos")),
         #log_data = ifelse(unique(is.na(logged_no)) & length(unique(is.na(logged_no))) == 1,
          #                 "nos",
          #                 ifelse(unique(is.na(logged_lbs)) & length(unique(is.na(logged_lbs))) == 1,
          #                        "lbs","mixed")),
                               
         set_catch_no_releases = sum(adj_logged_lbs, na.rm=TRUE)+sum(adj_logged_nos, na.rm=TRUE),
         set_catch_w_releases = sum(adj_logged_lbs, na.rm=TRUE)+sum(adj_logged_nos, na.rm=TRUE) +
                              sum(est_released_lbs, na.rm=TRUE)+sum(est_rel_lbs_nos, na.rm=TRUE),
         set_catch = set_catch_w_releases, # ifelse("good" %in% set_catch_flag,
                          #  ifelse(trip_recorded_releases == "no_logged_releases",
                          #         as.numeric(set_catch_no_releases),
                          #       as.numeric(set_catch_w_releases)),
                          #  as.numeric(set_catch_w_releases)),
         set_releases = set_catch_w_releases - set_catch_no_releases,
         prop_released = set_releases/(set_catch),
         skipper_bias_bytrip = 100*(logged_and_landed - catch)/catch,
         # standardize hook spacing (Sigler & Lunsford 2001, CJFAS), 1 m = 39.37 in
         #  std_hooks = 2.2 * no_hooks_p_set * (1 - exp(-0.57 * (mean_hook_spacing / 39.37))),
         std_cpue = set_catch / no_pots_p_set,
         std_cpue_lnd = set_catch_no_releases/no_pots_p_set, 
         
         # dummy varbs, for prediction with random effects
         dum = 1, 
         dumstat = 1) %>% 
  
  ungroup() %>%
  
  group_by(year) %>% 
  mutate(
    #The number of vessels participating in the fishery has descreased by 50% from
    #1997-2015. create new column is the total number of active vessels
    #participating in a given year
    total_vessels = n_distinct(Adfg),
    # Total unique trips per year
    total_trips = n_distinct(trip_no)) %>% 
  ungroup() %>% 
  select(year = Year, sell_date, set_date, julian_day_set, julian_day_sell, trip_no, 
         Adfg, Spp_cde, set_soak, set_length, #Gear, 
         gear_name, 
         pot_data, pot_space, line_diam, pot_dim, pot_type, pot_len, pot_diam,
         pot_volume_m3, no_pots_fished_on_trip, no_pots_p_set,            
         no_pots_p_set, 
         Stat, set_depth, set_no, no_sets, multigear_trip,
         trip_set_targets, trip_recorded_releases, #multi_gear_config, 
         disposition,
         #set_depredation, trip_depredation, p_sets_depredated, 
         catch, logged_no,
         logged_lbs, sable_lbs_set, start_lat, start_lon, set_target, #set_target_2,
         trip_target, #trip_target_2, 
         logged_and_landed, logged_and_landed_nos,
         logged_and_released, logged_and_released_nos, adj_logged_lbs, est_released_lbs,
         adj_logged_nos, est_rel_lbs_nos, #Depr_sum, 
         #Hook_size, Mean_hook_size,
         set_catch_flag, log_data_form, 
         set_landings = set_catch_no_releases, 
         total_set_catch = set_catch_w_releases,
         set_releases, prop_released,
         #std_hooks, 
         std_cpue,std_cpue_lnd,skipper_bias_bytrip, dum, dumstat, total_vessels, total_trips) %>%
  filter(disposition == "Retained", #release info now in columns so can get rid of those records
         total_set_catch != Inf) -> pot_cpue_log #getting rid of those mixed (no/lbs) logbook entries... 
nrow(pot_cpue_log)
nrow(unique(pot_cpue_log))

oi<-random_check(pot_cpue_log %>% filter(trip_recorded_releases == "Retained_&_Released"))
oi %>% filter(set_no == 1)
sum(oi$logged_lbs)*4
sum(oi$set_landings)
mean(oi$std_cpue)
mean(oi$std_cpue_lnd)

colnames(pot_cpue_log)
histogram(pot_cpue_log$std_cpue, breaks=100); min(pot_cpue_log$std_cpue)
nrow(pot_cpue_log %>% filter(std_cpue == min(pot_cpue_log$std_cpue)))

random_check(pot_cpue_log %>% filter(std_cpue == min(pot_cpue_log$std_cpue)))

pot_cpue_log %>% filter(sell_date == "2022-10-24",
                        Adfg == 45454) %>% data.frame() -> check
sum(check$adj_logged_lbs); sum(unique(check$logged_lbs))
#OK... this is happening when skippers logged some landings in lbs and some in numbers
# not many... 61 records, so lets purge... 
#nrow(ll_cpue_log)
#ll_cpue_log <- ll_cpue_log %>% filter(std_cpue > min(ll_cpue_log$std_cpue))
#nrow(ll_cpue_log)

sel<-sample(nrow(ll_cpue_log),1)# eg16473
eg<-as.data.frame(ll_cpue_log[ll_cpue_log$sell_date == ll_cpue_log$sell_date[sel] &
                            ll_cpue_log$Adfg == ll_cpue_log$Adfg[sel],])
#need to colapse duplicate columns with release data now that its captured
eg
eg %>% filter(disposition == "Retained")
unique(eg %>% filter(disposition == "Retained"))

#--------------------------------------------------------------------------------
colnames(pot_cpue_log)
unique(pot_cpue_log$trip_set_targets)

mean(pot_cpue_log$std_cpue)
mean(pot_cpue_ftx$std_cpue)
plot(density(pot_cpue_ftx$std_cpue)); lines(density(pot_cpue_log$std_cpue), col="blue")

checks<-data.frame()
for (i in 1:nrow(pot_cpue_ftx)) {
  arg<- pot_cpue_ftx[i,] %>% data.frame()
  oi<-pot_cpue_log %>% 
    filter(julian_day_sell == pot_cpue_ftx[i,]$julian_day_sell,
           Adfg == pot_cpue_ftx[i,]$Adfg, 
           Stat == pot_cpue_ftx[i,]$Stat) %>% 
    data.frame()
  checks[i,"row.no"]<-i
  checks[i,"ftx_cpue"]<-arg$std_cpue
  checks[i,"log_cpue"]<-mean(oi$std_cpue)
  checks[i,"log_cpue_lnd"]<-mean(oi$std_cpue_lnd)
  checks[i,"dif"]<-checks[i,"log_cpue"]-checks[i,"ftx_cpue"]
  checks[i,"dif2"]<-checks[i,"log_cpue_lnd"]-checks[i,"ftx_cpue"]
  fuckme<-test %>% filter(julian_day_sell == pot_cpue_ftx[i,]$julian_day_sell,
                          Adfg == pot_cpue_ftx[i,]$Adfg, 
                          Stat == pot_cpue_ftx[i,]$Stat) %>% 
    data.frame()
  checks
}

eg<-20
pot_cpue_ftx[eg,] %>% data.frame()
pot_cpue_log %>% 
  filter(julian_day_sell == pot_cpue_ftx[eg,]$julian_day_sell,
         Adfg == pot_cpue_ftx[eg,]$Adfg, 
         Stat == pot_cpue_ftx[eg,]$Stat) %>% 
  data.frame() ->ch
pot_cpue_ftx[eg,]$std_cpue; mean(ch$std_cpue); mean(ch$std_cpue_lnd)

gr<-test %>% filter(julian_day_sell == pot_cpue_ftx[eg,]$julian_day_sell,
                    Adfg == pot_cpue_ftx[eg,]$Adfg, 
                    Stat == pot_cpue_ftx[eg,]$Stat) %>% 
  data.frame()

###EUREKA MOMENT. FUCKING HELL.  Lower CPUE from when they release an entire set!!!
# happens with the quote limit.. hit quota and dump the entire next set... 
## wrapped up in no_pots (or hooks) fished on the trip... 
## some of the sable_lbs_set are alocated to the set that was dumped... fucking hell. 
## So, need to adjust how logged_lbs are being allocated when this situation arrises? 
## in this example, set_no 6 is getting booted? Because it's a mix report (no and lbs) and
## Fuck it?  Just do CPUE through fish tickets and logbooks be damned until someone
## finally gets the data collection for this project in order... 

pot_cpue_log %>% filter(trip_target == "Sablefish") %>%
  group_by(year) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> plot_boot6

rbind(as.data.frame(plot_boot1 %>% filter(trip_set_targets == "all_Sablefish") %>%
        mutate(CPUE_calc_derivation = "fish tickets")),
      as.data.frame(plot_boot6 %>% mutate(CPUE_calc_derivation = "adj. log books",
                            trip_set_targets = NA) %>%
        select(year, trip_set_targets, Mean, Lower, Upper, CPUE_calc_derivation))) -> plot_boot6

ggplot(plot_boot6) +
  geom_ribbon(aes(x = as.numeric(year), ymin = Lower, ymax = Upper, fill=CPUE_calc_derivation), 
  #                         alpha = 0.1, fill = "grey55") +
              alpha = 0.1) +
  geom_point(aes(x = as.numeric(year), y = Mean, col=CPUE_calc_derivation), size = 1) +
  geom_line(aes(x = as.numeric(year), y = Mean, col=CPUE_calc_derivation)) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") +
  lims(y = c(0, 50)); view(plot_boot6)

ggsave(paste0(YEAR+1,"/figures/fshcpue_ftx_bootCI_bytarget_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")
#----
pot_cpue_log %>% filter(trip_target == "Sablefish") %>%
  group_by(year,trip_recorded_releases) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> plot_boot7 #view(plot_boot7)

rbind(as.data.frame(plot_boot2 %>% #filter(trip_set_targets == "all_Sablefish") %>%
                      mutate(CPUE_calc_derivation = "fish tickets")),
      as.data.frame(plot_boot7 %>% mutate(CPUE_calc_derivation = "adj. log books") %>%
                      select(year, Mean, Lower, Upper, trip_recorded_releases, CPUE_calc_derivation))) -> plot_boot7

ggplot(plot_boot7) +
  geom_ribbon(aes(x = as.numeric(year), ymin = Lower, ymax = Upper, fill = CPUE_calc_derivation), 
              #             alpha = 0.1, fill = "grey55") +
              alpha = 0.1) +
  geom_point(aes(x = as.numeric(year), y = Mean, col = CPUE_calc_derivation), size = 1) +
  geom_line(aes(x = as.numeric(year), y = Mean, col = CPUE_calc_derivation)) +
  facet_wrap(~trip_recorded_releases) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") +
  lims(y = c(0, 60))

ggsave(paste0(YEAR+1,"/figures/fshcpue_ftx_bootCI_byrelease_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

#---- depredation
{depr_eff<-lm(data=ll_cpue_ftx, std_cpue ~ p_sets_depredated)
summary(depr_eff); plot(depr_eff)
plot(data=ll_cpue_ftx, std_cpue ~ p_sets_depredated)
abline(depr_eff)

ll_cpue_log %>% filter(trip_target == "Sablefish") %>%
  group_by(year,set_depredation) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> plot_boot8 #view(plot_boot2)

ggplot(plot_boot8) +
  geom_ribbon(aes(x = as.numeric(year), ymin = Lower, ymax = Upper, fill = set_depredation), 
              #             alpha = 0.1, fill = "grey55") +
              alpha = 0.1) +
  geom_point(aes(x = as.numeric(year), y = Mean, col = set_depredation), size = 1) +
  geom_line(aes(x = as.numeric(year), y = Mean, col = set_depredation)) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") +
  lims(y = c(0, 2))}

ggsave(paste0(YEAR+1,"/figures/fshcpue_ftx_bootCI_bydepr_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

pot_cpue_log %>% filter(trip_target == "Sablefish") %>%
  group_by(year,multigear_trip) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> plot_boot9 #view(plot_boot4)

ggplot(plot_boot9) +
  geom_ribbon(aes(x = as.numeric(year), ymin = Lower, ymax = Upper, fill = multigear_trip), 
              #             alpha = 0.1, fill = "grey55") +
              alpha = 0.1) +
  geom_point(aes(x = as.numeric(year), y = Mean, col = multigear_trip), size = 1) +
  geom_line(aes(x = as.numeric(year), y = Mean, col = multigear_trip)) +
  geom_errorbar(aes(x=as.numeric(year), y=Mean,ymin=Lower,ymax=Upper, col = multigear_trip),
                position=position_dodge(width=0), width=0.5, size=0.2) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") +
  lims(y = c(0, 70))

ggsave(paste0(YEAR+1,"/figures/fshcpue_ftx_bootCI_bygeartrip_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

Sum2022_log<-rbind(as.data.frame(plot_boot6) %>% mutate(category = "Overall total lbs/pot") %>% 
                     dplyr::select(category, Mean, Lower_CI = Lower, Upper_CI = Upper, -trip_set_targets),
                   as.data.frame(plot_boot7) %>% mutate(category = trip_recorded_releases) %>% 
                     select(category, Mean, Lower_CI = Lower, Upper_CI = Upper),
                   as.data.frame(plot_boot9) %>% mutate(category = multigear_trip) %>% 
                     select(category, Mean, Lower_CI = Lower, Upper_CI = Upper))
random_check(pot_cpue_log)
# Normality

#for analysis we will use CPUE from trips that targetted only sablefish and that
# experienced no depredation... 
pot_cpue_log_clean <-pot_cpue_log %>% 
  filter(trip_set_targets == "all_Sablefish" #,
         #set_depredation == "No depredation"
         )
# Long right tail
ggplot(pot_cpue_log_clean, aes(std_cpue)) + geom_density(alpha = 0.4, fill = 4)

# Better, but still not normal with log transformation
ggplot(pot_cpue_log_clean, aes(log(std_cpue + 1))) + geom_density(alpha = 0.4, fill = 4)

# Following Jenny Stahl and Ben Williams' work in the SSEI, increase CPUE by 10%
# of the mean per Cambell et al 1996 and Cambell 2004. Back-transform with
# exp(cpue - mean(fsh_cpue$std_cpue) * 0.1)
pot_cpue_log_clean %>% 
  mutate(cpue = log(std_cpue + (mean(pot_cpue_log_clean$std_cpue, na.rm=T) * 0.1))) -> pot_cpue_log_clean

ggplot(pot_cpue_log_clean, aes(cpue)) + geom_density(alpha = 0.4, fill = 4)

# EDA for GAM 

# Trends over time
ggplot(ll_cpue_log_clean, aes(year, std_cpue)) + geom_boxplot()

# Trends over time by area
ggplot(pot_cpue_log_clean %>% 
         filter(Stat %in% c("345603", "345631", "345701", "345731")), aes(Stat, std_cpue, fill = year)) + 
  geom_boxplot() +
  scale_fill_manual(values = rev(colorspace::sequential_hcl(c = 0, l = c(30, 90), 
                                                            power = c(1/5, 1.3), 
                                                            n_distinct(pot_cpue$year))),
                    guide = FALSE) +
  labs(x = NULL, y = "Fishery CPUE (round pounds per pot)\n")

ggsave(paste0(YEAR+1,"/figures/potcpue_trendsbyStat_22reboot_",min(pot_cpue$year), "_", YEAR, ".png"), 
       dpi=400, height=4, width=7.5, units="in")

#2022: Note that 2020 and 2021 have some very high CPUE outliers with new query from Justin...
#or... with recruitment of '13-'16 year classes there is far more variability based on discard
# behavior as some fisherman clean up on small fish (but most opt not to); could check the outlier
# vessel if there were lengths... but just poundage... 
# after review and conversation, outliers belonged to 2nd gen knowledgable and "honest" fisherman
# and data is considered legit.  Furthermore, 2022 reboot shows high cpues in the past now so
# seems to be OK... 

# No one fished in Fred. Sound in 2018, 2 in 2019, 4 in 2021
pot_cpue_log_clean %>% filter(Stat %in% c("345702", "335701") & year == YEAR) %>% distinct(Adfg)
# Activity in N Chatham 
pot_cpue_log_clean %>% filter(Stat %in% c("345731", "345803") & year == YEAR) %>% distinct(Adfg)
# Activity in S Chatham
pot_cpue_log_clean %>% filter(Stat %in% c("345603")) %>% group_by(year) %>%  dplyr::summarize(n_distinct(Adfg)) %>% View()
pot_cpue_log_clean %>% filter(Stat %in% c("335701")) %>% group_by(year) %>%  dplyr::summarize(n_distinct(Adfg)) %>% View()

pot_cpue_log_clean %>% 
  group_by(year, Stat) %>% 
  dplyr::summarize(trips = n_distinct(trip_no),
                   vessels = n_distinct(trip_no)) -> stat_sum2

# Gear performance by Stat
{ggplot(pot_cpue_log_clean, aes(Stat, cpue, fill = Gear)) + geom_boxplot()

# Gear performance over time
ggplot(ll_cpue_log_clean, aes(year, cpue, fill = Gear)) + geom_boxplot() +
  theme(axis.text.x = element_text(size = 14, angle = 90, h = 1)) +
  labs(x = "", y = "Fishery CPUE\n")

# Only a handful of vessels with autobaiter gear
ggplot(ll_cpue_log_clean, aes(Adfg, cpue, color = Gear)) + geom_jitter(alpha=.4) +
  theme(axis.text.x = element_text(colour = "white"))}


#--------------------------------------------------------------------------------
# RELEASE analysis
# now that we've looked at logbook data, lets see what we can glean about release
# behavior from the skippers who were good about recording that data... 
colnames(pot_cpue_log_clean)

#release trends
pot_cpue_log_clean %>% filter(trip_target == "Sablefish") %>%
  group_by(year) %>%
  do(data.frame(rbind(smean.cl.boot(.$set_releases)))) -> plot_boot10 #view(plot_boot7)

plot_boot10 <-as.data.frame(plot_boot10)
ggplot(plot_boot10) +
  geom_ribbon(aes(x = as.numeric(as.character(year)), ymin = Lower, ymax = Upper), 
                           alpha = 0.1, fill = "grey55") +
              #alpha = 0.1) +
  geom_point(aes(x = as.numeric(as.character(year)), y = Mean), size = 1) +
  geom_line(aes(x = as.numeric(as.character(year)), y = Mean)) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "mean fishery logged and inferred releases (lbs)\n") +
  lims(y = c(0, max(plot_boot10$Upper)))

pot_cpue_log_clean %>% filter(trip_target == "Sablefish") %>%
  group_by(year) %>%
  do(data.frame(rbind(smean.cl.boot(.$prop_released)))) -> plot_boot11 #view(plot_boot7)

ggplot(plot_boot11) +
  geom_ribbon(aes(x = as.numeric(as.character(year)), ymin = Lower, ymax = Upper), 
                           alpha = 0.1, fill = "grey55") +
              #alpha = 0.1) +
  geom_point(aes(x = as.numeric(as.character(year)), y = Mean), size = 1) +
  geom_line(aes(x = as.numeric(as.character(year)), y = Mean)) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Proportion of catch recorded as released") +
  lims(y = c(0, 0.05))

ggplot(pot_cpue_log_clean, aes(year, set_releases)) + geom_boxplot()

# Trends over time by area
ggplot(pot_cpue_log_clean %>% 
         filter(Stat %in% c("345603", "345631", "345701", "345731")), aes(Stat, set_releases, fill = year)) + 
  geom_boxplot() +
  scale_fill_manual(values = rev(colorspace::sequential_hcl(c = 0, l = c(30, 90), 
                                                            power = c(1/5, 1.3), 
                                                            n_distinct(pot_cpue$year))),
                    guide = FALSE) +
  labs(x = NULL, y = "Fishery releases (lbs)\n")

#numbers
#proportions relative to landings
#proportions of sets that recorded releases

pot_cpue_log_clean %>% group_by (year) %>%
  dplyr::summarise(by = "sets",
                   n_no_releases = sum(set_releases == 0),
                   Tot_n = n(),
                   n_releases = sum(set_releases > 0),
                   prop_w_releases = n_releases/n(),
                   sd_prop = sqrt(prop_w_releases*(1-prop_w_releases)/n()))  -> sets_by_year

#proportions of trips that recorded releases
pot_cpue_ftx_clean %>% group_by (year) %>%
  dplyr::summarise(by = "trips",
    n_no_releases = sum(trip_recorded_releases == "no_logged_releases"),
    Tot_n = n(),
    n_releases = sum(trip_recorded_releases == "logged_releases"),
    prop_w_releases = n_releases/n(),
    sd_prop = sqrt(prop_w_releases*(1-prop_w_releases)/n()))  -> trips_by_year

releases_by_year<-rbind(sets_by_year,trips_by_year)
unique(releases_by_year$by)

ggplot(releases_by_year) +
  geom_ribbon(aes(x = as.numeric(as.character(year)), 
                  ymin = prop_w_releases-1.96*sd_prop, 
                  ymax = prop_w_releases+1.96*sd_prop,
                  fill = by), 
              alpha = 0.1) +
  geom_point(aes(x=as.numeric(as.character(year)),y = prop_w_releases, col = by), size = 1) +
  geom_line(aes(x=as.numeric(as.character(year)),y = prop_w_releases, col=by)) +
  #scale_x_continuous() +
  labs(x = "", y = "Proportion of trips or sets that recorded releases") +
  lims(y = c(0, 0.5)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#I think that will suffice for understanding release behaviour for now
# Spike in 2019 was the result in regulatory changes that required more accurate
# logging of release behaviour. 

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# At this junction I think we will stick with using standardized CPUE calculated
# from the fish ticket data. 
colnames(pot_cpue_ftx_clean)

pot_cpue_ftx_clean %>%
  #filter(trip_set_targets == "all_Sablefish") %>%
  group_by(year) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> for_comp
















