
# Fishery catch 1985-present, fishery CPUE 1997-present
# Author: Jane Sullivan & Phil Joy
# Contact: jane.sullivan@noaa.gov & philip.joy@alaska.gov
# Last edited: Feb 2023

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
YEAR <- 2022

# Harvest ----

# There are multiple sources of catch data. The IFDB (Region I database) was
# used in the past, but J. Shriver recommends using the Gross Earnings File
# (GEF) for catch histories, which go back to 1975. The problem is the GEF isn't
# updated until data has been finalized, so it's never ready for the current
# year's assessment. The IFDB and GEF are the same from 1985-present, so use
# that. The SCAA model relies on Carlile et al 2002 published estimates of NSEI
# catch 1975-1984.

catch_ifdb <- read_csv(paste0(YEAR+1,"/data/fishery/nseiharvest_ifdb_1985_", YEAR,".csv"), 
                       guess_max = 50000) #%>% 

#exvessel_value <- read.csv("data/exvessel_value.csv") # request from Aaron.baldwin@alaska.gov
#update from OceanAK report for future standardization; 2022 onward will use this report...
# link:
exvessel_value <- read_csv(paste0(YEAR+1,"/data/exvessel_value_22ud.csv")) %>% 
  mutate(year=Year_Landed, exvessel_mil_usd = CFEC_Value/1000000)
#format like Jane's old for consistency with code... just year and value
exvessel_value<-exvessel_value[,c(5,6)]
#if new year not available, add in best est. from Aaron and groundfish crew.... 
exvessel_value[nrow(exvessel_value)+1,]<-list(YEAR,2.821949)
view(exvessel_value)

catch_ifdb %>% 
  filter(year > 2013) %>% 
  group_by(year, julian_day) %>% 
  dplyr::summarise(pounds = sum(whole_pounds)) %>% 
  mutate(cum_pounds = cumsum(pounds)) %>% 
  group_by(year) %>% 
  mutate(tot_pounds = sum(pounds)) %>% 
  ungroup() %>% 
  mutate(cum_pounds = cum_pounds / tot_pounds) -> catch_plot

# Cumulative catch over the fishery seasons
ggplot(catch_plot, aes(x = julian_day, colour = factor(year), size = factor(year), group = factor(year))) +
  geom_line(aes(y = cum_pounds)) +
  scale_color_stata() +
  scale_size_manual(values = c(rep(1, length(unique(catch_plot$year))-1), 2)) +
  # facet_wrap(~ year, ncol = 1) +
  labs(x = "Julian Day", y = "Cumulative catch", col = NULL, size = NULL) +
  ylim(0, 1)
  
# Total catch by year
catch_ifdb %>% 
  group_by(year) %>% 
  dplyr::summarize(total_pounds = sum(whole_pounds)) -> sum_catch #view(sum_catch)

# axis <- tickr(sum_catch, year, 5)
ggplot(sum_catch %>% 
         filter(year >= 1985), 
       aes(x = year, y = total_pounds/1e6)) +
  geom_line(group=1) +
  geom_point() +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  scale_x_continuous(breaks = seq(1985,2022,5), labels = seq(1985,2022,5)) + 
  scale_y_continuous(breaks = seq(0, 6, 1), limits = c(0, 6), labels = seq(0, 6, 1)) +
  # add a line for EQS starting in 1994 (1997 in the SSEI).
  geom_vline(xintercept = 1993.5, lty = 5, colour = "grey") +
  labs(x = NULL, y = "Catch (million round lb)\n") -> catch

write_csv(sum_catch, paste0(YEAR+1,"/output/harvest_1985_", YEAR, ".csv"))

catch
ggsave(paste0(YEAR+1,"/figures/fishery_harvest_1985_", YEAR, ".png"), 
       dpi=300,  height=4, width=7,  units="in")

# Catch by ports
'%ni%' <- Negate('%in%')

catch_ifdb %>% filter(year == YEAR) %>% distinct(Port)
catch_ifdb %>% filter(year == YEAR) %>% distinct(Vessel)
# catch_ifdb %>% filter(year == YEAR & Port == "HOM") 
catch_ifdb %>% 
  mutate(Port = derivedFactor(`SIT` = Port == "SIT",
                              `JNU` = Port == "JNU",
                              `PBG` = Port == "PBG",
                              `OTH` = Port %ni% c("SIT", "JNU", "PBG"))) %>% 
  filter(year >= 1985) %>% 
  group_by(year, Port) %>% 
  dplyr::summarise(pounds = sum(whole_pounds),
                   n_cfec = n_distinct(Cfec_permit),
                   n_vessels = n_distinct(Vessel)) %>% 
  filter(n_vessels > 3) %>% 
  group_by(year) %>% 
  mutate(tot_pounds = sum(pounds),
         perc = pounds/tot_pounds * 100) -> port_catch

ggplot(port_catch, aes(x = year, y = pounds/1e6, colour = Port, group = Port)) +
  geom_line()

ggplot(port_catch, aes(x = year, y = perc, fill = Port)) +
  geom_bar(stat = "identity", width = 1, colour = "black") +
  scale_fill_grey(start = 0.15, end = 1) +
  scale_x_continuous(breaks = seq(1985,2022,5), labels = seq(1985,2022,5)) +
  # add a line for EQS starting in 1994 (1997 in the SSEI).
  # geom_vline(xintercept = 1993.5, lty = 5, colour = "grey") +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  theme(legend.position = c(0.8,0.8),
        legend.background = element_rect(color = "black", 
                                         fill = "white", 
                                         linetype = "solid")) +
  labs(x = NULL, y = "Percent landings by port", fill = NULL) -> port 

plot_grid(catch, port, ncol = 1, align = 'hv')

ggsave(paste0(YEAR+1,"/figures/catch_byport_", YEAR, ".png"),
       dpi=300, height=10, width=7, units="in")

# Exvessel value ----
exvessel <- ggplot(exvessel_value, aes(x = year, y = exvessel_mil_usd)) +
#exvessel <- ggplot(exvessel_value, aes(x = Year.Landed, y = CFEC.Value)) +
  geom_point() +
  geom_line() +
  # add a line for EQS starting in 1994 (1997 in the SSEI).
  # geom_vline(xintercept = 1993.5, lty = 5, colour = "grey") +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  scale_x_continuous(breaks = seq(1985,2022,5), labels = seq(1985,2022,5)) +
  labs(x = NULL, y = "Ex-vessel value (million USD)\n") +
  ylim(c(0, 12.5))
exvessel
ggsave(paste0(YEAR+1,"/figures/exvessel_value_1985_", YEAR, ".png"), 
       dpi=300,  height=4, width=7,  units="in")

plot_grid(catch, port, exvessel, ncol = 1)#, align = 'hv')

ggsave(paste0(YEAR+1,"/figures/catch_exvesselvalue_", YEAR, "v3.png"),
       dpi=300, height=10, width=7, units="in")
# View(port_catch)

# Relationship between ex-vessel price and catch
if(!require("ggrepel"))   install.packages("ggrepel") 
sum_catch %>% 
  left_join(exvessel_value) %>% 
  mutate(flag = ifelse(year %in% c(YEAR, YEAR-1, YEAR-2), "a", "b")) %>% 
  ggplot(aes(x = total_pounds / 1e6, y = exvessel_mil_usd, col = flag)) +
  geom_smooth(method = "lm", se = FALSE, col = "grey") +
  geom_point() + 
  ggrepel::geom_text_repel(aes(label = year), max.overlaps = Inf) +
  scale_colour_manual(values = c("red", "black"), guide = FALSE) +
  labs(x = "\nCatch (million round lb)", y = "Ex-vessel value (million USD)") 

ggsave(paste0(YEAR+1,"/figures/exvessel_catch_correlation_1985_", YEAR, ".png"), 
       dpi=300,  height=4, width=7,  units="in")

#------------------------------------------------------------------------------
#_______________________________________________________________________________
#-------------------------------------------------------------------------------

# LONGLINE Logbook/CPUE data  ----
random_check<-function(data){ #data<-mixed_targets
  #length(unique(Sable_ll_CPUE$Year))
  data<-data  #data<-ll_cpue
  
  year_list<-unique(data$year)
  year_check<-sample(year_list,1)
  
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
read_csv(paste0(YEAR+1,"/data/fishery/fishery_LL_cpue_1997_", YEAR,".csv"), 
         guess_max = 50000) -> ll_cpue #%>% 
ll_cpue<-unique(ll_cpue)
random_check(ll_cpue)

length(is.na(ll_cpue$no_hooks_set))
unique(ll_cpue$depredation)
#data will still be stratified by set and some other variables so need to consolidate
# data by year, sell_date, Adfg, Stat, 

# 1) Look at cpue based on fish ticket landings... 
ll_cpue_ftx <- unique(ll_cpue %>% 
  group_by(year, sell_date, Adfg, Stat) %>% 
  select(-set_date,-julian_day_set,-set_soak,-set_length,-set_depth,-set_no,
         -logged_no,-logged_lbs,-disposition,-set_depredation,-start_lat,-start_lon))
random_check(ll_cpue_ftx) #should just be one row for each trip... 
histogram(ll_cpue_ftx$p_sets_depredated[ll_cpue_ftx$p_sets_depredated > 0])

unique(ll_cpue_ftx$trip_set_targets)

colnames(ll_cpue)
nrow(ll_cpue)
nrow(unique(ll_cpue))

unique(ll_cpue$Stat); with(ll_cpue, table(Stat))
unique(ll_cpue$multi_gear_config); with(ll_cpue, table(multi_gear_config))
unique(ll_cpue$multigear_trip)
unique(ll_cpue$trip_recorded_releases); with(ll_cpue, table(year, trip_recorded_releases))
unique(ll_cpue$set_depredation); unique(ll_cpue$trip_depredation)
unique(ll_cpue$no_hooks_p_set)
unique(ll_cpue$gear); str(ll_cpue$gear)

with(ll_cpue, table(hook_size)); nrow(ll_cpue %>% filter (is.na(trip_soak)))
with(ll_cpue, table(Stat))
hist(ll_cpue$julian_day_sell); abline(v=226, col="red")
hist(ll_cpue$no_hooks_fished_on_trip); abline(v=15000, col="blue")
hist(ll_cpue$no_hooks_p_set); abline(v=15000, col="blue")
table(ll_cpue$hook_space)

colnames(ll_cpue_ftx)
unique(ll_cpue_ftx$multi_gear_config)

ll_cpue_ftx %>% 
  #filter(depredation == "No depredation" | is.na(depredation)) %>% 
  filter(multi_gear_config == "single_config" &   #get rid of trips that reported 2 gear configurations
         #trip_set_targets == "all_Sablefish",   # only use trips that were dedicated to sablefish... but not yet...
           !is.na(sell_date) & 
           !is.na(mean_hook_spacing) & #!is.na(hook_space) & 
           !is.na(sable_lbs_set) &
          # !is.na(start_lon) & !is.na(start_lon) & #remove this because this is at the set level... 
           !is.na(trip_soak) & !is.na(trip_depth) &
           !is.na(mean_hook_size) &   #!is.na(hook_size) &
           hook_size != "MIX" &
             trip_soak > 0 & !is.na(trip_soak) & # soak time in hrs
           julian_day_sell > 226 & # if there were special projects before the fishery opened
           no_hooks_p_set < 15000 & # 15000 in Kray's scripts - 14370 is the 75th percentile
           # limit analysis to Chatham Strait and Frederick Sounds where the
           # majority of fishing occurs
           # target = 710 &
           Stat %in% c("345603", "345631", "345702",
                       "335701", "345701", "345731", "345803")) %>% 
  
  mutate(Year = factor(year), 
         Gear = factor(gear),
         Adfg = factor(Adfg),
         Stat = factor(Stat),
         Stat = fct_relevel(Stat,
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
         std_hooks = 2.2 * no_hooks_p_set * (1 - exp(-0.57 * (mean_hook_spacing / 39.37))),
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
  ungroup() -> ll_cpue_ftx

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

# Bootstrap ----

# axis <- tickr(fsh_cpue, year, 5)

# Simple bootstrap confidence intervals (smean.cl.boot from rms) ??rms
library(rms); library(viridis)
ll_cpue_ftx %>%
  group_by(year,trip_set_targets) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> plot_boot1
#view(plot_boot1)

ggplot(plot_boot1) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper, fill = trip_set_targets), 
               alpha = 0.1) +
  geom_point(aes(x = year, y = Mean, col = trip_set_targets), size = 1) +
  geom_line(aes(x = year, y = Mean, col = trip_set_targets)) +
  labs(x = "", y = "Sablefish CPUE (round lb per hook)\n") +
  lims(y = c(0, 1.25))  + 
  scale_color_viridis_d(name = "Set targets of trip",
                    labels = c("all halibut","all sablefish","sablefish and halibut mix"),
                    option = "C", begin=0,end=0.85) +
  scale_fill_viridis_d(name = "Set targets of trip",
                       labels = c("all halibut","all sablefish","sablefish and halibut mix"),
                     option = "C", begin=0,end=0.85) +
  theme(legend.position = "bottom") #, + #c(0.2,0.8),
  
ggsave(paste0(YEAR+1,"/figures/llcpue_ftx_bootCI_bytarget_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")
#----
ll_cpue_ftx %>%
  filter(trip_set_targets == "all_Sablefish") %>%
  group_by(year,trip_recorded_releases) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> plot_boot2 #view(plot_boot2)

ggplot(plot_boot2) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper, fill = trip_recorded_releases), 
              alpha = 0.1) +
  geom_point(aes(x = year, y = Mean, col = trip_recorded_releases), size = 1) +
  geom_line(aes(x = year, y = Mean, col = trip_recorded_releases)) +
  labs(x = "", y = "Sablefish CPUE (round lb per hook)\n") +
  lims(y = c(0, 2)) + 
  scale_color_viridis_d(name = "",
                        labels = c("Trip logged releases","Trip logged NO releases"),
                        option = "C", begin=0,end=0.65) +
  scale_fill_viridis_d(name = "",
                       labels = c("Trip logged releases","Trip logged NO releases"),
                       option = "C", begin=0,end=0.65) +
  theme(legend.position = "bottom") #, + #c(0.2,0.8),

ggsave(paste0(YEAR+1,"/figures/llcpue_ftx_bootCI_byrelease_1997_", YEAR, ".png"),
       dpi=300, height=4, width=6, units="in")

#---- 
depr_eff<-lm(data=ll_cpue_ftx, std_cpue ~ p_sets_depredated)
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

#ggsave(paste0(YEAR+1,"/figures/fshcpue_ftx_bootCI_bydepr_1997_", YEAR, ".png"),
#       dpi=300, height=4, width=7, units="in")

ll_cpue_ftx %>%
  filter(trip_set_targets == "all_Sablefish") %>%
  group_by(year,multigear_trip) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> plot_boot4 #view(plot_boot4)

ggplot(plot_boot4) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper, fill = multigear_trip), 
              #             alpha = 0.1, fill = "grey55") +
              alpha = 0.1) +
  geom_point(aes(x = year, y = Mean, col = multigear_trip), size = 1) +
  geom_line(aes(x = year, y = Mean, col = multigear_trip)) +
  geom_errorbar(aes(x=year, y=Mean,ymin=Lower,ymax=Upper, col = multigear_trip),
                position=position_dodge(width=0), width=0.5, size=0.2) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") +
  lims(y = c(0, 1.5)) + 
  scale_color_viridis_d(name = "",
                        labels = c("Longline trip","Mixed longline & pot trip"),
                        option = "A", begin=0,end=0.65) +
  scale_fill_viridis_d(name = "",
                       labels = c("Longline trip","Mixed longline & pot trip"),
                       option = "A", begin=0,end=0.65) +
  theme(legend.position = "bottom")

ggsave(paste0(YEAR+1,"/figures/llcpue_ftx_bootCI_bygeartrip_1997_", YEAR, ".png"),
       dpi=300, height=4, width=6, units="in")
# Prelim works towards CPUE analysis for NSEI, mirroring what was done by Jenny
# Stahl and Ben Williams in SSEI

#--------------------------------------------------------------------------------
# Normality

#for analysis we will use CPUE from trips that targetted only sablefish and that
# experienced no depredation... 
ll_cpue_ftx_clean <-ll_cpue_ftx %>% 
  filter(trip_set_targets == "all_Sablefish",
         Depr_sum == "none")
# Long right tail
ggplot(ll_cpue_ftx_clean, aes(std_cpue)) + geom_density(alpha = 0.4, fill = 4)

# Better, but still not normal with log transformation
ggplot(ll_cpue_ftx_clean, aes(log(std_cpue + 1))) + geom_density(alpha = 0.4, fill = 4)

# Following Jenny Stahl and Ben Williams' work in the SSEI, increase CPUE by 10%
# of the mean per Cambell et al 1996 and Cambell 2004. Back-transform with
# exp(cpue - mean(fsh_cpue$std_cpue) * 0.1)
ll_cpue_ftx_clean %>% 
  mutate(cpue = log(std_cpue + (mean(ll_cpue_ftx_clean$std_cpue, na.rm=T) * 0.1))) -> ll_cpue_ftx_clean

ggplot(ll_cpue_ftx_clean, aes(cpue)) + geom_density(alpha = 0.4, fill = 4)

# EDA for GAM 

# Trends over time
ggplot(ll_cpue_ftx_clean, aes(Year, std_cpue)) + geom_boxplot()

# Trends over time by area
ggplot(ll_cpue_ftx_clean %>% 
         filter(Stat %in% c("345603", "345631", "345701", "345731")), aes(Stat, std_cpue, fill = Year)) + 
  geom_boxplot() +
  scale_fill_manual(values = rev(colorspace::sequential_hcl(c = 0, l = c(30, 90), 
                                                            power = c(1/5, 1.3), 
                                                            n_distinct(ll_cpue$year))),
                    guide = FALSE) +
  labs(x = NULL, y = "Fishery CPUE (round pounds per hook)\n")

ggsave(paste0(YEAR+1,"/figures/fshcpue_trendsbyStat_",min(ll_cpue$year), "_", YEAR, ".png"), 
       dpi=400, height=4, width=7.5, units="in")

#2022: Note that 2020 and 2021 have some very high CPUE outliers with new query from Justin...
#or... with recruitment of '13-'16 year classes there is far more variability based on discard
# behavior as some fisherman clean up on small fish (but most opt not to); could check the outlier
# vessel if there were lengths... but just poundage... 
# after review and conversation, outliers belonged to 2nd gen knowledgable and "honest" fisherman
# and data is considered legit.  Furthermore, 2022 reboot shows high cpues in the past now so
# seems to be OK... 

# No one fished in Fred. Sound in 2018, 2 in 2019, 4 in 2021
ll_cpue_ftx_clean %>% filter(Stat %in% c("345702", "335701") & year == YEAR) %>% distinct(Adfg)
# Activity in N Chatham 
ll_cpue_ftx_clean %>% filter(Stat %in% c("345731", "345803") & year == YEAR) %>% distinct(Adfg)
# Activity in S Chatham
ll_cpue_ftx_clean %>% filter(Stat %in% c("345603")) %>% group_by(year) %>%  dplyr::summarize(n_distinct(Adfg)) %>% View()
ll_cpue_ftx_clean %>% filter(Stat %in% c("335701")) %>% group_by(year) %>%  dplyr::summarize(n_distinct(Adfg)) %>% View()

ll_cpue_ftx_clean %>% 
  group_by(year, Stat) %>% 
  dplyr::summarize(trips = n_distinct(trip_no),
            vessels = n_distinct(trip_no)) -> stat_sum

# Gear performance by Stat
ggplot(ll_cpue_ftx_clean, aes(Stat, cpue, fill = Gear)) + geom_boxplot()

# Gear performance over time
ggplot(ll_cpue_ftx_clean, aes(Year, cpue, fill = Gear)) + geom_boxplot() +
  theme(axis.text.x = element_text(size = 14, angle = 90, h = 1)) +
  labs(x = "", y = "Fishery CPUE\n")

# Only a handful of vessels with autobaiter gear
ggplot(ll_cpue_ftx_clean, aes(Adfg, cpue, color = Gear)) + geom_jitter(alpha=.4) +
  theme(axis.text.x = element_text(colour = "white"))

#--------------------------------------------------------------------------------
# lets do the same thing again, but use the lbs/hook calculated from the raw data 

# 1) Look at cpue based on fish ticket landings... 
ll_cpue_ftx2 <- unique(ll_cpue %>% 
                        group_by(year, sell_date, Adfg, Stat) %>% 
                        select(-set_date,-julian_day_set,-set_soak,-set_length,-set_depth,-set_no,
                               -logged_no,-logged_lbs,-disposition,-set_depredation,-start_lat,-start_lon))
random_check(ll_cpue_ftx2) #should just be one row for each trip... 
colnames(ll_cpue_ftx2)

ll_cpue_ftx2 %>% 
  #filter(depredation == "No depredation" | is.na(depredation)) %>% 
  filter(multi_gear_config == "single_config" &   #get rid of trips that reported 2 gear configurations
           #trip_set_targets == "all_Sablefish",   # only use trips that were dedicated to sablefish... but not yet...
           !is.na(sell_date) & 
           !is.na(mean_hook_spacing) & #!is.na(hook_space) & 
           !is.na(sable_lbs_set) &
           # !is.na(start_lon) & !is.na(start_lon) & #remove this because this is at the set level... 
           !is.na(trip_soak) & !is.na(trip_depth) &
           !is.na(mean_hook_size) &   #!is.na(hook_size) &
           hook_size != "MIX" &
           trip_soak > 0 & !is.na(trip_soak) & # soak time in hrs
           julian_day_sell > 226 & # if there were special projects before the fishery opened
           no_hooks_p_set < 15000 & # 15000 in Kray's scripts - 14370 is the 75th percentile
           # limit analysis to Chatham Strait and Frederick Sounds where the
           # majority of fishing occurs
           # target = 710 &
           Stat %in% c("345603", "345631", "345702",
                       "335701", "345701", "345731", "345803")) %>% 
  
  mutate(Year = factor(year), 
         Gear = factor(gear),
         Adfg = factor(Adfg),
         Stat = factor(Stat),
         Stat = fct_relevel(Stat,
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
         
         std_hooks = 2.2 *  (1 - exp(-0.57 * (mean_hook_spacing / 39.37))),  
         std_cpue_all = lbs_p_hk_all / std_hooks,
         std_cpue_exact = lbs_p_hk_exact / std_hooks,
         cpue_all = lbs_p_hk_all,
         cpue_exact = lbs_p_hk_exact,
         # dummy varbs, for prediction with random effects
         dum = 1, 
         dumstat = 1) %>% 
  
  group_by(year) %>% 
  mutate(
    #The number of vessels participating in the fishery has descreased by 50% from
    #1997-2015. create new column is the total number of active vessels
    #participating in a given year
    total_vessels = n_distinct(Adfg),
    # Total unique trips per year
    total_trips = n_distinct(trip_no)) %>% 
  ungroup() -> ll_cpue_ftx2

# Bootstrap ----

# Simple bootstrap confidence intervals (smean.cl.boot from rms) ??rms

rbind(ll_cpue_ftx2 %>%
        group_by(year,trip_set_targets) %>%
        do(data.frame(rbind(smean.cl.boot(.$std_cpue_all)))) %>%
        mutate(hook_cpue_spec = "std_cpue_all"),
      ll_cpue_ftx2 %>%
        group_by(year,trip_set_targets) %>%
        do(data.frame(rbind(smean.cl.boot(.$std_cpue_exact)))) %>%
        mutate(hook_cpue_spec = "std_cpue_exact"),
      ll_cpue_ftx2 %>%
        group_by(year,trip_set_targets) %>%
        do(data.frame(rbind(smean.cl.boot(.$lbs_p_hk_all)))) %>%
        mutate(hook_cpue_spec = "cpue_all"),
      ll_cpue_ftx2 %>%
        group_by(year,trip_set_targets) %>%
        do(data.frame(rbind(smean.cl.boot(.$lbs_p_hk_exact)))) %>%
        mutate(hook_cpue_spec = "cpue_exact")
      ) -> plot_boot5

ggplot(plot_boot5) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper, fill = trip_set_targets), 
              #             alpha = 0.1, fill = "grey55") +
              alpha = 0.1) +
  geom_point(aes(x = year, y = Mean, col = trip_set_targets), size = 1) +
  geom_line(aes(x = year, y = Mean, col = trip_set_targets)) +
  facet_wrap(~hook_cpue_spec) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") +
  lims(y = c(0, 1.5)); view(plot_boot5)

#ggsave(paste0(YEAR+1,"/figures/fshcpue_ftx2_bootCI_bytarget_1997_", YEAR, ".png"),
#       dpi=300, height=4, width=7, units="in")


ggplot(plot_boot5 %>% filter(trip_set_targets == "all_Sablefish")) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper, fill = hook_cpue_spec), 
              #             alpha = 0.1, fill = "grey55") +
              alpha = 0.1) +
  geom_point(aes(x = year, y = Mean, col = hook_cpue_spec), size = 1) +
  geom_line(aes(x = year, y = Mean, col = hook_cpue_spec)) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") +
  lims(y = c(0, 1.5)); view(plot_boot5)

ggplot(rbind(plot_boot5 %>% filter(trip_set_targets == "all_Sablefish"),
       plot_boot1 %>% filter(trip_set_targets == "all_Sablefish") %>%
         mutate(hook_cpue_spec = "from_lbs_per_set"))) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper, fill = hook_cpue_spec), 
              #             alpha = 0.1, fill = "grey55") +
              alpha = 0.1) +
  geom_point(aes(x = year, y = Mean, col = hook_cpue_spec), size = 1) +
  geom_line(aes(x = year, y = Mean, col = hook_cpue_spec)) +
  #facet_wrap(~hook_cpue_spec) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") +
  lims(y = c(0, 1.5)); view(plot_boot5)

ggplot(rbind(plot_boot5 %>% filter(trip_set_targets == "all_Sablefish", 
                                   hook_cpue_spec == "std_cpue_all") %>%
               mutate(hook_cpue_spec = "lbs per hook derivation"),
             plot_boot1 %>% filter(trip_set_targets == "all_Sablefish") %>%
               mutate(hook_cpue_spec = "lbs per set derivation"))) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper, 
                  fill = hook_cpue_spec, col = hook_cpue_spec), 
              #             alpha = 0.1, fill = "grey55") +
              alpha = 0.1) +
  geom_point(aes(x = year, y = Mean, col = hook_cpue_spec), 
             size = 1, position=position_jitter(h=0.0075,w=0.0075)) +
  geom_line(aes(x = year, y = Mean, col = hook_cpue_spec),
            position=position_jitter(h=0.005,w=0.005)) +
  #facet_wrap(~hook_cpue_spec) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") +
  lims(y = c(0, 1.5)) + 
  scale_color_viridis_d(name = "CPUE derivation",
                        labels = c("from raw lbs/hook","from lbs/set"),
                        option = "C", begin=0,end=0.65) +
  scale_fill_viridis_d(name = "CPUE derivation",
                       labels = c("from raw lbs/hook","from lbs/set"),
                       option = "C", begin=0,end=0.65) +
  theme(legend.position = "bottom")

ggsave(paste0(YEAR+1,"/figures/llcpue_by_derivation_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")


## Pretty cool... we get to the same CPUE weather from raw lbs/hook or through lbs/set... 
# Can skip the rest of this because we're getting the same answers
# Also, including the estimated number of hooks (i.e, when multiple configurations are present
# and we used the average and such...) doesn't produce anything different... very minor
# discrepencies in the point estimates.

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# CPUE Calc and exam using FISH TICKET DATA... 
#--------------------------------------------------------------------------------
# HOOK SIZE performance - hook size 11 should be removed due to sample size and
# infrequency of use. Probably size 7 too - 4 vessels fished size 7 hooks in
# 1997, and only 1 vessel fished it until 2004
# 2022: also size 6 hooks ... half as many as size 7
fsh_cpue_ll_ft<-ll_cpue_ftx_clean
fsh_cpue_ll_lb<-ll_cpue_log_clean

table(fsh_cpue_ll_ft$Hook_size)
#new filter includes mixed hook size sets... need to cull those for analysis if
# hook size is included... 
str(fsh_cpue_ll_ft$Hook_size)
fsh_cpue_hooks<-fsh_cpue_ll_ft %>% 
  filter(Hook_size %in% c("6","7","11","12","13","14","15","16")) %>%
  mutate(Hook_size = fct_relevel(Hook_size,c("6","7","11","12","13","14","15","16")))

fsh_cpue_hooks %>% filter(Hook_size == "7") %>% distinct(Adfg, Stat, Year)
fsh_cpue_hooks %>% filter(Hook_size == "6") %>% distinct(Adfg, Stat, Year)

# PJ22 obs: before censoring hooks run analysis with them in there... they actually don't
# make a big difference and can likely be ignored... pj2022
# fsh_cpue %>% filter(!Hook_size %in% c("11", "7", "6")) -> fsh_cpue
fsh_cpue_hooks %>% filter(!Hook_size %in% c("6","7","11")) -> fsh_cpue_hooks

# Not much contrast in CPUE between remaining hook sizes... maybe some
# difference in performance between years/areas
ggplot(fsh_cpue_hooks, aes(Hook_size, cpue)) + geom_boxplot()
ggplot(fsh_cpue_hooks, aes(Year, cpue, fill = Hook_size)) + geom_boxplot()+
  theme(axis.text.x = element_text(size = 14, angle = 90, h = 1)) +
  labs(x = "", y = "Fishery CPUE\n")
ggplot(fsh_cpue_hooks, aes(Stat, cpue, fill = Hook_size)) + geom_boxplot()+
  labs(x = "\nStat area", y = "Fishery CPUE\n")
# New hook size 6 in 2019, vessel look up:
fsh_cpue %>% filter(Hook_size == "6") %>% distinct(Adfg, cpue)

#**2022: I think the best way to get CPUE is cull 6,7, and 11 (<100 samples) and
# then ignore hook size in estimating CPUE since it doesn't make a difference
# (even if model fits better)

fsh_cpue_cl<-fsh_cpue_hooks %>%
  filter(!Hook_size %in% c("6","7","11")) %>%
  mutate(soak_p_set = trip_soak/no_sets)

# Depth - clear increasing trend, asymptotes ~ 450 m
ggplot(fsh_cpue_cl, aes(trip_depth, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 2, se = FALSE) 

# Soak time - cut off at 40 hrs b/c it looks like there's a slight outlier
# effect
# hmmm ... debatable.  Outliers, but legit?  probably long soaks do to weather or other issues?

ggplot(fsh_cpue_cl, aes(soak_p_set, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 2, se = FALSE) 
fsh_cpue_cl %>% filter(soak < 40) -> fsh_cpue_cl

#Total km fished 
ggplot(fsh_cpue_cl, aes(total_km_fished, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 2, se = FALSE)
fsh_cpue_cl %>% filter(total_km_fished < 200 & total_km_fished > 0) -> fsh_cpue_cl
ggplot(fsh_cpue_cl, aes(total_km_fished, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 2, se = FALSE)
# similar to soak time trends... Jane is right about the scavengers down there! 

# Inconsistent and very slight latitudinal effect #!!! NEED TO GET LAT LONG DATA  ???
# 2023 change: with fish tickets will ignore lat long and just use stat area as spatial variable
#ggplot(fsh_cpue_cl, aes(start_lat, cpue, group = Year, colour = Year)) +
#  geom_smooth(method = 'loess', span = 1, se = FALSE) 

# Inconsistent and very slight seasonal effect
ggplot(fsh_cpue_cl, aes(julian_day_sell, cpue, group = Year, colour = Year)) +
  geom_smooth(method = 'loess', span = 1, se = FALSE) 

# By state area... 
ggplot(fsh_cpue_cl, aes(Stat, cpue)) + geom_boxplot()+
  labs(x = "\nStat area", y = "Fishery CPUE\n")

# GAM cpue ----

# Potential variables influencing CPUE (ultimately interested in estimating a
# Year effect):
# depth - increase in CPUE up to ~ 450 m, then asymptote. Very clear and
# consistent trend between years
# julian_day - decrease towards the end of the season? EDA suggested there is no
# consistent seasonal trend. If there is a trend, its slightly decreasing over
# the season.  pj22: I agree with slight decreasing trend through season
# soak time and total km fished have similar patterns... 
# Adfg - vessel effect, some are better fishermen than others. Routinely
# improves model fit and doesn't grossly violate assumptions.
# Gear - higher for conventional gear (01) over autobaiter (06) consistently
# between years, although this becomes dampened with the inclusion on the vessel
# effect
# Hook_size - optimal hook size? No consistent trend between years. Treat as random
# effect (Pj22: not sure I agree with random effect designation?  )
# start_lat - is there some consistent trend in Chatham Strait going north into
# Chatham? Not one that is consistent between years. If one exists it tends to
# be decreasing with latitude. There was no spatial autocorrelation detected (done in previous analysis).
# start_lat/start_lon - spatial autocorrelation  (need to check on this - pj22)

m0 <- bam(cpue ~ Year + Gear, data=fsh_cpue_cl, gamma=1.4)
m0.hook <- bam(cpue ~ Year + Gear + Hook_size, data=fsh_cpue_cl, gamma=1.4)
m0.depth <- bam(cpue ~ Year + Gear + s(trip_depth, k=4), data=fsh_cpue_cl, gamma=1.4)
m0.soak <- bam(cpue ~ Year + Gear + s(soak_p_set, k=4) , data=fsh_cpue_cl, gamma=1.4)
m0.stat <- bam(cpue ~ Year + Gear + s(Stat, bs='re', by=dumstat), data=fsh_cpue_cl, gamma=1.4)
m0.adfg <- bam(cpue ~ Year + Gear + s(Adfg, bs='re', by=dum), data=fsh_cpue_cl, gamma=1.4)
#m0.lat_lon <- bam(cpue ~ Year + Gear + te(start_lon, start_lat), data=fsh_cpue_cl, gamma=1.4)
#m0.lat <- bam(cpue ~ Year + Gear + s(start_lat), data=fsh_cpue_cl, gamma=1.4)
#m0.lon <- bam(cpue ~ Year + Gear + s(start_lon), data=fsh_cpue_cl, gamma=1.4)
m0.jday <- bam(cpue ~ Year + Gear + s(julian_day_sell, k=4), data=fsh_cpue_cl, gamma=1.4)
m0.length <- bam(cpue ~ Year + Gear + s(total_km_fished), data=fsh_cpue_cl, gamma=1.4)

model.list<-list(m0,m0.hook,m0.depth,m0.soak,m0.stat,m0.adfg,
                 #0.lat_lon, m0.lat,m0.lon,
                 m0.jday,m0.length)
names(model.list)<-c("m0","hook","depth","soak","stat","adfg",#"lat_lon",
                     #"lat","lon",
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

global<-bam(cpue ~ Year + Gear + Hook_size + s(trip_depth, k=4) + s(soak_p_set, k=4) + 
              s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum) +
              s(julian_day_sell, k=4) + s(total_km_fished),
            data=fsh_cpue_cl, gamma=1.4)

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

# Determine if random variables should be included (Stat and Adfg)
#try some multimodel inference with glms...  
library(lme4)
library(arm)
library(plyr)
library(MuMIn)   #r.squaredGLMM(object)

library(glmulti)
library(AICcmodavg)


lmer.glmulti <- function (formula, data, random = "", ...) {
  lmer(paste(deparse(formula), random), data = data, REML=F, ...)
}

setMethod('getfit', 'merMod', function(object, ...) {
  summ<-summary(object)$coef
  summ1<-summ[,1:2]
  if (length(dimnames(summ)[[1]])==1) {
    summ1<-matrix(summ1, nr=1, dimnames=list(c("(Intercept)"),c("Estimate","Std.Error")))
  }
  cbind(summ1, df=rep(10000,length(fixef(object))))
})

old<-Sys.time()
runmodels_ftx<-glmulti(cpue~Year*Gear*Hook_size*set_depth*set_soak*julian_day_sell*set_length,#s(set_depth, k=4), 
                   data=fsh_cpue_cl,
                   method="h",imm=0.5, sexrate=0.1, crit=aicc , deltaM=0.01, 
                   conseq=20, marginality=T, maxsize=7, confsetsize=100, 
                   level=2, fitfunc=lmer.glmulti, random="+(1|Stat)+(1|Adfg)"); Sys.time() - old
coef.glmulti(runmodels)
plot(runmodels,type="s")

##--------
#OK, back to more appropriate additive models.. 

# Determine if random variables should be included (Stat and Adfg)

m1 <- bam(cpue ~ Year + Gear + Hook_size + s(trip_depth, k=4) + s(soak_p_set, k=4) + s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum), data=fsh_cpue_cl, gamma=1.4)
m2 <- bam(cpue ~ Year + Gear + Hook_size + s(trip_depth, k=4) + s(soak_p_set, k=4) + s(Adfg, bs='re', by=dum), data=fsh_cpue_cl, gamma=1.4)
m3 <- bam(cpue ~ Year + Gear + Hook_size + s(trip_depth, k=4) + s(soak_p_set, k=4) +s(Stat, bs='re', by=dumstat), data=fsh_cpue_cl, gamma=1.4)
m4 <- bam(cpue ~ Year + Gear + Hook_size + s(trip_depth, k=4) + s(soak_p_set, k=4), data=fsh_cpue_cl, gamma=1.4)

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
# random noise associated with the variable, hook size has a plausible effect on the response variable?
m5 <- bam(cpue ~ Year + Gear + s(trip_depth, k=4) + s(soak_p_set, k=4) +    #no hook size
            s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum), data=fsh_cpue_cl, gamma=1.4)
m6 <- bam(cpue ~ Year + Gear + s(trip_depth, k=4) + s(soak_p_set, k=4) +    #hook size as a random variable
            s(Hook_size, bs='re', by=dum) + s(Stat, bs='re', by=dumstat)+ 
            s(Adfg, bs='re', by=dum), data=fsh_cpue_cl, gamma=1.4)

AIC(m1, m3, m4, m5, m6)

plot(m6, page = 1, shade = TRUE, resid = TRUE, all = TRUE)
summary(m5)
summary(m6)

# By AIC, treating Hooksize as a factor has the best predictive pwr, but the
# model treating it as a random effect is a close second. Inclusion of hook size
# as a factor or re results in no change in the deviance explained. Because
# there's no strong trend or difference between hook sizes and it seems just to
# account up some of the random variation, I'm going carry m6 forward (the model
# with the re for hook size).
#2022:  OK, same results.  get Jane's point about no strong trend with hook size
#       but am disinclined to treat it as a random effect.  HS is not a random category
#       or group... there is an effect of HS on catchability.... 
#       I will carry model 1 forward instead of model 6.
# 2023: Now that we are just using fishticket data it probably doesn't make sense
#       to schwag the lat-long data into this and just use the stat area as our 
#       spatial variable.  For now models m7-m10 will be blocked out... 
{
#Determine whether to include lat and long
m7 <- bam(cpue ~ Year + Gear + s(trip_depth, k=4) + s(soak_p_set, k=4) + 
            Hook_size + #s(Hook_size, bs='re', by=dum) + 
            s(Stat, bs='re', by=dumstat) + 
            s(Adfg, bs='re', by=dum) + te(start_lon, start_lat), data=fsh_cpue_cl, gamma=1.4)
m8 <- bam(cpue ~ Year + Gear + s(trip_depth, k=4) + s(soak_p_set) + 
            Hook_size + #s(Hook_size, bs='re', by=dum) + 
            s(Stat, bs='re', by=dumstat) + 
            s(Adfg, bs='re', by=dum) + s(start_lon), data=fsh_cpue_cl, gamma=1.4)
m9 <- bam(cpue ~ Year + Gear + s(depth, k=4) + s(soak, k=4) + 
            Hook_size + #s(Hook_size, bs='re', by=dum) + 
            s(Stat, bs='re', by=dumstat) + 
            s(Adfg, bs='re', by=dum) + s(start_lat), data=fsh_cpue_cl, gamma=1.4)

AIC(m1, m6, m7, m8, m9) #AIC(m6, m7, m8, m9)

summary(m7)
summary(m8)
summary(m9)

# m9, the model with the latitudinal effect, performs best by AIC, but only
# results in a slight improvement in the dev explained. Try limiting the number
# of knots to guard against overfitting... but m9 still performs best by AIC.
# Phil note: knots = k
# Phil note; tensor smoother allows integration of two variables (lat and lon here)
# same in 2022
m10 <- bam(cpue ~ Year + Gear + s(depth, k=4) + s(soak, k=4) + 
             s(Hook_size, bs='re', by=dum) + s(Stat, bs='re', by=dumstat) + 
             s(Adfg, bs='re', by=dum) + s(start_lat, k=6), data=fsh_cpue_cl, gamma=1.4)
AIC(m1, m6, m7, m8, m9, m10)

plot(m9, page = 1, shade = TRUE, all = TRUE) #resid = TRUE,
plot(m10, page = 1, shade = TRUE, all = TRUE) #resid = TRUE,

# m7 with both lat and lon with a tensor smoother has the second best
# performance. red/orange is higher CPUE, green average and blue lower; can
# change "too.far" values to change what shows on graph. Highest cpue in the
# north, south and central chatham
vis.gam(m7, c('start_lon','start_lat'), type='response', plot.type='contour', color='topo', too.far=0.1)
}
# The inclusion of a seasonal effect  improves model fit - there is a
# slightly decreasing trend in cpue on average over the course of the season.
m11 <- bam(cpue ~ Year + Gear + s(julian_day_sell, k=4) + s(trip_depth, k=4) + 
             s(soak_p_set, k=4) + #s(start_lat) + 
             Hook_size + #s(Hook_size, bs='re', by=dum) + 
             s(Stat, bs='re', by=dumstat) + s(Adfg, bs='re', by=dum), data=fsh_cpue_cl, gamma=1.4)
AIC(global,m1, m11) #AIC(m1, m7, m9, m11)
summary(m11)
plot(m11, page = 1, shade = TRUE, all = TRUE) #resid = TRUE,

# # Relationship between depth and soak time - highest cpue in > 450 m
# # and ~ 10 hr soak time
vis.gam(m11, c('trip_depth', 'soak_p_set'), plot.type='contour', type='response', color='topo', too.far=0.1)

global<-bam(cpue ~ Year + Gear + Hook_size + s(trip_depth, k=4) + 
              s(soak_p_set, k=4) + s(total_km_fished) + 
              s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum) +
              s(julian_day_sell, k=4) ,
            data=fsh_cpue_cl, gamma=1.4)

AIC(global, m1, m11) #AIC(m1, m7, m9, m11)

AIC(global, m1,m2,m3,m4,m5,m6,m11)

summary(global)
plot(global, page = 1, shade = TRUE, all = TRUE) #resid = TRUE,

vis.gam(global, c('trip_depth', 'total_km_fished'), plot.type='contour', type='response', color='topo', too.far=0.1)
vis.gam(global, c('soak_p_set', 'total_km_fished'), plot.type='contour', type='response', color='topo', too.far=0.1)

plot.gam(global)

str(diag(vcov.gam(global)))

str(summary(m12))
summary(m12)$dev.expl
summary(m12)$r.sq
AIC(m12)

m0.1 <- bam(cpue ~ Year + Gear, data=fsh_cpue_cl, gamma=1.4)

model.list<-list(global,m1,m2,m3,m4,m5,m6,m11)
names(model.list)<-c("global","m1","m2","m3","m4","m5","m6","m11")
names(model.list[1])
modsum<-data.frame(); j<-1
for (i in model.list) {
  #mod<-i
  modsum[j,"model"]<-names(model.list[j])
  modsum[j,"aic"]<-AIC(i)
  modsum[j,"dev"]<-summary(i)$dev.expl
  modsum[j,"rsq"]<-summary(i)$r.sq
  j<-j+1
}

modsum %>% arrange(aic)  
modsum %>% arrange(-dev)  
modsum %>% arrange(-rsq) 

# GAM summary ----

# Final model structure (m12) (* = random effect):
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

# Predictions ----

#Create standard dataset to get standardized CPUE for each year

std_dat <- expand.grid(year = unique(fsh_cpue_cl$year),
                       Gear = 'CS',
                       trip_depth = mean(fsh_cpue_cl$trip_depth), 
                       soak_p_set = 10, 
                       julian_day_sell = median(fsh_cpue_cl$julian_day_sell),
                       #start_lat = mean(fsh_cpue$start_lat),
                       total_km_fished = median(fsh_cpue_cl$total_km_fished),
                       Stat = "345701",
                       Hook_size = "14",
                       Adfg = "35491",
                       dum = 0,
                       dumstat = 0) %>% 
  mutate(Year = factor(year))

pred_cpue <- predict(global, std_dat, type = "link", se = TRUE)

#checking my code with Jane's... checks out :)
preds<-predict.bam(global, type="response", std_dat, se = TRUE)
str(preds); head(preds)

#Put the standardized CPUE and SE into the data frame and convert to
#backtransformed (bt) CPUE
std_dat %>% 
  mutate(fit = pred_cpue$fit,
         se = pred_cpue$se.fit,
         upper = fit + (2 * se),
         lower = fit - (2 * se),
         bt_cpue = exp(fit) - (mean(fsh_cpue_cl$std_cpue) * 0.1),
         bt_upper = exp(upper) - (mean(fsh_cpue_cl$std_cpue) * 0.1),
         bt_lower = exp(lower) - (mean(fsh_cpue_cl$std_cpue) * 0.1),
         bt_se = (bt_upper - bt_cpue) / 2  #,
         #bt_cv = bt_se/bt_cpue
         ) -> std_dat

# Nominal CPUE ----

fsh_cpue_cl %>% 
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

#compare to the old methods... 

old_cpue<-read_csv("2022/output/fshcpue_1997_2021_nominal_for_fut.csv", 
                   guess_max = 50000) 

fsh_sum %>%
  select(year, cpue = fsh_cpue, upper, lower) %>% 
  mutate(CPUE = "Nominal") %>% 
  bind_rows(std_dat %>% 
              select(year, cpue = bt_cpue, upper = bt_upper, lower = bt_lower) %>% 
              mutate(CPUE = "Fully Standardized")) %>% 
  bind_rows(old_cpue %>% 
              select(year, cpue = fsh_cpue, upper, lower) %>% 
              mutate(CPUE = "Pre-2023 Nominal")) %>%
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

ggsave(paste0(YEAR+1,"/figures/compare_OLD_stdcpue_llfsh_", YEAR, ".png"), dpi=300, height=4, width=7, units="in")

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

data.frame(year = 1980:1996,
           fsh_cpue = hist_cpue) %>% 
  mutate(var = (fsh_cpue * mean(fsh_sum$cv)) ^ 2,
         se = mean(fsh_sum$se),
         upper = hist_cpue+(2*se),
         lower = hist_cpue-(2*se),
         CPUE = "Nominal") %>% 
  bind_rows(fsh_sum %>% mutate(CPUE = "Nominal") %>%
              select(year, fsh_cpue, var, se, upper, lower, CPUE)) %>% 
  mutate(cpue = round(fsh_cpue, 3),
         var = round(var, 3)) -> nom_cpue_ts

data.frame(year = 1980:1996,
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
              mutate(CPUE = "Fully Standardized")) %>% 
  mutate(cpue = round(fsh_cpue, 3),
         var = round(var, 3)) %>% data.frame() -> glob_cpue_ts

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
write_csv(nom_cpue_ts, paste0(YEAR+1,"/output/ll_cpue_nom_", min(nom_cpue_ts$year), "_", YEAR, ".csv"))

glob_cpue_ts<-as.matrix(m12_cpue_ts)
glob_cpue_ts[,c(1,2,3,4,5,6,8)]<-as.numeric(glob_cpue_ts[,c(1,2,3,4,5,6,8)])
glob_cpue_ts<-as.data.frame(m12_cpue_ts)

write_csv(glob_cpue_ts, paste0(YEAR+1,"/output/ll_cpue_m12_", min(nom_cpue_ts$year), "_", YEAR, ".csv"))


#=================================================================================
#=================================================================================
# lets look at the LOGBOOKS via the same route and see what we come up with... 
#--------------------------------------------------------------------------------
#==================================================================================

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

ll_cpue_logged <-ll_cpue
nrow(ll_cpue_logged)
colnames(ll_cpue_logged)

nrow(ll_cpue_logged %>% filter (!is.na(logged_no)))
nrow(ll_cpue_logged %>% filter (!is.na(logged_lbs)))

releases<-ll_cpue_logged %>% filter(trip_recorded_releases == "logged_releases")
random_check(releases)
unique(ll_cpue_logged$disposition)

old <- Sys.time()
ll_cpue_logged %>% group_by(year, sell_date, Adfg, Stat, disposition, trip_no) %>%
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
#since this takes a long time to run, lets save it in case we need to restart our
# computer because of a f%@#$^#g IT update...
write_csv(test, paste0(YEAR+1,"/output/temp_logbook_trans_", min(nom_cpue_ts$year), "_", YEAR, ".csv"))

read_csv(paste0(YEAR+1,"/output/temp_logbook_trans_", min(nom_cpue_ts$year), "_", YEAR, ".csv"), 
         col_names = FALSE) %>% as.numeric() -> test

test %>% group_by(year, sell_date, Adfg, Stat, set_no) %>%
  
  filter(multi_gear_config == "single_config" &   #get rid of trips that reported 2 gear configurations
           trip_set_targets == "all_Sablefish",   # only use trips that were dedicated to sablefish... above anlysis says this is smart
         !is.na(sell_date) & 
           !is.na(mean_hook_spacing) & #!is.na(hook_space) & 
           !is.na(sable_lbs_set) &
           # !is.na(start_lon) & !is.na(start_lon) & #remove this because this is at the set level... 
           !is.na(trip_soak) & !is.na(trip_depth) &
           !is.na(mean_hook_size) &   #!is.na(hook_size) &
           hook_size != "MIX" &
           trip_soak > 0 & !is.na(trip_soak) & # soak time in hrs
           julian_day_sell > 226 & # if there were special projects before the fishery opened
           no_hooks_p_set < 15000 & # 15000 in Kray's scripts - 14370 is the 75th percentile
           Stat %in% c("345603", "345631", "345702",
                       "335701", "345701", "345731", "345803")) %>% 
  
  mutate(Year = factor(year), 
         Gear = factor(gear),
         Adfg = factor(Adfg),
         Stat = factor(Stat),
         Stat = fct_relevel(Stat,
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
         #Need to calculate corrected set landing and flag those sets where skippers
         #recorded releases and retentions in different formats (ie, numbers for one 
         # lbs for the other)
         set_catch_flag = ifelse(Inf %in% est_released_lbs,
                                 "CPUE underestimated",
                                 ifelse(Inf %in% est_rel_lbs_nos,
                                        "CPUE underestimated",
                                        "good")),
         
         log_data_form = ifelse(length(unique(is.na(logged_no))) > 1,
                                "mixed",
                                ifelse(unique(is.na(logged_no)),"lbs","nos")),
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
         #std_hooks = 2.2 * no_hooks_p_set * (1 - exp(-0.57 * (hook_space / 39.37))), 
         std_hooks = 2.2 * no_hooks_p_set * (1 - exp(-0.57 * (mean_hook_spacing / 39.37))),
         std_cpue = set_catch / std_hooks,
         
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
         Adfg, Spp_cde, set_soak, set_length, Gear, gear_name, hook_size, mean_hook_size, 
         hook_space, mean_hook_spacing, size, mean_size, no_hooks_exact, no_hooks_est,
         start_lat,start_lon,
         no_hooks_p_set, Stat, set_depth, set_no, no_sets, multigear_trip,
         trip_set_targets, trip_recorded_releases, multi_gear_config, disposition,
         set_depredation, trip_depredation, p_sets_depredated, catch, logged_no,
         logged_lbs, sable_lbs_set, start_lat, start_lon, set_target, set_target_2,
         trip_target, trip_target_2, logged_and_landed, logged_and_landed_nos,
         logged_and_released, logged_and_released_nos, adj_logged_lbs, est_released_lbs,
         adj_logged_nos, est_rel_lbs_nos, Depr_sum, Hook_size, Mean_hook_size,
         set_catch_flag, log_data_form, 
         set_landings = set_catch_no_releases, 
         total_set_catch = set_catch_w_releases,
         set_releases, prop_released,
         std_hooks, std_cpue,skipper_bias_bytrip, dum, dumstat, total_vessels, total_trips) %>%
  filter(disposition == "Retained", #release info now in columns so can get rid of those records
         total_set_catch != Inf) -> ll_cpue_log #getting rid of those mixed (no/lbs) logbook entries... 

histogram(ll_cpue_log$std_cpue, breaks=100); min(ll_cpue_log$std_cpue)
nrow(ll_cpue_log %>% filter(std_cpue == min(ll_cpue_log$std_cpue)))

random_check(ll_cpue_log %>% filter(cpue == min(ll_cpue_log$std_cpue)))
#OK... this is happening when skippers logged some landings in lbs and some in numbers
# not many... 61 records, so lets purge... 
nrow(ll_cpue_log)
ll_cpue_log <- ll_cpue_log %>% filter(std_cpue > min(ll_cpue_log$std_cpue))
nrow(ll_cpue_log)

sel<-sample(nrow(ll_cpue_log),1)# eg16473
eg<-as.data.frame(ll_cpue_log[ll_cpue_log$sell_date == ll_cpue_log$sell_date[sel] &
                                ll_cpue_log$Adfg == ll_cpue_log$Adfg[sel],])
#need to colapse duplicate columns with release data now that its captured
eg
eg %>% filter(disposition == "Retained")
unique(eg %>% filter(disposition == "Retained"))


#--------------------------------------------------------------------------------
colnames(ll_cpue_log)
unique(ll_cpue_log$trip_set_targets)

ll_cpue_log %>% filter(trip_target == "Sablefish") %>%
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
  lims(y = c(0, 1.25))  + 
  scale_color_viridis_d(name = "CPUE derivation",
                        labels = c("Adjusted logbooks","Fish tickets"),
                        option = "C", begin=0,end=0.65) +
  scale_fill_viridis_d(name = "CPUE derivation",
                       labels = c("Adjusted logbooks","Fish tickets"),
                       option = "C", begin=0,end=0.65) +
  theme(legend.position = "bottom")

ggsave(paste0(YEAR+1,"/figures/llcpue_by_derivation2_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")


#----
ll_cpue_log %>% filter(trip_target == "Sablefish") %>%
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
  lims(y = c(0, 2)) + 
  scale_color_viridis_d(name = "CPUE derivation",
                        labels = c("Adjusted logbooks","Fish tickets"),
                        option = "C", begin=0,end=0.65) +
  scale_fill_viridis_d(name = "CPUE derivation",
                       labels = c("Adjusted logbooks","Fish tickets"),
                       option = "C", begin=0,end=0.65) +
  theme(legend.position = "bottom")

ggsave(paste0(YEAR+1,"/figures/llcpue_byrelease_and_datasource_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

#---- 
depr_eff<-lm(data=ll_cpue_log, std_cpue ~ p_sets_depredated)
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
  lims(y = c(0, 2))

ggsave(paste0(YEAR+1,"/figures/llcpue_log_bootCI_bydepr_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

ll_cpue_log %>% filter(trip_target == "Sablefish") %>%
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
  lims(y = c(0, 1.5))

ggsave(paste0(YEAR+1,"/figures/fshcpue_log_bootCI_bygeartrip_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

# Normality

#for analysis we will use CPUE from trips that targetted only sablefish and that
# experienced no depredation... 
ll_cpue_log_clean <-ll_cpue_log %>% 
  filter(trip_set_targets == "all_Sablefish",
         set_depredation == "No depredation")
# Long right tail
ggplot(ll_cpue_log_clean, aes(std_cpue)) + geom_density(alpha = 0.4, fill = 4)

# Better, but still not normal with log transformation
ggplot(ll_cpue_log_clean, aes(log(std_cpue + 1))) + geom_density(alpha = 0.4, fill = 4)

# Following Jenny Stahl and Ben Williams' work in the SSEI, increase CPUE by 10%
# of the mean per Cambell et al 1996 and Cambell 2004. Back-transform with
# exp(cpue - mean(fsh_cpue$std_cpue) * 0.1)
ll_cpue_log_clean %>% 
  mutate(cpue = log(std_cpue + (mean(ll_cpue_log_clean$std_cpue, na.rm=T) * 0.1))) -> ll_cpue_log_clean

ggplot(ll_cpue_log_clean, aes(cpue)) + geom_density(alpha = 0.4, fill = 4)

# EDA for GAM 

# Trends over time
ggplot(ll_cpue_log_clean, aes(year, std_cpue)) + geom_boxplot()

# Trends over time by area
ggplot(ll_cpue_log_clean %>% 
         filter(Stat %in% c("345603", "345631", "345701", "345731")), aes(Stat, std_cpue, fill = year)) + 
  geom_boxplot() +
  scale_fill_manual(values = rev(colorspace::sequential_hcl(c = 0, l = c(30, 90), 
                                                            power = c(1/5, 1.3), 
                                                            n_distinct(ll_cpue$year))),
                    guide = FALSE) +
  labs(x = NULL, y = "Fishery CPUE (round pounds per hook)\n")

ggsave(paste0(YEAR+1,"/figures/llcpue_trendsbyStat_logbooks_",min(as.numeric(ll_cpue_log_clean$year)), "_", YEAR, ".png"), 
       dpi=400, height=4, width=7.5, units="in")

#2022: Note that 2020 and 2021 have some very high CPUE outliers with new query from Justin...
#or... with recruitment of '13-'16 year classes there is far more variability based on discard
# behavior as some fisherman clean up on small fish (but most opt not to); could check the outlier
# vessel if there were lengths... but just poundage... 
# after review and conversation, outliers belonged to 2nd gen knowledgable and "honest" fisherman
# and data is considered legit.  Furthermore, 2022 reboot shows high cpues in the past now so
# seems to be OK... 

# No one fished in Fred. Sound in 2018, 2 in 2019, 4 in 2021
ll_cpue_log_clean %>% filter(Stat %in% c("345702", "335701") & year == YEAR) %>% distinct(Adfg)
# Activity in N Chatham 
ll_cpue_log_clean %>% filter(Stat %in% c("345731", "345803") & year == YEAR) %>% distinct(Adfg)
# Activity in S Chatham
ll_cpue_log_clean %>% filter(Stat %in% c("345603")) %>% group_by(year) %>%  dplyr::summarize(n_distinct(Adfg)) %>% View()
ll_cpue_log_clean %>% filter(Stat %in% c("335701")) %>% group_by(year) %>%  dplyr::summarize(n_distinct(Adfg)) %>% View()

ll_cpue_log_clean %>% 
  group_by(year, Stat) %>% 
  dplyr::summarize(trips = n_distinct(trip_no),
                   vessels = n_distinct(trip_no)) -> stat_sum2

# Gear performance by Stat
ggplot(ll_cpue_log_clean, aes(Stat, cpue, fill = Gear)) + geom_boxplot()

# Gear performance over time
ggplot(ll_cpue_log_clean, aes(year, cpue, fill = Gear)) + geom_boxplot() +
  theme(axis.text.x = element_text(size = 14, angle = 90, h = 1)) +
  labs(x = "", y = "Fishery CPUE\n")

# Only a handful of vessels with autobaiter gear
ggplot(ll_cpue_log_clean, aes(Adfg, cpue, color = Gear)) + geom_jitter(alpha=.4) +
  theme(axis.text.x = element_text(colour = "white"))


#--------------------------------------------------------------------------------
# RELEASE analysis
# now that we've looked at logbook data, lets see what we can glean about release
# behavior from the skippers who were good about recording that data... 
colnames(ll_cpue_log_clean)

#release trends
ll_cpue_log_clean %>% filter(trip_target == "Sablefish") %>%
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
  labs(x = "", y = "Releases/trip (lbs)\n") +
  lims(y = c(0, max(plot_boot10$Upper))) +
  ggtitle("Mean fishery logged and inferred releases per trip (lbs)\n") -> rel_p_trip

ll_cpue_log_clean %>% filter(trip_target == "Sablefish") %>%
  group_by(year) %>%
  do(data.frame(rbind(smean.cl.boot(.$prop_released)))) -> plot_boot11 #view(plot_boot7)

ggplot(plot_boot11) +
  geom_ribbon(aes(x = as.numeric(as.character(year)), ymin = Lower, ymax = Upper), 
              alpha = 0.1, fill = "grey55") +
  #alpha = 0.1) +
  geom_point(aes(x = as.numeric(as.character(year)), y = Mean), size = 1) +
  geom_line(aes(x = as.numeric(as.character(year)), y = Mean)) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Proportion released") +
  lims(y = c(0, 0.04)) +
  ggtitle("Proportion of catch recorded as released") -> prop_rel_p_trip

ggplot(ll_cpue_log_clean, aes(year, set_releases)) + geom_boxplot()

# Trends over time by area
ggplot(ll_cpue_log_clean %>% 
         filter(Stat %in% c("345603", "345631", "345701", "345731")), aes(Stat, set_releases, fill = year)) + 
  geom_boxplot() +
  scale_fill_manual(values = rev(colorspace::sequential_hcl(c = 0, l = c(30, 90), 
                                                            power = c(1/5, 1.3), 
                                                            n_distinct(ll_cpue$year))),
                    guide = FALSE) +
  labs(x = NULL, y = "Fishery releases (lbs)\n")

#numbers
#proportions relative to landings
#proportions of sets that recorded releases

ll_cpue_log_clean %>% group_by (year) %>%
  dplyr::summarise(by = "sets",
                   n_no_releases = sum(set_releases == 0),
                   Tot_n = n(),
                   n_releases = sum(set_releases > 0),
                   prop_w_releases = n_releases/n(),
                   sd_prop = sqrt(prop_w_releases*(1-prop_w_releases)/n()))  -> sets_by_year

#proportions of trips that recorded releases
ll_cpue_ftx_clean %>% group_by (year) %>%
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
  labs(x = "", y = "Proportion of trips/ sets") +
  lims(y = c(0, 0.5)) + 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
        legend.position = c(0.1,0.75)) +
  guides(fill = guide_legend(title=""),
         color = guide_legend(title="")) +
  ggtitle("Proportion of trips or sets that recorded releases") -> prop_trip_released

#I think that will suffice for understanding release behaviour for now
# Spike in 2019 was the result in regulatory changes that required more accurate
# logging of release behaviour. 
library(ggpubr)
ggarrange(rel_p_trip,prop_rel_p_trip,prop_trip_released,
          nrow=3, ncol=1)

ggsave(paste0(YEAR+1,"/figures/llcpue_release_trends_",min(as.numeric(ll_cpue_log_clean$year)), "_", YEAR, ".png"), 
       dpi=400, height=8, width=7.5, units="in")
#-------------------------------------------------------------------------------
# SKIPPER bias...

ggplot(ll_cpue_log_clean %>% filter(!is.na(skipper_bias_bytrip)),
       aes(skipper_bias_bytrip,year,group=year, fill=year)) +
  geom_density_ridges(aes(point_fill = year, point_color = year),
                      alpha = 0.3, scale=1.5, jittered_points = FALSE,
                      rel_min_height = 0.01) +
  #  geom_vline(xintercept = 400, linetype = 4) +
  xlim(-100, 150) + 
  xlab("% bias of logbook relative to landed lbs") + 
  ylab(NULL) +
  #ylim(1985,2021) +
  # scale_y_reverse() +
  theme(legend.position = "none") + 
  theme(text=element_text(size=20),
        axis.text.x = element_text(size=10,angle=45, hjust=1),
        axis.text.y = element_text(size=12)) +
  #facet_wrap(~ Groundfish.Management.Area.Code + Sex) + 
  #facet_grid(Groundfish.Management.Area.Code ~ Sex)+	#+
  facet_wrap(~ trip_recorded_releases)  
  #facet_grid(GFMU ~ Sex)+	
  #scale_x_continuous(limits=c(300,900),breaks = c(seq(from=300, to=900, by=50))) +	#, labels = axisx$labels) +
  #scale_y_continuous(limits=c(1984,2022),breaks = c(seq(from=1984, to=2024, by=2)))#scales::pretty_breaks(n=15)) 
  
  ggsave(paste0(YEAR+1,"/figures/ll_skipper_bias_trends_",min(as.numeric(ll_cpue_log_clean$year)), "_", YEAR, ".png"), 
         dpi=400, height=8, width=5, units="in")

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# CPUE Calc and exam using LOG BOOK DATA... 
#--------------------------------------------------------------------------------
# HOOK SIZE performance - hook size 11 should be removed due to sample size and
# infrequency of use. Probably size 7 too - 4 vessels fished size 7 hooks in
# 1997, and only 1 vessel fished it until 2004
# 2022: also size 6 hooks ... half as many as size 7
colnames(ll_cpue_log_clean)

fsh_cpue_ll_lb<-ll_cpue_log_clean

#new filter includes mixed hook size sets... need to cull those for analysis if
# hook size is included... 
str(fsh_cpue_ll_lb$Hook_size)
lb_cpue_hooks<-fsh_cpue_ll_lb %>% 
  filter(Hook_size %in% c("6","7","11","12","13","14","15","16")) %>%
  mutate(Hook_size = fct_relevel(Hook_size,c("6","7","11","12","13","14","15","16")))

lb_cpue_hooks %>% filter(Hook_size == "7") %>% distinct(Adfg, Stat, year)
lb_cpue_hooks %>% filter(Hook_size == "6") %>% distinct(Adfg, Stat, year)

# PJ22 obs: before censoring hooks run analysis with them in there... they actually don't
# make a big difference and can likely be ignored... pj2022
# fsh_cpue %>% filter(!Hook_size %in% c("11", "7", "6")) -> fsh_cpue
lb_cpue_hooks %>% filter(!Hook_size %in% c("6","7","11")) -> lb_cpue_hooks

# Not much contrast in CPUE between remaining hook sizes... maybe some
# difference in performance between years/areas
ggplot(lb_cpue_hooks, aes(Hook_size, cpue)) + geom_boxplot()
ggplot(lb_cpue_hooks, aes(year, cpue, fill = Hook_size)) + geom_boxplot()+
  theme(axis.text.x = element_text(size = 14, angle = 90, h = 1)) +
  labs(x = "", y = "Fishery CPUE\n")
ggplot(lb_cpue_hooks, aes(Stat, cpue, fill = Hook_size)) + geom_boxplot()+
  labs(x = "\nStat area", y = "Fishery CPUE\n")
# New hook size 6 in 2019, vessel look up:
fsh_cpue %>% filter(Hook_size == "6") %>% distinct(Adfg, cpue)

#**2022: I think the best way to get CPUE is cull 6,7, and 11 (<100 samples) and
# then ignore hook size in estimating CPUE since it doesn't make a difference
# (even if model fits better)

fsh_cpue_lb<-lb_cpue_hooks %>%
  filter(!Hook_size %in% c("6","7","11")) 

colnames(fsh_cpue_lb)

#quick look at variables
nrow(fsh_cpue_lb)
histogram(fsh_cpue_lb$set_depth); nrow(fsh_cpue_lb[is.na(fsh_cpue_lb$set_depth),]); min(fsh_cpue_lb$set_depth)
histogram(fsh_cpue_lb$set_soak, breaks=100); nrow(fsh_cpue_lb[is.na(fsh_cpue_lb$set_soak),]); min(fsh_cpue_lb$set_soak)
#FLAG some negative soak times
nrow(fsh_cpue_lb %>% filter(set_soak < 0))


histogram(fsh_cpue_lb$set_length, breaks=100); nrow(fsh_cpue_lb[is.na(fsh_cpue_lb$set_length),]); min(fsh_cpue_lb$set_length)
histogram(fsh_cpue_lb$set_length[fsh_cpue_lb$set_length<50], breaks=100)
nrow(fsh_cpue_lb[fsh_cpue_lb$set_length == 0,])

# Depth - clear increasing trend, asymptotes ~ 450 m
ggplot(fsh_cpue_lb %>% filter(!is.na(set_depth))
         , aes(set_depth, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 2, se = FALSE) 

# Soak time - cut off at 40 hrs b/c it looks like there's a slight outlier
# effect
# hmmm ... debatable.  Outliers, but legit?  probably long soaks do to weather or other issues?

ggplot(fsh_cpue_lb %>% filter(!(set_soak <= 0)),
       aes(set_soak, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 2, se = FALSE) 
fsh_cpue_lb %>% filter(set_soak < 40 & set_soak > 0) -> fsh_cpue_lb

#km fished per set
ggplot(fsh_cpue_lb, aes(set_length, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 2, se = FALSE)
ggplot(fsh_cpue_lb %>% filter(set_length > 0), aes(set_length, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 2, se = FALSE)
ggplot(fsh_cpue_lb %>% filter(set_length > 0 & set_length < 200), aes(set_length, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 2, se = FALSE)

#to standardize for all of this need to filter out some set length and soak time shit data... 
nrow(fsh_cpue_lb)
nrow(fsh_cpue_lb %>% filter(set_length < 200,
                            set_length > 0,
                            set_soak > 0))

fsh_cpue_lb_good <- fsh_cpue_lb %>% filter(set_length < 200,
                                           set_length > 0,
                                           set_soak > 0)

# similar to soak time trends... Jane is right about the scavengers down there! 

# Inconsistent and very slight latitudinal effect #!!! NEED TO GET LAT LONG DATA  ???
# 2023 change: with fish tickets will ignore lat long and just use stat area as spatial variable
ggplot(fsh_cpue_lb_good, aes(start_lat, cpue, group = year, colour = year)) +
  geom_smooth(method = 'loess', span = 1, se = FALSE) 

# Inconsistent and very slight seasonal effect
ggplot(fsh_cpue_lb_good, aes(julian_day_sell, cpue, group = year, colour = year)) +
  geom_smooth(method = 'loess', span = 1, se = FALSE) 

# By stat area... 
ggplot(fsh_cpue_lb_good, aes(Stat, cpue)) + geom_boxplot()+
  labs(x = "\nStat area", y = "Fishery CPUE\n")

# GAM cpue ---

# Potential variables influencing CPUE (ultimately interested in estimating a
# Year effect):
# depth - increase in CPUE up to ~ 450 m, then asymptote. Very clear and
# consistent trend between years
# julian_day - decrease towards the end of the season? EDA suggested there is no
# consistent seasonal trend. If there is a trend, its slightly decreasing over
# the season.  pj22: I agree with slight decreasing trend through season
# soak time and total km fished have similar patterns... 
# Adfg - vessel effect, some are better fishermen than others. Routinely
# improves model fit and doesn't grossly violate assumptions.
# Gear - higher for conventional gear (01) over autobaiter (06) consistently
# between years, although this becomes dampened with the inclusion on the vessel
# effect
# Hook_size - optimal hook size? No consistent trend between years. Treat as random
# effect (Pj22: not sure I agree with random effect designation?  )
# start_lat - is there some consistent trend in Chatham Strait going north into
# Chatham? Not one that is consistent between years. If one exists it tends to
# be decreasing with latitude. There was no spatial autocorrelation detected (done in previous analysis).
# start_lat/start_lon - spatial autocorrelation  (need to check on this - pj22)

fsh_cpue_lb_good <- fsh_cpue_lb_good %>% mutate(Year = year)

# So we know we like year (duh) and gear (autobaiters clearly suck).
# lets start by adding each of the other variables by themselves

m0 <- bam(cpue ~ Year + Gear, data=fsh_cpue_lb_good, gamma=1.4)
m0.hook <- bam(cpue ~ Year + Gear + Hook_size, data=fsh_cpue_lb_good, gamma=1.4)
m0.depth <- bam(cpue ~ Year + Gear + s(set_depth, k=4), data=fsh_cpue_lb_good, gamma=1.4)
m0.soak <- bam(cpue ~ Year + Gear + s(set_soak, k=4) , data=fsh_cpue_lb_good, gamma=1.4)
m0.stat <- bam(cpue ~ Year + Gear + s(Stat, bs='re', by=dumstat), data=fsh_cpue_lb_good, gamma=1.4)
m0.adfg <- bam(cpue ~ Year + Gear + s(Adfg, bs='re', by=dum), data=fsh_cpue_lb_good, gamma=1.4)
m0.lat_lon <- bam(cpue ~ Year + Gear + te(start_lon, start_lat), data=fsh_cpue_lb_good, gamma=1.4)
m0.lat <- bam(cpue ~ Year + Gear + s(start_lat), data=fsh_cpue_lb_good, gamma=1.4)
m0.lon <- bam(cpue ~ Year + Gear + s(start_lon), data=fsh_cpue_lb_good, gamma=1.4)
m0.jday <- bam(cpue ~ Year + Gear + s(julian_day_sell, k=4), data=fsh_cpue_lb_good, gamma=1.4)
m0.length <- bam(cpue ~ Year + Gear + s(set_length), data=fsh_cpue_lb_good, gamma=1.4)

model.list<-list(m0,m0.hook,m0.depth,m0.soak,m0.stat,m0.adfg,m0.lat_lon,
                 m0.lat,m0.lon,m0.jday,m0.length)
names(model.list)<-c("m0","hook","depth","soak","stat","adfg","lat_lon",
                     "lat","lon","jday","length")
modsum0<-data.frame(); j<-1
for (i in model.list) {
  #mod<-i
  modsum0[j,"model"]<-names(model.list[j])
  modsum0[j,"aic"]<-AIC(i)
  modsum0[j,"dev"]<-summary(i)$dev.expl
  modsum0[j,"rsq"]<-summary(i)$r.sq
  modsum0[j,"dev_exp"]<-summary(i)$dev.expl  - summary(m0)$dev.expl
  j<-j+1
}

modsum0 %>% arrange(aic)  
modsum0 %>% arrange(-dev)  
modsum0 %>% arrange(-rsq) 
modsum0 %>% arrange(-dev_exp) 

# 2023: dominant variable is adfg followed by depth and then lat_long, long and stat
# spatial variability thus at the top... 3-d when you think about depth... 
# but all variables better than the null... 

global<-bam(cpue ~ Year + Gear + Hook_size + s(set_depth, k=4) + s(set_soak, k=4) + 
              s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum) +
              + te(start_lon, start_lat) + s(julian_day_sell, k=4) + s(set_length),
            data=fsh_cpue_lb_good, gamma=1.4)
global_lat<-bam(cpue ~ Year + Gear + Hook_size + s(set_depth, k=4) + s(set_soak, k=4) + 
              s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum) +
              + s(start_lat) + s(julian_day_sell, k=4) + s(set_length),
            data=fsh_cpue_lb_good, gamma=1.4)
global_lon<-bam(cpue ~ Year + Gear + Hook_size + s(set_depth, k=4) + s(set_soak, k=4) + 
              s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum) +
              + s(start_lon) + s(julian_day_sell, k=4) + s(set_length),
            data=fsh_cpue_lb_good, gamma=1.4)
AIC(global,global_lat,global_lon) #way better than simple models

plot(global, page = 1, shade = TRUE, resid = TRUE, all = TRUE)
summary(global)

# No residual patterns, but may be some outliers
plot(fitted(global), resid(global))
abline(h = 0, col = "red", lty = 2)

# 14 outliers, get rid of them and refit models with new data set
which(fitted(global) < -1.5)   
length(which(fitted(global) < -1.5)) #32 outliers in 2023
not_outliers <- which(fitted(global) >= -1.5)
fsh_cpue_lb_good <- fsh_cpue_lb_good %>% 
  slice(not_outliers)

vcov.gam(global)

# Determine if random variables should be included (Stat and Adfg)
#try some multimodel inference with glms...  
library(lme4)
library(arm)
library(plyr)
library(MuMIn)   #r.squaredGLMM(object)

library(glmulti)
library(AICcmodavg)


lmer.glmulti <- function (formula, data, random = "", ...) {
  lmer(paste(deparse(formula), random), data = data, REML=F, ...)
}

setMethod('getfit', 'merMod', function(object, ...) {
  summ<-summary(object)$coef
  summ1<-summ[,1:2]
  if (length(dimnames(summ)[[1]])==1) {
    summ1<-matrix(summ1, nr=1, dimnames=list(c("(Intercept)"),c("Estimate","Std.Error")))
  }
  cbind(summ1, df=rep(10000,length(fixef(object))))
})

old<-Sys.time()  ##!!! 4 hours
runmodels<-glmulti(cpue~Year*Gear*Hook_size*set_depth*set_soak*start_lon*start_lat*julian_day_sell*set_length,#s(set_depth, k=4), 
             data=fsh_cpue_lb_good,
             method="h",imm=0.5, sexrate=0.1, crit=aicc , deltaM=0.01, 
             conseq=20, marginality=T, maxsize=7, confsetsize=100, includeobjects = TRUE, 
             level=2, fitfunc=lmer.glmulti, random="+(1|Stat)+(1|Adfg)"); Sys.time() - old
coef.glmulti(runmodels)
plot(runmodels,type="s")
plot(runmodels,type="w")
plot(runmodels,type="p")
plot(runmodels,type="r")

#So... the glm exercise show year, jday, gear and depth interaction as most important factors
# glm sees everything else as just noise... 

##--------
#OK, back to more appropriate additive models.. 

m1 <- bam(cpue ~ Year + Gear + Hook_size + s(set_depth, k=4) + s(set_soak, k=4) + s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum), data=fsh_cpue_lb_good, gamma=1.4)
m2 <- bam(cpue ~ Year + Gear + Hook_size + s(set_depth, k=4) + s(set_soak, k=4) + s(Adfg, bs='re', by=dum), data=fsh_cpue_lb_good, gamma=1.4)
m3 <- bam(cpue ~ Year + Gear + Hook_size + s(set_depth, k=4) + s(set_soak, k=4) +s(Stat, bs='re', by=dumstat), data=fsh_cpue_lb_good, gamma=1.4)
m4 <- bam(cpue ~ Year + Gear + Hook_size + s(set_depth, k=4) + s(set_soak, k=4), data=fsh_cpue_lb_good, gamma=1.4)

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
# random noise associated with the variable, hook size has a plausible effect on the response variable?
m5 <- bam(cpue ~ Year + Gear + s(set_depth, k=4) + s(set_soak, k=4) +    #no hook size
            s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum), data=fsh_cpue_lb_good, gamma=1.4)
m6 <- bam(cpue ~ Year + Gear + s(set_depth, k=4) + s(set_soak, k=4) +    #hook size as a random variable
            s(Hook_size, bs='re', by=dum) + s(Stat, bs='re', by=dumstat)+ 
            s(Adfg, bs='re', by=dum), data=fsh_cpue_lb_good, gamma=1.4)

AIC(m1, m2, m3, m4, m5, m6)

plot(m6, page = 1, shade = TRUE, resid = TRUE, all = TRUE)
summary(m5)
summary(m6)

# By AIC, treating Hooksize as a factor has the best predictive pwr, but the
# model treating it as a random effect is a close second. Inclusion of hook size
# as a factor or re results in no change in the deviance explained. Because
# there's no strong trend or difference between hook sizes and it seems just to
# account up some of the random variation, I'm going carry m6 forward (the model
# with the re for hook size).
#2022:  OK, same results.  get Jane's point about no strong trend with hook size
#       but am disinclined to treat it as a random effect.  HS is not a random category
#       or group... there is an effect of HS on catchability.... 
#       I will carry model 1 forward instead of model 6.
 
{
  #Determine whether to include lat and long
  m7 <- bam(cpue ~ Year + Gear + s(set_depth, k=4) + s(set_soak, k=4) + 
              Hook_size + #s(Hook_size, bs='re', by=dum) + 
              s(Stat, bs='re', by=dumstat) + 
              s(Adfg, bs='re', by=dum) + te(start_lon, start_lat), data=fsh_cpue_lb_good, gamma=1.4)
  m8 <- bam(cpue ~ Year + Gear + s(set_depth, k=4) + s(set_soak) + 
              Hook_size + #s(Hook_size, bs='re', by=dum) + 
              s(Stat, bs='re', by=dumstat) + 
              s(Adfg, bs='re', by=dum) + s(start_lon), data=fsh_cpue_lb_good, gamma=1.4)
  m9 <- bam(cpue ~ Year + Gear + s(set_depth, k=4) + s(set_soak, k=4) + 
              Hook_size + #s(Hook_size, bs='re', by=dum) + 
              s(Stat, bs='re', by=dumstat) + 
              s(Adfg, bs='re', by=dum) + s(start_lat), data=fsh_cpue_lb_good, gamma=1.4)
  
  AIC(m1, m6, m7, m8, m9) #AIC(m6, m7, m8, m9)
  
  summary(m7)
  summary(m8)
  summary(m9)
  
  # m9, the model with the latitudinal effect, performs best by AIC, but only
  # results in a slight improvement in the dev explained. Try limiting the number
  # of knots to guard against overfitting... but m9 still performs best by AIC.
  # Phil note: knots = k
  # Phil note; tensor smoother allows integration of two variables (lat and lon here)
  # same in 2022
  m10 <- bam(cpue ~ Year + Gear + s(set_depth, k=4) + s(set_soak, k=4) + 
               s(Hook_size, bs='re', by=dum) + s(Stat, bs='re', by=dumstat) + 
               s(Adfg, bs='re', by=dum) + s(start_lat, k=6), data=fsh_cpue_lb_good, gamma=1.4)
  AIC(m1, m6, m7, m8, m9, m10)
  
  plot(m9, page = 1, shade = TRUE, all = TRUE) #resid = TRUE,
  plot(m10, page = 1, shade = TRUE, all = TRUE) #resid = TRUE,
  
  # m7 with both lat and lon with a tensor smoother has the second best
  # performance. red/orange is higher CPUE, green average and blue lower; can
  # change "too.far" values to change what shows on graph. Highest cpue in the
  # north, south and central chatham
  vis.gam(m7, c('start_lon','start_lat'), type='response', plot.type='contour', color='topo', too.far=0.1)
}
# The inclusion of a seasonal effect  improves model fit - there is a
# slightly decreasing trend in cpue on average over the course of the season.
m11 <- bam(cpue ~ Year + Gear + s(julian_day_sell, k=4) + s(set_depth, k=4) + 
             s(set_soak, k=4) + #s(start_lat) + 
             Hook_size + #s(Hook_size, bs='re', by=dum) + 
             s(Stat, bs='re', by=dumstat) + s(Adfg, bs='re', by=dum), data=fsh_cpue_lb_good, gamma=1.4)
AIC(m1, m11) #AIC(m1, m7, m9, m11)
summary(m11)
plot(m11, page = 1, shade = TRUE, all = TRUE) #resid = TRUE,

# # Relationship between depth and soak time - highest cpue in > 450 m
# # and ~ 10 hr soak time
vis.gam(m11, c('set_depth', 'set_soak'), plot.type='contour', type='response', color='topo', too.far=0.1)

#In 2023 we also want to see if the total km fished makes a difference... 
m12 <- bam(cpue ~ Year + Gear + s(julian_day_sell, k=4) + s(set_depth, k=4) + 
             s(set_soak, k=4) + s(set_length) + 
             Hook_size + #s(Hook_size, bs='re', by=dum) + 
             s(Stat, bs='re', by=dumstat) + s(Adfg, bs='re', by=dum), data=fsh_cpue_lb_good, gamma=1.4)

AIC(m1, m9, m11, m12) #AIC(m1, m7, m9, m11)
sort(c(AIC(m1,m2,m3,m4,m5,m6,m11,m12)))
AIC(m1,m2,m3,m4,m5,m6,m11,m12)

summary(m12)
plot(m12, page = 1, shade = TRUE, all = TRUE) #resid = TRUE,

vis.gam(m12, c('set_depth', 'set_length'), plot.type='contour', type='response', color='topo', too.far=0.1)
vis.gam(m12, c('set_soak', 'set_length'), plot.type='contour', type='response', color='topo', too.far=0.1)

plot.gam(m12)

str(diag(vcov.gam(m12)))

#more complex models to finish the exam

glob_drop_length<-bam(cpue ~ Year + Gear + Hook_size + s(set_depth, k=4) + s(set_soak, k=4) + 
              s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum) +
              + te(start_lon, start_lat) + s(julian_day_sell, k=4) ,
            data=fsh_cpue_lb_good, gamma=1.4)
globlat_drop_length<-bam(cpue ~ Year + Gear + Hook_size + s(set_depth, k=4) + s(set_soak, k=4) + 
                  s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum) +
                  + s(start_lat) + s(julian_day_sell, k=4) ,
                data=fsh_cpue_lb_good, gamma=1.4)
globlon_drop_length<-bam(cpue ~ Year + Gear + Hook_size + s(set_depth, k=4) + s(set_soak, k=4) + 
                  s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum) +
                  + s(start_lon) + s(julian_day_sell, k=4) ,
                data=fsh_cpue_lb_good, gamma=1.4)
glob_drop_jday<-bam(cpue ~ Year + Gear + Hook_size + s(set_depth, k=4) + s(set_soak, k=4) + 
              s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum) +
              + te(start_lon, start_lat)  + s(set_length),
            data=fsh_cpue_lb_good, gamma=1.4)
globlat_drop_jday<-bam(cpue ~ Year + Gear + Hook_size + s(set_depth, k=4) + s(set_soak, k=4) + 
                  s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum) +
                  + s(start_lat)  + s(set_length),
                data=fsh_cpue_lb_good, gamma=1.4)
globlon_drop_jday<-bam(cpue ~ Year + Gear + Hook_size + s(set_depth, k=4) + s(set_soak, k=4) + 
                  s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum) +
                  + s(start_lon)  + s(set_length),
                data=fsh_cpue_lb_good, gamma=1.4)
glob_drop_hook<-bam(cpue ~ Year + Gear + s(set_depth, k=4) + s(set_soak, k=4) + 
              s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum) +
              + te(start_lon, start_lat) + s(julian_day_sell, k=4) + s(set_length),
            data=fsh_cpue_lb_good, gamma=1.4)
glob_drop_depth<-bam(cpue ~ Year + Gear + Hook_size + s(set_soak, k=4) + 
              s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum) +
              + te(start_lon, start_lat) + s(julian_day_sell, k=4) + s(set_length),
            data=fsh_cpue_lb_good, gamma=1.4)
glob_drop_soak<-bam(cpue ~ Year + Gear + Hook_size + s(set_depth, k=4) +  
              s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum) +
              + te(start_lon, start_lat) + s(julian_day_sell, k=4) + s(set_length),
            data=fsh_cpue_lb_good, gamma=1.4)
glob_drop_stat<-bam(cpue ~ Year + Gear + Hook_size + s(set_depth, k=4) + s(set_soak, k=4) + 
               s(Adfg, bs='re', by=dum) +
              + te(start_lon, start_lat) + s(julian_day_sell, k=4) + s(set_length),
            data=fsh_cpue_lb_good, gamma=1.4)
glob_drop_adfg<-bam(cpue ~ Year + Gear + Hook_size + s(set_depth, k=4) + s(set_soak, k=4) + 
              s(Stat, bs='re', by=dumstat)+ 
              + te(start_lon, start_lat) + s(julian_day_sell, k=4) + s(set_length),
            data=fsh_cpue_lb_good, gamma=1.4)
glob_drop_latlon<-bam(cpue ~ Year + Gear + Hook_size + s(set_depth, k=4) + s(set_soak, k=4) + 
              s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum) +
               s(julian_day_sell, k=4) + s(set_length),
            data=fsh_cpue_lb_good, gamma=1.4)

model.list<-list(global,global_lat,global_lon,
                 m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,
                 glob_drop_length,globlat_drop_length,globlon_drop_length,
                 glob_drop_jday,globlat_drop_jday,globlon_drop_jday,
                 glob_drop_hook,glob_drop_depth,glob_drop_soak,
                 glob_drop_stat,glob_drop_adfg,glob_drop_latlon)
names(model.list)<-c("global","global_lat","global_lon",
                     "m1","m2","m3","m4","m5","m6",
                     "m7","m8","m9","m10","m11","m12",
                     "glob_drop_length","globlat_drop_length","globlon_drop_length",
                     "glob_drop_jday","globlat_drop_jday","globlon_drop_jday",
                     "glob_drop_hook","glob_drop_depth","glob_drop_soak",
                     "glob_drop_stat","glob_drop_adfg","glob_drop_latlon")
names(model.list[1])
modsum<-data.frame(); j<-1
for (i in model.list) {
  #mod<-i
  modsum[j,"model"]<-names(model.list[j])
  modsum[j,"aic"]<-AIC(i)
  modsum[j,"delta_aic"]<-AIC(global)-AIC(i)
  modsum[j,"dev"]<-summary(i)$dev.expl
  modsum[j,"rsq"]<-summary(i)$r.sq
  modsum[j,"dev_exp"]<-summary(global)$dev.expl  - summary(i)$dev.expl
  j<-j+1
}

modsum %>% arrange(aic)  
modsum %>% arrange(-dev)  
modsum %>% arrange(-rsq) 
modsum %>% 
  filter (model %in% c("global","glob_drop_length","globlat_drop_length","globlon_drop_length",
                       "glob_drop_jday","globlat_drop_jday","globlon_drop_jday",
                       "glob_drop_hook","glob_drop_depth","glob_drop_soak",
                       "glob_drop_stat","glob_drop_adfg","glob_drop_latlon")) %>% 
  arrange(-dev_exp) #dropping adfg, depth and set length results in drop in deviance explained... 

modsum0 %>% arrange(aic) #adfg, depth and spatial variables from this direction... 

plot(glob_drop_stat, page = 1, shade = TRUE, all = TRUE)
plot(global, page = 1, shade = TRUE, all = TRUE)
plot(global_lat, page = 1, shade = TRUE, all = TRUE)

## Model likes a lot of variables... Ochum's razor suggests maybe eliminating some
#  of the minor effects? hook, soak time, sell date and 
#  and keeping one? spatial variable? lat, long, lat_long or stat... 
#  Bottum up suggests lat_lon
#  top down suggests ???
modsum %>% 
  filter (model %in% c("global","global_lat","global_lon",
                       "glob_drop_latlon","glob_drop_stat")) %>%
  arrange(aic)
# seems like most of the variability in Stat is contained in lat_long 

# "best" model according to ochum would be ...
occam<-bam(cpue ~ Year + Gear + s(set_depth, k=4) +  
                    s(Adfg, bs='re', by=dum) +
                    + te(start_lon, start_lat) + s(set_length),
                  data=fsh_cpue_lb_good, gamma=1.4)
AIC(occam); summary(occam)$dev.expl

#fully standardized would be glob_drop_stat... 
full<-glob_drop_stat
AIC(full); summary(full)$dev.expl
# GAM summary ----

# Final model structure (m12) (* = random effect):
# CPUE ~ Year + Gear + s(julian_day_sell, k=4) + s(trip_depth, k=4) + 
#         s(soak_p_set, k=4) + s(total_km_fished) + 
#        Hook_size + 
#  s(Stat, bs='re', by=dumstat) + s(Adfg, bs='re', by=dum)

# 36.7% deviance explained
# 33.9% in 2022 with pj mods.  With Jane's original code get 34.9%
# 44.1% in 2023 for fish tickets and ~36% for logbooks... 

# CPUE decreases throughout the season. CPUE increases with depth, then
# asymptotes ~ 450 m. CPUE is constant and then drops off ~ 10 hr soak time,

# The overall effect of julian day, soak time, and latitude is weaker than
# depth. Conventional gear performs slightly better than autobaiter gear. 

# Predictions ----

#Create standard dataset to get standardized CPUE for each year

std_dat_log <- expand.grid(year = unique(fsh_cpue_lb_good$year),
                       Gear = 'CS',
                       set_depth = mean(fsh_cpue_lb_good$set_depth), 
                       set_soak = 10, 
                       julian_day_sell = median(fsh_cpue_lb_good$julian_day_sell),
                       start_lat = mean(fsh_cpue_lb_good$start_lat),
                       start_lon = mean(fsh_cpue_lb_good$start_lon),
                       set_length = median(fsh_cpue_lb_good$set_length),
                       Stat = "345701",
                       Hook_size = "14",
                       Adfg = "35491",
                       dum = 0,
                       dumstat = 0) %>% 
  mutate(Year = factor(year))

pred_cpue_occam <- predict(occam, std_dat_log, type = "link", se = TRUE)
pred_cpue_full <- predict(full, std_dat_log, type = "link", se = TRUE)
#checking my code with Jane's... checks out :)
preds_occam<-predict.bam(occam, type="response", std_dat_log, se = TRUE)
preds_full<-predict.bam(full, type="response", std_dat_log, se = TRUE)

#Put the standardized CPUE and SE into the data frame and convert to
#backtransformed (bt) CPUE
std_dat_log %>% 
  mutate(fit = pred_cpue_occam$fit,
         se = pred_cpue_occam$se.fit,
         upper = fit + (2 * se),
         lower = fit - (2 * se),
         bt_cpue = exp(fit) - (mean(fsh_cpue_cl$std_cpue) * 0.1),
         bt_upper = exp(upper) - (mean(fsh_cpue_cl$std_cpue) * 0.1),
         bt_lower = exp(lower) - (mean(fsh_cpue_cl$std_cpue) * 0.1),
         bt_se = (bt_upper - bt_cpue) / 2  #,
         #bt_cv = bt_se/bt_cpue
  ) -> std_dat_log_occam

std_dat_log %>% 
  mutate(fit = pred_cpue_full$fit,
         se = pred_cpue_full$se.fit,
         upper = fit + (2 * se),
         lower = fit - (2 * se),
         bt_cpue = exp(fit) - (mean(fsh_cpue_cl$std_cpue) * 0.1),
         bt_upper = exp(upper) - (mean(fsh_cpue_cl$std_cpue) * 0.1),
         bt_lower = exp(lower) - (mean(fsh_cpue_cl$std_cpue) * 0.1),
         bt_se = (bt_upper - bt_cpue) / 2  #,
         #bt_cv = bt_se/bt_cpue
  ) -> std_dat_log_full

# Nominal CPUE ----

fsh_cpue_lb_good %>% 
  group_by(year) %>% 
  dplyr::summarise(fsh_cpue = mean(std_cpue),
                   sd = sd(std_cpue),
                   n = length(std_cpue),
                   se = sd / (n ^ (1/2)),
                   var = var(std_cpue),
                   cv = sd / fsh_cpue,
                   upper = fsh_cpue + (2 * se),
                   lower = fsh_cpue - (2 * se)) -> fsh_sum_log 

# Compare predicted cpue from gam to nominal cpue
fsh_sum_log %>%
  dplyr::select(year, cpue = fsh_cpue, upper, lower) %>% 
  mutate(CPUE = "Nominal", year = as.numeric(as.character(year))) %>% 
  bind_rows(std_dat_log_occam %>% 
              dplyr::select(year, cpue = bt_cpue, upper = bt_upper, lower = bt_lower) %>% 
              mutate(CPUE = "Occam Standardization",
                     year = as.numeric(as.character(year)))) %>% 
  bind_rows(std_dat_log_full %>% 
              dplyr::select(year, cpue = bt_cpue, upper = bt_upper, lower = bt_lower) %>% 
              mutate(CPUE = "Fully Standardized",
                     year = as.numeric(as.character(year)))) %>% 
  ggplot() +
  geom_ribbon(aes(year, ymin = lower, ymax = upper, fill = CPUE), 
              colour = NA, alpha = 0.2) +
  geom_point(aes(year, cpue, colour = CPUE, shape = CPUE), size = 2) +
  geom_line(aes(year, cpue, colour = CPUE, group = CPUE), size = 1) +
  scale_color_viridis_d(option = "E", begin=0.2,end=0.8) +
  scale_fill_viridis_d(option = "E", begin=0.2,end=0.8) +
  scale_shape_manual(values = c(19, 17,18)) +
  #scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  labs(x = "", y = "Fishery CPUE (round lb/hook)\n") +
  theme(legend.position = c(0.8, 0.2)) #+ 
  #scale_x_discrete(breaks=c(1997:YEAR))
  expand_limits(y = 0)

ggsave(paste0(YEAR+1,"/figures/compare_stdcpue_lllog_", YEAR, ".png"), dpi=300, height=4, width=7, units="in")

#compare to the old methods... 

old_cpue<-read_csv("2022/output/fshcpue_1997_2021_nominal_for_fut.csv", 
                   guess_max = 50000) 

fsh_sum_log %>%
  dplyr::select(year, cpue = fsh_cpue, upper, lower) %>% 
  mutate(CPUE = "Nominal", year = as.numeric(as.character(year))) %>% 
  bind_rows(std_dat_log_occam %>% 
              dplyr::select(year, cpue = bt_cpue, upper = bt_upper, lower = bt_lower) %>% 
              mutate(CPUE = "Occam Standardization",
                     year = as.numeric(as.character(year)))) %>% 
  bind_rows(std_dat_log_full %>% 
              dplyr::select(year, cpue = bt_cpue, upper = bt_upper, lower = bt_lower) %>% 
              mutate(CPUE = "Fully Standardized",
                     year = as.numeric(as.character(year)))) %>% 
  bind_rows(old_cpue %>% mutate(year = as.factor(year)) %>%
              select(year, cpue = fsh_cpue, upper, lower) %>% 
              mutate(CPUE = "Pre-2023 Nominal",
                     year = as.numeric(as.character(year)))) %>%
  ggplot() +
  geom_ribbon(aes(year, ymin = lower, ymax = upper, fill = CPUE), 
              colour = NA, alpha = 0.2) +
  geom_point(aes(year, cpue, colour = CPUE, shape = CPUE), size = 2) +
  geom_line(aes(year, cpue, colour = CPUE, group = CPUE), size = 1) +
  # scale_colour_grey(name = "Standardized CPUE") +
  # scale_fill_grey(name = "Standardized CPUE") +
  scale_color_viridis_d(option = "C", begin=0,end=0.85) +
  scale_fill_viridis_d(option = "C", begin=0,end=0.85) +
  scale_shape_manual(values = c(19, 17, 19, 18)) +
  #scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  labs(x = "", y = "Fishery CPUE (round lb/hook)\n") +
  theme(legend.position = c(0.8, 0.2)) + ylim(0,1.2)
  expand_limits(y = 0)

ggsave(paste0(YEAR+1,"/figures/compare_OLD_stdcpue_lllog_", YEAR, ".png"), dpi=300, height=4, width=7, units="in")

# Percent change in fishery nominal cpue compared to a ten year rolling average
fsh_sum_log %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  filter(year > YEAR - 10) %>% 
  mutate(lt_mean = mean(fsh_cpue),
         perc_change_lt = (fsh_cpue - lt_mean) / lt_mean * 100) 

# Percent change in fishery nominal cpue from last year
#not relevant since no CPUE in 2020 due to stupid covid
fsh_sum_log %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  filter(year >= YEAR - 1) %>%
  select(year, fsh_cpue) %>% 
  reshape2::dcast("fsh_cpue" ~ year) -> perc_ch_log

names(perc_ch_log) <- c("cpue", "last_year", "this_year") 
perc_ch_log %>% mutate(perc_change_ly = (`this_year` - `last_year`) / `last_year` * 100)

std_dat_log_full %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  filter(year >= YEAR - 1) %>%
  select(year, fsh_cpue = bt_cpue) %>% 
  reshape2::dcast("fsh_cpue" ~ year) -> perc_ch_log_full

names(perc_ch_log_full) <- c("cpue", "last_year", "this_year") 
perc_ch_log_full %>% mutate(perc_change_ly = (`this_year` - `last_year`) / `last_year` * 100)

#compare logbooks to fish tickets!

fsh_sum_log %>%
  dplyr::select(year, cpue = fsh_cpue, upper, lower) %>% 
  mutate(CPUE = "Nominal", 
         Source = "Logbooks",
         year = as.numeric(as.character(year))) %>% 
  bind_rows(fsh_sum %>% 
              dplyr::select(year, cpue = fsh_cpue, upper = upper, lower = lower) %>% 
              mutate(CPUE = "Nominal",
                     Source = "Fish Tickets",
                     year = as.numeric(as.character(year)))) %>%
  bind_rows(std_dat %>% 
              dplyr::select(year, cpue = bt_cpue, upper = bt_upper, lower = bt_lower) %>% 
              mutate(CPUE = "Fully Standardized",
                     Source = "Fish Tickets",
                     year = as.numeric(as.character(year)))) %>%
  bind_rows(std_dat_log_full %>% 
              dplyr::select(year, cpue = bt_cpue, upper = bt_upper, lower = bt_lower) %>% 
              mutate(CPUE = "Fully Standardized",
                     Source = "Logbooks",
                     year = as.numeric(as.character(year))))  %>%
  ggplot() +
  geom_ribbon(aes(year, ymin = lower, ymax = upper, fill = Source), 
              colour = NA, alpha = 0.2) +
  geom_point(aes(year, cpue, colour = Source, shape = Source), size = 2) +
  geom_line(aes(year, cpue, colour = Source, group = Source), size = 1) +
  facet_wrap(~ CPUE) +
  # scale_fill_grey(name = "Standardized CPUE") +
  scale_color_viridis_d(option = "C", begin=0.2,end=0.75) +
  scale_fill_viridis_d(option = "C", begin=0.2,end=0.75) +
  scale_shape_manual(values = c(19, 17, 19, 18)) +
  #scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  labs(x = "", y = "Fishery CPUE (round lb/hook)\n") +
  theme(legend.position = c(0.8, 0.2)) + ylim(0,1.25)
#expand_limits(y = 0)

ggsave(paste0(YEAR+1,"/figures/compare_llcpue_LOG_vs_FTX_", YEAR, ".png"), dpi=300, height=5, width=8, units="in")


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

data.frame(year = 1980:1996,
           fsh_cpue = hist_cpue) %>% 
  mutate(var = (fsh_cpue * mean(fsh_sum$cv)) ^ 2,
         se = mean(fsh_sum$se),
         upper = hist_cpue+(2*se),
         lower = hist_cpue-(2*se),
         CPUE = "Nominal") %>% 
  bind_rows(fsh_sum_log %>% mutate(CPUE = "Nominal") %>%
              select(year, fsh_cpue, var, se, upper, lower, CPUE) %>%
              mutate(year = as.numeric(as.character(year)))) %>% 
  mutate(cpue = round(fsh_cpue, 3),
         var = round(var, 3)) -> nom_cpue_log_ts

data.frame(year = 1980:1996,
           fsh_cpue = hist_cpue) %>% 
  mutate(var = sqrt(mean(std_dat$bt_se)),
         se = mean(std_dat$bt_se),
         upper = hist_cpue+(2*se),
         lower = hist_cpue-(2*se),
         CPUE = "Nominal") %>% 
  bind_rows(std_dat_log_full %>% 
              mutate(var = sqrt(bt_se)) %>% 
              select(year, fsh_cpue = bt_cpue, se = bt_se, var,
                     upper = bt_upper, lower = bt_lower) %>% 
              mutate(CPUE = "Fully Standardized",
                     year = as.numeric(as.character(year)))) %>% 
  mutate(cpue = round(fsh_cpue, 3),
         var = round(var, 3)) %>% data.frame() -> log_full_cpue_ts

cpue_ts_log<-rbind(nom_cpue_log_ts,log_full_cpue_ts %>% filter(year>1996))

cpue_ts_short <- nom_cpue_ts %>% 
  filter(year >= 1997)

ggplot(nom_cpue_log_ts) +
  geom_point(aes(year, cpue)) +
  geom_line(aes(year, cpue)) +
  geom_ribbon(aes(year, ymin = cpue - sqrt(var), ymax = cpue + sqrt(var)),
              alpha = 0.2,  fill = "grey") +
  geom_ribbon(aes(year, ymin = lower, ymax = upper),
              # geom_ribbon(aes(year, ymin = cpue - var, ymax = cpue + var),
              alpha = 0.4,  fill = "grey") +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  #lims(y = c(0, 1.5)) +
  #lims(y = c(-0.5, 1.5)) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") 

#NOTE2022: much more variance in data now... could easily run a straight line
# through error polygon = lack of information in this data!!! 

ggsave(paste0(YEAR+1,"/figures/ll_cpue_log_nom_1980_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

ggplot(cpue_ts_log) +
  geom_point(aes(year, cpue, col = CPUE)) +
  geom_line(aes(year, cpue, col=CPUE)) +
  geom_ribbon(aes(year, ymin = cpue - sqrt(var), ymax = cpue + sqrt(var), fill = CPUE),
              alpha = 0.2) +
  geom_ribbon(aes(year, ymin = lower, ymax = upper, fill = CPUE),
              # geom_ribbon(aes(year, ymin = cpue - var, ymax = cpue + var),
              alpha = 0.4) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") 

#NOTE2022: much more variance in data now... could easily run a straight line
# through error polygon = lack of information in this data!!! 

ggsave(paste0(YEAR+1,"/figures/ll_cpue__log_fullstand_1980_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

#compare logbooks to fish tickets!



# Write to file
write_csv(nom_cpue_log_ts, paste0(YEAR+1,"/output/ll_cpue_log_nom_", 
                                  min(nom_cpue_log_ts$year), "_", YEAR, ".csv"))

log_full_cpue_ts<-as.matrix(log_full_cpue_ts)
log_full_cpue_ts[,c(1,2,3,4,5,6,8)]<-as.numeric(log_full_cpue_ts[,c(1,2,3,4,5,6,8)])
log_full_cpue_ts<-as.data.frame(log_full_cpue_ts)

write_csv(log_full_cpue_ts, paste0(YEAR+1,"/output/ll_cpue_log_fullstand_", 
                                   min(log_full_cpue_ts$year), "_", YEAR, ".csv"))

#=================================================================================









