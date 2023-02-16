
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
unique(ll_cpue$depredation)
unique(ll_cpue$no_hooks_p_set)
unique(ll_cpue$gear); str(ll_cpue$gear)

with(ll_cpue, table(hook_size)); nrow(ll_cpue %>% filter (is.na(trip_soak)))
with(ll_cpue, table(Stat))
hist(ll_cpue$julian_day); abline(v=226, col="red")
hist(ll_cpue$no_hooks_fished_on_trip); abline(v=15000, col="blue")
hist(ll_cpue$no_hooks_p_set); abline(v=15000, col="blue")
nrow(ll_cpue)
table(ll_cpue$hook_space)
hist(ll_cpue$mea)

colnames(ll_cpue_ftx)
unique(ll_cpue_ftx$multi_gear_config)

nrow(ll_cpue_ftx %>% filter(Stat == "345803"))

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

nrow(ll_cpue)
unique(fsh_cpue$target) #checking that these are sablefish targetting trips (710)
# Consolidation of fishery - number of vessels fishing and total number of trips
# in Chatham over time
# axis <- tickr(fsh_cpue, year, 5)

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
library(rms)
ll_cpue_ftx %>%
  group_by(year,trip_set_targets) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> plot_boot1

ggplot(plot_boot1) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper, fill = trip_set_targets), 
 #             alpha = 0.1, fill = "grey55") +
              alpha = 0.1) +
  geom_point(aes(x = year, y = Mean, col = trip_set_targets), size = 1) +
  geom_line(aes(x = year, y = Mean, col = trip_set_targets)) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") +
  lims(y = c(0, 1.5)); view(plot_boot1)
  
ggsave(paste0(YEAR+1,"/figures/fshcpue_ftx_bootCI_bytarget_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")
#----
ll_cpue_ftx %>%
  filter(trip_set_targets == "all_Sablefish") %>%
  group_by(year,trip_recorded_releases) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> plot_boot2 #view(plot_boot2)

ggplot(plot_boot2) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper, fill = trip_recorded_releases), 
              #             alpha = 0.1, fill = "grey55") +
              alpha = 0.1) +
  geom_point(aes(x = year, y = Mean, col = trip_recorded_releases), size = 1) +
  geom_line(aes(x = year, y = Mean, col = trip_recorded_releases)) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") +
  lims(y = c(0, 2))

ggsave(paste0(YEAR+1,"/figures/fshcpue_ftx_bootCI_byrelease_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

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

ggsave(paste0(YEAR+1,"/figures/fshcpue_ftx_bootCI_bydepr_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

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
                position=position_dodge(width=0), width=1, size=0.2) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") +
  lims(y = c(0, 3))

ggsave(paste0(YEAR+1,"/figures/fshcpue_ftx_bootCI_bygeartrip_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")
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

ggsave(paste0(YEAR+1,"/figures/fshcpue_trendsbyStat_22reboot_",min(ll_cpue$year), "_", YEAR, ".png"), 
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
         
         std_hooks = 2.2 * no_hooks_p_set * (1 - exp(-0.57 * (mean_hook_spacing / 39.37))),  
         std_cpue_no = logged_no / std_hooks,
         std_cpue_lbs = logged_lbs / std_hooks,
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

ggsave(paste0(YEAR+1,"/figures/fshcpue_ftx2_bootCI_bytarget_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")


ggplot(plot_boot5 %>% filter(trip_set_targets == "all_Sablefish")) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper, fill = hook_cpue_spec), 
              #             alpha = 0.1, fill = "grey55") +
              alpha = 0.1) +
  geom_point(aes(x = year, y = Mean, col = hook_cpue_spec), size = 1) +
  geom_line(aes(x = year, y = Mean, col = hook_cpue_spec)) +
  #facet_wrap(~hook_cpue_spec) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
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
## Pretty cool... we get to the same CPUE weather from raw lbs/hook or through lbs/set... 
# Can skip the rest of this because we're getting the same ansers
# Also, including the estimated number of hooks (i.e, when multiple configurations are present
# and we used the average and such...) doesn't produce anything different... very minor
# discrependies in the point estimates.

#--------------------------------------------------------------------------------
# CPUE based on skipper's logbook data...
# Fishticket data is at the trip level and thus averages out the set specific performance.
# This is generally necessary because logbook entries are estimates 
ll_cpue_logged <-ll_cpue
nrow(ll_cpue_logged)
colnames(ll_cpue_logged)

nrow(ll_cpue_logged %>% filter (!is.na(logged_no)))
nrow(ll_cpue_logged %>% filter (!is.na(logged_lbs)))

releases<-ll_cpue_logged %>% filter(trip_recorded_releases == "logged_releases")
random_check(releases)
unique(ll_cpue_logged$disposition)

old <- Sys.time()
ll_cpue_logged %>% group_by(year, sell_date, Adfg, Stat, disposition) %>%
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
         #skipper_bias_byset = ,
         skipper_bias_bytrip = sum(unique(logged_and_landed_nos), na.rm=T)-unique(catch), 
         adj_logged_nos = case_when((disposition == "Retained"| is.na(disposition)) ~ 
                                      (logged_no/logged_and_landed_nos)*catch),
         est_rel_lbs_nos = case_when((disposition == "Released (Quota Limit)"| disposition == "Released") ~ 
                                      (logged_no/sum(unique(logged_and_landed_nos), na.rm=T))*unique(catch))
         ) %>% 
  ungroup() -> test; Sys.time() - old #%>%  #12 minutes for this piece!!!


old <- Sys.time()
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
         
         log_data = "fuckitfornow",
         #log_data = ifelse(unique(is.na(logged_no)),
        #                   ifelse(unique(!is.na(logged_lbs)),
        #                          "lbs","mixed"
        #                          ),"nos"
        #                   ),
                               
         set_catch_no_releases = sum(adj_logged_lbs, na.rm=TRUE)+sum(adj_logged_nos, na.rm=TRUE),
         set_catch_w_releases = sum(adj_logged_lbs, na.rm=TRUE)+sum(adj_logged_nos, na.rm=TRUE) +
                              sum(est_released_lbs, na.rm=TRUE)+sum(est_rel_lbs_nos, na.rm=TRUE),
         set_catch = ifelse("good" %in% set_catch_flag,
                            ifelse(trip_recorded_releases == "no_logged_releases",
                                   as.numeric(set_catch_no_releases),
                                 as.numeric(set_catch_w_releases)),
                            as.numeric(set_catch_w_releases)),
         set_releases = set_catch_w_releases - set_catch_no_releases,
         
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
  select(Year, sell_date, set_date, julian_day_set, julian_day_sell, trip_no, 
         Adfg, Spp_cde, set_soak, set_length, Gear, gear_name, hook_size, mean_hook_size, 
         hook_space, mean_hook_spacing, size, mean_size, no_hooks_exact, no_hooks_est,
         no_hooks_p_set, Stat, set_depth, set_no, no_sets, multigear_trip,
         trip_set_targets, trip_recorded_releases, multi_gear_config, disposition,
         set_depredation, trip_depredation, p_sets_depredated, catch, logged_no,
         logged_lbs, sable_lbs_set, start_lat, start_lon, set_target, set_target_2,
         trip_target, trip_target_2, logged_and_landed, logged_and_landed_nos,
         logged_and_released, logged_and_released_nos, adj_logged_lbs, est_released_lbs,
         adj_logged_nos, est_rel_lbs_nos, Depr_sum, Hook_size, Mean_hook_size,
         set_catch_flag, log_data, set_catch_no_releases, set_catch_w_releases,
         set_catch, set_releases, std_hooks, std_cpue, dum, dumstat, total_vessels, total_trips) -> ll_cpue_log; Sys.time() - old

#need to colapse duplicate columns with release data now that its captured

sel<-sample(nrow(ll_cpue_log),1)
as.data.frame(ll_cpue_log[ll_cpue_log$sell_date == ll_cpue_log$sell_date[sel] &
              ll_cpue_log$Adfg == ll_cpue_log$Adfg[sel],])


#--------------------------------------------------------------------------------
# RELEASE analysis
# now that we've looked at logbook data, lets see what we can glean about release
# behavior from the skippers who were good about recording that data... 

#--------------------------------------------------------------------------------
# HOOK SIZE performance - hook size 11 should be removed due to sample size and
# infrequency of use. Probably size 7 too - 4 vessels fished size 7 hooks in
# 1997, and only 1 vessel fished it until 2004
# 2022: also size 6 hooks ... half as many as size 7

table(fsh_cpue$Hook_size)
#new filter includes mixed hook size sets... need to cull those for analysis if
# hook size is included... 
fsh_cpue_hooks<-fsh_cpue %>% 
  filter(Hook_size %in% c("6","7","11","12","13","14","15","16"))
nrow(fsh_cpue); nrow(fsh_cpue_hooks)
table(fsh_cpue_hooks$Hook_size)

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
  filter(!Hook_size %in% c("6","7","11"))
nrow(fsh_cpue); nrow(fsh_cpue_hooks); nrow(fsh_cpue_cl)

# Depth - clear increasing trend, asymptotes ~ 450 m
ggplot(fsh_cpue_cl, aes(depth, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 2, se = FALSE) 

# Soak time - cut off at 40 hrs b/c it looks like there's a slight outlier
# effect
# hmmm ... debatable.  Outliers, but legit?  probably long soaks do to weather or other issues?
ggplot(fsh_cpue_cl, aes(soak, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 2, se = FALSE) 
fsh_cpue_cl %>% filter(soak < 40) -> fsh_cpue_cl

# Inconsistent and very slight latitudinal effect
ggplot(fsh_cpue_cl, aes(start_lat, cpue, group = Year, colour = Year)) +
  geom_smooth(method = 'loess', span = 1, se = FALSE) 

# Inconsistent and very slight seasonal effect
ggplot(fsh_cpue_cl, aes(julian_day, cpue, group = Year, colour = Year)) +
  geom_smooth(method = 'loess', span = 1, se = FALSE) 

# GAM cpue ----

# Potential variables influencing CPUE (ultimately interested in estimating a
# Year effect):
# depth - increase in CPUE up to ~ 450 m, then asymptote. Very clear and
# consistent trend between years
# julian_day - decrease twoards the end of the season? EDA suggested there is no
# consistent seasonal trend. If there is a trend, its slightly decreasing over
# the season.  pj22: I agree with slight decreasing trend through season
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

# Determine if random variables should be included (Stat and Adfg)
m1 <- bam(cpue ~ Year + Gear + Hook_size + s(depth, k=4) + s(soak, k=4) + 
            s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum), data=fsh_cpue_cl, gamma=1.4)
m1a <- bam(cpue ~ Year + Gear + s(depth, k=4) + s(soak, k=4) + #hook size removed
            s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum), data=fsh_cpue_cl, gamma=1.4)
m2 <- bam(cpue ~ Year + Gear + Hook_size + s(depth, k=4) + s(soak, k=4) + 
            s(Adfg, bs='re', by=dum), data=fsh_cpue_cl, gamma=1.4)
m3 <- bam(cpue ~ Year + Gear + Hook_size + s(depth, k=4) + s(soak, k=4) +
            s(Stat, bs='re', by=dumstat), data=fsh_cpue_cl, gamma=1.4)
m4 <- bam(cpue ~ Year + Gear + Hook_size + s(depth, k=4) + s(soak, k=4), 
          data=fsh_cpue_cl, gamma=1.4)

summary(m1); summary(m1a) 
summary(m2)
summary(m3)
summary(m4)

AIC(m1,m1a, m2,m3, m4)

# The model with the lowest AIC and highest deviance explained includes a random
# effect for vessel and area. #same in 2022-pj
# 2022 NOTE hook size is significant... oi... 

# No residual patterns, but may be some outliers
plot(fitted(m1), resid(m1))
abline(h = 0, col = "red", lty = 2)

# 14 outliers, get rid of them and refit models with new data set
which(fitted(m1) < -1.5)   #13 outliers in 2022
not_outliers <- which(fitted(m1) >= -1.5)
fsh_cpue_cl <- fsh_cpue_cl %>% 
  slice(not_outliers)

m1 <- bam(cpue ~ Year + Gear + Hook_size + s(depth, k=4) + s(soak, k=4) + s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum), data=fsh_cpue_cl, gamma=1.4)
m1a <- bam(cpue ~ Year + Gear + s(depth, k=4) + s(soak, k=4) + s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum), data=fsh_cpue_cl, gamma=1.4)
m2 <- bam(cpue ~ Year + Gear + Hook_size + s(depth, k=4) + s(soak, k=4) + s(Adfg, bs='re', by=dum), data=fsh_cpue_cl, gamma=1.4)
m3 <- bam(cpue ~ Year + Gear + Hook_size + s(depth, k=4) + s(soak, k=4) +s(Stat, bs='re', by=dumstat), data=fsh_cpue_cl, gamma=1.4)
m4 <- bam(cpue ~ Year + Gear + Hook_size + s(depth, k=4) + s(soak, k=4), data=fsh_cpue_cl, gamma=1.4)

AIC(m1, m1a, m2, m3, m4)

# Better, AIC still likes hook size in there...
plot(fitted(m1), resid(m1))
abline(h = 0, col = "red", lty = 2)

plot(m1, page = 1, shade = TRUE, resid = TRUE, all = TRUE)
summary(m1)

# CPUE increases with depth, then asymptotes ~ 450 m. CPUE is constant and then
# drops off ~ 10 hr soak time, but the overall effect is weaker than depth
# Conventional gear performs slightly better than autobaiter gear.

# Determine whether to keep hook size or keep it as a random effect
# PJ22: not crazy about hook size as a random effect - it should always be a fixed affect because
# it would directly affect cpue... as opposed to year or vessel random effect where there is 
# random noise associated with the variable, hook size has a plausible effect on the response variable?
m5 <- bam(cpue ~ Year + Gear + s(depth, k=4) + s(soak, k=4) +    #no hook size
            s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum), data=fsh_cpue_cl, gamma=1.4)
m6 <- bam(cpue ~ Year + Gear + s(depth, k=4) + s(soak, k=4) +    #hook size as a random variable
            s(Hook_size, bs='re', by=dum) + s(Stat, bs='re', by=dumstat)+ 
            s(Adfg, bs='re', by=dum), data=fsh_cpue_cl, gamma=1.4)

AIC(m1, m5, m6)

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

#Determine whether to include lat and long
m7 <- bam(cpue ~ Year + Gear + s(depth, k=4) + s(soak, k=4) + 
            Hook_size + #s(Hook_size, bs='re', by=dum) + 
            s(Stat, bs='re', by=dumstat) + 
            s(Adfg, bs='re', by=dum) + te(start_lon, start_lat), data=fsh_cpue_cl, gamma=1.4)
m8 <- bam(cpue ~ Year + Gear + s(depth, k=4) + s(soak, k=4) + 
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

# The inclusion of a seasonal effect  improves model fit - there is a
# slightly decreasing trend in cpue on average over the course of the season.
m11 <- bam(cpue ~ Year + Gear + s(julian_day, k=4) + s(depth, k=4) + 
             s(soak, k=4) + s(start_lat) + 
             Hook_size + #s(Hook_size, bs='re', by=dum) + 
             s(Stat, bs='re', by=dumstat) + s(Adfg, bs='re', by=dum), data=fsh_cpue_cl, gamma=1.4)
AIC(m7, m9, m11)
summary(m11)
plot(m11, page = 1, shade = TRUE, all = TRUE) #resid = TRUE,

# # Relationship between depth and soak time - highest cpue in > 450 m
# # and ~ 10 hr soak time
vis.gam(m11, c('depth', 'soak'), plot.type='contour', type='response', color='topo', too.far=0.1)

# GAM summary ----

# Final model structure (m11) (* = random effect):
# CPUE ~ Year + Gear + s(julian day) + s(depth) + s(soak time) + 
#   s(latitude) + hooksize* + statarea* + vessel* 

# 36.7% deviance explained
# 33.9% in 2022 with pj mods.  With Jane's original code get 34.9%
# 2022 reboot data only explains 28.7% of deviance

# CPUE decreases throughout the season. CPUE increases with depth, then
# asymptotes ~ 450 m. CPUE is constant and then drops off ~ 10 hr soak time,
# possibly due to sandfleas or hagfish. 
#!PJ2022: disagree about cause.  Assymptotic CPUE and time is expected as nearby
#         catch-able fish are caught and bait is degraded by loss of scent as well
#        as things like sand fleas and hagfish...
# CPUE is greatest in the northern and
# southern parts of chatham.

# The overall effect of julian day, soak time, and latitude is weaker than
# depth. Conventional gear performs slightly better than autobaiter gear. 

# Predictions ----

#Create standard dataset to get standardized CPUE for each year
# a little cnfused here... go through all this modelling and then producing 
# predicted values from average values for each year?  
std_dat <- expand.grid(year = unique(fsh_cpue$year),
                       Gear = 'CS',
                       depth = mean(fsh_cpue$depth), 
                       soak = 10, 
                       julian_day = median(fsh_cpue$julian_day),
                       start_lat = mean(fsh_cpue$start_lat),
                       Stat = "345701",
                       Hook_size = "14",
                       Adfg = "35491",
                       dum = 0,
                       dumstat = 0) %>% 
  mutate(Year = factor(year))

pred_cpue <- predict(m11, std_dat, type = "link", se = TRUE)

#checking my code with Jane's... checks out :)
preds<-predict.bam(m11, type="response", std_dat, se = TRUE)
str(preds); head(preds)

#Put the standardized CPUE and SE into the data frame and convert to
#backtransformed (bt) CPUE
std_dat %>% 
  mutate(fit = pred_cpue$fit,
         se = pred_cpue$se.fit,
         upper = fit + (2 * se),
         lower = fit - (2 * se),
         bt_cpue = exp(fit) - (mean(fsh_cpue$std_cpue) * 0.1),
         bt_upper = exp(upper) - (mean(fsh_cpue$std_cpue) * 0.1),
         bt_lower = exp(lower) - (mean(fsh_cpue$std_cpue) * 0.1),
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
              mutate(CPUE = "GAM")) %>% 
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

ggsave(paste0(YEAR+1,"/figures/compare_stdcpue_llfsh_2022reboot_", YEAR, ".png"), dpi=300, height=4, width=7, units="in")

# Percent change in fishery nominal cpue compared to a ten year rolling average
fsh_sum %>% 
  filter(year > YEAR - 10) %>% 
  mutate(lt_mean = mean(fsh_cpue),
         perc_change_lt = (fsh_cpue - lt_mean) / lt_mean * 100) 

# Percent change in fishery nominal cpue from last year
#not relevant since no CPUE in 2020 due to stupid covid
fsh_sum %>% 
  filter(year >= YEAR - 1) %>%
  select(year, fsh_cpue) %>% 
  reshape2::dcast("fsh_cpue" ~ year) -> perc_ch

names(perc_ch) <- c("cpue", "last_year", "this_year") 
perc_ch %>% mutate(perc_change_ly = (`this_year` - `last_year`) / `last_year` * 100)

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
  mutate(var = (fsh_cpue * mean(fsh_sum$cv)) ^ 2) %>% 
  bind_rows(fsh_sum %>% 
              select(year, fsh_cpue, var)) %>% 
  mutate(cpue = round(fsh_cpue, 3),
         var = round(var, 3)) -> cpue_ts

cpue_ts_short <- cpue_ts %>% 
         filter(year >= 1997)

ggplot(cpue_ts_short) +
  geom_point(aes(year, cpue)) +
  geom_line(aes(year, cpue)) +
  geom_ribbon(aes(year, ymin = cpue - sqrt(var), ymax = cpue + sqrt(var)),
  # geom_ribbon(aes(year, ymin = cpue - var, ymax = cpue + var),
              alpha = 0.2,  fill = "grey") +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  #lims(y = c(0, 1.5)) +
  #lims(y = c(-0.5, 1.5)) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") 

#NOTE2022: much more variance in data now... could easily run a straight line
# through error polygon = lack of information in this data!!! 

ggsave(paste0(YEAR+1,"/figures/fshcpue_1997_2022reboot_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

# Write to file
write_csv(cpue_ts, paste0(YEAR+1,"/output/fshcpue_", min(cpue_ts$year), "_", YEAR, "_base_nom.csv"))
# write_csv(cpue_ts, paste0("output/nominalwpue_var_llfsh_", min(cpue_ts$year), "_", YEAR, ".csv"))

#=================================================================================
#2022: Added two more data sets to run in SCAA
#1) use the gam output modelled using predictions of average values for each year
#   I don't like that approach, but Jane used it so we'll check it
#2) use the gam model to predict cpue for each trip, then bootstrap those predictions
#   (as is done to produce the nominal values)

#1) 
str(fsh_sum)
str(std_dat)
comp<-fsh_sum %>%
  select(year, cpue = fsh_cpue, upper, lower) %>% 
  mutate(CPUE = "Nominal") %>% 
  bind_rows(std_dat %>% 
              select(year, cpue = bt_cpue, upper = bt_upper, lower = bt_lower) %>% 
              mutate(CPUE = "GAM"))
view(comp)

std_dat$n<-fsh_sum$n
std_dat$cv<-(std_dat$bt_se*(std_dat$n^0.5))/std_dat$bt_cpue

data.frame(year = 1980:1996,
           bt_cpue = hist_cpue) %>% 
#mutate(var = (fsh_cpue * mean(fsh_sum$cv)) ^ 2) %>% 
  mutate(var = mean(std_dat$bt_se^2)) %>%
  bind_rows(std_dat %>% 
              select(year, bt_cpue) %>% 
              mutate(var = std_dat$bt_se^2)) %>% #std_dat$bt_se
  mutate(cpue = round(bt_cpue, 3),
         var = round(var, 5)) -> cpue_gam

cpue_gam$bt_cpue<-as.numeric(cpue_gam$bt_cpue)
cpue_gam$var<-as.numeric(cpue_gam$var)
cpue_gam$cpue<-as.numeric(cpue_gam$cpue)

str(cpue_gam)

cpue_gam_short <- cpue_gam %>% 
  filter(year >= 1997)

ggplot(cpue_gam_short) +
  geom_point(aes(year, cpue)) +
  geom_line(aes(year, cpue)) +
  geom_ribbon(aes(year, ymin = cpue - sqrt(var), ymax = cpue + sqrt(var)),
              # geom_ribbon(aes(year, ymin = cpue - var, ymax = cpue + var),
              alpha = 0.2,  fill = "grey") +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  #lims(y = c(0, 1.5)) +
  #lims(y = c(-0.5, 1.5)) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") 

ggsave(paste0(YEAR+1,"/figures/fshcpue_1997_2022reboot_", YEAR, "_base_gam.png"),
       dpi=300, height=4, width=7, units="in")

# Write to file
write_csv(cpue_gam, paste0(YEAR+1,"/output/fshcpue_2022rb_", min(cpue_gam$year), "_", YEAR, "_base_gam.csv"))

#2) bootstrapped gam fit predictions... (oi)
#2a) get predicted values for each data point... 
m11 <- bam(cpue ~ Year + Gear + s(julian_day, k=4) + s(depth, k=4) + 
             s(soak, k=4) + s(start_lat) + 
             Hook_size + #s(Hook_size, bs='re', by=dum) + 
             s(Stat, bs='re', by=dumstat) + s(Adfg, bs='re', by=dum), data=fsh_cpue_cl, gamma=1.4)


pred_cpue_fine <- predict(m11, fsh_cpue_cl, type = "link", se = TRUE)
str(pred_cpue_fine)
#checking my code with Jane's... checks out :)
preds_fine<-predict.bam(m11, type="response", fsh_cpue_cl, se = TRUE)
str(preds_fine)

nrow(fsh_cpue)
#Put the standardized CPUE and SE into the data frame and convert to
#backtransformed (bt) CPUE

#fsh_cpue %>% 
#  mutate(cpue = log(std_cpue + (mean(fsh_cpue$std_cpue) * 0.1))) -> fsh_cpue

fsh_cpue_cl %>% 
  mutate(fit = preds_fine$fit,
         se = preds_fine$se.fit,
         upper = fit + (1.96 * se),
         lower = fit - (1.96 * se),
#         bt_cpue = exp(fit) - (fsh_cpue$std_cpue * 0.1))->test
         bt_cpue = exp(fit) - (mean(fsh_cpue_cl$std_cpue) * 0.1),
         bt_upper = exp(upper) - (mean(fsh_cpue_cl$std_cpue) * 0.1),
         bt_lower = exp(lower) - (mean(fsh_cpue_cl$std_cpue) * 0.1),
         bt_se = (bt_upper - bt_cpue) / 2) -> preds_fine_bt

#std_dat %>% 
#  mutate(fit = pred_cpue$fit,
#         se = pred_cpue$se.fit,
#         upper = fit + (2 * se),
#         lower = fit - (2 * se),
#         bt_cpue = exp(fit) - (mean(fsh_cpue$std_cpue) * 0.1),
#         bt_upper = exp(upper) - (mean(fsh_cpue$std_cpue) * 0.1),
#         bt_lower = exp(lower) - (mean(fsh_cpue$std_cpue) * 0.1),
#         bt_se = (bt_upper - bt_cpue) / 2  #,
         #bt_cv = bt_se/bt_cpue
 # ) -> std_dat

#2b) bootstrap that shit to get annual estimates
preds_fine_bt %>%
  group_by(year) %>%
  do(data.frame(rbind(smean.cl.boot(.$bt_cpue)))) -> plot_boot_gam

preds_fine_bt %>%
  group_by(year) %>%
  do(data.frame(rbind(smean.cl.normal(.$bt_cpue)))) -> plot_norm_gam

fsh_cpue %>%
  group_by(year) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> plot_boot2

#CIs are narrower than should be? CI for the mean, but narrower than nominal estimates... 

ggplot(plot_boot_gam) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper), 
              alpha = 0.1, fill = "grey55") +
  geom_point(aes(x = year, y = Mean), size = 1) +
  geom_line(aes(x = year, y = Mean)) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") +
  lims(y = c(0, 1.1))

ggsave(paste0(YEAR+1,"/figures/fshcpue_bootCI_1997_22rb_", YEAR, "_boot_gam.png"),
       dpi=300, height=4, width=7, units="in")

#2c) compare to other estimates
fsh_sum_p<-full_join(fsh_sum, plot_boot_gam, by = "year") %>% 
  mutate(gam_cpue = Mean,
         gam_lower = Lower,
         gam_upper = Upper) %>%
  select(year,fsh_cpue,sd,n,se,var,cv,upper,lower,gam_cpue,gam_lower,gam_upper)

fsh_sum_p %>%
  select(year, cpue = fsh_cpue, upper, lower) %>% 
  mutate(CPUE = "Nominal") %>% 
  bind_rows(plot_boot_gam %>% 
              select(year, cpue = Mean, upper = Upper, lower = Lower) %>% 
              mutate(CPUE = "GAM")) %>% 
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

#2d) export for SCAA
plot_boot_gam %>% 
  mutate(cpue = Mean,
         var = Upper-Mean) %>% 
  select(year,cpue,var)

data.frame(year = 1980:1996,
           cpue = hist_cpue) %>% 
  #mutate(var = (fsh_cpue * mean(fsh_sum$cv)) ^ 2) %>% 
  mutate(var = mean(plot_boot_gam$Upper - plot_boot_gam$Mean)) %>%
  bind_rows(plot_boot_gam %>% 
              mutate(cpue = Mean,
                     var = Upper-Mean) %>% 
              select(year,cpue,var)) %>% #std_dat$bt_se
  mutate(cpue = round(cpue, 3),
         var = round(var, 5)) -> cpue_gam_boot

write_csv(cpue_gam_boot, paste0(YEAR+1,"/output/fshcpue_22rb_", min(cpue_ts$year), "_", YEAR, "_boot_gam.csv"))

#====scraps============
sd = sd(std_cpue),
n = length(std_cpue),
se = sd / (n ^ (1/2)),
var = var(std_cpue),
cv = sd / fsh_cpue,












