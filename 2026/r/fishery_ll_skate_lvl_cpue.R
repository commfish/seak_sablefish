
# Fishery catch 1985-present, fishery CPUE 1997-present
# Author: Phil Joy
# Contact: jphilip.joy@alaska.gov
# Last edited: Feb 2023

# This code calculates longline CPUE by skate for comparison with pot fshery

source("r_helper/helper.r")
source("r_helper/functions.r")

# if(!require("rms"))   install.packages("rms") # simple bootstrap confidence intervals

# Most recent year of data
YEAR <- 2022


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
colnames(ll_cpue_ftx)

hist(ll_cpue_ftx$no_skates_fished_on_trip); max(ll_cpue_ftx$no_skates_fished_on_trip, na.rm=T)

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
           !is.na(no_skates_fished_on_trip) &
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
         skate_p_set = no_skates_fished_on_trip/no_sets,
         Gear = derivedFactor("AB" = Gear == "6",
                              "CS" = Gear %in% c("1","2","5")),
         Hook_size = factor(hook_size),  #might be worth treating as numeric? 
         Mean_hook_size = mean_size, 
         
         std_cpue = sable_lbs_set / skate_p_set,
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
  labs(x = "", y = "Sablefish CPUE (round lb per skate)\n") +
  lims(y = c(0, 400))  + 
  scale_color_viridis_d(name = "Set targets of trip",
                    labels = c("all halibut","all sablefish","sablefish and halibut mix"),
                    option = "C", begin=0,end=0.85) +
  scale_fill_viridis_d(name = "Set targets of trip",
                       labels = c("all halibut","all sablefish","sablefish and halibut mix"),
                     option = "C", begin=0,end=0.85) +
  theme(legend.position = "bottom") #, + #c(0.2,0.8),
  
ggsave(paste0(YEAR+1,"/figures/llskate_cpue_ftx_bootCI_bytarget_1997_", YEAR, ".png"),
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
  labs(x = "", y = "Sablefish CPUE (round lb per skate)\n") +
  lims(y = c(0, 300)) + 
  scale_color_viridis_d(name = "",
                        labels = c("Trip logged releases","Trip logged NO releases"),
                        option = "C", begin=0,end=0.65) +
  scale_fill_viridis_d(name = "",
                       labels = c("Trip logged releases","Trip logged NO releases"),
                       option = "C", begin=0,end=0.65) +
  theme(legend.position = "bottom") #, + #c(0.2,0.8),

ggsave(paste0(YEAR+1,"/figures/llskate_cpue_ftx_bootCI_byrelease_1997_", YEAR, ".png"),
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
  labs(x = "", y = "Fishery CPUE (round lb per skate)\n") +
  lims(y = c(0, 400))

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
  labs(x = "", y = "Fishery CPUE (round lb per skate)\n") +
  lims(y = c(0, 300)) + 
  scale_color_viridis_d(name = "",
                        labels = c("Longline trip","Mixed longline & pot trip"),
                        option = "A", begin=0,end=0.65) +
  scale_fill_viridis_d(name = "",
                       labels = c("Longline trip","Mixed longline & pot trip"),
                       option = "A", begin=0,end=0.65) +
  theme(legend.position = "bottom")

ggsave(paste0(YEAR+1,"/figures/llskate_cpue_ftx_bootCI_bygeartrip_1997_", YEAR, ".png"),
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

ggsave(paste0(YEAR+1,"/figures/skate_cpue_trendsbyStat_",min(ll_cpue$year), "_", YEAR, ".png"), 
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
  labs(x = "", y = "Fishery CPUE (round lb/skate)\n") +
  theme(legend.position = c(0.8, 0.2)) +
  expand_limits(y = 0)

ggsave(paste0(YEAR+1,"/figures/compare_stdcpue_ll_skate_", YEAR, ".png"), dpi=300, height=4, width=7, units="in")

#compare to the old methods... 

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

ggsave(paste0(YEAR+1,"/figures/ll_cpue_nom_m12_1980_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")
# Write to file
write_csv(fsh_sum, paste0(YEAR+1,"/output/ll_cpue_SKATE_nom_", min(fsh_sum$year), "_", YEAR, ".csv"))

std_dat %>% 
              mutate(var = sqrt(bt_se)) %>% 
              select(year, fsh_cpue = bt_cpue, se = bt_se, var,
                     upper = bt_upper, lower = bt_lower) %>% 
              mutate(CPUE = "Fully Standardized") %>% 
  mutate(cpue = round(fsh_cpue, 3),
         var = round(var, 3)) %>% data.frame() -> std_dat2

#std_dat <-std_dat %>% select(year,fsh_cpue = bt_cpue,se = bt_se,var = se^2,cv,upper,lower)
std_dat2<-as.matrix(std_dat2)
std_dat2[,c(1,2,3,4,5,6,8)]<-as.numeric(std_dat2[,c(1,2,3,4,5,6,8)])
std_dat2<-as.data.frame(std_dat2)

write_csv(std_dat2, paste0(YEAR+1,"/output/ll_cpue_SKATE_std_", min(std_dat$year), "_", YEAR, ".csv"))











