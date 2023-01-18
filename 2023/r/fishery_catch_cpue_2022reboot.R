
# Fishery catch 1985-present, fishery CPUE 1997-present
# Author: Jane Sullivan
# Contact: jane.sullivan@noaa.gov
# Last edited: Feb 2022

#NEW fishery CPUE data processing to deal with revamp of data queries for fishery
# CPUE analysis.  Rhea and co. reworked log books and data was reentered
# query was then rerun on '97-21 data.  This code examines fishery CPUE using that
# new data set and code modified from fishery_catch_cpue

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

# Logbook/CPUE data  ----

# Read in data, standardize cpue, etc.
read_csv(paste0(YEAR+1,"/data/fishery/fishery_cpue_2022reboot_1997_", YEAR,".csv"), 
         guess_max = 50000) %>% 
  filter(!is.na(date) & !is.na(hook_space) & !is.na(sable_lbs_set) &
           !is.na(start_lon) & !is.na(start_lon) & !is.na(soak) & !is.na(depth) &
           !is.na(Hook_size) & Hook_size != "MIX" &
           soak > 0 & !is.na(soak) & # soak time in hrs
           julian_day > 226 & # if there were special projects before the fishery opened
           no_hooks < 15000 & # 15000 in Kray's scripts - 14370 is the 75th percentile
           # limit analysis to Chatham Strait and Frederick Sounds where the
           # majority of fishing occurs
           #target = 710 &
           Stat %in% c("345603", "345631", "345702",
                       "335701", "345701", "345731", "345803")) %>% 
  mutate(Year = factor(year), 
         Gear = factor(Gear),
         Adfg = factor(Adfg),
         Stat = fct_relevel(factor(Stat),
                            "345702", "335701", # Frederick Sound
                            # Chatham south to north
                            "345603", "345631", "345701", "345731", "345803"),
         # 01=Conventional, 02=Snap On, 05=Mixed, 06=Autobaiter -> 01, 02, 05
         # show no strong differences. main difference with autobaiters, which
         # have lwr cpue than conventional gear
         Gear = derivedFactor("AB" = Gear == "06",
                              "CS" = Gear %in% c("01","02","05")),
         Hook_size = factor(Hook_size),
         # standardize hook spacing (Sigler & Lunsford 2001, CJFAS), 1 m = 39.37 in
         std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (hook_space / 39.37))), 
         std_cpue = sable_lbs_set / std_hooks,
         # dummy varbs, for prediction with random effects
         dum = 1, 
         dumstat = 1) %>% 
  #"sets" (aka effort_no) is the set identifier. Currently Martina's scripts
  #filter out sets that Kamala identifies as halibut target sets. Create a new
  #column that is the total number of sablefish target sets in a trip (trip_no's
  #only unique within a year)
  group_by(year, trip_no) %>% 
  mutate(no_sets = n_distinct(sets)) %>% 
  group_by(year) %>% 
  mutate(
    #The number of vessels participating in the fishery has descreased by 50% from
    #1997-2015. create new column is the total number of active vessels
    #participating in a given year
    total_vessels = n_distinct(Adfg),
    # Total unique trips per year
    total_trips = n_distinct(trip_no)) %>% 
  ungroup() -> fsh_cpue

nrow(fsh_cpue)
unique(fsh_cpue$target) #checking that these are sablefish targetting trips (710)
# Consolidation of fishery - number of vessels fishing and total number of trips
# in Chatham over time
# axis <- tickr(fsh_cpue, year, 5)

fsh_cpue %>% 
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
fsh_cpue %>%
  group_by(year) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> plot_boot

ggplot(plot_boot) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper), 
              alpha = 0.1, fill = "grey55") +
  geom_point(aes(x = year, y = Mean), size = 1) +
  geom_line(aes(x = year, y = Mean)) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") +
  lims(y = c(0, 1.1))
  
ggsave(paste0(YEAR+1,"/figures/fshcpue_bootCI_22reboot_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

# Prelim works towards CPUE analysis for NSEI, mirroring what was done by Jenny
# Stahl and Ben Williams in SSEI

# Normality

# Long right tail
ggplot(fsh_cpue, aes(std_cpue)) + geom_density(alpha = 0.4, fill = 4)

# Better, but still not normal with log transformation
ggplot(fsh_cpue, aes(log(std_cpue + 1))) + geom_density(alpha = 0.4, fill = 4)

# Following Jenny Stahl and Ben Williams' work in the SSEI, increase CPUE by 10%
# of the mean per Cambell et al 1996 and Cambell 2004. Back-transform with
# exp(cpue - mean(fsh_cpue$std_cpue) * 0.1)
fsh_cpue %>% 
  mutate(cpue = log(std_cpue + (mean(fsh_cpue$std_cpue) * 0.1))) -> fsh_cpue

ggplot(fsh_cpue, aes(cpue)) + geom_density(alpha = 0.4, fill = 4)

# EDA for GAM 

# Trends over time
ggplot(fsh_cpue, aes(Year, std_cpue)) + geom_boxplot()

# Trends over time by area
ggplot(fsh_cpue %>% 
         filter(Stat %in% c("345603", "345631", "345701", "345731")), aes(Stat, std_cpue, fill = Year)) + 
  geom_boxplot() +
  scale_fill_manual(values = rev(colorspace::sequential_hcl(c = 0, l = c(30, 90), 
                                                            power = c(1/5, 1.3), 
                                                            n_distinct(fsh_cpue$year))),
                    guide = FALSE) +
  labs(x = NULL, y = "Fishery CPUE (round pounds per hook)\n")

ggsave(paste0(YEAR+1,"/figures/fshcpue_trendsbyStat_22reboot_",min(fsh_cpue$year), "_", YEAR, ".png"), 
       dpi=400, height=4, width=7.5, units="in")

#2022: Note that 2020 and 2021 have some very high CPUE outliers with new query from Justin...
#or... with recruitment of '13-'16 year classes there is far more variability based on discard
# behavior as some fisherman clean up on small fish (but most opt not to); could check the outlier
# vessel if there were lengths... but just poundage... 
# after review and conversation, outliers belonged to 2nd gen knowledgable and "honest" fisherman
# and data is considered legit.  Furthermore, 2022 reboot shows high cpues in the past now so
# seems to be OK... 

# No one fished in Fred. Sound in 2018, 2 in 2019, 4 in 2021
fsh_cpue %>% filter(Stat %in% c("345702", "335701") & year == YEAR) %>% distinct(Adfg)
# Activity in N Chatham 
fsh_cpue %>% filter(Stat %in% c("345731", "345803") & year == YEAR) %>% distinct(Adfg)
# Activity in S Chatham
fsh_cpue %>% filter(Stat %in% c("345603")) %>% group_by(year) %>%  dplyr::summarize(n_distinct(Adfg)) %>% View()
fsh_cpue %>% filter(Stat %in% c("335701")) %>% group_by(year) %>%  dplyr::summarize(n_distinct(Adfg)) %>% View()

fsh_cpue %>% 
  group_by(year, Stat) %>% 
  dplyr::summarize(trips = n_distinct(trip_no),
            vessels = n_distinct(trip_no)) -> stat_sum

# Gear performance by Stat
ggplot(fsh_cpue, aes(Stat, cpue, fill = Gear)) + geom_boxplot()

# Gear performance over time
ggplot(fsh_cpue, aes(Year, cpue, fill = Gear)) + geom_boxplot() +
  theme(axis.text.x = element_text(size = 14, angle = 90, h = 1)) +
  labs(x = "", y = "Fishery CPUE\n")

# Only a handful of vessels with autobaiter gear
ggplot(fsh_cpue, aes(Adfg, cpue, color = Gear)) + geom_jitter(alpha=.4) +
  theme(axis.text.x = element_text(colour = "white"))

# Hook size performance - hook size 11 should be removed due to sample size and
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












