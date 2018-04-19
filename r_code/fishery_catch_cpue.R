
# Fishery catch 1985-present, fishery CPUE 1997-present
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2018-04-09

source("r_code/helper.R")

# Most recent year of data
YEAR <- 2017

# Harvest ----

# There are multiple sources of catch data. The IFDB (Region I database) was
# used in the past, but J. Shriver recommends using the Gross Earnings File
# (GEF) for catch histories. The problem is the GEF isn't updated until data has
# been finalized, so 2017 isn't ready yet. The IFDB data goes back to 1969,
# while the GEF data only goes back to 1975. They are the same from 1985 to
# present, so for now, just IFDB's 1985-present

cols <- quos(year, date, julian_day, Mgmt_area, Stat, Cfec_permit, Delivery_cde, 
             Harvest_cde, Spp_cde, whole_pounds, pounds, src) 

catch_gef <- read_csv(paste0("data/fishery/nseiharvest_gef_1975_", YEAR-1,".csv"), 
                      guess_max = 50000) %>% 
  mutate(src = "gef") %>% 
  select(!!!cols)

catch_ifdb <- read_csv(paste0("data/fishery/nseiharvest_ifdb_1969_", YEAR,".csv"), 
                       guess_max = 50000) #%>% 
# mutate(src = "ifdb") #%>% 
#select(!!!cols)

bind_rows(catch_gef, catch_ifdb) -> catch

catch_ifdb %>% 
  filter(year %in% c("2013", "2014", "2015", "2016", "2017")) %>% 
  group_by(year, julian_day) %>% 
  summarise(pounds = sum(whole_pounds)) %>% 
  mutate(cum_pounds = cumsum(pounds)) -> catch_plot

ggplot(catch_plot, aes(x = julian_day)) +
  geom_line(aes(y = cum_pounds/1000000)) +
  facet_wrap(~ year, ncol = 1) +
  labs(x = "Julian Day", y = "Millions lbs") +
  ylim(0, 1)

catch_ifdb %>% 
  group_by(year) %>% 
  summarize(total_pounds = sum(whole_pounds)) -> sum_catch

ggplot(sum_catch %>% 
         filter(year >= 1985), 
       aes(x = year, y = total_pounds/1000000)) +
  geom_line(group=1) +
  geom_point() +
  scale_x_continuous(breaks = seq(min(sum_catch$year), YEAR, 4)) +
  labs(x = "", y = "Fishery harvest (millions lbs)\n") +
  ylim(0, 6)

ggsave(paste0("figures/fishery_harvest_1985_", YEAR, ".png"), 
       dpi=300, height=4, width=7.5, units="in")

# Consolidation of fishery - number of vessels fishing and total number of trips
# in Chatham over time

# Logbook/CPUE data  ----

# Read in data, standardize cpue, etc.
read_csv(paste0("data/fishery/fishery_cpue_1997_", YEAR,".csv"), 
         guess_max = 50000) %>% 
  filter(!is.na(date) & !is.na(hook_space) & !is.na(sable_lbs_set) &
           !is.na(start_lon) & !is.na(start_lon) & !is.na(soak) & !is.na(depth) &
           !is.na(Hook_size) & Hook_size != "MIX" &
           soak > 0 & !is.na(soak) & # soak time in hrs
           julian_day > 226 & # if there were special projects before the fishery opened
           no_hooks < 15000 & # 15000 in Kray's scripts - 14370 is the 75th percentile
           # limit analysis to Chatham Strait and Frederick Sounds where the
           # majority of fishing occurs
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

fsh_cpue %>% 
  select(year, Vessels = total_vessels, Trips = total_trips) %>% 
  gather(Variable, Count, -year) %>% 
  distinct() %>%   
  ggplot(aes(x = year, y = Count)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Variable, ncol = 1, scales = "free") +
  scale_x_continuous(breaks = seq(min(fsh_cpue$year), YEAR, 4)) +
  labs(x = "", y = "") 

ggsave(paste0("figures/fishery_tripandvessel_trends_1997_", YEAR, ".png"), 
       dpi=300, height=6, width=5, units="in")

# New CPUE analysis for NSEI, mirroring what was done by Jenny and Ben in SSEI

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
ggplot(fsh_cpue, aes(Year, cpue)) + geom_boxplot()

# Trends over time by area
ggplot(fsh_cpue, aes(Stat, cpue, fill = Year)) + 
  geom_boxplot() +
  scale_fill_manual(values = rev(colorspace::sequential_hcl(n_distinct(fsh_cpue$year))),
                    guide = FALSE) +
  labs(x = "", y = "Fishery CPUE\n")

ggsave(paste0("figures/fshcpue_trendsbyStat_",min(fsh_cpue$year), "_", YEAR, ".png"), 
       dpi=400, height=4, width=7.5, units="in")

# 4 vessels driving the downward trend of cpue in Fred. Sound
fsh_cpue %>% filter(Stat %in% c("345702", "335702") & year == 2017) %>% distinct(Adfg)
# 13 vessels driving downward trend in N. Chatham
fsh_cpue %>% filter(Stat %in% c("345731", "345803") & year == 2017) %>% distinct(Adfg)
# Out >40 vessels total - shows that bulk of effort is in S Chatham
fsh_cpue %>% filter(year == 2017) %>% distinct(Adfg)

# Gear performance by Stat
ggplot(fsh_cpue, aes(Stat, cpue, fill = Gear)) + geom_boxplot()
# Gear performance over time
ggplot(fsh_cpue, aes(Year, cpue, fill = Gear)) + geom_boxplot() +
  theme(axis.text.x = element_text(size = 14, angle = 90, h = 1)) +
  labs(x = "", y = "Fishery CPUE\n")
# Only a handful of vessels with autobaiter gear
ggplot(fsh_cpue, aes(Adfg, cpue, color = Gear)) + geom_jitter(alpha=.4) +
  theme(axis.text.x = element_text(colour = "white"))

# Hook size performance - hook size 11 ashould be removed due to sample size and
# infrequency of use. Probably size 7 too - 4 vessels fished size 7 hooks in
# 1997, and only 1 vessel fished it until 2004
table(fsh_cpue$Hook_size)
fsh_cpue %>% filter(Hook_size == "7") %>% distinct(Adfg, Stat, Year)
fsh_cpue %>% filter(!Hook_size %in% c("11", "7")) -> fsh_cpue
# Not much contrast in CPUE between remaining hook sizes... maybe some
# difference in performance between years/areas
ggplot(fsh_cpue, aes(Hook_size, cpue)) + geom_boxplot()
ggplot(fsh_cpue, aes(Year, cpue, fill = Hook_size)) + geom_boxplot()+
  theme(axis.text.x = element_text(size = 14, angle = 90, h = 1)) +
  labs(x = "", y = "Fishery CPUE\n")
ggplot(fsh_cpue, aes(Stat, cpue, fill = Hook_size)) + geom_boxplot()+
  labs(x = "\nStat area", y = "Fishery CPUE\n")

# Depth - clear increasing trend, asymptotes ~ 450 m
ggplot(fsh_cpue, aes(depth, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 2, se = FALSE) 

# Soak time - cut off at 40 hrs b/c it looks like there's a slight outlier
# effect
ggplot(fsh_cpue, aes(soak, cpue)) + geom_point(shape = 20) + 
  geom_smooth(size = 2, se = FALSE) 
fsh_cpue %>% filter(soak < 40) -> fsh_cpue

# Inconsistent and very slight latitudinal effect
ggplot(fsh_cpue, aes(start_lat, cpue, group = Year, colour = Year)) +
  geom_smooth(method = 'loess', span = 1, se = FALSE) 

# Inconsistent and very slight seasonal effect
ggplot(fsh_cpue, aes(julian_day, cpue, group = Year, colour = Year)) +
  geom_smooth(method = 'loess', span = 1, se = FALSE) 

# GAM cpue ----

# Potential variables influencing CPUE (ultimately interested in estimating a
# Year effect):
# depth - increase in CPUE up to ~ 450 m, then asymptote. Very clear and
# consistent trend between years
# julian_day - decrease twoards the end of the season? EDA suggested there is no
# consistent seasonal trend. If there is a trend, its slightly decreasing over
# the season.
# Adfg - vessel effect, some are better fishermen than others. Routinely
# improves model fit and doesn't grossly violate assumptions.
# Gear - higher for conventional gear (01) over autobaiter (06) consistently
# between years, although this becomes dampened with the inclusion on the vessel
# effect
# Hook_size - optimal hook size? No consistent trend between years. Treat as random
# effect
# start_lat - is there some consistent trend in Chatham Strait going north into
# Chatham? Not one that is consistent between years. If one exists it tends to
# be decreasing with latitude. There was no spatial autocorrelation detected (done in previous analysis).
# start_lat/start_lon - spatial autocorrelation

# Determine if random variables should be included
m1 <- bam(cpue ~ Year + Gear + Hook_size + s(depth, k=4) + s(soak, k=4) + s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum), data=fsh_cpue, gamma=1.4)
m2 <- bam(cpue ~ Year + Gear + Hook_size + s(depth, k=4) + s(soak, k=4) + s(Adfg, bs='re', by=dum), data=fsh_cpue, gamma=1.4)
m3 <- bam(cpue ~ Year + Gear + Hook_size + s(depth, k=4) + s(soak, k=4) +s(Stat, bs='re', by=dumstat), data=fsh_cpue, gamma=1.4)
m4 <- bam(cpue ~ Year + Gear + Hook_size + s(depth, k=4) + s(soak, k=4), data=fsh_cpue, gamma=1.4)

summary(m1) 
summary(m2)
summary(m3)
summary(m4)

AIC(m1,m2,m3, m4)

# The model with the lowest AIC and highest deviance explained includes a random
# effect for vessel and area.

# No residual patterns, but may be some outliers
plot(fitted(m1), resid(m1))
abline(h = 0, col = "red", lty = 2)

# 14 outliers, get rid of them and refit models with new data set
which(fitted(m1) < -1.5)
not_outliers <- which(fitted(m1) >= -1.5)
fsh_cpue <- fsh_cpue %>% 
  slice(not_outliers)

m1 <- bam(cpue ~ Year + Gear + Hook_size + s(depth, k=4) + s(soak, k=4) + s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum), data=fsh_cpue, gamma=1.4)
m2 <- bam(cpue ~ Year + Gear + Hook_size + s(depth, k=4) + s(soak, k=4) + s(Adfg, bs='re', by=dum), data=fsh_cpue, gamma=1.4)
m3 <- bam(cpue ~ Year + Gear + Hook_size + s(depth, k=4) + s(soak, k=4) +s(Stat, bs='re', by=dumstat), data=fsh_cpue, gamma=1.4)
m4 <- bam(cpue ~ Year + Gear + Hook_size + s(depth, k=4) + s(soak, k=4), data=fsh_cpue, gamma=1.4)

AIC(m1, m2, m3, m4)

# Better
plot(fitted(m1), resid(m1))
abline(h = 0, col = "red", lty = 2)

plot(m1, page = 1, shade = TRUE, resid = TRUE, all = TRUE)
summary(m1)

# CPUE increases with depth, then asymptotes ~ 450 m. CPUE is constant and then
# drops off ~ 10 hr soak time, but the overall effect is weaker than depth
# Conventional gear performs slightly better than autobaiter gear.

# Determine whether to keep hook size or keep it as a random effect
m5 <- bam(cpue ~ Year + Gear + s(depth, k=4) + s(soak, k=4) + s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum), data=fsh_cpue, gamma=1.4)
m6 <- bam(cpue ~ Year + Gear + s(depth, k=4) + s(soak, k=4) + s(Hook_size, bs='re', by=dum) + s(Stat, bs='re', by=dumstat)+ s(Adfg, bs='re', by=dum), data=fsh_cpue, gamma=1.4)

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

#Determine whether to include lat and long
m7 <- bam(cpue ~ Year + Gear + s(depth, k=4) + s(soak, k=4) + s(Hook_size, bs='re', by=dum) + s(Stat, bs='re', by=dumstat) + s(Adfg, bs='re', by=dum) + te(start_lon, start_lat), data=fsh_cpue, gamma=1.4)
m8 <- bam(cpue ~ Year + Gear + s(depth, k=4) + s(soak, k=4) + s(Hook_size, bs='re', by=dum) + s(Stat, bs='re', by=dumstat) + s(Adfg, bs='re', by=dum) + s(start_lon), data=fsh_cpue, gamma=1.4)
m9 <- bam(cpue ~ Year + Gear + s(depth, k=4) + s(soak, k=4) + s(Hook_size, bs='re', by=dum) + s(Stat, bs='re', by=dumstat) + s(Adfg, bs='re', by=dum) + s(start_lat), data=fsh_cpue, gamma=1.4)

AIC(m6, m7, m8, m9)

summary(m7)
summary(m8)
summary(m9)

# m9, the model with the latitudinal effect, performs best by AIC, but only
# results in a slight improvement in the dev explained. Try limitting the number
# of knots to guard against overfitting... but m9 still performs best by AIC.
m10 <- bam(cpue ~ Year + Gear + s(depth, k=4) + s(soak, k=4) + s(Hook_size, bs='re', by=dum) + s(Stat, bs='re', by=dumstat) + s(Adfg, bs='re', by=dum) + s(start_lat, k=6), data=fsh_cpue, gamma=1.4)
AIC(m6, m7, m8, m9, m10)

plot(m9, page = 1, shade = TRUE, all = TRUE) #resid = TRUE,
plot(m10, page = 1, shade = TRUE, all = TRUE) #resid = TRUE,

# m7 with both lat and lon with a tensor smoother has the second best
# performance. red/orange is higher CPUE, green average and blue lower; can
# change "too.far" values to change what shows on graph. Highest cpue in the
# north, south and central chatham
vis.gam(m7, c('start_lon','start_lat'), type='response', plot.type='contour', color='topo', too.far=0.1)

# The inclusion of a seasonal effect slightly improves model fit - there is a
# slightly decreasing trend in cpue on average over the course of the season.
m11 <- bam(cpue ~ Year + Gear + s(julian_day, k=4) + s(depth, k=4) + s(soak, k=4) + s(start_lat) + s(Hook_size, bs='re', by=dum) + s(Stat, bs='re', by=dumstat) + s(Adfg, bs='re', by=dum), data=fsh_cpue, gamma=1.4)
AIC(m9, m11)
summary(m11)
plot(m11, page = 1, shade = TRUE, all = TRUE) #resid = TRUE,

# Relationship between depth and soak time - highest cpue in > 450 m
# and ~ 10 hr soak time
vis.gam(m11, c('depth', 'soak'), plot.type='contour', type='response', color='topo', too.far=0.15)

# GAM summary ----

# Final model structure (m11) (* = random effect):
# CPUE ~ Year + Gear + s(julian day) + s(depth) + s(soak time) + s(latitude) + hooksize* + statarea* + vessel* 

# 36.7% deviance explained

# CPUE decreases throughout the season. CPUE increases with depth, then
# asymptotes ~ 450 m. CPUE is constant and then drops off ~ 10 hr soak time,
# possibly due to sandfleas or hagfish. CPUE is greatest in the northern and
# southern parts of chatham.

# The overall effect of julian day, soak time, and latitude is weaker than
# depth. Conventional gear performs slightly better than autobaiter gear. 

# Predictions ----

#Create standard dataset to get standardized CPUE for each year

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
         bt_se = (bt_upper - bt_cpue) / 2,
         bt_se / b) -> std_dat

# Nominal CPUE ----

fsh_cpue %>% 
  group_by(year) %>% 
  summarise(annual_cpue = mean(std_cpue),
            sdev = sd(std_cpue),
            n = length(std_cpue),
            se = sdev / (n ^ (1/2)),
            var = var(std_cpue),
            cv = sdev / annual_cpue,
            upper = annual_cpue + (2 * se),
            lower = annual_cpue - (2 * se)) -> fsh_sum 

# Compare predicted cpue from gam to nominal cpue
fsh_sum %>%
  select(year, cpue = annual_cpue, upper, lower) %>% 
  mutate(CPUE = "Nominal") %>% 
  bind_rows(std_dat %>% 
              select(year, cpue = bt_cpue, upper = bt_upper, lower = bt_lower) %>% 
              mutate(CPUE = "GAM")) %>% 
  ggplot() +
  geom_point(aes(year, cpue, colour = CPUE, shape = CPUE), size = 2) +
  geom_line(aes(year, cpue, colour = CPUE, group = CPUE), size = 1) +
  geom_ribbon(aes(year, ymin = lower, ymax = upper, fill = CPUE), 
              colour = "white", alpha = 0.2) +
  scale_colour_manual(values = c("darkcyan", "goldenrod"), name = "Standardized CPUE") +
  scale_fill_manual(values = c("darkcyan", "goldenrod"), name = "Standardized CPUE") +
  scale_shape_manual(values = c(19, 17), name = "Standardized CPUE") +
  scale_x_continuous(breaks = seq(min(fsh_cpue$year), YEAR, 4)) +
  labs(x = "", y = "Fishery CPUE (lbs/hook)\n") +
  theme(legend.position = "bottom")

ggsave("figures/compare_stdcpue_llfsh.png", dpi=300, height=4, width=7, units="in")

# Percent change in fishery nominal cpue compared to a ten year rolling average
fsh_sum %>% 
  filter(year > YEAR - 10) %>% 
  mutate(lt_mean = mean(annual_cpue),
         perc_change_lt = (annual_cpue - lt_mean) / lt_mean * 100) 

# Percent change in fishery nominal cpue from last year
fsh_sum %>% 
  filter(year >= YEAR - 1) %>%
  select(year, annual_cpue) %>% 
  dcast("annual_cpue" ~ year) %>% 
  mutate(perc_change_ly = (`2017` - `2016`) / `2016` * 100)

# Historical CPUE ----

# From KVK: Logbooks were not included in IFDB until 1997. Commercial fishery
# CPUE values prior to 1997, for use in the ASA or other medium, are LEGACY
# VALUES. Jane updates: I don't have any source information from these numbers
# other than this. Kray kept them in a csv file called
# data/legacy_fishery_cpue.csv. Similarly, I moved and renamed the same file as
# data/fishery/legacy_fisherycpue_1980_1996.csv

read_csv("data/fishery/legacy_fisherycpue_1980_1996.csv", 
           col_names = FALSE) %>% as.numeric() -> hist_cpue

# Because the variance is easier to interpret and the point estimates from the
# GAM are extremely similar to nominal CPUE, use nominal CPUE. Use the mean CV
# from 1997-present to estimate the variance for legacy CPUE values, following
# KVK.
data.frame(year = 1980:1996,
           cpue = hist_cpue) %>% 
  mutate(var = (cpue * mean(fsh_sum$cv)) ^ 2) %>% 
  bind_rows(fsh_sum %>% 
              select(year, cpue = annual_cpue, var)) %>% 
  mutate(cpue = round(cpue, 3),
         var = round(var, 3)) -> cpue_ts

ggplot(cpue_ts) +
  geom_point(aes(year, cpue)) +
  geom_line(aes(year, cpue)) +
  geom_ribbon(aes(year, ymin = cpue - sqrt(var), ymax = cpue + sqrt(var)),
  # geom_ribbon(aes(year, ymin = cpue - var, ymax = cpue + var),
              alpha = 0.3, col = "white", fill = "skyblue") +
  scale_x_continuous(breaks = seq(min(cpue_ts$year), YEAR, 4)) +
  labs(x = "", y = "Fishery CPUE (lbs/hook)\n") 

ggsave(paste0("figures/nominalwpue_1sd_llfsh_", min(cpue_ts$year), "_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

# Write to file
write_csv(cpue_ts, paste0("output/nominalwpue_var_llfsh_", min(cpue_ts$year), "_", YEAR, ".csv"))
