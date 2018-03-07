
# Fishery catch 1985-present, fishery CPUE 1997-present
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2018-01-12


# Also used for requested vessel-specific cpue

# Most recent year of data
YEAR <- 2017

# Load ----

source("r_code/helper.R")

read_csv(paste0("data/fishery/fishery_cpue_1997_", YEAR,".csv"), 
                 guess_max = 50000) %>% 
  mutate(Year = factor(year), 
         Adfg = factor(Adfg),
         Spp_cde = factor(Spp_cde), 
         Gear = factor(Gear), # 01=Conventional, 02=Snap On, 05=Mixed, 06=Autobaiter
         Hook_size = factor(Hook_size), 
         Size = factor(Size),
         Stat = factor(Stat),
         sable_kg_set = sable_lbs_set * 0.45359237, # conversion lb to kg
         std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (0.0254 * hook_space))), #standardize hook spacing (Sigler & Lunsford 2001, CJFAS)
         # To maintain consistency with Mateo & Hanselman 2014 (NMFS-AFSC-269),
         # we define CPUE as the logarithm of catch in kg + 1 per 1000 hooks. What's
         # not clear is whether the hook spacing standardization should be
         # included in this (Kray included it), so I do it both ways for
         # comparison.
         nonstd_cpue = log(((sable_kg_set / no_hooks) * 1000 ) + 1),
         std_cpue = log(((sable_kg_set / std_hooks ) * 1000 ) + 1)
  ) %>% 
  filter(!is.na(date) & #!is.na(hook_space) &
           !is.na(sable_lbs_set) &
           julian_day > 226 & # if there were special projects before the fishery opened
           no_hooks < 15000) %>%  # 15000 in Kray's scripts. Seems like a lot of hooks in a set. 14370 is the 75th percentile
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
    total_trips = n_distinct(trip_no),
    # Arithetic annual means, aka nominal cpue
    arithm_nonstdcpue = mean(nonstd_cpue, na.rm = TRUE),
    arithm_stdcpue = mean(std_cpue, na.rm = TRUE),
    # Harmonic annual means (dampens outliers, might be most appropropriate)
    harm_nonstdcpue = 1 / mean(1 / nonstd_cpue, na.rm = TRUE),
    harm_stdcpue = 1 / mean(1 / std_cpue, na.rm = TRUE) ) %>% 
  ungroup() -> cpue

# Just for industry presentation *FLAG*

read_csv(paste0("data/fishery/fishery_cpue_1997_", YEAR,".csv"), 
         guess_max = 50000) %>% 
  mutate(Year = factor(year), 
         std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (0.0254 * hook_space))), #standardize hook spacing (Sigler & Lunsford 2001, CJFAS)
         std_cpue = log(sable_lbs_set / std_hooks) + 1) %>% 
  filter(!is.na(date) & #!is.na(hook_space) &
           !is.na(sable_lbs_set) &
           julian_day > 226 & # if there were special projects before the fishery opened
           no_hooks < 15000) -> fsh_cpue  # 15000 in Kray's scripts. Seems like a lot of hooks in a set. 14370 is the 75th percentile

hist(fsh_cpue$std_cpue)

fsh_cpue %>% 
  group_by(year) %>% 
  summarise(annual_cpue = exp(mean(std_cpue, na.rm = TRUE)) - 1,
            # nn = length(std_cpue),
            sdev = exp(sd(std_cpue, na.rm = TRUE)) - 1,
            # std_error = sdev / sqrt(nn),
            upper = annual_cpue + sdev,
            lower = annual_cpue - sdev) %>% 
  mutate(lower = ifelse(lower < 0, 0, lower)) -> fsh_sum #-> srv_cpue

# figures

ggplot(fsh_sum) +
  # geom_jitter() + 
  geom_point(aes(year, annual_cpue)) +
  geom_line(aes(year, annual_cpue)) +
  # geom_ribbon(aes(year, ymin = CIlower, ymax = CIupper),
  geom_ribbon(aes(year, ymin = lower, ymax = upper),
              alpha = 0.3, col = "white", fill = "skyblue") +
  labs(x = "", y = "Pounds of sablefish per hook\n") #+
  # lims(y = c(0, 2))# theme(plot.title = element_text(hjust = .5)) +
# geom_vline(xintercept = 9.5, linetype = 2, col = "grey")

ggsave("figures/wpue_llfsh.png", 
       dpi=300, height=4, width=7, units="in")

#Vessel of interest if looking at cpue by vessel for a private request
VESSEL_REQUESTED <- "21465" # adfg number

#individual vessel cpue
vessel_cpue <- cpue %>% filter(Adfg == VESSEL_REQUESTED)

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
       aes(x = factor(year), y = total_pounds/1000000)) +
  geom_line(group=1) +
  geom_point() +
  labs(x = "", y = "Millions lbs\n") +
  ylim(0, 6)

ggsave(paste0("figures/fishery_harvest_1985_", YEAR, ".png"), 
       dpi=300, height=5, width=8, units="in")
# figures ----

# for most figs, the `data` could be inter-changed for vessel_cpue or cpue depending on your interest.

# Number of vessels fishing and total number of trips in Chatham over time
png(file = paste0('figures/fishery_tripandvessel_trends_1997_', YEAR, '.png'),
    res = 300, width = 7, height = 3.5, units = "in", bg = "transparent")

df <- cpue %>% 
  select(Year, total_vessels, total_trips) %>% 
  gather(Variable, Count, -Year) %>% 
  distinct()       

ggplot(df, aes(x = Year, y = Count, colour = Variable, 
           shape = Variable, group = Variable)) +
  geom_line() +
  geom_point() +
  # geom_vline(xintercept = which(df$Year == "2011"), lty = 2) +
  scale_y_continuous(breaks = seq(0, 400, 50), limits = c(0, 400)) +
  labs(x = "")

dev.off()

# nominal cpue over time, both standardized and unstandardized, using arithmetic
# and harmonic means

png(file = paste0('figures/nominal_fisherycpue_1997_', YEAR, '.png'),
    res = 300, width = 7, height = 3.5, units = "in", bg = "transparent")

df <- cpue %>% 
  select(Year, contains("arith"), contains("harm")) %>% 
  gather(CPUE, Index, -Year) %>% 
  distinct() 

ggplot(df, aes(x = Year, y = Index, colour = CPUE, 
               shape = CPUE, group = CPUE)) +
  geom_line() +
  geom_point() +
  labs(x = "", y = "CPUE Index")

dev.off()

# cpue by julian day
ggplot(cpue, aes(julian_day, std_cpue)) +
  geom_jitter() +
  stat_smooth(method='lm', se=FALSE) + 
  ylab('Fishery CPUE\n') +
  xlab('\nJulian day') 

# cpue by stat area
ggplot(cpue, aes(Stat, std_cpue)) +
  geom_boxplot() +
  ylab('Fishery CPUE\n') +
  xlab('\nStat area') 

table(cpue$Stat)

#Plot trends in cpue and catch in core stat areas
core_areas <- c("335701", "345603", "345631", "345701",
                "345702", "345731", "345803")
#areas with strong trends in catch over time
core_areas <- c("345603", "345631", "345701",
                "345731", "345803")

cpue_year_area <- cpue %>% 
  filter(Stat %in% core_areas) %>% 
  group_by(Stat, Year) %>% 
  summarise(catch = sum(sable_lbs_set),
            cpue = mean(std_cpue))

p_catch <- ggplot(cpue_year_area, aes(Year, catch, group=1)) +
  geom_point() +
  geom_line() + 
  facet_wrap(~Stat) +
  theme(axis.text.x = element_text(size=10, angle=45, hjust=1))

p_cpue <- ggplot(cpue_year_area, aes(Year, cpue, group=1)) +
  geom_point() +
  geom_line() + 
  facet_wrap(~Stat) +
  theme(axis.text.x = element_text(size=10, angle=45, hjust=1))

grid.arrange(p_catch, p_cpue)

ggplot(cpue_year_area, aes(cpue, catch, group=Stat, col=Stat)) +
  geom_point() +
  geom_smooth(method='gam',  alpha=0.1) +#formula= y ~ s(x, k=12), 
  xlab("CPUE") +
  ylab("Catch (lbs)")

# cpue by julian day, stat area
# png(file='figures/fsh_cpue_day.png', res=300, width=7, height=3.5, units ="in", bg="transparent")  
ggplot(vessel_cpue, aes(julian_day, std_cpue, col=Stat, group=1)) + 
  geom_jitter() +
  stat_smooth(aes(julian_day, std_cpue), alpha=.2, method='lm') +
  xlab("\nJulian day (from start of year)") +
  ylab("CPUE") 
# dev.off()

# cpue by depth
ggplot(cpue, aes(depth, std_cpue)) +
  geom_jitter() +
  stat_smooth(method='gam', formula= y ~ s(x, k=4)) + 
  xlab("\nDepth (meters)") + # *FLAG* - check units
  ylab("CPUE") +
  geom_vline(xintercept = 450, lty = 2) 

# cpue by depth, stat area
# png(file='figures/fsh_cpue_depth.png', res=300, width=7, height=3.5, units ="in", bg="transparent")  
ggplot(cpue, aes(depth, std_cpue, col=Stat, group=1)) +
  geom_jitter() +
  stat_smooth(alpha=.2, method='gam', formula= y ~ s(x, k=4)) +
  xlab("\nDepth (meters)") + # *FLAG* - check units
  ylab("CPUE") 
# dev.off()

# catch by depth and hook size
ggplot(vessel_cpue, aes(depth, sable_lbs_set, col=Size, group=1)) +
  geom_jitter()+
  stat_smooth(alpha=.2, method='gam', formula= y ~ s(x, k=4)) +
  xlab("\nDepth (meters)") + 
  ylab("Catch (lbs)")

# Hook size and cpue - looks like most of the hooks used are 13s and 14s and
# they don't have a measurable difference in cpue
ggplot(cpue, aes(Size, std_cpue)) +
  geom_boxplot() +
  # facet_wrap(~Stat) +
  xlab("\nHook Size") +
  ylab("CPUE") 
table(cpue$Size)

# Compare vessel cpue with longterm average cpue *FLAG* - this doesn't 
# png(file='figures/vessel.png', res=300, width=7, height=3.5, units ="in", bg="transparent")  
ggplot() + 
  geom_boxplot(data=cpue, aes(Year, std_cpue), fill="lightgrey") + 
  geom_line(data=overall_cpue, aes(Year, overall_cpue, col="Overall CPUE", group=1), lwd=1) +
  stat_summary(data=vessel_cpue, aes(Year, std_cpue, col="Vessel CPUE"), 
               fun.y='mean', geom="line", group=1, lwd=1.5, lty=2) +
  xlab("\nYear") +
  ylab("CPUE") 
# dev.off()

#Number of sets by Stat and year
ggplot(cpue %>% filter(Stat %in% core_areas), aes(Year)) +
  geom_bar() + 
  facet_wrap(~Stat) +
  theme(axis.text.x = element_text(size=10, angle=45, hjust=1))

# CPUE and catch by vessel
plot_cpue <- ggplot(cpue, aes(x=(reorder(Adfg, std_cpue)), y=std_cpue)) +
  geom_boxplot() +
  xlab("Individual vessels in the Chatham Strait longline fishery")+
  ylab("CPUE") +
  theme(axis.text.x = element_text(size=10, angle=45, hjust=1))

plot_catch <- ggplot(cpue, aes(x=(reorder(Adfg, sable_lbs_set)), y=sable_lbs_set)) +
  geom_boxplot() +
  xlab("Individual vessels in the Chatham Strait longline fishery")+
  ylab("Catch (lbs)") +#*FLAG* check units
  theme(axis.text.x = element_text(size=10, angle=45, hjust=1))  

grid.arrange(plot_cpue, plot_catch)

# catch and number of hooks in a set
ggplot(cpue, aes(no_hooks, sable_lbs_set, group=1)) +
  geom_point() +
  stat_smooth(method='gam', formula=y~s(x, k=4)) +
  xlab("\nTotal Number of Hooks") +
  ylab("Catch (lbs)")  #*FLAG check units
  

# cpue and number of hooks in a set
ggplot(cpue, aes(no_hooks, std_cpue, group=1)) +
  geom_point() +
  stat_smooth(method='gam', formula=y~s(x, k=4)) +
  xlab("\nTotal Number of Hooks") +
  ylab("CPUE")


# catch and number of sets
ggplot(cpue, aes(sets, sable_lbs_set, group=1)) +
  geom_point() +
  stat_smooth(method='gam', formula=y~s(x, k=4)) +
  xlab("\nTotal Number of Sets") +
  ylab("Catch (lbs)") #*FLAG check units

# catch and number of sets
ggplot(cpue, aes(sets, std_cpue, group=1)) +
  geom_point() +
  stat_smooth(method='gam', formula=y~s(x, k=4)) +
  xlab("\nTotal Number of Sets") +
  ylab("CPUE") 

# CPUE pdf 
ggplot(cpue, aes(std_cpue)) +
  geom_density(fill=4, alpha=.5)

# CPUE pdfs by stat areas 
ggplot(cpue, aes(std_cpue, fill=Stat)) +
  geom_density(alpha=.3)


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
# Size - optimal hook size? No consistent trend between years. Dropped this from
# the model after preliminary eda.
# start_lat - is there some consistent trend in Chatham Strait going north into
# Chatham? Not one that is consistent between years. If one exists it tends to
# be decreasing with latitude. There was no spatial autocorrelation detected.
# start_lat/start_lon - spatial autocorrelation

# Original explorations looking for consistent trends in factors between years,
# and spatial/temporal dependence

cpue %>% filter(!is.na(std_cpue),
                !is.na(depth),
                !is.na(start_lon),
                !is.na(start_lat),
                !is.na(Gear)
                # Gear: Mostly '01' (conventional) and '06' (autobaiters).
                # Exploration shows significant differences between 06 and 01
                # but not 02/05 and 01. Create new variable that is just
                # autobaiter or not
                # gam_cpue %>% group_by(Year, Gear) %>% summarize(n = n()) %>%
                #   dcast(Year ~ Gear, value.var = "n") %>% View()
                # Gear %in% c('01', '06')
                ) %>% 
  mutate(Gear = derivedFactor("AB" = Gear == "06",
                              "CS" = Gear %in% c("01","02","05"))) -> gam_cpue

# Aaron B. pointed out this vessel who is the only one who fishes snap-on gear
# gam_cpue %>% filter(Adfg == "17838" & Gear == "02") %>% View() 

# cpue %>% group_by(Year, Gear) %>% summarize(n = n()) %>%
#   dcast(Year ~ Gear, value.var = "n") %>% View()
# 
# cpue %>% group_by(Year) %>% 
#   summarise(min = min(julian_day),
#             max = max(julian_day),
#             lengthdays = max - min) %>% View()

library(sp)
library(gstat)

gam_cpue %>% 
  filter(Year == "2017") -> tst

fit <- gam(std_cpue ~ s(depth, k = 3) + 
             s(julian_day, k = 4) +
             s(start_lat, k = 4) +
             s(Adfg, bs="re") +
             #Size +
             Gear,
           family = gaussian, 
           data = tst) 

summary(fit)
#Each year after 2002 the R^2 increases steadily and the Gear effect becomes
#less reliably significant
plot(fit, shade=TRUE, residuals=TRUE, page=1, all=TRUE)

tstdat <- data.frame(resids = fit$residuals,
                     lon = tst$start_lon,
                     lat = tst$start_lat)

# Temporal autocorrelation - some consistant evidence of first order
# autocorrelation, esp. in earlier years of time series. Not signficant in 2002,
# 2007, 2008, 2009, 2010, 2012, 2014, 2015, 2016, or 2017.  Negative first order
# in 2011.
acf(tstdat$resids)

# Spatial autocorrelation
coordinates(tstdat) <- c("lon", "lat")
bubble(tstdat, "resids", col = c("black", "grey"))
vario <- variogram(resids ~ 1, tstdat)
plot(vario)

#


# Model structure with Year and Gear as factors, depth, julian day, and latitude
# as smooth terms. Drop hook size as a potential factor. Fit correlation
# structure within each year (residuals correlated). Test fit for 1st, 2nd, and 3rd order
# autocorrelation. The control list maybe boosts speed a little, beware that slight
# variants in model structure can result in really slow processing.


# fitting with BAM

bam0 <- bam(std_cpue ~ Gear + Year + 
              # Accomodate different trends in julian_day and latitude by year
              s(julian_day, k = 4) +
              s(start_lat, k = 4) +
              s(depth, k = 3),
            dat = gam_cpue)

bam1 <- bam(std_cpue ~ Gear + Year + 
              # Accomodate different trends in julian_day and latitude by year
              s(julian_day, k = 4, by = Year) +
              s(start_lat, k = 4, by = Year) +
              s(depth, k = 3) +
              s(Adfg, bs = "re"),
            dat = gam_cpue)

AIC(bam0, bam1)

summary(bam0)
summary(bam1)

plot(bam0,  residuals=TRUE, pch=19, rug=FALSE, seWithMean = TRUE,
     shade=2, all=TRUE) #shift=mean(gam_cpue$std_cpue), trans=exp, se=TRUE,

plot(bam1,  residuals=TRUE, pch=19, rug=FALSE, seWithMean = TRUE,
     shade=2, all=TRUE) #shift=mean(gam_cpue$std_cpue), trans=exp, se=TRUE,

par(mfrow=c(2,2))
gam.check(bam0)
gam.check(bam1)

# Not terrible but it looks like there are strongly negatively skewed residuals,
# especially in early years. It's < 50 observations (out of > 15,000), so I
# think I'm just going to delete these observations from the analysis. I'm going
# to omit them from the analysis
gam_cpue %>% 
  mutate(resids = resid(bam1)) %>% 
  filter(!resids < -2.0) %>% 
  select(-resids) -> gam_cpue

bam1 <- bam(std_cpue ~ Gear + Year + 
              # Accomodate different trends in julian_day and latitude by year
              s(julian_day, k = 4, by = Year) +
              s(start_lat, k = 4, by = Year) +
              s(depth, k = 3) +
              s(Adfg, bs = "re"),
            dat = gam_cpue)

summary(bam1)

plot(bam1,  residuals=TRUE, pch=19, rug=FALSE, seWithMean = TRUE,
     shade=2, all=TRUE) #shift=mean(gam_cpue$std_cpue), trans=exp, se=TRUE,

par(mfrow=c(2,2))
gam.check(bam1) # Much better. ;)

# Random effects on vessel
re_vessels <- tidy(bam1$coefficients) %>% 
  filter(grepl("Adfg", names)) %>% 
  mutate(Adfg = levels(gam_cpue$Adfg),
         random_effect = round(x, 1)) %>% 
  select(Adfg, random_effect)

#Evidence that normality assumption on vessel effects is violated. Positively
#skewed.
shapiro.test(re_vessels$random_effect)
hist(re_vessels$random_effect)

# #A lot of vessels only fished one or two sets... cut off vessels such that they
# #at least had to have fished 20 sets (20 based on looking at distribution of
# #trips/vessel)
# gam_cpue %>% 
#   group_by(Adfg) %>% 
#   summarize(mu = mean(std_cpue),
#             n = n()) %>% 
#   filter(n < 20) %>% 
#   distinct(Adfg) %>% c() -> inexperienced_Adfg
# 
# gam_cpue %>% filter(!Adfg %in% inexperienced_Adfg) -> gam_cpue
# 
# bam1 <- bam(std_cpue ~ Gear + Year + 
#               # Accomodate different trends in julian_day and latitude by year
#               s(julian_day, k = 4, by = Year) +
#               s(start_lat, k = 4, by = Year) +
#               s(depth, k = 3) +
#               s(Adfg, bs = "re"),
#             dat = gam_cpue)
# 
# summary(bam1)
# 
# plot(bam1,  residuals=TRUE, pch=19, rug=FALSE, seWithMean = TRUE,
#      shade=2, all=TRUE) #shift=mean(gam_cpue$std_cpue), trans=exp, se=TRUE,
# 
# par(mfrow=c(2,2))
# gam.check(bam1)

# Posts to revisit:
# https://www.fromthebottomoftheheap.net/2017/03/21/simultaneous-intervals-for-derivatives-of-smooths/ 
# https://www.fromthebottomoftheheap.net/2016/12/15/simultaneous-interval-revisited/ 
# http://www.sfs.uni-tuebingen.de/~jvanrij/Tutorial/GAMM.html#testing-for-significance
# https://cran.r-project.org/web/packages/broom/vignettes/bootstrapping.html
# https://stats.stackexchange.com/questions/63652/how-does-bootstrapping-in-r-actually-work 

# Bootstrap function 

# Bootstrap sample size: https://stats.stackexchange.com/questions/37918/why-is-the-error-estimated-adjustment-a-is-na-generated-from-r-boot-package
# (the subscript i in the function is used in the function 'boot' below):
# the function resamples the residuals, creates a new bootstrap dataset,
# fits the model to the bootstrapped dataset, and returns predicted values

dat <- cbind(gam_cpue, fit = fitted(bam1))

bf <- function(resids, i) {
  # create a bootstrapped dataset by adding resampled residuals 
  # to the fitted values:
  dat$std_cpue <- dat$fit + resids[i] # creating new data set
 
  res <- seq(50, 1000, 50)
  
  # Use the mode for predicting the mostly values for julian_day and start_lat
  mode_julian_day <- as.numeric(names(sort(-table(gam_cpue$julian_day)))[1])
  mode_start_lat <- as.numeric(names(sort(-table(gam_cpue$start_lat)))[1])

  # New dataframe for prediction. I use Gear = "CS" (this is conventional gear,
  # including some snap-on) for prediction because it's way more prevalent than
  # autobaiters
  pdat <- data.frame(Year = rep(levels(gam_cpue$Year), length(res)),
                     depth = rep(res, length(levels(gam_cpue$Year))),
                     julian_day = rep(mode_julian_day, length(res) * length(levels(gam_cpue$Year))),
                     start_lat = rep(mode_start_lat, length(res) * length(levels(gam_cpue$Year))),
                     Gear = rep("CS", length(res) * length(levels(gam_cpue$Year))),
                     Adfg = rep(vessel$Adfg, length(res) * length(levels(gam_cpue$Year))))
     # Fit the model to this new bootstrapped data set:
  fit_boot <- bam(std_cpue ~ Gear + Year + 
                    # Accomodate different trends in julian_day and latitude by year
                    s(julian_day, k = 4, by = Year) +
                    s(start_lat, k = 4, by = Year) +
                    s(depth, k = 3) +
                    s(Adfg, bs = "re"),
                  dat = dat)
  
  # Return predicted values:
  predict(bam1, pdat)   
}

library(boot)

# The 'boot' function uses the above function ('bf') to do the bootstrapping,
# i.e. it runs bf with 1000 different sets of resampled residuals from 'resid(fit)':) 
cpue_boot <- boot(resid(bam1), bf, R = 20000) # should be > length of data set 
save(cpue_boot, file="output/fishery_cpue_bootstrap.rda", compress='xz')
load("output/fishery_cpue_bootstrap.rda")

boot.ci(cpue_boot, index = 2)

res <- seq(50, 1000, 50)

# Use the mode for predicting the mostly values for julian_day and start_lat
mode_julian_day <- as.numeric(names(sort(-table(gam_cpue$julian_day)))[1])
mode_start_lat <- as.numeric(names(sort(-table(gam_cpue$start_lat)))[1])

vessel <- tidy(bam1$coefficients) %>% 
  filter(grepl("Adfg", names)) %>% 
  mutate(Adfg = levels(gam_cpue$Adfg),
         random_effect = round(x, 1)) %>% 
  select(Adfg, random_effect) %>% 
  filter(random_effect == 0.0) %>% 
  sample_n(1)

# New dataframe for prediction. I use Gear = "CS" (this is conventional gear,
# including some snap-on) for prediction because it's way more prevalent than
# autobaiters
pdat <- data.frame(Year = rep(levels(gam_cpue$Year), length(res)),
                   depth = rep(res, length(levels(gam_cpue$Year))),
                   julian_day = rep(mode_julian_day, length(res) * length(levels(gam_cpue$Year))),
                   start_lat = rep(mode_start_lat, length(res) * length(levels(gam_cpue$Year))),
                   Gear = rep("CS", length(res) * length(levels(gam_cpue$Year))),
                   Adfg = rep(vessel$Adfg, length(res) * length(levels(gam_cpue$Year))),
                   t0 = cpue_boot$t0)

# Predict fitted value and se
pdat$fit <- predict(bam1, pdat)
pdat$se <- predict(bam1, pdat, se.fit = TRUE)$se
pdat <- bind_cols(
  pdat,

  data.frame(cpue_boot$t) %>% 
    t() %>% 
    data.frame()) %>% 
  gather("replicate", "value", contains("X"))


#Pick a depth for plotting
dat <- pdat %>% filter(depth == 550 )

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right
dat <- dat %>%   
         # randomly select 20% of the bootstrap replicates for plotting
         # sample_frac(0.05, replace = FALSE) 
       filter(replicate == "X1")

ggplot() +
  geom_point(data = dat, aes(Year, fit)) + 
  geom_line(data = dat, aes(Year, fit, group = 1)) +
  geom_errorbar(data = dat, aes(x = Year, ymin = fit - 2*se,
                    ymax = fit + 2*se, group = 1),
                width = .3, position = pd) +
  # geom_line(data = gam_cpue, aes(x = Year, y = arithm_stdcpue, group = 1), col = "red") +
  # geom_point(data = gam_cpue, aes(x = Year, y = arithm_stdcpue), col = "red") +
  # labs(x = "", y = "CPUE Index") +
  labs(x = "", y = "Fishery CPUE  (ln(catch in kg + 1) / 1000 hooks)\n")
  # geom_line(aes(Year, t0, group = 1), col = "red") +
  # geom_line(aes(Year, value, group = replicate), alpha = 0.2, col = "grey")

ggsave(paste0("figures/fishery_gam_cpue_1997_", YEAR, ".png"), 
       dpi=300, height=5, width=8, units="in")

##
df <- cpue %>% 
  select(Year, contains("arith"), contains("harm")) %>% 
  gather(CPUE, Index, -Year) %>% 
  distinct() 

ggplot() +
  geom_line(data = cpue, aes(x = Year, y = arithm_stdcpue, group = 1)) +
  geom_point(data = cpue, aes(x = Year, y = arithm_stdcpue)) +
  labs(x = "", y = "CPUE Index")



plot_smooth(bam2, view = "year", cond=list(Gear="CS", depth = 300))

install.packages('itsadug')
library(itsadug)
check_resid(model = fit1$gam)


# New dataframe for prediction. I use Gear = "CS" (this is conventional gear,
# including some snap-on) for prediction because it's way more prevalent than
# autobaiters
pdat <- data.frame(Year = rep(levels(gam_cpue$Year), length(res)),
                   depth = rep(res, length(levels(gam_cpue$Year))),
                   julian_day = rep(mode_julian_day, length(res) * length(levels(gam_cpue$Year))),
                   start_lat = rep(mode_start_lat, length(res) * length(levels(gam_cpue$Year))),
                   Gear = rep("CS", length(res) * length(levels(gam_cpue$Year))) ) %>%
  arrange(Year, depth) 

# Compare fitted values and error between fit0 and fit1                   
pdat$fit0 <- exp(predict(fit0$gam, pdat)) - 1
pdat$ci0 <-  exp(1.96 * predict(fit0$gam, pdat, se = TRUE)$se )  - 1
pdat$ci01 <- exp(predict(fit0$gam, pdat, interval = "confidence")) - 1
pdat$pi <- exp(predict(fit0$gam, pdat, interval = "prediction")) - 1
pdat$fit1 <- exp(predict(fit1$gam, pdat)) - 1
pdat$ci1 <- 1.96 * exp(predict(fit1$gam, pdat, se = TRUE)$se) - 1
# pdat$fit2 <- exp(predict(fit2$gam, pdat)) - 1
# pdat$ci2 <- 1.96 * exp(predict(fit2$gam, pdat, se = TRUE)$se) - 1
# pdat$fit3 <- exp(predict(fit3$gam, pdat)) - 1
# pdat$ci3 <- 1.96 * exp(predict(fit3$gam, pdat, se = TRUE)$se) - 1

predict(fit0$gam, interval = "prediction")



View(pdat)

pdat %>% 
  gather(model, fitted, contains("fit")) %>% 
  gather(model1, conf, contains("ci")) %>% 
  select(-model1) -> pdat

# All data combined ~550 m is the mode depth
as.numeric(names(sort(-table(gam_cpue$depth)))[1])


dat <-  pdat %>% filter(depth == 550)

ggplot(data = dat) +
  geom_point(aes(Year, fitted, colour = model)) + 
  geom_line(aes(Year, fitted, colour = model, group = model)) +
  geom_errorbar(aes(x = Year, ymin = fitted - conf,
                    ymax = fitted + conf, 
                    colour = model, group = model),
                width = .3, position = pd) #+
  # 1st order
  geom_point(aes(Year, fitted_1), col = "red") + 
  geom_line(aes(Year, fitted_1, group=1), col = "red") +
  geom_errorbar(aes(x = Year, ymin = fitted_1 - ci_1,
                    ymax = fitted_1 + ci_1), 
                col = "red", width = .3, position = pd)

#, se = TRUE)$se.fit
length(unique(gam_cpue$year)) 
mean(gam_cpue$depth)




library(lmtest)
dwtest(fit0$gam)
  20*21
intervals(fit1$lme)
hist(vessels$X.Intercept.)
vessels %>% filter(X.Intercept. > 0.6)
# One outlier vessel who has really high CPUE and falls out of the "normal" curve
gam_cpue %>% filter(Adfg == "15369") %>% View()
names(vessels)
rm(fit0)
plot(fit2$gam, shade=TRUE, residuals=TRUE, page=1, all=TRUE)
plot(fit3$gam, shade=TRUE, residuals=TRUE, page=1, all=TRUE)
plot(fit1$gam, se=TRUE, residuals=FALSE, pch=19, rug=FALSE, 
     shift=mean(gam_cpue$std_cpue),shade=2, all=TRUE, trans=exp, page = 1)
ranef




# install.packages("MuMIn")
install.packages("maps")
install.packages("mapdata")
install.packages("voxel")
library(MuMIn)
library(maps)
library(mapdata)
library(voxel)

options(na.action = "na.fail")
out <- dredge(fit, extra = alist(AIC, BIC, ICOMP, Cp))#
par(mar = c(3,5,6,4))
plot(out, labAsExpr = TRUE)
model.avg(out, subset = delta < 4)
best.fit <- get.models(out, 1)[[1]]
summary(best.fit)
plot(best.fit, shade=TRUE, residuals=TRUE, page=1, all=TRUE)
plotGAM(gamFit = best.fit, 
        smooth.cov = c("start_lon", "start_lat"), 
        groupCovs = NULL, rawOrFitted = "raw", orderedAsFactor = FALSE)


View(out)
vis.gam(best.fit, c("start_lon", "start_lat"),
        plot.type = "contour", color = "topo", 
        type = "response", gg = TRUE)       
map('worldHires', fill = T, xlim = c(-133,-137),
    ylim=c(56,58.5),add=T)

plot.gam()

fit <- gam(std_cpue ~ Year + Stat + s(depth, k=4) + s(julian_day, k=4) + 
             s(total_vessels, k=4) +
             s(no_sets, k=4) , 
           data = cpue) #, gamma=1.4
           

length(which(is.na(cpue$depth)))
summary(fit)

fit2 <- gam(std_cpue ~ s(depth, k=4) + s(julian_day, k=4) +  s(total_vessels, k=4) +
             s(no_sets, k=4) + s(year) + Stat, 
           data = cpue) #, gamma=1.4

plot(fit2, shade=TRUE, residuals=TRUE, page=1, all=TRUE)
summary(fit2)


newd <- expand.grid(julian_day = min(cpue$julian_day), max(cpue$julian_day), 
                    depth = seq(min(cpue$depth), max(cpue$depth), by=10),
                    Year = levels(cpue$Year), dum=0)

# newdB <- expand.grid(day = min(dat$day), max(dat$day), depth = seq(min(dat$depth),max(dat$depth), by=10),sets=1:17,
#                      ADFG = "55900", Stat = "345701", Size=levels(dat$Size), 
#                      Year = levels(dat$Year), dum=0)
# 
# newdB <- expand.grid(day = min(median(dat$day)-25), max(median(dat$day)+25),depth = min(median(dat$depth)-147), max(median(dat$depth)+147),
#                      sets=median(dat$sets),
#                      ADFG = "55900", Stat = "345701", Size=levels(dat$Size), 
#                      Year = levels(dat$Year), dum=0)

summary(fit)




cpue$fit <- predict(fit, cpue, type="response")
newd$fit <- predict(fit, newd)


#png(file='figures/fsh_cpue_comparison.png', res=300, width=7, height=3.5, units ="in", bg="transparent") 


ggplot(cpue, aes(Year, fit, group = 1)) +
  stat_summary(fun.data='mean_cl_boot',geom="smooth",aes(Year, fit, colour="GAM"), fill="grey70")+
  stat_summary(data=dat, fun.data='mean_cl_boot',geom="smooth",aes(Year,cpue,colour="Simple ratio"),fill="blue",alpha=0.1)+
  stat_summary(fun.y='mean',geom="point",aes(Year,fit,colour="GAM"))+
  stat_summary(data=dat, fun.y='mean',geom="point",aes(Year,cpue,colour="Simple ratio"))+
  xlab("Year")+
  ylab("CPUE")+
  scale_colour_manual(name="CPUE",values=c("Commercial fisheries CPUE - GAM" = "black", "Commercial fisheries CPUE - simple ratio" = "blue"))+
  theme_eda


dev.off()

#stat_sub <- dat[is.element(dat$Stat,c("335701","345603","345631","345701","345702","345731","345803")),]
stat_sub <- dat[is.element(dat$Stat,c("345701","345631","345603","345731")),]

ggplot(stat_sub, aes(Stat,cpue, fill=factor(Stat)))+geom_boxplot()

vis.gam(fit,c("depth","sets"), plot.type="contour",type="response",color="topo", too.far = 0.1)
vis.gam(fit,c("depth","day"), plot.type="contour",type="response",color="topo", too.far = 0.1)
vis.gam(fit,c("sets","day"), plot.type="contour",type="response",color="topo", too.far = 0.1)


# Check normality of fit
histogram(~fit|Year, data=dat, breaks=15,strip = strip.custom(bg="grey"))
histogram(~fit|Year, data=newd, breaks=15,strip = strip.custom(bg="grey"))

a1<-ggplot(dat, aes(Year,fit, fill=Stat))+geom_boxplot()
b1<-ggplot(newd, aes(Year,fit, fill=Stat))+geom_boxplot()
grid.arrange(a1,b1,ncol=2)

# fake data
n <- 50
sig <- 2
dat <- gamSim(1,n=n,scale=sig)

# P-spline smoothers (with lambda=0.6) used for x1 and x2; x3 is parametric.
b1 <- mgcv::gam(y ~ s(x1, bs='ps', sp=0.6) + s(x2, bs='ps', sp=0.6) + x3, data = dat)
summary(b1)
plot(b1, scale=0, pages=1)


b2 <- mgcv::gam(y ~ s(x1) + s(x2) + x3, data = dat)
summary(b2,pages=1)
plot(b2, scale=0, pages=1)

# plot the smooth predictor function for x1 with ggplot to get a nicer looking graph
p <- predict(b1, type="lpmatrix")
beta <- coef(b1)[grepl("x1", names(coef(b1)))]
s <- p[,grepl("x1", colnames(p))] %*% beta
ggplot(data=cbind.data.frame(s, dat$x1), aes(x=dat$x1, y=s)) + geom_line()


# predict
newdf <- gamSim(1,n=n,scale=sig)
f <- predict(b1, newdata=newdf)



#------------------------------------------------------------------------------
# Write to file
#
# Note that we are using the GAM-derived estimates of variance, which are
#  smaller than the direct y calculation
# This is up for discussion
#
# We are back-calculating the variance for the legacy CPUE values 1980 - 1996
#  by taking the mean CV of 1997 - present CPUE and applying that to the legacy
#  CPUE values
#------------------------------------------------------------------------------

#Calculate mean CV for 1997 - present poundage CPUE
mean_cv<-mean(sqrt(var)/cpue)
mean_cv

# Legacy fishery cpue values and back-calculation of variance
hist<-as.numeric(read.csv("data/legacy_fishery_cpue.csv",header=FALSE,sep=","))
legacy_fishery_cpue_var <- (mean_cv * hist)^2

# Merge vectors
cpue_full<-c(hist,cpue)
cpue_full_450<-c(hist,cpue450)
var_full<-c(legacy_fishery_cpue_var,var)

#Write to file
write.table(round(rbind(cpue_full),4),"output/fish_cpue_gam_pounds.csv",row.names=FALSE,col.names = FALSE,sep=",")
write.table(round(rbind(cpue_full_450),4),"output/fish_cpue_gam_pounds_450.csv",row.names=FALSE,col.names = FALSE,sep=",")
write.table(round(rbind(var_full),4),"output/fish_cpue_gam_var.csv",row.names=FALSE,col.names = FALSE,sep=",")
write.table(round(rbind(x),4),"output/non_gam_cpue.csv",row.names=FALSE,col.names = FALSE,sep=",")
