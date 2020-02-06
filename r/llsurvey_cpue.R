# Longline Survey cpue
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: Feb 2020

# Currently includes current longline survey cpue, which is calculated at the
# skate level. The next version of this "v2" will be at the set level and may
# standardized by soak time, depth, lat/lon, tide, and catch of other species.
# v2 will also include a more rigorous review of invalid skates through
# inspection of set and skate specific comments

# At the end of the code is an exploration of the hook standardization
# relationship.

#load ----
source("r/helper.r")
source("r/functions.r")
library("rms")   #install.packages("rms") # simple bootstrap confidence intervals

YEAR <- 2019 # most recent year of data

# VERSION 2 (2020): ----

# data ----

# skate condition codes:
# 01 = valid
# 02 = invalid
# 05 = sperm whales present (but not depredating)

srv_cpue <- read_csv(paste0("data/survey/llsrv_cpue_v2_1985_", YEAR, ".csv"), 
                     guess_max = 500000)

# Checks
srv_cpue %>% filter(skate_condition_cde != "02") %>% 
  distinct(skate_comments) #%>% View() # any red flags?

srv_cpue %>% filter(skate_condition_cde != "02") %>% 
  distinct(set_comments) #%>% View() # any red flags?

srv_cpue %>% filter(no_hooks < 0)
srv_cpue %>% filter(year >= 1997 & c(is.na(no_hooks) | no_hooks == 0)) # there should be none

srv_cpue %>% filter(end_lon > 0 | start_lon > 0) #View() # should be none. these are data entry errors
# Hard code fix, sent error to Rhea and Aaron 20200205 to fix in database
srv_cpue <- srv_cpue %>%  mutate(end_lon = ifelse(end_lon > 0, -1 * end_lon, end_lon))

srv_cpue %>% filter(is.na(Adfg)) # should be none.
# Hard code fix, sent error to Rhea and Aaron 20200205 to fix in database
srv_cpue <- srv_cpue %>% mutate(Adfg = ifelse(is.na(Adfg), 55900, Adfg))

# Data clean up
srv_cpue  %>% 
  filter(year >= 1997 & 
           # Mike Vaughn 2018-03-06: Sets (aka subsets with 12 or more invalid
           # hooks are subset condition code "02" or invalid)
           skate_condition_cde != "02") %>% 
  replace_na(list(bare=0, bait=0, invalid=0, sablefish=0, halibut=0,
                  idiot=0, shortraker=0, rougheye=0, skate_general=0,
                  longnose_skate=0, big_skate=0, sleeper_shark=0)) %>% 
  mutate(Year = factor(year),
         Stat = factor(Stat),
         Adfg = factor(Adfg),
         no_hooks = no_hooks - invalid, # remove invalid hooks
         # Lump all skates since id's have changed over time. There are no big
         # skates on the survey, this was an error. Asked it to be fixed in the
         # data 20200204.
         skates = skate_general + longnose_skate + big_skate,
         shortraker_rougheye = shortraker + rougheye,
         #standardize hook spacing (Sigler & Lunsford 2001, CJFAS) changes in
         #hook spacing. pers. comm. with aaron.baldwin@alaska.gov: 1995 & 1996 -
         #118 in; 1997 - 72 in.; 1998 & 1999 - 64; 2000-present - 78". This is
         #different from KVK's code (he assumed 3 m before 1997, 2 m in 1997 and
         #after)
         std_hooks = case_when(year <= 1996 ~ 2.2 * no_hooks * (1 - exp(-0.57 * (118 * 0.0254))),
                               year == 1997 ~ 2.2 * no_hooks * (1 - exp(-0.57 * (72 * 0.0254))),
                               year %in% c(1998, 1999) ~ 2.2 * no_hooks * (1 - exp(-0.57 * (64 * 0.0254))),
                               TRUE ~ 2.2 * no_hooks * (1 - exp(-0.57 * (78 * 0.0254)))),
         # flags for clotheslined gear or sharks in gear. These are issues that
         # can be standardized instead of invalidating sets. FLAG -- look for
         # high proportions of baited hooks to identify clotheslined sets. Mike
         # Vaughn is suspect that all instances of clotheslining would be
         # documented in the comments
         clotheslined = ifelse(grepl("clotheslined|Clotheslined", skate_comments) | 
                                 grepl("clotheslined|Clotheslined", set_comments), 1, 0),
         shark = ifelse(grepl("sleeper|Sleeper|shark|sharks", skate_comments) | 
                          grepl("sleeper|Sleepershark|sharks", set_comments), 1, 
                        ifelse(sleeper_shark > 0, 1, 0))) %>% 
  rename(shortspine_thornyhead = idiot) -> srv_cpue

# Calculate the set (aka station) level cpue
srv_cpue %>% 
  # Summarize data at the set (or station) level
  group_by(year, Vessel, Station_no) %>% # julian_day, soak, depth, slope, Stat, end_lat, end_lon, clotheslined, shark) %>% 
  mutate(bare = sum(bare),
         bait = sum(bait),
         sablefish = sum(sablefish),
         halibut = sum(halibut),
         shortspine_thornyhead = sum(shortspine_thornyhead),
         shortraker_rougheye = sum(shortraker_rougheye),
         skates = sum(skates),
         sleeper_shark = sum(sleeper_shark),
         set_hooks = sum(std_hooks)) %>% 
  ungroup() %>% 
  melt(measure.vars = c("sablefish", "shortspine_thornyhead", 
                        "skates", "shortraker_rougheye", "halibut",
                        "sleeper_shark"),
       variable.name = "hook_accounting", value.name = "n") %>%
  mutate(set_cpue = n / set_hooks) %>% 
  # Calculate cpue by stat area
  group_by(year, Stat, hook_accounting) %>% 
  mutate(stat_cpue = mean(set_cpue)) %>% 
  # Calculate annual cpue (n = number of stations sampled in a year)
  group_by(year, hook_accounting) %>% 
  mutate(n_set = length(unique(Station_no)),
         cpue = mean(set_cpue),
         sd = round(sd(set_cpue), 4),
         se = round(sd / sqrt(n_set), 4)) -> srv_cpue
  
# Sablefish-specific dataframe for analysis
sable <- srv_cpue %>% 
  filter(hook_accounting == "sablefish") %>% 
  distinct(year, Adfg, Station_no, set_cpue, depth, end_lon, end_lat, 
           soak, slope, bare, bait, clotheslined, shark) %>%
  ungroup() %>% 
  mutate(Station_no = factor(Station_no),
         Year = factor(year),
         Clotheslined = factor(clotheslined),
         Shark = factor(shark)) 

srv_cpue %>% 
  ungroup() %>% 
  filter(hook_accounting == "sablefish") %>% 
  distinct(year, std_cpue = cpue, sd, se) %>% 
  arrange(year) -> srv_sum

write_csv(srv_sum, paste0("output/srvcpue_", min(srv_cpue$year), "_", YEAR, ".csv"))

# figures

axis <- tickr(srv_sum, year, 5)
ggplot(data = srv_sum) +
  geom_point(aes(x = year, y = std_cpue)) +
  geom_line(aes(x = year, y = std_cpue)) +
  geom_ribbon(aes(year, ymin = std_cpue - sd, ymax = std_cpue + sd),
              alpha = 0.2, col = "white", fill = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  lims(y = c(0, 0.35)) +
  labs(x = NULL, y = "Survey CPUE (number per hook)\n") 

ggsave(paste0("figures/npue_llsrv_", YEAR, ".png"), 
       dpi=300, height=4, width=7, units="in")

ggplot() +
  geom_line(data = srv_cpue, 
            aes(x = year, y = set_cpue, group = Station_no), col = "lightgrey") +
  geom_line(data = srv_cpue, aes(x = year, y = cpue)) +
  geom_hline(data = srv_cpue %>% 
               group_by(hook_accounting) %>% 
               droplevels() %>% 
               dplyr::summarize(mu_cpue = mean(cpue)),
             aes(yintercept = mu_cpue), lty = 2) +
  facet_wrap(~hook_accounting, scales = "free_y") +
  expand_limits(y = 0) +
  labs(x = NULL, y = "Number per standardized hook") 

ggsave(paste0("figures/npue_llsrv_allspp_", YEAR, ".png"), 
       dpi=300, height=7/1.6, width=7, units="in")

# By stat area
ggplot() +
  geom_line(data = srv_cpue %>% filter(hook_accounting == "sablefish"), 
            aes(x = year, y = set_cpue, group = Station_no), col = "lightgrey") +
  geom_line(data = srv_cpue %>% filter(hook_accounting == "sablefish"), 
            aes(x = year, y = stat_cpue)) +
  geom_hline(data = srv_cpue %>% 
               filter(hook_accounting == "sablefish") %>% 
               group_by(Stat) %>% 
               droplevels() %>% 
               dplyr::summarize(mu_cpue = mean(stat_cpue)),
             aes(yintercept = mu_cpue), lty = 2) +
  facet_wrap(~ Stat, scales = "free_y", ncol = 1) +
  labs(x = NULL, y = "Number per standardized hook")

ggsave(paste0("figures/npue_llsrv_stat_", YEAR, ".png"), 
       dpi=300, height=7/1.6, width=3.5, units="in")

# GAM explorations ----

library(GGally)
library(mgcViz)
library(mgcv)

sable %>% 
  select(set_cpue, depth, end_lon, soak, slope, bare, bait, Clotheslined, Shark) %>% 
  GGally::ggpairs() # cut off anything with a correlation less than 0.05 (just shark flag)

sable %>%
  ggplot()+
  geom_point(aes(x = end_lon,  y = end_lat, color = set_cpue), alpha = 0.2)+
  scale_color_gradientn(colours = terrain.colors(10))+
  labs(y = "Latitude", x = "Longitude", color = "CPUE")

# GAM modeling ----

sable %>% filter(set_cpue == 0) # there should be no zero values
# sable %>% filter(is.na(depth) & is.na(soak)) %>% nrow()
# sable <- sable %>% filter(!is.na(depth) & !is.na(soak)) 
# sable %>% filter(bait > 900) %>% View()
# sable <- sable %>% filter(soak < 10) 
# sable <- sable %>% filter(slope < 300) 

# bait = better than Clotheslined; also highly correlated with bare (so bare has
# been removed to avoid confounding)
mod0 <- bam(set_cpue ~ s(soak, k = 4) + s(depth, k = 4) + s(slope, k = 4) + 
              s(bait, k = 4) + 
              te(end_lon, end_lat) + Year + s(Adfg, bs='re') + 1, 
            data = sable, gamma=1.4, family=Gamma(link=log), select = TRUE,
            subset = soak < 10 & slope < 300 & bait < 900)

summary(mod0)
anova(mod0)
print(plot(getViz(mod0), allTerms = TRUE) + l_fitRaster() + l_fitContour() + 
        l_points(color = "grey60", size = 0.5)+ l_fitLine() + l_ciLine() +
        l_ciBar(linetype = 2) + l_fitPoints(size = 1), pages = 8)

par(mfrow=c(2, 2), cex=1.1); gam.check(mod0)

# drop soak time
mod1 <- bam(set_cpue ~ s(soak, k = 4) + s(depth, k = 4) + s(slope, k = 4) + 
              s(bait, k = 4) +
              te(end_lon, end_lat) + Year + s(Adfg, bs='re'), #  Clotheslined + 
            data = sable, gamma=1.4, family=Gamma(link=log), select = TRUE,
            subset = soak < 10 & slope < 300 & bait < 900)

summary(mod1)
BIC(mod0); BIC(mod1)
anova(mod1)
print(plot(getViz(mod1), allTerms = TRUE) + l_fitRaster() + l_fitContour() + 
        l_points(color = "grey60", size = 0.5)+ l_fitLine() + l_ciLine() 
      + l_ciBar(linetype = 2) + l_fitPoints(size = 1), pages = 1)

par(mfrow=c(2, 2), cex=1.1); gam.check(mod1)


# VERSION 1 (2017-2019): ----

# data -----
srv_cpue <- read_csv(paste0("data/survey/llsrv_cpue_1985_", YEAR, ".csv"),
                     guess_max = 500000)

srv_cpue  %>% 
  filter(year >= 1997 & 
           # Mike Vaughn 2018-03-06: Sets (aka subsets with 12 or more invalid
           # hooks are subset condition code "02" or invalid)
           subset_condition_cde != "02") %>% 
  filter(!c(is.na(no_hooks) | no_hooks == 0)) %>% 
  mutate(Year = factor(year),
         Stat = factor(Stat),
         #standardize hook spacing (Sigler & Lunsford 2001, CJFAS) changes in 
         #hook spacing. pers. comm. with aaron.baldwin@alaska.gov: 1995 & 1996 -
         #118 in; 1997 - 72 in.; 1998 & 1999 - 64; 2000-present - 78". This is
         #different from KVK's code (he assumed 3 m before 1997, 2 m in 1997 and
         #after)
         hooks_bare = ifelse(is.na(hooks_bare), 0, hooks_bare),
         hooks_bait = ifelse(is.na(hooks_bait), 0, hooks_bait),
         hook_invalid = ifelse(is.na(hook_invalid), 0, hook_invalid),
         no_hooks = no_hooks - hook_invalid,
         std_hooks = ifelse(year <= 1996, 2.2 * no_hooks * (1 - exp(-0.57 * (118 * 0.0254))),
                            ifelse(year == 1997, 2.2 * no_hooks * (1 - exp(-0.57 * (72 * 0.0254))),
                                   ifelse( year %in% c(1998, 1999), 2.2 * no_hooks * (1 - exp(-0.57 * (64 * 0.0254))),
                                           2.2 * no_hooks * (1 - exp(-0.57 * (78 * 0.0254)))))),
         # std_hooks = ifelse(year < 1997, 2.2 * no_hooks * (1 - exp(-0.57 * 3)),
         #                    2.2 * no_hooks * (1 - exp(-0.57 * 2))),
         # sablefish_retained = ifelse(discard_status_cde == "01", hooks_sablefish, 0),
         # sablefish_retained = hooks_sablefish,
         sablefish_retained = replace(hooks_sablefish, is.na(hooks_sablefish), 0), # make any NAs 0 values
         std_cpue = sablefish_retained/std_hooks #*FLAG* this is NPUE, the fishery is a WPUE
         # raw_cpue = sablefish_retained/no_hooks
  ) -> srv_cpue

# Bootstrap ----

axis <- tickr(srv_cpue, year, 5)

# Simple bootstrap confidence intervals (smean.cl.boot from rms)
srv_cpue %>%
  group_by(year) %>%
  do(data.frame(rbind(smean.cl.boot(.$std_cpue)))) -> plot_boot

ggplot(plot_boot) +
  geom_ribbon(aes(x = year, ymin = Lower, ymax = Upper), 
              alpha = 0.1, fill = "grey55") +
  geom_point(aes(x = year, y = Mean), size = 1) +
  geom_line(aes(x = year, y = Mean)) +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "Survey CPUE (number per hook)\n") +
  lims(y = c(0.15, 0.3)) 
  
ggsave(paste0("figures/srvcpue_bootCI_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

# With +/- 1 sd ----
hist(srv_cpue$std_cpue)
srv_cpue %>% 
  group_by(year) %>% 
  summarise(srv_cpue = round(mean(std_cpue), 2),
         n = length(std_cpue),
         sd = sd(std_cpue),
         se = sd / sqrt(n)) -> srv_sum

write_csv(srv_sum, paste0("output/srvcpue_", min(srv_cpue$year), "_", YEAR, ".csv"))

# figures

axis <- tickr(srv_sum, year, 5)
ggplot(data = srv_sum) +
  geom_point(aes(year, srv_cpue)) +
  geom_line(aes(year, srv_cpue)) +
  geom_ribbon(aes(year, ymin = srv_cpue - sd, ymax = srv_cpue + sd),
              alpha = 0.2, col = "white", fill = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  lims(y = c(0, 0.4)) +
  labs(x = NULL, y = "Survey CPUE (number per hook)\n") 

ggsave(paste0("figures/npue_llsrv_", YEAR, ".png"), 
       dpi=300, height=4, width=7, units="in")

# Explore hook standardization relationship ----

# Standardizing number of hooks based off hook spacing (Sigler & Lunsford 2001,
# CJFAS)

# range of hook number seen in fishery/survey
no_hooks <- c(100, 200, 800, 1000, 2000, 4000, 6000, 8000) 
# spacings observed in fishery/survey - in inches (converted to meters in the
# formula below)
hook_space <- seq(20, 120, by = 0.1) 
hk_stand <- expand.grid(no_hooks = no_hooks, hook_space = hook_space)

hk_stand %>% 
  mutate(std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * hook_space*0.0254))
  ) -> hk_stand

ggplot(hk_stand, aes(x = hook_space, y = std_hooks, col = factor(no_hooks))) +
  geom_line(size = 2) +
  xlab("\nHook spacing (inches)") +
  ylab("\nStandardized number of hooks") +
  scale_color_brewer(palette = "Greys", "Number of\nHooks") 

# other, probably clearer way of visualizing this

# range of hook number seen in fishery/survey
no_hooks <- seq(100, 8000, by = 100) 
# spacings observed in fishery/survey - in inches (converted to meters in the
# formula below)
hook_space <- seq(20, 180, by = 30) 
hk_stand <- expand.grid(no_hooks = no_hooks, hook_space = hook_space)

hk_stand %>% 
  mutate(std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * hook_space*0.0254))) -> hk_stand

ggplot(hk_stand, aes(x = no_hooks, y = std_hooks, col = factor(hook_space))) +
  geom_line(size = 2) +
  xlab("\nNumber of hooks") +
  ylab("\nStandardized number of hooks") +
  scale_color_brewer(palette = "Greys", "Hook spacing (in.)") +
  scale_x_continuous(breaks = seq(0, 15000, by = 1000)) +
  scale_y_continuous(breaks = seq(0, 15000, by = 1000)) +
  geom_abline(slope = 1, col = "red", linetype = 2) 
