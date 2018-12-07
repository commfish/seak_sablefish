# Data prep for Model 1 ASA inputs
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2018-04-09

# Model 1 - base ASA that only includes catch, fishery CPUE, fishery
# weight-at-age, and catch compositions. No sex structure.
source("r/helper.r")
source("r/functions.r")

YEAR <- 2017

# Harvest ----
read_csv(paste0("data/fishery/nseiharvest_ifdb_1969_", YEAR,".csv"), 
                       guess_max = 50000) %>% 
  group_by(year) %>% 
  summarize(total_pounds = sum(whole_pounds)) %>% 
  # Convert lbs to 100 mt
  mutate(total_100mt = total_pounds * 0.000453592 / 100) %>% 
  select(-total_pounds) %>% 
  filter(year >= 1980) -> sum_catch

# Fishery CPUE ----

# Read in data, standardize cpue, etc.
read_csv(paste0("data/fishery/fishery_cpue_1997_", YEAR,".csv"), 
         guess_max = 50000) %>% 
  filter(!is.na(hook_space) & !is.na(sable_lbs_set) &
           julian_day > 226) %>%  # if there were special projects before the fishery opened
  mutate(# standardize hook spacing (Sigler & Lunsford 2001, CJFAS), 1 m = 39.37 in
         std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (hook_space / 39.37))), 
         # convert lbs to kg
         std_cpue_kg = sable_lbs_set * 0.453592 / std_hooks) -> fsh_cpue  

# Nominal CPUE 
fsh_cpue %>% 
  group_by(year) %>% 
  summarise(cpue = mean(std_cpue_kg),
            sdev = sd(std_cpue_kg),
            n = length(std_cpue_kg),
            se = sdev / (n ^ (1/2)),
            var = var(std_cpue_kg),
            cv = sdev / cpue,
            upper = cpue + (2 * se),
            lower = cpue - (2 * se)) -> fsh_sum 

# Historical CPUE 

# From KVK: Logbooks were not included in IFDB until 1997. Commercial fishery
# CPUE values prior to 1997, for use in the ASA or other medium, are LEGACY
# VALUES. Jane updates: I don't have any source information from these numbers
# other than this. Kray kept them in a csv file called
# data/legacy_fishery_cpue.csv. Similarly, I moved and renamed the same file as
# data/fishery/legacy_fisherycpue_1980_1996.csv

read_csv("data/fishery/legacy_fisherycpue_1980_1996.csv", 
         col_names = FALSE) %>% as.numeric() -> hist_cpue

# Use the mean CV from 1997-present to estimate the variance for legacy CPUE
# values, following KVK.

data.frame(year = 1980:1996,
           # Convert to kg
           cpue = hist_cpue * 0.453592) %>% 
  mutate(var = (cpue * mean(fsh_sum$cv)) ^ 2) %>% 
  bind_rows(fsh_sum %>% 
              select(year, cpue, var)) %>% 
  mutate(cpue = round(cpue, 3),
         var = round(var, 3)) -> cpue_ts

axis <- tickr(cpue_ts, year, 3)

ggplot(sum_catch) + 
  geom_point(aes(year, total_100mt)) +
  geom_line(aes(year, total_100mt)) +
  geom_vline(xintercept = 1997, linetype = 2, colour = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  labs(x = "", y = "Fishery harvest\n(100 mt, round)") -> catch

ggplot(cpue_ts) +
  geom_point(aes(year, cpue)) +
  geom_line(aes(year, cpue)) +
  geom_ribbon(aes(year, ymin = cpue - sqrt(var), ymax = cpue + sqrt(var)),
              # geom_ribbon(aes(year, ymin = cpue - var, ymax = cpue + var),
              alpha = 0.2,  fill = "grey") +
  geom_vline(xintercept = 1997, linetype = 2, colour = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  lims(y = c(0, 1.4)) +
  labs(x = "", y = "Fishery CPUE\n(round kg/hook)") -> cpue

plot_grid(catch, cpue, ncol = 1, align = 'hv')

merge(sum_catch, cpue_ts) %>%
  select(year, catch = total_100mt, cpue, cpue_var = var) %>% 
  write_csv("tmb/mod1/cm_catch_cpue.csv")

# Weight-at-age ----

# Fishery biological data
read_csv(paste0("data/fishery/fishery_bio_2000_", YEAR,".csv"), 
         guess_max = 50000) %>%
  mutate(Year = factor(year),
         Sex = factor(Sex)) -> fsh_bio

# Empirical weight-at-age 

fsh_bio %>% 
  filter(!is.na(weight) & !is.na(age) & age < 30) %>% #& !is.na(Sex)
  group_by(age) %>% 
  summarise(weight_kg = mean(weight) %>% round(3)) %>% 
  bind_rows(
    fsh_bio %>% 
    # Plus group
    filter(!is.na(weight) & !is.na(age) & age >= 30) %>% #& !is.na(Sex)
      summarise(weight_kg = mean(weight) %>% round(3)) %>% 
      mutate(age = 30)) -> emp_waa

write_csv(emp_waa, "tmb/mod1/cm_waa.csv")

# Age compositions ----

# Combine survey and fishery data for age comp analysis
unique(fsh_bio$year)
fsh_bio %>% 
  select(year, age) %>% 
  filter(year >= 2002 & !is.na(age)) %>% 
  droplevels() %>% 
  mutate(age = ifelse(age >= 30, 30, age)) %>% 
  filter(age >= 2) -> fsh_comps  # Plus group

# Age comps (sex-specific)
fsh_comps %>% 
  count(year, age) %>%
  group_by(year) %>% 
  mutate(proportion = round(n / sum(n), 4)) -> agecomps   

# Check that they sum to 1
agecomps %>% 
  group_by(year) %>% 
  summarise(sum(proportion)) 

# Sample sizes 
agecomps %>% 
  group_by(year) %>% 
  summarize(n = sum(n)) 

# Reshape
agecomps %>% dcast(year ~ age, value.var = "proportion") -> agecomps
agecomps[is.na(agecomps)] <- 0

write_csv(agecomps, "tmb/mod1/cm_agecomps.csv")

# Used to get logN starting values, these are the mean age comps over all years
fsh_comps %>%
  count(age) %>% 
  mutate(proportion = round(n / sum(n), 4)) -> mean_comps

mean_comps %>% dcast(1 ~ age, value.var = "proportion") -> meancomps
meancomps[is.na(meancomps)] <- 0

write_csv(meancomps[,-1], "tmb/mod1/meancomps.csv")
