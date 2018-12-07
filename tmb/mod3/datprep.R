# Data prep for Model 1 ASA inputs
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2018-04-09

# Model 1 - base ASA that only includes catch, fishery CPUE, fishery
# weight-at-age, and catch compositions. No sex structure.
source("r/helper.r")
source("r/functions.r")

syr <- 1980
lyr <- 2017

rec_age <- 2
plus_group <- 26
nage <- length(rec_age:plus_group)

# Harvest ----
read_csv(paste0("data/fishery/nseiharvest_ifdb_1969_", lyr,".csv"), 
                       guess_max = 50000) %>% 
  group_by(year) %>% 
  summarize(total_pounds = sum(whole_pounds)) %>% 
  # Convert lbs to 100 mt
  mutate(total_100mt = total_pounds * 0.000453592 / 100) %>% 
  select(-total_pounds) %>% 
  filter(year >= fyr) -> sum_catch

# Fishery CPUE ----

# Read in data, standardize cpue, etc.
read_csv(paste0("data/fishery/fishery_cpue_1997_", lyr,".csv"), 
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
  summarise(cpue = mean(std_cpue_kg)) -> fsh_sum 

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
  bind_rows(fsh_sum ) %>% 
  mutate(upper = cpue + 0.2 * cpue,
         lower = cpue - 0.2 * cpue) -> cpue_ts


# Survey NPUE ----

read_csv(paste0("output/srvcpue_1997_", lyr, ".csv")) -> srv_sum

# FLAG Survey 2 - 3+hr soak time. I didn't vet these values and got them from Franz's
# data file

data.frame(year = 1988:1996,
           annual_cpue = c(0.2290,	0.1260,	0.1220,	0.1580,	0.1150,	0.2280,	
                           0.1720,	0.1520,	0.1600),
           survey = "1-hr soak")  %>% 
  full_join(srv_sum %>% 
              mutate(survey = "3+hr soak")) %>% 
  mutate(CIupper = annual_cpue + 0.2 * annual_cpue,
         CIlower = annual_cpue - 0.2 * annual_cpue) %>% 
  mutate(CIlower = ifelse(CIlower < 0, 0, CIlower)) -> srv_sum

# Mark-recapture index ----

read_csv(paste0("output/mr_index.csv")) -> mr_sum

# *FLAG* Franz had data for 2003 and 2004. Include his Petersen estimates + his
# 2*se - at some point go back and track these data down
early_mr <- data.frame(year = c(2003, 2004),
                       estimate = c(2.775, 2.675),
                       q025 = c(2.5429, 2.4505),
                       q975 = c(3.0071, 2.8995))

bind_rows(early_mr, mr_sum) -> mr_sum

mr_sum %>% 
  full_join(data.frame(year = min(mr_sum$year):lyr)) %>% 
  arrange(year) -> mr_sum

# Graphics ----

axis <- tickr(cpue_ts, year, 3)

sum_catch %>% 
  mutate(upper =  total_100mt + 0.05 * total_100mt,
         lower = total_100mt - 0.05 * total_100mt) %>% 
  ggplot() + 
  geom_point(aes(year, total_100mt)) +
  geom_line(aes(year, total_100mt)) +
  geom_ribbon(aes(year, ymin = lower, ymax = upper),
              alpha = 0.2,  fill = "grey") +
  geom_vline(xintercept = 1997, linetype = 2, colour = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  labs(x = "", y = "Catch\n(round x100 mt)") -> catch

ggplot(cpue_ts) +
  geom_point(aes(year, cpue)) +
  geom_line(aes(year, cpue)) +
  geom_ribbon(aes(year, ymin = lower , ymax = upper),
              alpha = 0.2,  fill = "grey") +
  geom_vline(xintercept = 1997, linetype = 2, colour = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  lims(y = c(0, 1.4)) +
  labs(x = "", y = "Fishery CPUE\n(round kg/hook)") -> cpue

ggplot(data = srv_sum) +
  geom_point(aes(year, annual_cpue, shape = survey)) +
  geom_line(aes(year, annual_cpue)) +
  geom_vline(xintercept = 1997, linetype = 2, colour = "grey") +
  geom_ribbon(aes(year, ymin = CIlower, ymax = CIupper),
              alpha = 0.2, col = "white", fill = "grey") +
  scale_x_continuous(limits = c(syr,lyr), breaks = axis$breaks, labels = axis$labels) + 
  lims(y = c(0, 0.5)) +
  labs(y = "Survey CPUE\n(number/hook)", x = NULL, shape = NULL) +
  theme(legend.position = c(.1, .8))-> srv

mr_sum %>% 
  mutate(year = as.Date(as.character(year), format = "%Y")) %>% 
  pad(interval = "year") %>% 
  mutate(year = year(year),
         Year = factor(year)) %>% 
  gather("Abundance", "N", estimate) %>% 
  mutate(# interpolate the CI in missing years for plotting purposes
         q025 = zoo::na.approx(q025, maxgap = 20, rule = 2),
         q975 = zoo::na.approx(q975, maxgap = 20, rule = 2)) %>% 
  ggplot() +
  geom_ribbon(aes(x = year, ymin = q025, ymax = q975), 
              alpha = 0.2, colour = "white", fill = "grey") +
  geom_point(aes(x = year, y = N)) +
  geom_line(aes(x = year, y = N, group = Abundance)) +
  scale_x_continuous(limits = c(syr,lyr), breaks = axis$breaks, 
                     labels = axis$labels) +
  ylim(c(1, 3.8)) +
  labs(x = "", y = "Abundance\n(millions)") -> mr
  
plot_grid(catch, cpue, srv, mr, ncol = 1, align = 'hv')

ggsave(paste0("tmb/mod3/abd_indices.png"),
       dpi=300, height=6, width=6, units="in")


full_join(sum_catch, cpue_ts) %>% 
  full_join(srv_sum %>% 
              spread(survey, annual_cpue)) %>% 
  full_join(mr_sum) %>%
  select(year, catch = total_100mt, fsh_cpue = cpue, srv1_cpue = `1-hr soak`, srv2_cpue = `3+hr soak`,
         mr = estimate) %>% 
  write_csv("tmb/mod1/abd_indices.csv")

# Fishery weight-at-age ----

# Fishery biological data
read_csv(paste0("data/fishery/fishery_bio_2000_", lyr,".csv"), 
         guess_max = 50000) %>%
  mutate(Year = factor(year),
         Sex = factor(Sex)) -> fsh_bio

# Empirical weight-at-age 
fsh_bio %>% 
  filter(!is.na(weight) & !is.na(age) & age < plus_group) %>% #& !is.na(Sex)
  group_by(age) %>% 
  summarise(weight_kg = mean(weight) %>% round(4)) %>% 
  bind_rows(
    fsh_bio %>% 
    # Plus group
    filter(!is.na(weight) & !is.na(age) & age >= plus_group) %>% #& !is.na(Sex)
      summarise(weight_kg = mean(weight) %>% round(4)) %>% 
      mutate(age = plus_group))  %>% 
  mutate(waa = "fsh") -> fsh_waa

# Survey weight-at-age ----

# Fishery biological data
read_csv(paste0("data/survey/llsrv_bio_1985_", lyr,".csv"), 
         guess_max = 50000) %>%
  mutate(Year = factor(year),
         Sex = factor(Sex)) -> srv_bio

# Empirical weight-at-age 
srv_bio %>% 
  filter(year >= 1997 & !is.na(weight) & !is.na(age) & age < plus_group) %>% #& !is.na(Sex)
  group_by(age) %>% 
  summarise(weight_kg = mean(weight) %>% round(4)) %>% 
  bind_rows(
    srv_bio %>% 
      # Plus group
      filter(!is.na(weight) & !is.na(age) & age >= plus_group) %>% #& !is.na(Sex)
      summarise(weight_kg = mean(weight) %>% round(4)) %>% 
      mutate(age = plus_group)) %>% 
  mutate(waa = "srv") -> srv_waa

# Spawner weight-at-age ----

srv_bio %>% 
  filter(Sex == "Female" & year >= 1997 & !is.na(weight) & 
           !is.na(age) & age < plus_group ) %>% 
  group_by(age) %>% 
  summarise(weight_kg = mean(weight) %>% round(4)) %>% 
  bind_rows(
    srv_bio %>% 
      # Plus group
      filter(Sex == "Female" &!is.na(weight) & !is.na(age) & age >= plus_group) %>% #& !is.na(Sex)
      summarise(weight_kg = mean(weight) %>% round(4)) %>% 
      mutate(age = plus_group)) %>% 
  mutate(waa = "fem") -> fem_waa

bind_rows(fsh_waa, srv_waa) %>% 
  bind_rows(fem_waa) -> waa

waa %>% 
  mutate(Age = factor(age, levels = c("2", "3", "4", "5", "6", "7", "8",
                                      "9", "10", "11", "12", "13", "14", "15",
                                      "16", "17", "18", "19", "20", "21", "22",
                                      "23", "24", "25", "26"),
                      labels = c("2", "3", "4", "5", "6", "7", "8",
                                 "9", "10", "11", "12", "13", "14", "15",
                                 "16", "17", "18", "19", "20", "21", "22",
                                 "23", "24", "25", "26+")),
         Source = factor(waa, levels = c("fsh", "srv", "fem"),
                         labels = c("Fishery (sexes combined)",
                                    "Survey (sexes combined)",
                                    "Survey females (spawning biomass)"))) -> waa

write_csv(waa, "tmb/mod3/waa.csv")

# Proportion mature -----

read_csv(paste0("output/fem_maturityatage_llsrv.csv"), 
         guess_max = 50000) -> mat

mat %>% 
  filter(age <= plus_group)  %>% 
  mutate(Age = factor(age, levels = c("2", "3", "4", "5", "6", "7", "8",
                                      "9", "10", "11", "12", "13", "14", "15",
                                      "16", "17", "18", "19", "20", "21", "22",
                                      "23", "24", "25", "26"),
                      labels = c("2", "3", "4", "5", "6", "7", "8",
                                 "9", "10", "11", "12", "13", "14", "15",
                                 "16", "17", "18", "19", "20", "21", "22",
                                 "23", "24", "25", "26+"))) -> mat

write_csv(waa, "tmb/mod3/prop_mature.csv")

# Sex ratio ----

# proportion female at age in the survey

srv_bio %>% 
  filter(age %in% c(rec_age:plus_group-1)) %>% 
  ungroup() %>% 
  select(Sex, age) %>% 
  filter(Sex %in% c("Female", "Male")) %>% 
  na.omit() %>% 
  droplevels() %>% 
  count(Sex, age) %>% 
  group_by(age) %>% 
  mutate(proportion = round(n / sum(n), 2)) %>% 
  filter(Sex == "Female") %>% 
  # Plus group
  bind_rows(srv_bio %>% 
              filter(age >= plus_group) %>% 
              ungroup() %>% 
              select(Sex) %>% 
              filter(Sex %in% c("Female", "Male")) %>% 
              na.omit() %>% 
              droplevels() %>% 
              count(Sex) %>% 
              mutate(proportion = round(n / sum(n), 2),
                     age = 26) %>% 
              filter(Sex == "Female")) -> byage

# get generalized additive model fits and predictions
srv_fitage <- gam(I(Sex == "Female") ~ s(age), 
                  data = filter(srv_bio, age %in% rec_age:plus_group, 
                                Sex %in% c("Female", "Male")),
                  family = "quasibinomial")

srv_predage <- predict(srv_fitage, newdata = data.frame(age = rec_age:plus_group),
                       type = "response", se = TRUE)

bind_cols(
  byage,
  #do.call cbinds each vector in the predict() output list 
  tbl_df(do.call(cbind, srv_predage))) -> byage

byage %>% 
  mutate(Age = factor(age, levels = c("2", "3", "4", "5", "6", "7", "8",
                                      "9", "10", "11", "12", "13", "14", "15",
                                      "16", "17", "18", "19", "20", "21", "22",
                                      "23", "24", "25", "26"),
                      labels = c("2", "3", "4", "5", "6", "7", "8",
                                 "9", "10", "11", "12", "13", "14", "15",
                                 "16", "17", "18", "19", "20", "21", "22",
                                 "23", "24", "25", "26+"))) -> byage

# Graphics ----

ggplot(waa %>% 
         filter(waa == "fsh")) +
  geom_point(aes(x = age, y = weight_kg)) +
  labs(x = NULL, y = "Mean weight (kg)\n") +
  scale_x_continuous(limits = c(rec_age,plus_group), 
                     breaks = seq(rec_age,plus_group,4), 
                     labels = seq(rec_age,plus_group,4)) +
  scale_y_continuous(limits = c(1, 5.2))

ggsave(paste0("tmb/mod3/fsh_waa.png"),
       dpi=300, height=2, width=4, units="in")

ggplot(waa) +
  geom_point(aes(x = Age, y = weight_kg, shape = Source,
                 colour = Source)) +
  labs(x = NULL, y = "Mean weight\n(kg)", colour = NULL, shape = NULL) +
  theme(legend.position = c(.2, .8)) -> waa_plot


# Equation text for plotting values of a_50
a50 <- 6.4
a50_txt <- as.character(
  as.expression(substitute(
    paste(italic(a[50]), " = ", xx),
    list(xx = formatC(a50, format = "f", digits = 1)))))

axis <- tickr(byage, age, 1)

ggplot(mat) +
  geom_point(aes(x = age, y = probability), colour = "cornflowerblue", shape = 15) +
  geom_line(aes(x = age, y = probability, group = 1), colour = "cornflowerblue") +
  geom_segment(aes(x = a50, y = 0, xend = a50, yend = 0.50), 
               lty = 2, col = "grey") +
  geom_segment(aes(x = 2, y = 0.50, xend = a50, yend = 0.50), 
               lty = 2, col = "grey") +
  # a_50 labels
  geom_text(aes(10, 0.5, label = a50_txt), face = "bold", size = 5,
            colour = "cornflowerblue", parse = TRUE) +
  scale_x_continuous(breaks = axis$breaks, 
                     labels = NULL) +
  labs(x = NULL, y = "Proportion\nmature\n") -> mat_plot


ggplot(byage, aes(x = Age)) +
  geom_line(aes(y = fit, group = 1), colour = "cornflowerblue") +
  geom_ribbon(aes(ymin = fit - se.fit*2, ymax = fit + se.fit*2, group = 1), 
              alpha = 0.2, fill = "cornflowerblue", colour = "white") +
  geom_point(aes(y = proportion), colour = "cornflowerblue") +  
  expand_limits(y = c(0.3, 0.6)) +
  xlab("\nAge") +
  ylab("Proportion\nfemale\n") +
  geom_hline(yintercept = 0.5, lty = 2, col = "grey") -> prop_fem

plot_grid(waa_plot, mat_plot, prop_fem, ncol = 1, align = 'hv')

ggsave(paste0("tmb/mod3/bio_dat.png"),
       dpi=300, height=6, width=7, units="in")
# Age compositions ----

# Fishery

fsh_bio %>% 
  select(year, age) %>% 
  filter(year >= 2002 & !is.na(age)) %>% 
  droplevels() %>% 
  # FLAG leave out plus group for now
  # mutate(age = ifelse(age >= plus_group, plus_group, age)) %>% 
  filter(age >= rec_age & age < plus_group ) -> fsh_comps 

fsh_comps %>% 
  count(year, age) %>%
  group_by(year) %>% 
  mutate(proportion = round(n / sum(n), 5),
         Source = "Fishery") -> fsh_comps

# Survey
srv_bio %>% 
  select(year, age) %>% 
  filter(year >= 1997 & !is.na(age)) %>% 
  droplevels() %>% 
  # FLAG leave out plus group for
  # mutate(age = ifelse(age > plus_group, plus_group, age)) %>% 
  filter(age >= rec_age & age < plus_group ) -> srv_comps  

srv_comps %>% 
  count(year, age) %>%
  group_by(year) %>% 
  mutate(proportion = round(n / sum(n), 5),
         Source = "Survey") -> srv_comps

bind_rows(fsh_comps, srv_comps) -> agecomps

# Check that they sum to 1
agecomps %>% 
  group_by(year, Source) %>% 
  summarise(proportion = sum(proportion)) 

# Sample sizes 
agecomps %>% 
  group_by(year, Source) %>% 
  summarize(n = sum(n)) 

axisy <- tickr(agecomps, year, 5)

ggplot(agecomps, aes(x = age, y = year, size = proportion)) + #*FLAG* could swap size with proportion_scaled
  geom_hline(yintercept = seq(2000, 2015, by = 5), 
             colour = "grey", linetype = 3, alpha = 0.7) +  
  geom_point(shape = 21, colour = "black", fill = "black") +
  scale_size(range = c(0, 4)) +
  facet_wrap(~ Source, scales = "free_y") +
  xlab('\nAge') +
  ylab('') +
  guides(size = FALSE) +
  scale_y_continuous(breaks = axisy$breaks, labels = axisy$labels) 

ggsave("tmb/mod3/agecomps.png", dpi = 300, height = 5, width = 6, units = "in")

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
