# Data prep for Model 1 ASA inputs
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2018-04-09

# Model 1 - base ASA that only includes catch, fishery CPUE, fishery
# weight-at-age, and catch compositions. No sex structure.
source("r/helper.r")
source("r/functions.r")

syr <- 1980
lyr <- YEAR <- 2018
nyr <- length(syr:lyr)

rec_age <- 2
plus_group <- 42
nage <- length(rec_age:plus_group)

# Harvest ----
read_csv(paste0("data/fishery/nseiharvest_ifdb_1969_", lyr,".csv"), 
                       guess_max = 50000) %>% 
  filter(year >= syr) %>% 
  group_by(year) %>% 
  summarize(total_pounds = sum(whole_pounds)) %>% 
  # Convert lbs to mt
  mutate(catch = total_pounds * 0.000453592,
         sigma_catch = 0.05,
         ln_catch = log(catch),
         std = 1.96 * sqrt(log(sigma_catch + 1)),
         upper_catch = exp(ln_catch + std),
         lower_catch = exp(ln_catch - std)) %>% 
  select(-total_pounds, -std) %>% 
  filter(year >= syr) -> catch

# Fishery CPUE ----

# Read in data, standardize cpue, etc.
read_csv(paste0("data/fishery/fishery_cpue_1997_", lyr,".csv"), 
         guess_max = 50000) %>% 
  filter(!is.na(hook_space) & !is.na(sable_lbs_set) &
           julian_day > 226) %>%  # if there were special projects before the fishery opened
  mutate(# standardize hook spacing (Sigler & Lunsford 2001, CJFAS), 1 m = 39.37 in
         std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (hook_space / 39.37))), 
         # convert lbs to kg
         std_cpue_kg = (sable_lbs_set * 0.453592) / std_hooks) -> fsh_cpue  

# Nominal CPUE 
fsh_cpue %>% 
  group_by(year) %>% 
  summarise(fsh_cpue = mean(std_cpue_kg),
            sigma_fsh_cpue = sd(std_cpue_kg)) -> fsh_cpue 

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
           fsh_cpue = hist_cpue * 0.453592,
           sigma_fsh_cpue = 0.2) %>%
  bind_rows(fsh_cpue) %>%
  mutate(ln_fsh_cpue = log(fsh_cpue),
         std = 1.96 * sqrt(log(sigma_fsh_cpue + 1)),
         upper_fsh_cpue = exp(ln_fsh_cpue + std),
         lower_fsh_cpue = exp(ln_fsh_cpue - std)) %>% 
  select(-std) -> fsh_cpue

# Survey NPUE ----

# read_csv(paste0("output/srvcpue_1997_", lyr, ".csv")) %>% 
#   mutate(cv = sdev / annual_cpue,
#          pred = obj$report()$pred_srv_cpue,
#          ln_pred = log(pred)) %>% 
#   select(year, srv_cpue = annual_cpue, sigma_srv_cpue = cv, pred_srv_cpue) %>% 
#   mutate(upper_srv_cpue = qnorm(0.975, log(srv_cpue), sigma_srv_cpue),
#          lower_srv_cpue = qnorm(0.025, log(srv_cpue), sigma_srv_cpue)) -> srv_cpue

read_csv(paste0("output/srvcpue_1997_", lyr, ".csv")) %>% 
  rename(srv_cpue = annual_cpue) %>% 
  mutate(sigma_srv_cpue = sdev / srv_cpue,
         ln_srv_cpue = log(srv_cpue),
         std = 1.96 * sqrt(log(sigma_srv_cpue + 1)),
         upper_srv_cpue = exp(ln_srv_cpue + std ),
         lower_srv_cpue = exp(ln_srv_cpue - std )) %>% 
  select(-c(sdev, CIupper, CIlower)) -> srv_cpue 

# ggplot(data = srv_cpue) +
#   geom_point(aes(year, annual_cpue), col = "darkgrey") +
#   geom_line(aes(year, pred)) +
#   geom_ribbon(aes(year, ymin = lower_srv_cpue, ymax = upper_srv_cpue),
#               alpha = 0.2, col = "white", fill = "grey") +
#   scale_x_continuous(limits = c(syr,lyr), breaks = axis$breaks, labels = axis$labels) + 
#   # lims(y = c(0, 0.3)) +
#   labs(y = "\n\nSurvey CPUE\n(number/hook)", x = NULL) #-> srv_cpue_plot


# Mark-recapture index ----

read_csv(paste0("output/mr_index.csv")) %>% 
  select(year, mr = estimate, sigma_mr = sd) %>% 
  mutate(sigma_mr = sigma_mr / mr,
         ln_mr = log(mr),
         std = 1.96 * sqrt(log(sigma_mr + 1)),
         upper_mr = exp(ln_mr + std),
         lower_mr = exp(ln_mr - std)) %>% 
  select(-std) -> mr

# mr_sum %>% 
#   full_join(data.frame(year = min(mr_sum$year):lyr)) %>% 
#   arrange(year) -> mr_sum

# Graphics ----

axis <- tickr(catch, year, 5)

catch %>% 
  ggplot() + 
  geom_point(aes(year, catch)) +
  geom_line(aes(year, catch)) +
  geom_ribbon(aes(year, ymin = lower_catch, ymax = upper_catch),
              alpha = 0.2,  fill = "grey") +
  # Board implemented Limitted Entry in 1985
  geom_vline(xintercept = 1985, linetype = 2, colour = "grey") +
  # Board implemented Equal Quota Share in 1994
  geom_vline(xintercept = 1994, linetype = 2, colour = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  scale_y_continuous(labels = scales::comma) +
  labs(x = "", y = "\n\nCatch\n(round mt)") -> catch_plot

ggplot(fsh_cpue) +
  geom_point(aes(year, fsh_cpue)) +
  geom_line(aes(year, fsh_cpue)) +
  geom_ribbon(aes(year, ymin = lower_fsh_cpue , ymax = upper_fsh_cpue),
              alpha = 0.2,  fill = "grey") +
  # Board implemented Limitted Entry in 1985
  geom_vline(xintercept = 1985, linetype = 2, colour = "grey") +
  # Board implemented Equal Quota Share in 1994
  geom_vline(xintercept = 1994, linetype = 2, colour = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  # lims(y = c(0, 1.05)) +
  labs(x = "", y = "\n\nFishery CPUE\n(round kg/hook)") -> fsh_cpue_plot

ggplot(data = srv_cpue) +
  geom_point(aes(year, srv_cpue)) +
  geom_line(aes(year, srv_cpue)) +
  geom_ribbon(aes(year, ymin = lower_srv_cpue, ymax = upper_srv_cpue),
              alpha = 0.2, col = "white", fill = "grey") +
  scale_x_continuous(limits = c(syr,lyr), breaks = axis$breaks, labels = axis$labels) + 
  # lims(y = c(0, 0.3)) +
  labs(y = "\n\nSurvey CPUE\n(number/hook)", x = NULL) -> srv_cpue_plot

mr %>% 
  mutate(year = as.Date(as.character(year), format = "%Y")) %>% 
  pad(interval = "year") %>% 
  mutate(year = year(year),
         Year = factor(year)) %>% 
  gather("Abundance", "N", mr) %>% 
  mutate(# interpolate the CI in missing years for plotting purposes
         lower_mr = zoo::na.approx(lower_mr, maxgap = 20, rule = 2),
         upper_mr = zoo::na.approx(upper_mr, maxgap = 20, rule = 2)) %>%
  ggplot() +
  # geom_errorbar(aes(x = year, ymin = N - 2*sd, ymax = N + 2*sd),
  #             colour = "grey", width = 0) +
  geom_ribbon(aes(x = year, ymin = lower_mr, ymax = upper_mr),
              alpha = 0.2, colour = "white", fill = "grey") +
  geom_point(aes(x = year, y = N)) +
  geom_line(aes(x = year, y = N, group = Abundance)) +
  scale_x_continuous(limits = c(syr,lyr), breaks = axis$breaks, 
                     labels = axis$labels) +
  expand_limits(y = c(0, 5)) +
  labs(x = "", y = "\n\nAbundance\n(millions)") -> mr_plot
  
plot_grid(catch_plot, fsh_cpue_plot, srv_cpue_plot, mr_plot, ncol = 1, align = 'hv', labels = c('(A)', '(B)', '(C)', '(D)'))

ggsave(paste0("figures/tmb/abd_indices_", YEAR, ".png"),
       dpi=300, height=8, width=7, units="in")

full_join(catch, fsh_cpue) %>% 
  full_join(srv_cpue) %>% 
  full_join(mr) %>% 
  mutate(index = year - min(year)) -> ts
  
write_csv(ts, "data/tmb_inputs/abd_indices.csv")

# Biological data ----

# Fishery biological data
read_csv(paste0("data/fishery/fishery_bio_2000_", lyr,".csv"), 
         guess_max = 50000) %>%
  mutate(Year = factor(year),
         Sex = factor(Sex)) -> fsh_bio

# Survey biological data
read_csv(paste0("data/survey/llsrv_bio_1985_", lyr,".csv"), 
         guess_max = 50000) %>%
  mutate(Year = factor(year),
         Sex = factor(Sex)) -> srv_bio

# Weight-at-age ----

waa <- read_csv("output/pred_waa.csv")

waa %>% 
  mutate(Source = derivedFactor("Survey (males)" = Source == "LL survey" & Sex == "Male",
                             "Survey (females)" = Source == "LL survey" & Sex == "Female",
                             "Survey (sexes combined)" = Source == "LL survey" & Sex == "Combined",
                             .default = NA),
         Age = factor(age, levels = c("2", "3", "4", "5", "6", "7", "8",
                                      "9", "10", "11", "12", "13", "14", "15",
                                      "16", "17", "18", "19", "20", "21", "22",
                                      "23", "24", "25", "26", "27", "28", "29", "30",
                                      "31", "32", "33", "34", "35", "36", "37", "38",
                                      "39", "40", "41", "42"),
                      labels = c("2", "3", "4", "5", "6", "7", "8",
                                 "9", "10", "11", "12", "13", "14", "15",
                                 "16", "17", "18", "19", "20", "21", "22",
                                 "23", "24", "25", "26", "27", "28", "29", "30",
                                 "31", "32", "33", "34", "35", "36", "37", "38",
                                 "39", "40", "41", "42+"))) -> waa
waa <- na.omit(waa)
write_csv(waa, "data/tmb_inputs/waa.csv")

# Proportion mature -----

read_csv(paste0("output/fem_maturityatage_llsrv.csv"), 
         guess_max = 50000) -> mat

mat %>% 
  filter(age <= plus_group)  %>% 
  rename(prop_mature = probability) %>% 
  mutate(Age = factor(age, levels = c("2", "3", "4", "5", "6", "7", "8",
                                      "9", "10", "11", "12", "13", "14", "15",
                                      "16", "17", "18", "19", "20", "21", "22",
                                      "23", "24", "25", "26", "27", "28", "29", "30",
                                      "31", "32", "33", "34", "35", "36", "37", "38",
                                      "39", "40", "41", "42"),
                      labels = c("2", "3", "4", "5", "6", "7", "8",
                                 "9", "10", "11", "12", "13", "14", "15",
                                 "16", "17", "18", "19", "20", "21", "22",
                                 "23", "24", "25", "26", "27", "28", "29", "30",
                                 "31", "32", "33", "34", "35", "36", "37", "38",
                                 "39", "40", "41", "42+"))) -> mat

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
                     age = plus_group) %>% 
              filter(Sex == "Female")) -> byage

# get generalized additive model fits and predictions
srv_fitage <- gam(I(Sex == "Female") ~ s(age), 
                  data = filter(srv_bio, age %in% rec_age:plus_group, 
                                Sex %in% c("Female", "Male")),
                  family = "binomial")

srv_predage <- predict(srv_fitage, newdata = data.frame(age = rec_age:plus_group),
                       type = "response", se = TRUE)

bind_cols(
  byage,
  #do.call cbinds each vector in the predict() output list 
  tbl_df(do.call(cbind, srv_predage))) -> byage

byage %>% 
  rename(prop_fem = proportion) %>% 
  mutate(Age = factor(age, levels = c("2", "3", "4", "5", "6", "7", "8",
                                      "9", "10", "11", "12", "13", "14", "15",
                                      "16", "17", "18", "19", "20", "21", "22",
                                      "23", "24", "25", "26", "27", "28", "29", "30",
                                      "31", "32", "33", "34", "35", "36", "37", "38",
                                      "39", "40", "41", "42"),
                      labels = c("2", "3", "4", "5", "6", "7", "8",
                                 "9", "10", "11", "12", "13", "14", "15",
                                 "16", "17", "18", "19", "20", "21", "22",
                                 "23", "24", "25", "26", "27", "28", "29", "30",
                                 "31", "32", "33", "34", "35", "36", "37", "38",
                                 "39", "40", "41", "42+"))) -> byage
full_join(byage %>% 
            select(age, prop_fem = fit),
          mat %>% 
            select(age, prop_mature)) %>% 
  write_csv("data/tmb_inputs/maturity_sexratio.csv")

# Graphics ----

# Custom axes
age_labs <- c("2", "", "", "", "6", "", "", "", "10", "", "", "", "14", "",
              "", "", "18", "", "", "", "22", "", "", "", "26", "",
              "", "", "30", "", "", "", "34", "", "", "", "38", "",
              "", "", "42+") 

ggplot(waa %>% 
         filter(Source != "Survey (sexes combined)") %>% 
         droplevels(), aes(x = Age, y = weight, shape = Source,
                 colour = Source, group = Source)) +
  geom_point() +
  geom_line() + 
  scale_colour_grey() +
  expand_limits(y = c(0, 10)) +
  labs(x = NULL, y = "\n\nMean weight\n(kg)", colour = NULL, shape = NULL) +
  scale_x_discrete(breaks = unique(waa$Age), labels = age_labs) +
  theme(legend.position = c(.2, .8)) -> waa_plot

# Equation text for plotting values of a_50
a50 <- 6.4
a50_txt <- as.character(
  as.expression(substitute(
    paste(italic(a[50]), " = ", xx),
    list(xx = formatC(a50, format = "f", digits = 1)))))

axis <- tickr(byage, age, 1)

ggplot(mat) +
  geom_point(aes(x = age, y = prop_mature), colour = "black", shape = 15) +
  geom_line(aes(x = age, y = prop_mature, group = 1), colour = "black") +
  geom_segment(aes(x = a50, y = 0, xend = a50, yend = 0.50), 
               lty = 2, col = "grey") +
  geom_segment(aes(x = 2, y = 0.50, xend = a50, yend = 0.50), 
               lty = 2, col = "grey") +
  # a_50 labels
  geom_text(aes(10, 0.5, label = a50_txt), 
            colour = "black", parse = TRUE, family = "Times New Roman") +
  scale_x_continuous(limits = c(2,42), breaks = axis$breaks, 
                     labels = age_labs) +
  labs(x = NULL, y = "\n\nProportion\nmature\n") -> mat_plot


ggplot(byage, aes(x = Age)) +
  geom_line(aes(y = fit, group = 1), colour = "black") +
  geom_ribbon(aes(ymin = fit - se.fit*2, ymax = fit + se.fit*2, group = 1), 
              alpha = 0.2, fill = "black", colour = "white") +
  geom_point(aes(y = prop_fem), colour = "black") +  
  expand_limits(y = c(0.3, 0.6)) +
  scale_x_discrete(breaks = unique(waa$Age), labels = age_labs) +
  xlab("\nAge") +
  ylab("\n\nProportion\nfemale\n") +
  geom_hline(yintercept = 0.5, lty = 2, col = "grey") -> prop_fem

plot_grid(waa_plot, mat_plot, prop_fem, ncol = 1, align = 'hv', labels = c('(A)', '(B)', '(C)'))

ggsave(paste0("figures/tmb/bio_dat.png"),
       dpi=300, height=7, width=6, units="in")

# Length compositions ----

# Not used yet in ASA model - FLAG where are all the pot survey lengths? We
# should have more years than we do. Filter out for now and come back to this
# later.
lencomps <- read_csv("output/lengthcomps.csv", guess_max = 500000)
lencomps <- lencomps %>% 
  mutate(Source = derivedVariable(`fsh_len` = Source == "LL fishery",
                                  `srv_len` = Source == "LL survey",
                                  `pot_srv_len` = Source == "Pot survey")) %>% 
  filter(Source != "pot_srv_len")

# Data source by year ----

ts %>% 
  gather("Source", "value", c(catch, fsh_cpue, srv_cpue, mr), na.rm = TRUE) %>% 
  select(year, Source) %>% 
  bind_rows(select(agecomps, year, Source)) %>% 
  bind_rows(select(lencomps, year, Source)) %>% 
  mutate(Source = derivedFactor(`Survey lengths` = Source == "srv_len",
                                `Fishery lengths` = Source == "fsh_len",
                                `Survey ages` = Source == "Survey",
                                `Fishery ages` = Source == "Fishery",
                                `Mark-recapture` = Source == "mr",
                                `Survey CPUE` = Source == "srv_cpue",
                                `Fishery CPUE` = Source == "fsh_cpue",
                                `Catch` = Source == "catch",
                                .ordered = TRUE)) -> df

df %>% 
  mutate(value = ifelse(year == YEAR & 
                          Source %in% c("Mark-recapture", "Fishery ages", "Catch", "Fishery CPUE"),
                        "1", "0")) -> df

axisx <- tickr(df, year, 5)

ggplot(df, aes(x = year, y = Source)) +
  geom_point(shape = 21, colour = "black", fill = "black", size = 2) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(breaks = axisx$breaks, labels = axisx$labels) 

ggsave(paste0("figures/tmb/sable_data_all.png"),
       dpi=300, height=4, width=6, units="in")

ggplot(df, aes(x = year, y = Source, fill = value)) +
  geom_point(shape = 21, colour = "black", size = 2) +
  labs(x = NULL, y = NULL) +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("white", "black")) +
  scale_x_continuous(breaks = axisx$breaks, labels = axisx$labels) 

ggsave(paste0("figures/sable_data.png"),
       dpi=300, height=3.5, width=7, units="in")
# Age compositions ----

# Fishery
fsh_bio %>% 
  select(year, age) %>% 
  filter(year >= 2002 & !is.na(age)) %>% 
  droplevels() %>% 
  mutate(age = ifelse(age > plus_group, plus_group, age)) %>%
  filter(age >= rec_age & age <= plus_group ) -> fsh_comps 

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
  mutate(age = ifelse(age > plus_group, plus_group, age)) %>%
  filter(age >= rec_age & age <= plus_group ) -> srv_comps  

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
  summarize(n = sum(n)) -> n_agecomps

axisx <- tickr(agecomps, year, 5)

ggplot(agecomps, aes(x = year, y = age, size = proportion)) +
  geom_point(shape = 21, colour = "black", fill = "black") +
  scale_size(range = c(0, 4)) +
  facet_wrap(~ Source) +
  xlab('\nAge') +
  ylab('') +
  guides(size = FALSE) +
  scale_x_continuous(breaks = axisx$breaks, labels = axisx$labels) +
  scale_y_continuous(breaks = unique(agecomps$age), labels = age_labs) 

ggsave("figures/tmb/agecomps.png", dpi = 300, height = 7, width = 9, units = "in")

agecomps %>% 
  left_join(data.frame(year = syr:lyr) %>% 
              mutate(index = year - min(year))) -> agecomps

# Add in effective sample size. For now effn = sqrt(n) until tuning methods can
# be developed.
full_join(select(agecomps, -n), n_agecomps) %>%
  mutate(effn = sqrt(n)) -> agecomps

# Reshape
agecomps %>% dcast(year + index + Source + n + effn ~ age, value.var = "proportion") -> agecomps
agecomps[is.na(agecomps)] <- 0

write_csv(agecomps, "data/tmb_inputs/agecomps.csv")

# Starting values ------

#fishing mortality deviations

# Parameter estimates for fishing mortality deviations for 2006 ASA run by F
# Mueter
finits <- c( -1.56630826679, -1.99300652020, -2.01246349536, -1.71978991896,
             -1.83724533971, -1.27684899334, -0.983905919896, -1.02192341186,
             -0.927685571254, -1.00247299047, -1.07852014044, -0.837760996556,
             -0.672220868864, -0.311074414121, -0.345622227419, -0.254984198245,
             -0.0825745205838, 0.0567339057886, 0.189847547504, -0.142833191728,
             -0.440286665534, -0.446782301623, -0.552812445258, -0.546158911033
             -0.385300803085, -0.417195861174, -0.296751206500)

# Fishing mortalities most correlated with catch; use simple linear regression
# to develop starting values for 2006-2017
tmp <- catch %>% 
  filter(year %in% 1980:2005) %>% 
  mutate(finits = finits)
f <- lm(finits ~ log(catch), data = tmp)
summary(f)
predf <- predict(f, 
                 newdata = data.frame(catch = ts %>% filter(year > 2005) %>% 
                                        pull(catch) %>% log()), 
                 type = "response")

finits <- data.frame(year = syr:lyr,
                     finits = c(finits, predf))

# Starting values for initial numbers-at-age deviations (nage-2)
inits_rinit <- c( 0.0405374218174, 0.0442650418415, 0.0482389177436,
                  0.0523509979580, 0.0564424635349, 0.0601823372231,
                  0.0631895950940, 0.0660174557708, 0.0543158576109,
                  0.00467416429863, 0.0405374218174, 0.0442650418415, 
                  0.0523509979580, 0.0564424635349, 0.0601823372231,
                  0.0631895950940, 0.0660174557708, 0.0543158576109,
                  0.0482389177436, 0.0442650418415, 0.0482389177436,
                  0.0523509979580, 0.0564424635349, 0.0601823372231,
                  0.0631895950940, 0.0660174557708, 0.0543158576109,
                  0.00467416429863, 0.0405374218174, 0.0442650418415, 
                  0.0523509979580, 0.0564424635349, 0.0601823372231,
                  0.0631895950940, 0.0660174557708, 0.0543158576109,
                  0.0631895950940, 0.0660174557708, 0.0543158576109)

inits_rinit <- data.frame(age = (rec_age+1):(plus_group-1),
                          inits_rinit = inits_rinit)

# Starting values for recruitment deviations           
inits_rec_dev <- c(-0.0188002294100, -0.0697329463801, -0.152542991556,
                   -0.124574875686, -0.152542991556, -0.148273569414,
                   -0.103762760913, -0.0592664599196, -0.0417456966159,
                   -0.0293342663812, 0.0655298533471, -0.105369343293,
                   -0.282231768850, 2.33324844068, 2.02041811445,
                   0.430788527045, 0.136257510816, 1.38475669371,
                   0.106716839276, 0.834180223062, 0.129001661339,
                   0.192456696119, 0.149727613444, -0.227607751620,
                   -0.385442447714, 0.0419339981551, -0.0971306887540,
                   0.00714024634689, -0.928016835774, 0.285420095181,
                   -0.138735004317, 0.469180409653, 0.566364647489,
                   0.349805017285, -0.448967807172, -0.169286675728,
                   -1.42262349732, -1.74624961284, -1.42262349732)

inits_rec_dev <- data.frame(year = syr:lyr,
                            inits_rec_dev = inits_rec_dev)
# Only write files if the vectors are the right lengths
write_csv(finits, "data/tmb_inputs/inits_f_devs.csv")
write_csv(inits_rinit, "data/tmb_inputs/inits_rinit.csv")
write_csv(inits_rec_dev, "data/tmb_inputs/inits_rec_devs.csv")

