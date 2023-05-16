# Data prep for statistical catch-at-age model
# Author: Jane Sullivan
# Contact: jane.sullivan@noaa.gov
# Last edited: March 2021

# Model is sex-structured, catch, fishery and survey CPUE, fishery and survey
# weight-at-age, fishery and survey age compositions (sexes combined due to
# sample size), and fishery and survey length compositions.

# Set up ----

source("r_helper/helper.r")
source("r_helper/functions.r")

syr <- 1975
lyr <- YEAR <- 2022
nyr <- length(syr:lyr)

rec_age <- 2
plus_group <- 31
nage <- length(rec_age:plus_group)

# Harvest ----

# Use IFDB/fish ticket data 1986-present and Dave Carlile's published catch time
# series for 1975-1985, Carlile et al 2002 (Regional Information Report No.1
# 1J02-02). Could use full catch time series in a model if you were interested
# in reconstructing a long biomass time series.

histcatch <- read_csv(paste0(YEAR+1,"/data/fishery/nsei_historicalsablecatch_nosource_carlile_1907_2000.csv"))
names(histcatch) <- c("year", "catch")
histcatch <- histcatch %>% 
  # Convert round/whole lbs to mt
  mutate(catch = catch * 0.000453592) %>% 
  filter(year >= 1975 & year <= 1985)

read_csv(paste0(YEAR+1,"/data/fishery/nseiharvest_ifdb_1969_", lyr,".csv"), 
                       guess_max = 50000) %>% 
  filter(year > 1985) %>% 
  group_by(year) %>% 
  # Convert lbs to mt
  dplyr::summarize(catch = sum(whole_pounds) * 0.000453592) %>% 
  bind_rows(histcatch) %>% 
  # Assumed variability in log space (CV) = 0.05
  mutate(sigma_catch = 0.05,
         ln_catch = log(catch),
         std = 1.96 * sqrt(log(sigma_catch + 1)),
         upper_catch = exp(ln_catch + std),
         lower_catch = exp(ln_catch - std)) %>% 
  select(-c(std, ln_catch)) %>% 
  filter(year >= syr) %>% 
  arrange(year) -> catch

# Fishery CPUE ----
# in 2023 we are switching to a fully standardized fishery cpue index from the longline fishery
# in 2023 we began work on moving towards using the "true" sigma for the 3 indices.  This will require
# adding in extra variance at some stage because narrow sigmas results in either unconverged
# models or very strange results.  The goal is to have the "tau terms (extra varriance) estimated
# within the model, but could could not be implemented in 2023.  

read_csv(paste0(YEAR+1,"/output/ll_cpue_fullstand_1980_",YEAR,".csv")) -> fsh_cpue 

fsh_cpue <- fsh_cpue %>% mutate(fsh_cpue = fsh_cpue * 0.453592,
                                q975 = fsh_cpue + 1.96*se* 0.453592,
                                ln_fsh_cpue = log(fsh_cpue),
                                #tau = 12,
                                #true_sig = tau*(exp(((log(q975)-ln_fsh_cpue)/1.96)^2)-1),
                                sigma_fsh_cpue = ifelse(year %in% (c(seq(1980,1996,1))),0.1,0.08),
                                #sigma_fsh_cpue = ifelse(year %in% (c(seq(1980,1996,1))),
                                #                        0.1,
                                #                        true_sig),
                                #sigma_fsh_cpue = true_sig,
                                std = 1.96 * sqrt(log(sigma_fsh_cpue + 1)),
                                upper_fsh_cpue = exp(ln_fsh_cpue + std ),
                                lower_fsh_cpue = exp(ln_fsh_cpue - std ), 
                               #upper_fsh_cpue = exp(log(fsh_cpue)+1.96*sqrt(log(sigma_fsh_cpue+1))),
                               #lower_fsh_cpue = exp(log(fsh_cpue)-1.96*sqrt(log(sigma_fsh_cpue+1)))
                               ) %>%
                               #upper_fsh_cpue = upper,
                               #lower_fsh_cpue = lower) %>% 
  select(year, fsh_cpue, sigma_fsh_cpue, #truesigma_fsh_cpue = var, 
         upper_fsh_cpue, lower_fsh_cpue)
view(fsh_cpue)
# Read in data, standardize cpue, etc.
#read_csv(paste0(YEAR+1,"/data/fishery/fishery_ll_cpue_1997_", YEAR,".csv"), 
##read_csv(paste0("data/fishery/fishery_cpue_2022reboot_1997_", fsh_lyr,".csv"), 
#         guess_max = 50000) %>% 
#  filter(!is.na(hook_space) & !is.na(sable_lbs_set) & !is.na(no_hooks) &
#           julian_day > 226) %>%  # if there were special projects before the fishery opened
#  mutate(# standardize hook spacing (Sigler & Lunsford 2001, CJFAS), 1 m = 39.37 in
#         std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (hook_space / 39.37))), 
         # convert lbs to kg
#         std_cpue_kg = (sable_lbs_set * 0.453592) / std_hooks) -> fsh_cpue  
#view(fsh_cpue)
#str(fsh_cpue)
# Nominal CPUE 
#fsh_cpue %>% 
#  group_by(year) %>% 
#  dplyr::summarise(fsh_cpue = mean(std_cpue_kg),
#            n = n(),
#            sd = sd(std_cpue_kg),
#            se = sd / sqrt(n),
            # sigma_fsh_cpue = se / fsh_cpue, # relative standard error too low
            # sigma_fsh_cpue = sd / fsh_cpue#, # CV too high
            # *FLAG* currently just assume cv=0.05 for new ts, 0.1 for old
#            sigma_fsh_cpue = 0.08,  #why assume sigma when you have sd measurements which are much larger?
#            truesigma_fsh_cpue = se / fsh_cpue
#            ) -> fsh_cpue 

#22rebooted data...
#read_csv(paste0("data/fishery/fishery_cpue_2022reboot_1997_", fsh_lyr,".csv"), 
#         guess_max = 50000) %>% 
#  filter(!is.na(hook_space) & !is.na(sable_lbs_set) & !is.na(no_hooks) &
#           julian_day > 226) %>%  # if there were special projects before the fishery opened
#  mutate(# standardize hook spacing (Sigler & Lunsford 2001, CJFAS), 1 m = 39.37 in
#    std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (hook_space / 39.37))), 
    # convert lbs to kg
#    std_cpue_kg = (sable_lbs_set * 0.453592) / std_hooks) -> fsh_cpue_22rb  
# Nominal CPUE 
#fsh_cpue_22rb %>% 
#  group_by(year) %>% 
#  dplyr::summarise(fsh_cpue = mean(std_cpue_kg),
#                   n = n(),
#                   sd = sd(std_cpue_kg),
#                   se = sd / sqrt(n),
                   # sigma_fsh_cpue = se / fsh_cpue, # relative standard error too low
                   # sigma_fsh_cpue = sd / fsh_cpue#, # CV too high
                   # *FLAG* currently just assume cv=0.05 for new ts, 0.1 for old
#                   sigma_fsh_cpue = 0.08,  #why assume sigma when you have sd measurements which are much larger?
#                   truesigma_fsh_cpue = se / fsh_cpue
#  ) -> fsh_cpue_22rb 

# Historical CPUE 

# From KVK: Logbooks were not included in IFDB until 1997. Commercial fishery
# CPUE values prior to 1997, for use in the ASA or other medium, are LEGACY
# VALUES. Jane updates: I don't have any source information from these numbers
# other than this. Kray kept them in a csv file called
# data/legacy_fishery_cpue.csv. Similarly, I moved and renamed the same file as
# data/fishery/legacy_fisherycpue_1980_1996.csv

#read_csv("legacy_data/fishery/legacy_fisherycpue_1980_1996.csv",
#         col_names = FALSE) %>% as.numeric() -> hist_cpue

# Use the mean CV from 1997-present to estimate the variance for legacy CPUE
# values, following KVK.

#data.frame(year = 1980:1996,
#           # Convert to kg
#           fsh_cpue = hist_cpue * 0.453592,
#           sigma_fsh_cpue = 0.1) %>%
#  bind_rows(fsh_cpue) %>%
#  mutate(ln_fsh_cpue = log(fsh_cpue),
         #std = 1.96 * sqrt(log(sigma_fsh_cpue + 1)),
#         std = 1.96 * sqrt(log(se + 1)),
#         upper_fsh_cpue = exp(ln_fsh_cpue + std),
#         lower_fsh_cpue = exp(ln_fsh_cpue - std)) %>% 
  #select(-c(n, sd, se, ln_fsh_cpue, std)) -> fsh_cpue
#  select(-c(var, se, upper, lower, ln_fsh_cpue)) -> fsh_cpue

# Temporary: include rows for missing years (can remove this once fishery CPUE
# are reestablished)
#fsh_cpue <- data.frame(year = syr:lyr) %>% 
#  full_join(fsh_cpue) %>% 
#  arrange(year)

#view(fsh_cpue)


#note that processed CPUEs are already in pounds, but one's processed here are in kg
# Survey NPUE ----

# If you wanted to show it in log space
# read_csv(paste0("output/srvcpue_1997_", lyr, ".csv")) %>% 
#   mutate(cv = sdev / annual_cpue,
#          pred = obj$report()$pred_srv_cpue,
#          ln_pred = log(pred)) %>% 
#   select(year, srv_cpue = annual_cpue, sigma_srv_cpue = cv, pred_srv_cpue) %>% 
#   mutate(upper_srv_cpue = qnorm(0.975, log(srv_cpue), sigma_srv_cpue),
#          lower_srv_cpue = qnorm(0.025, log(srv_cpue), sigma_srv_cpue)) -> srv_cpue

read_csv(paste0(YEAR+1,"/output/srvcpue_1997_", YEAR, ".csv")) %>% 
  rename(srv_cpue = std_cpue) %>% 
  # assuming lognormal distribution, use relative se as model input sigma
  mutate(#sigma_srv_cpue = se / srv_cpue, # relative standard error too low! 
         #sigma_srv_cpue = sd / srv_cpue, # cv too high!
         #sigma_srv_cpue = 0.08,
         true_sig = sd,
         q975 = srv_cpue + 1.96*true_sig,
         ln_srv_cpue = log(srv_cpue),
         sigma_srv_cpue = exp(((log(q975)-ln_srv_cpue)/1.96)^2)-1,
         std = 1.96 * sqrt(log(sigma_srv_cpue + 1)),
         upper_srv_cpue = exp(ln_srv_cpue + std ),
         lower_srv_cpue = exp(ln_srv_cpue - std )) %>% 
         #upper_srv_cpue = srv_cpue + 1.96*sigma_srv_cpue,
         #lower_srv_cpue = srv_cpue - 1.96*sigma_srv_cpue) %>% 
  select(-c(sd, se, ln_srv_cpue, std, true_sig, q975)) -> srv_cpue 

# Mark-recapture index ----

read_csv(paste0(YEAR+1,"/output/mr_index_", YEAR, ".csv")) %>% 
  select(year, mr = estimate, sd, q025, q975) %>% 
  mutate(# sigma_mr = sigma_mr / mr,
         sigma_mr = 0.05,
         #true_sig = sd,
         #tau = 5,
         #sigma_mr = tau*(exp(((log(q975)-log(mr))/1.96)^2)-1),
         ln_mr = log(mr),
         std = 1.96 * sqrt(log(sigma_mr + 1)),
         upper_mr = exp(ln_mr + std),
         lower_mr = exp(ln_mr - std)) %>% 
         #upper_mr = q975,
         #lower_mr = q025) %>% 
  select(-c(std, ln_mr, q025, q975, sd)) -> mr

full_join(catch, fsh_cpue) %>% 
  full_join(srv_cpue) %>% 
  full_join(mr) %>% 
  mutate(index = year - min(year)) -> ts

view(ts)

write_csv(ts, paste0(YEAR+1,"/data/tmb_inputs/abd_indices_", YEAR, ".csv"))


# Figure for industry mtg
# axis <- tickr(data.frame(year = 2005:YEAR), year, 3)
read_csv(paste0(YEAR+1,"/output/mr_index_", YEAR, ".csv")) %>% 
  # mutate(year = as.Date(as.character(year), format = "%Y")) %>% 
  # pad(interval = "year") %>% 
  full_join(data.frame(year = 2005:lyr)) %>%
  arrange(year) %>% 
  mutate(#year = year(year),
         Year = factor(year)) %>% 
  # gather("Abundance", "N", mr) %>% 
  mutate(# interpolate the CI in missing years for plotting purposes
    lower_mr = zoo::na.approx(q025, maxgap = 20, rule = 2),
    upper_mr = zoo::na.approx(q975, maxgap = 20, rule = 2),
    estimate_interp = zoo::na.approx(estimate, maxgap = 20, rule = 2)) %>%
  ggplot() +
  geom_ribbon(aes(x = year, ymin = lower_mr, ymax = upper_mr),
              alpha = 0.4, col = "white", fill = "grey") +
  geom_point(aes(x = year, y = estimate)) +
  geom_line(aes(x = year, y = estimate)) +
  geom_line(aes(x = year, y = estimate_interp), lty = 2) +
  # scale_x_continuous(breaks = axis$breaks, 
  #                    labels = axis$labels) +
  expand_limits(y = c(0, 4)) +
  labs(x = NULL, y = "\n\nAbundance (millions)\n") 

ggsave(paste0(YEAR+1,"/figures/mr_abd_", YEAR, ".png"), 
       dpi=300, height=4, width=7, units="in")

# Percent change in compared to a ten year rolling average
mr %>% 
  filter(year > YEAR - 10 & year <= YEAR) %>% 
  mutate(lt_mean = mean(mr),
         perc_change_lt = (mr - lt_mean) / lt_mean * 100,
         eval_lt = ifelse(perc_change_lt < 0, "decrease", "increase")) %>% 
  filter(year == YEAR) -> mr_lt
mr_lt

# Percent change from last mr estimate
years_since_last<-2

mr %>% 
  filter(year >= YEAR - years_since_last & year <= YEAR) %>%
  select(year, mr) %>% 
  mutate(year2 = ifelse(year == YEAR, "thisyr", "lastyr")) %>% 
  reshape2::dcast("mr" ~ year2, value.var = "mr") %>% 
  mutate(perc_change_ly = (thisyr - lastyr) / lastyr * 100,
         eval_ly = ifelse(perc_change_ly < 0, "decreased", "increased")) -> mr_ly
mr_ly

# Graphics ----

# axis <- tickr(catch, year, 5)

catch %>% 
  ggplot() + 
  geom_point(aes(year, catch)) +
  geom_line(aes(year, catch)) +
  geom_ribbon(aes(year, ymin = lower_catch, ymax = upper_catch),
              alpha = 0.2, fill = "black", colour = NA) +
  # Board implemented Limited Entry in 1985
  # geom_vline(xintercept = 1985, linetype = 2, colour = "grey") +
  # Board implemented Equal Quota Share in 1994
  # geom_vline(xintercept = 1994, linetype = 2, colour = "grey") +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  scale_x_continuous(breaks = seq(1975,2021,5), labels = seq(1975,2021,5)) +
  scale_y_continuous(labels = scales::comma) + 
  expand_limits(y = 0) +
  labs(x = "", y = "\n\nCatch\n(round mt)") +
  theme(axis.title.y = element_text(angle=0)) -> catch_plot
catch_plot

ggplot(fsh_cpue) +
  geom_point(aes(year, fsh_cpue * 2.20462)) +#* 2.20462)) +
  geom_line(aes(year, fsh_cpue * 2.20462)) + #* 2.20462)) +
  geom_ribbon(aes(year, ymin = lower_fsh_cpue * 2.20462 , ymax = upper_fsh_cpue * 2.20462),
              alpha = 0.2, fill = "black", colour = NA) +
  #geom_ribbon(aes(year, ymin = lower_fsh_cpue  , ymax = upper_fsh_cpue ),
  #            alpha = 0.2, fill = "black", colour = NA) +
  # Board implemented Limitted Entry in 1985
  # geom_vline(xintercept = 1985, linetype = 2, colour = "grey") +
  # Board implemented Equal Quota Share in 1994
  geom_vline(xintercept = 1994, linetype = 2, colour = "grey") +
  #scale_x_continuous(limits = c(syr,lyr)) + #, breaks = axis$breaks, labels = axis$labels) +
  scale_x_continuous(breaks = seq(1975,2021,5), labels = seq(1975,2021,5)) +
  # scale_y_continuous(limits = c(0, ))
  expand_limits(y = 0) +
  labs(x = "", y = "\n\nFishery CPUE\n(round lb/hook)") +
  theme(axis.title.y = element_text(angle=0)) -> fsh_cpue_plot
fsh_cpue_plot

ggplot(data = srv_cpue) +
  geom_point(aes(year, srv_cpue)) +
  geom_line(aes(year, srv_cpue)) +
  geom_ribbon(aes(year, ymin = lower_srv_cpue, ymax = upper_srv_cpue),
              alpha = 0.2, fill = "black", colour = NA) +
 # scale_x_continuous(limits = c(syr,lyr)) + #, breaks = axis$breaks, labels = axis$labels) + 
  scale_x_continuous(breaks = seq(1975,2021,5), labels = seq(1975,2021,5),limits = c(syr,lyr)) +
  expand_limits(y = 0) +
  labs(y = "\n\nSurvey CPUE\n(number/hook)", x = NULL) +
  theme(axis.title.y = element_text(angle=0)) -> srv_cpue_plot
srv_cpue_plot

mr %>% 
  # mutate(year = as.Date(as.character(year), format = "%Y")) %>% 
  # pad(interval = "year") %>% 
  full_join(data.frame(year = 2005:lyr)) %>%
  arrange(year) %>% 
  mutate(#year = year(year),
         Year = factor(year)) %>% 
  gather("Abundance", "N", mr) %>% 
  mutate(# interpolate the CI in missing years for plotting purposes
         lower_mr = zoo::na.approx(lower_mr, maxgap = 20, rule = 2),
         upper_mr = zoo::na.approx(upper_mr, maxgap = 20, rule = 2)) %>%
  ggplot() +
  geom_ribbon(aes(x = year, ymin = lower_mr, ymax = upper_mr),
              alpha = 0.2, fill = "black", colour = NA) +
  geom_point(aes(x = year, y = N)) +
  geom_line(aes(x = year, y = N, group = Abundance)) +
 # scale_x_continuous(limits = c(syr,lyr)) +#, breaks = axis$breaks, 
                     # labels = axis$labels) +
  scale_x_continuous(breaks = seq(1975,2021,5), labels = seq(1975,2021,5),limits = c(syr,lyr)) +
  expand_limits(y = c(0, 5)) +
  labs(x = "", y = "\n\nAbundance\n(millions)") +
  theme(axis.title.y = element_text(angle=0)) -> mr_plot
mr_plot
  
plot_grid(catch_plot, fsh_cpue_plot, srv_cpue_plot, mr_plot, ncol = 1, align = 'hv',
          labels = c('(A)', '(B)', '(C)', '(D)'))
# plot_grid(fsh_cpue_plot, srv_cpue_plot, mr_plot, ncol = 1, align = 'hv')

ggsave(paste0(YEAR+1,"/figures/tmb/abd_indices_", YEAR, "V2.png"),
       dpi=300, height=10, width=7.7, units="in")




#==========================================================================
# Biological data ----

# Fishery biological data
read_csv(paste0(YEAR+1,"/data/fishery/fishery_bio_2000_", YEAR,".csv"), 
         guess_max = 50000) %>%
  mutate(Year = factor(year),
         Sex = factor(Sex)) -> fsh_bio

# Survey biological data
read_csv(paste0(YEAR+1,"/data/survey/llsrv_bio_1988_", lyr,".csv"), 
         guess_max = 50000) %>%
  mutate(Year = factor(year),
         Sex = factor(Sex)) -> srv_bio

# Weight-at-age ----

waa <- read_csv(paste0(YEAR+1,"/output/pred_waa_plsgrp", plus_group, "_", YEAR, ".csv"))

waa %>% 
  mutate(Age = factor(age, levels = c("2", "3", "4", "5", "6", "7", "8",
                                      "9", "10", "11", "12", "13", "14", "15",
                                      "16", "17", "18", "19", "20", "21", "22",
                                      "23", "24", "25", "26", "27", "28", "29", "30",
                                      "31"),
                      labels = c("2", "3", "4", "5", "6", "7", "8",
                                 "9", "10", "11", "12", "13", "14", "15",
                                 "16", "17", "18", "19", "20", "21", "22",
                                 "23", "24", "25", "26", "27", "28", "29", "30",
                                 "31+"))) -> waa

waa2 <- waa %>% arrange(Source, Sex, age)
write_csv(waa2, paste0(YEAR+1,"/data/tmb_inputs/waa_", YEAR, ".csv"))

# Proportion mature -----

read_csv(paste0(YEAR+1,"/output/fem_maturityatage_llsrv_plsgrp", plus_group, "_", YEAR, ".csv"), 
         guess_max = 50000) -> mat

mat %>% 
  filter(age <= plus_group)  %>% 
  rename(prop_mature = probability) %>% 
  mutate(Age = factor(age, levels = c("2", "3", "4", "5", "6", "7", "8",
                                      "9", "10", "11", "12", "13", "14", "15",
                                      "16", "17", "18", "19", "20", "21", "22",
                                      "23", "24", "25", "26", "27", "28", "29", "30",
                                      "31"),
                      labels = c("2", "3", "4", "5", "6", "7", "8",
                                 "9", "10", "11", "12", "13", "14", "15",
                                 "16", "17", "18", "19", "20", "21", "22",
                                 "23", "24", "25", "26", "27", "28", "29", "30",
                                 "31+"))) -> mat

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
summary(srv_fitage) #note that this relationship is signifcant, but with a tiny r2

srv_predage <- predict(srv_fitage, newdata = data.frame(age = rec_age:plus_group),
                       type = "response", se = TRUE)

bind_cols(
  byage,
  #do.call cbinds each vector in the predict() output list 
  as_tibble(do.call(cbind, srv_predage))) -> byage
view(byage)

byage %>% 
  rename(prop_fem = proportion) %>% 
  mutate(Age = factor(age, levels = c("2", "3", "4", "5", "6", "7", "8",
                                      "9", "10", "11", "12", "13", "14", "15",
                                      "16", "17", "18", "19", "20", "21", "22",
                                      "23", "24", "25", "26", "27", "28", "29", "30",
                                      "31"),
                      labels = c("2", "3", "4", "5", "6", "7", "8",
                                 "9", "10", "11", "12", "13", "14", "15",
                                 "16", "17", "18", "19", "20", "21", "22",
                                 "23", "24", "25", "26", "27", "28", "29", "30",
                                 "31+"))) -> byage
full_join(byage %>% 
            select(age, prop_fem = fit),
          mat %>% 
            select(age, prop_mature)) %>% 
  write_csv(paste0(YEAR+1,"/data/tmb_inputs/maturity_sexratio_", YEAR, ".csv"))

# Graphics ----

# Custom axes
age_labs <- c("2", "", "", "", "6", "", "", "", "10", "", "", "", "14", "",
              "", "", "18", "", "", "", "22", "", "", "", "26", "",
              "", "", "30", "") 

ggplot(waa %>% 
         filter(! (Sex %in% c("Combined"))) %>% 
         droplevels() %>% 
         mutate(group = paste(Sex, Source)), 
       aes(x = Age, y = round_kg * 2.20462, shape = Sex, 
           linetype = Sex, colour = Source, group = group)) +
  geom_point() +
  geom_line() + 
  scale_colour_grey() +
  expand_limits(y = c(0, 7)) +
  labs(x ="Age", y = "\n\nMean weight (lb)", colour = NULL, shape = NULL, linetype = NULL) +
  scale_x_discrete(breaks = unique(waa$Age), labels = age_labs) +
  theme(legend.position = c(.1, .8),
        legend.spacing.y = unit(0, "cm")) -> waa_plot
waa_plot 

# Equation text for plotting values of a_50 (from biological.R)
(a50 <- read_csv(paste0(YEAR+1,"/output/maturity_param_", YEAR)) %>% 
  pull(a50))

a50_txt <- as.character(
  as.expression(substitute(
    paste(italic(a[50]), " = ", xx),
    list(xx = formatC(a50, format = "f", digits = 1)))))

mat_plot <- mat %>% filter(age <= 20)
# axis <- tickr(mat_plot, age, 1)

# age_labs2 <- c("2", "", "", "", "6", "", "", "", "10", "", "", "", "14", "",
              # "", "", "18", "", "") 

ggplot(mat_plot %>% filter(age <= 20)) +
  geom_point(aes(x = age, y = prop_mature), colour = "black") +
  geom_line(aes(x = age, y = prop_mature, group = 1), colour = "black") +
  geom_segment(aes(x = a50, y = 0, xend = a50, yend = 0.50), 
               lty = 2, col = "grey") +
  geom_segment(aes(x = 2, y = 0.50, xend = a50, yend = 0.50), 
               lty = 2, col = "grey") +
  # a_50 labels
  geom_text(aes(12, 0.5, label = a50_txt), 
            colour = "black", parse = TRUE, family = "Times New Roman") +
  scale_x_continuous(limits = c(rec_age, 20)) + # breaks = axis$breaks, 
                     # labels = age_labs2) +
  labs(x = "Age", y = "\n\nProportion mature") -> mat_plot
mat_plot

# axis <- tickr(byage, age, 1)

ggplot(byage, aes(x = Age)) +
  geom_line(aes(y = fit, group = 1), colour = "black") +
  geom_ribbon(aes(ymin = fit - se.fit*2, ymax = fit + se.fit*2, group = 1), 
              alpha = 0.2, fill = "black", colour = NA) +
  geom_point(aes(y = prop_fem), colour = "black") +  
  expand_limits(y = c(0.3, 0.6)) +
  scale_x_discrete(breaks = unique(waa$Age), labels = age_labs) +
  labs(x = "Age", y = "\n\nProportion female") +
  geom_hline(yintercept = 0.5, lty = 2, col = "grey") -> prop_fem
prop_fem

bottom_plot <- plot_grid(mat_plot, prop_fem, ncol = 2, align = 'hv', labels = c('(B)', '(C)'))
bottom_plot
plot_grid(waa_plot, bottom_plot, nrow = 2, rel_widths = c(1, 1), labels = c('(A)', ''))

# Drop sex ratios since it's not used in SCAA model when sex-structured
plot_grid(waa_plot, mat_plot, nrow = 2, rel_widths = c(1, 1), labels = c('(A)', '(B)'))

ggsave(paste0(YEAR+1,"/figures/tmb/bio_dat_", YEAR, ".png"), dpi=300, height=10, width=7.5, units="in")

# Length compositions ----

lencomps <- read_csv(paste0(YEAR+1,"/output/lengthcomps_", YEAR, ".csv"), guess_max = 500000)
unique(lencomps$Source)
lencomps <- lencomps %>%   
  filter(Source != "Pot survey" & Source != "Pot fishery") %>% 
  mutate(Source = derivedVariable(`fsh_len` = Source == "LL fishery",
                                  `srv_len` = Source == "LL survey")) %>% 
  group_by(Source, Sex, year) %>% 
  mutate(n = sum(n),
         effn = sqrt(n)) %>% 
  left_join(data.frame(year = syr:lyr) %>% 
              mutate(index = year - min(year))) %>% 
  arrange(Source, year, Sex)

write_csv(lencomps, paste0(YEAR+1,"/data/tmb_inputs/lencomps_", YEAR, ".csv"))

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
  dplyr::summarize(proportion = sum(proportion)) %>% 
  filter(proportion > 1.0001) # should be none of these!

# Sample sizes 
agecomps %>% 
  group_by(year, Source) %>% 
  dplyr::summarize(n = sum(n)) -> n_agecomps

# axisx <- tickr(agecomps, year, 5)

ggplot(agecomps, aes(x = year, y = age, size = proportion)) +
  geom_point(shape = 21, colour = "black", fill = "black") +
  scale_size(range = c(0, 4)) +
  facet_wrap(~ Source) +
  labs(x = '\nYear', y = 'Observed age\n') +
  guides(size = FALSE) +
  # scale_x_continuous(breaks = axisx$breaks, labels = axisx$labels) +
  scale_y_continuous(breaks = unique(agecomps$age), labels = age_labs) 

ggsave(paste0(YEAR+1,"/figures/tmb/agecomps_", YEAR, ".png"), dpi = 300, height = 7, width = 9, units = "in")

agecomps %>% 
  left_join(data.frame(year = syr:lyr) %>% 
              mutate(index = year - min(year))) -> agecomps

# Add in effective sample size. For now effn = sqrt(n) until tuning methods can
# be developed.
full_join(select(agecomps, -n), n_agecomps) %>%
  mutate(effn = sqrt(n)) -> agecomps

# Reshape
agecomps %>% reshape2::dcast(year + index + Source + n + effn ~ age, value.var = "proportion") -> agecomps
agecomps[is.na(agecomps)] <- 0

agecomps <- agecomps %>% arrange(Source, year)

write_csv(agecomps, paste0(YEAR+1,"/data/tmb_inputs/agecomps_", YEAR, ".csv"))

# Data source by year ----
view(ts)
ts %>% 
  ungroup() %>% 
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
                          # data were used previously for the
                          # mark-recapture + yield per recruit (YPR) model
                          Source %in% c("Mark-recapture", "Fishery ages"),
                        "1", "0")) -> df
view(df)
# axisx <- tickr(df, year, 5)

ggplot(df, aes(x = year, y = Source)) +
  geom_point(shape = 21, colour = "black", fill = "black", size = 2) +
  labs(x = NULL, y = NULL) #+
  # scale_x_continuous(breaks = axisx$breaks, labels = axisx$labels) 

ggsave(paste0(YEAR+1,"/figures/tmb/sable_data_all_", YEAR, ".png"),
       dpi=300, height=4, width=6, units="in")

# This is the figure that shows which data were used previously for the
# mark-recapture + yield per recruit (YPR) model
ggplot(df, aes(x = year, y = Source, fill = value)) +
  geom_point(shape = 21, colour = "black", size = 2) +
  labs(x = NULL, y = NULL) +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("white", "black")) #+
  # scale_x_continuous(breaks = axisx$breaks, labels = axisx$labels) 

ggsave(paste0(YEAR+1,"/figures/sable_data_yprmodel_", YEAR, ".png"),
       dpi=300, height=3.5, width=7, units="in")

# Retention probabilities ----

# Background: prior to 2018, discarding behavior in the NSEI fishery was
# ignored. Due to influx of small fish from 2014 recruitment event, concerns
# over discarding were brought forward by then GF project leader Andrew Olson.
# Because data on discards by size are not available, expert opinion was used to
# develop a retention probability curve to be fixed in the assessment. This
# curve was based on conversations with A Olson, Mike Vaughn, Kamala Carroll,
# and Stephen Rhoades. The actual shape of the retention curve is not known and
# there realistically could be a lot more highgrading than suggested by this
# curve.

# Sent from M Vaughn 2018-06-04:
# Sablefish grade conversion Eastern cut to round weight (0.63)					
# | E/C grade 	| Converted round lb minimum 	| Converted round lb maximum 	| min kg 	| max kg 	| E/C dock price 	|
# |-----------	|----------------------------	|---------------------------:	|-------:	|-------:	|---------------:	|
# | 1/2       	| 1.6                        	|                        3.2 	|    0.7 	|    1.4 	|         $1.00  	|
# | 2/3       	| 3.2                        	|                        4.8 	|    1.4 	|    2.2 	|         $2.20  	|
# | 3/4       	| 4.8                        	|                        6.3 	|    2.2 	|    2.9 	|         $3.25  	|
# | 4/5       	| 6.3                        	|                        7.9 	|    2.9 	|    3.6 	|         $4.75  	|
# | 5/7       	| 7.9                        	|                       11.1 	|    3.6 	|    5.0 	|         $7.55  	|
# | +7        	| 11.1                       	|                            	|    5.0 	|        	|         $8.05  	|


# processor will be used to define the probability of retaini# Sent from R. Ehresmann 2022-03-13n#This is for 2022 Chatham sablefish data only, from fish tickets in OceanAK subject area “eLandings – Grading and Pricing”. I show #1 fish only below and then #1 and #2 combined (a bit less per pound). It does not include survey fish since we typically get a lower price than dock price. 
#Grade	#1 Fish only

# Size Grade	Average of Price/lb
# 1/2	$1.08
# 2/3	$2.21
# 3/4	$2.94
# 4/5	$4.72
# 5/7	$7.69
# 7+	$8.56
# Grand Total	$4.93

# Grade	#1 / #2 Fish Combined

# Size Grade	Average of Price/lb
# 1/2	$1.04
# 2/3	$2.12
# 3/4	$2.81
# 4/5	$4.52
# 5/7	$7.37
# 7+	$8.27
# Grand Total	$4.76

# 2023 note: price disparity between low and high grades appears bigger... 


grades <- data.frame(
  # Round kg
  kg = c(0.5, 0.6, 0.7, 1.4, 2.2, 2.9, 3.6, 5.0),
  # Based off conversation with A. Olson 2018-06-04, set grade 3/4 as 50%
  # probability of retention (p), and very low for grades below.
  p = c(0.0, 0.0, 0.0, 0.1, 0.5, 1.0, 1, 1.0)) %>%  
  right_join(data.frame(kg = seq(0.5, 8.5, by = 0.1)) %>% 
               mutate(grade = derivedFactor('No grade' = kg < 0.7,
                                            'Grade 1/2' = kg >= 0.7 & kg < 1.4,
                                            'Grade 2/3' = kg >= 1.4 & kg < 2.2,
                                            'Grade 3/4' = kg >= 2.2 & kg < 2.9,
                                            'Grade 4/5' = kg >= 2.9 & kg < 3.6,
                                            'Grade 5/7' = kg >= 3.6 & kg < 5,
                                            'Grade 7+' = kg >= 5,
                                            .method = "unique",
                                            .ordered = TRUE),
                      price = derivedFactor('$0' = grade == 'No grade',
                                            '$1.00' = grade == 'Grade 1/2',
                                            '$2.20' = grade == 'Grade 2/3',
                                            '$3.25' = grade == 'Grade 3/4',
                                            '$4.75' = grade == 'Grade 4/5',
                                            '$7.55' = grade == 'Grade 5/7',
                                            '$8.05' = grade == 'Grade 7+',
                                            .method = "unique",
                                            .ordered = TRUE),
                      # For plotting purposes (alternating grey and white panels)
                      plot_cde = ifelse(grade %in% c('No grade', 'Grade 2/3', 'Grade 4/5', 'Grade 7+'), "0", "1")), by = "kg") %>% # 
  # set p = 1 for all large fish, interpolate p's using a cubic spline across
  # smaller sizes
  arrange(kg) %>% 
  mutate(p = ifelse(kg > 3.6, 1, zoo::na.spline(p)),
         lb = 2.20462 * kg, # convert to lbs for visualization in memo
         y = 1 - p,
         dressed_kg = 0.63 * kg,
         dressed_lb = 0.63 * lb,
         kg = round(kg, 1))

# Retention probability inputs for TMB model, single sex and sex-structued:
ret <- waa %>% filter(Source == "LL survey") %>% 
  mutate(kg = round(round_kg, 1)) %>%
  left_join(grades, by = "kg") 

# View(ret)
write_csv(ret %>% select(Source, Sex, age, kg, grade, price, p), 
          paste0(YEAR+1,"/data/tmb_inputs/retention_probs.csv"))

# Plot size, sex, and age-specific probabilities of discarding a fish that
# parameterize model
# Min lbs by grade
grades %>% 
  filter(lb <= 12) %>% 
  group_by(grade, price, plot_cde) %>% 
  dplyr::summarize(mn = min(lb),
                   mx = max(lb),
                   mu = mean(lb)) %>% 
  ungroup() %>% 
  mutate(label = paste0(price, "/lb"),
         y = c(0.1, 0.2, 0.5, 0.4, 0.75, 0.9, 0.9))  -> grades2

# axis <- tickr(grades, lb , 1)

ggplot() +
  geom_line(data = grades %>% filter(lb <= 12), 
            aes(x = lb, y = p)) +
  geom_rect(data = grades2, aes(xmin = mn, xmax = mx, ymin = -Inf, ymax = Inf, fill = plot_cde, group = 1), 
            colour = NA, alpha = 0.2, show.legend = FALSE) +
  scale_fill_manual(values = c("white", "grey80")) +
  labs(x = "\n Round weight (lb)", y = "Retention probability\n") + 
  geom_text(data = grades2, aes(label = label, x = mu, y = y), 
            vjust = 1, size = 2) +
  geom_text(data = grades2, aes(label = grade, x = mu, y = y + 0.03), 
            vjust = 1, size = 2) -> size 

size
ret_sex <- ret %>% filter(Sex %in% c("Female", "Male"))

# axis <- tickr(ret_sex, age, 5)

ggplot(ret_sex, aes(x = age, y = p, col = Sex, linetype = Sex)) +
  geom_line() +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  scale_color_manual(values = c("black", "grey75")) + 
  scale_linetype_manual(values = c(1, 4)) + 
  # ylim(c(0, 1)) +
  labs(x = "\nAge", y = NULL) +
  theme(legend.position = c(.8, .7)) -> sex

plot_grid(size, sex, align = "h")

ggsave(paste0(YEAR+1,"/figures/tmb/retention_prob_", YEAR, "_", plus_group, ".png"), dpi=300,  height=4, width=8,  units="in")

# Alternative retention probability ----

# # Started this analysis May 2019 after a conversation in April 2019 with Steven
# Rhoades about alternative ways of developing these curves. This approach
# assumes that anything below a 7+ has some probability of being released
# because the risk of releasing a smaller fish comes with the potential reward
# of catching a larger fish. I use the same Eastern cut processor grade/price
# data from M. Vaughn used in the earlier analysis.

data.frame(dressed_lb = seq(0.1, 7, 0.1)) %>% 
  mutate(grade = derivedFactor('no_grade' = dressed_lb < 1,
                               '1/2' = dressed_lb >= 1 & dressed_lb < 2,
                               '2/3' = dressed_lb >= 2 & dressed_lb < 3,
                               '3/4' = dressed_lb >= 3 & dressed_lb < 4,
                               '4/5' = dressed_lb >= 4 & dressed_lb < 5,
                               '5/7' = dressed_lb >= 5 & dressed_lb < 7,
                               '7+' = dressed_lb >= 7,
                               .method = "unique",
                               .ordered = TRUE),
         Price = derivedFactor('$0' = grade == 'no_grade',
                               '$1.00' = grade == '1/2',
                               '$2.20' = grade == '2/3',
                               '$3.25' = grade == '3/4',
                               '$4.75' = grade == '4/5',
                               '$7.55' = grade == '5/7',
                               '$8.05' = grade == '7+',
                               .method = "unique",
                               .ordered = TRUE),
         price = readr::parse_number(as.character(Price)),
         value = dressed_lb * price,
         p_retain = value / max(value),
         whole_lb = dressed_lb/0.63) %>% # E/C to whole conversion
  ggplot(aes(x = whole_lb, y = p_retain)) +
  geom_point() +
  geom_line()

# Allometry ----

# New tables in report to help users translate figures during the industry
# meeting and when reading the stock assessment

# Federal sex-specific allometry Hanselman et al 2006 for comparison
# ma	<- 1.24E-05	#	Male
# mb	<- 2.96	#	Male
# fa	<- 1.01E-05	#	Female
# fb	<- 3.015	#	Female

# Use Chatham allomotry for sexes comb from biological.r
allom <- read_csv(paste0(YEAR+1,"/output/compare_vonb_adfg_noaa.csv")) %>% 
  filter(Sex == "Combined" & Survey == "ADFG Longline")

a	<- allom %>% filter(Parameter == "a") %>% pull(Estimate) 	
b	<- allom %>% filter(Parameter == "b") %>% pull(Estimate) 	

fork_len <- seq(40, 90, 1)

df <- data.frame(
  fork_length_cm = fork_len,
  sex_comb = a * fork_len ^ b
) %>% 
  pivot_longer(-fork_length_cm, names_to = "Sex", values_to = "round_kg") %>% 
  mutate(round_kg = round(round_kg, 2),
         round_lb = round_kg * 2.20462,
         easterncut_lb_0.63 = 0.63 * round_lb) # Eastern cut conversion 

df %>% 
  mutate(Sex = "Sexes combined") %>% 
  select(Sex, fork_length_cm, round_kg, round_lb, easterncut_lb_0.63) %>% 
  write_csv(paste0(YEAR+1,"/output/NSEI_forklen_dressedwt_conversion_", YEAR, ".csv"))

df %>% filter(fork_length_cm == 63)

waa <- read_csv(paste0(YEAR+1,"/output/pred_waa_plsgrp", plus_group, "_", YEAR, ".csv"), guess_max = 50000) %>% 
  rename(sex = Sex)
laa <- read_csv(paste0(YEAR+1,"/output/pred_laa_plsgrp", plus_group, "_", YEAR, ".csv"), guess_max = 50000) 
full_join(waa, laa) %>% 
#  rename(fork_length_cm = fork_len) %>% 
  rename(fork_length_cm = length) %>% 
  mutate(round_kg = round(round_kg, 2),
         round_lb = round(round_kg * 2.20462, 2),
         easterncut_lb_0.63 = round(round_lb * 0.63, 2)) -> waa_laa
  
waa_laa %>% filter(Source == "LL fishery") %>% 
  write_csv(paste0(YEAR+1,"/output/NSEI_fishery_forklen_weight_age_conversion_", YEAR, ".csv"))
  
waa_laa %>% filter(Source == "LL survey") %>% 
  write_csv(paste0(YEAR+1,"/output/NSEI_survey_forklen_weight_age_conversion_", YEAR, ".csv"))

# Selectivity ----

# update Apr 2021: tmb model indexes selectivity by 0, not the starting age of
# 2. until i can fix this, transform federal selectivity predictions
# appropriately. using predicted federal values, fit logistic curves to obtain
# fishery/survey selectivity from 0:29 instead of 2:31
sel <- read_csv(paste0(YEAR+1,"/data/fed_selectivity_2022.csv"))
view(sel)

unique(sel$fleet)
sel <- sel %>% 
  filter(fleet %in% c("Dom LL Survey Female", 
                      "Dom LL Survey Male",
                      "Fixed Gear Fish Pre-IFQ (Derby) Female",
                      "Fixed Gear Fishery Pre-IFQ (Derby) Male",
                      "Fixed gear Recent (not est/used) Female",
                      "Fixed gear Recent (not est/used) Male",
                      "Fixed Gear Fish Post-IFQ Female",
                      "Fixed Gear Fish Post-IFQ Male"))
sel <- sel %>% 
  pivot_longer(cols = 4:33, names_to = "age") %>% 
  mutate(age = as.numeric(age),
         tmb_age = age - 2)

params <- sel %>% 
  group_nest(fleet) %>% 
  mutate(model = map(data, 
                     ~glm(value ~ tmb_age, data = .x, family = "quasibinomial")),
         a = map(model, ~coef(.x)[1]),
         b = map(model, ~coef(.x)[2])) %>% 
  unnest(cols = c(a,b)) %>% 
  select(fleet, a, b)

view(params)
#params_new<-params

tmb_ages <- unique(sel$tmb_age)

params <- params %>% 
  mutate(a50 = -a/b) %>% 
  group_by(fleet) %>% 
  mutate(k = ((a + b * tmb_ages) / (tmb_ages - a50))[1],
         log_a50 = log(a50),
         log_k = log(k))

params <- params %>% 
  mutate(fleet = case_when(fleet == "Dom LL Survey Female" ~ "srv_f",
                           fleet == "Dom LL Survey Male" ~ "srv_m",
                           fleet == "Fixed Gear Fish Pre-IFQ (Derby) Female" ~ "fsh_t1_f" ,
                           fleet == "Fixed Gear Fishery Pre-IFQ (Derby) Male" ~ "fsh_t1_m",
                           fleet == "Fixed Gear Fish Post-IFQ Female" ~ "fsh_t2_f" ,
                           fleet == "Fixed Gear Fish Post-IFQ Male" ~ "fsh_t2_m",
                           fleet == "Fixed gear Recent (not est/used) Female" ~ "fsh_t3_f",
                           fleet == "Fixed gear Recent (not est/used) Male" ~ "fsh_t3_m"))

write_csv(params, paste0(YEAR+1,"/data/tmb_inputs/fed_selectivity_transformed_2022_3fsh.csv"))
