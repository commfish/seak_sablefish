# Work up of survey and fishery biological data
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov (ummjane@gmail.com)
# Last updated: June 2020 

source("r/helper.r")
source("r/functions.r")

YEAR <- 2019
rec_age <- 2
plus_group <- 31

# data -----

# survey biological  data

read_csv(paste0("data/survey/llsrv_bio_1985_", YEAR,".csv"), 
         guess_max = 50000) %>% 
  mutate(Year = factor(year),
         Project_cde = factor(Project_cde),
         Stat = factor(Stat),
         # Station = factor(Station),
         Sex = factor(Sex),
         Mature = Maturity) %>% 
  group_by(Year, Stat) %>% 
  mutate(n = length(age),
         length_mu = mean(length, na.rm = TRUE),
         weight_mu = mean(weight, na.rm = TRUE)) %>% 
  ungroup() -> srv_bio

# Hanselman et al. 2007 Appendix GOA Sablefish SAFE Appendix 3C von bertalanffy
# parameter estimates (length and weight) for comparison with estimates from the
# survey. The coastwide parameters are still used for management, but Southeast
# slope are informative.
noaa_lvb <- read_csv("data/survey/noaa_lvb_params_hanselman2007.csv")

# Fishery biological data
read_csv(paste0("data/fishery/fishery_bio_2000_", YEAR,".csv"), 
         guess_max = 50000) %>%
  mutate(Year = factor(year),
         Project_cde = factor(Project_cde),
         Adfg = factor(Adfg),
         Stat = factor(Stat),
         Sex = factor(Sex)) %>% 
  group_by(Year, Stat) %>% 
  mutate(n = length(age),
         length_mu = mean(length, na.rm = TRUE),
         weight_mu = mean(weight, na.rm = TRUE)) %>% 
  ungroup() -> fsh_bio

# Pot survey biological data
read_csv(paste0("data/survey/potsrv_bio_1981_", YEAR, ".csv"), 
         guess_max = 50000) %>% 
  mutate(Year = factor(year),
         Project_cde = factor(Project_cde),
         Stat = factor(Stat),
         Sex = factor(Sex)) -> potsrv_bio

# Empirical weight-at-age ----

# All years combined

bind_rows(
  srv_bio %>% 
    select(year, Project_cde, Sex, age, weight) %>% 
    filter(year >= 1997),
  fsh_bio %>% 
    select(year, Project_cde, Sex, age, weight) %>% 
    filter(year >= 2002)) %>% 
  filter(!is.na(weight) & !is.na(age) & !is.na(Sex)) %>% 
  mutate(Source = derivedFactor('LL survey' = Project_cde == "603",
                                'LL fishery' = Project_cde == "02",
                                .method = "unique")) %>% 
  filter(year <= YEAR & age >= rec_age & age <= plus_group) -> waa

bind_rows(
  waa %>%
    group_by(Source, Sex, age) %>% 
    summarise(weight = mean(weight) %>% round(4)),
  waa %>% 
    group_by(Source, age) %>% 
    summarise(weight = mean(weight) %>% round(4)) %>% 
    mutate(Sex = "Combined")) -> emp_waa

# Expand to grid to include all age combos and fill in NAs if there are any
# using linear interpolation
expand.grid(Source = unique(emp_waa$Source),
            Sex = unique(emp_waa$Sex),
            age = seq(rec_age, plus_group, 1))  %>% 
  data.frame()  %>% 
  full_join(emp_waa) %>%
  group_by(Source, Sex) %>% 
  mutate(weight = zoo::na.approx(weight, maxgap = 20, rule = 2)) -> emp_waa

write_csv(emp_waa, paste0("output/empircal_waa_", YEAR, ".csv"))

# Changes in weight-at-age

srv_bio %>% 
  select(year, Project_cde, Sex, age, weight) %>% 
  filter(year >= 1997) %>% 
  filter(!is.na(weight) & !is.na(age) & !is.na(Sex)) %>% 
  filter(year <= YEAR & age >= rec_age) %>% 
  group_by(year, Sex, age) %>% 
  summarise(weight = mean(weight) %>% round(4)) %>%  
  ungroup() %>% 
  mutate(Year = as.character(year),
         Age = factor(age),
         cohort = year - age,
         Cohort = as.factor(cohort))-> df

pal <- ggthemes::canva_pal("Warm and cool")(4) 

# By cohort
df_cohort <- df %>% 
         filter(cohort >= 2010 & cohort <= YEAR-3 & age >=2 & age <= 5) %>% 
         droplevels()

# Axis ticks for plot (see helper.r tickr() fxn for details)
axis <- tickr(df_cohort, year, 2)

ggplot(df_cohort, aes(year, weight, colour = Cohort, group = Cohort)) +
  geom_line(size = 1) +
  geom_point(aes(fill = Cohort), show.legend = FALSE, size = 1) +
  facet_grid(~ Sex) +
  labs(x = "Year", y = "Weight-at-age (grams)\n", colour = "Cohort") +
  guides(colour = guide_legend(ncol = 9)) +
  scale_colour_manual(values = colorRampPalette(pal)(n_distinct(df_cohort$Cohort))) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels)# -> waa_cohort_plot

ggsave("figures/waa_cohort.png", dpi = 300, height = 5, width = 7, units = "in")

df %>% 
  filter(Age %in% c("2", "3", "4")) %>% 
  droplevels() -> df
df %>% 
  group_by(Age, Sex) %>% 
  dplyr::summarize(mean_weight = mean(weight, na.rm = TRUE)) -> means

axis <- tickr(df, year, 5)

ggplot(df, 
       aes(year, weight, group = Age, colour = Age)) + 
  geom_line() + 
  geom_point() +
  facet_wrap(~ Sex, ncol = 1) +
  geom_hline(data = means, aes(colour = Age, yintercept = mean_weight), alpha = 0.4, linetype = 2) + 
  labs(x = "Year", y = "Weight-at-age (grams)\n", colour = "Age") +
  theme(legend.position = "bottom") +
  scale_colour_manual(values = colorRampPalette(pal)(n_distinct(df$Age))) +
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels)

ggsave("figures/waa_trends.png", dpi = 300, height = 5, width = 7, units = "in")

# Survey length-at-age -----

# subsets by length, age, sex
srv_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & # *FLAG* advent of "modern" survey
           !is.na(length) &
           !is.na(age)) %>% 
  droplevels() -> laa_sub

laa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Female") -> laa_f

laa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Male") -> laa_m

#sex-specific starting values from Hanselman et al. 2007 (Appendix C, Table 1),
#except sigma
start_f <- c(l_inf = 80, k = 0.22, t0 = -1.9, sigma = 10) 
start_m <- c(l_inf = 68, k = 0.29, t0 = -2.3, sigma = 10)

# mle fit for females
vb_mle_f <- vonb_len(obs_length = laa_f$length,
                     age = laa_f$age,
                     starting_vals = start_f,
                     sex = "Female")

# mle fit for males
vb_mle_m <- vonb_len(obs_length = laa_m$length,
                   age = laa_m$age,
                   starting_vals = start_m,
                   sex = "Male")

# combine predictions and parameter estimates and plot mle
bind_rows(vb_mle_f$predictions, vb_mle_m$predictions) %>% 
  mutate(std_resid = scale(resid)) -> pred

bind_rows(vb_mle_f$results, vb_mle_m$results) %>% 
  mutate(Survey = "ADF&G Longline",
         Years = paste0(min(laa_sub$year), "-", max(laa_sub$year)),
         Region = "Chatham Strait",
         Function = "Length-based LVB") %>% 
  full_join(laa_sub %>% 
              group_by(Sex) %>% 
              summarise(n = n())) -> lvb_pars

laa_plot <- laa_sub %>% filter(age <= plus_group)
pred_pl <- pred %>% filter(pred <= plus_group)
axis <- tickr(laa_plot, age, 5)

ggplot(laa_plot, aes(age, length)) +
  geom_jitter(aes(col = Sex, shape = Sex), alpha=.2, shape = 20) +
  geom_line(data = pred, aes(y = pred, col = Sex, group = Sex), size = 1) + #"#00BFC4"
  # geom_line(data = pred, aes(y = pred, group = Sex), col = "darkgrey" ) + #"#00BFC4"
  # scale_colour_grey(start = 0, end = 0.5) +
  scale_linetype_manual(values = c(2,1)) +
  scale_x_continuous(limits = c(rec_age, plus_group), breaks = axis$breaks, labels = axis$labels) +
  xlab("Age (yrs)") +
  ylab("Fork length (cm)") + 
  expand_limits(y = 0) +
  theme(legend.position = c(0.9, 0.2)) 

ggsave(paste0("figures/length_vonb_chathamllsurvey_1997_", YEAR, ".png"), dpi=300, height=4, width=6, units="in")

# residual plots
ggplot(data = pred) + 
  geom_histogram(aes(x = std_resid), bins=100) +
  facet_wrap(~ Sex)

# Are there annual trends in length-at-age? First vonB curves by Year for
# illustrative purposes

list_yrs <- unique(laa_f$year)

for (i in 1:length(list_yrs)) {
  
  #Get year-specific subset and fit model
  laa_yr <- laa_f %>% filter(year == list_yrs[i])
  
  vbfit <- vonb_len(obs_length = laa_yr$length,
                    age = laa_yr$age, starting_vals = start_f,
                    sex = "Female")
  
  #Append resuts for vonB predictions and parameter estimates
  pred <- vbfit$predictions %>% mutate(year = list_yrs[i])
  if(i == 1) {
    pred_out <- pred
    rm(pred) 
  } else {
    pred_out <- rbind(pred_out, pred)
  }
  
  estimates <- vbfit$results %>% mutate(year = list_yrs[i])
  if(i == 1) {
    estimates_out <- estimates
    rm(estimates) 
  } else {
    estimates_out <- rbind(estimates_out, estimates)
  }
  
  vb_out <- list(pred_out, estimates_out)
}

# Deviations by year for length-based vonB param estimates
ggplot() + 
  geom_segment(data = vb_out[[2]] %>% 
                 filter(Parameter != "sigma") %>% 
                 group_by(Parameter) %>% 
                 mutate(scaled_est = scale(Estimate),
                        mycol = ifelse(scaled_est < 0, "blue", "red")) %>% 
                 ungroup(),
               aes(x = year, y = 0,
                   xend = year, yend = scaled_est, 
                   color = mycol), size = 2) +
  scale_colour_grey() +
  geom_hline(yintercept = 0, lty = 2) + 
  guides(colour = FALSE) +
  labs(x = "", y = "Scaled parameter estimates\n") +
  facet_wrap(~ Parameter, ncol = 1) 

ggsave(paste0("figures/trends_lenvonbpars_1997_", YEAR, ".png"), 
       dpi=300, height=7, width=6, units="in")

# Fishery length-at-age ----

# subsets by length, age, sex
fsh_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & # *FLAG* advent of "modern" survey
           !is.na(length) &
           !is.na(age)) %>% 
  droplevels() -> fsh_laa_sub

fsh_laa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Female") -> fsh_laa_f

fsh_laa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Male") -> fsh_laa_m

#sex-specific starting values from Hanselman et al. 2007 (Appendix C, Table 1),
#except sigma
fsh_start_f <- c(l_inf = 80, k = 0.22, t0 = -1.9, sigma = 10) 
fsh_start_m <- c(l_inf = 68, k = 0.29, t0 = -2.3, sigma = 10)

# mle fit for females
vb_fsh_f <- vonb_len(obs_length = fsh_laa_f$length,
                     age = fsh_laa_f$age,
                     starting_vals = fsh_start_f,
                     sex = "Female")

# mle fit for males
vb_fsh_m <- vonb_len(obs_length = fsh_laa_m$length,
                     age = fsh_laa_m$age,
                     starting_vals = fsh_start_m,
                     sex = "Male")

# combine predictions and parameter estimates and plot mle
bind_rows(vb_fsh_f$predictions, vb_fsh_m$predictions) %>% 
  mutate(std_resid = scale(resid)) -> fsh_pred

fsh_laa_plot <- fsh_laa_sub %>% filter(age <= plus_group)
fsh_pred_pl <- fsh_pred %>% filter(pred <= plus_group)
axis <- tickr(fsh_laa_plot, age, 5)

ggplot(fsh_laa_plot, aes(age, length)) +
  geom_jitter(aes(col = Sex, shape = Sex), alpha=.2, shape = 20) +
  geom_line(data = fsh_pred, aes(y = pred, col = Sex, group = Sex), size = 1) + #"#00BFC4"
  # geom_line(data = pred, aes(y = pred, group = Sex), col = "darkgrey" ) + #"#00BFC4"
  # scale_colour_grey(start = 0, end = 0.5) +
  scale_linetype_manual(values = c(2,1)) +
  scale_x_continuous(limits = c(rec_age, plus_group), breaks = axis$breaks, labels = axis$labels) +
  xlab("Age (yrs)") +
  ylab("Fork length (cm)") + 
  expand_limits(y = 0) +
  theme(legend.position = c(0.9, 0.2)) 

ggsave(paste0("figures/length_vonb_chathamllfishery_2002_", YEAR, ".png"), dpi=300, height=4, width=6, units="in")

bind_rows(vb_mle_f$ypr_predictions, vb_mle_m$ypr_predictions) %>% mutate(Source = "LL survey") %>% 
  bind_rows(bind_rows(vb_fsh_f$ypr_predictions, vb_fsh_m$ypr_predictions) %>% mutate(Source = "LL fishery")) %>% 
  rename(fork_len = length) %>% 
  write_csv(paste0("output/pred_laa_plsgrp", plus_group, "_", YEAR, ".csv"))

# Weight-length allometry W = alpha * L ^ beta ----

# KVK assumed beta = 3, but there are data to estimate this value (e.g., Hanselman et
# al. 2007 values beta_f = 3.02 and beta_m = 2.96 are used in the current
# assessment

# subsets by weight, age, sex
srv_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & #advent of "modern" survey
           !is.na(length) &
           !is.na(weight)) %>% 
  droplevels() -> allom_sub

# length-weight relationship
lw_allometry <- function(length, a, b) {a * length ^ b}

START <- c(a = 1e-5, b = 3) #Starting values close to Hanselman et al. 2007

fem_fit <- nls(weight ~ lw_allometry(length = length, a, b), 
               data = filter(allom_sub, Sex == "Female"), start = START)

male_fit <- nls(weight ~ lw_allometry(length = length, a, b), 
               data = filter(allom_sub, Sex == "Male"), start = START)

all_fit <- nls(weight ~ lw_allometry(length = length, a, b), 
               data = allom_sub, start = START)

# parameter estimates and plot fit 
beta_m <- tidy(male_fit)$estimate[2]
beta_f <- tidy(fem_fit)$estimate[2]
beta_a <- tidy(all_fit)$estimate[2]

bind_rows(tidy(male_fit) %>% mutate(Sex = "Male"),
          tidy(fem_fit) %>% mutate(Sex = "Female")) %>% 
  bind_rows(tidy(all_fit) %>% mutate(Sex = "Combined")) %>% 
  dplyr::select(Parameter = term, Estimate = estimate, SE = std.error, Sex) %>% 
  mutate(Survey = "ADFG Longline",
         Years = paste0(min(laa_sub$year), "-", max(laa_sub$year)),
         Region = "Chatham Strait",
         Function = "Allometric - NLS") %>% 
  full_join(allom_sub %>% 
              group_by(Sex) %>% 
              summarise(n = n())) -> allom_pars

ggplot(allom_sub, aes(length, weight, col = Sex, shape = Sex)) +
  geom_jitter(alpha =0.8) + 
  stat_function(fun = lw_allometry, 
                args = as.list(tidy(fem_fit)$estimate),
                col = "#F8766D") + 
  stat_function(fun = lw_allometry, 
                args = as.list(tidy(male_fit)$estimate),
                col = "#00BFC4", lty = 2) + 
  labs(x = "Fork length (cm)", y = "Round weight (kg)", alpha = NULL) +
  # scale_colour_grey() +
  theme(legend.position = c(0.85, 0.2))

ggsave(paste0("figures/allometry_chathamllsurvey_1997_", YEAR, ".png"),
       dpi=300, height=4, width=6, units="in")
  
# Survey weight-at-age ----

# subsets by weight, age, sex
srv_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & # *FLAG* advent of "modern" survey
           !is.na(age) &
           !is.na(weight)) %>% 
  droplevels() -> waa_sub

waa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Female") -> waa_f

waa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Male") -> waa_m

# fit weight-based lvb with a multiplicative error structure using max likelihood estimation
# log(w_i) = log(w_inf) + beta * log(1 - exp * (-k * (age_i - t0))) + error

# starting values from Hanselman et al. 2007 Appendix C Table 5
start_f <- c(w_inf = 5.5, k = 0.24, t0 = -1.4, sigma = 10)
start_m <- c(w_inf = 3.2, k = 0.36, t0 = -1.1, sigma = 10)
start_a <- c(w_inf = 4.5, k = 0.30, t0 = -1.2, sigma = 10)

# mle fit for females
wvb_mle_f <- vonb_weight(obs_weight = waa_f$weight,
                   age = waa_f$age,
                   b = beta_f,
                   starting_vals = start_f,
                   sex = "Female")

# mle fit for males
wvb_mle_m <- vonb_weight(obs_weight = waa_m$weight,
                   age = waa_m$age,
                   b = beta_m, 
                   starting_vals = start_m,
                   sex = "Male")

# mle fit for all (for sex-combined asa model)
wvb_mle <- vonb_weight(obs_weight = waa_sub$weight,
                         age = waa_sub$age,
                         b = beta_a, 
                         starting_vals = start_a,
                         sex = "Combined")

# Past assessments: for the plus group take the mean of all samples >=
# plus group. Now just use the predicted mean asymptotic length.
srv_f_waa <- wvb_mle_f$ypr_predictions
# srv_f_waa[31, 2] <- mean(waa_f$weight[waa_f$age >= plus_group], na.rm = TRUE)

srv_m_waa <- wvb_mle_m$ypr_predictions
# srv_m_waa[31, 2] <- mean(waa_m$weight[waa_m$age >= plus_group], na.rm = TRUE)

srv_a_waa <- wvb_mle$ypr_predictions

rbind(srv_f_waa, srv_m_waa, srv_a_waa) %>% 
  mutate(Source = "LL survey") %>% 
  rename(round_kg = weight) -> srv_waa

# combine predictions and parameter estimates and plot fitted values
wvb_mle_f$predictions %>% 
  rbind(wvb_mle_m$predictions) %>% 
  mutate(std_resid = scale(resid)) -> pred

wvb_mle_f$results %>% 
  rbind(wvb_mle_m$results) %>% 
  mutate(Survey = "ADFG Longline",
         Years = paste0(min(waa_sub$year), "-", max(waa_sub$year)),
         Region = "Chatham Strait",
         Function = "Weight-based LVB") %>% 
  full_join(waa_sub %>% 
              group_by(Sex) %>% 
              summarise(n = n()), by = 'Sex') -> wvb_pars

ggplot() +
  geom_jitter(data = waa_sub, aes(x = age, y = weight, col = Sex, shape = Sex), shape = 20, alpha = 0.2) +
  geom_line(data = pred, aes(x = age, y = pred, col = Sex, group = Sex), size = 1) + #"#00BFC4"
  # scale_colour_grey(start = 0, end = 0.5) +
  scale_linetype_manual(values = c(2,1)) +
  scale_x_continuous(limits = c(2,plus_group),breaks = axis$breaks, labels = axis$labels) +
  ylim(c(0,10)) +
  xlab("Age (yrs)") +
  ylab("Round weight (kg)") + 
  expand_limits(y = 0) +
  theme(legend.position = c(0.2, 0.8))

ggsave(paste0("figures/weight_vonb_chathamllsurvey_1997_", YEAR, ".png"), 
       dpi=300, height=4, width=6, units="in")

# residual plots
pred %>% 
  ggplot(aes(std_resid)) + geom_histogram(bins=100) +
  facet_wrap(~Sex)

pred %>% # females don't look great, but this is already with a multiplicative error structure
  ggplot(aes(age, std_resid)) + 
  geom_point(alpha=.2) +
  geom_hline(yintercept=0, lty=4, alpha=.5) +
  facet_wrap(~Sex)

# Fishery weight-at-age ----

# Use same methods as survey and same starting values)

# subsets by weight, age, sex
fsh_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 2002 & # use same years as survey
           !is.na(age) &
           !is.na(weight)) %>% 
  droplevels() -> fsh_waa_sub

fsh_waa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Female") -> fsh_waa_f

fsh_waa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Male") -> fsh_waa_m

# mle fit for females
fsh_wvb_f <- vonb_weight(obs_weight = fsh_waa_f$weight,
                         age = fsh_waa_f$age,
                         b = beta_f,
                         starting_vals = start_f,
                         sex = "Female")

# mle fit for males
fsh_wvb_m <- vonb_weight(obs_weight = fsh_waa_m$weight,
                         age = fsh_waa_m$age,
                         b = beta_m, 
                         starting_vals = start_m,
                         sex = "Male")

# mle fit for sexes combined
fsh_wvb_a <- vonb_weight(obs_weight = fsh_waa_sub$weight,
                         age = fsh_waa_sub$age,
                         b = beta_a, 
                         starting_vals = start_a,
                         sex = "Combined")

# combine predictions and parameter estimates and plot fitted values
fsh_wvb_f$predictions %>% 
  rbind(fsh_wvb_m$predictions) %>% 
  mutate(std_resid = scale(resid)) -> pred

fsh_wvb_f$results %>% 
  rbind(fsh_wvb_m$results) %>% 
  mutate(Survey = "EQS longline fishery",
         Years = paste0(min(fsh_waa_sub$year), "-", max(fsh_waa_sub$year)),
         Region = "Chatham Strait",
         Function = "Weight-based LVB") %>% 
  full_join(fsh_waa_sub %>% 
              group_by(Sex) %>% 
              summarise(n = n()), by = 'Sex') -> fsh_wvb_pars

ggplot() +
  geom_jitter(data = fsh_waa_sub, aes(x = age, y = weight, col = Sex, shape = Sex), shape = 20, alpha = 0.2) +
  geom_line(data = pred, aes(x = age, y = pred, col = Sex, group = Sex), size = 1) + #"#00BFC4"
  # scale_colour_grey(start = 0, end = 0.5) +
  scale_linetype_manual(values = c(2,1)) +
  scale_x_continuous(limits = c(2,plus_group),breaks = axis$breaks, labels = axis$labels) +
  ylim(c(0,10)) +
  xlab("Age (yrs)") +
  ylab("Round weight (kg)") + 
  expand_limits(y = 0) +
  theme(legend.position = c(0.2, 0.8))

ggsave(paste0("figures/weight_vonb_chathamfishery_1997_", YEAR, ".png"), 
       dpi=300, height=4, width=6, units="in")

# Past assessments: for the plus group take the mean of all samples >=
# plus_group Now just use the predicted mean asymptotic length.
fsh_f_waa <- fsh_wvb_f$ypr_predictions
# fsh_f_waa[31, 2] <- mean(fsh_waa_f$weight[fsh_waa_f$age >= plus_group], na.rm = TRUE)
fsh_m_waa <- fsh_wvb_m$ypr_predictions
# fsh_m_waa[31, 2] <- mean(fsh_waa_m$weight[fsh_waa_m$age >= plus_greoup], na.rm = TRUE)
fsh_a_waa <- fsh_wvb_a$ypr_predictions

rbind(fsh_f_waa, fsh_m_waa, fsh_a_waa) %>% 
  mutate(Source = "LL fishery") %>% 
  rename(round_kg = weight) %>% 
  rbind(srv_waa) %>% 
  mutate(round_kg = round(round_kg, 4)) %>% 
  select(Source, Sex, age, round_kg) -> pred_waa 
write_csv(pred_waa, paste0("output/pred_waa_plsgrp", plus_group, "_", YEAR, ".csv"))

ggplot(data = pred_waa, aes(x = age, y = round_kg * 2.20462 * 0.63, colour = Sex, 
                            linetype = Source)) +
  # geom_point() +
  geom_line(size = 1) + 
  scale_colour_manual(values = c("#F8766D", "#00BFC4", "#7CAE00")) +
  xlab("Age (yrs)") +
  ylab("Dressed weight (lb)") +
  expand_limits(y = 0) +
  scale_x_continuous(limits = c(2,plus_group),breaks = axis$breaks, labels = axis$labels)
  
ggsave(paste0("figures/waa_dressedlb_1997_", YEAR, ".png"), 
       dpi=300, height=4, width=6, units="in")

# Compare empirical and predicted weight-at-age
ggplot() +
  geom_point(data = emp_waa, 
       aes(x = age, y = weight, col = Source, shape = Sex)) +
  geom_line(data = pred_waa,
            aes(x = age, y = round_kg, col = Source, linetype = Sex), size = 1) +
  scale_colour_grey() +
  expand_limits(y = 0) +
  labs(x = "\nAge", y = "Weight (kg)\n", linetype = "Sex", shape = "Sex")

ggsave(paste0("figures/compare_empirical_predicted_waa_", YEAR, ".png"), 
              dpi=300, height=4, width=6, units="in")

# Compare growth results ----

# Comparison of Hanselman et al. 2007 values with the Chatham Strait longline
# survey. Units: length (cm), weight (kg), and age (yrs)

bind_rows(allom_pars, lvb_pars, wvb_pars, fsh_wvb_pars) %>% 
      mutate(Source = "seak_sablefish/code/biological.r") %>% 
  bind_rows(noaa_lvb) %>% 
  write_csv(., "output/compare_vonb_adfg_noaa.csv")

# Maturity ----

# Maturity could use a more rigorous analysis. Based on recommendations from Ben
# Williams, the maturity used for stock assessment is a length-based maturity
# curve (fit to 1997-current longline survey data) that is then translated to
# age using survey length-at-age predictions from vonB. There is also code here
# to compare this with yearly fits to length and also fits to age.

# 0 = immature, 1 = mature. Only conducted for females (because biological
# reference points / harvest strategy are based on female spawning biomass)

# subsets by length
srv_bio %>% 
  ungroup() %>% 
  filter(Sex == "Female" &
           year >= 1997 & # *FLAG* advent of "modern" survey
           !is.na(Mature) &
           !is.na(length)) %>% 
  droplevels() -> len_f

# Sample sizes by year
with(len_f, table(year))

# base model
fit_length <- glm(Mature ~ length, data = len_f, family = binomial)
len <- seq(0, 120, 0.05)
(L50 <- round(- coef(fit_length)[1]/coef(fit_length)[2],1))
(kmat <- round(((coef(fit_length)[1] + coef(fit_length)[2]*len) / (len - L50))[1], 2))

# by year, for comparison
fit_length_year <- glm(Mature ~ length * Year, data = len_f, family = binomial)

AIC(fit_length, fit_length_year)

# fit_length_year <- glm(Mature ~ length * Year, data = len_f, family = quasibinomial)

# New df for prediction for fit_length
new_len_f_simple <- data.frame(length = seq(0, 120, 0.05))

# New df for prediction for fit_length_year
new_len_f <- data.frame(length = rep(seq(0, 120, 0.05), n_distinct(len_f$year)),
                        Year = factor(sort(rep(unique(len_f$year), length(seq(0, 120, 0.05))), decreasing = FALSE)))

# Get predicted values for fit_length       
broom::augment(x = fit_length, 
               newdata = new_len_f_simple, 
               type.predict = "response") %>% 
  select(length, fitted = .fitted, se =.se.fit) -> pred_simple

# Get predicted values by year for fit_length_year          
broom::augment(x = fit_length_year, 
               newdata = new_len_f, 
               type.predict = "response") %>% 
  select(Year, length, fitted = .fitted, se =.se.fit) -> pred

#Length-based maturity curves - 2019 does appear to have early age-at-maturation
#relative to other years
ggplot() +
  geom_line(data = pred, 
            aes(x = length, y = fitted, group = Year, colour = as.numeric(as.character(Year)))) +
  geom_line(data = pred_simple, aes(x = length, y = fitted, lty = "All years combined"),
            colour = "black", size = 1) +
  lims(x = c(40, 85)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  scale_linetype_manual(values = 2) +
  labs(x = "\nLength (cm)", y = "Probability\n", colour = "Year", lty = NULL) +
  theme(legend.position = c(.8, .4))

ggsave(paste0("figures/maturity_atlength_byyear_srvfem.png"), 
       dpi=300, height=4, width=6, units="in")

# Parameter estimates by year
tidy(coef(summary(fit_length_year))) %>% 
  select(param = `.rownames`, 
         est = Estimate) -> mature_results

# Note on glm() output b/c I can never remember
# (Intercept) = intercept for Year1997 (or first level of factor) on logit scale
# length = slope for Year1997 on logit scale
# Year1998 = difference in intercept (Year1998 - Year1997)
# length:Year1998 = difference in slope (lengthYear1998 - Year1997)
bind_rows(
  # filter out intercepts, derive estimates by yr  
  mature_results %>% 
    filter(param == "(Intercept)") %>% 
    mutate(param = "Year1997",
           Parameter = "b_0"),
  mature_results %>% 
    filter(!param %in% c("(Intercept)", "length") &
             !grepl(':+', param)) %>%     # filter(!grepl('length:Year\\d{4}', param)) %>% #alternative regex
    mutate(est = est + mature_results$est[mature_results$param == "(Intercept)"],
           Parameter = "b_0"),
  # filter out slopes (contain >1 :), derive estimates by year
  mature_results %>% 
    filter(param == "length") %>% 
    mutate(param = "length:Year1997",
           Parameter = "b_1"),
  mature_results %>% 
    filter(grepl(':+', param)) %>% 
    mutate(est = est + mature_results$est[mature_results$param == "length"],
           Parameter = "b_1")) %>% 
  group_by(Parameter) %>% 
  mutate(scaled_est = scale(est)) %>% 
  ungroup() %>% 
  mutate(year = rep(1997:max(len_f$year), 2)) -> mature_results

# Deviations by year for length-based maturity curve param estimates
ggplot() + 
  geom_segment(data = mature_results %>% 
                 mutate(mycol = ifelse(scaled_est < 0, "blue", "red")),
               aes(x = year, y = 0,
                   xend = year, yend = scaled_est, 
                   color = mycol), size = 2) +
  geom_hline(yintercept = 0, lty = 2) + 
  guides(colour = FALSE) +
  labs(x = "", y = "Scaled parameter estimates") +
  facet_wrap(~ Parameter, ncol = 1)

# Next convert predictions to age using von B preditions from ll survey
# length-at-age data
age_pred <- seq(0, plus_group, by = 0.01)
vb_pars <- vb_mle_f$results
age_pred <- data.frame(age = age_pred,
                       length = round(vb_pars$Estimate[1] * (1 - exp(- vb_pars$Estimate[2] * (age_pred - vb_pars$Estimate[3]))), 1))

# Match lengths back to lengths predicted by vonb for fit_length
pred_simple <- merge(pred_simple, age_pred, by = "length")
which(is.na(pred_simple)) # should be integer(0)

# Match lengths back to lengths predicted by vonb for fit_length_year
pred <- merge(pred, age_pred, by = "length") 
which(is.na(pred)) # should be integer(0)

# Get length at 50% maturity (L_50 = -b_0/b_1) and get a_50 from age_pred
# (predicted from vonB)
merge(mature_results %>% 
        select(-scaled_est, -param) %>% 
        # dcast(year ~ Parameter, value.var = "est") %>%
        pivot_wider(names_from = Parameter, values_from = est) %>%
        mutate(length = - round(b_0 / b_1, 1)), #length at 50% maturity)
      age_pred, by = "length") %>% 
  arrange(year) %>% 
  select(year, l_50 = length, a_50 = age) %>% 
  group_by(year) %>% 
  mutate(a_50 = round(mean(a_50), 1)) %>%
  distinct() %>% ungroup() %>% 
  mutate(mu_a_50 = mean(a_50),
         mu_l_50 = mean(l_50)) -> mat_50_year

# trends in L50 and a50 by year
ggplot(mat_50_year, aes(x = year, y = l_50)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = mu_l_50), lty = 2)

ggplot(mat_50_year, aes(x = year, y = a_50)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = mu_a_50), lty = 2)

merge(pred %>% mutate(year = Year), mat_50_year, by = "year") -> pred

# Age-based maturity curves estimated from length-based maturity and vonB growth
# curve (light blue lines are annual mean preditions, dark blue is the mean)
ggplot() +
  geom_line(data = pred, aes(x = age, y = fitted, group = Year, colour = as.numeric(as.character(Year)))) +  
  geom_line(data = pred_simple, aes(x = age, y = fitted, lty = "All years combined"),
            colour = "black", size = 1) +
  scale_colour_gradientn(colours=rainbow(4)) +
  lims(x = c(0, 20)) +
  scale_linetype_manual(values = 2) +
  labs(x = "\nAge (yr)", y = "Probability\n", colour = "Year", lty = NULL) +
  theme(legend.position = c(.8, .4)) 

ggsave(paste0("figures/maturity_atage_byyear_srvfem.png"), 
       dpi=300, height=4, width=6, units="in")

# Comparison with age-based maturity curve
fit_age_year <- glm(Mature ~ age * Year, data = len_f, family = binomial)

# New df for prediction
new_f <- data.frame(age = seq(0, 30, by = 0.01), n_distinct(laa_f$year),
                    Year = factor(sort(rep(unique(laa_f$year), 
                                           length(seq(0, 30, by = 0.01))), 
                                       decreasing = FALSE)))

# Get predicted values by year and take the mean           
broom::augment(x = fit_age_year, 
               newdata = new_f, 
               type.predict = "response") %>% 
  select(Year, age, fitted = .fitted, se =.se.fit) %>% 
  group_by(age) %>% 
  # Just use mean for illustrative purposes
  mutate(Probability = mean(fitted)) -> pred_age

# Comparison of maturity at age curves. Blue is derived from length-based
# maturity cuve, red is estimated directly from age. Light lines are annual mean
# predictions, dark lines are the means.
ggplot() +
  geom_line(data = pred, aes(x = age, y = fitted, group = Year), col = "lightblue") +  
  geom_line(data = pred_simple, aes(x = age, y = fitted),
            colour = "blue", size = 2, alpha = 0.5) +
  lims(x = c(0, 20)) +
  # scale_linetype_manual(values = 2) +
  labs(x = "\nAge (yr)", y = "Probability\n") +
  theme(legend.position = c(.8, .4)) +
  geom_line(data = pred_age,
            aes(x = age, y = fitted, group = Year), 
            colour = "lightpink", alpha = 0.5) +
  geom_line(data = pred_age,
            aes(x = age, y = Probability), 
            colour = "red", size = 2, alpha = 0.5) +
  labs(x = "Age", y = "Probability")

# Length-based (translated to age) is more realistic than age-based. Also there
# is no clear reason to choose the more complicated model (fit_length_year) over
# the simpler model (fit_length)

# Maturity at age for YPR and SCAA models
pred_simple %>%  
  filter(age %in% c(rec_age:plus_group)) %>%
  right_join(data.frame(age = rec_age:plus_group)) %>%
  # interpolate fitted probability to fill in any missing values - feel free to
  # revisit rounding if so desired
  mutate(Sex = "Female",
         Source = "LL survey",
         probability = round(zoo::na.approx(fitted, maxgap = 20, rule = 2), 2)) %>% 
  select(age, probability) %>% 
  write_csv(paste0("output/fem_maturityatage_llsrv_plsgrp", plus_group, "_", YEAR, ".csv"))

#Derive age at 50% maturity and kmat (slope of logistic curve)
b0 <- fit_length$coefficients[1]
b1 <- fit_length$coefficients[2]
(L50 <- round(-b0/b1, 1))
(a50 <- age_pred %>% 
  right_join(data.frame(length = L50)) %>% 
  group_by(length) %>% 
  dplyr::summarise(a50 = round(mean(age), 1)) %>% 
    pull(a50))
(kmat <- round(((coef(fit_length)[1] + coef(fit_length)[2]*len) / (len - L50))[1], 2))

data.frame(year_updated = YEAR,
           L50 = L50,
           kmat = kmat,
           a50 = a50) %>% 
  write_csv(paste0("output/maturity_param_", YEAR))

# # Equation text for plotting values of a_50 and kmat
# a50_txt <- as.character(
#   as.expression(substitute(
#     paste(italic(a[50]), " = ", xx),
#     list(xx = formatC(a50, format = "f", digits = 1)))))
# 
# kmat_txt <- as.character(
#   as.expression(substitute(
#     paste(italic(k[mat]), " = ", xx),
#     list(xx = formatC(kmat, format = "f", digits = 1)))))

# Sex ratios ----

# proportion of females by age in survey and fishery

# restrict age range
aa <- c(2:plus_group)

# see helper for f_sex_ratio() documentation - couldn't get this to run 2018-03-01.
# f_sex_ratio(data = filter(srv_bio, age %in% aa),
#               src = "LL survey", age) %>%
#   bind_rows(f_sex_ratio(data = filter(fsh_bio, age %in% aa),
#               src = "LL fishery", age)) -> byage

# Manual until I can get f_sex_ratio() running again:
srv_bio %>% 
  filter(age %in% aa) %>% 
  ungroup() %>% 
  select(Sex, age) %>% 
  filter(Sex %in% c("Female", "Male")) %>% 
  na.omit() %>% 
  droplevels() %>% 
  count(Sex, age) %>% 
  group_by(age) %>% 
  mutate(proportion = round(n / sum(n), 2),
         Source = "LL survey") %>% 
  filter(Sex == "Female") %>% 
  bind_rows(fsh_bio %>% 
              filter(age %in% aa) %>% 
              ungroup() %>% 
              select(Sex, age) %>% 
              filter(Sex %in% c("Female", "Male")) %>% 
              na.omit() %>% 
              droplevels() %>% 
              count(Sex, age) %>% 
              group_by(age) %>% 
              mutate(proportion = round(n / sum(n), 2),
                     Source = "LL fishery") %>% 
              filter(Sex == "Female")) -> byage

# get generalized additive model fits and predictions
# survey
srv_fitage <- gam(I(Sex == "Female") ~ s(age), 
                  data = filter(srv_bio, age %in% aa, 
                                Sex %in% c("Female", "Male")),
               family = "quasibinomial")

srv_predage <- predict(srv_fitage, newdata = data.frame(age = aa),
                       type = "response", se = TRUE)

# fishery
fsh_fitage <- gam(I(Sex == "Female") ~ s(age), 
               data = filter(fsh_bio, age %in% aa,
                             Sex %in% c("Female", "Male")),
               family = "quasibinomial")

fsh_predage <- predict(fsh_fitage, newdata = data.frame(age = aa),
                       type = "response", se = TRUE)

# combine with the sex_ratio df
# *FLAG* bind_cols goes by col position, make sure survey is first

bind_cols(
  byage,
  #do.call cbinds each vector in the predict() output list 
  bind_rows(tbl_df(do.call(cbind, srv_predage)) %>% 
              mutate(source_check = "LL survey"),
            tbl_df(do.call(cbind, fsh_predage)) %>% 
              mutate(source_check = "LL fishery") ) 
  ) -> byage

# plot
axis <- tickr(byage, age, 10)

ggplot(byage, aes(x = age)) +
  geom_line(aes(y = fit, col = Source)) +
  geom_ribbon(aes(ymin = fit - se.fit*2, ymax = fit + se.fit*2, fill = Source),  alpha = 0.2) +
  geom_point(aes(y = proportion, col = Source)) +  
  expand_limits(y = c(0.0, 1)) +
  xlab("\nAge") +
  ylab("Proportion of females\n") +
  geom_hline(yintercept = 0.5, lty = 2, col = "grey") +
  scale_colour_manual(values = c("black", "grey")) +
  scale_fill_manual(values = c("black", "grey")) +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  theme(legend.position = c(0.8, 0.8)) -> byage_plot

# ggsave(paste0("figures/proportion_fembyage_", YEAR, ".png"), dpi=300, 
#        height=4, width=7,  units="in")

# proportion of females by year in the fishery and survey

# f_sex_ratio(data = filter(srv_bio), src = "LL survey", year) %>% 
#   bind_rows(f_sex_ratio(data = filter(fsh_bio), 
#               src = "LL fishery", year)) -> byyear

# Manual until I can get f_sex_ratio() running again:
srv_bio %>% 
  filter(age %in% aa) %>% 
  ungroup() %>% 
  select(Sex, year) %>% 
  filter(Sex %in% c("Female", "Male")) %>% 
  na.omit() %>% 
  droplevels() %>% 
  count(Sex, year) %>% 
  group_by(year) %>% 
  mutate(proportion = round(n / sum(n), 2),
         Source = "LL survey") %>% 
  filter(Sex == "Female") %>% 
  bind_rows(fsh_bio %>% 
              filter(age %in% aa) %>% 
              ungroup() %>% 
              select(Sex, year) %>% 
              filter(Sex %in% c("Female", "Male")) %>% 
              na.omit() %>% 
              droplevels() %>% 
              count(Sex, year) %>% 
              group_by(year) %>% 
              mutate(proportion = round(n / sum(n), 2),
                     Source = "LL fishery") %>% 
              filter(Sex == "Female")) -> byyear

# Save output for YPR analysis
write_csv(byyear, paste0("output/sexratio_byyear_plsgrp", plus_group, "_", YEAR, ".csv"))

# get generalized additive model fits and predictions
# survey
srv_fityear <- gam(I(Sex == "Female") ~ s(year, k = 4), 
                  data = filter(srv_bio, Sex %in% c("Female", "Male")),
                  family = "quasibinomial")

srv_yrs <- byyear %>% filter(Source == "LL survey") %>% select(year) %>% range()

srv_predyear <- predict(srv_fityear, 
                        newdata = data.frame(year = min(srv_yrs):max(srv_yrs) ),
                       type = "response", se = TRUE)

# fishery
fsh_fityear <- gam(I(Sex == "Female") ~ s(year, k = 4), 
                   data = filter(fsh_bio, Sex %in% c("Female", "Male")),
                   family = "quasibinomial")


fsh_yrs <- byyear %>% filter(Source == "LL fishery") %>% select(year) %>% range()

fsh_predyear <- predict(fsh_fityear, 
                        newdata = data.frame(year = min(fsh_yrs):max(fsh_yrs) ),
                        type = "response", se = TRUE)

# combine with the sex_ratio df
# *FLAG* bind_cols goes by col position, make sure survey is first
bind_cols(
  byyear,
  #do.call cbinds each vector in the predict() output list 
  bind_rows(tbl_df(do.call(cbind, srv_predyear))%>% mutate(source_check = "LL survey"),
            tbl_df(do.call(cbind, fsh_predyear))%>% mutate(source_check = "LL fishery") ) 
) -> byyear

# plots

axis <- tickr(byyear, year, 5)

ggplot(data = byyear, aes(x = year)) +
  geom_line(aes(y = fit, col = Source), size = 1) +
  geom_ribbon(aes(ymin = fit - se.fit*2, ymax = fit + se.fit*2, 
                  fill = Source),  alpha = 0.2) +
  geom_point(aes(y = proportion, col = Source)) +  
  expand_limits(y = c(0.0, 1)) +
  geom_hline(yintercept = 0.5, lty = 2, col = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  xlab("\nYear") +
  ylab("Proportion of females\n") +
  scale_colour_manual(values = c("black", "grey")) +
  scale_fill_manual(values = c("black", "grey")) +
  theme(legend.position = "none") -> byyear_plot
# ggsave(paste0("figures/proportion_fembyyear_", YEAR, ".png"), dpi=300,  height=4, width=7, units="in")

plot_grid(byage_plot, byyear_plot, align = c("h"), ncol = 1)
ggsave(paste0("figures/sex_ratios_", YEAR, ".png"), dpi=300,  height=6, width=7, units="in")

# proportion of females by year and age in survey and fishery

# f_sex_ratio(data = filter(srv_bio, age %in% aa), 
#               src = "LL survey", year, age) %>% 
#   bind_rows(f_sex_ratio(data = filter(fsh_bio, age %in% aa), 
#               src = "LL fishery", year, age)) -> byyrage

# Age compositions ----

# Combine survey and fishery data for age comp analysis

#quos() uses stand eval in dplyr, eval cols with nonstand eval using !!!
cols <- quos(Source, year, Sex, age) 

bind_rows(fsh_bio %>% mutate(Source = "LL fishery") %>% select(!!!cols), 
          srv_bio %>% mutate(Source = "LL survey") %>% select(!!!cols)) %>% 
  bind_rows(potsrv_bio %>% mutate(Source = "Pot survey") %>% select(!!!cols)) %>% 
  filter(Sex %in% c('Female', 'Male') & !is.na(age)) %>% 
  droplevels() %>% 
  mutate(age = ifelse(age >= plus_group, plus_group, age)) %>% 
  filter(age >= 2) -> all_bio  # Plus group

# Sensitivity for 2019 YPR model on age-2s
all_bio %>% 
  filter(age < 10) %>% 
  group_by(year, age) %>% 
  dplyr::summarise(n = n()) %>% 
  pivot_wider(names_from = age, values_from = n, values_fill = list(n = 0)) %>% 
  print(n = Inf)
# a lot of years don't have any age-2s
# used for YPR analysis in 2019 to examine the impact of leaving out age-2s
# (must uncomment following line to rerun analysis):
# all_bio <- all_bio %>% filter(!c(year == YEAR & age == 2)) 

# Age comps (sex-specific)
all_bio %>% 
  count(Source, Sex, year, age) %>%
  group_by(Source, Sex, year) %>% 
  mutate(proportion = round( n / sum(n), 5)) %>% 
  bind_rows(all_bio %>% # Age comps (sexes combined)
          count(Source, year, age) %>%
          group_by(Source, year) %>% 
          mutate(proportion = round( n / sum(n), 5),
                 Sex = "Sex combined")) -> agecomps  

# Years with pot bio data
# potsrv_bio %>% 
#   filter(!is.na(age) & !is.na(Sex)) %>% 
#   distinct(year) -> pot_yrs

# complete() was behaving weirdly. Expand to grid to include all age combos
expand.grid(year = unique(agecomps$year), 
            Source = unique(agecomps$Source),
            Sex = unique(agecomps$Sex),
            age = seq(2, plus_group, 1))  %>% 
  data.frame()  %>% 
  full_join(agecomps) %>%
  fill_by_value(n, proportion, value = 0) %>% 
  mutate(Age = factor(age),
         proportion = round(proportion, 5)) %>%
  # Keep only relevant years for each Source
  filter(c(Source == "LL fishery" & year >= 2002) |
           c(Source == "LL survey" & year >= 1997)) -> agecomps

# Check that they sum to 1
agecomps %>% 
  group_by(Source, Sex, year) %>% 
  summarise(sum(proportion)) %>% 
  print(n = Inf)

# Sample sizes by source/year/sex
agecomps %>% 
  group_by(Source, year, Sex) %>% 
  dplyr::summarize(n = sum(n)) %>% 
  arrange(year) %>% 
  pivot_wider(names_from = year, values_from = n, values_fill = list(n = 0)) %>% 
  write_csv(paste0("output/n_agecomps_plsgrp", plus_group, "_", YEAR, ".csv"))

# Age comp matrix
agecomps %>% write_csv(paste0("output/agecomps_plsgrp", plus_group, "_", YEAR, ".csv"))

# Bargraph for presentation
agecomps %>% 
  filter(year == YEAR & Source == "LL fishery" &
           Sex %in% c("Male", "Female")) %>% 
  ggplot(aes(age, proportion, fill = Sex)) +
  geom_bar(stat = "identity",
           position = "dodge") +
           # position = position_dodge(preserve = "single")) +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_x_continuous(breaks = seq(min(agecomps$age), max(agecomps$age), 4), 
                     labels =  seq(min(agecomps$age), max(agecomps$age), 4)) +
  labs(x = "\nAge", y = "Proportion\n") +
  theme(legend.position = c(0.9, 0.7))

ggsave(paste0("figures/agecomp_bargraph_", YEAR, ".png"), 
              dpi=300, height=3, width=9, units="in")

# All years smoothed by source
agecomps %>% 
  filter(age < plus_group & Sex == "Sex combined") %>% 
ggplot(aes(x = age, y = proportion, colour = Source, linetype = Source)) +
  geom_point(size = 1, alpha = 0.1) +
  stat_smooth(size = 1, se = FALSE) +
  scale_colour_grey() +
  # scale_colour_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb")) +
  scale_y_continuous(limits = c(0, 0.1),
                     breaks = round(seq(min(agecomps$proportion), 0.1, 0.02), 2), 
                     labels =  round(seq(min(agecomps$proportion), 0.1, 0.02), 2)) +
  xlab('\nAge') +
  ylab('Proportion\n') +
  theme(legend.position = c(0.8, 0.8))

ggsave("figures/agecomp_bydatasource.png", 
       dpi=300, height=5, width=5, units="in")

# bubble plots filled circles

# survey
agecompdat <- agecomps %>% 
  filter(Sex %in% c("Female", "Male") &
           Source %in% c("LL survey") &
           year >= 1997 &
           age <= plus_group) %>% 
  ungroup()

axisx <- tickr(agecompdat, year, 3)
axisy <- tickr(agecompdat, age, 5)

ggplot(data = agecompdat,
       aes(x = year, y = age, size = proportion)) + #*FLAG* could swap size with proportion_scaled
  geom_point(shape = 21, fill = "black", colour = "black") +
  scale_size(range = c(0, 4)) +
  facet_wrap(~ Sex) +
  labs(x = "\nYear", y = "Observed age\n") +
  guides(size = FALSE) +
  scale_x_continuous(breaks = axisx$breaks, labels = axisx$labels) +
  scale_y_continuous(breaks = axisy$breaks, labels = axisy$labels)

ggsave("figures/bubble_survey_agecomp_byyear.png", dpi=300, height=5, width=7.5, units="in")

# fishery
agecompdat <- agecomps %>% 
  filter(Sex %in% c("Female", "Male") &
           Source %in% c("LL fishery") &
           year >= 2002 &
           age <= plus_group) %>% 
  ungroup()

axisx <- tickr(agecompdat, year, 3)
axisy <- tickr(agecompdat, age, 5)

ggplot(data = agecompdat,
       aes(x = year, y = age, size = proportion)) + #*FLAG* could swap size with proportion_scaled
  geom_point(shape = 21, colour = "black", fill = "black") +
  scale_size(range = c(0, 4)) +
  facet_wrap(~ Sex) +
  labs(x = "\nYear", y = "Observed age\n") +
  guides(size = FALSE) +
  scale_x_continuous(breaks = axisx$breaks, labels = axisx$labels) +
  scale_y_continuous(breaks = axisy$breaks, labels = axisy$labels)

ggsave("figures/bubble_fishery_agecomp_byyear.png", 
       dpi=300, height=5, width=7.5, units="in")

# Length compositions ----

# Pers. comm. K. Fenske 2018-01-05: NMFS uses length bins 41, 43, 45 ... 99.
# These bins represent the center of the bin, so a 43 bin represents fish
# 42-43.9 cm. They omit fish smaller than 40 and fish larger than 100 cm are
# lumped into the 100 bin. I've maintained these conventions for easy
# comparison:
bind_rows(srv_bio %>% 
            filter(year >= 1997 &
                     Sex %in% c("Female", "Male") &
                     !is.na(length)) %>% 
            select(year, Sex, length) %>% 
            mutate(Source = "LL survey"),
          fsh_bio %>% 
            filter(year >= 2002 &
                     Sex %in% c("Female", "Male") &
                     !is.na(length)) %>% 
            select(year, Sex, length) %>% 
            mutate(Source = "LL fishery")#,
          # potsrv_bio %>% 
          #   filter(Sex %in% c("Female", "Male") &
          #            !is.na(length)) %>% 
          #   select(year, Sex, length) %>% 
          #   mutate(Source = "Pot survey")
          ) %>% 
  filter(!c(length < 40)) %>% 
  mutate(length2 = ifelse(length < 41, 41,
                          ifelse(length > 99, 99, length)),
         length_bin = cut(length2, breaks = seq(39.9, 99.9, 2),
                          labels = paste(seq(41, 99, 2)))) %>% 
  select(-length2) -> lendat

lendat %>% 
  # Length comps by Source, year, and Sex 
  count(Source, Sex, year, length_bin) %>%
  group_by(Source, Sex, year) %>% 
  mutate(proportion = round( n / sum(n), 4)) %>% 
  bind_rows(lendat %>% # Sexes combined
              count(Source, year, length_bin) %>%
              group_by(Source, year) %>% 
              mutate(proportion = round( n / sum(n), 4),
                     Sex = "Sex combined")) -> lencomps

# complete() was behaving weirdly. Expand to grid to include all length combos
expand.grid(year = unique(lencomps$year), 
            Source = unique(lencomps$Source),
            Sex = unique(lencomps$Sex),
            length_bin = sort(unique(lendat$length_bin)))  %>% 
  data.frame()  %>% 
  full_join(lencomps) %>%
  fill_by_value(n, proportion, value = 0) %>% 
  mutate(#length_bin = factor(length_bin),
         proportion = round(proportion, 4)) %>%
  # Keep only relevant years for each Source
  filter(c(Source == "LL fishery" & year >= 2002) |
           c(Source == "LL survey" & year >= 1997) #|
           # c(Source == "Pot survey" & year %in% pot_yrs$year)
         ) -> lencomps

# Check that they sum to 1
lencomps %>% 
  group_by(Source, Sex, year) %>% 
  summarise(sum(proportion)) #%>% View()

write_csv(lencomps, paste0("output/lengthcomps_", YEAR, ".csv"))

lendat %>% 
  # Mean length comp for comparison
  count(Source, Sex, length_bin) %>%
  group_by(Source, Sex) %>% 
  mutate(proportion = round( n / sum(n), 4)) %>% 
  arrange(Source, Sex, length_bin) %>% 
  # Fill in the blanks with 0's
  complete(Source, length_bin,
           fill = list(n = 0, proportion = 0)) %>% 
  bind_rows(lendat %>% # Sexes combined
              count(Source, length_bin) %>%
              group_by(Source) %>% 
              mutate(proportion = round( n / sum(n), 4),
                     Sex = "Sex combined")) -> mu_lencomps

mu_lencomps %>% 
  group_by(Source, Sex) %>% 
  summarise(sum(proportion))

s_lencomps <- lencomps %>% 
  filter(Source == "LL survey")

f_lencomps <- lencomps %>% 
  filter(Source == "LL fishery")

# ggridge plots

lendat %>% 
  filter(Source != "Pot survey") %>% 
  mutate(Source = derivedFactor("Survey" = Source == "LL survey",
                                "Fishery" = Source == "LL fishery",
                                .ordered = TRUE)) %>% 
  ggplot(aes(length, year, group = year, fill = year)) + 
  geom_density_ridges(aes(point_fill = year, point_color = year),
                      alpha = 0.3) +
  geom_vline(xintercept = 63, linetype = 4) +
  xlim(40, 90) + 
  xlab("\nLength (cm)") + 
  ylab(NULL) +
  scale_y_reverse() +
  theme(legend.position = "none") + 
  facet_wrap(~ Source)

ggsave(paste0("figures/lengthcomp_ggridges_", YEAR, ".png"), 
       dpi=300, height=8, width=10, units="in")

# ggride plot for len dat by sex (for TMB inputs)

lendat %>% 
  filter(! c(Source %in% c("Pot survey", "LL fishery"))) %>% 
  mutate(Source = derivedFactor("Survey" = Source == "LL survey")) %>% 
  ggplot(aes(length, year, group = year, fill = year)) + 
  geom_density_ridges(aes(point_fill = year, point_color = year), alpha = 0.3) +
  #geom_vline(xintercept = 61, linetype = 4) + # L50
  xlim(40, 90) + 
  labs(x = "\nLength (cm)", y = "Year\n") +
  scale_y_reverse() +
  theme(legend.position = "none") + 
  facet_wrap(~ Sex) +
  ggtitle("Survey")

ggsave(paste0("figures/tmb/lencomp_srv_",YEAR,".png"), 
       dpi=300, height=8, width=10, units="in")

# fishery
lendat %>% 
  filter(! c(Source %in% c("Pot survey", "LL survey"))) %>% 
  mutate(Source = derivedFactor("Fishery" = Source == "LL fishery")) %>% 
  ggplot(aes(length, year, group = year, fill = year)) + 
  geom_density_ridges(aes(point_fill = year, point_color = year), alpha = 0.3) +
  # geom_vline(xintercept = 61, linetype = 4) + # L50
  xlim(40, 90) + 
  labs(x = "\nLength (cm)", y = "Year\n") +
  scale_y_reverse() +
  theme(legend.position = "none") + 
  facet_wrap(~ Sex) +
  ggtitle("Fishery")

ggsave(paste0("figures/tmb/lencomp_fsh_",YEAR,".png"), 
       dpi=300, height=8, width=10, units="in")

# All years smoothed by source
ggplot() +
  geom_point(data = lencomps %>% 
               filter(Sex == "Sex combined"),
             aes(x = length_bin, y = proportion, 
                 colour = Source),
             size = 1, alpha = 0.2) +
  # stat_smooth(size = 1.5, se = FALSE) +
  geom_line(data = mu_lencomps %>% 
              filter(Sex == "Sex combined"),
            aes(x = length_bin, y = proportion, colour = Source, 
                group = Source, linetype = Source), size = 1) +
  scale_x_discrete(breaks = seq(41, 99, 6),
                   labels = seq(41, 99, 6)) +
  scale_colour_grey() +
  xlab('\nFork length (cm)') +
  ylab('Proportion\n') +
  theme(legend.position = c(0.8, 0.8))

ggsave("figures/lengthcomp_bydatasource.png", 
       dpi=300, height=4.5, width=5, units="in")

# New length comp figs, requested by AJ Lindley 2018-09-07
lencomps %>% 
  group_by(year, Source, Sex) %>% 
  dplyr::summarize(N = sum(n),
         label = paste0("n = ", prettyNum(N, big.mark = ","))) %>% 
  ungroup() %>% 
  mutate(length_bin = "91", proportion = 0.18) -> labels 

# For survey
ggplot(data = lencomps %>% 
             # Last 10 years of data
             filter(year >= YEAR - 10 & 
                      Sex != "Sex combined" &
                      Source == "LL survey"), 
           aes(x = length_bin, y = proportion)) + 
  geom_bar(stat = "identity", colour = "lightgrey", fill = "lightgrey", width = 0.8) +
  geom_line(data = lencomps %>% 
              # Compare all past years to this year
              filter(year == YEAR & 
                       Sex != "Sex combined" &
                       Source == "LL survey") %>% 
              select(-year),
            aes(x = length_bin, y = proportion, group = 1),
            colour = "black") +
  geom_text(data = labels %>% 
              filter(year >= YEAR - 10 & 
                       Sex != "Sex combined" &
                       Source == "LL survey"),
            aes(x = length_bin, y = proportion, label = label),
            size = 3, family = "Times") +
  scale_y_continuous(limits = c(0, 0.25),
                     breaks = round(seq(0, 0.2, 0.1), 2),
                     labels =  round(seq(0, 0.2, 0.1), 2)) +
  scale_x_discrete(breaks = seq(41, 99, 6),
                   labels = seq(41, 99, 6)) +
  facet_grid(year ~ Sex) +
  labs(x = "\nFork length (cm)", y = "Proportion-at-length (longline survey)\n") +
  theme(strip.placement = "outside") 

ggsave(paste0("figures/llsrv_lencomps_", YEAR-10, "_", YEAR, ".png"), 
       dpi=300, height=8, width=6.5, units="in")

# For fishery
ggplot(data = lencomps %>% 
         # Last 10 years of data
         filter(year >= YEAR - 10 & 
                  Sex != "Sex combined" &
                  Source == "LL fishery"), 
       aes(x = length_bin, y = proportion)) + 
  geom_bar(stat = "identity", colour = "lightgrey", fill = "lightgrey", width = 0.8) +
  geom_line(data = lencomps %>% 
              # Compare all past years to this year
              filter(year == YEAR & 
                       Sex != "Sex combined" &
                       Source == "LL fishery") %>% 
              select(-year),
            aes(x = length_bin, y = proportion, group = 1),
            colour = "black") +
  geom_text(data = labels %>% 
              filter(year >= YEAR - 10 & 
                       Sex != "Sex combined" &
                       Source == "LL fishery"),
            aes(x = length_bin, y = proportion, label = label),
            size = 3, family = "Times") +
  scale_y_continuous(limits = c(0, 0.25),
                     breaks = round(seq(0, 0.2, 0.1), 2),
                     labels =  round(seq(0, 0.2, 0.1), 2)) +
  scale_x_discrete(breaks = seq(41, 99, 6),
                   labels = seq(41, 99, 6)) +
  facet_grid(year ~ Sex) +
  labs(x = "\nFork length (cm)", y = "Proportion-at-length (longline fishery)\n") +
  theme(strip.placement = "outside") 

ggsave(paste0("figures/llfsh_lencomps_", YEAR-10, "_", YEAR, ".png"), 
       dpi=300, height=8, width=6.5, units="in")

# Summary stats output for length comps (requested by AJ Linsley 20180907)
lendat %>% 
  filter(Source %in% c("LL survey", "LL fishery")) %>% 
  group_by(Source, Sex, year) %>% 
  dplyr::summarize(mean = mean(length),
            min = min(length),
            max = max(length)) %>% 
  mutate(variable = "Fork length") -> lensum

axis <- tickr(lensum, year, 5)

lensum %>% 
  ggplot(aes(x = year, y = mean, colour = Source)) +
  geom_point() +
  geom_line() +
  scale_colour_grey(guide = FALSE) +
  facet_wrap(~ Sex) +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = NULL, y = "Mean\nfork\nlength\n(cm)") +
  theme(axis.title.y = element_text(angle=0)) -> l

#quos() uses stand eval in dplyr, eval cols with nonstand eval using !!!
cols <- quos(Source, year, Sex, age) 

bind_rows(
  fsh_bio %>% mutate(Source = "LL fishery") %>% select(!!!cols), 
  srv_bio %>% mutate(Source = "LL survey") %>% select(!!!cols)) %>% 
  filter(year >= 1997 & Sex %in% c('Female', 'Male') & !is.na(age)) %>% 
  group_by(Source, Sex, year) %>% 
  dplyr::summarize(mean = mean(age),
            min = min(age),
            max = max(age)) %>% 
  mutate(variable = "Age") -> agesum

agesum %>% 
  mutate(age = round(mean, 0)) -> agesum1

axisy <- tickr(agesum1, age, 3)

agesum %>% 
  ggplot(aes(x = year, y = mean, colour = Source)) +
  geom_point() +
  geom_line() +
  scale_colour_grey() +
  facet_wrap(~ Sex) +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  # scale_y_continuous(breaks = axisy$breaks, labels = axisy$labels) +
  labs(x = NULL, y = "Mean\nage\n(yrs)") +
  theme(legend.position = "bottom",
        axis.title.y = element_text(angle=0)) -> a

cowplot::plot_grid(l, a, axis = "lrtb", align = "hv", ncol = 1) -> compare_comp_sums

compare_comp_sums
ggsave("figures/compare_comp_summaries.png",
       plot = compare_comp_sums,
       dpi=300, height=5.5, width=6.5, units="in")

bind_rows(agesum, lensum) %>% 
  write_csv("output/comps_summary.csv")
