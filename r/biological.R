# work up of survey and fishery biological data
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2018-02-06

source("r/helper.r")
source("r/functions.r")
library(ggridges)

YEAR <- 2017

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

# Potentially switch to using empirical waa for yield-per-recruit or future ASA,
# but for now stick with predictions from the lvb

bind_rows(
  srv_bio %>% select(year, Project_cde, Sex, age, weight),
  fsh_bio %>% select(year, Project_cde, Sex, age, weight)) %>% 
  filter(!is.na(weight) & !is.na(age) & !is.na(Sex)) %>% 
  mutate(Source = derivedFactor('LL survey' = Project_cde == "603",
                                'LL fishery' = Project_cde == "02",
                                .method = "unique")) -> waa

waa %>%
  filter(year >= 2005 & age > 1 & age < 42) %>% 
  group_by(year, Source, Sex, age) %>% 
  summarise(weight = mean(weight) %>% round(1)) -> emp_waa

# Expand to grid to include all age combos and fill in NAs using linear
# interpolation
expand.grid(year = unique(emp_waa$year), 
            Source = unique(emp_waa$Source),
            Sex = unique(emp_waa$Sex),
            age = seq(2, 41, 1))  %>% 
  data.frame()  %>% 
  full_join(emp_waa) %>%
  group_by(year, Source, Sex) %>% 
  # interpolate weight column to fill in any missing mean weights
  mutate(weight = zoo::na.approx(weight, maxgap = 20, rule = 2)) -> emp_waa

write_csv(emp_waa, paste0("output/empircal_waa_2005_", YEAR, ".csv"))

# Length-based Ludwig von Bertalanffy growth model -----

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

laa_plot <- laa_sub %>% filter(age <= 50)
pred_pl <- pred %>% filter(pred <= 50)
axis <- tickr(laa_plot, age, 10)

ggplot(laa_plot, aes(age, length)) +
  geom_jitter(aes(col = Sex, shape = Sex), alpha=.2) +
  geom_line(data = pred, aes(y = pred, col = Sex, group = Sex), lwd = 2 ) + #"#00BFC4"
  geom_line(data = pred, aes(y = pred, group = Sex), col = "black" ) + #"#00BFC4"
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  xlab("\nAge (yrs)") +
  ylab("Length (cm)\n") + 
  theme(legend.position = c(0.9, 0.2)) +
  xlim(0, 50)

ggsave(paste0("figures/length_vonb_chathamllsurvey_1997_", YEAR, ".png"), dpi=300, height=4, width=6, units="in")

# residual plots
ggplot(data = pred) + 
  geom_histogram(aes(x = std_resid), bins=100) +
  facet_wrap(~ Sex)

ggplot(pred, aes(age, std_resid)) + 
  geom_point(alpha=.5) +
  geom_hline(yintercept = 0, lty = 2, col = "red") + 
  facet_wrap(~ Sex)

ggplot(pred, aes(pred, std_resid)) + 
  geom_point(alpha=.5) +
  geom_hline(yintercept = 0, lty = 2, col = "red") + 
  facet_wrap(~ Sex)

# Are there annual trends in length-at-age? First vonB curves by Year for illustrative
# purposes

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
  geom_hline(yintercept = 0, lty = 2) + 
  guides(colour = FALSE) +
  labs(x = "", y = "Scaled parameter estimates\n") +
  facet_wrap(~ Parameter, ncol = 1) 

ggsave(paste0("figures/trends_lenvonbpars_1997_", YEAR, ".png"), 
       dpi=300, height=7, width=6, units="in")

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

# parameter estimates and plot fit 
beta_m <- tidy(male_fit)$estimate[2]
beta_f <- tidy(fem_fit)$estimate[2]

bind_rows(tidy(male_fit) %>% mutate(Sex = "Male"),
          tidy(fem_fit) %>% mutate(Sex = "Female")) %>% 
  dplyr::select(Parameter = term, Estimate = estimate, SE = std.error, Sex) %>% 
  mutate(Survey = "ADF&G Longline",
         Years = paste0(min(laa_sub$year), "-", max(laa_sub$year)),
         Region = "Chatham Strait",
         Function = "Allometric") %>% 
  full_join(allom_sub %>% 
              group_by(Sex) %>% 
              summarise(n = n())) -> allom_pars

ggplot(allom_sub, aes(length, weight, col = Sex, shape = Sex)) +
  geom_jitter(alpha=.2) + 
  stat_function(fun = lw_allometry, 
                args = as.list(tidy(fem_fit)$estimate),
                col = "salmon") + 
  stat_function(fun = lw_allometry, 
                args = as.list(tidy(male_fit)$estimate),
                col = "#00BFC4", lty = 2) + 
  xlab("\nLength (cm)") +
  ylab("Weight (kg)\n") + 
  theme(legend.position = c(0.9, 0.2))

ggsave(paste0("figures/allometry_chathamllsurvey_1997_", YEAR, ".png"),
       dpi=300, height=4, width=6, units="in")
  
# Weight-based Ludwig von Bertalanffy growth model ----

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

# Hold-over from KVK: for the plus group (42+) take the mean of all samples >=
# 42
srv_f_waa <- wvb_mle_f$ypr_predictions
srv_f_waa[41, 2] <- mean(waa_f$weight[waa_f$age >= 42], na.rm = TRUE)
srv_m_waa <- wvb_mle_m$ypr_predictions
srv_m_waa[41, 2] <- mean(waa_m$weight[waa_m$age >= 42], na.rm = TRUE)

rbind(srv_f_waa, srv_m_waa) %>% 
  mutate(Source = "LL survey") -> srv_waa

# combine predictions and parameter estimates and plot fitted values
wvb_mle_f$predictions %>% 
  rbind(wvb_mle_m$predictions) %>% 
  mutate(std_resid = scale(resid)) -> pred

wvb_mle_f$results %>% 
  rbind(wvb_mle_m$results) %>% 
  mutate(Survey = "ADF&G Longline",
         Years = paste0(min(waa_sub$year), "-", max(waa_sub$year)),
         Region = "Chatham Strait",
         Function = "Weight-based LVB") %>% 
  full_join(waa_sub %>% 
              group_by(Sex) %>% 
              summarise(n = n()), by = 'Sex') -> wvb_pars

ggplot() +
  geom_jitter(data = waa_sub, aes(x = age, y = weight, col = Sex, shape = Sex)) +
  geom_line(data = pred, aes(x = age, y = pred, col = Sex, group = Sex), lwd = 2 ) + #"#00BFC4"
  geom_line(data = pred, aes(x = age, y = pred, group = Sex), col = "black" ) + #"#00BFC4"
  xlab("\nAge (yrs)") +
  ylab("Weight (kg)\n") +
  theme(legend.position = c(0.9, 0.8))

ggsave("figures/weight_vonb_chathamllsurvey_1997_2016.png", 
       dpi=300, height=4, width=6, units="in")

# residual plots
pred %>% 
  ggplot(aes(std_resid)) + geom_histogram(bins=100) +
  facet_wrap(~Sex)

pred %>% 
  ggplot(aes(age, std_resid)) + 
  geom_point(alpha=.2) +
  geom_hline(yintercept=0, lty=4, alpha=.5) +
  facet_wrap(~Sex)

pred %>% 
  ggplot(aes(pred, std_resid)) + geom_point(alpha=.2) +
  geom_hline(yintercept=0, lty=4, alpha=.5) +
  facet_wrap(~Sex)

# Fishery weight-at-age ----

# For YPR analysis, use same methods as survey (same time period, same starting
# values)

# subsets by weight, age, sex
fsh_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & # use same years as survey
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

# Hold-over from KVK: for the plus group (42+) take the mean of all samples >=
# 42
fsh_f_waa <- fsh_wvb_f$ypr_predictions
fsh_f_waa[41, 2] <- mean(fsh_waa_f$weight[fsh_waa_f$age >= 42], na.rm = TRUE)
fsh_m_waa <- fsh_wvb_m$ypr_predictions
fsh_m_waa[41, 2] <- mean(fsh_waa_m$weight[fsh_waa_m$age >= 42], na.rm = TRUE)

rbind(fsh_f_waa, fsh_m_waa) %>% 
  mutate(Source = "LL fishery") %>% 
  rbind(srv_waa) %>% 
  mutate(weight = round(weight, 1)) %>% 
  select(Source, Sex, age, weight) -> pred_waa 

write_csv(pred_waa, paste0("output/pred_waa_1997_", YEAR, ".csv"))

# Compare empirical and predicted weight-at-age
ggplot() +
  geom_point(data = emp_waa %>% filter(year == YEAR), 
       aes(x = age, y = weight, col = Sex, shape = Source)) +
  geom_line(data = pred_waa,
            aes(x = age, y = weight, col = Sex, linetype = Source)) +
  labs(x = "\nAge", y = "Weight (kg)\n")

ggsave(paste0("figures/compare_empirical_predicted_waa_", YEAR, ".png"), 
              dpi=300, height=4, width=6, units="in")

# Compare growth results ----

# Comparison of Hanselman et al. 2007 values with the Chatham Strait longline
# survey. Units: length (cm), weight (kg), and age (yrs)

bind_rows(allom_pars, lvb_pars, wvb_pars) %>% 
      mutate(Source = "seak_sablefish/code/biological.r") %>% 
  bind_rows(noaa_lvb) %>% 
  write_csv(., "output/compare_vonb_adfg_noaa.csv")

# Maturity ----
# 0 = immature, 1 = mature

# base models
fit_length <- glm(Mature ~ length, data = laa_f, family = binomial)
fit_age <- glm(Mature ~ age, data = laa_f, family = binomial)

# by year
fit_length_year <- glm(Mature ~ length * Year, data = laa_f, family = binomial)
fit_age_year <- glm(Mature ~ age * Year, data = laa_f, family = binomial)      
# Warning message:
#   glm.fit: fitted probabilities numerically 0 or 1 occurred 

AIC(fit_length, fit_age, fit_length_year, fit_age_year)

## select the "best model" (fit_length_year) and run the model on the new full
# dataset (there is more length data than age so it will usually fit better)

# subsets by length
srv_bio %>% 
  ungroup() %>% 
  filter(Sex == "Female" &
           year >= 1997 & # *FLAG* advent of "modern" survey
           !is.na(length)) %>% 
  droplevels() -> len_f

fit_length_year <- glm(Mature ~ length * Year, data = len_f, family = binomial)

# New df for prediction
new_len_f <- data.frame(length = rep(seq(0, 120, 0.05), n_distinct(len_f$year)),
           Year = factor(sort(rep(unique(len_f$year), length(seq(0, 120, 0.05))), decreasing = FALSE)))

# Get predicted values by year and take the mean           
broom::augment(x = fit_length_year, 
               newdata = new_len_f, 
               type.predict = "response") %>% 
  select(Year, length, fitted = .fitted, se =.se.fit) %>% 
  group_by(length) %>% 
  mutate(Probability = mean(fitted)) -> pred

#Length-based maturity curves (light blue lines are annual mean preditions, dark
#blue is the mean)
ggplot(pred) +
  geom_line(aes(x = length, y = fitted, group = Year), colour = "lightblue") +
  geom_line(aes(x = length, y = Probability), 
            colour = "darkblue", size = 2) +
  lims(x = c(40, 85)) +
  labs(x = "\nLength (cm)", y = "Probability\n")

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

# Next convert predictions back to age via vonb
age_pred <- seq(0, 42, by = 0.01)
vb_pars <- vb_mle_f$results
age_pred <- data.frame(age = age_pred,
                       length = round(vb_pars$Estimate[1] * (1 - exp(- vb_pars$Estimate[2] * (age_pred - vb_pars$Estimate[3]))), 1))

# Match lengths back to lengths predicted by vonb
pred <- merge(pred, age_pred, by = "length") 

# Get length at 50% maturity (L_50 = -b_0/b_1) and get a_50 from age_pred
# (predicted from vonB)
merge(mature_results %>% 
        dcast(year ~ Parameter, value.var = "est") %>% 
        mutate(length = - round(b_0 / b_1, 1)), #length at 50% maturity)
      age_pred, by = "length") %>% 
  arrange(year) %>% 
  select(year, l_50 = length, a_50 = age) %>% 
  group_by(year) %>% 
  mutate(a_50 = round(mean(a_50), 1)) %>%
  distinct() %>% ungroup() %>% 
  mutate(mu_a_50 = mean(a_50),
         mu_l_50 = mean(l_50)) -> mat_50_year

merge(pred %>% mutate(year = Year), mat_50_year, by = "year") -> pred

# Age-based maturity curves estimated from length-based maturity and vonB growth
# curve (light blue lines are annual mean preditions, dark blue is the mean)
ggplot(pred) +
  geom_line(aes(x = age, y = fitted, group = Year), 
            colour = "lightblue") +
  geom_line(aes(x = age, y = Probability), 
            colour = "darkblue", size = 2) +
  lims(x = c(0, 20)) +
  labs(x = "Age", y = "Probability") -> maturity_at_age_plot

# Comparison with age-based maturity curve
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
  mutate(Probability = mean(fitted)) -> pred_age

# Comparison of maturity at age curves. Blue is derived from length-based
# maturity cuve, red is estimated directly from age. Light lines are annual mean
# preditions, dark lines are the means.
maturity_at_age_plot +
  geom_line(data = pred_age,
            aes(x = age, y = fitted, group = Year), 
            colour = "lightpink", alpha = 0.5) +
  geom_line(data = pred_age,
            aes(x = age, y = Probability), 
            colour = "red", size = 2, alpha = 0.5) +
  labs(x = "Age", y = "Probability")

#Length-based (translated to age) is more realistic than age-based. Also there
#is no clear reason to choose the more complicated model (fit_length_year) over
#the simpler model (fit_length)

# Get predicted values for the simpler model, then merge with age predictions from the vonb          
left_join(broom::augment(x = fit_length, 
               newdata = data.frame(length = seq(0, 200, 0.01)), 
               type.predict = "response") %>% 
  select(length, fitted = .fitted, se =.se.fit), 
  age_pred, by = "length") -> simple_fit

# Maturity at age for YPR
simple_fit %>%  
  filter(age %in% c(2:42)) %>%
  right_join(data.frame(age = 2:42)) %>% 
  # interpolate fitted probability to fill in any missing values
  mutate(Sex = "Female",
         Source = "LL survey",
         probability = round(zoo::na.approx(fitted, maxgap = 20, rule = 2),2)) %>% 
  select(age, probability) %>% 
  write_csv("output/fem_maturityatage_llsrv.csv")

# Fit age-based model to fitted values so you can derive parameter estimates
fit_age <- glm(fitted ~ age, data = simple_fit, family = binomial)

#Derive age at 50% maturity and kmat (slope of logistic curve)
b0 <- fit_age$coefficients[1]
b1 <- fit_age$coefficients[2]
a50 <- -b0/b1
age <- min(laa_f$age):max(laa_f$age)
kmat <- ((b0 + b1*age) / (age - a50))[1]

# proportion mature at age
laa_f %>% ungroup() %>%
  count(Mature, age) %>%
  group_by(age) %>%
  mutate(proportion = round(nn / sum(nn), 2)) %>% 
  filter(Mature == 1) -> proportion_mature

# Final age-based maturity curve estimated from length-based maturity and vonB growth
# curve. Points are proportion mature at age (1997-present)

# Equation text for plotting values of a_50 and kmat
a50_txt <- as.character(
  as.expression(substitute(
    paste(italic(a[50]), " = ", xx),
    list(xx = formatC(a50, format = "f", digits = 1)))))

kmat_txt <- as.character(
  as.expression(substitute(
    paste(italic(k[mat]), " = ", xx),
    list(xx = formatC(kmat, format = "f", digits = 1)))))

simple_fit %>%
  sample_frac(0.1) %>% 
ggplot() +
  geom_line(aes(x = age, y = fitted), 
            colour = "darkblue", size = 2) +
  geom_segment(aes(x = a50, y = 0, xend = a50, yend = 0.50), 
               lty = 2, col = "darkblue") +
  geom_segment(aes(x = 0, y = 0.50, xend = a50, yend = 0.50), 
               lty = 2, col = "darkblue") +
  # Porportion mature by age
  geom_point(data = proportion_mature,
             aes(x = age, y = proportion),
             colour = "lightblue") +
  # a_50 and kmat labels
  geom_text(aes(10, 0.5, label = a50_txt), face = "bold", size = 5, parse = TRUE) +
  # geom_text(aes(10, 0.46, label = kmat_txt), face = "bold", parse = TRUE) +
  lims(x = c(0, 15)) +
  labs(x = "\nAge", y = "Probability\n") 

# ggsave("figures/fem_maturity_at_age.png", dpi=300, 
#        height = 4, width = 5, units="in")
ggsave("figures/fem_maturity_at_age2.png", dpi=300, 
       height = 4, width = 4, units="in")

# Sex ratios ----

# proportion of females by age in survey and fishery

# restrict age range
aa <- c(2:42)

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
  theme(legend.position = c(0.8, 0.8))

ggsave("figures/proportion_fembyage.png", dpi=300, 
       height=4, width=7,  units="in")

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
write_csv(byyear, "output/sexratio_byyear.csv")

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
  xlab("") +
  ylab("Proportion of females\n") +
  scale_colour_manual(values = c("black", "grey")) +
  scale_fill_manual(values = c("black", "grey")) +
  theme(legend.position = c(0.8, 0.2))

ggsave("figures/proportion_fembyyear.png", dpi=300,  height=4, width=7, units="in")

# proportion of females by year and age in survey and fishery

f_sex_ratio(data = filter(srv_bio, age %in% aa), 
              src = "LL survey", year, age) %>% 
  bind_rows(f_sex_ratio(data = filter(fsh_bio, age %in% aa), 
              src = "LL fishery", year, age)) -> byyrage

# Age compositions ----

# Combine survey and fishery data for age comp analysis

#quos() uses stand eval in dplyr, eval cols with nonstand eval using !!!
cols <- quos(Source, year, Sex, age) 

rbind(
  fsh_bio %>% mutate(Source = "LL fishery") %>% select(!!!cols), 
  rbind( 
    srv_bio %>% mutate(Source = "LL survey") %>% select(!!!cols), 
    potsrv_bio %>% mutate(Source = "Pot survey") %>% select(!!!cols)) 
  ) %>% 
  filter(Sex %in% c('Female', 'Male') & !is.na(age)) %>% 
  droplevels() %>% 
  mutate(age = ifelse(age >= 42, 42, age)) %>% 
  filter(age >= 2) -> all_bio  # Plus group

# Age comps (sex-specific)
all_bio %>% 
  count(Source, Sex, year, age) %>%
  group_by(Source, Sex, year) %>% 
  mutate(proportion = round( n / sum(n), 4)) %>% 
  bind_rows(all_bio %>% # Age comps (sexes combined)
          count(Source, year, age) %>%
          group_by(Source, year) %>% 
          mutate(proportion = round( n / sum(n), 4),
                 Sex = "Sex combined")) -> agecomps   #%>% 
  # *FLAG* weight the proportion-at-age by the sample size in a given year
  # (N_year) divided by the total sample size (N)
  #mutate(N_year = sum(n)) %>% 
  # group_by(Source, Sex) %>% 
  # mutate(N = sum(n)) %>% 
  # ungroup() %>% 
  # arrange(age, Source, Sex, year) %>% 
  # complete(Source, Sex, year, age, 
  #          fill = list(n = 0, proportion = 0)) %>% 
  # mutate(#weight = N_year/N,
         #proportion_scaled = proportion * weight,
         # Age = factor(age)) # for plotting, ordered = TRUE

# Years with pot bio data
potsrv_bio %>% 
  filter(!is.na(age) & !is.na(Sex)) %>% 
  distinct(year) -> pot_yrs

# complete() was behaving weirdly. Expand to grid to include all age combos
expand.grid(year = unique(agecomps$year), 
            Source = unique(agecomps$Source),
            Sex = unique(agecomps$Sex),
            age = seq(2, 42, 1))  %>% 
  data.frame()  %>% 
  full_join(agecomps) %>%
  fill_by_value(n, proportion, value = 0) %>% 
  mutate(Age = factor(age),
         proportion = round(proportion, 3)) %>%
  # Keep only relevant years for each Source
  filter(c(Source == "LL fishery" & year >= 2002) |
           c(Source == "LL survey" & year >= 1997) |
           c(Source == "Pot survey" & year %in% pot_yrs$year)) -> agecomps

# Check that they sum to 1
agecomps %>% 
  group_by(Source, Sex, year) %>% 
  summarise(sum(proportion)) 

# Sample sizes by source/year/sex
agecomps %>% 
  group_by(Source, year, Sex) %>% 
  summarize(n = sum(n)) %>% 
  dcast(Source + Sex ~ year, value.var = "n") %>% 
  write_csv("output/n_agecomps.csv")

# Age comp matrix
agecomps %>% write_csv("output/agecomps.csv")

# Bargraph for presentation
agecomps %>% 
  filter(year == YEAR & Source == "LL fishery" &
           Sex %in% c("Male", "Female")) %>% 
  ggplot(aes(age, proportion, fill = Sex)) +
  geom_bar(stat = "identity",
           position = "dodge") +
           # position = position_dodge(preserve = "single")) +
  scale_x_continuous(breaks = seq(min(agecomps$age), max(agecomps$age), 4), 
                     labels =  seq(min(agecomps$age), max(agecomps$age), 4)) +
  labs(x = "\nAge", y = "Proportion\n") +
  theme(legend.position = c(0.9, 0.7))

ggsave(paste0("figures/agecomp_bargraph_", YEAR, ".png"), 
              dpi=300, height=3, width=9, units="in")

# All years smoothed by source
agecomps %>% 
  filter(Sex == "Sex combined") %>% 
ggplot(aes(x = age, y = proportion, colour = Source)) +
  geom_point(size = 1, alpha = 0.1) +
  stat_smooth(size = 1, se = FALSE) +
  # facet_wrap( ~ Sex) +
  # lims(y = c(0, 0.1)) + 
  scale_y_continuous(limits = c(0, 0.1),
                     breaks = round(seq(min(agecomps$proportion), 0.1, 0.02), 2), 
                     labels =  round(seq(min(agecomps$proportion), 0.1, 0.02), 2)) +
  scale_colour_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb")) +
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
           age <= 30) %>% 
  ungroup()

axisy <- tickr(agecompdat, year, 3)
axisx <- tickr(agecompdat, age, 10)

ggplot(data = agecompdat,
       aes(x = age, y = year, size = proportion)) + #*FLAG* could swap size with proportion_scaled
  geom_point(shape = 21, fill = "black", colour = "black") +
  scale_size(range = c(0, 4)) +
  facet_wrap(~ Sex) +
  xlab('\nObserved age') +
  ylab('') +
  guides(size = FALSE) +
  scale_x_continuous(breaks = axisx$breaks, labels = axisx$labels) +
  scale_y_continuous(breaks = axisy$breaks, labels = axisy$labels)

ggsave("figures/bubble_survey_agecomp_byyear.png", dpi=300, height=5, width=7.5, units="in")

# fishery
agecompdat <- agecomps %>% 
  filter(Sex %in% c("Female", "Male") &
           Source %in% c("LL fishery") &
           year >= 2002 &
           age <= 30) %>% 
  ungroup()

axisy <- tickr(agecompdat, year, 3)
axisx <- tickr(agecompdat, age, 10)

ggplot(data = agecompdat,
       aes(x = age, y = year, size = proportion)) + #*FLAG* could swap size with proportion_scaled
  geom_point(shape = 21, colour = "black", fill = "black") +
  scale_size(range = c(0, 4)) +
  facet_wrap(~ Sex) +
  xlab('\nObserved age') +
  ylab('') +
  guides(size = FALSE) +
  scale_x_continuous(breaks = axisx$breaks, labels = axisx$labels) +
  scale_y_continuous(breaks = axisy$breaks, labels = axisy$labels)

ggsave("figures/bubble_fishery_agecomp_byyear.png", 
       dpi=300, height=5, width=7.5, units="in")

# ggridges exploration *FLAG*
agecomps %>% 
  filter(Sex == "Female" &
           Source == "LL fishery") %>% 
  droplevels() %>% 
ggplot(aes(x = year, y = Age, group = Age,height = proportion_scaled)) + #
  # geom_ridgeline(scale = 0.5) +
  geom_density_ridges2(stat = "identity",
                       rel_min_height = 0.01,
                       scale = 2 ) + # >1 more overlap
  # cycles through these colors (track even/odd years)
  # scale_fill_cyclical(values = c("lightgrey", "darkgrey"),
  #                     guide = "legend", # leave silent if you don't want legend
  #                     labels = c("Odd years", "Even year"),
  #                     name = "") +
  # theme_ridges(grid = TRUE) +
  scale_y_discrete(expand = c(0.01, 0)) #reduces the space between x and y axis labels
  # facet_grid(Source ~ Sex) 

# Length compositions ----

# Pers. comm. K. Fenske 2018-01-05: NMFS uses length bins 41, 43, 45 ... 99.
# These bins represent the center of the bin, so a 43 bin represents fish
# 42-43.9 cm. They omit fish smaller than 40 or larger than 100 cm for the
# length comp analysis. I've maintained these conventions for easy comparison:
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
            mutate(Source = "LL fishery"),
          potsrv_bio %>% 
            filter(Sex %in% c("Female", "Male") &
                     !is.na(length)) %>% 
            select(year, Sex, length) %>% 
            mutate(Source = "Pot survey")) %>% 
  filter(!c(length < 40 | length > 100)) %>% 
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
         proportion = round(proportion, 3)) %>%
  # Keep only relevant years for each Source
  filter(c(Source == "LL fishery" & year >= 2002) |
           c(Source == "LL survey" & year >= 1997) |
           c(Source == "Pot survey" & year %in% pot_yrs$year)) -> lencomps

# Check that they sum to 1
lencomps %>% 
  group_by(Source, Sex, year) %>% 
  summarise(sum(proportion)) 

write_csv(lencomps,"output/lengthcomps.csv")

lendat %>% 
  # Mean length comp for comparison
  count(Source, Sex, length_bin) %>%
  group_by(Source, Sex) %>% 
  mutate(proportion = round( n / sum(n), 4)) %>% 
  arrange(Source, Sex, length_bin) %>% 
  # Fill in the blanks with 0's
  complete(Source, length_bin,
           fill = list(n = 0, proportion = 0)) -> mu_lencomps

mu_lencomps %>% 
  group_by(Source, Sex) %>% 
  summarise(sum(proportion))

s_lencomps <- lencomps %>% 
  filter(Source == "LL survey")

ggplot() + 
  geom_bar(data = s_lencomps %>% 
             filter(year >= 2014 & 
                      Sex == "Female"), 
           aes(x = length_bin, y = proportion), 
           stat = "identity") +
  geom_line(data = mu_lencomps %>% 
              filter(Source == "LL survey" & 
                       Sex == "Female"),
            aes(x = length_bin, y = proportion, group = 1), 
            colour = "skyblue", size = 2) +
  scale_y_continuous(limits = c(0, 0.16),
                     breaks = round(seq(min(lencomps$proportion), 0.16, 0.06), 2), 
                     labels =  round(seq(min(lencomps$proportion), 0.16, 0.06), 2)) +
  scale_x_discrete(breaks = seq(41, 99, 6),
                   labels = seq(41, 99, 6)) +
  facet_wrap(~ year, ncol = 1) +
  labs(x = "\nLength (cm)", y = "Proportion\n")

ggsave("figures/llsrv_fem_lengthcomps_2014_2017.png", 
       dpi=300,  height=6, width=5.5, units="in")

f_lencomps <- lencomps %>% 
  filter(Source == "LL fishery")

ggplot() + 
  geom_bar(data = f_lencomps %>% 
             filter(year >= 2014 & 
                      Sex == "Female"), 
           aes(x = length_bin, y = proportion), 
           stat = "identity") +
  geom_line(data = mu_lencomps %>% 
              filter(Source == "LL fishery" & 
                       Sex == "Female"),
            aes(x = length_bin, y = proportion, group = 1), 
            colour = "skyblue", size = 2) +
  facet_wrap(~ year, ncol = 1) +
  scale_y_continuous(limits = c(0, 0.16),
                     breaks = round(seq(min(lencomps$proportion), 0.16, 0.06), 2), 
                     labels =  round(seq(min(lencomps$proportion), 0.16, 0.06), 2)) +
  scale_x_discrete(breaks = seq(41, 99, 6),
                   labels = seq(41, 99, 6)) +
  labs(x = "\nLength (cm)", y = "Proportion\n")

ggsave("figures/llfsh_fem_lengthcomps_2014_2017.png", 
       dpi=300, height=6, width=5.5, units="in")

# All years smoothed by source
lencomps %>% 
  filter(Sex == "Sex combined") %>%
  ggplot(aes(x = as.numeric(as.character(length_bin)), y = proportion, 
             colour = Source, linetype = Source)) +
  geom_point(size = 1, alpha = 0.1) +
  stat_smooth(size = 2, se = FALSE) +
  # facet_wrap( ~ Sex, ncol = 1) +
  # lims(y = c(0, 0.1)) + 
  scale_y_continuous(limits = c(0, 0.1),
                     breaks = round(seq(min(lencomps$proportion), 0.1, 0.02), 2), 
                     labels =  round(seq(min(lencomps$proportion), 0.1, 0.02), 2)) +
  scale_colour_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb")) +
  xlab('\nLength (cm)') +
  ylab('Proportion\n') +
  theme(legend.position = c(0.8, 0.8))

ggsave("figures/lengthcomp_bydatasource.png", 
       dpi=300, height=4.5, width=5, units="in")

# Age-length transition matrices ----


#OLD CODE
###############################################################################
##  SABLEFISH LENGTH-AT-AGE FROM ALEX DATA
##  QUINN AND DERISO: 301 : 305
##  Eq. 8.14a, b, and c
##  Updated 2/19/2015 Kray Van Kirk
##
##  THIS SCRIPT INCLUDES LENGTH-TO-AGE TRANSITION
##  MATRICES FOR EACH SEX AND FISHERY/SURVEY
##
##  Note: the calcs below don't make a huge difference relative to simply
##  calling the age-dist from the ADU; this was mostly an experiment, but
##  it's fairly involved so keep it for future use
##

#----------------------------------------------------------------
# ALEX QUERY CRITERIA FOR FISH_BIO_DATA
#----------------------------------------------------------------
# year BETWEEN 1980 AND 2015 AND species_code = '710' AND 
# g_management_area_code = 'NSEI' AND project_code = '02'
# NOTE: when running, always make sure that all data have been read 
# and uploaded into IFDB by the ADU
#----------------------------------------------------------------


#----------------------------------------------------------------
# ALEX QUERY CRITERIA FOR SRV_BIO_DATA
#----------------------------------------------------------------
#
# BIOLOGICAL DATA >> Age Sex Size Sampled at Sea
#  Base Table	out_g_bio_effort_age_sex_size							
#  Select Clause	*							
# Where Clause	year BETWEEN 1988 AND 2015 AND 
#                   species_code = '710' AND 
#                   project_code = '03'																	
#----------------------------------------------------------------


#----------------------------------------------------------------
# LIBRARIES
#----------------------------------------------------------------
library(lattice)
library(nlme)
library(plyr)

#----------------------------------------------------------------

#----------------------------------------------------------------
# SET WORKING DIRECTORY AND READ IN DATA
#----------------------------------------------------------------
LWA.LL  <-read.table("data/srv_bio_data.csv",header=TRUE,sep=",")
LWA.fshy<-read.table("data/fish_bio_data.csv",header=TRUE,sep=",")


#----------------------------------------------------------------
# LONGLINE SURVEY DATA
#
# catch-age from catch-length and aging subset frequencies
#----------------------------------------------------------------


#----------------------------------------------------------------
# Create subset of length data only
# NOTE: DO *NOT* subset on age - the subset of all lengthed fish
# selected for aging is integral to the analysis, so you need all
# those non-aged fish for which there are lengths to remain
#----------------------------------------------------------------
j <- !is.na(LWA.LL$LENGTH_MILLIMETERS) 
dat <- LWA.LL[j,c("YEAR", "SEX", "AGE", "LENGTH_MILLIMETERS")]

dat$LENGTH_MILLIMETERS<-round_any(dat$LENGTH_MILLIMETERS,10)

dat$AGE[dat$AGE > 41] <- 42
dat$LENGTH_MILLIMETERS[dat$LENGTH_MILLIMETERS > 990] <- 1000
dat$LENGTH_MILLIMETERS[dat$LENGTH_MILLIMETERS < 470] <- 470

#Check
head(dat)

#Check sample size
dim(dat)

#----------------------------------------------------------------
# Calculate alpha_l = C_l / C
# Proportion of fish lengthed per length over all fish measured
# (Okay - I could have used the word 'lengthed' again, but... )
#----------------------------------------------------------------
L<-as.matrix(table(dat$LENGTH_MILLIMETERS,dat$YEAR))
L_tot<-colSums(L)
alpha_l<-as.matrix(sweep(L, 2, L_tot,"/"))
alpha_l
colSums(alpha_l)

#----------------------------------------------------------------
# Calculate theta_la = A_la / A_l
# Proportion of all fished of length 'l' measured that were
# then selected for aging
#----------------------------------------------------------------
theta_la<-as.array(table(dat$LENGTH_MILLIMETERS,dat$AGE,dat$YEAR))

theta_la_tot<-as.matrix(apply(theta_la,c(1,3),sum)) 
theta_la_tot

theta_la <- sweep(theta_la, c(1,3), apply(theta_la,c(1,3),sum)+1E-10, "/")

round(theta_la,3)

#----------------------------------------------------------------
# Eq. 8.14 a page 305 Q&D
#----------------------------------------------------------------
theta_a<-sweep(theta_la, c(1,3),alpha_l, "*")
theta_A<-colSums(theta_a)
round(theta_A,4)
#write.table(theta_A,"clipboard")

#----------------------------------------------------------------
# Calculate variance, which includes both within length and
# between length variability  8.14b, c
#----------------------------------------------------------------

tmpw<-theta_la*(1-theta_la)
tmpw1<-sweep(tmpw,c(1,3),alpha_l^2,"*")
tmpw2<-sweep(tmpw1, c(1,3), theta_la_tot-1+1E-10, "/")
tmpw3<-apply(tmpw2,c(1,3),sum)
tmpw3
SE_within<-tmpw2

tmpb<-sweep(theta_la, c(2,3), theta_A,"*")
tmpb<-tmpb^2
tmpb1<-sweep(tmpb, c(1,3), alpha_l, "*")
tmpb2<-sweep(tmpb1, 3, L_tot, "/")
tmpb3<-apply(tmpb2,c(1,3),sum)
SE_between<-tmpb2

SE<-SE_within + SE_between
SE_tot<-apply(SE, c(2,3), sum)
SE_tot<-sqrt(SE_tot)
round(SE_tot,4)


#----------------------------------------------------------------
# Write longline survey theta_A and SE_tot to file
# ---------------------------------------------------------------
write.table(theta_A,"output/age_length_s_all.csv")
write.table(SE_tot,"output/age_length_s_allvar.csv")

rm(dat,tmpw,tmpw1,tmpw2,tmpb,tmpb1,tmpb2,alpha_l, theta_la, theta_A, L_tot, theta_la_tot, L)



#----------------------------------------------------------------
#
# Survey males
#
#----------------------------------------------------------------
j <- !is.na(LWA.LL$LENGTH_MILLIMETERS)
j <- j & is.element(LWA.LL$SEX,"Male")
dat <- LWA.LL[j,c("YEAR", "SEX", "AGE", "LENGTH_MILLIMETERS")]

dat$LENGTH_MILLIMETERS<-round_any(dat$LENGTH_MILLIMETERS,10)

dat$AGE[dat$AGE > 41] <- 42
dat$LENGTH_MILLIMETERS[dat$LENGTH_MILLIMETERS > 800] <- 800
dat$LENGTH_MILLIMETERS[dat$LENGTH_MILLIMETERS < 470] <- 470



#----------------------------------------------------------------
# Calculate alpha_l = C_l / C
# Proportion of fish lengthed per length over all fish measured
# (Okay - I could have used the word 'lengthed' again, but... )
#----------------------------------------------------------------
L<-as.matrix(table(dat$LENGTH_MILLIMETERS,dat$YEAR))
dim(L)
L_tot<-colSums(L)
alpha_l<-as.matrix(sweep(L, 2, L_tot,"/"))
alpha_l
colSums(alpha_l)

#----------------------------------------------------------------
# Calculate theta_la = A_la / A_l
# Proportion of all fished of length 'l' measured that were
# then selected for aging
#----------------------------------------------------------------
theta_la<-as.array(table(dat$LENGTH_MILLIMETERS,dat$AGE,dat$YEAR))

theta_la_tot<-as.matrix(apply(theta_la,c(1,3),sum)) 
theta_la_tot

theta_la <- sweep(theta_la, c(1,3), apply(theta_la,c(1,3),sum)+1E-10, "/")

round(theta_la,3)
dim(theta_la)

#----------------------------------------------------------------
# Eq. 8.14 a page 305 Q&D
#----------------------------------------------------------------
theta_a<-sweep(theta_la, c(1,3),alpha_l, "*")
theta_A<-colSums(theta_a)
round(theta_A,4)
#write.table(theta_A,"clipboard")

#----------------------------------------------------------------
# Calculate variance, which includes both within length and
# between length variability  8.14b, c
#----------------------------------------------------------------

tmpw<-theta_la*(1-theta_la)
tmpw1<-sweep(tmpw,c(1,3),alpha_l^2,"*")
tmpw2<-sweep(tmpw1, c(1,3), theta_la_tot-1+1E-10, "/")
SE_within<-tmpw2

tmpb<-sweep(theta_la, c(2,3), theta_A,"-")
tmpb<-tmpb^2
tmpb1<-sweep(tmpb, c(1,3), alpha_l, "*")
tmpb2<-sweep(tmpb1, 3, L_tot, "/")
SE_between<-tmpb2

SE<-SE_within + SE_between
SE_tot<-apply(SE, c(2,3), sum)
SE_tot<-sqrt(SE_tot)
round(SE_tot,4)

#----------------------------------------------------------------
# Write longline survey theta_A and SE_tot to file
# ---------------------------------------------------------------
write.table(theta_A,"output/age_length_s_m.csv")
write.table(SE_tot,"output/age_length_s_mvar.csv")

rm(tmpw,tmpw1,tmpw2,tmpb,tmpb1,tmpb2,alpha_l, theta_la, theta_A, L_tot, theta_la_tot, L)

#----------------------------------------------------------------
#
# Survey females
#
#----------------------------------------------------------------
j <- !is.na(LWA.LL$LENGTH_MILLIMETERS)
j <- j & is.element(LWA.LL$SEX,"Female")
dat.f <- LWA.LL[j,c("YEAR", "SEX", "AGE", "LENGTH_MILLIMETERS")]


dat.f$LENGTH_MILLIMETERS<-round_any(dat.f$LENGTH_MILLIMETERS,10)

dat.f$AGE[dat.f$AGE > 41] <- 42
dat.f$LENGTH_MILLIMETERS[dat.f$LENGTH_MILLIMETERS > 990] <- 1000
dat.f$LENGTH_MILLIMETERS[dat.f$LENGTH_MILLIMETERS < 470] <- 470

head(dat.f)
dim(dat.f)

#----------------------------------------------------------------
# Calculate alpha_l = C_l / C
# Proportion of fish lengthed per length over all fish measured
# (Okay - I could have used the word 'lengthed' again, but... )
#----------------------------------------------------------------
L<-as.matrix(table(dat.f$LENGTH_MILLIMETERS,dat.f$YEAR))
L_tot<-colSums(L)
alpha_l<-as.matrix(sweep(L, 2, L_tot,"/"))
alpha_l
colSums(alpha_l)

#----------------------------------------------------------------
# Calculate theta_la = A_la / A_l
# Proportion of all fished of length 'l' measured that were
# then selected for aging
#----------------------------------------------------------------
theta_la<-as.array(table(dat.f$LENGTH_MILLIMETERS,dat.f$AGE,dat.f$YEAR))

theta_la_tot<-as.matrix(apply(theta_la,c(1,3),sum)+0.000000000001) 
theta_la_tot

theta_la <- sweep(theta_la, c(1,3), apply(theta_la,c(1,3),sum)+1E-10, "/")

colSums(theta_la_tot)
round(theta_la,3)

#----------------------------------------------------------------
# Eq. 8.14 a page 305 Q&D
#----------------------------------------------------------------
theta_a<-sweep(theta_la, c(1,3),alpha_l, "*")
theta_A<-colSums(theta_a)
round(theta_A,4)
colSums(theta_A)

#----------------------------------------------------------------
# Calculate variance, which includes both within length and
# between length variability  8.14b, c
#----------------------------------------------------------------

tmpw<-theta_la*(1-theta_la)
tmpw1<-sweep(tmpw,c(1,3),alpha_l^2,"*")
tmpw2<-sweep(tmpw1, c(1,3), theta_la_tot-1+1E-10, "/")
tmpw3<-apply(tmpw2,c(1,3),sum)
tmpw3
SE_within<-tmpw2

tmpb<-sweep(theta_la, c(2,3), theta_A,"*")
tmpb<-tmpb^2
tmpb1<-sweep(tmpb, c(1,3), alpha_l, "*")
tmpb2<-sweep(tmpb1, 3, L_tot+1E-10, "/")
tmpb3<-apply(tmpb2,c(1,3),sum)
SE_between<-tmpb2

SE<-SE_within + SE_between
SE_tot<-apply(SE, c(2,3), sum)
SE_tot<-sqrt(SE_tot)
round(SE_tot,4)

#----------------------------------------------------------------
# Write longline survey theta_A and SE_tot to file
# ---------------------------------------------------------------
write.table(theta_A,"output/age_length_s_f.csv")
write.table(SE_tot,"output/age_length_s_fvar.csv")




#----------------------------------------------------------------
# COMMERCIAL FISHERY DATA
#
# catch-age from catch-length and aging subset frequencies
#----------------------------------------------------------------


#----------------------------------------------------------------
# Create subset of length data only
# NOTE: DO *NOT* subset on age - the subset of all lengthed fish
# selected for aging is integral to the analysis, so you need all
# those non-aged fish for which there are lengths to remain
#----------------------------------------------------------------
j <- !is.na(LWA.fshy$LENGTH_MILLIMETERS) & LWA.fshy$YEAR > 2001
dat <- LWA.fshy[j,c("YEAR", "SEX_CODE", "AGE", "LENGTH_MILLIMETERS")]

dat$LENGTH_MILLIMETERS<-round_any(dat$LENGTH_MILLIMETERS,10)

dat$AGE[dat$AGE > 41] <- 42
dat$LENGTH_MILLIMETERS[dat$LENGTH_MILLIMETERS > 990] <- 1000
dat$LENGTH_MILLIMETERS[dat$LENGTH_MILLIMETERS < 470] <- 470

#Check
head(dat)

#Check sample size
dim(dat)

#----------------------------------------------------------------
# Calculate alpha_l = C_l / C
# Proportion of fish lengthed per length over all fish measured
# (Okay - I could have used the word 'lengthed' again, but... )
#----------------------------------------------------------------
L<-as.matrix(table(dat$LENGTH_MILLIMETERS,dat$YEAR))
L_tot<-colSums(L)
alpha_l<-as.matrix(sweep(L, 2, L_tot,"/"))
alpha_l
colSums(alpha_l)

#----------------------------------------------------------------
# Calculate theta_la = A_la / A_l
# Proportion of all fished of length 'l' measured that were
# then selected for aging
#----------------------------------------------------------------
theta_la<-as.array(table(dat$LENGTH_MILLIMETERS,dat$AGE,dat$YEAR))

theta_la_tot<-as.matrix(apply(theta_la,c(1,3),sum)) 
theta_la_tot

theta_la <- sweep(theta_la, c(1,3), apply(theta_la,c(1,3),sum)+1E-10, "/")

round(theta_la,3)

#----------------------------------------------------------------
# Eq. 8.14 a page 305 Q&D
#----------------------------------------------------------------
theta_a<-sweep(theta_la, c(1,3),alpha_l, "*")
theta_A<-colSums(theta_a)
round(theta_A,4)
#write.table(theta_A,"clipboard")

#----------------------------------------------------------------
# Calculate variance, which includes both within length and
# between length variability  8.14b, c
#----------------------------------------------------------------

tmpw<-theta_la*(1-theta_la)
tmpw1<-sweep(tmpw,c(1,3),alpha_l^2,"*")
tmpw2<-sweep(tmpw1, c(1,3), theta_la_tot-1+1E-10, "/")
tmpw3<-apply(tmpw2,c(1,3),sum)
tmpw3
SE_within<-tmpw2

tmpb<-sweep(theta_la, c(2,3), theta_A,"*")
tmpb<-tmpb^2
tmpb1<-sweep(tmpb, c(1,3), alpha_l, "*")
tmpb2<-sweep(tmpb1, 3, L_tot+1E-10, "/")
tmpb3<-apply(tmpb2,c(1,3),sum)
SE_between<-tmpb2

SE<-SE_within + SE_between
SE_tot<-apply(SE, c(2,3), sum)
SE_tot<-sqrt(SE_tot)
round(SE_tot,4)

#----------------------------------------------------------------
# Write longline survey theta_A and SE_tot to file
# ---------------------------------------------------------------
write.table(theta_A,"output/age_length_f_all.csv")
write.table(SE_tot,"output/age_length_f_allvar.csv")



#----------------------------------------------------------------
#
# Fishery males
#
#----------------------------------------------------------------
j <- !is.na(LWA.fshy$LENGTH_MILLIMETERS)
j <- j & is.element(LWA.fshy$SEX_CODE, 1)
dat <- LWA.fshy[j,c("YEAR", "SEX_CODE", "AGE", "LENGTH_MILLIMETERS")]

dat$LENGTH_MILLIMETERS<-round_any(dat$LENGTH_MILLIMETERS,10)

dat$AGE[dat$AGE > 41] <- 42
dat$LENGTH_MILLIMETERS[dat$LENGTH_MILLIMETERS > 790] <- 800
dat$LENGTH_MILLIMETERS[dat$LENGTH_MILLIMETERS < 470] <- 470


head(dat)
dim(dat)


#----------------------------------------------------------------
# Calculate alpha_l = C_l / C
# Proportion of fish lengthed per length over all fish measured
# (Okay - I could have used the word 'lengthed' again, but... )
#----------------------------------------------------------------
L<-as.matrix(table(dat$LENGTH_MILLIMETERS,dat$YEAR))
L_tot<-colSums(L)
alpha_l<-as.matrix(sweep(L, 2, L_tot,"/"))
alpha_l
colSums(alpha_l)


#----------------------------------------------------------------
# Calculate theta_la = A_la / A_l
# Proportion of all fished of length 'l' measured that were
# then selected for aging
#----------------------------------------------------------------
theta_la<-as.array(table(dat$LENGTH_MILLIMETERS,dat$AGE,dat$YEAR))

theta_la_tot<-as.matrix(apply(theta_la,c(1,3),sum)) 
theta_la_tot

theta_la <- sweep(theta_la, c(1,3), apply(theta_la,c(1,3),sum)+1E-10, "/")

round(theta_la,3)

#----------------------------------------------------------------
# Eq. 8.14 a page 305 Q&D
# dimensions 'alpha_l'      = 54 x 13      (length x year)
# dimensions 'theta_la'     = 54 x 41 x 13 (length x age x year)
# dimensions 'theta_la_tot' = 54 x 13      (length x year)
# dimensions 'theta_a'      = 54 x 41 x 13 (length x age x year)
#----------------------------------------------------------------
theta_a<-sweep(theta_la, c(1,3),alpha_l, "*")
theta_A<-colSums(theta_a)
round(theta_A,4)
#write.table(theta_A,"clipboard")

#----------------------------------------------------------------
# Calculate variance, which includes both within length and
# between length variability  8.14b, c
#----------------------------------------------------------------
tmpw<-theta_la*(1-theta_la)
tmpw1<-sweep(tmpw,c(1,3),alpha_l^2,"*")
tmpw2<-sweep(tmpw1, c(1,3), theta_la_tot-1+1E-10, "/")
tmpw3<-apply(tmpw2,c(1,3),sum)
tmpw3
SE_within<-tmpw2

tmpb<-sweep(theta_la, c(2,3), theta_A,"*")
tmpb<-tmpb^2
tmpb1<-sweep(tmpb, c(1,3), alpha_l, "*")
tmpb2<-sweep(tmpb1, 3, L_tot+1E-10, "/")
tmpb3<-apply(tmpb2,c(1,3),sum)
SE_between<-tmpb2

SE<-SE_within + SE_between
SE_tot<-apply(SE, c(2,3), sum)
SE_tot<-sqrt(SE_tot)
round(SE_tot,4)


#----------------------------------------------------------------
# Write longline survey theta_A and SE_tot to file
# ---------------------------------------------------------------
write.table(theta_A,"output/age_length_f_m.csv")
write.table(SE_tot,"output/age_length_f_mvar.csv")




#----------------------------------------------------------------
#
# Fishery females
#
#----------------------------------------------------------------
j <- !is.na(LWA.fshy$LENGTH_MILLIMETERS)
j <- j & is.element(LWA.fshy$SEX_CODE, 2)
dat <- LWA.fshy[j,c("YEAR", "SEX_CODE", "AGE", "LENGTH_MILLIMETERS")]

dat$LENGTH_MILLIMETERS<-round_any(dat$LENGTH_MILLIMETERS,10)

dat$AGE[dat$AGE > 41] <- 42
dat$LENGTH_MILLIMETERS[dat$LENGTH_MILLIMETERS > 990] <- 1000
dat$LENGTH_MILLIMETERS[dat$LENGTH_MILLIMETERS < 470] <- 470


head(dat)
dim(dat)



#----------------------------------------------------------------
# Calculate alpha_l = C_l / C
# Proportion of fish lengthed per length over all fish measured
# (Okay - I could have used the word 'lengthed' again, but... )
#----------------------------------------------------------------

L<-as.matrix(table(dat$LENGTH_MILLIMETERS,dat$YEAR))
L_tot<-colSums(L)
alpha_l<-as.matrix(sweep(L, 2, L_tot,"/"))
alpha_l
colSums(alpha_l)


#----------------------------------------------------------------
# Calculate theta_la = A_la / A_l
# Proportion of all fished of length 'l' measured that were
# then selected for aging
#----------------------------------------------------------------
theta_la<-as.array(table(dat$LENGTH_MILLIMETERS,dat$AGE,dat$YEAR))

theta_la_tot<-as.matrix(apply(theta_la,c(1,3),sum)) 
theta_la_tot

theta_la <- sweep(theta_la, c(1,3), apply(theta_la,c(1,3),sum)+1E-10, "/")

round(theta_la,3)

#----------------------------------------------------------------
# Eq. 8.14 a page 305 Q&D
# dimensions 'alpha_l'      = 54 x 13      (length x year)
# dimensions 'theta_la'     = 54 x 41 x 13 (length x age x year)
# dimensions 'theta_la_tot' = 54 x 13      (length x year)
# dimensions 'theta_a'      = 54 x 41 x 13 (length x age x year)
#----------------------------------------------------------------
theta_a<-sweep(theta_la, c(1,3),alpha_l, "*")
theta_A<-colSums(theta_a)
round(theta_A,4)
#write.table(theta_A,"clipboard")

#----------------------------------------------------------------
# Calculate variance, which includes both within length and
# between length variability  8.14b, c
#----------------------------------------------------------------
tmpw<-theta_la*(1-theta_la)
tmpw1<-sweep(tmpw,c(1,3),alpha_l^2,"*")
tmpw2<-sweep(tmpw1, c(1,3), theta_la_tot-1+1E-10, "/")
tmpw3<-apply(tmpw2,c(1,3),sum)
tmpw3
SE_within<-tmpw2

tmpb<-sweep(theta_la, c(2,3), theta_A,"*")
tmpb<-tmpb^2
tmpb1<-sweep(tmpb, c(1,3), alpha_l, "*")
tmpb2<-sweep(tmpb1, 3, L_tot, "/")
tmpb3<-apply(tmpb2,c(1,3),sum)
SE_between<-tmpb2

SE<-SE_within + SE_between
SE_tot<-apply(SE, c(2,3), sum)
SE_tot<-sqrt(SE_tot)
round(SE_tot,4)

#----------------------------------------------------------------
# Write longline survey theta_A and SE_tot to file
# ---------------------------------------------------------------
write.table(theta_A,"output/age_length_f_f.csv")
write.table(SE_tot,"output/age_length_f_fvar.csv")

