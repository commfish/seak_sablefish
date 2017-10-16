# work up of biological data
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2017-10-09

source("r_code/helper.R")
library("broom")
library("stats4")

# data -----
srv_bio <- read_csv("data/survey/survey_bio_1988_2016.csv")

glimpse(srv_bio)

srv_bio %>% 
  mutate(Year = factor(year),
         Project = factor(Project),
         Stat = factor(Stat),
         Station = factor(Station),
         Sex = factor(Sex),
         Maturity = factor(Maturity),
         length_cm = length_mm/10) %>% 
  group_by(Year, Stat) %>% 
  mutate(n = length(age),
         length_mu = mean(length_mm, na.rm = TRUE),
         weight_mu = mean(weight_kg, na.rm = TRUE)) -> srv_bio

# Hanselman et al. 2007 Appendix GOA Sablefish SAFE Appendix 3C for comparison.
# The coastwide parameters are still used for management, but Southeast
# slope are informative.
noaa_lvb <- read_csv("data/survey/noaa_lvb_params_hanselman2007.csv")

glimpse(noaa_lvb)

# Length-based Ludwig von Bertalanffy growth model -----

# subsets by length, age, sex
srv_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & # *FLAG* advent of "modern" survey
           !is.na(length_cm) &
           !is.na(age)) %>% 
  droplevels() -> laa_sub

laa_sub %>% ungroup() %>% filter(Sex == "Female") -> laa_f
laa_sub %>% ungroup() %>% filter(Sex == "Male") -> laa_m

# fit length-based lvb with max likelihood 
vb_like <- function(obs_length, age, l_inf, k, t0, sigma) { 
  pred <- l_inf * (1 - exp(-k * (age - t0))) # predictions based on lvb growth curve
  like <- dnorm(obs_length, pred, sigma) # likelihood
  neg_like <- -1 * (sum(log(like))) # negative log likelihood
  return(neg_like) # returning negative log likelihood
}

vb_mle <- function(obs_length, age, starting_vals, sex) {
  #minimizing negative log likelihood with mle function
  vb_mle <- mle(vb_like, start = as.list(starting_vals), 
             fixed = list(obs_length = obs_length, age = age),
             method = "BFGS")
  print(summary(vb_mle))
  l_inf_opt <- coef(summary(vb_mle))[1] # parameter estimates
  k_opt <- coef(summary(vb_mle))[2]
  t0_opt <- coef(summary(vb_mle))[3]
  sigma_opt <- coef(summary(vb_mle))[4]
  logl <- attributes(summary(vb_mle))$m2logL/-2
  pred <- l_inf_opt * (1 - exp(-k_opt * (age - t0_opt))) # retaining predicted values
  resids <- obs_length - pred # retaining residuals
  results <- list(predictions = data.frame(obs_length = obs_length,
                                           age = age, pred = pred, 
                                           resid = resids, Sex = sex),
                  results = tidy(coef(summary(vb_mle))) %>% 
                    select(Parameter = `.rownames`, Estimate, SE = `Std..Error`) %>% 
                    mutate(Sex = sex), 
                  logl = logl)
  return(results)
}

#sex-specific starting values from Hanselman et al. 2007 (Appendix C, Table 1),
#except sigma
start_f <- c(l_inf = 80, k = 0.22, t0 = -1.9, sigma = 10) 
start_m <- c(l_inf = 68, k = 0.29, t0 = -2.3, sigma = 10)

# mle fit for females
vb_mle_f <- vb_mle(obs_length = laa_f$length_cm,
                   age = laa_f$age,
                   starting_vals = start_f,
                   sex = "Female")

# mle fit for males
vb_mle_m <- vb_mle(obs_length = laa_m$length_cm,
                   age = laa_m$age,
                   starting_vals = start_m,
                   sex = "Male")

# combine predictions and parameter estimates and plot mle
pred <- rbind(vb_mle_f$predictions, vb_mle_m$predictions) %>% 
  mutate(std_resid = resid/sd(resid))


lvb_pars <- full_join(
  rbind(vb_mle_f$results, vb_mle_m$results) %>% 
  mutate(Survey = "ADF&G Longline",
         Years = paste0(min(laa_sub$year), "-", max(laa_sub$year)),
         Region = "Chatham Strait",
         Function = "Length-based LVB")
  ,
  laa_sub %>% group_by(Sex) %>% summarise(n = n()),
  by = "Sex")

png("figures/length_vonb_chatham_1997_2016.png", height = 4, width = 6, units = "in", res = 300)
ggplot() +
  geom_jitter(data = laa_sub, aes(x = age, y = length_cm, col = Sex, shape = Sex)) +
  geom_line(data = pred, aes(x = age, y = pred, col = Sex, group = Sex), lwd = 2 ) + #"#00BFC4"
  geom_line(data = pred, aes(x = age, y = pred, group = Sex), col = "black" ) + #"#00BFC4"
  xlab("\nAge (yrs)") +
  ylab("Length (cm)\n")
dev.off()

# residual plots
ggplot(data = pred) + 
  geom_histogram(aes(x = std_resid)) +
  facet_wrap(~ Sex)

ggplot(data = pred) + 
  geom_point(aes(x = age, y = std_resid)) +
  geom_hline(aes(yintercept = 0), linetype = 2, col = "red") + 
  facet_wrap(~ Sex)

ggplot(data = pred) + 
  geom_point(aes(x = pred, y = std_resid)) +
  geom_hline(aes(yintercept = 0), linetype = 2, col = "red") + 
  facet_wrap(~ Sex)

# weight-length allometry W = alpha * L ^ beta ----

# KVK assumed beta = 3, but there are data to estimate this value (e.g., Hanselman et
# al. 2007 values beta_f = 3.02 and beta_m = 2.96 are used in the current
# assessment)


# subsets by weight, age, sex
srv_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & #advent of "modern" survey
           !is.na(length_cm) &
           !is.na(weight_kg)) %>% droplevels() -> allom_sub

# length-weight relationship
lw_allometry <- function(length, a, b) {
  a * length ^ b
}

START <- c(a = 1e-5, b = 3) #Starting values close to Hanselman et al. 2007

fem_fit <- nls(weight_kg ~ lw_allometry(length = length_cm, a, b), 
               data = filter(allom_sub, Sex == "Female"), start = START)

male_fit <- nls(weight_kg ~ lw_allometry(length = length_cm, a, b), 
               data = filter(allom_sub, Sex == "Male"), start = START)

# parameter estimates and plot fit 
beta_m <- tidy(male_fit)$estimate[2]
beta_f <- tidy(fem_fit)$estimate[2]

allom_pars <- full_join(
  rbind(tidy(male_fit) %>% mutate(Sex = "Male"),
        tidy(fem_fit) %>% mutate(Sex = "Female")
  ) %>% 
    select(Parameter = term, Estimate = estimate, SE = std.error, Sex) %>% 
    mutate(Survey = "ADF&G Longline",
           Years = paste0(min(laa_sub$year), "-", max(laa_sub$year)),
           Region = "Chatham Strait",
           Function = "Allometric")
  ,
  allom_sub %>% group_by(Sex) %>% summarise(n = n()),
  by = "Sex")

png("figures/allometry_chatham_1997_2016.png", height = 4, width = 6, units = "in", res = 300)
ggplot(allom_sub,  
       aes(x = length_cm, y = weight_kg, col = Sex, shape = Sex)) +
  geom_jitter() + 
  stat_function(fun = lw_allometry, 
                args = as.list(tidy(fem_fit)$estimate),
                lwd = 1.5, col = "salmon") + 
  stat_function(fun = lw_allometry, 
                args = as.list(tidy(male_fit)$estimate),
                lwd = 1.5, col = "#00BFC4", lty = 2) + 
  xlab("\nLength (cm)") +
  ylab("Weight (kg)\n")
dev.off()
  
# weight-at-age using Ludwig von Bertalanffy growth model ----

# subsets by weight, age, sex
srv_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & # *FLAG* advent of "modern" survey
           !is.na(age) &
           !is.na(weight_kg)) %>% 
  droplevels() -> waa_sub

waa_sub %>% ungroup() %>% filter(Sex == "Female") -> waa_f
waa_sub %>% ungroup() %>% filter(Sex == "Male") -> waa_m

# fit weight-based lvb with a multiplicative error structure using max likelihood estimation
# log(w_i) = log(w_inf) + beta * log(1 - exp * (-k * (age_i - t0))) + error
# multiplicative error distrubtion accounts for increasing variability in weight-at-age with age
wvb_like <- function(obs_weight, age, w_inf, k, t0, b, sigma) { 
  log_pred <- log(w_inf) + b * log(1 - exp(-k * (age - t0))) # predictions based on von bertalanffy growth curve
  # pred <- exp(log_pred)
  like <- dnorm(log(obs_weight), log_pred, sigma) # likelihood
  neg_like <- -1 * (sum(log(like))) # negative log likelihood
  return(neg_like) # returning negative log likelihood
}

wvb_mle <- function(obs_weight, age, b, starting_vals, sex) {
  #minimizing negative log likelihood with mle function
  wvb_mle <- mle(wvb_like, start = as.list(starting_vals), 
                fixed = list(obs_weight = obs_weight, age = age, b = b),
                method = "BFGS")
  print(summary(wvb_mle))
  w_inf_opt <- coef(summary(wvb_mle))[1] #retaining optimized parameter values
  k_opt <- coef(summary(wvb_mle))[2]
  t0_opt <- coef(summary(wvb_mle))[3]
  sigma_opt <- coef(summary(wvb_mle))[4]
  logl <- attributes(summary(wvb_mle))$m2logL/-2
  log_pred <- log(w_inf_opt) + b * log(1 - exp(-k_opt * (age - t0_opt))) #  predicted values
  pred <- exp(log_pred)
  resids <- obs_weight - pred # retaining residuals
  results <- list(predictions = data.frame(obs_weight = obs_weight,
                            age = age, pred = pred, 
                            resid = resids, Sex = sex),
    results = tidy(coef(summary(wvb_mle))) %>% 
      select(Parameter = `.rownames`, Estimate, SE = `Std..Error`) %>% 
      mutate(Sex = sex), 
    logl = logl)
  return(results)
}

# starting values from Hanselman et al. 2007 Appendix C Table 5
start_f <- c(w_inf = 5.5, k = 0.24, t0 = -1.4, sigma = 10)
start_m <- c(w_inf = 3.2, k = 0.36, t0 = -1.1, sigma = 10)

# mle fit for females
wvb_mle_f <- wvb_mle(obs_weight = waa_f$weight_kg,
                   age = waa_f$age,
                   b = beta_f,
                   starting_vals = start_f,
                   sex = "Female")

# mle fit for males
wvb_mle_m <- wvb_mle(obs_weight = waa_m$weight_kg,
                   age = waa_m$age,
                   b = beta_m, 
                   starting_vals = start_m,
                   sex = "Male")


# combine predictions and parameter estimates and plot fitted values
pred <- rbind(wvb_mle_f$predictions, wvb_mle_m$predictions) %>% 
  mutate(std_resid = resid/sd(resid))


wvb_pars <- full_join(
  rbind(wvb_mle_f$results, wvb_mle_m$results) %>% 
    mutate(Survey = "ADF&G Longline",
           Years = paste0(min(waa_sub$year), "-", max(waa_sub$year)),
           Region = "Chatham Strait",
           Function = "Weight-based LVB")
  ,
  waa_sub %>% group_by(Sex) %>% summarise(n = n()),
  by = "Sex")

png("figures/weight_vonb_chatham_1997_2016.png", height = 4, width = 6, units = "in", res = 300)
ggplot() +
  geom_jitter(data = waa_sub, aes(x = age, y = weight_kg, col = Sex, shape = Sex)) +
  geom_line(data = pred, aes(x = age, y = pred, col = Sex, group = Sex), lwd = 2 ) + #"#00BFC4"
  geom_line(data = pred, aes(x = age, y = pred, group = Sex), col = "black" ) + #"#00BFC4"
  xlab("\nAge (yrs)") +
  ylab("Weight (kg))\n")
dev.off()

# residual plots
ggplot(data = pred) + 
  geom_histogram(aes(x = std_resid)) +
  facet_wrap(~ Sex)

ggplot(data = pred) + 
  geom_point(aes(x = age, y = std_resid)) +
  geom_hline(aes(yintercept = 0), linetype = 2, col = "red") + 
  facet_wrap(~ Sex)

ggplot(data = pred) + 
  geom_point(aes(x = pred, y = std_resid)) +
  geom_hline(aes(yintercept = 0), linetype = 2, col = "red") + 
  facet_wrap(~ Sex)

# Compare growth results ----

# Comparison of Hanselman et al. 2007 values with the Chatham Strait longline
# survey. Units: length (cm), weight (kg), and age (yrs)

rbind(allom_pars,
      rbind(lvb_pars, wvb_pars)) %>%
      mutate(Source = "seak_sablefish/code/biological.r") -> adfg_lvb 
      
write_csv(rbind(noaa_lvb, adfg_lvb) , 
          "output/compare_vonb_adfg_noaa.csv")

# 
