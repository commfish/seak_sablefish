# work up of biological data
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2017-10-09

# load ----
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

# length-at-age using Ludwig von Bertalanffy growth model -----

lvb_laa <- function(t, l_inf, k, t0) {
  l_inf * (1 - exp(-k * (t - t0)) )
}

srv_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & #advent of "modern" survey
           !is.na(length_cm) &
           !is.na(age)) %>% 
  droplevels() -> laa_sub

#sex-specific starting values close to Hanselman et al. 2007 (Appendix C, Table 1)
START_f <- c(l_inf = 80, k = 0.22, t0 = -1.9) 
START_m <- c(l_inf = 68, k = 0.29, t0 = -2.3)

fem_fit <- nls(length_cm ~ lvb_laa(t = age, l_inf, k, t0), 
               data = filter(laa_sub, Sex == "Female"), 
               trace = TRUE, algorithm = "port", 
               lower = c(60, 0.02, -5), upper = c(100, 0.5, 5),
               start = START_f)
# *FLAG* nls() having problems estimating parameters, hitting lower bounds of
# t0. next steps: optim(), admb

# multiplicative error structure (increasing variance with age)
fem_fit_log <- nls(log(length_cm) ~ log(lvb_laa(t = age, l_inf, k, t0)), 
               data = filter(laa_sub, Sex == "Female"), 
               trace = TRUE, algorithm = "port", 
               lower = c(60, 0.02, -5), upper = c(100, 0.5, 5),
               start = START_f)

male_fit <- nls(length_cm ~ lvb_laa(t = age, l_inf, k, t0), 
                data = filter(laa_sub, Sex == "Male"), 
                trace = TRUE, algorithm = "port", 
                lower = c(30, 0.01, -5), upper = c(80, 0.5, 5), start = START_m)

# multiplicative error structure (increasing variance with age)
male_fit_log <- nls(log(length_cm) ~ log(lvb_laa(t = age, l_inf, k, t0)), 
                data = filter(laa_sub, Sex == "Male"), 
                trace = TRUE, algorithm = "port", 
                lower = c(30, 0.01, -5), upper = c(80, 0.5, 5), start = START_m)

#plot
ggplot(laa_sub,  
       aes(x = age, y = length_cm, col = Sex, shape = Sex)) +
  geom_jitter() + #alpha = .8
  stat_function(fun = lvb_laa, 
                args = as.list(tidy(fem_fit)$estimate),
                lwd = 2, col = "salmon") + #"salmon"
  stat_function(fun = lvb_laa, 
                args = as.list(tidy(fem_fit_log)$estimate),
                lwd = 2, col = "pink") + #"salmon"
  stat_function(fun = lvb_laa, 
                args = as.list(tidy(male_fit)$estimate),
                lwd = 2, col = "blue") + #"#00BFC4"
  stat_function(fun = lvb_laa, 
                args = as.list(tidy(male_fit_log)$estimate),
                lwd = 2, col = "lightblue") + #"#00BFC4"
  scale_color_manual(values = c("salmon", "blue")) +
  xlab("\nAge (yrs)") +
  ylab("Length (cm)\n")

# fit length-based lbv with max likelihood 
vb_like <- function(obs_length, age, l_inf, k, t0, sigma) { 
  pred <- l_inf * (1 - exp(-k * (age - t0))) # predictions based on von bertalanffy growth curve
  like <- dnorm(obs_length, pred, sigma) # likelihood
  neg_like <- -1 * (sum(log(like))) # negative log likelihood
  return(neg_like) # returning negative log likelihood
}

vb_mle <- function(obs_length, age, starting_vals) {
  #minimizing negative log likelihood with mle function
  vb_mle <- mle(vb_like, start = as.list(starting_vals), 
             fixed = list(obs_length = obs_length, age = age),
             method = "BFGS")
  print(summary(vb_mle))
  l_inf_opt <- coef(summary(vb_mle))[1] #retaining optimized parameter values
  k_opt <- coef(summary(vb_mle))[2]
  t0_opt <- coef(summary(vb_mle))[3]
  sigma_opt <- coef(summary(vb_mle))[4]
  logl <- attributes(summary(vb_mle))$m2logL/-2
  pred <- l_inf_opt * (1 - exp(-k_opt * (age - t0_opt))) # retaining predicted values
  resids <- obs_length - pred # retaining residuals
  results <- list(predictions = data.frame(obs_length = obs_length,
                                           age = age, pred = pred, resid = resids),
                  results = coef(summary(vb_mle)), logl = logl)
  return(results)
}



laa_sub %>% ungroup() %>% filter(Sex == "Female") -> laa_f
laa_sub %>% ungroup() %>% filter(Sex == "Male") -> laa_m

#sex-specific starting values close to Hanselman et al. 2007 (Appendix C, Table 1)
start_f <- c(l_inf = 80, k = 0.22, t0 = -1.9, sigma = 10) 
start_m <- c(l_inf = 68, k = 0.29, t0 = -2.3, sigma = 10)

# mle fit for females
vb_mle_f <- vb_mle(obs_length = laa_f$length_cm,
                   age = laa_f$age,
                   starting_vals = start_f)

pred_f <- vb_mle_f$predictions %>% mutate(Sex = "Female")

# mle fit for males
vb_mle_m <- vb_mle(obs_length = laa_m$length_cm,
                   age = laa_m$age,
                   starting_vals = start_m)
pred_m <- vb_mle_m$predictions %>% mutate(Sex = "Male")

# combine output and plot mle
pred <- rbind(pred_f, pred_m) %>% 
  mutate(std_resid = resid/sd(resid))

ggplot() +
  geom_jitter(data = laa_sub, aes(x = age, y = length_cm, col = Sex)) +
  geom_line(data = pred, aes(x = age, y = pred, col = Sex, group = Sex), lwd = 2 ) + #"#00BFC4"
  geom_line(data = pred, aes(x = age, y = pred, group = Sex), col = "black" ) + #"#00BFC4"
  xlab("\nAge (yrs)") +
  ylab("Length (cm)\n")

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

lw_allometry <- function(length, a, b) {
  a * length ^ b
}

srv_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & #advent of "modern" survey
           !is.na(length_cm) &
           !is.na(weight_kg)) %>% 
  droplevels() -> allom_sub

START <- c(a = 1e-5, b = 3) #Starting values close to Hanselman et al. 2007

fem_fit <- nls(weight_kg ~ lw_allometry(length = length_cm, a, b), 
               data = filter(allom_sub, Sex == "Female"), start = START)

male_fit <- nls(weight_kg ~ lw_allometry(length = length_cm, a, b), 
               data = filter(allom_sub, Sex == "Male"), start = START)

ggplot(allom_sub,  
       aes(x = length_cm, y = weight_kg, col = Sex, shape = Sex)) +
  geom_jitter() + #alpha = .8
  stat_function(fun = lw_allometry, 
                args = as.list(tidy(fem_fit)$estimate),
                lwd = 1.5, col = "salmon") + #"salmon"
  stat_function(fun = lw_allometry, 
                args = as.list(tidy(male_fit)$estimate),
                lwd = 1.5, col = "blue", lty = 2) + #"#00BFC4"
  scale_color_manual(values = c("salmon", "blue")) +
  xlab("\nLength (cm)") +
  ylab("Weight (kg)\n")
  
beta_m <- tidy(male_fit)$estimate[2]
beta_f <- tidy(fem_fit)$estimate[2]

# weight-at-age using Ludwig von Bertalanffy growth model ----
# use sex-spec allometric betas. probably going to need to use a
# multiplicative error structure

lvb_waa <- function(weight, t, k, w_inf, t0, b) {
  pred <- w_inf * (1 - exp(-k * (t - t0) ) ) ^ b
  pred <- ifelse(pred < 0, abs(pred), pred)
  log(weight) - log(pred)
}

srv_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & #advent of "modern" survey
           !is.na(age) &
           !is.na(weight_kg)) %>% 
  droplevels() -> waa_sub

# starting values from Hanselman et al. 2007 Appendix C Table 5
START_f <- c(w_inf = 5.5, k = 0.24, t0 = -1.4, b = beta_f)
START_m <- c(w_inf = 3.2, k = 0.36, t0 = -1.1, b = beta_m)

fem_waa <- nls(weight_kg ~ lvb_waa(weight = weight_kg, t = age, 
                                   w_inf, k, t0, b = beta_f), 
               data = filter(allom_sub, Sex == "Female"), 
               start = START_f)

male_waa <- nls(weight_kg ~ lvb_waa(weight = weight_kg, t = age, 
                                   w_inf, k, t0, b = beta_m), 
               data = filter(allom_sub, Sex == "Male"), 
               start = START_m)

ggplot(waa_sub,  
       aes(x = age, y = weight_kg, col = Sex, shape = Sex)) +
  geom_jitter() + #alpha = .8
  stat_function(fun = lvb_waa, 
                args = START_f,
                lwd = 2, col = "salmon") + #"salmon"
  # stat_function(fun = lvb_laa, 
  #               args = as.list(tidy(fem_fit_log)$estimate),
  #               lwd = 2, col = "pink") + #"salmon"
  stat_function(fun = lvb_waa, 
                args = START_m,
                lwd = 2, col = "blue") + #"#00BFC4"
  # stat_function(fun = lvb_laa, 
  #               args = as.list(tidy(male_fit_log)$estimate),
  #               lwd = 2, col = "lightblue") + #"#00BFC4"
  scale_color_manual(values = c("salmon", "blue")) +
  xlab("\nAge (yrs)") +
  ylab("Weight (kg)\n")
# figures ----

