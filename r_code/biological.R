# work up of biological data
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2017-10-09

# load ----
source("r_code/helper.R")
library("broom")

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

lvb_waa <- function(t, k, W.inf, t0, b) {
  W.inf * (1 - exp(-k * (t - t0) ) ) ^ b
}
# *FLAG* use sex-spec allometric betas. probably going to need to use a
# multiplicative error structure

# figures ----

