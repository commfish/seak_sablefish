# work up of survey and fishery biological data
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2017-12-08

source("r_code/helper.r")
source("r_code/functions.r")
library(ggridges)

# data -----

# survey biological  data

read_csv("data/survey/llsurvey_bio_1988_2016.csv", guess_max = 50000) %>% 
  mutate(Year = factor(year),
         Project = factor(Project),
         Stat = factor(Stat),
         Station = factor(Station),
         Sex = factor(Sex),
         Maturity = factor(Maturity),
         # *FLAG* there's an [unresolved and unreproducible] issue with the maturity
         # codes in Kray's data files, so the cleaner data will need to be updated using
         # the new codes. For old codes 1 and 2 = immature, 3 = mature. For new codes, 1
         # = immature, 7 = mature
         Mature = derivedFactor("0" = Maturity_cde %in% c("1", "2"),
                                "1" = Maturity_cde,
                                .method = "first", .default = NA,
                                .ordered = TRUE)) %>% 
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
read_csv("data/fishery/fishery_bio_2000_2016.csv", guess_max = 50000) %>%
  mutate(Year = factor(year),
         Project_cde = factor(Project_cde),
         Adfg = factor(Adfg),
         Stat = factor(Stat),
         Sex_cde = factor(Sex_cde),
         Sex = factor(Sex),
         Maturity = factor(Maturity),
         Maturity_cde = factor(Maturity_cde)) %>% 
  group_by(Year, Stat) %>% 
  mutate(n = length(age),
         length_mu = mean(length, na.rm = TRUE),
         weight_mu = mean(weight, na.rm = TRUE)) %>% 
  ungroup() -> fsh_bio

# Pot survey biological data
read_csv("data/survey/potsurvey_bio_2009_2015.csv", guess_max = 50000) %>% 
  mutate(Year = factor(year),
         Project_cde = factor(Project_cde),
         Stat = factor(Stat),
         Sex = factor(Sex),
         Maturity_cde = factor(Maturity_cde),
         Discard_status_cde = factor(Discard_status_cde)) -> potsrv_bio

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
              summarise(n = n())) 

ggplot(laa_sub, aes(age, length_cm)) +
  geom_jitter(aes(col = Sex, shape = Sex), alpha=.2) +
  geom_line(data = pred, aes(y = pred, col = Sex, group = Sex), lwd = 2 ) + #"#00BFC4"
  geom_line(data = pred, aes(y = pred, group = Sex), col = "black" ) + #"#00BFC4"
  xlab("\nAge (yrs)") +
  ylab("Length (cm)\n") + 
  theme(legend.justification=c(1,0), legend.position=c(1,0))

ggsave("figures/length_vonb_chathamllsurvey_1997_2016.png", dpi=300, height=4, width=6, units="in")

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
              summarise(n = n())) 

ggplot(allom_sub, aes(length_cm, weight_kg, col = Sex, shape = Sex)) +
  geom_jitter(alpha=.2) + 
  stat_function(fun = lw_allometry, 
                args = as.list(tidy(fem_fit)$estimate),
                col = "salmon") + 
  stat_function(fun = lw_allometry, 
                args = as.list(tidy(male_fit)$estimate),
                col = "#00BFC4", lty = 2) + 
  xlab("\nLength (cm)") +
  ylab("Weight (kg)\n") + 
  theme(legend.justification=c(1,0), legend.position=c(1,0))

ggsave("figures/allometry_chathamllsurvey_1997_2016.png", dpi=300, height=4, width=6, units="in")
  
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
              summarise(n = n()), by = 'Sex')

ggplot() +
  geom_jitter(data = waa_sub, aes(x = age, y = weight, col = Sex, shape = Sex)) +
  geom_line(data = pred, aes(x = age, y = pred, col = Sex, group = Sex), lwd = 2 ) + #"#00BFC4"
  geom_line(data = pred, aes(x = age, y = pred, group = Sex), col = "black" ) + #"#00BFC4"
  xlab("\nAge (yrs)") +
  ylab("Weight (kg))\n") +
  theme(legend.justification=c(1,0), legend.position=c(1,0))

ggsave("figures/weight_vonb_chathamllsurvey_1997_2016.png", dpi=300, height=4, width=6, units="in")

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

# temporary detour
fit_length <- glm(Mature ~ length, data = len_f, family = binomial)
tidy(coef(summary(fit_length))) %>% select(par = `.rownames`, est = Estimate ) -> pars
b0 <- fit_length$coefficients[1]
b1 <- fit_length$coefficients[2]
l50 <- -b0/b1
lens <- min(len_f$length):max(len_f$length)
kmat <- ((b0 + b1*lens) / (lens - l50))[1]
# detour over

## select the "best model" (fit_length_year) and run the model on the new full
# dataset (there is more length data than age)

# subsets by length, age, sex
srv_bio %>% 
  ungroup() %>% 
  filter(Sex == "Female" &
           year >= 1997 & # *FLAG* advent of "modern" survey
           !is.na(length)) %>% 
  droplevels() -> len_f

fit_length_year <- glm(Mature ~ length * Year, data = len_f, family = binomial)

data.frame(length = seq(0:max(len_f$length)),
           year = unique(len_)
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
    mutate(param = "Year1997"),
  
  mature_results %>% 
    filter(!param %in% c("(Intercept)", "length") &
             !grepl(':+', param)) %>%     # filter(!grepl('length:Year\\d{4}', param)) %>% #alternative regex
    mutate(est = est + mature_results$est[mature_results$param == "(Intercept)"]),
    

  ) %>% mutate(Parameter = "b_0")
    ,
    
    # filter out slopes (contain >1 :), derive estimates by yr
    mature_results %>% 
      filter(grepl(':+', param)) %>% 
      mutate(est = est + mature_results$est[mature_results$param == "length"],
             Parameter = "b_1")
  )
)

# Then convert back to age via VonB
mature_results$Parameter
max(na.omit(srv_bio$age))
age_vec <- seq(0, 40, 0.1)

pred <- vb_pars$Estimate[1] * (1 - exp(- vb_pars$Estimate[2] * (age_vec - vb_pars$Estimate[3])))
vb_pars <- vb_mle_f$results

# Sex ratios ----

# proportion of females by age in survey and fishery

# restrict age range
aa <- c(2:42)

# see helper for f_sex_ratio() documentation
f_sex_ratio(data = filter(srv_bio, age %in% aa), 
              src = "LL survey", age) %>% 
  bind_rows(f_sex_ratio(data = filter(fsh_bio, age %in% aa), 
              src = "LL fishery", age)) -> byage

# get generalized additive model fits and predictions
# survey
srv_fitage <- gam(I(Sex == "Female") ~ s(age), 
                  data = filter(srv_bio, age %in% aa, Sex %in% c("Female", "Male")),
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
  bind_rows(tbl_df(do.call(cbind, srv_predage))%>% mutate(source_check = "LL survey"),
            tbl_df(do.call(cbind, fsh_predage))%>% mutate(source_check = "LL fishery") ) 
  ) -> byage

# plot
ggplot(byage, aes(x = age)) +
  geom_line(aes(y = fit, col = Source)) +
  geom_ribbon(aes(ymin = fit - se.fit*2, ymax = fit + se.fit*2, fill = Source, col = Source),  alpha = 0.2) +
  geom_point(aes(y = proportion, col = Source)) +  
  expand_limits(y = c(0.30, 0.8)) +
  xlab("\nAge") +
  ylab("Proportion of females\n") 

ggsave("figures/proportion_fembyage.png", dpi=300, height=4, width=6, units="in")

# proportion of females by year in the fishery and survey

f_sex_ratio(data = filter(srv_bio), src = "LL survey", year) %>% 
  bind_rows(f_sex_ratio(data = filter(fsh_bio), 
              src = "LL fishery", year)) -> byyear

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

ggplot(data = byyear, aes(x = year)) +
  geom_line(aes(y = fit, col = Source), size = 1) +
  geom_ribbon(aes(ymin = fit - se.fit*2, ymax = fit + se.fit*2, 
                  fill = Source, col = Source),  alpha = 0.2) +
  geom_point(aes(y = proportion, col = Source)) +  
  scale_x_continuous(breaks = seq(min(byyear$year), max(byyear$year), 2), 
                     labels =  seq(min(byyear$year), max(byyear$year), 2)) +
  xlab("") +
  ylab("Proportion of females\n") +
  theme(axis.text.x = element_text(size=10, angle=45, hjust=1))

ggsave("figures/proportion_fembyyear.png", dpi=300, height=4, width=6, units="in")

## proportion of females by year and age in survey and fishery

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
    potsrv_bio %>% mutate(Source = "Pot survey") %>% select(!!!cols) ) 
  ) %>% 
  filter(Sex %in% c('Female', 'Male') & !is.na(age)) %>% 
  droplevels() %>% 
  mutate(age = ifelse(age >= 42, 42, age)) -> all_bio  # Plus group

# Age comps (sex-specific)
all_bio %>% 
  count(Source, Sex, year, age) %>%
  group_by(Source, Sex, year) %>% 
  mutate(proportion = round( n / sum(n), 4)) %>% 
  bind_rows(all_bio %>% # Age comps (sexes combined)
          count(Source, year, age) %>%
          group_by(Source, year) %>% 
          mutate(proportion = round( n / sum(n), 4),
                 Sex = "Sex combined"))  %>% 
  # *FLAG* weight the proportion-at-age by the sample size in a given year
  # (N_year) divided by the total sample size (N)
  mutate(N_year = sum(n)) %>% 
  group_by(Source, Sex) %>% 
  mutate(N = sum(n)) %>% 
  ungroup() %>% 
  mutate(weight = N_year/N,
         proportion_scaled = proportion * weight,
         Age = factor(age)) %>% # for plotting, ordered = TRUE
  arrange(age, Source, Sex, year) -> agecomps

# Sample sizes by source/year/sex
agecomps %>% 
  group_by(Source, year, Sex) %>% 
  summarize(n = sum(n)) %>% 
  dcast(Source + Sex ~ year, value = "n") %>% 
  write_csv("output/n_agecomps.csv")

# Age comp matrix
agecomps %>% 
  dcast(Source + Sex + year ~ age, value = "proportion") %>%  
  write_csv("output/agecomps.csv")

# Graphics
ggplot(data = agecomps, 
       aes(x = age, y = proportion, colour = Source)) +
  geom_point(size = 1, alpha = 0.1) +
  stat_smooth(size = 1, se = FALSE) +
  facet_wrap( ~ Sex) +
  scale_colour_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb")) +
  xlab('\nAge') +
  ylab('Proportion\n')

ggsave("figures/agecomp_bydatasource.png", dpi=300, height=3.5, width=7, units="in")

ggplot(data = agecomps %>% 
         filter(Sex %in% c("Female", "Male") &
                Source %in% c("LL fishery", "LL survey")),
       aes(x = year, y = age, size = proportion)) + #*FLAG* could swap size with proportion_scaled
  geom_point(shape = 21) +
  scale_size(range = c(0, 6)) +
  facet_grid(Source ~ Sex) +
  xlab('') +
  ylab('Observed age\n') +
  guides(size = FALSE) +
  scale_x_continuous(breaks = seq(min(agecomps$year), 
                                  max(agecomps$year), 2)) +
  theme(axis.text.x = element_text(size=10, angle=45, hjust=1))

ggsave("figures/agecomp_byyear.png", dpi=300, height=7.5, width=7.5, units="in")

# ggridges exploration *FLAG*
agecomps %>% 
  filter(Sex == "Female" &
           Source == "LL fishery") %>% 
  droplevels() %>% 
ggplot(aes(x = year, y = Age, group = Age,  height = proportion_scaled)) + #
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

