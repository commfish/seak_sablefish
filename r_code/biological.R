# work up of survey and fishery biological data
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2017-10-16

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
         Maturity = factor(Maturity)) %>% 
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

# waa_sub %>% filter(Sex == "Female") -> waa_f
# waa_sub %>% filter(Sex == "Male") -> waa_m

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

# plot
png("figures/proportion_fembyyear.png", height = 4, width = 6, units = "in", res = 300)
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
dev.off()

## proportion of females by year and age in survey and fishery

bind_rows(
  f_sex_ratio(data = filter(srv_bio, age %in% aa), 
              src = "LL survey", year, age),
  f_sex_ratio(data = filter(fsh_bio, age %in% aa), 
              src = "LL fishery", year, age)
) -> byyrage

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

rbind(
  # Age comps (sex-specific)
  all_bio %>% 
    count(Source, Sex, year, age) %>%
    group_by(Source, Sex, year) %>% 
    mutate(proportion = round( n / sum(n), 4)),
  # Age comps (sexes combined)
  all_bio %>% 
    count(Source, year, age) %>%
    group_by(Source, year) %>% 
    mutate(proportion = round( n / sum(n), 4),
           Sex = "Sex combined")
) -> agecomps

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

png("figures/agecomp_bydatasource.png", 
    height = 3.5, width = 7, units = "in", res = 300)
ggplot(data = agecomps, 
       aes(x = age, y = proportion, colour = Source)) +
  # geom_line(aes(colour = Source, group = Source)) +
  geom_point(size = 1, alpha = 0.1) +
  stat_smooth(size = 1, se = FALSE) +
  # stat_smooth(method='gam', formula= y ~ s(x, k=6),
              # size = 1.2, se = FALSE) + 
  facet_wrap( ~ Sex) +
  scale_colour_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb")) +
  xlab('\nAge') +
  ylab('Proportion\n')
dev.off()


png("figures/agecomp_byyear.png", 
    height = 7, width = 7, units = "in", res = 300)
ggplot(data = agecomps %>% 
         filter(Sex %in% c("Female", "Male") &
                Source %in% c("LL fishery", "LL survey")),
       aes(x = year, y = age, size = proportion)) +
  geom_point(shape = 21) +
  scale_size(range = c(0, 6)) +
  facet_grid(Source ~ Sex) +
  xlab('') +
  ylab('Observed age\n') +
  guides(size = FALSE) +
  scale_x_continuous(breaks = seq(min(agecomps$year), 
                                  max(agecomps$year), 2)) +
  theme(axis.text.x = element_text(size=10, angle=45, hjust=1))
dev.off()

# ggridges exploration

agecomps %>% 
  filter(Sex %in% c("Female") &
           Source %in% c("LL fishery")) %>% 
ggplot(aes(x = year, y = age, group = age,  height = n)) +
  # geom_ridgeline(scale = 0.5) +
  geom_density_ridges2(stat = "identity", 
                       rel_min_height = 0.01,
                       scale = 3 ) + # >1 more overlap
  # cycles through these colors (track even/odd years)
  scale_fill_cyclical(values = c("lightgrey", "darkgrey"),
                      guide = "legend", # leave silent if you don't want legend
                      labels = c("Odd years", "Even year"),
                      name = "") +
  theme_ridges(grid = TRUE) +
  scale_y_discrete(expand = c(0.01, 0)) #reduces the space between x and y axis labels
  # facet_grid(Source ~ Sex) 
  