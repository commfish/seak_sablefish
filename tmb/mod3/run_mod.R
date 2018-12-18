
# ASA that includes catch, fishery and survey CPUE, mark-recapture abundance
# estimates, fishery and survey weight-at-age, survey data about maturity-at-age
# and proportions-at-age, and fishery and survey age compositions. No sex
# structure.

# Libraries and helper functions ----

source("r/helper.r")
source("r/functions.r")

library(TMB)


# Data -----
setwd("tmb/mod3")

ts <- read_csv("abd_indices.csv")        # time series
age <- read_csv("agecomps.csv")          # age comps
bio <- read_csv("maturity_sexratio.csv") # proportion mature and proportion-at-age in the survey
waa <- read_csv("waa.csv")               # weight-at-age

# Starting values
finits <- read_csv("inits_f_devs.csv")   # log F devs
rinits <- read_csv("inits_rec_devs.csv") # log rec devs

# Model dimensions
syr <- min(ts$year)       # model start year
lyr <- max(ts$year)       # end year
nyr <- length(syr:lyr)    # number of years        
rec_age <- min(waa$age)   # recruitment age                  
plus_group <- max(waa$age)            # plus group age
nage <- length(rec_age:plus_group)    # number of ages

# Subsets
mr <- filter(ts, !is.na(mr))
fsh_cpue <- filter(ts, !is.na(fsh_cpue))
srv1_cpue <- filter(ts, !is.na(srv1_cpue))
srv2_cpue <- filter(ts, !is.na(srv2_cpue))
fsh_age <- filter(age, Source == "Fishery")
srv_age <- filter(age, Source == "Survey")

# Structure data for TMB - must use same variable names as .cpp
data <- list(
  
  # Model dimensions
  nyr = nyr,
  nage = nage,

  # Fixed parameters
  M = 0.1,
  sigma_catch = 0.05,
  sigma_cpue = 0.1,
  sigma_mr = 0.05,
  omega = 50,
  
  # Catch
  data_catch = ts$catch,
  
  # Mark-recapture estimates
  nyr_mr = n_distinct(mr, mr),
  yrs_mr = mr %>% distinct(index) %>% pull(),
  data_mr = pull(mr, mr),
  
  # Fishery CPUE
  nyr_fsh_cpue = fsh_cpue %>% n_distinct(fsh_cpue),
  yrs_fsh_cpue = fsh_cpue %>% distinct(index) %>% pull(),
  data_fsh_cpue = pull(fsh_cpue, fsh_cpue),
  
  # Survey CPUE 1-hr soak time
  nyr_srv1_cpue = srv1_cpue %>% n_distinct(srv1_cpue),
  yrs_srv1_cpue = srv1_cpue %>% distinct(index) %>% pull(),
  data_srv1_cpue = pull(srv1_cpue, srv1_cpue),
  
  # Survey CPUE 3+ hr soak time
  nyr_srv2_cpue = srv2_cpue %>% n_distinct(srv2_cpue),
  yrs_srv2_cpue = srv2_cpue %>% distinct(index) %>% pull(),
  data_srv2_cpue = pull(srv2_cpue, srv2_cpue),
  
  # Timing in month fractions
  spawn_month = 2/12, # Feb
  srv_month = 7/12,   # Jul
  fsh_month = 8/12,   # Aug
  
  # Proportion mature-at-age
  prop_mature = bio$prop_mature,
  
  # Proportion female-at-age in the survey
  prop_fem = bio$prop_fem,
  
  # Weight-at-age
  data_fsh_waa = filter(waa, waa == "fsh") %>% pull(weight_kg),
  data_srv_waa = filter(waa, waa == "srv") %>% pull(weight_kg),
  data_fem_waa = filter(waa, waa == "fem") %>% pull(weight_kg),
  
  # Fishery age comps
  nyr_fsh_age = fsh_age %>% distinct(year) %>% nrow(),
  yrs_fsh_age = fsh_age %>% distinct(index) %>% pull(),
  data_fsh_age = fsh_age %>% select(-c(year, index, Source)) %>% as.matrix(),
  
  # Survey age comps
  nyr_srv_age = srv_age %>% distinct(year) %>% nrow(),
  yrs_srv_age = srv_age %>% distinct(index) %>% pull(),
  data_srv_age = srv_age %>% select(-c(year, index, Source)) %>% as.matrix()
)

# Parameters ----

# Parameter starting values
parameters <- list(

  dummy = 0,   # Used for troubleshooting model               

  # Selectivity
  fsh_sel50 = 3.52,
  fsh_sel95 = 5.43,
  srv_sel50 = 3.86,
  srv_sel95 = 5.13, 
  
  # Catchability
  fsh_logq = -3.6726,
  srv1_logq = -3.8929,
  srv2_logq = -2.4019,
  mr_logq = -0.0001,
  
  # Recruitment (rec_devs include a parameter for all ages in the inital yr plus
  # age-2 in all yrs, nyr+nage-2)
  log_rbar = -0.3798,
  log_rec_devs = rinits$rinits,
  
  # Fishing mortality
  log_Fbar = -1.8289,
  log_F_devs = finits$finits
)

# Parameter bounds
lower <- c(             # Lower bounds
  rep(0.1, 4),          # Selectivity
  rep(-15, 4),          # Catchability log_q
  -Inf,                 # Mean recruitment
  rep(-10, nyr+nage-2), # log recruitment deviations
  -Inf,                 # Mean log F
  rep(-15, nyr)         # log F deviations
)

upper <- c(             # Upper bounds
  rep(10, 4),           # Selectivity
  rep(5, 4),            # Catchability q
  Inf,                  # Mean recruitment
  rep(10, nyr+nage-2),  # log recruitment deviations
  Inf,                  # Mean log F  
  rep(15, nyr)          # log F deviations  
)

# Run model ----

# Use map to turn off parameters, either for testing with dummy, phasing, or to
# fix parameter values

# When testing the code
# map <- list(fsh_sel50 = factor(NA), fsh_sel95 = factor(NA),
#           srv_sel50 = factor(NA), srv_sel95 = factor(NA),
#           fsh_logq = factor(NA), srv1_logq = factor(NA),
#           srv2_logq = factor(NA), mr_logq = factor(NA),
#           log_rbar = factor(NA), log_rec_devs = rep(factor(NA), nyr+nage-2),
#           log_Fbar = factor(NA), log_F_devs = rep(factor(NA), nyr))
        
# Compile
compile("mod3.cpp")
dyn.load(dynlib("mod3"))

# # Estimate everything at once
# map <- list(dummy=factor(NA))
# 
# model <- MakeADFun(data, parameters, DLL = "mod3", silent = TRUE, map = map)
# 
# fit <- nlminb(model$par, model$fn, model$gr,
#               control=list(eval.max=100000,iter.max=1000),
#               lower = lower, upper = upper)
# for (i in 1:100){
#   fit <- nlminb(model$env$last.par.best, model$fn, model$gr)}
# best <- model$env$last.par.best
# print(as.numeric(best))
# rep <- sdreport(model)
# print(best)
# print(rep)
# model$report()$priors
# model$report()$catch_like
# model$report()$index_like
# model$report()$age_like
# model$report()$pred_mr
# model$report()$obj_fun
# N <- model$report()$N
# C <- model$report()$C

# Phase -----

# PHASE 1 - estimate mark-recapture catchability (mr_logq)
map <- list(dummy=factor(NA), 
            fsh_sel50 = factor(NA), fsh_sel95 = factor(NA),
            srv_sel50 = factor(NA), srv_sel95 = factor(NA),
            fsh_logq = factor(NA), srv1_logq = factor(NA),
            srv2_logq = factor(NA), # mr_logq = factor(NA),
            log_rbar = factor(NA), log_rec_devs = rep(factor(NA), nyr+nage-2),
            log_Fbar = factor(NA), log_F_devs = rep(factor(NA), nyr))
model <- MakeADFun(data, parameters, DLL = "mod3", silent = TRUE, map = map)
lower <- c(-15)
upper <- c(5)
fit <- nlminb(model$par, model$fn, model$gr,
              control=list(eval.max=100000,iter.max=1000),
              lower = lower, upper = upper)
best <- model$env$last.par.best
print(as.numeric(best))
model$report()$priors
model$report()$catch_like
model$report()$index_like
model$report()$age_like
model$report()$pred_mr

# PHASE 2 - estimate fishery and survey catchabilities and average F (fsh_logq,
# srv1_logq, srv2_logq, logFbar) and mr_logq
map <- list(dummy=factor(NA), 
            fsh_sel50 = factor(NA), fsh_sel95 = factor(NA),
            srv_sel50 = factor(NA), srv_sel95 = factor(NA),
            # fsh_logq = factor(NA), #srv1_logq = factor(NA),
            #srv2_logq = factor(NA), # mr_logq = factor(NA),
            log_rbar = factor(NA), log_rec_devs = rep(factor(NA), nyr+nage-2),
            #log_Fbar = factor(NA), 
            log_F_devs = rep(factor(NA), nyr))
model <- MakeADFun(data, parameters, DLL = "mod3", silent = TRUE, map = map)
lower <- c(rep(-15, 4),-Inf)
upper <- c(rep(5, 4), Inf)
phase2_inits <- c(parameters$fsh_logq, parameters$srv1_logq, 
                  parameters$srv2_logq, as.numeric(best), parameters$log_Fbar)
fit <- nlminb(phase2_inits, model$fn, model$gr,
              control=list(eval.max=100000,iter.max=1000),
              lower = lower, upper = upper)
best <- model$env$last.par.best
print(as.numeric(best))

# PHASE 3 - estimate recruitment deviations (log_rec_devs) and fsh_logq,
# srv1_logq, srv2_logq, log_Fbar, and mr_logq
map <- list(dummy=factor(NA), 
            fsh_sel50 = factor(NA), fsh_sel95 = factor(NA),
            srv_sel50 = factor(NA), srv_sel95 = factor(NA),
            #fsh_logq = factor(NA), #srv1_logq = factor(NA),
            #srv2_logq = factor(NA), # mr_logq = factor(NA),
            log_rbar = factor(NA), # log_rec_devs = rep(factor(NA), nyr+nage-2),
            #log_Fbar = factor(NA), 
            log_F_devs = rep(factor(NA), nyr))
model <- MakeADFun(data, parameters, DLL = "mod3", silent = TRUE, map = map)
lower <- c(rep(-15, 4), rep(-10, nyr+nage-2), -Inf)
upper <- c(rep(5, 4), rep(10, nyr+nage-2), Inf)
phase3_inits <- c(as.numeric(best)[1:4], parameters$log_rec_devs, as.numeric(best)[5])
fit <- nlminb(phase3_inits, model$fn, model$gr,
              control=list(eval.max=100000,iter.max=1000),
              lower = lower, upper = upper)
best <- model$env$last.par.best
print(as.numeric(best))

# PHASE 4 - estimate fishing mortality deviations and selectivities and
# log_rec_devs, fsh_logq, srv1_logq, srv2_logq, log_Fbar and mr_logq
map <- list(dummy=factor(NA), 
            # fsh_sel50 = factor(NA), fsh_sel95 = factor(NA),
            # srv_sel50 = factor(NA), srv_sel95 = factor(NA),
            #fsh_logq = factor(NA), #srv1_logq = factor(NA),
            #srv2_logq = factor(NA), #mr_logq = factor(NA),
            log_rbar = factor(NA)#, log_rec_devs = rep(factor(NA), nyr+nage-2),
            #log_Fbar = factor(NA), 
            #log_F_devs = rep(factor(NA), nyr)
            )
model <- MakeADFun(data, parameters, DLL = "mod3", silent = TRUE, map = map)
lower <- c(rep(0.1, 4), rep(-15, 4), rep(-10, nyr+nage-2), -Inf, rep(-15, nyr))
upper <- c(rep(10, 4), rep(5, 4), rep(10, nyr+nage-2), Inf, rep(15, nyr))
phase4_inits <- c(parameters$fsh_sel50, parameters$fsh_sel95, 
                  parameters$srv_sel50, parameters$srv_sel95,
                  as.numeric(best), parameters$log_F_devs)
fit <- nlminb(phase4_inits, model$fn, model$gr,
              control=list(eval.max=100000,iter.max=1000),
              lower = lower, upper = upper)
best <- model$env$last.par.best
print(as.numeric(best))

# PHASE 5 - Estimate all parameters
map <- list(dummy=factor(NA)#, 
            # fsh_sel50 = factor(NA), fsh_sel95 = factor(NA),
            # srv_sel50 = factor(NA), srv_sel95 = factor(NA),
            #fsh_logq = factor(NA), #srv1_logq = factor(NA),
            #srv2_logq = factor(NA), #mr_logq = factor(NA),
            #log_rbar = factor(NA)#, log_rec_devs = rep(factor(NA), nyr+nage-2),
            #log_Fbar = factor(NA), 
            #log_F_devs = rep(factor(NA), nyr)
            )
model <- MakeADFun(data, parameters, DLL = "mod3", silent = TRUE, map = map)
lower <- c(rep(0.1, 4), rep(-15, 4), -Inf, rep(-10, nyr+nage-2), -Inf, rep(-15, nyr))
upper <- c(rep(10, 4), rep(5, 4), Inf, rep(10, nyr+nage-2), Inf, rep(15, nyr))
phase5_inits <- c(as.numeric(best)[1:8], parameters$log_rbar, as.numeric(best)[9:length(best)])
fit <- nlminb(phase5_inits, model$fn, model$gr,
              control=list(eval.max=100000,iter.max=1000),
              lower = lower, upper = upper)
for (i in 1:5){
  fit <- nlminb(model$env$last.par.best, model$fn, model$gr)}
best <- model$env$last.par.best
print(as.numeric(best))
rep <- sdreport(model)

print(best)
print(rep)
VarCo <- solve(model$he())
# Check for Hessian
print(sqrt(diag(VarCo)))

# Likelihood components
# model$report()$priors
model$report()$catch_like
model$report()$index_like
model$report()$age_like
model$report()$fpen
model$report()$rec_like
model$report()$obj_fun

exp(as.list(rep, what = "Estimate")$fsh_logq)
exp(as.list(rep, what = "Estimate")$srv1_logq)
exp(as.list(rep, what = "Estimate")$srv2_logq)
exp(as.list(rep, what = "Estimate")$mr_logq)
as.list(rep, what = "Std")

# Plot age comps ----

pred_fsh_age <- as.data.frame(model$report()$pred_fsh_age)
names(pred_fsh_age) <- as.character(rec_age:plus_group)
pred_fsh_age %>% 
  mutate(Source = "Fishery",
         index = data$yrs_fsh_age) -> pred_fsh_age

pred_srv_age <- as.data.frame(model$report()$pred_srv_age)
names(pred_srv_age) <- as.character(rec_age:plus_group)
pred_srv_age %>% 
  mutate(Source = "Survey",
         index = data$yrs_srv_age) -> pred_srv_age

# Reshape age comp observations and predictions into long format, calculate
# residuals and prep results for plotting
age %>% 
  gather("age", "obs", 2:plus_group+2) %>%
  left_join(
    bind_rows(pred_fsh_age, pred_srv_age) %>% 
      gather("age", "pred", 1:41),
    by = c("Source", "index", "age")) %>% 
  group_by(Source) %>% 
  mutate(resid = obs - pred,
         # Get standardized residual (mean of 0, sd of 1)
         std_resid = resid / sd(resid),
         # Pearson's residual
         pearson = resid / sqrt(var(pred)),
         # positive or negative
         `Model performance` = ifelse(std_resid >= 0, "Observed greater than estimated",
                                      ifelse(is.na(obs), "",
                                             "Observed less than estimated")),
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
                                 "39", "40", "41", "42+")))  -> agecomps

# Custom axes
axis <- tickr(agecomps, year, 5)
age_labs <- c("2", "", "", "", "6", "", "", "", "10", "", "", "", "14", "",
           "", "", "18", "", "", "", "22", "", "", "", "26", "",
           "", "", "30", "", "", "", "34", "", "", "", "38", "",
           "", "", "42+") 

ggplot(agecomps, aes(x = Age, y = year, size = std_resid,
               fill = `Model performance`)) + 
  # geom_hline(yintercept = seq(2000, 2015, by = 5), colour = "grey", linetype = 3, alpha = 0.7) +  
  geom_point(shape = 21, colour = "black") +
  scale_size(range = c(0, 4.5)) +
  facet_wrap(~ Source) +
  labs(x = '\nAge', y = '') +
  guides(size = FALSE) +
  scale_fill_manual(values = c("black", "white")) +
  scale_x_discrete(breaks = unique(agecomps$Age), labels = age_labs) +
  scale_y_continuous(breaks = axis$breaks, labels = axis$labels) +
  theme(legend.position = "bottom")

ggsave("agecomps_residplot.png", dpi = 300, height = 7, width = 8, units = "in")

# Barplot age comps ----

# Fishery
ggplot(agecomps %>% filter(Source == "Fishery")) +
  geom_bar(aes(x = Age, y = obs), 
           stat = "identity", colour = "grey", fill = "lightgrey",
           width = 0.8, position = position_dodge(width = 0.5)) +
  geom_line(aes(x = Age, y = pred, group = 1), size = 0.6) +
  facet_wrap(~ year, dir = "v", ncol = 5) +
  scale_x_discrete(breaks = unique(agecomps$Age), labels = age_labs) +
  labs(x = '\nAge', y = 'Proportion-at-age\n') 

ggsave("fsh_agecomps_barplot.png", dpi = 300, height = 6, width = 7, units = "in")

# Survey
agecomps %>% filter(Source == "Survey") %>% 
  ggplot() +
  geom_bar(aes(x = Age, y = obs), 
           stat = "identity", colour = "grey", fill = "lightgrey",
           width = 0.8, position = position_dodge(width = 0.5)) +
  geom_line(aes(x = Age, y = pred, group = 1), size = 0.6) +
  facet_wrap(~ year, dir = "v", ncol = 3) +
  scale_x_discrete(breaks = unique(agecomps$Age), labels = age_labs) +
  labs(x = '\nAge', y = 'Proportion-at-age\n')

ggsave("srv_agecomps_barplot.png", dpi = 300, height = 8, width = 7, units = "in")

# Plot selectivity ----

agecomps %>% 
  ungroup %>% 
  distinct(age, Age) %>% 
  mutate(age = as.numeric(age)) %>% 
  arrange(age) %>% 
  mutate(Fishery = model$report()$fsh_sel,
         Survey = model$report()$srv_sel) %>% 
  gather("Source", "sel", c(Fishery, Survey)) %>% 
  filter(age <= 15) -> sel

ggplot(sel, aes(x = Age, y = sel, colour = Source, 
                shape = Source, lty = Source, group = Source)) +
  geom_point() +
  geom_line() +
  scale_colour_grey() +
  labs(y = "Selectivity\n", x = NULL, 
       colour = NULL, lty = NULL, shape = NULL) +
  theme(legend.position = c(.7, .4)) 

ggsave("selectivity.png", dpi = 300, height = 4, width = 4, units = "in")

# Plot time series ----

# Catch 
ts$pred_catch <- model$report()$pred_catch
axis <- tickr(ts, year, 5)
ggplot(ts, aes(x = year)) +
  geom_point(aes(y = catch)) +
  geom_line(aes(y = pred_catch), colour = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "\n\nCatch\n(round x100 mt)") -> p_catch

# Fishery cpue
fsh_cpue$pred_fsh_cpue <- model$report()$pred_fsh_cpue
ggplot(fsh_cpue, aes(x = year)) +
  geom_point(aes(y = fsh_cpue)) +
  geom_line(aes(y = pred_fsh_cpue), colour = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "\n\nFishery CPUE\n(round kg/hook)") -> p_fsh

# Survey cpue
ts %>% 
  select(year, contains("srv")) %>% 
  gather("survey", "obs", c("srv1_cpue", "srv2_cpue")) %>%
  na.omit() %>%
  mutate(survey = ifelse(survey == "srv1_cpue", "1-hr soak", "3+hr soak"),
         pred = c(model$report()$pred_srv1_cpue,
                  model$report()$pred_srv2_cpue)) -> srv

ggplot(srv, aes(x = year)) +
  geom_point(aes(y = obs, shape = survey)) +
  geom_line(aes(y = pred, group = survey), colour = "grey") +
  geom_vline(xintercept = 1997, linetype = 2, colour = "grey") +
  # scale_y_continuous(limits = c(0.05, 0.45)) +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(y = "\n\nSurvey CPUE\n(number/hook)", x = NULL, shape = NULL) +
  theme(legend.position = c(.1, .8)) -> p_srv

# Mark recapture 
mr %>% 
  mutate(pred_mr = model$report()$pred_mr) %>% 
  select(year, pred_mr) %>% 
  right_join(ts %>%
              select(year, mr) %>%
              mutate(pred_mr_all = model$report()$pred_mr_all)) -> mr_plot
              
ggplot(mr_plot, aes(x = year)) +
  geom_point(aes(y = mr)) +
  geom_line(aes(y = pred_mr, group = 1), colour = "grey") +
  # geom_point(aes(y = pred_mr), colour = "grey") +
  geom_line(aes(y = pred_mr_all, group = 1), lty = 2, colour = "grey") +
  scale_x_continuous( breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "\n\nAbundance\n(millions)") -> p_mr

plot_grid(p_catch, p_fsh, p_srv, p_mr, ncol = 1, align = 'hv', 
          labels = c('(A)', '(B)', '(C)', '(D)'))

ggsave(paste0("pred_abd_indices.png"), 
       dpi=300, height=7, width=6, units="in")

# Resids for time series ----
ts %>% 
  mutate(catch_resid = catch - pred_catch,
         catch_sresid = catch_resid / sd(catch_resid)) -> ts

ggplot(ts, aes(x = year, y = catch_sresid)) + 
  geom_hline(yintercept = 0, colour = "grey", size = 1) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = catch_resid), 
               size = 0.2, colour = "grey") +
  geom_point() +
  labs(x = "", y = "\n\nCatch\nresiduals") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) -> r_catch

# Fishery cpue resids
fsh_cpue %>% 
  mutate(fsh_cpue_resid = fsh_cpue - pred_fsh_cpue,
         fsh_cpue_sresid = fsh_cpue_resid / sd(fsh_cpue_resid)) -> fsh_cpue

ggplot(fsh_cpue, aes(x = year, y = fsh_cpue_sresid)) + 
  geom_hline(yintercept = 0, colour = "grey", size = 1) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = fsh_cpue_resid), 
               size = 0.2, colour = "grey") +
  geom_point() +
  labs(x = "", y = "\n\nFishery CPUE\nresiduals") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) -> r_fsh

# Survey cpues resids
srv %>% 
  mutate(srv_cpue_resid = obs - pred,
         srv_cpue_sresid = srv_cpue_resid / sd(srv_cpue_resid)) -> srv

ggplot(srv, aes(x = year, y = srv_cpue_sresid, shape = survey)) + 
  geom_hline(yintercept = 0, colour = "grey", size = 1) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = srv_cpue_resid), 
               size = 0.2, colour = "grey") +
  geom_point() +
  labs(x = "", y = "\n\nSurvey CPUE\nresiduals", shape = NULL) +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  theme(legend.position = "none") +
  geom_vline(xintercept = 1997, linetype = 2, colour = "grey") -> r_srv

# Mark-recapture abundance estimate resids
mr_plot %>% 
  mutate(mr_resid = mr - pred_mr,
         mr_sresid = mr_resid / sd(mr_resid)) -> mr_plot

ggplot(mr_plot, aes(x = year, y = mr_sresid)) + 
  geom_hline(yintercept = 0, colour = "grey", size = 1) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = mr_resid), 
               size = 0.2, colour = "grey") +
  geom_point() +
  labs(x = "", y = "\n\nMR abundance\nresiduals\n") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) -> r_mr

plot_grid(r_catch, r_fsh, r_srv, r_mr, ncol = 1, align = 'hv', 
          labels = c('(A)', '(B)', '(C)', '(D)'))

ggsave(paste0("presid_abd_indices.png"), 
       dpi=300, height=7, width=6, units="in")

# Plot derived variables ----
ts %>% 
  mutate(Fmort = model$report()$Fmort,
         pred_rec = model$report()$pred_rec,
         biom = model$report()$biom,
         expl_biom = model$report()$expl_biom,
         vuln_abd = model$report()$vuln_abd,
         spawn_biom = model$report()$spawn_biom,
         exploit = catch / expl_biom) -> ts

p <- ggplot(ts, aes(x = year)) +
  scale_x_continuous( breaks = axis$breaks, labels = axis$labels)

# Recruitment
p + geom_point(aes(y = pred_rec)) +
  geom_line(aes(y = pred_rec, group = 1)) +
  labs(x = "", y = "\n\nAge-2 recruits\n(millions)") -> p_rec

# Total biomass
p + geom_point(aes(y = biom)) +
  geom_line(aes(y = biom, group = 1)) +
  labs(x = "", y = "\n\nTotal\nbiomass (mt)") -> p_biom

# Exploitable biomass (to fishery)
p + geom_point(aes(y = expl_biom)) +
  geom_line(aes(y = expl_biom, group = 1)) +
  labs(x = "", y = "\n\nExploitatble\nbiomass (mt)") -> p_ebiom

# Vulnerable abundance (to survey)
p + geom_point(aes(y = vuln_abd)) +
  geom_line(aes(y = vuln_abd, group = 1)) +
  labs(x = "", y = "\n\nVulnerable\nabundance (millions)") -> p_vabd

# Spawning biomass 
p + geom_point(aes(y = spawn_biom)) +
  geom_line(aes(y = spawn_biom, group = 1)) +
  labs(x = "", y = "\n\nSpawning\nbiomass(mt)") -> p_sbiom

plot_grid(p_rec, p_biom, p_ebiom, p_vabd, p_sbiom, ncol = 1, align = 'hv',
          labels = c('(A)', '(B)', '(C)', '(D)', '(E)'))

ggsave(paste0("derived_ts.png"), 
       dpi=300, height=7, width=6, units="in")

# Plot F ----
p + geom_point(aes(y = Fmort)) +
  geom_line(aes(y = Fmort, group = 1)) +
  labs(x = "", y = "Fishing mortality") 

ggsave(paste0("fishing_mort.png"), 
       dpi=300, height=4, width=6, units="in")

