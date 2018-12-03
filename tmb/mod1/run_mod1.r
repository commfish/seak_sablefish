
# Model 1 - base ASA that only includes catch, fishery CPUE, fishery
# weight-at-age, and catch compositions. No sex structure.
# source("r/helper.r")
# source("r/functions.r")
library(tidyverse)
library(TMB)

################################################################################

# Read in data
cm <- read_csv("tmb/mod1/cm_catch_cpue.csv")
cm_agecomps <- read_csv("tmb/mod1/cm_agecomps.csv")
cm_waa <- read_csv("tmb/mod1/cm_waa.csv")
# the labundance_age2plus index, used to initial N matrix 
abd <- read_csv("data/chatham_sablefish_abd_index.csv")
# mean age comps over all years used to get logN starting values/initialize N matrix (length of nyr + nage - 1)
# meancomps <- c(read_csv("tmb/mod1/meancomps.csv"), recursive = TRUE)

# Number of ages
nage <- n_distinct(cm_waa$age)

# Get starting values for log abundance at age
abd %>% 
  # select(year, abundance_age2plus) %>% 
  mutate(abd = abundance_age2plus / 1e4,
         log_abd = log(abd),
         log_naa = log_abd/nage) -> abd

logN <- c(rep(abd$log_naa[1], nage), abd %>% filter(year >= 1981) %>% pull(log_naa))
exp(logN)
# Starting values for F
abd %>% 
  mutate(biomass_kg = biomass_round_lbs_age4plus * 0.453592,
         hr = cm$catch/biomass_kg) -> abd

# Index years
cm %>% 
  mutate(yr_index = year - min(year)) %>% 
  select(year, yr_index) %>% 
  left_join(cm_agecomps, by = "year") %>% 
  na.omit() -> cm_agecomps

# Structure data for TMB - must use same variable names as .cpp
data <- list(
  # Model dimensions
  nyr = length(cm$year),
  nage = length(cm_waa$age),
  cm_comp_nyr = n_distinct(cm_agecomps$year),
  cm_comp_yrs = unique(cm_agecomps$yr_index),
  # Fixed parameters
  M = 0.1,
  sig_catch = 0.05,
  sig_cm_cpue = 0.2,
  omega_comp = 50,
  # Data inputs
  data_cm_waa = cm_waa$weight_kg,
  data_catch = cm$catch,
  data_cm_cpue = cm$cpue,
  data_cm_comp = as.matrix(cm_agecomps[,-c(1,2)]))

# Parameter starting values
parameters <- list(
  dummy = 0, # used for troubleshooting model
  # the log of abundance_age2plus / 1e6 in data/chatham_sablefish_abd_index.csv
  logN = logN,
  cm_sel50 = 3.86,
  cm_sel95 = 5.22, # guestimate from BSAI SAFE 2017 Fig 3.40
  logF = rep(-11.4, data$nyr),#log(rnorm(n = data$nyr, mean = .01, sd = .001)),
  logq = log(0.001))#-1.11


################################################################################
setwd("tmb/mod1")

# When testing the code
#map<-list(logN=rep(factor(NA),length(logN)),cm_sel50=factor(NA),
# cm_sel95=factor(NA),logF=rep(factor(NA),length(logF)),logq=factor(NA))

# Estimate everything
map<-list(dummy=factor(NA),logN=rep(factor(NA),length(logN)),cm_sel50=factor(NA),
          cm_sel95=factor(NA))
compile("mod1.cpp")
dyn.load(dynlib("mod1"))
model <- MakeADFun(data, parameters, DLL="mod1",silent=T,map=map)

# test code - for checking for minimization
xx <- model$fn(model$env$last.par)
#print(model$report())
#cat(model$report()$obj_fun,model$report()$Like1,model$report()$Like2,model$report()$Like3,"\n")

# Actual minimzation (with some "Bonus" parameters from nlminb)
fit <- nlminb(model$par, model$fn, model$gr, control=list(eval.max=100000,iter.max=1000))
best <- model$env$last.par.best
rep <- sdreport(model)
# print(best)
print(rep)

print(model$report()$S)
print(model$report()$N)

pred_cm_agecomps <- as.data.frame(model$report()$pred_cm_comp)
names(pred_cm_agecomps) <- as.character(2:30)

cm$pred_cpue <- model$report()$pred_cm_cpue

cm$pred_catch <- model$report()$pred_catch

cat(model$report()$obj_fun,model$report()$Like1,model$report()$Like2,model$report()$Like3,"\n")
rep <- sdreport(model)
print(summary(rep))

