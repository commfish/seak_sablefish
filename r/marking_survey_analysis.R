# Sensitivity analysis on marking survey:

# One of the benefits of transitioning to an integrated statistical catch-at-age
# model is reduced reliance on the marking survey, which forms the foundation of
# the current YPR model. The marking survey did not occur in 2011, 2014, and
# 2016, and is subject to future budget cuts.

# This analysis aims to answer the following questions:
# 1:  What would the ABC have been in past years without a
# survey (2011, 2014, and 2016)? What would the 2020 ABC have been had we not had a marking survey in 2019?
# 2:  What is the impact be on stock status and ABC recommendations if we had
# a biennial marking survey?
# 3:  " " triennial marking survey?

# Set up ----

YEAR <- 2019 # most recent year of data

# Directory setup
root <- getwd() # project root
tmb_dat <- file.path(root, "data/tmb_inputs") # location of tmb model data inputs
tmb_path <- file.path(root, "tmb") # location of cpp
tmbfigs <- file.path(root, "figures/tmb") # location where model figs are saved
tmbout <- file.path(root, "output/tmb") # location where model output is saved
mr_sens_dir <- file.path(tmbout, "sensitivity_marking_survey") # subdirectory for analysis
dir.create(mr_sens_dir, showWarnings = FALSE)

source("r/helper.r")
source("r/functions.r")

library(TMB) 
library(tmbstan)
library(shinystan)

ts <- read_csv(paste0(tmb_dat, "/abd_indices_", YEAR, ".csv"))        # time series
age <- read_csv(paste0(tmb_dat, "/agecomps_", YEAR, ".csv"))          # age comps
len <- read_csv(paste0(tmb_dat, "/lencomps_", YEAR, ".csv"))          # len comps
bio <- read_csv(paste0(tmb_dat, "/maturity_sexratio_", YEAR, ".csv")) # proportion mature and proportion-at-age in the survey
waa <- read_csv(paste0(tmb_dat, "/waa_", YEAR, ".csv"))               # weight-at-age
retention <- read_csv(paste0(tmb_dat, "/retention_probs.csv"))        # retention probability (not currently updated annually. saved from ypr.r)

# Ageing error transition matrix from D. Hanselman 2019-04-18. On To Do list to
# develop one for ADFG. Row = true age, Column = observed age. Proportion
# observed at age given true age.
ageing_error <- scan("data/tmb_inputs/ageing_error_fed.txt", sep = " ") %>% matrix(ncol = 30) %>% t()
rowSums(ageing_error) # should be 1

# Age-length key from D. Hanselman 2019-04-18. On To DO list to develop one for
# ADFG (will need separate onces for fishery and survey).  Proportion at length
# given age. Row = age, Column = length bin
agelen_key_m <- scan("data/tmb_inputs/agelen_key_male.txt", sep = " ", skip = 1) %>% matrix(ncol = 30) %>% t()
rowSums(agelen_key_m) # should all = 1
agelen_key_f <- scan("data/tmb_inputs/agelen_key_fem.txt", sep = " ", skip = 1) %>% matrix(ncol = 30) %>% t()
rowSums(agelen_key_f) 

# Starting values
inits <- read_csv(paste0(tmbout, "/tmb_allparams_mle_", YEAR, ".csv"))
rec_devs_inits <- inits %>% filter(grepl("rec_devs", Parameter)) %>% pull(Estimate)
rinit_devs_inits <- inits %>% filter(grepl("rinit_devs", Parameter)) %>% pull(Estimate)
Fdevs_inits <- inits %>% filter(grepl("F_devs", Parameter)) %>% pull(Estimate)

# Model dimensions / user inputs
syr <- min(ts$year)                   # model start year
rec_age <- min(waa$age)               # recruitment age                  
plus_group <- max(waa$age)            # plus group age
nage <- length(rec_age:plus_group)    # number of ages
nlenbin <- length(unique(len$length_bin)) # number of length bins
nsex <- 2                 # single sex or sex-structured
nproj <- 1                # projection years *FLAG* eventually add to cpp file, currently just for graphics
include_discards <- TRUE  # include discard mortality, TRUE or FALSE
tmp_debug <- TRUE         # Temporary debug flag, shut off estimation of selectivity pars

# Model switches
rec_type <- 1     # Recruitment: 0 = penalized likelihood (fixed sigma_r), 1 = random effects
slx_type <- 1     # Selectivity: 0 = a50, a95 logistic; 1 = a50, slope logistic
comp_type <- 0    # Age comp likelihood (not currently developed for len comps): 0 = multinomial, 1 = Dirichlet-multinomial
spr_rec_type <- 1 # SPR equilbrium recruitment: 0 = arithmetic mean, 1 = geometric mean, 2 = median (not coded yet)
M_type <- 0       # Natural mortality: 0 = fixed, 1 = estimated with a prior

# Question 1 ----

# What would the ABC have been in past years without a
# survey (2011, 2014, and 2016)

obj1_dir <- file.path(mr_sens_dir, "obj1_missing_surveys") # subdirectory for analysis
dir.create(obj1_dir, showWarnings = FALSE)

no_srv_yrs <- ts %>% 
  filter(year %in% c(2011, 2014, 2016, YEAR)) %>% 
  distinct(year, index) 

ABC_out <- list()
rep_out <- list()

for(i in 1:length(no_srv_yrs$year)) { # TODO: I had to run each iteration manually

  iter_YEAR <- no_srv_yrs$year[i]
  iter_dir <- file.path(obj1_dir, iter_YEAR) # subdirectory for analysis
  dir.create(iter_dir, showWarnings = FALSE)
  
  lyr <- iter_YEAR           # end year
  nyr <- length(syr:lyr)     # number of years 
  
  # Subsets for make_data()
  iter_ts <- filter(ts, year <= iter_YEAR)
  mr <- filter(iter_ts, !is.na(mr))
  if(iter_YEAR == YEAR) {
    mr <- filter(mr, year != YEAR)
  }
  fsh_cpue <- filter(iter_ts, !is.na(fsh_cpue))
  srv_cpue <- filter(iter_ts, !is.na(srv_cpue))
  fsh_age <- age %>% filter(year <= iter_YEAR & Source == "Fishery")
  srv_age <- age %>% filter(year <= iter_YEAR & Source == "Survey")
  fsh_len <- len %>% filter(year <= iter_YEAR & Source == "fsh_len")
  srv_len <- len %>% filter(year <= iter_YEAR & Source == "srv_len")
  
  # Starting values
  iter_rec_devs_inits <- rep(0, nyr) #rec_devs_inits[1:nyr]
  iter_Fdevs_inits <- rep(0, nyr) #Fdevs_inits[1:nyr]  
  
  # Build TMB objects
  data <- build_data(ts = iter_ts)
  parameters <- build_parameters(rec_devs_inits = iter_rec_devs_inits, Fdevs_inits = iter_Fdevs_inits)
  random_vars <- build_random_vars()
  
  setwd(tmb_path)
  
  # Run model using MLE
  out <- TMBphase(data, parameters, random = random_vars, 
                  model_name = "mod", phase = FALSE, 
                  debug = FALSE)
  
  obj <- out$obj # TMB model object
  opt <- out$opt # fit
  rep <- out$rep # sdreport
  best <- obj$env$last.par.best
  
  # save MLE estimates
  tidyrep <- save_mle(path = iter_dir, save_inits = FALSE, year = iter_YEAR)

  # Get ABC
  ABC <- as.data.frame(obj$report(best)$ABC * 2.20462)
  names(ABC) <- data$Fxx_levels
  ABC <- ABC %>% 
    mutate(year = c(unique(iter_ts$year), max(iter_ts$year)+1)) %>% 
    data.table::melt(id.vars = c("year"), variable.name = "Fxx", value.name = "ABC") %>% 
    filter(Fxx == "0.5" & year == iter_YEAR+1) %>% 
    mutate(missing_MR_srv = iter_YEAR)
  
  # units = "metric" to switch between units. 
  plot_ts(save = TRUE, units = "imperial", plot_variance = FALSE, path = iter_dir, ts = iter_ts)
  # plot_derived_ts(save = FALSE, path = iter_dir, units = "imperial", plot_variance = FALSE, ts = iter_ts)
  plot_derived_ts(save = TRUE, path = iter_dir, units = "imperial", plot_variance = FALSE, ts = iter_ts)
  
  ABC_out[[i]] <- ABC
  rep_out[[i]] <- rep
}

ABC_out
rep_out




