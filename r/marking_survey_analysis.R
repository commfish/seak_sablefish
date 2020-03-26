# Sensitivity analysis on marking survey:

# One of the benefits of transitioning to an integrated statistical catch-at-age
# model is reduced reliance on the marking survey, which forms the foundation of
# the current YPR model. The marking survey did not occur in 2011, 2014, and
# 2016, and is subject to future budget cuts.

# This analysis aims to answer the following questions:
# 1:  What would the ABC have been in past years without a
# survey (2011, 2014, and 2016)?
# 2:  What would the 2020 ABC have been had we not had a marking survey in 2019?
# 3:  What is the impact be on stock status and ABC recommendations if we had
# a bi- or triennial marking survey?

YEAR <- 2019 # most recent year of data

# Directory setup
root <- getwd() # project root
tmb_dat <- file.path(root, "data/tmb_inputs") # location of tmb model data inputs
tmb_path <- file.path(root, "tmb") # location of cpp
tmbfigs <- file.path(root, "figures/tmb") # location where model figs are saved
tmbout <- file.path(root, "output/tmb") # location where model output is saved

source("r/helper.r")
source("r/functions.r")

library(TMB) 
library(tmbstan)
library(shinystan)

ts <- read_csv(paste0(tmb_dat, "/abd_indices_", YEAR, ".csv"))        # time series
age <- read_csv(paste0(tmb_dat, "/agecomps_", YEAR, ".csv"))          # age comps
len <- read_csv(paste0(tmb_dat, "/lencomps_", YEAR, ".csv"))          # len comps
# age <- read_csv(paste0(tmb_dat, "/tuned_agecomps_", YEAR, ".csv"))  # tuned age comps
# len <- read_csv(paste0(tmb_dat, "/tuned_lencomps_", YEAR, ".csv"))  # tunedlen comps
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
inits <- read_csv(paste0(tmb_dat, "/inits_for_", YEAR, ".csv"))
rec_devs_inits <- inits %>% filter(grepl("rec_devs", Parameter)) %>% pull(Estimate)
rec_devs_inits <- c(rec_devs_inits, mean(rec_devs_inits)) # mean for current year starting value
rinit_devs_inits <- inits %>% filter(grepl("rinit_devs", Parameter)) %>% pull(Estimate)
Fdevs_inits <- inits %>% filter(grepl("F_devs", Parameter)) %>% pull(Estimate)
Fdevs_inits <- c(Fdevs_inits, mean(Fdevs_inits)) # mean for current year starting value

# Model dimensions / user inputs
syr <- min(ts$year)                   # model start year
lyr <- YEAR <- max(ts$year)           # end year
nyr <- length(syr:lyr)                # number of years        
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

no_srv_yrs <- ts %>% 
  filter(year %in% c(2011, 2014, 2016)) %>% 
  distinct(year, index) 

out <- list()

for(i in 1:length(no_srv_yrs$year)) {
  
  iter_YEAR <- no_srv_yrs$year[i]
  out[[i]] <- new_YEAR
}


out
# Subsets
mr <- filter(ts, !is.na(mr))
fsh_cpue <- filter(ts, !is.na(fsh_cpue))
srv_cpue <- filter(ts, !is.na(srv_cpue))
fsh_age <- filter(age, Source == "Fishery")
srv_age <- filter(age, Source == "Survey")
fsh_len <- filter(len, Source == "fsh_len")
srv_len <- filter(len, Source == "srv_len")



