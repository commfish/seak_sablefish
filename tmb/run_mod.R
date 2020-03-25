
# Sex-structured statistical catch-at-age model that includes catch, fishery and
# survey CPUE, mark-recapture abundance estimates, fishery and survey
# weight-at-age, survey data about maturity-at-age and proportions-at-age, and
# fishery and survey age and length compositions.

# Contact: jane.sullivan1@alaska.gov
# Last updated Feb 2020

# Set up ----

# most recent year of data
YEAR <- 2019

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

# Subsets
mr <- filter(ts, !is.na(mr))
fsh_cpue <- filter(ts, !is.na(fsh_cpue))
srv_cpue <- filter(ts, !is.na(srv_cpue))
fsh_age <- filter(age, Source == "Fishery")
srv_age <- filter(age, Source == "Survey")
fsh_len <- filter(len, Source == "fsh_len")
srv_len <- filter(len, Source == "srv_len")

# Data -----

# Structure data for TMB - must use same variable names as .cpp
data <- list(
  
  # Model dimensions
  nyr = nyr,
  nage = nage,
  nsex = nsex,
  nlenbin = nlenbin,
  lenbin = unique(len$length_bin), 
  
  # Switch recruitment estimation: 0 = penalized likelihood (fixed sigma_r), 1 =
  # random effects
  random_rec = 1,
  
  # Switch for selectivity type: 0 = a50, a95 logistic; 1 = a50, slope logistic
  slx_type = 1,
  
  # Swtich for age composition type (hopefully one day length comps too): 0 =
  # multinomial; 1 = Dirichlet-multinomial
  comp_type = 0,
  
  # Switch for assumption on SPR equilibrium recruitment. 0 = arithmetic mean
  # (same as Federal assessment), 1 = geometric mean, 2 = median (2 not coded
  # yet)
  spr_rec_type = 1,
  
  # Natural mortality
  M_type = 0,  # Switch for natural mortality: fixed = 0, estimated with prior = 1. 
  p_log_M = log(0.1), # Priors for natural mortality (same as 2016-2019 Federal assessment
  p_sigma_M = 0.1,
  
  # Time varying parameters - each vector contains the terminal years of each time block
  fsh_blks = c(14, max(ts$index)), #  fishery selectivity: limited entry in 1985, EQS in 1994 = c(5, 14, max(ts$year))
  srv_blks = c(max(ts$index)), # no breaks survey selectivity
  
  # Discard mortality rate in the directed fishery (currently either 0 or 0.16,
  # borrowed from the halibut fishery)
  dmr = array(data = ifelse(include_discards == TRUE, 0.16, 0), dim = c(nyr, nage, nsex)),

  # Probability of retaining a fish, sex- and age-based
  retention = 
    # 100% retention (assuming no discards)
    if(include_discards == FALSE & nsex == 1) {
      array(data = 1,
            # Number of rows could = time blocks but currently doesn't
            dim = c(1, nage, nsex))
      # Discards, single sex model
    } else if (include_discards == TRUE & nsex == 1) {
      array(data = filter(retention, Sex == "Combined") %>% pull(p),
            dim = c(1, nage, nsex))
    } else { # Discards, sex-structured
      array(data = filter(retention, Sex %in% c("Female","Male")) %>%
              mutate(sex = ifelse(Sex == "Male", 1, 2)) %>% 
              arrange(sex) %>% pull(p), dim = c(1, nage, nsex))
    },
  

  # Fxx levels that correspond with log_spr_Fxx in Parameter section
  Fxx_levels = c(0.35, 0.40, 0.50, 0.60, 0.70),

  # Priors ("p_" denotes prior)
  p_fsh_q = c(exp(-16), exp(-16)),
  sigma_fsh_q = c(1, 1),
  p_srv_q = exp(-17), 
  sigma_srv_q = 1,
  p_mr_q = 1.0,
  sigma_mr_q = 0.01,
  
  # Weights on likelihood components ("wt_" denotes weight) based on weights in Federal model
  wt_catch = 1.0,
  wt_fsh_cpue = 1.0,
  wt_srv_cpue = 1.0,
  wt_mr = 1.0,
  wt_fsh_age = 1.0,
  wt_srv_age = 1.0,
  wt_fsh_len = 1.0,
  wt_srv_len = 1.0,
  wt_rec_like = 2.0,
  wt_fpen = 0.1,
  wt_spr = 100,
  
  # Catch
  data_catch = ts$catch,
  sigma_catch = pull(ts, sigma_catch),
  
  # Mark-recapture estimates
  nyr_mr = n_distinct(mr, mr),
  yrs_mr = mr %>% distinct(index) %>% pull(),
  data_mr = pull(mr, mr),
  sigma_mr = rep(0.05, n_distinct(mr, mr)), #mr %>% pull(sigma_mr), 
  
  # Fishery CPUE
  nyr_fsh_cpue = fsh_cpue %>% n_distinct(fsh_cpue),
  yrs_fsh_cpue = fsh_cpue %>% distinct(index) %>% pull(),
  data_fsh_cpue = pull(fsh_cpue, fsh_cpue),
  sigma_fsh_cpue = pull(fsh_cpue, sigma_fsh_cpue),
  
  # Survey CPUE 
  nyr_srv_cpue = srv_cpue %>% n_distinct(srv_cpue),
  yrs_srv_cpue = srv_cpue %>% distinct(index) %>% pull(),
  data_srv_cpue = pull(srv_cpue, srv_cpue),
  sigma_srv_cpue = pull(srv_cpue, sigma_srv_cpue),
  
  # Timing in month fractions
  spawn_month = 2/12, # Feb
  srv_month = 7/12,   # Jul
  fsh_month = 8/12,   # Aug
  
  # Proportion mature-at-age - flexible to vary over time if you wanted, where
  # rows would be time blocks or annual variations
  prop_mature = matrix(data = rep(bio$prop_mature, 1),
                       ncol = nage, byrow = TRUE),
  
  # Vector of prop_fem *Need both prop_fem and sex_ratio to accommodate single
  # sex and sex-structured models*. In sex-structured version, the N matrix is
  # already split so you don't need prop_fem in spawning biomass calculation (it
  # will be a vector of 1's).
  prop_fem = if (nsex == 1) {bio$prop_fem} else { rep(1, nage) } ,

  # Sex ratio in the survey (matrix of 1's if single sex model so that N matrix
  # doesn't get split up by sex ratio)
  sex_ratio = if (nsex == 1) {matrix(data = 1, ncol = nage)
    } else {matrix(data =  c(c(1 - bio$prop_fem), # Proprtion male
                               bio$prop_fem), # Proportion female
                     ncol = nage, byrow = TRUE)},

  # Weight-at-age: currently weight-at-age is averaged over all years. If for
  # whatever reason you want to include multiple time periods for weight-at-age,
  # you would change the number of rows from 1 to the number of time periods or
  # years
  data_fsh_waa =
    if (nsex == 1) { # Single sex model
      filter(waa, Source == "Fishery (sexes combined)") %>%
        pull(weight) %>%  matrix(ncol = nage, nrow = nsex)  %>%
        array(dim = c(1, nage, nsex))} else {
          # Sex-structured - males in first matrix, females in second
          filter(waa, Source %in% c("Fishery (males)", "Fishery (females)")) %>%
            arrange(desc(Sex)) %>% pull(weight) %>% matrix(ncol = nage, nrow = nsex)  %>%
            array(dim = c(1, nage, nsex))},
  
  # Survey weight-at-age used for everything except predicting catch
  data_srv_waa = 
    if (nsex == 1) { # Single sex model
      filter(waa, Source == "Survey (sexes combined)") %>% 
    pull(weight) %>%  matrix(ncol = nage, nrow = nsex)  %>% 
    array(dim = c(1, nage, nsex))} else {
     # Sex-structured - males in first matrix, females in second
      filter(waa, Source %in% c("Survey (males)", "Survey (females)")) %>% 
         arrange(desc(Sex)) %>% pull(weight) %>% matrix(ncol = nage, nrow = nsex)  %>% 
        array(dim = c(1, nage, nsex))},
  
  # Fishery age comps
  nyr_fsh_age = fsh_age %>% distinct(year) %>% nrow(),
  yrs_fsh_age = fsh_age %>% distinct(index) %>% pull(),
  data_fsh_age = fsh_age %>% select(-c(year, index, Source, n, effn)) %>% as.matrix(),
  n_fsh_age = pull(fsh_age, n),        # total sample size
  effn_fsh_age = pull(fsh_age, effn),  # effective sample size, currently sqrt(n_fsh_age)
  
  # Survey age comps
  nyr_srv_age = srv_age %>% distinct(year) %>% nrow(),
  yrs_srv_age = srv_age %>% distinct(index) %>% pull(),
  data_srv_age = srv_age %>% select(-c(year, index, Source, n, effn)) %>% as.matrix(),
  n_srv_age = pull(srv_age, n),        # total sample size
  effn_srv_age = pull(srv_age, effn),  # effective sample size, currently sqrt(n_srv_age)
  
  # Fishery length comps
  nyr_fsh_len = length(unique(fsh_len$year)),
  yrs_fsh_len = fsh_len %>% distinct(index) %>% pull(),
  data_fsh_len =
    if (nsex == 1) { # Single sex model
      array(data = fsh_len %>% filter(Sex == "Sex combined") %>% pull(proportion),
            dim = c(length(unique(fsh_len$year)), nlenbin, nsex)) } else {
              # Sex-structured (make sure males are first)
              array(data = fsh_len %>% filter(Sex != "Sex combined") %>%
                      arrange(desc(Sex), year) %>% pull(proportion),
                    dim = c(length(unique(fsh_len$year)), nlenbin, nsex))},

  n_fsh_len =
    if (nsex == 1) { # Single sex model
      array(data = fsh_len %>% filter(Sex == "Sex combined") %>% distinct(year, Sex, n) %>% pull(n),
            dim = c(length(unique(fsh_len$year)), 1, nsex)) } else {
              # Sex-structured (make sure males are first)
              array(data = fsh_len %>% filter(Sex != "Sex combined") %>% 
                      distinct(year, Sex, n) %>%
                      arrange(desc(Sex)) %>% pull(n),
                    dim = c(length(unique(fsh_len$year)), 1, nsex))},

  effn_fsh_len =
    if (nsex == 1) { # Single sex model
      array(data = fsh_len %>% filter(Sex == "Sex combined") %>% distinct(year, Sex, effn) %>% pull(effn),
            dim = c(length(unique(fsh_len$year)), 1, nsex)) } else {
              # Sex-structured (make sure males are first)
              array(data = fsh_len %>% filter(Sex != "Sex combined") %>%
                      distinct(year, Sex, effn) %>% 
                      arrange(desc(Sex)) %>% pull(effn),
                    dim = c(length(unique(fsh_len$year)), 1, nsex))},

  # Survey length comps
  nyr_srv_len = length(unique(srv_len$year)),
  yrs_srv_len = srv_len %>% distinct(index) %>% pull(),
  data_srv_len =
    if (nsex == 1) { # Single sex model
      array(data = srv_len %>% filter(Sex == "Sex combined") %>% pull(proportion),                              
            dim = c(length(unique(srv_len$year)), nlenbin, nsex)) } else {
              # Sex-structured (make sure males are first)
              array(data = srv_len %>% filter(Sex != "Sex combined") %>%
                      arrange(desc(Sex)) %>% pull(proportion),
                    dim = c(length(unique(srv_len$year)), nlenbin, nsex))},
  n_srv_len =
    if (nsex == 1) { # Single sex model
      array(data = srv_len %>% filter(Sex == "Sex combined") %>% distinct(year, Sex, n) %>% pull(n),
            dim = c(length(unique(srv_len$year)), 1, nsex)) } else {
              # Sex-structured (make sure males are first)
              array(data = srv_len %>% filter(Sex != "Sex combined") %>%
                      distinct(year, Sex, n) %>% 
                      arrange(desc(Sex)) %>% pull(n),
                    dim = c(length(unique(srv_len$year)), 1, nsex))},
  effn_srv_len =
    if (nsex == 1) { # Single sex model
      array(data = srv_len %>% filter(Sex == "Sex combined") %>%  distinct(year, Sex, effn) %>% pull(effn),
            dim = c(length(unique(srv_len$year)), 1, nsex)) } else {
              # Sex-structured (make sure males are first)
              array(data = srv_len %>% filter(Sex != "Sex combined") %>%
                      distinct(year, Sex, effn) %>% 
                      arrange(desc(Sex)) %>% pull(effn),
                    dim = c(length(unique(srv_len$year)), 1, nsex))},

  # Ageing error matrix
  ageing_error = ageing_error,

  # Fishery age-length transition matrix *FLAG* currently only using the
  # survey-based matrices from the Feds. Use these as a placeholder for now.
  agelen_key_fsh =
    if (nsex == 1) { # Single sex model
      array(data = c(agelen_key_m), # only have sex-specific, use male curve as placeholder
            dim = c(nage, nage, nsex))} else {
              # Sex-structured (make sure males are first)
              array(data = c(agelen_key_m, agelen_key_f),
                    dim = c(nage, nage, nsex))},

  # Survey age-length transition matrix
  agelen_key_srv =
    if (nsex == 1) { # Single sex model
      array(data = c(agelen_key_m), # only have sex-specific, use male curve as placeholder
            dim = c(nage, nage, nsex))} else {
      # Sex-structured (make sure males are first)
                      array(data = c(agelen_key_m, agelen_key_f),
                            dim = c(nage, nage, nsex))}
)

# Parameters ----

# Parameter starting values
parameters <- list(
  
  dummy = 0,   # Used for troubleshooting model               
  
  log_M = log(0.1),  # M_type = 0 is fixed, 1 is estimated
  
  # Fishery selectivity - starting values developed using NOAA selectivity
  # curves see data/NOAA_sablefish_selectivities_2017_jys.xlxs
  log_fsh_slx_pars = 
    # Logistic with a50 and a95, data$slx_type = 0, single sex model
    if(data$slx_type == 0 & nsex == 1) {
      array(data = c(log(4.05), log(3.99), # Sexes combined
                     log(5.30), log(5.20)),
            dim = c(length(data$fsh_blks), 2, nsex)) # 2 = npar for this slx_type

    # Logistic with a50 and a95, data$slx_type = 0, sex-structured model
    } else if (data$slx_type == 0 & nsex == 2) {
      array(data = c(log(4.19), log(5.12), # Male
                     log(5.50), log(6.30),
                     log(3.91), log(2.87), # Female
                     log(5.20), log(4.15)),
            dim = c(length(data$fsh_blks), 2, nsex)) # 2 = npar for this slx_type

    # Logistic with a50 and slope, data$slx_type = 1, single sex model
    } else if (data$slx_type == 1 & nsex == 1) {
      array(data = c(log(4.05), log(3.99),
                     log(2.29), log(2.43)),
            dim = c(length(data$fsh_blks), 2, nsex)) # 2 = npar for this slx_type

    } else {  # Logistic with a50 and slope, data$slx_type = 1, sex-structured model
      array(data = c(log(5.12), log(4.22), # male
                     log(2.57), log(2.61),
                     log(2.87), log(3.86), # female
                     log(2.29), log(2.61)),
            dim = c(length(data$fsh_blks), 2, nsex)) }, # 2 = npar for this slx_type

  # Survey selectivity - starting values developed using NOAA selectivity curves
  log_srv_slx_pars = 
    # Logistic with a50 and a95, data$slx_type = 0, single sex model
    if(data$slx_type == 0 & nsex == 1) {
      array(data = c(rep(log(3.74), length(data$srv_blks)),
                     rep(log(5.20), length(data$srv_blks))),
            dim = c(length(data$srv_blks), 2, nsex)) # 2 = npar for this slx_type 
      
    # Logistic with a50 and a95, data$slx_type = 0, sex-structured model
    } else if (data$slx_type == 0 & nsex == 2) {
      array(data = c(rep(log(3.73), length(data$srv_blks)), # male
                     rep(log(5.20), length(data$srv_blks)),
                     rep(log(3.74), length(data$srv_blks)), # female
                     rep(log(5.20), length(data$srv_blks))),
            dim = c(length(data$srv_blks), 2, nsex)) # 2 = npar for this slx_type 
      
    # Logistic with a50 and slope, data$slx_type = 1, single sex model
    } else if (data$slx_type == 1 & nsex == 1) {
      array(data = c(rep(log(3.74), length(data$srv_blks)),
                     rep(log(1.96), length(data$srv_blks))),
            dim = c(length(data$srv_blks), 2, nsex)) # 2 = npar for this slx_type 

    # Logistic with a50 and slope, data$slx_type = 1, sex-structured model
    } else { 
      array(data = c(rep(log(3.72), length(data$srv_blks)), # male
                     rep(log(2.21), length(data$srv_blks)),
                     rep(log(3.75), length(data$srv_blks)), # female
                     rep(log(2.21), length(data$srv_blks))),
            dim = c(length(data$srv_blks), 2, nsex)) }, # 2 = npar for this slx_type
  
  # Catchability
  fsh_logq = inits %>% filter(grepl("fsh_logq", Parameter)) %>% pull(Estimate), 
  srv_logq = inits %>% filter(grepl("srv_logq", Parameter)) %>% pull(Estimate),
  mr_logq = inits %>% filter(grepl("mr_logq", Parameter)) %>% pull(Estimate),
  
  # Log mean recruitment and deviations (nyr)
  log_rbar = inits %>% filter(Parameter == "log_rbar") %>% pull(Estimate), 
  log_rec_devs = rec_devs_inits,

  # Log mean initial numbers-at-age and deviations (nage-2)
  log_rinit = inits %>% filter(Parameter == "log_rinit") %>% pull(Estimate), 
  log_rinit_devs = rinit_devs_inits,

  # Variability in rec_devs and rinit_devs
  log_sigma_r = log(0.6), #log(1.2), # Federal value of 1.2 on log scale
  
  # Fishing mortality
  log_Fbar = inits %>% filter(Parameter == "log_Fbar") %>% pull(Estimate),
  log_F_devs = Fdevs_inits,
  
  # SPR-based fishing mortality rates, i.e. the F at which the spawning biomass
  # per recruit is reduced to xx% of its value in an unfished stock
  log_spr_Fxx = inits %>% filter(grepl("spr_Fxx", Parameter)) %>% pull(Estimate), # F35, F40, F50, F60, F70

  # Parameter related to effective sample size for Dirichlet-multinomial
  # likelihood used for composition data. Default of 10 taken from LIME model by
  # M. Rudd. Estimated in log-space b/c it can only be positive.
  log_fsh_theta = log(10),   
  log_srv_theta = log(10)
)

# If you have a single sigma_r that governs the rinits and the rec_devs, in
# MakeADFun() the random = c("rinits", "rec_devs") not random = "sigma_r". When
# you're building the map for phases, it's sigma_r that gets muted as an "NA" if
# it's not estimated as a random effect

# Setup random effects
random_vars <- c()
if (data$random_rec == 1) {
  random_vars <- c("log_rec_devs", "log_rinit_devs")
}

# Fix parameter if sigma_r is not estimated via random effects
if(data$random_rec == 0) {
  random_vars <- NULL #rep(factor(NA),2)
}

# Run model ----

setwd(tmb_path)

# Four ways to run model:
# (1) MLE in phases (like ADMB). Code defined by build_phases() in functions.R.
# Use TMBphase(phase = TRUE)
# (2) MLE without phased optimation (phase = FALSE)
# (3) Bayesian using tmbstan and the no-U-turn sampler (NUTS): Run step 2 first
# to build TMB object, map, and bounds
# (4) Debug mode with debug = TRUE (will need to uncomment out obj_fun = dummy * dummy; )

# MLE, phased estimation (phase = TRUE) or not (phase = FALSE)
out <- TMBphase(data, parameters, random = random_vars, 
                model_name = "mod", phase = FALSE, 
                debug = FALSE)

obj <- out$obj # TMB model object
opt <- out$opt # fit
rep <- out$rep # sdreport
lower <- out$lower # bounds
upper <- out$upper

rep
best <- obj$env$last.par.best
# TODO: run model using TMBhelper::fit_tmb() and run Newton steps to reduce
# final gradient

# MLE results ----

# MLE parameter estimates and standard errors in useable format
tidyrep <- tidy(summary(rep))
names(tidyrep) <- c("Parameter", "Estimate", "se")
key_params <- filter(tidyrep, !grepl('devs', Parameter)) # "Key" parameters (exclude devs)
write_csv(key_params, paste0(tmbout, "/tmb_params_mle_", YEAR, ".csv"))
write_csv(tidyrep, paste0(tmbout, "/tmb_allparams_mle_", YEAR, ".csv"))

# Save starting values for next year
write_csv(tidyrep, paste0(tmb_dat, "/inits_for_", YEAR+1, ".csv"))

# obj$report(best)$pred_landed * 2204.62
# obj$report(best)$pred_catch * 2204.62
# obj$report(best)$pred_wastage * 2204.62
# obj$report(best)$ABC * 2204.62 
# obj$report(best)$SB * 2204.62 
# obj$report(best)$Fxx
# 
# exp(as.list(rep, what = "Estimate")$fsh_logq)
# exp(as.list(rep, what = "Estimate")$srv_logq)
# exp(as.list(rep, what = "Estimate")$mr_logq)
# exp(as.list(rep, what = "Estimate")$log_rbar)
# exp(as.list(rep, what = "Estimate")$log_rinit)
# exp(as.list(rep, what = "Estimate")$log_Fbar)

# variance-covriance
VarCo <- solve(obj$he())
# Check  Hessian is pos def
print(sqrt(diag(VarCo)))

# Not totally clear on this...
# D <- obj$he()
# D <- as.data.frame(D)
# names(D) <- rep$par.fixed %>% names
# write_csv(D, paste0(tmbout, "/Dmatrix_", YEAR, ".csv"))

# MLE likelihood components
obj$report(best)$priors
obj$report(best)$prior_M

dat_like <- sum(obj$report(best)$catch_like,
                obj$report(best)$index_like[1], 
                obj$report(best)$index_like[2],
                obj$report(best)$index_like[3],
                obj$report(best)$age_like[1], obj$report(best)$age_like[2],
                sum(obj$report(best)$fsh_len_like),  sum(obj$report(best)$srv_len_like))

sum(obj$report()$catch_like,
    obj$report()$index_like[1], 
    obj$report()$index_like[2],
    obj$report()$index_like[3],
    obj$report()$age_like[1], obj$report()$age_like[2],
    sum(obj$report()$fsh_len_like),  sum(obj$report()$srv_len_like))

like_sum <- data.frame(like = c("Catch", 
                                "Fishery CPUE", 
                                "Survey CPUE", 
                                "Mark-recapture abundance",
                                "Fishery ages",
                                "Survey ages", 
                                "Fishery lengths",
                                "Survey lengths", 
                                "Data likelihood",
                                "Fishing mortality penalty",
                                "Recruitment likelihood",
                                "SPR penalty",
                                "Sum of catchability priors",
                                "Total likelihood"),
                       value = c(obj$report(best)$catch_like,
                                 obj$report(best)$index_like[1], 
                                 obj$report(best)$index_like[2],
                                 obj$report(best)$index_like[3],
                                 obj$report(best)$age_like[1], 
                                 obj$report(best)$age_like[2],
                                 sum(obj$report(best)$fsh_len_like),
                                 sum(obj$report(best)$srv_len_like),
                                 dat_like,
                                 obj$report(best)$fpen,
                                 obj$report(best)$rec_like,
                                 obj$report(best)$spr_pen,
                                 sum(obj$report(best)$priors),
                                 obj$report(best)$obj_fun)) %>% 
  mutate(tot = dat_like,
         perc = (value / tot) * 100) %>% 
  select(-tot) %>% 
  rename(`Likelihood component` = like, Likelihood = value, `Percent of data likelihood` = perc)

write_csv(like_sum, paste0(tmbout, "/likelihood_components_", YEAR, ".csv"))

# MLE figs ----

# Fits to abundance indices, derived time series, and F. Use units = "imperial" or
# "metric" to switch between units. 
plot_ts(save = TRUE, units = "imperial", plot_variance = FALSE, path = tmbfigs)
plot_ts_resids(save = TRUE, path = tmbfigs)
plot_derived_ts(save = TRUE, path = tmbfigs, units = "imperial", plot_variance = FALSE)
plot_F()

agecomps <- reshape_age()
plot_sel() # Selectivity
plot_age_resids() # Fits to age comps
barplot_age("Survey")
barplot_age("Fishery")

lencomps <- reshape_len()
plot_len_resids()
barplot_len("Survey", sex = "Female")
barplot_len("Survey", sex = "Male")
barplot_len("Fishery", sex = "Female")
barplot_len("Fishery", sex = "Male")

# Compare current ABC with past harvest ----

ABC <- as.data.frame(obj$report(best)$ABC * 2.20462)
names(ABC) <- data$Fxx_levels
ABC <- ABC %>% 
  mutate(year = c(unique(ts$year), max(ts$year)+1)) %>% 
  data.table::melt(id.vars = c("year"), variable.name = "Fxx", value.name = "ABC")

ABC %>% filter(Fxx == "0.5" & year == YEAR+1)

wastage <- as.data.frame(obj$report()$wastage * 2.20462)
names(wastage) <- data$Fxx_levels
wastage <- wastage %>% 
  mutate(year = c(unique(ts$year), max(ts$year)+1)) %>% 
  data.table::melt(id.vars = c("year"), variable.name = "Fxx", value.name = "discarded")

wastage %>% filter(Fxx == "0.5" & year == YEAR+1)

retro_mgt <- ABC %>% 
  left_join(wastage %>% filter(year != YEAR+1)) %>% 
  left_join(ts %>% select(year, landed = catch) %>% 
              mutate(landed = landed * 2204.62)) %>% 
  # filter(Fxx == "0.5") %>% 
  melt(id.vars = c("year", "Fxx", "ABC")) %>% 
  rename(Fspr = Fxx)

df <- data.frame(year = 2000:YEAR+1)
xaxis <- tickr(df, year, 5)
ggplot() +
  geom_area(data = retro_mgt %>% filter(year > 2000 & Fspr == "0.5"),
            aes(x = year, y = value, fill = variable), 
            position = "stack") +
  scale_fill_grey() +
  geom_line(data = retro_mgt %>% filter(year > 2000 & Fspr %in% c("0.4", "0.5", "0.6")), #"0.5"),
            aes(x = year, y = ABC, linetype = Fspr)) +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  scale_y_continuous(label = scales::comma) +
  labs(x = NULL, y = "Catch (round lb)", fill = "Catch")
  
ggsave(filename = paste0(tmbfigs, "/catch_ABC_Fspr_", YEAR, ".png"), 
       dpi = 300, height = 4, width = 6, units = "in")


# Last years values
LYR_ABC <- 1058037 # ABC for YEAR
LYR_wastage <- 19142
LYR_F <- 0.0632

# Current values
(maxABC <- ABC %>% filter(Fxx == "0.5" & year == YEAR+1) %>% pull(ABC))
(wastage_maxABC <- wastage %>% filter(Fxx == "0.5" & year == YEAR+1) %>% pull(wastage))
# Get maxF_ABC
(maxF_ABC <- tidyrep %>% 
    filter(grepl('log_spr_Fxx', Parameter)) %>% 
    mutate(Fspr = data$Fxx_levels) %>% 
    filter(Fspr == 0.5) %>% 
    mutate(maxF_ABC = exp(Estimate)) %>% 
    pull(maxF_ABC))

# Percent changes
(maxABC_increase <- (maxABC - LYR_ABC) / LYR_ABC)
round(maxABC-LYR_ABC,0)
(wastage_maxABC - LYR_wastage)/ LYR_wastage
(maxF_ABC - LYR_F) / LYR_F

# Constant 15% change management procedure:
if( maxABC_increase > 0.15 ) {
  recABC <- LYR_ABC * 1.15
} else {recABC <- maxABC}
recABC

round(recABC-LYR_ABC,0)

# Estimate recommended F_ABC using Pope's approx
N <- obj$report()$N 
N <- sum(N[nyr+1,,1]) + sum(N[nyr+1,,2]) # sum of projected abundance across age and sex

# F_ABC <- recABC / (N * exp(-0.5 * exp(parameters$log_M))) * 0.5

nat_mort <- exp(parameters$log_M)
catch <- recABC

# Search sequence of F values to obtain recommended F_ABC based on 15% constant
# change MP
fish_mort <- seq(0.03, 1.6, .000001)

# Calculate F for a given catch
catch_to_F <- function(fish_mort, N, nat_mort, catch, F_to_catch) {
  
  F_to_catch <- function(N = n, nat_mort, fish_mort){
    Z <- fish_mort + nat_mort
    N * (1 - exp(-Z)) * fish_mort / Z
  }
  
  F_ABC <- catch - F_to_catch(N, nat_mort, fish_mort)
  return(F_ABC)
}

(F_ABC <- uniroot(catch_to_F, interval = c(0.03, 1.6), N = N, catch = recABC, nat_mort = nat_mort, F_to_catch = F_to_catch)$root*0.5)

(F_ABC - LYR_F) / F_ABC


# Projected total age-2+ projected biomass
obj$report(best)$tot_biom[nyr+1] * 2.20462
obj$report(best)$tot_biom[nyr] * 2.20462

# Projected total female spawning biomass
obj$report(best)$tot_spawn_biom[nyr+1] * 2.20462

SB <- as.data.frame(obj$report(best)$SB * 2.20462)
names(SB) <- "SB"
SB <- SB %>% 
  mutate(Fspr = c(0.0, data$Fxx_levels))

(SB100 <- SB %>% filter(Fspr == 0) %>% pull(SB))
(SB50 <- SB %>% filter(Fspr == 0.5) %>% pull(SB))
obj$report()$SBPR

# Sigma R

tidyrep %>% 
  filter(grepl('log_sigma_r', Parameter)) %>% 
  pull(Estimate) %>% 
  exp()

# Percent of forecasted ssb 2014 year class makes up
# Projected total female spawning biomass
f_ssb <- obj$report(best)$spawn_biom[nyr+1,] * 2.20462
round(f_ssb[5] / sum(f_ssb) * 100, 1)

# Bayesian model -----

# Run in parallel with a init function
cores <- parallel::detectCores()-1
options(mc.cores = cores)

fit <- tmbstan(obj, chains = 3, iter = 1000000, warmup = 100000, thin = 100, 
               open_progress = FALSE,  # chains = cores
               # init = init_fn, 
               init = "last.par.best",
               lower = lower, upper = upper)

# Save b/c this can take awhile to run
save(list = c("fit"), file = paste0(tmbout,"/tmbstan_fit_", YEAR, ".Rdata"))
load(paste0(tmbout,"/tmbstan_fit_", YEAR, ".Rdata"))


# Bayesian results ----
summary(fit)

# Summary of parameter estimates 
pars_sum <- summary(fit)$summary
write_csv(as.data.frame(pars_sum), paste0(tmbout, "/tmb_parameter_sum_", YEAR, ".csv"))

# Summarize mcmc posterior samples for all derived variables (see functions.r
# for documentation)
post <- as.matrix(fit) # Posterior samples
sum_mcmc <- summarize_mcmc(post) # slow...~3 min (To Do - make more efficient)
sum(length(which(sum_mcmc$tst==0))) # iterations that led to non-sensical results (either NAs or Inf)

plot_derived_ts(save = TRUE, path = tmbfigs, units = "metric", plot_variance = TRUE)
plot_ts(save = TRUE, path = tmbfigs, units = "metric", plot_variance = TRUE)
plot_ts(save = TRUE, path = tmbfigs, units = "imperial", plot_variance = FALSE)

# Diagnostics
mon <- monitor(fit)
write_csv(mon, paste0(tmbout, "/tmb_mcmc_convergence_", YEAR, ".csv"))
max(mon$Rhat)
min(mon$Bulk_ESS)
min(mon$Tail_ESS)

# Trace plots 
# Key params
trace <- traceplot(fit, pars = key_params$Parameter, inc_warmup = FALSE, ncol = 3)
trace + scale_color_grey() + theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(filename = paste0(tmbfigs, "/trace_keypars_", YEAR, ".png"), width = 8, height = 10, units = "in")

# Rec devs
trace <- traceplot(fit, pars = names(obj$par)[which(grepl("rec_devs", names(obj$par)))], inc_warmup = FALSE, ncol = 3)
trace + scale_color_grey() + theme(legend.position = , legend.direction = "horizontal")
ggsave(filename = paste0(tmbfigs, "/trace_logrecdevs_", YEAR, ".png"), width = 8, height = 30, units = "in")




# ts %>% 
#   # Add another year to hold projected values
#   full_join(data.frame(year = max(ts$year) + nproj)) %>%
#   # For ts by numbers go divide by 1e6 to get values in millions, for biomass
#   # divide by 1e3 to go from kg to mt
#   mutate(Fmort = c(obj$report()$Fmort, rep(NA, nproj)),
#          pred_rec = c(obj$report()$pred_rec, rep(NA, nproj)) / 1e6,
#          biom = obj$report()$tot_biom * 2.20462 / 1e6,
#          expl_biom = obj$report()$tot_expl_biom * 2.20462 / 1e6,
#          expl_abd = obj$report()$tot_expl_abd / 1e6,
#          spawn_biom = obj$report()$tot_spawn_biom * 2.20462 / 1e6,
#          exploit = obj$report()$pred_catch / (expl_biom / 1e3)) %>% View()
# 
# vuln_abd <- as.data.frame(obj$report()$vuln_abd[,,1]) %>% 
#   mutate(Sex = "Male") %>% 
#   bind_rows(as.data.frame(obj$report()$vuln_abd[,,2]) %>% 
#               mutate(Sex = "Female"))       
# names(vuln_abd) <- c(rec_age:plus_group, "Sex")
# vuln_abd %>% 
#   mutate(Source = "Survey",
#          index = rep(0:39, nsex)) -> vuln_abd
# 
# vuln_abd %>% gather("age", "pred", 1:data$nage) %>% 
#   mutate(age = as.numeric(age)) -> vuln_abd
# 
# vuln_abd %>% 
#   group_by(Sex, index) %>% 
#   mutate(tot = sum(pred)) %>% 
#   ungroup() %>% 
#   mutate(pred_age = pred/tot) %>% 
#   arrange(rev(Sex)) %>% 
#   filter(index %in% data$yrs_srv_len) -> pred
# 
# a_pred <- array(data = pred$pred_age,
#                 dim = c(data$nyr_srv_len, nage, nsex))
# 
# fem_pred <- a_pred[,,2]
# 
# prod <- fem_pred %*% agelen_key_f
# 
# rowSums(prod)
# 
# 
# derived <- data.frame(index = names(rep$value),
#                      estimate = rep$value,
#                      se = rep$sd)
# 
# ssb <- filter(derived, index == "tot_spawn_biom") %>% 
#   mutate(year = syr:(lyr+nproj),
#          estimate = estimate * 2.20462,
#          se = se * 2.20462,
#          lower = estimate - 1.96 * se,
#          upper = estimate + 1.96 * se)
# 
# axis <- tickr(ssb, year, 5)
# ggplot(ssb, aes(x = year, y = estimate)) +
#   geom_point() +
#   geom_line() +
#   geom_ribbon(aes(year, ymin = lower, ymax = upper),
#               alpha = 0.2,  fill = "grey") +
#   scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
#   scale_y_continuous(label = scales::comma) +
#   labs(y = "Spawning biomass (lb)\n", x = NULL) +
#   expand_limits(y = 0) -> p_ssb
# 
# ebiom <- filter(derived, index == "tot_expl_biom") %>% 
#   mutate(year = syr:(lyr+nproj),
#          estimate = estimate * 2.20462,
#          se = se * 2.20462,
#          lower = estimate - 1.96 * se,
#          upper = estimate + 1.96 * se)
# 
# axis <- tickr(ebiom, year, 5)
# ggplot(ebiom, aes(x = year, y = estimate)) +
#   geom_point() +
#   geom_line() +
#   geom_ribbon(aes(year, ymin = lower, ymax = upper),
#               alpha = 0.2,  fill = "grey") +
#   scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
#   scale_y_continuous(label = scales::comma) +
#   labs(y = "Exploitable biomass (lb)\n", x = NULL) +
#     expand_limits(y = 0) -> p_ebiom
# 
# waste <- filter(derived, index == "pred_wastage") %>% 
#   mutate(year = syr:(lyr),
#          estimate = estimate * 2204.62,
#          se = se * 2204.62,
#          lower = estimate - 1.96 * se,
#          upper = estimate + 1.96 * se)
# 
# axis <- tickr(waste, year, 5)
# ggplot(waste, aes(x = year, y = estimate)) +
#   geom_point() +
#   geom_line() +
#   geom_ribbon(aes(year, ymin = lower, ymax = upper),
#               alpha = 0.2,  fill = "grey") +
#   scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
#   scale_y_continuous(label = scales::comma) +
#   labs(y = "Wastage (lb)\n", x = NULL) +
#   expand_limits(y = 0) -> p_waste
# 
# eabd <- filter(derived, index == "tot_expl_abd") %>%
#   mutate(year = syr:(lyr+nproj),
#          estimate = estimate / 1e6,
#          se = se  / 1e6,
#          lower = estimate - 1.96 * se,
#          upper = estimate + 1.96 * se) 
# 
# axis <- tickr(eabd, year, 5)
# ggplot(eabd, aes(x = year, y = estimate)) +
#   geom_point() +
#   geom_line() +
#   geom_ribbon(aes(year, ymin = lower, ymax = upper),
#               alpha = 0.2,  fill = "grey") +
#   scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
#   scale_y_continuous(label = scales::comma) +
#   labs(y = "Exploitable abundance\n(millions)\n", x = NULL) +
#   expand_limits(y = 0) -> p_eabd
# 
# rec <- filter(derived, index == "pred_rec") %>%
#   mutate(year = syr:(lyr),
#          estimate = estimate / 1e6,
#          se = se  / 1e6,
#          std = 1.96 * sqrt(log(se + 1)),
#          ln_estimate = log(estimate),
#          lower = exp(ln_estimate - std),
#          upper = exp(ln_estimate + std)) 
# 
# axis <- tickr(rec, year, 5)
# ggplot(rec, aes(x = year, y = estimate)) +
#   geom_point() +
#   geom_line() +
#   # geom_ribbon(aes(year, ymin = lower, ymax = upper),
#   #             alpha = 0.2,  fill = "grey") +
#   scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
#   scale_y_continuous(label = scales::comma) +
#   labs(y = "Age-2 recruits\n(millions)\n", x = NULL) +
#   expand_limits(y = 0) -> p_rec
# 
# expl_abd %>% filter(year >= 2005)

# data.frame(parameter =  rep$par.fixed %>% names,
#            estimate = rep$par.fixed) %>%
#   write_csv("../data/tmb_inputs/inits_v2.csv")

# 
# Compile
compile("mod.cpp")
dyn.load(dynlib("mod"))
# Use map to turn off parameters, either for testing with dummy, phasing, or to
# fix parameter values

# Debug
map <- list(log_fsh_slx_pars = factor(array(data = c(rep(factor(NA), length(data$fsh_blks)),
                                     rep(factor(NA), length(data$fsh_blks))),
                            dim = c(length(data$fsh_blks), 2, nsex))),
            log_srv_slx_pars = factor(array(data = c(rep(factor(NA), length(data$srv_blks)),
                                                 rep(factor(NA), length(data$srv_blks))),
                                        dim = c(length(data$srv_blks), 2, nsex))),
            fsh_logq = factor(NA), srv_logq = factor(NA), mr_logq = factor(NA),
            log_rbar = factor(NA), log_rec_devs = rep(factor(NA), nyr),
            log_rinit = factor(NA), log_rinit_devs = rep(factor(NA), nage-2),
            log_sigma_r = factor(NA), log_Fbar = factor(NA), log_F_devs = rep(factor(NA), nyr),
            log_spr_Fxx = rep(factor(NA), length(data$Fxx_levels)),
            log_fsh_theta = factor(NA), log_srv_theta = factor(NA))
model <- MakeADFun(data, parameters, DLL = "mod",
                   silent = TRUE, map = map,
                   random = random_vars)

# fit <- nlminb(model$par, model$fn, model$gr,
#               control=list(eval.max=100000,iter.max=1000))

# Ageing error ----

ageing_error <- as.data.frame(ageing_error)
names(ageing_error) <- rec_age:plus_group
ageing_error %>% 
  mutate(true_age = rec_age:plus_group) %>% 
  gather("age", "obs", -c("true_age")) %>% 
  mutate(age = as.numeric(age)) -> ageing_error

axis <- tickr(ageing_error, age, 5)
ggplot(ageing_error, aes(x = age, y = true_age, size = obs)) +
  geom_point(shape = 21, fill = "black") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  scale_y_continuous(breaks = axis$breaks, labels = axis$labels) +
  guides(size = FALSE) +
  labs(x = "\nObserved age", y = "True age\n", size = NULL)

rowSums(ageing_error)
ggsave(paste0("figures/tmb/ageing_error.png"), dpi = 300, height = 6, width = 6, units = "in")
# Caption: Ageing error matrix used in the model, showing the probability of
# observing an age given the true age. Ref: Heifetz, J., D. Anderl, N.E.
# Maloney, and T.L. Rutecki. 1999. Age validation and analysis of ageing error
# from marked and recaptured sablefish, Anoplopoma fimbria. Fish. Bull. 97:
# 256-263. Hanselman, D. Pers. comm. 2019-04-18.

# Age length key ----
agelen_key_f <- as.data.frame(t(agelen_key_f)) %>% 
  mutate(Sex = "Female",
         length = unique(len$length_bin))
names(agelen_key_f) <- c(rec_age:plus_group, "Sex", "length")

agelen_key_m <- as.data.frame(t(agelen_key_m)) %>% 
  mutate(Sex = "Male",
         length = unique(len$length_bin))
names(agelen_key_m) <- c(rec_age:plus_group, "Sex", "length")

al_key <- agelen_key_f %>% 
  gather("age", "obs", -c("Sex", "length")) %>% 
  bind_rows(agelen_key_m %>% 
              gather("age", "obs", -c("Sex", "length"))) %>% 
  mutate(age = as.numeric(age))

axis <- tickr(al_key, age, 5)
ggplot(al_key, aes(x = age, y = length, size = obs)) +
  geom_point(shape = 21, fill = "black") +
  scale_size(range = c(0, 4)) +
  guides(size = FALSE) +
  facet_wrap(~Sex) +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "\nObserved age", y = "Length (cm)\n", size = NULL)

ggsave(paste0("figures/tmb/age_length_key.png"), dpi = 300, height = 7, width = 9, units = "in")

# Fixed selectivity ----

# Pre-EQS
pref50_f <-  2.87
prefslp_f <- 2.29
pref50_m <-  5.12
prefslp_m <- 2.57

# Post-EQS
f50_f <-  3.86
fslp_f <- 2.61
f50_m <-  4.22
fslp_m <- 2.61

# Survey
s50_f <- 3.75
sslp_f <- 2.21
s50_m <- 3.72
sslp_m <- 2.21

# Selectivity vectors 
age <- 2:31


pref_sel <- 1 / (1 + exp(-prefslp_f * (age - pref50_f)))
prem_sel <- 1 / (1 + exp(-prefslp_m * (age - pref50_m)))
f_sel <- 1 / (1 + exp(-fslp_f * (age - f50_f)))
m_sel <- 1 / (1 + exp(-fslp_m * (age - f50_m)))
sf_sel <- 1 / (1 + exp(-sslp_f * (age - s50_f)))
sm_sel <- 1 / (1 + exp(-sslp_m * (age - s50_m)))

# Plot selectivity 

sel_df <- data.frame(age = rep(age, 6),
                     Source = c(rep("Pre-EQS Fishery", 2 * length(age)),
                                rep("EQS Fishery", 2 * length(age)),
                                rep("Longline survey", 2 * length(age))),
                     Sex = c(rep("Female", length(age)),
                             rep("Male", length(age)),
                             rep("Female", length(age)),
                             rep("Male", length(age)),
                             rep("Female", length(age)),
                             rep("Male", length(age))),
                     selectivity = c(pref_sel, prem_sel, f_sel, m_sel, sf_sel, sm_sel)) %>% 
  filter(age <= 8) %>% 
  mutate(Source = fct_relevel(Source, "Pre-EQS Fishery", "EQS Fishery", "Longline survey"))

axis <- tickr(sel_df, age, 1)
ggplot(sel_df, 
       aes(x = age, y = selectivity, colour = Sex, 
           linetype = Sex, shape = Sex)) +
  geom_point() + 
  geom_line() +
  scale_colour_grey() +
  facet_wrap(~ Source, ncol = 1) +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  theme(legend.position = c(0.8, 0.1)) +
  labs(x = "\nAge", y = "Selectivity\n")

ggsave(paste0("figures/tmb/fixed_selectivity_", YEAR, ".png"),
       dpi=300, height=7, width=6, units="in")
