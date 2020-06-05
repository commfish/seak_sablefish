# Tune composition data ----

# McAllister-Ianelli (1997): This method sets the effective sample size by
# comparing the residual variance with the variance expected under a multinomial
# distribution. An overall effective sample size for each composition series is
# calculated as the harmonic mean across years at each iteration (Stewart and
# Hamel 2014). 

# Equations used:
#   
# Nhat_i = sum_j{phat_ij * (1 - phat_ij)} / sum_j{(p_ij - phat_ij)^2}
# Nhat = n / sum_i{(Nhat_i)^(-1)}

# i = year, j = age or length
# Nhat = effective sample size
# p_ij = observed age or length comp in a given year
# phat_ij = estimated age or length comp in a given year
# n = number of years in the comp index

# Sex-structured statistical catch-at-age model that includes catch, fishery and
# survey CPUE, mark-recapture abundance estimates, fishery and survey
# weight-at-age, survey data about maturity-at-age and proportions-at-age, and
# fishery and survey age and length compositions.

# Contact: jane.sullivan1@alaska.gov
# Last updated Jan 2020

# Set up ----

# most recent year of data
YEAR <- 2018

# Directory setup
root <- getwd() # project root
tmb_path <- file.path(root, "tmb") # location of cpp
tmb_dat <- file.path(root, "data/tmb_inputs")

# Temporary debug flag, shut off estimation of selectivity pars
tmp_debug <- TRUE

source("r/helper.r")
source("r/functions.r")

library(TMB) 
library(tmbstan)
library(shinystan)

ts <- read_csv("data/tmb_inputs/abd_indices.csv")             # time series
age <- read_csv("data/tmb_inputs/agecomps.csv")               # age comps
len <- read_csv("data/tmb_inputs/lencomps.csv")               # len comps
bio <- read_csv("data/tmb_inputs/maturity_sexratio.csv")      # proportion mature and proportion-at-age in the survey
waa <- read_csv("data/tmb_inputs/waa.csv")                    # weight-at-age
retention <- read_csv("data/tmb_inputs/retention_probs.csv")  # weight-at-age

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
# finits <- read_csv("data/tmb_inputs/inits_f_devs.csv")   # log F devs
# inits_rec_dev <- read_csv("data/tmb_inputs/inits_rec_devs.csv") # log rec devs
# inits_rinit <- read_csv("data/tmb_inputs/inits_rinit.csv") # log rec devs
inits <- read_csv("data/tmb_inputs/inits_v2.csv")

# Model dimensions / user inputs
syr <- min(ts$year)                   # model start year
lyr <- YEAR <- max(ts$year)           # end year
nyr <- length(syr:lyr)                # number of years        
rec_age <- min(waa$age)               # recruitment age                  
plus_group <- max(waa$age)            # plus group age
nage <- length(rec_age:plus_group)    # number of ages
nlenbin <- n_distinct(len$length_bin) # number of length bins
nsex <- 2                             # single sex or sex-structured
# number of years to project forward *FLAG* eventually add to cpp file,
# currently just for graphics
nproj <- 1                            
include_discards <- TRUE # include discard mortality, TRUE or FALSE

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
  random_rec = 0,
  
  # Switch for selectivity type: 0 = a50, a95 logistic; 1 = a50, slope logistic
  slx_type = 1,
  
  # Swtich for age composition type (hopefully one day length comps too): 0 =
  # multinomial; 1 = Dirichlet-multinomial
  comp_type = 0,
  
  # Switch for assumption on SPR equilibrium recruitment. 0 = arithmetic mean
  # (same as Federal assessment), 1 = geometric mean, 2 = median (2 not coded
  # yet)
  spr_rec_type = 1,
  
  # Time varying parameters - each vector contains the terminal years of each time block
  fsh_blks = c(14, max(ts$index)), #  fishery selectivity: limited entry in 1985, EQS in 1994 = c(5, 14, max(ts$year))
  srv_blks = c(max(ts$index)), # no breaks survey selectivity
  
  # Natural mortality (fixed to 0.1 per Johnson and Quinn 1988). Can accomodate
  # variation by year, age, or sex, but currently M is fixed across all
  # dimensions.
  M = array(data = 0.1, dim = c(nyr, nage, nsex)),
  
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
  
  # Weights on likelihood components ("wt_" denotes weight)
  wt_catch = 1.0,
  wt_fsh_cpue = 1.0,
  wt_srv_cpue = 1.0,
  wt_mr = 1.0,
  wt_fsh_age = 1.0,
  wt_srv_age = 1.0,
  wt_fsh_len = 1.0,
  wt_srv_len = 1.0,
  wt_rec_like = 2,
  wt_fpen = 0.1,
  wt_spr = 100,
  
  # Catch
  data_catch = ts$catch,
  sigma_catch = pull(ts, sigma_catch),
  
  # Mark-recapture estimates
  nyr_mr = n_distinct(mr, mr),
  yrs_mr = mr %>% distinct(index) %>% pull(),
  data_mr = pull(mr, mr),
  sigma_mr = rep(0.05,11),#mr %>% pull(sigma_mr)
  
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
  effn_fsh_age = pull(fsh_age, n), # pull(fsh_age, effn),  # effective sample size, currently sqrt(n_fsh_age)
  
  # Survey age comps
  nyr_srv_age = srv_age %>% distinct(year) %>% nrow(),
  yrs_srv_age = srv_age %>% distinct(index) %>% pull(),
  data_srv_age = srv_age %>% select(-c(year, index, Source, n, effn)) %>% as.matrix(),
  n_srv_age = pull(srv_age, n),        # total sample size
  effn_srv_age = pull(srv_age, n), #pull(srv_age, effn),  # effective sample size, currently sqrt(n_srv_age)
  
  # Fishery length comps
  nyr_fsh_len = length(unique(fsh_len$year)),
  yrs_fsh_len = fsh_len %>% distinct(index) %>% pull(),
  data_fsh_len = 
    if (nsex == 1) { # Single sex model
      array(data = fsh_len %>% filter(Sex == "Sex combined") %>% pull(proportion),                              
            dim = c(length(unique(fsh_len$year)), nlenbin, nsex)) } else {
              # Sex-structured (make sure males are first)
              array(data = fsh_len %>% filter(Sex != "Sex combined") %>% 
                      arrange(desc(Sex)) %>% pull(proportion),
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
      array(data = fsh_len %>% filter(Sex == "Sex combined") %>% distinct(year, Sex, n) %>% pull(n),
            dim = c(length(unique(fsh_len$year)), 1, nsex)) } else {
              # Sex-structured (make sure males are first)
              array(data = fsh_len %>% filter(Sex != "Sex combined") %>%
                      distinct(year, Sex, n) %>% 
                      arrange(desc(Sex)) %>% pull(n),
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
      array(data = srv_len %>% filter(Sex == "Sex combined") %>% distinct(year, Sex, n) %>%  pull(n),
            dim = c(length(unique(srv_len$year)), 1, nsex)) } else {
              # Sex-structured (make sure males are first)
              array(data = srv_len %>% filter(Sex != "Sex combined") %>%
                      distinct(year, Sex, n) %>% 
                      arrange(desc(Sex)) %>% pull(n),
                    dim = c(length(unique(srv_len$year)), 1, nsex))},
  # if (nsex == 1) { # Single sex model
  #   array(data = srv_len %>% filter(Sex == "Sex combined") %>% pull(effn),
  #         dim = c(length(unique(srv_len$year)), 1, nsex))} else {
  #           # Sex-structured (make sure males are first)
  #           array(data = srv_len %>% filter(Sex != "Sex combined") %>%
  #                   arrange(desc(Sex)) %>% pull(effn),
  #                 dim = c(length(unique(srv_len$year)), 1, nsex))},
  
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
  fsh_logq = c(-16.6, -16),
  srv_logq = -17,
  mr_logq = log(1),
  
  # Log mean recruitment and deviations (nyr)
  log_rbar = 2.5,
  log_rec_devs = inits %>% filter(grepl("rec_devs", parameter)) %>% pull(estimate) %>% head(nyr),
  # inits_rec_dev$inits_rec_dev,
  
  # Log mean initial numbers-at-age and deviations (nage-2)
  log_rinit = 3.5,
  log_rinit_devs = inits %>% filter(grepl("init_devs", parameter)) %>% pull(estimate),
  # inits_rinit$inits_rinit,
  
  # Variability in rec_devs and rinit_devs
  log_sigma_r = log(1.2), # Federal value of 1.2 on log scale
  
  # Fishing mortality
  log_Fbar = -1.8289,
  log_F_devs = inits %>% filter(grepl("F_devs", parameter)) %>% pull(estimate) %>% head(nyr),
  #finits$finits,
  
  # SPR-based fishing mortality rates, i.e. the F at which the spawning biomass
  # per recruit is reduced to xx% of its value in an unfished stock
  log_spr_Fxx = c(log(0.128), log(0.105), log(0.071), log(0.066), log(0.058)), # F35, F40, F50, F60, F70
  
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

tune_fsh_age <- list()
tune_srv_age <- list()
tune_fsh_len <- list()
tune_srv_len <- list()

# Iterate ----
niter <- 15

for(iter in 1:niter) {
  
  # MLE, phased estimation (phase = TRUE) or not (phase = FALSE)
  out <- TMBphase(data, parameters, random = random_vars, 
                  model_name = "mod", phase = FALSE, 
                  debug = FALSE)
  
  obj <- out$obj # TMB model object
  opt <- out$opt # fit
  
  # Quick look at MLE results
  best <- obj$env$last.par.best 

  # Fishery age comps (sexes combined) ----
  pred_fsh_age <- as.matrix(obj$report(best)$pred_fsh_age)
  data_fsh_age <- as.matrix(data$data_fsh_age)
  effn_fsh_age <- vector(length = nrow(pred_fsh_age))
  
  for(i in 1:nrow(pred_fsh_age)){
    effn_fsh_age[i] <- sum(pred_fsh_age[i,]*(1-pred_fsh_age[i,])) / sum((data_fsh_age[i,]-pred_fsh_age[i,])^2)
  }
  
  effn_fsh_age <- 1/mean(1/effn_fsh_age) # harmonic mean

  tune_fsh_age[[iter]] <- effn_fsh_age
  data$effn_fsh_age <- rep(effn_fsh_age, length(data$effn_fsh_age)) # replace data for next iteration
  
  # Survey age comps (sexes combined) ----
  pred_srv_age <- as.matrix(obj$report(best)$pred_srv_age)
  data_srv_age <- as.matrix(data$data_srv_age)
  effn_srv_age <- vector(length = nrow(pred_srv_age))
  
  for(i in 1:nrow(pred_srv_age)){
    effn_srv_age[i] <- sum(pred_srv_age[i,]*(1-pred_srv_age[i,])) / sum((data_srv_age[i,]-pred_srv_age[i,])^2)
  }
  
  effn_srv_age <- 1/mean(1/effn_srv_age) # harmonic mean
  tune_srv_age[[iter]] <- effn_srv_age
  data$effn_srv_age <- rep(effn_srv_age, length(data$effn_srv_age)) # replace data for next iteration
  
  # Fishery length comps (currently only for sex-structured model where nsex = 2) ----
  pred_fsh_len <- obj$report(best)$pred_fsh_len
  data_fsh_len <- data$data_fsh_len
  effn_fsh_len <- matrix(nrow = nrow(pred_fsh_len[,,2]), ncol = 2)
  
  data_fsh_len <- data_fsh_len + 1e-6 # add tiny constant so we don't get NaNs
  
  for(a in 1:nsex) {
    for(i in 1:nrow(pred_fsh_len)){
      effn_fsh_len[i,a] <- sum(pred_fsh_len[i,,a]*(1-pred_fsh_len[i,,a])) / sum((data_fsh_len[i,,a]-pred_fsh_len[i,,a])^2)
    }
  }
  
  new_effn_fsh_len <- matrix(ncol = 2, nrow = 1)

  for(a in 1:nsex) {
    new_effn_fsh_len[a] <- 1/mean(1/effn_fsh_len[,a]) # harmonic mean
  }
  tune_fsh_len[[iter]] <- new_effn_fsh_len
  
  # replace data for next iteration
  data$effn_fsh_len <- array(dim = c(nrow = nrow(pred_fsh_len), 1, nsex),
                             data = c(rep(new_effn_fsh_len[,1],  nrow(pred_fsh_len)), rep(new_effn_fsh_len[,2],  nrow(pred_fsh_len))))
  
  # Survey length comps (currently only for sex-structured model where nsex = 2) ----
  pred_srv_len <- obj$report(best)$pred_srv_len
  data_srv_len <- data$data_srv_len
  effn_srv_len <- matrix(nrow = nrow(pred_srv_len[,,2]), ncol = 2)
  
  data_srv_len <- data_srv_len + 1e-6 # add tiny constant so we don't get NaNs
  
  for(a in 1:nsex) {
    for(i in 1:nrow(pred_srv_len)){
      effn_srv_len[i,a] <- sum(pred_srv_len[i,,a]*(1-pred_srv_len[i,,a])) / sum((data_srv_len[i,,a]-pred_srv_len[i,,a])^2)
    }
  }
  
  new_effn_srv_len <- matrix(ncol = 2, nrow = 1)
  
  for(a in 1:nsex) {
    new_effn_srv_len[a] <- 1/mean(1/effn_srv_len[,a]) # harmonic mean
  }
  tune_srv_len[[iter]] <- new_effn_srv_len
  
  # replace data for next iteration
  data$effn_srv_len <- array(dim = c(nrow = nrow(pred_srv_len), 1, nsex),
                             data = c(rep(new_effn_srv_len[,1],  nrow(pred_srv_len)), rep(new_effn_srv_len[,2],  nrow(pred_srv_len))))
}

tune_srv_len <- as.data.frame(do.call("rbind", tune_srv_len))
names(tune_srv_len) <- c("male_srv_len_ess", "fem_srv_len_ess")
tune_srv_age <- as.data.frame(do.call("rbind", tune_srv_age))
names(tune_srv_age) <- c("srv_age_ess")
tune_fsh_len <- as.data.frame(do.call("rbind", tune_fsh_len))
names(tune_fsh_len) <- c("male_fsh_len_ess", "fem_fsh_len_ess")
tune_fsh_age <- as.data.frame(do.call("rbind", tune_fsh_age))
names(tune_fsh_age) <- c("fsh_age_ess")

tune_srv_len
tune_srv_age
tune_fsh_len
tune_fsh_age

# Write new ESS ----

unique(age$Source)# check: should just be "Survey" and "Fishery"

# Save tuned age comps
age %>% 
  mutate(effn = ifelse(age$Source == "Survey", tune_srv_age[niter,], tune_fsh_age[niter,])) %>%
  write_csv(paste0(tmb_dat, "/tuned_agecomps_", YEAR, ".csv"))

# Save fishery and survey length comps
fsh_len %>% 
  mutate(effn = ifelse(fsh_len$Sex == "Male", tune_fsh_len[niter,1], tune_fsh_len[niter,2])) %>% 
  bind_rows(srv_len %>% 
              mutate(effn = ifelse(srv_len$Sex == "Male", tune_srv_len[niter,1], tune_srv_len[niter,2]))) %>% 
  write_csv(paste0(tmb_dat, "/tuned_lencomps_", YEAR, ".csv"))
