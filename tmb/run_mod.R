
# ASA that includes catch, fishery and survey CPUE, mark-recapture abundance
# estimates, fishery and survey weight-at-age, survey data about maturity-at-age
# and proportions-at-age, and fishery and survey age compositions. No sex
# structure.

# Libraries and helper functions ----

# Temporary debug flag, shut off estimation of mgmt ref pts
tmp_debug <- TRUE

source("r/helper.r")
source("r/functions.r")

library(TMB) 

# Data -----

ts <- read_csv("data/tmb_inputs/abd_indices.csv")             # time series
age <- read_csv("data/tmb_inputs/agecomps.csv")               # age comps
bio <- read_csv("data/tmb_inputs/maturity_sexratio.csv")      # proportion mature and proportion-at-age in the survey
waa <- read_csv("data/tmb_inputs/waa.csv")                    # weight-at-age
retention <- read_csv("data/tmb_inputs/retention_probs.csv")  # weight-at-age

# Ageing error transition matrix: proportion at reader age given TRUE age -
# ageage(1,nages,1,nages) There are no true ages in ADF&G data; only
# RELEASE_AUTHORITATIVE for comparison of following reads for a given otolith
# Ageing error matrix constructed from 1988 - 2013 read data from both
# commercial longline and longline survey samples Kray Van Kirk updated and
# improved code originally developed by Pete Hulson dated 2015-12-08. I haven't
# reviewed the code or data, but need to use this one for now until I have time
# in future years.
ageing_error <- read_csv("data/tmb_inputs/ageing_error.csv", col_names = FALSE)
names(ageing_error) <- 2:42

# Starting values
finits <- read_csv("data/tmb_inputs/inits_f_devs.csv")   # log F devs
inits_rec_dev <- read_csv("data/tmb_inputs/inits_rec_devs.csv") # log rec devs
inits_rinit <- read_csv("data/tmb_inputs/inits_rinit.csv") # log rec devs

setwd("tmb")

# Model dimensions / user inputs
syr <- min(ts$year)                   # model start year
lyr <- max(ts$year)                   # end year
nyr <- length(syr:lyr)                # number of years        
rec_age <- min(waa$age)               # recruitment age                  
plus_group <- max(waa$age)            # plus group age
nage <- length(rec_age:plus_group)    # number of ages
nsex <- 1                             # single sex or sex-structured
# number of years to project forward *FLAG* eventually add to cpp file,
# currently just for graphics
nproj <- 1                            
include_discards <- FALSE # include discard mortality, TRUE or FALSE


# Subsets
mr <- filter(ts, !is.na(mr))
fsh_cpue <- filter(ts, !is.na(fsh_cpue))
srv_cpue <- filter(ts, !is.na(srv_cpue))
fsh_age <- filter(age, Source == "Fishery")
srv_age <- filter(age, Source == "Survey")

# Structure data for TMB - must use same variable names as .cpp
data <- list(
  
  # Model dimensions
  nyr = nyr,
  nage = nage,
  nsex = nsex,
  
  # Switch recruitment estimation: 0 = penalized likelihood (fixed sigma_r), 1 =
  # random effects
  random_rec = 0,
  
  # Switch for selectivity type: 0 = a50, a95 logistic; 1 = a50, slope logistic
  slx_type = 0,
  
  # Swtich for age composition type (hopefully one day length comps too): 0 =
  # multinomial; 1 = Dirichlet-multinomial
  comp_type = 0,
  
  # Time varying parameters - each vector contains the terminal years of each time block
  blks_fsh_slx = c(5,14,max(ts$index)), #  fishery selectivity: limited entry in 1985, EQS in 1994 = c(5, 14, max(ts$year))
  blks_srv_slx = c(max(ts$index)), # no breaks survey selectivity
  
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
  Fxx_levels = c(0.35, 0.40, 0.50),
  
  # Priors ("p_" denotes prior)
  p_fsh_q = 5.8e-8,
  sigma_fsh_q = 1,
  p_srv_q = exp(-18.15), 
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
  wt_rec_like = 0.1,
  wt_fpen = 0.1,
  wt_spr = 200,
  
  # Catch
  data_catch = ts$catch,
  sigma_catch = pull(ts, sigma_catch),
  
  # Mark-recapture estimates
  nyr_mr = n_distinct(mr, mr),
  yrs_mr = mr %>% distinct(index) %>% pull(),
  data_mr = pull(mr, mr),
  sigma_mr = mr %>% pull(sigma_mr),
  
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

  # Weight-at-age: currently weight-at-age is averaged over all years. If for whatever reason
  # you want to include multiple time periods for weight-at-age, you would
  # change the number of rows from 1 to the number of time periods or years
  data_srv_waa = 
    if (nsex == 1) { # Single sex model
      filter(waa, Source == "Survey (sexes combined)") %>% 
    pull(weight) %>%  matrix(ncol = nage, nrow = nsex)  %>% 
    array(dim = c(1, nage, nsex))} else {
     # Sex-structured - males in first matrix, females in second
      filter(waa, Source %in% c("Survey (males)", "Survey (females)")) %>% 
        mutate(sex = ifelse(Sex == "Male", 1, 2)) %>% arrange(sex) %>% 
        pull(weight) %>% matrix(ncol = nage, nrow = nsex)  %>% 
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
  
  # Ageing error matrix
  ageing_error = as.matrix(ageing_error)
)

# Parameters ----

# Parameter starting values
parameters <- list(
  
  dummy = 0,   # Used for troubleshooting model               
  
  # Fishery selectivity - starting values developed using NOAA selectivity
  # curves
  log_fsh_slx_pars = 
    # Logistic with a50 and a95, data$slx_type = 0, single sex model
    if(data$slx_type == 0 & nsex == 1) {
      array(data = c(rep(log(4.12), length(data$blks_fsh_slx)), # Sexes combined
                     rep(log(5.54), length(data$blks_fsh_slx))),
            dim = c(length(data$blks_fsh_slx), 2, nsex)) # 2 = npar for this slx_type 
      
    # Logistic with a50 and a95, data$slx_type = 0, sex-structured model
    } else if (data$slx_type == 0 & nsex == 2) {
      array(data = c(rep(log(4.33), length(data$blks_fsh_slx)), # Male
                     rep(log(5.65), length(data$blks_fsh_slx)),
                     rep(log(3.91), length(data$blks_fsh_slx)), # Female
                     rep(log(5.43), length(data$blks_fsh_slx))),
            dim = c(length(data$blks_fsh_slx), 2, nsex)) # 2 = npar for this slx_type 
      
    # Logistic with a50 and slope, data$slx_type = 1, single sex model
    } else if (data$slx_type == 1 & nsex == 1) {
      array(data = c(rep(log(4.04), length(data$blks_fsh_slx)),
                     rep(log(2.61), length(data$blks_fsh_slx))),
            dim = c(length(data$blks_fsh_slx), 2, nsex)) # 2 = npar for this slx_type 
      
    } else {  # Logistic with a50 and slope, data$slx_type = 1, sex-structured model
      array(data = c(rep(log(4.22), length(data$blks_fsh_slx)), # male
                     rep(log(2.61), length(data$blks_fsh_slx)),
                     rep(log(3.86), length(data$blks_fsh_slx)), # female
                     rep(log(2.61), length(data$blks_fsh_slx))),
            dim = c(length(data$blks_fsh_slx), 2, nsex)) }, # 2 = npar for this slx_type 
  
  # Survey selectivity - starting values developed using NOAA selectivity curves
  log_srv_slx_pars = 
    # Logistic with a50 and a95, data$slx_type = 0, single sex model
    if(data$slx_type == 0 & nsex == 1) {
      array(data = c(rep(log(3.86), length(data$blks_srv_slx)),
                     rep(log(5.21), length(data$blks_srv_slx))),
            dim = c(length(data$blks_srv_slx), 2, nsex)) # 2 = npar for this slx_type 
      
    # Logistic with a50 and a95, data$slx_type = 0, sex-structured model
    } else if (data$slx_type == 0 & nsex == 2) {
      array(data = c(rep(log(3.68), length(data$blks_srv_slx)), # male
                     rep(log(5.21), length(data$blks_srv_slx)),
                     rep(log(3.68), length(data$blks_srv_slx)), # female
                     rep(log(5.21), length(data$blks_srv_slx))),
            dim = c(length(data$blks_srv_slx), 2, nsex)) # 2 = npar for this slx_type 
      
    # Logistic with a50 and slope, data$slx_type = 1, single sex model
    } else if (data$slx_type == 1 & nsex == 1) {
      array(data = c(rep(log(3.68), length(data$blks_srv_slx)),
                     rep(log(2.21), length(data$blks_srv_slx))),
            dim = c(length(data$blks_srv_slx), 2, nsex)) # 2 = npar for this slx_type 
      
    # Logistic with a50 and slope, data$slx_type = 1, sex-structured model
    } else { 
      array(data = c(rep(log(3.68), length(data$blks_srv_slx)), # male
                     rep(log(2.21), length(data$blks_srv_slx)),
                     rep(log(3.68), length(data$blks_srv_slx)), # female
                     rep(log(2.21), length(data$blks_srv_slx))),
            dim = c(length(data$blks_srv_slx), 2, nsex)) }, # 2 = npar for this slx_type
  
  # Catchability
  fsh_logq = -16.6,
  srv_logq = -18.15,
  mr_logq = 0.265902273,
  
  # Log mean recruitment and deviations (nyr)
  log_rbar = 2.5,
  log_rec_devs = inits_rec_dev$inits_rec_dev,
  # Log mean initial numbers-at-age and deviations (nage-2)
  log_rinit = 3.5,
  log_rinit_devs = inits_rinit$inits_rinit,
  # Variability in rec_devs and rinit_devs
  log_sigma_r = log(1.2), # Federal value of 1.2 on log scale
  
  # Fishing mortality
  log_Fbar = -1.8289,
  log_F_devs = finits$finits,
  
  # SPR-based fishing mortality rates, i.e. the F at which the spawning biomass
  # per recruit is reduced to xx% of its value in an unfished stock
  log_spr_Fxx = c(log(0.128), log(0.105), log(0.071)),       # F35, F40, F50
  
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

# Use map to turn off parameters, either for testing with dummy, phasing, or to
# fix parameter values

# Debug
# map <- list(log_fsh_slx_pars = factor(array(data = c(rep(factor(NA), length(data$blks_fsh_slx)),
#                                      rep(factor(NA), length(data$blks_fsh_slx))),
#                             dim = c(length(data$blks_fsh_slx), 2, nsex))),
#             log_srv_slx_pars = factor(array(data = c(rep(factor(NA), length(data$blks_srv_slx)),
#                                                  rep(factor(NA), length(data$blks_srv_slx))),
#                                         dim = c(length(data$blks_srv_slx), 2, nsex))),
#             fsh_logq = factor(NA), srv_logq = factor(NA), mr_logq = factor(NA),
#             log_rbar = factor(NA), log_rec_devs = rep(factor(NA), nyr),
#             log_rinit = factor(NA), log_rinit_devs = rep(factor(NA), nage-2),
#             log_sigma_r = factor(NA), log_Fbar = factor(NA), log_F_devs = rep(factor(NA), nyr),
#             log_spr_Fxx = rep(factor(NA), length(data$Fxx_levels)),
#             log_fsh_theta = factor(NA), log_srv_theta = factor(NA))

# Compile
compile("mod.cpp")
dyn.load(dynlib("mod"))

# model <- MakeADFun(data, parameters, DLL = "mod", 
#                    silent = TRUE, map = map,
#                    random = random_vars)
# 
# fit <- nlminb(model$par, model$fn, model$gr,
#               control=list(eval.max=100000,iter.max=1000))

# Run model
phases <- build_phases(parameters, data)
out <- TMBphase(data, parameters, random = random_vars, phases, model_name = "mod", debug = FALSE)

obj <- out$obj # TMB model object
opt <- out$opt # fit
rep <- out$rep # sdreport
print(rep)

# Figures 

# Fits to abundance indices, derived time series, and F
plot_ts()
plot_ts_resids()
plot_derived_ts()
plot_F()

# Fits to age comps
agecomps <- reshape_age()
plot_age_resids()
barplot_age("Survey")
barplot_age("Fishery")

# Plot selectivity
plot_sel()


data.frame(parameter =  rep$par.fixed %>% names,
           estimate = rep$par.fixed) %>% 
  write_csv("../data/tmb_inputs/inits_v2.csv")

# Results ----
best <- obj$env$last.par.best
print(as.numeric(best))
print(best)
obj$report()$priors
obj$report()$S[nyr,,1]
obj$report()$S[1,,2]
obj$report()$catch_like
obj$report()$index_like
obj$report()$age_like
obj$report()$pred_mr
obj$report()$obj_fun
obj$report()$pred_landed ==obj$report()$pred_catch
obj$report()$pred_wastage

exp(as.list(rep, what = "Estimate")$fsh_logq)
exp(as.list(rep, what = "Estimate")$srv_logq)
exp(as.list(rep, what = "Estimate")$mr_logq)
# as.list(rep, what = "Std")

exp(as.list(rep, what = "Estimate")$log_rbar)
# variance-covriance
VarCo <- solve(obj$he())
# Check for Hessian
print(sqrt(diag(VarCo)))

