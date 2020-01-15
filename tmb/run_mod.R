
# ASA that includes catch, fishery and survey CPUE, mark-recapture abundance
# estimates, fishery and survey weight-at-age, survey data about maturity-at-age
# and proportions-at-age, and fishery and survey age compositions. No sex
# structure.

# Set up ----

# Directory setup
root <- getwd() # project root
tmb_path <- file.path(root, "tmb") # location of cpp
tmbfigs <- file.path(root, "figures/tmb")
tmbout <- file.path(root, "output/tmb")

# Temporary debug flag, shut off estimation of mgmt ref pts
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
      array(data = fsh_len %>% filter(Sex == "Sex combined") %>% pull(proportion),                              ,
            dim = c(length(unique(fsh_len$year)), nlenbin, nsex)) } else {
              # Sex-structured (make sure males are first)
              array(data = fsh_len %>% filter(Sex != "Sex combined") %>% 
                      arrange(desc(Sex)) %>% pull(proportion),
                    dim = c(length(unique(fsh_len$year)), nlenbin, nsex))},
  n_fsh_len =
    if (nsex == 1) { # Single sex model
      array(data = fsh_len %>% filter(Sex == "Sex combined") %>% pull(n),
            dim = c(length(unique(fsh_len$year)), 1, nsex)) } else {
              # Sex-structured (make sure males are first)
              array(data = fsh_len %>% filter(Sex != "Sex combined") %>%
                      arrange(desc(Sex)) %>% pull(n),
                    dim = c(length(unique(fsh_len$year)), 1, nsex))},

  effn_fsh_len =
    if (nsex == 1) { # Single sex model
      array(data = fsh_len %>% filter(Sex == "Sex combined") %>% pull(effn),
            dim = c(length(unique(fsh_len$year)), 1, nsex))} else {
              # Sex-structured (make sure males are first)
              array(data = fsh_len %>% filter(Sex != "Sex combined") %>%
                      arrange(desc(Sex)) %>% pull(effn),
                    dim = c(length(unique(fsh_len$year)), 1, nsex))},

  # Survey length comps
  nyr_srv_len = length(unique(srv_len$year)),
  yrs_srv_len = srv_len %>% distinct(index) %>% pull(),
  data_srv_len =
    if (nsex == 1) { # Single sex model
      array(data = srv_len %>% filter(Sex == "Sex combined") %>% pull(proportion),                              ,
            dim = c(length(unique(srv_len$year)), nlenbin, nsex)) } else {
              # Sex-structured (make sure males are first)
              array(data = srv_len %>% filter(Sex != "Sex combined") %>%
                      arrange(desc(Sex)) %>% pull(proportion),
                    dim = c(length(unique(srv_len$year)), nlenbin, nsex))},
  n_srv_len =
    if (nsex == 1) { # Single sex model
      array(data = srv_len %>% filter(Sex == "Sex combined") %>% pull(n),
            dim = c(length(unique(srv_len$year)), 1, nsex)) } else {
              # Sex-structured (make sure males are first)
              array(data = srv_len %>% filter(Sex != "Sex combined") %>%
                      arrange(desc(Sex)) %>% pull(n),
                    dim = c(length(unique(srv_len$year)), 1, nsex))},
  effn_srv_len =
    if (nsex == 1) { # Single sex model
      array(data = srv_len %>% filter(Sex == "Sex combined") %>% pull(effn),
            dim = c(length(unique(srv_len$year)), 1, nsex))} else {
              # Sex-structured (make sure males are first)
              array(data = srv_len %>% filter(Sex != "Sex combined") %>%
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

# Three ways to run model:
# 1) In phases, defined by build_phases() in functions.R. Use TMBphase(phase = TRUE)
# 2) MLE without phased optimation (phase = FALSE)
# 3) Bayesian using tmbstan and the no-U-turn sampler (NUTS): Run step 2 first
# to build TMB object, map, and bounds

# MLE, phased estimation (phase = TRUE) or not (phase = FALSE)
out <- TMBphase(data, parameters, random = random_vars, model_name = "mod", phase = FALSE, debug = FALSE)

obj <- out$obj # TMB model object
opt <- out$opt # fit
rep <- out$rep # sdreport
lower <- out$lower 
upper <- out$upper

# MCMC

# Run in parallel with a init function
cores <- parallel::detectCores()-1
options(mc.cores = cores)

fit <- tmbstan(obj, chains = cores, open_progress = FALSE, init = init_fn, lower = lower, upper = upper)
summary(fit)
mon <- monitor(fit)
write_csv(mon, "../output/convergence.csv")
max(mon$Rhat)
min(mon$Bulk_ESS)
min(mon$Tail_ESS)


post <- as.matrix(fit)
write_csv(as.data.frame(post), "../output/tmb_posterior_samples.csv")

# Summary of parameter estimates 
pars_sum <- summary(fit)$summary
write_csv(as.data.frame(pars_sum), "../output/param_summary.csv")

# Posterior for derived quantities 
post_catch <- list()
post_fsh_cpue <- list()
post_srv_cpue <- list()
post_mr <- list()
post_rec <- list()
post_biom <- list()
post_expl_biom <- list()
post_expl_abd <- list()
post_spawn_biom <- list()
post_ABC <- list()
post_wastage <- list()
post_SB100 <- list()
post_SB50 <- list()
post_F50 <- list()

for(i in 1:nrow(post)){
  
  # Last column is log-posterior density (lp__) and needs to be dropped
  r <- obj$report(post[i,-ncol(post)]) 
  
  # Data time series
  post_catch[[i]] <- cbind(r$pred_landed, rep(i, length(r$pred_landed)), ts$year)
  post_fsh_cpue[[i]] <- cbind(r$pred_fsh_cpue, rep(i, length(r$pred_fsh_cpue)), ts$year)
  post_srv_cpue[[i]] <- cbind(r$pred_srv_cpue, rep(i, length(r$pred_srv_cpue)), srv_cpue$year)
  post_mr[[i]] <- cbind(r$pred_mr_all, rep(i, length(r$pred_mr_all)), ts$year)
  
  # Derived indices (includes projection years)
  post_rec[[i]] <- cbind(r$pred_rec, rep(i, length(r$pred_rec)), ts$year)
  post_biom[[i]] <- cbind(r$tot_biom, rep(i, length(r$tot_biom)), c(ts$year,  (max(ts$year) + 1):(max(ts$year) + nproj)))
  post_expl_biom[[i]] <- cbind(r$tot_expl_biom, rep(i, length(r$tot_expl_biom)), c(ts$year,  (max(ts$year) + 1):(max(ts$year) + nproj)))
  post_expl_abd[[i]] <- cbind(r$tot_expl_abd, rep(i, length(r$tot_expl_abd)), c(ts$year,  (max(ts$year) + 1):(max(ts$year) + nproj)))
  post_spawn_biom[[i]] <- cbind(r$tot_spawn_biom, rep(i, length(r$tot_spawn_biom)), c(ts$year,  (max(ts$year) + 1):(max(ts$year) + nproj)))

  # Reference points and ABC calculations
  post_ABC[[i]] <- cbind(r$ABC[data$nyr + 1, which(data$Fxx_levels == 0.5)], i) # ABC is a matrix with row = nyr+1 and col = data$Fxx_levels
  post_wastage[[i]] <- cbind(r$wastage[data$nyr + 1, which(data$Fxx_levels == 0.5)], i) # same dimensions as ABC 
  post_SB100[[i]] <- cbind(r$SB[1], i) # SB is a vector with unfished SB followed by SB at data$Fxx_levels
  post_SB50[[i]] <- cbind(r$SB[which(data$Fxx_levels == 0.5) + 1], i) # data$Fxx_levels shifted by 1 to account for unfished SB
  post_F50[[i]] <- cbind(r$Fxx[which(data$Fxx_levels == 0.5) + 1], i) # same as SB50
  
  # To Do - age and length comps
}

post_catch <- as.data.frame(do.call(rbind, post_catch))
post_fsh_cpue <- as.data.frame(do.call(rbind, post_fsh_cpue))
post_srv_cpue <- as.data.frame(do.call(rbind, post_srv_cpue))
post_mr <- as.data.frame(do.call(rbind, post_mr))
post_rec <- as.data.frame(do.call(rbind, post_rec))
post_biom <- as.data.frame(do.call(rbind, post_biom))
post_expl_biom <- as.data.frame(do.call(rbind, post_expl_biom))
post_expl_abd <- as.data.frame(do.call(rbind, post_expl_abd))
post_spawn_biom <- as.data.frame(do.call(rbind, post_spawn_biom))
post_ABC <- as.data.frame(do.call(rbind, post_ABC))
post_wastage <- as.data.frame(do.call(rbind, post_wastage))
post_SB100 <- as.data.frame(do.call(rbind, post_SB100))
post_SB50 <- as.data.frame(do.call(rbind, post_SB50))
post_F50 <- as.data.frame(do.call(rbind, post_F50))

# Summarize posterior samples (see function defns in functions.r)
post_catch <- post_byyear(post_catch, year)
post_fsh_cpue <- post_byyear(post_fsh_cpue, year)
post_srv_cpue <- post_byyear(post_srv_cpue, year)
post_mr <- post_byyear(post_mr, year)
post_rec <- post_byyear(post_rec, year)
post_biom <- post_byyear(post_biom, year)
post_expl_biom <- post_byyear(post_expl_biom, year)
post_expl_abd <-  post_byyear(post_expl_abd, year)
post_spawn_biom <-  post_byyear(post_spawn_biom, year)
post_ABC <- postsum(post_ABC)
post_wastage <- postsum(post_wastage)
post_SB100 <- postsum(post_SB100)
post_SB50 <- postsum(post_SB50)
post_F50 <- postsum(post_F50)

# Save output
write_csv(post_catch, paste0(tmbout, "/post_catch_", YEAR, ".csv"))
write_csv(post_fsh_cpue, paste0(tmbout, "/post_fsh_cpue_", YEAR, ".csv"))
write_csv(post_srv_cpue, paste0(tmbout, "/post_srv_cpue_", YEAR, ".csv"))
write_csv(post_mr, paste0(tmbout, "/post_mr_", YEAR, ".csv"))
write_csv(post_rec, paste0(tmbout, "/post_rec_", YEAR, ".csv"))
write_csv(post_biom, paste0(tmbout, "/post_biom_", YEAR, ".csv"))
write_csv(post_expl_biom, paste0(tmbout, "/post_expl_biom_", YEAR, ".csv"))
write_csv(post_expl_abd, paste0(tmbout, "/post_expl_abd_", YEAR, ".csv"))
write_csv(post_spawn_biom, paste0(tmbout, "/post_spawn_biom_", YEAR, ".csv"))
write_csv(post_ABC, paste0(tmbout, "/post_ABC_", YEAR, ".csv"))
write_csv(post_wastage, paste0(tmbout, "/post_wastage_", YEAR, ".csv"))
write_csv(post_SB100, paste0(tmbout, "/post_SB100_", YEAR, ".csv"))
write_csv(post_SB50, paste0(tmbout, "/post_SB50_", YEAR, ".csv"))
write_csv(post_F50, paste0(tmbout, "/post_F50_", YEAR, ".csv"))

out <- list()
out$post_catch <- post_catch
out$post_fsh_cpue <- post_fsh_cpue
out$post_srv_cpue <- post_srv_cpue
out$post_mr <- post_mr
out$post_rec <- post_rec
out$post_biom <- post_biom
out$post_expl_biom <- post_expl_biom
out$post_expl_abd <- post_expl_abd
out$post_spawn_biom <- post_spawn_biom
out$post_ABC <- post_ABC
out$post_wastage <- post_wastage
out$post_SB100 <- post_SB100
out$post_SB50 <- post_SB50
out$post_F50 <- post_F50


# Results ----
best <- obj$env$last.par.best
print(as.numeric(best))
print(best)
obj$report()$priors
pred_rec <- obj$report()$pred_rec
mean(pred_rec)


obj$report()$S[nyr,,1]
obj$report()$S[1,,2]
obj$report()$catch_like
obj$report()$index_like
obj$report()$age_like
obj$report()$pred_mr
obj$report()$obj_fun
obj$report()$pred_landed ==obj$report()$pred_catch
obj$report()$pred_wastage * 2204.62
obj$report()$F

ABC <- as.data.frame(obj$report(obj$env$last.par.best)$ABC * 2.20462)
names(ABC) <- data$Fxx_levels
ABC <- ABC %>% 
  mutate(year = c(unique(ts$year), max(ts$year)+1)) %>% 
  data.table::melt(id.vars = c("year"), variable.name = "Fxx", value.name = "ABC")

ABC %>% filter(Fxx == "0.5" & year == 2019)
ABC %>% filter(Fxx == "0.5")
obj$report()$SB
obj$report()$SBPR

obj$report()$mean_rec #obj$env$last.par.best
obj$report(obj$env$last.par.best)$pred_rec

wastage <- as.data.frame(obj$report()$wastage * 2.20462)
names(wastage) <- data$Fxx_levels
wastage <- wastage %>% 
  mutate(year = c(unique(ts$year), max(ts$year)+1)) %>% 
  data.table::melt(id.vars = c("year"), variable.name = "Fxx", value.name = "wastage")

retro_mgt <- ABC %>% 
  left_join(wastage) %>% 
  melt(id.vars = c("year", "Fxx")) %>% 
  mutate(variable = factor(variable, 
                           levels = c("wastage", "ABC"),
                           labels = c("Wastage", "ABC"),
                           ordered = TRUE))

# Parameter estimates and standard errors in useable format
tidyrep <- tidy(summary(rep))
names(tidyrep) <- c("Parameter", "Estimate", "se")

# "Key" parameters (exclude)
key_params <- filter(tidyrep, !grepl('devs', Parameter))

write_csv(key_params, paste0("../output/tmb_params.csv"))

# Figures ----

# Fits to abundance indices, derived time series, and F
plot_ts()
plot_ts_resids()
plot_derived_ts()
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

summary(rep, "report")
rep$value
rep$sd

ggplot() +
  geom_area(data = retro_mgt %>% 
              filter(Fxx == "0.5"), aes(x = year, y = value, fill = variable), 
            position = "stack") +
  geom_line(data = ts %>% 
              select(year, catch) %>% 
              mutate(catch = catch * 2204.62),
            aes(x = year, y = catch)) 

obj$report()$Fxx

dat_like <- sum(obj$report()$catch_like,
                obj$report()$index_like[1], obj$report()$index_like[3],
                obj$report()$index_like[3],
                obj$report()$age_like[1], obj$report()$age_like[2],
                sum(obj$report()$fsh_len_like),  sum(obj$report()$srv_len_like))

like_sum <- data.frame(like = c("Catch", 
                                "Fishery CPUE", "Survey CPUE", 
                                "Mark-recapture abundance",
                                "Survey ages", "Fishery ages", 
                                "Survey lengths", "Fishery lengths",
                                "Data likelihood",
                                "Total likelihood"),
                       value = c(obj$report()$catch_like,
                                 obj$report()$index_like[1], obj$report()$index_like[3],
                                 obj$report()$index_like[3],
                                 obj$report()$age_like[1], obj$report()$age_like[2],
                                 sum(obj$report()$fsh_len_like),  sum(obj$report()$srv_len_like),
                                 dat_like,
                                 obj$report()$obj_fun
                       )) %>% 
  mutate(tot = obj$report()$obj_fun,
         perc = (value / tot) * 100) %>% 
  select(-tot)

write_csv(like_sum, paste0("../output/tmb_likelihoods.csv"))

exp(as.list(rep, what = "Estimate")$fsh_logq)
exp(as.list(rep, what = "Estimate")$srv_logq)
exp(as.list(rep, what = "Estimate")$mr_logq)
# as.list(rep, what = "Std")

exp(as.list(rep, what = "Estimate")$log_rbar)
# variance-covriance
VarCo <- solve(obj$he())
# Check for Hessian
print(sqrt(diag(VarCo)))

D <- obj$he()
D <- as.data.frame(D)
names(D) <- rep$par.fixed %>% names

write_csv(x = D, "Dmatrix.csv")


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
