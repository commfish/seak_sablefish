
# ASA that includes catch, fishery and survey CPUE, mark-recapture abundance
# estimates, fishery and survey weight-at-age, survey data about maturity-at-age
# and proportions-at-age, and fishery and survey age compositions. No sex
# structure.

# Set up ----

# Temporary debug flag, shut off estimation of mgmt ref pts
tmp_debug <- TRUE

source("r/helper.r")
source("r/functions.r")

library(TMB) 

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
rowSums(ageing_error)

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
lyr <- max(ts$year)                   # end year
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
  Fxx_levels = c(0.35, 0.40, 0.50),
  
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
  wt_rec_like = 0.5,
  wt_fpen = 0.1,
  wt_spr = 100,
  
  # Catch
  data_catch = ts$catch,
  sigma_catch = pull(ts, sigma_catch),
  
  # Mark-recapture estimates
  nyr_mr = n_distinct(mr, mr),
  yrs_mr = mr %>% distinct(index) %>% pull(),
  data_mr = pull(mr, mr),
  sigma_mr = rep(0.05,11),#mr %>% pull(sigma_mr),
  
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
setwd("tmb")

# Compile and run model
out <- TMBphase(data, parameters, random = random_vars, model_name = "mod", debug = FALSE)

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

agecomps <- reshape_age()
plot_sel() # Selectivity
plot_age_resids() # Fits to age comps
barplot_age("Survey")
barplot_age("Fishery")

lencomps <- reshape_len()
plot_len_resids()
barplot_len("Survey", sex = "Female")
barplot_len("Survey", sex = "Male")

summary(rep, "report")
rep$value
rep$sd

vuln_abd <- as.data.frame(obj$report()$vuln_abd[,,1]) %>% 
  mutate(Sex = "Male") %>% 
  bind_rows(as.data.frame(obj$report()$vuln_abd[,,2]) %>% 
              mutate(Sex = "Female"))       
names(vuln_abd) <- c(rec_age:plus_group, "Sex")
vuln_abd %>% 
  mutate(Source = "Survey",
         index = rep(0:39, nsex)) -> vuln_abd

vuln_abd %>% gather("age", "pred", 1:data$nage) %>% 
  mutate(age = as.numeric(age)) -> vuln_abd

vuln_abd %>% 
  group_by(Sex, index) %>% 
  mutate(tot = sum(pred)) %>% 
  ungroup() %>% 
  mutate(pred_age = pred/tot) %>% 
  arrange(rev(Sex)) %>% 
  filter(index %in% data$yrs_srv_len) -> pred

a_pred <- array(data = pred$pred_age,
                dim = c(data$nyr_srv_len, nage, nsex))

fem_pred <- a_pred[,,2]

prod <- fem_pred %*% agelen_key_f

rowSums(prod)


derived <- data.frame(index = names(rep$value),
                     estimate = rep$value,
                     se = rep$sd)

ssb <- filter(derived, index == "tot_spawn_biom") %>% 
  mutate(year = syr:(lyr+nproj),
         estimate = estimate * 2.20462,
         se = se * 2.20462,
         lower = estimate - 1.96 * se,
         upper = estimate + 1.96 * se)

axis <- tickr(ssb, year, 5)
ggplot(ssb, aes(x = year, y = estimate)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(year, ymin = lower, ymax = upper),
              alpha = 0.2,  fill = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  scale_y_continuous(label = scales::comma) +
  labs(y = "Spawning biomass (lb)\n", x = NULL) +
  expand_limits(y = 0) -> p_ssb

ebiom <- filter(derived, index == "tot_expl_biom") %>% 
  mutate(year = syr:(lyr+nproj),
         estimate = estimate * 2.20462,
         se = se * 2.20462,
         lower = estimate - 1.96 * se,
         upper = estimate + 1.96 * se)

axis <- tickr(ebiom, year, 5)
ggplot(ebiom, aes(x = year, y = estimate)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(year, ymin = lower, ymax = upper),
              alpha = 0.2,  fill = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  scale_y_continuous(label = scales::comma) +
  labs(y = "Exploitable biomass (lb)\n", x = NULL) +
    expand_limits(y = 0) -> p_ebiom

waste <- filter(derived, index == "pred_wastage") %>% 
  mutate(year = syr:(lyr),
         estimate = estimate * 2204.62,
         se = se * 2204.62,
         lower = estimate - 1.96 * se,
         upper = estimate + 1.96 * se)

axis <- tickr(waste, year, 5)
ggplot(waste, aes(x = year, y = estimate)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(year, ymin = lower, ymax = upper),
              alpha = 0.2,  fill = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  scale_y_continuous(label = scales::comma) +
  labs(y = "Wastage (lb)\n", x = NULL) +
  expand_limits(y = 0) -> p_waste

eabd <- filter(derived, index == "tot_expl_abd") %>%
  mutate(year = syr:(lyr+nproj),
         estimate = estimate / 1e6,
         se = se  / 1e6,
         lower = estimate - 1.96 * se,
         upper = estimate + 1.96 * se) 

axis <- tickr(eabd, year, 5)
ggplot(eabd, aes(x = year, y = estimate)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(year, ymin = lower, ymax = upper),
              alpha = 0.2,  fill = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  scale_y_continuous(label = scales::comma) +
  labs(y = "Exploitable abundance\n(millions)\n", x = NULL) +
  expand_limits(y = 0) -> p_eabd

rec <- filter(derived, index == "pred_rec") %>%
  mutate(year = syr:(lyr),
         estimate = estimate / 1e6,
         se = se  / 1e6,
         std = 1.96 * sqrt(log(se + 1)),
         ln_estimate = log(estimate),
         lower = exp(ln_estimate - std),
         upper = exp(ln_estimate + std)) 

axis <- tickr(rec, year, 5)
ggplot(rec, aes(x = year, y = estimate)) +
  geom_point() +
  geom_line() +
  # geom_ribbon(aes(year, ymin = lower, ymax = upper),
  #             alpha = 0.2,  fill = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  scale_y_continuous(label = scales::comma) +
  labs(y = "Age-2 recruits\n(millions)\n", x = NULL) +
  expand_limits(y = 0) -> p_rec

expl_abd %>% filter(year >= 2005)


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
obj$report()$pred_wastage * 2204.62
obj$report()$ABC * 2.20462
obj$report()$wastage * 2.20462
obj$report()$Fxx

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
# 
# Compile
compile("mod.cpp")
# dyn.load(dynlib("mod"))
# Use map to turn off parameters, either for testing with dummy, phasing, or to
# fix parameter values

# Debug
# map <- list(log_fsh_slx_pars = factor(array(data = c(rep(factor(NA), length(data$fsh_blks)),
#                                      rep(factor(NA), length(data$fsh_blks))),
#                             dim = c(length(data$fsh_blks), 2, nsex))),
#             log_srv_slx_pars = factor(array(data = c(rep(factor(NA), length(data$srv_blks)),
#                                                  rep(factor(NA), length(data$srv_blks))),
#                                         dim = c(length(data$srv_blks), 2, nsex))),
#             fsh_logq = factor(NA), srv_logq = factor(NA), mr_logq = factor(NA),
#             log_rbar = factor(NA), log_rec_devs = rep(factor(NA), nyr),
#             log_rinit = factor(NA), log_rinit_devs = rep(factor(NA), nage-2),
#             log_sigma_r = factor(NA), log_Fbar = factor(NA), log_F_devs = rep(factor(NA), nyr),
#             log_spr_Fxx = rep(factor(NA), length(data$Fxx_levels)),
#             log_fsh_theta = factor(NA), log_srv_theta = factor(NA))
# model <- MakeADFun(data, parameters, DLL = "mod", 
#                    silent = TRUE, map = map,
#                    random = random_vars)
# 
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
  labs(x = "\nObserved age", y = "True age\n", size = NULL)

ggsave(paste0("figures/tmb/ageing_error.png"), dpi = 300, height = 6, width = 7, units = "in")
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

# MCMC ----

install.packages("shinystan")
library(tmbstan)
library(rstan)
library(shinystan)

map <- list(dummy = factor(NA),
            log_fsh_slx_pars = factor(array(data = c(rep(factor(NA), length(data$fsh_blks)),
                                                     rep(factor(NA), length(data$fsh_blks))),
                                            dim = c(length(data$fsh_blks), 2, nsex))),
            log_srv_slx_pars = factor(array(data = c(rep(factor(NA), length(data$srv_blks)),
                                                     rep(factor(NA), length(data$srv_blks))),
                                            dim = c(length(data$srv_blks), 2, nsex))),
            log_sigma_r = factor(NA), log_Fbar = factor(NA), log_F_devs = rep(factor(NA), nyr),
            log_spr_Fxx = rep(factor(NA), length(data$Fxx_levels)),
            log_fsh_theta = factor(NA), log_srv_theta = factor(NA))

# Compile
compile("mod.cpp")
dyn.load(dynlib("mod"))

obj <- MakeADFun(data, parameters, DLL = "mod",
                 silent = TRUE, map = map,
                 random = random_vars)
# obj <- MakeADFun(data = data, parameters = parameters, random = random)

options(mc.cores = 3)

fit <- tmbstan(obj = obj, chains = 3, init = rep$par.fixed) # used MLE estimates for initial values

launch_shinystan(fit)

# Can you get all these values by iterating over the posterior samples that is
# output from tmbstan into the obj$report(). I have attached some pseudo code to
# hopefully help, probably not the must efficient but I think it should work.
# 
# obj = MakeADFun(...)
# tmb_stan_par_post = tmbstan(obj = obj, chains = 1,...); # one chain for simplicity, you will have to append multiple chains
# ## assuming we satisfy Bayesian checks convergence to an equilibrium posterior
# for (i in 1:nrow(tmb_stan_par_post)) {
#   rep_values = obj$report(tmb_stan_par_post[i]) ## might not be exactly right syntactically, but hopefully you get the gist
#   # save values that you want, rbind or other ways to store specific values of the list object rep_values 
# }
