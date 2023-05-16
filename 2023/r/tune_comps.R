# Tune composition data ----

# McAllister-Ianelli (1997): This method sets the effective sample size by
# comparing the residual variance with the variance expected under a multinomial
# distribution. An overall effective sample size for each composition series is
# calculated as the harmonic mean across years at each iteration (Stewart and
# Hamel 2014). 

# Equations used:
#   
# Nhat_i = sum_j{phat_ij * (1 - phat_ij)} / sum_j{(p_ij - phat_ij)^2}  #R, equ6 in Stewart and Hamel?? 
# Nhat = n / sum_i{(Nhat_i)^(-1)}

# i = year, j = age or length
# Nhat = effective sample size
# p_ij = observed age or length comp in a given year #
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
YEAR <- 2022
source("r_helper/helper.r")
source("r_helper/functions.r")
source("r_helper/exp_functions.r")

library(TMB) 
library(tmbstan)
library(shinystan)

#-------------------------------------------------------------------------------
# get data and parameter stuff as per 2023 model run... 

root <- getwd() # project root
tmb_dat <- file.path(root, paste0(YEAR+1,"/data/tmb_inputs")) # location of tmb model data inputs
tmb_path <- file.path(root, paste0(YEAR+1,"/tmb")) # location of cpp
tmbfigs <- file.path(root, paste0(YEAR+1,"/figures/tmb")) # location where model figs are saved
tmbout <- file.path(root, paste0(YEAR+1,"/output/tmb")) # location where model output is saved

# Model switches
{
  rec_type <- 1     # Recruitment: 0 = penalized likelihood (fixed sigma_r), 1 = random effects (still under development)
  slx_type <- 1     # Selectivity: 0 = a50, a95 logistic; 1 = a50, slope logistic
  comp_type <- 0    # Age  and length comp likelihood (not currently developed for len comps): 0 = multinomial, 1 = Dirichlet-multinomial
  spr_rec_type <- 1 # SPR equilbrium recruitment: 0 = arithmetic mean, 1 = geometric mean, 2 = median (not coded yet)
  M_type <- 0       # Natural mortality: 0 = fixed, 1 = estimated with a prior
  ev_type <- 0     # extra variance in indices; 0 = none, 1 = estimated
}

# Load prepped data from scaa_dataprep.R
{
  ts <- read_csv(paste0(tmb_dat, "/abd_indices_truesig_", YEAR, ".csv")) #"/abd_indices_", YEAR, ".csv"))       # time series
  age <- read_csv(paste0(tmb_dat, "/agecomps_", YEAR, ".csv"))          # age comps
  len <- read_csv(paste0(tmb_dat, "/lencomps_", YEAR, ".csv"))          # len comps
  # age <- read_csv(paste0(tmb_dat, "/tuned_agecomps_", YEAR, ".csv"))  # tuned age comps - see tune_comps.R for prelim work on tuning comps using McAllister/Ianelli method
  # len <- read_csv(paste0(tmb_dat, "/tuned_lencomps_", YEAR, ".csv"))  # tuned len comps
  bio <- read_csv(paste0(tmb_dat, "/maturity_sexratio_", YEAR, ".csv")) # proportion mature and proportion-at-age in the survey
  waa <- read_csv(paste0(tmb_dat, "/waa_", YEAR, ".csv"))               # weight-at-age
  retention <- read_csv(paste0(tmb_dat, "/retention_probs.csv"))        # retention probability (not currently updated annually. saved from ypr.r)
}
  
VER<-"Base"
slx_pars <- read_csv(paste0(YEAR+1,"/data/tmb_inputs/fed_selectivity_transformed_2020.csv"))
#2) Base with updated selectivity
VER<-"Base_new_slx"
slx_pars <- read_csv(paste0(YEAR+1,"/data/tmb_inputs/fed_selectivity_transformed_2022.csv"))
#3) v23 old selectivity
VER<-"v23_old_slx"
slx_pars <- read_csv(paste0(YEAR+1,"/data/tmb_inputs/fed_selectivity_transformed_2020.csv"))
tmp<-slx_pars[3:4,]  #using 3 time block model so need to duplicate 2nd into 3rd block... 
tmp$fleet<-c("fsh_t3_f","fsh_t3_m")
slx_pars<-rbind(slx_pars,tmp)

#4) v23 new selectivity, 2 time blocks
VER<-"v23_new_2slx"
slx_pars <- read_csv(paste0(YEAR+1,"/data/tmb_inputs/fed_selectivity_transformed_2022.csv"))
tmp<-slx_pars[3:4,]  #using 3 time block model so need to duplicate 2nd into 3rd block... 
tmp$fleet<-c("fsh_t3_f","fsh_t3_m")
slx_pars<-rbind(slx_pars,tmp)

#5) v23 new selectivity, 3 time blocks
VER<-"v23_new_3slx"
slx_pars <- read_csv(paste0(YEAR+1,"/data/tmb_inputs/fed_selectivity_transformed_2022_3fsh.csv"))# fed slx transformed to ages 0:29 instead of ages 2:31. see scaa_datprep.R for more info
  
  # Ageing error transition matrix from D. Hanselman 2019-04-18. On To Do list to
  # develop one for ADFG. Row = true age, Column = observed age. Proportion
  # observed at age given true age.
  
{  
  ageing_error <- scan(paste0(YEAR+1,"/data/tmb_inputs/ageing_error_fed.txt", sep = " ")) %>% matrix(ncol = 30) %>% t()
  rowSums(ageing_error) # should be 1
  
  # Age-length key from D. Hanselman 2019-04-18. On To DO list to develop one for
  # ADFG (will need separate ones for fishery and survey). See
  # ageing_error_matrix.R for KVK's code, which may be a good start.  Proportion
  # at length given age. Row = age, Column = length bin
  agelen_key_m <- scan(paste0(YEAR+1,"/data/tmb_inputs/agelen_key_male.txt", sep = " "), skip = 1) %>% matrix(ncol = 30) %>% t()
  rowSums(agelen_key_m) # should all = 1
  agelen_key_f <- scan(paste0(YEAR+1,"/data/tmb_inputs/agelen_key_fem.txt", sep = " "), skip = 1) %>% matrix(ncol = 30) %>% t()
  rowSums(agelen_key_f) 
  
  #inits <- read_csv(paste0(tmb_dat, "/inits_for_", YEAR+1, ".csv"))
  #need to add in new selectivity block since not there last year
  inits <- read_csv(paste0(tmb_dat, "/inits_for_", YEAR+1, "_NEW_SLX.csv"))
  
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
  tmp_debug <- TRUE         # Shuts off estimation of selectivity pars - once selectivity can be estimated, turn to FALSE
  
  # Subsets
  mr <- filter(ts, !is.na(mr))
  fsh_cpue <- filter(ts, !is.na(fsh_cpue))
  srv_cpue <- filter(ts, !is.na(srv_cpue))
  fsh_age <- filter(age, Source == "Fishery")
  srv_age <- filter(age, Source == "Survey")
  fsh_len <- filter(len, Source == "fsh_len")
  srv_len <- filter(len, Source == "srv_len")
  
  length(c(YEAR - length(rec_devs_inits)+1):YEAR)
  length(rec_devs_inits)
  length(Fdevs_inits)
  
  # initial value processing
  tmp_inits <- data.frame(year = c(YEAR - length(rec_devs_inits)+1):YEAR,
                          rec_devs_inits = rec_devs_inits,
                          Fdevs_inits = Fdevs_inits) %>% 
    filter(between(year, syr, lyr))
  rec_devs_inits <- tmp_inits %>% pull(rec_devs_inits)
  Fdevs_inits <- tmp_inits %>% pull(Fdevs_inits)
  
  # some other things
}

# TMB set up ----
ts$fsh_cpue          #in 2023 we are using the fully standardized time series
ts$fsh_cpue_nom      #nominal fishery CPUE from analysis
ts$fsh_cpue_base     #cpue clculation in scaa_dataprep.R.. similar to nom

#=====================================
# *** Checking sensitivity to fishery CPUE data versions
VER<-"v23_hand_taued_sig" #"boot_gam22"  #"base_22rb" #"base" #"boot_gam" #"base_gam" #"base_nom" 
#data$data_fsh_cpue<-ts$fsh_cpue_22rb[!is.na(ts$fsh_cpue_22rb)]

#-------------------------------------------------------------------------------
# Load data and parameters
# If base model
data <- build_data(ts = ts); str(data)  #see this function to change weights and 
parameters <- build_parameters(rec_devs_inits = rec_devs_inits, Fdevs_inits = Fdevs_inits)
#if v23 model

data <- build_data_exp(ts = ts, weights=FALSE)   #TRUE means fixed weights, FALSE = flat weights (all wts = 1)
parameters <- build_parameters_exp(rec_devs_inits = rec_devs_inits, Fdevs_inits = Fdevs_inits)

#no matter model, do this... 
random_vars <- build_random_vars() # random effects still in development
#-------------------------------------------------------------------------------
# Run model ----

setwd(tmb_path)

tune_fsh_age <- list()
tune_srv_age <- list()
tune_fsh_len <- list()
tune_srv_len <- list()

# Iterate ----
niter <- 8

for(iter in 1:niter) { #iter<-1
  
  # MLE, phased estimation (phase = TRUE) or not (phase = FALSE)
  out <- TMBphase_exp(data, parameters, random = random_vars, 
                  model_name = "scaa_mod_dir_ev", phase = FALSE, 
                  newtonsteps = 5, #3 make this zero initially for faster run times (using 5)
                  debug = FALSE)
  #out <- TMBphase(data, parameters, random = random_vars, 
  #                    model_name = "scaa_mod", phase = FALSE, 
  #                    newtonsteps = 5, #3 make this zero initially for faster run times (using 5)
  #                    debug = FALSE)
  
  obj <- out$obj # TMB model object
  opt <- out$opt # fit
  
  # Quick look at MLE results
  best <- obj$env$last.par.best 

  # Fishery age comps (sexes combined) ----
  pred_fsh_age <- as.matrix(obj$report(best)$pred_fsh_age)
  data_fsh_age <- as.matrix(data$data_fsh_age)
  effn_fsh_age <- vector(length = nrow(pred_fsh_age))
  
  for(i in 1:nrow(pred_fsh_age)){
    effn_fsh_age[i] <- sum(pred_fsh_age[i,]*(1-pred_fsh_age[i,])) / sum((data_fsh_age[i,]-pred_fsh_age[i,])^2)  #Equation 2.5 in Mcalister and Ianelli
    # Nhat_i = sum_j{phat_ij * (1 - phat_ij)} / sum_j{(p_ij - phat_ij)^2}  #R, equ6 in Stewart and Hamel?? 
    #    phils_q_fhs_age[i] <- sum(pred_fsh_age[i,]*(1-pred_fsh_age[i,])) / sum((pred_fsh_age[i,]-data_fsh_age[i,])^2) #based on eq6 from Stewart & Hamel??
    ## but to be true to Stewart and Hamel data should be bootstrapped estimates? 
  }
  
  effn_fsh_age <- 1/mean(1/effn_fsh_age) # harmonic mean from Stewat and Hamel... 

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
  write_csv(paste0(tmb_dat, "/tuned_agecomps_", YEAR,"_", VER,".csv"))

# Save fishery and survey length comps
fsh_len %>% 
  mutate(effn = ifelse(fsh_len$Sex == "Male", tune_fsh_len[niter,1], tune_fsh_len[niter,2])) %>% 
  bind_rows(srv_len %>% 
              mutate(effn = ifelse(srv_len$Sex == "Male", tune_srv_len[niter,1], tune_srv_len[niter,2]))) %>% 
  write_csv(paste0(tmb_dat, "/tuned_lencomps_", YEAR,"_", VER, ".csv"))

# Compare tuned to data...
# or don't... we've just changed the effective sample size... 
tuned_age<-read.csv(paste0(tmb_dat, "/tuned_agecomps_", YEAR, ".csv"))
tuned_len<-read.csv(paste0(tmb_dat, "/tuned_lencomps_", YEAR, ".csv"))

age <- read_csv(paste0(tmb_dat, "/agecomps_", YEAR, ".csv"))          # age comps
len <- read_csv(paste0(tmb_dat, "/lencomps_", YEAR, ".csv"))  

colnames(tuned_age)<- colnames(age)
colnames(tuned_len)<- colnames(len)

age_comp<-age %>% mutate(derivation = "raw") %>%
  bind_rows(tuned_age %>% mutate(derivation = "tuned"))

len_comp<-len %>% mutate(derivation = "raw") %>%
  bind_rows(tuned_len %>% mutate(derivation = "tuned"))

str(age_comp)
age_comp %>% filter(year == 2010) %>% data.frame()
tuned_age %>% filter(year == 2010) %>% data.frame()
age %>% filter(year == 2010) %>% data.frame()

age_comp %>% 
  pivot_longer(cols=c(as.character(seq(2,31,1))),
               names_to = "age",
               values_to = "proportion") %>% 
  data.frame() -> age_comp

ggplot(age_comp %>% filter(Source == "Fishery") %>%
         mutate(age = as.numeric(age))) + 
  geom_col(aes(x = age, y = proportion, 
               fill = derivation, col=derivation),
           position = position_dodge(width = 0.8)) +
  facet_wrap(.~year)

ggplot(age_comp %>% filter(Source == "Survey") %>%
         mutate(age = as.numeric(age))) + 
  geom_col(aes(x = age, y = proportion, 
               fill = derivation, col=derivation),
           position = position_dodge(width = 0.8)) +
  facet_wrap(.~year)

unique(len_comp$Source)
ggplot(len_comp %>% filter(Source == "fsh_len",
                           Sex == "Female")) + 
  geom_col(aes(x = length_bin, y = proportion, 
               fill = derivation, col=derivation),
           position = position_dodge(width = 2)) +
  facet_wrap(.~year)

ggplot(len_comp %>% filter(Source == "srv_len",
                           Sex == "Female")) + 
  geom_col(aes(x = length_bin, y = proportion, 
               fill = derivation, col=derivation),
           position = position_dodge(width = 2)) +
  facet_wrap(.~year)

ggplot(len_comp %>% filter(Source == "fsh_len",
                           Sex == "Male")) + 
  geom_col(aes(x = length_bin, y = proportion, 
               fill = derivation, col=derivation),
           position = position_dodge(width = 2)) +
  facet_wrap(.~year)

ggplot(len_comp %>% filter(Source == "srv_len",
                           Sex == "Male")) + 
  geom_col(aes(x = length_bin, y = proportion, 
               fill = derivation, col=derivation),
           position = position_dodge(width = 2)) +
  facet_wrap(.~year)



