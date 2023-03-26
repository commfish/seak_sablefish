
# Sex-structured statistical catch-at-age model that includes catch, fishery and
# survey CPUE, mark-recapture abundance estimates, fishery and survey
# weight-at-age, survey data about maturity-at-age and proportions-at-age, and
# fishery and survey age and length compositions.

# Original author: jane.sullivan@noaa.gov
# Current driver: philip.joy@alaska.gov
# Last updated March 2023

# The following libraries are used for running the SCAA as a Bayesian model,
# which are still under development. A lot of the infrastructure has been
# developed (running models, saving output, diagnostics, plotting figure fxns_,
# but the model is very slow and needs to be optimized. A great place to start
# is the publication and supplementary material for: Cole C Monnahan, Trevor A
# Branch, James T Thorson, Ian J Stewart, Cody S Szuwalski, Overcoming long
# Bayesian run times in integrated fisheries stock assessments, ICES Journal of
# Marine Science, Volume 76, Issue 6, November-December 2019, Pages 1477–1488,
# https://doi-org.arlis.idm.oclc.org/10.1093/icesjms/fsz059

# library(tmbstan)
# library(shinystan)

# Manual inputs ----

# These must be checked or updated annually!

# most recent year of data (YEAR+1 should be the forecast year)
{
YEAR <- 2022

# Last years ABC, mortality from discards, and F_ABC values - manually input
# from previous assessment! Double check these values with the summary table.
# Note that in years when the recommended ABC = maxABC, there will be repeats
# (previous years values commented out for reference/check)
LYR_maxABC <- 1595932#for 2022: 1255056 #pj22; 1280406 #1058037 # maxABC for YEAR (ABC under F50)
LYR_recABC <- 1443314#for 2022: 1255056 #pj22 1216743 #1058037 # recommended ABC for YEAR
LYR_wastage <- 72190 #59017 #pj22; 57716 #19142 # wastage for YEAR (this is only defined under maxABC b/c it's included in the calculation of maxABC for the 2020 forecast and beyond)
LYR_maxF_ABC <- 0.0617#0.0611 #pjj22 (same as Rec ABC in 21 report?) ;  0.0765 #0.0632 # F50 for YEAR
LYR_F_ABC <- 0.0559 #0.0611 #pj22, 0.0659 #0.0632 # F under the recommended ABC

# Last years projected biomass and SPR - the old assessment framework didn't
# report these. They are reported in 2020 and will be reported for
# comparison moving forward similar to federal assessment. These values are
# reported in assessment summary table

LYR_proj_age2plus <- 51885665 #for 2022: 43357877 #pj22; 48513401 # projected age-2+ biomass
LYR_proj_fSSB <- 19714244 #for 2022:15278067 #pj22; 15679118 # projected female spawning biomass
LYR_SB100 <- 28995917 #for 2022:26775615 #pj22; 24853774 # unfished equilibrium female spawning biomass (SPR = 100)
LYR_SB50 <- 14497958 #for 2022:13387807 #pj22; 12426887 # equilibrium female spawning biomass under F50 (SPR = 50)

# Set up ----

# Directory setup
root <- getwd() # project root
tmb_dat <- file.path(root, paste0(YEAR+1,"/data/tmb_inputs")) # location of tmb model data inputs
tmb_path <- file.path(root, paste0(YEAR+1,"/tmb")) # location of cpp
tmbfigs <- file.path(root, paste0(YEAR+1,"/figures/tmb")) # location where model figs are saved
tmbout <- file.path(root, paste0(YEAR+1,"/output/tmb")) # location where model output is saved

source("r_helper/helper.r")
source("r_helper/functions.r")

library(TMB) 
}

# Model switches
{
rec_type <- 0     # Recruitment: 0 = penalized likelihood (fixed sigma_r), 1 = random effects (still under development)
slx_type <- 1     # Selectivity: 0 = a50, a95 logistic; 1 = a50, slope logistic
comp_type <- 0    # Age comp likelihood (not currently developed for len comps): 0 = multinomial, 1 = Dirichlet-multinomial
spr_rec_type <- 1 # SPR equilbrium recruitment: 0 = arithmetic mean, 1 = geometric mean, 2 = median (not coded yet)
M_type <- 0       # Natural mortality: 0 = fixed, 1 = estimated with a prior
}

# Load prepped data from scaa_dataprep.R
{
ts <- read_csv(paste0(tmb_dat, "/abd_indices_CPUEsense_", YEAR, ".csv")) #"/abd_indices_", YEAR, ".csv"))       # time series
age <- read_csv(paste0(tmb_dat, "/agecomps_", YEAR, ".csv"))          # age comps
len <- read_csv(paste0(tmb_dat, "/lencomps_", YEAR, ".csv"))          # len comps
# age <- read_csv(paste0(tmb_dat, "/tuned_agecomps_", YEAR, ".csv"))  # tuned age comps - see tune_comps.R for prelim work on tuning comps using McAllister/Ianelli method
# len <- read_csv(paste0(tmb_dat, "/tuned_lencomps_", YEAR, ".csv"))  # tuned len comps
bio <- read_csv(paste0(tmb_dat, "/maturity_sexratio_", YEAR, ".csv")) # proportion mature and proportion-at-age in the survey
waa <- read_csv(paste0(tmb_dat, "/waa_", YEAR, ".csv"))               # weight-at-age
retention <- read_csv(paste0(tmb_dat, "/retention_probs.csv"))        # retention probability (not currently updated annually. saved from ypr.r)
slx_pars <- read_csv(paste0(YEAR+1,"/data/tmb_inputs/fed_selectivity_transformed_2020.csv")) # fed slx transformed to ages 0:29 instead of ages 2:31. see scaa_datprep.R for more info

# Ageing error transition matrix from D. Hanselman 2019-04-18. On To Do list to
# develop one for ADFG. Row = true age, Column = observed age. Proportion
# observed at age given true age.
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

# User-defined fxns in functions.R
data <- build_data(ts = ts); str(data)  #see this function to change weights and 
# some other things
}
# TMB set up ----
ts$fsh_cpue          #in 2023 we are using the fully standardized time series
ts$fsh_cpue_nom      #nominal fishery CPUE from analysis
ts$fsh_cpue_base     #cpue clculation in scaa_dataprep.R.. similar to nom


str(data$data_fsh_cpue)

#=====================================
# *** Checking sensitivity to fishery CPUE data versions
VER<-"base" #"boot_gam22"  #"base_22rb" #"base" #"boot_gam" #"base_gam" #"base_nom" 
VER<-"tuned"
VER<-"dirichlet_full_DEV"
VER<-"extra_ind_var_DEV"
#data$data_fsh_cpue<-ts$fsh_cpue_22rb[!is.na(ts$fsh_cpue_22rb)]
#==================================================

#ughs<-inits %>% filter(grepl("spr_Fxx", Parameter)) %>% pull(Estimate)
#exp(ughs)

parameters <- build_parameters(rec_devs_inits = rec_devs_inits, Fdevs_inits = Fdevs_inits)

parameters <- build_parameters_exp(rec_devs_inits = rec_devs_inits, Fdevs_inits = Fdevs_inits)
random_vars <- build_random_vars() # random effects still in development

# parameters <- list(dummy = 0)
# compile("tst.cpp")
data$data_fsh_len
str(data$data_fsh_len)
data$data_fsh_len[,,2]

str(data$n_fsh_len)
data$n_fsh_len[,2,]

data$nlenbin
data$nyr_fsh_len

data$nage
data$data_srv_age

bounds <- build_bounds(param_list = parameters)
# Run model ----

setwd(tmb_path)

# Four ways to run model:
# (1) Maximum Likelihood Estimation (MLE) in phases (like ADMB). Code defined by build_phases() in functions.R.
# Use TMBphase(phase = TRUE)
# (2) MLE without phased optimization (phase = FALSE)
# (3) Bayesian using tmbstan and the no-U-turn sampler (NUTS): Run step 2 first
# to build TMB object, map, and bounds
# (4) Debug mode with debug = TRUE (will need to uncomment out obj_fun = dummy * dummy; )

str(data)

# MLE, phased estimation (phase = TRUE) or not (phase = FALSE)
out <- TMBphase(data, parameters, random = random_vars, 
                model_name = "scaa_mod_dir_ev", phase = FALSE, 
                newtonsteps = 3, #3 make this zero initially for faster run times (using 5)
                debug = FALSE)

obj <- out$obj # TMB model object
opt <- out$opt # fit
rep <- out$rep # sdreport
lower <- out$lower # bounds
upper <- out$upper

# Report gives you a look at estimated parameters with standard errors. The
# maximum gradient component is a diagnostic of convergence; it should be <=
# 0.001. If sufficiently small use newtonsteps > 0 to further reduce grad.
rep 
best <- obj$env$last.par.best # maximum likelihood estimates

# MLE results ----

# MLE parameter estimates and standard errors in useable format. Saves output to
# tmbout and starting vals for next year to tmb_dat by default. See functions.R
# for more info.
tidyrep <- save_mle(save = TRUE,
                    save_inits = TRUE) 

# MLE likelihood components
obj$report(best)$obj_fun
obj$report(best)$priors
obj$report(best)$prior_M # should be 0 as long as M is not estimated 

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
like_sum

write_csv(like_sum, paste0(tmbout, "/likelihood_components_", YEAR,"_",VER, ".csv"))

# MLE figs ----

# Fits to abundance indices, derived time series, and F. Use units = "imperial" or
# "metric" to switch between units. 
plot_ts(ts = ts, save = TRUE, units = "imperial", plot_variance = FALSE, path = tmbfigs)
plot_derived_ts(ts = ts, save = TRUE, path = tmbfigs, units = "imperial", plot_variance = FALSE)
plot_F(save = TRUE)

# Recruitment estimates
logrbar <- tidyrep %>% filter(Parameter == "log_rbar") %>% pull(Estimate)
logrdevs <- tidyrep %>% filter(grepl("log_rec_devs", Parameter)) %>% pull(Estimate)
rec <- data.frame(year = syr:lyr,
                  rec = exp(logrbar + logrdevs) / 1e6)
# Add brood year
rec <- rec %>% 
  mutate(brood_year = year - rec_age)
rec
rec %>% filter(brood_year == 2014) %>% pull(rec)
rec %>% filter(brood_year == 2015) %>% pull(rec)
rec %>% filter(brood_year == 2016) %>% pull(rec)
rec %>% filter(brood_year == 1978) %>% pull(rec)

agecomps <- reshape_age()
plot_sel(save = TRUE) # Selectivity, fixed at Federal values

plot_age_resids() # Fits to age comps
barplot_age("Survey")
barplot_age("Fishery")

lencomps <- reshape_len()
plot_len_resids()
barplot_len("Survey", sex = "Female")
barplot_len("Survey", sex = "Male")
barplot_len("Fishery", sex = "Female")
barplot_len("Fishery", sex = "Male")

# Assessment results  ----

# Prep ABC output (ABC = Acceptable Biological Catch)
ABC <- as.data.frame(obj$report(best)$ABC * 2.20462)
names(ABC) <- data$Fxx_levels
ABC <- ABC %>% 
  mutate(year = c(unique(ts$year), max(ts$year)+1)) %>% 
  pivot_longer(-year, names_to = "Fxx", values_to = "ABC")

ABC %>% filter(Fxx == 0.5) %>% data.frame()

# Current (YEAR + 1) maximum ABC under F50
(maxABC <- ABC %>% filter(Fxx == "0.5" & year == YEAR+1) %>% pull(ABC))

# wastage = mortality from discards. Already included in ABC calculation
wastage <- as.data.frame(obj$report()$wastage * 2.20462)
names(wastage) <- data$Fxx_levels
wastage <- wastage %>% 
  mutate(year = c(unique(ts$year), max(ts$year)+1)) %>% 
  pivot_longer(-year, names_to = "Fxx", values_to = "discarded")

# Current (YEAR + 1) wastage under F50
(wastage_maxABC <- wastage %>% filter(Fxx == "0.5" & year == YEAR+1) %>% pull(discarded))

# Get maxF_ABC
(maxF_ABC <- tidyrep %>% 
    filter(grepl('log_spr_Fxx', Parameter)) %>% 
    mutate(Fspr = data$Fxx_levels) %>% 
    filter(Fspr == 0.5) %>% 
    mutate(maxF_ABC = exp(Estimate)) %>% 
    pull(maxF_ABC))

write_csv(ABC %>% left_join(wastage), paste0(tmbfigs, "/abc_wastage_", YEAR,"_",VER, ".csv"))

# Percent changes and differences for ABC and wastage (used in assessment text)
round((maxABC_diff <- (maxABC - LYR_recABC) / LYR_recABC) * 100, 1)
round(maxABC - LYR_recABC,0)
round((wastage_maxABC - LYR_wastage)/ LYR_wastage * 100, 1)
round((maxF_ABC - LYR_F_ABC) / LYR_F_ABC * 100, 1)

# Constant 15% change management procedure:
if( maxABC_diff > 0.15 ) {
  recABC <- LYR_recABC * 1.15
} else if (maxABC_diff < -0.15) {
  recABC <- LYR_recABC * 0.85
  } else {recABC <- maxABC}
recABC # recommended ABC

# Difference between recommended ABC and last year's recommended ABC
round(recABC-LYR_recABC,0)

# If the maxABC is within 15%, then F_ABC = maxF_ABC, otherwise calculate new F
# for constrained ABC

#CHECK for unstable recruitment issues.  In 2023 we discovered a bug in the code
# that was cuasing unstable estimates.  Fixed now, but check every year as the model
# is refined and developed.  When these were messed up we got different values everytime
# they were called.  These loops should produce identical results with realistic numbers
obj$report()$pred_rec
obj$report()$pred_rbar
obj$report()$mean_rec
obj$report(best)$SB
for (i in 1:25){
  print(obj$report(best)$SB)
}
for (i in 1:25){
  print(obj$report(best)$SBPR)
}
for (i in 1:25){
  print(obj$report(best)$pred_rec)
}

obj$report()$spawn_biom[1,]

for (i in 1:25){
  print(obj$report(best)$mean_rec)
}

# PJ 2022: I think there is an error in here; value for F comes in above maxABC but
# should be less since we are removing fewer fish.  This seems to calculate things
# based on abundance, but should be biomass?  Not sure whats going on here... 
if(recABC == maxABC) {
  (F_ABC <- maxF_ABC)
} else {
  
  # Estimate recommended F_ABC using numerical methods
  N <- obj$report()$N  #str(obj$report()) 
  #Phil insert looking for how to do this ... 
  #str(obj$report())
  #obj$report()$Fxx
  #obj$report()$fsh_slx
  #obj$report()$sel_Fxx
  #obj$report()$spr_Fxx  #not saved
  #obj$report()$spr_fsh_slx #not saved
  #obj$report()$Z_Fxx   #not saved in output...
  #obj$report()$S_Fxx   #not saved in output...
  #obj$report()$pred_rec
  #obj$report()$pred_rbar
  #obj$report()$mean_rec
  #obj$report()$SBPR #SBPR at each fishing level, female biomass only
  #obj$report()$SB   #equilibrium biomass at each fishing level, female biomass only #coming up as Inf first run in 2023??? !!!! 
  
  #back to Jane's code here... 
  N <- sum(N[nyr+1,,1]) + sum(N[nyr+1,,2]) # sum of projected abundance across age and sex
  
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
  
  #catch_to_F(fish_mort=0.07,N=N,nat_mort=nat_mort,catch=recABC,F_to_catch=)
  
  # F under recommended ABC
  (F_ABC <- uniroot(catch_to_F, interval = c(0.03, 1.6), N = N, catch = recABC, nat_mort = nat_mort, F_to_catch = F_to_catch)$root*0.5)
  (F_ABC <- uniroot(catch_to_F, interval = c(0.01, 1.9), N = N, catch = recABC, nat_mort = nat_mort, F_to_catch = F_to_catch)$root*0.5)
}
(F_ABCtest <- uniroot(catch_to_F, interval = c(0.03, 1.6), N = N, catch = maxABC, nat_mort = nat_mort, F_to_catch = F_to_catch)$root*0.5)
#PJ22: this function is producing different F_ABC than that coming out of TMB code!!!
#PJ23: yup, still not working. 0.087 here but 0.063 from TMB outbput... 
#      trusting the TMB output now and will schwag the recABC F value by deprecating
#      the maxABC_F proportional ro reduction from maXABC to recABC..
#      These results saved in output

F_ABC_schwag<-recABC*maxF_ABC/maxABC

#
(F_ABC_schwag - LYR_F_ABC) / LYR_F_ABC # Percent difference from Last year's F

# Projected total age-2+ projected biomass
(proj_age2plus <- obj$report(best)$tot_biom[nyr+1] * 2.20462)

# Comparison with current age-2+ biomass
obj$report(best)$tot_biom[nyr] * 2.20462

# Projected total female spawning biomass
(proj_fSSB <- obj$report(best)$tot_spawn_biom[nyr+1] * 2.20462)

# Unfished/fished SSB  str(obj$report(best))
# *** Something f'ed up here... same call gives different results when you repeat...
# *** gives different multiples of whats happening... DO NOT UNDERSTAND!!!

obj$report(best)$SBPR
data$Fxx_levels

SB <- as.data.frame(obj$report(best)$SB * 2.20462); SB
names(SB) <- "SB"
SB <- SB %>% 
  mutate(Fspr = c(0.0, data$Fxx_levels))

(SB100 <- SB %>% filter(Fspr == 0) %>% pull(SB)) # unfished equilibrium
(SB50 <- SB %>% filter(Fspr == 0.5) %>% pull(SB)) # F50 equilibrium
obj$report()$SBPR # Spawning biomass per recruit

# Differences between this year and last year
(proj_age2plus - LYR_proj_age2plus)/LYR_proj_age2plus

# Sigma R - only if estimated using random effects (still in development)
if(rec_type == 1){
  tidyrep %>%
    filter(grepl('log_sigma_r', Parameter)) %>%
    pull(Estimate) %>%
    exp()
}
# Percent of forecasted ssb 2014 year class makes up
# Projected total female spawning biomass
f_ssb <- obj$report(best)$spawn_biom[nyr+1,] * 2.20462
index_2014 <- which(((YEAR+1) - rec_age:plus_group) == 2014)
round(f_ssb[index_2014] / sum(f_ssb) * 100, 1)

tmp <- data.frame(ssb = f_ssb,
           age = 2:31) %>% 
  mutate(year_class = (YEAR+1) - age,
         perc = ssb / sum(f_ssb),
         label = ifelse(perc > 0.01, paste0(year_class), NA)) 

tmp %>% 
  ggplot(aes(x = year_class, y = ssb / 1e6, size = perc, label = label)) +
  geom_point() +
  geom_text(position = position_nudge(y = .2)) +
  scale_size(labels = scales::percent) +
  labs(x = "Year class", y = "SSB (kt)", size = paste0("Percent\ncontribution\nto the ", YEAR+1, "\nSSB"))

ggsave(paste0(tmbout, "/percentSSB_bycohort_",VER,"_", YEAR, ".png"),
       dpi=300, height=6, width=8, units="in")

breaks <- c(seq(min(tmp$year_class),max(tmp$year_class),1))
tmp %>% filter(!is.na(label)) %>% mutate(sum(perc)) %>%
  ggplot() + geom_col(aes(y=perc, x= year_class)) +
  labs(x = "year class", y = "Proportion of biomass") +
  scale_x_discrete(limits = c(breaks),
                   breaks = c(breaks)) + 
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

ggsave(paste0(tmbout, "/percentSSB_bycohort_2_",VER,"_", YEAR, ".png"),
       dpi=300, height=6, width=8, units="in")

tmp %>% filter(year_class >= 2013)%>% mutate(sum(perc))

# Figure of current estimates of ABC under contemporary estimates of
# F50 (and F40, F60) compare with Catch
retro_mgt <- ABC %>% 
  left_join(wastage %>% filter(year != YEAR+1)) %>% 
  left_join(ts %>% select(year, landed = catch) %>% 
              mutate(landed = landed * 2204.62)) %>% 
  pivot_longer(-c(year, Fxx, ABC)) %>% 
  rename(Fspr = Fxx)

df <- data.frame(year = 1990:YEAR+1)
# xaxis <- tickr(df, year, 5)
ggplot() +
  geom_area(data = retro_mgt %>% filter(year > 2000 & Fspr == "0.5"),
            aes(x = year, y = value, fill = name), 
            position = "stack") +
  scale_fill_grey() +
  geom_line(data = retro_mgt %>% filter(year > 2000 & Fspr %in% c("0.4", "0.5", "0.6")), #"0.5"),
            aes(x = year, y = ABC, linetype = Fspr)) +
  # scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  scale_y_continuous(label = scales::comma) +
  labs(x = NULL, y = "Catch (round lb)", fill = "Catch")

ggsave(filename = paste0(tmbfigs, "/catch_ABC_Fspr_",VER,"_", YEAR, ".png"), 
       dpi = 300, height = 4, width = 6, units = "in")

# Write BRPs ----
SB
res <- c(paste0("STATISTICAL CATCH-AT-AGE MODEL RESULTS FOR NSEI SABLEFISH", "\n",
                "\n",
                "Report produced by scaa.R", "\n",
                # Update as needed!
                "Developed by Jane Sullivan, ummjane@gmail.com",
                "Contact: philip.joy@alaska.gov or rhea.ehresmann@alaska.gov", "\n",
                "Report generated: ", paste0(Sys.Date()),  "\n",
                "\n",
                "Model diagnostics", "\n",
                "Number of parameters:,", "\n", length(best),  "\n",
                "Negative log likelihood:,", "\n",round(obj$report(best)$obj_fun),  "\n",
                "Maximum gradient component:,", "\n", max(rep$gradient.fixed), "\n", "\n", 
                "ALL VARIABLES REPORTED IN ROUND LB UNLESS OTHERWISE SPECIFIED", "\n", "\n"))

write.table(res, file = paste0(tmbout, "/scaa_brps_", YEAR,"_",VER, ".csv"), sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE, eol = "\n")

# Last years reference points
res <- c(paste0("Summary of ", YEAR, " (last year's) Biological Reference Points", "\n",
                
                "Projected age-2 biomass: ", "\n",
                LYR_proj_age2plus, "\n",
                
                "Projected female spawning biomass: ", "\n",
                LYR_proj_fSSB, "\n",
                
                "Unfished equilibrium female spawning biomass (SPR = 100): ", "\n",
                LYR_SB100, "\n",
                
                "Equilibrium female spawning biomass under F50 (SPR = 50): ", "\n",
                LYR_SB50, "\n",
                
                "Max ABC: ", "\n",
                LYR_maxABC, "\n",
                
                "Recommended ABC: ", "\n",
                LYR_recABC, "\n",
                
                "Mortality from fishery discards under max ABC: ", "\n",
                LYR_wastage, "\n",
                
                "max F_ABC = F_50: ", "\n",
                LYR_maxF_ABC, "\n",
                
                "F under recommended ABC: ", "\n",
                LYR_F_ABC,  "\n"))

write.table(res, file = paste0(tmbout, "/scaa_brps_", YEAR,"_",VER,  ".csv"), sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE, eol = "\n", append = TRUE)

# Current estimated reference points
res <- c(paste0("Summary of ", YEAR+1, " (current year's) Biological Reference Points", "\n",
                
                "Projected age-2 biomass: ", "\n",
                proj_age2plus, "\n",
                
                "Projected female spawning biomass: ", "\n",
                proj_fSSB, "\n",
                
                "Unfished equilibrium female spawning biomass (SPR = 100): ", "\n",
                SB100, "\n",
                
                "Equilibrium female spawning biomass under F50 (SPR = 50): ", "\n",
                SB50, "\n",
                
                "Max ABC: ", "\n",
                maxABC, "\n",
                
                "Recommended ABC: ", "\n",
                recABC, "\n",
                
                "Mortality from fishery discards under max ABC: ", "\n",
                wastage_maxABC, "\n",
                
                "max F_ABC = F_50: ", "\n",
                maxF_ABC, "\n",
                
                "F under recommended ABC: ", "\n",
                #F_ABC, "\n"))
                F_ABC_schwag, "\n"))

write.table(res, file = paste0(tmbout, "/scaa_brps_", YEAR,"_",VER,  ".csv"), sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE, eol = "\n", append = TRUE)

# Summary of raw and percent changes between last year and current years
# reference points (used for text summaries)
res <- c(paste0("Summary of percent changes and differences between ", YEAR, " and ", YEAR+1, " Biological Reference Points", "\n",
                
                "Difference between projected age-2+ biomass (round lb): ", "\n",
                round(proj_age2plus - LYR_proj_age2plus, 0), "\n",
                "Percent difference between projected age-2+ biomass: ", "\n",
                round((proj_age2plus - LYR_proj_age2plus)/ LYR_proj_age2plus * 100, 1), "%", "\n",
                
                "Difference between projected female spawning biomass (round lb): ", "\n",
                round(proj_fSSB - LYR_proj_fSSB, 0), "\n",
                "Percent difference between projected female spawning biomass: ", "\n",
                round((proj_fSSB - LYR_proj_fSSB)/ LYR_proj_fSSB * 100, 1), "%", "\n",
                
                "Difference between Unfished equilibrium female spawning biomass (round lb): ", "\n",
                round(SB100 - LYR_SB100, 0), "\n",
                "Percent difference between Unfished equilibrium female spawning biomass: ", "\n",
                round((SB100 - LYR_SB100)/ LYR_SB100 * 100, 1), "%", "\n",
                
                "Difference between equilibrium female spawning biomass under F50 (round lb): ", "\n",
                round(SB50 - LYR_SB50, 0), "\n",
                "Percent difference between equilibrium female spawning biomass under F50: ", "\n",
                round((SB50 - LYR_SB50)/ LYR_SB50 * 100, 1), "%", "\n",
                
                "Difference between max ABCs (round lb): ", "\n",
                round(maxABC-LYR_maxABC,0), "\n",
                "Percent difference between max ABCs: ", "\n",
                round((maxABC - LYR_maxABC) / LYR_maxABC) * 100, 1), "%", "\n",
                
                "Difference between recommended ABCs (round lb): ", "\n",
                round(recABC-LYR_recABC,0), "\n",
                "Percent difference between recommended ABCs: ", "\n",
                round((recABC - LYR_recABC)/LYR_recABC * 100, 1), "%", "\n",
                
                "Difference between mortalities from fishery discards under max ABC: ", "\n",
                round(wastage_maxABC - LYR_wastage, 0), "\n",
                "Percent difference between mortalities from fishery discards under max ABC: ", "\n",
                round((wastage_maxABC - LYR_wastage)/ LYR_wastage * 100, 1), "%", "\n",
                
                "Percent difference between maxF_ABCs: ", "\n",
                round((maxF_ABC - LYR_maxF_ABC) / LYR_maxF_ABC * 100, 1), "%", "\n",
                "Percent difference between recomended F_ABCs: ", "\n",
                round((F_ABC_schwag - LYR_F_ABC) / LYR_F_ABC * 100, 1), "%", "\n")

write.table(res, file = paste0(tmbout, "/scaa_brps_", YEAR,"_",VER,  ".csv"), sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE, eol = "\n", append = TRUE)

# You could continue to append any variables of interest to the SCAA report.

# Data request May 2020 ----

# Luke Rogers, post-doc working with the coastwide sablefish group (contact:
# Luke.Rogers@dfo-mpo.gc.ca). Requests numbers-at-length and fishing mortality
# annual estimates. I added the numbers-at-length output to the scaa_mod.cpp
# using the federal age-length keys.

# males first in array, females second
male_Nlen <- as.data.frame((obj$report(best)$Nlen/1e6)[,,1])
female_Nlen <- as.data.frame((obj$report(best)$Nlen/1e6)[,,2])

# Clean up dataframe
names(male_Nlen) <- unique(sort(len$length_bin))
names(female_Nlen) <- unique(sort(len$length_bin))
row.names(male_Nlen) <- syr:(lyr+1)
row.names(female_Nlen) <- syr:(lyr+1)
write.csv(male_Nlen, paste0(tmbout, "/Chatham_Nlength_millions_male.csv"))
write.csv(female_Nlen, paste0(tmbout, "/Chatham_Nlength_millions_female.csv"))

# Fishing mortality
data.frame(Year = syr:lyr,
           Fmort = obj$report(best)$Fmort) %>% 
  write_csv(paste0(tmbout, "/Chatham_annual_F.csv"))


# Write model output ----


# Bayesian model -----

# UNDER DEVELOPMENT

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

# Ageing error figure ----

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

# Age length key figure ----
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

# Old code ----

# The following code was used to get new starting values for rec devs and F
# devs. Use relationship (gam) between current initial values and catch to
# obtain starting values for 1907-1979

# new_inits <- data.frame(year = 1980:YEAR,
#                        rec_devs = rec_devs_inits,
#                        Fdevs = Fdevs_inits,
#                        catch = ts %>% 
#                          filter(year >= 1980) %>% 
#                          select(catch))
# 
# hist(new_inits$rec_devs)
# mean(new_inits$rec_devs)
# fit_recdevs <- gam(rec_devs ~ s(catch), data = new_inits %>% 
#                      # exclude highly influential low catches
#                      filter(catch > 500))
# summary(fit_recdevs)
# plot(fit_recdevs, se = TRUE, resid = TRUE, pch = 20)
# 
# hist(new_inits$Fdevs)
# fit_Fdevs <- gam(Fdevs ~ s(catch), data = new_inits)
# summary(fit_Fdevs)
# plot(fit_Fdevs, se = TRUE, resid = TRUE, pch = 20)
# 
# pred_inits <- ts %>% 
#   select(year, catch) %>% 
#   filter(year < 1980)
# 
# pred_inits <- pred_inits %>% 
#   mutate(rec_devs = predict(fit_recdevs, pred_inits),
#          Fdevs = predict(fit_Fdevs, pred_inits))
# 
# new_inits <- pred_inits %>% 
#   bind_rows(new_inits) %>% 
#   arrange(year)
#   
# rec_devs_inits <- new_inits$rec_devs
# Fdevs_inits <- new_inits$Fdevs
