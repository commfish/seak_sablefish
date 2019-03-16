
# ASA that includes catch, fishery and survey CPUE, mark-recapture abundance
# estimates, fishery and survey weight-at-age, survey data about maturity-at-age
# and proportions-at-age, and fishery and survey age compositions. No sex
# structure.

# Libraries and helper functions ----

source("r/helper.r")
source("r/functions.r")

library(TMB)

# Data -----

ts <- read_csv("data/tmb_inputs/abd_indices.csv")        # time series
age <- read_csv("data/tmb_inputs/agecomps.csv")          # age comps
bio <- read_csv("data/tmb_inputs/maturity_sexratio.csv") # proportion mature and proportion-at-age in the survey
waa <- read_csv("data/tmb_inputs/waa.csv")               # weight-at-age

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

# Model dimensions
syr <- min(ts$year)                   # model start year
lyr <- max(ts$year)                   # end year
nyr <- length(syr:lyr)                # number of years        
rec_age <- min(waa$age)               # recruitment age                  
plus_group <- max(waa$age)            # plus group age
nage <- length(rec_age:plus_group)    # number of ages
# number of years to project forward *FLAG* eventually add to cpp file,
# currently just for graphics
nproj <- 1                            

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
  
  # Switch recruitment estimation: 0 = penalized likelihood (fixed sigma_r), 1 =
  # random effects
  random_rec = 0,
  
  # Time varying parameters - each vector contains the terminal years of each time block
  
  blks_fsh_sel = c(37), #  fishery selectivity: limited entry in 1985, EQS in 1994 = c(5, 14, 37)
  blks_srv_sel = c(37), # no breaks survey selectivity
  
  # Fixed parameters
  M = 0.1,
  
  # Fxx levels that correspond with spr_Fxx in Parameter section
  Fxx_levels = c(0.35, 0.40, 0.50),
  
  # Priors ("p_" denotes prior)
  p_fsh_q = 0.001,
  sigma_fsh_q = 1,
  p_srv_q = 0.001,
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
  
  # Proportion mature-at-age
  prop_mature = bio$prop_mature,
  
  # Proportion female-at-age in the survey
  prop_fem = bio$prop_fem,
  
  # Weight-at-age
  data_fsh_waa = filter(waa, Source == "Fishery (sexes combined)") %>% pull(weight),
  data_srv_waa = filter(waa, Source == "Survey (sexes combined)") %>% pull(weight),
  data_fem_waa = filter(waa, Source == "Survey females (spawning biomass)") %>% pull(weight),
  
  # Fishery age comps
  nyr_fsh_age = fsh_age %>% distinct(year) %>% nrow(),
  yrs_fsh_age = fsh_age %>% distinct(index) %>% pull(),
  data_fsh_age = fsh_age %>% select(-c(year, index, Source, effn)) %>% as.matrix(),
  effn_fsh_age = pull(fsh_age, effn),
  
  # Survey age comps
  nyr_srv_age = srv_age %>% distinct(year) %>% nrow(),
  yrs_srv_age = srv_age %>% distinct(index) %>% pull(),
  data_srv_age = srv_age %>% select(-c(year, index, Source, effn)) %>% as.matrix(),
  effn_srv_age = pull(srv_age, effn),
  
  # Ageing error matrix
  ageing_error = as.matrix(ageing_error)
)

# Parameters ----

# Parameter starting values
parameters <- list(
  
  dummy = 0,   # Used for troubleshooting model               
  
  # Selectivity
  fsh_sel50 = rep(3.52, length(data$blks_fsh_sel)),
  fsh_sel95 = rep(5.43, length(data$blks_fsh_sel)),
  srv_sel50 = rep(3.86, length(data$blks_srv_sel)),
  srv_sel95 = rep(5.13, length(data$blks_srv_sel)), 
  
  # Catchability
  fsh_logq = -3.6726,
  srv_logq = -2.4019,
  mr_logq = -0.00000001,
  
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
  spr_Fxx = c(0.128, 0.105, 0.071)       # e.g. F35, F40, F50
  
)

# Run model ----

# Use map to turn off parameters, either for testing with dummy, phasing, or to
# fix parameter values

# Compile
compile("mod.cpp")
dyn.load(dynlib("mod"))

# Setup random effects
random_vars <- c()
if (data$random_rec == 1) {
  random_vars <- c("log_rec_devs", "log_rinit_devs")
}

# Fix parameter if sigma_r is not estimated via random effects
# if(data$random_rec == 0) {
#   random_vars <- rep(factor(NA),2)
# }

phases <- build_phases(parameters, data)

TMBphase(data, parameters, random = random_vars, phases, model_name = "mod", debug = FALSE)



# Estimate everything at once
map <- list(dummy=factor(NA))

library(TMBhelper)


model <- MakeADFun(data, parameters, DLL = "mod", 
                   silent = TRUE, map = map,
                   random = random_vars)

bounds <- build_bounds(parameters, data)

bounds$upper <- bounds$upper[!names(bounds$upper) %in% names(map_use)]

# Remove inactive parameters from bounds and vectorize
lower <- unlist(bounds$lower)
upper <- unlist(bounds$upper)

opt <- TMBhelper::Optimize(obj = model, 
                           fn = model$fn, 
                           gr = model$gr,
                           startpar = model$par,
                           # control=list(eval.max=100000,iter.max=1000),
                           lower = lower, 
                           upper = upper, 
                           loopnum = 5)

# Turn off parameters that don't scale the population
map <- list(dummy=factor(NA),
            fsh_sel50 = rep(factor(NA), length(data$blks_fsh_sel)),
            fsh_sel95 = rep(factor(NA), length(data$blks_fsh_sel)),
            srv_sel50 = rep(factor(NA), length(data$blks_srv_sel)),
            srv_sel95 = rep(factor(NA), length(data$blks_srv_sel)),
             log_Fbar = factor(NA), log_F_devs = rep(factor(NA), nyr),
            spr_Fxx = rep(factor(NA), length(data$Fxx_levels))
            )

model <- MakeADFun(data, parameters, DLL = "mod", 
                   silent = TRUE, map = map,
                   random = random_vars)

fit <- nlminb(model$par, model$fn, model$gr,
              control=list(eval.max=100000,iter.max=1000),
              lower = lower, upper = upper)

for (i in 1:3){
  fit <- nlminb(model$env$last.par.best, model$fn, model$gr)
}

best <- model$env$last.par.best
print(as.numeric(best))
rep <- sdreport(model)
print(best)
print(rep)
model$report()$priors
model$report()$catch_like
model$report()$index_like
model$report()$age_like
model$report()$pred_mr
model$report()$obj_fun

exp(as.list(rep, what = "Estimate")$fsh_logq)
exp(as.list(rep, what = "Estimate")$srv_logq)
exp(as.list(rep, what = "Estimate")$mr_logq)
# as.list(rep, what = "Std")

exp(as.list(rep, what = "Estimate")$log_rbar)

# Phase -----

# PHASE 1 - estimate mark-recapture catchability (mr_logq)
map <- list(dummy=factor(NA),
            fsh_sel50 = rep(factor(NA), length(data$blks_fsh_sel)), fsh_sel95 = rep(factor(NA), length(data$blks_fsh_sel)),
            srv_sel50 = rep(factor(NA), length(data$blks_srv_sel)), srv_sel95 = rep(factor(NA), length(data$blks_srv_sel)),
            fsh_logq = factor(NA), srv_logq = factor(NA), mr_logq = factor(NA),
            # log_rbar = factor(NA), log_rec_devs = rep(factor(NA), nyr),
            # log_rinit = factor(NA), log_rinit_devs = rep(factor(NA), nage-2),
            log_sigma_r = factor(NA),
            log_Fbar = factor(NA), log_F_devs = rep(factor(NA), nyr),
            spr_Fxx = rep(factor(NA), length(data$Fxx_levels)))

# Setup random effects
random_vars <- c()
if (data$random_rec == 1) {
  random_vars <- c("log_rec_devs")
}

model <- MakeADFun(data, parameters, DLL = "mod", silent = TRUE, map = map,
                   random = random_vars)
lower <- c(-Inf,                 # log mean recruitment
           rep(-10, nyr),        # log recruitment deviations
           -Inf,                 # log mean init numbers-at-age
           rep(-10, nage-2),     # log initial numbers-at-age deviations
           -Inf)                 # log sigma R)

upper <- c(Inf,                  # log mean recruitment
           rep(10, nyr),         # log recruitment deviations
           Inf,                  # log mean init numbers-at-age
           rep(10, nage-2),      # log initial numbers-at-age deviations
           Inf)                  # log sigma R)

# Remove random effects from bounds
if (random_rec == TRUE) {
  lower <- lower[-grep(random_vars, names(lower))]
  upper <- upper[-grep(random_vars, names(upper))]
}

fit <- nlminb(model$par, model$fn, model$gr,
              control=list(eval.max=100000,iter.max=1000),
              lower = lower, upper = upper)
model$report(model$env$last.par.best)

best <- model$env$last.par.best
print(as.numeric(best))
rep <- sdreport(model)
print(summary(rep))
exp(as.list(rep, what = "Estimate")$mr_logq)
model$report()$pred_mr_all
data$data_mr
# PHASE 2 - estimate fishery and survey catchabilities and average F (fsh_logq,
# srv1_logq, srv2_logq, logFbar) and mr_logq
map <- list(dummy=factor(NA),
            fsh_sel50 = factor(NA), fsh_sel95 = factor(NA),
            srv_sel50 = factor(NA), srv_sel95 = factor(NA),
            # fsh_logq = factor(NA), #srv1_logq = factor(NA),
            #srv2_logq = factor(NA), # mr_logq = factor(NA),
            log_rbar = factor(NA), log_rec_devs = rep(factor(NA), nyr+nage-2),
            #log_Fbar = factor(NA),
            log_F_devs = rep(factor(NA), nyr))
model <- MakeADFun(data, parameters, DLL = "mod", silent = TRUE, map = map)
lower <- c(rep(-15, 4),-Inf)
upper <- c(rep(5, 4), Inf)
phase2_inits <- c(parameters$fsh_logq, parameters$srv1_logq,
                  parameters$srv2_logq, as.numeric(best), parameters$log_Fbar)
fit <- nlminb(phase2_inits, model$fn, model$gr,
              control=list(eval.max=100000,iter.max=1000),
              lower = lower, upper = upper)
best <- model$env$last.par.best
print(as.numeric(best))

# PHASE 3 - estimate recruitment deviations (log_rec_devs) and fsh_logq,
# srv1_logq, srv2_logq, log_Fbar, and mr_logq
map <- list(dummy=factor(NA),
            fsh_sel50 = factor(NA), fsh_sel95 = factor(NA),
            srv_sel50 = factor(NA), srv_sel95 = factor(NA),
            #fsh_logq = factor(NA), #srv1_logq = factor(NA),
            #srv2_logq = factor(NA), # mr_logq = factor(NA),
            log_rbar = factor(NA), # log_rec_devs = rep(factor(NA), nyr+nage-2),
            #log_Fbar = factor(NA),
            log_F_devs = rep(factor(NA), nyr))
model <- MakeADFun(data, parameters, DLL = "mod", silent = TRUE, map = map)
lower <- c(rep(-15, 4), rep(-10, nyr+nage-2), -Inf)
upper <- c(rep(5, 4), rep(10, nyr+nage-2), Inf)
phase3_inits <- c(as.numeric(best)[1:4], parameters$log_rec_devs, as.numeric(best)[5])
fit <- nlminb(phase3_inits, model$fn, model$gr,
              control=list(eval.max=100000,iter.max=1000),
              lower = lower, upper = upper)
best <- model$env$last.par.best
print(as.numeric(best))

# PHASE 4 - estimate fishing mortality deviations and selectivities and
# log_rec_devs, fsh_logq, srv1_logq, srv2_logq, log_Fbar and mr_logq
map <- list(dummy=factor(NA),
            # fsh_sel50 = factor(NA), fsh_sel95 = factor(NA),
            # srv_sel50 = factor(NA), srv_sel95 = factor(NA),
            #fsh_logq = factor(NA), #srv1_logq = factor(NA),
            #srv2_logq = factor(NA), #mr_logq = factor(NA),
            log_rbar = factor(NA)#, log_rec_devs = rep(factor(NA), nyr+nage-2),
            #log_Fbar = factor(NA),
            #log_F_devs = rep(factor(NA), nyr)
            )
model <- MakeADFun(data, parameters, DLL = "mod", silent = TRUE, map = map)
lower <- c(rep(0.1, 4), rep(-15, 4), rep(-10, nyr+nage-2), -Inf, rep(-15, nyr))
upper <- c(rep(10, 4), rep(5, 4), rep(10, nyr+nage-2), Inf, rep(15, nyr))
phase4_inits <- c(parameters$fsh_sel50, parameters$fsh_sel95,
                  parameters$srv_sel50, parameters$srv_sel95,
                  as.numeric(best), parameters$log_F_devs)
fit <- nlminb(phase4_inits, model$fn, model$gr,
              control=list(eval.max=100000,iter.max=1000),
              lower = lower, upper = upper)
best <- model$env$last.par.best
print(as.numeric(best))

# PHASE 5 - Estimate all parameters
map <- list(dummy=factor(NA)#,
            # fsh_sel50 = factor(NA), fsh_sel95 = factor(NA),
            # srv_sel50 = factor(NA), srv_sel95 = factor(NA),
            #fsh_logq = factor(NA), #srv1_logq = factor(NA),
            #srv2_logq = factor(NA), #mr_logq = factor(NA),
            #log_rbar = factor(NA)#, log_rec_devs = rep(factor(NA), nyr+nage-2),
            #log_Fbar = factor(NA),
            #log_F_devs = rep(factor(NA), nyr)
            )
model <- MakeADFun(data, parameters, DLL = "mod", silent = TRUE, map = map)
lower <- c(rep(0.1, 4), rep(-15, 4), -Inf, rep(-10, nyr+nage-2), -Inf, rep(-15, nyr))
upper <- c(rep(10, 4), rep(5, 4), Inf, rep(10, nyr+nage-2), Inf, rep(15, nyr))
phase5_inits <- c(as.numeric(best)[1:8], parameters$log_rbar, as.numeric(best)[9:length(best)])
fit <- nlminb(phase5_inits, model$fn, model$gr,
              control=list(eval.max=100000,iter.max=1000),
              lower = lower, upper = upper)
for (i in 1:5){
  fit <- nlminb(model$env$last.par.best, model$fn, model$gr)}
best <- model$env$last.par.best
print(as.numeric(best))
rep <- sdreport(model)

print(best)
print(rep)
VarCo <- solve(model$he())
# Check for Hessian
print(sqrt(diag(VarCo)))

# Likelihood components
# model$report()$priors
model$report()$catch_like
model$report()$index_like
model$report()$age_like
model$report()$fpen
model$report()$rec_like
model$report()$obj_fun
model$report()$offset

# Plot time series ----

# Catch 
ts$pred_catch <- model$report()$pred_catch
axis <- tickr(ts, year, 5)
ggplot(ts, aes(x = year)) +
  geom_point(aes(y = catch)) +
  geom_line(aes(y = pred_catch), colour = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = NULL, y = "\n\nCatch\n(round mt)") -> p_catch

# Fishery cpue
fsh_cpue$pred_fsh_cpue <- model$report()$pred_fsh_cpue
ggplot(fsh_cpue, aes(x = year)) +
  geom_point(aes(y = fsh_cpue)) +
  geom_line(aes(y = pred_fsh_cpue), colour = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = NULL, y = "\n\nFishery CPUE\n(round kg/hook)") -> p_fsh

# Survey cpue

srv_cpue$pred_srv_cpue <- model$report()$pred_srv_cpue
ggplot(srv_cpue, aes(x = year)) +
  geom_point(aes(y = srv_cpue)) +
  geom_line(aes(y = pred_srv_cpue), colour = "grey") +
  scale_x_continuous(limits = c(min(ts$year), max(ts$year)),
                     breaks = axis$breaks, labels = axis$labels) +
  labs(x = NULL, y = "\n\nSurvey CPUE\n(number/hook)") -> p_srv

# Mark recapture 
mr %>% 
  mutate(pred_mr = model$report()$pred_mr) %>% 
  select(year, pred_mr) %>% 
  right_join(ts %>%
               select(year, mr) %>%
               mutate(pred_mr_all = model$report()$pred_mr_all)) -> mr_plot

ggplot(mr_plot, aes(x = year)) +
  geom_point(aes(y = mr)) +
  geom_line(aes(y = pred_mr, group = 1), colour = "grey") +
  geom_line(aes(y = pred_mr_all, group = 1), lty = 2, colour = "grey") +
  scale_x_continuous( breaks = axis$breaks, labels = axis$labels) +
  labs(x = NULL, y = "\n\nAbundance\n(millions)") -> p_mr

plot_grid(p_catch, p_fsh, p_srv, p_mr, ncol = 1, align = 'hv', 
          labels = c('(A)', '(B)', '(C)', '(D)'))

# ggsave(paste0("pred_abd_indices.png"), 
#        dpi=300, height=7, width=6, units="in")

# Resids for time series ----
ts %>% 
  mutate(catch_resid = catch - pred_catch,
         catch_sresid = catch_resid / sd(catch_resid)) -> ts

ggplot(ts, aes(x = year, y = catch_sresid)) + 
  geom_hline(yintercept = 0, colour = "grey", size = 1) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = catch_resid), 
               size = 0.2, colour = "grey") +
  geom_point() +
  labs(x = "", y = "\n\nCatch\nresiduals") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) -> r_catch

# Fishery cpue resids
fsh_cpue %>% 
  mutate(fsh_cpue_resid = fsh_cpue - pred_fsh_cpue,
         fsh_cpue_sresid = fsh_cpue_resid / sd(fsh_cpue_resid)) -> fsh_cpue

ggplot(fsh_cpue, aes(x = year, y = fsh_cpue_sresid)) + 
  geom_hline(yintercept = 0, colour = "grey", size = 1) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = fsh_cpue_resid), 
               size = 0.2, colour = "grey") +
  geom_point() +
  labs(x = "", y = "\n\nFishery CPUE\nresiduals") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) -> r_fsh

# Survey cpues resids
srv %>% 
  mutate(srv_cpue_resid = obs - pred,
         srv_cpue_sresid = srv_cpue_resid / sd(srv_cpue_resid)) -> srv

ggplot(srv, aes(x = year, y = srv_cpue_sresid, shape = survey)) + 
  geom_hline(yintercept = 0, colour = "grey", size = 1) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = srv_cpue_resid), 
               size = 0.2, colour = "grey") +
  geom_point() +
  labs(x = "", y = "\n\nSurvey CPUE\nresiduals", shape = NULL) +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  theme(legend.position = "none") +
  geom_vline(xintercept = 1997, linetype = 2, colour = "grey") -> r_srv

# Mark-recapture abundance estimate resids
mr_plot %>% 
  mutate(mr_resid = mr - pred_mr,
         mr_sresid = mr_resid / sd(mr_resid)) -> mr_plot

ggplot(mr_plot, aes(x = year, y = mr_sresid)) + 
  geom_hline(yintercept = 0, colour = "grey", size = 1) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = mr_resid), 
               size = 0.2, colour = "grey") +
  geom_point() +
  labs(x = "", y = "\n\nMR abundance\nresiduals\n") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) -> r_mr

plot_grid(r_catch, r_fsh, r_srv, r_mr, ncol = 1, align = 'hv', 
          labels = c('(A)', '(B)', '(C)', '(D)'))

# ggsave(paste0("presid_abd_indices.png"), 
#        dpi=300, height=7, width=6, units="in")

# Plot derived variables ----
ts %>% 
  # Add another year to hold projected values
  full_join(data.frame(year = max(ts$year) + nproj)) %>%
  # For ts by numbers go divide by 1e6 to get values in millions, for biomass
  # divide by 1e3 to go from kg to mt
  mutate(Fmort = c(model$report()$Fmort, rep(NA, nproj)),
         pred_rec = c(model$report()$pred_rec, rep(NA, nproj)) / 1e6,
         biom = model$report()$biom / 1e3,
         expl_biom = model$report()$expl_biom / 1e3,
         vuln_abd = model$report()$vuln_abd / 1e6,
         spawn_biom = model$report()$spawn_biom / 1e3,
         exploit = catch / expl_biom / 1e3) -> ts

p <- ggplot(ts, aes(x = year)) +
  scale_x_continuous( breaks = axis$breaks, labels = axis$labels)+
  scale_y_continuous(label = scales::comma)

# Recruitment
p + geom_point(aes(y = pred_rec)) +
  geom_line(aes(y = pred_rec, group = 1)) +
  labs(x = "", y = "\n\nAge-2 recruits\n(millions)") -> p_rec

# Total biomass
p + geom_point(aes(y = biom)) +
  geom_line(aes(y = biom, group = 1)) +
  labs(x = "", y = "\n\nTotal\nbiomass (mt)") -> p_biom

# Exploitable biomass (to fishery)
p + geom_point(aes(y = expl_biom)) +
  geom_line(aes(y = expl_biom, group = 1)) +
  labs(x = "", y = "\n\nExploitatble\nbiomass (mt)") -> p_ebiom

# Vulnerable abundance (to survey)
p + geom_point(aes(y = vuln_abd)) +
  geom_line(aes(y = vuln_abd, group = 1)) +
  labs(x = "", y = "\n\nVulnerable\nabundance (millions)") -> p_vabd

# Spawning biomass 
p + geom_point(aes(y = spawn_biom)) +
  geom_line(aes(y = spawn_biom, group = 1)) +
  labs(x = "", y = "\n\nSpawning\nbiomass(mt)") -> p_sbiom

plot_grid(p_rec, p_biom, p_ebiom, p_vabd, p_sbiom, ncol = 1, align = 'hv',
          labels = c('(A)', '(B)', '(C)', '(D)', '(E)'))

ggsave(paste0("derived_ts.png"), 
       dpi=300, height=7, width=6, units="in")

# Plot F ----
p + geom_point(aes(y = Fmort)) +
  geom_line(aes(y = Fmort, group = 1)) +
  labs(x = "", y = "Fishing mortality") 

ggsave(paste0("fishing_mort.png"), 
       dpi=300, height=4, width=6, units="in")

# Plot age comps ----

pred_fsh_age <- as.data.frame(model$report()$pred_fsh_age)
names(pred_fsh_age) <- as.character(rec_age:plus_group)
pred_fsh_age %>% 
  mutate(Source = "Fishery",
         index = data$yrs_fsh_age) -> pred_fsh_age

pred_srv_age <- as.data.frame(model$report()$pred_srv_age)
names(pred_srv_age) <- as.character(rec_age:plus_group)
pred_srv_age %>% 
  mutate(Source = "Survey",
         index = data$yrs_srv_age) -> pred_srv_age

# Reshape age comp observations and predictions into long format, calculate
# residuals and prep results for plotting
age %>% 
  gather("age", "obs", 2:plus_group+2) %>%
  left_join(
    bind_rows(pred_fsh_age, pred_srv_age) %>% 
      gather("age", "pred", 1:41),
    by = c("Source", "index", "age")) %>% 
  group_by(Source) %>% 
  mutate(resid = obs - pred,
         # Get standardized residual (mean of 0, sd of 1)
         std_resid = resid / sd(resid),
         # Pearson's residual
         pearson = resid / sqrt(var(pred)),
         # positive or negative
         `Model performance` = ifelse(std_resid >= 0, "Observed greater than estimated",
                                      ifelse(is.na(obs), "",
                                             "Observed less than estimated")),
         Age = factor(age, levels = c("2", "3", "4", "5", "6", "7", "8",
                                      "9", "10", "11", "12", "13", "14", "15",
                                      "16", "17", "18", "19", "20", "21", "22",
                                      "23", "24", "25", "26", "27", "28", "29", "30",
                                      "31", "32", "33", "34", "35", "36", "37", "38",
                                      "39", "40", "41", "42"),
                      labels = c("2", "3", "4", "5", "6", "7", "8",
                                 "9", "10", "11", "12", "13", "14", "15",
                                 "16", "17", "18", "19", "20", "21", "22",
                                 "23", "24", "25", "26", "27", "28", "29", "30",
                                 "31", "32", "33", "34", "35", "36", "37", "38",
                                 "39", "40", "41", "42+")))  -> agecomps

# Custom axes
axis <- tickr(agecomps, year, 5)
age_labs <- c("2", "", "", "", "6", "", "", "", "10", "", "", "", "14", "",
              "", "", "18", "", "", "", "22", "", "", "", "26", "",
              "", "", "30", "", "", "", "34", "", "", "", "38", "",
              "", "", "42+") 

ggplot(agecomps, aes(x = Age, y = year, size = std_resid,
                     fill = `Model performance`)) + 
  # geom_hline(yintercept = seq(2000, 2015, by = 5), colour = "grey", linetype = 3, alpha = 0.7) +  
  geom_point(shape = 21, colour = "black") +
  scale_size(range = c(0, 4.5)) +
  facet_wrap(~ Source) +
  labs(x = '\nAge', y = '') +
  guides(size = FALSE) +
  scale_fill_manual(values = c("black", "white")) +
  scale_x_discrete(breaks = unique(agecomps$Age), labels = age_labs) +
  scale_y_continuous(breaks = axis$breaks, labels = axis$labels) +
  theme(legend.position = "bottom")

ggsave("agecomps_residplot.png", dpi = 300, height = 7, width = 8, units = "in")

# Barplot age comps ----

# Fishery
ggplot(agecomps %>% filter(Source == "Fishery")) +
  geom_bar(aes(x = Age, y = obs), 
           stat = "identity", colour = "grey", fill = "lightgrey",
           width = 0.8, position = position_dodge(width = 0.5)) +
  geom_line(aes(x = Age, y = pred, group = 1), size = 0.6) +
  facet_wrap(~ year, dir = "v", ncol = 5) +
  scale_x_discrete(breaks = unique(agecomps$Age), labels = age_labs) +
  labs(x = '\nAge', y = 'Proportion-at-age\n') 

ggsave("fsh_agecomps_barplot.png", dpi = 300, height = 6, width = 7, units = "in")

# Survey
agecomps %>% filter(Source == "Survey") %>% 
  ggplot() +
  geom_bar(aes(x = Age, y = obs), 
           stat = "identity", colour = "grey", fill = "lightgrey",
           width = 0.8, position = position_dodge(width = 0.5)) +
  geom_line(aes(x = Age, y = pred, group = 1), size = 0.6) +
  facet_wrap(~ year, dir = "v", ncol = 3) +
  scale_x_discrete(breaks = unique(agecomps$Age), labels = age_labs) +
  labs(x = '\nAge', y = 'Proportion-at-age\n')

ggsave("srv_agecomps_barplot.png", dpi = 300, height = 8, width = 7, units = "in")

# Plot selectivity ----

# Extract selectivity matrices and convert to dfs and create a second index col
# as a dummy var (must supply an interval to foverlaps). Set as data.table
# object so it is searchable
sel <- model$report()$fsh_sel %>% as.data.frame() %>% 
  mutate(Selectivity = "Fishery") %>% 
  bind_rows(model$report()$srv_sel %>% as.data.frame() %>% 
              mutate(Selectivity = "Survey"))

names(sel) <- c(unique(agecomps$age), "Selectivity")

sel <- sel %>% 
  mutate(year = rep(ts$year[1:nyr], 2)) %>% 
  gather("Age", "proportion", -c(year, Selectivity)) %>% 
  mutate(year2 = year) # needed for foverlaps()

setDT(sel)

# Look up table for selectivity time blocks
blks_sel <- data.frame(Selectivity = c(rep("Fishery", length(data$blks_fsh_sel)),
                                       rep("Survey", length(data$blks_srv_sel))),
                       end = c(data$blks_fsh_sel, data$blks_srv_sel)) %>%
  left_join(ts %>%
              mutate(end = index) %>% 
              select(year, end), by = "end") %>% 
  rename(end_year = year) %>% 
  # Define start of the interval based on the end of the interval
  group_by(Selectivity) %>% 
  mutate(start_year = c(min(ts$year), head(end_year, -1) + 1)) 
  

setkey(setDT(blks_sel), Selectivity, start_year, end_year)

# Match each year to the start and end year in blks_sel
foverlaps(x = sel, y = blks_sel,
          by.x = c("Selectivity", "year", "year2"),
          type = "within") -> sel

sel <- sel %>% 
  mutate(`Time blocks` = paste0(start_year, "-", end_year),
         age = as.numeric(Age)) %>% 
  filter(age <= 15)

ggplot(sel, aes(x = age, y = proportion, colour = `Time blocks`, 
                shape = `Time blocks`, lty = `Time blocks`, group = `Time blocks`)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Selectivity) +
  scale_colour_grey() +
  labs(y = "Selectivity\n", x = NULL, 
       colour = NULL, lty = NULL, shape = NULL) +
  theme(legend.position = c(.85, .15)) 

ggsave("selectivity.png", dpi = 300, height = 4, width = 6, units = "in")
