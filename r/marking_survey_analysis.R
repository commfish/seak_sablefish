# Sensitivity analysis on NSEI marking survey
# jane.sullivan1@alaska.gov
# April 8, 2020

# One of the benefits of transitioning to an integrated statistical catch-at-age
# model is reduced reliance on the marking survey, which forms the foundation of
# the current YPR model. The marking survey did not occur in 2011, 2014, and
# 2016, and is subject to future budget cuts.

# This analysis aims to answer the following questions:
# 1:  What would the ABC have been in past years without a
# survey (2011, 2014, and 2016)? What would the 2020 ABC have been had we not had a marking survey in 2019?
# 2:  What is the impact be on stock status and ABC recommendations if we had
# a biennial or triennial marking survey?

# Set up ----

YEAR <- 2019 # most recent year of data

# Directory setup
root <- getwd() # project root
tmb_dat <- file.path(root, "data/tmb_inputs") # location of tmb model data inputs
tmb_path <- file.path(root, "tmb") # location of cpp
tmbout <- file.path(root, "output/tmb") # location where model output is saved
mr_sens_dir <- file.path(tmbout, "sensitivity_marking_survey") # subdirectory for analysis
dir.create(mr_sens_dir, showWarnings = FALSE)
obj1_dir <- file.path(mr_sens_dir, "obj1_missing_surveys") # subdirectory for obj 1 analysis
dir.create(obj1_dir, showWarnings = FALSE)
obj2_dir <- file.path(mr_sens_dir, "obj2_periodical_surveys") # subdirectory for analysis
dir.create(obj2_dir, showWarnings = FALSE)

source("r/helper.r")
source("r/functions.r")

library(TMB) 
library(tmbstan)
library(shinystan)

# Assessment summary for graphics
assessment_summary <- read_csv(paste0("output/assessment_summary_", YEAR, ".csv"))

# Data for SCAA
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

# Starting values - base on current assessment's mle
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
  iter_rec_devs_inits <- rep(0, nyr)
  iter_Fdevs_inits <- rep(0, nyr)
  
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

rep_out # check for any clear signed of non-covergence

ABC_out <- do.call("rbind", ABC_out)
write_csv(ABC_out, paste0(obj1_dir, "/ABC_summary.csv"))
ABC_out <- read_csv(paste0(obj1_dir, "/ABC_summary.csv"))

ABC_out %>% 
  left_join(assessment_summary %>% 
              select(year, past_ABC = abc_round_lbs) %>% 
              bind_rows(data.frame(year = YEAR + 1,
                                   # max ABC (from this year, results from run_mod.R)
                                   past_ABC = 1382902))) %>% 
  mutate(diff = (ABC - past_ABC),
         perc_diff = diff / past_ABC * 100)

assessment_summary %>% 
  select(year, abc = abc_round_lbs) %>% 
  bind_rows(data.frame(year = YEAR + 1,
                       # max ABC (from this year, results from run_mod.R)
                       abc = 1382902)) %>% 
  # Fill in missing years - pulled these from AHO subdirectories from the M
  # drive
  bind_rows(data.frame(year = c(2011, 2014, 2016), 
                       abc = c(1046873, 952538, 807559))) %>% 
  arrange(year) -> df

axis <- tickr(df, year, 2)
p1 <- ggplot(df, aes(x = year, y = abc / 1e6)) + 
  geom_point(colour = "grey") +
  geom_line(colour = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  scale_y_continuous(limits = c(0, 2.5)) +
  geom_point(data = ABC_out %>% 
               mutate(model = "ABC when MR estimate is missing in previous year"),
             aes(x = year, y = ABC / 1e6, colour = model, shape = model),
             size = 3) +
  # scale_colour_manual(values = "black") +
  scale_shape_manual(values = 17) +
  labs(x = NULL, y = "ABC (million round lb)\n",
       colour = NULL, shape = NULL) +
  theme(legend.position = c(0.5, 0.9),
        legend.text.align = 0.5)
  # theme(legend.position = "top")
  
ggsave(plot = p1, filename = paste0(obj1_dir, "/ABC_", min(df$year), "_", YEAR+1, ".png"), 
       dpi=300,  height=4, width=7,  units="in")


# Question 2 ----

# Impact of biennial or triennial surveys

# Approach: 
# 1. Run model normally for 2019, use predicted values from MR index to fill in
# the full time series.
# 2. Create survey scenarios (bi or triennial)

# Run model normally for 2019
data <- build_data(ts = ts)
parameters <- build_parameters(rec_devs_inits = rec_devs_inits, Fdevs_inits = Fdevs_inits)
random_vars <- build_random_vars()
out <- TMBphase(data, parameters, random = random_vars, 
                model_name = "mod", phase = FALSE, 
                debug = FALSE)
obj <- out$obj # TMB model object
best <- obj$env$last.par.best

# Get predicted values for MR for full ts
ts <- ts %>% mutate(mr2 = obj$report(best)$pred_mr_all)

# Bi- and triennial survey options
survey_scenarios <- list(
  # Bienniel options
  bi_srv_1 = seq(min(mr$index), max(mr$index), 2),
  bi_srv_2 = seq(min(mr$index)+1, max(mr$index), 2),
  # Triennial options
  tri_srv_1 = seq(min(mr$index), max(mr$index), 3),
  tri_srv_2 = seq(min(mr$index)+1, max(mr$index), 3)
)

# Years to retrospectively examine
MR_years <- 2015:YEAR

ABC_out2 <- matrix(nrow = length(survey_scenarios), 
                   ncol = length(MR_years))
rep_out2 <- matrix(nrow = length(survey_scenarios), 
                   ncol = length(MR_years))

for(i in 1:length(survey_scenarios)) { 
  
  SRV <- names(survey_scenarios)[i]
  dir_SRV <- file.path(obj2_dir, SRV)
  dir.create(dir_SRV, showWarnings = FALSE)
  iter_SRV <- survey_scenarios[[i]] # index years for the MR in this survey scenario
  
  for(j in 1:length(MR_years)) {
    
    iter_YEAR <- MR_years[j]
    iter_dir <- file.path(dir_SRV, iter_YEAR) # subdirectory for analysis
    dir.create(iter_dir, showWarnings = FALSE)
  
    lyr <- iter_YEAR           # end year
    nyr <- length(syr:lyr)     # number of years 
    
    # Subsets for make_data()
    iter_ts <- filter(ts, year <= iter_YEAR)
    mr <- iter_ts %>% 
      filter(index %in% iter_SRV) %>% 
      select(year, index, mr, mr2, sigma_mr, upper_mr, lower_mr) %>% 
      mutate(mr = ifelse(is.na(mr), mr2, mr),
             sigma_mr = ifelse(is.na(sigma_mr), 0.05, sigma_mr))
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
      pull(ABC)
    
    # units = "metric" to switch between units. 
    plot_ts(save = TRUE, units = "imperial", plot_variance = FALSE, path = iter_dir, ts = iter_ts)
    plot_derived_ts(save = TRUE, path = iter_dir, units = "imperial", plot_variance = FALSE, ts = iter_ts)
    
    ABC_out2[i,j] <- ABC
    rep_out2[i,j] <- max(rep$gradient.fixed)
  }
}

rep_out2 <- as.data.frame(rep_out2)
names(rep_out2) <- MR_years
rep_out2 <- rep_out2 %>% 
  mutate(survey_scenarios = names(survey_scenarios)) %>% 
  gather("year", "mgc", -survey_scenarios)
rep_out2 <- rep_out2 %>% 
  mutate(converged = ifelse(mgc > 0.1, 0, 1))

ABC_out2 <- as.data.frame(ABC_out2)
names(ABC_out2) <- MR_years
ABC_out2 <- ABC_out2 %>% 
  mutate(survey_scenarios = names(survey_scenarios)) %>% 
  gather("year", "ABC", -survey_scenarios)

out <- left_join(rep_out2, ABC_out2)
out <- tbl_df(out)
ts %>% select(year, index) %>% right_join(data.frame(index = survey_scenarios$tri_srv_1))
out <- out %>% 
  mutate(`Survey scenarios` = derivedFactor(
    `Biennial ('05, '07, '09, '11, '13, '15, '17, '19)` = survey_scenarios == "bi_srv_1",
    `Biennial ('06, '08, '10, '12, '14, '16, '18)` = survey_scenarios == "bi_srv_2",
    `Triennial ('05, '08, '11, '14, '17)` = survey_scenarios == "tri_srv_1",
    `Triennial ('06, '09, '12, '15, '18)` = survey_scenarios == "tri_srv_2"))

write_csv(out, paste0(obj2_dir, "/ABC_summary.csv"))
out <- read_csv(paste0(obj2_dir, "/ABC_summary.csv"))

out %>%
  left_join(assessment_summary %>% 
                 select(year, past_ABC = abc_round_lbs) %>% 
                 bind_rows(data.frame(year = c(2016, YEAR + 1),
                                      # max ABC (from this year, results from run_mod.R)
                                      past_ABC = c(807559, 1382902)))) %>% 
  mutate(diff = (ABC - past_ABC),
         perc_diff = diff / past_ABC * 100)

p2 <- ggplot(df, aes(x = year, y = abc / 1e6)) + 
  geom_point(colour = "grey") +
  geom_line(colour = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  scale_y_continuous(limits = c(0, 2.5)) +
  geom_point(data = out %>% 
               filter(converged == 1) %>% 
               mutate(year = as.numeric(year)+1),
             aes(x = year, y = ABC / 1e6, colour = `Survey scenarios`, 
                 shape = `Survey scenarios`),
             size = 3) +
  # scale_shape_manual(guide = FALSE) +
  # scale_color_grey() +
  labs(x = NULL, y = "ABC (million round lb)\n",
       colour = NULL, shape = NULL) +
  theme(legend.position = "none") +
  theme(legend.position = c(0.5, 0.85))

ggsave(plot = p2, filename = paste0(obj2_dir, "/ABC_", min(df$year), "_", YEAR+1, ".png"), 
       dpi=300,  height=5.5, width=7,  units="in")

plot_grid(p1, p2, ncol = 1, labels = c("(A)", "(B)"))
ggsave(filename = paste0(mr_sens_dir, "/ABC_", min(df$year), "_", YEAR+1, ".png"), 
       dpi=300,  height=7, width=6,  units="in")

