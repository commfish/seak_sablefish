# my user-defined functions for the sablefish assessment
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2017-10-19

# includes functions for: 
# -  estimating length- and weight- based von bertalanffy using maximum
# likelihood estimation
# - getting sex ratios by age or year (or both)

source("r_code/helper.R")

if(!require("broom"))   install.packages("broom") # tidy() useful for tidying mle() output
if(!require("stats4"))   install.packages("stats4") # needed for mle()


# length-based lvb ----

# length-based lvb with max likelihood 
vb_like <- function(obs_length, age, l_inf, k, t0, sigma) { 
  pred <- l_inf * (1 - exp(-k * (age - t0))) # predictions based on lvb growth curve
  like <- dnorm(obs_length, pred, sigma) # likelihood
  neg_like <- -1 * (sum(log(like))) # negative log likelihood
  return(neg_like) # returning negative log likelihood
}

# minimize negative log likelihood with mle function
vonb_len <- function(obs_length, #vector of lengths
                   age, #vector of ages
                   starting_vals, #vector of starting values (l_inf, k, t0, and sigma)
                   sex # "Male" or "Female"
                   ) {
  vb_mle <- mle(vb_like, start = as.list(starting_vals), 
                fixed = list(obs_length = obs_length, age = age),
                method = "BFGS")
  print(summary(vb_mle))
  l_inf_opt <- coef(summary(vb_mle))[1] # parameter estimates
  k_opt <- coef(summary(vb_mle))[2]
  t0_opt <- coef(summary(vb_mle))[3]
  sigma_opt <- coef(summary(vb_mle))[4]
  logl <- attributes(summary(vb_mle))$m2logL/-2
  pred <- l_inf_opt * (1 - exp(-k_opt * (age - t0_opt))) # retaining predicted values
  resids <- obs_length - pred # retaining residuals
  results <- list(predictions = data.frame(obs_length = obs_length,
                                           age = age, pred = pred, 
                                           resid = resids, Sex = sex),
                  results = tidy(coef(summary(vb_mle))) %>% 
                    select(Parameter = `.rownames`, Estimate, SE = `Std..Error`) %>% 
                    mutate(Sex = sex), 
                  logl = logl)
  return(results)
}

# weight-based lvb ----

# log(w_i) = log(w_inf) + beta * log(1 - exp * (-k * (age_i - t0))) + error
# multiplicative error distrubtion accounts for increasing variability in weight-at-age with age

wvb_like <- function(obs_weight, age, w_inf, k, t0, b, sigma) { 
  log_pred <- log(w_inf) + b * log(1 - exp(-k * (age - t0))) # predictions based on von bertalanffy growth curve
  # pred <- exp(log_pred)
  like <- dnorm(log(obs_weight), log_pred, sigma) # likelihood
  neg_like <- -1 * (sum(log(like))) # negative log likelihood
  return(neg_like) # returning negative log likelihood
}

vonb_weight <- function(obs_weight, #vector of observed weights
                    age, # vector of observed ages
                    b, # this is fixed, not estimated
                    starting_vals, #w_inf, k, t0, sigma
                    sex # = "Male" or "Female"
                    ) {
  #minimizing negative log likelihood with mle function
  wvb_mle <- mle(wvb_like, start = as.list(starting_vals), 
                 fixed = list(obs_weight = obs_weight, age = age, b = b),
                 method = "BFGS")
  print(summary(wvb_mle))
  w_inf_opt <- coef(summary(wvb_mle))[1] #retaining optimized parameter values
  k_opt <- coef(summary(wvb_mle))[2]
  t0_opt <- coef(summary(wvb_mle))[3]
  sigma_opt <- coef(summary(wvb_mle))[4]
  logl <- attributes(summary(wvb_mle))$m2logL/-2
  log_pred <- log(w_inf_opt) + b * log(1 - exp(-k_opt * (age - t0_opt))) #  predicted values
  pred <- exp(log_pred)
  resids <- obs_weight - pred # retaining residuals
  results <- list(predictions = data.frame(obs_weight = obs_weight,
                                           age = age, pred = pred, 
                                           resid = resids, Sex = sex),
                  results = tidy(coef(summary(wvb_mle))) %>% 
                    select(Parameter = `.rownames`, Estimate, SE = `Std..Error`) %>% 
                    mutate(Sex = sex), 
                  logl = logl)
  return(results)
}

# Sex ratios ----

# Generalized function to get raw proportion by age or year

f_sex_ratio <- function(
  data, # biological survey or fishery data, each line is an individual
  src, # data source e.g. "longline survey", "longline fishery"
  ... # the variable(s) you're trying to get the proportions of females by (e.g. age, year, or both)
) {
  # move these to arg list if interested in extending fxn 
  # var # variable of interest (e.g. = Sex)
  var_levels = c("Female", "Male") # levels of interest in 'var'
  proportion_of = "Female" # e.g. proportion of females in survey/fishery
  
  #captures 'var' as a formula (so it can be dynamic)
  var <- quo(Sex) # replace with enquo(var) if you want to extend fxn
  # like enquo, but captures the '...' as a list of formulas (in case you want to group
  # by both year and age)
  proportion_by <- quos(...)
  
  # subset with known sex
  data %>% ungroup() %>% 
    # extract relevant cols. !! converts var (a single object) into formula; !!!
    # does the same for list of args in fxn (...)
    select(!!var, !!!proportion_by) %>%
    # UQE = unquote the expression for evaluation while ignoring the environment
    filter(UQE(var) %in% var_levels) %>%
    na.omit() %>% droplevels() -> data
  
  # proportion by
  data %>% ungroup() %>%
    count(!!var, !!!proportion_by) %>%
    group_by(!!!proportion_by) %>%
    mutate(proportion = round(n / sum(n), 2) ,
           Source = src) %>%
    filter(Sex == proportion_of) -> props
  
  return(props)
}
