# my user-defined functions for the sablefish assessment
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2018-02-26

# includes functions for: 
# -  estimating length- and weight- based von bertalanffy using maximum
# likelihood estimation
# - getting sex ratios by age or year (or both)
# - a modification to the captioner library's captioner() fxn
# - cleaning up coda output

# source("r/helper.R")

if(!require("broom"))   install.packages("broom") # tidy() useful for tidying mle() and other model output
if(!require("stats4"))   install.packages("stats4") # needed for mle()
if(!require("captioner"))   install.packages("captioner") #numbering, ordering, & creating captions for tables and figures

# length-based lvb ----

# length-based lvb with max likelihood 
vb_like <- function(obs_length, age, l_inf, k, t0, sigma) { 
  pred <- l_inf * (1 - exp(-k * (age - t0))) # predictions based on lvb growth curve
  like <- dnorm(obs_length, pred, sigma) # likelihood
  neg_like <- -1 * (sum(log(like))) 
  return(neg_like) 
}

# minimize negative log likelihood with mle function
vonb_len <- function(obs_length, age, starting_vals, sex) {
  
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
  neg_like <- -1 * (sum(log(like))) 
  return(neg_like) 
}

vonb_weight <- function(obs_weight, age, b, starting_vals, sex ) {
  
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
  
  # For YPR and future ASA, get predictions for ages 2:42
  new_ages <- c(2:42)
  log_pred <- log(w_inf_opt) + b * log(1 - exp(-k_opt * (new_ages - t0_opt))) #  predicted values
  ypr_preds <- exp(log_pred)
  
  results <- list(predictions = data.frame(obs_weight = obs_weight,
                                           age = age, pred = pred, 
                                           resid = resids, Sex = sex),
                  ypr_predictions = data.frame(age = new_ages, 
                                               weight = ypr_preds,
                                               Sex = sex),
                  results = tidy(coef(summary(wvb_mle))) %>% 
                    select(Parameter = `.rownames`, Estimate, SE = `Std..Error`) %>% 
                    mutate(Sex = sex), 
                  logl = logl)
  return(results)
}

# sex ratios ----

# Generalized function to get raw proportion by age or year

f_sex_ratio <- function(data, src, ...) {
  # data = biological survey or fishery data, each line is an individual
  # src = data source e.g. "longline survey", "longline fishery"
  # ... = the variable(s) you're trying to get the proportions of females by (e.g. age, year, or both)
  
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
    na.omit() %>% 
    droplevels() -> data
  
  # proportion by
  data %>% ungroup() %>%
    count(!!var, !!!proportion_by) %>%
    group_by(!!!proportion_by) %>%
    mutate(proportion = round(n / sum(n), 2) ,
           Source = src) %>%
    filter(Sex == proportion_of) -> props

    return(props)
}

# captioner ----

#fig_nums, tbl_nums, and appendix_nums fxns created fromm captioner() fxn in
#'captioner' library, which I've tweaked below, changed separator from a colon
#to period) - these fxns are used for autonumbering figs and tables in text and
#creating captions.

captioner <- function (prefix = "Figure", auto_space = TRUE, levels = 1, 
                       type = NULL, infix = ".") {
  check_class(prefix, "character")
  check_class(auto_space, "logical")
  check_class(levels, "numeric")
  check_class(infix, "character")
  if (is.null(type)) {
    type <- c(rep("n", times = levels))
  }
  else if (length(type) < levels) {
    type[(length(type) + 1):levels] <- "n"
  }
  else if (length(type) > levels) {
    type <- type[1:levels]
  }
  if (!all(type %in% c("n", "c", "C"))) {
    stop("Invalid 'type' value used.  Expecting 'n', 'c', or 'C'.")
  }
  if (auto_space) {
    prefix <- paste(prefix, " ")
  }
  force(levels)
  force(prefix)
  force(infix)
  OBJECTS <- list(name = NULL, caption = NULL, number = list(list()))
  OBJECTS$number[[1]][which(type == "n")] <- 1
  OBJECTS$number[[1]][which(type == "c")] <- "a"
  OBJECTS$number[[1]][which(type == "C")] <- "A"
  function(name, caption = "", display = "full", level = FALSE, 
           cite = FALSE, num = FALSE) {
    if (level > levels) {
      stop("Level too large.")
    }
    objects <- OBJECTS
    if (any(objects$name == name)) {
      obj_ind <- match(name, objects$name)
      if (objects$caption[obj_ind] == "") {
        objects$caption[obj_ind] <- caption
      }
      else {
        caption <- objects$caption[obj_ind]
      }
    }
    else {
      obj_ind <- length(objects$name) + 1
      if (length(objects$number) == length(objects$name)) {
        if (level) {
          objects$number[[obj_ind]] <- increment(objects$number[[obj_ind - 
                                                                   1]], level)
        }
        else {
          objects$number[[obj_ind]] <- increment(objects$number[[obj_ind - 
                                                                   1]], levels)
        }
      }
      objects$name[obj_ind] <- name
      objects$caption[obj_ind] <- caption
    }
    assign("OBJECTS", objects, envir = parent.env(environment()))
    obj_num <- paste(objects$number[[obj_ind]], collapse = infix)
    if (cite) {
      .Deprecated(new = "display", old = "cite")
      return(paste0(prefix, obj_num))
    }
    if (num) {
      .Deprecated(new = "display", old = "num")
      return(obj_num)
    }
    if (display == FALSE) {
      return(invisible())
    }
    #FLAG: Jane changed ": " to ". "
    else if (display == "full" || display == "f") {
      return(paste0(prefix, obj_num, ". ", caption))
    }
    else if (display == "cite" || display == "c") {
      return(paste0(prefix, obj_num))
    }
    else if (display == "num" || display == "n") {
      return(obj_num)
    }
    else {
      warning("Invalid display mode used.  Caption was still saved.")
      return(invisible())
    }
  }
}

fig <- captioner(prefix = "Figure")

tbl <- captioner(prefix = "Table")

appendix_tbl <- captioner(prefix = "Table") #Numbers tables in the appendix

appendix_fig <- captioner(prefix = "Figure") #Numbers figures in the appendix

# Coda output ----

# Makes dealing with Bayesian JAGS/BUGS coda output easier to work with.

coda_df <- function(coda.object, parameters = NULL) {
  
  if (!coda::is.mcmc(coda.object) && !coda::is.mcmc.list(coda.object)) 
    stop("Not an mcmc or mcmc.list object")
  
  n.chain   <- coda::nchain(coda.object)
  mat       <- as.matrix(coda.object, iter = TRUE, chain = TRUE)
  df        <- as.data.frame(mat)
  
  if(n.chain == 1)
    df <- data.frame(1, df)
  
  names(df) <- c("chain", "iter", coda::varnames(coda.object))
  
  if(is.null(parameters))
    out.df <- df
  
  if(!is.null(parameters))
    out.df <- subset(df, select = c("chain", "iter", parameters))
  
  out.df
}

# MR Jags ----

# Run same (or multiple) models over full time series, exporting posterior
# distributions, deviance information criterion, and convergance diagnostics all
# in one

mr_jags <- function(
  mod, # model formula (as a character string)
  mod_name, # what you want to name your model, keep them straight during model selection
  model_dat, # list of lists of model data 
  model_years, # vector of years over which you want to run the model
  mpar, # parameters of interest, sample poterior distribution of 'mpar' variables
  mchains = 4, # number of desired chains
  madapt = 1000, # burn in
  miter = 10000, # number of iternations to run each chain
  mthin = 10 # thinning rate
) {
  
  # # Create an empty list to store model output 
  # model_output <- vector('list', 1)
  
  for(i in 1:length(model_years)){
    
    dat <- model_dat[[i]]
    
    cat(mod, file = paste0(mod_name, "_", model_years[i], ".jag"))
    
    # initialize and run model
    mod_init <- jags.model(paste0(mod_name, "_", model_years[i], ".jag"),
                      data = dat,
                      n.chains = mchains,
                      # init = tst_inits,
                      n.adapt = madapt)
    
    # sample posterior
    res <- coda.samples(mod_init,
                        var = mpar,
                        n.iter = miter,
                        thin = mthin)
    
    # format output using user-defined fxn coda_df
    coda_df(res) %>% 
      mutate(year = model_years[i],
             model = mod_name) -> coda_res
    
    #Append results 
    if(i == 1){
      coda_res_out <- coda_res
      rm(coda_res)
    } else {
      coda_res_out <- rbind(coda_res_out, coda_res) }
    
    # Diagnostic trace plots - These MR models were tested individually by year.
    # They are well mixed and have no issues with convergence.
    
    # plot(res, col = 2)
    
    # Get DIC for model selection
    # https://www4.stat.ncsu.edu/~reich/st590/code/DICpois
    
    dic <- dic.samples(mod_init,
                       var = mpar,
                       n.iter = miter,
                       thin = mthin)
    
    dic <- data.frame(model = mod_name,
                      year = model_years[i],
                      deviance = sum(dic$deviance), # over all fit (smaller deviance better)
                      parameter_penalty = sum(dic$penalty), # number of parameters
                      DIC = sum(dic$deviance) + sum(dic$penalty)) # penalized deviance (aka DIC), smaller is better
    
    # Append results 
    if(i == 1){
      dic_out <- dic
      rm(dic)
    } else {
      dic_out <- rbind(dic_out, dic) }
    
    # Convergance diagnostic: gelman.diag gives you the scale reduction factors for
    # each parameter. A factor of 1 means that between variance and within chain
    # variance are equal, larger values mean that there is still a notable
    # difference between chains. General rule: everything below 1.1 or so
    # is ok.
    # https://theoreticalecology.wordpress.com/2011/12/09/mcmc-chain-analysis-and-convergence-diagnostics-with-coda-in-r/
    gelman.diag(res, multivariate = FALSE)[[1]] %>%
      data.frame() %>% 
      mutate(year = model_years[i],
             model = mod_name) %>% 
      distinct() -> convergence
    
    # Append results 
    if(i == 1){
      convergence_out <- convergence
      rm(convergence)
    } else {
      convergence_out <- rbind(convergence_out, convergence) }
    
  } 
  
  # Summarize the abundance estimate for quick model comparison
  coda_res_out %>% 
    group_by(year) %>% 
    mutate(N.avg = N.avg / 1000000) %>% 
    summarize(median = median(N.avg),
           q025 = quantile(N.avg, 0.025),
           q975 = quantile(N.avg, 0.975)) %>% 
    mutate(model = mod_name) -> n_summary_out
  
    model_output <- list("results" = coda_res_out,
                         "N_summary" = n_summary_out,
                         "dic" = dic_out,
                         "convergence_diagnostic" = convergence_out)
    
    return(model_output)
   
}

# Depends on dplyr
tickr <- function(
  data, # dataframe
  var, # column of interest
  to # break point definition 
){
  
  VAR <- enquo(var) # makes VAR a dynamic variable
  
  data %>% 
    distinct(!!VAR) %>%
    ungroup(!!VAR) %>% 
    mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
                           !!VAR, "")) %>%
    select(breaks = UQ(VAR), labels)
}

#' Build parameter bounds
#' Original code by Grant Adams, adapted for use with the sablefish model
#' 
#' @param param_list Parameter list object built from \code{\link{build_params}}
#'
#' @return List of upper and lower bounds
#' @export
#'
build_bounds <- function(param_list = NULL, data_list){
  
  upper_bnd <- param_list
  lower_bnd <- param_list
  
  # upper_bnd <- lapply(upper_bnd, as.numeric)
  # lower_bnd <- lapply(lower_bnd, as.numeric)
  
  # General bounds
  for(i in 1:length(param_list)){
    upper_bnd[[i]] <- replace(upper_bnd[[i]], values = rep(Inf, length(upper_bnd[[i]])))
    lower_bnd[[i]] <- replace(lower_bnd[[i]], values = rep(-Inf, length(lower_bnd[[i]])))
  }
  
  # Fishery selectivity
  lower_bnd$fsh_sel50 <- replace(lower_bnd$fsh_sel50, values = rep(0.1, length(lower_bnd$fsh_sel50)))
  lower_bnd$fsh_sel95 <- replace(lower_bnd$fsh_sel95, values = rep(0.1, length(lower_bnd$fsh_sel95)))
  upper_bnd$fsh_sel50 <- replace(upper_bnd$fsh_sel50, values = rep(10, length(upper_bnd$fsh_sel50)))
  upper_bnd$fsh_sel95 <- replace(upper_bnd$fsh_sel95, values = rep(10, length(upper_bnd$fsh_sel95)))  
  
  # Survey selectivity
  lower_bnd$sel_sel50 <- replace(lower_bnd$sel_sel50, values = rep(0.1, length(lower_bnd$sel_sel50)))
  lower_bnd$sel_sel95 <- replace(lower_bnd$sel_sel95, values = rep(0.1, length(lower_bnd$sel_sel95)))
  upper_bnd$sel_sel50 <- replace(upper_bnd$sel_sel50, values = rep(10, length(upper_bnd$sel_sel50)))
  upper_bnd$sel_sel95 <- replace(upper_bnd$sel_sel95, values = rep(10, length(upper_bnd$sel_sel95)))  
  
  # Fishery catchability
  lower_bnd$fsh_logq <- replace(lower_bnd$fsh_logq, values = rep(-15, length(lower_bnd$fsh_logq)))
  upper_bnd$fsh_logq <- replace(upper_bnd$fsh_logq, values = rep(5, length(upper_bnd$fsh_logq)))
  
  # Survey catchability
  lower_bnd$srv_logq <- replace(lower_bnd$srv_logq, values = rep(-15, length(lower_bnd$srv_logq)))
  upper_bnd$srv_logq <- replace(upper_bnd$srv_logq, values = rep(5, length(upper_bnd$srv_logq)))
  
  # Mark-recapture catchability
  lower_bnd$mr_logq <- replace(lower_bnd$mr_logq, values = rep(-0.1, length(lower_bnd$mr_logq)))
  upper_bnd$mr_logq <- replace(upper_bnd$mr_logq, values = rep(0.1, length(upper_bnd$mr_logq)))
  
  # Recruitment devs
  lower_bnd$log_rec_devs <- replace(lower_bnd$log_rec_devs, values = rep(-10, length(lower_bnd$log_rec_devs)))
  upper_bnd$log_rec_devs <- replace(upper_bnd$log_rec_devs, values = rep(10, length(upper_bnd$log_rec_devs)))
  
  # Initial numbers-at-age devs
  lower_bnd$log_rinit_devs <- replace(lower_bnd$log_rinit_devs, values = rep(-10, length(lower_bnd$log_rinit_devs)))
  upper_bnd$log_rinit_devs <- replace(upper_bnd$log_rinit_devs, values = rep(10, length(upper_bnd$log_rinit_devs)))
  
  # F devs
  lower_bnd$log_F_devs <- replace(lower_bnd$log_F_devs, values = rep(-15, length(lower_bnd$log_F_devs)))
  upper_bnd$log_F_devs <- replace(upper_bnd$log_F_devs, values = rep(15, length(upper_bnd$log_F_devs)))
  
  # SPR F rates
  lower_bnd$spr_Fxx <- replace(lower_bnd$spr_Fxx, values = rep(0.01, length(lower_bnd$spr_Fxx)))
  upper_bnd$spr_Fxx <- replace(upper_bnd$spr_Fxx, values = rep(1, length(upper_bnd$spr_Fxx)))
  
  # Put bounds together
  bounds <- list(upper= upper_bnd, lower = lower_bnd)
  
  # Make sure inits are within bounds
  if( sum(unlist(bounds$upper) < as.numeric(unlist(param_list))) > 0 | sum(as.numeric(unlist(param_list)) < unlist(bounds$lower)) > 0 ){
    lower_check <- param_list
    upper_check <- param_list
    param_check <- data.frame(matrix(NA, nrow = length(param_list), ncol = 3))
    colnames(param_check) <- c("Parameter", "Lower", "Upper")
    param_check$Parameter <- names(param_list)
    
    for(i in 1:length(param_list)){
      lower_check[[i]] <- param_list[[i]] < lower_bnd[[i]]
      upper_check[[i]] <- param_list[[i]] > upper_bnd[[i]]
      param_check$Lower[i] <- sum(lower_check[[i]])
      param_check$Upper[i] <- sum(upper_check[[i]])
    }
    
    print("Non-zero value indicates error in initial value")
    print(param_check)
    stop("Initial parameter values are not within bounds")
  }
  
  return(bounds)
}

build_phases <- function(param_list = NULL, data_list){
  
  phases <- param_list
  
  # General bounds
  for(i in 1:length(param_list)){
    phases[[i]] <- replace(phases[[i]], values = rep(1, length(phases[[i]])))
  }
  
  # DEBUG
  phases$dummy <- 0
  
  # 1: Scaling parameters: log_rbar, log_rinit, mr_logq
  
  # 2: Secondary scaling parameters
  phases$log_rec_devs <- replace(phases$log_rec_devs, values = rep(2, length(phases$log_rec_devs)))
  phases$log_rinit_devs <- replace(phases$log_rinit_devs, values = rep(2, length(phases$log_rinit_devs)))
  phases$log_Fbar <- replace(phases$log_Fbar, values = rep(2, length(phases$log_Fbar)))
  phases$log_sigma_r <- replace(phases$log_sigma_r, values = rep(2, length(phases$log_sigma_r)))
  
  # 3: Fishing mortality devs and catchability
  phases$log_F_devs <- replace(phases$log_F_devs, values = rep(3, length(phases$log_F_devs)))
  phases$fsh_logq <- replace(phases$fsh_logq, values = rep(2, length(phases$fsh_logq)))
  phases$srv_logq <- replace(phases$srv_logq, values = rep(2, length(phases$srv_logq)))
  
  # 4: Selectivity
  phases$fsh_sel50 <- replace(phases$fsh_sel50, values = rep(4, length(phases$fsh_sel50)))
  phases$fsh_sel95 <- replace(phases$fsh_sel95, values = rep(4, length(phases$fsh_sel95)))
  phases$srv_sel50 <- replace(phases$srv_sel50, values = rep(4, length(phases$srv_sel50)))
  phases$srv_sel95 <- replace(phases$srv_sel95, values = rep(4, length(phases$srv_sel95)))
  
  # 5: Reference points
  phases$spr_Fxx <- replace(phases$spr_Fxx, values = rep(5, length(phases$spr_Fxx)))
  
  return(phases)
}


phases <- build_phases(parameters, data)

TMBphase(data, parameters, random = random_vars, phases, model_name = "mod", debug = FALSE)

# Original code by Gavin Fay, adaped for use in the sablefish model
TMBphase <- function(data, parameters, random, phases, model_name,
                     optimizer = "nlminb", debug = FALSE) {
  
  # function to fill list component with a factor
  fill_vals <- function(x,vals){rep(as.factor(vals), length(x))}
  
  # compile the model
  TMB::compile(paste0(model_name,".cpp"))
  dyn.load(TMB::dynlib(model_name))
  DLL_use <- model_name  
  
  #loop over phases
  for (phase_cur in 1:max(unlist(phases))) {
    phase_cur <- 2
    
    # If debugging build the map to have all parameters as factored NAs
    if (debug == TRUE) {
      map_use <- parameters
      for(i in 1:length(map_use)){
        map_use[[i]] <- replace(map_use[[i]], values = rep(NA, length(map_use[[i]])))
      }
      map_use <- map_use[!names(map_use) %in% "dummy"]
      
      for(i in 1:length(map_use)){
        map_use[[i]] <- factor(map_use[[i]])
      }
      
    } else {
      
      # work out the map for this phase if phases for parameters is less than the
      # current phase then map will contain a factor filled with NAs
      map_use <- list()
      map_use$dummy <- fill_vals(parameters$dummy, NA)
      j <- 1 # change to 0 if you get rid of the dummy debugging feature
      for (i in 1:length(parameters)) {
        if (phases[[i]]>phase_cur) {
          j <- j+1
          map_use[[j]] <- fill_vals(parameters[[i]], NA)
          names(map_use)[j] <- names(parameters)[i]               
        }
      }
      map_use
    }
    # remove the random effects if they are not estimated
    # random <- random_vars
    random_use <- random[!random%in%names(map_use)]
    
    # Build upper and lower parameter bounds and remove any that are not
    # estimated (should be the inverse of the map_use)
    bounds <- build_bounds(param_list = parameters)
    bounds$upper <- bounds$upper[!names(bounds$upper) %in% names(map_use)]
    bounds$lower <- bounds$lower[!names(bounds$lower) %in% names(map_use)]
    
    # Remove inactive parameters from bounds and vectorize
    lower <- unlist(bounds$lower)
    upper <- unlist(bounds$upper)
    
    # Remove random effects from bounds
    if (data$random_rec == TRUE) {
      lower <- lower[!c(random %in% names(lower))]
      upper <- upper[!c(random %in% names(upper))]
    }
    
    # initialize the parameters at values in previous phase
    params_use <- parameters
    if (phase_cur>1) params_use <- obj$env$parList(opt$par)
    
    # Fit the model
    obj <- TMB::MakeADFun(data,params_use,random=random_use,DLL=DLL_use,map=map_use)  
    
    TMB::newtonOption(obj,smartsearch=FALSE)
    # obj$fn()
    # obj$gr()
    opt <- nlminb(obj$par,obj$fn,obj$gr,
                  control=list(eval.max=100000,iter.max=1000),
                  lower = lower, upper = upper)
    rep <- TMB::sdreport(obj)
    rep
    
    #close phase loop
  }
  
  return(rep)  
  
  #close function TMBphase
}
