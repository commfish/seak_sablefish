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

source("r_code/helper.R")

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
    summarize(N.avg = mean(N.avg) / 1000000,
           q025 = quantile(N.avg, 0.025),
           q975 = quantile(N.avg, 0.975),
           median = median(N.avg)) %>% 
    mutate(model = mod_name) -> n_summary_out
  
    model_output <- list("results" = coda_res_out,
                         "N_summary" = n_summary_out,
                         "dic" = dic_out,
                         "convergence_diagnostic" = convergence_out)
    
    return(model_output)
   
}
