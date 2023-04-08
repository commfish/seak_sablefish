# my user-defined functions for the sablefish assessment
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov (ummjane@gmail.com)
# Last edited: 2020-06-04

# Includes functions for: 
# -  estimating length- and weight- based von bertalanffy using maximum
# likelihood estimation
# - getting sex ratios by age or year (or both)
# - a modification to the captioner library's captioner() fxn
# - cleaning up coda Bayesian posterior output
# - Functions for building TMB data/parameter lists, running models
# - Function to save TMB model output, reshape results, and create figures

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
  
  vb_mle <- optim(vb_like, 
                start = as.list(starting_vals), 
                fixed = list(obs_length = obs_length, age = age),
                # method = "BFGS") # stopped working when I went from R v 3.6.3 to R v 4.0.2
                method = "L-BFGS-B",
                lower = as.list(c(l_inf = 0.00001, k = 0.000000001, t0 = -5000000, sigma = 10000000)),
                upper = as.list(c(l_inf = 500000, k = 1000000.0, t0 = 5000000, sigma = 0.00000001)))
  
  
  
  print(summary(vb_mle))
  l_inf_opt <- coef(summary(vb_mle))[1] # parameter estimates
  k_opt <- coef(summary(vb_mle))[2]
  t0_opt <- coef(summary(vb_mle))[3]
  sigma_opt <- coef(summary(vb_mle))[4]
  logl <- attributes(summary(vb_mle))$m2logL/-2
  pred <- l_inf_opt * (1 - exp(-k_opt * (age - t0_opt))) # retaining predicted values
  resids <- obs_length - pred # retaining residuals
  
   # For YPR and future ASA, get predictions for age range in the model
  new_ages <- c(rec_age:plus_group)
  ypr_preds <- l_inf_opt * (1 - exp(-k_opt * (new_ages - t0_opt))) 
  
  results <- list(predictions = data.frame(obs_length = obs_length,
                                           age = age, pred = pred, 
                                           resid = resids, Sex = sex),
                  ypr_predictions = data.frame(age = new_ages, 
                                               length = ypr_preds,
                                               Sex = sex),
                  results = as.tibble(row.names(coef(summary(vb_mle)))) %>% 
                                        bind_cols(as_tibble(coef(summary(vb_mle)))) %>% 
                    select(Parameter = value, Estimate, SE = `Std. Error`) %>% 
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
  
  # For YPR and future ASA, get predictions for age range in the model
  new_ages <- c(rec_age:plus_group)
  log_pred <- log(w_inf_opt) + b * log(1 - exp(-k_opt * (new_ages - t0_opt))) #  predicted values
  ypr_preds <- exp(log_pred)
  
  results <- list(predictions = data.frame(obs_weight = obs_weight,
                                           age = age, pred = pred, 
                                           resid = resids, Sex = sex),
                  ypr_predictions = data.frame(age = new_ages, 
                                               weight = ypr_preds,
                                               Sex = sex),
                  results = as.tibble(row.names(coef(summary(wvb_mle)))) %>% 
                    bind_cols(as_tibble(coef(summary(wvb_mle)))) %>% 
                    select(Parameter = `.rownames`, Estimate, SE = `Std. Error`) %>% 
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
# distributions, deviance information criterion, and convergence diagnostics all
# in one

mr_jags <- function(
  mod, # model formula (as a character string)
  mod_name, # what you want to name your model, keep them straight during model selection
  model_dat, # list of lists of model data 
  model_years, # vector of years over which you want to run the model
  mpar, # parameters of interest, sample poterior distribution of 'mpar' variables
  mchains = 4, # number of desired chains
  madapt = 2000, # burn in
  miter = 50000, # number of iterations to run each chain
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
    dplyr::summarize(mean = mean(N.avg),
           q025 = quantile(N.avg, 0.025),
           q975 = quantile(N.avg, 0.975)) %>% 
    mutate(model = mod_name) -> n_summary_out
  
    model_output <- list("results" = coda_res_out,
                         "N_summary" = n_summary_out,
                         "dic" = dic_out,
                         "convergence_diagnostic" = convergence_out)
    
    return(model_output)
   
}

# Tick marks ----

# Format ggplot figures with ticked axes (especially good for marking year and
# age) https://github.com/ben-williams/funcr/blob/be1751f34388aecd2e3703c19b2fac8e3380ee07/R/tickr.R

# Depends on dplyr
tickr <- function(data, var, to = 5, start = NULL, end = NULL, min = NULL){
  
  data %>%
    dplyr::summarise(min = min({{var}}, na.rm = T),
                     max = max({{var}}, na.rm = T)) -> out
  
  if(is.null(start) & is.null(end)){
    
    data.frame(breaks = out$min:out$max) %>%
      dplyr::mutate(labels = ifelse(breaks %in%
                                      seq(to * min(breaks) / to,
                                          max(breaks), by = to), breaks, ""))
    
  } else if(!is.null(start) & is.null(end) & is.null(min)){
    
    data.frame(breaks = start:out$max) %>%
      dplyr::mutate(labels = ifelse(breaks %in%
                                      seq(to * start / to, max(breaks),
                                          by = to), breaks, ""))
    
  } else if(!is.null(start) & is.null(end) & !is.null(min)){
    data.frame(breaks = start:out$max) %>%
      dplyr::mutate(labels = ifelse(breaks %in%
                                      seq(to * start / to, max(breaks),
                                          by = to), breaks, "")) %>%
      dplyr::filter(breaks >= min) -> lb
    lb$labels[1] <- lb$breaks[1]
    lb
    
  } else if(is.null(start) & !is.null(end)){
    
    data.frame(breaks = out$min:end) %>%
      dplyr::mutate(labels = ifelse(breaks %in%
                                      seq(to * min(breaks) / to, end, by = to),
                                    breaks, ""))
  } else {
    
    data.frame(breaks = start:end) %>%
      dplyr::mutate(labels = ifelse(breaks %in%
                                      seq(to * start / to, end, by = to),
                                    breaks, ""))
  }
}

# TMB functions ----

# Make data object for TMB
build_data <- function(
  # data sources (makes it easier to generalize for sensitivity and
  # retrospective analysts)
  ts,  ...) {
  
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
    random_rec = rec_type,
    
    # Switch for selectivity type: 0 = a50, a95 logistic; 1 = a50, slope logistic
    slx_type = slx_type,
    
    # Swtich for age composition type (hopefully one day length comps too): 0 =
    # multinomial; 1 = Dirichlet-multinomial
    comp_type = comp_type,
    
    # Switch for assumption on SPR equilibrium recruitment. 0 = arithmetic mean
    # (same as Federal assessment), 1 = geometric mean, 2 = median (2 not coded
    # yet)
    spr_rec_type = spr_rec_type,
    
    # Natural mortality
    M_type = M_type,  # Switch for natural mortality: fixed = 0, estimated with prior = 1. 
    p_log_M = log(0.1), # Priors for natural mortality (same as 2016-2019 Federal assessment
    p_sigma_M = 0.1,
    
    #Switch for extra variance in indices
    #ev_type = ev_type,
    
    # Time varying parameters - each vector contains the terminal years of each time block
    fsh_blks = c(ts %>% filter(year == 1994) %>% pull(index), max(ts$index)), #  fishery selectivity: limited entry in 1985, EQS in 1994 = c(5, 14, max(ts$year))
    srv_blks = c(max(ts$index)), # no breaks survey selectivity
    
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
    sigma_mr_q = 0.01, #0.01,
    
    # Weights on likelihood components ("wt_" denotes weight) based on weights
    # in Federal model
    
    # Updated weights for 2020 assessment (2021 ABC)
    wt_catch = 10.0, # fed = 50
    wt_fsh_cpue = 1,  # fed = 0.448
    wt_srv_cpue = 4, # fed = 0.448
    wt_mr = 1.5,
    wt_fsh_age = 6, # fed = 7.8
    wt_srv_age = 8, # fed = 7.95
    wt_fsh_len = 1, # fed = 1
    wt_srv_len = 1, # fed = 1
    wt_rec_like = 1,
    wt_fpen = 0.1,
    wt_spr = 100,
    
    # Original weights for 2019 assessment (2020 ABC)
    # wt_catch = 1.0, 
    # wt_fsh_cpue = 1.0,  
    # wt_srv_cpue = 1.0, 
    # wt_mr = 1.0,
    # wt_fsh_age = 1.0, 
    # wt_srv_age = 1.0, 
    # wt_fsh_len = 1.0, 
    # wt_srv_len = 1.0, 
    # wt_rec_like = 1.0,
    # wt_fpen = 0.1, 
    # wt_spr = 100,
    
    # Catch
    data_catch = ts$catch,
    sigma_catch = pull(ts, sigma_catch),
    
    # Mark-recapture estimates
    nyr_mr = n_distinct(mr, mr),
    yrs_mr = mr %>% distinct(index) %>% pull(),
    data_mr = pull(mr, mr),
    sigma_mr = rep(0.08, n_distinct(mr, mr)), #mr %>% pull(sigma_mr), 
    
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
        filter(waa, Source == "LL fishery" & Sex == "Combined") %>%
          pull(round_kg) %>%  matrix(ncol = nage, nrow = nsex)  %>%
          array(dim = c(1, nage, nsex))} else {
            # Sex-structured - males in first matrix, females in second
            filter(waa, Source == "LL fishery" & Sex %in% c("Male", "Female")) %>%
              arrange(desc(Sex)) %>% 
              pull(round_kg) %>% 
              matrix(ncol = nage, nrow = nsex)  %>%
              array(dim = c(1, nage, nsex))},
    
    # Survey weight-at-age used for everything except predicting catch
    data_srv_waa = 
      if (nsex == 1) { # Single sex model
        filter(waa, Source == "LL survey" & Sex == "Combined") %>% 
          pull(round_kg) %>%  matrix(ncol = nage, nrow = nsex)  %>% 
          array(dim = c(1, nage, nsex))} else {
            # Sex-structured - males in first matrix, females in second
            filter(waa, Source == "LL survey" & Sex %in% c("Male", "Female")) %>% 
              arrange(desc(Sex)) %>% 
              pull(round_kg) %>% 
              matrix(ncol = nage, nrow = nsex)  %>% 
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
        array(data = fsh_len %>% filter(Sex == "Sex combined") %>% pull(proportion),
              dim = c(length(unique(fsh_len$year)), nlenbin, nsex)) } else {
                # Sex-structured (make sure males are first)
                array(data = fsh_len %>% filter(Sex != "Sex combined") %>%
                        arrange(desc(Sex), length_bin, year) %>% pull(proportion),
                      dim = c(length(unique(fsh_len$year)), nlenbin, nsex))},
    
    n_fsh_len =
      if (nsex == 1) { # Single sex model
        array(data = fsh_len %>% filter(Sex == "Sex combined") %>% distinct(year, Sex, n) %>% pull(n),
              dim = c(length(unique(fsh_len$year)), 1, nsex)) } else {
                # Sex-structured (make sure males are first)
                array(data = fsh_len %>% filter(Sex != "Sex combined") %>% 
                        distinct(year, Sex, n) %>%
                        arrange(desc(Sex)) %>% pull(n),
                      dim = c(length(unique(fsh_len$year)), 1, nsex))},
    
    effn_fsh_len =
      if (nsex == 1) { # Single sex model
        array(data = fsh_len %>% filter(Sex == "Sex combined") %>% distinct(year, Sex, effn) %>% pull(effn),
              dim = c(length(unique(fsh_len$year)), 1, nsex)) } else {
                # Sex-structured (make sure males are first)
                array(data = fsh_len %>% filter(Sex != "Sex combined") %>%
                        distinct(year, Sex, effn) %>% 
                        arrange(desc(Sex)) %>% pull(effn),
                      dim = c(length(unique(fsh_len$year)), 1, nsex))},
    
    # Survey length comps
    nyr_srv_len = length(unique(srv_len$year)),
    yrs_srv_len = srv_len %>% distinct(index) %>% pull(),
    data_srv_len =
      if (nsex == 1) { # Single sex model
        array(data = srv_len %>% filter(Sex == "Sex combined") %>% pull(proportion),                              
              dim = c(length(unique(srv_len$year)), nlenbin, nsex)) } else {
                # Sex-structured (make sure males are first)
                array(data = srv_len %>% filter(Sex != "Sex combined") %>%
                      arrange(desc(Sex), length_bin, year) %>% pull(proportion),
                      dim = c(length(unique(srv_len$year)), nlenbin, nsex))},
    n_srv_len =
      if (nsex == 1) { # Single sex model
        array(data = srv_len %>% filter(Sex == "Sex combined") %>% distinct(year, Sex, n) %>% pull(n),
              dim = c(length(unique(srv_len$year)), 1, nsex)) } else {
                # Sex-structured (make sure males are first)
                array(data = srv_len %>% filter(Sex != "Sex combined") %>%
                        distinct(year, Sex, n) %>% 
                        arrange(desc(Sex)) %>% pull(n),
                      dim = c(length(unique(srv_len$year)), 1, nsex))},
    effn_srv_len =
      if (nsex == 1) { # Single sex model
        array(data = srv_len %>% filter(Sex == "Sex combined") %>%  distinct(year, Sex, effn) %>% pull(effn),
              dim = c(length(unique(srv_len$year)), 1, nsex)) } else {
                # Sex-structured (make sure males are first)
                array(data = srv_len %>% filter(Sex != "Sex combined") %>%
                        distinct(year, Sex, effn) %>% 
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
  
  return(data)
}

# Make parameter list for TMB
build_parameters <- function(
  # data,
  # nsex,
  # inits,
  rec_devs_inits,
  # rinit_devs_inits,
  Fdevs_inits,
  ...) {
  
  # Parameter starting values
  parameters <- list(
    
    dummy = 0,   # Used for troubleshooting model               
    
    log_M = log(0.1),  # M_type = 0 is fixed, 1 is estimated
    
    # Fishery selectivity - Contact fed sablefish author, tell him you want the
    # most recent tem.std file. You're looking for parameters for the pre-IFQ
    # domestic longline fishery (log_a50_fish1) and the IFQ fixed gear fishery
    # (e.g. log_a50_fish4_f, log_delta_fish4_f). Note that at least as of 2020
    # deltas are shared across time blocks and sex, but all a50s are distinct.
    # Logistic parameters need to be adjusted to a different scale b/c tmb model
    # fits between ages 0:29 instead of 2:31. see scaa_datprep.R for more info
    log_fsh_slx_pars = 
      # Logistic with a50 and a95, data$slx_type = 0, single sex model - needs
      # updating eventually if ever used
      if(data$slx_type == 0 & nsex == 1) {
        array(data = c(log(4.05), log(3.99), # Sexes combined
                       log(5.30), log(5.20)),
              dim = c(length(data$fsh_blks), 2, nsex)) # 2 = npar for this slx_type
        
        # Logistic with a50 and a95, data$slx_type = 0, sex-structured model - needs
        # updating eventually if ever used
      } else if (data$slx_type == 0 & nsex == 2) {
        array(data = c(log(4.19), log(5.12), # Male
                       log(5.50), log(6.30),
                       log(3.91), log(2.87), # Female
                       log(5.20), log(4.15)),
              dim = c(length(data$fsh_blks), 2, nsex)) # 2 = npar for this slx_type
        
        # Logistic with a50 and slope, data$slx_type = 1, single sex model - needs
        # updating eventually if ever used
      } else if (data$slx_type == 1 & nsex == 1) {
        array(data = c(log(4.05), log(3.99),
                       log(2.29), log(2.43)),
              dim = c(length(data$fsh_blks), 2, nsex)) # 2 = npar for this slx_type
        
        # FISHERY SELECTIVITY FOR 2020 assessment (2021 ABC)
      } else {  # Logistic with a50 and slope, data$slx_type = 1, sex-structured model - UPDATE ME!
        array(data = c(slx_pars$log_a50[slx_pars$fleet == "fsh_t1_m"], # male a50s time block 1
                       slx_pars$log_a50[slx_pars$fleet == "fsh_t2_m"], #  then time block 2
                       slx_pars$log_k[slx_pars$fleet == "fsh_t1_m"], # male slopes time block 1
                       slx_pars$log_k[slx_pars$fleet == "fsh_t2_m"], # then time block 2
                       slx_pars$log_a50[slx_pars$fleet == "fsh_t1_f"], # female a50s time block 1
                       slx_pars$log_a50[slx_pars$fleet == "fsh_t2_f"], #  then time block 2
                       slx_pars$log_k[slx_pars$fleet == "fsh_t1_f"], # female slopes time block 1
                       slx_pars$log_k[slx_pars$fleet == "fsh_t2_f"]), # then time block 2
              dim = c(length(data$fsh_blks), 2, nsex)) }, # 2 = npar for this slx_type
        
        # FISHERY SELECTIVITY FOR 2019 assessment (2020 ABC)
      # } else {  # Logistic with a50 and slope, data$slx_type = 1, sex-structured model
      #   array(data = c(log(5.12), log(4.22), # male
      #                  log(2.57), log(2.61),
      #                  log(2.87), log(3.86), # female
      #                  log(2.29), log(2.61)),
      #         dim = c(length(data$fsh_blks), 2, nsex)) }, # 2 = npar for this slx_type
    
    # Survey selectivity - Contact fed sablefish author, tell him you want the
    # most recent tem.std file. You're looking for parameters for the domestic
    # survey (e.g. log_a50_srv1_f) and at least as of 2020 deltas are shared
    # with log_a50_srv2_m (the male parameter for the cooperative US/JPN
    # survey). # Logistic parameters need to be adjusted to a different scale
    # b/c tmb model fits between ages 0:29 instead of 2:31. see scaa_datprep.R
    # for more info
    log_srv_slx_pars = 
      # Logistic with a50 and a95, data$slx_type = 0, single sex model- needs
      # updating eventually if ever used
      if(data$slx_type == 0 & nsex == 1) {
        array(data = c(rep(log(3.74), length(data$srv_blks)),
                       rep(log(5.20), length(data$srv_blks))),
              dim = c(length(data$srv_blks), 2, nsex)) # 2 = npar for this slx_type 
        
        # Logistic with a50 and a95, data$slx_type = 0, sex-structured model- needs
        # updating eventually if ever used
      } else if (data$slx_type == 0 & nsex == 2) {
        array(data = c(rep(0.911681626710, length(data$srv_blks)), # male
                       rep(0.656852516513, length(data$srv_blks)),
                       rep(0.961385402921, length(data$srv_blks)), # female
                       rep(0.656852516513, length(data$srv_blks))),
              dim = c(length(data$srv_blks), 2, nsex)) # 2 = npar for this slx_type 
        
        # Logistic with a50 and slope, data$slx_type = 1, single sex model- needs
        # updating eventually if ever used
      } else if (data$slx_type == 1 & nsex == 1) {
        array(data = c(rep(log(3.74), length(data$srv_blks)),
                       rep(log(1.96), length(data$srv_blks))),
              dim = c(length(data$srv_blks), 2, nsex)) # 2 = npar for this slx_type 
        
        
        # SURVEY SELECTIVITY FOR 2020 assessment (2021 ABC)
        # Logistic with a50 and slope, data$slx_type = 1, sex-structured model UPDATE ME!
      } else {
        array(data = c(rep(slx_pars$log_a50[slx_pars$fleet == "srv_m"], length(data$srv_blks)), # male a50
                       rep(slx_pars$log_k[slx_pars$fleet == "srv_m"], length(data$srv_blks)), # slope
                       rep(slx_pars$log_a50[slx_pars$fleet == "srv_f"], length(data$srv_blks)), # female a50
                       rep(slx_pars$log_k[slx_pars$fleet == "srv_f"], length(data$srv_blks))), # slope
              dim = c(length(data$srv_blks), 2, nsex))}, # 2 = npar for this slx_type
        
    # SURVEY SELECTIVITY FOR 2019 assessment (2020 ABC)
      # } else { 
      #   array(data = c(rep(log(3.72), length(data$srv_blks)), # male
      #                  rep(log(2.21), length(data$srv_blks)),
      #                  rep(log(3.75), length(data$srv_blks)), # female
      #                  rep(log(2.21), length(data$srv_blks))),
      #         dim = c(length(data$srv_blks), 2, nsex)) }, # 2 = npar for this slx_type
    
    
    # Catchability
    
    
    fsh_logq = inits %>% filter(grepl("fsh_logq", Parameter)) %>% pull(Estimate), 
    srv_logq = inits %>% filter(grepl("srv_logq", Parameter)) %>% pull(Estimate),
    mr_logq = inits %>% filter(grepl("srv_logq", Parameter)) %>% pull(Estimate),
    
    # Log mean recruitment and deviations (nyr)
    log_rbar = 2.5, #inits %>% filter(Parameter == "log_rbar") %>% pull(Estimate), 
    log_rec_devs = rec_devs_inits, #rnorm(nyr,0,1), #
    
    # Log mean initial numbers-at-age and deviations (nage-2)
    log_rinit = 8, #inits %>% filter(Parameter == "log_rinit") %>% pull(Estimate), 
    log_rinit_devs = rinit_devs_inits,
    
    # Variability in rec_devs and rinit_devs
    log_sigma_r = log(1.2), # Federal value of 1.2 on log scale
    
    # Fishing mortality
    log_Fbar = -3, #inits %>% filter(Parameter == "log_Fbar") %>% pull(Estimate),
    log_F_devs = rnorm(nyr,0,1), #Fdevs_inits,
    
    # SPR-based fishing mortality rates, i.e. the F at which the spawning biomass
    # per recruit is reduced to xx% of its value in an unfished stock
    log_spr_Fxx = inits %>% filter(grepl("spr_Fxx", Parameter)) %>% pull(Estimate), # F35, F40, F50, F60, F70
    
    # Parameter related to effective sample size for Dirichlet-multinomial
    # likelihood used for composition data. Default of 10 taken from LIME model by
    # M. Rudd. Estimated in log-space b/c it can only be positive.
    log_fsh_theta = log(10),   
    log_srv_theta = log(10)
  )
  
  return(parameters)
}

# Build random vars list for estimating random effects

build_random_vars <- function(
  # data
  ...) {
  
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
    random_vars <- NULL 
  }
  
  return(random_vars)
}

# Build parameter bounds
# Original code by Grant Adams, adapted for use with the sablefish model

build_bounds <- function(param_list = NULL, data_list){
  
  # Debug function
  # param_list <- parameters
  # data_list <- data
  
  upper_bnd <- param_list
  lower_bnd <- param_list
  
  # General bounds
  for(i in 1:length(param_list)){
    upper_bnd[[i]] <- replace(upper_bnd[[i]], values = rep(Inf, length(upper_bnd[[i]])))
    lower_bnd[[i]] <- replace(lower_bnd[[i]], values = rep(-Inf, length(lower_bnd[[i]])))
  }
  
  # Natural mortality
  lower_bnd$log_M <- replace(lower_bnd$log_M, values = -3) 
  upper_bnd$log_M <- replace(upper_bnd$log_M, values = -1) 
  
  # Fishery selectivity
  lower_bnd$log_fsh_slx_pars[,,] <- replace(lower_bnd$log_fsh_slx_pars[,,], values = -2) 
  upper_bnd$log_fsh_slx_pars[,,] <- replace(upper_bnd$log_fsh_slx_pars[,,], values = 3) 
  
  # Survey selectivity
  lower_bnd$log_srv_slx_pars[,,] <- replace(lower_bnd$log_srv_slx_pars[,,], values = -2) 
  upper_bnd$log_srv_slx_pars[,,] <- replace(upper_bnd$log_srv_slx_pars[,,], values = 3) 
  
  # Fishery catchability
  lower_bnd$fsh_logq <- replace(lower_bnd$fsh_logq, values = rep(-30, length(lower_bnd$fsh_logq)))
  upper_bnd$fsh_logq <- replace(upper_bnd$fsh_logq, values = rep(5, length(upper_bnd$fsh_logq)))
  
  # Survey catchability
  lower_bnd$srv_logq <- replace(lower_bnd$srv_logq, values = rep(-30, length(lower_bnd$srv_logq)))
  upper_bnd$srv_logq <- replace(upper_bnd$srv_logq, values = rep(5, length(upper_bnd$srv_logq)))
  
  # Mark-recapture catchability
  lower_bnd$mr_logq <- replace(lower_bnd$mr_logq, values = rep(-20, length(lower_bnd$mr_logq)))
  upper_bnd$mr_logq <- replace(upper_bnd$mr_logq, values = rep(1, length(upper_bnd$mr_logq)))
  
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
  lower_bnd$log_spr_Fxx <- replace(lower_bnd$log_spr_Fxx, values = rep(-5, length(lower_bnd$log_spr_Fxx)))
  upper_bnd$log_spr_Fxx <- replace(upper_bnd$log_spr_Fxx, values = rep(5, length(upper_bnd$log_spr_Fxx)))

  # Fishery age comp Dirichlet-multinomial theta
  lower_bnd$log_fsh_theta <- replace(lower_bnd$log_fsh_theta, values = rep(-5, length(lower_bnd$log_fsh_theta)))
  upper_bnd$log_fsh_theta <- replace(upper_bnd$log_fsh_theta, values = rep(15, length(upper_bnd$log_fsh_theta)))
  
  # Survey age comp Dirichlet-multinomial theta
  lower_bnd$log_srv_theta <- replace(lower_bnd$log_srv_theta, values = rep(-5, length(lower_bnd$log_srv_theta)))
  upper_bnd$log_srv_theta <- replace(upper_bnd$log_srv_theta, values = rep(15, length(upper_bnd$log_srv_theta)))
  
  #Fishery length comp Dirilichlet-multinomial theta
  lower_bnd$log_fsh_l_theta <- replace(lower_bnd$log_fsh_l_theta, values = rep(-5, length(lower_bnd$log_fsh_l_theta)))
  upper_bnd$log_fsh_l_theta <- replace(upper_bnd$log_fsh_l_theta, values = rep(15, length(upper_bnd$log_fsh_l_theta)))
  
  #Survey length comp Dirilichlet-multinomial theta
  lower_bnd$log_srv_l_theta <- replace(lower_bnd$log_srv_l_theta, values = rep(-5, length(lower_bnd$log_srv_l_theta)))
  upper_bnd$log_srv_l_theta <- replace(upper_bnd$log_srv_l_theta, values = rep(15, length(upper_bnd$log_srv_l_theta)))
  
  #extra variance terms for indices
  lower_bnd$log_tau_fsh <- replace(lower_bnd$log_tau_fsh, values = -20) 
  upper_bnd$log_tau_fsh <- replace(upper_bnd$log_tau_fsh, values = -0.5) 
 
  lower_bnd$log_tau_srv <- replace(lower_bnd$log_tau_srv, values = -20) 
  upper_bnd$log_tau_srv <- replace(upper_bnd$log_tau_srv, values = -0.5) 
  
  lower_bnd$log_tau_mr <- replace(lower_bnd$log_tau_mr, values = -20) 
  upper_bnd$log_tau_mr <- replace(upper_bnd$log_tau_mr, values = -0.5) 
  
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

  # 2: Fishing mortality devs and catchability
  phases$log_F_devs <- replace(phases$log_F_devs, values = rep(4, length(phases$log_F_devs)))
  phases$fsh_logq <- replace(phases$fsh_logq, values = rep(3, length(phases$fsh_logq)))
  phases$srv_logq <- replace(phases$srv_logq, values = rep(3, length(phases$srv_logq)))
  
  # 4: Selectivity - turned off until selectivity/discarding issue can be fixed
  phases$log_fsh_slx_pars[,,] <- replace(phases$log_fsh_slx_pars[,,], values = 4)
  phases$log_srv_slx_pars[,,] <- replace(phases$log_srv_slx_pars[,,], values = 2)

  # 4: Reference points
  phases$log_spr_Fxx <- replace(phases$log_spr_Fxx, values = rep(5, length(phases$log_spr_Fxx)))
  phases$log_M <- replace(phases$log_M, values = rep(5, length(phases$log_M)))
  
  # 5: Dirichlet-multinomial theta parameters (this will get turned off if
  # comp_type != 1 in the TMBphase function)
  phases$log_fsh_theta <- replace(phases$log_fsh_theta, values = rep(5, length(phases$log_fsh_theta)))
  phases$log_srv_theta <- replace(phases$log_srv_theta, values = rep(5, length(phases$log_srv_theta)))

  return(phases)
}

# function to fill list component with a factor
fill_vals <- function(x,vals){rep(as.factor(vals), length(x))}

# Original code by Gavin Fay, adaped for use in the sablefish model - requires fill_vals() fxn
TMBphase <- function(data, parameters, random, model_name, phase = FALSE,
                     optimizer = "nlminb", debug = FALSE, loopnum = 3, newtonsteps = 0) {
  
  # Debug function
  # random <-  random_vars# <- NULL
  # phases <- build_phases(parameters, data)
  # model_name <- "scaa_mod"
  # debug <- FALSE

  # compile the model
  TMB::compile(paste0(model_name,".cpp"))
  dyn.load(TMB::dynlib(model_name))
  DLL_use <- model_name  
  out <- list() 
  
  if (phase == FALSE) {
    
    # work out the map for this phase if phases for parameters is less than the
    # current phase then map will contain a factor filled with NAs
    map_use <- list()
    map_use$dummy <- fill_vals(parameters$dummy, NA)
    
    # if not using random effects, assign log_sigma_r an NA in the map so it's not estimated
    if (data$random_rec == FALSE) {
      map_use$log_sigma_r <- fill_vals(parameters$log_sigma_r, NA)
    }
    
    # if natural mortality is fixed, assign log_M an NA in the map so its not
    # estimated
    if (data$M_type == 0) {
      map_use$log_M <- fill_vals(parameters$log_M, NA)
    }
    
    # if not using the Dirichlet-multinonial, assign log_fsh_theta and
    # log_srv_theta NAs in the map so they're not estimated
    if (data$comp_type != 1) {
      map_use$log_fsh_theta <- fill_vals(parameters$log_fsh_theta, NA)
      map_use$log_srv_theta <- fill_vals(parameters$log_srv_theta, NA)
      #map_use$log_fsh_l_theta <- fill_vals(parameters$log_fsh_l_theta, NA)  #add these back in when full dirichlet developed... 
      #map_use$log_srv_l_theta <- fill_vals(parameters$log_srv_l_theta, NA)
    }
    
    if (data$ev_type != 1) {
      map_use$log_tau_fsh <- fill_vals(parameters$log_tau_fsh, NA)
      map_use$log_tau_srv <- fill_vals(parameters$log_tau_srv, NA)
      map_use$log_tau_mr <- fill_vals(parameters$log_tau_mr, NA)
    }
    
    # Temporary debug trying to figure out why I'm getting NA/NaN function
    # evaluation - think it's something to do with discarding
    if (tmp_debug == TRUE) {
      map_use$log_fsh_slx_pars <- fill_vals(parameters$log_fsh_slx_pars, NA)
      map_use$log_srv_slx_pars <- fill_vals(parameters$log_srv_slx_pars, NA)
      # map_use$mr_logq <- fill_vals(parameters$mr_logq, NA)
      # map_use$log_fsh_slx_pars <- factor(c(1, 2, NA, NA, 3, 4, NA, NA))
      # map_use$log_srv_slx_pars <- factor(c(1, NA, 2, NA))
    }
    
    # Build upper and lower parameter bounds and remove any that are not
    # estimated (should be the inverse of the map_use)
    bounds <- build_bounds(param_list = parameters)
    bounds$upper <- bounds$upper[!names(bounds$upper) %in% names(map_use)]
    bounds$lower <- bounds$lower[!names(bounds$lower) %in% names(map_use)]
    
    # Remove inactive parameters from bounds and vectorize
    lower <- unlist(bounds$lower)
    upper <- unlist(bounds$upper)
    
    # Remove random effects from bounds
    if (data$random_rec == FALSE) {
      lower <- lower[!names(lower) %in% "log_sigma_r"]
      upper <- upper[!names(lower) %in% "log_sigma_r"]
    }    
    
    # if (data$random_rec == TRUE) {
    #   lower <- lower[-which(grepl(random[1], names(lower)))]
    #   lower <- lower[-which(grepl(random[2], names(lower)))]
    #   upper <- upper[-which(grepl(random[1], names(upper)))]
    #   upper <- upper[-which(grepl(random[2], names(upper)))]
    # }
    
    # initialize the parameters at values in previous phase
    params_use <- parameters
    
    # Fit the model
    obj <- TMB::MakeADFun(data,params_use,random=NULL,
                          DLL=DLL_use,map=map_use)  
    
    TMB::newtonOption(obj,smartsearch=FALSE)
    # lower and upper bounds relate to obj$par and must be the same length as obj$par
    opt <- nlminb(start = obj$par, objective = obj$fn, hessian = obj$gr,
                  control = list(eval.max = 1e4, iter.max = 1e4, trace = 0),
                  lower = lower, upper = upper)
    
    # Re-run to further decrease final gradient - used tmb_helper.R -
    # https://github.com/kaskr/TMB_contrib_R/blob/master/TMBhelper/R/fit_tmb.R
    for(i in seq(2, loopnum, length = max(0, loopnum - 1))){
      tmp <- opt[c('iterations','evaluations')]
      opt <- nlminb(start = opt$par, objective = obj$fn, gradient = obj$gr, 
                   control = list(eval.max = 1e4, iter.max = 1e4, trace = 0), 
                   lower = lower, upper = upper )
      opt[['iterations']] <- opt[['iterations']] + tmp[['iterations']]
      opt[['evaluations']] <- opt[['evaluations']] + tmp[['evaluations']]
    }
    
    # Run some Newton steps - slow but reduces final gradient (code also from
    # tmb_helper.R)
    for(i in seq_len(newtonsteps)) {
      g <- as.numeric(obj$gr(opt$par))
      h <- optimHess(opt$par, fn = obj$fn, gr = obj$gr)
      opt$par <- opt$par - solve(h, g)
      opt$objective <- obj$fn(opt$par)
    }
    rep <- TMB::sdreport(obj)
  }
  
  if (phase == TRUE) {
    
    phases <- build_phases(parameters, data)
    
    #loop over phases
    for (phase_cur in 1:max(unlist(phases))) {
      
      # phase_cur <- 1 # Debug function
      
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
        
        # if not using random effects, assign log_sigma_r an NA in the map so it's not estimated
        if (data$random_rec == FALSE) {
          map_use$log_sigma_r <- fill_vals(parameters$log_sigma_r, NA)
        }
        
        # if natural mortality is fixed, assign log_M an NA in the map so its not
        # estimated
        if (data$M_type == 0) {
          map_use$log_M <- fill_vals(parameters$log_M, NA)
        }
        
        # if not using the Dirichlet-multinonial, assign log_fsh_theta and
        # log_srv_theta NAs in the map so they're not estimated
        if (data$comp_type != 1) {
          map_use$log_fsh_theta <- fill_vals(parameters$log_fsh_theta, NA)
          map_use$log_srv_theta <- fill_vals(parameters$log_srv_theta, NA)
        }
        
        # Temporary debug trying to figure out why I'm getting NA/NaN function
        # evaluation - think it's something to do with discarding
        if (tmp_debug == TRUE) {
          map_use$log_fsh_slx_pars <- fill_vals(parameters$log_fsh_slx_pars, NA)
          map_use$log_srv_slx_pars <- fill_vals(parameters$log_srv_slx_pars, NA)
          # map_use$mr_logq <- fill_vals(parameters$mr_logq, NA)
          
          # map_use$log_fsh_slx_pars <- factor(c(1, 2, NA, NA, 3, 4, NA, NA))
          # map_use$log_srv_slx_pars <- factor(c(1, NA, 2, NA))
        }
        
        # j <- 1 # change to 0 if you get rid of the dummy debugging feature
        # 
        # for (i in 1:length(parameters)) {
        #   # if (phases[[i]]>=phase_cur) {
        #   if (phases[[i]]>phase_cur) {
        #     j <- j+1
        #     map_use[[j]] <- fill_vals(parameters[[i]], NA)
        #     names(map_use)[j] <- names(parameters)[i]               
        #   }
        # }
      }
      
      # Build upper and lower parameter bounds and remove any that are not
      # estimated (should be the inverse of the map_use)
      bounds <- build_bounds(param_list = parameters)
      bounds$upper <- bounds$upper[!names(bounds$upper) %in% names(map_use)]
      bounds$lower <- bounds$lower[!names(bounds$lower) %in% names(map_use)]
      
      # Remove inactive parameters from bounds and vectorize
      lower <- unlist(bounds$lower)
      upper <- unlist(bounds$upper)
      
      # Remove random effects from bounds
      if (data$random_rec == FALSE) {
        lower <- lower[!names(lower) %in% "log_sigma_r"]
        upper <- upper[!names(lower) %in% "log_sigma_r"]
      }
      
      # if (data$random_rec == TRUE) {
      #   lower <- lower[which(!grepl(random[1], names(lower)))]
      #   lower <- lower[which(!grepl(random[2], names(lower)))]
      #   upper <- upper[which(!grepl(random[1], names(upper)))]
      #   upper <- upper[which(!grepl(random[2], names(upper)))]
      # }
      
      # initialize the parameters at values in previous phase
      params_use <- parameters
      if (phase_cur>1) params_use <- obj$env$parList(obj$env$last.par.best)
      
      # Fit the model
      obj <- TMB::MakeADFun(data,params_use,random=NULL, 
                            DLL=DLL_use,map=map_use)  
      
      TMB::newtonOption(obj,smartsearch=FALSE)
      # lower and upper bounds relate to obj$par and must be the same length as obj$par
      opt <- nlminb(start = obj$env$last.par.best, objective = obj$fn, hessian = obj$gr,
                    control=list(eval.max = 1e4, iter.max = 1e4, trace = 0),
                    lower = lower, upper = upper)
      rep <- TMB::sdreport(obj)
    }
  }
  out$obj <- obj
  out$opt <- opt 
  out$rep <- rep
  out$map <- map_use
  out$upper <- upper
  out$lower <- lower
  return(out)  
}

# Function to jitter or generate random starting values when running MCMC on tmb model.
init_fn <- function(){list(
  #log_M = rnorm(n = 1, mean = log(0.1), sd = 0.1),
  fsh_logq = sort(runif(2, -18, -16)),
  srv_logq = runif(1, -18, -16),
  mr_logq = runif(1, -1, 1),
  log_rbar = runif(1, 1, 15),
  log_rec_devs = runif(length(parameters$log_rec_devs), -10, 10),
  log_rinit = runif(1, 1, 15),
  log_rinit_devs = runif(length(parameters$log_rinit_devs), -10, 10),
  log_Fbar = runif(1, -5, 5),
  log_F_devs = runif(length(parameters$log_rec_devs), -5, 5),
  log_spr_Fxx = sort(runif(length(data$Fxx_levels), -4, -1), decreasing = TRUE)
)
}

# TMB figure/output functions ----

# Save MLE parameter estimates from TMB
save_mle <- function(#rep, # output from sdreport()
                     save = TRUE,   # save output
                     path = tmbout, # file path for tmb output 
                     save_inits = TRUE, # save mle estimates as starting values for next yr?
                     path_inits = tmb_dat, # file path for starting values
                     year = YEAR) {
  
  # MLE parameter estimates and standard errors in useable format
  # tidyrep <- tidy(summary(rep)) # broom no longer works. it's sad.
  tidyrep <- as.data.frame(summary(rep), row.names = FALSE)
  names(tidyrep) <- c("Estimate", "se")
  tidyrep <- tidyrep %>% 
    mutate(Parameter = rownames(as.data.frame(summary(rep)))) %>% 
    select(Parameter, Estimate, se)
  
  key_params <- filter(tidyrep, !grepl('devs', Parameter)) # "Key" parameters (exclude devs)
  
  # Save output
  if(save == TRUE) {
  write_csv(key_params, paste0(path, "/tmb_params_mle_", year, ".csv"))
  write_csv(tidyrep, paste0(path, "/tmb_allparams_mle_", year, ".csv"))
  }
  
  # Save starting values for next year
  if(save_inits == TRUE) {
    write_csv(tidyrep, paste0(path_inits, "/inits_for_", year+1, ".csv"))
  }
  
  return(tidyrep)
}

# Unit translations

kg2lb <- function(x) 2.20462 * x 
mt2mlb <- function(x) 2204.62 * x / 1e6
n2millions <- function(x) x / 1e6
kg2mlb <- function(x) x * 2.20462 / 1e6

# Functions to summarize posterior samples by year or without a 'by' statement
post_byyear <- function(df, by = year) {
  
  names(df) <- c("var", "iter", "year")
  df <- df %>% 
    group_by({{ by }}) %>% 
    dplyr::summarise(mean = mean(var, na.rm = TRUE),
              median = median(var, na.rm = TRUE),
              q025 = quantile(var, 0.025, na.rm = TRUE),
              q975 = quantile(var, 0.975, na.rm = TRUE))
  return(df)
}

postsum <- function(df) {
  
  names(df) <- c("var", "iter")
  df <- df %>% 
    dplyr::summarise(mean = mean(var, na.rm = TRUE),
              median = median(var, na.rm = TRUE),
              q025 = quantile(var, 0.025, na.rm = TRUE),
              q975 = quantile(var, 0.975, na.rm = TRUE))
  return(df)
}

# Summarize mcmc posterior samples for all derived variables
summarize_mcmc <- function(post) {
  
  # Create a test for posterior samples that enter an invalid space
  tst <- list()
  
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
    
    # Test for NAs for Inf values and skip this iteration if results are invalid.
    tmp_tst <- lapply(r, function(x) sum(which(is.na(x) | is.infinite(x))))
    
    tmp_tst <- colSums(do.call(rbind, tmp_tst))
    
    if(tmp_tst > 0) {
      
      tst[[i]] <- 0 
      
    } else {
      
      tst[[i]] <- 1
      
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
    
  }
  
  tst <- as.data.frame(do.call(rbind, tst))
  
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
  write_csv(post_catch, paste0(tmbout, "/post_catch_kg_", YEAR, ".csv"))
  write_csv(post_fsh_cpue, paste0(tmbout, "/post_fsh_cpue_kgperhook_", YEAR, ".csv"))
  write_csv(post_srv_cpue, paste0(tmbout, "/post_srv_cpue_nperhook_", YEAR, ".csv"))
  write_csv(post_mr, paste0(tmbout, "/post_mr_millions_", YEAR, ".csv"))
  write_csv(post_rec, paste0(tmbout, "/post_rec_age2_numbers_", YEAR, ".csv"))
  write_csv(post_biom, paste0(tmbout, "/post_biom_kg_", YEAR, ".csv"))
  write_csv(post_expl_biom, paste0(tmbout, "/post_expl_biom_kg_", YEAR, ".csv"))
  write_csv(post_expl_abd, paste0(tmbout, "/post_expl_abd_numbers", YEAR, ".csv"))
  write_csv(post_spawn_biom, paste0(tmbout, "/post_spawn_biom_kg_", YEAR, ".csv"))
  write_csv(post_ABC, paste0(tmbout, "/post_ABC_kg_", YEAR, ".csv"))
  write_csv(post_wastage, paste0(tmbout, "/post_wastage_kg_", YEAR, ".csv"))
  write_csv(post_SB100, paste0(tmbout, "/post_SB100_kg_", YEAR, ".csv"))
  write_csv(post_SB50, paste0(tmbout, "/post_SB50_kg_", YEAR, ".csv"))
  write_csv(post_F50, paste0(tmbout, "/post_F50_", YEAR, ".csv"))
  
  out <- list()
  out$tst <- tst
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
  
  return(out)
  
}

# Reshape ts (index) data and get resids
reshape_ts <- function(ts){
  
  ts$pred_catch <- obj$report(best)$pred_landed
  ts %>% 
    mutate(catch_resid = catch - pred_catch,
           catch_sresid = catch_resid / sd(catch_resid)) -> ts
  
  fsh_cpue$pred_fsh_cpue <- obj$report(best)$pred_fsh_cpue
  fsh_cpue %>% 
    mutate(fsh_cpue_resid = fsh_cpue - pred_fsh_cpue,
           fsh_cpue_sresid = fsh_cpue_resid / sd(fsh_cpue_resid),
           period = ifelse(year <= 1994, "pre-EQS", "EQS")) -> fsh_cpue
  
  srv_cpue$pred_srv_cpue <- obj$report(best)$pred_srv_cpue
  srv_cpue %>% 
    mutate(srv_cpue_resid = srv_cpue - pred_srv_cpue,
           srv_cpue_sresid = srv_cpue_resid / sd(srv_cpue_resid)) -> srv_cpue
  
  mr %>% 
    mutate(pred_mr = obj$report(best)$pred_mr,
           mr_resid = mr - pred_mr,
           mr_sresid = mr_resid / sd(mr_resid)) %>% 
    select(year, mr, pred_mr, upper_mr, lower_mr, mr_sresid, mr_resid) -> mr_plot
  
  ts %>%
    select(year, mr) %>%
    mutate(pred_mr_all = obj$report(best)$pred_mr_all) -> mr_plot_all

  out <- list()
  out$ts <- ts
  out$fsh_cpue <- fsh_cpue
  out$srv_cpue <- srv_cpue
  out$mr_plot <- mr_plot
  out$mr_plot_all <- mr_plot_all
  
  return(out)
}

# Plot time series
plot_ts <- function(save = TRUE, path = tmbfigs, ts,
                    units = c('metric', 'imperial'), plot_variance = TRUE){
  

  out <- reshape_ts(ts = ts)
  out$ts -> catch # catch in kg
  out$fsh_cpue -> fsh_cpue # kg/hk
  out$srv_cpue -> srv_cpue
  out$mr_plot -> mr_plot
  out$mr_plot_all -> mr_plot_all
  
  if(save == TRUE) {
    out$ts %>% 
      select(year, catch, pred_catch) %>% 
      left_join(out$fsh_cpue %>% select(year, fsh_cpue, pred_fsh_cpue)) %>%
      left_join(out$srv_cpue %>% select(year, srv_cpue, pred_srv_cpue)) %>%
      left_join(out$mr_plot %>% select(year, mr, pred_mr)) %>% 
    write_csv(paste0(path, "/ts_pred_imperial_", YEAR, ".csv"))}
  
  if(units == "imperial") { # Convert catch from mt to million lb and fsh_cpue from kg to lb
    
    catch <- catch %>% mutate_at(c("catch", "pred_catch", "upper_catch", "lower_catch"), mt2mlb) 
    fsh_cpue <- fsh_cpue %>% mutate_at(c("fsh_cpue", "pred_fsh_cpue", "upper_fsh_cpue", "lower_fsh_cpue"), kg2lb)
  }
  
  if(plot_variance == TRUE) {
    
    post_catch <- sum_mcmc$post_catch
    post_fsh_cpue <- sum_mcmc$post_fsh_cpue
    post_fsh_cpue <- post_fsh_cpue %>% mutate(period = ifelse(year <= 1994, "pre-EQS", "EQS"))
    post_srv_cpue <- sum_mcmc$post_srv_cpue
    post_mr <- sum_mcmc$post_mr
    
    if(units == "imperial") {
      post_catch <- post_catch %>% mutate_at(c("mean", "median", "q025", "q975"), mt2mlb)
      post_fsh_cpue <- post_fsh_cpue %>% mutate_at(c("mean", "median", "q025", "q975"), kg2lb)
    }
    
    # axis <- tickr(catch, year, 5)
    
    # Catch
    ggplot(data = catch, aes(x = year)) +
      geom_point(aes(y = catch), colour = "black") +
      geom_line(data = post_catch, aes(y = median)) +
      # model credible intervals 
      geom_ribbon(data = post_catch, aes(year, ymin = q025, ymax = q975),
                  alpha = 0.2,  fill = "black", colour = NA) +
      # assumed error for data
      geom_errorbar(aes(year, ymin = lower_catch, ymax = upper_catch), width = 0.2) +
      # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
      # scale_y_continuous(label = scales::comma) +
      labs(x = NULL, y = ifelse(units == "metric", "\n\nCatch\n(round mt)",
                                "\n\nCatch\n(million round lb)")) +
      expand_limits(y = 0) +
      theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) -> p_catch
    
    # Fishery cpue
    ggplot(fsh_cpue, aes(x = year)) +
      geom_point(aes(y = fsh_cpue), colour = "black") +
      geom_line(data = post_fsh_cpue, aes(y = median, lty = period)) +
      geom_ribbon(data = post_fsh_cpue, aes(year, ymin = q025 , ymax = q975, group = period),
                  alpha = 0.2, fill = "black", colour = NA) +
      geom_errorbar(aes(year, ymin = lower_fsh_cpue , ymax = upper_fsh_cpue), width = 0.2) +
      scale_x_continuous(limits = c(min(ts$year), max(ts$year))) + #,
                         # breaks = axis$breaks, labels = axis$labels) +
      expand_limits(y = 0) +
      labs(x = NULL, y = ifelse(units == "metric", "\n\nFishery CPUE\n(round kg/hook)",
                                "\n\nFishery CPUE\n(round lb/hook)"), lty = NULL) +
      theme(legend.position = c(0.8, 0.8),
            axis.title.y = element_text(angle = 0, vjust = 0.5)) -> p_fsh
    
    # Survey cpue
    ggplot(srv_cpue, aes(x = year)) +
      geom_point(aes(y = srv_cpue), colour = "black") +
      geom_line(data = post_srv_cpue, aes(y = median)) +
      geom_ribbon(data = post_srv_cpue, aes(year, ymin = q025, ymax = q975),
                  alpha = 0.2, fill = "black", colour = NA) +
      geom_errorbar(aes(year, ymin = lower_srv_cpue, ymax = upper_srv_cpue), width = 0.2) +
      scale_x_continuous(limits = c(min(ts$year), max(ts$year))) + #,
                         # breaks = axis$breaks, labels = axis$labels) +
      expand_limits(y = 0) +
      labs(x = NULL, y = "\n\nSurvey CPUE\n(fish/hook)") +
      theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) -> p_srv
    
    # Mark recapture 
    ggplot(post_mr, aes(x = year)) +
      geom_point(data = mr_plot, aes(y = mr), colour = "black") +
      geom_line(aes(y = median)) +
      geom_ribbon(aes(x = year, ymin = q025, ymax = q975),
                  alpha = 0.2, fill = "black", colour = NA) +
      geom_errorbar(data = mr_plot, aes(x = year, ymin = lower_mr, ymax = upper_mr), width = 0.2) +
      # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
      expand_limits(y = 0) +
      labs(x = NULL, y = "\n\nAbundance\n(millions)") +
      theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) -> p_mr
  }
  
  if(plot_variance == FALSE) {
    
  # Catch 
  # axis <- tickr(catch, year, 5)
  ggplot(catch, aes(x = year)) +
    geom_point(aes(y = catch), colour = "darkgrey") +
    geom_line(aes(y = pred_catch)) +
    geom_ribbon(aes(year, ymin = lower_catch, ymax = upper_catch),
                alpha = 0.2,  fill = "black", colour = NA) +
    # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
    # scale_y_continuous(label = scales::comma) +
    expand_limits(y = 0) +
    labs(x = NULL, y = ifelse(units == "metric", "\n\nCatch\n(round mt)",
                              "\n\nCatch\n(million round lb)")) +
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) -> p_catch
  
  # Fishery cpue
  ggplot(fsh_cpue, aes(x = year)) +
    geom_point(aes(y = fsh_cpue), colour = "darkgrey") +
    geom_line(aes(y = pred_fsh_cpue, lty = period)) +
    geom_ribbon(aes(year, ymin = lower_fsh_cpue , ymax = upper_fsh_cpue),
                alpha = 0.2, fill = "black", colour = NA) +
    scale_x_continuous(limits = c(min(ts$year), max(ts$year))) + #,
                       # breaks = axis$breaks, labels = axis$labels) +
    expand_limits(y = 0) +
    labs(x = NULL, y = ifelse(units == "metric", "\n\nFishery CPUE\n(round kg/hook)",
                              "\n\nFishery CPUE\n(round lb/hook)"), lty = NULL) +
    theme(legend.position = c(0.8, 0.8),
          axis.title.y = element_text(angle = 0, vjust = 0.5))-> p_fsh
  
  # Survey cpue
  ggplot(srv_cpue, aes(x = year)) +
    geom_point(aes(y = srv_cpue), colour = "darkgrey") +
    geom_line(aes(y = pred_srv_cpue)) +
    geom_ribbon(aes(year, ymin = lower_srv_cpue, ymax = upper_srv_cpue),
                alpha = 0.2, fill = "black", colour = NA) +
    scale_x_continuous(limits = c(min(ts$year), max(ts$year))) + #,
                       # breaks = axis$breaks, labels = axis$labels) +
    expand_limits(y = 0) +
    labs(x = NULL, y = "\n\nSurvey CPUE\n(fish/hook)") +
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) -> p_srv
  
  # Mark recapture 
  mr_plot %>% 
    mutate(year = as.Date(as.character(year), format = "%Y")) %>% 
    pad(interval = "year") %>% 
    mutate(year = year(year),
           Year = factor(year)) %>% 
    gather("Abundance", "mr", mr) %>% 
    arrange(year) %>% 
    mutate(# interpolate the CI in missing years for plotting purposes
      lower_mr = zoo::na.approx(lower_mr, maxgap = 20, rule = 2),
      upper_mr = zoo::na.approx(upper_mr, maxgap = 20, rule = 2)) %>%
  ggplot() +
    geom_ribbon(aes(x = year, ymin = lower_mr, ymax = upper_mr),
                alpha = 0.2, fill = "black", colour = NA) +
    geom_point(aes(x = year, y = mr), colour = "darkgrey") +
    geom_line(aes(x = year, y = pred_mr, group = 1)) +
    geom_line(data = mr_plot_all, aes(x = year, y = pred_mr_all, group = 1), lty = 2) +
    # scale_x_continuous( breaks = axis$breaks, labels = axis$labels) +
    expand_limits(y = 0) +
    labs(x = NULL, y = "\n\nAbundance\n(millions)") +
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) -> p_mr
  }
  
  plot_grid(p_catch, p_fsh, p_srv, p_mr, ncol = 1, align = 'hv', 
            labels = c('(A)', '(B)', '(C)', '(D)')) -> p
  
  print(p)
  
  mcmc_flag <- ifelse(plot_variance == TRUE, "_mcmc", "")

  if(save == TRUE){ 
    ggsave(plot = p, filename = paste0(path, "/pred_abd_indices_", units, "_", YEAR, mcmc_flag, ".png"), 
           # dpi = 300, height = 7, width = 6, units = "in")
           dpi = 300, height = 10, width = 7, units = "in")
  }
}

# Resids for time series
plot_ts_resids <- function(save = TRUE, path = tmbfigs) {

  out <- reshape_ts()
  out$ts -> ts
  out$fsh_cpue -> fsh_cpue
  out$srv_cpue -> srv_cpue
  out$mr_plot -> mr_plot
  
  # axis <- tickr(ts, year, 5)
  
  ggplot(ts, aes(x = year, y = catch_sresid)) + 
    geom_hline(yintercept = 0, colour = "grey", size = 1) +
    geom_segment(aes(x = year, xend = year, y = 0, yend = catch_sresid), 
                 size = 0.2, colour = "grey") +
    geom_point() +
    labs(x = "", y = "\n\nCatch\nresiduals") +
    expand_limits(y = c(-3, 3)) +
    # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
    theme(axis.title.y = element_text(angle=0)) -> r_catch
  
  # Fishery cpue resids
  ggplot(fsh_cpue, aes(x = year, y = fsh_cpue_sresid)) + 
    geom_hline(yintercept = 0, colour = "grey", size = 1) +
    geom_segment(aes(x = year, xend = year, y = 0, yend = fsh_cpue_sresid), 
                 size = 0.2, colour = "grey") +
    geom_point() +
    labs(x = "", y = "\n\nFishery CPUE\nresiduals") +
    expand_limits(y = c(-3, 3)) +
    # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
    theme(axis.title.y = element_text(angle=0)) -> r_fsh
  
  # Survey cpues resids
  ggplot(srv_cpue, aes(x = year, y = srv_cpue_sresid)) + 
    geom_hline(yintercept = 0, colour = "grey", size = 1) +
    geom_segment(aes(x = year, xend = year, y = 0, yend = srv_cpue_sresid), 
                 size = 0.2, colour = "grey") +
    geom_point() +
    labs(x = "", y = "\n\nSurvey CPUE\nresiduals") +
    expand_limits(y = c(-3, 3)) +
    # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
    theme(legend.position = "none",
          axis.title.y = element_text(angle=0)) -> r_srv
  
  # Mark-recapture abundance estimate resids
  ggplot(mr_plot, aes(x = year, y = mr_sresid)) + 
    geom_hline(yintercept = 0, colour = "grey", size = 1) +
    geom_segment(aes(x = year, xend = year, y = 0, yend = mr_sresid), 
                 size = 0.2, colour = "grey") +
    geom_point() +
    labs(x = "", y = "\n\nMR abundance\nresiduals\n") +
    expand_limits(y = c(-3, 3)) +
    # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
    theme(axis.title.y = element_text(angle=0)) -> r_mr
  
  plot_grid(r_catch, r_fsh, r_srv, r_mr, ncol = 1, align = 'hv', 
            labels = c('(A)', '(B)', '(C)', '(D)')) -> p
  
  print(p)
  
  if(save == TRUE){ 
    ggsave(plot = p, filename = paste0(path, "/presid_abd_indices.png"), 
           dpi = 300, height = 7, width = 6, units = "in")
  }

}

# Plot derived variables
plot_derived_ts <- function(save = TRUE, 
                            path = tmbfigs, 
                            units = c("imperial", "metric"), 
                            plot_variance = FALSE,
                            ts) {
  
  if(plot_variance == TRUE) {
    
    post_rec <- sum_mcmc$post_rec
    post_expl_abd <- sum_mcmc$post_expl_abd
    post_expl_biom <- sum_mcmc$post_expl_biom
    post_spawn_biom <- sum_mcmc$post_spawn_biom
    
    # Scale numbers to millions
    post_rec <- post_rec %>% mutate_at(c("mean", "median", "q025", "q975"), n2millions)
    post_expl_abd <- post_expl_abd %>% mutate_at(c("mean", "median", "q025", "q975"), n2millions)
    
    if(units == "metric") { # Scale biomass to kt
      post_expl_biom <- post_expl_biom %>% mutate_at(c("mean", "median", "q025", "q975"), n2millions)
      post_spawn_biom <- post_spawn_biom %>% mutate_at(c("mean", "median", "q025", "q975"), n2millions)
    }
    
    if(units == "imperial") { # Scale biomass to million lb
      post_expl_biom <- post_expl_biom %>% mutate_at(c("mean", "median", "q025", "q975"), kg2mlb)
      post_spawn_biom <- post_spawn_biom %>% mutate_at(c("mean", "median", "q025", "q975"), kg2mlb)
    }  
    
    # axis <- tickr(post_expl_abd, year, 5)

    # Recruitment
    ggplot(post_rec, aes(x = year)) +    
      geom_bar(aes(y = median), stat = "identity", fill = "grey") +
      geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.2) +
      labs(x = "", y = "\n\nAge-2\nrecruits\n(millions)") +
      # scale_x_continuous( breaks = axis$breaks, labels = axis$labels) +
      scale_y_continuous(label = scales::comma) +
      expand_limits(y = 0) +
      theme(axis.title.y = element_text(angle = 0)) -> p_rec
    
    # Exploitable abundance (to fishery)
    ggplot(post_expl_abd, aes(x = year)) +
      geom_point(aes(y = median)) +
      geom_line(aes(y = median, group = 1)) +
      geom_ribbon(aes(ymin = q025 , ymax = q975),
                  alpha = 0.2, fill = "black", colour = NA) +
      labs(x = "", y = "\n\nExploitable\nabundance\n(millions)") +
      # scale_x_continuous( breaks = axis$breaks, labels = axis$labels) +
      scale_y_continuous(label = scales::comma) +
      expand_limits(y = 0) +
      theme(axis.title.y = element_text(angle = 0)) -> p_eabd  
    
    # Exploitable biomass (to fishery)
    ggplot(post_expl_biom, aes(x = year)) +
      geom_point(aes(y = median)) +
      geom_line(aes(y = median, group = 1)) +
      geom_ribbon(aes(ymin = q025 , ymax = q975),
                  alpha = 0.2, fill = "black", colour = NA) +
      labs(x = "", y = ifelse(units == "imperial", "\n\nExploitable\nbiomass\n(million lb)", 
                              "\n\nExploitable\nbiomass\n(kt)")) +
      # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
      scale_y_continuous(label = scales::comma) +
      expand_limits(y = 0) +
      theme(axis.title.y = element_text(angle = 0)) -> p_ebiom
    
    # Spawning biomass 
    ggplot(post_spawn_biom, aes(x = year)) +
      geom_point(aes(y = median)) +
      geom_line(aes(y = median, group = 1)) +
      geom_ribbon(aes(ymin = q025 , ymax = q975),
                  alpha = 0.2, fill = "black", colour = NA) +
      labs(x = "", y = ifelse(units == "imperial", "\n\nSpawning\nbiomass\n(million lb)",
                              "\n\nSpawning\nbiomass\n(kt)")) +
      # scale_x_continuous( breaks = axis$breaks, labels = axis$labels) +
      scale_y_continuous(label = scales::comma) +
      expand_limits(y = 0) +
      theme(axis.title.y = element_text(angle = 0)) -> p_sbiom
    
  }
  
  
  if(plot_variance == FALSE) {
    
    # Get recruitment
    
    # log_rbar <- tidyrep %>% filter(Parameter == "log_rbar") %>% pull(Estimate)
    # # log_rec_devs <- tidyrep %>% filter(Parameter == "log_rec_devs") %>% pull(Estimate)
    # log_rec_devs <- tidyrep %>% filter(grepl("log_rec_devs", Parameter)) %>% pull(Estimate)
    # tmp_rec <- exp(log_rbar + log_rec_devs)
    
    sum <- summary(rep, "report") %>%
      as.data.frame()
    
    sum <- rownames_to_column(sum)
    names(sum) <- c("var", "est", "se")
    
    sum <- sum %>%
      mutate(lower = est - 1.96 * se,
             upper = est + 1.96 * se) %>% 
      mutate(est = est / 1e6,
             lower = lower / 1e6,
             upper = upper / 1e6)
    
    rec <- sum %>%
      filter(grepl("rec", var)) %>%
      mutate(year = syr:(lyr),
             year_class = year - 2,
             lower = ifelse(lower < 0, 0, lower)) %>% 
      full_join(data.frame(year = (max(ts$year) + 1):(max(ts$year) + nproj)))
    
    if(units == "metric") {
      ts %>% 
        # Add another year to hold projected values
        full_join(data.frame(year = (max(ts$year) + 1):(max(ts$year) + nproj))) %>%
        # For ts by numbers go divide by 1e6 to get values in millions, for biomass
        # divide by 1e6 to go from kg to kilotons
        mutate(Fmort = c(obj$report(best)$Fmort, rep(NA, nproj)),
               # pred_rec = c(obj$report(best)$pred_rec, rep(NA, nproj)) / 1e6,
               # pred_rec = c(tmp_rec, rep(NA, nproj)) / 1e6,
               biom = obj$report(best)$tot_biom / 1e6, 
               expl_biom = obj$report(best)$tot_expl_biom / 1e6,
               expl_abd = obj$report(best)$tot_expl_abd / 1e6, 
               spawn_biom = obj$report(best)$tot_spawn_biom / 1e6) -> ts
    }
    
    if(units == "imperial") { # ends up reported as million lb
      ts %>% 
        # Add another year to hold projected values
        full_join(data.frame(year = (max(ts$year) + 1):(max(ts$year) + nproj))) %>%
        # For ts by numbers go divide by 1e6 to get values in millions, for biomass
        # divide report in million lb
        mutate(Fmort = c(obj$report(best)$Fmort, rep(NA, nproj)),
               # pred_rec = c(obj$report(best)$pred_rec, rep(NA, nproj)) / 1e6,
               # pred_rec = c(tmp_rec, rep(NA, nproj)) / 1e6,
               biom = obj$report(best)$tot_biom * 2.20462 / 1e6, 
               expl_biom = obj$report(best)$tot_expl_biom * 2.20462 / 1e6,
               expl_abd = obj$report(best)$tot_expl_abd / 1e6, 
               spawn_biom = obj$report(best)$tot_spawn_biom * 2.20462 / 1e6 ) -> ts
    }
    
    # axis <- tickr(ts, year, 5)
    
    p <- ggplot(ts, aes(x = year)) +
      # scale_x_continuous( breaks = axis$breaks, labels = axis$labels)+
      scale_y_continuous(label = scales::comma) +
      expand_limits(y = 0) +
      theme(axis.title.y = element_text(angle=0, vjust = 0.5))
    
    # Recruitment
    # ggplot(ts, aes(year, pred_rec)) +    
    #   geom_bar(stat = "identity") +
    #   # scale_x_continuous( breaks = axis$breaks, labels = axis$labels) +
    #   # theme(axis.title.y = element_text(angle=0)) +
    #   labs(x = "", y = "\n\nAge-2\nrecruits\n(millions)") +
    #   theme(axis.title.y = element_text(angle=0))-> p_rec
    
    # Recruitment
    ggplot(rec, aes(year, est)) +
      geom_bar(stat = "identity", fill = "lightgrey", col = "lightgrey") +
      # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
      labs(x = NULL, y = "Age-2\nrecruits\n(millions)") +
      theme(axis.title.y = element_text(angle=0, vjust = 0.5)) -> p_rec
    
    
    # Exploitable abundance (to fishery)
    p + geom_point(aes(y = expl_abd)) +
      geom_line(aes(y = expl_abd, group = 1)) +
      expand_limits(y = 0) +
      labs(x = NULL, y = "Exploitable\nabundance\n(millions)") -> p_eabd  
    
    # Exploitable biomass (to fishery)
    p + geom_point(aes(y = expl_biom)) +
      geom_line(aes(y = expl_biom, group = 1)) +
      expand_limits(y = 0) +
      labs(x = NULL, y = ifelse(units == "imperial", "Exploitable\nbiomass\n(million lb)", 
                              "Exploitable\nbiomass\n(kt)")) -> p_ebiom
    
    # Spawning biomass 
    p + geom_point(aes(y = spawn_biom)) +
      geom_line(aes(y = spawn_biom, group = 1)) +
      expand_limits(y = 0) +
      labs(x = NULL, y = ifelse(units == "imperial", "Spawning\nbiomass\n(million lb)",
                              "Spawning\nbiomass\n(kt)")) -> p_sbiom
  
    if(save == TRUE){ 
      write_csv(ts %>% select(year, index, Fmort, biom, expl_biom, expl_abd, spawn_biom),
                paste0(path, "/derived_ts_", units, "_", YEAR, ".csv"))
      write_csv(rec,
                paste0(path, "/pred_rec_", YEAR, ".csv"))
    }
  }
  
  plot_grid(p_rec, p_sbiom, p_eabd, p_ebiom, ncol = 1, align = 'hv', 
            labels = c('(A)', '(B)', '(C)', '(D)')) -> p
  
  print(p)
  
  mcmc_flag <- ifelse(plot_variance == TRUE, "_mcmc", "")
  
  if(save == TRUE){ 
    ggsave(plot = p, filename = paste0(path, "/derived_ts_", units, "_", YEAR, mcmc_flag, ".png"), 
           dpi = 300, height = 7, width = 6, units = "in")
  }
  
}

# # Plot derived variables
# plot_derived_ts2 <- function(save = TRUE,
#                             path = tmbfigs,
#                             units = c("imperial", "metric"),
#                             ts) {
# 
#   sum <- summary(rep, "report") %>%
#     as.data.frame()
# 
#   sum <- rownames_to_column(sum)
#   names(sum) <- c("var", "est", "se")
# 
#   sum <- sum %>%
#     mutate(lower = est - 1.96 * se,
#            upper = est + 1.96 * se) %>% 
#     mutate(est = est / 1e6,
#            lower = lower / 1e6,
#            upper = upper / 1e6)
# 
#   rec <- sum %>%
#     filter(grepl("rec", var)) %>%
#     mutate(year = syr:(lyr),
#            year_class = year - 2,
#            lower = ifelse(lower < 0, 0, lower))
#   rec  
#   ssb <- sum %>%
#     filter(grepl("spawn", var)) %>%
#     mutate(year = syr:(lyr+nproj))
#     
#   expl_b <- sum %>%
#     filter(grepl("tot_expl_biom", var)) %>%
#     mutate(year = syr:(lyr+nproj))
#     
#   expl_n <- sum %>%
#     filter(grepl("tot_expl_abd", var)) %>%
#     mutate(year = syr:(lyr+nproj))
#   
#   
#   if(units == "imperial") { # ends up reported as million lb
# 
#     p <- ggplot(ts, aes(x = year)) +
#       scale_y_continuous(label = scales::comma) +
#       expand_limits(y = 0) +
#       theme(axis.title.y = element_text(angle=0, vjust = 0.5))
# 
#     # Recruitment
#     ggplot(rec, aes(year_class, est)) +
#       geom_bar(stat = "identity", fill = "lightgrey", col = "lightgrey") +
#       geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
#       labs(x = "Year class", y = "Recruits\n(millions)") +
#       theme(axis.title.y = element_text(angle=0, vjust = 0.5)) -> p_rec

#     # Exploitable abundance (to fishery)
#     expl_n %>% ggplot(aes(x = year, y = est)) +
#       geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightgrey", col = "lightgrey") +
#       geom_point() +
#       geom_line() +
#       expand_limits(y = 0) +
#       labs(x = "", y = "Exploitable\nabundance\n(millions)") +
#       scale_y_continuous(label = scales::comma) +
#       expand_limits(y = 0) +
#       theme(axis.title.y = element_text(angle=0, vjust = 0.5))-> p_eabd
# 
#     # Exploitable biomass (to fishery)
#     expl_b %>% ggplot(aes(x = year, y = est)) +
#       geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightgrey", col = "lightgrey") +
#       geom_point() +
#       geom_line() +
#       expand_limits(y = 0) +
#       labs(x = "", y = "Exploitable\nbiomass\n(million lb)") +
#       scale_y_continuous(label = scales::comma) +
#       expand_limits(y = 0) +
#       theme(axis.title.y = element_text(angle=0, vjust = 0.5))-> p_ebiom
#     
#     # SSB
#     ssb %>% ggplot(aes(x = year, y = est)) +
#       geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightgrey", col = "lightgrey") +
#       geom_point() +
#       geom_line() +
#       expand_limits(y = 0) +
#       labs(x = "", y = "Spawning\nbiomass\n(million lb)") +
#       scale_y_continuous(label = scales::comma) +
#       expand_limits(y = 0) +
#       theme(axis.title.y = element_text(angle=0, vjust = 0.5))-> p_sbiom
# 
# 
#     if(save == TRUE){
#       write_csv(ts %>% select(year, index, Fmort, pred_rec, biom, expl_biom, expl_abd, spawn_biom),
#                 paste0(path, "/derived_ts_", units, "_", YEAR, ".csv"))
#     }
#   }
# 
#   plot_grid(p_rec, p_sbiom, p_eabd, p_ebiom, ncol = 1, align = 'hv',
#             labels = c('(A)', '(B)', '(C)', '(D)')) -> p
# 
#   print(p)
# 
#   mcmc_flag <- ifelse(plot_variance == TRUE, "_mcmc", "")
# 
#   if(save == TRUE){
#     ggsave(plot = p, filename = paste0(path, "/derived_ts_", units, "_", YEAR, mcmc_flag, ".png"),
#            dpi = 300, height = 7, width = 6, units = "in")
#   }
# 
# }

# Fishing mort
plot_F <- function(save = TRUE, path = tmbfigs) {
  
  ts %>% 
    # Add another year to hold projected values
    full_join(data.frame(year = max(ts$year))) %>%
    mutate(Fmort = c(obj$report(best)$Fmort),
           expl_biom = obj$report(best)$tot_expl_biom[1:nyr] / 1e3,
           exploit = obj$report(best)$pred_catch / expl_biom ) -> ts
           
  # axis <- tickr(ts, year, 5)
  
  ggplot(ts, aes(x = year)) + 
    geom_point(aes(y = Fmort)) +
    geom_line(aes(y = Fmort, group = 1)) +
    # scale_x_continuous( breaks = axis$breaks, labels = axis$labels) +
    labs(x = "", y = "Fishing mortality\n") -> fmort
  
  ggplot(ts, aes(x = year)) + 
    geom_point(aes(y = exploit)) +
    geom_line(aes(y = exploit, group = 1)) +
    # scale_x_continuous( breaks = axis$breaks, labels = axis$labels) +
    labs(x = "", y = "Harvest rate\n") -> hr
  
  plot_grid(fmort, hr,  ncol = 1, align = 'hv') -> p
  
  print(p)
  
  if(save == TRUE){ 
    ggsave(plot = p, filename = paste0(path, "/fishing_mort.png"), 
           dpi = 300, height = 7, width = 6, units = "in")
  }
  
}

reshape_age <- function() {
  
  pred_fsh_age <- as.data.frame(obj$report(best)$pred_fsh_age)
  names(pred_fsh_age) <- as.character(rec_age:plus_group)
  pred_fsh_age %>% 
    mutate(Source = "Fishery",
           index = data$yrs_fsh_age) -> pred_fsh_age
  
  pred_srv_age <- as.data.frame(obj$report(best)$pred_srv_age)
  names(pred_srv_age) <- as.character(rec_age:plus_group)
  pred_srv_age %>% 
    mutate(Source = "Survey",
           index = data$yrs_srv_age) -> pred_srv_age
  
  # Reshape age comp observations and predictions into long format, calculate
  # residuals and prep results for plotting
  age %>% 
    gather("age", "obs", -c("year", "index", "Source", "n", "effn")) %>% 
    left_join(
      bind_rows(pred_fsh_age, pred_srv_age) %>% 
        gather("age", "pred", 1:30),
      by = c("Source", "index", "age")) %>% 
    group_by(Source) %>% 
    mutate(resid = obs - pred,
           # Get standardized residual (mean of 0, sd of 1)
           std_resid = resid / sd(resid),
           # Pearson's residual
           pearson = resid / sqrt(var(pred)),
           # positive or negative
           `obj performance` = ifelse(std_resid >= 0, "Observed greater than estimated",
                                      ifelse(is.na(obs), "",
                                             "Observed less than estimated")),
           Age = factor(age, levels = c("2", "3", "4", "5", "6", "7", "8",
                                        "9", "10", "11", "12", "13", "14", "15",
                                        "16", "17", "18", "19", "20", "21", "22",
                                        "23", "24", "25", "26", "27", "28", "29", "30",
                                        "31"),
                        labels = c("2", "3", "4", "5", "6", "7", "8",
                                   "9", "10", "11", "12", "13", "14", "15",
                                   "16", "17", "18", "19", "20", "21", "22",
                                   "23", "24", "25", "26", "27", "28", "29", "30",
                                   "31+")))  -> agecomps
  
  return(agecomps)
}

reshape_len <- function() {
  
  if(nsex == 1) {
    pred_srv_len <- as.data.frame(obj$report(best)$pred_srv_len[,,1]) %>% 
      mutate(Sex = "Sexes combined") 
  } else {
    pred_srv_len <- as.data.frame(obj$report(best)$pred_srv_len[,,1]) %>% 
      mutate(Sex = "Male") %>% 
      bind_rows(as.data.frame(obj$report(best)$pred_srv_len[,,2]) %>% 
                  mutate(Sex = "Female"))               
  }
  names(pred_srv_len) <- c(as.character(data$lenbin), "Sex")
  pred_srv_len %>% 
    mutate(Source = "Survey",
           index = rep(data$yrs_srv_len, nsex)) -> pred_srv_len
  
  if(nsex == 1) {
    pred_fsh_len <- as.data.frame(obj$report(best)$pred_fsh_len[,,1]) %>% 
      mutate(Sex = "Sexes combined") 
  } else {
    pred_fsh_len <- as.data.frame(obj$report(best)$pred_fsh_len[,,1]) %>% 
      mutate(Sex = "Male") %>% 
      bind_rows(as.data.frame(obj$report(best)$pred_fsh_len[,,2]) %>% 
                  mutate(Sex = "Female"))               
  }
  names(pred_fsh_len) <- c(as.character(data$lenbin), "Sex")
  pred_fsh_len %>% 
    mutate(Source = "Fishery",
           index = rep(data$yrs_fsh_len, nsex)) -> pred_fsh_len
  
  bind_rows(pred_srv_len, pred_fsh_len) -> pred_len
  
  # residuals and prep results for plotting
  if(nsex == 1) {
    len <- len %>% filter(Sex == "Sex combined") %>% droplevels()
  } else {
    len <- len %>% filter(Sex != "Sex combined") %>% droplevels()
  }
  len %>% 
    #filter(Source == "srv_len") %>% # FLAG - temporary until we incorporate fishery len comps into the model
    rename(obs = proportion) %>% 
    mutate(length_bin = as.character(length_bin),
           Source = factor(Source, levels = c("srv_len", "fsh_len"),
                           label = c("Survey", "Fishery"))) %>% 
    left_join(pred_len %>% gather("length_bin", "pred", 1:data$nlenbin)) %>% 
    group_by(Source, Sex) %>% 
    mutate(resid = obs - pred,
           # Get standardized residual (mean of 0, sd of 1)
           std_resid = resid / sd(resid),
           # Pearson's residual
           pearson = resid / sqrt(var(pred)),
           # positive or negative
           `obj performance` = ifelse(std_resid >= 0, "Observed greater than estimated",
                                      ifelse(is.na(obs), "",
                                             "Observed less than estimated")),
           length_bin = factor(length_bin, levels = c("41", "43", "45", "47", "49", "51", "53", "55", "57", "59", "61",
                                                      "63", "65", "67", "69", "71", "73", "75", "77", "79", "81", "83",
                                                      "85", "87", "89", "91", "93", "95", "97", "99"),
                        labels = c("41", "43", "45", "47", "49", "51", "53", "55", "57", "59", "61",
                                   "63", "65", "67", "69", "71", "73", "75", "77", "79", "81", "83",
                                   "85", "87", "89", "91", "93", "95", "97", "99+")))  -> lencomps
  
  return(lencomps)
}

# Labels for ages
get_age_labs <- function() {
  age_labs <- c("2", "", "", "", "6", "", "", "", "10", "", "", "", "14", "",
                "", "", "18", "", "", "", "22", "", "", "", "26", "",
                "", "", "30", "") 
  return(age_labs)
}

# Labels for length bins
get_len_labs <- function() {
  len_labs <- c("", "", "45", "", "", "", "", "55", "", "", "",
                "", "65", "", "", "", "", "75", "", "", "", "",
                "85", "", "", "", "", "95", "", "") 
  return(len_labs)
}

# Age comp resids
plot_age_resids <- function(save = TRUE, path = tmbfigs) {
  
  age_labs <- get_age_labs()
  
  # axis <- tickr(agecomps, year, 5)
  
  ggplot(agecomps, aes(x = year, y = Age, size = std_resid,
                       fill = `obj performance`)) + 
    geom_point(shape = 21, colour = "black") +
    scale_size(range = c(0, 3.5)) +
    facet_wrap(~ Source) +
    labs(x = '\nYear', y = 'Age\n', fill = NULL) +
    guides(size = FALSE) +
    scale_fill_manual(values = c("white", "black")) +
    scale_y_discrete(breaks = unique(agecomps$Age), labels = age_labs) +
    # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
    theme(legend.position = "bottom",
          strip.text.x = element_text(size = 11, colour = "black"),
          strip.text.y = element_text(size = 11, colour = "black")) -> p
  
  print(p)
  
  if(save == TRUE){ 
    ggsave(plot = p, filename = paste0(path, "/agecomps_residplot.png"), dpi = 300, height = 6, width = 7, units = "in")
  }
}

# Length comp resids
plot_len_resids <- function(save = TRUE, path = tmbfigs) {
  
  len_labs <- get_len_labs()
  
  # axis <- tickr(lencomps, year, 5)
  
  ggplot(lencomps, aes(x = year, y = length_bin, size = std_resid,
                       fill = `obj performance`)) + 
    geom_point(shape = 21, colour = "black") +
    scale_size(range = c(0, 3)) +
    facet_grid(Source ~ Sex) +
    labs(x = '\nYear', y = 'Length (cm)\n', fill = NULL) +
    guides(size = FALSE) +
    scale_fill_manual(values = c("white", "black")) +
    scale_y_discrete(breaks = unique(lencomps$length_bin), labels = len_labs) +
    # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
    theme(legend.position = "bottom",
          strip.text.x = element_text(size = 11, colour = "black"),
          strip.text.y = element_text(size = 11, colour = "black")) -> p
  
  print(p)
  
  if(save == TRUE){ 
    ggsave(plot = p, filename = paste0(path, "/lencomps_residplot.png"), dpi = 300, height = 6, width = 7, units = "in")
  }
}

barplot_age <- function(src = "Survey", save = TRUE, path = tmbfigs) {
  
  age_labs <- get_age_labs()
  
  ggplot(agecomps %>% filter(Source == src )) + #& year >= 2003
    geom_bar(aes(x = Age, y = obs), 
             stat = "identity", colour = "grey", fill = "lightgrey",
             width = 0.8, position = position_dodge(width = 0.5)) +
    geom_line(aes(x = Age, y = pred, group = 1), size = 0.6) +
    facet_wrap(~ year, dir = "v", ncol = 5) +
    scale_x_discrete(breaks = unique(agecomps$Age), labels = age_labs) +
    labs(x = '\nAge', y = 'Proportion-at-age\n') +
    ggtitle(paste0(src)) -> p
  
  print(p)
  
  if(save == TRUE){ 
    ggsave(plot = p, filename = paste0(path, "/", src, "_agecomps_barplot.png"), dpi = 300, height = 6, width = 7, units = "in")
  }
  
}

barplot_len <- function(src = "Survey", sex = "Female", save = TRUE, path = tmbfigs) {
  
  len_labs <- get_len_labs()
  
  ggplot(lencomps %>% filter(Source == src & Sex == sex)) + #, year >= 2003
    geom_bar(aes(x = length_bin, y = obs), 
             stat = "identity", colour = "grey", fill = "lightgrey",
             width = 0.8, position = position_dodge(width = 0.5)) +
    geom_line(aes(x = length_bin, y = pred, group = 1), size = 0.6) +
    facet_wrap(~ year, dir = "v", ncol = 5) + 
    scale_x_discrete(breaks = unique(lencomps$length_bin), labels = len_labs) +
    labs(x = '\nLength (cm)', y = 'Proportion-at-length\n') +
    ggtitle(paste0(src, " (", sex, ")")) -> p
  
  print(p)
  
  if(save == TRUE){ 
    ggsave(plot = p, filename = paste0(path, "/", src, "_", sex, "_lencomps_barplot.png"), dpi = 300, height = 6, width = 7, units = "in")
  }
}

plot_sel <- function(save = TRUE, path = tmbfigs) {
  
  require(data.table)
  
  # Extract selectivity matrices and convert to dfs and create a second index col
  # as a dummy var (must supply an interval to foverlaps). Set as data.table
  # object so it is searchable
  if(nsex == 1) {
    sel <- obj$report(best)$fsh_slx[,,1] %>% as.data.frame() %>% 
      mutate(Selectivity = "Fishery", Sex = "Sexes combined") %>% 
      bind_rows(obj$report(best)$srv_slx[,,1] %>% as.data.frame() %>% 
                  mutate(Selectivity = "Survey", Sex = "Sexes combined"))
  } else { # Sex-structured
    sel <- obj$report(best)$fsh_slx[,,1] %>% as.data.frame() %>% 
      mutate(Selectivity = "Fishery", Sex = "Male") %>% 
      bind_rows(obj$report(best)$fsh_slx[,,2] %>% as.data.frame() %>% 
                  mutate(Selectivity = "Fishery", Sex = "Female")) %>% 
      bind_rows(obj$report(best)$srv_slx[,,1] %>% as.data.frame() %>% 
                  mutate(Selectivity = "Survey", Sex = "Male")) %>% 
      bind_rows(obj$report(best)$srv_slx[,,2] %>% as.data.frame() %>% 
                  mutate(Selectivity = "Survey", Sex = "Female"))}
  
  names(sel) <- c(unique(agecomps$age), "Selectivity", "Sex")
  
  sel <- sel %>% 
    mutate(year = rep(ts$year[1:nyr], 2*nsex)) %>% 
    gather("Age", "proportion", -c(year, Selectivity, Sex)) %>% 
    mutate(year2 = year) # needed for foverlaps()
  
  setDT(sel)
  
  # Look up table for selectivity time blocks
  blks_sel <- data.frame(Selectivity = c(rep("Fishery", length(data$fsh_blks)),
                                         rep("Survey", length(data$srv_blks))),
                         end = c(data$fsh_blks, data$srv_blks)) %>%
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
  
  if(save == TRUE) {write_csv(sel, paste0(path, "/selectivity_", YEAR, ".csv"))}
  
  sel <- sel %>% 
    mutate(`Time blocks` = paste0(start_year, "-", end_year),
           age = as.numeric(Age)) %>% 
    filter(age <= 15)
  
  # axis <- tickr(sel, age, 3)
  
  ggplot(sel %>% filter(Selectivity == "Fishery"), 
         aes(x = age, y = proportion, colour = Sex, 
                  shape = Sex, lty = Sex, group = Sex)) +
    geom_point() +
    geom_line() +
    facet_wrap(~`Time blocks`, ncol = 1) +
    scale_colour_grey() +
    # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
    labs(#y = "Proportion retained\n", 
      x = NULL, y = NULL,
      colour = NULL, lty = NULL, shape = NULL,
      title = "Fishery selectivity") +
    theme(legend.position = c(.85, .15)) -> fsh_sel
  
  
  ggplot(sel %>% filter(Selectivity == "Survey"), 
         aes(x = age, y = proportion, colour = Sex, 
             shape = Sex, lty = Sex, group = Sex)) +
    geom_point() +
    geom_line() +
    facet_grid(~`Time blocks`) +
    scale_colour_grey() +
    # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
    labs(#y = "Proportion retained\n", 
      x = NULL, y = NULL,
      colour = NULL, lty = NULL, shape = NULL,
      title = "Survey selectivity") +
    theme(legend.position = c(.85, .2)) -> srv_sel
  
  
  plot_grid(fsh_sel, srv_sel, ncol = 1, rel_heights = c(2/3, 1/3)) -> p
  
  print(p)
  
  if(save == TRUE){ 
    ggsave(plot = p, filename = paste0(path, "/selectivity_", YEAR, ".png"), 
           dpi = 300, height = 7, width = 6, units = "in")
  }
  # ggsave("selectivity.png", dpi = 300, height = 4, width = 6, units = "in")
  
}

