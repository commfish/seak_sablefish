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

#========================================================================================
#TMBphase <- function(data, parameters, random, model_name, phase = FALSE,
#                     optimizer = "nlminb", debug = FALSE, loopnum = 3, newtonsteps = 0) {
  
  # Debug function
  # random <-  random_vars# <- NULL
  # phases <- build_phases(parameters, data)
  # model_name <- "scaa_mod"
  # debug <- FALSE
#TMBphase(data, parameters, random = random_vars, 
#         model_name = "scaa_mod", phase = FALSE, 
#         newtonsteps = 3, #3 make this zero initially for faster run times
#         debug = FALSE)


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
#}
