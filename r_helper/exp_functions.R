###############################################################################
## This is my code for developing and building on the original sablefish model 
## Author: Phil Joy philip.joy@alaska.gov
## March 2023
###############################################################################

build_bounds_exp <- function(param_list = NULL, data_list){
  
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
  upper_bnd$log_tau_fsh <- replace(upper_bnd$log_tau_fsh, values = 4) 
  
  lower_bnd$log_tau_srv <- replace(lower_bnd$log_tau_srv, values = -20) 
  upper_bnd$log_tau_srv <- replace(upper_bnd$log_tau_srv, values = 4) 
  
  lower_bnd$log_tau_mr <- replace(lower_bnd$log_tau_mr, values = -20) 
  upper_bnd$log_tau_mr <- replace(upper_bnd$log_tau_mr, values = 4) 
  
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

TMBphase_exp <- function(data, parameters, random, model_name, phase = FALSE,
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
      map_use$log_fsh_l_theta <- fill_vals(parameters$log_fsh_l_theta, NA)  #add these back in when full dirichlet developed... 
      map_use$log_srv_l_theta <- fill_vals(parameters$log_srv_l_theta, NA)
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
      #estimate selectivity in 3rd time block... turn NA's back to values... 
      
      #map_use$log_fsh_slx_pars <- factor(c(NA,NA,1,NA,NA,2,
      #                                     NA,NA,3,NA,NA,4
      #                                     ))
      #dim(map_use$log_fsh_slx_pars)<-dim(parameters$log_fsh_slx_pars)
      #map_use$log_srv_slx_pars <- fill_vals(parameters$log_srv_slx_pars, NA)

    }
    
    # Build upper and lower parameter bounds and remove any that are not
    # estimated (should be the inverse of the map_use)
    bounds <- build_bounds_exp(param_list = parameters)
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
          map_use$log_fsh_l_theta <- fill_vals(parameters$log_fsh_l_theta, NA)
          map_use$log_srv_l_theta <- fill_vals(parameters$log_srv_l_theta, NA)
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


build_parameters_exp <- function(
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
        
        # FISHERY SELECTIVITY FOR 2022 assessment (2023 ABC)
      } else {  # Logistic with a50 and slope, data$slx_type = 1, sex-structured model - UPDATE ME!
        array(data = c(slx_pars$log_a50[slx_pars$fleet == "fsh_t1_m"], # male a50s time block 1
                       slx_pars$log_a50[slx_pars$fleet == "fsh_t2_m"], #  then time block 2
                       slx_pars$log_a50[slx_pars$fleet == "fsh_t3_m"], #  then time block 3
                       slx_pars$log_k[slx_pars$fleet == "fsh_t1_m"], # male slopes time block 1
                       slx_pars$log_k[slx_pars$fleet == "fsh_t2_m"], # then time block 2
                       slx_pars$log_k[slx_pars$fleet == "fsh_t3_m"], # then time block 3
                       slx_pars$log_a50[slx_pars$fleet == "fsh_t1_f"], # female a50s time block 1
                       slx_pars$log_a50[slx_pars$fleet == "fsh_t2_f"], #  then time block 2
                       slx_pars$log_a50[slx_pars$fleet == "fsh_t3_f"], #  then time block 3
                       slx_pars$log_k[slx_pars$fleet == "fsh_t1_f"], # female slopes time block 1
                       slx_pars$log_k[slx_pars$fleet == "fsh_t2_f"], #  then time block 2
                       slx_pars$log_k[slx_pars$fleet == "fsh_t3_f"]), # then time block 3
              dim = c(length(data$fsh_blks), 2, nsex)) }, # 2 = npar for this slx_type   
        
        # FISHERY SELECTIVITY FOR 2020 assessment (2021 ABC)
   #   } else {  # Logistic with a50 and slope, data$slx_type = 1, sex-structured model - UPDATE ME!
  #      array(data = c(slx_pars$log_a50[slx_pars$fleet == "fsh_t1_m"], # male a50s time block 1
  #                     slx_pars$log_a50[slx_pars$fleet == "fsh_t2_m"], #  then time block 2
  #                     slx_pars$log_k[slx_pars$fleet == "fsh_t1_m"], # male slopes time block 1
  #                     slx_pars$log_k[slx_pars$fleet == "fsh_t2_m"], # then time block 2
  #                     slx_pars$log_a50[slx_pars$fleet == "fsh_t1_f"], # female a50s time block 1
  #                     slx_pars$log_a50[slx_pars$fleet == "fsh_t2_f"], #  then time block 2
  #                     slx_pars$log_k[slx_pars$fleet == "fsh_t1_f"], # female slopes time block 1
  #                     slx_pars$log_k[slx_pars$fleet == "fsh_t2_f"]), # then time block 2
  #            dim = c(length(data$fsh_blks), 2, nsex)) }, # 2 = npar for this slx_type
    
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
        
        
        # SURVEY SELECTIVITY FOR 2022 assessment (2023 ABC)
        # Logistic with a50 and slope, data$slx_type = 1, sex-structured model UPDATE ME!
      } else {
        array(data = c(rep(slx_pars$log_a50[slx_pars$fleet == "srv_m"], length(data$srv_blks)), # male a50
                       rep(slx_pars$log_k[slx_pars$fleet == "srv_m"], length(data$srv_blks)), # slope
                       rep(slx_pars$log_a50[slx_pars$fleet == "srv_f"], length(data$srv_blks)), # female a50
                       rep(slx_pars$log_k[slx_pars$fleet == "srv_f"], length(data$srv_blks))), # slope
              dim = c(length(data$srv_blks), 2, nsex))}, # 2 = npar for this slx_type
          # SURVEY SELECTIVITY FOR 2020 assessment (2021 ABC)
        # Logistic with a50 and slope, data$slx_type = 1, sex-structured model UPDATE ME!
      #} else {
      #  array(data = c(rep(slx_pars$log_a50[slx_pars$fleet == "srv_m"], length(data$srv_blks)), # male a50
      #                 rep(slx_pars$log_k[slx_pars$fleet == "srv_m"], length(data$srv_blks)), # slope
      #                 rep(slx_pars$log_a50[slx_pars$fleet == "srv_f"], length(data$srv_blks)), # female a50
      #                 rep(slx_pars$log_k[slx_pars$fleet == "srv_f"], length(data$srv_blks))), # slope
      #        dim = c(length(data$srv_blks), 2, nsex))}, # 2 = npar for this slx_type
    
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
    log_srv_theta = log(10),
    log_fsh_l_theta = c(log(5),log(5)),
    log_srv_l_theta = c(log(5),log(5)),
    
    #extra variance for index terms
    log_tau_fsh = log(0.001),
    log_tau_srv = log(0.001),
    log_tau_mr = log(0.001) #log(1.1)
  )
  
  return(parameters)
}

#for model experimenting
build_data_exp <- function(weights = FALSE,
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
    ev_type = ev_type,
    
    # Time varying parameters - each vector contains the terminal years of each time block
    #fsh_blks = c(ts %>% filter(year == 1994) %>% pull(index), max(ts$index)), #  fishery selectivity: limited entry in 1985, EQS in 1994 = c(5, 14, max(ts$year))
    fsh_blks = c(ts %>% filter(year == 1994 | year == 2015) %>% pull(index), 
                 max(ts$index)),  # 3 time blocks as per current federal (2022) assessment
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
    #p_fsh_q = c(exp(-16), exp(-16)),
    #sigma_fsh_q = c(1, 1),
    p_fsh_q = c(exp(-16), exp(-16), exp(-16)),
    sigma_fsh_q = c(1, 1, 1),
    p_srv_q = exp(-17), 
    sigma_srv_q = 1,
    p_mr_q = 1.0,
    sigma_mr_q = 0.01, #0.01,
    
    # Weights on likelihood components ("wt_" denotes weight) based on weights
    # in Federal model
    
    wt_catch = if (weights == TRUE) {wt_catch = 10.0} else {wt_catch = 1.0},
    wt_fsh_cpue = if (weights == TRUE) {wt_fsh_cpue = 1.0} else {wt_fsh_cpue = 1.0},
    wt_srv_cpue = if (weights == TRUE) {wt_srv_cpue = 4.0} else {wt_srv_cpue = 1.0},
    wt_mr = if (weights == TRUE) {wt_mr = 1.5} else {wt_mr = 1.0},
    wt_fsh_age = if (weights == TRUE) {wt_fsh_age = 6.0} else {wt_fsh_age = 1.0},
    wt_srv_age = if (weights == TRUE) {wt_srv_age = 8.0} else {wt_srv_age = 1.0},
    wt_fsh_len = 1.0, 
    wt_srv_len = 1.0, 
    wt_rec_like = 1.0,
    wt_fpen = 0.1, 
    wt_spr = 100,
    
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

#-------------------------------------------------------------------------------
# Model Tuning function for retrospective
# This will tune the last model run and then rerun the model with the ess and output
# those model results
#model will use data and parameters as currently set... 

tune_it <-function(niter=1,modelname="scaa_mod_dir_ev",newtonsteps=newtonsteps){
  tune_fsh_age <- list()
  tune_srv_age <- list()
  tune_fsh_len <- list()
  tune_srv_len <- list()
  
  for(iter in 1:niter) { #iter<-1
    
    # MLE, phased estimation (phase = TRUE) or not (phase = FALSE)
    out <- TMBphase_exp(data, parameters, random = random_vars, 
                        model_name = modelname, phase = FALSE, 
                        newtonsteps = newtonsteps, #3 make this zero initially for faster run times (using 5)
                        debug = FALSE)
    
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
  
  age <- age %>% 
    mutate(effn = ifelse(age$Source == "Survey", tune_srv_age[niter,], tune_fsh_age[niter,]))  # tuned age comps - see tune_comps.R for prelim work on tuning comps using McAllister/Ianelli method
  len <- fsh_len %>% 
    mutate(effn = ifelse(fsh_len$Sex == "Male", tune_fsh_len[niter,1], tune_fsh_len[niter,2])) %>% 
    bind_rows(srv_len %>% 
                mutate(effn = ifelse(srv_len$Sex == "Male", tune_srv_len[niter,1], tune_srv_len[niter,2])))  # tuned len comps
  
  fsh_age <- filter(age, Source == "Fishery")
  srv_age <- filter(age, Source == "Survey")
  fsh_len <- filter(len, Source == "fsh_len")
  srv_len <- filter(len, Source == "srv_len")
  
  #OK, now run the model with tuned comps
  data <- build_data_exp(ts = ts, weights = TRUE)
  
  parameters <- build_parameters_exp(rec_devs_inits = rec_devs_inits, Fdevs_inits = Fdevs_inits)
  random_vars <- build_random_vars() # random effects still in development
  
  out <- TMBphase(data, parameters, random = random_vars, 
                  model_name = "scaa_mod", phase = FALSE, 
                  debug = FALSE)
  
  return(out)
}

















