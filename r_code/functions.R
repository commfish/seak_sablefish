# my user-defined functions for the sablefish assessment
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2017-10-19

# includes functions for: 
# -  estimating length- and weight- based von bertalanffy using maximum
# likelihood estimation
# - getting sex ratios by age or year (or both)
# - a modification to the captioner library's captioner() fxn

source("r_code/helper.R")

if(!require("broom"))   install.packages("broom") # tidy() useful for tidying mle() output
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
  
  results <- list(predictions = data.frame(obs_weight = obs_weight,
                                           age = age, pred = pred, 
                                           resid = resids, Sex = sex),
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

# captions

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
