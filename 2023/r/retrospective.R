# Retrospective analysis
# jane.sullivan@noaa.gov
# Last updated May 2021

# Set up ----

source("r_helper/helper.r")
source("r_helper/functions.r")

YEAR <- 2022 # most recent year of data

# Directory setup
root <- getwd() # project root
tmb_dat <- file.path(root, paste0(YEAR+1,"/data/tmb_inputs")) # location of tmb model data inputs
tmb_path <- file.path(root, paste0(YEAR+1,"/tmb")) # location of cpp
tmbout <- file.path(root, paste0(YEAR+1,"/output/tmb")) # location where model output is saved
retro_dir <- file.path(tmbout, paste0("retrospective")) # subdirectory for analysis
dir.create(retro_dir, showWarnings = FALSE)

#source("r_helper/helper.r")
#source("r_helper/functions.r")

library(TMB) 

VER<-"tuned"
# Data for SCAA
ts <- read_csv(paste0(tmb_dat, "/abd_indices_", YEAR, ".csv"))        # time series
#ts <- read_csv(paste0(tmb_dat, "/abd_indices_CPUEsense_", YEAR, ".csv"))        # time series
#age <- read_csv(paste0(tmb_dat, "/agecomps_", YEAR, ".csv"))          # age comps
#len <- read_csv(paste0(tmb_dat, "/lencomps_", YEAR, ".csv"))          # len comps
age <- read_csv(paste0(tmb_dat, "/tuned_agecomps_", YEAR, ".csv"))  # tuned age comps - see tune_comps.R for prelim work on tuning comps using McAllister/Ianelli method
len <- read_csv(paste0(tmb_dat, "/tuned_lencomps_", YEAR, ".csv"))  # tuned len comps
bio <- read_csv(paste0(tmb_dat, "/maturity_sexratio_", YEAR, ".csv")) # proportion mature and proportion-at-age in the survey
waa <- read_csv(paste0(tmb_dat, "/waa_", YEAR, ".csv"))               # weight-at-age
retention <- read_csv(paste0(tmb_dat, "/retention_probs.csv"))        # retention probability (not currently updated annually. saved from ypr.r)
slx_pars <- read_csv("legacy_data/tmb_inputs/fed_selectivity_transformed_2020.csv") # fed slx transformed to ages 0:29 instead of ages 2:31. see scaa_datprep.R for more info

# Ageing error transition matrix from D. Hanselman 2019-04-18. On To Do list to
# develop one for ADFG. Row = true age, Column = observed age. Proportion
# observed at age given true age.
ageing_error <- scan("legacy_data/tmb_inputs/ageing_error_fed.txt", sep = " ") %>% matrix(ncol = 30) %>% t()
rowSums(ageing_error) # should be 1

# Age-length key from D. Hanselman 2019-04-18. On To DO list to develop one for
# ADFG (will need separate onces for fishery and survey).  Proportion at length
# given age. Row = age, Column = length bin
agelen_key_m <- scan("legacy_data/tmb_inputs/agelen_key_male.txt", sep = " ", skip = 1) %>% matrix(ncol = 30) %>% t()
rowSums(agelen_key_m) # should all = 1
agelen_key_f <- scan("legacy_data/tmb_inputs/agelen_key_fem.txt", sep = " ", skip = 1) %>% matrix(ncol = 30) %>% t()
rowSums(agelen_key_f) 

# Starting values - base on current assessment's mle
# pick saved model results to work with...
model_run <-"" #use this if just running last model that was not saved in a separate folder
model_run <- "/first_run_base"
model_run <- "/first_tuned_run"

inits <- read_csv(paste0(tmbout,model_run, "/tmb_allparams_mle_", YEAR, ".csv"))
rec_devs_inits <- inits %>% filter(grepl("rec_devs", Parameter)) %>% pull(Estimate)
rinit_devs_inits <- inits %>% filter(grepl("rinit_devs", Parameter)) %>% pull(Estimate)
Fdevs_inits <- inits %>% filter(grepl("F_devs", Parameter)) %>% pull(Estimate)

# Model dimensions / user inputs
syr <- min(ts$year)                       # model start year
rec_age <- min(waa$age)                   # recruitment age                  
plus_group <- max(waa$age)                # plus group age
nage <- length(rec_age:plus_group)        # number of ages
nlenbin <- length(unique(len$length_bin)) # number of length bins
nsex <- 2                 # single sex or sex-structured
nproj <- 1                # projection years *FLAG* eventually add to cpp file, currently just for graphics
include_discards <- TRUE  # include discard mortality, TRUE or FALSE
tmp_debug <- TRUE         # Shuts off estimation of selectivity pars - once selectivity can be estimated, turn to FALSE

# Model switches
rec_type <- 0     # Recruitment: 0 = penalized likelihood (fixed sigma_r), 1 = random effects
slx_type <- 1     # Selectivity: 0 = a50, a95 logistic; 1 = a50, slope logistic
comp_type <- 0    # Age comp likelihood (not currently developed for len comps): 0 = multinomial, 1 = Dirichlet-multinomial
spr_rec_type <- 1 # SPR equilbrium recruitment: 0 = arithmetic mean, 1 = geometric mean, 2 = median (not coded yet)
M_type <- 0       # Natural mortality: 0 = fixed, 1 = estimated with a prior

# Retrospective ----

# Store output from each restrospective peel:
rec_ls <- list()        # recruitment time series
SB_ls <- list()         # spawning stock biomass
Fmort_ls <- list()      # fishing mortality rate (Ft)
ABC_ls <- list()        # abc forecast
SB100_ls <- list()      # unfished equilibrium spawning biomass
SB50_ls <- list()       # equilibrium spawning biomass at F50
if(rec_type == 1){
  sigmaR_ls <- list()     # sigma_R (estimated used random effects when rec_type = 1)
}
mgc_ls <- list()        # max gradient component

retro <- 0:10           # number of peels

for(i in 1:length(retro)){  #i<-1
  
  iter_dir <- file.path(retro_dir, paste0("retro_", retro[i]))
  dir.create(iter_dir, showWarnings = FALSE)
  
  lyr <- YEAR - retro[i]     # end year
  nyr <- length(syr:lyr)     # number of years 
  
  # Subsets for make_data()
  iter_ts <- filter(ts, year <= lyr)
  mr <- filter(iter_ts, !is.na(mr))
  fsh_cpue <- filter(iter_ts, !is.na(fsh_cpue))
  srv_cpue <- filter(iter_ts, !is.na(srv_cpue))
  fsh_age <- age %>% filter(year <= lyr & Source == "Fishery")
  srv_age <- age %>% filter(year <= lyr & Source == "Survey")
  fsh_len <- len %>% filter(year <= lyr & Source == "fsh_len")
  srv_len <- len %>% filter(year <= lyr & Source == "srv_len")
  
  # Starting values
  iter_rec_devs_inits <- rep(0, nyr) 
  iter_Fdevs_inits <- rep(0, nyr)   
  
  # Build TMB objects
  data <- build_data(ts = iter_ts)
  #change out fishery CPUE choice...
  data$data_fsh_cpue<-ts$fsh_cpue[!is.na(ts$fsh_cpue)]
  
  parameters <- build_parameters(rec_devs_inits = iter_rec_devs_inits, 
                                 Fdevs_inits = iter_Fdevs_inits)
  random_vars <- build_random_vars()
  
  # Run model using MLE
  setwd(tmb_path)
  out <- TMBphase(data, parameters, random = random_vars, 
                  model_name = "scaa_mod", phase = FALSE, 
                  debug = FALSE)
  
  obj <- out$obj # TMB model object
  opt <- out$opt # fit
  rep <- out$rep # sdreport
  best <- obj$env$last.par.best
  
  # save MLE estimates
  tidyrep <- save_mle(path = iter_dir, save_inits = FALSE, year = lyr, save=TRUE)
 
  # recruitment in millions
  log_rbar <- tidyrep %>% filter(Parameter == "log_rbar") %>% pull(Estimate)
  log_rec_devs <- tidyrep %>% filter(grepl("log_rec_devs", Parameter)) %>% pull(Estimate)
  rec_ls[[i]] <- data.frame(year = syr:lyr,
                            rec = exp(log_rbar + log_rec_devs) / 1e6,
                            retro = paste0("retro_", retro[i]))    
  
  # spawning stock biomass in million lb
  SB_ls[[i]] <- data.frame(year = syr:(lyr+1),
                           spawn_biom =  obj$report(best)$tot_spawn_biom * 2.20462 / 1e6,
                           retro = paste0("retro_", retro[i]))

  # annual fishing mortality rate (Ft)
  Fmort_ls[[i]] <- data.frame(year = syr:lyr,
                              Fmort = obj$report(best)$Fmort,
                              retro = paste0("retro_", retro[i]))  # fishing mortality rate (Ft)

  # ABC over time (current estimate of F50 imposed on all years) in million lb
  ABC <- as.data.frame(obj$report(best)$ABC * 2.20462 / 1e6)
  names(ABC) <- data$Fxx_levels
  ABC_ls[[i]] <- ABC %>% 
    mutate(year = syr:(lyr+1)) %>% 
    tidyr::pivot_longer(cols = -year, names_to = "Fxx", values_to = "ABC") %>% 
    # data.table::melt(id.vars = c("year"), variable.name = "Fxx", value.name = "ABC") %>% 
    filter(Fxx == "0.5") %>% 
    mutate(retro = paste0("retro_", retro[i])) %>% 
    select(-Fxx)
  
  # equilibrium unfished and fished spawning biomass in million lb
  SB <- as.data.frame(obj$report(best)$SB * 2.20462 / 1e6)
  names(SB) <- "SB"
  SB <- SB %>% mutate(Fspr = c(0.0, data$Fxx_levels))
  SB100_ls[[i]] <- SB %>% 
    filter(Fspr == 0) %>% 
    select(-Fspr) %>% 
    mutate(retro = paste0("retro_", retro[i]))
  SB50_ls[[i]] <- SB %>% 
    filter(Fspr == 0.5) %>% 
    select(-Fspr) %>% 
    mutate(retro = paste0("retro_", retro[i]))
  
  # sigma_R (estimated used random effects when rec_type = 1)
  if(rec_type == 1){
    sigmaR_ls[[i]] <- data.frame(sigmaR = tidyrep %>% 
                                   filter(Parameter == "log_sigma_r") %>% 
                                   pull(Estimate) %>% 
                                   exp(),
                                 retro = paste0("retro_", retro[i]))
  }
  
  # check on max gradient component
  mgc_ls[[i]] <- data.frame(retro = paste0("retro_", retro[i]),
                                    mgc = max(rep$gradient.fixed))  
}
  
rec <- do.call(rbind, rec_ls)
SB <- do.call(rbind, SB_ls)
Fmort <- do.call(rbind, Fmort_ls)
ABC <- do.call(rbind, ABC_ls)
SB100 <- do.call(rbind, SB100_ls)
SB50 <- do.call(rbind, SB50_ls)
if(rec_type == 1){
  sigmaR <- do.call(rbind, sigmaR_ls)
}
mgc <- do.call(rbind, mgc_ls)

# Save objects so you don't have to rerun analysis ever time.
write_csv(rec, paste0(retro_dir, "/retro_recruitment_",VER,"_", YEAR, ".csv"))
write_csv(SB, paste0(retro_dir, "/retro_SB_",VER,"_", YEAR, ".csv"))
write_csv(Fmort, paste0(retro_dir, "/retro_Fmort_",VER,"_", YEAR, ".csv"))
write_csv(ABC, paste0(retro_dir, "/retro_ABC_",VER,"_", YEAR, ".csv"))
write_csv(SB100, paste0(retro_dir, "/retro_SB100_",VER,"_", YEAR, ".csv"))
write_csv(SB50, paste0(retro_dir, "/retro_SB50_",VER,"_", YEAR, ".csv"))
if(rec_type == 1){
  write_csv(sigmaR, paste0(retro_dir, "/retro_sigmaR_",VER,"_", YEAR, ".csv"))
}
write_csv(mgc, paste0(retro_dir, "/retro_convergence_",VER,"_", YEAR, ".csv"))

# Load ----
rec <- read_csv(paste0(retro_dir, "/retro_recruitment_",VER,"_", YEAR, ".csv"))
SB <- read_csv(paste0(retro_dir, "/retro_SB_",VER,"_", YEAR, ".csv"))
Fmort <- read_csv(paste0(retro_dir, "/retro_Fmort_",VER,"_", YEAR, ".csv"))
ABC <- read_csv(paste0(retro_dir, "/retro_ABC_",VER,"_", YEAR, ".csv"))
SB100 <- read_csv(paste0(retro_dir, "/retro_SB100_",VER,"_", YEAR, ".csv"))
SB50 <- read_csv(paste0(retro_dir, "/retro_SB50_",VER,"_", YEAR, ".csv"))
if(rec_type == 1){
  sigmaR <- read_csv(paste0(retro_dir, "/retro_sigmaR_",VER,"_", YEAR, ".csv"))
}
mgc <- read_csv(paste0(retro_dir, "/retro_convergence_",VER,"_", YEAR, ".csv"))

# Do estimates of sigmaR change over time? *Only when estimated*
if(rec_type == 1){
  ggplot(sigmaR, aes(x = retro, y = sigmaR)) +
    geom_col
}

order_retro <- SB %>% 
  distinct(retro) %>% 
  mutate(order_retro = factor(order(nchar(retro), retro), ordered = TRUE))

# Function plots and saves retrospective plot (including traditional
# retrospective plot plus another that shows relative % diff between peel and
# terminal year estimate per Clark et al 2012), also calculates AFSC Mohn's Rho
make_retro <- function(df, y, min_year, y_lab, plot_lab) {   #df<-SB
  
  y <- enquo(y)    #y<-enquo(spawn_biom)
  
  df %>% 
    group_by(retro) %>% 
    full_join(df %>% 
                filter(retro == "retro_0") %>% 
                select(year, term = !!y),
              by = "year") %>% 
    mutate(diff = (!!y - term) / term * 100) %>% 
    # NOTE - join order_retro into df for plotting - this variable is external
    # to this function
    left_join(order_retro) %>% 
    filter(year >= min_year) -> df
  
  # Rho calculations
  df %>% 
    # Alaska Fisheries Science Center and Hurtado-Ferro et al. (2015) Mohn's rho
    # https://www.afsc.noaa.gov/REFM/stocks/Plan_Team/2013/Sept/Retrospectives_2013_final3.pdf
    # # mean over all peels (Estimate in peel year - reference estimate (current
    # # year's estimate) / reference estimate)
    filter(year == max(year)) %>% 
    mutate(m = (!!y - term) / term) %>% 
    ungroup() %>% 
    dplyr::summarize(mohns_rho = mean(m)) -> rhos
  
  mohns_rho <- pull(rhos, mohns_rho)
  rho_txt <- as.character(
    as.expression(substitute(
      paste("Mohn's ", italic(rho), " = ", xx),
      list(xx = formatC(mohns_rho, format = "f", digits = 2)))))
  
  axisx <- tickr(df, year, 5)

  # Conventional retrospective plot
  p1 <- ggplot() +
    geom_line(data = df, aes(x = year, y = !!y, col = order_retro, group = order_retro)) +
    geom_point(data = df %>%
                 group_by(order_retro) %>%
                 dplyr::summarise(year = max(year)) %>%
                 left_join(df),
               aes(x = year, y = !!y, col = order_retro)) +
    scale_colour_grey(guide = FALSE) +
    geom_text(aes(x = min_year + 6,
                  y = df %>%
                    ungroup() %>%
                    dplyr::summarize(txt_y = 0.9 * max(!!y)) %>%
                    pull(txt_y), label = rho_txt),
              colour = "black", parse = TRUE, family = "Times New Roman") +
    labs(x = NULL, y = y_lab) +
    scale_x_continuous(breaks = axisx$breaks, labels = axisx$labels) #+
    # theme(axis.title.y = element_text(vjust = 0.5, angle = 0))

  # Percent difference plot
  p2 <- ggplot(df) +
    # geom_hline(yintercept = 0, linetype = 2) +
    # geom_line(data = filter(df, retro != "retro_0"),
    geom_line(data = df,
              aes(x = year, y = diff, colour = order_retro, group = order_retro)) +
    geom_point(data = df %>%
                 group_by(order_retro) %>%
                 dplyr::summarise(year = max(year)) %>%
                 left_join(df),
               aes(x = year, y = diff, col = order_retro)) +
    scale_color_grey(guide = FALSE) +
    labs(x = NULL, y = "Percent change\nfrom terminal year") +
    scale_x_continuous(breaks = axisx$breaks, labels = axisx$labels) #+
    # theme(axis.title.y = element_text(vjust = 0.5, angle = 0))

  cowplot::plot_grid(p1, p2, align = "hv", nrow = 2) -> retro_plot

  ggsave(filename = paste0(retro_dir, "/retrospective_", plot_lab, "_", YEAR, ".png"), plot = retro_plot,
         dpi = 300, height = 5, width = 6, units = "in")

  print(retro_plot)
  return(df)
}

SB <- make_retro(df = SB, y = spawn_biom, min_year = 2000, y_lab = "Spawning biomass\n(million lb)", plot_lab = "spawn_biom"); view(SB)
rec <- make_retro(df = rec, y = rec, min_year = 2000, y_lab = "Age-2 recruits\n(millions)", plot_lab = "recruitment"); view(rec)
Fmort <- make_retro(df = Fmort, y = Fmort, min_year = 2000, y_lab = "Fishing mortality", plot_lab = "Fmort")

SB_eg<-as.data.frame(SB); mean(SB_eg$diff/100)

rec_eg<-as.data.frame(rec); mean(rec_eg$diff)

#-------------------------------------------------------------------------------
# Hand cranking the function to make sure things are doing what I think they should
# be doing... 

df<-SB
min_year <- YEAR-21
#y <- enquo(y)    #y<-enquo(spawn_biom)

df %>% 
  group_by(retro) %>% 
  full_join(df %>% 
              filter(retro == "retro_0") %>% 
              #select(year, term = !!y),
              select(year, term = spawn_biom),
            by = "year") %>% 
  mutate(diff = (spawn_biom - term) / term * 100) %>% 
  # NOTE - join order_retro into df for plotting - this variable is external
  # to this function
  left_join(order_retro) %>% 
  filter(year >= min_year) -> df

print(df,n=40)
unique(df$term) #biomass estimates for each year from the terminal year model run
# Rho calculations
df %>% 
  # Alaska Fisheries Science Center and Hurtado-Ferro et al. (2015) Mohn's rho
  # https://www.afsc.noaa.gov/REFM/stocks/Plan_Team/2013/Sept/Retrospectives_2013_final3.pdf
  # # mean over all peels (Estimate in peel year - reference estimate (current
  # # year's estimate) / reference estimate)
  filter(year == max(year)) %>% 
  #mutate(m = (!!y - term) / term) %>% 
  mutate(m = (spawn_biom - term) / term) %>% 
  ungroup() %>% 
  dplyr::summarize(mohns_rho = mean(m),
                   mean_perc_diff = mean(diff)) -> rhos

mohns_rho <- pull(rhos, mohns_rho)
rho_txt <- as.character(
  as.expression(substitute(
    paste("Mohn's ", italic(rho), " = ", xx),
    list(xx = formatC(mohns_rho, format = "f", digits = 3)))))

axisx <- tickr(df, year, 5)
y_lab <- "Spawning biomass\n(million lb)"
plot_lab <- "spawn_biom"
# Conventional retrospective plot
p1 <- ggplot() +
  geom_line(data = df, aes(x = year, y = spawn_biom, col = order_retro, group = order_retro)) +
  geom_point(data = df %>%
               group_by(order_retro) %>%
               dplyr::summarise(year = max(year)) %>%
               left_join(df),
             aes(x = year, y = spawn_biom, col = order_retro)) +
  scale_colour_grey(guide = FALSE) +
  geom_text(aes(x = min_year + 6,
                y = df %>%
                  ungroup() %>%
                  dplyr::summarize(txt_y = 0.9 * max(spawn_biom)) %>%
                  pull(txt_y), label = rho_txt),
            colour = "black", parse = TRUE, family = "Times New Roman") +
  labs(x = NULL, y = y_lab) +
  scale_x_continuous(breaks = axisx$breaks, labels = axisx$labels) #+
# theme(axis.title.y = element_text(vjust = 0.5, angle = 0))

# Percent difference plot
p2 <- ggplot(df) +
  # geom_hline(yintercept = 0, linetype = 2) +
  # geom_line(data = filter(df, retro != "retro_0"),
  geom_line(data = df,
            aes(x = year, y = diff, colour = order_retro, group = order_retro)) +
  geom_point(data = df %>%
               group_by(order_retro) %>%
               dplyr::summarise(year = max(year)) %>%
               left_join(df),
             aes(x = year, y = diff, col = order_retro)) +
  scale_color_grey(guide = FALSE) +
  labs(x = NULL, y = "Percent change\nfrom terminal year") +
  scale_x_continuous(breaks = axisx$breaks, labels = axisx$labels) #+
# theme(axis.title.y = element_text(vjust = 0.5, angle = 0))

cowplot::plot_grid(p1, p2, align = "hv", nrow = 2) -> retro_plot

ggsave(filename = paste0(retro_dir, "/retrospective_", plot_lab, "_", YEAR, ".png"), plot = retro_plot,
       dpi = 300, height = 5, width = 6, units = "in")

#-------------------------------------------------------------------------------
##hand crank recruitment
df<-rec
min_year <- YEAR-21
#y <- enquo(y)    #y<-enquo(spawn_biom)

df %>% 
  group_by(retro) %>% 
  full_join(df %>% 
              filter(retro == "retro_0") %>% 
              #select(year, term = !!y),
              select(year, term = rec),
            by = "year") %>% 
  mutate(diff = (rec - term) / term * 100) %>% 
  # NOTE - join order_retro into df for plotting - this variable is external
  # to this function
  left_join(order_retro) %>% 
  filter(year >= min_year) -> df

print(df,n=40)
unique(df$term) #biomass estimates for each year from the terminal year model run
# Rho calculations
df %>% 
  # Alaska Fisheries Science Center and Hurtado-Ferro et al. (2015) Mohn's rho
  # https://www.afsc.noaa.gov/REFM/stocks/Plan_Team/2013/Sept/Retrospectives_2013_final3.pdf
  # # mean over all peels (Estimate in peel year - reference estimate (current
  # # year's estimate) / reference estimate)
  filter(year == max(year)) %>% 
  #mutate(m = (!!y - term) / term) %>% 
  mutate(m = (rec - term) / term) %>% 
  ungroup() %>% 
  dplyr::summarize(mohns_rho = mean(m),
                   mean_perc_diff = mean(diff)) -> rhos

mohns_rho <- pull(rhos, mohns_rho)
rho_txt <- as.character(
  as.expression(substitute(
    paste("Mohn's ", italic(rho), " = ", xx),
    list(xx = formatC(mohns_rho, format = "f", digits = 3)))))

axisx <- tickr(df, year, 5)
y_lab <- "Age-2 recruits\n(millions)"
plot_lab <- "recruitment"

# Conventional retrospective plot
p1 <- ggplot() +
  geom_line(data = df, aes(x = year, y = rec, col = order_retro, group = order_retro)) +
  geom_point(data = df %>%
               group_by(order_retro) %>%
               dplyr::summarise(year = max(year)) %>%
               left_join(df),
             aes(x = year, y = rec, col = order_retro)) +
  scale_colour_grey(guide = FALSE) +
  geom_text(aes(x = min_year + 6,
                y = df %>%
                  ungroup() %>%
                  dplyr::summarize(txt_y = 0.9 * max(rec)) %>%
                  pull(txt_y), label = rho_txt),
            colour = "black", parse = TRUE, family = "Times New Roman") +
  labs(x = NULL, y = y_lab) +
  scale_x_continuous(breaks = axisx$breaks, labels = axisx$labels) #+
# theme(axis.title.y = element_text(vjust = 0.5, angle = 0))

# Percent difference plot
p2 <- ggplot(df) +
  # geom_hline(yintercept = 0, linetype = 2) +
  # geom_line(data = filter(df, retro != "retro_0"),
  geom_line(data = df,
            aes(x = year, y = diff, colour = order_retro, group = order_retro)) +
  geom_point(data = df %>%
               group_by(order_retro) %>%
               dplyr::summarise(year = max(year)) %>%
               left_join(df),
             aes(x = year, y = diff, col = order_retro)) +
  scale_color_grey(guide = FALSE) +
  labs(x = NULL, y = "Percent change\nfrom terminal year") +
  scale_x_continuous(breaks = axisx$breaks, labels = axisx$labels) #+
# theme(axis.title.y = element_text(vjust = 0.5, angle = 0))

cowplot::plot_grid(p1, p2, align = "hv", nrow = 2) -> retro_plot

ggsave(filename = paste0(retro_dir, "/retrospective_", plot_lab, "_", YEAR, ".png"), plot = retro_plot,
       dpi = 300, height = 5, width = 6, units = "in")

# Mohn's Rho ----

# Interpretation of Mohn's Rho (Hurtado-Ferro et al 2015):
# Given that the variability of Mohn’s r depends on life history, and that the
# statistic appears insensitive to F, we propose the following rule of thumb
# when determining whether a retrospective pattern should be addressed
# explicitly: values of Mohn’s r higher than 0.20 or lower than 20.15 for
# longer-lived species (upper and lower bounds of the 90% simulation intervals
# for the flatfish base case), or higher than 0.30 or lower than 20.22 for
# shorter-lived species (upper and lower bounds of the 90% simulation intervals
# for the sardine base case) should be cause for concern and taken as indicators
# of retrospective patterns. However, Mohn’s r values smaller than those
# proposed should not be taken as confirmation that a given assessment does not
# present a retrospective pattern, and the choice of 90% means that a “false
# positive” will arise 10% of the time.
