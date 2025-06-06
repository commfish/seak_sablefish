
# Yield per recruit analysis
# Author: Jane Sullivan
# Contact: jane.sullivan@noaa.gov
# Last edited: May 2021

source("r/helper.r")
source("r/functions.r")

# NOTE Feb 2019: that fishery weight-at-age variables are set equal to survey
# weight-at-age. The original variable names were retained to quickly see where
# fishery weight-at-age was used previously.

# User inputs ----

FIRST_YEAR <- 2005 # First year for which we have consistent mark-recapture abundance estimates
YEAR <- 2020 # Assessment year

# F implemented in previous year fishery. From final assessment summary table.
F_previous <- 0.0765 # 0.0632 in 2019, 0.0635 in 2018, 0.0683 in 2017, 0.0677 in 2016

mort <- 0.1 # natural mortality

rec_age <- 2
plus_group <- 31

# Mark-recapture results ----

# Outputs from mark_recapture.r

# Posterior samples from the M-R analysis
results <- read_csv(paste0("output/final_mod_posterior_", FIRST_YEAR, "_", YEAR, ".csv"))

# Assessment summary for graphics
assessment_summary <- read_csv(paste0("output/assessment_summary_", YEAR, ".csv"))

# Selectivity ----

# NOAA fishery and survey age-based selectivity coefficients. Fishery females,
# males, then survey females, males Manual input from Dana's NMFS spreadsheet -
# request from him

# Updated 20200211 with values from D Hanselman
f50_f <-  3.98 # 3.86
fslp_f <- 1.87 # 2.61
f50_m <-  4.20 # 4.22
fslp_m <- 2.57 # 2.61

s50_f <- 3.74 # 3.75
sslp_f <- 1.90 # 2.21
s50_m <- 4.52 # 3.72
sslp_m <- 1.90 # 2.21

# Selectivity vectors 
age <- rec_age:plus_group

f_sel <- 1 / (1 + exp(-fslp_f * (age - f50_f)))
m_sel <- 1 / (1 + exp(-fslp_m * (age - f50_m)))
sf_sel <- 1 / (1 + exp(-sslp_f * (age - s50_f)))
sm_sel <- 1 / (1 + exp(-sslp_m * (age - s50_m)))

# Plot selectivity 

sel_df <- data.frame(age = rep(age, 4),
                     Source = c(rep("Fishery", 2 * length(age)),
                                rep("Survey", 2 * length(age))),
                     Sex = c(rep("Female", length(age)),
                             rep("Male", length(age)),
                             rep("Female", length(age)),
                             rep("Male", length(age))),
                     selectivity = c(f_sel, m_sel, sf_sel, sm_sel)) %>% 
  filter(age <= 8)

# axis <- tickr(sel_df, age, 1)
ggplot(sel_df %>% filter(Source == "Fishery"), 
       aes(x = age, y = selectivity, #colour = Source, 
           linetype = Sex, shape = Sex)) +
  geom_point() + 
  geom_line() +
  scale_colour_grey() +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  theme(legend.position = c(0.8, 0.2)) +
  labs(x = "\nAge", y = "Selectivity\n")

ggsave(paste0("figures/fixed_selectivity_", YEAR, ".png"),
       dpi=300, height=4, width=6, units="in")

# Weight-at-age and maturity ----

# Outputs from biological.r

# Weight-at-age
read_csv(paste0("output/pred_waa_plsgrp", plus_group, "_", YEAR, ".csv"), guess_max = 50000) -> waa_l
waa_l %>% dcast(Source + Sex ~ age, value.var = "round_kg") -> waa

# Female, survey
wt_s_f <- waa %>% filter(Source == "LL survey" & Sex == "Female") %>%  select(matches("^[[:digit:]]+$")) %>% as.numeric() 

# Male, survey
wt_s_m <- waa %>%  filter(Source == "LL survey" & Sex == "Male") %>% select(matches("^[[:digit:]]+$")) %>%  as.numeric()

#Female, fishery
wt_f_f <- waa %>% filter(Source == "LL fishery" & Sex == "Female") %>%  select(matches("^[[:digit:]]+$")) %>% as.numeric()

# Male, fishery
wt_f_m <- waa %>%  filter(Source == "LL fishery" & Sex == "Male") %>%  select(matches("^[[:digit:]]+$")) %>% as.numeric()

# FLAG - Use survey weight-at-age instead of fishery weight-at-age because
# fishery weight-at-age is biased, especially at young ages
wt_f_f <- wt_s_f
wt_f_m <- wt_s_m

# Maturity at age, female survey
read_csv(paste0("output/fem_maturityatage_llsrv_plsgrp", plus_group, "_", YEAR, ".csv"), guess_max = 50000) -> mat

mat_s_f <- mat %>% dcast(. ~ age, value.var = "probability") %>% select(matches("^[[:digit:]]+$")) %>% as.numeric()

#Check to make sure all have been read in as numeric vectors
length(wt_s_f); length(wt_s_m); length(wt_f_f); length(wt_f_m); length(mat_s_f)

# axis <- tickr(waa_l, age, 5)
ggplot(waa_l %>% filter(Sex != "Combined"),
       aes(x = age, y = round_kg, colour = Source, linetype = Sex, shape = Sex)) +
  geom_point() +
  geom_line() +
  scale_colour_grey() +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  scale_y_continuous(limits = c(0,7), breaks = seq(0,7,1), labels = seq(0,7,1)) +
  labs(x = NULL, y = "Round weight (kg)\n", colour = NULL, linetype = NULL, shape = NULL) +
  theme(legend.position = c(0.8, 0.2),
        legend.key = element_rect(size = 0.5),
        legend.key.size = unit(0.8, 'lines'),
        legend.spacing.y = unit(0, "cm")) -> waa_fig

mat %>% filter(age <= 20) -> mat
# axis <- tickr(mat, age, 5)
ggplot(mat, aes(x = age, y = probability, shape = "Female, LL survey", linetype = "Female, LL survey")) + 
  geom_line() +
  geom_point() +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "\nAge", y = "Proportion mature\n", shape = NULL, linetype = NULL) +
  theme(legend.position = c(0.75, 0.2)) -> mat_fig
  
cowplot::plot_grid(waa_fig, mat_fig, align = "v", ncol = 1) 

ggsave(paste0("figures/ypr_bio_inputs_", YEAR, ".png"), dpi=300, height=7, width=6, units="in")

# Sex ratio ----

# Sex ratio in the commercial fishery in YEAR 
female_p <- read_csv(paste0("output/sexratio_byyear_plsgrp", plus_group, "_", YEAR, ".csv"), guess_max = 50000) %>%
  filter(Source == "LL fishery" & year == YEAR) %>%
  select(proportion) %>%
  as.numeric()

male_p <-  1 - female_p

# Fishery age comps ----

read_csv(paste0("output/agecomps_plsgrp", plus_group, "_", YEAR, ".csv"), guess_max = 50000) %>% 
  filter(Source == "LL fishery" & year == YEAR & Sex %in% c("Female", "Male")) -> agecomps 

# # If looking at the impact of the three age-2 fish sampled.
# read_csv(paste0("output/agecomps_plsgrp", plus_group, "_", YEAR, "_sens.csv"), guess_max = 50000) %>%
#   filter(Source == "LL fishery" & year == YEAR & Sex %in% c("Female", "Male")) -> agecomps

f <- filter(agecomps, Sex == "Female") %>% pull(proportion)
m <- filter(agecomps, Sex == "Male") %>% pull(proportion) 

# axis <- tickr(agecomps, age, 5)
ggplot(agecomps, aes(x = age, y = proportion)) +
  geom_bar(stat = "identity",
           position = "dodge", width = 0.8, fill = "grey") +
  scale_fill_grey(start = 0.55, end = 0.85) +
  facet_wrap(~Sex, ncol = 1) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  # labs(x = "\nAge", y = "Proportion\n") +
  theme(legend.position = c(0.9, 0.7))

read_csv(paste0("output/agecomps_plsgrp", plus_group, "_", YEAR, ".csv"), guess_max = 50000) %>% 
  filter(year >= 2015 & Sex %in% c("Sex combined") & age <= 10) -> agecomps 

agecomps %>% 
  mutate(cohort = year - age,
         Cohort = as.factor(cohort)) -> agecomps2

# axis <- tickr(agecomps2, age, 1)
agecomps2 %>% filter(Source == "LL survey") %>% 
  ggplot(aes(x = age, y = proportion, fill = Cohort)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8, colour = "black") +
  facet_wrap(~year, ncol = 1) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "\nAge", y = "Proportion\n") +
  theme(legend.position = "bottom")

# Discard mortality -----

# From M. Vaughn and K. Carroll 2018-06-04: Size grade and cost definition from
# processor will be used to define the probability of retaining a fish
grades <- data.frame(
  # Round kg
  kg = c(0.5, 0.6, 0.7, 1.4, 2.2, 2.9, 3.6, 5.0),
  # Based off conversation with A. Olson 2018-06-04, set grade 3/4 as 50%
  # probability of retention (p), and very low for grades below.
  p = c(0.0, 0.0, 0.0, 0.1, 0.5, 1.0, 1, 1.0)) %>%  
  right_join(data.frame(kg = seq(0.5, 8.5, by = 0.1)) %>% 
               mutate(grade = derivedFactor('No grade' = kg < 0.7,
                                            'Grade 1/2' = kg >= 0.7 & kg < 1.4,
                                            'Grade 2/3' = kg >= 1.4 & kg < 2.2,
                                            'Grade 3/4' = kg >= 2.2 & kg < 2.9,
                                            'Grade 4/5' = kg >= 2.9 & kg < 3.6,
                                            'Grade 5/7' = kg >= 3.6 & kg < 5,
                                            'Grade 7+' = kg >= 5,
                                            .method = "unique",
                                            .ordered = TRUE),
                      price = derivedFactor('$0' = grade == 'No grade',
                                            '$1.00' = grade == 'Grade 1/2',
                                            '$2.20' = grade == 'Grade 2/3',
                                            '$3.25' = grade == 'Grade 3/4',
                                            '$4.75' = grade == 'Grade 4/5',
                                            '$7.55' = grade == 'Grade 5/7',
                                            '$8.05' = grade == 'Grade 7+',
                                            .method = "unique",
                                            .ordered = TRUE),
                      # For plotting purposes (alternating grey and white panels)
                      plot_cde = ifelse(grade %in% c('No grade', 'Grade 2/3', 'Grade 4/5', 'Grade 7+'), "0", "1")), by = "kg") %>% # 
  # set p = 1 for all large fish, interpolate p's using a cubic spline across
  # smaller sizes
  arrange(kg) %>% 
  mutate(p = ifelse(kg > 3.6, 1, zoo::na.spline(p)),
         lb = 2.20462 * kg, # convert to lbs for visualization in memo
         y = 1 - p,
         dressed_kg = 0.63 * kg,
         dressed_lb = 0.63 * lb,
         kg = round(kg, 1))

# Female and male probabilities of retention at age based on survey (population) weight-at-age 
f_retention <- data.frame(age = age, kg = round(wt_s_f, 1)) %>% left_join(grades, by = "kg") %>%  pull(p)
m_retention <- data.frame(age = age, kg = round(wt_s_m, 1)) %>% left_join(grades, by = "kg") %>% pull(p)

# # Retention probability inputs for TMB model, single sex and sex-structued: -
# NOW IN scaa_datprep.R
# waa_l %>% filter(Source == "LL survey") %>% 
#   mutate(kg = round(round_kg, 1)) %>%
#   left_join(grades, by = "kg") %>% 
#   select(Source, Sex, age, kg, grade, price, p) %>% 
#   write_csv("data/tmb_inputs/retention_probs.csv")

# Plot size, sex, and age-specific probabilities of discarding a fish that
# parameterize model
# Min lbs by grade
# grades %>% 
#   filter(lbs <= 12) %>% 
#   group_by(grade, price, plot_cde) %>% 
#   dplyr::summarize(mn = min(lbs),
#             mx = max(lbs),
#             mu = mean(lbs)) %>% 
#   ungroup() %>% 
#   mutate(label = paste0(price, "/lb"),
#          y = c(0.1, 0.2, 0.5, 0.4, 0.75, 0.9, 0.9))  -> grades2
# 
# # axis <- tickr(grades, lbs , 1)
# 
# ggplot() +
#   geom_line(data = grades %>% filter(lbs <= 10), 
#             aes(x = lbs, y = p), size = 1) +
#   # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
#   geom_rect(data = grades2, aes(xmin = mn, xmax = mx, ymin = -Inf, ymax = Inf, fill = plot_cde, group = 1), 
#             colour = NA, alpha = 0.2, show.legend = FALSE) +
#   scale_fill_manual(values = c("white", "grey80")) +
#   labs(x = "\n Dressed weight (lb)", y = "Retention probability\n") + 
#   geom_text(data = grades2, aes(label = label, x = mu, y = y), 
#             vjust = 1, size = 2.5) -> size 
# 
# data.frame(Age = age, Female = f_retention, Male = m_retention) %>% 
#   melt(id.vars = c("Age"), measure.vars = c("Female", "Male"), variable.name = "Sex") -> ret_sex 
# 
# # axis <- tickr(ret_sex, Age, 5)
# 
# ggplot(ret_sex, aes(x = Age, y = value, col = Sex, linetype = Sex)) +
#   geom_line(size = 1) +
#   # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
#   scale_color_manual(values = c("black", "grey75")) + 
#   scale_linetype_manual(values = c(1, 4)) + 
#   # ylim(c(0, 1)) +
#   labs(x = "\nAge", y = NULL) +
#   theme(legend.position = c(.8, .7)) -> sex
# 
# plot_grid(size, sex, align = "h")
# 
# ggsave(paste0("figures/retention_prob_", YEAR, "_", plus_group, ".png"), dpi=300,  height=4, width=8,  units="in")

# Adjust F to include discard mortality ----

N_MR_sex <- results %>% filter(year == YEAR) %>% dplyr::summarize(mean(N.avg)) %>% pull()

AGE <- rec_age:plus_group
Nm <- (rec_age-1):(plus_group-1)
Nf <- (rec_age-1):(plus_group-1)

for(i in (rec_age-1):(plus_group-1)){
  
  Nm[i] <- (N_MR_sex * male_p * m[i]) / m_sel[i]
  Nf[i] <- (N_MR_sex * female_p * f[i]) / f_sel[i]
  
}

N_MR_sex
N_avail <- sum(Nm, Nf)
sum(Nm[2:41], Nf[2:41]) # excluding age-2s

# Assume discard mortality = 0.16, same as halibut directed fishery

dm <- 0.16

# PROPAGATE LAST YEAR'S ESTIMATED ABUNDANCE-AT-AGE USING STANDARD 
# AGE-STRUCTURED EQUATIONS. NOTE: AGE 2 TRANSLATES **WITH** MORTALITY 

#Fishing mortality * selectivity. Note that we are using HALF of the previous
#full-recruitment F value due to the estimate of abundance being the MEAN
#abundance in the middle of the commercial fishery. The first term is the
#retention portion of the fishery. The second term is the discard mortality,
#which we are assuming has the same selectivity as the survey (catching smaller
#fish).
# How it was when I started:
# Fm <- F_previous/2 * m_sel 
# Ff <- F_previous/2 * f_sel

# How we did it for 2017 to account for dm, but only in the first part of the
# analysis (not in the estimation of F). Not right because I didn't link the
# dicard mortality rate to the fishing intensity.
# Fm <- F_previous/2 * m_sel + sm_sel * (1 - m_retention) * dm 
# Ff <- F_previous/2 * f_sel + sf_sel * (1 - f_retention) * dm

# How I think it should be based on what they do in crab land:

estimate_F <- function(x, N_MR_sex, discard) {
  
  if(discard == TRUE) {
    
  Fm <- x/2 * m_sel * (m_retention + dm * (1 - m_retention))
  Ff <- x/2 * f_sel * (f_retention + dm * (1 - f_retention))
  
  } else {
    
  Fm <- x/2 * m_sel 
  Ff <- x/2 * f_sel 
  
  }
  
  N_fp <- (rec_age-1):(plus_group-1) 
  N_mp <- (rec_age-1):(plus_group-1)
  
  N_fp[1] <- Nf[1]
  N_mp[1] <- Nm[1]
  
  # Ages 3 - 41
  # for(i in 2:40){
  for(i in rec_age:(plus_group-2)){
    N_fp[i] <- Nf[i-1] * exp(-(Ff[i-1] + mort))
    N_mp[i] <- Nm[i-1] * exp(-(Fm[i-1] + mort))
  }
  
  # Plus class
  N_mp[plus_group-1] <- Nm[plus_group-2] * exp(-(Fm[plus_group-2] + mort)) + 
    ((Nm[plus_group-2] * exp(-(Fm[plus_group-2] + mort))) * exp(-(Fm[plus_group-1] + mort)))
  
  N_fp[plus_group-1] <- Nf[plus_group-2] * exp(-(Ff[plus_group-2] + mort)) +
    ((Nf[plus_group-2] * exp(-(Ff[plus_group-2] + mort))) * exp(-((Ff[plus_group-1]) + mort)))
  
  N_sex <- sum(N_fp,N_mp)
  
  (N_MR_sex - N_sex)^2
}

F_old <- optimize(f = estimate_F, discard = FALSE, N_MR_sex = N_MR_sex, lower = 0.01, upper = 0.7)
F_old <- F_old$minimum
Fm_old <- F_old/2 * m_sel 
Ff_old <- F_old/2 * f_sel 

F_adj <- optimize(f = estimate_F, discard = TRUE, N_MR_sex = N_MR_sex, lower = 0.01, upper = 0.7)
F_adj <- F_adj$minimum
Fm <- F_adj/2 * m_sel * (m_retention + dm * (1 - m_retention)) 
Ff <- F_adj/2 * f_sel * (f_retention + dm * (1 - f_retention)) 

fmort_df <- data.frame(age = rep(age, 4),
                       d = c(rep("None", 2 * length(age)),
                             rep("0.16", 2 * length(age))),
                       Sex = c(rep("Female", length(age)),
                               rep("Male", length(age)),
                               rep("Female", length(age)),
                               rep("Male", length(age))),
                       fmort = c(Ff_old, Fm_old, Ff, Fm)) %>% 
  filter(age <= 30) %>%
  mutate(`Discard mortality` = factor(d, levels = c("None", "0.16"),
                                      labels = c("None", "0.16"), ordered = TRUE))

ggplot(fmort_df, aes(x = age, y = fmort, linetype = `Discard mortality`)) +
  geom_line() +
  facet_wrap(~Sex) +
  scale_colour_grey() +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  theme(legend.position = c(0.8, 0.2)) +
  labs(x = "\nAge", y = "Fishing mortality\n")

ggsave(paste0("figures/fmort_discards_", YEAR, ".png"),
       dpi=300, height=4, width=6, units="in")

# *FLAG_DISCARD* To test impact of including discard mortality, comment out when not in use
# dm <- 0
# f_retention <- data.frame(age = age, p = 1) %>%  pull(p)
# m_retention <- data.frame(age = age, p = 1) %>%  pull(p)
# Fm <- Fm_old
# Ff <- Ff_old

N_fp <- (rec_age-1):(plus_group-1) # females
N_mp <- (rec_age-1):(plus_group-1) # males

N_fp[1] <- Nf[1]
N_mp[1] <- Nm[1]

# Ages: rec_age+1 to plus_group-1 (indexed from 1)
for(i in rec_age:(plus_group-2)){
  N_fp[i] <- Nf[i-1] * exp(-(Ff[i-1] + mort))
  N_mp[i] <- Nm[i-1] * exp(-(Fm[i-1] + mort))
}

# Plus group
N_mp[plus_group-1] <- Nm[plus_group-2] * exp(-(Fm[plus_group-2] + mort)) + 
  ((Nm[plus_group-2] * exp(-(Fm[plus_group-2] + mort))) * exp(-(Fm[plus_group-1] + mort)))

N_fp[plus_group-1] <- Nf[plus_group-2] * exp(-(Ff[plus_group-2] + mort)) +
  ((Nf[plus_group-2] * exp(-(Ff[plus_group-2] + mort))) * exp(-((Ff[plus_group-1]) + mort)))


#CHECK
N_sex <- sum(N_fp,N_mp)

N_sex
N_MR_sex

# BEGIN CALCS FOR FORECAST NUMBERS

#kilograms to pounds
ktp <- 2.20462

# Spawning Biomass
SB_age_s <- 1:(plus_group-1)

SB_age_s <- N_fp * mat_s_f * wt_s_f * ktp

SBs <- sum(SB_age_s)

SBs

# Simulate pop to get F levels ----

# Simulated population (females)
# Unfished spawning biomass (F = 0) (and no discard mortality)
F <- 0.0

#Abundance-at-age
N <- 1:(plus_group-1)#1:41

N[1] <- 500

for(i in rec_age:(plus_group-2)){
  
  N[i] <- N[i-1] * exp(-((F * f_sel[i-1]) + mort))
  
}

N[plus_group-1] <- N[plus_group-2] * exp(-((F * f_sel[plus_group-2]) + mort)) + ((N[plus_group-2] * exp(-((F * f_sel[plus_group-2]) + mort))) * exp(-((F * f_sel[plus_group-1]) + mort)))
N
sum(N)

SB_age <- 1:(plus_group-1)

SB_age <- N * mat_s_f * wt_s_f * ktp
SB <- sum(SB_age)
SB

# Parameter estimation
SB50 <- 0.5 * SB

# Spawning biomass function to compute values 
SBf <- function(x,SB) {
  
  NS <- 500
  
  for(i in 1:(plus_group-1)){
    
    if(i == 1)
      N[i] <- NS
    else
      # Base management reference point on landed catch otherwise the estimated
      # F is much higher to account for the fact that you have to fish harder on
      # the population in order to kill the same biomass of fish when you
      # account for dm
      N[i] <- N[i-1] * exp(-((x * f_sel[i-1]) + mort ))  
    }
  
  # Plus group
  N[plus_group-1] <- N[plus_group-2] * exp(-((x * f_sel[plus_group-2]) + mort)) + N[plus_group-2] * exp(-( mort + x * f_sel[plus_group-2] )) * exp(-( mort + x * f_sel[plus_group-1]))
  
  SB_ageS <- N * mat_s_f * wt_s_f * ktp
  
  SBS <- sum(SB_ageS)
  
  (SBS - SB)^2
  
}

fit50 <- optimize(f = SBf, SB = SB50, lower = 0.01, upper = 0.2)

# FULL RECRUITMENT FISHING MORTALITY USING **SURVEY** WEIGHT-AT-AGE FOR 
# SPAWNING BIOMASS CALCS

F50 <-  fit50$minimum

# Forecast ----

# Total mortality
f_Z50 <- mort + F50 * f_sel * (f_retention + dm * (1 - f_retention)) 
m_Z50 <- mort + F50 * m_sel * (m_retention + dm * (1 - m_retention)) 

# Total pounds encountering the gear
T50 <- sum( (N_fp * wt_f_f * ktp) * (F50 * f_sel) / (f_Z50) * (1 - exp(-(f_Z50))) +
              (N_mp * wt_f_m * ktp) * (F50 * m_sel) / (m_Z50) * (1 - exp(-(m_Z50))))

# Pounds landed (quota)
Q50 <- sum( (N_fp * wt_f_f * ktp) * (f_retention * F50 * f_sel) / (f_Z50) * (1 - exp(-(f_Z50))) +
              (N_mp * wt_f_m * ktp) * (m_retention * F50 * m_sel) / (m_Z50) * (1 - exp(-(m_Z50))))

# Discards
D50 <- sum( (N_fp * wt_f_f * ktp) * (F50 * f_sel * dm * (1 - f_retention)) / (f_Z50) * (1 - exp(-(f_Z50))) +
              (N_mp * wt_f_m * ktp) * (F50 * m_sel * dm * (1 - m_retention)) / (m_Z50) * (1 - exp(-(m_Z50))))

# Total forecasted exploited biomass (Reference: Quinn and Deriso p 339)
exp_b <- ktp * sum((N_fp * wt_f_f * f_sel) + (N_mp * wt_f_m * m_sel))
exp_b

# Total forecasted exploited abundance
exp_n <- sum((N_fp * f_sel) + (N_mp * m_sel))
exp_n

# Summary for comparison
data.frame("Quantity" = c("Assessment year mark-recapture abundance estimate",
                          "Assessment year available abundance",
                          "Forecasted available abundance",
                          "Forecasted spawning biomass",
                          "Forecasted exploitable abundance",
                          "Forecasted exploitable biomass",
                          "F_50",
                          "ABC",
                          "Mortality from discards"),
           "Value" = c(N_MR_sex, N_avail, N_sex, SBs, exp_n, exp_b, F50, Q50, D50)) %>% 
  write_csv(paste0("output/ypr_summary_", YEAR, ".csv"))
  # write_csv(paste0("output/ypr_summary_", YEAR, "_sens.csv")) # if removing three age-2 samples

# For retrospective/forecast plot in Forecast summary
fcast <- data.frame(year = YEAR, n = exp_n) %>% 
  mutate(`Current estimate` = n) %>% 
  select(- n)

data.frame(age = age, 
           Sex = c(rep("Female", plus_group-1), rep("Male", plus_group-1)),
           N = c(N_fp * f_sel, N_mp * m_sel),
           B = ktp * c(N_fp * wt_f_f * f_sel, N_mp * wt_f_m * m_sel)) %>% 
  mutate(N = N / 1000,
         B = B / 1000,
         Age = factor(age)) -> forec_byage

# Proportion exploitable abundance that is 50% or less mature
forec_byage %>% filter(age < 7) %>% dplyr::summarize(sum(N)) %>% pull / forec_byage %>% dplyr::summarize(sum(N)) %>% pull

# Proportion exploitable biomass that is 50% or less mature
forec_byage %>% filter(age < 7) %>% dplyr::summarize(sum(B)) %>% pull / forec_byage %>% dplyr::summarize(sum(B)) %>% pull

# Bargraph of forecasted numbers at age by sex
forec_byage %>% 
  ggplot(aes(age, N)) +
  geom_bar(stat = "identity", fill = "grey",  position = "dodge", width = 0.8) +
  facet_wrap(~Sex, ncol = 1) +
  # scale_x_continuous(breaks = axis$breaks, labels =  axis$labels) +
  # geom_vline(xintercept = 7, lty = 2) +
  labs(x = "\nAge", y = "Numbers (x 1000)\n") +
  theme(legend.position = c(0.9, 0.7)) #-> N

ggsave(paste0("figures/forecasted_Natage_", YEAR + 1, ".png"), 
       dpi=300, height=5, width=7, units="in")

# Bargraph of forecasted biomass at age by sex
forec_byage %>% 
  ggplot(aes(age, B)) +
  geom_bar(stat = "identity", fill = "grey",  position = "dodge", width = 0.8) +
  facet_wrap(~Sex, ncol = 1) +
  scale_x_continuous(breaks = axis$breaks, labels =  axis$labels) +
  # geom_vline(xintercept = 7, lty = 2) +
  labs(x = "\nAge", y = "Exploitable biomass (x 1000)\n") +
  theme(legend.position = c(0.9, 0.7)) #-> B

vFOREC <- c(exp_n, exp_b, F50, Q50) 

# Inputs for previous assessment (from summary table): Mark recap estimate for
# YEAR, YEAR forecast for exploitable biomass from YEAR-1, YEAR estimate of F50
# from YEAR-1, ABC for YEAR from YEAR-1
vYEAR <- c(N_MR_sex, 20131204, 0.0632, 1058037)
# vYEAR <- c(N_MR_sex, 16454232, 0.0635, 965354)

format(round((vFOREC - vYEAR)/vYEAR * 100, 1),  nsmall=1) -> perc_change

vYEAR[c(1, 2, 4)] <- lapply(vYEAR[c(1, 2, 4)], prettyNum, trim=TRUE, big.mark=",")
vYEAR[3] <- lapply(vYEAR[3], format, nsmall=3, digits=3)

vFOREC[c(1, 2, 4)] <- lapply(vFOREC[c(1, 2, 4)], prettyNum, trim=TRUE, big.mark=",")
vFOREC[3] <- lapply(vFOREC[3], format, nsmall=3, digits=3)

rbind(vYEAR, vFOREC, perc_change) %>% data.frame() -> forecast

# *FLAG_DISCARD* To test impact of including discard mortality, comment out when not in use
# save(forecast, file = paste0("output/forecast_table_", YEAR+1, "_NO_DM.rda"))

save(forecast, file = paste0("output/forecast_table_", YEAR+1, ".rda"))

# Update the YEAR exploitable biomass ---- 

updYEAR_expb <- ktp * sum((N_mp * wt_f_f  * f_sel) + (N_fp * wt_f_m * m_sel))

# Summary table ----

# FLAG this language isn't generalized, need to update each year
data.frame("Quantity" = c("Exploited abundance (2019 value from last year)",
                          "Updated exploited abundance",
                          "Exploited biomass (round lb, 2019 value from last year)",
                          "Updated exploited biomass (round lb)",
                          "$F_{ABC}=F_{50\\%}$",
                          "Mortality from discards (round lb)",
                          "Recommended $ABC$ (round lb)"),
           # "Y2018" = c(1931191, N_MR_sex, 
           #             16454232, updYEAR_expb,
           #             0.0635, NA, 965354),
           "Y2019" = c(2484601 , N_MR_sex, 
                       20131204, updYEAR_expb,
                       0.0632, 19142, 1058037),
           "Y2020" = c(exp_n, exp_n, 
                      exp_b, exp_b,
                       F50, D50, Q50)) %>% 
  # *FLAG_DISCARD* To test impact of including discard mortality, comment out when not in use
  # write_csv(paste0("output/",YEAR+1, "_summary_table_NO_DM.csv"))
  write_csv(paste0("output/",YEAR+1, "_summary_table.csv"))

# Updated retrospective plot ----

# Graph with forecast abundance estimate

# Credibility intervals N and p, N by time period
results %>% 
  gather("time_period", "N.avg", contains("N.avg")) %>% 
  group_by(year, time_period) %>% 
  dplyr::summarise(current_estimate = mean(N.avg),
            q025 = quantile(N.avg, 0.025),
            q975 = quantile(N.avg, 0.975)) %>% 
  arrange(year, time_period) %>% 
  left_join(assessment_summary %>% 
              select(year, `Previous estimate` = abundance_age2plus), 
            by = "year") %>% 
  # bind_rows(fcast) %>% 
  ungroup() %>% 
  mutate(year = as.Date(as.character(year), format = "%Y")) %>% 
  pad(interval = "year") %>% 
  mutate(year = year(year),
         Year = factor(year),
         `Current estimate recommended for forecast` = ifelse(year == YEAR, N_MR_sex, current_estimate)) %>% 
  # Add forecasted year
  bind_rows(data.frame(year = YEAR + 1,
                       time_period = "N.avg", 
                       q025 = NA, 
                       q975 = NA, 
                       "Previous estimate" = NA,
                       Year = factor(YEAR + 1),
                       "Current estimate recommended for forecast" = exp_n,
                       check.names = FALSE)) %>% 
  gather("Abundance", "N", `Previous estimate`, `Current estimate recommended for forecast`) %>% 
  mutate(N = N / 1000000,
         # interpolate the CI in missing years for plotting purposes
         q025 = zoo::na.approx(q025 / 1000000, maxgap = 20, rule = 2),
         q975 = zoo::na.approx(q975 / 1000000, maxgap = 20, rule = 2),
         # get rid of interpolated values for forecast
         q025 = ifelse(year == YEAR + 1, NA, q025),
         q975 = ifelse(year == YEAR + 1, NA, q975)) -> forec_plot

axis <- tickr(forec_plot, year, 3)

ggplot(data = forec_plot) +  
  geom_ribbon(aes(x = year, ymin = q025, ymax = q975), 
              alpha = 0.1, fill = "grey55") +
  geom_point(aes(x = year, y = N, col = Abundance, shape = Abundance), 
             size = 1.5) +
  geom_line(data = forec_plot %>% filter(!is.na(N)),
            aes(x = year, y = N, col = Abundance, linetype = Abundance)) +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  scale_color_manual(values = c("black", "grey75")) + 
  scale_linetype_manual(values = c(1, 4)) + 
  ylim(c(1, 4)) +
  labs(x = "", y = "Number of sablefish (millions)\n",
       colour = NULL, shape = NULL, linetype = NULL) +
  theme(legend.position = c(.7, .9))

ggsave(paste0("figures/model1_N_retrospective_", FIRST_YEAR, "_", YEAR, ".png"), 
       dpi=300,  height=4, width=,  units="in")

# ABC time series ----

assessment_summary %>% select(year, abc = abc_round_lbs) %>% 
  bind_rows(data.frame(year = YEAR + 1,
                       abc = Q50)) %>% 
  bind_rows(assessment_summary %>% select(year, abc = abc_round_lbs)) -> df

ggplot(df, aes(x = year, y = abc / 1e6)) + 
  geom_point() +
  geom_line() +
  # geom_vline(xintercept = 2009, lty = 2, colour = "lightgrey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = NULL, y = "ABC (million round lb)\n") +
  scale_y_continuous(limits = c(0, 2.5)) 

ggsave(paste0("figures/ABC_", FIRST_YEAR, "_", YEAR+1, ".png"), 
       dpi=300,  height=4, width=7,  units="in")
