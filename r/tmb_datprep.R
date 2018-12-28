# Data prep for Model 1 ASA inputs
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2018-04-09

# Model 1 - base ASA that only includes catch, fishery CPUE, fishery
# weight-at-age, and catch compositions. No sex structure.
source("r/helper.r")
source("r/functions.r")

syr <- 1980
lyr <- 2017
nyr <- length(syr:lyr)

rec_age <- 2
plus_group <- 42
nage <- length(rec_age:plus_group)

# Harvest ----
read_csv(paste0("data/fishery/nseiharvest_ifdb_1969_", lyr,".csv"), 
                       guess_max = 50000) %>% 
  group_by(year) %>% 
  summarize(total_pounds = sum(whole_pounds)) %>% 
  # Convert lbs to mt
  mutate(catch = total_pounds * 0.000453592) %>% 
  select(-total_pounds) %>% 
  filter(year >= syr) -> sum_catch

# Fishery CPUE ----

# Read in data, standardize cpue, etc.
read_csv(paste0("data/fishery/fishery_cpue_1997_", lyr,".csv"), 
         guess_max = 50000) %>% 
  filter(!is.na(hook_space) & !is.na(sable_lbs_set) &
           julian_day > 226) %>%  # if there were special projects before the fishery opened
  mutate(# standardize hook spacing (Sigler & Lunsford 2001, CJFAS), 1 m = 39.37 in
         std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (hook_space / 39.37))), 
         # convert lbs to kg
         std_cpue_kg = sable_lbs_set * 0.453592 / std_hooks) -> fsh_cpue  

# Nominal CPUE 
fsh_cpue %>% 
  group_by(year) %>% 
  summarise(cpue = mean(std_cpue_kg)) -> fsh_sum 

# Historical CPUE 

# From KVK: Logbooks were not included in IFDB until 1997. Commercial fishery
# CPUE values prior to 1997, for use in the ASA or other medium, are LEGACY
# VALUES. Jane updates: I don't have any source information from these numbers
# other than this. Kray kept them in a csv file called
# data/legacy_fishery_cpue.csv. Similarly, I moved and renamed the same file as
# data/fishery/legacy_fisherycpue_1980_1996.csv

read_csv("data/fishery/legacy_fisherycpue_1980_1996.csv", 
         col_names = FALSE) %>% as.numeric() -> hist_cpue

# Use the mean CV from 1997-present to estimate the variance for legacy CPUE
# values, following KVK.

data.frame(year = 1980:1996,
           # Convert to kg
           cpue = hist_cpue * 0.453592) %>% 
  bind_rows(fsh_sum ) %>% 
  mutate(upper = cpue + 0.2 * cpue,
         lower = cpue - 0.2 * cpue) -> cpue_ts


# Survey NPUE ----

read_csv(paste0("output/srvcpue_1997_", lyr, ".csv")) -> srv_sum

# FLAG Survey 2 - 3+hr soak time. I didn't vet these values and got them from Franz's
# data file

data.frame(year = 1988:1996,
           annual_cpue = c(0.2290,	0.1260,	0.1220,	0.1580,	0.1150,	0.2280,	
                           0.1720,	0.1520,	0.1600),
           survey = "1-hr soak")  %>% 
  full_join(srv_sum %>% 
              mutate(survey = "3+hr soak")) %>% 
  mutate(CIupper = annual_cpue + 0.2 * annual_cpue,
         CIlower = annual_cpue - 0.2 * annual_cpue) %>% 
  mutate(CIlower = ifelse(CIlower < 0, 0, CIlower)) -> srv_sum

# Mark-recapture index ----

read_csv(paste0("output/mr_index.csv")) -> mr_sum

# *FLAG* F. Mueter (2010) had data for 2003 and 2004. I could include his
# Petersen estimates + his 2*se - Dec 2018 I asked A. Olson to track these data
# down and will not use these in an analysis until I have the raw data.
# early_mr <- data.frame(year = c(2003, 2004), estimate = c(2.775, 2.675), q025 =
#                         c(2.5429, 2.4505), q975 = c(3.0071, 2.8995))

# Alternatively, these are KVK's esitmates for 2003 and 2004
# early_mr <- data.frame(year = c(2003, 2004), 
#                        estimate = c(1633115.34,	1734101.552),
#                        q025 = c(NA, NA), 
#                        q975 = c(NA, NA))

# bind_rows(early_mr, mr_sum) -> mr_sum
# 
# mr_sum %>% 
#   full_join(data.frame(year = min(mr_sum$year):lyr)) %>% 
#   arrange(year) -> mr_sum

# Graphics ----

axis <- tickr(cpue_ts, year, 5)

sum_catch %>% 
  mutate(upper =  catch + 0.05 * catch,
         lower = catch - 0.05 * catch) %>% 
  ggplot() + 
  geom_point(aes(year, catch)) +
  geom_line(aes(year, catch)) +
  geom_ribbon(aes(year, ymin = lower, ymax = upper),
              alpha = 0.2,  fill = "grey") +
  geom_vline(xintercept = 1997, linetype = 2, colour = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  scale_y_continuous(labels = scales::comma) +
  labs(x = "", y = "\n\nCatch\n(round mt)") -> catch

ggplot(cpue_ts) +
  geom_point(aes(year, cpue)) +
  geom_line(aes(year, cpue)) +
  geom_ribbon(aes(year, ymin = lower , ymax = upper),
              alpha = 0.2,  fill = "grey") +
  geom_vline(xintercept = 1997, linetype = 2, colour = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  lims(y = c(0, 1.4)) +
  labs(x = "", y = "\n\nFishery CPUE\n(round kg/hook)") -> cpue

ggplot(data = srv_sum) +
  geom_point(aes(year, annual_cpue, shape = survey)) +
  geom_line(aes(year, annual_cpue)) +
  geom_vline(xintercept = 1997, linetype = 2, colour = "grey") +
  geom_ribbon(aes(year, ymin = CIlower, ymax = CIupper),
              alpha = 0.2, col = "white", fill = "grey") +
  scale_x_continuous(limits = c(syr,lyr), breaks = axis$breaks, labels = axis$labels) + 
  lims(y = c(0, 0.5)) +
  labs(y = "\n\nSurvey CPUE\n(number/hook)", x = NULL, shape = NULL) +
  theme(legend.position = c(.1, .8))-> srv

mr_sum %>% 
  mutate(year = as.Date(as.character(year), format = "%Y")) %>% 
  pad(interval = "year") %>% 
  mutate(year = year(year),
         Year = factor(year)) %>% 
  gather("Abundance", "N", estimate) %>% 
  # mutate(# interpolate the CI in missing years for plotting purposes
  #        q025 = zoo::na.approx(q025, maxgap = 20, rule = 2),
  #        q975 = zoo::na.approx(q975, maxgap = 20, rule = 2)) %>% 
  ggplot() +
  geom_errorbar(aes(x = year, ymin = N - 2*sd, ymax = N + 2*sd),
              colour = "grey", width = 0) +
  # geom_ribbon(aes(x = year, ymin = N - 2*sd, ymax = N + 2*sd),
  #             alpha = 0.2, colour = "white", fill = "grey") +
  geom_point(aes(x = year, y = N)) +
  geom_line(aes(x = year, y = N, group = Abundance)) +
  scale_x_continuous(limits = c(syr,lyr), breaks = axis$breaks, 
                     labels = axis$labels) +
  ylim(c(0, 3.8)) +
  labs(x = "", y = "\n\nAbundance\n(millions)") -> mr
  
plot_grid(catch, cpue, srv, mr, ncol = 1, align = 'hv', labels = c('(A)', '(B)', '(C)', '(D)'))

ggsave(paste0("figures/tmb/abd_indices.png"),
       dpi=300, height=6, width=6, units="in")


full_join(sum_catch, cpue_ts) %>% 
  full_join(srv_sum %>% 
              spread(survey, annual_cpue)) %>% 
  full_join(mr_sum) %>%
  select(year, catch, fsh_cpue = cpue, srv1_cpue = `1-hr soak`, srv2_cpue = `3+hr soak`,
         mr = estimate, mr_sd = sd) %>% 
  mutate(index = year -min(year)) -> ts
  
write_csv(ts, "data/tmb_inputs/abd_indices.csv")

# Biological data ----

# Fishery biological data
read_csv(paste0("data/fishery/fishery_bio_2000_", lyr,".csv"), 
         guess_max = 50000) %>%
  mutate(Year = factor(year),
         Sex = factor(Sex)) -> fsh_bio

# Survey biological data
read_csv(paste0("data/survey/llsrv_bio_1985_", lyr,".csv"), 
         guess_max = 50000) %>%
  mutate(Year = factor(year),
         Sex = factor(Sex)) -> srv_bio

# Weight-at-age ----

waa <- read_csv("output/pred_waa.csv")

waa %>% 
  mutate(Source = derivedFactor("Fishery (sexes combined)" = Source == "LL fishery" & Sex == "Combined",
                             "Survey (sexes combined)" = Source == "LL survey" & Sex == "Combined",
                             "Survey females (spawning biomass)" = Source == "LL survey" & Sex == "Female",
                             .default = NA),
         Age = factor(age, levels = c("2", "3", "4", "5", "6", "7", "8",
                                      "9", "10", "11", "12", "13", "14", "15",
                                      "16", "17", "18", "19", "20", "21", "22",
                                      "23", "24", "25", "26", "27", "28", "29", "30",
                                      "31", "32", "33", "34", "35", "36", "37", "38",
                                      "39", "40", "41", "42"),
                      labels = c("2", "3", "4", "5", "6", "7", "8",
                                 "9", "10", "11", "12", "13", "14", "15",
                                 "16", "17", "18", "19", "20", "21", "22",
                                 "23", "24", "25", "26", "27", "28", "29", "30",
                                 "31", "32", "33", "34", "35", "36", "37", "38",
                                 "39", "40", "41", "42+"))) -> waa
waa <- na.omit(waa)
write_csv(waa, "data/tmb_inputs/waa.csv")

# Proportion mature -----

read_csv(paste0("output/fem_maturityatage_llsrv.csv"), 
         guess_max = 50000) -> mat

mat %>% 
  filter(age <= plus_group)  %>% 
  rename(prop_mature = probability) %>% 
  mutate(Age = factor(age, levels = c("2", "3", "4", "5", "6", "7", "8",
                                      "9", "10", "11", "12", "13", "14", "15",
                                      "16", "17", "18", "19", "20", "21", "22",
                                      "23", "24", "25", "26", "27", "28", "29", "30",
                                      "31", "32", "33", "34", "35", "36", "37", "38",
                                      "39", "40", "41", "42"),
                      labels = c("2", "3", "4", "5", "6", "7", "8",
                                 "9", "10", "11", "12", "13", "14", "15",
                                 "16", "17", "18", "19", "20", "21", "22",
                                 "23", "24", "25", "26", "27", "28", "29", "30",
                                 "31", "32", "33", "34", "35", "36", "37", "38",
                                 "39", "40", "41", "42+"))) -> mat

# Sex ratio ----

# proportion female at age in the survey

srv_bio %>% 
  filter(age %in% c(rec_age:plus_group-1)) %>% 
  ungroup() %>% 
  select(Sex, age) %>% 
  filter(Sex %in% c("Female", "Male")) %>% 
  na.omit() %>% 
  droplevels() %>% 
  count(Sex, age) %>% 
  group_by(age) %>% 
  mutate(proportion = round(n / sum(n), 2)) %>% 
  filter(Sex == "Female") %>% 
  # Plus group
  bind_rows(srv_bio %>% 
              filter(age >= plus_group) %>% 
              ungroup() %>% 
              select(Sex) %>% 
              filter(Sex %in% c("Female", "Male")) %>% 
              na.omit() %>% 
              droplevels() %>% 
              count(Sex) %>% 
              mutate(proportion = round(n / sum(n), 2),
                     age = plus_group) %>% 
              filter(Sex == "Female")) -> byage

# get generalized additive model fits and predictions
srv_fitage <- gam(I(Sex == "Female") ~ s(age), 
                  data = filter(srv_bio, age %in% rec_age:plus_group, 
                                Sex %in% c("Female", "Male")),
                  family = "binomial")

srv_predage <- predict(srv_fitage, newdata = data.frame(age = rec_age:plus_group),
                       type = "response", se = TRUE)

bind_cols(
  byage,
  #do.call cbinds each vector in the predict() output list 
  tbl_df(do.call(cbind, srv_predage))) -> byage

byage %>% 
  rename(prop_fem = proportion) %>% 
  mutate(Age = factor(age, levels = c("2", "3", "4", "5", "6", "7", "8",
                                      "9", "10", "11", "12", "13", "14", "15",
                                      "16", "17", "18", "19", "20", "21", "22",
                                      "23", "24", "25", "26", "27", "28", "29", "30",
                                      "31", "32", "33", "34", "35", "36", "37", "38",
                                      "39", "40", "41", "42"),
                      labels = c("2", "3", "4", "5", "6", "7", "8",
                                 "9", "10", "11", "12", "13", "14", "15",
                                 "16", "17", "18", "19", "20", "21", "22",
                                 "23", "24", "25", "26", "27", "28", "29", "30",
                                 "31", "32", "33", "34", "35", "36", "37", "38",
                                 "39", "40", "41", "42+"))) -> byage
full_join(byage %>% 
            select(age, prop_fem = fit),
          mat %>% 
            select(age, prop_mature)) %>% 
  write_csv("data/tmb_inputs/maturity_sexratio.csv")

# Graphics ----

# Custom axes
age_labs <- c("2", "", "", "", "6", "", "", "", "10", "", "", "", "14", "",
              "", "", "18", "", "", "", "22", "", "", "", "26", "",
              "", "", "30", "", "", "", "34", "", "", "", "38", "",
              "", "", "42+") 

ggplot(waa, aes(x = Age, y = weight, shape = Source,
                 colour = Source, group = Source)) +
  geom_point() +
  geom_line() + 
  scale_colour_grey() +
  expand_limits(y = c(0, 7)) +
  labs(x = NULL, y = "\n\nMean weight\n(kg)", colour = NULL, shape = NULL) +
  scale_x_discrete(breaks = unique(waa$Age), labels = age_labs) +
  theme(legend.position = c(.2, .8)) -> waa_plot

# Equation text for plotting values of a_50
a50 <- 6.4
a50_txt <- as.character(
  as.expression(substitute(
    paste(italic(a[50]), " = ", xx),
    list(xx = formatC(a50, format = "f", digits = 1)))))

axis <- tickr(byage, age, 1)

ggplot(mat) +
  geom_point(aes(x = age, y = prop_mature), colour = "black", shape = 15) +
  geom_line(aes(x = age, y = prop_mature, group = 1), colour = "black") +
  geom_segment(aes(x = a50, y = 0, xend = a50, yend = 0.50), 
               lty = 2, col = "grey") +
  geom_segment(aes(x = 2, y = 0.50, xend = a50, yend = 0.50), 
               lty = 2, col = "grey") +
  # a_50 labels
  geom_text(aes(10, 0.5, label = a50_txt), size = 5,
            colour = "black", parse = TRUE) +
  scale_x_continuous(limits = c(2,42), breaks = axis$breaks, 
                     labels = age_labs) +
  labs(x = NULL, y = "\n\nProportion\nmature\n") -> mat_plot


ggplot(byage, aes(x = Age)) +
  geom_line(aes(y = fit, group = 1), colour = "black") +
  geom_ribbon(aes(ymin = fit - se.fit*2, ymax = fit + se.fit*2, group = 1), 
              alpha = 0.2, fill = "black", colour = "white") +
  geom_point(aes(y = prop_fem), colour = "black") +  
  expand_limits(y = c(0.3, 0.6)) +
  scale_x_discrete(breaks = unique(waa$Age), labels = age_labs) +
  xlab("\nAge") +
  ylab("\n\nProportion\nfemale\n") +
  geom_hline(yintercept = 0.5, lty = 2, col = "grey") -> prop_fem

plot_grid(waa_plot, mat_plot, prop_fem, ncol = 1, align = 'hv', labels = c('(A)', '(B)', '(C)'))

ggsave(paste0("figures/tmb/bio_dat.png"),
       dpi=300, height=6, width=7, units="in")

# Age compositions ----

# Fishery
fsh_bio %>% 
  select(year, age) %>% 
  filter(year >= 2002 & !is.na(age)) %>% 
  droplevels() %>% 
  mutate(age = ifelse(age > plus_group, plus_group, age)) %>%
  filter(age >= rec_age & age <= plus_group ) -> fsh_comps 

fsh_comps %>% 
  count(year, age) %>%
  group_by(year) %>% 
  mutate(proportion = round(n / sum(n), 5),
         Source = "Fishery") -> fsh_comps

# Survey
srv_bio %>% 
  select(year, age) %>% 
  filter(year >= 1997 & !is.na(age)) %>% 
  droplevels() %>% 
  mutate(age = ifelse(age > plus_group, plus_group, age)) %>%
  filter(age >= rec_age & age <= plus_group ) -> srv_comps  

srv_comps %>% 
  count(year, age) %>%
  group_by(year) %>% 
  mutate(proportion = round(n / sum(n), 5),
         Source = "Survey") -> srv_comps

bind_rows(fsh_comps, srv_comps) -> agecomps

# Check that they sum to 1
agecomps %>% 
  group_by(year, Source) %>% 
  summarise(proportion = sum(proportion)) 

# Sample sizes 
agecomps %>% 
  group_by(year, Source) %>% 
  summarize(n = sum(n)) 

axisy <- tickr(agecomps, year, 5)

ggplot(agecomps, aes(x = age, y = year, size = proportion)) + #*FLAG* could swap size with proportion_scaled
  geom_hline(yintercept = seq(2000, 2015, by = 5), 
             colour = "grey", linetype = 3, alpha = 0.7) +  
  geom_point(shape = 21, colour = "black", fill = "black") +
  scale_size(range = c(0, 4)) +
  facet_wrap(~ Source) +
  xlab('\nAge') +
  ylab('') +
  guides(size = FALSE) +
  scale_y_continuous(breaks = axisy$breaks, labels = axisy$labels) +
  scale_x_continuous(breaks = unique(agecomps$age), labels = age_labs) 

ggsave("figures/tmb/agecomps.png", dpi = 300, height = 5, width = 7, units = "in")

agecomps %>% 
  left_join(data.frame(year = syr:lyr) %>% 
              mutate(index = year - min(year))) -> agecomps

# Reshape
agecomps %>% dcast(year + index + Source ~ age, value.var = "proportion") -> agecomps
agecomps[is.na(agecomps)] <- 0

write_csv(agecomps, "data/tmb_inputs/agecomps.csv")

# Starting values ------

#fishing mortality deviations

# Parameter estimates for fishing mortality deviations for 2006 ASA run by F
# Mueter
finits <- c( -1.56630826679, -1.99300652020, -2.01246349536, -1.71978991896,
             -1.83724533971, -1.27684899334, -0.983905919896, -1.02192341186,
             -0.927685571254, -1.00247299047, -1.07852014044, -0.837760996556,
             -0.672220868864, -0.311074414121, -0.345622227419, -0.254984198245,
             -0.0825745205838, 0.0567339057886, 0.189847547504, -0.142833191728,
             -0.440286665534, -0.446782301623, -0.552812445258, -0.546158911033
             -0.385300803085, -0.417195861174, -0.296751206500)

# Fishing mortalities most correlated with catch; use simple linear regression
# to develop starting values for 2006-2017
tmp <- sum_catch %>% 
  filter(year %in% 1980:2005) %>% 
  mutate(finits = finits)
f <- lm(finits ~ log(catch), data = tmp)
summary(f)
predf <- predict(f, 
                 newdata = data.frame(catch = ts %>% filter(year > 2005) %>% 
                                        pull(catch) %>% log()), 
                 type = "response")

finits <- data.frame(year = syr:lyr,
                     finits = c(finits, predf))

write_csv(finits, "data/tmb_inputs/inits_f_devs.csv")

# Starting values for recruitment deviations

rinits = c( 0.0405374218174, 0.0442650418415, 0.0482389177436,
            0.0523509979580, 0.0564424635349, 0.0601823372231,
            0.0631895950940, 0.0660174557708, 0.0543158576109,
            0.00467416429863, -0.0188002294100, -0.0697329463801,
            -0.124574875686, -0.152542991556, -0.148273569414,
            -0.103762760913, -0.0592664599196, -0.0417456966159,
            -0.0293342663812, 0.0655298533471, -0.105369343293,
            -0.282231768850, 2.33324844068, 2.02041811445,
            0.430788527045, 0.136257510816, 1.38475669371,
            0.106716839276, 0.834180223062, 0.129001661339,
            0.192456696119, 0.149727613444, -0.227607751620,
            -0.385442447714, 0.0419339981551, -0.0971306887540,
            0.00714024634689, -0.928016835774, 0.285420095181,
            -0.138735004317, 0.469180409653, 0.566364647489,
            0.349805017285, -0.448967807172, -0.169286675728,
            -1.42262349732, -1.74624961284, -1.84338180058,
            -1.45935292134, -0.368436954372 )

# (rec_devs include a parameter for all ages in the inital yr plus age-2 in all
# yrs, nyr+nage-2). For obtaining new starting values, strip the index of the
# initial yr all ages

sub <- rinits[25:50]
plot(sub)

rinits <- data.frame(rinits = c(rinits, rep(0.1, nyr+nage-2 - length(rinits))))

write_csv(rinits, "data/tmb_inputs/inits_rec_devs.csv")
