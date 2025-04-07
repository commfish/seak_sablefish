# Work up of survey and fishery biological data
# OG: Jane Sullivan
# Current owner: Phil Joy
# Contact:philip.joy@alaska.gov;   jane.sullivan@noaa.gov 
# Last updated: March  2024

source("r_helper/helper.r")
source("r_helper/functions.r")

if(!require("fishmethods"))   install.packages("fishmethods") # use this package for growth modeling

YEAR <- 2024
rec_age <- 2
plus_group <- 31

# data -----

# survey biological  data

read_csv(paste0(YEAR+1,"/data/survey/llsrv_bio_1988_", YEAR,".csv"), 
         guess_max = 50000) %>% 
  mutate(Year = factor(year),
         Project_cde = factor(Project_cde),
         Stat = factor(Stat),
         # Station = factor(Station),
         Sex = factor(Sex),
         Mature = Maturity) %>% 
  group_by(Year, Stat) %>% 
  mutate(n = length(age),
         length_mu = mean(length, na.rm = TRUE),
         weight_mu = mean(weight, na.rm = TRUE)) %>% 
  ungroup() -> srv_bio

# Hanselman et al. 2007 Appendix GOA Sablefish SAFE Appendix 3C von bertalanffy
# parameter estimates (length and weight) for comparison with estimates from the
# survey. The coastwide parameters are still used for management, but Southeast
# slope are informative.
noaa_lvb <- read_csv("legacy_data/noaa_lvb_params_hanselman2007.csv")
str(noaa_lvb)

# Fishery biological data
read_csv(paste0(YEAR+1,"/data/fishery/fishery_bio_2000_", YEAR,".csv"), 
         guess_max = 50000) %>%
  mutate(Year = factor(year),
         Project_cde = factor(Project_cde),
         Adfg = factor(Adfg),
         Stat = factor(Stat),
         Sex = factor(Sex)) %>% 
  group_by(Year, Stat) %>% 
  mutate(n = length(age),
         length_mu = mean(length, na.rm = TRUE),
         weight_mu = mean(weight, na.rm = TRUE)) %>% 
  ungroup() -> fsh_bio
unique(fsh_bio$Gear)
str(fsh_bio)
# Pot survey biological data

read_csv(paste0(YEAR+1,"/data/survey/potsrv_bio_1981_", YEAR, ".csv"), 
         guess_max = 50000) %>% 
  mutate(Year = factor(year),
         Project_cde = factor(Project_cde),
         Stat = factor(Stat),
         Sex = factor(Sex),
         Gear = "Pot") -> potsrv_bio

str(potsrv_bio)

#some quick plots of raw data 
bind_rows(
  srv_bio %>% 
    mutate(Gear = "Longline", Source = "Longline survey") %>%
    select(year, Project_cde, Sex, age, weight, length, Gear, Source) %>% 
    filter(year >= 1997, !is.na(weight), !is.na(age)),
  fsh_bio %>% 
    mutate(Source = paste0(Gear, " fishery")) %>%
    select(year, Project_cde, Sex, age, weight, length, Gear, Source) %>% 
    #filter(year >= 2002)) %>% 
  filter(!is.na(weight) & !is.na(age) & !is.na(Sex))) -> all_bio #%>% 
  #mutate(Source = derivedFactor('LL survey' = Project_cde %in% c("03","603"),
  #                              'LL fishery' = Project_cde %in% c("02","602"),
                                #'Pot fishery' = Project_cde %in% c("02","602") & Gear == "Pot",
  #                              method = "unique"))) -> all_bio# %>% 
  #filter(year <= YEAR & age >= rec_age & age <= plus_group) -> all_bio

#bind_rows(
  srv_bio %>% 
    mutate(Gear = "Longline", Source = "Longline survey") %>%  select(year, Project_cde, Sex, age, weight, length, Gear, Source) %>% 
    filter(year >= 1997, !is.na(weight), !is.na(age)) -> llsrv.test
  

  
fsh_bio %>% 
  mutate(Source = paste0(Gear, "fishery")) %>%
 select(year, Project_cde, Sex, age, weight, length, Gear, Source) %>% 
    filter(year >= 2002) %>% 
   filter(!is.na(weight) & !is.na(age) & !is.na(Sex)) -> fshbio.test

#sample sizes by year:
all_bio %>%
  group_by(year, Source) %>%
  dplyr::summarise(samples = n()) %>%
  ggplot() +
  geom_col(aes(year,samples,fill = Source))

#aged? 
all_bio %>% filter(!is.na(age)) %>%
  group_by(year, Source) %>%
  dplyr::summarise(samples = n()) %>%
  ggplot() +
  geom_col(aes(year,samples,fill = Source))
  

ggplot(as.data.frame(all_bio %>% filter(Sex == "Female" )),
       aes(length, year, group = year, fill = year)) +
    geom_density_ridges(aes(point_fill = year, point_color = year),
                        alpha = 0.3, scale=2, #jittered_points = TRUE,
                        rel_min_height = 0.01) +
  xlim(40,110) +
 # stat_density_ridges(quantile_lines = FALSE, alpha = 0.75) +
  facet_wrap(~ Source) + theme(legend.position="none") +
  ggtitle("Female") + xlab("Length (cm)") + ylab("Year") -> female_lengths

ggplot(as.data.frame(all_bio %>% filter(Sex == "Male")),
       aes(length, year, group = year, fill = year)) +
  geom_density_ridges(aes(point_fill = year, point_color = year),
                      alpha = 0.3, scale=2, #jittered_points = TRUE,
                      rel_min_height = 0.01) +
  xlim(40,110) +
  #stat_density_ridges(quantile_lines = FALSE, alpha = 0.5) +
  facet_wrap(~ Source) + theme(legend.position="none") +
  ggtitle("Male")+ xlab("Length (cm)") + ylab("Year")-> male_lengths

library(ggpubr)
ggarrange(female_lengths, male_lengths, ncol=1,nrow=2)
ggsave(paste0(YEAR+1,"/figures/raw_length_comps.png"), dpi = 300, height = 8, width = 6, units = "in")
#==============================================================================================
###OULIER REMOVAL STEP for too fat and too skinny sablefish... usually weighing error
unique(fsh_bio$year)
new<-fsh_bio[fsh_bio$year == YEAR,]; nrow(new)
new$length
new$weight
new$Sex

str(srv_bio)
histogram(srv_bio$weight)
histogram(srv_bio$weight_mu)
histogram(srv_bio$length)
histogram(srv_bio$length_mu)

plot(srv_bio$weight ~ srv_bio$length)

nrow(fsh_bio %>% filter(year == 2024 & Sex == "Male"))

srv_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & # *FLAG* advent of "modern" survey
           !is.na(length) &
           !is.na(weight)) %>% 
  droplevels() -> srv_bio_mod
unique(srv_bio_mod$year)
plot(srv_bio_mod$weight ~ srv_bio_mod$length)

# length-weight relationship
lw_allometry <- function(length, a, b) {a * length ^ b}

START <- c(a = 1e-5, b = 3) #Starting values close to Hanselman et al. 2007

fem_fit <- nls(weight ~ lw_allometry(length = length, a, b), 
               data = filter(srv_bio_mod, Sex == "Female"), start = START)

male_fit <- nls(weight ~ lw_allometry(length = length, a, b), 
                data = filter(srv_bio_mod, Sex == "Male"), start = START)

all_fit <- nls(weight ~ lw_allometry(length = length, a, b), 
               data = srv_bio_mod, start = START)

# parameter estimates and plot fit 
beta_m <- tidy(male_fit)$estimate[2]
beta_f <- tidy(fem_fit)$estimate[2]
beta_a <- tidy(all_fit)$estimate[2]

srv_bio_mod<-bind_rows(srv_bio_mod %>% filter(Sex %in% "Male") %>% 
                            mutate(Condition = weight/(length^beta_m),
                                   Quantile = ntile(Condition,1000)/1000),
                        srv_bio_mod %>% filter(Sex %in% "Female") %>% 
                            mutate(Condition = weight/(length^beta_f),
                                   Quantile = ntile(Condition,1000)/1000))

#remove fat and skinny fish in 99.9 and 0.1 percentiles
srv_bio_noOL<-srv_bio_mod[srv_bio_mod$Quantile > 0.0005 & srv_bio_mod$Quantile < 0.9995, ]
nrow(srv_bio_noOL)
nrow(srv_bio_mod)

### #cull outliers from fishery ?  
histogram(fsh_bio$length, breaks = 100)
histogram(fsh_bio$weight, breaks = 100)
max(srv_bio_mod$length, na.rm=T); max(fsh_bio$length, na.rm=T)
plot(fsh_bio$weight ~ fsh_bio$length)

fsh_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 2002 & # *FLAG* advent of "modern" survey
           !is.na(length) &
           !is.na(weight)) %>% 
  droplevels() -> fsh_bio_mod
nrow(fsh_bio_mod)

plot(fsh_bio_mod$weight ~ fsh_bio_mod$length)

plot(fsh_bio$weight ~ fsh_bio$length)

# lets compare LW of pot and longline gear since pot fishery started in 2022
ggplot(fsh_bio) +
  geom_point(aes(length,weight,col=Gear))

ggplot(fsh_bio %>% filter(!is.na(age))) +
  geom_point(aes(age,weight,col=Gear))

ggplot(fsh_bio %>% filter(year >= 2022),
       aes(x=length, fill=Gear, col=Gear)) + 
  #geom_histogram(binwidth=1,aes(y = ..density..,col=Gear)) +
  geom_density(alpha=0.25, aes(fill = Gear))

ggsave(paste0(YEAR+1,"/figures/fsh_ll_vs_pot_lengths.png"), dpi = 300, height = 3, width = 4, units = "in")
  
ks.test(fsh_bio$length[fsh_bio$year >= 2022 & fsh_bio$Gear == "Longline"],
        fsh_bio$length[fsh_bio$year >= 2022 & fsh_bio$Gear == "Pot"])
#results show that the length distributions are significantly different! (P value < 0.05)

#lets plot the cumulative distribution functions of the data 
cdf1<-ecdf(fsh_bio$length[fsh_bio$year >= 2022 & fsh_bio$Gear == "Longline"])
cdf2<-ecdf(fsh_bio$length[fsh_bio$year >= 2022 & fsh_bio$Gear == "Pot"]) 
plot(cdf1, verticals=TRUE, do.points=FALSE, main="cdf", xlab="length")
plot(cdf2, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")

#longline catching bigger fish... 

# length-weight relationship of all fishery samples... 
fem_fit_fsh <- nls(weight ~ lw_allometry(length = length, a, b), 
               data = filter(fsh_bio_mod, Sex == "Female"), start = START)

male_fit_fsh <- nls(weight ~ lw_allometry(length = length, a, b), 
                data = filter(fsh_bio_mod, Sex == "Male"), start = START)

all_fit_fsh <- nls(weight ~ lw_allometry(length = length, a, b), 
               data = fsh_bio_mod, start = START)

# parameter estimates and plot fit 
beta_m_fsh <- tidy(male_fit_fsh)$estimate[2]
beta_f_fsh <- tidy(fem_fit_fsh)$estimate[2]
beta_a_fsh <- tidy(all_fit_fsh)$estimate[2]

fsh_bio_mod<-bind_rows(fsh_bio_mod %>% filter(Sex %in% "Male") %>% 
                          mutate(Condition = weight/(length^beta_m_fsh),
                                 Quantile = ntile(Condition,1000)/1000),
                        fsh_bio_mod %>% filter(Sex %in% "Female") %>% 
                          mutate(Condition = weight/(length^beta_f_fsh),
                                 Quantile = ntile(Condition,1000)/1000))
#remove fat and skinny fish in 99.9 and 0.1 percentiles
fsh_bio_noOL<-fsh_bio_mod[fsh_bio_mod$Quantile > 0.00099 & fsh_bio_mod$Quantile < 0.9995, ]
plot(fsh_bio_mod$weight ~ fsh_bio_mod$length); points(fsh_bio_noOL$weight ~ fsh_bio_noOL$length, col="green")
nrow(fsh_bio_noOL)
nrow(fsh_bio_mod)

unique(fsh_bio$year); unique(fsh_bio_mod$year)
#================================================================================================
# Empirical weight-at-age ---- outliers left in for this piece
colnames(srv_bio_noOL)
colnames(fsh_bio_noOL)
# All years combined
bind_rows(
  srv_bio_noOL %>% mutate(Gear = "Longline") %>%
    select(year, Project_cde, Gear, Sex, age, weight) %>% 
    filter(year >= 1997),
  fsh_bio_noOL %>% 
    select(year, Project_cde, Gear,Sex, age, weight) %>% 
    filter(year >= 2002)) %>% 
  filter(!is.na(weight) & !is.na(age) & !is.na(Sex)) %>% 
  mutate(Source = derivedFactor('LL survey' = Project_cde %in% c("03","603"),
                                'LL fishery' = Project_cde %in% c("02","602") & Gear == "Longline",
                                'Pot fishery' = Project_cde %in% c("02","602") & Gear == "Pot",
                                method = "unique")) %>% 
  filter(year <= YEAR & age >= rec_age & age <= plus_group) -> waa

bind_rows(
  waa %>%
    group_by(Source, Sex, age) %>% 
    summarise(weight = mean(weight) %>% round(4),
              samples = n()),
  waa %>% 
    group_by(Source, age) %>% 
    summarise(weight = mean(weight) %>% round(4),
              samples = n()) %>% 
    mutate(Sex = "Combined")) -> emp_waa

ggplot(emp_waa) +
  geom_point(aes(age,weight,col=Source,size=samples)) +
  facet_wrap(~ Sex) +
  geom_text(aes(age,weight,label=samples,col=Source),hjust=0,vjust=0)

# OK, looks like wonky weights for bigger fish in pot fishery, but its just limited samples of those older fish

str(emp_waa)

# Expand to grid to include all age combos and fill in NAs if there are any
# using linear interpolation
expand.grid(Source = unique(emp_waa$Source),
            Sex = unique(emp_waa$Sex),
            age = seq(rec_age, plus_group, 1))  %>% 
  data.frame()  %>% 
  full_join(emp_waa) %>%
  group_by(Source, Sex) %>% 
  mutate(weight = zoo::na.approx(weight, maxgap = 20, rule = 2)) -> emp_waa

ggplot(emp_waa) +
  geom_point(aes(age,weight,col=Source, size = samples)) +
  facet_wrap(~ Sex)

write_csv(emp_waa, paste0(YEAR+1,"/output/empircal_waa_", YEAR, ".csv"))

# Changes in weight-at-age
srv_bio_noOL %>% 
  select(year, Project_cde, Sex, age, weight) %>% 
  filter(year >= 1997) %>% 
  filter(!is.na(weight) & !is.na(age) & !is.na(Sex)) %>% 
  filter(year <= YEAR & age >= rec_age) %>% 
  group_by(year, Sex, age) %>% 
  summarise(weight = mean(weight) %>% round(4)) %>%  
  ungroup() %>% 
  mutate(Year = as.character(year),
         Age = factor(age),
         cohort = year - age,
         Cohort = as.factor(cohort))-> df

unique(df$year)

pal <- ggthemes::canva_pal("Warm and cool")(4) 

# By cohort
df_cohort <- df %>% 
         filter(cohort >= 2010 & cohort <= YEAR-3 & age >=2 & age <= 5) %>% 
         droplevels()

# Axis ticks for plot (see helper.r tickr() fxn for details)
# axis <- tickr(df_cohort, year, 2)

ggplot(df_cohort, aes(age, weight, colour = Cohort, group = Cohort)) +
  geom_line(linewidth = 1) +
  geom_point(aes(fill = Cohort), show.legend = FALSE, size = 1) +
  facet_grid(~ Sex) +
  labs(x = "Age", y = "Weight-at-age (grams)\n", colour = "Cohort") +
  guides(colour = guide_legend(ncol = 9)) +
  scale_colour_manual(values = colorRampPalette(pal)(n_distinct(df_cohort$Cohort))) +
  theme(legend.position = "bottom") #+
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels)# -> waa_cohort_plot

ggsave(paste0(YEAR+1,"/figures/waa_cohort.png"), dpi = 300, height = 5, width = 7, units = "in")

df %>% 
  filter(Age %in% c("2", "3", "4", "5")) %>% 
  droplevels() -> df
df %>% 
  group_by(Age, Sex) %>% 
  dplyr::summarize(mean_weight = mean(weight, na.rm = TRUE)) -> means

# axis <- tickr(df, year, 5)

ggplot(df, 
       aes(year, weight, group = Age, colour = Age)) + 
  geom_line() + 
  geom_point() +
  facet_wrap(~ Sex, ncol = 1) +
  geom_hline(data = means, aes(colour = Age, yintercept = mean_weight), alpha = 0.4, linetype = 2) + 
  labs(x = "Year", y = "Weight-at-age (grams)\n", colour = "Age") +
  theme(legend.position = "bottom") +
  scale_colour_manual(values = colorRampPalette(pal)(n_distinct(df$Age))) +
  guides(colour = guide_legend(nrow = 1))# +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels)

ggsave(paste0(YEAR+1,"/figures/waa_trends.png"), dpi = 300, height = 5, width = 7, units = "in")


#=========================================================================================
# Survey length-at-age -----

# subsets by length, age, sex
srv_bio_noOL %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & # *FLAG* advent of "modern" survey
           !is.na(length) &
           !is.na(age)) %>% 
  droplevels() -> laa_sub

laa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Female") -> laa_f

laa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Male") -> laa_m

# females
lvb_f <- fishmethods::growth(unit = 1, # length (2 = weight)
                size = laa_f$length, age = laa_f$age,
                error = 1, # additive (2 = multiplicative)
                # starting values from Hanselman et al. 2007 (Appendix C, Table 1)
                Sinf = 70, K = 0.22, t0 = -1.9)
lvb_f

# males
lvb_m <- fishmethods::growth(unit = 1, # length (2 = weight)
                size = laa_m$length, age = laa_m$age,
                error = 1, # additive (2 = multiplicative)
                # starting values from Hanselman et al. 2007 (Appendix C, Table 1)
                Sinf = 68, K = 0.29, t0 = -2.3)
lvb_m

# save param estimates
as_tibble(summary(lvb_f$vout)$parameters[,1:2]) %>% 
  mutate(Parameter = c("l_inf", "k", "t0"),
         Sex = "Female") %>% 
  bind_rows(as_tibble(summary(lvb_m$vout)$parameters[,1:2]) %>% 
  mutate(Parameter = c("l_inf", "k", "t0"),
         Sex = "Male")) %>% mutate(Source = "ADFG longline survey",
       Years = paste0(min(laa_sub$year), "-", max(laa_sub$year)),
       Region = "Chatham Strait",
       Function = "Length-based LVB") %>% 
  full_join(laa_sub %>% 
              group_by(Sex) %>% 
              summarise(n = n())) -> lvb_pars

unique(lvb_pars$Function)

# Are there annual trends in length-at-age? First vonB curves by Year. I only
# look at Linf here because vonB parameters are highly correlated. The following
# code could be adapted to look at trends in K. There's also a nifty fxn in this
# package growthlrt() that does likelihood ratio tests to compare multiple
# growth curves
laa_f <- laa_f %>% 
  mutate(iyr = as.integer(as.factor(year)),
         gyear = paste0(LETTERS[iyr], year)) %>% #gyears? add sequential letters to 
                           # front of year bc growthmultifit needs letter at front for group
  arrange(gyear)

laa_f$year
laa_f$gyear

#l_inf for females in each year... 
multi_f <- fishmethods::growthmultifit(len = laa_f$length, age = laa_f$age, 
               group = as.character(laa_f$gyear),
               model = 1,
               fixed = c(2, 1, 1), # 2 = not fixed, 1 = fixed. order is Linf, K, t0
               select = 2,
               error = 1, # additive (2 = multiplicative)
               # starting values from Hanselman et al. 2007 (Appendix C, Table 1)
               Linf = rep(70, length(unique(laa_f$iyr))), 
               K = rep(0.22, length(unique(laa_f$iyr))), 
               t0 = rep(-1.9, length(unique(laa_f$iyr))),
               plot = FALSE)

laa_m <- laa_m %>% 
  mutate(iyr = as.integer(as.factor(year)),
         gyear = paste0(LETTERS[iyr], year)) %>% 
  arrange(gyear)

multi_m <- growthmultifit(len = laa_m$length, age = laa_m$age, 
               group = as.character(laa_m$gyear),
               model = 1,
               fixed = c(2, 1, 1), 
               select = 2,
               error = 1, # additive (2 = multiplicative)
               # starting values from Hanselman et al. 2007 (Appendix C, Table 1)
               Linf = rep(68, length(unique(laa_m$iyr))), 
               K = rep(0.29, length(unique(laa_m$iyr))), 
               t0 = rep(-2.3, length(unique(laa_m$iyr))),
               plot = FALSE)

multi <- as_tibble(multi_f$results$parameters[,1:2]) %>% 
  mutate(Parameter = rownames(multi_f$results$parameters),
         Sex = "F") %>% 
  bind_rows(as_tibble(multi_m$results$parameters[,1:2]) %>% 
              mutate(Parameter = rownames(multi_m$results$parameters),
                     Sex = "M")) %>% 
  select(Sex, Parameter, Estimate)

view(multi) #year specific linf for male and female, also one row each for male/fem t0 and K

#get linf for each year sex by adding year specific deviation to year 1
#add col for sex specifc t0 and k, which not varying by year
multi <- multi %>%   #getting linf for each year with a bunch of dplyr steps...
  filter(Parameter %in% c("Linf1", "K1", "t01")) %>% 
  pivot_wider(id_cols = c(Sex), names_from = Parameter, values_from = Estimate) %>% 
  left_join(multi %>% 
              filter(!Parameter %in% c("Linf1", "K1", "t01"))) %>% 
  mutate(Estimate = Linf1 + Estimate) %>% 
  pivot_wider(names_from = Parameter, values_from = Estimate) %>% 
  pivot_longer(cols = -c(K1, t01, Sex), names_to = "Parameter", values_to = "Estimate") %>% 
  bind_cols(as_tibble(c(unique(laa_f$year), unique(laa_m$year)))) %>% 
  rename(year = value)

# Deviations by year for Linf vonB 
#dev.off() if not plotting may need to run this...
ggplot() + 
  geom_segment(data = multi %>% 
                 group_by(Sex) %>%
                 mutate(scaled_est = scale(Estimate),
                        mycol = ifelse(scaled_est < 0, "blue", "red")) %>% 
                 ungroup(),
               aes(x = year, y = 0,
                   xend = year, yend = scaled_est, 
                   color = mycol), size = 2) +
  scale_colour_grey() +
  geom_hline(yintercept = 0, lty = 2) + 
  guides(colour = FALSE) +
  labs(x = "", y = "Scaled parameter estimates of Linf\n") +
  facet_wrap(~ Sex, ncol = 1)

ggsave(paste0(YEAR+1,"/figures/trends_Linf_1997_", YEAR, ".png"), 
       dpi=300, height=7, width=6, units="in")

#==================================================================================
# Fishery length-at-age ----
# 1) Base, pre- 2 fleets... 
# Note that current port sampling protocols were implemented in 2002

# subsets by length, age, sex
fsh_bio_noOL %>% 
  filter(Sex %in% c("Female", "Male") &
           !is.na(length) &
           !is.na(age)) %>% 
  droplevels() -> fsh_laa_sub; str(fsh_laa_sub)

fsh_laa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Female",
         Gear == "Longline") -> fsh_laa_f_ll

fsh_laa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Male",
         Gear == "Longline") -> fsh_laa_m_ll

fsh_laa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Female",
         Gear == "Pot") -> fsh_laa_f_pot

fsh_laa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Male",
         Gear == "Pot") -> fsh_laa_m_pot

# females
lvb_f_fsh_ll <- fishmethods::growth(unit = 1, # length (2 = weight)
                             size = fsh_laa_f_ll$length, age = fsh_laa_f_ll$age,
                             error = 1, # additive (2 = multiplicative)
                             # starting values from Hanselman et al. 2007 (Appendix C, Table 1)
                             Sinf = 70, K = 0.22, t0 = -1.9)
lvb_f_fsh_pot <- fishmethods::growth(unit = 1, # length (2 = weight)
                                    size = fsh_laa_f_pot$length, age = fsh_laa_f_pot$age,
                                    error = 1, # additive (2 = multiplicative)
                                    # starting values from Hanselman et al. 2007 (Appendix C, Table 1)
                                    Sinf = 70, K = 0.22, t0 = -1.9)

# males
lvb_m_fsh_ll <- fishmethods::growth(unit = 1, # length (2 = weight)
                             size = fsh_laa_m_ll$length, age = fsh_laa_m_ll$age,
                             error = 1, # additive (2 = multiplicative)
                             # starting values from Hanselman et al. 2007 (Appendix C, Table 1)
                             Sinf = 68, K = 0.29, t0 = -2.3)
lvb_m_fsh_pot <- fishmethods::growth(unit = 1, # length (2 = weight)
                                    size = fsh_laa_m_pot$length, age = fsh_laa_m_pot$age,
                                    error = 1, # additive (2 = multiplicative)
                                    # starting values from Hanselman et al. 2007 (Appendix C, Table 1)
                                    Sinf = 68, K = 0.29, t0 = -2.3)

# save param estimates
fsh_laa_sub_ll <- fsh_laa_sub %>% filter(Gear == "Longline")
fsh_laa_sub_pot <- fsh_laa_sub %>% filter(Gear == "Pot")

as_tibble(summary(lvb_f_fsh_ll$vout)$parameters[,1:2]) %>% 
  mutate(Parameter = c("l_inf", "k", "t0"),
         Sex = "Female",
         Source = "EQS longline fishery",
         Years = paste0(min(fsh_laa_sub_ll$year), "-", max(fsh_laa_sub_ll$year)),
         Gear = "Longline") -> f_ll1

as_tibble(summary(lvb_f_fsh_pot$vout)$parameters[,1:2]) %>% 
  mutate(Parameter = c("l_inf", "k", "t0"),
         Sex = "Female",
         Source = "EQS pot fishery",
         Years = paste0(min(fsh_laa_sub_pot$year), "-", max(fsh_laa_sub_pot$year)),
         Gear = "Pot") -> f_pot1

as_tibble(summary(lvb_m_fsh_ll$vout)$parameters[,1:2]) %>% 
  mutate(Parameter = c("l_inf", "k", "t0"),
         Sex = "Male",
         Source = "EQS longline fishery",
         Years = paste0(min(fsh_laa_sub_ll$year), "-", max(fsh_laa_sub_ll$year)),
         Gear = "Longline") -> m_ll1

as_tibble(summary(lvb_m_fsh_pot$vout)$parameters[,1:2]) %>% 
  mutate(Parameter = c("l_inf", "k", "t0"),
         Sex = "Male",
         Source = "EQS pot fishery",
         Years = paste0(min(fsh_laa_sub_pot$year), "-", max(fsh_laa_sub_pot$year)),
         Gear = "Pot") -> m_pot1

bind_rows(lvb_pars %>% mutate(Gear = "Longline"),
          bind_rows(f_ll1,bind_rows(f_pot1,bind_rows(m_ll1,m_pot1))) %>%
            mutate(Region = "Chatham Strait",
                   Function = "Length-based LVB") %>%
            full_join(fsh_laa_sub %>% 
                        group_by(Sex,Gear) %>% 
                        summarise(n = n()) %>% ungroup())) -> lvb_pars

unique(lvb_pars$Function)

# Save length-at-age predictions -----
age_pred <- data.frame(age = rec_age:plus_group)

# Survey laa
laa_preds <- age_pred %>% 
  mutate(sex = "Female",
         length = predict(lvb_f$vout, newdata = age_pred)) %>% 
  bind_rows(age_pred %>% 
              mutate(sex = "Male",
                     length = predict(lvb_m$vout, newdata = age_pred))) %>% 
  mutate(Source = "LL survey") %>% 
  # ll Fishery laa
  bind_rows(age_pred %>% 
              mutate(sex = "Female",
                     length = predict(lvb_f_fsh_ll$vout, newdata = age_pred)) %>% 
              bind_rows(age_pred %>% 
                          mutate(sex = "Male",
                                 length = predict(lvb_m_fsh_ll$vout, newdata = age_pred))) %>% 
              mutate(Source = "LL fishery") ) %>% 
  # pot Fishery laa
  bind_rows(age_pred %>% 
            mutate(sex = "Female",
                   length = predict(lvb_f_fsh_pot$vout, newdata = age_pred)) %>% 
            bind_rows(age_pred %>% 
                        mutate(sex = "Male",
                               length = predict(lvb_m_fsh_pot$vout, newdata = age_pred))) %>% 
            mutate(Source = "Pot fishery") )

write_csv(laa_preds, paste0(YEAR+1,"/output/pred_laa_plsgrp", plus_group, "_", YEAR, ".csv"))

# Weight-length allometry W = alpha * L ^ beta ----

# KVK assumed beta = 3, but there are data to estimate this value (e.g., Hanselman et
# al. 2007 values beta_f = 3.02 and beta_m = 2.96 are used in the current
# assessment
#ditto Jane.. simple to calculate exponent for a population...

# subsets by weight, age, sex
srv_bio_noOL %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & #advent of "modern" survey
           !is.na(length) &
           !is.na(weight)) %>% 
  droplevels() -> allom_sub

# length-weight relationship
lw_allometry <- function(length, a, b) {a * length ^ b}

START <- c(a = 1e-5, b = 3) #Starting values close to Hanselman et al. 2007

fem_fit <- nls(weight ~ lw_allometry(length = length, a, b), 
               data = filter(allom_sub, Sex == "Female"), start = START)

male_fit <- nls(weight ~ lw_allometry(length = length, a, b), 
               data = filter(allom_sub, Sex == "Male"), start = START)

all_fit <- nls(weight ~ lw_allometry(length = length, a, b), 
               data = allom_sub, start = START)

# parameter estimates and plot fit 
beta_m <- tidy(male_fit)$estimate[2]
beta_f <- tidy(fem_fit)$estimate[2]
beta_a <- tidy(all_fit)$estimate[2]

unique(allom_sub$Sex)

bind_rows(tidy(male_fit) %>% mutate(Sex = "Male"),     
          tidy(fem_fit) %>% mutate(Sex = "Female")) %>% 
  bind_rows(tidy(all_fit) %>% mutate(Sex = "Combined")) %>% 
  dplyr::select(Parameter = term, Estimate = estimate, SE = std.error, Sex) %>% 
  mutate(Source = "ADFG longline survey",
         Years = paste0(min(laa_sub$year), "-", max(laa_sub$year)),
         Region = "Chatham Strait",
         Function = "Allometric - NLS") %>% 
  full_join(allom_sub %>% 
              group_by(Sex) %>% 
              summarise(n = n()) %>% 
              bind_rows(allom_sub %>% 
                          summarise(n = n()) %>% 
                          mutate(Sex = "Combined"))) -> allom_pars

ggplot(allom_sub, aes(length, weight, col = Sex, shape = Sex)) +
  geom_jitter(alpha =0.8) + 
  stat_function(fun = lw_allometry, 
                args = as.list(tidy(fem_fit)$estimate),
                col = "#F8766D") + 
  stat_function(fun = lw_allometry, 
                args = as.list(tidy(male_fit)$estimate),
                col = "#00BFC4", lty = 2) + 
  labs(x = "Fork length (cm)", y = "Round weight (kg)", alpha = NULL) +
  # scale_colour_grey() +
  theme(legend.position = c(0.85, 0.2))

ggsave(paste0(YEAR+1,"/figures/allometry_chathamllsurvey_1997_", YEAR, ".png"),
       dpi=300, height=4, width=6, units="in")

#==================================================================================
# Fishery length-at-age ----
# Lets compare allometry between pot and longline gear during concurrent years... 
# 2) 2022+,  fleets... 
#===== We should see if this differs between gears... 
#----- get laa for each of the two fisheries for when we add the second fleet to the SCAA

# First lets just look at data from years when both fisheries occured
# after this we'll set up the calculations for laa for the scaa
# subsets by length, age, sex
fsh_bio_noOL %>% 
  filter(Sex %in% c("Female", "Male") &
           !is.na(length) &
           !is.na(age)) %>% 
  droplevels() -> fsh_laa_sub; str(fsh_laa_sub)

#sample sizes? 
fsh_laa_sub %>% group_by(year,Gear,Sex) %>%
  dplyr::summarise(samples = n()) %>%
  ggplot() +
  geom_col(aes(year,samples,fill = Gear)) +
  facet_wrap(~Sex)

fsh_laa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Female", Gear == "Longline", year >= 2022) -> fsh_laa_f_ll22

fsh_laa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Male", Gear == "Longline", year >= 2022) -> fsh_laa_m_ll22

fsh_laa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Female", Gear == "Pot", year >= 2022) -> fsh_laa_f_pot22

fsh_laa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Male", Gear == "Pot", year >= 2022) -> fsh_laa_m_pot22

nrow(fsh_laa_f_ll22);nrow(fsh_laa_m_ll22);nrow(fsh_laa_f_pot22);nrow(fsh_laa_m_pot22)

# females
lvb_f_fsh_ll22 <- fishmethods::growth(unit = 1, # length (2 = weight)
                                 size = fsh_laa_f_ll22$length, age = fsh_laa_f_ll22$age,
                                 error = 1, # additive (2 = multiplicative)
                                 # starting values from Hanselman et al. 2007 (Appendix C, Table 1)
                                 Sinf = 70, K = 0.22, t0 = -1.9)
lvb_f_fsh_pot22 <- fishmethods::growth(unit = 1, # length (2 = weight)
                                      size = fsh_laa_f_pot22$length, age = fsh_laa_f_pot22$age,
                                      error = 1, # additive (2 = multiplicative)
                                      # starting values from Hanselman et al. 2007 (Appendix C, Table 1)
                                      Sinf = 70, K = 0.22, t0 = -1.9)

# males
lvb_m_fsh_ll22 <- fishmethods::growth(unit = 1, # length (2 = weight)
                                      size = fsh_laa_m_ll22$length, age = fsh_laa_m_ll22$age,
                                      error = 1, # additive (2 = multiplicative)
                                      # starting values from Hanselman et al. 2007 (Appendix C, Table 1)
                                      Sinf = 70, K = 0.22, t0 = -1.9)
lvb_m_fsh_pot22 <- fishmethods::growth(unit = 1, # length (2 = weight)
                                       size = fsh_laa_m_pot22$length, age = fsh_laa_m_pot22$age,
                                       error = 1, # additive (2 = multiplicative)
                                       # starting values from Hanselman et al. 2007 (Appendix C, Table 1)
                                       Sinf = 70, K = 0.22, t0 = -1.9)

#not much data after just two years of fishing in 2022 & 2023.. But this will be good as fishery develops... 
# subsets by weight, age, sex
fsh_bio_noOL %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 2022 & #advent of slinky pot fishery
           !is.na(length) &
           !is.na(weight)) %>% 
  droplevels() -> allom_sub_fsh22
colnames(allom_sub_fsh22)
# length-weight relationship
lw_allometry <- function(length, a, b) {a * length ^ b}

START <- c(a = 1e-5, b = 3) #Starting values close to Hanselman et al. 2007

ll22_fem_fit <- nls(weight ~ lw_allometry(length = length, a, b), 
               data = filter(allom_sub_fsh22, Sex == "Female", Gear == "Longline"), start = START)
pot22_fem_fit <- nls(weight ~ lw_allometry(length = length, a, b), 
                  data = filter(allom_sub_fsh22, Sex == "Female", Gear == "Pot"), start = START)
ll22_male_fit <- nls(weight ~ lw_allometry(length = length, a, b), 
                  data = filter(allom_sub_fsh22, Sex == "Male", Gear == "Longline"), start = START)
pot22_male_fit <- nls(weight ~ lw_allometry(length = length, a, b), 
                   data = filter(allom_sub_fsh22, Sex == "Male", Gear == "Pot"), start = START)

# parameter estimates and plot fit 
beta_m_ll22 <- tidy(ll22_male_fit)$estimate[2]
beta_m_pot22 <- tidy(pot22_male_fit)$estimate[2]
beta_f_ll22 <- tidy(ll22_fem_fit)$estimate[2]
beta_f_pot22 <- tidy(pot22_fem_fit)$estimate[2]

unique(allom_sub$Sex)

bind_rows(tidy(ll22_fem_fit) %>% mutate(Sex = "Female",
                                      Gear = "Longline"),     
          tidy(pot22_fem_fit) %>% mutate(Sex = "Female",
                                       Gear = "Pot"),
          tidy(ll22_male_fit) %>% mutate(Sex = "Male",
                                       Gear = "Longline"),     
          tidy(pot22_male_fit) %>% mutate(Sex = "Male",
                                        Gear = "Pot")) %>% 
  #bind_rows(tidy(all_fit) %>% mutate(Sex = "Combined")) %>% 
  dplyr::select(Parameter = term, Estimate = estimate, SE = std.error, Sex, Gear) %>% 
  mutate(Source = "Chatham longline and pot fishery",
         Years = paste0(min(allom_sub_fsh22$year), "-", max(allom_sub_fsh22$year)),
         Region = "Chatham Strait",
         Function = "Allometric - NLS") %>% 
  full_join(allom_sub_fsh22 %>% 
              group_by(Sex, Gear) %>% 
              summarise(n = n()))  -> allom_pars_fsh_gear22

length<-rep(as.numeric(seq(min(allom_sub_fsh22$length),max(allom_sub_fsh22$length),1)),4)
pred<-cbind(length,c(rep("Longline",length(length)/2),rep("Pot",length(length)/2)))
pred<-cbind(pred,c(rep("Male",nrow(pred)/4),
                   rep("Female",nrow(pred)/4),
                   rep("Male",nrow(pred)/4),
                   rep("Female",nrow(pred)/4)))
pred<-cbind(pred,c(rep(tidy(ll22_male_fit)$estimate[1],nrow(pred)/4),
                   rep(tidy(ll22_fem_fit)$estimate[1],nrow(pred)/4),
                   rep(tidy(pot22_male_fit)$estimate[1],nrow(pred)/4),
                   rep(tidy(pot22_fem_fit)$estimate[1],nrow(pred)/4)))
pred<-cbind(pred,c(rep(tidy(ll22_male_fit)$estimate[2],nrow(pred)/4),
                   rep(tidy(ll22_fem_fit)$estimate[2],nrow(pred)/4),
                   rep(tidy(pot22_male_fit)$estimate[2],nrow(pred)/4),
                   rep(tidy(pot22_fem_fit)$estimate[2],nrow(pred)/4)))
colnames(pred)<-c("length","Gear","Sex","allom_a","allom_b")
pred<-as.data.frame(pred)
pred$length<-as.numeric(pred$length)
pred$allom_a<-as.numeric(pred$allom_a)
pred$allom_b<-as.numeric(pred$allom_b)

pred$weight<-lw_allometry(pred$length,pred$allom_a,pred$allom_b)

str(pred)

ggplot(allom_sub_fsh22, aes(length, weight, col = Gear, shape = Gear)) +
  #facet_grid(~Sex) +
  geom_jitter(alpha =0.8) + 
  geom_line(data=pred, aes(length, weight, col = Gear)) +
  
  labs(x = "Fork length (cm)", y = "Round weight (kg)", alpha = NULL) +
  facet_grid(~Sex) +
  # scale_colour_grey() +
  theme(legend.position = c(0.15, 0.85))

ggsave(paste0(YEAR+1,"/figures/allometry_pot_vs_ll_2022-", YEAR, ".png"),
       dpi=300, height=4, width=6, units="in")

#==================================================================================
# Save length-at-age predictions -----
age_pred <- data.frame(age = rec_age:plus_group)

# Survey laa
laa_preds_2fleets <- age_pred %>% 
  mutate(sex = "Female",
         length = predict(lvb_f$vout, newdata = age_pred)) %>% 
  bind_rows(age_pred %>% 
              mutate(sex = "Male",
                     length = predict(lvb_m$vout, newdata = age_pred))) %>% 
  mutate(Source = "LL survey") %>% 
  # Longline laa
  bind_rows(age_pred %>% 
              mutate(sex = "Female",
                     length = predict(lvb_f_fsh_ll$vout, newdata = age_pred)) %>% 
              bind_rows(age_pred %>% 
                          mutate(sex = "Male",
                                 length = predict(lvb_m_fsh_ll$vout, newdata = age_pred))) %>% 
              mutate(Source = "LL fishery")) %>%
  bind_rows(age_pred %>% 
              mutate(sex = "Female",
                     length = predict(lvb_f_fsh_pot$vout, newdata = age_pred)) %>% 
              bind_rows(age_pred %>% 
                          mutate(sex = "Male",
                                 length = predict(lvb_m_fsh_pot$vout, newdata = age_pred))) %>% 
              mutate(Source = "Pot fishery"))
              
              
write_csv(laa_preds_2fleets, paste0(YEAR+1,"/output/pred_laa_plsgrp_2fleets_", plus_group, "_", YEAR, ".csv"))

#------------------

fsh_bio_noOL %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & 
           !is.na(length) &
           !is.na(weight)) %>% 
  droplevels() -> allom_sub_fsh
colnames(allom_sub_fsh)
# length-weight relationship

ll_fem_fit <- nls(weight ~ lw_allometry(length = length, a, b), 
                    data = filter(allom_sub_fsh, Sex == "Female", Gear == "Longline"), start = START)
pot_fem_fit <- nls(weight ~ lw_allometry(length = length, a, b), 
                     data = filter(allom_sub_fsh, Sex == "Female", Gear == "Pot"), start = START)
ll_male_fit <- nls(weight ~ lw_allometry(length = length, a, b), 
                     data = filter(allom_sub_fsh, Sex == "Male", Gear == "Longline"), start = START)
pot_male_fit <- nls(weight ~ lw_allometry(length = length, a, b), 
                      data = filter(allom_sub_fsh, Sex == "Male", Gear == "Pot"), start = START)

# parameter estimates and plot fit 
beta_m_ll <- tidy(ll_male_fit)$estimate[2]
beta_m_pot <- tidy(pot_male_fit)$estimate[2]
beta_f_ll <- tidy(ll_fem_fit)$estimate[2]
beta_f_pot <- tidy(pot_fem_fit)$estimate[2]

unique(allom_sub$Sex)

bind_rows(tidy(ll_fem_fit) %>% mutate(Sex = "Female",
                                        Gear = "Longline"),     
          tidy(pot_fem_fit) %>% mutate(Sex = "Female",
                                         Gear = "Pot"),
          tidy(ll_male_fit) %>% mutate(Sex = "Male",
                                         Gear = "Longline"),     
          tidy(pot_male_fit) %>% mutate(Sex = "Male",
                                          Gear = "Pot")) %>% 
  #bind_rows(tidy(all_fit) %>% mutate(Sex = "Combined")) %>% 
  dplyr::select(Parameter = term, Estimate = estimate, SE = std.error, Sex, Gear) %>% 
  mutate(Source = "Chatham longline and pot fishery",
         Years = paste0(min(allom_sub_fsh$year), "-", max(allom_sub_fsh$year)),
         Region = "Chatham Strait",
         Function = "Allometric - NLS") %>% 
  full_join(allom_sub_fsh %>% 
              group_by(Sex, Gear) %>% 
              summarise(n = n()))  -> allom_pars_fsh_gear

length<-rep(as.numeric(seq(min(allom_sub_fsh$length),max(allom_sub_fsh$length),1)),4)
pred<-cbind(length,c(rep("Longline",length(length)/2),rep("Pot",length(length)/2)))
pred<-cbind(pred,c(rep("Male",nrow(pred)/4),
                   rep("Female",nrow(pred)/4),
                   rep("Male",nrow(pred)/4),
                   rep("Female",nrow(pred)/4)))
pred<-cbind(pred,c(rep(tidy(ll_male_fit)$estimate[1],nrow(pred)/4),
                   rep(tidy(ll_fem_fit)$estimate[1],nrow(pred)/4),
                   rep(tidy(pot_male_fit)$estimate[1],nrow(pred)/4),
                   rep(tidy(pot_fem_fit)$estimate[1],nrow(pred)/4)))
pred<-cbind(pred,c(rep(tidy(ll_male_fit)$estimate[2],nrow(pred)/4),
                   rep(tidy(ll_fem_fit)$estimate[2],nrow(pred)/4),
                   rep(tidy(pot_male_fit)$estimate[2],nrow(pred)/4),
                   rep(tidy(pot_fem_fit)$estimate[2],nrow(pred)/4)))
colnames(pred)<-c("length","Gear","Sex","allom_a","allom_b")
pred<-as.data.frame(pred)
pred$length<-as.numeric(pred$length)
pred$allom_a<-as.numeric(pred$allom_a)
pred$allom_b<-as.numeric(pred$allom_b)

pred$weight<-lw_allometry(pred$length,pred$allom_a,pred$allom_b)

str(pred)

ggplot(allom_sub_fsh, aes(length, weight, col = Gear, shape = Gear)) +
  #facet_grid(~Sex) +
  geom_jitter(alpha =0.8) + 
  geom_line(data=pred, aes(length, weight, col = Gear)) +
  
  labs(x = "Fork length (cm)", y = "Round weight (kg)", alpha = NULL) +
  facet_grid(~Sex) +
  # scale_colour_grey() +
  theme(legend.position = c(0.15, 0.85))

ggsave(paste0(YEAR+1,"/figures/allometry_pot_vs_ll_1997-", YEAR, ".png"),
       dpi=300, height=4, width=6, units="in")

#===================================================================================================
# Survey weight-at-age ----
#PJ 2022 note: this could be sensitive to those outliers... at least look at effect of removing them... 

# subsets by weight, age, sex
srv_bio_noOL %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & # *FLAG* advent of "modern" survey
           !is.na(age) &
           !is.na(weight)) %>% 
  droplevels() -> waa_sub

waa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Female") -> waa_f

waa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Male") -> waa_m

#w at age curves
# females
wvb_f <- fishmethods::growth(unit = 2, # 1 = length, 2 = weight
                             size = waa_f$weight, age = waa_f$age,
                             error = 2, # 1 = additive, 2 = multiplicative log(w_i) = log(w_inf) + beta * log(1 - exp * (-k * (age_i - t0))) + error
                             # starting values from Hanselman et al. 2007 (Appendix C, Table 5)
                             Sinf = 5.5, K = 0.24, t0 = -1.4,
                             B = allom_pars %>% filter(Sex == "Female" & Parameter == "b") %>% pull(Estimate))
wvb_f # gompertz failed but that wasn't our target so we're good

# males
wvb_m <- fishmethods::growth(unit = 2, 
                             size = waa_m$weight, age = waa_m$age,
                             error = 2, 
                             # starting values from Hanselman et al. 2007 (Appendix C, Table 1)
                             Sinf = 3.2, K = 0.36, t0 = -1.1,
                             B = allom_pars %>% filter(Sex == "Male" & Parameter == "b") %>% pull(Estimate))
wvb_m

# save param estimates
lvb_pars %>% bind_rows(
  as_tibble(summary(wvb_f$vout)$parameters[,1:2]) %>% 
  mutate(Parameter = c("w_inf", "k", "t0"),
         Sex = "Female") %>% 
  bind_rows(as_tibble(summary(wvb_m$vout)$parameters[,1:2]) %>% 
              mutate(Parameter = c("w_inf", "k", "t0"),
                     Sex = "Male")) %>% 
    mutate(Source = "ADFG longline survey",
           Years = paste0(min(waa_sub$year), "-", max(waa_sub$year)),
           Region = "Chatham Strait",
           Function = "Weight-based LVB",
           Gear = "Longline") %>% 
  full_join(waa_sub %>% 
              group_by(Sex) %>% 
              summarise(n = n())))  -> lvb_pars_X
View(lvb_pars_X)
lvb_pars <- lvb_pars_X

# sexes combined
wvb <- fishmethods::growth(unit = 2, 
                           size = waa_sub$weight, age = waa_sub$age,
                           error = 2, 
                           Sinf = 4.5, K = 0.30, t0 = -1.2,
                           B = allom_pars %>% filter(Sex == "Combined" & Parameter == "b") %>% pull(Estimate))
wvb # horrible fit. only going through this exercise to supply data inputs to a single sex model if ever desired for comparison

lvb_pars %>% bind_rows(
  as_tibble(summary(wvb$vout)$parameters[,1:2]) %>% 
    mutate(Parameter = c("w_inf", "k", "t0"),
           Sex = "Combined") %>% 
    full_join(waa_sub %>% 
                summarise(n = n()) %>% 
                mutate(Sex = "Combined")) %>% 
    mutate(Source = "ADFG longline survey",
           Years = paste0(min(waa_sub$year), "-", max(waa_sub$year)),
           Region = "Chatham Strait",
           Function = "Weight-based LVB",
           Gear = "Longline")) -> lvb_pars_X
lvb_pars <- lvb_pars_X

#======================================================================================================================
# Fishery weight-at-age ----

# Use same methods as survey and same starting values)

# subsets by weight, age, sex
fsh_bio_noOL %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 2002 & # use same years as survey
           !is.na(age) &
           !is.na(weight)) %>% 
  droplevels() -> fsh_waa_sub

unique(fsh_waa_sub$Gear)

fsh_waa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Female",
         Gear == "Longline") -> fsh_waa_f_ll

fsh_waa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Male",
         Gear == "Longline") -> fsh_waa_m_ll

fsh_waa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Female",
         Gear == "Pot") -> fsh_waa_f_pot

fsh_waa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Male",
         Gear == "Pot") -> fsh_waa_m_pot

# mle fit for females

# females
wvb_f_fsh_ll <- fishmethods::growth(unit = 2, # 1 = length, 2 = weight
                             size = fsh_waa_f_ll$weight, age = fsh_waa_f_ll$age,
                             error = 2, # 1 = additive, 2 = multiplicative log(w_i) = log(w_inf) + beta * log(1 - exp * (-k * (age_i - t0))) + error
                             # starting values from Hanselman et al. 2007 (Appendix C, Table 5)
                             Sinf = 5.5, K = 0.24, t0 = -1.4,
                             B = allom_pars %>% filter(Sex == "Female" & Parameter == "b") %>% pull(Estimate))
# gompertz failed but that wasn't our target so we're good

wvb_f_fsh_pot <- fishmethods::growth(unit = 2, # 1 = length, 2 = weight
                                    size = fsh_waa_f_pot$weight, age = fsh_waa_f_pot$age,
                                    error = 2, # 1 = additive, 2 = multiplicative log(w_i) = log(w_inf) + beta * log(1 - exp * (-k * (age_i - t0))) + error
                                    # starting values from Hanselman et al. 2007 (Appendix C, Table 5)
                                    Sinf = 5.5, K = 0.24, t0 = -1.4,
                                    B = allom_pars %>% filter(Sex == "Female" & Parameter == "b") %>% pull(Estimate))

# males
wvb_m_fsh_ll <- fishmethods::growth(unit = 2, 
                             size = fsh_waa_m_ll$weight, age = fsh_waa_m_ll$age,
                             error = 2, 
                             # starting values from Hanselman et al. 2007 (Appendix C, Table 1)
                             Sinf = 3.2, K = 0.36, t0 = -1.1,
                             B = allom_pars %>% filter(Sex == "Male" & Parameter == "b") %>% pull(Estimate))
wvb_m_fsh_pot <- fishmethods::growth(unit = 2, 
                                    size = fsh_waa_m_pot$weight, age = fsh_waa_m_pot$age,
                                    error = 2, 
                                    # starting values from Hanselman et al. 2007 (Appendix C, Table 1)
                                    Sinf = 3.5, K = 0.32, t0 = -1.1,
                                    B = allom_pars %>% filter(Sex == "Male" & Parameter == "b") %>% pull(Estimate))

# save param estimates
as_tibble(summary(wvb_f_fsh_ll$vout)$parameters[,1:2]) %>% 
  mutate(Parameter = c("w_inf", "k", "t0"),
         Sex = "Female",
         Source = "EQS longline fishery",
         Years = paste0(min(fsh_waa_f_ll$year), "-", max(fsh_waa_f_ll$year)),
         Gear = "Longline") -> f_ll

as_tibble(summary(wvb_f_fsh_pot$vout)$parameters[,1:2]) %>% 
  mutate(Parameter = c("w_inf", "k", "t0"),
         Sex = "Female",
         Source = "EQS pot fishery",
         Years = paste0(min(fsh_waa_f_pot$year), "-", max(fsh_waa_f_pot$year)),
         Gear = "Pot") -> f_pot

as_tibble(summary(wvb_m_fsh_ll$vout)$parameters[,1:2]) %>% 
  mutate(Parameter = c("w_inf", "k", "t0"),
         Sex = "Male",
         Source = "EQS longline fishery",
         Years = paste0(min(fsh_waa_m_ll$year), "-", max(fsh_waa_m_ll$year)),
         Gear = "Longline") -> m_ll

as_tibble(summary(wvb_m_fsh_pot$vout)$parameters[,1:2]) %>% 
  mutate(Parameter = c("w_inf", "k", "t0"),
         Sex = "Male",
         Source = "EQS pot fishery",
         Years = paste0(min(fsh_waa_m_pot$year), "-", max(fsh_waa_m_pot$year)),
         Gear = "Pot") -> m_pot

bind_rows(lvb_pars,
               bind_rows(f_ll,bind_rows(f_pot,bind_rows(m_ll,m_pot))) %>%
                 mutate(Region = "Chatham Strait",
                        Function = "Weight-based LVB") %>%
                 full_join(fsh_waa_sub %>% 
                             group_by(Sex,Gear) %>% 
                             summarise(n = n())))-> lvb_pars_X; View(lvb_pars_X)

lvb_pars <- lvb_pars_X

# sexes and gear combined
fsh_waa_sub_ll <- fsh_waa_sub %>% filter(Gear == "Longline")
fsh_waa_sub_pot <- fsh_waa_sub %>% filter(Gear == "Pot")

wvb_fsh_ll <- fishmethods::growth(unit = 2, 
                           size = fsh_waa_sub_ll$weight, age = fsh_waa_sub_ll$age,
                           error = 2, 
                           Sinf = 4.5, K = 0.30, t0 = -1.2,
                           B = allom_pars %>% filter(Sex == "Combined" & Parameter == "b") %>% pull(Estimate))
wvb_fsh_pot <- fishmethods::growth(unit = 2, 
                                  size = fsh_waa_sub_pot$weight, age = fsh_waa_sub_pot$age,
                                  error = 2, 
                                  Sinf = 4.5, K = 0.30, t0 = -1.2,
                                  B = allom_pars %>% filter(Sex == "Combined" & Parameter == "b") %>% pull(Estimate))

as_tibble(summary(wvb_fsh_ll$vout)$parameters[,1:2]) %>% 
  mutate(Parameter = c("w_inf", "k", "t0"),
         Sex = "Combined",
         Source = "EQS longline fishery",
         Gear = "Longline",
         Years = paste0(min(fsh_waa_sub_ll$year), "-", max(fsh_waa_sub_ll$year))) -> all_ll

as_tibble(summary(wvb_fsh_pot$vout)$parameters[,1:2]) %>% 
  mutate(Parameter = c("w_inf", "k", "t0"),
         Sex = "Combined",
         Source = "EQS pot fishery",
         Gear = "Pot",
         Years = paste0(min(fsh_waa_sub_pot$year), "-", max(fsh_waa_sub_pot$year))) -> all_pot

bind_rows(lvb_pars,
          bind_rows(all_ll,all_pot) %>% 
            full_join(fsh_waa_sub %>% group_by(Gear) %>%
                        summarise(n = n()) %>% 
                        mutate(Sex = "Combined")) %>%
            mutate(Region = "Chatham Strait",
                   Function = "Weight-based LVB"))-> lvb_pars_X; View(lvb_pars_X)

lvb_pars <- lvb_pars_X

view(lvb_pars)
# Save weight-at-age predictions -----

# unfortunately, while the fishmethods package makes it easy to do everything
# else, it does not make it easy to extract model predictions from the growth
# curves when using the multiplicative error structure (the one that assumes
# residuals are lognormally distributed). As such, we have to extract the
# coefficients and do the predictions manually. Part of this is applying a
# log-normal bias correction, which gets us the mean (instead of median)
# predicted value. This correction is E(w_i) = exp(mu_i + 0.5 * sigma^2), where
# mu is the predicted value in log space (i.e. log(w_i) in the formula below):

# log(w_i) = log(w_inf) + beta * log(1 - exp * (-k * (age_i - t0))) + error

coef_f_wvb <- summary(wvb_f$vout)$parameter[,1] # survey females
coef_f_wvb_fsh_ll <- summary(wvb_f_fsh_ll$vout)$parameter[,1] # ll fishery females
coef_f_wvb_fsh_pot <- summary(wvb_f_fsh_pot$vout)$parameter[,1] # pot fishery females
beta_f <- allom_pars %>% filter(Sex == "Female" & Parameter == "b") %>% pull(Estimate) # beta from allometric model (weight-length relationship)

coef_m_wvb <- summary(wvb_m$vout)$parameter[,1] # survey males
coef_m_wvb_fsh_ll <- summary(wvb_m_fsh_ll$vout)$parameter[,1] # fishery males
coef_m_wvb_fsh_pot <- summary(wvb_m_fsh_pot$vout)$parameter[,1] # fishery males
beta_m <- allom_pars %>% filter(Sex == "Male" & Parameter == "b") %>% pull(Estimate) # beta from allometric model (weight-length relationship)

coef_wvb <- summary(wvb$vout)$parameter[,1] # survey sexes combined
coef_wvb_fsh_ll <- summary(wvb_fsh_ll$vout)$parameter[,1] # fishery sexes combined
coef_wvb_fsh_pot <- summary(wvb_fsh_pot$vout)$parameter[,1] # fishery sexes combined
beta_c <- allom_pars %>% filter(Sex == "Combined" & Parameter == "b") %>% pull(Estimate) # beta from allometric model (weight-length relationship)

waa_preds <- age_pred %>% 
  mutate(Sex = "Female",
         round_kg = exp(log(coef_f_wvb[1]) + beta_f * log(1 - exp(-coef_f_wvb[2] * (age_pred$age - coef_f_wvb[3]))) +
                          0.5 * (sigma(wvb_f$vout)^2))) %>% 
  bind_rows(age_pred %>% 
              mutate(Sex = "Male",
                     round_kg = exp(log(coef_m_wvb[1]) + beta_m * log(1 - exp(-coef_m_wvb[2] * (age_pred$age - coef_m_wvb[3]))) +
                                      0.5 * (sigma(wvb_m$vout)^2)))) %>% 
  bind_rows(age_pred %>% 
              mutate(Sex = "Combined",
                     round_kg = exp(log(coef_wvb[1]) + beta_c * log(1 - exp(-coef_wvb[2] * (age_pred$age - coef_wvb[3]))) +
                                      0.5 * (sigma(wvb$vout)^2)))) %>% 
  mutate(Source = "LL survey") %>% 
  # LL Fishery waa
  bind_rows(age_pred %>% 
              mutate(Sex = "Female",
                     round_kg = exp(log(coef_f_wvb_fsh_ll[1]) + beta_f * log(1 - exp(-coef_f_wvb_fsh_ll[2] * (age_pred$age - coef_f_wvb_fsh_ll[3]))) +
                                      0.5 * (sigma(wvb_f_fsh_ll$vout)^2))) %>% 
              bind_rows(age_pred %>% 
                          mutate(Sex = "Male",
                                 round_kg = exp(log(coef_m_wvb_fsh_ll[1]) + beta_m * log(1 - exp(-coef_m_wvb_fsh_ll[2] * (age_pred$age - coef_m_wvb_fsh_ll[3]))) +
                                                  0.5 * (sigma(wvb_m_fsh_ll$vout)^2)))) %>% 
              bind_rows(age_pred %>% 
                          mutate(Sex = "Combined",
                                 round_kg = exp(log(coef_wvb_fsh_ll[1]) + beta_c * log(1 - exp(-coef_wvb_fsh_ll[2] * (age_pred$age - coef_wvb_fsh_ll[3]))) +
                                                  0.5 * (sigma(wvb_fsh_ll$vout)^2)))) %>% 
              mutate(Source = "LL fishery") ) %>%
  # Pot Fishery waa
  bind_rows(age_pred %>% 
            mutate(Sex = "Female",
                   round_kg = exp(log(coef_f_wvb_fsh_pot[1]) + beta_f * log(1 - exp(-coef_f_wvb_fsh_pot[2] * (age_pred$age - coef_f_wvb_fsh_pot[3]))) +
                                    0.5 * (sigma(wvb_f_fsh_pot$vout)^2))) %>% 
            bind_rows(age_pred %>% 
                        mutate(Sex = "Male",
                               round_kg = exp(log(coef_m_wvb_fsh_pot[1]) + beta_m * log(1 - exp(-coef_m_wvb_fsh_pot[2] * (age_pred$age - coef_m_wvb_fsh_pot[3]))) +
                                                0.5 * (sigma(wvb_m_fsh_pot$vout)^2)))) %>% 
            bind_rows(age_pred %>% 
                        mutate(Sex = "Combined",
                               round_kg = exp(log(coef_wvb_fsh_pot[1]) + beta_c * log(1 - exp(-coef_wvb_fsh_pot[2] * (age_pred$age - coef_wvb_fsh_pot[3]))) +
                                                0.5 * (sigma(wvb_fsh_pot$vout)^2)))) %>% 
            mutate(Source = "Pot fishery") )

write_csv(waa_preds, paste0(YEAR+1,"/output/pred_waa_plsgrp", plus_group, "_", YEAR, ".csv"))

# Compare empirical and predicted weight-at-age
library(wesanderson); names(wes_palettes)
pal <- wes_palette("GrandBudapest1", 4, type = "discrete")

ggplot() +
  scale_colour_manual(values = pal) +
  geom_point(data = emp_waa %>% filter(!is.na(Source)), 
       aes(x = age, y = weight, col = Source, shape = Sex)) +
  geom_line(data = waa_preds,
            aes(x = age, y = round_kg, col = Source, linetype = Sex), size = 1) +
  #scale_colour_grey() +
  # expand_limits(y = 0) +
  labs(x = "\nAge", y = "Weight (kg)\n", linetype = "Sex", shape = "Sex") 

ggsave(paste0(YEAR+1,"/figures/compare_empirical_predicted_waa_", YEAR, ".png"), 
              dpi=300, height=4, width=6, units="in")

# Compare growth results ----

# Comparison of Hanselman et al. 2007 values with the Chatham Strait longline
# survey. Units: length (cm), weight (kg), and age (yrs)

bind_rows(allom_pars, lvb_pars %>% rename(SE = `Std. Error`)) %>% 
      mutate(Notes = "seak_sablefish/code/biological.r") %>% 
  bind_rows(noaa_lvb %>% rename(Notes = Source) %>% rename(Source = Survey)) -> lvb_comp; view(lvb_comp)

write_csv(lvb_comp, paste0(YEAR+1,"/output/compare_vonb_adfg_noaa_", YEAR, ".csv"))

#===========================================================================================
# Maturity ----

# Maturity could use a more rigorous analysis. Based on recommendations from Ben
# Williams, the maturity used for stock assessment is a length-based maturity
# curve (fit to 1997-current longline survey data) that is then translated to
# age using survey length-at-age predictions from vonB. There is also code here
# to compare this with yearly fits to length and also fits to age.

# 0 = immature, 1 = mature. Only conducted for females (because biological
# reference points / harvest strategy are based on female spawning biomass)

# subsets by length
srv_bio_noOL %>% 
  ungroup() %>% 
  filter(Sex == "Female" &
           year >= 1997 & # *FLAG* advent of "modern" survey
           !is.na(Mature) &
           !is.na(length)) %>% 
  droplevels() -> len_f; nrow(len_f)

# Sample sizes by year
with(len_f, table(year))

# base model
fit_length <- glm(Mature ~ length, data = len_f, family = binomial)
len <- seq(0, 120, 0.05)
(L50 <- round(- coef(fit_length)[1]/coef(fit_length)[2],1))
(kmat <- round(((coef(fit_length)[1] + coef(fit_length)[2]*len) / (len - L50))[1], 2))

plot(data = len_f, Mature ~ length)
plot(fit_length)
# by year, for comparison
fit_length_year <- glm(Mature ~ length * Year, data = len_f, family = binomial)
plot(fit_length_year)

summary(fit_length_year)
AIC(fit_length, fit_length_year)

# fit_length_year <- glm(Mature ~ length * Year, data = len_f, family = quasibinomial)

# New df for prediction for fit_length
new_len_f_simple <- data.frame(length = seq(0, 120, 0.05))

# New df for prediction for fit_length_year
new_len_f <- data.frame(length = rep(seq(0, 120, 0.05), n_distinct(len_f$year)),
                        Year = factor(sort(rep(unique(len_f$year), length(seq(0, 120, 0.05))), decreasing = FALSE)))

# Get predicted values for fit_length   ; ?broom::augment    
broom::augment(x = fit_length, 
               newdata = new_len_f_simple, 
               type.predict = "response") %>% 
  select(length, fitted = .fitted) -> pred_simple #, se =.se.fit #(earlier versions of broom output .se.fit!)

# Get predicted values by year for fit_length_year          
broom::augment(x = fit_length_year, 
               newdata = new_len_f, 
               type.predict = "response") %>% 
  select(Year, length, fitted = .fitted) -> pred #, se =.se.fit #(earlier versions of broom output .se.fit!)

#Length-based maturity curves - 2019 and 2020 appear to have early
#age-at-maturation relative to other years
ggplot() +
  geom_line(data = pred, 
            aes(x = length, y = fitted, group = Year, colour = as.numeric(as.character(Year)))) +
  geom_line(data = pred_simple, aes(x = length, y = fitted, lty = "All years combined"),
            colour = "black", size = 1) +
  lims(x = c(40, 85)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  scale_linetype_manual(values = 2) +
  labs(x = "\nLength (cm)", y = "Probability\n", colour = "Year", lty = NULL) +
  theme(legend.position = c(.8, .4))

ggsave(paste0(YEAR+1,"/figures/maturity_atlength_byyear_srvfem_", YEAR, ".png"), 
       dpi=300, height=4, width=6, units="in")

# Parameter estimates by year
broom::tidy(fit_length_year) %>% 
  select(param = term,
         est = estimate) -> mature_results
view(mature_results)

# Note on glm() output b/c I can never remember
# (Intercept) = intercept for Year1997 (or first level of factor) on logit scale
# length = slope for Year1997 on logit scale
# Year1998 = difference in intercept (Year1998 - Year1997)
# length:Year1998 = difference in slope (lengthYear1998 - Year1997)
bind_rows(
  # filter out intercepts, derive estimates by yr  
  mature_results %>% 
    filter(param == "(Intercept)") %>% 
    mutate(param = "Year1997",
           Parameter = "b_0"),
  mature_results %>% 
    filter(!param %in% c("(Intercept)", "length") &
             !grepl(':+', param)) %>%     # filter(!grepl('length:Year\\d{4}', param)) %>% #alternative regex
    mutate(est = est + mature_results$est[mature_results$param == "(Intercept)"],
           Parameter = "b_0"),
  # filter out slopes (contain >1 :), derive estimates by year
  mature_results %>% 
    filter(param == "length") %>% 
    mutate(param = "length:Year1997",
           Parameter = "b_1"),
  mature_results %>% 
    filter(grepl(':+', param)) %>% 
    mutate(est = est + mature_results$est[mature_results$param == "length"],
           Parameter = "b_1")) %>% #View()
  group_by(Parameter) %>% 
  # mutate(scaled_est = scale(est)) %>%
  mutate(scaled_est = (est - mean(est)) / sd(est)) %>% 
  ungroup() %>% 
  mutate(year = rep(1997:max(len_f$year), 2)) -> mature_results; view(mature_results)

# Deviations by year for length-based maturity curve param estimates
ggplot() + 
  geom_segment(data = mature_results %>% 
                 mutate(mycol = ifelse(scaled_est < 0, "blue", "red")),
               aes(x = year, y = 0,
                   xend = year, yend = scaled_est, 
                   color = mycol), size = 2) +
  geom_hline(yintercept = 0, lty = 2) + 
  guides(colour = FALSE) +
  labs(x = "", y = "Scaled parameter estimates") +
  facet_wrap(~ Parameter, ncol = 1)

# Next convert maturity length-based predictions to age using vonB predictions
# from ll survey length-at-age data
age_pred <- seq(0, plus_group, by = 0.01)

vb_pars <- lvb_pars %>% filter(Function == "Length-based LVB" &
                                 Sex == "Female" & 
                                 Source == "ADFG longline survey")
age_pred <- data.frame(age = age_pred,
                       length = round(vb_pars$Estimate[1] * (1 - exp(- vb_pars$Estimate[2] * (age_pred - vb_pars$Estimate[3]))), 1))

# Match lengths back to lengths predicted by vonb for fit_length
pred_simple <- merge(pred_simple, age_pred, by = "length")
which(is.na(pred_simple)) # should be integer(0) head(pred_simple)

# Match lengths back to lengths predicted by vonb for fit_length_year
pred <- merge(pred, age_pred, by = "length") 
which(is.na(pred)) # should be integer(0)
str(pred)
# Get length at 50% maturity (L_50 = -b_0/b_1) and get a_50 from age_pred
# (predicted from vonB)
merge(mature_results %>% 
        select(-scaled_est, -param) %>% 
        pivot_wider(names_from = Parameter, values_from = est) %>%
        mutate(length = - round(b_0 / b_1, 1)), #length at 50% maturity)
      age_pred, by = "length") %>% 
  arrange(year) %>% 
  select(year, l_50 = length, a_50 = age) %>% 
  group_by(year) %>% 
  mutate(a_50 = round(mean(a_50), 1)) %>%
  distinct() %>% ungroup() %>% 
  mutate(mu_a_50 = mean(a_50),
         mu_l_50 = mean(l_50)) -> mat_50_year
data.frame(mat_50_year)
# trends in L50 and a50 by year
ggplot(mat_50_year, aes(x = year, y = l_50)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = mu_l_50), lty = 2)

ggplot(mat_50_year, aes(x = year, y = a_50)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = mu_a_50), lty = 2)

merge(pred %>% mutate(year = Year), mat_50_year, by = "year") -> pred

# Age-based maturity curves estimated from length-based maturity and vonB growth
# curve (light blue lines are annual mean predictions, dark blue is the mean)
ggplot() +
  geom_line(data = pred, aes(x = age, y = fitted, group = Year, colour = as.numeric(as.character(Year)))) +  
  geom_line(data = pred_simple, aes(x = age, y = fitted, lty = "All years combined"),
            colour = "black", size = 1) +
  scale_colour_gradientn(colours=rainbow(4)) +
  lims(x = c(0, 20)) +
  scale_linetype_manual(values = 2) +
  labs(x = "\nAge (yr)", y = "Probability\n", colour = "Year", lty = NULL) +
  theme(legend.position = c(.8, .4)) 

ggsave(paste0(YEAR+1,"/figures/maturity_atage_byyear_srvfem_", YEAR, ".png"), 
       dpi=300, height=4, width=6, units="in")

# Comparison with age-based maturity curve
fit_age_year <- glm(Mature ~ age * Year, data = len_f, family = binomial)

# New df for prediction
new_f <- data.frame(age = seq(0, 30, by = 0.01), n_distinct(laa_f$year),
                    Year = factor(sort(rep(unique(laa_f$year), 
                                           length(seq(0, 30, by = 0.01))), 
                                       decreasing = FALSE)))

# Get predicted values by year and take the mean           
broom::augment(x = fit_age_year, 
               newdata = new_f, 
               type.predict = "response") %>% 
  select(Year, age, fitted = .fitted) %>% #, se =.se.fit # prev versions of broom output se
  group_by(age) %>% 
  # Just use mean for illustrative purposes
  mutate(Probability = mean(fitted)) -> pred_age

# Comparison of maturity at age curves. Blue is derived from length-based
# maturity cuve, red is estimated directly from age. Light lines are annual mean
# predictions, dark lines are the means.
ggplot() +
  geom_line(data = pred, aes(x = age, y = fitted, group = Year), col = "lightblue") +  
  geom_line(data = pred_simple, aes(x = age, y = fitted),
            colour = "blue", size = 2, alpha = 0.5) +
  lims(x = c(0, 20)) +
  # scale_linetype_manual(values = 2) +
  labs(x = "\nAge (yr)", y = "Probability\n") +
  theme(legend.position = c(.8, .4)) +
  geom_line(data = pred_age,
            aes(x = age, y = fitted, group = Year), 
            colour = "lightpink", alpha = 0.5) +
  geom_line(data = pred_age,
            aes(x = age, y = Probability), 
            colour = "red", size = 2, alpha = 0.5) +
  labs(x = "Age", y = "Probability")

# Length-based (translated to age; aka the blue one) is more realistic than
# age-based (the red one). Also there is no clear reason to choose the more
# complicated model (fit_length_year) over the simpler model (fit_length)
### pj2023: trend of earlier maturity continues in last two years with the influx
#           of those young year-classes that are dominating the biomass.  
#           Something to try/consider is year specific maturity curves...?

# Maturity at age for YPR and SCAA models
pred_simple %>%  
  filter(age %in% c(rec_age:plus_group)) %>%
  right_join(data.frame(age = rec_age:plus_group)) %>%
  arrange(age) %>% 
  # interpolate fitted probability to fill in any missing values - feel free to
  # revisit rounding if so desired
  mutate(Sex = "Female",
         Source = "LL survey",
         # changed rounding from 2 to 4 in 2020
         probability = round(zoo::na.approx(fitted, maxgap = Inf, rule = 2), 4)) %>% 
  select(age, probability) %>% 
  # arrange(age) %>% 
  write_csv(paste0(YEAR+1,"/output/fem_maturityatage_llsrv_plsgrp", plus_group, "_", YEAR, ".csv"))

#Derive age at 50% maturity and kmat (slope of logistic curve)
b0 <- fit_length$coefficients[1]
b1 <- fit_length$coefficients[2]
(L50 <- round(-b0/b1, 1))
(a50 <- age_pred %>% 
  right_join(data.frame(length = L50)) %>% 
  group_by(length) %>% 
  dplyr::summarise(a50 = round(mean(age), 1)) %>% 
    pull(a50))
(kmat <- round(((coef(fit_length)[1] + coef(fit_length)[2]*len) / (len - L50))[1], 2))

data.frame(year_updated = YEAR,
           L50 = L50,
           kmat = kmat,
           a50 = a50) %>% 
  write_csv(paste0(YEAR+1,"/output/maturity_param_", YEAR))  #should this be a csv file? 

#if we wanted to update this to using year specific maturity curves... 
# This code is a little broken and will need to be worked on if time varying maturity is added to the model
fit_length_year <- glm(Mature ~ length * Year, data = len_f, family = binomial)

b0r <- fit_length_year$coefficients[1]
b1r <- fit_length_year$coefficients[2]
b2r <- c(0,fit_length_year$coefficients[c(3:(2+(YEAR-1997)))])#Year effect
b3r <- c(0,fit_length_year$coefficients[c((3+(YEAR-1997)):(length(fit_length_year$coefficients)))]) #year * length interaction

#need to rerun and round for predicted values... 
new_len_f2 <- data.frame(length = rep(seq(0, 120, 0.0005), n_distinct(len_f$year)),
                         Year = factor(sort(rep(unique(len_f$year), length(seq(0, 120, 0.0005))), decreasing = FALSE)))
broom::augment(x = fit_length_year, 
               newdata = new_len_f2, 
               type.predict = "response") %>% 
  select(Year, length, fitted = .fitted) -> pred2
pred2$length = round(pred2$length,3)
pred2 <- merge(pred2, age_pred, by = "length") 
merge(pred2 %>% mutate(year = Year), mat_50_year, by = "year") -> pred2

round(-(b0r+b2r[1])/(b1r+ b3r[1]), 1) #1997
round(-(b0r+b2r[2])/(b1r+ b3r[2]), 1) 
round(-(b0r+b2r[3])/(b1r+ b3r[3]), 1) 
round(-(b0r+b2r[24])/(b1r+ b3r[24]), 1)
round(-(b0r+b2r[25])/(b1r+ b3r[25]), 1)
round(-(b0r+b2r[26])/(b1r+ b3r[26]), 1) #2022
round(-(b0r+b2r[27])/(b1r+ b3r[27]), 1) #2023

L50r <- round(-(b0r+b2r)/(b1r+ b3r), 1) #all years

L50df<-data.frame(length = L50r,
                  year = as.factor(replace_na(as.numeric(gsub("[^0-9]","",names(L50r))),1997)))

a50r <- pred2 %>% 
  right_join(data.frame(L50df)) %>% 
  group_by(year,length) %>% 
  dplyr::summarise(a50r = round(mean(age), 1)) %>% 
  pull(a50r)
#******* Need to check if katr1 is correctly calculated? 
kmatr <- round(((b0r + b2r + b3r+ (b1r)*len_r) / (len_r - L50r))[c(1:length(L50r))], 2)
#kmatr2 <- round(((b0r + b2r + b3r* (b1r)*len_r) / (len_r - L50r))[c(1:length(L50r))], 2)
#kmatr3 <- round(((b0r + b2r + (b3r*b1r)*len_r) / (len_r - L50r))[c(1:length(L50r))], 2)

data.frame(year = seq(1997,YEAR,1),
           year_updated = YEAR,
           L50 = L50r,
           kmat = kmatr,
           a50 = a50r) %>% 
  write_csv(paste0(YEAR+1,"/output/maturity_param_byyear_", YEAR))


# # Equation text for plotting values of a_50 and kmat
# a50_txt <- as.character(
#   as.expression(substitute(
#     paste(italic(a[50]), " = ", xx),
#     list(xx = formatC(a50, format = "f", digits = 1)))))
# 
# kmat_txt <- as.character(
#   as.expression(substitute(
#     paste(italic(k[mat]), " = ", xx),
#     list(xx = formatC(kmat, format = "f", digits = 1)))))

#======================================================================================
# Sex ratios ----
#Note PJ first pass: fit of sex ratio models is awful?  Need to follow up...

# proportion of females by age in survey and fishery

# restrict age range
aa <- rec_age:plus_group

srv_bio_noOL %>% 
  filter(age %in% aa) %>% 
  ungroup() %>% 
  select(Sex, age) %>% 
  filter(Sex %in% c("Female", "Male")) %>% 
  na.omit() %>% 
  droplevels() %>% 
  count(Sex, age) %>% 
  group_by(age) %>% 
  mutate(proportion = round(n / sum(n), 2),
         Source = "LL survey") %>% 
  filter(Sex == "Female") %>% 
  bind_rows(fsh_bio %>% 
              filter(age %in% aa) %>% 
              ungroup() %>% 
              select(Sex, age, Gear) %>% 
              filter(Sex %in% c("Female", "Male"),
                     Gear == "Longline") %>% 
              na.omit() %>% 
              droplevels() %>% 
              count(Sex, age) %>% 
              group_by(age) %>% 
              mutate(proportion = round(n / sum(n), 2),
                     Source = "LL fishery") %>% 
              filter(Sex == "Female")) %>% 
  bind_rows(fsh_bio %>% 
              filter(age %in% aa) %>% 
              ungroup() %>% 
              select(Sex, age, Gear) %>% 
              filter(Sex %in% c("Female", "Male"),
                     Gear == "Pot") %>% 
              na.omit() %>% 
              droplevels() %>% 
              count(Sex, age) %>% 
              group_by(age) %>% 
              mutate(proportion = round(n / sum(n), 2),
                     Source = "Pot fishery") %>% 
              filter(Sex == "Female"))-> byage
str(byage); view(byage)

#pot_missing <- byage %>% filter(Source == "Pot fishery") %>% select(age) %>% unique() %>% data.frame()

#fist year of pot fishery does not have all ages represented so we need to insert them
ages<-aa %>% data.frame %>% filter(!(aa %in% byage$age[byage$Source == "Pot fishery"]))
nrow(ages)
pot_missing_ages<-cbind(Sex = rep("Female",nrow(ages)),
#pot_missing_ages<-cbind(Sex = rep("Female",nrow(pot_missing)),
      age = ages,
      n = rep(0,nrow(ages)),
      proportion = rep(NA,nrow(ages)),
      Source = rep("Pot fishery",nrow(ages))) %>%
  rename(age = ".")

byage<-rbind(byage,pot_missing_ages) %>% 
  arrange(factor(Source, levels = c("LL survey","LL fishery","Pot fishery")),
          age)


# get generalized additive model fits and predictions
# survey
srv_fitage <- gam(I(Sex == "Female") ~ s(age, k=4), 
                  data = filter(srv_bio_noOL, age %in% aa, 
                                Sex %in% c("Female", "Male")),
               family = "quasibinomial")
summary(srv_fitage)
plot(srv_fitage)

srv_predage <- predict(srv_fitage, newdata = data.frame(age = aa),
                       type = "response", se = TRUE)

# longline fishery
fsh_ll_fitage <- gam(I(Sex == "Female") ~ s(age, k=4), 
               data = filter(fsh_bio_noOL, age %in% aa,
                             Sex %in% c("Female", "Male"),
                             Gear == "Longline"),
               family = "quasibinomial")
summary(fsh_ll_fitage)
plot(fsh_ll_fitage)

fsh_ll_predage <- predict(fsh_ll_fitage, newdata = data.frame(age = aa),
                       type = "response", se = TRUE)

# pot fishery
fsh_pot_fitage <- gam(I(Sex == "Female") ~ s(age, k=4), 
                     data = filter(fsh_bio_noOL, age %in% aa,
                                   Sex %in% c("Female", "Male"),
                                   Gear == "Pot"),
                     family = "quasibinomial")
summary(fsh_pot_fitage)
plot(fsh_pot_fitage)

fsh_pot_predage <- predict(fsh_pot_fitage, 
                           newdata = data.frame(age = aa),
                          type = "response", se = TRUE)

# combine with the sex_ratio df
# *FLAG* bind_cols goes by col position, make sure survey is first

bind_cols(  #1st year pot data does not have all ages... 
  byage,
  #do.call cbinds each vector in the predict() output list 
  bind_rows(as_tibble(do.call(cbind, srv_predage)) %>% 
              mutate(source_check = "LL survey"),
            as_tibble(do.call(cbind, fsh_ll_predage)) %>% 
              mutate(source_check = "LL fishery"),
            as_tibble(do.call(cbind, fsh_pot_predage)) %>% 
              mutate(source_check = "Pot fishery")) 
  ) -> byage
view(byage); view(byage_simp)

# plot
ggplot(byage, aes(x = age)) +
  geom_line(aes(y = fit, col = Source)) +
  geom_ribbon(aes(ymin = fit - se.fit*2, ymax = fit + se.fit*2, fill = Source),  alpha = 0.2) +
  geom_point(aes(y = proportion, col = Source)) +  
  expand_limits(y = c(0.0, 1)) +
  xlab("\nAge") +
  ylab("Proportion of females\n") +
  geom_hline(yintercept = 0.5, lty = 2, col = "grey") +
  scale_colour_viridis_d(option="D",begin=0,end=0.75) +
  scale_fill_viridis_d(option="D",begin=0,end=0.75) +
  #scale_fill_manual(values = c("black", "grey")) +
  theme(legend.position = c(0.15, 0.3),
        legend.box.background = element_rect(fill = "transparent",
                                             colour = "transparent")) -> byage_plot

# proportion of females by year in the fishery and survey 

srv_bio_noOL %>% 
  filter(age %in% aa) %>% 
  ungroup() %>% 
  select(Sex, year) %>% 
  filter(Sex %in% c("Female", "Male")) %>% 
  na.omit() %>% 
  droplevels() %>% 
  count(Sex, year) %>% 
  group_by(year) %>% 
  mutate(proportion = round(n / sum(n), 2),
         Source = "LL survey") %>% 
  filter(Sex == "Female") %>% 
  bind_rows(fsh_bio_noOL %>% 
              filter(age %in% aa) %>% 
              ungroup() %>% 
              select(Sex, year) %>% 
              filter(Sex %in% c("Female", "Male")) %>% 
              na.omit() %>% 
              droplevels() %>% 
              count(Sex, year) %>% 
              group_by(year) %>% 
              mutate(proportion = round(n / sum(n), 2),
                     Source = "LL fishery") %>% 
              filter(Sex == "Female")) -> byyear_base
view(byyear_base)

srv_bio_noOL %>% 
  filter(age %in% aa) %>% 
  ungroup() %>% 
  select(Sex, year) %>% 
  filter(Sex %in% c("Female", "Male")) %>% 
  na.omit() %>% 
  droplevels() %>% 
  count(Sex, year) %>% 
  group_by(year) %>% 
  mutate(proportion = round(n / sum(n), 2),
         Source = "LL survey") %>% 
  filter(Sex == "Female") %>% 
  bind_rows(fsh_bio_noOL %>% 
              filter(age %in% aa) %>% 
              ungroup() %>% 
              select(Sex, year, Gear) %>% 
              filter(Sex %in% c("Female", "Male"),
                     Gear == "Longline") %>% 
              na.omit() %>% 
              droplevels() %>% 
              count(Sex, year) %>% 
              group_by(year) %>% 
              mutate(proportion = round(n / sum(n), 2),
                     Source = "LL fishery") %>% 
              filter(Sex == "Female")) %>%
  bind_rows(fsh_bio_noOL %>% 
              filter(age %in% aa) %>% 
              ungroup() %>% 
              select(Sex, year, Gear) %>% 
              filter(Sex %in% c("Female", "Male"),
                     Gear == "Pot") %>% 
              na.omit() %>% 
              droplevels() %>% 
              count(Sex, year) %>% 
              group_by(year) %>% 
              mutate(proportion = round(n / sum(n), 2),
                     Source = "Pot fishery") %>% 
              filter(Sex == "Female"))-> byyear
view(byyear)

# Save output for YPR analysis
write_csv(byyear, paste0(YEAR+1,"/output/sexratio_byyear_plsgrp", plus_group, "_", YEAR, ".csv"))

ys<-seq(1997,2021,1)
pot_missing_ys<-cbind(Sex = rep("Female",length(ys)),
                        year = ys,
                        n = rep(as.numeric(0),length(ys)),
                        proportion = rep(NA,length(ys)),
                        Source = rep("Pot fishery",length(ys))) %>% data.frame() %>%
  mutate(year = as.numeric(year),n = as.numeric(n))

byyear<-rbind(byyear %>% data.frame(),pot_missing_ys) %>% 
  arrange(factor(Source, levels = c("LL survey","LL fishery","Pot fishery")),
          year) %>%
  mutate(proportion = as.numeric(proportion))

# get generalized additive model fits and predictions
# survey
srv_fityear <- gam(I(Sex == "Female") ~ s(year, k = 4), 
                  data = filter(srv_bio_noOL, Sex %in% c("Female", "Male")),
                  family = "quasibinomial") #quasi- allows non-integers and avoids warnings...
summary(srv_fityear); plot(srv_fityear)

srv_yrs <- byyear %>% filter(Source == "LL survey") %>% select(year) %>% range()

srv_predyear <- predict(srv_fityear, 
                        newdata = data.frame(year = min(srv_yrs):max(srv_yrs) ),
                       type = "response", se = TRUE)

# ll fishery
fsh_ll_fityear <- gam(I(Sex == "Female") ~ s(year, k = 4), 
                   data = filter(fsh_bio_noOL, Sex %in% c("Female", "Male"),
                                 Gear == "Longline"),
                   family = "quasibinomial")
summary(fsh_ll_fityear); plot(fsh_ll_fityear)

fsh_ll_yrs <- byyear %>% filter(Source == "LL fishery") %>% select(year) %>% range()

fsh_ll_predyear <- predict(fsh_ll_fityear, 
                        newdata = data.frame(year = min(fsh_ll_yrs):max(fsh_ll_yrs) ),
                        type = "response", se = TRUE)

# pot fishery
fsh_pot_fityear <- gam(I(Sex == "Female") ~ s(year, k = 4), 
                      data = filter(fsh_bio_noOL, Sex %in% c("Female", "Male"),
                                    Gear == "Pot"),
                      family = "quasibinomial")
summary(fsh_pot_fityear); plot(fsh_pot_fityear)

fsh_pot_yrs <- byyear %>% filter(Source == "Pot fishery") %>% select(year) %>% range()

fsh_pot_predyear <- predict(fsh_pot_fityear, 
                           newdata = data.frame(year = min(fsh_pot_yrs):max(fsh_pot_yrs) ),
                           type = "response", se = TRUE)


# combine with the sex_ratio df
# *FLAG* bind_cols goes by col position, make sure survey is first
bind_cols(
  byyear %>% filter(!(Source == "Pot fishery")),
  #do.call cbinds each vector in the predict() output list 
  bind_rows(tibble::as_tibble(do.call(cbind, srv_predyear))%>% mutate(source_check = "LL survey"),
            tibble::as_tibble(do.call(cbind, fsh_ll_predyear))%>% mutate(source_check = "LL fishery") ) 
) %>% mutate(proportion = as.numeric(proportion))-> byyear_nopot
str(byyear_nopot)

# plots

ggplot(data = byyear_nopot, aes(x = year)) +
  geom_line(aes(y = fit, col = Source), size = 1) +
  geom_ribbon(aes(ymin = fit - se.fit*2, ymax = fit + se.fit*2, 
                  fill = Source),  alpha = 0.2) +
  geom_point(data=byyear, aes(y = proportion, col = Source)) +  
  #geom_point(data=byyear %>% filter(Source == "Pot fishery"),
  #           aes(x=year,y=proportion, col = "green")) +
  expand_limits(y = c(0.0, 1)) +
  geom_hline(yintercept = 0.5, lty = 2, col = "grey") +
  xlab("\nYear") +
  ylab("Proportion of females\n") +
  scale_colour_viridis_d(option="D",begin=0,end=0.75) +
  scale_fill_viridis_d(option="D",begin=0,end=0.5) +
  #scale_colour_manual(values = c("black", "grey")) +
  #scale_fill_manual(values = c("black", "grey")) +
  theme(legend.position = "none") -> byyear_plot

plot_grid(byage_plot, byyear_plot, align = c("h"), ncol = 1)
ggsave(paste0(YEAR+1,"/figures/sex_ratios_", YEAR, ".png"), dpi=300,  height=6, width=7, units="in")

#===============================================================================================
# Age compositions ----

# Combine survey and fishery data for age comp analysis
?quos

#quos() uses stand eval in dplyr, eval cols with nonstand eval using !!!
cols <- quos(Source, year, Sex, age) 

bind_rows(fsh_bio_noOL %>% filter(Gear == "Longline") %>%
            mutate(Source = "LL fishery") %>% select(!!!cols), 
          fsh_bio_noOL %>% filter(Gear == "Pot") %>%
            mutate(Source = "Pot fishery") %>% select(!!!cols)) %>%
  bind_rows(srv_bio_noOL %>% mutate(Source = "LL survey") %>% select(!!!cols)) %>% 
  bind_rows(potsrv_bio %>% mutate(Source = "Pot survey") %>% select(!!!cols)) %>% 
  filter(Sex %in% c('Female', 'Male') & !is.na(age)) %>% 
  droplevels() %>% 
  mutate(age = ifelse(age >= plus_group, plus_group, age)) %>% 
  filter(age >= 2) -> all_bio  # Plus group
view(all_bio)

# Sensitivity for 2019 YPR model on age-2s
all_bio %>% 
  filter(age < 10) %>% 
  group_by(year, age) %>% 
  dplyr::summarise(n = n()) %>% 
  arrange(age) %>% 
  pivot_wider(names_from = age, values_from = n, values_fill = list(n = 0)) %>% 
  print(n = Inf)
# a lot of years don't have any age-2s
# used for YPR analysis in 2019 to examine the impact of leaving out age-2s
# (must uncomment following line to rerun analysis):
# all_bio <- all_bio %>% filter(!c(year == YEAR & age == 2)) 

# Age comps (sex-specific)
all_bio %>% 
  count(Source, Sex, year, age) %>%
  group_by(Source, Sex, year) %>% 
  mutate(proportion = round( n / sum(n), 5)) %>% 
  bind_rows(all_bio %>% # Age comps (sexes combined)
          count(Source, year, age) %>%
          group_by(Source, year) %>% 
          mutate(proportion = round( n / sum(n), 5),
                 Sex = "Sex combined")) -> agecomps  

# Years with pot bio data
# potsrv_bio %>% 
#   filter(!is.na(age) & !is.na(Sex)) %>% 
#   distinct(year) -> pot_yrs

# complete() was behaving weirdly. Expand to grid to include all age combos
expand.grid(year = unique(agecomps$year), 
            Source = unique(agecomps$Source),
            Sex = unique(agecomps$Sex),
            age = seq(2, plus_group, 1))  %>% 
  data.frame()  %>% 
  full_join(agecomps) %>%
  fill_by_value(n, proportion, value = 0) %>% 
  mutate(Age = factor(age),
         proportion = round(proportion, 5)) %>%
  # Keep only relevant years for each Source
  filter(c(Source == "LL fishery" & year >= 2002) |
           c(Source == "LL survey" & year >= 1997) |
           c(Source == "Pot fishery" & year >= 2022)) -> agecomps

# Check that they sum to 1
agecomps %>% 
  group_by(Source, Sex, year) %>% 
  summarise(sum(proportion)) %>% 
  print(n = Inf)

# Sample sizes by source/year/sex
agecomps %>% 
  group_by(Source, year, Sex) %>% 
  dplyr::summarize(n = sum(n)) %>% 
  arrange(year) %>% 
  pivot_wider(names_from = year, values_from = n, values_fill = list(n = 0)) %>% 
  write_csv(paste0(YEAR+1,"/output/n_agecomps_plsgrp", plus_group, "_", YEAR, ".csv"))

# Age comp matrix
agecomps %>% write_csv(paste0(YEAR+1,"/output/agecomps_plsgrp", plus_group, "_", YEAR, ".csv"))

# Bargraph for presentation
agecomps %>% 
  filter(year == YEAR & Source == "LL fishery" &
           Sex %in% c("Male", "Female")) %>% 
  ggplot(aes(age, proportion, fill = Sex)) +
  geom_bar(stat = "identity",
           position = "dodge") +
           # position = position_dodge(preserve = "single")) +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_x_continuous(breaks = seq(min(agecomps$age), max(agecomps$age), 4), 
                     labels =  seq(min(agecomps$age), max(agecomps$age), 4)) +
  labs(x = "\nAge", y = "Proportion\n") +
  theme(legend.position = c(0.7, 0.9))

ggsave(paste0(YEAR+1,"/figures/agecomp_bargraph_ll_fsh_", YEAR, ".png"), 
              dpi=300, height=3, width=9, units="in")

# All years smoothed by source
agecomps %>% 
  filter(age < plus_group & Sex == "Sex combined") %>% 
ggplot(aes(x = age, y = proportion, colour = Source, linetype = Source)) +
  geom_point(size = 1, alpha = 0.1) +
  stat_smooth(size = 1, se = FALSE) +
  scale_colour_grey() +
  # scale_colour_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb")) +
  scale_y_continuous(limits = c(0, 0.1),
                     breaks = round(seq(min(agecomps$proportion), 0.1, 0.02), 2), 
                     labels =  round(seq(min(agecomps$proportion), 0.1, 0.02), 2)) +
  xlab('\nAge') +
  ylab('Proportion\n') +
  theme(legend.position = c(0.8, 0.8))

ggsave(paste0(YEAR+1,"/figures/agecomp_bydatasource.png"), 
       dpi=300, height=5, width=5, units="in")

# bubble plots filled circles

# survey
agecompdat <- agecomps %>% 
  filter(Sex %in% c("Female", "Male") &
           Source %in% c("LL survey") &
           year >= 1997 &
           age <= plus_group) %>% 
  ungroup()

# axisx <- tickr(agecompdat, year, 3)
# axisy <- tickr(agecompdat, age, 5)

ggplot(data = agecompdat,
       aes(x = year, y = age, size = proportion)) + #*FLAG* could swap size with proportion_scaled
  geom_point(shape = 21, fill = "black", colour = "black") +
  scale_size(range = c(0, 4)) +
  facet_wrap(~ Sex) +
  labs(x = "\nYear", y = "Observed age\n") +
  guides(size = FALSE)# +
  # scale_x_continuous(breaks = axisx$breaks, labels = axisx$labels) +
  # scale_y_continuous(breaks = axisy$breaks, labels = axisy$labels)

ggsave(paste0(YEAR+1,"/figures/bubble_survey_agecomp_byyear.png"), dpi=300, height=5, width=7.5, units="in")

# fishery
agecompdat <- agecomps %>% 
  filter(Sex %in% c("Female", "Male") &
           Source %in% c("LL fishery") &
           year >= 2002 &
           age <= plus_group) %>% 
  ungroup()

# axisx <- tickr(agecompdat, year, 3)
# axisy <- tickr(agecompdat, age, 5)

ggplot(data = agecompdat,
       aes(x = year, y = age, size = proportion)) + #*FLAG* could swap size with proportion_scaled
  geom_point(shape = 21, colour = "black", fill = "black") +
  scale_size(range = c(0, 4)) +
  facet_wrap(~ Sex) +
  labs(x = "\nYear", y = "Observed age\n") +
  guides(size = FALSE) #+
  # scale_x_continuous(breaks = axisx$breaks, labels = axisx$labels) +
  # scale_y_continuous(breaks = axisy$breaks, labels = axisy$labels)

ggsave(paste0(YEAR+1,"/figures/bubble_fishery_agecomp_byyear.png"), 
       dpi=300, height=5, width=7.5, units="in")

# Length compositions ----

# Pers. comm. K. Fenske 2018-01-05: NMFS uses length bins 41, 43, 45 ... 99.
# These bins represent the center of the bin, so a 43 bin represents fish
# 42-43.9 cm. They omit fish smaller than 40 and fish larger than 100 cm are
# lumped into the 100 bin. I've maintained these conventions for easy
# comparison:
bind_rows(srv_bio_noOL %>% 
            filter(year >= 1997 &
                     Sex %in% c("Female", "Male") &
                     !is.na(length)) %>% 
            select(year, Sex, length) %>% 
            mutate(Source = "LL survey"),
          fsh_bio_noOL %>% 
            filter(year >= 2002 &
                     Sex %in% c("Female", "Male") &
                     !is.na(length),
                   Gear == "Longline") %>% 
            select(year, Sex, length) %>% 
            mutate(Source = "LL fishery"),
          fsh_bio_noOL %>% 
            filter(year >= 2002 &
                     Sex %in% c("Female", "Male") &
                     !is.na(length),
                   Gear == "Pot") %>% 
            select(year, Sex, length) %>% 
            mutate(Source = "Pot fishery")#,
          # potsrv_bio %>% 
          #   filter(Sex %in% c("Female", "Male") &
          #            !is.na(length)) %>% 
          #   select(year, Sex, length) %>% 
          #   mutate(Source = "Pot survey")
          ) %>% 
  filter(!c(length < 40)) %>% 
  mutate(length2 = ifelse(length < 41, 41,
                          ifelse(length > 99, 99, length)),
         length_bin = cut(length2, breaks = seq(39.9, 99.9, 2),
                          labels = paste(seq(41, 99, 2)))) %>% 
  select(-length2) -> lendat

lendat %>% 
  # Length comps by Source, year, and Sex 
  count(Source, Sex, year, length_bin) %>%
  group_by(Source, Sex, year) %>% 
#  mutate(proportion = round( n / sum(n), 4)) %>% 
  mutate(proportion = n / sum(n)) %>%
  bind_rows(lendat %>% # Sexes combined
              count(Source, year, length_bin) %>%
              group_by(Source, year) %>% 
              mutate(proportion = round( n / sum(n), 4),
                     Sex = "Sex combined")) -> lencomps

# complete() was behaving weirdly. Expand to grid to include all length combos
expand.grid(year = unique(lencomps$year), 
            Source = unique(lencomps$Source),
            Sex = unique(lencomps$Sex),
            length_bin = sort(unique(lendat$length_bin)))  %>% 
  data.frame()  %>% 
  full_join(lencomps) %>%
  fill_by_value(n, proportion, value = 0) %>% 
  mutate(#length_bin = factor(length_bin),
         proportion = round(proportion, 6)) %>%
  # Keep only relevant years for each Source
  filter(c(Source == "LL fishery" & year >= 2002) |
           c(Source == "LL survey" & year >= 1997) |
           c(Source == "Pot fishery" & year >= 2022) #|
           # c(Source == "Pot survey" & year %in% pot_yrs$year)
         ) -> lencomps

# Check that they sum to 1
lencomps %>% 
  group_by(Source, Sex, year) %>% 
  summarise(sum(proportion)) %>% View()

write_csv(lencomps, paste0(YEAR+1,"/output/lengthcomps_", YEAR, ".csv"))

str(lendat)
lendat %>% 
  # Mean length comp for comparison
  count(Source, Sex, length_bin) %>%
  group_by(Source, Sex) %>% 
  mutate(proportion = round( n / sum(n), 4)) %>% 
  arrange(Source, Sex, length_bin)  %>% 
  data.frame() %>%  
  complete(Source, length_bin,
           fill = list(n = 0, proportion = 0)) %>% 
  bind_rows(lendat %>% # Sexes combined
              count(Source, length_bin) %>%
              group_by(Source) %>% 
              mutate(proportion = round( n / sum(n), 4),
                     Sex = "Sex combined")) -> mu_lencomps

mu_lencomps %>% 
  group_by(Source, Sex) %>% 
  summarise(sum(proportion))

s_lencomps <- lencomps %>% 
  filter(Source == "LL survey")

f_lencomps <- lencomps %>% 
  filter(Source == "LL fishery")

# ggridge plots
mean(lendat$length)
median(lendat$length)

lendat %>% 
  filter(Source != "Pot survey") %>% 
  mutate(Source = derivedFactor("Survey" = Source == "LL survey",
                                "LL Fishery" = Source == "LL fishery",
                                "Pot Fishery" = Source == "Pot fishery",
                                .ordered = TRUE)) %>% 
  ggplot(aes(length, year, group = year, fill = year)) + 
  geom_density_ridges(aes(point_fill = year, point_color = year),
                      alpha = 0.3) +
  geom_vline(xintercept = median(lendat$length), linetype = 4) +
  xlim(40, 90) + 
  xlab("\nLength (cm)") + 
  ylab(NULL) +
  # scale_y_reverse() +
  theme(legend.position = "none") + 
  facet_wrap(~ Source)

ggsave(paste0(YEAR+1,"/figures/lengthcomp_ggridges_", YEAR, ".png"), 
       dpi=300, height=8, width=10, units="in")

# ggride plot for len dat by sex (for TMB inputs)

lendat %>% 
  filter(! c(Source %in% c("Pot survey", "LL fishery", "Pot fishery"))) %>% 
  mutate(Source = derivedFactor("Survey" = Source == "LL survey")) %>% 
  ggplot(aes(length, year, group = year, fill = year)) + 
  geom_density_ridges(aes(point_fill = year, point_color = year), alpha = 0.3) +
  #geom_vline(xintercept = 61, linetype = 4) + # L50
  xlim(40, 90) + 
  labs(x = "\nLength (cm)", y = "Year\n") +
  # scale_y_reverse() +
  theme(legend.position = "none") + 
  facet_wrap(~ Sex) +
  ggtitle("Survey")

ggsave(paste0(YEAR+1,"/figures/tmb/lencomp_srv_",YEAR,".png"), 
       dpi=300, height=8, width=10, units="in")

# fishery
lendat %>% 
  filter(! c(Source %in% c("Pot survey", "LL survey", "Pot fishery"))) %>% 
  mutate(Source = derivedFactor("Fishery" = Source == "LL fishery")) %>% 
  ggplot(aes(length, year, group = year, fill = year)) + 
  geom_density_ridges(aes(point_fill = year, point_color = year), alpha = 0.3) +
  # geom_vline(xintercept = 61, linetype = 4) + # L50
  xlim(40, 90) + 
  labs(x = "\nLength (cm)", y = "Year\n") +
  # scale_y_reverse() +
  theme(legend.position = "none") + 
  facet_wrap(~ Sex) +
  ggtitle("Longline Fishery")

ggsave(paste0(YEAR+1,"/figures/tmb/lencomp_fsh_",YEAR,".png"), 
       dpi=300, height=8, width=10, units="in")

# All years smoothed by source
ggplot() +
  geom_point(data = lencomps %>% 
               filter(Sex == "Sex combined"),
             aes(x = length_bin, y = proportion, 
                 colour = Source),
             size = 1, alpha = 0.2) +
  # stat_smooth(size = 1.5, se = FALSE) +
  geom_line(data = mu_lencomps %>% 
              filter(Sex == "Sex combined"),
            aes(x = length_bin, y = proportion, colour = Source, 
                group = Source, linetype = Source), size = 1) +
  scale_x_discrete(breaks = seq(41, 99, 6),
                   labels = seq(41, 99, 6)) +
  #scale_colour_grey() +
  scale_colour_viridis_d(option = "A", begin=0, end=0.75) +
  xlab('\nFork length (cm)') +
  ylab('Proportion\n') +
  theme(legend.position = c(0.8, 0.8))

ggsave(YEAR+1,"/figures/lengthcomp_bydatasource.png", 
       dpi=300, height=4.5, width=5, units="in")

# length comp figs, requested by AJ Lindley 2018-09-07
lencomps %>% 
  group_by(year, Source, Sex) %>% 
  dplyr::summarize(N = sum(n),
         label = paste0("n = ", prettyNum(N, big.mark = ","))) %>% 
  ungroup() %>% 
  mutate(length_bin = "91", proportion = 0.18) -> labels 

# For survey
ggplot(data = lencomps %>% 
             # Last 10 years of data
             filter(year >= YEAR - 10 & 
                      Sex != "Sex combined" &
                      Source == "LL survey"), 
           aes(x = length_bin, y = proportion)) + 
  geom_bar(stat = "identity", colour = "lightgrey", fill = "lightgrey", width = 0.8) +
  geom_line(data = lencomps %>% 
              # Compare all past years to this year
              filter(year == YEAR & 
                       Sex != "Sex combined" &
                       Source == "LL survey") %>% 
              select(-year),
            aes(x = length_bin, y = proportion, group = 1),
            colour = "black") +
  geom_text(data = labels %>% 
              filter(year >= YEAR - 10 & 
                       Sex != "Sex combined" &
                       Source == "LL survey"),
            aes(x = length_bin, y = proportion, label = label),
            size = 3, family = "Times") +
  scale_y_continuous(limits = c(0, 0.25),
                     breaks = round(seq(0, 0.2, 0.1), 2),
                     labels =  round(seq(0, 0.2, 0.1), 2)) +
  scale_x_discrete(breaks = seq(41, 99, 6),
                   labels = seq(41, 99, 6)) +
  facet_grid(year ~ Sex) +
  labs(x = "\nFork length (cm)", y = "Proportion-at-length (longline survey)\n") +
  theme(strip.placement = "outside") 

ggsave(paste0(YEAR+1,"/figures/llsrv_lencomps_", YEAR-10, "_", YEAR, ".png"), 
       dpi=300, height=8, width=6.5, units="in")

# For fishery
ggplot(data = lencomps %>% 
         # Last 10 years of data
         filter(year >= YEAR - 10 & 
                  Sex != "Sex combined" &
                  Source == "LL fishery"), 
       aes(x = length_bin, y = proportion)) + 
  geom_bar(stat = "identity", colour = "lightgrey", fill = "lightgrey", width = 0.8) +
  geom_line(data = lencomps %>% 
              # Compare all past years to this year
              filter(year == YEAR & 
                       Sex != "Sex combined" &
                       Source == "LL fishery") %>% 
              select(-year),
            aes(x = length_bin, y = proportion, group = 1),
            colour = "black") +
  geom_text(data = labels %>% 
              filter(year >= YEAR - 10 & 
                       Sex != "Sex combined" &
                       Source == "LL fishery"),
            aes(x = length_bin, y = proportion, label = label),
            size = 3, family = "Times") +
  scale_y_continuous(limits = c(0, 0.25),
                     breaks = round(seq(0, 0.2, 0.1), 2),
                     labels =  round(seq(0, 0.2, 0.1), 2)) +
  scale_x_discrete(breaks = seq(41, 99, 6),
                   labels = seq(41, 99, 6)) +
  facet_grid(year ~ Sex) +
  labs(x = "\nFork length (cm)", y = "Proportion-at-length (longline fishery)\n") +
  theme(strip.placement = "outside") 

ggsave(paste0(YEAR+1,"/figures/llfsh_lencomps_", YEAR-10, "_", YEAR, ".png"), 
       dpi=300, height=8, width=6.5, units="in")

# Summary stats output for length comps (requested by AJ Linsley 20180907)
lendat %>% 
  filter(Source %in% c("LL survey", "LL fishery","Pot fishery")) %>% 
  group_by(Source, Sex, year) %>% 
  dplyr::summarize(mean = mean(length),
            min = min(length),
            max = max(length)) %>% 
  mutate(variable = "Fork length") -> lensum

# axis <- tickr(lensum, year, 5)

names(wes_palettes)
pal <- wes_palette("GrandBudapest1", 4, type = "discrete")

lensum %>% 
  ggplot(aes(x = year, y = mean, colour = Source)) +
  geom_point() +
  geom_line() +
  scale_colour_manual(values = pal) +
  #scale_colour_grey(guide = FALSE) +
  facet_wrap(~ Sex) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = NULL, y = "Mean\nfork\nlength\n(cm)") +
  theme(axis.title.y = element_text(angle=0),legend.position = "none") -> l

#quos() uses stand eval in dplyr, eval cols with nonstand eval using !!!
cols <- quos(Source, year, Sex, age) 

bind_rows(
  fsh_bio %>% filter(Gear == "Longline") %>% 
    mutate(Source = "LL fishery") %>% select(!!!cols), 
  fsh_bio %>% filter(Gear == "Pot") %>% 
    mutate(Source = "Pot fishery") %>% select(!!!cols)) %>% 
  bind_rows(srv_bio %>% mutate(Source = "LL survey") %>% select(!!!cols)) %>% 
  filter(year >= 1997 & Sex %in% c('Female', 'Male') & !is.na(age)) %>% 
  group_by(Source, Sex, year) %>% 
  dplyr::summarize(mean = mean(age),
            min = min(age),
            max = max(age)) %>% 
  mutate(variable = "Age") -> agesum

agesum %>% 
  mutate(age = round(mean, 0)) -> agesum1

# axisy <- tickr(agesum1, age, 3)

agesum %>% 
  ggplot(aes(x = year, y = mean, colour = Source)) +
  geom_point() +
  geom_line() +
  scale_colour_manual(values = pal) +
  #scale_colour_grey() +
  facet_wrap(~ Sex) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  # scale_y_continuous(breaks = axisy$breaks, labels = axisy$labels) +
  labs(x = NULL, y = "Mean\nage\n(yrs)") +
  theme(legend.position = "bottom",
        axis.title.y = element_text(angle=0)) -> a

cowplot::plot_grid(l, a, axis = "lrtb", align = "hv", ncol = 1) -> compare_comp_sums
compare_comp_sums
ggsave(paste0(YEAR+1,"/figures/compare_comp_summaries.png"),
       plot = compare_comp_sums,
       # dpi=300, height=5.5, width=6.5, units="in")
       dpi=300, height=7, width=7, units="in")

bind_rows(agesum, lensum) %>% 
  write_csv(paste0(YEAR+1, "/output/comps_summary.csv"))

# survey length comps by stat area, requested by A Olson 2021-02-18
srv_bio %>% 
  filter(year >= 1997 &
           Sex %in% c("Female", "Male") &
           !is.na(length)) %>% 
  select(year, Stat, length) %>% 
  filter(!c(length < 40)) %>% 
  mutate(length2 = ifelse(length < 41, 41,
                          ifelse(length > 99, 99, length)),
         length_bin = cut(length2, breaks = seq(39.9, 99.9, 2),
                          labels = paste(seq(41, 99, 2)))) %>% 
  select(-length2) %>% 
  mutate(Stat = derivedFactor("345731" = Stat == "345731",
                              "345701" = Stat == "345701",
                              "345631" = Stat == "345631",
                              "345603" = Stat == "345603",
                              .ordered = TRUE)) -> statlen

statlen %>% 
  # Length comps by Source, year, and Sex 
  count(Stat, year, length_bin) %>%
  group_by(Stat, year) %>% 
  mutate(proportion = round( n / sum(n), 4)) -> statcomps

# complete() was behaving weirdly. Expand to grid to include all length combos
expand.grid(year = unique(statcomps$year), 
            Stat = unique(statcomps$Stat),
            length_bin = sort(unique(statlen$length_bin)))  %>% 
  data.frame()  %>% 
  full_join(statcomps) %>%
  fill_by_value(n, proportion, value = 0) %>% 
  mutate(proportion = round(proportion, 4)) -> statcomps

# Check that they sum to 1
statcomps %>% 
  group_by(Stat, year) %>% 
  summarise(sum(proportion)) %>% view

statcomps %>% 
  group_by(year, Stat) %>% 
  dplyr::summarize(N = sum(n),
                   label = paste0("n = ", prettyNum(N, big.mark = ","))) %>% 
  ungroup() %>% 
  mutate(length_bin = "91", proportion = 0.18) -> labels 

# For survey
ggplot(data = statcomps %>% 
         # Last 10 years of data
         filter(year >= YEAR - 10), 
       aes(x = length_bin, y = proportion)) + 
  geom_bar(stat = "identity", colour = "lightgrey", fill = "lightgrey", width = 0.8) +
  geom_line(data = statcomps %>% 
              # Compare all past years to this year
              filter(year == YEAR) %>% 
              select(-year),
            aes(x = length_bin, y = proportion, group = 1),
            colour = "black") +
  geom_text(data = labels %>% 
              filter(year >= YEAR - 10),
            aes(x = length_bin, y = proportion, label = label),
            size = 3, family = "Times") +
  scale_y_continuous(limits = c(0, 0.25),
                     breaks = round(seq(0, 0.2, 0.1), 2),
                     labels =  round(seq(0, 0.2, 0.1), 2)) +
  scale_x_discrete(breaks = seq(41, 99, 6),
                   labels = seq(41, 99, 6)) +
  facet_grid(year ~ Stat) +
  labs(x = "\nFork length (cm)", y = "Proportion-at-length (longline survey)\n") +
  theme(strip.placement = "outside") 

# ggridge plots

statlen %>% 
  filter(year >= YEAR - 5) %>% 
  ggplot(aes(length, year, group = year, fill = year)) + 
  geom_density_ridges(aes(point_fill = year, point_color = year),
                      alpha = 0.3) +
  xlim(40, 90) + 
  xlab("\nLength (cm)") + 
  ylab(NULL) +
  # scale_y_reverse() +
  theme(legend.position = "none") + 
  facet_wrap(~ Stat, ncol = 1)

statlen %>%
  filter(year >= YEAR - 10 & between(length, 40, 99)) %>%
  ggplot(aes(x = length, y = factor(year), group = interaction(year, Stat),
             fill = Stat)) + #, col = Stat)) +
  geom_density_ridges(alpha = 0.5, size = 0.5, col = "white") +
  # scale_fill_manual(values = c("grey30", "#00BFC4", "#da2055", "#daa520" )) +
  # scale_colour_manual(values = c("grey30", "#00BFC4", "#daa520", "#da2055")) +
  labs(x = "Length (cm)", y = NULL, fill = NULL, col = NULL) + #, title = "Dusky rockfish") +
  theme_light() +
  scale_x_continuous(limits = c(40, 99)) +
  theme(legend.position = "top")

ggsave(paste0(YEAR+1,"/figures/lengthcomp_statarea_", YEAR, ".png"), 
       dpi=300, height=8, width=10, units="in")

################################################################################
## SCRAP:
#----------------------------------------------------------------------------------
## Do the same thing now, but split up the fishery data by gear... 

# subsets by weight, age, sex
fsh_bio_noOL %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 2002 & # use same years as survey
           !is.na(age) &
           !is.na(weight)) %>% 
  droplevels() -> fsh_waa_sub

fsh_waa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Female", Gear == "Longline") -> fsh_waa_f_ll

fsh_waa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Male", Gear == "Longline") -> fsh_waa_m_ll

fsh_waa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Female", Gear == "Pot") -> fsh_waa_f_pot

fsh_waa_sub %>% 
  ungroup() %>% 
  filter(Sex == "Male", Gear == "Pot") -> fsh_waa_m_pot

# mle fit for females

# females
wvb_f_fsh_ll <- fishmethods::growth(unit = 2, # 1 = length, 2 = weight
                                    size = fsh_waa_f_ll$weight, age = fsh_waa_f_ll$age,
                                    error = 2, # 1 = additive, 2 = multiplicative log(w_i) = log(w_inf) + beta * log(1 - exp * (-k * (age_i - t0))) + error
                                    # starting values from Hanselman et al. 2007 (Appendix C, Table 5)
                                    Sinf = 5.5, K = 0.24, t0 = -1.4,
                                    B = allom_pars_fsh_gear %>% filter(Sex == "Female" & Parameter == "b" & Gear == "Longline") %>% pull(Estimate))
wvb_f_fsh_pot <- fishmethods::growth(unit = 2, # 1 = length, 2 = weight
                                     size = fsh_waa_f_pot$weight, age = fsh_waa_f_pot$age,
                                     error = 2, # 1 = additive, 2 = multiplicative log(w_i) = log(w_inf) + beta * log(1 - exp * (-k * (age_i - t0))) + error
                                     # starting values from Hanselman et al. 2007 (Appendix C, Table 5)
                                     Sinf = 5.5, K = 0.24, t0 = -1.4,
                                     B = allom_pars_fsh_gear %>% filter(Sex == "Female" & Parameter == "b" & Gear == "Pot") %>% pull(Estimate))
wvb_f_fsh # gompertz failed but that wasn't our target so we're good

# males
wvb_m_fsh_ll <- fishmethods::growth(unit = 2, # 1 = length, 2 = weight
                                    size = fsh_waa_m_ll$weight, age = fsh_waa_m_ll$age,
                                    error = 2, # 1 = additive, 2 = multiplicative log(w_i) = log(w_inf) + beta * log(1 - exp * (-k * (age_i - t0))) + error
                                    # starting values from Hanselman et al. 2007 (Appendix C, Table 5)
                                    Sinf = 5.5, K = 0.24, t0 = -1.4,
                                    B = allom_pars_fsh_gear %>% filter(Sex == "Male" & Parameter == "b" & Gear == "Longline") %>% pull(Estimate))
wvb_m_fsh_pot <- fishmethods::growth(unit = 2, # 1 = length, 2 = weight
                                     size = fsh_waa_m_pot$weight, age = fsh_waa_m_pot$age,
                                     error = 2, # 1 = additive, 2 = multiplicative log(w_i) = log(w_inf) + beta * log(1 - exp * (-k * (age_i - t0))) + error
                                     # starting values from Hanselman et al. 2007 (Appendix C, Table 5)
                                     Sinf = 5.5, K = 0.24, t0 = -1.4,
                                     B = allom_pars_fsh_gear %>% filter(Sex == "Male" & Parameter == "b" & Gear == "Pot") %>% pull(Estimate))

# save param estimates
lvb_pars_2fleet %>% bind_rows(
  as_tibble(summary(wvb_f_fsh_ll$vout)$parameters[,1:2]) %>% 
    mutate(Parameter = c("w_inf", "k", "t0"),
           Sex = "Female",
           Gear = "Longline",
           Source = "EQS longline fishery") %>% 
    bind_rows(as_tibble(summary(wvb_m_fsh_ll$vout)$parameters[,1:2]) %>% 
                mutate(Parameter = c("w_inf", "k", "t0"),
                       Sex = "Male",
                       Gear = "Longline",
                       Source = "EQS longline fishery")) %>% 
    bind_rows(as_tibble(summary(wvb_f_fsh_pot$vout)$parameters[,1:2]) %>% 
                mutate(Parameter = c("w_inf", "k", "t0"),
                       Sex = "Female",
                       Gear = "Pot",
                       Source = "EQS pot fishery")) %>%
    bind_rows(as_tibble(summary(wvb_m_fsh_pot$vout)$parameters[,1:2]) %>% 
                mutate(Parameter = c("w_inf", "k", "t0"),
                       Sex = "Male",
                       Gear = "Pot",
                       Source = "EQS pot fishery")) %>% 
    mutate(#Source = "EQS longline fishery",
      Years = paste0(min(fsh_waa_sub$year), "-", max(fsh_waa_sub$year)),
      Region = "Chatham Strait",
      Function = "Weight-based LVB") %>% 
    full_join(fsh_waa_sub %>% 
                group_by(Sex,Gear) %>% 
                summarise(n = n()))) -> lvb_pars_2fleet
view(lvb_pars_2fleet)


# Save weight-at-age predictions -----

# unfortunately, while the fishmethods package makes it easy to do everything
# else, it does not make it easy to extract model predictions from the growth
# curves when using the multiplicative error structure (the one that assumes
# residuals are lognormally distributed). As such, we have to extract the
# coefficients and do the predictions manually. Part of this is applying a
# log-normal bias correction, which gets us the mean (instead of median)
# predicted value. This correction is E(w_i) = exp(mu_i + 0.5 * sigma^2), where
# mu is the predicted value in log space (i.e. log(w_i) in the formula below):

# log(w_i) = log(w_inf) + beta * log(1 - exp * (-k * (age_i - t0))) + error

coef_f_wvb <- summary(wvb_f$vout)$parameter[,1] # survey females
coef_f_wvb_fsh_ll <- summary(wvb_f_fsh_ll$vout)$parameter[,1] # ll fishery females
coef_f_wvb_fsh_pot <- summary(wvb_f_fsh_pot$vout)$parameter[,1] # pot fishery females
beta_f_ll <- allom_pars_fsh_gear %>% filter(Sex == "Female" & Parameter == "b" & Gear == "Longline") %>% pull(Estimate) # beta from allometric model (weight-length relationship)
beta_f_pot <- allom_pars_fsh_gear %>% filter(Sex == "Female" & Parameter == "b" & Gear == "Pot") %>% pull(Estimate)

coef_m_wvb <- summary(wvb_m$vout)$parameter[,1] # survey males
coef_m_wvb_fsh_ll <- summary(wvb_m_fsh_ll$vout)$parameter[,1] # ll fishery males
coef_m_wvb_fsh_pot <- summary(wvb_m_fsh_pot$vout)$parameter[,1] # pot fishery males
beta_m_ll <- allom_pars_fsh_gear %>% filter(Sex == "Male" & Parameter == "b" & Gear == "Longline") %>% pull(Estimate) # beta from allometric model (weight-length relationship)
beta_m_pot <- allom_pars_fsh_gear %>% filter(Sex == "Male" & Parameter == "b" & Gear == "Pot") %>% pull(Estimate)

#coef_wvb <- summary(wvb$vout)$parameter[,1] # survey sexes combined
#coef_wvb_fsh <- summary(wvb_fsh$vout)$parameter[,1] # fishery sexes combined
#beta_c <- allom_pars %>% filter(Sex == "Combined" & Parameter == "b") %>% pull(Estimate) # beta from allometric model (weight-length relationship)

waa_preds_2fleets <- age_pred %>% 
  mutate(Sex = "Female",
         round_kg = exp(log(coef_f_wvb[1]) + beta_f * log(1 - exp(-coef_f_wvb[2] * (age_pred$age - coef_f_wvb[3]))) +
                          0.5 * (sigma(wvb_f$vout)^2))) %>% 
  bind_rows(age_pred %>% 
              mutate(Sex = "Male",
                     round_kg = exp(log(coef_m_wvb[1]) + beta_m * log(1 - exp(-coef_m_wvb[2] * (age_pred$age - coef_m_wvb[3]))) +
                                      0.5 * (sigma(wvb_m$vout)^2)))) %>% 
  #bind_rows(age_pred %>% 
  #            mutate(Sex = "Combined",
  #                   round_kg = exp(log(coef_wvb[1]) + beta_c * log(1 - exp(-coef_wvb[2] * (age_pred$age - coef_wvb[3]))) +
  #                                    0.5 * (sigma(wvb$vout)^2)))) %>% 
  mutate(Source = "LL survey") %>% 
  # Fishery waa
  bind_rows(age_pred %>% 
              mutate(Sex = "Female",
                     round_kg = exp(log(coef_f_wvb_fsh_ll[1]) + beta_f_ll * log(1 - exp(-coef_f_wvb_fsh_ll[2] * (age_pred$age - coef_f_wvb_fsh_ll[3]))) +
                                      0.5 * (sigma(wvb_f_fsh_ll$vout)^2))) %>% 
              bind_rows(age_pred %>% 
                          mutate(Sex = "Male",
                                 round_kg = exp(log(coef_m_wvb_fsh_ll[1]) + beta_m_ll * log(1 - exp(-coef_m_wvb_fsh_ll[2] * (age_pred$age - coef_m_wvb_fsh_ll[3]))) +
                                                  0.5 * (sigma(wvb_m_fsh_ll$vout)^2)))) %>% 
              mutate(Source = "LL fishery") ) %>%
  bind_rows(age_pred %>% 
              mutate(Sex = "Female",
                     round_kg = exp(log(coef_f_wvb_fsh_pot[1]) + beta_f_pot * log(1 - exp(-coef_f_wvb_fsh_pot[2] * (age_pred$age - coef_f_wvb_fsh_pot[3]))) +
                                      0.5 * (sigma(wvb_f_fsh_pot$vout)^2))) %>% 
              bind_rows(age_pred %>% 
                          mutate(Sex = "Male",
                                 round_kg = exp(log(coef_m_wvb_fsh_pot[1]) + beta_m_ll * log(1 - exp(-coef_m_wvb_fsh_pot[2] * (age_pred$age - coef_m_wvb_fsh_pot[3]))) +
                                                  0.5 * (sigma(wvb_m_fsh_pot$vout)^2)))) %>% 
              mutate(Source = "Pot fishery") )

write_csv(waa_preds_2fleets, paste0(YEAR+1,"/output/pred_waa_plsgrp_2fleets_", plus_group, "_", YEAR, ".csv"))

# Compare empirical and predicted weight-at-age
ggplot() +
  geom_point(data = emp_waa_2fleet %>% filter(!is.na(Source)), 
             aes(x = age, y = weight, col = Source, shape = Sex)) +
  geom_line(data = waa_preds_2fleets,
            aes(x = age, y = round_kg, col = Source, linetype = Sex), size = 1) +
  scale_colour_grey() +
  # expand_limits(y = 0) +
  labs(x = "\nAge", y = "Weight (kg)\n", linetype = "Sex", shape = "Sex")

ggsave(paste0(YEAR+1,"/figures/compare_empirical_predicted_waa_2fleet_", YEAR, ".png"), 
       dpi=300, height=4, width=6, units="in")

# Compare growth results ----

# Comparison of Hanselman et al. 2007 values with the Chatham Strait longline
# survey. Units: length (cm), weight (kg), and age (yrs)

# for MArch 2 2023 pick up here... 

bind_rows(allom_pars_fsh_gear, lvb_pars_2fleet %>% rename(SE = `Std. Error`)) %>% 
  mutate(Notes = "seak_sablefish/code/biological.r") %>% 
  bind_rows(noaa_lvb %>% rename(Notes = Source) %>% rename(Source = Survey) %>%
              mutate(Gear = "Longline")) -> lvb_comp_2fleet; view(lvb_comp_2fleet)

write_csv(lvb_comp_2fleet, paste0(YEAR+1,"/output/compare_vonb_adfg_noaa_2fleet_", YEAR, ".csv"))

