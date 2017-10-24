# work up of survey and fishery biological data
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2017-10-16

source("r_code/helper.r")
source("r_code/functions.r")

# data -----

# survey biological  data
read_csv("data/survey/llsurvey_bio_1988_2016.csv") %>% 
  mutate(Year = factor(year),
         Project = factor(Project),
         Stat = factor(Stat),
         Station = factor(Station),
         Sex = factor(Sex),
         Maturity = factor(Maturity)) %>% 
  group_by(Year, Stat) %>% 
  mutate(n = length(age),
         length_mu = mean(length, na.rm = TRUE),
         weight_mu = mean(weight, na.rm = TRUE)) -> srv_bio

# Hanselman et al. 2007 Appendix GOA Sablefish SAFE Appendix 3C von bertalanffy
# parameter estimates (length and weight) for comparison with estimates from the
# survey. The coastwide parameters are still used for management, but Southeast
# slope are informative.
noaa_lvb <- read_csv("data/survey/noaa_lvb_params_hanselman2007.csv")

# Fishery biological data
read_csv("data/fishery/fishery_bio_2000_2016.csv") %>%
  mutate(Year = factor(year),
         Project_cde = factor(Project_cde),
         Adfg = factor(Adfg),
         Stat = factor(Stat),
         Sex_cde = factor(Sex_cde),
         Sex = factor(Sex),
         Maturity = factor(Maturity),
         Maturity_cde = factor(Maturity_cde),
         Discard_status_cde = factor(Discard_status_cde)) %>% 
  group_by(Year, Stat) %>% 
  mutate(n = length(age),
         length_mu = mean(length, na.rm = TRUE),
         weight_mu = mean(weight, na.rm = TRUE)) -> fsh_bio

# Pot survey biological data
read_csv("data/survey/potsurvey_bio_2009_2015.csv", guess_max = 50000) %>% 
  mutate(Year = factor(year),
         Project_cde = factor(Project_cde),
         Stat = factor(Stat),
         Sex = factor(Sex),
         Maturity_cde = factor(Maturity_cde))  %>% 
  filter(Sex %in% c('Female', 'Male') & !is.na(age)) %>% 
  droplevels() -> potsrv_bio

# Length-based Ludwig von Bertalanffy growth model -----

# subsets by length, age, sex
srv_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & # *FLAG* advent of "modern" survey
           !is.na(length) &
           !is.na(age)) %>% 
  droplevels() -> laa_sub

laa_sub %>% ungroup() %>% filter(Sex == "Female") -> laa_f
laa_sub %>% ungroup() %>% filter(Sex == "Male") -> laa_m

#sex-specific starting values from Hanselman et al. 2007 (Appendix C, Table 1),
#except sigma
start_f <- c(l_inf = 80, k = 0.22, t0 = -1.9, sigma = 10) 
start_m <- c(l_inf = 68, k = 0.29, t0 = -2.3, sigma = 10)

# mle fit for females
vb_mle_f <- vonb_len(obs_length = laa_f$length,
                   age = laa_f$age,
                   starting_vals = start_f,
                   sex = "Female")

# mle fit for males
vb_mle_m <- vonb_len(obs_length = laa_m$length,
                   age = laa_m$age,
                   starting_vals = start_m,
                   sex = "Male")

# combine predictions and parameter estimates and plot mle
pred <- rbind(vb_mle_f$predictions, vb_mle_m$predictions) %>% 
  mutate(std_resid = resid/sd(resid))


lvb_pars <- full_join(
  rbind(vb_mle_f$results, vb_mle_m$results) %>% 
  mutate(Survey = "ADF&G Longline",
         Years = paste0(min(laa_sub$year), "-", max(laa_sub$year)),
         Region = "Chatham Strait",
         Function = "Length-based LVB")
  ,
  laa_sub %>% group_by(Sex) %>% summarise(n = n()),
  by = "Sex")

png("figures/length_vonb_chathamllsurvey_1997_2016.png", height = 4, width = 6, units = "in", res = 300)
ggplot() +
  geom_jitter(data = laa_sub, aes(x = age, y = length, col = Sex, shape = Sex)) +
  geom_line(data = pred, aes(x = age, y = pred, col = Sex, group = Sex), lwd = 2 ) + #"#00BFC4"
  geom_line(data = pred, aes(x = age, y = pred, group = Sex), col = "black" ) + #"#00BFC4"
  xlab("\nAge (yrs)") +
  ylab("Length (cm)\n")
dev.off()

# residual plots
ggplot(data = pred) + 
  geom_histogram(aes(x = std_resid)) +
  facet_wrap(~ Sex)

ggplot(data = pred) + 
  geom_point(aes(x = age, y = std_resid)) +
  geom_hline(aes(yintercept = 0), linetype = 2, col = "red") + 
  facet_wrap(~ Sex)

ggplot(data = pred) + 
  geom_point(aes(x = pred, y = std_resid)) +
  geom_hline(aes(yintercept = 0), linetype = 2, col = "red") + 
  facet_wrap(~ Sex)

# Weight-length allometry W = alpha * L ^ beta ----

# KVK assumed beta = 3, but there are data to estimate this value (e.g., Hanselman et
# al. 2007 values beta_f = 3.02 and beta_m = 2.96 are used in the current
# assessment)


# subsets by weight, age, sex
srv_bio %>% 
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

# parameter estimates and plot fit 
beta_m <- tidy(male_fit)$estimate[2]
beta_f <- tidy(fem_fit)$estimate[2]

allom_pars <- full_join(
  rbind(tidy(male_fit) %>% mutate(Sex = "Male"),
        tidy(fem_fit) %>% mutate(Sex = "Female")
  ) %>% 
    select(Parameter = term, Estimate = estimate, SE = std.error, Sex) %>% 
    mutate(Survey = "ADF&G Longline",
           Years = paste0(min(laa_sub$year), "-", max(laa_sub$year)),
           Region = "Chatham Strait",
           Function = "Allometric")
  ,
  allom_sub %>% group_by(Sex) %>% summarise(n = n()),
  by = "Sex")

png("figures/allometry_chathamllsurvey_1997_2016.png", height = 4, width = 6, units = "in", res = 300)
ggplot(allom_sub,  
       aes(x = length, y = weight, col = Sex, shape = Sex)) +
  geom_jitter() + 
  stat_function(fun = lw_allometry, 
                args = as.list(tidy(fem_fit)$estimate),
                lwd = 1.5, col = "salmon") + 
  stat_function(fun = lw_allometry, 
                args = as.list(tidy(male_fit)$estimate),
                lwd = 1.5, col = "#00BFC4", lty = 2) + 
  xlab("\nLength (cm)") +
  ylab("Weight (kg)\n")
dev.off()
  
# Weight-based Ludwig von Bertalanffy growth model ----

# subsets by weight, age, sex
srv_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & # *FLAG* advent of "modern" survey
           !is.na(age) &
           !is.na(weight)) %>% 
  droplevels() -> waa_sub

waa_sub %>% ungroup() %>% filter(Sex == "Female") -> waa_f
waa_sub %>% ungroup() %>% filter(Sex == "Male") -> waa_m

# fit weight-based lvb with a multiplicative error structure using max likelihood estimation
# log(w_i) = log(w_inf) + beta * log(1 - exp * (-k * (age_i - t0))) + error

# starting values from Hanselman et al. 2007 Appendix C Table 5
start_f <- c(w_inf = 5.5, k = 0.24, t0 = -1.4, sigma = 10)
start_m <- c(w_inf = 3.2, k = 0.36, t0 = -1.1, sigma = 10)

# mle fit for females
wvb_mle_f <- vonb_weight(obs_weight = waa_f$weight,
                   age = waa_f$age,
                   b = beta_f,
                   starting_vals = start_f,
                   sex = "Female")

# mle fit for males
wvb_mle_m <- vonb_weight(obs_weight = waa_m$weight,
                   age = waa_m$age,
                   b = beta_m, 
                   starting_vals = start_m,
                   sex = "Male")


# combine predictions and parameter estimates and plot fitted values
pred <- rbind(wvb_mle_f$predictions, wvb_mle_m$predictions) %>% 
  mutate(std_resid = resid/sd(resid))


wvb_pars <- full_join(
  rbind(wvb_mle_f$results, wvb_mle_m$results) %>% 
    mutate(Survey = "ADF&G Longline",
           Years = paste0(min(waa_sub$year), "-", max(waa_sub$year)),
           Region = "Chatham Strait",
           Function = "Weight-based LVB")
  ,
  waa_sub %>% group_by(Sex) %>% summarise(n = n()),
  by = "Sex")

png("figures/weight_vonb_chathamllsurvey_1997_2016.png", height = 4, width = 6, units = "in", res = 300)
ggplot() +
  geom_jitter(data = waa_sub, aes(x = age, y = weight, col = Sex, shape = Sex)) +
  geom_line(data = pred, aes(x = age, y = pred, col = Sex, group = Sex), lwd = 2 ) + #"#00BFC4"
  geom_line(data = pred, aes(x = age, y = pred, group = Sex), col = "black" ) + #"#00BFC4"
  xlab("\nAge (yrs)") +
  ylab("Weight (kg))\n")
dev.off()

# residual plots
ggplot(data = pred) + 
  geom_histogram(aes(x = std_resid)) +
  facet_wrap(~ Sex)

ggplot(data = pred) + 
  geom_point(aes(x = age, y = std_resid)) +
  geom_hline(aes(yintercept = 0), linetype = 2, col = "red") + 
  facet_wrap(~ Sex)

ggplot(data = pred) + 
  geom_point(aes(x = pred, y = std_resid)) +
  geom_hline(aes(yintercept = 0), linetype = 2, col = "red") + 
  facet_wrap(~ Sex)

# Compare growth results ----

# Comparison of Hanselman et al. 2007 values with the Chatham Strait longline
# survey. Units: length (cm), weight (kg), and age (yrs)

rbind(allom_pars,
      rbind(lvb_pars, wvb_pars)) %>%
      mutate(Source = "seak_sablefish/code/biological.r") -> adfg_lvb 
      
write_csv(rbind(noaa_lvb, adfg_lvb) , 
          "output/compare_vonb_adfg_noaa.csv")


# Sex ratios ----

# proportion of females by age in survey and fishery

# restrict age range
aa <- c(2:42)

bind_rows(
  f_sex_ratio(data = filter(srv_bio, age %in% aa), 
              src = "LL survey", age),
  f_sex_ratio(data = filter(fsh_bio, age %in% aa), 
              src = "LL fishery", age)
) -> byage

# get generalized additive model fits and predictions
# survey
srv_fitage <- gam(I(Sex == "Female") ~ s(age), 
               data = filter(srv_bio, age %in% aa,
                             Sex %in% c("Female", "Male")),
               family = "quasibinomial")

srv_predage <- predict(srv_fitage, newdata = data.frame(age = aa),
                       type = "response", se = TRUE)

# fishery
fsh_fitage <- gam(I(Sex == "Female") ~ s(age), 
               data = filter(fsh_bio, age %in% aa,
                             Sex %in% c("Female", "Male")),
               family = "quasibinomial")

fsh_predage <- predict(fsh_fitage, newdata = data.frame(age = aa),
                       type = "response", se = TRUE)

# combine with the sex_ratio df
# *FLAG* bind_cols goes by col position, make sure survey is first
bind_cols(
  byage,
  #do.call cbinds each vector in the predict() output list 
  bind_rows(tbl_df(do.call(cbind, srv_predage))%>% mutate(source_check = "LL survey"),
            tbl_df(do.call(cbind, fsh_predage))%>% mutate(source_check = "LL fishery") ) 
  ) -> byage

# plot
png("figures/proportion_fembyage.png", height = 4, width = 6, units = "in", res = 300)
ggplot(data = byage, aes(x = age)) +
  geom_line(aes(y = fit, col = Source), size = 1) +
  geom_ribbon(aes(ymin = fit - se.fit*2, ymax = fit + se.fit*2, fill = Source, col = Source),  alpha = 0.2) +
  geom_point(aes(y = proportion, col = Source)) +  
  expand_limits(y = c(0.30, 0.8)) +
  xlab("\nAge") +
  ylab("Proportion of females\n") 
dev.off()


# proportion of females by year in the fishery and survey

bind_rows(
  f_sex_ratio(data = filter(srv_bio), 
              src = "LL survey", year),
  f_sex_ratio(data = filter(fsh_bio), 
              src = "LL fishery", year)
) -> byyear

# get generalized additive model fits and predictions
# survey
srv_fityear <- gam(I(Sex == "Female") ~ s(year, k = 4), 
                  data = filter(srv_bio, Sex %in% c("Female", "Male")),
                  family = "quasibinomial")

srv_yrs <- byyear %>% filter(Source == "LL survey") %>% select(year) %>% range()

srv_predyear <- predict(srv_fityear, 
                        newdata = data.frame(year = min(srv_yrs):max(srv_yrs) ),
                       type = "response", se = TRUE)

# fishery
fsh_fityear <- gam(I(Sex == "Female") ~ s(year, k = 4), 
                   data = filter(fsh_bio, Sex %in% c("Female", "Male")),
                   family = "quasibinomial")


fsh_yrs <- byyear %>% filter(Source == "LL fishery") %>% select(year) %>% range()

fsh_predyear <- predict(fsh_fityear, 
                        newdata = data.frame(year = min(fsh_yrs):max(fsh_yrs) ),
                        type = "response", se = TRUE)

# combine with the sex_ratio df
# *FLAG* bind_cols goes by col position, make sure survey is first
bind_cols(
  byyear,
  #do.call cbinds each vector in the predict() output list 
  bind_rows(tbl_df(do.call(cbind, srv_predyear))%>% mutate(source_check = "LL survey"),
            tbl_df(do.call(cbind, fsh_predyear))%>% mutate(source_check = "LL fishery") ) 
) -> byyear

# plot
png("figures/proportion_fembyyear.png", height = 4, width = 6, units = "in", res = 300)
ggplot(data = byyear, aes(x = year)) +
  geom_line(aes(y = fit, col = Source), size = 1) +
  geom_ribbon(aes(ymin = fit - se.fit*2, ymax = fit + se.fit*2, 
                  fill = Source, col = Source),  alpha = 0.2) +
  geom_point(aes(y = proportion, col = Source)) +  
  scale_x_continuous(breaks = seq(min(byyear$year), max(byyear$year), 2), 
                     labels =  seq(min(byyear$year), max(byyear$year), 2)) +
  xlab("") +
  ylab("Proportion of females\n") +
  theme(axis.text.x = element_text(size=10, angle=45, hjust=1))
dev.off()

## proportion of females by year and age in survey and fishery

bind_rows(
  f_sex_ratio(data = filter(srv_bio, age %in% aa), 
              src = "LL survey", year, age),
  f_sex_ratio(data = filter(fsh_bio, age %in% aa), 
              src = "LL fishery", year, age)
) -> byyrage

# Age compositions ----

# pot survey

potsrv_bio %>% ungroup() %>%
  count(Sex, year) %>%
  group_by(year) %>% 
  mutate(proportion = round(n / sum(n), 2))

srv_bio %>% ungroup() %>% 
  filter(Sex %in% c("Female", "Male") &
           !is.na(age)) %>% 
  droplevels() %>% 
  count(Sex, year) %>% 
  group_by(year) %>% 
  mutate(proportion = round(nn / sum(nn), 2))

unique(potsrv_bio$year)


#OLD ----

###############################################################################
# LIBRARIES
################################################################################
library(plyr)
library(reshape2)
library(lattice)
library(latticeExtra)
library(gridExtra)
library(ggplot2)
library(lubridate)
library(gam)
library(mgcv)


#----------------------------------------------------------------
# Pot survey 2003 - 2009
# Update as needed
# Determine sex %; gender determined onboard, not ADU
# Sex data only through 2009 at present
#----------------------------------------------------------------
# CC.pot<-read.table("data/pot_sex.csv",header=TRUE,sep=",")
CC.pot <- read_csv("~/Kray/Sablefish/2017/data/pot_sex.csv")

k <- is.element(CC.pot$SEX_CODE, c(1,2))
k <- is.element(CC.pot$SEX_CODE, c(1,2))
datgen <- CC.pot[k, c("SEX_CODE", "YEAR")]

datgen$SEX <- factor(datgen$SEX_CODE)
levels(datgen$SEX) <- c("Male", "Female")

gen<-table(datgen$YEAR,datgen$SEX)
Ngen <- apply(gen,1,sum)
gen
gen <- sweep(gen, 1, apply(gen,1,sum), "/")
round(gen,4)


#----------------------------------------------------------------
# Longline survey, 1988-present
#----------------------------------------------------------------
CC.surv<-read.table("~/Kray/Sablefish/2017/data/srv_bio_data2.csv",header=TRUE,sep=",")
k <- is.element(CC.surv$Sex, c("Female","Male"))
datgen <- CC.surv[k, c("Sex", "Year")]

datgen$SEX <- factor(datgen$Sex)
levels(datgen$SEX) <- c("Male", "Female")

gen<-table(datgen$Year,datgen$Sex)
Ngen <- apply(gen,1,sum)
gen
gen <- sweep(gen, 1, apply(gen,1,sum), "/")
round(gen,4)


#----------------------------------------------------------------
# For the time being, I have ignored 'Age_READABILITY_CODE"
# because it's a mess and currently under revision
# Pool fish over 42 Years
#----------------------------------------------------------------

j <- is.element(CC.surv$Sex, c("Female","Male")) & !is.na(CC.surv$Age)
#j <- j & !is.na(dat$Age_READABILITY_CODE) 
#j <- j & dat$Age_READABILITY_CODE < 4
dat <- CC.surv[j, c("Age", "Sex", "Year", "Weight.Kilograms")]
dat$origin <-"survey"

dat.s <- dat

#----------------------------------------------------------------
# Primary Age composition with no sexes
#----------------------------------------------------------------
dat$Age[dat$Age >=42] <- 42
xt <- table(dat$Year, dat$Age)
Nt <- apply(xt,1,sum)
xt
xt <- sweep(xt, 1, apply(xt,1,sum), "/")
round(xt,4)
sqrt(Nt)

write.csv(round(xt,4),"output/srv_Agecomp.csv",row.names=FALSE)
write.csv(round(rbind(Nt),4),"output/srv_Agecomp_s.csv",row.names=FALSE)


#----------------------------------------------------------------
# Sex-specific Age-composition
#----------------------------------------------------------------
x<-is.element(dat$Sex,"Male")
dat_m<-dat[x,c("Year","Age")]
y<-is.element(dat$Sex,"Female")
dat_f<-dat[y,c("Year","Age")]

#Males
dat_m$Age[dat_m$Age >=42] <- 42
xm <- table(dat_m$Year, dat_m$Age)
Nm <- apply(xm,1,sum)
xm
sqrt(rowSums(xm))


#Females
dat_f$Age[dat_f$Age >=42] <- 42
xf <- table(dat_f$Year, dat_f$Age)
Nf <- apply(xf,1,sum)
sqrt(rowSums(xf))


#----------------------------------------------------------------
# Write longline survey proportion results to file
# ---------------------------------------------------------------

xmp <- sweep(xm, 1, apply(xm,1,sum), "/")
round(xmp,4)

write.csv(round(xmp,4),"output/srv_Agecomp_m.csv",row.names=FALSE)
write.csv(as.vector(round(xmp,4)),"output/srv_Agecomp_m_graphic.csv",row.names=FALSE)
write.csv(round(rbind(Nm),4),"output/srv_Agecomp_s_males.csv",row.names=FALSE)

xfp <- sweep(xf, 1, apply(xf,1,sum), "/")
round(xfp,4)
write.csv(round(xfp,4),"output/srv_Agecomp_f.csv",row.names=FALSE)
write.csv(as.vector(round(xfp,4)),"output/srv_Agecomp_f_graphic.csv",row.names=FALSE)
write.csv(round(rbind(Nf),4),"output/srv_Agecomp_s_females.csv",row.names=FALSE)





#------------------------------------------------------------------------------
# Fishery, 1988-present
#------------------------------------------------------------------------------

CC.fshy<-read.table("data/fish_bio_data.csv",header=TRUE,sep=",")
k <- is.element(CC.fshy$SEX_CODE, c(1,2))
datgen <- CC.fshy[k, c("SEX_CODE", "YEAR","SELL_DATE", "G_STAT_AREA","AGE")]

datgen$SEX <- factor(datgen$SEX_CODE)
levels(datgen$SEX) <- c("Male", "Female")
table(datgen$YEAR,datgen$AGE)

#----------------------------------------------------------------
# Calcs to examine catch data during the commercial fishery by week
# Looking at changes in sex proportion over the course of the fishery
# Not implemented every Year - just for checking
#----------------------------------------------------------------

# datgen$DATE<-strptime(datgen$SELL_DATE,"%m/%d/%Y")
# datgen$WEEK<-week(datgen$DATE)
# 
# j<-is.element(datgen$WEEK,c(8,15))
# datgen<-datgen[!j,c("SEX_CODE", "Year","SELL_DATE", "G_STAT_AREA","Age","WEEK","SEX")]
# 
# gen<-table(datgen$WEEK,datgen$SEX)
# Ngen <- apply(gen,1,sum)
# gen
# gen <- sweep(gen, 1, apply(gen,1,sum), "/")
# round(gen,4)
# 
# area<-table(datgen$SEX_CODE,datgen$G_STAT_AREA)
# area <- sweep(area, 2, apply(area,2,sum), "/")
# round(area,4)
# 
# datgen$Age[datgen$Age >=60] <- 60
# Age_week<-table(datgen$WEEK,datgen$Age)
# Age_week<-t(Age_week)
# Age_week <- sweep(Age_week, 2, apply(Age_week,2,sum), "/")
# round(Age_week,4)
# Age_week<-t(Age_week)
# Age_week
# 
# write.table(round(Age_week,4), "clipboard", row.names=F)

#----------------------------------------------------------------
# For the time being, I have ignored 'Age_READABILITY_CODE"
# because it's a mess and under revision
# Pool fish over 42 Years
#----------------------------------------------------------------

dat <- CC.fshy
# Pool fish over 25 Years:
j <- is.element(dat$SEX_CODE, c(1,2)) & !is.na(dat$AGE)
#j <- j & !is.na(dat$Age_READABILITY_CODE) 
#j <- j & dat$Age_READABILITY_CODE < 4
dat <- dat[j, c("AGE", "SEX_CODE", "YEAR","G_STAT_AREA")]
dat$origin <- "fishery"

dat.f <- dat

#----------------------------------------------------------------
# Primary Age composition with no sexes
#----------------------------------------------------------------
dat$AGE[dat$AGE >=42] <- 42
xt <- table(dat$YEAR, dat$AGE)
Nt <- apply(xt,1,sum)
xt
xt <- sweep(xt, 1, apply(xt,1,sum), "/")
round(xt,4)
sqrt(Nt)

write.csv(round(xt,4),"output/fsh_Agecomp.csv",row.names=FALSE)
write.csv(round(rbind(Nt),4),"output/fsh_Agecomp_s.csv",row.names=FALSE)


#----------------------------------------------------------------
# Sex-specific Age-composition
#----------------------------------------------------------------
x<-is.element(dat$SEX_CODE,"1")
dat_m<-dat[x,c("YEAR","AGE")]
y<-is.element(dat$SEX_CODE,"2")
dat_f<-dat[y,c("YEAR","AGE")]


# Males
dat_m$AGE[dat_m$AgE >=42] <- 42
xm <- table(dat_m$YEAR, dat_m$AGE)
Nm <- apply(xm,1,sum)
xm
sqrt(rowSums(xm))


# Females
dat_f$AGE[dat_f$AGE >=42] <- 42
xf <- table(dat_f$YEAR, dat_f$AGE)
Nf <- apply(xf,1,sum)
xf
sqrt(rowSums(xf))

#----------------------------------------------------------------
# Write fishery proportion results to file
# ---------------------------------------------------------------

xmp <- sweep(xm, 1, apply(xm,1,sum), "/")
round(xmp,4)

write.csv(round(xmp,4),"output/fish_Agecomp_m.csv",row.names=FALSE)
write.csv(as.vector(round(xmp,4)),"output/fish_Agecomp_m_graphic.csv",row.names=FALSE)
write.csv(round(rbind(Nm),4),"output/fish_Agecomp_s_males.csv",row.names=FALSE)

xfp <- sweep(xf, 1, apply(xf,1,sum), "/")
round(xfp,4)
write.csv(round(xfp,4),"output/fish_Agecomp_f.csv",row.names=FALSE)
write.csv(as.vector(round(xfp,4)),"output/fish_Agecomp_f_graphic.csv",row.names=FALSE)
write.csv(round(rbind(Nf),4),"output/fish_Agecomp_s_females.csv",row.names=FALSE)
#----------------------------------------------------------------








