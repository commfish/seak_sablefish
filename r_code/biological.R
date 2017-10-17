# work up of survey and fishery biological data
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2017-10-16

source("r_code/helper.r")
source("r_code/vonb_fxns.r")

# data -----

# survey biological  data
read_csv("data/survey/survey_bio_1988_2016.csv") %>% 
  mutate(Year = factor(year),
         Project = factor(Project),
         Stat = factor(Stat),
         Station = factor(Station),
         Sex = factor(Sex),
         Maturity = factor(Maturity),
         length_cm = length_mm/10) %>% 
  group_by(Year, Stat) %>% 
  mutate(n = length(age),
         length_mu = mean(length_mm, na.rm = TRUE),
         weight_mu = mean(weight_kg, na.rm = TRUE)) -> srv_bio

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
         length_cm = length_mm/10) %>% 
  group_by(Year, Stat) %>% 
  mutate(n = length(age),
         length_mu = mean(length_mm, na.rm = TRUE),
         weight_mu = mean(weight_kg, na.rm = TRUE)) -> fsh_bio

# length-based Ludwig von Bertalanffy growth model -----

# subsets by length, age, sex
srv_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & # *FLAG* advent of "modern" survey
           !is.na(length_cm) &
           !is.na(age)) %>% 
  droplevels() -> laa_sub

laa_sub %>% ungroup() %>% filter(Sex == "Female") -> laa_f
laa_sub %>% ungroup() %>% filter(Sex == "Male") -> laa_m

#sex-specific starting values from Hanselman et al. 2007 (Appendix C, Table 1),
#except sigma
start_f <- c(l_inf = 80, k = 0.22, t0 = -1.9, sigma = 10) 
start_m <- c(l_inf = 68, k = 0.29, t0 = -2.3, sigma = 10)

# mle fit for females
vb_mle_f <- vb_mle(obs_length = laa_f$length_cm,
                   age = laa_f$age,
                   starting_vals = start_f,
                   sex = "Female")

# mle fit for males
vb_mle_m <- vb_mle(obs_length = laa_m$length_cm,
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
  geom_jitter(data = laa_sub, aes(x = age, y = length_cm, col = Sex, shape = Sex)) +
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

# weight-length allometry W = alpha * L ^ beta ----

# KVK assumed beta = 3, but there are data to estimate this value (e.g., Hanselman et
# al. 2007 values beta_f = 3.02 and beta_m = 2.96 are used in the current
# assessment)


# subsets by weight, age, sex
srv_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & #advent of "modern" survey
           !is.na(length_cm) &
           !is.na(weight_kg)) %>% droplevels() -> allom_sub

# length-weight relationship
lw_allometry <- function(length, a, b) {a * length ^ b}

START <- c(a = 1e-5, b = 3) #Starting values close to Hanselman et al. 2007

fem_fit <- nls(weight_kg ~ lw_allometry(length = length_cm, a, b), 
               data = filter(allom_sub, Sex == "Female"), start = START)

male_fit <- nls(weight_kg ~ lw_allometry(length = length_cm, a, b), 
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
       aes(x = length_cm, y = weight_kg, col = Sex, shape = Sex)) +
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
  
# weight-at-age using Ludwig von Bertalanffy growth model ----

# subsets by weight, age, sex
srv_bio %>% 
  filter(Sex %in% c("Female", "Male") &
           year >= 1997 & # *FLAG* advent of "modern" survey
           !is.na(age) &
           !is.na(weight_kg)) %>% 
  droplevels() -> waa_sub

waa_sub %>% ungroup() %>% filter(Sex == "Female") -> waa_f
waa_sub %>% ungroup() %>% filter(Sex == "Male") -> waa_m

# fit weight-based lvb with a multiplicative error structure using max likelihood estimation
# log(w_i) = log(w_inf) + beta * log(1 - exp * (-k * (age_i - t0))) + error

# starting values from Hanselman et al. 2007 Appendix C Table 5
start_f <- c(w_inf = 5.5, k = 0.24, t0 = -1.4, sigma = 10)
start_m <- c(w_inf = 3.2, k = 0.36, t0 = -1.1, sigma = 10)

# mle fit for females
wvb_mle_f <- wvb_mle(obs_weight = waa_f$weight_kg,
                   age = waa_f$age,
                   b = beta_f,
                   starting_vals = start_f,
                   sex = "Female")

# mle fit for males
wvb_mle_m <- wvb_mle(obs_weight = waa_m$weight_kg,
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
  geom_jitter(data = waa_sub, aes(x = age, y = weight_kg, col = Sex, shape = Sex)) +
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

# by age and year in the fishery and survey

fsh_bio %>% ungroup() %>% 
  filter(Sex %in% c("Female", "Male")) %>% 
  droplevels() %>% 
  count(Sex, Year) %>% 
  group_by(Year) %>% 
  mutate(prop_byyear = nn / sum(nn)) %>% 
  View()

fsh_bio %>% ungroup() %>% 
  filter(Sex %in% c("Female", "Male")) %>% 
  droplevels() %>% 
  count(Sex, age) %>% 
  group_by(age) %>% 
  mutate(prop_byage = nn / sum(nn)) %>% 
  View()

fsh_bio %>% ungroup() %>% 
  filter(Sex %in% c("Female", "Male")) %>% 
  droplevels() %>% 
  count(Sex, Year, age) %>% 
  group_by(Year, age) %>% 
  mutate(prop_by_yearage = nn / sum(nn)) %>% 
  View()



ggplot(fsh_bio, aes(age, length_cm, col = Sex)) +
  geom_point()


# OLD

###############################################################################
###############################################################################
#
# SABLEFISH SEX COMPOSITION GRAPHICS, SURVEY & FISHERY
#
#  Updated 3/15/2016 Kray Van Kirk 
#
###############################################################################
###############################################################################


###############################################################################
# LIBRARIES
###############################################################################
library(lattice)
library(nlme)
library(plyr)
library(ggplot2)
library(dplyr)


#------------------------------------------------------------------------------
#  DATA
#------------------------------------------------------------------------------
surv<-read.table("data/srv_bio_data.csv",header=TRUE,sep=",")
fish<-read.table("data/fish_bio_data.csv",header=TRUE,sep=",")


#------------------------------------------------------------------------------
# Subsets and conditions
#------------------------------------------------------------------------------
j <- !is.na(surv$SEX) 
j <- j & !is.na(surv$AGE)
dats <- surv[j,c("YEAR", "SEX", "AGE", "LENGTH_MILLIMETERS")]

colnames(dats) <- c("year","sex","age","length")
levels(dats$sex) <- c(NA,"Female", NA, "Male", NA, NA)
dats$sex <- ordered(dats$sex, c("Male","Female"))


j <- is.element(fish$SEX_CODE,c(1,2)) 
datf <- fish[j,c("YEAR", "SEX_CODE", "AGE", "LENGTH_MILLIMETERS")]

colnames(datf) <- c("year","sex","age","length")
datf$sex <-as.factor(datf$sex)
levels(datf$sex) <- c("Male", "Female")

#
# YEAR
#
b <- c(2002:2015)
c <- c(1988:2015)
fityra <- gam(I(sex == "Female") ~ s(year), data = dats, family = "quasibinomial")
fityrb <- gam(I(sex == "Female") ~ s(year), data = datf, family = "quasibinomial")

x <- predict(fityra, newdata = data.frame(year = c), type="response", se=T)
y <- predict(fityrb, newdata = data.frame(year = b), type="response", se=T)


#
# AGE
#
a <- c(2:42)

fitagea <- gam(I(sex == "Female") ~ s(age), data = dats, family = "quasibinomial")
fitageb <- gam(I(sex == "Female") ~ s(age), data = datf, family = "quasibinomial")

xx <- predict(fitagea, newdata = data.frame(age = a), type="response", se=T)
yy <- predict(fitageb, newdata = data.frame(age = a), type="response", se=T)

#
#Data frames
#

sexy<-as.data.frame(x$fit)
sexy$x_se<-x$se.fit
sexy$yfit<-c(rep(0,14),y$fit)
sexy$y_se<-c(rep(0,14),y$se.fit)
sexy$Year<-c
colnames(sexy)<-c("Survey","S_se","Fishery","F_se","Year")

sexa<-as.data.frame(xx$fit)
sexa$xx_se<-xx$se.fit
sexa$yyfit<-yy$fit
sexa$yy_se<-yy$se.fit
sexa$Age<-a
colnames(sexa)<-c("Survey","S_se","Fishery","F_se","Age")



AA<-ggplot(sexa,aes(Age,Survey,colour="Survey"))+geom_line()+
  geom_line(aes(Age,Fishery,colour="Fishery"))+
  #scale_x_discrete(limits=c(2, 42), expand = c(0.01, 0.01)) + 
  ylab("Proportion of females")+
  geom_line(aes(Age,Fishery+(2*F_se),colour="Fishery"),lty=2)+
  geom_line(aes(Age,Fishery-(2*F_se),colour="Fishery"),lty=2)+
  geom_line(aes(Age,Survey+(2*S_se),colour="Survey"),lty=2)+
  geom_line(aes(Age,Survey-(2*S_se),colour="Survey"),lty=2)+
  annotate("text", x= 10, y = 0.32, label = paste("ADF&G Longline survey"), colour="blue")+
  annotate("text", x= 10, y = 0.28, label = paste("Commercial longline fishery"), colour="red")+
  theme_bw()+
  theme(legend.position = "bottomleft")+
  scale_colour_manual(values=c("Survey" = "blue", "Fishery" = "red"))+
  theme(panel.grid.major = element_line(colour="transparent"))+
  theme(panel.grid.minor = element_line(colour="transparent"))+
  theme(strip.text = element_text(size=10))+
  theme(axis.text.x = element_text(size=10,colour="black"),
        axis.title.x = element_text(size=10,colour="black"))+
  theme(axis.text.y = element_text(size=10,colour="black"),
        axis.title.y = element_text(size=10,colour="black"))+
  theme(plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(legend.background = element_rect(fill = "transparent",colour = NA))

BB<-ggplot(sexy,aes(Year,Survey,colour="blue"))+geom_line(colour="blue")+
  geom_line(data=subset(sexy, Year>2001),aes(Year,Fishery),colour="red")+
  #scale_x_discrete(limits=c(1988, 2015), expand = c(0.01, 0.01)) + 
  geom_line(data=subset(sexy, Year>2001),aes(Year,Fishery+(2*F_se)),colour="red",lty=2)+
  geom_line(data=subset(sexy, Year>2001),aes(Year,Fishery-(2*F_se)),colour="red",lty=2)+
  geom_line(aes(Year,Survey+(2*S_se)),colour="blue",lty=2)+
  geom_line(aes(Year,Survey-(2*S_se)),colour="blue",lty=2)+
  annotate("text", x= 1995, y = 0.4, label = paste("ADF&G Longline survey"), colour="blue")+
  annotate("text", x= 1995, y = 0.37, label = paste("Commercial longline fishery"), colour="red")+
  ylab("Proportion of females")+
  theme_bw()+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_line(colour="transparent"))+
  theme(panel.grid.minor = element_line(colour="transparent"))+
  theme(strip.text = element_text(size=10))+
  theme(axis.text.x = element_text(size=10,colour="black"),
        axis.title.x = element_text(size=10,colour="black"))+
  theme(axis.text.y = element_text(size=10,colour="black"),
        axis.title.y = element_text(size=10,colour="black"))+
  theme(plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(legend.background = element_rect(fill = "transparent",colour = NA))

png(file='figures/sex_year_age.png', res=300, width=7, height=6., units ="in", bg="transparent")       
grid.arrange(AA,BB)
dev.off()

#
# FIGURES
#

png(file='figures/sex_year.png', res=300, width=7, height=4., units ="in", bg="transparent")  
par(oma=c(0,0,0,0), mar=c(5,5,1,1))
plot(c, x$fit, ylab = "Proportion females by year", xlab = "Year", type="l", ylim = c(0.3, 0.8), cex.lab = 1., col="blue", lwd=2)
lines(c, x$fit + 2*x$se.fit, lty=2, col = "blue")
lines(c, x$fit - 2*x$se.fit, lty=2, col = "blue")
lines(b, y$fit, col="red", lwd=2)
lines(b, y$fit + 2*y$se.fit, lty=2, col = "red")
lines(b, y$fit - 2*y$se.fit, lty=2, col = "red")
legend(2002, 0.4, lty = 1, lwd=2, col=c("blue","red"),c("ADF&G longline survey","Commercial longline fishery"),bty="n")
abline(h=0.5, lty=3)
dev.off()

png(file='figures/sex_age.png', res=300, width=7, height=4., units ="in", bg="transparent")  
par(oma=c(0,0,0,0), mar=c(5,5,1,1))
plot(a, xx$fit, ylab = "Proportion females by age", xlab = "Age", type="l", ylim = c(0.2, 0.8), cex.lab = 1., col="blue", lwd=2)
lines(a, xx$fit + 2*xx$se.fit, lty=2, col = "blue")
lines(a, xx$fit - 2*xx$se.fit, lty=2, col = "blue")
lines(a, yy$fit, col="red", lwd=2)
lines(a, yy$fit + 2*yy$se.fit, lty=2, col = "red")
lines(a, yy$fit - 2*yy$se.fit, lty=2, col = "red")
legend(5, 0.4, lty = 1, lwd=2, col=c("blue","red"),c("ADF&G longline survey","Commercial longline fishery"),bty="n")
abline(h=0.5, lty=3)
dev.off()


#----------------------------------------------------------------
# OLD GRAPHICS FOR 2014 DISCUSSION ON DEPTH, FEMALES, AND SIZE
#----------------------------------------------------------------
# 
# j <- is.element(fish$SEX_CODE,c(2))
# j <- j & !is.na(fish$AGE)
# dat2 <- fish[j,c("YEAR", "SEX_CODE", "AGE", "LENGTH_MILLIMETERS")]
# 
# j <- is.element(surv$SEX,"Female") 
# j <- j & !is.na(surv$AGE)
# dat <- surv[j,c("YEAR", "SEX", "AGE", "LENGTH_MILLIMETERS")]
# 
# # Set levels of sex to Male and Female only (all others = NA):
# levels(dat$SEX) <- c(NA,"Female", NA, "Male", NA, NA, NA)
# dat$SEX <- ordered(dat$SEX, c("Male","Female"))
# #dat <- na.omit(dat)
# 
# x<-table(dat$AGE)
# x
# y<-sum(x)
# z<-x/y
# z
# write.table(round(z,4), "clipboard", row.names=F)
# a<-table(dat2$AGE)
# b<-sum(a)
# c<-a/b
# c
# write.table(round(c,4), "clipboard", row.names=F)
# 
# 
# barplot(z$fit)
# par(new=TRUE)
# barplot(xf$fit,col="cyan4",yaxt="n")
# 
# dat<-dat[dat$AGE<41 &dat$SEX == "Female" ,]
# dat2<-dat2[dat2$AGE<41 & dat2$SEX=="Female",]
# plot(table(dat$AGE),col="grey", main="", las=3); abline(h=0.5, lwd=2)
# par(new=TRUE)
# plot(table(dat2$AGE), col="cyan4", main="", las=3)
# 






