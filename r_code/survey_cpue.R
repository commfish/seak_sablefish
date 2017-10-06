# Survey cpue
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2017-10-05

# data -----

srv_cpue <- read_csv("data/survey/survey_cpue_1988_2016.csv")

str(srv_cpue)

srv_cpue  %>% 
  mutate(Year = factor(year),
         Stat = factor(Stat),
         Station = factor(Station),
         #standardize hook spacing (Sigler & Lunsford 2001, CJFAS) changes in 
         #hook spacing. pers. comm. with aaron.baldwin@alaska.gov: 1995 & 1996 -
         #118 in; 1997 - 72 in.; 1998 & 1999 - 64; 2000-present - 78". This is
         #different from KVK's code (he assumed 3 m before 1997, 2 m in 1997 and
         #after)
         std_hooks = ifelse(year <= 1996, 2.2 * no_hooks * (1 - exp(-0.57 * (118 * 0.0254))),
                            ifelse(year == 1997, 2.2 * no_hooks * (1 - exp(-0.57 * (72 * 0.0254))),
                                   ifelse( year %in% c(1998, 1999), 2.2 * no_hooks * (1 - exp(-0.57 * (64 * 0.0254))),
                                           2.2 * no_hooks * (1 - exp(-0.57 * (78 * 0.0254)))))),
         no_sablefish = ifelse(is.na(no_sablefish), 0, no_sablefish), # make any NAs 0 values
         std_cpue = no_sablefish/std_hooks #*FLAG* this is NPUE, the fishery is a WPUE
  )


###############################################################################
###############################################################################
#
# SABLEFISH LONGLINE SURVEY CPUE 
#
#  Updated 3/3/2016 Kray Van Kirk 
#

#----------------------------------------------------------------
# ALEX QUERY CRITERIA FOR SRV_BIO_DATA
#----------------------------------------------------------------
#
# BIOLOGICAL DATA >> Age Sex Size Sampled at Sea
#  Base Table	out_g_bio_effort_age_sex_size							
#  Select Clause	*							
# Where Clause	year BETWEEN 1988 AND 2015 AND 
#                   species_code = '710' AND 
#                   project_code = '03'																	
#----------------------------------------------------------------

#----------------------------------------------------------------
# ALEX QUERY CRITERIA FOR CPUE_SRV
#----------------------------------------------------------------
#
# SURVEY >> LONGLINE SURVEY - CATCH AND HOOK ACCOUNTING
# Base Table	out_g_sur_longline_hooks_catch																				
# Select Clause	*																				
#   Where Clause	project_code = '03' AND year IN (1988:present)																				
# Group By Clause																					
# Order By Clause	year, project_code, trip_no,effort_no,subset_no																				
#----------------------------------------------------------------


###############################################################################
# LIBRARIES
###############################################################################
library(plyr)
library(reshape)
library(lattice)
library(mgcv)
library(lubridate)
library(ggplot2)
library(dplyr)
library(gridExtra)


###############################################################################
# READ IN DATA
###############################################################################
#setwd("C:/Users/kfvan_kirk/Documents/NEW_sablefish/2016")
cpue.srv <- read.table("data/srv_cpue.csv",header=TRUE,sep=",")
#bio.srv <- read.table("data/srv_cpue.csv",header=TRUE,sep=",")

names(cpue.srv)
dim(cpue.srv)

#------------------------------------------------------------------------------
# Subset:
#        removing "Not valid" SUBSET_CONDITION
#        sampling stations== 3 digits
#        converting null cells to 0
#------------------------------------------------------------------------------
j <- !is.element(cpue.srv$Subset.Condition, "Not Valid")
j <- j & cpue.srv$Station.No < 99
j <-  !is.na(cpue.srv$Hooks...Total)
cpue.srv <- cpue.srv[j,c('Year','G.Stat.Area','Station.No','Subset.Condition','Sablefish','Hooks...Total','Hooks...Bare','Hooks...Invalid')]

#----------------------------------------------------------------
# STANDARDIZE HOOK SPACING 
#   Sigler,M.F. 2000. Abundance estimation and capture of sablefish, Anoplopoma fimbria, 
#   by longline gear CJFAS 57:1270 - 1283
# NOTE: changes in 1997 due to changes in survey standardization!!
#----------------------------------------------------------------
cpue.srv$hooks <- cpue.srv$Hooks...Total# - cpue.srv$Hooks...Number.Invalid
cpue.srv$standard<-ifelse(cpue.srv$Year>"1996",(cpue.srv$standard=(cpue.srv$hooks*2.2)*(1-exp(-0.57*2))),
                            (cpue.srv$standard=(cpue.srv$hooks*2.2)*(1-exp(-0.57*3))))

head(cpue.srv)
#-----------------------------------------------------------------------------
# Rename, define factors
#-----------------------------------------------------------------------------
names(cpue.srv) <- c('year',
                     'stat_area', 'station','ss_cond', 'sablefish','no_hooks', 
                     'bare', 'invalid','hooks','standard')

cpue.srv %>% 
  mutate(Year = factor(year), Station = factor(station), Stat = factor(stat_area)) -> dat

dat$sablefish[is.na(dat$sablefish)] <- 0

dat$dum<-1

dim(dat)
head(dat)

#-----------------------------------------------------------------------------
# Graphical explorations
#-----------------------------------------------------------------------------
# ggplot(dat, aes(Station,sablefish))+geom_jitter()+facet_grid(.~Year)
# # Note 1997 extremes
# # Note 1997 - 1999. Soak time modded in 1997 to 3 hours,
# #  but drop-off in 2000 is marked
# 
# ggplot(dat, aes(Year,sablefish))+geom_jitter()+facet_grid(.~Station)
# # Station 39
# 
# ggplot(dat, aes(Year,sablefish))+geom_jitter()+facet_grid(.~Stat)
# 
# ggplot(dat, aes(Stat,sablefish))+geom_jitter()+facet_grid(.~Year)
# 
# ggplot(dat, aes(Year,sablefish, color=factor(Stat), group=1 ))+
#   geom_jitter()+stat_smooth(alpha=.2, method='gam', formula=y~s(x, bs='re'))
# 
# ggplot(dat, aes(Year,sablefish, color=standard, group=1 ))+
#   geom_jitter()+stat_smooth(alpha=.2, method='gam', formula=y~s(x, bs='re'))
# 
# ggplot(dat, aes(Year,sablefish, fill=Stat))+geom_boxplot()
# # Note station 39 in Stat 345731
# 
# ggplot(dat, aes(Year,sablefish, fill=Station))+geom_boxplot()
# 
# ggplot(dat, aes(standard,sablefish, color=factor(stat_area), group=1))+
#   geom_point()+stat_smooth(method='gam', formula=y~s(x, k=4))
# 
# ggplot(dat, aes(no_hooks,sablefish, color=factor(stat_area), group=1))+
#   geom_point()+stat_smooth(method='gam', formula=y~s(x, k=4))
# 
# ggplot(dat, aes(sablefish))+geom_density(fill=4, alpha=.5)
# 
# ggplot(dat, aes(sablefish, fill=Stat))+geom_density(alpha=.2)
# 
# ggplot(dat, aes(sablefish, fill=Station))+geom_density(alpha=.2)

png(file='figures/srv_cpue_stat.png', res=300, width=7, height=3.5, units ="in", bg="transparent")  
ggplot(dat, aes(x=(reorder(Stat,cpue)), y=cpue, fill=factor(Stat)))+geom_boxplot()+
  xlab("Stat area")+
  ylab("CPUE")+
  theme_bw()+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_line(colour="transparent"))+
  theme(panel.grid.minor = element_line(colour="transparent"))+
  theme(strip.text = element_text(size=10))+
  theme(axis.text.x = element_text(size=10,colour="black",angle=90,vjust=0.25),
        axis.title.x = element_text(size=10,colour="black"))+
  theme(axis.text.y = element_text(size=10,colour="black"),
        axis.title.y = element_text(size=10,colour="black"))+
  theme(plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(legend.background = element_rect(fill = "transparent",colour = NA))
dev.off()


png(file='figures/srv_cpue_station.png', res=300, width=7, height=3.5, units ="in", bg="transparent")  
ggplot(dat, aes(x=(reorder(Station,cpue)), y=cpue, fill=factor(Station)))+geom_boxplot()+
  xlab("Station")+
  ylab("CPUE")+
  theme_bw()+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_line(colour="transparent"))+
  theme(panel.grid.minor = element_line(colour="transparent"))+
  theme(strip.text = element_text(size=10))+
  theme(axis.text.x = element_text(size=10,colour="black",angle=90,vjust=0.25),
        axis.title.x = element_text(size=10,colour="black"))+
  theme(axis.text.y = element_text(size=10,colour="black"),
        axis.title.y = element_text(size=10,colour="black"))+
  theme(plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(legend.background = element_rect(fill = "transparent",colour = NA))
dev.off()




#------------------------------------------------------------------------------
# CPUE
#------------------------------------------------------------------------------
cpue.srv$cpue<-cpue.srv$Sablefish/cpue.srv$standard

dat$cpue<-dat$sablefish/dat$standard

# ggplot(dat, aes(Station,cpue))+geom_jitter()+facet_grid(.~Year)
# 
# ggplot(dat, aes(Stat,cpue))+geom_jitter()+facet_grid(.~Year)
# 
# ggplot(dat, aes(cpue, fill=Stat))+geom_density(alpha=.2)
# 
# ggplot(dat, aes(cpue, fill=Station))+geom_density(alpha=.2)
# 
 ggplot(dat, aes(Year,cpue, color=Station, group=1))+
   geom_point()+stat_smooth(method='gam', formula=y~s(x, bs='re'))
 
 ggplot(dat, aes(Year,cpue))+
   geom_point()+stat_summary(fun.data='mean_cl_boot',geom="smooth",aes(Year,fit))
 
 ggplot(newd, aes(Year,fit, group=1))+
   geom_point()+geom_line()+
   stat_summary(data=dat, fun.data='mean_cl_boot',geom="smooth",aes(Year,fit),colour="cyan")+
  stat_summary(data=dat, fun.data='mean_cl_boot',geom="smooth",aes(Year,cpue),colour="orange")
 
# 
# ggplot(dat, aes(Year,cpue, color=Stat, group=1))+
#   geom_point()+stat_smooth(method='gam', formula=y~s(x,bs='re'))
# 
# ggplot(dat, aes(Year,cpue, fill=Stat))+geom_boxplot()
# Note that the catch anomalies from station #39 in 1997
#  and the higher catches 1997 - 1999 have disappeared
#  in the cpue values

#------------------------------------------------------------------------------
# GAM for *numbers-per-hook* cpue
#------------------------------------------------------------------------------
fit <- gam(cpue ~  Year + s(Station, bs='re', by=dum) +
       s(Stat, bs='re', by=dum), data=dat, gamma=1.4)

newd <- expand.grid(Year = levels(dat$Year),Station = "4", Stat = "345631", dum = 0)

#Predict fit againsty GAM parameters
dat$fit <- predict(fit, dat, type="response")
newd$fit <- predict(fit, newd, type="response")

summary(fit)
plot(fit, shade=T, page=1)

#ggplot(dat, aes(Year,fit))+ geom_point()+stat_summary(fun.data = "mean_cl_boot", 
                                                      #geom="errorbar", colour="red", size=2)

png(file='figures/srv_cpue_comparison.png', res=300, width=7, height=3.5, units ="in", bg="transparent") 
ggplot(newd, aes(Year,fit, group = 1))+
  stat_summary(fun.data='mean_cl_boot',geom="smooth",aes(Year,fit,colour="Longline survey CPUE - GAM"),fill="grey70")+
  stat_summary(data=dat, fun.data='mean_cl_boot',geom="smooth",aes(Year,cpue,colour="Longline survey CPUE - simple ratio"),fill="blue",alpha=0.1)+
  stat_summary(fun.y='mean',geom="point",aes(Year,fit,colour="Longline survey CPUE - GAM"))+
  stat_summary(data=dat, fun.y='mean',geom="point",aes(Year,cpue,colour="Longline survey CPUE - simple ratio"))+
  xlab("Year")+
  ylab("CPUE")+
  scale_colour_manual(name="CPUE",values=c("Longline survey CPUE - GAM" = "black", "Longline survey CPUE - simple ratio" = "blue"))+
  theme_bw()+
  #scale_x_discrete(breaks=c(1997:2015),labels=seq(1997,2015,by=1))+
  theme(legend.position = "bottom")+
  theme(panel.grid.major = element_line(colour="transparent"))+
  theme(panel.grid.minor = element_line(colour="transparent"))+
  theme(legend.position = c(0.6,0.25))+
  theme(legend.key = element_rect(fill = "white", colour="white"))+
  theme(strip.text = element_text(size=10))+
  theme(axis.text.x = element_text(size=10,colour="black",angle=45,hjust=1),
        axis.title.x = element_text(size=10,colour="black"))+
  theme(axis.text.y = element_text(size=10,colour="black"),
        axis.title.y = element_text(size=10,colour="black"))+
  theme(plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(legend.background = element_rect(fill = "transparent",colour = NA))
dev.off()

# a1<-ggplot(dat, aes(Year,cpue, fill=Stat))+geom_boxplot()
# b1<-ggplot(dat, aes(Year,fit, fill=Stat))+geom_boxplot()
# grid.arrange(a1,b1,ncol=2)
# 
# a1<-ggplot(dat, aes(Station,cpue, fill=Stat))+geom_boxplot()
# b1<-ggplot(dat, aes(Station,fit, fill=Stat))+geom_boxplot()
# grid.arrange(a1,b1,ncol=2)

#ggplot(dat,aes(fit,cpue))+geom_jitter()+stat_smooth()


#Mean cpue and and variance for each year 1997 - present
newd %>% 
  group_by(Year) %>% 
  summarise(mfit=mean(fit))-> cpue_gam
cpue_g<-as.numeric(round(cpue_gam$mfit,4))


newd %>% 
  group_by(Year) %>% 
  summarise(var=var(fit))-> cpue_var
cpue_v<-as.numeric(round(cpue_var$var,4)) 
cpue_v



# Check normality of fit
histogram(~cpue|Year, data=dat,strip = strip.custom(bg="grey"))
histogram(~fit|Year, data=newd,breaks=25,strip = strip.custom(bg="grey"))

#------------------------------------------------------------------------------
# Note that numbers-per-hook cpue for 1997 - present is already normally
#  distributed, and that are no additional drivers for smoothing or
#  accounting in the GAM, so there isn't justification for using it. It does
#  reduce the estimates of variance, however, so we'll use it for that reason
#  when fitting into the ASA. 
#
# 1988 - 1996 cpue distributions appear more log-normally distributed. The
#  variances are sufficiently small, however, that even when compensating
#  for Jensen's inequality, the differences are miniscule (4th decimal place)
#  and so for ease we approximate with the straight mean cpue value. 
#
# Note: explore fitting in the ASA to 1988 - 1996 using log-normal
#  likelihood
#
# I've added a few additional tests looking at taking a weighted mean
#  across incrementally-declining criteria, but the difference is also 
#  virtually null. 
#------------------------------------------------------------------------------

# Numbers-per-standardized hook CPUE
x<-with(dat, tapply(dat$cpue,dat$Year,mean, na.rm=TRUE))
round(x,4)

a<-with(newd, tapply(fit,Year,mean, na.rm=TRUE))
round(a,4)

#Variance
y<-with(dat, tapply(dat$cpue,dat$Year, var, na.rm=TRUE))
round(y,4)

y<-with(cpue.srv, tapply(cpue,Year, var, na.rm=TRUE))
round(y,4)

# dat %>% 
#   group_by(Year,Station,Stat) %>% 
#   summarise(cpue1=mean(cpue)) %>%
#   group_by(Year,Stat) %>% 
#   summarise(cpue2=mean(cpue1)) %>%
#   group_by(Year) %>% 
#   summarise(cpue3=mean(cpue2)) -> mean_cpue
# b<-as.numeric(round(mean_cpue$cpue3,4)) 
# b
# 
# dat %>% 
#   group_by(Year,Station,Stat) %>% 
#   summarise(fit1=mean(fit)) %>%
#   group_by(Year,Stat) %>% 
#   summarise(fit2=mean(fit1)) %>%
#   group_by(Year) %>% 
#   summarise(fit3=mean(fit2)) -> mean_fit
# c<-as.numeric(round(mean_fit$fit3,4)) 
# c



#------------------------------------------------------------------------------
# pounds-per-standardized hook CPUE
#------------------------------------------------------------------------------
names(bio.srv)

# Create subset of year, weight, station and stat area:
j <- !is.na(bio.srv$WEIGHT_KILOGRAMS) 
bio.srv <- bio.srv[j,c("YEAR","WEIGHT_KILOGRAMS","STATION_NO","G_STAT_AREA")]
names(bio.srv)<-c("year","weight","station","stat")

# Compute weighted mean by decrement - station, stat, year
bio.srv %>% 
  group_by(year,station,stat) %>% 
  summarise(wt=mean(weight)*2.2) %>%
     group_by(year,stat) %>% 
     summarise(wt2=mean(wt)) %>%
       group_by(year) %>% 
       summarise(wt3=mean(wt2)) -> mean_wt
a<-as.numeric(round(mean_wt$wt3,4)) 
a

# Merge weights and data by year
dat<-merge(dat, mean_wt, by="year")

head(dat)
#------------------------------------------------------------------------------
# cpue for pounds-per-standardized-hook
#------------------------------------------------------------------------------
dat$cpue<-(dat$sablefish * dat$wt3)/dat$standard


#------------------------------------------------------------------------------
# Given the above performance of the GAM, here we simply do a weighted mean
#   No estimate of variance is needed, as we use numbers cpue in the ASA
#------------------------------------------------------------------------------
fit <- gam(cpue ~  Year + s(Station, bs='re', by=dum) +
             s(Stat, bs='re', by=dum), data=dat, gamma=1.4)

newd <- expand.grid(Year = levels(dat$Year),Station = "4", Stat = "345631", dum = 0)

#Predict fit againsty GAM parameters
dat$fit <- predict(fit, dat, type="response")
newd$fit <- predict(fit, newd, type="response")

summary(fit)
plot(fit, shade=T, page=1)

#ggplot(dat, aes(Year,fit))+ geom_point()+stat_summary(fun.data = "mean_cl_boot", 
#geom="errorbar", colour="red", size=2)

# a1<-ggplot(dat, aes(Year,cpue, fill=Stat))+geom_boxplot()
# b1<-ggplot(dat, aes(Year,fit, fill=Stat))+geom_boxplot()
# grid.arrange(a1,b1,ncol=2)
# 
# a1<-ggplot(dat, aes(Station,cpue, fill=Stat))+geom_boxplot()
# b1<-ggplot(dat, aes(Station,fit, fill=Stat))+geom_boxplot()
# grid.arrange(a1,b1,ncol=2)

#ggplot(dat,aes(fit,cpue))+geom_jitter()+stat_smooth()


#Mean cpue and and variance for each year 1997 - present
newd %>% 
  group_by(Year) %>% 
  summarise(mfit=mean(fit))-> cpue_gam
cpue_p<-as.numeric(round(cpue_gam$mfit,4))


newd %>% 
  group_by(Year) %>% 
  summarise(var=var(fit))-> cpue_var
cpue_pv<-as.numeric(round(cpue_var$var,4)) 
cpue_pv



# dat %>%
#   group_by(Year,Station,Stat) %>%
#   summarise(cpue1p=mean(cpuep)) %>%
#   group_by(Year,Stat) %>%
#   summarise(cpue2p=mean(cpue1p)) %>%
#   group_by(Year) %>%
#   summarise(cpue3p=mean(cpue2p)) -> mean_cpue
# e<-as.numeric(round(mean_cpue$cpue3p,4))


# Mean over all stations by year - just as a check
# x<-with(dat, tapply(dat$cpuep,dat$year,mean, na.rm=TRUE))
# round(x,4)
# 
# # Standard deviation over all stations by year
# y<-with(dat, tapply(dat$cpuep,dat$year, var, na.rm=TRUE))
# round(y,4)


#Write to file
write.table(round(rbind(cpue_g),4),"output/srv_cpue_gam_numbers.csv",row.names=FALSE,col.names = FALSE,sep=",")
write.table(round(rbind(cpue_v),4),"output/srv_cpue_var_numbers.csv",row.names=FALSE,col.names = FALSE,sep=",")
write.table(round(rbind(cpue_p),4),"output/srv_cpue_pounds.csv",row.names=FALSE,col.names = FALSE,sep=",")
write.table(round(rbind(x),4),"output/srv_non_gam.csv",row.names=FALSE,col.names = FALSE,sep=",")

