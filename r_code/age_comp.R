###############################################################################
###############################################################################
#
# Age COMPOSITIONS FOR NSEI SABLEFISH
# POT SURVEY, LONGLINE SURVEY, COMMERCIAL FISHERY 
#
#  Updated 3/3/2016 Kray Van Kirk 
#
#

#----------------------------------------------------------------
# ALEX QUERY CRITERIA FOR FISH_BIO_DATA
#----------------------------------------------------------------
# Year BETWEEN 1980 AND 2015 AND species_code = '710' AND 
# g_manAgement_area_code = 'NSEI' AND project_code = '02'
# NOTE: when running, always make sure that all data have been read 
# and uploaded into IFDB by the ADU
#----------------------------------------------------------------



#----------------------------------------------------------------
# ALEX QUERY CRITERIA FOR SRV_BIO_DATA -  obsolete
#----------------------------------------------------------------
#
# BIOLOGICAL DATA >> Age Sex Size Sampled at Sea
#  Base Table	out_g_bio_effort_Age_sex_size							
#  Select Clause	*							
# Where Clause	Year BETWEEN 1988 AND 2015 AND 
#                   species_code = '710' AND 
#                   project_code = '03'																	
#----------------------------------------------------------------



#----------------------------------------------------------------
# OceanAK QUERY CRITERIA FOR SRV_BIO_DATA
#----------------------------------------------------------------
# Commercial Fisheries / Region I / Groundfish / Age-Sex-Size Sampled at Sea
#
# Criteria     	    Year is between 1988 AND Present AND 
#                   Species = 'Sablefish' AND 
#                   Project = 'Chatham Sablefish LL Survey'																	
#----------------------------------------------------------------




#----------------------------------------------------------------
# ALEX QUERY CRITERIA - legacy - keep
#----------------------------------------------------------------
# Subsample of those fish on the pot marking survey sacrificed to obtain
#  sex data. Note that this table is distinct from POT_LENGTH above, as 
#  this one selects from Age-Sex-Size *Sampled* at Sea, not *Tagged*
#   
# pot_sex and srv_bio_data could potentially be joined into a single
#  call with interior R scripts to parse.
#
# BIOLOGICAL DATA >> Age-Sex-Size Sampled at Sea
# Base Table	out_g_bio_eff_Age_sex_size							
# Select Clause	*							
#   Where Clause	Year BETWEEN 2009 AND 2015 AND
#                   species_code = '710' AND
#                   gear_code = '91' AND
#                   project_code = '11'							
#----------------------------------------------------------------




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
CC.pot<-read.table("data/pot_sex.csv",header=TRUE,sep=",")

k <- is.element(CC.pot$SEX_CODE, c(1,2))
datgen <- CC.pot[k, c("SEX_CODE", "Year")]

datgen$SEX <- factor(datgen$SEX_CODE)
levels(datgen$SEX) <- c("Male", "Female")

gen<-table(datgen$Year,datgen$SEX)
Ngen <- apply(gen,1,sum)
gen
gen <- sweep(gen, 1, apply(gen,1,sum), "/")
round(gen,4)


#----------------------------------------------------------------
# Longline survey, 1988-present
#----------------------------------------------------------------
CC.surv<-read.table("data/srv_bio_data2.csv",header=TRUE,sep=",")
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








