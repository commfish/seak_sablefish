###############################################################################
###############################################################################
##
##  AGING ERROR MATRIX FOR CHATHAM SABLEFISH
##
##  Updated 2/19/2016 Kray Van Kirk
##
###############################################################################
###############################################################################



#----------------------------------------------------------------
# The aging error script was supplied by Pete Hulson, NMFS
# 
# There are two sections below:
#
#  SECTION A: pre-processing of ADU age reading data prior
#             to implementation of the aging error script
#
#  SECTION B: the aging error script
#----------------------------------------------------------------



###############################################################################
# LIBRARIES
###############################################################################
library(lattice)
library(plyr)



###############################################################################
#
# SECTION A
#
###############################################################################

#setwd("C:/Users/kfvan_kirk/Documents/NEW_sablefish/2016")
#path<-getwd()


#------------------------------------------------------------------------------
#
# Read in ADU aging data
#
#  SECTION A is the product of how aging data are stored by the ADU, using
#    "RELEASE AUTHORITATIVE" for the "true" age of an otolith, and all other
#    reads as second/third/etc. reads. Not every otolith has more than one
#    reader, and some have more than two. The script below can be improved
#    by including all reads of every otolith as available; the current
#    forms uses ONLY otoliths with two more more reads but removes all
#    reads > 2. 
#
#------------------------------------------------------------------------------
age.read<-read.table("data/sablefish_ADU_age.csv",header=TRUE,sep=",")
dat <- age.read 
dim (dat)


dplyr::select(dat, AGE, READABILITY_CODE, RELEASE_AUTHORITATIVE, SAMPLE_ID, 
              SPECIMEN_ID) -> dat

colnames(dat) <- c("age","read","true","sample","specimen")


#------------------------------------------------------------------------------
# Extract subset of data to use: Eliminate fish that were not aged
#
# Create unique ID = sample + specimen
#------------------------------------------------------------------------------
filter(dat, !is.na(age), !is.element(read,c("MO","NO"))) -> dat

dat<-transform(dat,id=interaction(sample,specimen))


#------------------------------------------------------------------------------
# Split into two data frames - one for RELEASE AUTHORITATIVE (true == 1)
#  and one for the second read (true == 0)
#------------------------------------------------------------------------------
filter(dat, true == 1, age <= 75) -> dat.true
filter(dat, true == 0, age <= 75) -> dat.read


#------------------------------------------------------------------------------
# Remove duplicate ID entries - this should be completely null for dat.true
#  and remove only a relatively small number in dat.read, as these are 3rd
#  4th, etc. reads
#------------------------------------------------------------------------------
dat.true %>%
  group_by(id) %>%
  distinct(.keep_all = TRUE) -> dat.true

dat.read %>%
  group_by(id) %>%
  distinct(.keep_all = TRUE) -> dat.read


#------------------------------------------------------------------------------
# Match ID between the two, meaning that we are left with only otoliths with
#  a first and second read.
#
# Order both data frames to make sure we match the proper reads and otoliths
# Note that we could do this with a join, but that would necessitate renaming
# the columns in one of the data frame to preserve it in the resulting matrix
# so here we settle for one less line of code
#------------------------------------------------------------------------------
filter(dat.true, is.element(id,dat.read$id)) -> dat.true
filter(dat.read, is.element(id,dat.true$id)) -> dat.read

dim(dat.true)
dim(dat.read)

dat.true<-arrange(dat.true,desc(id))
dat.read<-arrange(dat.read,desc(id))


#------------------------------------------------------------------------------
# Create matrix for Section B, below
#------------------------------------------------------------------------------
AgeReads2<-matrix(nrow=length(dat.true$age),ncol=3)
AgeReads2[,1]<-dat.true$age
AgeReads2[,2]<-dat.read$age
AgeReads2[,3]<-dat.true$age
head(AgeReads2)
colnames(AgeReads2)<-c("Read_Age","Test_Age","Final_Age")
write.table(AgeReads2,"output/AgeReads2.csv",row.names=FALSE,col.names=TRUE,sep=",")



###############################################################################
#
# SECTION B
#
###############################################################################

# NOTE DIFFERENT DIRECTORY FOR ADMB CALLS

#setwd("C:/Users/kfvan_kirk/Documents/NEW_sablefish/2016/ADMB_ageage")
#path<-getwd()

# Results folder
#pathR<-paste(path,"/Results",sep="")


#------------------------------------------------------------------------------
# Ageing error function
#------------------------------------------------------------------------------
get_ageage<-function(norpac_species,recage,nages_D){

# Read in, subset data to species and region
  # In this case, these are extraneous, as the data are already parsed,
  #   but keep these here for flexibility
  
ae_DAT<-read.csv("output/AgeReads2.csv",sep=",")
#ae_DAT<-subset(ae_DAT,ae_DAT$Species==710)
#ae_DAT<-subset(ae_DAT,ae_DAT$Region=="NSEI")

# Send reader-tester agreement data to ADMB and estimate ageing error SDs
r<-ae_DAT$Read_Age
t<-ae_DAT$Test_Age
f<-ae_DAT$Final_Age
DAT<-as.data.frame(cbind(r,t,f))
x_r<-which(DAT$r<0)
x_t<-which(DAT$t<0)
x_f<-which(DAT$f<0)
if(length(c(x_r,x_t,x_f))>0){DAT<-DAT[-c(x_r,x_t,x_f),]}
Test_Age<-DAT$t
Read_Age<-DAT$r
z_tab<-table(Test_Age,Read_Age)
z<-cbind(as.numeric(rownames(z_tab)),seq(as.numeric(colnames(z_tab))[1],as.numeric(colnames(z_tab))[1],length.out=length(rownames(z_tab))),z_tab[,1])
colnames(z)<-c("Test_Age","Read_Age","Freq")

for(c in 2:length(z_tab[1,])){
t<-cbind(as.numeric(rownames(z_tab)),seq(as.numeric(colnames(z_tab))[c],as.numeric(colnames(z_tab))[c],length.out=length(rownames(z_tab))),z_tab[,c])
z<-rbind(z,t)}

rownames(z)<-seq(1:length(z[,1]))
z<-as.data.frame(z)
z$AGREE<-z$Test_Age==z$Read_Age
ape<-(1-(tapply(z$Freq[z$AGREE==FALSE],z$Test_Age[z$AGREE==FALSE],sum))/tapply(z$Freq,z$Test_Age,sum))*100
ss<-tapply(z$Freq,z$Test_Age,sum)
perc_ag<-data.frame(cbind(ape,ss))
age<-seq(as.numeric(rownames(perc_ag)[1]),as.numeric(rownames(perc_ag)[length(perc_ag[,1])]))
Data<-matrix(nrow=length(age),ncol=3)
colnames(Data)<-c("a","ape","ss")
Data[,1]<-age
for(a in 1:length(age)){
r<-which(as.numeric(rownames(perc_ag))==age[a])
if(length(r)==1){
Data[a,2]<-perc_ag$ape[r]/100
Data[a,3]<-perc_ag$ss[r]}
if(length(r)==0){
Data[a,2]<-(-9)
Data[a,3]<-(-9)}}
DATs<-c("#Number of obs",length(age),"#Age vector",Data[,1],"#Percent agreement vector",Data[,2],"#Sample size vector",Data[,3])
write.table(DATs,file="ADMB_ageage/ageage.DAT",sep="",quote=F,row.names=F,col.names=F)

setwd("ADMB_ageage")

system("admb ageage")
system("./ageage")
STD<-read.delim("ageage.STD",sep="")

setwd("..")

# Calculate ageing error matrix out to length(age)+2
age_sd<-cbind(age,STD$value[3:(length(age)+2)])
colnames(age_sd)<-c("Age","SD")
SDs<-lm(age_sd[,2]~age_sd[,1])

ages<-seq(2,42)
ages_sd<-cbind(ages,coef(SDs)[1]+coef(SDs)[2]*ages)
ae_mtx_100<-matrix(nrow=length(ages),ncol=length(ages))
colnames(ae_mtx_100)<-ages
rownames(ae_mtx_100)<-ages
for(j in 1:length(ages)){
ae_mtx_100[j,1]<-pnorm(ages[1]+0.5,ages[j],ages_sd[which(ages_sd[,1]==ages[j]),2])}
for(i in 2:(length(ages)-1)){
for(j in 1:length(ages)){
ae_mtx_100[j,i]<-pnorm(ages[i]+0.5,ages[j],ages_sd[which(ages_sd[,1]==ages[j]),2])-pnorm(ages[i-1]+0.5,ages[j],ages_sd[which(ages_sd[,1]==ages[j]),2])}}
for(j in 1:length(ages)){
ae_mtx_100[j,length(ages)]<-1-sum(ae_mtx_100[j,1:(length(ages)-1)])}
write.csv(ae_mtx_100,"output/aging_error.csv",row.names=FALSE)
write.csv(age_sd,"output/aging_error_SD.csv")

# Compute ageing error matrix for model - NOTE: for YE, unnecessary as the matrix is recage - + age and does not exceed those dimensions
#ae_Mdl<-matrix(nrow=length(ages),ncol=nages_D)
#ae_Mdl[,1:(nages_D)]<-as.matrix(ae_mtx_100[,1:(nages_D)])
#ae_Mdl[,nages_D]<-rowSums(ae_mtx_100[,nages_D:length(ages)])
#ae_Mdl<-round(ae_Mdl,digits=4)
#r<-which(ae_Mdl[,nages_D]>=0.999)[1]
#ae_Mdl<-ae_Mdl[1:r,]

#ae_Mdl}
}

get_ageage(710,2,42)

