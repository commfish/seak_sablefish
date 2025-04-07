################################################################################
# Survey Gear Experiments 2022-2023 (analyzed in 2024)
#
# The groundfish team has conducted a number of different side by side gear
# comparisons to keep up with the changing fishery as it transitions from longline
# gear to slinky pots. The following comparison tests have been performed:
#
# 1) Chatham 2022 Marking Survey: Slinky versus conical pot comparison, pots fished
#   on same string with pot type alternating along the string
# 2) Clarence LL survey 2023 (SSEI): Slinky pots versus longline with two boats setting
#   the different gears as close in time and space as possible
# 3) Chatham LL survey 2023: Slinky pots versus longline with two boats setting
#   the different gears as close in time and space as possible
#
# I've moved all of these scripts into this one script for review and consolidation
#
# Clarence code is also in the ssei git/R project
# Chatham 2022 exam is found in 2023 folder as well
#
# Author: Phil Joy
# January 2024
#
################################################################################
source("r_helper/helper.r")
source("r_helper/functions.r")

{library(mosaic)
  library(tidyverse)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(ggridges)
  library(ggthemes)
  library(GGally)
  library(mgcViz)
  library(mgcv)
  library(lme4)
  library(boot)}

#-------------------------------------------------------------------------------
# 1) Chatham 2022 Marking Survey: Slinky versus conical pot comparison, pots fished
#   on same string with pot type alternating along the string

read.csv("2024/data/survey/Slinky_and_conical_pot_data_2022.csv") -> pot_dat22

str(pot_dat22)
unique(pot_dat22$Time.Second.Anchor.Overboard)
unique(pot_dat22$Time.First.Anchor.Onboard)
unique(pot_dat22$Time.Second.Anchor.Onboard)

parse_time3 <- function(x) {   #https://stackoverflow.com/questions/69738674/how-to-convert-time-hhmmss-to-decimal-form-in-r
  res <- do.call(rbind, strsplit(x, ":", TRUE))
  mode(res) <- "numeric"
  c(res %*% (1/c(1, 60)))
}

pot_dat22 <-pot_dat22 %>% 
  mutate(Date = as.Date(Time.Second.Anchor.Overboard,c("%m/%d/%Y")),
         soak.time = parse_time3(Soak.Time),
         set = Effort.Number,
         pot_no = Subset.Number,
         pot_type = as.factor(Pot.Type.1),
         pot_type_code = ifelse(pot_type == "Slinky",1,2),
         stat_area = Groundfish.Stat.Area,
         depth = Average.Depth.Fathoms,
         set_x_pot = paste0(set,"_",pot_no,sep=""),
         length = Length.Millimeters) %>% 
  select(Date,soak.time,depth, set,pot_no,set_x_pot,pot_type,pot_type_code,stat_area,length)

pot_dat22[is.na(pot_dat22$Date),]

set<-sort(rep(seq(1,24,1),20))
pot_no<-rep(seq(1,20,1),24)
sets_pots<-as.data.frame(cbind(set,pot_no)) %>% mutate(set_x_pot = paste0(set,"_",pot_no,sep=""))

fished<-unique(pot_dat22$set_x_pot)
allsets<-unique(sets_pots$set_x_pot)

blanks<-setdiff(allsets,fished)

blanks<-data.frame(cbind(NA,NA,NA,as.numeric(sub("_.*","",blanks)),
      as.numeric(substring(blanks,regexpr("_",blanks)+1)),blanks,
      NA,NA,NA,NA))
colnames(blanks)<-colnames(pot_dat22)
blanks<-blanks %>% mutate(set = as.numeric(set),pot_no=as.numeric(pot_no))

pot_dat22<-rbind(pot_dat22,blanks) %>% arrange(set,pot_no) 
str(pot_dat22)
pot_dat22$depth[is.na(pot_dat22$depth)] <- pot_dat22$depth[which(is.na(pot_dat22$depth))-1]
pot_dat22$Date[is.na(pot_dat22$Date)] <- pot_dat22$Date[which(is.na(pot_dat22$Date))-1]
pot_dat22$soak.time[is.na(pot_dat22$soak.time)] <- pot_dat22$soak.time[which(is.na(pot_dat22$soak.time))-1]
pot_dat22$stat_area[is.na(pot_dat22$stat_area)] <- pot_dat22$stat_area[which(is.na(pot_dat22$stat_area))-1]
view(pot_dat22)
pot_dat22$pot_type[is.na(pot_dat22$pot_type)] <- pot_dat22$pot_type[which(is.na(pot_dat22$pot_type))-1]
pot_dat22<-pot_dat22 %>% mutate(pot_type = ifelse(is.na(pot_type_code) & pot_type %in% c("Slinky"),"Cone",
                                              ifelse(is.na(pot_type_code) & pot_type %in% c("Cone"),"Slinky",
                                                     ifelse(!is.na(pot_type_code),as.character(pot_type),NA))),
                            length = as.numeric(length),
                            depth = as.numeric(depth))

view(pot_dat22)
str(pot_dat22)
which(is.na(pot_dat22$pot_type_code))

pot_dat22[c(8:10,3101:3103,3133:3135,5724:5726,6107:6109),]

#get rid of sets that were all slinky or all conical...
check <- pot_dat22 %>% group_by(set, pot_type) %>%
  summarise(n = n())
view(check)

pot_comps<-pot_dat22[pot_dat22$set < 21 & pot_dat22$set != 9,]
unique(pot_comps$set)
#-------------------------------------------------------------------------------
# Straight CPUE ...............
#-------------------------------------------------------------------------------
CPUE_pot<-pot_comps %>% group_by(set, pot_no,pot_type) %>%
  summarise(n = sum(!is.na(length)))  
CPUE_set<-CPUE_pot %>% group_by(set,pot_type) %>%
  summarise(set_CPUE = mean(n),
         var_CPUE = var(n),
         sd_CPUE = sqrt(var_CPUE)) %>% ungroup()
view(CPUE_set)
plot(CPUE_set$set_CPUE[CPUE_set$pot_type == "Slinky"] ~ 
       CPUE_set$set_CPUE[CPUE_set$pot_type == "Cone"], ylim=c(0,40))
abline(lm(CPUE_set$set_CPUE[CPUE_set$pot_type == "Slinky"] ~ 
            CPUE_set$set_CPUE[CPUE_set$pot_type == "Cone"]))

CPUE_overall<-CPUE_set %>% group_by(pot_type) %>%
  summarise(count = n(),
            CPUE = mean(set_CPUE),
            var = var(set_CPUE),
            sd = sqrt(var)); CPUE_overall

#At this level there is no significant difference in CPUE

#-------------------------------------------------------------------------------
# Length break down
#-------------------------------------------------------------------------------
slinkys<-pot_comps %>% filter (!is.na(length) & pot_type == c("Slinky")) 
cones<-pot_comps %>% filter (!is.na(length) & pot_type == c("Cone"))  
  
plot(density(slinkys$length)); lines(density(cones$length), col="blue")
abline(v=mean(slinkys$length)); abline(v=mean(cones$length),col="blue")
#are the length distributions different?

ks.test(slinkys$length,cones$length)
#results show that the length distributions are significantly different! (P value < 0.05)

#lets plot the cumulative distribution functions of the data 
cdf_s<-ecdf(slinkys$length)
cdf_c<-ecdf(cones$length) 
plot(cdf_s, verticals=TRUE, do.points=FALSE, main="cdf", xlab="length")
plot(cdf_c, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")

# At this point it should be obvious that there are more little fish in group 1

# So, let's identify the length that we should stratify the data.  We do this by 
# finding the length where the two distributions are the furthest apart.  
# Here is a cute function to do that... 

D.func<-function(x,y){  #x<-C; y<-M
  n.x <- length(x)
  n.y <- length(y)
  n <- n.x * n.y/(n.x + n.y)
  w <- c(x, y)
  z <- cumsum(ifelse(order(w) <= n.x, 1/n.x, -1/n.y))
  max.at <- sort(w)[which(abs(z) == max(abs(z)))]
  return(max.at)
  
}

D.func(slinkys$length,cones$length)  #this tells you that the greatest distance between the distributions 
# is at the number you see (random numbers so might be different... 
# 46 last time I ran this)
abline(v=D.func(slinkys$length,cones$length), col="red")

# so, even though the overall CPUE is similar between gear, this shows that there
# is some difference between the length distributions with generally larger fish 
# caught in the conical pots.  Let's break it down a little further... 

#-------------------------------------------------------------------------------
Break1<-D.func(slinkys$length,cones$length)
sl_sm<-slinkys$length[slinkys$length<=Break1]
sl_lg<-slinkys$length[slinkys$length>Break1]
co_sm<-cones$length[cones$length<=Break1]
co_lg<-cones$length[cones$length>Break1]

#1) Lets look at the smaller fish first... 
ks.test(sl_sm,co_sm)

cdf_s_sm<-ecdf(sl_sm)
cdf_c_sm<-ecdf(co_sm) 
plot(cdf_s_sm, verticals=TRUE, do.points=FALSE, main="cdf", xlab="length")
plot(cdf_c_sm, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")
abline(v=D.func(sl_sm,co_sm), col="red")

#1b) further break
Break2<-D.func(sl_sm,co_sm)
sl_really_small<-sl_sm[sl_sm<=Break2]
sl_med_small<-sl_sm[sl_sm>Break2]
co_really_small<-co_sm[co_sm<=Break2]
co_med_small<-co_sm[co_sm>Break2]

ks.test(sl_really_small,co_really_small)

cdf_s_rsm<-ecdf(sl_really_small)
cdf_c_rsm<-ecdf(co_really_small) 
plot(cdf_s_rsm, verticals=TRUE, do.points=FALSE, main="cdf", xlab="length")
plot(cdf_c_rsm, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")
abline(v=D.func(sl_really_small,co_really_small), col="red")

ks.test(sl_med_small,co_med_small)

cdf_s_msm<-ecdf(sl_med_small)
cdf_c_msm<-ecdf(co_med_small) 
plot(cdf_s_msm, verticals=TRUE, do.points=FALSE, main="cdf", xlab="length")
plot(cdf_c_msm, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")
abline(v=D.func(sl_med_small,co_med_small), col="red")

# 3) Big fish.... 
ks.test(sl_lg,co_lg)
ks.test(sl_lg,co_lg, alternative="two.sided")
ks.test(sl_lg,co_lg, alternative="less")
ks.test(sl_lg,co_lg, alternative="greater")
?ks.test
cdf_s_lg<-ecdf(sl_lg)
cdf_c_lg<-ecdf(co_lg) 
plot(cdf_s_lg, verticals=TRUE, do.points=FALSE, main="cdf", xlab="length")
plot(cdf_c_lg, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")
abline(v=D.func(sl_lg,co_lg), col="red")
 #-- significantly different, but big sample size.  Distributions look pretty close

# 3a) one more split... 
Break3<-D.func(sl_lg,co_lg)
sl_med_big<-sl_lg[sl_lg<=Break3]
sl_really_big<-sl_lg[sl_lg>Break3]
co_med_big<-co_lg[co_lg<=Break3]
co_really_big<-co_lg[co_lg>Break3]

ks.test(sl_med_big,co_med_big)

cdf_s_mbg<-ecdf(sl_med_big)
cdf_c_mbg<-ecdf(co_med_big) 
plot(cdf_s_mbg, verticals=TRUE, do.points=FALSE, main="cdf", xlab="length")
plot(cdf_c_mbg, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")
abline(v=D.func(sl_med_big,co_med_big), col="red")

ks.test(sl_really_big,co_really_big)

cdf_s_rbg<-ecdf(sl_really_big)
cdf_c_rbg<-ecdf(co_really_big) 
plot(cdf_s_rbg, verticals=TRUE, do.points=FALSE, main="cdf", xlab="length")
plot(cdf_c_rbg, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")
abline(v=D.func(sl_really_big,co_really_big), col="red")

#-------------------------------------------------------------------------------
# Lets look at CPUE for each of 4 size categories... 
plot(cdf_s, verticals=TRUE, do.points=FALSE, main="cdf", xlab="length")
plot(cdf_c, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")
abline(v=c(Break1,Break2,Break3), col="red")

#lets add two more breaks
Break4<-D.func(sl_really_big,co_really_big)
Break5<-D.func(sl_really_small,co_really_small)
abline(v=c(Break4,Break5),col="red")

#break5 same as Break 2
breaks<-c(0,Break2,Break1,Break3,Break4,1000)

meanfun <- function(x, d) {
  return(mean(x[d]))
}

for (i in 2:length(breaks)) {  #i<-2
  bin <- pot_comps %>% filter(length > breaks[i-1] & length <= breaks[i])
  C_pot<-bin %>% group_by(set, pot_no,pot_type) %>%
    summarise(n = sum(!is.na(length)))  
  C_set<-C_pot %>% group_by(set,pot_type) %>%
    summarise(set_CPUE = mean(n),
              var_CPUE = var(n),
              sd_CPUE = sqrt(var_CPUE)) %>% ungroup()
  t<-t.test(C_set$set_CPUE[C_set$pot_type == "Slinky"],
            C_set$set_CPUE[C_set$pot_type == "Cone"])
  tv<-t$statistic; pv<-t$p.value
  
  boot_slinky<-boot(C_set$set_CPUE[C_set$pot_type == "Slinky"],
                    statistic=meanfun, R=1000)
  bo_sl.ci<-boot.ci(boot_slinky, conf=0.95, type="bca")
  #bo_sl.ci$bca[4]; bo_sl.ci$bca[4]
  boot_cone<-boot(C_set$set_CPUE[C_set$pot_type == "Cone"],
                    statistic=meanfun, R=1000)
  bo_co.ci<-boot.ci(boot_cone, conf=0.95, type="bca")
  
  C_overall<-C_set %>% group_by(pot_type) %>%
    summarise(count = n(),
              CPUE = mean(set_CPUE),
              var = var(set_CPUE),
              sd = sqrt(var)) %>% 
    mutate(min_l = breaks[i-1],
           max_l = breaks[i],
           t = tv,
           p.value = pv,
           boot_lo = NA,
           boot_hi = NA)
  
  C_overall$boot_lo[C_overall$pot_type == "Slinky"]<-bo_sl.ci$bca[4]
  C_overall$boot_hi[C_overall$pot_type == "Slinky"]<-bo_sl.ci$bca[5]
  C_overall$boot_lo[C_overall$pot_type == "Cone"]<-bo_co.ci$bca[4]
  C_overall$boot_hi[C_overall$pot_type == "Cone"]<-bo_co.ci$bca[5]
  
  if(i == 2) {
    out<-C_overall
  } else {
    out<-rbind(out,C_overall)
  }
}

t<-t.test(CPUE_set$set_CPUE[CPUE_set$pot_type == "Slinky"],
          CPUE_set$set_CPUE[CPUE_set$pot_type == "Cone"])
tv<-t$statistic; pv<-t$p.value

boot_slinky<-boot(CPUE_set$set_CPUE[CPUE_set$pot_type == "Slinky"],
                  statistic=meanfun, R=1000)
bo_sl.ci<-boot.ci(boot_slinky, conf=0.95, type="all")
#bo_sl.ci$bca[4]; bo_sl.ci$bca[4]
boot_cone<-boot(CPUE_set$set_CPUE[CPUE_set$pot_type == "Cone"],
                statistic=meanfun, R=1000)
bo_co.ci<-boot.ci(boot_cone, conf=0.95, type="bca")

CPUE_sum<-rbind(out,CPUE_overall %>% mutate(min_l = 0, max_l = 1000, t = tv, 
                                            p.value = pv,
                                            boot_lo = NA, boot_hi = NA)) %>%
  mutate(size_cat = paste0(min_l," - ",max_l))

CPUE_sum$boot_lo[CPUE_sum$pot_type == "Slinky" & CPUE_sum$size_cat == "0 - 1000"]<-bo_sl.ci$bca[4]
CPUE_sum$boot_hi[CPUE_sum$pot_type == "Slinky" & CPUE_sum$size_cat == "0 - 1000"]<-bo_sl.ci$bca[5]
CPUE_sum$boot_lo[CPUE_sum$pot_type == "Cone" & CPUE_sum$size_cat == "0 - 1000"]<-bo_co.ci$bca[4]
CPUE_sum$boot_hi[CPUE_sum$pot_type == "Cone" & CPUE_sum$size_cat == "0 - 1000"]<-bo_co.ci$bca[5]

ggplot(CPUE_sum %>% filter(size_cat != "0 - 1000"),aes(x=size_cat,y=CPUE, col=pot_type)) +
  geom_point(aes(fill = pot_type),size=5,position=position_dodge(width=0.2)) +
  geom_errorbar(aes(y=CPUE,ymin=boot_lo,ymax=boot_hi),
                position=position_dodge(width=0.2), width=0.5, size=0.8) +
  theme(legend.position = c(0.8, 0.80),
        axis.text = element_text(size=12),
        axis.title = element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=0),
        panel.background = element_rect(fill = "white", color = 'purple'),
        panel.grid.major = element_line(color = 'lightgrey')) +
  #theme_grey() +
  #scale_colour_viridis_d(option = "C",end=0.8) +
  #scale_x_discrete(limits=factor(c(4:11,"12p"))) + #,labels=3:17
  scale_y_continuous(breaks=seq(0,9,1), limits=c(0,9)) +
  ylab("CPUE (sablefish/pot)") +
  xlab("Size category (mm)") -> CPUEplot

png(file.path("2023/figures/Slinky_conical_comp_2022.png"),
    width=6,height=5,#width=9.5,height=8.5,
    units="in",res=1200)
CPUEplot
dev.off()

ggsave(file.path("2024/figures/Slinky_conical_comp_2022.png"), dpi = 500, height = 6, width = 7, units = "in")
  
write.csv(CPUE_sum,
          "2024/output/NSEI_surv_pot_comp_2022.csv")

#-------------------------------------------------------------------------------
# 2) Clarence LL survey 2023 (SSEI): Slinky pots versus longline with two boats setting
#   the different gears as close in time and space as possible

ss23_ll_bio<-read.csv("2024/data/survey/2023 SSEI LL Survey Bio Data.csv")
ss23_ll_set<-read.csv("2024/data/survey/2023 SSEI LL Survey Set Data.csv")
ss23_pot<-read.csv("2024/data/survey/SSEI Pot Survey Data 2023.csv")

ss23_ll_bio<-left_join(ss23_ll_bio %>% 
                    select(Trip.No,Adfg.No,Effort.No,Station.No,G.Stat.Area,Species,Specimen.No,
                           Sex, Maturity, Length = Length.Millimeters, Weight = Weight.Kilograms),
                  unique(ss23_ll_set %>% group_by(Set.No) %>% 
                           mutate(Sable.set.cpue = sum(Sablefish)) %>% 
                           #group_by(Subset.No) %>%
                           #mutate(Sable.skate.cpue = sum(Sablefish)) %>% 
                           ungroup() %>%
                           select(Trip.No, Adfg.No, Effort.No = Set.No, G.Stat.Area, Station.No, 
                                  Start.lat = Start.Latitude.Decimal.Degrees,
                                  Start.Lon = Start.Longitude.Decimal.Degree,
                                  Soak = Set.Soak.Time, Depth = Avg.Depth.Fathoms,
                                  Substrate = Substrate.Type, Sable.set.cpue) ),
                  by = c("Trip.No", "Adfg.No", "Effort.No", "Station.No", "G.Stat.Area")) %>%
  mutate(Gear = "ll")

head(ss23_ll_bio,30)
ss23_ll_bio[c(30:60),]
ss23_ll_bio %>% filter(Effort.No == 4)
plot(ss23_ll_bio$Sable.set.cpue ~ ss23_ll_bio$Effort.No)

plot(ss23_ll_bio$Sablefish.No ~ ss23_ll_bio$Effort.No)

ll_cpue<-ss23_ll_set %>% group_by(Set.No) %>% 
  mutate(Sable.set.cpue = sum(Sablefish),
         Sleeper.Shark.set.cpue = sum(Sleeper.Shark), 
         Skate.Longnose.set.cpue = sum(Skate.Longnose), 
         Skate.General.set.cpue = sum(Skate.General),
         Skate.Big.set.cpue = sum(Skate.Big), 
         Shortraker.set.cpue = sum(Shortraker), 
         Rougheye.set.cpue = sum(Rougheye), 
         Rockfish.Other.set.cpue = sum(Rockfish.Other), 
         Rex.Sole.set.cpue = sum(Rex.Sole),
         Redbanded.set.cpue = sum(Redbanded), 
         Pacific.Cod.set.cpue = sum(Pacific.Cod), 
         Other.set.cpue = sum(Other), 
         Idiot.set.cpue = sum(Idiot), 
         Grenadier.set.cpue = sum(Grenadier), 
         English.Sole.set.cpue = sum(English.Sole), 
         Dover.Sole.set.cpue = sum(Dover.Sole), 
         Dogfish.set.cpue = sum(Dogfish), 
         Coral.set.cpue = sum(Coral), 
         Brown.King.Crab.set.cpue = sum(Brown.King.Crab), 
         Arrowtooth.set.cpue = sum(Arrowtooth), 
         Yelloweye.set.cpue = sum(Yelloweye), 
         Halibut.set.cpue = sum(Halibut)) %>% ungroup() %>% 
  #summarize()
  select(Trip.No, Adfg.No, Effort.No = Set.No, Subset.No, G.Stat.Area, Station.No, 
         Start.lat = Start.Latitude.Decimal.Degrees,
         Start.Lon = Start.Longitude.Decimal.Degree,
         Soak = Set.Soak.Time, Depth = Avg.Depth.Fathoms,
         Substrate = Substrate.Type,
         Sablefish.skate.cpue = Sablefish, Sable.set.cpue,
         Sleeper.Shark.skate.cpue = Sleeper.Shark, Sleeper.Shark.set.cpue,
         Skate.Longnose.skate.cpue = Skate.Longnose, Skate.Longnose.set.cpue,
         Skate.General.skate.cpue = Skate.General, Skate.General.set.cpue,
         Skate.Big.skate.cpue = Skate.Big, Skate.Big.set.cpue,
         Shortraker.skate.cpue = Shortraker, Shortraker.set.cpue,
         Rougheye.skate.cpue = Rougheye, Rougheye.set.cpue, 
         Rockfish.Other.skate.cpue = Rockfish.Other, Rockfish.Other.set.cpue,
         Rex.Sole.skate.cpue = Rex.Sole, Rex.Sole.set.cpue,
         Redbanded.skate.cpue = Redbanded, Redbanded.set.cpue, 
         Pacific.Cod.skate.cpue = Pacific.Cod, Pacific.Cod.set.cpue,
         Other.skate.cpue = Other, Other.set.cpue,
         Idiot.skate.cpue = Idiot, Idiot.set.cpue, 
         Grenadier.skate.cpue = Grenadier, Grenadier.set.cpue, 
         English.Sole.skate.cpue = English.Sole, English.Sole.set.cpue, 
         Dover.Sole.skate.cpue = Dover.Sole, Dover.Sole.set.cpue, 
         Dogfish.skate.cpue = Dogfish, Dogfish.set.cpue, 
         Coral.skate.cpue = Coral, Coral.set.cpue, 
         Brown.King.Crab.skate.cpue = Brown.King.Crab, Brown.King.Crab.set.cpue, 
         Arrowtooth.skate.cpue = Arrowtooth, Arrowtooth.set.cpue, 
         Yelloweye.skate.cpue = Yelloweye, Yelloweye.set.cpue, 
         Halibut.skate.cpue = Halibut, Halibut.set.cpue)


str(ss23_pot)
unique(ss23_pot$Species)
ss23_pot<-ss23_pot %>% select(Trip.No = Trip.Number, Adfg.No = ADFG.Number,
                    Effort.No = Effort.Number,
                    Subset.No = Pot.Number.in.Order.Set,
                    G.Stat.Area = Groundfish.Stat.Area,
                    Station.No = Station.Number,
                    Start.lat = Start.Latitude.Decimal.Degrees,
                    Start.Lon = Start.Longitude.Decimal.Degrees,
                    Soak = Soak.Time, Depth = Average.Depth.Fathoms,
                    No.Pots = Number.of.Pots.Retrieved,
                    Pot.Type, Pot.Type.1, Substrate = Substrate.Type,
                    Specimen.No = Specimen.Number, Species, 
                    Length = Length.Millimeters) %>% 
  mutate(Gear = "pot")

str(ss23_ll_bio)
str(ss23_pot)

#-------------------------------------------------------------------------------

bios<-rbind(ss23_ll_bio %>% select(-c(Sable.set.cpue, Weight, Sex, Maturity)) %>%
              mutate(Pot.Type = "Longline",
                     Pot.Type.1 = "Longline") %>%
              filter(Species == "Sablefish"),
            ss23_pot %>% select(-c(No.Pots, Subset.No)) %>% filter(Species == "Sablefish"))  


cpue<-rbind(ll_cpue %>% select(Trip.No, Adfg.No, Effort.No, Subset.No, G.Stat.Area,
                               Station.No, Start.lat, Start.Lon, Soak, Depth, Substrate,
                               set.cpue = Sable.set.cpue, sub.cpue = Sablefish.skate.cpue) %>%
              mutate(Gear = "ll", Pot.Type = "ll", Pot.Type.1 = "Longline", No.subs = 25, Gear = "ll") %>%
              data.frame(),
            unique(ss23_pot %>% select(-c(Specimen.No,Length)) %>%
                     group_by(Effort.No) %>% mutate(set.cpue = n()) %>%
                     group_by(Effort.No, Subset.No) %>% mutate(sub.cpue = n(),
                                                               No.subs = No.Pots) %>% 
                     select(-c(No.Pots, Species)) %>% data.frame())) %>%
  mutate(soak.time = hm(Soak)@hour+hm(Soak)@minute/60)

str(cpue)
sort(unique(cpue$Station.No))

eg12<-cpue %>% filter(Station.No == 12)
View(eg12); unique(eg12$Gear)
eg18<-cpue %>% filter(Station.No == 18)
unique(eg18$Gear)
eg37<-cpue %>% filter(Station.No == 37)
unique(eg37$Gear)
eg101<-cpue %>% filter(Station.No == 101)
unique(eg101$Gear)

# get the stations that had side by side fishing of pot and ll gear
ss_comp_st <- cpue %>% group_by(Station.No) %>%
  dplyr::summarise(no.gears = length(unique(Gear))) %>%
  filter(no.gears > 1) %>% select(Station.No) %>% data.frame()

str(comp_st)
#-------------------------------------------------------------------------------
# cpue by skate/pot
#cpue_comp %>% group_by(Pot.Type.1) %>%
#  dplyr::summarize(sub_samples = n(),
#                   mean_cpue = mean(sub.cpue))
#cpue_comp_w<-cpue %>% filter(Station.No %in% c(12,37,44,103,107,109,120,122,125))

ss_cpue_comp<-cpue %>% filter(Station.No %in% comp_st$Station.No)

ss_cpue_comp %>% group_by(Station.No,Pot.Type.1) %>% 
  dplyr::summarize(sub_samples = n(),
                   mean_cpue = mean(sub.cpue),
                   std = sd(sub.cpue)) -> ss_pot_skate_cpue

ss_pot_skate_cpue %>% group_by(Pot.Type.1) %>%
  dplyr::summarize(sets = n(),
                   cpue = mean(mean_cpue),
                   mean_std = sqrt(sum(std^2)/sets),
                   cpue_lo95 = cpue - 1.96*mean_std,
                   cpue_hi95 = cpue + 1.96*mean_std) -> ss_mean_potskate_cpue

ss_cpue_comp %>% group_by(Station.No,Gear) %>% 
  dplyr::summarize(sub_samples = n(),
                   cpue = mean(set.cpue)) -> ss_set_cpue

ss_cpue_comp %>% group_by(Station.No,Pot.Type.1) %>% 
  dplyr::summarize(sub_samples = n(),
                   cpue = mean(set.cpue)) -> ss_set_cpue_det

#---------------------------------------------------------------------------

colnames(bios)

cols <- c("#F76D5E", "#FFFFBF", "#72D8FF")

# Basic density plot in ggplot2
ggplot(bios, aes(x = Length, fill = Gear)) +
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = cols) + 
  geom_vline(xintercept=mean(bios$Length[bios$Gear == "ll"]), col = cols[1]) +
  geom_vline(xintercept=mean(bios$Length[bios$Gear == "pot"], na.rm=T), col = cols[2]) 

getwd()
ggsave(paste0("2024/figures/ssei23_length_distr_pot_vs_ll.png"), dpi = 300, height = 5, width = 5, units = "in")

ggplot(bios, aes(x = Length, fill = Pot.Type.1)) +
  geom_density(alpha = 0.5) + 
  scale_fill_manual(values = cols) +
  geom_vline(xintercept=mean(bios$Length[bios$Pot.Type.1 == "Lg. Slinky"], na.rm=T), col = cols[1]) +
  geom_vline(xintercept=mean(bios$Length[bios$Pot.Type.1 == "Longline"], na.rm=T), col = cols[2]) +
  geom_vline(xintercept=mean(bios$Length[bios$Pot.Type.1 == "Sm. Slinky"], na.rm=T), col = cols[3])

ggsave(paste0("2024/figures/ssei23_length_distr_pot_vs_ll_2.png"), dpi = 300, height = 5, width = 5, units = "in")

colnames(cpue)

ggplot(cpue,aes(x=set.cpue, col = Gear, fill = Gear)) + 
  geom_density(alpha=0.5) + #, aes(y=..count../sum(..count..))) +
  xlab("set cpue")
ggplot(cpue,aes(x=sub.cpue, col = Gear, fill = Gear)) + 
  geom_density(alpha=0.5) + #, aes(y = stat(count / sum(count)))) +
  xlab("Skate/Pot cpue")
ggplot(cpue,aes(x=sub.cpue, col = Pot.Type.1, fill = Pot.Type.1)) + 
  #geom_histogram(alpha=0.5) +
  geom_density(alpha=0.5) +
  xlab("Skate/Pot cpue")

ggplot(ss_cpue_comp,aes(x=sub.cpue, col = Gear, fill = Gear)) + geom_histogram(alpha=0.5) +
  xlab("Skate/Pot cpue")
ggplot(ss_cpue_comp,aes(x=sub.cpue, col = Pot.Type.1, fill = Pot.Type.1)) + geom_histogram(alpha=0.5) +
  xlab("Skate/Pot cpue")

ggplot(data=ss_cpue_comp, aes(x=Gear, y=set.cpue, color = Gear, fill = Gear)) + 
  geom_bar(stat = "summary", fun = "mean") +
  ylab("Fish per set") +
  facet_wrap(~Station.No)

ggsave(paste0("2024/figures/ssei_cpue_pot_vs_ll_by_station.png"), dpi = 300, height = 5, width = 5, units = "in")

ggplot(data=ss_cpue_comp, mapping=aes(x=Pot.Type.1 , y=sub.cpue, color = Pot.Type.1, fill = Pot.Type.1))+
  geom_bar(stat = "summary", fun = "mean") +
  xlab("Fish per skate/pot") +
  facet_wrap(~Station.No)

ggsave(paste0("2024/figures/ssei_cpue_pot_vs_ll_by_station2.png"), dpi = 300, height = 5, width = 5, units = "in")

ggplot(data=ss_set_cpue, mapping=aes(x=Gear, y=cpue, fill = Gear),alpha=0.7)+
  geom_boxplot() +
  #geom_violin() +
  geom_jitter(width=0.1, height=0, col="black", size=2, alpha=0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  xlab("Fish per set")

ggsave(paste0("2024/figures/ssei_cpue_byset_pot_vs_ll.png"), dpi = 300, height = 5, width = 5, units = "in")


#ggplot(data=cpue_comp, mapping=aes(x=set.cpue, y=Pot.Type.1, fill = Pot.Type.1))+geom_boxplot() +
#  stat_summary(fun = "mean", geom = "point", shape = 8,
#               size = 2, color = "black")  +
#  coord_flip() + xlab("Fish per set")

#ggplot(data=cpue_comp, mapping=aes(x=sub.cpue, y=Gear, fill = Gear))+geom_boxplot() +
#  stat_summary(fun = "mean", geom = "point", shape = 8,
#               size = 2, color = "black")  +
#  coord_flip() + xlab("Fish per skate or pot")

ggplot(data=ss_pot_skate_cpue, mapping=aes(x=Pot.Type.1, y=mean_cpue, fill = Pot.Type.1))+
  geom_boxplot(outlier.shape=NA) +
  #geom_violin()+
  geom_jitter(width=0.1, height=0, col="black", size=2, alpha=0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  xlab("Fish per skate or pot")           
#ggsave(paste0("2024/figures/ssei_cpue_byset_2pot_vs_ll.png"), dpi = 300, height = 5, width = 5, units = "in") 

ggplot(data=ss_cpue_comp, mapping=aes(x=Gear, y=set.cpue, fill = Gear))+
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  xlab("Fish per set") +
  facet_wrap(~Substrate)

ggplot(data=ss_cpue_comp, mapping=aes(x=Gear, y=set.cpue, fill = Gear))+geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  xlab("Fish per set") +
  facet_wrap(~G.Stat.Area)

ggplot(data=ss_cpue_comp, mapping=aes(x=sub.cpue, y=Pot.Type.1, fill = Pot.Type.1))+geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  coord_flip() + xlab("Fish per skate/pot") +
  facet_wrap(~G.Stat.Area)

#ggplot(cpue_comp, aes(soak.time, sub.cpue, col=Pot.Type.1, fill = Pot.Type.1)) + geom_point(shape = 2) + 
#  geom_smooth(size = 2, se = TRUE, method = "glm")  #"lm", "glm", "gam", "loess"

#ggplot(cpue_comp, aes(Depth, sub.cpue, col=Pot.Type.1, fill = Pot.Type.1)) + geom_point(shape = 2) + 
#  geom_smooth(size = 2, se = TRUE, method = "loess")

ggplot(ss_cpue_comp, aes(soak.time, set.cpue, col=Gear, fill = Gear)) + geom_point(shape = 2) + 
  geom_smooth(size = 2, se = TRUE, method = "glm")  #"lm", "glm", "gam", "loess"

ggplot(ss_cpue_comp, aes(Depth, set.cpue, col=Gear, fill = Gear)) + geom_point(shape = 2) + 
  geom_smooth(size = 2, se = TRUE, method = "glm")

plot(data=ss_cpue_comp, soak.time ~ Depth); abline(lm(data=ss_cpue_comp, soak.time ~ Depth))

mod<-bam(data = ss_cpue_comp, set.cpue ~ s(G.Stat.Area, bs='re') + s(Station.No, bs = 're') +
           s(soak.time, k=3, m=1) + s(Depth, k=3) + Pot.Type.1,
         gamma=1.4)
summary(mod) 
plot(mod)

#mod<-lmer(data = cpue_comp, sub.cpue ~ s(G.Stat.Area, bs='re') +
#           s(soak.time, k=3, m=1) + s(Depth, k=3) + Pot.Type.1)

lmer <-lmer(data = ss_cpue_comp, set.cpue ~ soak.time*Depth*Pot.Type.1*(1|G.Stat.Area))
help('isSingular')
summary(lmer)
anova(lmer)

{library(arm)
  library(plyr)
  library(MuMIn)   #r.squaredGLMM(object)
  
  library(glmulti)
  library(AICcmodavg)}


lmer.glmulti <- function (formula, data, random = "", ...) {
  lmer(paste(deparse(formula), random), data = data, REML=F, ...)
}

setMethod('getfit', 'merMod', function(object, ...) {
  summ<-summary(object)$coef
  summ1<-summ[,1:2]
  if (length(dimnames(summ)[[1]])==1) {
    summ1<-matrix(summ1, nr=1, dimnames=list(c("(Intercept)"),c("Estimate","Std.Error")))
  }
  cbind(summ1, df=rep(10000,length(fixef(object))))
})

runmodels<-glmulti(sub.cpue ~ soak.time*Depth*Pot.Type.1,#s(set_depth, k=4), 
                   data=ss_cpue_comp,
                   method="h",imm=0.5, sexrate=0.1, crit=aicc , deltaM=0.01, 
                   conseq=20, marginality=T, maxsize=5, confsetsize=17, plotty=T,
                   level=2, fitfunc=lmer.glmulti, random="+(1|G.Stat.Area)+(1|Station.No)")
coef.glmulti(runmodels)
plot(runmodels,type="s")

summary(runmodels)
summary(runmodels)$bestmodel

runmodels@objects


simp<-glmulti(sub.cpue ~ soak.time*Depth*Pot.Type.1,#s(set_depth, k=4), 
              data=ss_cpue_comp,
              method="h",imm=0.5, sexrate=0.1, crit=aicc , deltaM=0.01, 
              conseq=20, marginality=T, maxsize=5, confsetsize=17, plotty=T,
              level=1, fitfunc=lmer.glmulti, random="+(1|G.Stat.Area)+(1|Station.No)")
coef.glmulti(simp)
plot(simp,type="s")

summary(simp)
summary(simp)$bestmodel

simp@objects

#-------------------------------------------------------------------------------
# 3) Chatham LL survey 2023: Slinky pots versus longline with two boats setting
#   the different gears as close in time and space as possible

ns23_ll_bio<-read.csv("2024/data/survey/2023 NSEI LL Survey Bio Data.csv")
ns23_ll_set<-read.csv("2024/data/survey/2023 NSEI LL Survey Set Data.csv")
ns23_pot<-read.csv("2024/data/survey/NSEI Pot Survey Data.csv")

ns23_ll_bio<-left_join(ns23_ll_bio %>% 
                         select(Trip.No, Adfg.No, Effort.No, Station.No, G.Stat.Area, 
                                Species, Specimen.No,
                                Sex, Maturity, 
                                Length = Length.Millimeters, 
                                Weight = Weight.Kilograms),
                       unique(ns23_ll_set %>% group_by(Set.No) %>% 
                                mutate(Sable.set.cpue = sum(Sablefish)) %>% 
                                #group_by(Subset.No) %>%
                                #mutate(Sable.skate.cpue = sum(Sablefish)) %>% 
                                ungroup() %>%
                                select(Trip.No, Adfg.No, Effort.No = Set.No, G.Stat.Area, Station.No, 
                                       Start.lat = Start.Latitude.Decimal.Degrees,
                                       Start.Lon = Start.Longitude.Decimal.Degree,
                                       Soak = Set.Soak.Time, Depth = Avg.Depth.Fathoms,
                                       Substrate = Substrate.Type, Sable.set.cpue) ),
                       by = c("Trip.No", "Adfg.No", "Effort.No", "Station.No", "G.Stat.Area")) %>%
  mutate(Gear = "ll")

head(ns23_ll_bio,30)
ns23_ll_bio[c(30:60),]
ns23_ll_bio %>% filter(Effort.No == 4)
plot(ns23_ll_bio$Sable.set.cpue ~ ns23_ll_bio$Effort.No)

ns_ll_cpue<-ns23_ll_set %>% group_by(Set.No) %>% 
  mutate(Sable.set.cpue = sum(Sablefish),
         Sleeper.Shark.set.cpue = sum(Sleeper.Shark), 
         Skate.Longnose.set.cpue = sum(Skate.Longnose), 
         Skate.General.set.cpue = sum(Skate.General),
         Skate.Big.set.cpue = sum(Skate.Big), 
         Shortraker.set.cpue = sum(Shortraker), 
         Rougheye.set.cpue = sum(Rougheye), 
         Rockfish.Other.set.cpue = sum(Rockfish.Other), 
         Rex.Sole.set.cpue = sum(Rex.Sole),
         Redbanded.set.cpue = sum(Redbanded), 
         Pacific.Cod.set.cpue = sum(Pacific.Cod), 
         Other.set.cpue = sum(Other), 
         Idiot.set.cpue = sum(Idiot), 
         Grenadier.set.cpue = sum(Grenadier), 
         English.Sole.set.cpue = sum(English.Sole), 
         Dover.Sole.set.cpue = sum(Dover.Sole), 
         Dogfish.set.cpue = sum(Dogfish), 
         Coral.set.cpue = sum(Coral), 
         Brown.King.Crab.set.cpue = sum(Brown.King.Crab), 
         Arrowtooth.set.cpue = sum(Arrowtooth), 
         Yelloweye.set.cpue = sum(Yelloweye), 
         Halibut.set.cpue = sum(Halibut)) %>% ungroup() %>% 
  #summarize()
  select(Trip.No, Adfg.No, Effort.No = Set.No, Subset.No, G.Stat.Area, Station.No, 
         Start.lat = Start.Latitude.Decimal.Degrees,
         Start.Lon = Start.Longitude.Decimal.Degree,
         Soak = Set.Soak.Time, Depth = Avg.Depth.Fathoms,
         Substrate = Substrate.Type,
         Sablefish.skate.cpue = Sablefish, Sable.set.cpue,
         Sleeper.Shark.skate.cpue = Sleeper.Shark, Sleeper.Shark.set.cpue,
         Skate.Longnose.skate.cpue = Skate.Longnose, Skate.Longnose.set.cpue,
         Skate.General.skate.cpue = Skate.General, Skate.General.set.cpue,
         Skate.Big.skate.cpue = Skate.Big, Skate.Big.set.cpue,
         Shortraker.skate.cpue = Shortraker, Shortraker.set.cpue,
         Rougheye.skate.cpue = Rougheye, Rougheye.set.cpue, 
         Rockfish.Other.skate.cpue = Rockfish.Other, Rockfish.Other.set.cpue,
         Rex.Sole.skate.cpue = Rex.Sole, Rex.Sole.set.cpue,
         Redbanded.skate.cpue = Redbanded, Redbanded.set.cpue, 
         Pacific.Cod.skate.cpue = Pacific.Cod, Pacific.Cod.set.cpue,
         Other.skate.cpue = Other, Other.set.cpue,
         Idiot.skate.cpue = Idiot, Idiot.set.cpue, 
         Grenadier.skate.cpue = Grenadier, Grenadier.set.cpue, 
         English.Sole.skate.cpue = English.Sole, English.Sole.set.cpue, 
         Dover.Sole.skate.cpue = Dover.Sole, Dover.Sole.set.cpue, 
         Dogfish.skate.cpue = Dogfish, Dogfish.set.cpue, 
         Coral.skate.cpue = Coral, Coral.set.cpue, 
         Brown.King.Crab.skate.cpue = Brown.King.Crab, Brown.King.Crab.set.cpue, 
         Arrowtooth.skate.cpue = Arrowtooth, Arrowtooth.set.cpue, 
         Yelloweye.skate.cpue = Yelloweye, Yelloweye.set.cpue, 
         Halibut.skate.cpue = Halibut, Halibut.set.cpue)


str(ns23_pot)
unique(ns23_pot$Species)
ns23_pot<-ns23_pot %>% select(Trip.No = Trip.Number, Adfg.No = ADFG.Number,
                              Effort.No = Effort.Number,
                              Subset.No = Subset.Number.in.Order.Set,
                              G.Stat.Area = Groundfish.Stat.Area,
                              Station.No = Station.Number,
                              Start.lat = Start.Latitude.Decimal.Degrees,
                              Start.Lon = Start.Longitude.Decimal.Degrees,
                              Soak = Soak.Time, Depth = Average.Depth.Fathoms,
                              No.Pots = Number.of.Pots.Retrieved,
                              Pot.Type, Pot.Type.1, Substrate = Substrate.Type,
                              Specimen.No = Specimen.Number, Species, 
                              Length = Length.Millimeters) %>% 
  mutate(Gear = "pot")

str(ns23_ll_bio)
str(ns23_pot)

#-------------------------------------------------------------------------------

ns_bios<-rbind(ns23_ll_bio %>% select(-c(Sable.set.cpue, Weight, Sex, Maturity)) %>%
              mutate(Pot.Type = "Longline",
                     Pot.Type.1 = "Longline") %>%
              filter(Species == "Sablefish"),
            ns23_pot %>% select(-c(No.Pots, Subset.No)) %>% filter(Species == "Sablefish"))  


ns_cpue<-rbind(ns_ll_cpue %>% select(Trip.No, Adfg.No, Effort.No, Subset.No, G.Stat.Area,
                               Station.No, Start.lat, Start.Lon, Soak, Depth, Substrate,
                               set.cpue = Sable.set.cpue, sub.cpue = Sablefish.skate.cpue) %>%
              mutate(Gear = "ll", Pot.Type = "ll", Pot.Type.1 = "Longline", No.subs = 25, Gear = "ll") %>%
              data.frame(),
            unique(ns23_pot %>% select(-c(Specimen.No,Length)) %>%
                     group_by(Effort.No) %>% mutate(set.cpue = n()) %>%
                     group_by(Effort.No, Subset.No) %>% mutate(sub.cpue = n(),
                                                               No.subs = No.Pots) %>% 
                     select(-c(No.Pots, Species)) %>% data.frame())) %>%
  mutate(soak.time = hm(Soak)@hour+hm(Soak)@minute/60)

str(cpue)
sort(unique(cpue$Station.No))

eg12<-cpue %>% filter(Station.No == 12)
View(eg12); unique(eg12$Gear)
eg18<-cpue %>% filter(Station.No == 18)
unique(eg18$Gear)
eg37<-cpue %>% filter(Station.No == 37)
unique(eg37$Gear)
eg101<-cpue %>% filter(Station.No == 101)
unique(eg101$Gear)

# get the stations that had side by side fishing of pot and ll gear
ns_comp_st <- ns_cpue %>% group_by(Station.No) %>%
  dplyr::summarise(no.gears = length(unique(Gear))) %>%
  filter(no.gears > 1) %>% select(Station.No) %>% data.frame()

str(ns_comp_st)
#-------------------------------------------------------------------------------
# cpue by skate/pot
#cpue_comp %>% group_by(Pot.Type.1) %>%
#  dplyr::summarize(sub_samples = n(),
#                   mean_cpue = mean(sub.cpue))
#cpue_comp_w<-cpue %>% filter(Station.No %in% c(12,37,44,103,107,109,120,122,125))

ns_cpue_comp<-ns_cpue %>% filter(Station.No %in% ns_comp_st$Station.No)

ns_cpue_comp %>% group_by(Station.No,Pot.Type.1) %>% 
  dplyr::summarize(sub_samples = n(),
                   mean_cpue = mean(sub.cpue),
                   std = sd(sub.cpue)) -> ns_pot_skate_cpue

ns_pot_skate_cpue %>% group_by(Pot.Type.1) %>%
  dplyr::summarize(sets = n(),
                   cpue = mean(mean_cpue),
                   mean_std = sqrt(sum(std^2)/sets),
                   cpue_lo95 = cpue - 1.96*mean_std,
                   cpue_hi95 = cpue + 1.96*mean_std) -> ns_mean_potskate_cpue

ns_cpue_comp %>% group_by(Station.No,Gear) %>% 
  dplyr::summarize(sub_samples = n(),
                   cpue = mean(set.cpue)) -> ns_set_cpue

ns_cpue_comp %>% group_by(Station.No,Pot.Type.1) %>% 
  dplyr::summarize(sub_samples = n(),
                   cpue = mean(set.cpue)) -> ns_set_cpue_det

#---------------------------------------------------------------------------

colnames(ns_bios)

cols <- c("#F76D5E", "#FFFFBF", "#72D8FF")

# Basic density plot in ggplot2
ggplot(ns_bios, aes(x = Length, fill = Gear)) +
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = cols) + 
  geom_vline(xintercept=mean(bios$Length[bios$Gear == "ll"]), col = cols[1]) +
  geom_vline(xintercept=mean(bios$Length[bios$Gear == "pot"], na.rm=T), col = cols[2]) 

getwd()
ggsave(paste0("2024/figures/nsei23_length_distr_pot_vs_ll.png"), dpi = 300, height = 5, width = 5, units = "in")

ggplot(ns_bios, aes(x = Length, fill = Pot.Type.1)) +
  geom_density(alpha = 0.5) + 
  scale_fill_manual(values = cols) +
  geom_vline(xintercept=mean(bios$Length[bios$Pot.Type.1 == "Lg. Slinky"], na.rm=T), col = cols[1]) +
  geom_vline(xintercept=mean(bios$Length[bios$Pot.Type.1 == "Longline"], na.rm=T), col = cols[2]) +
  geom_vline(xintercept=mean(bios$Length[bios$Pot.Type.1 == "Sm. Slinky"], na.rm=T), col = cols[3])

ggsave(paste0("2024/figures/nsei23_length_distr_pot_vs_ll_2.png"), dpi = 300, height = 5, width = 5, units = "in")

colnames(cpue)

ggplot(ns_cpue,aes(x=set.cpue, col = Gear, fill = Gear)) + 
  geom_density(alpha=0.5) + #, aes(y=..count../sum(..count..))) +
  xlab("set cpue")
ggplot(ns_cpue,aes(x=sub.cpue, col = Gear, fill = Gear)) + 
  geom_density(alpha=0.5) + #, aes(y = stat(count / sum(count)))) +
  xlab("Skate/Pot cpue")
ggplot(ns_cpue,aes(x=sub.cpue, col = Pot.Type.1, fill = Pot.Type.1)) + 
  #geom_histogram(alpha=0.5) +
  geom_density(alpha=0.5) +
  xlab("Skate/Pot cpue")

ggplot(ns_cpue_comp,aes(x=sub.cpue, col = Gear, fill = Gear)) + geom_histogram(alpha=0.5) +
  xlab("Skate/Pot cpue")
ggplot(ns_cpue_comp,aes(x=sub.cpue, col = Pot.Type.1, fill = Pot.Type.1)) + geom_histogram(alpha=0.5) +
  xlab("Skate/Pot cpue")

ggplot(data=ns_cpue_comp, aes(x=Gear, y=set.cpue, color = Gear, fill = Gear)) + 
  geom_bar(stat = "summary", fun = "mean") +
  ylab("Fish per set") +
  facet_wrap(~Station.No)

ggsave(paste0("2024/figures/ssei_cpue_pot_vs_ll_by_station.png"), dpi = 300, height = 5, width = 5, units = "in")

ggplot(data=ns_cpue_comp, mapping=aes(x=Pot.Type.1 , y=sub.cpue, color = Pot.Type.1, fill = Pot.Type.1))+
  geom_bar(stat = "summary", fun = "mean") +
  xlab("Fish per skate/pot") +
  facet_wrap(~Station.No)

ggsave(paste0("2024/figures/ssei_cpue_pot_vs_ll_by_station2.png"), dpi = 300, height = 5, width = 5, units = "in")

ggplot(data=ns_set_cpue, mapping=aes(x=Gear, y=cpue, fill = Gear),alpha=0.7)+
  geom_boxplot() +
  #geom_violin() +
  geom_jitter(width=0.1, height=0, col="black", size=2, alpha=0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  xlab("Fish per set")

ggsave(paste0("2024/figures/ssei_cpue_byset_pot_vs_ll.png"), dpi = 300, height = 5, width = 5, units = "in")


#ggplot(data=cpue_comp, mapping=aes(x=set.cpue, y=Pot.Type.1, fill = Pot.Type.1))+geom_boxplot() +
#  stat_summary(fun = "mean", geom = "point", shape = 8,
#               size = 2, color = "black")  +
#  coord_flip() + xlab("Fish per set")

#ggplot(data=cpue_comp, mapping=aes(x=sub.cpue, y=Gear, fill = Gear))+geom_boxplot() +
#  stat_summary(fun = "mean", geom = "point", shape = 8,
#               size = 2, color = "black")  +
#  coord_flip() + xlab("Fish per skate or pot")

ggplot(data=ns_pot_skate_cpue, mapping=aes(x=Pot.Type.1, y=mean_cpue, fill = Pot.Type.1))+
  geom_boxplot(outlier.shape=NA) +
  #geom_violin()+
  geom_jitter(width=0.1, height=0, col="black", size=2, alpha=0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  xlab("Fish per skate or pot")           
#ggsave(paste0("2024/figures/ssei_cpue_byset_2pot_vs_ll.png"), dpi = 300, height = 5, width = 5, units = "in") 

ggplot(data=ns_cpue_comp, mapping=aes(x=Gear, y=set.cpue, fill = Gear))+
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  xlab("Fish per set") +
  facet_wrap(~Substrate)

ggplot(data=ns_cpue_comp, mapping=aes(x=Gear, y=set.cpue, fill = Gear))+geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  xlab("Fish per set") +
  facet_wrap(~G.Stat.Area)

ggplot(data=ns_cpue_comp, mapping=aes(x=sub.cpue, y=Pot.Type.1, fill = Pot.Type.1))+geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  coord_flip() + xlab("Fish per skate/pot") +
  facet_wrap(~G.Stat.Area)

#ggplot(cpue_comp, aes(soak.time, sub.cpue, col=Pot.Type.1, fill = Pot.Type.1)) + geom_point(shape = 2) + 
#  geom_smooth(size = 2, se = TRUE, method = "glm")  #"lm", "glm", "gam", "loess"

#ggplot(cpue_comp, aes(Depth, sub.cpue, col=Pot.Type.1, fill = Pot.Type.1)) + geom_point(shape = 2) + 
#  geom_smooth(size = 2, se = TRUE, method = "loess")

ggplot(ns_cpue_comp, aes(soak.time, set.cpue, col=Gear, fill = Gear)) + geom_point(shape = 2) + 
  geom_smooth(size = 2, se = TRUE, method = "glm")  #"lm", "glm", "gam", "loess"

ggplot(ns_cpue_comp, aes(Depth, set.cpue, col=Gear, fill = Gear)) + geom_point(shape = 2) + 
  geom_smooth(size = 2, se = TRUE, method = "glm")

plot(data=ns_cpue_comp, soak.time ~ Depth); abline(lm(data=ss_cpue_comp, soak.time ~ Depth))

#-------------------------------------------------------------------------------
# Lets look at Clarence and Chatham more together:

ns_cpue_comp
ns_pot_skate_cpue
ns_set_cpue
ns_set_cpue
ns_cpue
ns_bios

ss_cpue_comp
ss_pot_skate_cpue
ss_set_cpue
ss_set_cpue
ss_cpue <- cpue
ss_bios <- bios

bios <- rbind(ss_bios %>% mutate(area = "Clarence"),
              ns_bios %>% mutate(area = "Chatham"))

ll_bio <- rbind(ss23_ll_bio %>% mutate(area = "Clarence"),
                ns23_ll_bio %>% mutate(area = "Chatham"))
cpue_comp <- rbind(ss_cpue %>% mutate(area = "Clarence") %>%
                filter(Station.No %in% ss_comp_st$Station.No),
              ns_cpue %>% mutate(area = "Chatham") %>% 
                filter(Station.No %in% ns_comp_st$Station.No))

cpue_comp %>% group_by(Station.No,Pot.Type.1,area) %>% 
  dplyr::summarize(sub_samples = n(),
                   mean_cpue = mean(sub.cpue),
                   std = sd(sub.cpue)) -> pot_skate_cpue

pot_skate_cpue %>% group_by(Pot.Type.1,area) %>%
  dplyr::summarize(sets = n(),
                   cpue = mean(mean_cpue),
                   mean_std = sqrt(sum(std^2)/sets),
                   cpue_lo95 = cpue - 1.96*mean_std,
                   cpue_hi95 = cpue + 1.96*mean_std) -> mean_potskate_cpue

cpue_comp %>% group_by(Station.No,Gear,area) %>% 
  dplyr::summarize(sub_samples = n(),
                   cpue = mean(set.cpue)) -> set_cpue

cpue_comp %>% group_by(Station.No,Pot.Type.1,area) %>% 
  dplyr::summarize(sub_samples = n(),
                   cpue = mean(set.cpue)) -> set_cpue_det

#---------------------------------------------------------------------------

colnames(bios)

library(wesanderson)
names(wes_palettes)
cols <- wes_palette("Darjeeling1",3,type="discrete")
#cols <- c("#F76D5E", "#FFFFBF", "#72D8FF")

# Basic density plot in ggplot2
ggplot(bios, aes(x = Length, fill = Gear)) +
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = cols) + 
  facet_wrap(~area) +
  geom_vline(xintercept=mean(bios$Length[bios$Gear == "ll"],na.rm=T), col = cols[1], size=1.5) +
  geom_vline(xintercept=mean(bios$Length[bios$Gear == "pot"], na.rm=T), col = cols[2], size = 1.5) 

getwd()
ggsave(paste0("2024/figures/length_distr_pot_vs_ll.png"), dpi = 300, height = 5, width = 5, units = "in")

ggplot(bios, aes(x = Length, fill = Pot.Type.1)) +
  geom_density(alpha = 0.5) + 
  scale_fill_manual(values = cols) +
  facet_wrap(~area) +
  geom_vline(xintercept=mean(bios$Length[bios$Pot.Type.1 == "Lg. Slinky"], na.rm=T),
             col = cols[1], size = 1.5, lty = 2) +
  geom_vline(xintercept=mean(bios$Length[bios$Pot.Type.1 == "Longline"], na.rm=T),
             col = cols[2], size = 1.5, lty = 3) +
  geom_vline(xintercept=mean(bios$Length[bios$Pot.Type.1 == "Sm. Slinky"], na.rm=T),
             col = cols[3], size = 1.5, lty = 3)

ggsave(paste0("2024/figures/length_distr_pot_vs_ll_2.png"), dpi = 300, height = 5, width = 7, units = "in")

colnames(cpue)

ggplot(cpue,aes(x=set.cpue, col = Gear, fill = Gear)) + 
  geom_density(alpha=0.5) + #, aes(y=..count../sum(..count..))) +
  facet_wrap(~area) +
  xlab("set cpue")
ggplot(cpue,aes(x=sub.cpue, col = Gear, fill = Gear)) + 
  geom_density(alpha=0.5) + #, aes(y = stat(count / sum(count)))) +
  facet_wrap(~area) +
  xlab("Skate/Pot cpue")
ggplot(cpue,aes(x=sub.cpue, col = Pot.Type.1, fill = Pot.Type.1)) + 
  facet_wrap(~area) +
  #geom_histogram(alpha=0.5) +
  geom_density(alpha=0.5) +
  xlab("Skate/Pot cpue")

ggplot(cpue_comp,aes(x=sub.cpue, col = Gear, fill = Gear)) + 
  geom_histogram(alpha=0.5) +
  facet_wrap(~area) +
  xlab("Skate/Pot cpue")

ggplot(cpue_comp,aes(x=sub.cpue, col = Pot.Type.1, fill = Pot.Type.1)) + 
  geom_histogram(alpha=0.5) +
  facet_wrap(~area) +
  xlab("Skate/Pot cpue")

ggplot(data=cpue_comp, aes(x=Gear, y=set.cpue, color = area, fill = Gear)) + 
  geom_bar(stat = "summary", fun = "mean") +
  ylab("Fish per set") +
  facet_wrap(~Station.No)

ggsave(paste0("2024/figures/ssei_cpue_pot_vs_ll_by_station.png"), dpi = 300, height = 5, width = 5, units = "in")

ggplot(data=cpue_comp, mapping=aes(x=Pot.Type.1 , y=sub.cpue, color = Pot.Type.1, fill = Pot.Type.1))+
  geom_bar(stat = "summary", fun = "mean") +
  xlab("Fish per skate/pot") +
  facet_wrap(~Station.No)

ggsave(paste0("2024/figures/ssei_cpue_pot_vs_ll_by_station2.png"), dpi = 300, height = 5, width = 5, units = "in")

ggplot(data=set_cpue, mapping=aes(x=Gear, y=cpue, fill = Gear),alpha=0.7)+
  geom_boxplot() +
  facet_wrap(~area) +
  geom_jitter(width=0.1, height=0, col="black", size=2, alpha=0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  xlab("Fish per set")

ggsave(paste0("2024/figures/ssei_cpue_byset_pot_vs_ll.png"), dpi = 300, height = 5, width = 5, units = "in")

#ggplot(data=cpue_comp, mapping=aes(x=set.cpue, y=Pot.Type.1, fill = Pot.Type.1))+geom_boxplot() +
#  stat_summary(fun = "mean", geom = "point", shape = 8,
#               size = 2, color = "black")  +
#  coord_flip() + xlab("Fish per set")

#ggplot(data=cpue_comp, mapping=aes(x=sub.cpue, y=Gear, fill = Gear))+geom_boxplot() +
#  stat_summary(fun = "mean", geom = "point", shape = 8,
#               size = 2, color = "black")  +
#  coord_flip() + xlab("Fish per skate or pot")

ggplot(data=pot_skate_cpue, mapping=aes(x=Pot.Type.1, y=mean_cpue, fill = Pot.Type.1))+
  geom_boxplot(outlier.shape=NA) +
  facet_wrap(~area) +
  geom_jitter(width=0.1, height=0, col="black", size=2, alpha=0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  xlab("Fish per skate or pot")           
ggsave(paste0("2024/figures/cpue_byset_2pot_vs_ll.png"), dpi = 300, height = 5, width = 7, units = "in") 

ggplot(data=cpue_comp, mapping=aes(x=Gear, y=set.cpue, fill = Gear))+
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  xlab("Fish per set") +
  facet_wrap(~Substrate)

ggplot(data=cpue_comp, mapping=aes(x=Gear, y=set.cpue, fill = Gear))+geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  xlab("Fish per set") +
  facet_wrap(~G.Stat.Area)

ggplot(data=cpue_comp, mapping=aes(x=sub.cpue, y=Pot.Type.1, fill = Pot.Type.1))+geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  coord_flip() + xlab("Fish per skate/pot") +
  facet_wrap(~G.Stat.Area)

#ggplot(cpue_comp, aes(soak.time, sub.cpue, col=Pot.Type.1, fill = Pot.Type.1)) + geom_point(shape = 2) + 
#  geom_smooth(size = 2, se = TRUE, method = "glm")  #"lm", "glm", "gam", "loess"

#ggplot(cpue_comp, aes(Depth, sub.cpue, col=Pot.Type.1, fill = Pot.Type.1)) + geom_point(shape = 2) + 
#  geom_smooth(size = 2, se = TRUE, method = "loess")

ggplot(cpue_comp, aes(soak.time, set.cpue, col=Gear, fill = Gear)) + geom_point(aes(shape = area)) + 
  geom_smooth(size = 2, se = TRUE, method = "glm")  #"lm", "glm", "gam", "loess"

ggplot(cpue_comp, aes(Depth, set.cpue, col=Gear, fill = Gear)) + geom_point(aes(shape = area)) + 
  geom_smooth(size = 2, se = TRUE, method = "glm")

plot(data=cpue_comp, soak.time ~ Depth); abline(lm(data=cpue_comp, soak.time ~ Depth))

################################################################################
## SCRAP
mod<-bam(data = ns_cpue_comp, set.cpue ~ s(G.Stat.Area, bs='re') + s(Station.No, bs = 're') +
           s(soak.time, k=3, m=1) + s(Depth, k=3) + Pot.Type.1,
         gamma=1.4)
summary(mod) 
plot(mod)

#mod<-lmer(data = cpue_comp, sub.cpue ~ s(G.Stat.Area, bs='re') +
#           s(soak.time, k=3, m=1) + s(Depth, k=3) + Pot.Type.1)

lmer <-lmer(data = ss_cpue_comp, set.cpue ~ soak.time*Depth*Pot.Type.1*(1|G.Stat.Area))
help('isSingular')
summary(lmer)
anova(lmer)

{library(arm)
  library(plyr)
  library(MuMIn)   #r.squaredGLMM(object)
  
  library(glmulti)
  library(AICcmodavg)}


lmer.glmulti <- function (formula, data, random = "", ...) {
  lmer(paste(deparse(formula), random), data = data, REML=F, ...)
}

setMethod('getfit', 'merMod', function(object, ...) {
  summ<-summary(object)$coef
  summ1<-summ[,1:2]
  if (length(dimnames(summ)[[1]])==1) {
    summ1<-matrix(summ1, nr=1, dimnames=list(c("(Intercept)"),c("Estimate","Std.Error")))
  }
  cbind(summ1, df=rep(10000,length(fixef(object))))
})

runmodels<-glmulti(sub.cpue ~ soak.time*Depth*Pot.Type.1,#s(set_depth, k=4), 
                   data=ss_cpue_comp,
                   method="h",imm=0.5, sexrate=0.1, crit=aicc , deltaM=0.01, 
                   conseq=20, marginality=T, maxsize=5, confsetsize=17, plotty=T,
                   level=2, fitfunc=lmer.glmulti, random="+(1|G.Stat.Area)+(1|Station.No)")
coef.glmulti(runmodels)
plot(runmodels,type="s")

summary(runmodels)
summary(runmodels)$bestmodel

runmodels@objects


simp<-glmulti(sub.cpue ~ soak.time*Depth*Pot.Type.1,#s(set_depth, k=4), 
              data=ss_cpue_comp,
              method="h",imm=0.5, sexrate=0.1, crit=aicc , deltaM=0.01, 
              conseq=20, marginality=T, maxsize=5, confsetsize=17, plotty=T,
              level=1, fitfunc=lmer.glmulti, random="+(1|G.Stat.Area)+(1|Station.No)")
coef.glmulti(simp)
plot(simp,type="s")

summary(simp)
summary(simp)$bestmodel

simp@objects







