#We've got two groups of fish caught in two gears.  We fished 100 pots for each 
# group.
#in the first group we caught 500 fish that averaged 50 cm in length: 
source("r/helper.r")
source("r/functions.r")
library(boot)

read.csv("2023/data/survey/Slinky_and_conical_pot_data_2022.csv") -> pot_dat

str(pot_dat)
unique(pot_dat$Time.Second.Anchor.Overboard)
unique(pot_dat$Time.First.Anchor.Onboard)
unique(pot_dat$Time.Second.Anchor.Onboard)

parse_time3 <- function(x) {   #https://stackoverflow.com/questions/69738674/how-to-convert-time-hhmmss-to-decimal-form-in-r
  res <- do.call(rbind, strsplit(x, ":", TRUE))
  mode(res) <- "numeric"
  c(res %*% (1/c(1, 60)))
}

pot_dat <-pot_dat %>% 
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

pot_dat[is.na(pot_dat$Date),]

set<-sort(rep(seq(1,24,1),20))
pot_no<-rep(seq(1,20,1),24)
sets_pots<-as.data.frame(cbind(set,pot_no)) %>% mutate(set_x_pot = paste0(set,"_",pot_no,sep=""))

fished<-unique(pot_dat$set_x_pot)
allsets<-unique(sets_pots$set_x_pot)

blanks<-setdiff(allsets,fished)

blanks<-data.frame(cbind(NA,NA,NA,as.numeric(sub("_.*","",blanks)),
      as.numeric(substring(blanks,regexpr("_",blanks)+1)),blanks,
      NA,NA,NA,NA))
colnames(blanks)<-colnames(pot_dat)
blanks<-blanks %>% mutate(set = as.numeric(set),pot_no=as.numeric(pot_no))

pot_dat<-rbind(pot_dat,blanks) %>% arrange(set,pot_no) 
str(pot_dat)
pot_dat$depth[is.na(pot_dat$depth)]<-pot_dat$depth[which(is.na(pot_dat$depth))-1]
pot_dat$Date[is.na(pot_dat$Date)]<-pot_dat$Date[which(is.na(pot_dat$Date))-1]
pot_dat$soak.time[is.na(pot_dat$soak.time)]<-pot_dat$soak.time[which(is.na(pot_dat$soak.time))-1]
pot_dat$stat_area[is.na(pot_dat$stat_area)]<-pot_dat$stat_area[which(is.na(pot_dat$stat_area))-1]
view(pot_dat)
pot_dat$pot_type[is.na(pot_dat$pot_type)]<-pot_dat$pot_type[which(is.na(pot_dat$pot_type))-1]
pot_dat<-pot_dat %>% mutate(pot_type = ifelse(is.na(pot_type_code) & pot_type %in% c("Slinky"),"Cone",
                                              ifelse(is.na(pot_type_code) & pot_type %in% c("Cone"),"Slinky",
                                                     ifelse(!is.na(pot_type_code),as.character(pot_type),NA))),
                            length = as.numeric(length),
                            depth = as.numeric(depth))

view(pot_dat)
str(pot_dat)
which(is.na(pot_dat$pot_type_code))

pot_dat[c(8:10,3101:3103,3133:3135,5724:5726,6107:6109),]

#get rid of sets that were all slinky or all conical...
check <- pot_dat %>% group_by(set, pot_type) %>%
  summarise(n = n())
view(check)



pot_comps<-pot_dat[pot_dat$set < 21 & pot_dat$set != 9,]
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
            sd = sqrt(var))

#At this level there is no significant difference in CPUE

ggplot()

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

#At this point it should be obvious that there are more little fish in group 1

#So, let's identify the length that we should stratify the data.  We do this by 
#finding the length where the two distributions are the furthest apart.  
#Here is a cute function to do that... 

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
#is at the number you see (random numbers so might be different... 
# 46 last time I ran this)
abline(v=D.func(slinkys$length,cones$length), col="red")

#so, even though the overall CPUE is similar between gear, this shows that there
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

SSEO.w<-boot(Port.SSEO.last3$Weight.Kilograms, statistic=meanfun, R=1000)
SSEO.w
bo.ci<-boot.ci(SSEO.w, conf=0.95, type="bca")

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

ggsave(file.path("2023/figures/Slinky_conical_comp_2022.png"), dpi = 500, height = 6, width = 7, units = "in")
  
write.csv(CPUE_sum,
          "2023/output/Slinky_conical_comp_2022.csv")




#-------------------------------------------------------------------------------
# pre-data simulated analysis...
#-------------------------------------------------------------------------------

set.seed(1457)
fish1<-round(rnorm(500,50,10),0); hist(fish1)
CPUE1<-500/100
#In the second group we caught 450 fish that averaged 54 cm in length: 
fish2<-round(rnorm(450,54,7),0); hist(fish2)
CPUE2<-450/100

#This is a simple example.  You should calculate the CPUE for each pot and then
#calculate the average CPUE across all pots/sets and get the variance and standard deviation
# which will allow you to see if they are significantly different.  You can repeat that at
# the set level as well.  

#In this simple example we know we caught more fish in group 1, but we want to know
# did we catch more fish of all length categories or did we just catch more small 
# fish and the same number of big fish?  

plot(density(fish1), xlim=c(10,90)); lines(density(fish2), col="blue")

#are the length distributions different?

ks.test(fish1,fish2)
#results show that the length distributions are significantly different! (P value < 0.05)

#lets plot the cumulative distribution functions of the data 
cdf1<-ecdf(fish1)
cdf2<-ecdf(fish2) 
plot(cdf1, verticals=TRUE, do.points=FALSE, main="cdf", xlab="length")
plot(cdf2, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")

#At this point it should be obvious that there are more little fish in group 1

#So, let's identify the length that we should stratify the data.  We do this by 
#finding the length where the two distributions are the furthest apart.  
#Here is a cute function to do that... 

D.func<-function(x,y){  #x<-C; y<-M
  n.x <- length(x)
  n.y <- length(y)
  n <- n.x * n.y/(n.x + n.y)
  w <- c(x, y)
  z <- cumsum(ifelse(order(w) <= n.x, 1/n.x, -1/n.y))
  max.at <- sort(w)[which(abs(z) == max(abs(z)))]
  return(max.at)
  
}

D.func(fish1,fish2)  #this tells you that the greatest distance between the distributions 
                     #is at the number you see (random numbers so might be different... 
                      # 46 last time I ran this)
abline(v=D.func(fish1,fish2), col="red")

#So, 50 is the length we should stratify around.  Divide both groups into fish
#greater and less than 51cm.

bp1<-D.func(fish1,fish2)

f1_small<-fish1[fish1 <=bp1]
f1_big<-fish1[fish1 >bp1]

f2_small<-fish2[fish2 <=bp1]
f2_big<-fish2[fish2 >bp1]

#Lets compare the length distribution of big fish... 
cdf1_big<-ecdf(f1_big)
cdf2_big<-ecdf(f2_big) 
plot(cdf1_big, verticals=TRUE, do.points=FALSE, main="cdf")
plot(cdf2_big, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")

ks.test(f1_big,f2_big)
# No difference in the length distribution of big fish. 
# Go ahead and compare the CPUE of big fish...
length(f1_big)/100 #<-CPUE of big fish in first gear
length(f2_big)/100 #<-CPUE of big fish in second gear
#For the full data set, you'll calculate by set and average across sets so you get
# variance and SD and can see if they are significantly different... 


## Lets look at the smaller fish first.  Are those distributions different?
cdf1_small<-ecdf(f1_small)
cdf2_small<-ecdf(f2_small) 
plot(cdf1_small, verticals=TRUE, do.points=FALSE, main="cdf")
plot(cdf2_small, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")

ks.test(f1_small,f2_small)
# these distributions are different. 
# At this point you could compare the CPUE of fish <= "break point" in the two groups.
# But lets find the next break point...

D.func(f1_small,f2_small)
bp2<-D.func(f1_small,f2_small)
abline(v=D.func(f1_small,f2_small), col="red")
# stratify at break point 2 (bp2).  In this example we are probably getting into sample size issues...
# But we can see that those smaller fish are disappearing from sample 2.  

f1_ltbp2<-f1_small[f1_small <=bp2]
f1_bp1_bp2<-f1_small[f1_small >bp2]

f2_ltbp2<-f2_small[f2_small <=bp2]
f2_bp1_bp2<-f2_small[f2_small >bp2]

#Compare the length distribution of fish bp-50cm 
cdf1_bp1_bp2<-ecdf(f1_bp1_bp2)
cdf2_bp1_bp2<-ecdf(f2_bp1_bp2) 
plot(cdf1_bp1_bp2, verticals=TRUE, do.points=FALSE, main="cdf")
plot(cdf2_bp1_bp2, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")

ks.test(fish1_bp_50,fish2_bp_50)
#same length distributions.  Compare the CPUE of fish in this length category.

#what about fish less than bp2... 
cdf1_ltbp<-ecdf(f1_ltbp2)
cdf2_ltbp<-ecdf(f2_ltbp2) 
plot(cdf1_ltbp, verticals=TRUE, do.points=FALSE, main="cdf")
plot(cdf2_ltbp, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")

ks.test(f1_ltbp2,f2_ltbp2)
abline(v=D.func(f1_ltbp2,f2_ltbp2), col="red")
#Still significant difference, so you could repeat or call it good at this point.
#You can see there are no fish in the second group less than a certain size (changes a little bit
# every time you run the simulations)
# make a break point there and compare the CPUE of fish between the minimum length 
# of group 2 and bp2 above and note that gear 2 did not catch any fish less than 
# the smallest fish 




##modify









