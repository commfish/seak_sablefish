##################################################################################
## January 2022
## More Scrap Script for rooting through data files to get a sense of what's there
## 
##################################################################################
#with(DAT, table(Stage,Sample.Label))
#Useful little exploratory functions
histos<-function(dat,var){    #dat<-fish.bio     var<-"weight"
  ys<-sort(unique(dat$year))
  coln<-3 #round(sqrt(length(ys)))
  rown<-3 #ceiling(length(ys)/coln)
  ref<-which(colnames(dat)==var)
  par(mfrow=c(rown,coln))
  for (y in ys){   #y<-ys[1]
    daty<-dat[dat$year == y,]
    if (is.na(unique(daty[,ref]))) {} else {
      hist(daty[,ref], main=y)
    }
  }
}
dens<-function(dat,var){    #dat<-fish.bio     var<-"age"
  ys<-sort(unique(dat$year))
  ref<-which(colnames(dat)==var)
  par(mfrow=c(3,3))
  for (y in ys){   #y<-ys[20]
    daty<-dat[dat$year == y,]
    if (is.na(unique(daty[,ref]))) {} else {
      plot(density(daty[,ref], na.rm=TRUE), main=y)
    }
  }
}
catplot<-function(dat,var){  #dat<-fish.bio     var<-"Sex"
  ys<-sort(unique(dat$year))
  ref<-which(colnames(dat)==var)
  par(mfrow=c(3,3))
  for (y in ys){   #y<-ys[20]
    daty<-dat[dat$year == y,]
    if (is.na(unique(daty[,ref]))) {} else {
      count<-table(daty$year,daty[,ref])
      barplot(count, main=y)
    }
  }
}
##############################################################
#3) Fishery Bio
fish.bio<-read.csv("data/fishery/fishery_bio_2000_2021.csv")
str(fish.bio)
view(fish.bio)
unique(fish.bio$year)
unique(fish.bio$Sample_type)
unique(fish.bio$trip_no)
unique(fish.bio$Adfg)  #not sure what this is...
unique(fish.bio$Stat)  #not sure
unique(fish.bio$Mgmt_area)
unique(fish.bio$Spp_cde)
unique(fish.bio$Sex)
unique(fish.bio$Maturity)

with(fish.bio, table(year,length))
with(fish.bio, table(year,Sample_type))
with(fish.bio, table(year,Stat))

histos(fish.bio,"length")
dens(fish.bio,"length")
histos(fish.bio,"weight")
histos(fish.bio,"age")
dens(fish.bio,"age")

catplot(fish.bio,"Sex")
catplot(fish.bio,"Maturity")

#================================================================================
#
tagaccount<-read.csv("data/fishery/nsei_daily_tag_accounting_2004_2020.csv")
str(tagaccount)
view(tagaccount)
tagaccount$MRratio<-tagaccount$marked/tagaccount$unmarked
tagaccount$mark.prop<-tagaccount$marked/tagaccount$total_obs
ys<-sort(unique(tagaccount$year))

tagsmples<-tagaccount[!is.na(tagaccount$unmarked),]

dens(tagsmples,"MRratio")
histos(tagsmples,"MRratio")
#I wonder about assumption testing for MR part ofproject??
#spatial and temporal variability does not appear to have been explored
#mixing has not been evaluated
# potential biases in estimates... ??? could be a time-suck project to suss out

#plot mark.prop against julian date by year
par(mfrow=c(3,3))
for (y in ys){   #y<-ys[10]
  daty<-tagsmples[tagsmples$year == y,]
  daty<-daty[!is.na(daty$mark.prop),]
  #daty$mark.prop
  if (length(daty$mark.prop) == 0){}else {
    plot(daty$mark.prop ~ daty$julian_day, main=y)
    abline(lm(daty$mark.prop ~ daty$julian_day), 
           xlim=c(min(daty$julian_day),max(daty$julian_day)),
           col="blue")
  }
}

#plot by "area" or boat? 
par(mfrow=c(3,3))
for (y in ys){   #y<-ys[10]
  daty<-tagsmples[tagsmples$year == y,]
  daty<-daty[!is.na(daty$mark.prop),]
  if (length(daty$mark.prop) == 0){}else {
    boxplot(daty$mark.prop ~ daty$trip_no, main=y)
  }
}

#======================================================================
# 1) Harvest Data
harvest<-read.csv("data/fishery/nseiharvest_ifdb_1969_2021.csv")
str(harvest)
ys<-unique(harvest$year)
ann<-data.frame(); j<-1
for (i in ys){
  dy<-harvest[harvest$year == i,]
  ann[j,"year"]<-i
  ann[j,"w.pnds"]<-sum(dy$whole_pounds)
  j<-j+1
}
plot(ann$w.pnds ~ ann$year)

dens(harvest,"whole_pounds")

for (i in ys){
  dy<-harvest[harvest$year == i,]
  dy<-dy[order(dy$julian_day),]
  plot(dy$whole_pounds~dy$julian_day, type="l")
}
#===============================================================
#2) Fishery cpue ----
f21<-read_csv(paste0("data/fishery/raw_data/fishery_cpue_",
                #                 max(fsh_eff$YEAR), ".csv"), 
                YEAR, ".csv"), 
         guess_max = 50000)

f20<-read_csv(paste0("data/fishery/raw_data/fishery_cpue_",
                #                 max(fsh_eff$YEAR), ".csv"), 
                YEAR-1, ".csv"), 
         guess_max = 50000)
str(f21); str(f20)
ff<-rbind(f21,f20)

str(ff)

fc<-read_csv(paste0("data/fishery/fishery_cpue_1997_", YEAR,".csv"), 
         guess_max = 50000)
str(fc)


read_csv(paste0("data/fishery/fishery_cpue_1997_", YEAR,".csv"), 
         guess_max = 50000) %>% 
  filter(!is.na(date) & !is.na(hook_space) & !is.na(sable_lbs_set) &
           !is.na(start_lon) & !is.na(start_lon) & !is.na(soak) & !is.na(depth) &
           !is.na(Hook_size) & Hook_size != "MIX" &
           soak > 0 & !is.na(soak) & # soak time in hrs
           julian_day > 226 & # if there were special projects before the fishery opened
           no_hooks < 15000 & # 15000 in Kray's scripts - 14370 is the 75th percentile
           # limit analysis to Chatham Strait and Frederick Sounds where the
           # majority of fishing occurs
           Stat %in% c("345603", "345631", "345702",
                       "335701", "345701", "345731", "345803")) %>% 
  mutate(Year = factor(year), 
         Gear = factor(Gear),
         Adfg = factor(Adfg),
         Stat = fct_relevel(factor(Stat),
                            "345702", "335701", # Frederick Sound
                            # Chatham south to north
                            "345603", "345631", "345701", "345731", "345803"),
         # 01=Conventional, 02=Snap On, 05=Mixed, 06=Autobaiter -> 01, 02, 05
         # show no strong differences. main difference with autobaiters, which
         # have lwr cpue than conventional gear
         Gear = derivedFactor("AB" = Gear == "06",
                              "CS" = Gear %in% c("01","02","05")),
         Hook_size = factor(Hook_size),
         # standardize hook spacing (Sigler & Lunsford 2001, CJFAS), 1 m = 39.37 in
         std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (hook_space / 39.37))), 
         std_cpue = sable_lbs_set / std_hooks,
         # dummy varbs, for prediction with random effects
         dum = 1, 
         dumstat = 1) %>% 
  #"sets" (aka effort_no) is the set identifier. Currently Martina's scripts
  #filter out sets that Kamala identifies as halibut target sets. Create a new
  #column that is the total number of sablefish target sets in a trip (trip_no's
  #only unique within a year)
  group_by(year, trip_no) %>% 
  mutate(no_sets = n_distinct(sets)) %>% 
  group_by(year) %>% 
  mutate(
    #The number of vessels participating in the fishery has descreased by 50% from
    #1997-2015. create new column is the total number of active vessels
    #participating in a given year
    total_vessels = n_distinct(Adfg),
    # Total unique trips per year
    total_trips = n_distinct(trip_no)) %>% 
  ungroup() -> fsh_cpue

str(fsh_cpue)
#---------------------------------------------------------------
# 9) tag recovery data
tagrec<-read.csv("data/fishery/tag_recoveries_2003_2021.csv")
str(tagrec)
plot(tagrec$tag_batch_no~tagrec$year)
histos(tagrec,"length")

plotcolors<-colorRampPalette(c("green","blue"),alpha = FALSE)(10)
plotcolors<-colorRampPalette(c(rgb(0,0,1,1), rgb(0,0,1,0)), 
                             bias=100,alpha = TRUE)(20)
ys<-unique(tagrec$year)
for (i in ys){   #i<-ys[2]
  dy<-tagrec[tagrec$year == i,]
  jds<-unique(dy$landing_julian_day)
  jds<-sort(jds[!is.na(jds)])
  yeard<-data.frame()
  k<-1
  for (j in jds){   #j<-jds[1]  unique(dy$landing_julian_day)
    dj<-dy[dy$landing_julian_day == j,]
    dj<-dj[!is.na(dj$landing_julian_day),]
    yeard[k,"day"]<-j
    yeard[k,"tagrecs"]<-nrow(dj)
    k<-k+1
  }
  if (i == ys[1]){
    m<-1
    plot(yeard$tagrecs ~ yeard$day, type="l", pch=18, cex=0.3, col=plotcolors[m],
         xlim=c(min(tagrec$landing_julian_day, na.rm=TRUE),
                max(tagrec$landing_julian_day, na.rm=TRUE)),
         ylim=c(0,200))
  } else {
    lines(yeard$tagrecs ~ yeard$day, col=plotcolors[m+1])
    m<-m+1
  }
}

meas.tags<-tagrec[!is.na(tagrec$length),]
dens(meas.tags,"length")

#================================================================================
#6) 
llsbio<-read.csv("data/survey/llsrv_bio_1988_2021.csv")
str(llsbio)
dens(llsbio,"length")
dens(llsbio,"weight")
dens(llsbio,"age")
catplot(llsbio,"Sex")
catplot(llsbio,"Maturity")

#===============================================================================
#5)
llcond<-read.csv("data/survey/llsrv_by_condition_1988_2021.csv")
str(llcond)
histos(llcond,"no_hooks")
histos(llcond,"hooks_bare")
histos(llcond,"hooks_bait")
histos(llcond,"hook_invalid")
unique(llcond$discard_status)

#===============================================================================
# 4)
llcpue<-read.csv("data/survey/llsrv_cpue_v2_1985_2021.csv")
str(llcpue)
#no_hooks SHOULD = bare+bait+invalid+fish caught
#CPUE should be calc from no_hooks-invalid after scrapping all skates with more than 12 invalid... 
#also get rid of sleeper shark sets?
histos(llcpue,"skate")
histos(llcpue,"soak")   #!!! Negative soak times 1988-1996!!! 
histos(llcpue,"depth")
histos(llcpue,"skate_condition_cde")
unique(llcpue$skate_condition_cde)

unique(llcpue$skate_comments)
ys<-sort(unique(llcpue$year))
com.cnt<-data.frame(); i<-1
for (y in ys){
  dat<-llcpue[llcpue$year == y,]
  com.cnt[i,"year"]<-y
  com.cnt[i,"no.comments"]<-nrow(dat[!is.na(dat$skate_comments),])
  i<-i+1
}
com.cnt   #need to clean up comments, at least for 2021!!! 

plot(llcpue$skate_condition_cde ~ llcpue$year)
with(llcpue, table(year,skate_condition_cde))

code5<-llcpue[llcpue$skate_condition_cde==5,]; code5$skate_comments   #not sure
  #whales present but not predation
  #verified comments
code6<-llcpue[llcpue$skate_condition_cde==6,]; code6$skate_comments   #sperm whales?
code2<-llcpue[llcpue$skate_condition_cde==2 & llcpue$year == 2021,]; code2$skate_comments  #snarls
code1<-llcpue[llcpue$skate_condition_cde==1 & llcpue$year == 2021,]
code1$skate_comments  #verified?  means good?

with(llcpue, table(year,bait_cde))
histos(llcpue,"bait_cde")

unique(llcpue$trip_comments) #these look pretty inoccuous; mostly list of crew names

unique(llcpue$set_comments)
set.coms<-llcpue[!is.na(llcpue$set_comments),]
setcom.cnt<-data.frame(); i<-1
for (y in ys){
  dat<-llcpue[llcpue$year == y,]
  setcom.cnt[i,"year"]<-y
  setcom.cnt[i,"no.comments"]<-nrow(dat[!is.na(dat$set_comments),])
  i<-i+1
}
setcom.cnt

histos(llcpue, "no_hooks")  #total number of hooks retrieved and effectively fished? or no. deployed?
histos(llcpue, "bare")
histos(llcpue, "bait")
head(llcpue,20)

histos(llcpue,"invalid")
invalid.max<-data.frame(); i<-1
for (y in ys){   #y<-ys[13]
  dat<-llcpue[llcpue$year == y,]
  invalid.max[i,"year"]<-y
  invalid.max[i,"no.comments"]<-max(dat$invalid,na.rm=TRUE)
  i<-i+1
}
invalid.max
#lots of data retained from sets with more than 12 invalid hooks... 
#get rid of those for CPUE calcs
# but keep for MR as # fish examined for marks

#lets see if skate_condition_code 5 has different CPUE from condition 1... 
sk5v1<-data.frame(); i<-1
for (y in ys){   #y<-ys[13]
  dat1<-llcpue[llcpue$year == y & llcpue$skate_condition_cde==1,]
  dat5<-llcpue[llcpue$year == y & llcpue$skate_condition_cde==5,]
  sk5v1[i,"year"]<-y
  sk5v1[i,"cpue.1"]<-sum(dat1$sablefish, na.rm=TRUE)/(sum(dat1$no_hooks, na.rm=TRUE)-sum(dat1$invalid, na.rm=TRUE))
  sk5v1[i,"cpue.5"]<-sum(dat5$sablefish, na.rm=TRUE)/(sum(dat5$no_hooks, na.rm=TRUE)-sum(dat5$invalid, na.rm=TRUE))
  i<-i+1
}
sk5v1  #looks good... only two years with code 5

#===================================================================================
# 7)
pot<-read.csv("data/survey/potsrv_bio_1981_2020.csv")
str(pot)
histos(pot,"release_condition_cde")
unique(pot$release_condition_cde)
head(pot,20)

#==================================================================================
# 8)
tagrel<-read.csv("data/survey/tag_releases_2003_2020.csv")
str(tagrel)
with(tagrel, table(year,release_condition_cde))
unique(tagrel$release_condition_cde)

tc1<-tagrel[tagrel$release_condition_cde == 1,]; unique(tc1$discard_status)  #some already tagged?  most just tgd and rlsd
tc0<-tagrel[tagrel$release_condition_cde == 0,]; unique(tc0$discard_status)   #good!
tc2<-tagrel[tagrel$release_condition_cde == 2,]; unique(tc2$discard_status)   #hmm... stil tagged and released
tc3<-tagrel[tagrel$release_condition_cde == 3,]; unique(tc3$discard_status) 
tc4<-tagrel[tagrel$release_condition_cde == 4,]; unique(tc4$discard_status) 
tc5<-tagrel[tagrel$release_condition_cde == 5,]; unique(tc5$discard_status) 
tc6<-tagrel[tagrel$release_condition_cde == 6,]; unique(tc6$discard_status) 
tc7<-tagrel[tagrel$release_condition_cde == 7,]; unique(tc7$discard_status) 
tc8<-tagrel[tagrel$release_condition_cde == 8,]; unique(tc8$discard_status) 
 #codes have nothing with discard_status... 
unique(tagrel$discard_status)
