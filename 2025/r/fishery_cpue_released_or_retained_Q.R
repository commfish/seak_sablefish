################################################################################
## Trouble shooting code for dealing with Released and retained fish...
## This picks up after Step 10 in fishery_cpue_fr_OceanAK_ftx_lb_dat.R
## Lesson learned: you can't use the Ticket_subref # to calculate CPUE
## although it should be OK for the join... I think... 
## development example 1: 2021, ADFG: 45454, landing and sell date 10-21-2021
################################################################################

letgo<-sable_ll_lb %>% filter(Disposition == "Released")
nrow(letgo)

#random sample with release code... 
letgo_samp<-sample(letgo,1)
as.data.frame(letgo_samp)

#or development example 1:
letgo_samp <- letgo %>% filter (Year == 2021, ADFG.Number == 45454,
                                Sell.Date == "2021-10-21 00:00:00")

#colnames(sable_ftx)
letgo_tx<-sable_ftx %>% filter(Year == letgo_samp$Year,
                               ADFG == letgo_samp$ADFG.Number,
                               Date.of.Landing == letgo_samp$Sell.Date)
try2<-sable_ftx %>% filter(Ticket_subref == letgo_samp$Ticket_subref,
                           Year == letgo_samp$Year)

as.data.frame(sable_ll_lb %>% filter(Year == letgo_samp$Year,
                       ADFG.Number == letgo_samp$ADFG.Number,
                       Ticket_subref == letgo_tx$Ticket_subref))

#as.data.frame(ll_lb %>% filter(Year == Y, ADFG.Number == AN & Species == "Sablefish"))
#unique(as.data.frame(ll_lb %>% filter(Year == Y, ADFG.Number == AN & Species == "Sablefish"))$Sell.Date)
#colnames(ftx)
#unique(ftx$Fishery.Name)
ftx_samp<-ftx %>% filter(Year %in% letgo_samp$Year & 
                           ADFG %in% letgo_samp$ADFG.Number & 
                           Harvest.Code != 43 & 
                           Fishery.Name %in% unique(ftx$Fishery.Name)[1] &
                           Date.of.Landing == letgo_samp$Sell.Date
                           ) %>% 
  mutate(Ticket_subref = str_remove(str_sub(Sequential.Number,nchar(Sequential.Number)-2,nchar(Sequential.Number)), "^0+"),
         Trip.Number.FTX = Trip.Number) %>%
  select(Year, Date.of.Landing, Stat.Area, Sequential.Number, Ticket_subref, CFEC, 
         Year.Office.BN,Whole.Weight..sum.,
         Ticket.Count, Harvest.Code, Harvest.Name,
         Port, ADFG, Fishery.Name, Expanded.Fishery.Code,
         CFEC.1, Office.Code, Batch.Number, Trip.Number.FTX)

letgo_samp$Ticket_subref
unique(ftx_samp$Ticket_subref)



#-------------------------------------------------------------------------------

sf_try<-full_join(letgo_samp, ftx_samp, by = c("Year", "ADFG.Number" = "ADFG",
                                                   "Sell.Date" = "Date.of.Landing",
                                                   "Groundfish.Stat.Area" = "Stat.Area",
                                                   #"Species" = "Species.Name",
                                                   "Ticket_subref"),multiple="all") %>% filter(!is.na(Fishery.Name)) #%>% #this gets rid of logbook entries with no matching fish tickets
as.data.frame(letgo_samp)
as.data.frame(sf_try[order(sf_try$Ticket_LB),]) 

missing_lb<-sable_ll_lb %>% filter(Year == unique(sf_try$Year),
                                   ADFG.Number == unique(sf_try$ADFG.Number),
                                   Ticket_subref == sf_try$Ticket_subref[is.na(sf_try$Disposition)])
as.data.frame(missing_lb)
missing_lb$Ticket_subref
letgo_samp$Ticket_subref
#OK, so in the missing logbook we see that "releases" are in numbers and "retained" are in lbs
#lets add the missing logbook to the "released" logbook entry and then rejoin with
# fish tickets and see what we get
colnames(missing_lb)
colnames(letgo_samp)
arg<-rbind(as.data.frame(missing_lb),as.data.frame(letgo_samp %>% select(-orig.id)))
unique(arg$Ticket_subref)

ftx_samp2<-ftx %>% filter(Year %in% arg$Year & 
                           ADFG %in% arg$ADFG.Number & 
                           Harvest.Code != 43 & 
                           Fishery.Name %in% unique(ftx$Fishery.Name)[1] &
                           Date.of.Landing %in% arg$Sell.Date
) %>% 
  mutate(Ticket_subref = str_remove(str_sub(Sequential.Number,nchar(Sequential.Number)-2,nchar(Sequential.Number)), "^0+"),
         Trip.Number.FTX = Trip.Number) %>%
  select(Year, Date.of.Landing, Stat.Area, Sequential.Number, Ticket_subref, CFEC, 
         Year.Office.BN,Whole.Weight..sum.,
         Ticket.Count, Harvest.Code, Harvest.Name,
         Port, ADFG, Fishery.Name, Expanded.Fishery.Code,
         CFEC.1, Office.Code, Batch.Number, Trip.Number.FTX)

tryagain<-full_join(arg, ftx_samp2, by = c("Year", "ADFG.Number" = "ADFG",
                                               "Sell.Date" = "Date.of.Landing",
                                               "Groundfish.Stat.Area" = "Stat.Area",
                                               #"Species" = "Species.Name",
                                               "Ticket_subref"),multiple="all") %>% filter(!is.na(Fishery.Name)) #%>% #this gets rid of logbook entries with no matching fish tickets
as.data.frame(arg)
as.data.frame(tryagain[order(tryagain$Ticket_LB),]) 
unique(arg$CFEC.Permit.Number); unique(ftx_samp2$CFEC)

tryagain %>% filter(Disposition == "Released")
tryagain %>% filter(Disposition == "Retained")

colnames(tryagain)
try2<-tryagain %>% select(Year,Disposition,Groundfish.Stat.Area,Effort.Primary.Target.Species,
                    Effort.Number,ADFG.Number,Sell.Date,Ticket_subref,set.count,
                    Ticket_LB,Species_LB,Pounds_LB,Numbers_LB,Sequential.Number,
                    Fishery.Name,Whole.Weight..sum.)
try2[order(try2$Effort.Number,try2$Disposition),]
try2[try2$Effort.Number==2,]

#lets see what the CPUE looks like when we take "tryagain" and run
tryagain %>% group_by(Ticket_LB, Groundfish.Stat.Area, Effort.Number) %>% 
  mutate(partial_soak_time = soak_time_hrs/n(),
         partial_km_fished = set_length_km/n(),
         partial_hook_count1 = as.numeric(num_of_hooks1_calc)/n(),
         partial_hook_count2 = as.numeric(num_of_hooks2_calc)/n()) %>% ungroup %>%
  group_by(Ticket_LB, Groundfish.Stat.Area) %>%
  mutate(set.count = set.count,
         catch = Whole.Weight..sum.,
         pre.lbs_per_set = catch/set.count,
         total_soak_time = sum(partial_soak_time),
         total_km_fished = sum(partial_km_fished),
         total_hook_count1 = sum(partial_hook_count1),
         total_hook_count2 = sum(partial_hook_count2),
         lbs_per_set = sum(unique(catch))/set.count,
         lbs_per_set_per_km = sum(unique(catch))/set.count/total_km_fished,
         lbs_per_set_per_hour = sum(unique(catch))/set.count/total_soak_time,
         lbs_per_set_per_km_per_hour = sum(unique(catch))/set.count/total_km_fished/total_soak_time,
         lbs_per_hook1 = sum(unique(catch))/total_hook_count1,
         lbs_per_hook_per_km1 = sum(unique(catch))/total_hook_count1/total_km_fished,
         lbs_per_hook_per_hour1 = sum(unique(catch))/total_hook_count1/total_soak_time,
         lbs_per_hook_per_km_per_hour1 = sum(unique(catch))/total_hook_count1/total_km_fished/total_soak_time,
         lbs_per_hook2 = sum(unique(catch))/total_hook_count2,
         lbs_per_hook_per_km2 = sum(unique(catch))/total_hook_count2/total_km_fished,
         lbs_per_hook_per_hour2 = sum(unique(catch))/total_hook_count2/total_soak_time,
         lbs_per_hook_per_km_per_hour2 = sum(unique(catch))/total_hook_count2/total_km_fished/total_soak_time
  ) %>% 
  select(Year,Disposition,Groundfish.Stat.Area,Effort.Primary.Target.Species,
         Effort.Number,ADFG.Number,Sell.Date,Ticket_subref,set.count,
         Ticket_LB,Species_LB,Pounds_LB,Numbers_LB,Sequential.Number,
         Fishery.Name,catch,pre.lbs_per_set,lbs_per_set,lbs_per_hook1) ->test  #don't change! This attaches to joined
#Note regarding two gear configurations.  When calculating final CPUE in analysis
# you will need to be mindful of gear_target_1 and _2.  When there are two sets of
# gear targeting different species (halibut and sablefish) the CPUE will NOT be
# useful for analysis because there is no way to separate out the catch by target 
# if they are both on the same ticket. :(

as.data.frame(unique(test))    
as.data.frame(test[order(test$Effort.Number,test$Disposition),])

as.data.frame(test[order(test$Effort.Number,test$Disposition),])%>%filter(Effort.Number == 2)

tryagain %>% group_by(Groundfish.Stat.Area, Effort.Number) %>% 
  mutate(partial_soak_time = soak_time_hrs/n(),
         partial_km_fished = set_length_km/n(),
         partial_hook_count1 = as.numeric(num_of_hooks1_calc)/n(),
         partial_hook_count2 = as.numeric(num_of_hooks2_calc)/n()) %>% ungroup %>%
  group_by(Groundfish.Stat.Area) %>%
  #summarize(catch = sum(Whole.Weight..sum.)) %>%
  mutate(set.count = set.count,
         tix.catch = Whole.Weight..sum.,
         catch = sum(unique(Whole.Weight..sum.)),
         pre.lbs_per_set = catch/set.count,
         total_soak_time = sum(partial_soak_time),
         total_km_fished = sum(partial_km_fished),
         total_hook_count1 = sum(partial_hook_count1),
         total_hook_count2 = sum(partial_hook_count2),
         lbs_per_set = sum(unique(catch))/set.count,
         lbs_per_set_per_km = sum(unique(catch))/set.count/total_km_fished,
         lbs_per_set_per_hour = sum(unique(catch))/set.count/total_soak_time,
         lbs_per_set_per_km_per_hour = sum(unique(catch))/set.count/total_km_fished/total_soak_time,
         lbs_per_hook1 = sum(unique(catch))/total_hook_count1,
         lbs_per_hook_per_km1 = sum(unique(catch))/total_hook_count1/total_km_fished,
         lbs_per_hook_per_hour1 = sum(unique(catch))/total_hook_count1/total_soak_time,
         lbs_per_hook_per_km_per_hour1 = sum(unique(catch))/total_hook_count1/total_km_fished/total_soak_time,
         lbs_per_hook2 = sum(unique(catch))/total_hook_count2,
         lbs_per_hook_per_km2 = sum(unique(catch))/total_hook_count2/total_km_fished,
         lbs_per_hook_per_hour2 = sum(unique(catch))/total_hook_count2/total_soak_time,
         lbs_per_hook_per_km_per_hour2 = sum(unique(catch))/total_hook_count2/total_km_fished/total_soak_time
  ) %>% 
  select(Year,Disposition,Groundfish.Stat.Area,Effort.Primary.Target.Species,
         Effort.Number,ADFG.Number,Sell.Date,Ticket_subref,set.count,
         Ticket_LB,Species_LB,Pounds_LB,Numbers_LB,Sequential.Number,
         Fishery.Name,tix.catch,catch,
         pre.lbs_per_set,lbs_per_set,lbs_per_hook1) ->test_noTickref

as.data.frame(test_noTickref[order(test_noTickref$Effort.Number,test_noTickref$Disposition),])

###############################################################################################
## Check after fix... 

released<-Sable_CPUE %>% filter(Disposition == "Released")

{
  year_check<-sample(length(unique(released$Year)),1)
  year_check
  
  adfg_list<-unique(released$ADFG.Number[which(released$Year == unique(released$Year)[year_check])])
  length(adfg_list)
  adfg_list
  adfg_check<-adfg_list[sample(length(adfg_list),1)]
  
  sd_list<-
    unique(released$Sell.Date[which(released$Year == unique(released$Year)[year_check] & 
                                      released$ADFG.Number == adfg_check)])
  sd_check<-sd_list[sample(length(sd_list),1)]
  
  gsa_list<-
    unique(released$Groundfish.Stat.Area[which(released$Year == unique(released$Year)[year_check] & 
                                                 released$ADFG.Number == adfg_check &
                                                 released$Sell.Date == sd_check)])
  length(gsa_list)
  gsa_check<-gsa_list[sample(length(gsa_list),1)]
}

check<-as.data.frame(Sable_CPUE %>% filter(Sell.Date == sd_check,
                                           Groundfish.Stat.Area == gsa_check,
                                           Year == unique(Sable_CPUE$Year)[year_check],
                                           ADFG.Number == adfg_check)); check
unique(check$Ticket_subref); unique(check$tix.ref.catch)
unique(check$Pounds_LB); unique(check$Numbers_LB)

check$Pounds_LB[check$Disposition == "Retained"]; check$Pounds_LB[check$Disposition == "Released"]
check$Numbers_LB[check$Disposition == "Retained"]; check$Numbers_LB[check$Disposition == "Released"]
#######################################################################################

nrow(test)
nrow(unique(test))

length(unique(test$Year))
year_check<-1
length(unique(test$ADFG.Number))
adfg_check<-1

tix_list<-
  unique(test$Ticket_LB[which(test$Year == unique(test$Year)[year_check] & 
                                test$ADFG.Number == unique(test$ADFG.Number)[adfg_check])])
length(tix_list)
tix_list
tix_check<-tix_list[1]

gsa_list<-
  unique(sf_try$Groundfish.Stat.Area[which(sf_try$Year == unique(sf_try$Year)[year_check] & 
                                             sf_try$ADFG.Number == unique(sf_try$ADFG.Number)[adfg_check] &
                                             sf_try$Ticket == tix_check)])
length(gsa_list)
gsa_check<-gsa_list[1]


#as.data.frame(sf_try %>% filter(Ticket == unique(sf_try$Ticket)[check],
#                                Groundfish.Stat.Area == unique(sf_try$Groundfish.Stat.Area)[check2]))
as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                  Groundfish.Stat.Area == gsa_check,
                                  Year == unique(sf_cpue2$Year)[year_check],
                                  ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))
#THIS IS THE SABLEFISH CPUE FOR THIS TICKET!! 

unique(as.data.frame(test %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(test$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_set)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_set_per_km)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_set_per_hour)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_set_per_km_per_hour)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_hook)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_hook_per_km)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_hook_per_hour)
unique(as.data.frame(sf_cpue2 %>% filter(Ticket == tix_check,
                                         Groundfish.Stat.Area == gsa_check,
                                         Year == unique(sf_cpue2$Year)[year_check],
                                         ADFG.Number == unique(sf_cpue2$ADFG.Number)[adfg_check]))$lbs_per_hook_per_km_per_hour)

################################################################################
missing_tix<-ftx %>% 
  mutate(Ticket_subref = str_remove(str_sub(Sequential.Number,nchar(Sequential.Number)-2,nchar(Sequential.Number)), "^0+"),
         Trip.Number.FTX = Trip.Number) %>%
  filter(Year %in% letgo_samp$Year & 
           ADFG %in% letgo_samp$ADFG.Number & 
           Ticket_subref == sf_try$Ticket_subref[is.na(sf_try$Disposition)]
  ) %>% 
  select(Year, Date.of.Landing, Stat.Area, Sequential.Number, Ticket_subref, CFEC, 
         Year.Office.BN,Whole.Weight..sum.,
         Ticket.Count, Harvest.Code, Harvest.Name,
         Port, ADFG, Fishery.Name, Expanded.Fishery.Code,
         CFEC.1, Office.Code, Batch.Number, Trip.Number.FTX)







check<-check %>% mutate(multi = ifelse(!is.na(num_of_hooks1),num_of_hooks1,
                                       ifelse(is.numeric(hooks_per_skate1),as.numeric(num_of_skates1)*as.numeric(hooks_per_skate1),
                                              mean(c(as.numeric(strsplit(hooks_per_skate1,",")[[1]][1]),
                                                     as.numeric(strsplit(hooks_per_skate1,",")[[1]][2])))*as.numeric(num_of_skates1))),
                        NS_test = ifelse(!is.na(num_of_skates1),num_of_skates1,
                                                     as.numeric(num_of_hooks1)/as.numeric(hooks_per_skate1)),     
                        NH_test = ifelse(!is.na(num_of_hooks1),num_of_hooks1,
                                                    ifelse(is.numeric(hooks_per_skate1),as.numeric(num_of_skates1_calc)*as.numeric(hooks_per_skate1),
                                                           mean(c(as.numeric(strsplit(hooks_per_skate1,",")[[1]][1]),
                                                                  as.numeric(strsplit(hooks_per_skate1,",")[[1]][2])))*as.numeric(num_of_skates1_calc))))
check$multi
check$num_of_skates1
check$hooks_per_skate1
mean(check$hooks_per_skate1)
is.numeric(check$hooks_per_skate1)

as.numeric(gsub(".*?([0-9]+).*", "\\1", check$hooks_per_skate1))

as.numeric(gsub(",",".*?([0-9]+).*",  check$hooks_per_skate1))

as.numeric(gsub(".*?([0-9]+).*", "\\", check$hooks_per_skate1))

a<-as.vector(strsplit(check$hooks_per_skate1[1],","))
a<-strsplit(check$hooks_per_skate1[1],",")
str(a)
as.numeric(a[[1]])
str(a[[1]])
mean(c(as.numeric(a[[1]][1]),as.numeric(a[[1]][2])))

mean(c(as.numeric(strsplit(check$hooks_per_skate1[1],",")[[1]][1]),
       as.numeric(strsplit(check$hooks_per_skate1[1],",")[[1]][2])))


library(qdapRegex)
rm_number(check$hooks_per_skate1)

qdapRegex::as_numeric2(gsub(",", "", check$hooks_per_skate1))

as_numeric2((ex_number(check$hooks_per_skate1)))

as_numeric2((ex_number(check$hooks_per_skate1)))

ex_number(check$hooks_per_skate1)
separate(check$hooks_per_skate1,into= paste0("blec_",1),sep = ",")
separate(split_gf1,into = paste0("gear_target_",1),sep = ",")

rm_number(check$hooks_per_skate1)

sd_check<-"2000-10-03 00:00:00"
gsa_check<-345631
year_check<-4
adfg_check<-61667

log9check<-as.data.frame(ll_log %>% filter(Sell.Date == sd_check,
                                           Groundfish.Stat.Area == gsa_check,
                                           Year == unique(ll_log$Year)[year_check],
                                           ADFG.Number == adfg_check)); log9check

log9check <-log9check %>% 
  mutate(#num_of_hooks1_calc = ifelse(!is.na(num_of_hooks1),num_of_hooks1,
    #                                             as.numeric(num_of_skates1)*as.numeric(hooks_per_skate1)),
    num_of_hooks1_calc = ifelse(!is.na(num_of_hooks1),num_of_hooks1,
                                as.numeric(num_of_skates1)*as.numeric(hooks_per_skate1)),
    num_of_skates1_calc = ifelse(!is.na(num_of_skates1),num_of_skates1,
                                 as.numeric(num_of_hooks1)/as.numeric(hooks_per_skate1)), 
    num_of_skates1_tst = ifelse(!is.na(num_of_skates1),num_of_skates1,
                                ifelse(is.numeric(hooks_per_skate1),as.numeric(num_of_hooks1)/as.numeric(hooks_per_skate1),
                                       as.numeric(num_of_hooks1)/mean(c(as.numeric(strsplit(hooks_per_skate1,",")[[1]][1]),
                                                                        as.numeric(strsplit(hooks_per_skate1,",")[[1]][2]))))),
    num_of_hooks1_tst = ifelse(!is.na(num_of_hooks1),num_of_hooks1,
                               ifelse(is.numeric(hooks_per_skate1),as.numeric(num_of_skates1_calc)*as.numeric(hooks_per_skate1),
                                      mean(c(as.numeric(strsplit(hooks_per_skate1,",")[[1]][1]),
                                             as.numeric(strsplit(hooks_per_skate1,",")[[1]][2])))*as.numeric(num_of_skates1_calc))),                             
    num_of_hooks2_test = as.numeric(num_of_skates2)*as.numeric(hooks_per_skate2),
    set_length_km = 1.609344*Set.Length..mi.,
    soak_time_hrs = Soak.Time.Hours,
    #Ticket_log = Ticket,
    #Species_log = Species,
    #Pounds_log = Pounds,
    #Numbers_log = Numbers
  ) 

!is.na(log9check$num_of_hooks1_calc)
is.numeric(log9check$num_of_hooks1_calc[1])
is.numeric(log9check$hooks_per_skate1)

mean(c(as.numeric(strsplit(log9check$hooks_per_skate1,",")[[1]][1]),
       as.numeric(strsplit(log9check$hooks_per_skate1,",")[[1]][2])))

as.numeric(strsplit(log9check$hooks_per_skate1,",")[[]][1])

as.numeric(log9check$num_of_skates1_test)

try<-log9check$hooks_per_skate1[1]
as.numeric(strsplit(try,",")[[1]][2])
