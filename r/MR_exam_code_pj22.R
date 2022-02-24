###########################################################################
## MR diagnostics and year-by-year review...
## Use mark_recapture.R lines 1:XX to load data
## then come back here to check shit out

# assessment_summary = record of pervious abundance estimates
# mr_summary = past mr variables as summarized by Jane
# releases = marks
# tag_summary = batch codes by year
# recoveries = recaps, currently filtering out past year's marks... 
# rel_sel = 5cm length bins for releases
# rec_sel = 5cm length binds for recoveries
# growth = growth of recaptured fish... 
#          culls out fish that grew too much; Jane got rid of over 5 cm
#         I opted to get rid of fish based on % increase in body mass
# growth_sel = releases (binned) adjusted for growth
# throw_out = tags from marked fish that were deemed too small based on recapture lengths
# *** want to examine effect of leaving these in, performing calculations and declaring
#     abundance estimate is germaine to population greater than size of smallest recap
# move = data examining movement of tagged fish... 
# marks = #'s of marked fish recvered... very confusing terminology
#fsh_cpue = need to get number of fish observed using weight data when not all fish were observed in the countback...
#        so this gives us trip specific WPUE for those calculations... 

#=====================================================================
# old school diagnostics
#1) check lengths with KS tests
D.func<-function(x,y){  #x<-C; y<-M
  n.x <- length(x)
  n.y <- length(y)
  n <- n.x * n.y/(n.x + n.y)
  w <- c(x, y)
  z <- cumsum(ifelse(order(w) <= n.x, 1/n.x, -1/n.y))
  max.at <- sort(w)[which(abs(z) == max(abs(z)))]
  return(max.at)
  
}

KS.func<-function(M,C,R){
  print(ks.test(M,R))
  print(D.func(M,R))
  print(ks.test(C,R))
  print(D.func(C,R))
  print(ks.test(M,C))
  print(D.func(M,C))
}

#Mark data frames; from mark_recapture.R
#these data have "too small" marks tossed aside
str(releases_bin)
releases_raw
#capture (2nd event sample) data frame
cees<-read.csv(paste0("data/fishery/fishery_bio_2000_", YEAR,".csv"))
#recaps
read_csv(paste0("data/fishery/tag_recoveries_2003_", YEAR, ".csv"), 
         guess_max = 50000) -> recaps
recaps %>% 
  mutate(year_batch = paste0(year, "_", tag_batch_no),
         year_trip = paste0(year, "_", trip_no),
         # use landing_date (same as the countbacks - see section below), otherwise catch_date
         date = as.Date(ifelse(is.na(landing_date), catch_date, landing_date))) %>% 
  filter(year >= FIRST_YEAR & year_batch %in% tag_summary$year_batch) -> recaps
recaps %>% 
  mutate(Project_cde = ifelse(is.na(Project_cde) &
                                grepl(c("Personal Use|subsistance|sport"), comments), 
                              "27", Project_cde)) -> recaps
recaps %>% 
  filter(!is.na(length) &
           measurer_type == "Scientific staff") %>% 
  mutate(length_bin = cut(length, breaks = seq(32.5, 117.5, 5),
                          labels = paste(seq(35, 115, 5)))) %>% 
  select(year, rec_date = date, rec_stat = Stat, tag_no, tag_batch_no, length = length, #rec_len = length, 
         rec_bin = length_bin) -> recaps

#FUNCTION to examine lengths and make at least first division

Length.bias.exam<-function(M,C,R){   #M,C,R are data frames with year and either
                                     #marks, recaps, or capture (2nd event sample)
  #M<-growth_sel; R<-recaps; C<-cees
  #
  j<-1
  Res<-data.frame()
  Ys<-sort(unique(M$year))
  #par(mfrow=c(ceiling(length(Ys)/3),3))
  par(mfrow=c(3,ceiling(length(Ys)/3)))
  for (i in Ys) {  #i<-Ys[1]
    Ms<-M$length[!is.na(M$length) & M$year == i]
    Rs<-R$length[!is.na(R$length) & R$year == i]
    Cs<-C$length[!is.na(C$length) & C$year == i]
    
    Ccdf<-ecdf(Cs); Mcdf<-ecdf(Ms) 
    plot(Ccdf, verticals=TRUE, do.points=FALSE, main=i)
    plot(Mcdf, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")
    if(length(Rs) > 1) {
    Rcdf<-ecdf(Rs)
    plot(Rcdf, verticals=TRUE, do.points=FALSE, add=TRUE, col="forestgreen")
    }
    
    Res[j,"year"]<-i
    Res[j,"min.M"]<-min(Ms)
    Res[j,"min.C"]<-min(Cs)
    Res[j,"min.R"]<-min(Rs)
    
    Res[j,"MvR.p"]<-round(ks.test(Ms,Rs)$p.value,2)
    #Res[j,"MvR.D"]<-round(ks.test(Ms,Rs)$statistic,3)
    Res[j,"MR.strat.lgth"]<-D.func(Ms,Rs)
    Res[j,"MvC.p"]<-round(ks.test(Ms,Cs)$p.value,2)
    #Res[j,"MvC.D"]<-round(ks.test(Ms,Cs)$statistic,3)
    Res[j,"MC.strat.lgth"]<-D.func(Ms,Cs)
    Res[j,"CvR.p"]<-round(ks.test(Cs,Rs)$p.value,2)
    #Res[j,"CvR.D"]<-round(ks.test(Cs,Rs)$statistic,3)
    Res[j,"CR.strat.lgth"]<-D.func(Cs,Rs)

    #if significant different, stratify once in the loop and recheck.  
    if (Res[j,"MvR.p"] < 0.05 & Res[j,"MvC.p"] < 0.05 & Res[j,"CvR.p"]<0.05) {
      #CASE IV stratify around MR length = round(Res[j,"MC.strat.lgth"],0)
      Strat<-round(Res[j,"MC.strat.lgth"],0)
      Res[j,"Strat_length"]<-Strat
      
      #small
      Ms.sm<-Ms[Ms<= Strat]; Cs.sm<-Cs[Cs<=Strat]; Rs.sm<-Rs[Rs<=Strat]
      Res[j,"MvR.sm.p"]<-round(ks.test(Ms.sm,Rs.sm)$p.value,2)
      #Res[j,"MvR.sm.D"]<-round(ks.test(Ms.sm,Rs.sm)$statistic,3)
      Res[j,"MR.sm.strat.lgth"]<-D.func(Ms.sm,Rs.sm)
      Res[j,"MvC.sm.p"]<-round(ks.test(Ms.sm,Cs.sm)$p.value,2)
      #Res[j,"MvC.sm.D"]<-round(ks.test(Ms.sm,Cs.sm)$statistic,3)
      Res[j,"MC.sm.strat.lgth"]<-D.func(Ms.sm,Cs.sm)
      Res[j,"CvR.sm.p"]<-round(ks.test(Cs.sm,Rs.sm)$p.value,2)
      #Res[j,"CvR.sm.D"]<-round(ks.test(Cs.sm,Rs.sm)$statistic,3)
      Res[j,"CR.sm.strat.lgth"]<-D.func(Cs.sm,Rs.sm)
      
      #big
      Ms.bg<-Ms[Ms> Strat]; Cs.bg<-Cs[Cs>Strat]; Rs.bg<-Rs[Rs>Strat]
      Res[j,"MvR.bg.p"]<-round(ks.test(Ms.bg,Rs.bg)$p.value,2)
      #Res[j,"MvR.bg.D"]<-round(ks.test(Ms.bg,Rs.bg)$statistic,3)
      Res[j,"MR.bg.strat.lgth"]<-D.func(Ms.bg,Rs.bg)
      Res[j,"MvC.bg.p"]<-round(ks.test(Ms.bg,Cs.bg)$p.value,2)
      #Res[j,"MvC.bg.D"]<-round(ks.test(Ms.bg,Cs.bg)$statistic,3)
      Res[j,"MC.bg.strat.lgth"]<-D.func(Ms.bg,Cs.bg)
      Res[j,"CvR.bg.p"]<-round(ks.test(Cs.bg,Rs.bg)$p.value,2)
      #Res[j,"CvR.bg.D"]<-round(ks.test(Cs.bg,Rs.bg)$statistic,3)
      Res[j,"CR.bg.strat.lgth"]<-D.func(Cs.bg,Rs.bg)
      
    } else {
      Res[j,"Strat_length"]<-"No"
    }
    
    j<-j+1
  }
  return(Res)
}

#M<-growth_sel; R<-recaps; C<-cees
Length_exam<-Length.bias.exam(M=releases_raw,C=cees,R=recaps)

#!!! 5 years suggest stratification is necessary!!! 

#=================================================================================
# Spatial bias exam...
head(move,20)
head(move[move$year == 2005,],20)
move5<-move[move$year == 2005,]
rel5<-releases_raw[releases_raw$year == 2005,]

#1 Test of complete mixing
as<-unique(move5$recaptures)
mvmt.chi<-matrix(nrow=length(as),ncol=length(as)+1)
i<-1
for (a in as){   #a<-as[1]
  a1<-move5[move5$recaptures == a,]
  mvmt.chi[,i]<-a1$Freq
  i<-i+1
}
i<-1
for (a in as){
  mvmt.chi[i,length(as)+1] <- nrow(rel5[rel5$Stat == a,])-
    sum(mvmt.chi[i,], na.rm=TRUE)
  i<-i+1
}

chisq.test(mvmt.chi)
xt<-chisq.test(mvmt.chi)
xt$p.value

head(releases_raw)

#------------------------------------------------------------------------
#2 Test of equal prob of capture 1st Event
# get Stat area for modified Marks - need to recreate marks data frame from Jane's code
#load everything and then start here ~line639 in MR code
read_csv(paste0("data/fishery/nsei_daily_tag_accounting_2004_", YEAR-1, ".csv")) -> marks3 
marks3<-marks
#** marks already loaded with summed weight by trip number

marks3 %>% 
  filter(year >= FIRST_YEAR &
           !year %in% NO_MARK_SRV) %>% 
  mutate(all_observed = ifelse(
    !grepl(c("Missing|missing|Missed|missed|eastern|Eastern|not counted|
             Did not observe|did not observe|dressed|Dressed"), comments) & 
      observed_flag == "Yes", "Yes", "No"),
    mean_weight = ifelse(all_observed == "Yes", whole_kg/total_obs, NA),
    year_trip = paste0(year, "_", trip_no)) -> marks3
nrow(marks3)

#load fsh_bio: line ~658 in MR code
left_join(marks3, fsh_bio, by = c("date", "trip_no")) %>% 
  mutate(mean_weight = ifelse(!is.na(mean_weight_bios), mean_weight_bios, mean_weight)) %>% 
  select(-mean_weight_bios) -> marks3

##!! Thru here should be the same as Jane's "mark" through line ~ 696
marks3<-marks
#load fsh_cpue: line ~699 in Jane's.  Modified here to include Stat so we get
# CPUE by trip and stat area....
read_csv(paste0("data/fishery/fishery_cpue_2022reboot_1997_", YEAR,".csv"),
         guess_max = 50000) %>% 
  filter(Spp_cde == "710") %>% 
  mutate(sable_kg_set = sable_lbs_set * 0.45359237, # conversion lb to kg
         std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (0.0254 * hook_space))), #standardize hook spacing (Sigler & Lunsford 2001, CJFAS)
         # kg sablefish/1000 hooks, following Mueter 2007
         WPUE = sable_kg_set / (std_hooks / 1000)) %>% 
  filter(!is.na(date) & 
           !is.na(sable_lbs_set) &
           # omit special projects before/after fishery
           julian_day > 226 & julian_day < 322) %>% 
  group_by(year, trip_no, Stat) %>% 
  dplyr::summarize(WPUE = mean(WPUE)) -> fsh_cpue_stat

left_join(marks3, fsh_cpue_stat, by = c("year", "trip_no")) %>% 
  mutate(year_trip_stat = paste0(year, "_", trip_no,"_",Stat))-> marks4

#at this point we have stat*trip specific WCPUE, but whole_kg is only trip specific...?

recoveries %>% 
  filter(year_trip %in% marks$year_trip & Project_cde == "02") %>%  
  mutate(year_trip_stat=paste0(year_trip,"_",Stat)) %>%
  group_by(year_trip_stat) %>% 
  dplyr::summarize(tags_from_fishery = n_distinct(tag_no)) %>% 
  right_join(marks4, by = "year_trip_stat") -> marks5

#still same with spec wpue but biomass is only trip specific... 

anti_join(recoveries %>% 
            filter(!trip_no %in% c(1, 2, 3) &  # pot and longline surveys
                     !is.na(trip_no) & 
                     Project_cde == "02"), 
          marks4, by = "year_trip") %>% 
  group_by(year_trip, Mgmt_area, date) %>% 
  dplyr::summarize(tags_from_fishery = n_distinct(tag_no)) -> no_match3

#get stat areas by trip number ... but... this doesn't work because some trips
# cover more than on stat area... see 2005_106 for example...

#line 810 gets N numbers frm weight data... 
marks5 %>% 
  # padr::pad fills in missing dates with NAs, grouping by years.
  pad(group = "year") %>% 
  group_by(year, date) %>% 
  dplyr::summarize(whole_kg = sum(whole_kg),
                   total_obs = sum(total_obs),
                   total_marked = sum(marked),
                   tags_from_fishery = sum(tags_from_fishery),
                   mean_weight = mean(mean_weight),
                   mean_wpue = mean(WPUE)) %>% 
  # interpolate mean_weight column to get npue from wpue (some trips have wpue
  # data but no bio data)
  mutate(interp_mean = zoo::na.approx(mean_weight, maxgap = 20, rule = 2),
         mean_npue = mean_wpue / interp_mean) %>%    #<-weight to n
  # padr::fill_ replaces NAs with 0 for specified cols
  fill_by_value(whole_kg, total_obs, total_marked, tags_from_fishery, value = 0) %>% 
  group_by(year) %>% 
  mutate(cum_whole_kg = cumsum(whole_kg), #cumsum makes vector
         cum_obs = cumsum(total_obs),
         cum_marks = cumsum(total_marked),
         julian_day = yday(date)) -> daily_marks3
view(daily_marks3)
view(daily_marks)
#test run/development
marks3_05<-marks3[marks3$year == 2005,]

e1.chi<-data.frame()
i<-1
for (a in as){
  m2<-move5[move5$recaptures == a,]
  e1.chi[i,1]<-sum(m2$Freq) #recaptured m2
  n2<-marks3_05[marks3_05$Stat == a,]
  e1.chi[i,2]<-56565 -e1.chi[i,1]#unmarked (n2-m2) #!!!need number of fish examined 
                                              #in each area!!! 
}

#------------------------------------------------------------------------------
#3 Test of equal prob of capture 2nd Event

#==================================================================================
# SCRAP.... 

rel20<-growth_sel[growth_sel$year == 2020,]
nrow(rel20)
str(rel20); nrow(rel20[is.na(rel20$tag_no),])
M<-rel20$growth_len[!is.na(rel20$growth_len)]
length(M)

read_csv(paste0("data/fishery/tag_recoveries_2003_", YEAR, ".csv"), 
         guess_max = 50000) -> recaps
recaps %>% 
  mutate(year_batch = paste0(year, "_", tag_batch_no),
         year_trip = paste0(year, "_", trip_no),
         # use landing_date (same as the countbacks - see section below), otherwise catch_date
         date = as.Date(ifelse(is.na(landing_date), catch_date, landing_date))) %>% 
  filter(year >= FIRST_YEAR & year_batch %in% tag_summary$year_batch) -> recaps
nrow(recaps)
rec20<-recaps[recaps$year == 2020,]
nrow(rec20[is.na(rec20$tag_no),])
recaps20<-rec20$length[!is.na(rec20$length)]
length(recaps20) 

#*** need to figure out Jane's Cs... ???

str(cees)
cees20<-cees[cees$year == 2020,]
C<-cees20$length[!is.na(cees20$length)]
length(C)

Ccdf<-ecdf(C); Mcdf<-ecdf(M); Rcdf<-ecdf(recaps20)
plot(Ccdf, verticals=TRUE, do.points=FALSE)
plot(Mcdf, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")
plot(Rcdf, verticals=TRUE, do.points=FALSE, add=TRUE, col="forestgreen")

ks.test(M,recaps20); summary(ks.test(M,recaps20)); D.func(M,recaps20)
ks.test(M,C); summary(ks.test(M,C)); D.func(M,C)
ks.test(C,recaps); summary(ks.test(C,recaps)); D.func(C,recaps)

min(C)
min(M)
min(recaps20)
Clte61<-C[C<=61]; Mlte61<-M[M<=61]; Rlte61<-recaps20[recaps20<=61]

KS.func(Mlte61,Clte61,Rlte61)

Cgt61<-C[C>61]; Mgt61<-M[M>61]; Rgt61<-recaps20[recaps20>61]

KS.func(Mgt61,Cgt61,Rgt61)

#** results show unequal cap prob in 1st event but not in second once stratified around 61... 
#** should evaluate how stratification changes abundance!!! 
#** not sure thats practical because unmarked second event fish are not all measured
#** would need to schwag out numbers based on length distributions... acceptable... 

#2) check mixing and geographic problems with chi-square tests
# make mixing table
# make st event table: two rows of marked (m2) and unmarked (n2-m2) for each geographic strate
# make 2nd event table with rows of recaptured (m2) - not recaptured (n1-m2) for each area (stat area


#=====================================================================
# Jane's MR jagsfunction
j=1
# Base strata on percentiles of cumulative catch. Could also use number of marks
# observed or some other variable. STRATA_NUM is the dynamic variable specifying
# the number of time strata to split the fishery into and it currently
# accomodates 8 or fewer strata.
STRATA_NUM <- j

daily_marks %>% 
  group_by(year) %>% 
  mutate(catch_strata = cut(percent_rank(cum_whole_kg) %>% round(2),
                            breaks = seq(0, 1, by = 1 / STRATA_NUM),
                            include.lowest = TRUE, right = TRUE, ordered_result = TRUE,
                            labels = paste(seq(2, STRATA_NUM + 1, by = 1)))) -> daily_marks

# Summarize by strata
#PJ note 2022: calculating C (number of fish examined for marks) from biomass of harvest? 
daily_marks %>% #filter(year == YEAR) %>% 
  group_by(year, catch_strata) %>% 
  dplyr::summarize(t = n_distinct(date),
                   catch_kg = sum(whole_kg) %>% round(1),
                   n = sum(total_obs),
                   k = sum(total_marked),
                   D = sum(fishery_D),
                   NPUE = mean(mean_npue, na.rm = TRUE) %>% round(1),
                   # Use the interpolated mean weight here so it doesn't break at large
                   # STRATA_NUMs
                   mean_weight = mean(interp_mean, na.rm = TRUE)) %>% 
  mutate(C = (catch_kg / mean_weight) %>% round(0)) -> strata_sum

# Running Chapman estimator
strata_sum %>% 
  left_join(tag_summary %>% select(year, K.0), by = "year") %>% 
  # cumulative marked and observed
  group_by(year) %>% 
  mutate(cumn = cumsum(n),
         cumk = cumsum(k),
         cumd = cumsum(D),
         K_running = K.0 - cumd) %>% 
  # This deducts period specific D's
  mutate(running_Chapman = ((K_running + 1)*(cumn + 1) / (cumk + 1)) - 1,
         Chap_var = ((K_running + 1)*(cumn + 1)*(K_running - cumk)*(cumn - cumk)) / ((cumk + 1)*(cumk + 1)*(cumk + 2)) ) -> strata_sum
#view(strata_sum)
# Prep data for JAGS ----  

# Here we're pivoting each of the catch strata into their own columns, e.g. D is now D.2 and D.3, for example.
dcast(setDT(strata_sum), year ~ catch_strata, #, fun.aggregate = function(x) sum(!is.na(x)), # sum acts like identity
      value.var = c("D", "C", "n", "t", "k", "NPUE"), sep = ".") %>%  
  left_join(tag_summary, by = "year") %>% 
  # order the columns alphanumerically - very convenient
  select(year, order(colnames(.))) -> jags_dat

jags_dat %>%
  select(year, K.0, contains("D."), contains("C."), starts_with("n."), 
         starts_with("t."), contains("k."), contains("NPUE.")) -> jags_dat

# Add in the abundance estimates from the past assessments as mu.N (the mean for
# the prior on N.1)
assessment_summary %>% 
  filter(year >= FIRST_YEAR & !year %in% NO_MARK_SRV) %>% 
  select(year, mu.N = abundance_age2plus) -> abd_est

left_join(jags_dat, abd_est, by = "year") -> jags_dat

# Length of unique years to run model
model_years <- unique(jags_dat$year)

# Create an empty list to store JAGS data (we want a list of lists)
model_dat <- vector('list', length(model_years))

# Reshape data into lists of each variable. Flexible by number of time strata.
# Will look for a more efficient way to do this next year - maybe start here:
# https://stackoverflow.com/questions/31561238/lapply-function-loops-on-list-of-lists-r

for(i in 1:length(model_years)){
  #i<-10
  sub <- jags_dat %>% filter(year == model_years[i])
  
  sub_dat <-
    list(P = STRATA_NUM + 1, # number of time Periods
         M = 0.1 / 365, # Daily natural mortality
         mu.N = sub$mu.N, # mean of starting abundance values, from past assessments
         K.0 = sub$K.0,
         D.0 = sub$D.0,
         C = select(sub, contains("C.")) %>% as.numeric() , 
         D = select(sub, contains("D."), -D.0) %>% as.numeric(),
         t = select(sub, contains("t.")) %>% as.numeric(),
         n = select(sub, contains("n.")) %>% as.numeric(),
         k = select(sub, contains("k."), -K.0) %>% as.numeric(),
         NPUE = select(sub, contains("NPUE.")) %>% as.numeric()
    )
  
  model_dat[[i]] <- sub_dat
  rm(sub_dat)
  
}

mr_jags <- function(
  mod, # model formula (as a character string)
  mod_name, # what you want to name your model, keep them straight during model selection
  model_dat, # list of lists of model data 
  model_years, # vector of years over which you want to run the model
  mpar, # parameters of interest, sample poterior distribution of 'mpar' variables
  mchains = 4, # number of desired chains
  madapt = 2000, # burn in
  miter = 50000, # number of iterations to run each chain
  mthin = 10 # thinning rate
) {
  
  
  mod1_out <- mr_jags(mod = mod1, mod_name = "Model1", 
                      model_dat = model_dat, model_years = model_years, 
                      mpar = c("N.avg", "N", "p"),
                      miter = 50000)
  
  
  # # Create an empty list to store model output 
  # model_output <- vector('list', 1)
  
  for(i in 1:length(model_years)){  #for each stratum opt run each year
    i<-1
    dat <- model_dat[[i]]
    
    cat(mod, file = paste0(mod_name, "_", model_years[i], ".jag"))  #recording for output
    
    # initialize and run model
    mod_init <- jags.model(paste0(mod_name, "_", model_years[i], ".jag"),
                           data = dat,
                           n.chains = mchains,
                           # init = tst_inits,
                           n.adapt = madapt)
    
    # sample posterior
    res <- coda.samples(mod_init,
                        var = mpar,
                        n.iter = miter,
                        thin = mthin)
    
    # format output using user-defined fxn coda_df
    coda_df(res) %>% 
      mutate(year = model_years[i],
             model = mod_name) -> coda_res
    
    #Append results 
    if(i == 1){
      coda_res_out <- coda_res
      rm(coda_res)
    } else {
      coda_res_out <- rbind(coda_res_out, coda_res) }
    
    # Diagnostic trace plots - These MR models were tested individually by year.
    # They are well mixed and have no issues with convergence.
    
     plot(res, col = 2)
     library(MCMCvis)
     MCMCtrace(res, params=c("N","N.avg","p"), ISB=TRUE, pdf=FALSE, Rhat=TRUE)
     traceplot(res, parameters=c("N","N.avg","p"), Rhat_min=1.5, layout=c(2,2), ask=FALSE)
    # Get DIC for model selection
    # https://www4.stat.ncsu.edu/~reich/st590/code/DICpois
    
    dic <- dic.samples(mod_init,
                       var = mpar,
                       n.iter = miter,
                       thin = mthin)
    
    dic <- data.frame(model = mod_name,
                      year = model_years[i],
                      deviance = sum(dic$deviance), # over all fit (smaller deviance better)
                      parameter_penalty = sum(dic$penalty), # number of parameters
                      DIC = sum(dic$deviance) + sum(dic$penalty)) # penalized deviance (aka DIC), smaller is better
    
    # Append results 
    if(i == 1){
      dic_out <- dic
      rm(dic)
    } else {
      dic_out <- rbind(dic_out, dic) }
    
    # Convergance diagnostic: gelman.diag gives you the scale reduction factors for
    # each parameter. A factor of 1 means that between variance and within chain
    # variance are equal, larger values mean that there is still a notable
    # difference between chains. General rule: everything below 1.1 or so
    # is ok.
    # https://theoreticalecology.wordpress.com/2011/12/09/mcmc-chain-analysis-and-convergence-diagnostics-with-coda-in-r/
    gelman.diag(res, multivariate = FALSE)[[1]] %>%
      data.frame() %>% 
      mutate(year = model_years[i],
             model = mod_name) %>% 
      distinct() -> convergence
    
    # Append results 
    if(i == 1){
      convergence_out <- convergence
      rm(convergence)
    } else {
      convergence_out <- rbind(convergence_out, convergence) }
    
  } 
  
  # Summarize the abundance estimate for quick model comparison
  coda_res_out %>% 
    group_by(year) %>% 
    mutate(N.avg = N.avg / 1000000) %>% 
    dplyr::summarize(mean = mean(N.avg),
                     q025 = quantile(N.avg, 0.025),
                     q975 = quantile(N.avg, 0.975)) %>% 
    mutate(model = mod_name) -> n_summary_out
  
  model_output <- list("results" = coda_res_out,
                       "N_summary" = n_summary_out,
                       "dic" = dic_out,
                       "convergence_diagnostic" = convergence_out)
  
  return(model_output)
  
}

#=========================================================
