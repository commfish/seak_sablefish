###########################################################################
## MR diagnostics and year-by-year review...
## Use mark_recapture.R lines 1:XX to load data
## then come back here to check things out

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
#================================================================================
#Getting a handle on comments and whether there was an accurate countback
marks20<-marks[marks$year == 2020,]
nrow(marks20)
view(marks20)
unique(marks20$comments)
com20<-marks20[!is.na(marks20$comments),]
view(com20); unique(com20$comments)

marks19<-marks[marks$year == 2019,]
nrow(marks19)
view(marks19)
unique(marks19$comments)
com19<-marks19[!is.na(marks19$comments),]
view(com19)
unique(com19$comments)
#looks like Jane may be overly cautious here, but 

# Next step (3-18-22): go back through code and build "marks" data frame so that "tag_summery" 
# df can have length strata (separate rows) and area strat (marking, recovery, both)
# Once thats done, model can be modified to accommodate length and spatial strata
# Bring over all mark_recapture code over here so new analysis all in this script
# Try again to run spatial diagnostics: if epc in one event then we wouldn't need
# to fret about that.  Complete mixing will obviously not be satisfied (test anyways)




#========================================================================
#Line 836 is where daily_marks are figured out by extrapolating from landed biomass
# here is code to do so just using accurate countback data
# !! note - will still need Jane's code to account for tags beging removed
#play code looking at one day
days<-unique(marks20$date)

eg<-marks20[marks20$date == days[21] & marks20$observed_flag == "Yes",]
view(eg)

rec20<-recoveries[recoveries$year == 2020,]
unique(rec20$year_trip)
view(rec20); nrow(rec20)

re119<-recoveries[recoveries$year_trip == "2020_119",] 
re2513<-recoveries[recoveries$year_trip == "2020_2513",] 
re4060<-recoveries[recoveries$year_trip == "2020_4060",]
re9302<-recoveries[recoveries$year_trip == "2020_9302",]

view(reeg)
nrow(reeg)

#--- Goodmarks = only use data from full countbacks...
# picking up from line 822 (or so) : Daily summary or so in mark_recapture.R
# Trial 1: filter out only good countbacks...
# Results: not so good... miscalculate's the D's so won't work.
{
goodmarks<-marks[marks$observed_flag == "Yes",]

goodmarks %>% 
  # padr::pad fills in missing dates with NAs, grouping by years.
  pad(group = "year") %>% 
  group_by(year, date) %>% 
  dplyr::summarize(whole_kg = sum(whole_kg, na.rm=TRUE),
                   total_obs = sum(total_obs, na.rm=TRUE), #total accounted for in countbacks
                   total_marked = sum(marked, na.rm=TRUE), #number marks in countback ... here including incomplete countbacks
                   tags_from_fishery = sum(tags_from_fishery, na.rm=TRUE),  #using marks from recoveries, which don't agree with count_back data!
                   mean_weight = mean(mean_weight, na.rm=TRUE),
                   mean_wpue = mean(WPUE, na.rm=TRUE)) %>% 
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
         julian_day = yday(date)) -> gooddaily_marks

recoveries %>% 
  filter(date %in% daily_marks$date) %>% 
  group_by(date) %>% 
  dplyr::summarize(other_tags = n_distinct(tag_no)) %>% 
  right_join(gooddaily_marks, by = "date") %>% 
  # padr::fill_ replaces NAs with 0 for specified cols
  fill_by_value(other_tags, value = 0) %>% 
  # If # of id'd tags  > marks from countback, the D for the day is the difference
  # between tags and countback marks plus other tags from the recoveries df that haven't
  # been accounted for yet. This assumes any tags recovered at time of countback
  # are potentially linked with a marked fish.
  mutate(fishery_D = ifelse(tags_from_fishery - total_marked > 0,  #
                            tags_from_fishery - total_marked + other_tags, 
                            other_tags)) -> gooddaily_marks

# Remove these matched tags from the releases df so they don't get double-counted by accident. 
recoveries %>% filter(!c(date %in% gooddaily_marks$date)) -> recoveries_good

view(gooddaily_marks)
view(daily_marks)

ys<-unique(daily_marks$year)
par(mfrow=c(2,2))

for (y in ys) {  #y<-ys[1]
  colp<-adjustcolor("red", alpha.f=0.5)
daily<-daily_marks[daily_marks$year == y,]
good<-gooddaily_marks[gooddaily_marks$year == y,]
  plot(daily$total_marked ~ daily$date, main=y, col="blue", pch=18)
  points(good$total_marked ~ good$date, col=colp)
  daily2<-daily[daily$date %in% good$date,]
  r2<-summary(lm(good$total_marked ~ daily2$total_marked))
  mtext(round(r2$adj.r.squared,2),side=3,adj=1,padj=1.2)
}

for (y in ys) {  #y<-ys[1]
  colp<-adjustcolor("red", alpha.f=0.5)
  daily<-daily_marks[daily_marks$year == y,]
  good<-gooddaily_marks[gooddaily_marks$year == y,]
  plot(daily$total_obs ~ daily$date, main=y, col="blue", pch=18)
  points(good$total_obs ~ good$date, col=colp)
  daily2<-daily[daily$date %in% good$date,]
  r2<-summary(lm(good$total_obs ~ daily2$total_obs))
  mtext(round(r2$adj.r.squared,2),side=3,adj=1,padj=1.2)
}

for (y in ys) {  #y<-ys[1]
  colp<-adjustcolor("red", alpha.f=0.5)
  daily<-daily_marks[daily_marks$year == y,]
  good<-gooddaily_marks[gooddaily_marks$year == y,]
  plot(daily$fishery_D ~ daily$date, main=y, col="blue", pch=18)
  points(good$fishery_D ~ good$date, col=colp)
  daily2<-daily[daily$date %in% good$date,]
  r2<-summary(lm(good$fishery_D ~ daily2$fishery_D))
  mtext(round(r2$adj.r.squared,2),side=3,adj=1,padj=1.2)
}

for (y in ys) {  #y<-ys[1]
  colp<-adjustcolor("red", alpha.f=0.5)
  daily<-daily_marks[daily_marks$year == y,]
  good<-gooddaily_marks[gooddaily_marks$year == y,]
  daily %>% mutate(cum_fish_D =cumsum(fishery_D) ) -> daily
  good %>% mutate(cum_fish_D =cumsum(fishery_D) ) -> good
  plot(daily$cum_fish_D ~ daily$date, main=y, col="blue", pch=18)
  points(good$cum_fish_D ~ good$date, col=colp)
  daily2<-daily[daily$date %in% good$date,]
  r2<-summary(lm(good$cum_fish_D ~ daily2$cum_fish_D))
  mtext(round(r2$adj.r.squared,2),side=3,adj=1,padj=1.2)
}
#observed and marks are pretty close except 2020 (covid?)
#however, D values are pretty different... but r2 not good way to look at that...
# look at cumulative stuff too... ->
str(gooddaily_marks)

for (y in ys) {  #y<-ys[1]
  colp<-adjustcolor("red", alpha.f=0.5)
  daily<-daily_marks[daily_marks$year == y,]
  good<-gooddaily_marks[gooddaily_marks$year == y,]
  plot(daily$cum_obs ~ daily$date, main=y, col="blue", pch=18)
  points(good$cum_obs ~ good$date, col=colp)
  daily2<-daily[daily$date %in% good$date,]
  r2<-summary(lm(good$cum_obs ~ daily2$cum_obs))
  mtext(round(r2$adj.r.squared,2),side=3,adj=1,padj=1.2)
}   #2019 and 2020 have more in good data set?!?!? 

for (y in ys) {  #y<-ys[12]
  colp<-adjustcolor("red", alpha.f=0.5)
  daily<-daily_marks[daily_marks$year == y,]
  good<-gooddaily_marks[gooddaily_marks$year == y,]
  plot(daily$cum_marks ~ daily$date, main=y, col="blue", pch=18)
  points(good$cum_marks ~ good$date, col=colp)
  daily2<-daily[daily$date %in% good$date,]
  r2<-summary(lm(good$cum_marks ~ daily2$cum_marks))
  mtext(round(r2$adj.r.squared,2),side=3,adj=1,padj=1.2)
}



view(good); view(daily)

ma19<-marks[marks$year == 2019,]
go19<-goodmarks[goodmarks$year == 2019,]

nrow(ma19); nrow(go19)

days<-unique(ma19$date)

day<-days[3]
ma19[ma19$date == day,]
go19[go19$date == day,]
daily_marks[daily_marks$date == day,]; daily_marks[daily_marks$date == day,]$cum_marks
gooddaily_marks[gooddaily_marks$date == day,]; gooddaily_marks[gooddaily_marks$date == day,]$cum_marks

view(ma19); view(go19)
plot(ma19$marked ~ ma19$date)
points(go19$marked ~ go19$date, col="red", pch=18, cex=0.7)

mac19<-daily_marks[daily_marks$year == 2019,]
gac19<-gooddaily_marks[gooddaily_marks$year == 2019,]

plot(mac19$cum_marks ~ mac19$date)
points(gac19$cum_marks ~ gac19$date, col="red")
#Below are my first attempts at running diagnostics on the MR project.  Some is 
# repreated in what is above this.  Retained here to keep in one script file
gooddaily_marks %>% 
  group_by(year) %>% 
  dplyr::summarize(k_fishery = sum(total_marked),
                   n_fishery = sum(total_obs),
                   D_fishery = sum(fishery_D)) %>% 
  left_join(tag_summary) -> goodtag_summary

view(goodtag_summary)
#similar to original except D_fishery MUCH smaller now... (rechecking...)
}
#Trial 2: need to filter within for no observed and no marks, but keep all D's and other
# fishery data... 
marks  %>%
  filter(observed_flag == "Yes") %>%
  pad(group = "year") %>% 
  group_by(year, date) %>% 
  dplyr::summarize(#whole_kg = sum(whole_kg, na.rm=TRUE),
                   clean_total_obs = sum(total_obs, na.rm=TRUE), #total accounted for in countbacks
                   clean_total_marked = sum(marked, na.rm=TRUE)) %>% 
  fill_by_value(clean_total_obs, clean_total_marked, value = 0) %>% 
  group_by(year) %>% 
  mutate(clean_cum_obs = cumsum(clean_total_obs),
         clean_cum_marks = cumsum(clean_total_marked)) %>%
  select(date,clean_total_obs,clean_total_marked,clean_cum_obs,clean_cum_marks)-> cleandaily#daily_marks_good

daily_marks %>% right_join(cleandaily, by="date") %>%
  mutate(year = year.x) -> clean_daily_marks
view(clean_daily_marks)

ys<-unique(clean_daily_marks$year)
par(mfrow=c(2,2))

for (y in ys) {  #y<-ys[1]
  colp<-adjustcolor("red", alpha.f=0.5)
  daily<-clean_daily_marks[clean_daily_marks$year == y,]
  plot(daily$cum_obs ~ daily$date, main=y, col="blue", pch=18)
  points(daily$clean_cum_obs ~ daily$date, col=colp)
}
for (y in ys) {  #y<-ys[1]
  colp<-adjustcolor("red", alpha.f=0.5)
  daily<-clean_daily_marks[clean_daily_marks$year == y,]
  plot(daily$cum_marks ~ daily$date, main=y, col="blue", pch=18)
  points(daily$clean_cum_marks ~ daily$date, col=colp)
}
#OK, I'm liking this... now for calculating fishery_D do I use raw marks, or clean_marks?
# use raw marks, because goal is to account for tags that have been removed from the population
# n and k are calculated from the observations and marks separately from D

#**increasingly uncomfortable with the way we remove tags from recoveries at various steps of the code
#* would be better to properly account for them with filters (less likely to make errors?) view(recoveries)
#* # maybe even make seperate df's for the different category... 

recoveries %>% 
  filter(date %in% clean_daily_marks$date) %>% 
  group_by(date) %>% 
  dplyr::summarize(other_tags = n_distinct(tag_no)) %>% 
  right_join(clean_daily_marks, by = "date") %>% 
  # padr::fill_ replaces NAs with 0 for specified cols
  fill_by_value(other_tags, value = 0) %>% 
  # If # of id'd tags  > marks from countback, the D for the day is the difference
  # between tags and countback marks plus other tags from the recoveries df that haven't
  # been accounted for yet. This assumes any tags recovered at time of countback
  # are potentially linked with a marked fish.
  mutate(fishery_D = ifelse(tags_from_fishery - total_marked > 0,  #
                            tags_from_fishery - total_marked + other_tags,  #other tags - total number of unique tags in recoveries df
                            other_tags),
         fish_D_ch = ifelse(tags_from_fishery - clean_total_marked > 0,  #
                               tags_from_fishery - clean_total_marked + other_tags,  #other tags - total number of unique tags in recoveries df
                               other_tags)) -> clean_daily_marks

for (y in ys) {  #y<-ys[1]
  colp<-adjustcolor("red", alpha.f=0.5)
  daily<-clean_daily_marks[clean_daily_marks$year == y,]
  plot(daily$fishery_D ~ daily$date, main=y, col="blue", pch=18)
  points(daily$fish_D_ch ~ daily$date, col=colp)
}
clean_daily_marks$D_check<-clean_daily_marks$fishery_D-clean_daily_marks$fish_D_ch
hist(clean_daily_marks$D_check, breaks=100); plot(density(clean_daily_marks$D_check))
min(clean_daily_marks$D_check); max(clean_daily_marks$D_check)

# Remove these matched tags from the releases df so they don't get double-counted by accident. 
recoveries %>% filter(!c(date %in% clean_daily_marks$date)) -> recoveries

# using clean marks makes for bigger fishery D's; size difference probably trivial
# relative to the number of marks out there... 
# That being said, I think we should use fish_D_ch here as the larger number reflects
# the tags filtered out from incomplete countbacks
# model estimates K by subtracting k (marks used) and D (other marks not in M:U ratio)

# TANGENT (PJ22): This has me questioning this whole part of the model; if tags are being removed proportional
# to the population (i.e, no selectivity for marks) then marked:unmarked ratio
# should not be affected.  Make's me think a simpler model could be developed?  Would be
# more relevant in an open jolly-seber style pop model... 

# Remaining tag loss ---- (from original)

# These are tags from other fisheries, from the directed fishery but without
# trip/date info, or Canada, or IPHCS/NMFS/other surveys. Potential periods of
# remaining tag loss:
#   1) Between start of pot survey and day before start of LL survey (D.0)
#   2) Beginning of longline survey and day before start of fishery (D.1)
#   3) During fishery (fishery_D will become D.i)
#   4) After survey (postfishery_D) 

# The remaining tag recoveries without dates appear to be best suited to the 
# prefishery_D category (they're almost all project 02's and most comments seem 
# to suggest that the sampler just didn't know which trip to assign the tag to).
# Note that is.na(date)'s have decreased through time, likely due to better
# reporting, both by samplers and the fleet.

# Approach:
#   1) merge dates from tag_summary into recoveries
#   2) create new factor based on time periods
#   3) summarize by period, reshape, and join back to tag_summary

right_join(recoveries %>% 
             select(year, date, Project_cde, tag_no),
           tag_summary %>% 
             select(year, potsrv_beg, llsrv_beg, llsrv_end, fishery_beg, fishery_end),
           by = "year") %>%
  mutate(D = derivedFactor(
    # as a check there should be none of these
    'error' = date < potsrv_beg,
    'D.0' = date >= potsrv_beg & date < llsrv_beg,
    'D.1' = date >= llsrv_beg & date < fishery_beg | is.na(date),
    'fishery_D' = date >= fishery_beg & date <= fishery_end,
    'postfishery_D' = date > fishery_end,
    .method = "unique")) %>% 
  group_by(year, D) %>% 
  dplyr::summarize(n = n_distinct(tag_no)) %>% 
  ungroup() %>% 
  reshape2::dcast(year ~ D, value.var = "n", fill = 0) %>% 
  left_join(tag_summary, by = "year") %>% 
  # Add the two D.1 variables together (D.1.x are recaptures from longline
  # survey, D.1.y are recaptures from other fisheries that occurred between the
  # beginning of longline survey and day before start of fishery
  mutate(D.1 = D.1.x + D.1.y) %>% 
  select(-D.1.x, -D.1.y) -> tag_summary_clean

# Currently n.1 and k.1 reflect observed and marked in the longline survey. Add
# similar columns for the fishery
clean_daily_marks %>% 
  group_by(year) %>% 
  dplyr::summarize(k_fishery = sum(total_marked),
                   n_fishery = sum(total_obs),
                   D_fishery = sum(fish_D_ch)) %>% #Changed from fishery_D to this... see above
  left_join(tag_summary_clean) -> tag_summary_clean

view(tag_summary_clean)

# Cumulation curves of marks collected and catch over the season
ggplot(clean_daily_marks, aes(x = julian_day)) +
  geom_line(aes(y = cum_marks)) +
  geom_line(aes(y = clean_cum_marks)) +
  facet_wrap(~ year, scales = "free") +
  labs(x = "Julian Day", y = "Cumulative Marks Collected")

ggplot(daily_marks, aes(x = julian_day)) +
  geom_line(aes(y = cum_whole_kg)) +
  facet_wrap(~ year, scales = "free") +
  labs(x = "Julian Day", y = "Cumulative Catch (kg)")

# Trends in mean weight and NPUE over the season - trends provide
# justification to doing a time-stratified estimator
ggplot(daily_marks,
       aes(x = julian_day, y = mean_weight)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~ year, dir = "v") +
  labs(x = "Julian Day", y = "Mean Individual Weight (kg)")

ggplot(daily_marks,
       aes(x = julian_day, y = mean_npue)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~ year, scales = "free", ncol = 2, dir = "v") +
  labs(x = "Julian Day", y = "Number sablefish per 1000 hooks\n")

#======================================================================
#*** If doing PJ diagnostics here is where you go to "old school diagnostics" below
# will be inserted in here eventually... PJ March 22
# Good code below for dealing with length/size bias
# Still developing spatial exam - not sure how to Darroch this...
# But code set up for temporal vairability already. (may provide structure for spatial question?)  
#===============================================================================================================
#model prep

tag_summary<-tag_summary_clean

#PJ goals with this:
#1) Use marked: and unmarked from full countback data... get rid of biomass conversion to C
#   All good: Need biomass C for model, but marked:unmarked ratio is using counts
#   Still good correction to just apply full countbacks and censure incomplete ones...
#2) Run this for each year separately and examine differences
#3) Insert ability for length stratification using biological data to partition C's, M/K's and n's...
#   (this will take further evaluation of length data by sampling period)
#   this will involve reworking the code from the top to partition marks and recaps 
#   based on lengths.  Reinsert diagnostics at top to id stratification points
#4) Come up with way to deal with potential spatial heterogeneity in capture prob
#   Similarly to length, diagnostics will need to be before all this code.  Data will need
#   to be reorganized both to diagnose and then structure data for that.  Instead of
#   just time stratification (which is done below), will need to organize tag_summary
#   by day AND spatial unit: 
#      -different row for each marking location?
#      -different row for each recapture location?
#      -different row for each combo of mark and recapture location? (getting pretty strat here...)
#      -still haven't figured out how to extract data for diagnostics: run different models
#       and compare fit? 

# Stratify by time ----

# create empty lists to hold the iterations of output from mr_jags fxn. have to
# do model posteriors separately because they estimate different parameters

mod1_posterior_ls <- list()
mod2_posterior_ls <- list()
mod3_posterior_ls <- list()
mod4_posterior_ls <- list()

N_summary_ls <- list()
dic_ls <- list()
converge_ls <- list()

data_ls <- list() # store all 'versions' of model input data for comparison
data_df <- list() 

# Number of models - currently 4 competing models to the simple Chapman: 
# 1) model 1 -  Peterson with time strata, accounts for natural mortality and catch
# 2) model 1 + migration parameter
# 3) model 1 + catchability parameter (incorportates NPUE data)
# 4) model 2 + model 3

#PJ 2022 notes; batch codes so no length data to stratify by fish size
# no exam of variable capture probabilites by area... looks like the data is available for that analysis?

# Number of period (time strata) combos to test
strats <- 8

#Prepare progress bar
pb <- txtProgressBar(min = 0, max = strats, style = 3)

#============================================================================================
# Modelling loop

for(j in 1:strats) {
  
  # j = 8
  #j=3
  # Base strata on percentiles of cumulative catch. Could also use number of marks
  # observed or some other variable. STRATA_NUM is the dynamic variable specifying
  # the number of time strata to split the fishery into and it currently
  # accomodates 8 or fewer strata.
  
  STRATA_NUM <- j
  
  clean_daily_marks %>% 
    group_by(year) %>% 
    mutate(catch_strata = cut(percent_rank(cum_whole_kg) %>% round(2),
                              breaks = seq(0, 1, by = 1 / STRATA_NUM),
                              include.lowest = TRUE, right = TRUE, ordered_result = TRUE,
                              labels = paste(seq(2, STRATA_NUM + 1, by = 1)))) -> clean_daily_marks
  
  # Summarize by strata
  #PJ note 2022: calculating C (number of fish examined for marks) from biomass of harvest? 
  clean_daily_marks %>% #filter(year == YEAR) %>% 
    group_by(year, catch_strata) %>% 
    dplyr::summarize(t = n_distinct(date),
                     catch_kg = sum(whole_kg) %>% round(1),
                     n = sum(clean_total_obs),  #hmmm - here's this number... change to clean_? 
                     k = sum(clean_total_marked),
                     D = sum(fish_D_ch),  #change to our new on fisher_D to fish_D_ch
                     #C = sum(),
                     NPUE = mean(mean_npue, na.rm = TRUE) %>% round(1),
                     # Use the interpolated mean weight here so it doesn't break at large
                     # STRATA_NUMs
                     mean_weight = mean(interp_mean, na.rm = TRUE)) %>% 
    mutate(C = (catch_kg / mean_weight) %>% round(0)) -> strata_sum   #!!FLAG!! conversion from biomass to C
  #  view(strata_sum)
  #***PJ22? Need to figure out how C(based on biomass) and n (count of observations) are used in model?
  
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
  #PJ 2022 note: could this be carrying forward past errors or poor analysis? 
  
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
  
  for(i in 1:length(model_years)){    # i<-1  #get rid of this loop so just looking at each year separately
    
    sub <- jags_dat %>% filter(year == model_years[i])
    
    sub_dat <-
      list(P = STRATA_NUM + 1, # number of time Periods
           #!! do an "S" for size strata
           #!! do a "G" for spatial strata
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
  
  # Initial values ----
  
  # Franz had initial values, but you don't need them (JAGS initializes chains
  # from a central value from the population, the mean of which is defined from
  # past assessments)
  
  # # Create an empty list to store JAGS inital values (we want a list of lists)
  # inits <- vector('list', length(model_years))
  # 
  # for(i in 1:length(model_years)){
  #   for(j in 1:length(model_years)){
  #     
  #     sub <- abd_est %>% filter(year == model_years[i])
  #     
  #     sub_dat <-
  #       list(init.N = select(sub, mu.N) %>% as.numeric())
  #     
  #     inits[[j]] <- sub_dat
  #     rm(sub_dat)
  #     
  #   }
  # }
  
  # Prior on p ----
  
  # Visualize choice of prior for p, the probabilty of catching a marked
  # sabelfish.
  
  # https://stats.stackexchange.com/questions/58564/help-me-understand-bayesian-prior-and-posterior-distributions/58792#58792
  
  # beta prior for plotting
  # prior <- function(m, n_eq){
  #   a <- n_eq * m
  #   b <- n_eq * (1 - m)
  #   dom <- seq(0, 1, 0.0001)
  #   val <- dbeta(dom, a, b)
  #   return(data.frame('x' = dom,
  #                     'y' = val,
  #                     'n_eq'= rep(n_eq, length(dom))))
  # }
  #
  # m <- 0.0031 # The K/N in 2017
  #
  # bind_rows(prior(m, n_eq = 10000),
  #           prior(m, n_eq = 5000),
  #           prior(m, n_eq = 1000),
  #           prior(m, n_eq = 500),
  #           prior(m, n_eq = 100),
  #           prior(m, n_eq = 10)) %>%
  # ggplot(aes(x, y, col = factor(n_eq))) +
  #   geom_line(size = 1) +
  #   geom_vline(aes(xintercept = m), lty = 2) +
  #   theme_bw() +
  #   xlim(c(0, 0.010)) +
  #   ylab(expression(paste('p(',theta,')', sep = ''))) +
  #   xlab(expression(theta))
  
  # prior_mean <- function(m, n_eq){
  #   a <- n_eq * m
  #   b <- n_eq * (1 - m)
  #   mean = a / (a + b)
  #   var = (a * b) / ((a + b)^2 * (a + b + 1))
  #   return(data.frame('mean' = mean,
  #                     'var' = var,
  #                     'n_eq'= n_eq))
  # }
  #
  # # All the priors have the same mean, variance increases with decreasing n_eq
  # bind_rows(prior_mean(m, n_eq = 10000),
  #           prior_mean(m, n_eq = 5000),
  #           prior_mean(m, n_eq = 1000),
  #           prior_mean(m, n_eq = 500),
  #           prior_mean(m, n_eq = 100),
  #           prior_mean(m, n_eq = 10)) %>% kable()
  
  # *FLAG* I am most comfortable with an neq > 5000, which keeps the range of p within
  # the same order of magnitude as M/N. If we decrease neq the point estimate on
  # the population decreases dramatically. We ultimately selected 10000.
  
  # Model 1 ----
  
  # Time-stratified mark-recapture model with natural mortality includes all
  # clipped fish recaptured in longline survey data and fishery data
  
  # Informed prior option = 10000
  # Uninformed prior = 100
  
  mod1 <- "
model {
# Priors
N.1 ~ dnorm(mu.N,1.0E-12) #I(0,)	# number of sablefish in Chatham at beginning of period 1

N[1] <- N.1 * exp(-M * t[1])
K[1] <- (K.0 - D.0) * exp(-M * t[1]) # number of marks at beginning of period 1 (longline survey)
# K.0 = Number of tags released
# D = Number of tags lost to fishery or longline survey
# M = natural mortality (daily instantaneous mortality)

for(i in 2:P) {    #for each period... calculates abundance
                   #then mean N at end
                   #should be able to add extra loops for size strata
                   #and spatial strata.
                   #but instead of average, we would add size and spatial strata
                   #for each period and then average across
K[i] <- (K[i-1] - k[i-1] - D[i-1]) * exp(-M * t[i])		# Number of marks at beginning of period i
N[i] <- (N[i-1] - C[i-1]) * exp(-M * t[i])		# Total number of sablefish at beginning of period i
#*** PJ lightbulb!!: need biomass C for the model... not part of Marked:Unmarked ratio!!!! (Duh)

}

for(i in 1:P) {

# Use a weakly informative beta prior on p. Note that x (K/N) doesn't change
# much through time b/c the population numbers are large, though we want to
# allow p to change through time due to changes in CPUE and mean size

x[i] <- K[i] / N[i]	 # probability that a caught sablefish is clipped (x = nominal p)

# Generate a prior for p, informed by x. A large multiplier indicates our
# confidence in x

a[i] <- x[i] * 10000  # the alpha parameter in the beta distribution, used as a prior for p
b[i] <- (1 - x[i]) * 10000  # the beta paramter in the beta distribution
p[i] ~ dbeta(a[i],b[i]) # beta prior for p, the probability that a caught sablefish is clipped

k[i] ~ dbin(p[i], n[i])	 # Number of clipped fish ~ binomial(p, n)  !! marked:unmarked ratio!! 
}

N[P+1] <- N[P] - C[P] # account for remaining catch by adding a final period

# Compute quantities of interest:
N.avg <- mean(N[])
}
"

# Model 2 ----

# Same as Model 1 but now estimates immigration (b) *FLAG* the normal prior on
# the immigration parameter, r,  allows immigration to be negative, and the
# model estimates negative values in two years over the time series (2008 and
# 2017). This can be fixed with a uniform prior, but I kind of like the idea of
# a migration parameter that accounts for net movement in/out of Chatham,
# without relying on natural mortality to absorb emigration.

mod2 <- "
model {
# Priors
N.1 ~ dnorm(mu.N,1.0E-12) #I(0,)	# number of sablefish in Chatham at beginning of period 1
r ~ dnorm(5000, 1.0E-12) # vague prior on number of immigrants (r)

N[1] <- N.1 * exp(-M * t[1])
K[1] <- (K.0 - D.0) * exp(-M * t[1]) # number of marks at beginning of period 1 (longline survey)
# K.0 = Number of tags released
# D = Number of tags lost to fishery or longline survey
# M = natural mortality (daily instantaneous mortality)

for(i in 2:P) {
K[i] <- (K[i-1] - k[i-1] - D[i-1]) * exp(-M * t[i])		# Number of marks at beginning of period i
N[i] <- (N[i-1] - C[i-1]) * exp(-M * t[i]) + r*t[i]		# Total number of sablefish at beginning of period i, including immigration
}

for(i in 1:P) {

# Use a weakly informative beta prior on p. Note that x (K/N) doesn't change
# much through time b/c the population numbers are large, though we want to
# allow p to change through time due to changes in CPUE and mean size

x[i] <- K[i] / N[i]	 # probability that a caught sablefish is clipped (x = nominal p)

# Generate a prior for p, informed by x. A large multiplier indicates our
# confidence in x

a[i] <- x[i] * 10000  # the alpha parameter in the beta distribution, used as a prior for p
b[i] <- (1 - x[i]) * 10000  # the beta paramter in the beta distribution
p[i] ~ dbeta(a[i],b[i]) # beta prior for p, the probability that a caught sablefish is clipped

k[i] ~ dbin(p[i], n[i])	 # Number of clipped fish ~ binomial(p, n)
}

N[P+1] <- N[P] - C[P] # account for remaining catch by adding a final period

# Compute quantities of interest:
N.avg <- mean(N[])
}
"

# Model 3 ----

# Same as Model 1, but now estimates incorporates NPUE data and estimates catchability (q)

mod3 <- "
model {
# Priors
N.1 ~ dnorm(mu.N,1.0E-12) #I(0,)	# number of sablefish in Chatham at beginning of period 1
# q ~ dnorm(0.00035, 0.1)I(0,) # catchability coefficient: NPUE = q*N # Franz's prior
q ~ dbeta(1,1) # catchability coefficient: NPUE = q*N
tau ~ dgamma(0.001, 1) #  tau = 1/sigma^2 for normal distribution of CPUE

N[1] <- N.1 * exp(-M * t[1])
K[1] <- (K.0 - D.0) * exp(-M * t[1]) # number of marks at beginning of period 1 (longline survey)
# K.0 = Number of tags released
# D = Number of tags lost to fishery or longline survey
# M = natural mortality (daily instantaneous mortality)

for(i in 2:P) {
K[i] <- (K[i-1] - k[i-1] - D[i-1]) * exp(-M * t[i])		# Number of marks at beginning of period i
N[i] <- (N[i-1] - C[i-1]) * exp(-M * t[i])		# Total number of sablefish at beginning of period i
NPUE[i-1] ~ dnorm(npue.hat[i], tau)    # NPUE ~ normal(mean, tau)
npue.hat[i] <- q * N[i]     # predicted NPUE    
}

for(i in 1:P) {

# Use a weakly informative beta prior on p. Note that x (K/N) doesn't change
# much through time b/c the population numbers are large, though we want to
# allow p to change through time due to changes in CPUE and mean size

x[i] <- K[i] / N[i]	 # probability that a caught sablefish is clipped (x = nominal p)

# Generate a prior for p, informed by x. A large multiplier indicates our
# confidence in x

a[i] <- x[i] * 10000  # the alpha parameter in the beta distribution, used as a prior for p
b[i] <- (1 - x[i]) * 10000  # the beta paramter in the beta distribution
p[i] ~ dbeta(a[i],b[i]) # beta prior for p, the probability that a caught sablefish is clipped

k[i] ~ dbin(p[i], n[i])	 # Number of clipped fish ~ binomial(p, n)
}

N[P+1] <- N[P] - C[P] # account for remaining catch by adding a final period

# Compute quantities of interest:
N.avg <- mean(N[])
sigma <- 1/sqrt(tau)

}
"

# Model 4 ----

# A combination of Models 1-3, estimating migration and catchability.

mod4 <- "
model {
# Priors
N.1 ~ dnorm(mu.N,1.0E-12) #I(0,)	# number of sablefish in Chatham at beginning of period 1
r ~ dnorm(5000, 1.0E-12) # vague prior on number of migrants (r)
# q ~ dnorm(0.00035, 0.1)I(0,) # catchability coefficient: NPUE = q*N # Franz's prior
q ~ dbeta(1,1) # catchability coefficient: NPUE = q*N
tau ~ dgamma(0.001, 1) #  tau = 1/sigma^2 for normal distribution of CPUE

N[1] <- N.1 * exp(-M * t[1])
K[1] <- (K.0 - D.0) * exp(-M * t[1]) # number of marks at beginning of period 1 (longline survey)
# K.0 = Number of tags released
# D = Number of tags lost to fishery or longline survey
# M = natural mortality (daily instantaneous mortality)

for(i in 2:P) {
K[i] <- (K[i-1] - k[i-1] - D[i-1]) * exp(-M * t[i])		# Number of marks at beginning of period i
N[i] <- (N[i-1] - C[i-1]) * exp(-M * t[i])	+ r*t[i]	# Total number of sablefish at beginning of period i
NPUE[i-1] ~ dnorm(npue.hat[i], tau)    # NPUE ~ normal(mean, tau)
npue.hat[i] <- q * N[i]     # predicted NPUE    
}

for(i in 1:P) {

# Use a weakly informative beta prior on p. Note that x (K/N) doesn't change
# much through time b/c the population numbers are large, though we want to
# allow p to change through time due to changes in CPUE and mean size

x[i] <- K[i] / N[i]	 # probability that a caught sablefish is clipped (x = nominal p)

# Generate a prior for p, informed by x. A large multiplier indicates our
# confidence in x

a[i] <- x[i] * 10000  # the alpha parameter in the beta distribution, used as a prior for p
b[i] <- (1 - x[i]) * 10000  # the beta parameter in the beta distribution
p[i] ~ dbeta(a[i],b[i]) # beta prior for p, the probability that a caught sablefish is clipped

k[i] ~ dbin(p[i], n[i])	 # Number of clipped fish ~ binomial(p, n)
}

N[P+1] <- N[P] - C[P] # account for remaining catch by adding a final period

# Compute quantities of interest:
N.avg <- mean(N[])
sigma <- 1/sqrt(tau)

}
"

# Run models ----

mod1_out <- mr_jags(mod = mod1, mod_name = "Model1", 
                    model_dat = model_dat, model_years = model_years, 
                    mpar = c("N.avg", "N", "p"),
                    miter = 50000)

# Add a new column for the number of time period strata
mod1_out$results$P <- j + 1
mod1_out$N_summary$P <- j + 1
mod1_out$dic$P <- j + 1
mod1_out$convergence_diagnostic$P <- j + 1

# Repeat for model 2
mod2_out <- mr_jags(mod = mod2, mod_name = "Model2",
                    model_dat = model_dat, model_years = model_years, 
                    mpar = c("N.avg", "N", "p", "r"),
                    miter = 50000)

mod2_out$results$P <- j + 1
mod2_out$N_summary$P <- j + 1
mod2_out$dic$P <- j + 1
mod2_out$convergence_diagnostic$P <- j + 1

# Repeat for model 3
mod3_out <- mr_jags(mod = mod3, mod_name = "Model3",
                    model_dat = model_dat, model_years = model_years, 
                    mpar = c("N.avg", "N", "p", "q", "npue.hat", "sigma"),
                    miter = 50000)

mod3_out$results$P <- j + 1
mod3_out$N_summary$P <- j + 1
mod3_out$dic$P <- j + 1
mod3_out$convergence_diagnostic$P <- j + 1

# Repeat for model 4
mod4_out <- mr_jags(mod = mod4, mod_name = "Model4",
                    model_dat = model_dat, model_years = model_years, 
                    mpar = c("N.avg", "N", "p", "r", "q", "npue.hat", "sigma"),
                    miter = 50000)

mod4_out$results$P <- j + 1
mod4_out$N_summary$P <- j + 1
mod4_out$dic$P <- j + 1
mod4_out$convergence_diagnostic$P <- j + 1

# combine and then break apart results to put them in n_summary_ls, dic_ls, and
# converge_ls lists. Can do these easily because all models have the same
# dimensions

N_summary_ls[[j]] <- rbind(mod1_out$N_summary,
                           mod2_out$N_summary,
                           mod3_out$N_summary,
                           mod4_out$N_summary) 

dic_ls[[j]] <- rbind(mod1_out$dic,
                     mod2_out$dic,
                     mod3_out$dic,
                     mod4_out$dic) 

converge_ls[[j]] <- rbind(mod1_out$convergence_diagnostic,
                          mod2_out$convergence_diagnostic,
                          mod3_out$convergence_diagnostic,
                          mod4_out$convergence_diagnostic)

mod1_posterior_ls[[j]] <- mod1_out$results
mod2_posterior_ls[[j]] <- mod2_out$results
mod3_posterior_ls[[j]] <- mod3_out$results
mod4_posterior_ls[[j]] <- mod4_out$results

data_ls[[j]] <- model_dat # save the model inputs for comparison with predicted values 
data_df[[j]] <- strata_sum # same data but in a df, more useable

setTxtProgressBar(pb, j)  

}
# end model loop
#======================================================================================


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
# load everything and then start here ~line639 in MR code
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
