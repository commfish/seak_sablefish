# Mark-recapture analysis
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2018-02-13

# ** currently missing daily tag accounting for 2003, 2017 will need to be updated when finalized by M. Vaugn.

source("r_code/helper.r")
library(padr) # helps pad time series
library(zoo) # interpolate values
library(rjags) # run jags/bugs models
library(tidyr)
library(magrittr) # for extra piping (%$%)

# Variable definitions ----

# N.0 = number of sablefish in Chatham Strait at time of marking
# M.0 = number of marks released
# D.0 = number of marks that are not available to either the LL survey or to the fishery
# D_i = number of tags lost in a time period that should be decremented from the
#       next time period
# i = subscript for time period i, which may refer to the LL survey (i = 1)
#        to one of the fishery time periods based on time of landing
# N_i = number of sablefish in Chatham Strait at the beginning of time period i
# M_i = number of marked sablefish in Chatham Straight at the beginning of time period i
# t_i = total number of days in time period i
# n_i = observed catch (number of marked and unmarked sablefish that were
#       checked for clips, or tags in the LL survey) during period i
# m_i = number of marked fish recovered in period i (tags in the LL survey)

# Summary of marking effort 2003 to present. This could be wrong. I tried to
# pull from the original assessments but the numbers Kray used to estimate past
# years were different from the numbers in those years. I'm instead going to
# pull numbers (total marked, recovered, dead tags) from the database, that way
# it's at least reproducible. I'm only using data >= 2005 because I can't find
# any batch 15 (2004) tag recoveries. It turns out in 2004 ADF&G was experimenting
# with PIT tags, which was not successful. I asked Aaron Baldwin if he'd track
# down the batch 15 recoveries on 2018-02-21. 

# Make this an object for when it changes. Our goal should be at least getting
# back to 2003.
FIRST_YEAR <- 2005 

# Assessment year
YEAR <- 2017

# years without a marking survey
NO_MARK_SRV <- c(2011, 2014, 2016)

# Past assessments ----

# A summary of past estimated abundance and biomass in a given year - I use the
# abundance_age2plus to help inform the prior of the starting abundance (it will
# still be vague). See Priors and starting values section
read_csv("data/chatham_sablefish_abd_index.csv") -> assessment_summary

# Past years of mark-recapture variables, summarized to best of my ability based
# on files on the server. This is just used for comparison purposes.
read_csv("data/fishery/raw_data/mr_variable_summary.csv") -> mr_summary

# Released tags ----

# Released fish. Each year has a unique batch_no.

read_csv(paste0("data/survey/tag_releases_2003_", YEAR, ".csv"), 
         guess_max = 50000) -> releases

releases %>% group_by(discard_status) %>% summarise(n_distinct(tag_no)) # All tagged and released or re-released

# Total number tagged and released by year
releases %>% 
  filter(year >= FIRST_YEAR) %>% 
  group_by(year, tag_batch_no) %>% 
  summarise(M.0 = n_distinct(tag_no),
            potsrv_beg = min(date),
            potsrv_end = max(date),
            # FLAG: Mueter (2007) defined the length of time period 1 (t.1) as
            # the number of days between the middle of the pot survey and the
            # middle of the LL survey, and t.2 as the middle of the LL survey to
            # the end of the first fishing period. We have changed this methodology
            # such that t.1 goes from the middle of the pot survey to the day
            # before the fishery begins. We set it up this way to account for
            # instantaneous natural mortality (and functionally emigration)
            # during this period.
            potsrv_middle = (potsrv_end - potsrv_beg) / 2 + potsrv_beg ) %>% 
  mutate(year_batch = paste0(year, "_", tag_batch_no)) -> tag_summary

# Recovered tags ----

# Recaptured fish. Match up to the daily tag accounting (countback) data in the
# fishery to determine dead tags, number caught in survey, etc.

read_csv(paste0("data/fishery/tag_recoveries_2003_", YEAR, ".csv"), 
         guess_max = 50000) -> recoveries

# project codes: query zprod: "select new_project_code, project_code, project from
# lookup.project_conversion where category_code = 'g'"

# new (Zander) = old (IFDB) = description
# 601 = 01 = Clarence Sablefish LL Survey
# 602 = 02 = Commercial Longline Trip
# 603 = 03 = Chatham Sablefish LL Survey
# 604 = 04 = Commercial Jig Trip
# 605 = 05 = Longline Survey **NMFS survey
# 606 = 06 = Jig Survey
# 607 = 07 = Atypical Sample (unknown gear)
# 608 = 08 = Atypical Longline Sample
# 609 = 09 = Atypical Jig Sample
# 610 = 10 = Clarence Sablefish Pot Survey
# 611 = 11 = Chatham Sablefish Pot Survey
# 612 = 12 = Sitka Harbor Sablefish Survey
# 613 = 13 = Kodiak Trawl Sablefish Survey
# 614 = 14 = 1979 NSEI Crab Survey
# 615 = 15 = IPHC Annual Survey
# 616 = 16 = NMFS Coop Tagging Survey
# 617 = 17 = Commercial Pot Trip
# 618 = 18 = Lingcod Stock Assessment
# 619 = 19 = Black Rockfish Stock Assessment
# 620 = 20 = Commercial Troll
# 621 = 21 = Commercial Halibut Longline
# 622 = 22 = Atypical Trawl Sample
# 623 = 23 = Canadian Commercial Longline
# 624 = 24 = Canadian Commercial Pot
# 625 = 25 = Canadian Commercial Trawl
# 626 = 26 = Canadian Scientific Survey
# 627 = 27 = Subsistence/Personal Use
# 628 = 28 = Sport-caught Sample

# Filter out all recoveries that are not from the current year's tag batch
# (i.e., get rid of recovered tags that were deployed in a past year)
recoveries %>% 
  mutate(year_batch = paste0(year, "_", tag_batch_no),
         year_trip = paste0(year, "_", trip_no),
         # use landing_date (same as the countbacks - see section below), otherwise catch_date
         date = as.Date(ifelse(is.na(landing_date), catch_date, landing_date))) %>% 
  filter(year_batch %in% tag_summary$year_batch) -> recoveries

# There is only one Project_cde NA and it is a personal use trip, should be code
# "27" - change it to appropriate code
recoveries %>% 
  mutate(Project_cde = ifelse(is.na(Project_cde) &
                                grepl("Personal Use", comments), 
                              "27", Project_cde)) -> recoveries

# Add Chatham longline survey recoveries to the summary (survey is time period i = 1)
recoveries %>% 
  filter(Project_cde == "03") %>% 
  group_by(year, tag_batch_no) %>%
  summarise(m.1 = n_distinct(tag_no)) %>% 
  left_join(tag_summary, by = c("year", "tag_batch_no")) -> tag_summary

# Remove these matched tags from the releases df so they don't get double-counted by accident. 
recoveries %>% filter(Project_cde != "03") -> recoveries

# Fish tickets ----

# From IFDB database. These will help us check the countback daily accounting
# sheets and also account for fishing mortality in the LL survey.

read_csv(paste0("data/fishery/nseiharvest_ifdb_1969_", YEAR,".csv"), 
         guess_max = 50000) %>% 
  filter(year >= FIRST_YEAR) %>% 
  mutate(whole_kg = whole_pounds * 0.453592,
         year_trip = paste0(year, "_", trip_no)) -> fsh_tx

# LL survey mean weight ----

read_csv(paste0("data/survey/llsrv_bio_1985_", YEAR,".csv"), 
         guess_max = 50000) %>% 
  filter(year >= FIRST_YEAR & !is.na(weight)) %>% 
  group_by(year) %>% 
  summarise(mean_weight_1 = mean(weight)) -> srv_mean_weights

# LL survey effort ----

# Get start and end dates, total n (number of fish observed for marks), and fishery npue

srv_cpue <- read_csv(paste0("data/survey/llsrv_cpue_1988_", YEAR, ".csv"), 
                     guess_max = 50000) %>% 
  filter(year >= FIRST_YEAR)

srv_cpue %>%
  mutate(
    #standardize hook spacing (Sigler & Lunsford 2001, CJFAS) changes in 
    #hook spacing. pers. comm. with aaron.baldwin@alaska.gov: 1995 & 1996 -
    #118 in; 1997 - 72 in.; 1998 & 1999 - 64; 2000-present - 78". This is
    #different from KVK's code (he assumed 3 m before 1997, 2 m in 1997 and
    #after)
    std_hooks = ifelse(year <= 1996, 2.2 * no_hooks * (1 - exp(-0.57 * (118 * 0.0254))),
                       ifelse(year == 1997, 2.2 * no_hooks * (1 - exp(-0.57 * (72 * 0.0254))),
                              ifelse( year %in% c(1998, 1999), 2.2 * no_hooks * (1 - exp(-0.57 * (64 * 0.0254))),
                                      2.2 * no_hooks * (1 - exp(-0.57 * (78 * 0.0254)))))),
    # std_hooks = ifelse(year < 1997, 2.2 * no_hooks * (1 - exp(-0.57 * 3)),
    #                    2.2 * no_hooks * (1 - exp(-0.57 * 2))),
    # Per Mike Vaughn & Aaron Baldwin, only use discard status "01" for retained,
    # b/c these are the ones that are checked for marks
    sablefish_retained = ifelse(discard_status_cde == "01", hooks_sablefish, 0),
    # number of sablefish/1000 hooks, following Mueter 2007
    std_cpue = sablefish_retained / (std_hooks / 1000)) %>% 
  group_by(year) %>%
  summarise(llsrv_beg = min(date),
            llsrv_end = max(date),
            n.1 = sum(sablefish_retained),
            C.1 = sum(sablefish_retained),
            NPUE.1 = mean(std_cpue) %>% round(1)) %>% 
  left_join(srv_mean_weights, by = "year") %>% 
  # estimate wpue: kg sablefish/1000 hooks, following Mueter 2007  
  mutate(WPUE.1 = (NPUE.1 * mean_weight_1) %>% round(1)) %>% 
  right_join(tag_summary, by = "year") -> tag_summary

# Fishery countbacks ----

# Daily tag accounting data in the fishery, includes catch. Note that many of
# the comments indicate that not all fish were observed, so be careful using
# these to estimate mean weight. Filter out language like missed, Missed, not
# counted, did not observe. There is no difference between dressed and round fish for
# detecting marks, but we shouldn't be using it for estimating weight.

read_csv(paste0("data/fishery/nsei_daily_tag_accounting_2004_", YEAR, ".csv")) -> marks

marks %>% 
  filter(year >= FIRST_YEAR &
    !year %in% NO_MARK_SRV) %>% 
  mutate(all_observed = ifelse(
    !grepl(c("Missing|missing|Missed|missed|eastern|Eastern|not counted|Did not observe|did not observe|dressed|Dressed"), comments) & 
                                 observed_flag == "Yes", "Yes", "No"),
    mean_weight = ifelse(all_observed == "Yes", whole_kg/total_obs, NA),
    year_trip = paste0(year, "_", trip_no)) -> marks

# Fishery mean weight ----

# Biological data to get mean weight to get numbers estimated on unobserved catch. 
read_csv(paste0("data/fishery/fishery_bio_2000_", YEAR,".csv"), 
         guess_max = 50000) %>%
  filter(!is.na(weight)) %>% 
  mutate(date = ymd(as.Date(date, "%m/%d/%Y"))) %>% 
  select(date, trip_no, weight) %>% 
  group_by(date, trip_no) %>% 
  summarise(mean_weight_bios = mean(weight)) -> fsh_bio

# Join the mark sampling with the biological sampling. If random bio samples
# were taken, use those as the mean weight, if not use the estimated mean weight
# from the total weight of the catch and the number of fish sampled
left_join(marks, fsh_bio, by = c("date", "trip_no")) %>% 
  mutate(mean_weight = ifelse(!is.na(mean_weight_bios), mean_weight_bios, mean_weight)) %>% 
  select(-mean_weight_bios) -> marks

# Fishery CPUE ----

# CPUE data - use nominal CPUE for now (it's close to the GAM output)
read_csv(paste0("data/fishery/fishery_cpue_1997_", YEAR,".csv"), 
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
  group_by(year, trip_no) %>% 
  summarize(WPUE = mean(WPUE)) -> fsh_cpue

# Join the mark sampling with the fishery cpue
left_join(marks, fsh_cpue, by = c("year", "trip_no")) -> marks

# Fishery summary:
marks %>% group_by(year) %>% 
  summarise(fishery_beg = min(date),
            fishery_end = max(date)) %>% 
  left_join(tag_summary, by = "year") %>% 
  # See earlier notes on changes in methods from Franz Mueter in the Released
  # tags section
  mutate(t.1 = as.numeric((fishery_beg - 1) - potsrv_middle)) -> tag_summary

# Tags from fishery ----

# Join the recovered trips from a specific year-trip combination to the daily 
# accounting dataframe. This will ultimately go into a D calculation. IMPORTANT
# NOTE from Mike Vaughn 2018-02-22: trip_nos are year and project specific,
# which is why tags recaptured might have the same trip_no as a NSEI fishery
# longline trip in a given year. Canadian-recovered tags get a trip_no if there
# was an otolith collected.

recoveries %>% 
  filter(year_trip %in% marks$year_trip & Project_cde == "02") %>% 
  group_by(year_trip) %>% 
  summarize(tags_from_fishery = n_distinct(tag_no)) %>% 
  right_join(marks, by = "year_trip") -> marks

# Remove these matched tags from the releases df so they don't get double-counted by accident. 
recoveries %>% filter(!c(year_trip %in% marks$year_trip & Project_cde == "02")) -> recoveries

# Data discrepancy FLAG - Some year_trip fishery combos in recoveries df are not
# in marks df (exclude trip_no's 1, 2, and 3, which are the pot and longline
# surveys)
anti_join(recoveries %>% 
            filter(!trip_no %in% c(1, 2, 3) & 
                     !is.na(trip_no) & 
                     Project_cde == "02"), 
          marks, by = "year_trip") %>% 
  group_by(year_trip, Mgmt_area) %>% 
  summarize(tags_from_fishery = n_distinct(tag_no)) -> no_match # 7 trips from 2008, all in the NSEI

# See if these landings have fish tickets. These 7 trips from 2008 are in the IFDB and look like they were missed. I'm
# choosing to add them into the marks df, because they are from the NSEI state
# managed fishery and appear not to have been accounted for in the countbacks
# spreadsheets.
fsh_tx %>% 
  filter(year_trip %in% no_match$year_trip) %>% 
  group_by(year, trip_no, year_trip, sell_date, julian_day) %>% 
  summarise(whole_kg = sum(whole_kg)) %>% 
  rename(date = sell_date) %>% 
  left_join(no_match, by = "year_trip") %>% 
  select(-Mgmt_area) %>% 
  full_join(marks, by = c("year", "trip_no", "year_trip", "date", "julian_day",
                          "whole_kg", "tags_from_fishery")) %>% 
  arrange(date) -> marks

# Remove these matched tags from the releases df so they don't get double-counted by accident. 
recoveries %>% 
  filter(!c(year_trip %in% no_match$year_trip & Project_cde == "02")) -> recoveries

# Pothole - check that all fish ticket trips have been accounted for in the NSEI in other
# years in the daily accounting form... or not. How about next year. This is a mess.
# full_join(fsh_tx %>%             
#             filter(year >= FIRST_YEAR) %>% 
#             distinct(year_trip, whole_kg) %>% 
#             arrange(year_trip) %>% 
#             select(year_trip, whole_kg1 = whole_kg),
#           marks %>% 
#             distinct(year_trip, whole_kg) %>% 
#             arrange(year_trip)) %>% View()

# Daily summary ----

# Get a daily summary of observed, marks, and tag loss during the directed NSEI season.

marks %>% 
  # padr::pad fills in missing dates with NAs, grouping by years.
  pad(group = "year") %>%
  group_by(year, date) %>% 
  summarize(whole_kg = sum(whole_kg),
            total_obs = sum(total_obs),
            total_marked = sum(marked),
            tags_from_fishery = sum(tags_from_fishery),
            mean_weight = mean(mean_weight),
            mean_wpue = mean(WPUE)) %>% 
  # interpolate mean_weight column to get npue from wpue (some trips have wpue
  # data but no bio data)
  mutate(interp_mean = zoo::na.approx(mean_weight, maxgap = 20, rule = 2),
         mean_npue = mean_wpue / interp_mean) %>% 
  # padr::fill_ replaces NAs with 0 for specified cols
  fill_by_value(whole_kg, total_obs, total_marked, tags_from_fishery, value = 0) %>% 
  group_by(year) %>% 
  mutate(cum_whole_kg = cumsum(whole_kg),
         cum_obs = cumsum(total_obs),
         cum_marks = cumsum(total_marked),
         julian_day = yday(date)) -> daily_marks

# Join remaining recovered tags that have a matching data with the daily marks 
# df. These could be from other fisheries in or outside the NSEI or from Canada.
# Get fishery_D, the final count of tags to be decremented for the next time
# period.
recoveries %>% 
  filter(date %in% daily_marks$date) %>% 
  group_by(date) %>% 
  summarize(other_tags = n_distinct(tag_no)) %>% 
  right_join(daily_marks, by = "date") %>% 
  # padr::fill_ replaces NAs with 0 for specified cols
  fill_by_value(other_tags, value = 0) %>% 
  mutate(fishery_D = ifelse(tags_from_fishery - total_marked > 0, 
                            tags_from_fishery - total_marked + other_tags, 
                            other_tags)) -> daily_marks

# Remove these matched tags from the releases df so they don't get double-counted by accident. 
recoveries %>% filter(!c(date %in% daily_marks$date)) -> recoveries

# Remaining tag loss ----

# These are tags from other fisheries, from the directed fishery but without
# trip/date info, or Canada, or IPHCS/NMFS/other surveys.Potential periods of
# remaining tag loss:
#   1) Between start of pot survey and day before start of LL survey (D.0)
#   2) Beginning of longline survey and day before start of fishery (D_1)
#   3) During fishery (fishery_D will become D_i)
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
    .method="unique")) %>% 
  group_by(year, D) %>% 
  summarise(n = n_distinct(tag_no)) %>% 
  ungroup() %>% 
  dcast(year ~ D, value.var = "n", fill = 0) %>% 
  left_join(tag_summary, by = "year") -> tag_summary

# Trends ----

# Cumulation curves of marks collected and catch over the season

ggplot(daily_marks, aes(x = julian_day)) +
  geom_line(aes(y = cum_marks)) +
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
  facet_wrap(~ year) +
  labs(x = "Julian Day", y = "Mean Individual Weight (kg)")

ggplot(daily_marks,
       aes(x = julian_day, y = mean_npue)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~ year, scales = "free") +
  labs(x = "Julian Day", y = "Number sablefish per 1000 hooks")

# Stratify by time ----

# *FLAG* For now base strata on percentiles of cumulative catch. Could also used
# number of marks observed or some other variable. STRATA_NUM is the dynamic
# variable specifying the number of time strata to split the fishery into and it
# currently accomodates 9 or fewer strata.
STRATA_NUM <- 5

daily_marks %>% 
  group_by(year) %>% 
  mutate(catch_strata = cut(percent_rank(cum_whole_kg) %>% round(2),
                            breaks = seq(0, 1, by = 1 / STRATA_NUM),
                            include.lowest = TRUE, right = TRUE, ordered_result = TRUE,
                            labels = paste(seq(2, STRATA_NUM + 1, by = 1)))) -> daily_marks
 
# Summarize by strata
daily_marks %>% 
  group_by(year, catch_strata) %>% 
  summarize(t = n_distinct(date),
            catch_kg = sum(whole_kg) %>% round(1),
            n = sum(total_obs),
            m = sum(total_marked),
            D = sum(fishery_D),
            NPUE = mean(mean_npue, na.rm = TRUE) %>% round(1),
            # Use the interpolated mean weight here so it doesn't break at large
            # STRATA_NUMs
            mean_weight = mean(interp_mean, na.rm = TRUE)) %>% 
  mutate(C = (catch_kg / mean_weight) %>% round(0)) -> strata_sum

# Running Chapman estimator
strata_sum %>% 
  left_join(tag_summary %>% select(year, M.0), by = "year") %>% 
  group_by(year, catch_strata) %>% 
  # This doesn't deduct the period specific D's and is a rough estimator
  mutate(running_Chapman = ((M.0 + 1)*(n + 1) / (m + 1)) - 1) -> strata_sum

# Prep data for JAGS ----  
dcast(setDT(strata_sum), year ~ catch_strata, 
      value.var = c("D", "C", "n", "t","m", "NPUE"), sep = ".") %>% 
  left_join(tag_summary, by = "year") %>% 
  # order the columns alphanumerically - very convenient
  select(year, order(colnames(.))) -> jags_dat

jags_dat %>%
  select(year, M.0, contains("D."), contains("C."), starts_with("n."), 
         starts_with("t."), contains("m."), contains("NPUE."))  -> jags_dat

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
  for(j in 1:length(model_years)){
  
  sub <- jags_dat %>% filter(year == model_years[i])
  
  sub_dat <-
    list(P = STRATA_NUM + 1, # number of time Periods
         mu = 0.1/365, # Daily natural mortality
         mu.N = sub$mu.N, # mean of starting abundance values, from past assessments
         M.0 = sub$M.0,
         D.0 = sub$D.0,
         C = select(sub, contains("C.")) %>% as.numeric() , 
         D = select(sub, contains("D."), -D.0) %>% as.numeric(),
         t = select(sub, contains("t.")) %>% as.numeric(),
         n = select(sub, contains("n.")) %>% as.numeric(),
         m = select(sub, contains("m."), -M.0) %>% as.numeric(),
         NPUE = select(sub, contains("NPUE.")) %>% as.numeric()
         )
  
  model_dat[[j]] <- sub_dat
  rm(sub_dat)
  
  }
}

# Initial values ----

# Create an empty list to store JAGS inital values (we want a list of lists)
inits <- vector('list', length(model_years))

for(i in 1:length(model_years)){
  for(j in 1:length(model_years)){
    
    sub <- abd_est %>% filter(year == model_years[i])
    
    sub_dat <-
      list(init.N = select(sub, mu.N) %>% as.numeric())
    
    inits[[j]] <- sub_dat
    rm(sub_dat)
    
  }
}

# Single year test ----

# Test 2006 data (i = 2 in previous loops)

tst_dat <- model_dat[[2]]
tst_inits <- inits[[2]]

# Model 1 ----

# Time-stratified mark-recapture model with natural mortality and immigration
# includes all clipped fish recaptured in longline survey data and fishery data

# mod = function() {

cat("
    model {

    # Priors
    N.1 ~ dnorm(mu.N,1.0E-12) #I(0,)	# number of sablefish in Chatham at beginning of period 1
    
    N[1] <- N.1
    #M <- M.0 - D.0 # number of remaining marks at beginning of longline survey (period 1)
    M[1] <- M.0 * exp(-mu * t[1]) - D.0	# number of marks at beginning of period 1 (longline survey)
    # M.0 = Number of tags released
    # D = Number of tags lost to fishery or longline survey
    # mu = natural mortality (daily instantaneous mortality)
    
    for(i in 2:P) {
    M[i] <- (M[i-1] - m[i-1] - D[i-1]) * exp(-mu * t[i])		# Number of marks at beginning of period i
    N[i] <- (N[i-1] - C[i-1]) * exp(-mu * t[i])		# Total number of sablefish at beginning of period i
    }

    for(i in 1:P) {
    p[i] <- M[i] / N[i]	 # probability that a caught sablefish is clipped 
    m[i] ~ dbin(p[i], n[i])	 # Number of clipped fish ~ binomial(n, p)
    }
    
    N[P+1] <- N[P] - C[P] # account for remaining catch by adding a final period

    # Compute quantities of interest:
    N.avg <- mean(N[])
    }
    ", file = "m1.jag")

# initialize and run model
m1 <- jags.model("m1.jag",
                 data = tst_dat,
                 n.chains = 2,
                 # init = tst_inits,
                 n.adapt = 1000)

# Sample poterior distribution of 'mpar' variables
mpar <- c("N.avg", "N", "p")

res <- coda.samples(m1,
                    var = mpar,
                    n.iter = 10000,
                    thin = 10)

# plot(res, col = 4) 

coda_df(res) %>% 
  mutate(q025 = quantile(N.avg, 0.025),
         q975 = quantile(N.avg, 0.975),
         ci = ifelse(N.avg >= q025 & N.avg <=q975, 1, 0),
         median = median(N.avg)) %>% 
  ggplot(aes(N.avg)) + 
  geom_histogram(fill = 4, alpha = 0.2, bins = 100, color = 'black') + 
  geom_histogram(data = . %>% filter(ci==1), 
                 aes(N.avg), fill = 4, alpha = 0.6, bins = 100) +
  geom_vline(aes(xintercept = median), col = "red", linetype = 2, size = 1)

head(coda_df(res))

# Credibility intervals for p and
coda_df(res) %>% 
  gather("time_period", "p", contains("p[")) %>% 
  group_by(time_period) %>% 
  summarise(median = median(p),
            q025 = quantile(p, 0.025),
            q975 = quantile(p, 0.975))

coda_df(res) %>% 
  gather("time_period", "N", contains("N[")) %>% 
  group_by(time_period) %>% 
  summarise(median = median(N),
            q025 = quantile(N, 0.025),
            q975 = quantile(N, 0.975)) %>% 
  arrange(time_period)

# Simple Chapmanized Peterson estimator ----

n1 <- 1000 # number of fish caught and marks
n2 <- 2000 # number of fish caught
m2 <- 280 # number of fish with marks

N <- ((n2 + 1)*(n1 + 1)/(m2 + 1))-1
varN <- ((n1 + 1) * (n2 + 1) * (n2 - m2)) / ((m2 + 1)^2 * (m2 + 2))
seN <- sqrt(varN)

# Binomial likelihood estimation of the population abundance

N <- seq(n1, 12000, 100)
likelihood <- exp(lfactorial(n2) - lfactorial(m2) - lfactorial(n2 - m2) + m2 * log(n1 / N) + (n2 - m2) * log(1 - n1 / N))

data.frame(N = N, likelihood = likelihood) %>% 
  filter(likelihood == max(likelihood)) -> maxlike

plot(N, likelihood, type = 'l',
     xlim = c(5000, 9500))
abline(v = maxlike$N, lty = 2)

# Bayesian implementation with a prior on the U interval (unmarked fish numbers)

cat("
    model {
      # likelihood function
      m2 ~ dbin(theta, n1) # marked fish
      u ~ dbin(theta, U) # unmarked fish
      
      # prior distribution
      theta ~ dunif(0.0, 0.6) # capture probabilities
      U_ ~ dunif(500, 180000) # number of unmarked fish
      U = round(U_)
    }
    ", file = "m1.jag")  

dat <- list(m2 = 280, n1 = 1000, u = n2 - m2)

ini <- list(theta = 0.2, U_ = 10000)

m1 <- jags.model("m1.jag",
                 data = dat,
                 n.chains = 2,
                 init = ini,
                 n.adapt = 1000)

mpar <- c("U", "theta")
res <- coda.samples(m1,
                    var = mpar,
                    n.iter = 10000,
                    thin = 10)
plot(res, col = 2)
head(res)

summary(res)

coda_df(res) %>% 
  mutate(q025 = quantile(U, 0.025),
         q975 = quantile(U, 0.975),
         ci = ifelse(U >= q025 & U<=q975, 1, 0)) %>% 
  ggplot(aes(U)) + geom_histogram(fill = 4, alpha = 0.2, bins = 100, color = 'black') + 
  geom_histogram(data = . %>% filter(ci==1), aes(U), fill = 4, alpha = 0.6, bins = 100)



