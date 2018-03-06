# Mark-recapture analysis
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2018-02-13

# ** currently missing daily tag accounting for 2003, 2017 will need to be updated when finalized by M. Vaughn.

source("r_code/helper.r")
source("r_code/functions.r")
library(zoo) # interpolate values
library(rjags) # run jags/bugs models
library(tidyr)
library(knitr)

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

# *FLAG* don't add these landings back in right now, otherwise the population
# assessment is way off. Wait until I have a chance to work with M. Vaughn.
# # See if these landings have fish tickets. These 7 trips from 2008 are in the IFDB and look like they were missed. I'm
# # choosing to add them into the marks df, because they are from the NSEI state
# # managed fishery and appear not to have been accounted for in the countbacks
# # spreadsheets.
# fsh_tx %>% 
#   filter(year_trip %in% no_match$year_trip) %>% 
#   group_by(year, trip_no, year_trip, sell_date, julian_day) %>% 
#   summarise(whole_kg = sum(whole_kg)) %>% 
#   rename(date = sell_date) %>% 
#   left_join(no_match, by = "year_trip") %>% 
#   select(-Mgmt_area) %>% 
#   full_join(marks, by = c("year", "trip_no", "year_trip", "date", "julian_day",
#                           "whole_kg", "tags_from_fishery")) %>% 
#   arrange(date)  %>% 
#   fill_by_value(unmarked, marked, bio_samples, total_obs, value = 0) %>% 
#   fill_by_value(observed_flag, all_observed, value = "No") -> marks

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
    .method = "unique")) %>% 
  group_by(year, D) %>% 
  summarise(n = n_distinct(tag_no)) %>% 
  ungroup() %>% 
  dcast(year ~ D, value.var = "n", fill = 0) %>% 
  left_join(tag_summary, by = "year") -> tag_summary

# Trends ----

# Cumulation curves of marks collected and catch over the season
# 
# ggplot(daily_marks, aes(x = julian_day)) +
#   geom_line(aes(y = cum_marks)) +
#   facet_wrap(~ year, scales = "free") +
#   labs(x = "Julian Day", y = "Cumulative Marks Collected")
# 
# ggplot(daily_marks, aes(x = julian_day)) +
#   geom_line(aes(y = cum_whole_kg)) +
#   facet_wrap(~ year, scales = "free") +
#   labs(x = "Julian Day", y = "Cumulative Catch (kg)")
# 
# # Trends in mean weight and NPUE over the season - trends provide
# # justification to doing a time-stratified estimator
# 
# ggplot(daily_marks,
#        aes(x = julian_day, y = mean_weight)) +
#   geom_point() +
#   geom_smooth(method = 'lm') +
#   facet_wrap(~ year) +
#   labs(x = "Julian Day", y = "Mean Individual Weight (kg)")
# 
# ggplot(daily_marks,
#        aes(x = julian_day, y = mean_npue)) +
#   geom_point() +
#   geom_smooth(method = 'lm') +
#   facet_wrap(~ year, scales = "free") +
#   labs(x = "Julian Day", y = "Number sablefish per 1000 hooks")

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
  
  model_dat[[i]] <- sub_dat
  rm(sub_dat)
  
}

# Initial values ----

# Franz had initial values, but you don't need them (JAGS initialized chains
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

# Visualize choice of prior for p, the probabilty of catching a marked sabelfish.

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
# m <- 0.0031 # The M/N in 2017
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
# the population decreases dramatically.

# Model 1 ----

# Create an empty list to store model output 
model_output <- vector('list', 1)

for(i in 1:length(model_years)){

dat <- model_dat[[i]]

# Time-stratified mark-recapture model with natural mortality and immigration
# includes all clipped fish recaptured in longline survey data and fishery data

# Informed prior option = 10000
# Uninformed prior = 100
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

    # Use a weakly informative beta prior on p. Note that x (M/N) doesn't change
    # much through time b/c the population numbers are large, though we want to
    # allow p to change through time due to changes in CPUE and mean size

    x[i] <- M[i] / N[i]	 # probability that a caught sablefish is clipped (x = nominal p)
    
    # Generate a prior for p, informed by x. A large multiplier indicates our
    # confidence in x

    a[i] <- x[i] * 10000  # the alpha parameter in the beta distribution, used as a prior for p
    b[i] <- (1 - x[i]) * 10000  # the beta paramter in the beta distribution
    p[i] ~ dbeta(a[i],b[i]) # beta prior for p, the probability that a caught sablefish is clipped
    
    m[i] ~ dbin(p[i], n[i])	 # Number of clipped fish ~ binomial(p, n)
    }
    
    N[P+1] <- N[P] - C[P] # account for remaining catch by adding a final period

    # Compute quantities of interest:
    N.avg <- mean(N[])
    }
    ", file = paste0("m1_", model_years[i], ".jag"))

# initialize and run model
m1 <- jags.model(paste0("m1_", model_years[i], ".jag"),
                 data = dat,
                 n.chains = 4,
                 # init = tst_inits,
                 n.adapt = 1000)

# Sample poterior distribution of 'mpar' variables
mpar <- c("N.avg", "N", "p")

res <- coda.samples(m1,
                    var = mpar,
                    n.iter = 10000,
                    thin = 1)

coda_df(res) %>% 
  mutate(year = model_years[i]) -> coda_res

#Append resuts 
if(i == 1){
  coda_res_out <- coda_res
  rm(coda_res)
} else {
  coda_res_out <- rbind(coda_res_out, coda_res) }

# Diagnostic trace plots - these were tested individually by year. These
# models are well mixed and have no issues with convergence.

# plot(res, col = 2)

# Get DIC 
# *FLAG* - update this section once competing models are complete.
# https://www4.stat.ncsu.edu/~reich/st590/code/DICpois

# Convergance diagnostic: gelman.diag gives you the scale reduction factors for
# each parameter. A factor of 1 means that between variance and within chain
# variance are equal, larger values mean that there is still a notable
# difference between chains. General rule: everything below 1.1 or so
# is ok.
# https://theoreticalecology.wordpress.com/2011/12/09/mcmc-chain-analysis-and-convergence-diagnostics-with-coda-in-r/
gelman.diag(res, multivariate = FALSE)[[1]] %>%
  data.frame() %>% 
  mutate(year = model_years[i]) -> convergence

model_output <- list("results" = coda_res_out,
                     "convergence_diagnostic" = convergence)
}

# Model 1 Results ----

results <- model_output$results

# Credibility intervals N and p, N by time period
results %>% 
  gather("time_period", "N.avg", contains("N.avg")) %>% 
  group_by(year, time_period) %>% 
  summarise(`Current estimate` = median(N.avg),
            q025 = quantile(N.avg, 0.025),
            q975 = quantile(N.avg, 0.975)) %>% 
  arrange(year, time_period) %>% 
  left_join(assessment_summary %>% 
              select(year, `Previous estimate` = abundance_age2plus) %>% 
              mutate(`Previous estimate` = ifelse(year == 2017, NA, `Previous estimate`)), 
            by = "year") %>% 
  ungroup() %>% 
  mutate(year = as.Date(as.character(year), format = "%Y")) %>% 
  pad(interval = "year") %>% 
  mutate(year = year(year),
         Year = factor(year)) %>%
  gather("Abundance", "N", `Previous estimate`, `Current estimate`) %>% 
  mutate(N = N / 1000000,
         # interpolate the CI in missing years for plotting purposes
         q025 = zoo::na.approx(q025 / 1000000, maxgap = 20, rule = 2),
         q975 = zoo::na.approx(q975 / 1000000, maxgap = 20, rule = 2)) %>% 
  ggplot() +
  geom_point(aes(x = year, y = N, col = Abundance, shape = Abundance), 
             size = 3) +
  geom_smooth(aes(x = year, y = N, col = Abundance), 
              se = FALSE) +
  geom_ribbon(aes(x = year, ymin = q025, ymax = q975), 
              alpha = 0.2, fill = "#F8766D") +
  scale_x_continuous(breaks = seq(min(model_years), max(model_years), 2), 
                     labels = seq(min(model_years), max(model_years), 2)) +
  geom_point(data = assessment_summary %>% 
               filter(year %in% c(2017)) %>% 
               mutate(est = abundance_age2plus / 1000000),
             aes(x = year, y = est), 
             shape = 8, size = 3, colour = "darkcyan") +
  ylim(c(1, 3.5)) +
  labs(x = "", y = "Number of sablefish (millions)\n",
       colour = NULL, shape = NULL) +
  theme(legend.position = c(.8, .8))


ggsave(paste0("figures/model1_N_retrospective_", 
              FIRST_YEAR, "_", YEAR, ".png"), 
       dpi=300, height=4, width=6, units="in")


# results %>%
#   gather("time_period", "p", contains("p[")) %>% 
#   group_by(year, time_period) %>% 
#   summarise(median = median(p),
#             q025 = quantile(p, 0.025),
#             q975 = quantile(p, 0.975)) 

# results %>%
#   gather("time_period", "N", contains("N[")) %>%
#   group_by(year, time_period) %>%
#   summarise(median = median(N),
#             q025 = quantile(N, 0.025),
#             q975 = quantile(N, 0.975)) %>%
#   arrange(year, time_period) %>% View()

# Posterior distributions from the last 4 years with MR data
results %>% 
  filter(year > 2010) %>%
  group_by(year) %>% 
  mutate(N.avg = N.avg / 1000000,
         q025 = quantile(N.avg, 0.025),
         q975 = quantile(N.avg, 0.975),
         ci = ifelse(N.avg >= q025 & N.avg <=q975, 1, 0),
         median = median(N.avg)) %>% 
  ggplot(aes(N.avg)) + 
  geom_histogram(fill = 4, alpha = 0.2, bins = 100, color = 'black') + 
  geom_histogram(data = . %>% filter(ci==1), 
                 aes(N.avg), fill = 4, alpha = 0.6, bins = 100) +
  geom_vline(aes(xintercept = median), col = "red", linetype = 2, size = 1) +
  facet_wrap(~ year) +
  labs(x = "Number of sablefish in millions",
       y = "Posterior distribution")

results %>% 
  group_by(year) %>% 
  summarize(N.avg = mean(N.avg) ,
         q025 = quantile(N.avg, 0.025),
         q975 = quantile(N.avg, 0.975))  %>% 
  left_join(assessment_summary) -> assessment_summary

results %>% 
  group_by(year) %>% 
  summarize(N = mean(N.avg) ,
            q025 = quantile(N.avg, 0.025),
            q975 = quantile(N.avg, 0.975)) -> ci

ci[c(2:4)] <- lapply(ci[,c(2:4)], prettyNum, trim=TRUE, big.mark=",")

ci %>% filter(year == 2017) %>% 
  select(`Estimate N` = N, `Lower CI` = q025, 
         `Upper CI` = q975) %>% 
  kable()

# Forecast/YPR Inputs ----

# All inputs from biological.r

# Call selectivities for males and females - Dana @ NOAA 
# DANGER, WILL ROBINSON! DANGER!
# DANGER:: ADF&G ages are different than NOAA ages
# For selectivity-at-age to be equivalent, you need to scale them!

# Provide this for comparison this year, but this conversion is not necessary.

# Conversion of ADF&G to NOAA age:
# NOAA = 0.9085+0.7105*ADF&G
# int <- 0.9085
# slp <- 0.7105
# tmp <- seq(2,42,by=1)
# age_noaa<-int + (slp * tmp)
# 
# #Check
# age_noaa

# NOAA fishery and survey selectivity coefficients Fishery females, males, then
# survey females, males Manual input from Dana's NMFS spreadsheet - request from
# him

# NOTE THAT THESE ARE *AGE*, NOT *LENGTH*

f50_f <-  3.86
fslp_f <- 2.61
f50_m <-  4.22
fslp_m <- 2.61

s50_f <- 3.75
sslp_f <- 2.21
s50_m <- 3.72
sslp_m <- 2.21

# Selectivity vectors 
age <- 2:42

# f_sel <- 1 / (1 + exp(-fslp_f * (age_noaa - f50_f)))
# m_sel <- 1 / (1 + exp(-fslp_m * (age_noaa - f50_m)))
# sf_sel <- 1 / (1 + exp(-sslp_f * (age_noaa - s50_f)))
# sm_sel <- 1 / (1 + exp(-sslp_m * (age_noaa - s50_m)))

f_sel <- 1 / (1 + exp(-fslp_f * (age - f50_f)))
m_sel <- 1 / (1 + exp(-fslp_m * (age - f50_m)))
sf_sel <- 1 / (1 + exp(-sslp_f * (age - s50_f)))
sm_sel <- 1 / (1 + exp(-sslp_m * (age - s50_m)))

# Input estimate of abundance from previous year last year's full recruitment
# 'F' values, total commercial catch, and M. Clip data, not tag data. This is
# the POOLED estimate of abundance, which is subsequently partitioned into
# sex-specific selectivities below

N_MR_sex <- assessment_summary %>% 
  filter(year == YEAR) %>% 
  select(N.avg) %>% 
  as.numeric()

# M assumed = 0.1
# Selectivities from above
# Read in female SURVEY weight-at-age in order to 
# calculate spawning biomass and F levels
#
# Recall that we then use FISHERY weights-at-age
# to calculate exploitable biomass

# F implemented in previous year fishery. From assessment
F_previous <- 0.0683 #0.0677 in 2016

mort <- 0.1 # natural mortality

# Weight-at-age
read_csv(paste0("output/pred_waa_1997_", YEAR, ".csv"),
         guess_max = 50000) %>% 
  dcast(Source + Sex ~ age, value.var = "weight") -> waa

# Female, survey
wt_s_f <- waa %>% 
  filter(Source == "LL survey" &
           Sex == "Female") %>% 
  select(matches("^[[:digit:]]+$")) %>% 
  as.numeric()

# Male, survey
wt_s_m <- waa %>% 
  filter(Source == "LL survey" &
           Sex == "Male") %>% 
  select(matches("^[[:digit:]]+$")) %>% 
  as.numeric()

#Female, fishery
wt_f_f <- waa %>% 
  filter(Source == "LL fishery" &
           Sex == "Female") %>% 
  select(matches("^[[:digit:]]+$")) %>% 
  as.numeric()

# Male, fishery
wt_f_m <- waa %>% 
  filter(Source == "LL Fishery" &
           Sex == "Male") %>% 
  select(matches("^[[:digit:]]+$")) %>% 
  as.numeric()

# Maturity at age, female survey
read_csv("output/fem_maturityatage_llsrv.csv",
         guess_max = 50000) %>% 
  dcast(. ~ age, value.var = "probability") %>% 
  select(matches("^[[:digit:]]+$")) %>% 
  as.numeric() -> mat_s_f
  
#Check to make sure all have been read in as numeric vectors
length(wt_s_f)
length(wt_s_m)
length(wt_f_f)
length(wt_f_m)
length(mat_s_f)

#Fishing mortality * selectivity. Note that we are using HALF of the previous
#full-recruitment F value due to the estimate of abundance being the MEAN
#abundance in the middle of the commercial fishery

Fm <- F_previous/2 * m_sel
Ff <- F_previous/2 * f_sel

# Multiply N by fishery proportions to estimate *EXPLOITED* 
# numbers-at-age and divide by age-specific selectivity
#
# AS PER MUETER 2010 sablefish ASA report to ADF&G,
# the 'exploited' population refers to the population targeted
# by both gear AND fishing fleet behavior
# 'Exploitable' means those vulnerable solely to gear under
# conditions of random sampling

# Sex ratio in the commercial fishery in YEAR

female_p <- read_csv("output/sexratio_byyear.csv", guess_max = 50000) %>% 
  filter(Source == "LL fishery" & year == YEAR) %>% 
  select(proportion) %>% 
  as.numeric() 

male_p <- 1 - female_p

# Translate mark-recapture abundance into general exploited
# abundance by partitioning mark-recapture abundance into
# fishery age composition and then dividing by selectivity
#
# Note that male abundance at ages 2 and 3 will be MUCH higher
# than female because male selectivity at age for those cohorts
# is much lower than females

# Fishery age comps for YEAR
read_csv("output/agecomps.csv", guess_max = 50000) %>% 
  filter(Source == "LL fishery" & 
           year == YEAR &
           Sex %in% c("Female", "Male")) %>% 
  dcast(Sex ~ age, value.var = "proportion") -> agecomps

f <- filter(agecomps, Sex == "Female") %>% select(-Sex) %>% as.numeric()
m <- filter(agecomps, Sex == "Male") %>% select(-Sex) %>% as.numeric()

AGE <- 2:42
Nm <- 1:41
Nf <- 1:41

for(i in 1:41){
  
  Nm[i] <- (N_MR_sex * male_p * m[i]) / m_sel[i]
  Nf[i] <- (N_MR_sex * female_p * f[i]) / f_sel[i]

}

sum(Nf+Nm) 

# PROPAGATE LAST YEAR'S ESTIMATED ABUNDANCE-AT-AGE USING STANDARD 
# AGE-STRUCTURED EQUATIONS. NOTE: AGE 2 TRANSLATES **WITH** MORTALITY 

N_fp <- 1:41 # THIS IS FOR FEMALE SPAWNING BIOMASS
N_mp <- 1:41 # THIS IS FOR MALES

N_fp[1] <- Nf[1]
N_mp[1] <- Nm[1]

# Ages 3 - 41
for(i in 2:40){
  N_fp[i] <- Nf[i-1] * exp(-(Ff[i-1] + mort))
  N_mp[i] <- Nm[i-1] * exp(-(Fm[i-1] + mort))
}

# Plus class
N_mp[41] <- Nm[40] * exp(-(Fm[40] + mort)) + 
  ((Nm[40] * exp(-(Fm[40] + mort))) * exp(-(Fm[41] + mort)))

N_fp[41] <- Nf[40] * exp(-(Ff[40] + mort)) +
  ((Nf[40] * exp(-(Ff[40] + mort))) * exp(-((Ff[41]) + mort)))


#CHECK
N_sex <- sum(N_fp,N_mp)

N_sex
N_MR_sex

# BEGIN CALCS FOR FORECAST NUMBERS
# KILOGRAMS TO POUNDS = 2.20462

#kilograms to pounds
ktp <- 2.20462

# Spawning Biomass
SB_age_s <- 1:41

SB_age_s <- N_fp * mat_s_f * wt_s_f * ktp

SBs <- sum(SB_age_s)

SBs

# Simulate pop to get F levels ----

# YPR analysis (from yield_per_recruit.r)

# M and F
# Note that Fs = a placeholder to check the N vector
# F is replaced during estimation

F <- 0.0

# Simulated population (females)
# UNFISHED Spawning biomass (F = 0)

#Abundance-at-age
N <- 1:41

N[1] <- 500

for(i in 2:40){
  
  N[i] <- N[i-1] * exp(-((F * f_sel[i-1]) + mort))
  
  }

N[41] <- N[40] * exp(-((F * f_sel[40]) + mort)) + ((N[40] * exp(-((F * f_sel[40]) + mort))) * exp(-((F * f_sel[41]) + mort)))
N
sum(N)

SB_age <- 1:41

SB_age <- N * mat_s_f * wt_s_f * ktp
SB <- sum(SB_age)
SB

# Parameter estimation
SB60 <- 0.6 * SB
SB55 <- 0.55* SB
SB50 <- 0.5 * SB
SB45 <- 0.45* SB
SB40 <- 0.4 * SB
SB35 <- 0.35* SB

# Spawning biomass function to compute values 
SBf <- function(x,SB) {
  
  NS <- 500
  
  for(i in 1:41){
    
    if(i == 1)
      N[i] <- NS
    else
      N[i] <- N[i-1] * exp(-((x * f_sel[i-1]) + mort))
    }
  
  N[41] <- N[40] * exp(-((x * f_sel[40]) + mort)) + ((N[40] * exp(-((x * f_sel[40]) + mort))) * exp(-((x * f_sel[41]) + mort)))
  
  SB_ageS <- N * mat_s_f * wt_s_f * ktp
  
  SBS <- sum(SB_ageS)
  
  (SBS - SB)^2
  
}

fit60 <- optimize(f = SBf, SB = SB60, lower = 0.01, upper = 0.2)
fit55 <- optimize(f = SBf, SB = SB55, lower = 0.01, upper = 0.2)
fit50 <- optimize(f = SBf, SB = SB50, lower = 0.01, upper = 0.2)
fit45 <- optimize(f = SBf, SB = SB45, lower = 0.01, upper = 0.2)
fit40 <- optimize(f = SBf, SB = SB40, lower = 0.01, upper = 0.2)
fit35 <- optimize(f = SBf, SB = SB35, lower = 0.01, upper = 0.2)

Fxx <- c(fit35$minimum, fit40$minimum, fit45$minimum, fit50$minimum, fit55$minimum, fit60$minimum)

Fxx

# FULL RECRUITMENT FISHING MORTALITY USING **SURVEY** WEIGHT-AT-AGE FOR 
# SPAWNING BIOMASS CALCS

F35 <- Fxx[[1]]
F40 <- Fxx[[2]]
F45 <- Fxx[[3]]
F50 <- Fxx[[4]]
F55 <- Fxx[[5]]
F60 <- Fxx[[6]]

# Forecast ----

##QUOTAS FOR VARIOUS F LEVELS

Q35s <- sum((N_fp * wt_f_f * ktp)* ((F35 * f_sel) / (((F35 * f_sel) + mort)) * (1 - exp(-((F35 * f_sel) + mort))))+
             (N_mp * wt_f_m * ktp) * ((F35 * m_sel)/(((F35 * m_sel) + mort)) * (1 - exp(-((F35 * m_sel) + mort)))))

Q40s <- sum((N_fp * wt_f_f * ktp)* ((F40 * f_sel) / (((F40 * f_sel) + mort)) * (1 - exp(-((F40 * f_sel) + mort))))+
             (N_mp * wt_f_m * ktp) * ((F40 * m_sel)/(((F40 * m_sel) + mort)) * (1 - exp(-((F40 * m_sel) + mort)))))

Q45s <- sum((N_fp * wt_f_f * ktp)* ((F45 * f_sel) / (((F45 * f_sel) + mort)) * (1 - exp(-((F45 * f_sel) + mort))))+
             (N_mp * wt_f_m * ktp) * ((F45 * m_sel)/(((F45 * m_sel) + mort)) * (1 - exp(-((F45 * m_sel) + mort)))))

Q50s <- sum((N_fp * wt_f_f * ktp)* ((F50 * f_sel) / (((F50 * f_sel) + mort)) * (1 - exp(-((F50 * f_sel) + mort))))+
             (N_mp * wt_f_m * ktp) * ((F50 * m_sel)/(((F50 * m_sel) + mort)) * (1 - exp(-((F50 * m_sel) + mort)))))

Q55s <- sum((N_fp * wt_f_f * ktp)* ((F55 * f_sel) / (((F55 * f_sel) + mort)) * (1 - exp(-((F55 * f_sel) + mort))))+
             (N_mp * wt_f_m * ktp) * ((F55 * m_sel)/(((F55 * m_sel) + mort)) * (1 - exp(-((F55 * m_sel) + mort)))))

Q60s <- sum((N_fp * wt_f_f * ktp)* ((F60 * f_sel) / (((F60 * f_sel) + mort)) * (1 - exp(-((F60 * f_sel) + mort))))+
             (N_mp * wt_f_m * ktp) * ((F60 * m_sel)/(((F60 * m_sel) + mort)) * (1 - exp(-((F60 * m_sel) + mort)))))

quota_s<-(c(Q35s,Q40s,Q45s,Q50s,Q55s,Q60s))

quota_s

# THIS IS TOTAL EXPLOITED BIOMASS - TOTAL ABUNDANCE PARTITIONED INTO COHORTS * 
# FISHERY WEIGHT * SELECTIVITY (REFER: Q&D bottom page 339)

exp_b <- ktp * sum((N_fp * wt_f_f * f_sel) + (N_mp * wt_f_m * m_sel))
exp_b

exp_n<-sum((N_fp*f_sel)+(N_mp*m_sel))
exp_n

# For retrospective/forecast plot in Forecast summary
fcast <- data.frame(year = 2018, n = exp_n) %>% 
  mutate(`Current estimate` = n) %>% 
  select(- n)

# Bargraph of forecasted numbers at age by sex for presentation
data.frame(age = 2:42, 
           Sex = c(rep("Female", 41), rep("Male", 41)),
           N = c(N_fp * f_sel, N_mp * m_sel)) %>% 
  mutate(N = N / 1000000,
         Age = factor(age)) %>% 
ggplot(aes(Age, N, fill = Sex)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  # position = position_dodge(preserve = "single")) +
  scale_x_discrete(breaks = seq(2, 42, 4), 
                     labels =  seq(2, 42, 4)) +
  labs(x = "\nAge", y = "Abundance\n") +
  theme(legend.position = c(0.9, 0.7))


ggsave(paste0("figures/forecasted_Natage_", YEAR + 1, ".png"), 
       dpi=300, height=3, width=9, units="in")

# Bargraph for presentation
agecomps %>% 
  filter(year == YEAR & Source == "LL fishery" &
           Sex %in% c("Male", "Female")) %>% 
  ggplot(aes(age, proportion, fill = Sex)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  # position = position_dodge(preserve = "single")) +
  scale_x_continuous(breaks = seq(min(agecomps$age), max(agecomps$age), 4), 
                     labels =  seq(min(agecomps$age), max(agecomps$age), 4)) +
  labs(x = "\nAge", y = "Proportion\n") +
  theme(legend.position = c(0.9, 0.7))

ggsave(paste0("figures/agecomp_bargraph_", YEAR, ".png"), 
       dpi=300, height=3, width=9, units="in")

# Forecast summary ----

# Graph with forecast abundance estimate

# Credibility intervals N and p, N by time period
results %>% 
  gather("time_period", "N.avg", contains("N.avg")) %>% 
  group_by(year, time_period) %>% 
  summarise(`Current estimate` = median(N.avg),
            q025 = quantile(N.avg, 0.025),
            q975 = quantile(N.avg, 0.975)) %>% 
  arrange(year, time_period) %>% 
  left_join(assessment_summary %>% 
              select(year, `Previous estimate` = abundance_age2plus) %>% 
              mutate(`Previous estimate` = ifelse(year == 2017, NA, `Previous estimate`)), 
            by = "year") %>% 
  # bind_rows(fcast) %>% 
  ungroup() %>% 
  mutate(year = as.Date(as.character(year), format = "%Y")) %>% 
  pad(interval = "year") %>% 
  mutate(year = year(year),
         Year = factor(year)) %>% 
  gather("Abundance", "N", `Previous estimate`, `Current estimate`) %>% 
  mutate(N = N / 1000000,
         # interpolate the CI in missing years for plotting purposes
         q025 = zoo::na.approx(q025 / 1000000, maxgap = 20, rule = 2),
         q975 = zoo::na.approx(q975 / 1000000, maxgap = 20, rule = 2)) %>% 
  ggplot() +
  geom_point(aes(x = year, y = N, col = Abundance, shape = Abundance), 
             size = 3) +
  geom_smooth(aes(x = year, y = N, col = Abundance), 
              se = FALSE) +
  geom_ribbon(aes(x = year, ymin = q025, ymax = q975), 
              alpha = 0.2, fill = "#F8766D") +
  scale_x_continuous(breaks = seq(min(model_years), max(model_years) + 2, 2), 
                     labels = seq(min(model_years), max(model_years) + 2, 2)) +
  geom_point(data = assessment_summary %>% 
               filter(year %in% c(2017)) %>% 
               mutate(est = abundance_age2plus / 1000000),
             aes(x = year, y = est), 
             shape = 8, size = 3, colour = "darkcyan") +
  geom_point(aes(x = 2018, y = exp_n / 1000000),
             shape = 21, size = 3, colour = "#F8766D", fill = "#F8766D") +
  ylim(c(1, 3.5)) +
  labs(x = "", y = "Number of\nsablefish\n(millions)\n",
       colour = NULL, shape = NULL) +
  theme(legend.position = c(.8, .8),
        axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        legend.text = element_text(size = 14))

ggsave(paste0("figures/model1_N_retrospective_", 
              FIRST_YEAR, "_", YEAR, ".png"), 
       dpi=300, height=4, width=9, units="in")


# Format tables

v2018 <- c(exp_n, exp_b, F50, Q50s, Q45s, Q40s) 

# From 2017 assessment
v2017 <- c(1564409, 13502591, 0.0683, 850113, 1005851, 1189083)

format(round((v2018 - v2017)/v2017 * 100, 1),  nsmall=1) -> perc_change

v2017[c(1, 2, 4, 5, 6)] <- lapply(v2017[c(1, 2, 4, 5, 6)], prettyNum, trim=TRUE, big.mark=",")
v2017[3] <- lapply(v2017[3], format, nsmall=3, digits=3)

v2018[c(1, 2, 4, 5, 6)] <- lapply(v2018[c(1, 2, 4, 5, 6)], prettyNum, trim=TRUE, big.mark=",")
v2018[3] <- lapply(v2018[3], format, nsmall=3, digits=3)

rbind(v2017, v2018, perc_change) %>% data.frame() -> new_df
new_df %>% t() %>% data.frame() %>% 
  mutate(Quantity = c("Forecast exploited abundance",
                      "Forecast exploited biomass",
                      "F ACB = F 50%",
                      "ABC - F50% (round pounds)",
                      "ABC - F45% (round pounds)",
                      "ABC - F40% (round pounds)")) %>%  
  select(Quantity, `2017` = v2017, `2018` = v2018, `Percent Change` = perc_change)  %>%
  kable()
  
