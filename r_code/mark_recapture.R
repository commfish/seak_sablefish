# Mark-recapture analysis
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2018-02-13

# ** currently missing daily tag accounting for 2003, 2017 will need to be updated when finalized by M. Vaugn.

source("r_code/helper.r")
library(padr) # helps pad time series
library(zoo) # interpolate values
library(rjags)

YEAR <- 2017

# years without a marking survey
NO_MARK_SRV <- c(2011, 2014, 2016)

# data ----

# Summary of marking effort 2003 to present. This could be wrong. I tried to
# pull from the original assessments but the numbers Kray used to estimate past
# years were different from the numbers in those years. I'm instead going to
# pull numbers (total marked, recovered, dead tags) from the database, that way
# it's at least reproducible. I'm only using data >= 2005 because I can't find
# any batch 15 (2004) tag recoveries. It turns out 2004 they were experimenting
# with PIT tags, which was not successful. I asked Aaron Baldwin if he'd track
# down the batch 15 recoveries on 2018-02-21. tra

read_csv("data/fishery/raw_data/mr_variable_summary.csv") -> mr_summary

# Released fish. Each year has a unique batch_no.

read_csv(paste0("data/survey/tag_releases_2003_", YEAR, ".csv"), 
         guess_max = 50000) -> releases

releases %>% group_by(discard_status) %>% summarise(n_distinct(tag_no)) # All tagged and released or re-released

# Total number tagged and released by year
releases %>% 
  filter(year >= 2005) %>% 
  group_by(year, tag_batch_no) %>% 
  summarise(M = n_distinct(tag_no),
            potsrv_beg = min(date),
            potsrv_end = max(date)) %>% 
  mutate(potsrv_len = potsrv_end - potsrv_beg,
         year_batch = paste0(year, "_", tag_batch_no)) -> tag_summary

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
         year_trip = paste0(year, "_", trip_no)) %>% 
  filter(year_batch %in% tag_summary$year_batch) -> recoveries

# Add Chatham longline survey recoveries to the summary
recoveries %>% 
  filter(Project_cde == "03") %>% 
  group_by(year, tag_batch_no) %>%
  summarise(llsrv_m = n_distinct(tag_no)) %>% 
  left_join(tag_summary, by = c("year", "tag_batch_no")) -> tag_summary

# number of tags not associated with a trip number, project code, information source, or a date
length(which(is.na(recoveries$trip_no))) # 258
length(which(is.na(recoveries$Project_cde))) # 1
length(which(is.na(recoveries$info_source))) # 0
length(which(is.na(recoveries$landing_date))) # 397
length(which(is.na(recoveries$catch_date))) # 1263

# recoveries %>% 
#   mutate(Project_cde2 = derivedVariable(
#     # "D" = dead tags: any tag accounted for but not sampled by an ADF&G sampler
#     # (e.g., outside waters, in IFQ fishery, sport fishery, in Canadian waters,
#     # or in the NMFS or IPHC surveys). Also include the one NA as a dead tag
#     "D" = Project_cde %in% c("24", "27", NA, "23", "05", "26", "28", "15") |
#       is.na(Project_cde) |
#       c(Project_cde == "02" &
#   ))
# "02"
# "03"
# "11"
# "17"

# recoveries %>% filter(Project_cde == '02') %>% distinct(Mgmt_area, info_source, measurer_type, comments) %>% View()
# recoveries %>% filter(Project_cde == "05")
# recoveries %>% filter(is.na(Project_cde)) %>% View()

# Fishery recoveries
recoveries %>% 
  # 
  filter(!is.na(trip_no) & !trip_no %in% c(1, 2, 3)) %>% 
  group_by(year, info_source, Project_cde) %>% 
  summarise(n_distinct(tag_no)) %>% 
  View()

recoveries %>% 
  filter(trip_no %in% c(1, 2, 3) &
           Project_cde %in% c("26", "15")) %>% View()

recoveries %>% 
  filter(Project_cde %in% c("02", "17") & is.na(trip_no)) %>% 
  group_by(info_source) %>% summarize(n_distinct(tag_no)) %>% View()
  
# Daily tag accounting data in the fishery, includes catch. Note that many of 
# the comments indicate that not all fish were observed, so be careful using 
# these to estimate mean weight. Filter out language like missed, Missed, not
# counted, did not observe - update: dressed fish are fine.
read_csv(paste0("data/fishery/nsei_daily_tag_accounting_2004_", YEAR, ".csv")) -> marks

marks %>% 
  filter(year >= 2005 &
    !year %in% NO_MARK_SRV) %>% 
  mutate(all_observed = ifelse(
    !grepl(c("Missing|missing|Missed|missed|not counted|Did not observe|did not observe"), comments) & 
                                 observed_flag == "Yes", "Yes", "No"),
    mean_weight = ifelse(all_observed == "Yes", whole_kg/total_obs, NA),
    year_trip = paste0(year, "_", trip_no)) -> marks

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

# CPUE data - use nominal CPUE for now (it's close to the GAM output)
read_csv(paste0("data/fishery/fishery_cpue_1997_", YEAR,".csv"), 
         guess_max = 50000) %>% 
  filter(Spp_cde == "710") %>% 
  mutate(sable_kg_set = sable_lbs_set * 0.45359237, # conversion lb to kg
         std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (0.0254 * hook_space))), #standardize hook spacing (Sigler & Lunsford 2001, CJFAS)
         # kg sablefish/1000 hooks, following Mueter 2007
         wpue = sable_kg_set / (std_hooks / 1000)) %>% 
  filter(!is.na(date) & 
           !is.na(sable_lbs_set) &
           # omit special projects before/after fishery
           julian_day > 226 & julian_day < 322) %>% 
  group_by(year, trip_no) %>% 
  summarize(wpue = mean(wpue)) -> fsh_cpue

# Join the mark sampling with the fishery cpue
left_join(marks, fsh_cpue, by = c("year", "trip_no")) -> marks

# Fishery summary of observed (n) and marks (m) by year:
marks %>% group_by(year) %>% 
  summarise(fishery_n = sum(total_obs), 
            fishery_m = sum(marked),
            fishery_beg = min(date),
            fishery_end = max(date),
            fishery_len = fishery_end - fishery_beg) %>% 
  left_join(tag_summary, by = "year") -> tag_summary
  
# Look-up table of trips where none or only some of the fish were observed. Some
# of these could have other tags associated with them.
marks %>% filter(observed_flag == "Yes" & all_observed == "No") %>% nrow() # 30 trips from 2005-2017 were partially observed
marks %>% filter(observed_flag == "Yes" & all_observed == "No") %>% 
  select(year_trip, marked, unmarked, comments_countback = comments) -> partialobs 

# Look in recoveries for tags that may be associated with partialobs trips
recoveries %>% 
  filter(year_trip %in% partialobs$year_trip)  %>% 
  group_by(year_trip) %>% 
  summarize(no_tags = n_distinct(tag_no)) %>% 
  right_join(partialobs, by = "year_trip") -> partialobs

# For now the partialobs isn't particularly informative. The current logic I've
# developed applies to all trips, not just partialobs trip, since we have
# no way of knowing if tags come from the observed fish or not. If the number of
# tags recovered from a trip exceeds the number of marks, treat the excess as
# dead tags from the fishery (D_fishery) for that time period. There will be other
# D's potentially, from outside waters, etc. These are only D's associated with
# a specific trip_no.

recoveries %>% 
  filter(year_trip %in% marks$year_trip) %>% 
  group_by(year_trip) %>% 
  summarize(no_tags = n_distinct(tag_no)) %>% 
  right_join(marks, by = "year_trip") %>% 
  # padr::fill_ replaces NAs with 0 for specified cols
  fill_by_value(no_tags, value = 0) %>% 
  mutate(D_fishery = ifelse(no_tags - marked > 0, no_tags - marked, 0)) -> marks

# Figure out how many marks year_trip combos are in recoveries vs. the
# countbacks (marks), not including trip_no's 1, 2, and 3, which are the pot and
# longline surveys. anti_join(a, b) = all rows in a that do not have a match in
# b
recoveries %>% filter(!trip_no %in% c(1, 2, 3) & !is.na(trip_no)) -> a
marks  -> b
anti_join(a, b, by = "year_trip") -> no_match 
n_distinct(no_match$year_trip) #101 trips supposedly had recoveries but aren't in the daily accounting countback forms.
# Most of these are Project_cde 20-something (from Canada), 17 (pot fishery,
# Clarence), or 05 (NMFS survey), but quite a few are 02's... going to see if
# these trips match up with fish ticket data from IFDB
no_match %>% filter(Project_cde == "02") %>% distinct(year_trip) -> no_match_02 # 7 trips from 2008. uugh

read_csv(paste0("data/fishery/nseiharvest_ifdb_1969_", YEAR,".csv"), 
                       guess_max = 50000) %>% 
  mutate(whole_kg = whole_pounds * 0.453592,
         year_trip = paste0(year, "_", trip_no)) -> fsh_tx

# These 7 trips from 2008 are in the IFDB and look like they were missed, add them into 
fsh_tx %>% 
  filter(year_trip %in% no_match_02$year_trip) %>% 
  group_by(year, trip_no, year_trip, sell_date) %>% 
  summarise(whole_kg = sum(whole_kg)) %>% 
  rename(date = sell_date) %>% 
  full_join(marks, by = c("year", "trip_no", "year_trip", "date", "whole_kg")) -> narjs

# Pothole - check that all fish ticket trips have been accounted for in the NSEI in other
# years in the daily accounting form... or not. This is a mess.
full_join(fsh_tx %>%             
            filter(year >= 2005) %>% 
            distinct(year_trip, whole_kg) %>% 
            arrange(year_trip) %>% 
            select(year_trip, whole_kg1 = whole_kg),
          marks %>% 
            distinct(year_trip, whole_kg) %>% 
            arrange(year_trip)) %>% View()
          
# Summarize by day
marks %>% 
  # padr::pad fills in missing dates with NAs, grouping by years.
  pad(group = "year") %>%
  group_by(year, date) %>% 
  summarize(whole_kg = sum(whole_kg),
            total_obs = sum(total_obs),
            total_marked = sum(marked),
            mean_weight = mean(mean_weight),
            mean_wpue = mean(wpue)) %>% 
  # interpolate mean_weight column to get npue from wpue (some trips have wpue data but no bio data)
  mutate(interp_mean = zoo::na.approx(mean_weight, maxgap = 20, rule = 2),
         mean_npue = mean_wpue / interp_mean) %>%
  # padr::fill_ replaces NAs with 0 for specified cols
  fill_by_value(whole_kg, total_obs, total_marked, value = 0) %>% 
  group_by(year) %>% 
  mutate(cum_whole_kg = cumsum(whole_kg),
         cum_obs = cumsum(total_obs),
         cum_marks = cumsum(total_marked),
         julian_day = yday(date)) -> daily_marks

# Trends ----

ggplot(daily_marks, aes(x = julian_day)) +
  geom_line(aes(y = cum_marks)) +
  facet_wrap(~ year, scales = "free") +
  labs(x = "Julian Day", y = "Cumulative Marks Collected")

ggplot(daily_marks, aes(x = julian_day)) +
  geom_line(aes(y = cum_whole_kg)) +
  facet_wrap(~ year, scales = "free") +
  labs(x = "Julian Day", y = "Cumulative Catch (kg)")

# Trends in mean weight over the course of the season
ggplot(daily_marks,
       aes(x = julian_day, y = mean_weight)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~ year) +
  labs(x = "Julian Day", y = "Mean Individual Weight (kg)")

# Trends in NPUE over the course of the season
ggplot(daily_marks,
       aes(x = julian_day, y = mean_npue)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~ year, scales = "free") +
  labs(x = "Julian Day", y = "Number sablefish per 1000 hooks")

# Stratify by time ----

# For now base time strata on percentiles catch or number of marks observed?
# They mostly match up (see mutate -> tst).
daily_marks %>% 
  group_by(year) %>% 
  mutate(catch_strata = percent_rank(cum_whole_kg) %>% round(1),
         mark_strata = percent_rank(cum_marks) %>% round(1)
         #tst = ifelse(catch_strata == mark_strata, "Yes", "No"),
         ) -> daily_marks
 
# *FLAG* better way to do this? Ideally would like to automate and generalize
# based off of desired number of strata. 
daily_marks %>% 
  mutate(catch_strata = 
           # recode_factor(catch_strata, 
           #               `0.0` = 1L, `0.1` = 1L, `0.2` = 1L, 
           #               `0.3` = 2L, `0.4` = 2L,
           #               `0.5` = 3L, `0.6` = 3L, 
           #               `0.7` = 4L, `0.8` = 4L, 
           #               `0.9` = 5L, `1.0` = 5L))
           recode_factor(catch_strata, 
                         `0.0` = 1L, `0.1` = 1L, 
                         `0.2` = 2L, `0.3` = 2L, 
                         `0.4` = 3L, `0.5` = 3L, 
                         `0.6` = 4L, `0.7` = 4L, 
                         `0.8` = 5L, `0.9` = 5L, `1.0` = 5L)) -> daily_marks

# Summarize by strata
daily_marks %>% 
  group_by(year, catch_strata) %>% 
  summarize(days = n_distinct(date),
            tot_catch = sum(whole_kg),
            total_obs = sum(total_obs),
            total_marked = sum(total_marked),
            mean_npue = mean(mean_npue, na.rm = TRUE),
            mean_weight = mean(mean_weight, na.rm = TRUE)) %>% 
  mutate(est_catch_numbers = tot_catch / mean_weight) -> strata_sum

# Summarize by year to see if it matches up with mr_summary
daily_marks %>% 
  group_by(year) %>% 
  summarize(days = n_distinct(date),
            tot_catch = sum(whole_kg),
            total_obs = sum(total_obs),
            total_marked = sum(total_marked),
            mean_npue = mean(mean_npue, na.rm = TRUE),
            mean_weight = mean(mean_weight, na.rm = TRUE)) %>% 
  mutate(est_catch_numbers = tot_catch / mean_weight) -> yearly_sum


mr_summary %>% View()

# Simple Chapmanized Peterson estimator

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

coda_df <- function(coda.object, parameters = NULL) {
  
  if (!coda::is.mcmc(coda.object) && !coda::is.mcmc.list(coda.object)) 
    stop("Not an mcmc or mcmc.list object")
  
  n.chain   <- coda::nchain(coda.object)
  mat       <- as.matrix(coda.object, iter = TRUE, chain = TRUE)
  df        <- as.data.frame(mat)
  
  if(n.chain == 1)
    df <- data.frame(1, df)
  
  names(df) <- c("chain", "iter", coda::varnames(coda.object))
  
  if(is.null(parameters))
    out.df <- df
  
  if(!is.null(parameters))
    out.df <- subset(df, select = c("chain", "iter", parameters))
  
  out.df
}


coda_df(res) %>% 
  mutate(q025 = quantile(U, 0.025),
         q975 = quantile(U, 0.975),
         ci = ifelse(U >= q025 & U<=q975, 1, 0)) %>% 
  ggplot(aes(U)) + geom_histogram(fill = 4, alpha = 0.2, bins = 100, color = 'black') + 
  geom_histogram(data = . %>% filter(ci==1), aes(U), fill = 4, alpha = 0.6, bins = 100)




# Time-stratified mark-recapture model with natural mortality and immigration
# includes all clipped fish recaptured in longline survey data and fishery data

# mod = function() {

cat("
  model {

  # Priors
  N.1 ~ dnorm(2400000,1.0E-12) #I(0,)	# number of sablefish in Chatham at beginning of period 1
  
  N[1] <- N.1
  M[1] <- M.0*exp(-mu*t[1]) - D	# number of marks at beginning of period 1 (longline survey)
  # M.0 = Number of tags released
  # D = Number of tags lost to fishery or longline survey
  # mu = natural mortality (daily instantaneous mortality)
  
  for(i in 2:19) {
    M[i] <- (M[i-1] - m[i-1]) * exp(-mu * t[i])		# Number of marks at beginning of period i
    N[i] <- (N[i-1] - C[i-1]) * exp(-mu * t[i])		# Total number of sablefish at beginning of period i
  }
  
  for(i in 1:19) {
    p[i] <- M[i] / N[i]	 # probability that a caught sablefish is clipped 
    m[i] ~ dbin(p[i], n[i])	 # Number of clipped fish ~ binomial(n, p)
  }
  
  # Compute quantities of interest:
  N.avg <- mean(N[])
  }
", file = "m1.jag")

# Data to pass to JAGS
dat <- list(mu = 2.739726E-04, 
            D = 10, 
            M.0 = 6075, 
            C = c(14413, 49457, 22579, 18828, 13264, 11163, 6258, 30212, 25289, 15295, 13598, 4012, 8044, 7980, 3506, 7808, 5158, 4773), 
            n = c(14413, 32247, 13860, 17070, 8593, 8432, 3390, 21345, 17943, 14633, 9251, 2995, 5752, 4513, 3263, 4696, 5109, 4781, 5234), 
            t = c(57, 13, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5), 
            m = c(27, 105, 35, 51, 18, 19, 5, 60, 46, 34, 17, 9, 12, 6, 5, 8, 6, 9, 11))

ini <- list(N.1 = 2400000)

m1 <- jags.model("m1.jag",
                    data = dat,
                    n.chains = 2,
                    init = ini,
                    n.adapt = 1000)

mpar <- c("N.avg", "N")
res <- coda.samples(m1,
                    var = mpar,
                    n.iter = 10000,
                    thin = 10)
plot(res, col = 4)


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
