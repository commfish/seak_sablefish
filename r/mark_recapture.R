# Mark-recapture analysis
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2018-02-13

# ** currently missing daily tag accounting for 2003, 2017 will need to be updated when finalized by M. Vaughn.

source("r/helper.r")
source("r/functions.r")
library(zoo) # interpolate values
library(rjags) # run jags/bugs models
library(purrr)

# Variable definitions ----

# N.0 = number of sablefish in Chatham Strait at time of marking
# K.0 = number of marks released
# D.0 = number of marks that are not available to either the LL survey or to the fishery
# D_i = number of tags lost in a time period that should be decremented from the
#       next time period
# i = subscript for time period i, which may refer to the LL survey (i = 1)
#        to one of the fishery time periods based on time of landing
# N_i = number of sablefish in Chatham Strait at the beginning of time period i
# K_i = number of marked sablefish in Chatham Straight at the beginning of time period i
# t_i = total number of days in time period i
# n_i = observed catch (number of marked and unmarked sablefish that were
#       checked for clips, or tags in the LL survey) during period i
# k_i = number of marked fish recovered in period i (tags in the LL survey)

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

# Temporary lookup table for year and tag batch no combos
releases %>% 
  mutate(year_batch = paste0(year, "_", tag_batch_no)) %>% 
  distinct(year_batch) -> tag_summary

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

# Size selectivity differences ----

# Check range of data
releases %>% 
  group_by(year) %>% 
  summarize(min = min(length, na.rm = TRUE),
            max = max(length, na.rm = TRUE))

recoveries %>% 
  group_by(year) %>% 
  summarize(min = min(length, na.rm = TRUE),
            max = max(length, na.rm = TRUE))

# Create bins
releases %>% 
  filter(year >= FIRST_YEAR &
           !is.na(length)) %>% 
  mutate(length_bin = cut(length, breaks = seq(32.5, 117.5, 5),
                          labels = paste(seq(35, 115, 5)))) %>% 
  select(year, rel_date = date, rel_stat = Stat, tag_no, tag_batch_no, rel_len = length, 
         rel_bin = length_bin) -> rel_sel

# Same for recoveries
recoveries %>% 
  filter(year >= FIRST_YEAR & 
           !is.na(length) &
           measurer_type == "Scientific staff") %>% 
  mutate(length_bin = cut(length, breaks = seq(32.5, 117.5, 5),
                          labels = paste(seq(35, 115, 5)))) %>% 
  select(year, rec_date = date, rec_stat = Stat, tag_no, tag_batch_no, rec_len = length, 
         rec_bin = length_bin) -> rec_sel

# Growth in tagged individuals
merge(rel_sel, rec_sel, by = c("year", "tag_no", "tag_batch_no")) %>% 
  mutate(growth = rec_len - rel_len) %>%
  # lots of negative growth... 
  filter(!growth < 0) %>% 
  arrange(year, rel_bin) %>% 
  group_by(rel_bin) %>% 
  summarize(n = length(tag_no),
            mean_g = round(mean(growth), 2),
            max_g = max(growth),
            sd_g = round(sd(growth), 2)) %>% 
  # Deal with outliers in shoulder length bins: if the sample size of less than
  # 25, assume 0 growth
  mutate(g = ifelse(n < 25, 0, mean_g)) %>% 
  select(rel_bin, g) -> growth

write_csv(growth, "output/tag_estimated_growth.csv")

# Add growth to released fish then recalculate the bins
merge(rel_sel, growth, by = "rel_bin") %>% 
  mutate(growth_len = rel_len + g,
         growth_bin = cut(growth_len, breaks = seq(32.5, 117.5, 5),
                          labels = paste(seq(35, 115, 5)))) %>% 
  select(year, tag_no, growth_bin) -> growth_sel

# Proportion by bin
rel_sel %>% 
  count(year, bin = rel_bin) %>% 
  group_by(year) %>% 
  mutate(proportion = round( n / sum(n), 3),
         period = "release") %>% 
  bind_rows(rec_sel %>% 
              count(year, bin = rec_bin) %>% 
              group_by(year) %>% 
              mutate(proportion = round( n / sum(n), 3),
                     period = "recapture"),
            growth_sel %>% 
              count(year, bin = growth_bin) %>% 
              group_by(year) %>% 
              mutate(proportion = round( n / sum(n), 3),
                     period = "post-release growth")) %>% 
  mutate(period = fct_relevel(factor(period), 
                              "release", "post-release growth", "recapture")) %>% 
  ggplot(aes(x = bin, y = proportion, 
             col = period, shape = period, group = period)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~year, ncol = 2) +
  scale_x_discrete(breaks = seq(40, 110, 10), 
                   labels = seq(40, 110, 10)) +
  # scale_colour_manual(values = c("#a6bddb", "#0570b0", "#023858")) +
  scale_colour_manual(values = c("grey75", "grey50", "grey10")) +
  labs(x = "\nLength bin (cm)", 
       y = "Proportion\n") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14))

ggsave(paste0("figures/rel_rec_lengthcomps_", 
              FIRST_YEAR, "_", YEAR, ".png"), 
       dpi=300, height=8.5, width=7.5, units="in")

# Cumulative proportion by bin
rel_sel %>%   
  group_by(year, bin = rel_bin) %>% 
  mutate(bin_n = length(tag_no)) %>% 
  arrange(year, bin) %>% 
  group_by(year) %>% 
  mutate(tot_n = length(tag_no)) %>% 
  distinct(year, bin, bin_n, tot_n) %>% 
  mutate(cum = cumsum(bin_n)) %>% 
  ungroup() %>% 
  mutate(cum_prop = cum / tot_n,
         period = "release") %>% 
  bind_rows(rec_sel %>%   
              group_by(year, bin = rec_bin) %>% 
              mutate(bin_n = length(tag_no)) %>% 
              arrange(year, bin) %>% 
              group_by(year) %>% 
              mutate(tot_n = length(tag_no)) %>% 
              distinct(year, bin, bin_n, tot_n) %>% 
              mutate(cum = cumsum(bin_n)) %>% 
              ungroup() %>% 
              mutate(cum_prop = cum / tot_n,
                     period = "recapture"),
            growth_sel %>%   
              group_by(year, bin = growth_bin) %>% 
              mutate(bin_n = length(tag_no)) %>% 
              arrange(year, bin) %>% 
              group_by(year) %>% 
              mutate(tot_n = length(tag_no)) %>% 
              distinct(year, bin, bin_n, tot_n) %>% 
              mutate(cum = cumsum(bin_n)) %>% 
              ungroup() %>% 
              mutate(cum_prop = cum / tot_n,
                     period = "post-release growth")) %>% 
  mutate(period = fct_relevel(factor(period), 
                              "release", "post-release growth", "recapture")) %>% 
  ggplot(aes(x = bin, y = cum_prop, 
             col = period, shape = period, group = period)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~year, ncol = 2) +
  scale_x_discrete(breaks = seq(40, 110, 10), 
                     labels = seq(40, 110, 10)) +
  # scale_colour_manual(values = c("#a6bddb", "#0570b0", "#023858")) +
  scale_colour_manual(values = c("grey75", "grey50", "grey10")) +
  labs(x = "\nLength bin (cm)", 
       y = "Cumulative proportion\n") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14))
  
ggsave(paste0("figures/rel_rec_cumproplength_", 
              FIRST_YEAR, "_", YEAR, ".png"), 
       dpi=300, height=8.5, width=7.5, units="in")

# Approach discussed with B. Williams 2018-03-21: In a given year use the
# minimum length bin recaptured as the cut off. Throw tags with a post-release
# growth length bin less than the cut off out.

rec_sel %>% 
  group_by(year) %>% 
  summarize(cutoff = min(as.numeric(as.character(rec_bin)))) %>% 
  right_join(growth_sel, "year") %>% 
  mutate(growth_bin = as.numeric(as.character(growth_bin))) %>% 
  filter(growth_bin < cutoff) %>% 
  distinct(year, tag_no) -> throw_out

# Total number tagged and released by year
releases %>% 
  filter(year >= FIRST_YEAR &
           # Remove the one tag once adjustments for size-selectivity have been
           # made
           !tag_no %in% throw_out$tag_no) %>% 
  group_by(year, tag_batch_no) %>% 
  summarise(K.0 = n_distinct(tag_no),
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
            potsrv_middle = (potsrv_end - potsrv_beg) / 2 + potsrv_beg) %>% 
  mutate(year_batch = paste0(year, "_", tag_batch_no)) -> tag_summary

# Movement in Chatham ----

# *FLAG* in the future do a better job accounting for recoveries outside of
# Chatham (need to go back to recoveries df and sort it out by project cde). For
# now, just focus on movement within Chatham.
merge(rel_sel, 
      rec_sel %>% 
        filter(!is.na(rec_stat)) %>% 
        mutate(rec_stat = ifelse(rec_stat %in% distinct(rel_sel, rel_stat)$rel_stat, 
                                 rec_stat, NA)), 
      by = c("year", "tag_no", "tag_batch_no")) %>% 
  # Create a factor sorts Stat areas roughly south to north
  mutate(rel_stat = fct_relevel(factor(rel_stat),
                                "345603", "345631", "345702", "335701", "345701", "345731", "345803"),
         rec_stat = fct_relevel(factor(rec_stat),
                                "345603", "345631", "345702", "335701", "345701", "345731", "345803")) %>% 
  select(year, rel_stat, rec_stat) %>% 
  # filter out Frederick Sound areas because sampling has been so spotty
  filter(!rel_stat %in% c("345702", "335701") &
           !rec_stat %in% c("345702", "335701")) %>% 
  droplevels() -> move

# Summarize movement matrix - get a count of recaptures by area and year
table(releases = move$rel_stat, 
      recaptures = move$rec_stat, 
      year = move$year) %>% 
  data.frame() %>% 
  group_by(year, releases) %>% 
  mutate(N = sum(Freq), # number of releases in an area
         Probability = Freq/N) %>%  # probability of being recaptured in an area if released in an area
  ggplot(aes(x = releases, y = recaptures)) +
  geom_tile(aes(fill = Probability), colour = "grey") +
  facet_wrap(~ year, ncol = 2) +
  scale_fill_gradient(low = "white", high = "black", space = "Lab",
                      na.value = "red", guide = "colourbar",
                      name = "Probability\n") +
  # scale_fill_gradient(low = "white", high = "red", space = "Lab",
  #                     na.value = "white", guide = "colourbar",
  #                     name = "Probability\n") +
  labs(x = "\nRelease Stat Area", y = "Recapture Stat Area\n") +
  theme(axis.text.x = element_text(size = 12 ,angle = 90, hjust = 1), 
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

# The darker colours represent probability of being captured in an area if
# released in an area. The stat areas are increasing south to north along each
# axis. Darker shades along the diagonal represent no movement between areas,
# darker above means movement northward, darker below means movement southward.

ggsave(paste0("figures/movement_matrix_", 
              FIRST_YEAR, "_", YEAR, ".png"), 
       dpi=300, height=8.5, width=7.5, units="in")

# Recoveries in survey ----

# Add Chatham longline survey recoveries to the summary (survey is time period i = 1)
recoveries %>% 
  filter(Project_cde == "03") %>% 
  group_by(year, tag_batch_no) %>%
  summarise(k.1 = n_distinct(tag_no)) %>% 
  left_join(tag_summary, by = c("year", "tag_batch_no")) -> tag_summary

# Remove tags recovered in the survey from the releases df so they don't get double-counted by accident. 
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
  summarise(n = n_distinct(tag_no)) %>% 
  ungroup() %>% 
  dcast(year ~ D, value.var = "n", fill = 0) %>% 
  left_join(tag_summary, by = "year") -> tag_summary

# Currently n.1 and k.1 refelct observed and marked in the longline survey. Add
# similar columns for the fishery
daily_marks %>% 
  group_by(year) %>% 
  summarize(k_fishery = sum(total_marked),
            n_fishery = sum(total_obs),
            D_fishery = sum(fishery_D)) %>% 
  left_join(tag_summary) -> tag_summary

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
  facet_wrap(~ year, scales = "free", ncol = 2) +
  labs(x = "Julian Day", y = "Number sablefish per 1000 hooks\n")

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
data_df <- list() #
# Number of models - currently 4 competing models to the simple Chapman: 
# 1) model 1 -  Peterson with time strata, accounts for natural mortality and catch
# 2) model 1 + migration parameter
# 3) model 1 + catchability parameter (incorportates NPUE data)
# 4) model 2 + model 3

# Number of period (time strata) combos to test
strats <- 8

#Prepare progress bar
pb <- txtProgressBar(min = 0, max = strats, style = 3)

for(j in 1:strats) {
  
# *FLAG* For now base strata on percentiles of cumulative catch. Could also use
# number of marks observed or some other variable. STRATA_NUM is the dynamic
# variable specifying the number of time strata to split the fishery into and it
# currently accomodates 8 or fewer strata.
STRATA_NUM <- j

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

# Prep data for JAGS ----  
dcast(setDT(strata_sum), year ~ catch_strata, 
      value.var = c("D", "C", "n", "t", "k", "NPUE"), sep = ".") %>% 
  left_join(tag_summary, by = "year") %>% 
  # order the columns alphanumerically - very convenient
  select(year, order(colnames(.))) %>% 
  # Remove NPUE from survey from the data, because it's calculated a little
  # differently than the fishery and shouldn't be used to estimate catchability
  select(- NPUE.1) -> jags_dat

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

for(i in 2:P) {
K[i] <- (K[i-1] - k[i-1] - D[i-1]) * exp(-M * t[i])		# Number of marks at beginning of period i
N[i] <- (N[i-1] - C[i-1]) * exp(-M * t[i])		# Total number of sablefish at beginning of period i
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

#Calls rbind on each list item to convert the list of dfs into one long df
N_summary <- do.call("rbind", N_summary_ls)
dic_summary <- do.call("rbind", dic_ls)
convergence_summary <- do.call("rbind", converge_ls)

# Save relevant files for later use - with 50K iterations and the default
# thinning rate it takes about an hour to run.

save(list = c("dic_summary", "N_summary", "convergence_summary",
              "mod1_posterior_ls", "mod2_posterior_ls", 
              "mod3_posterior_ls", "mod4_posterior_ls",
              "data_ls", "data_df"),
     file = "output/mark_recap_model_selection.Rdata")

# Model selection ----

load("output/mark_recap_model_selection.Rdata")

convergence_summary %>% 
  mutate(mod_version = paste(year, model, P, sep = "_")) %>% 
  filter(# Convergance diagnostic: A factor of 1 means that between variance and within chain
    # variance are equal, larger values mean that there is still a notable
    # difference between chains. General rule: everything below 1.1 or so
    # is ok.
    Point.est. >= 1.1) -> not_converged

not_converged %>% distinct(model, P) # do not include models that estimate q unless they have > 3 periods

# Models with P>=6
dic_summary %>% 
  filter(P >= 6 & year == YEAR) %>% 
  group_by(year) %>% 
  mutate(min_DIC = min(DIC)) %>% 
  ungroup() %>% 
  mutate(delta_DIC = round(DIC - min_DIC, 2),
         mod_version = paste(year, model, P, sep = "_")) %>%
  arrange(year, delta_DIC) %>% 
  group_by(model) %>% 
  filter(delta_DIC == min(delta_DIC)) -> top_models

# Tmp: fixed n_summary_out in mr_jags, need to re-run.
bind_rows(mod1_posterior_ls[[5]] %>% select(N.avg, year, model, P),
          mod2_posterior_ls[[5]] %>% select(N.avg, year, model, P),
          mod3_posterior_ls[[5]] %>% select(N.avg, year, model, P),
          mod4_posterior_ls[[5]] %>% select(N.avg, year, model, P)) %>% 
  group_by(model, year, P) %>% 
  # mutate(N.avg = N.avg / 1000000) %>% 
  summarize(median = median(N.avg),
            q025 = quantile(N.avg, 0.025),
            q975 = quantile(N.avg, 0.975)) -> N_summary

N_summary %>% 
  mutate(mod_version = paste(year, model, P, sep = "_"),
         mod_version2 = paste(model, P, sep = "_")) -> N_summary

N_summary %>% 
  filter(mod_version %in% top_models$mod_version) %>% 
  left_join(top_models, by = c("year", "model", "P", "mod_version")) %>% 
  select(year, model, P, Estimate = median, q025, q975, deviance, parameter_penalty, delta_DIC) -> top_models 
  
tag_summary %>% 
  # CI formula from wikipedia 
  mutate(Estimate = ((K.0 - D.0 - D.1 + 1)*(n.1 + 1) / (k.1 + 1)) - 1,
         chap_ci = K.0 - D.0 - D.1 + n.1 - k.1 + 
           (K.0 - D.0 - D.1 - k.1 + 0.5) * (n.1 - k.1 + 0.5) / (k.1 + 0.5) * 
           exp(1 - 0.95 / 2) *
           sqrt(1 / (k.1 + 0.5) + 1 / (K.0 - D.0 - D.1 - k.1 + 0.5) + 
                  1 / (n.1 - k.1 + 0.5) + 
                  (k.1 + 0.5) / (n.1 - k.1 + 0.5) * (K.0 - D.0 - D.1 - k.1 + 0.5)),
         q975 = Estimate + chap_ci,
         q025 = Estimate - chap_ci,
         model = "Model0",
         P = 1) %>% 
  select(year, model, P, Estimate) %>% 
  filter(year == YEAR) %>% 
  full_join(top_models) %>% 
  write_csv(paste0("output/top_models_", YEAR, ".csv"))

# N_summary %>%
#   group_by(year) %>%
#   summarize(min = min(N.avg),
#             max = max(N.avg)) %>%
#   melt(id.vars = "year", measure.vars = c("min", "max"), value.name = "N.avg") %>%
#   left_join(N_summary, by = c("year", "N.avg")) %>%
#   bind_rows(
#     # N estimates for top models
#     N_summary %>%
#       filter(mod_version %in% top_models$mod_version) %>%
#       mutate(variable = "best_fit") ) %>%
#   arrange(year)

ggplot() +
  geom_histogram(data = N_summary %>% 
                   # leave out models that did not converge
                   filter(!mod_version %in% not_converged$mod_version), 
                 aes(N.avg),  binwidth = 0.05) +
  geom_vline(data = N_summary %>%
               filter(mod_version2 %in% top_mods2$mod_version2),
             aes(colour = model, linetype = model,
                 xintercept = N.avg),
             size = 1) +
  facet_wrap(~year, ncol = 2) +
  labs(x = "\nAbundance estimate (millions)", 
       y = "Frequency\n") +
  scale_color_brewer(palette = "BrBG") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14))

# Histogram shows point estimates for all model versions (P = 2-9), except the
# ones that did not converge. Coloured vertical bars show point estimate of
# "best fit" model by DIC.
ggsave(paste0("figures/Nest_range_histogram", 
              FIRST_YEAR, "_", YEAR, ".png"), 
       dpi=300, height=8.5, width=7.5, units="in")

# Summary of posterior distributions of quantities of interest, including
# period-specific estimates of N (N[]), estimates of mean abundance (N.avg), net
# migration (r) which can be positive or negative to indicate net immigration or
# emigration, respetively, and catchability (q): map() applies the summary
# functions after the ~ to each df in the list. do.call() rbinds the output
# together. Repeat this for each model and rbind into one summary df.
bind_rows(
  do.call("rbind", map(mod1_posterior_ls, 
                       ~select(.x, year, P, contains("N["), N.avg) %>%
                         melt(id.vars = c("year", "P")) %>% 
                         group_by(year, P, variable) %>% 
                         summarise(median = median(value),
                                   sd = sd(value),
                                   q025 = quantile(value, 0.025),
                                   q975 = quantile(value, 0.975)))) %>% 
    arrange(year, variable) %>%
    mutate(model = "Model1",
           mod_version = paste(year, model, P, sep = "_")),
  
  do.call("rbind", map(mod2_posterior_ls, 
                       ~select(.x, year, P, contains("N["), N.avg, r) %>%
                         melt(id.vars = c("year", "P")) %>% 
                         group_by(year, P, variable) %>% 
                         summarise(median = median(value),
                                   sd = sd(value),
                                   q025 = quantile(value, 0.025),
                                   q975 = quantile(value, 0.975)))) %>% 
    arrange(year, variable) %>%
    mutate(model = "Model2",
           mod_version = paste(year, model, P, sep = "_")),
  
  do.call("rbind", map(mod3_posterior_ls, 
                       ~select(.x, year, P, contains("N["), N.avg, q) %>%
                         melt(id.vars = c("year", "P")) %>% 
                         group_by(year, P, variable) %>% 
                         summarise(median = median(value),
                                   sd = sd(value),
                                   q025 = quantile(value, 0.025),
                                   q975 = quantile(value, 0.975)))) %>% 
    arrange(year, variable) %>%
    mutate(model = "Model3",
           mod_version = paste(year, model, P, sep = "_")),
  
  do.call("rbind", map(mod4_posterior_ls, 
                       ~select(.x, year, P, contains("N["), N.avg, r, q) %>%
                         melt(id.vars = c("year", "P")) %>% 
                         group_by(year, P, variable) %>% 
                         summarise(median = median(value),
                                   sd = sd(value),
                                   q025 = quantile(value, 0.025),
                                   q975 = quantile(value, 0.975)))) %>% 
    arrange(year, variable) %>% 
    mutate(model = "Model4",
           mod_version = paste(year, model, P, sep = "_"))) -> post_sums

# Scale parameters within model and year to test effect of increasing P (time
# periods) on point and variance estimates
post_sums %>% 
  group_by(year, model, variable) %>% 
  mutate(scale_within_median = scale(median),
         scale_within_sd = scale(sd)) %>% 
  # scale parameters between models to test the effect of different
  # parameterizations and increasing P on parameter point and variance
  # estimates.
  group_by(year, variable) %>% 
  mutate(scale_btwn_median = scale(median),
         scale_btwn_sd = scale(sd)) -> post_sums

# Examination of abundance (scaled within model) - trends are strikingly similar
# between models, but there is no clear trend in abundance estimate response to
# increasing P. 4/10 years the estimates decrease with increasing P, 5/10
# increase, and 1/10 (2008) decreases with P then increases again
ggplot(post_sums %>% filter(variable == "N.avg" #&
                              #P >= 4
                              )) +
  geom_point(aes(x = P, y = scale_within_median, 
                 colour = model)) +
  geom_smooth(span = 2, se = FALSE,
              aes(x = P, y = scale_within_median, 
                  colour = model, linetype = model,
                  group = model)) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "\nNumber of time periods",
       y = "Abundance scaled within models\n") +
  scale_color_brewer(palette = "BrBG", "") +
  scale_linetype_manual(values = 1:4, "") +
    facet_wrap(~ year, ncol = 2)

ggsave(paste0("figures/N_scaledwithin_2005_", YEAR, ".png"), 
       dpi=300, height=7, width=7, units="in")

# Steep decreasing trend in variability with increasing P
ggplot(post_sums %>% filter(variable == "N.avg")) +
  geom_point(aes(x = P, y = scale_within_sd, 
                 colour = model)) +
  geom_smooth(span = 2, se = FALSE,
              aes(x = P, y = scale_within_sd, 
                  colour = model, linetype = model,
                  group = model)) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_color_brewer(palette = "BrBG") +
  labs(x = "\nNumber of time periods",
       y = "SD of abundance scaled within models\n")

ggsave(paste0("figures/Nvar_scaledwithin.png"), 
       dpi=300, height=4, width=4, units="in")

# Examination of abundance (scaled between models) - trends with P are similar
# as the within model examination... more strinking are the consistent between
# model trends. Model 2 has the greatest estimates for N by ~ 1 sd for all yrs
# except 2017, followed by Model 4 or 1. Model 3 has the lowest estimate of N
# for all years.
ggplot(post_sums %>% filter(variable == "N.avg")) +
  geom_point(aes(x = P, y = scale_btwn_median, 
                 colour = model)) +
  geom_smooth(span = 2, se = FALSE,
              aes(x = P, y = scale_btwn_median, 
                  colour = model, linetype = model,
                  group = model)) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "\nNumber of time periods",
       y = "Abundance scaled between models\n") +
  scale_color_brewer(palette = "BrBG") +
  facet_wrap(~ year, ncol = 2)

ggsave(paste0("figures/N_scaledbtwn_2005_", YEAR, ".png"), 
       dpi=300, height=7, width=7, units="in")

# All models show sd of abundance decreasing with P, but between model variation
# shows that Model 2 consistently has the highest variability, followed by Model
# 4, and Models 1/3.
ggplot(post_sums %>% filter(variable == "N.avg")) +
  geom_point(aes(x = P, y = scale_btwn_sd, 
                 colour = model)) +
  geom_smooth(span = 2, se = FALSE,
              aes(x = P, y = scale_btwn_sd, 
                  colour = model, linetype = model,
                  group = model)) +
  scale_color_brewer(palette = "BrBG") +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "\nNumber of time periods",
       y = "SD of abundance scaled between models\n")

ggsave(paste0("figures/Nvar_scaledbtwn.png"), 
       dpi=300, height=4, width=4, units="in")

# Examination of migration (r) (scaled within model) - trends with P between
# models are  similar but don't track quite as closely as they did with N.
# Trends are also different between years, which confirms that increasing P does
# not have a predictable response in terms of it's directional impact on a point
# esitmate.
ggplot(post_sums %>% filter(variable == "r")) +
  geom_point(aes(x = P, y = scale_within_median, 
                 colour = model)) +
  geom_smooth(span = 2, se = FALSE,
              aes(x = P, y = scale_within_median, 
                  colour = model, group = model)) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "\nNumber of time periods",
       y = "Net migration scaled within models\n") +
  facet_wrap(~ year, ncol = 2)

# Examination of migration (r) (scaled between models) - Model 2 has higher
# estimates of r in all years except 2008 and 2017.
ggplot(post_sums %>% filter(variable == "r")) +
  geom_point(aes(x = P, y = scale_btwn_median, 
                 colour = model)) +
  geom_smooth(span = 2, se = FALSE,
              aes(x = P, y = scale_btwn_median, 
                  colour = model, linetype = model,
                  group = model)) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "\nNumber of time periods",
       y = "Net migration scaled between models\n") +
  scale_color_manual(values = c("#dfc27d", "#018571")) +
  scale_linetype_manual(values = c(2, 4)) +
  facet_wrap(~ year, ncol = 2)

# Variance in r decreases with P, as with N.avg and is greater in Model 2 than
# Model 4
ggplot(post_sums %>% filter(variable == "r")) +
  geom_point(aes(x = P, y = scale_btwn_sd, 
                 colour = model)) +
  geom_smooth(span = 2, se = FALSE,
              aes(x = P, y = scale_btwn_sd, 
                  colour = model, linetype = model,
                  group = model)) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_color_manual(values = c("#dfc27d", "#018571")) +
  scale_linetype_manual(values = c(2, 4)) +
  labs(x = "\nNumber of time periods",
       y = "SD of net migration scaled between models\n")

# Examination of catchability (q) (scaled within model) *IMPORTANT* models
# estimating q must have P >= 3. Otherwise q appears to be constant and well
# estimated across years and P
ggplot(post_sums %>% filter(variable == "q")) +
  geom_point(aes(x = P, y = scale_within_median, 
                 colour = model)) +
  geom_smooth(span = 2, se = FALSE,
              aes(x = P, y = scale_within_median, 
                  colour = model, linetype = model,
                  group = model)) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "\nNumber of time periods",
       y = "Catchability scaled within models\n") +
  scale_color_manual(values = c("#80cdc1", "#018571")) +
  scale_linetype_manual(values = c(3, 4)) +
  facet_wrap(~ year, ncol = 2)

# Examination of of catchability (q) estimates(scaled between models) - there
# appears to be few differences between models.
ggplot(post_sums %>% filter(variable == "q")) +
  geom_point(aes(x = P, y = scale_btwn_median, 
                 colour = model)) +
  geom_smooth(span = 2, se = FALSE,
              aes(x = P, y = scale_btwn_median, 
                  colour = model, linetype = model,
                  group = model)) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "\nNumber of time periods",
       y = "Catchability scaled between models\n") +
  scale_color_manual(values = c("#80cdc1", "#018571")) +
  scale_linetype_manual(values = c(3, 4)) +
  facet_wrap(~ year, ncol = 2)

# The variance estimate of q is very high when P=2, but otherwise stays constant
# across all values of P (does not decrease with increasing P as with other
# parameters). There is no difference in variance between models
# and years is very small.
ggplot(post_sums %>% filter(variable == "q")) +
  geom_point(aes(x = P, y = scale_btwn_sd, 
                 colour = model)) +
  geom_smooth(se = FALSE,
              aes(x = P, y = scale_btwn_sd,
                  colour = model, linetype = model,
                  group = model)) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_color_manual(values = c("#80cdc1", "#018571")) +
  scale_linetype_manual(values = c(3, 4)) +
  labs(x = "\nNumber of time periods",
       y = "SD of catchability scaled between models\n")


results <- mod2_posterior_ls[[5]]

results %>% 
  group_by(year) %>% 
  mutate(r = r / 1000,
         q025 = quantile(r, 0.025),
         q975 = quantile(r, 0.975),
         ci = ifelse(r >= q025 & r <=q975, 1, 0),
         median = median(r)) %>% 
  ggplot(aes(r)) + 
  geom_histogram(fill = "white", alpha = 0.2, bins = 100, color = 'black') + 
  geom_histogram(data = . %>% filter(ci==1), 
                 aes(r), fill = "grey50", alpha = 0.9, bins = 100) +
  geom_vline(aes(xintercept = median), col = "black", lty = 2, size = 1) +
  geom_vline(aes(xintercept = 0), col = "black") +
  facet_wrap(~ year, ncol = 2) +
  labs(x = "\nNet migration of sablefish (x 1000)",
       y = "\nPosterior distribution") +
  xlim(c(-30, 40))

ggsave(paste0("figures/net_migration_estimates.png"), 
       dpi=300, height=8.5, width=7.5, units="in")

# N thru time ----

post_sums %>% 
  ungroup() %>% 
  filter(!grepl('N.avg|r|q', variable)) %>% 
  # Re-order levels of N[]
  mutate(variable = fct_relevel(factor(variable),
                                "N[10]", after = Inf),
         median = median / 1000000) %>% 
  ggplot() +
  geom_line(aes(x = variable, y = median, 
                  colour = factor(P), group = factor(P))) +
  facet_grid(year ~ model) +
  scale_color_brewer(palette = "Blues", "Number of\nPeriods") +
  labs(x = "\nAbundance by time period",
       y = "Abundance (in millions)\n") +
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        legend.position="none")

ggsave(paste0("figures/Nest_byP_2005_", YEAR, ".png"), 
       dpi=300, height=11, width=11, units="in")

head(model_dat)

# Plot catchability and obs vs. fitted NPUE
results <- mod3_posterior_ls[[5]]

results %>% 
  group_by(year) %>% 
  mutate(q025 = quantile(q, 0.025),
         q975 = quantile(q, 0.975),
         ci = ifelse(q >= q025 & q <=q975, 1, 0),
         median = median(q)) %>% 
  ggplot(aes(q)) + 
  geom_histogram(fill = "grey50", alpha = 0.2, bins = 100, color = 'black') + 
  geom_histogram(data = . %>% filter(ci==1), 
                 aes(q), fill = "grey50", alpha = 0.6, bins = 100) +
  geom_vline(aes(xintercept = median), col = "black", lty = 2, size = 1) +
  facet_wrap(~ year, ncol = 2) +
  labs(x = "Catchability (q)",
       y = "Posterior distribution") +
  xlim(c(0.00002, 0.00009))

results %>% 
  group_by(year) %>% 
  summarize(median = median(q),
            q025 = quantile(q, 0.025),
            q975 = quantile(q, 0.975))

data_df[[5]] %>% 
  select(year, P = catch_strata, NPUE) %>% 
  mutate(Type = "Observed") -> NPUE

results %>% 
  select(year, P, contains("npue.hat")) %>% 
  melt(id.vars = c("year", "P")) %>% 
  group_by(year, variable) %>% 
  summarize(NPUE = median(value),
            q025 = quantile(value, 0.025),
            q975 = quantile(value, 0.975)) %>% 
  mutate(Type = "Estimated",
         P = fct_recode(variable,
                               `2` = "npue.hat[2]" ,
                               `3` = "npue.hat[3]" ,
                               `4` = "npue.hat[4]" ,
                               `5` = "npue.hat[5]",
                               `6` = "npue.hat[6]")) %>% 
  select(-variable) %>% 
  full_join(NPUE) -> NPUE

ggplot() +
  geom_point(data = NPUE, aes(x = P, y = NPUE, colour = Type)) +
  geom_smooth(data = NPUE %>% filter(Type == "Estimated"),
             aes(x = P, y = NPUE, colour = Type, group = Type)) +
    geom_ribbon(data = NPUE %>% filter(Type == "Estimated"),
                aes(x = P, ymin = q025, ymax = q975, 
                    fill = Type, group = Type), 
              alpha = 0.2, fill = "grey") +
  scale_colour_manual(values = c("grey", "black")) +
  facet_wrap(~ year, ncol = 2) +
  labs(x = "\nTime period",
       y = "CPUE (sablefish per 1000 hooks)\n") +
  theme(legend.position = "none")

ggsave(paste0("figures/NPUE_obsvsfitted.png"), 
       dpi=300, height=8.5, width=7.5, units="in")
                        

# Model 1 P = 6 Results ----

results <- mod1_posterior_ls[[5]]
model_years <- unique(tag_summary$year)

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
  # ylim(c(1, 3.5)) +
  labs(x = "", y = "Number of sablefish (millions)\n",
       colour = NULL, shape = NULL) +
  theme(legend.position = c(.8, .8))
# 
# ggsave(paste0("figures/model1_N_retrospective_", 
#               FIRST_YEAR, "_", YEAR, ".png"), 
#        dpi=300, height=4, width=6, units="in")


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

# Tag summary

tag_summary %>% 
  select(Year = year, `$K_0$` = K.0, `$D_0$` = D.0, 
         `$k_{srv}$` = k.1, `$n_{srv}$` = n.1, `$D_{srv}$` = D.1,
         `$k_{fsh}$` = k_fishery, `$n_{fsh}$` = n_fishery, `$D_{fsh}$` = D_fishery) %>% 
  write_csv("output/tag_summary_report.csv")

# Forecast/YPR Inputs ----

# All inputs from biological.r

# NOAA fishery and survey age-based selectivity coefficients Fishery females, males, then
# survey females, males Manual input from Dana's NMFS spreadsheet - request from
# him

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

f_sel <- 1 / (1 + exp(-fslp_f * (age - f50_f)))
m_sel <- 1 / (1 + exp(-fslp_m * (age - f50_m)))
sf_sel <- 1 / (1 + exp(-sslp_f * (age - s50_f)))
sm_sel <- 1 / (1 + exp(-sslp_m * (age - s50_m)))

# Input estimate of abundance from previous year last year's full recruitment
# 'F' values, total commercial catch, and M. Clip data, not tag data. This is
# the POOLED estimate of abundance, which is subsequently partitioned into
# sex-specific selectivities below

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
  filter(Source == "LL fishery" &
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
length(wt_s_f); length(wt_s_m); length(wt_f_f); length(wt_f_m); length(mat_s_f)

# Multiply N by fishery proportions to estimate *EXPLOITED* 
# numbers-at-age and divide by age-specific selectivity
#
# AS PER MUETER 2010 sablefish ASA report to ADF&G, the 'exploited' population
# refers to the population targeted by both gear AND fishing fleet behavior
# 'Exploitable' means those vulnerable solely to gear under conditions of random
# sampling

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

N_MR_sex <- assessment_summary %>% 
  filter(year == YEAR) %>% 
  select(N.avg) %>% 
  as.numeric()

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

# Discard mortality ----

# From M. Vaughn and K. Carroll 2018-06-04: Size grade and cost definition from
# processor will be used to define the probability of retaining a fish
grades <- data.frame(
  # grade and price given by processor
  # grade = c("no_grade", "1/2", "2/3", "3/4", "4/5", "5/7", "7+"),
  # price = c(0, 1, 2.2, 3.25, 4.75, 7.55, 8.05),
  # Based off conversation with A. Alson 2018-06-04, set grade 3/4 as 50%
  # probability of retention (p), and very low for grades below.
  p = c(0, 0.02, 0.1, 0.5, 1, 1, 1),
  kg = c(0.5, 0.7, 1.4, 2.2, 2.9, 3.6, 5.0)) %>%  
  right_join(data.frame(kg = seq(0.5, 8.5, by = 0.1)) %>% 
               mutate(grade = derivedVariable(
                 'no_grade' = kg < 0.7,
                 '1/2' = kg >= 0.7 & kg < 1.4,
                 '2/3' = kg >= 1.4 & kg < 2.2,
                 '3/4' = kg >= 2.2 & kg < 2.9,
                 '4/5' = kg >= 2.9 & kg < 3.6,
                 '5/7' = kg >= 3.6 & kg < 5,
                 '7+' = kg >= 5,
                 .method = "unique")), by = "kg") %>% 
  # set p = 1 for all large fish, interpolate p's using a cubic spline across
  # smaller sizes
  mutate(p = ifelse(kg > 3.6, 1, zoo::na.spline(p)),
         kg = round(kg, 1))

plot(grades$p ~ grades$kg, type = 'l') # good enough

# *FLAG* Assume that the discard fishery has the same selectivity as the survey.
# Other notes: Grades 1/2 and 2/3 are usually only for the survey, but now these
# small fish are showing up in force in the fishery, so map these p_retain
# (probabilities of retention) to the survey selectivies and survey
# weight-at-ages. This is an indication that selectivity in the fishery may look
# more like the survey at smaller sizes. *FLAG* Need to revisit this
# (selectivity) in future years.

# Female and male probabilities of discarding at age
f_discard <- 1 - data.frame(age = AGE, kg = wt_s_f) %>% 
  left_join(grades, by = "kg") %>% 
  pull(p)

m_discard <- 1 - data.frame(age = AGE, kg = wt_s_m) %>% 
  left_join(grades, by = "kg") %>% 
  pull(p)

plot(f_discard ~ AGE, type = "l", col = "magenta")
lines(m_discard ~ AGE, col = "blue", lty = 4)

for(i in 1:41){
  
  Nm[i] <- (N_MR_sex * male_p * m[i]) / m_sel[i]
  Nf[i] <- (N_MR_sex * female_p * f[i]) / f_sel[i]

}

sum(Nf+Nm) 

# Assume discard mortality = 0.16, same as halibut directed fishery

dm <- 0.16

# PROPAGATE LAST YEAR'S ESTIMATED ABUNDANCE-AT-AGE USING STANDARD 
# AGE-STRUCTURED EQUATIONS. NOTE: AGE 2 TRANSLATES **WITH** MORTALITY 

#Fishing mortality * selectivity. Note that we are using HALF of the previous
#full-recruitment F value due to the estimate of abundance being the MEAN
#abundance in the middle of the commercial fishery. The first term is the
#retention portion of the fishery. The second term is the discard mortality,
#which we are assuming has the same selectivity as the survey (catching smaller
#fish).
Fm <- F_previous/2 * m_sel + F_previous/2 * sm_sel * m_discard * dm
Ff <- F_previous/2 * f_sel + F_previous/2 * sf_sel * f_discard * dm

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
  mutate(N = N / 1000,
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
# agecomps %>% 
#   filter(year == YEAR & Source == "LL fishery" &
#            Sex %in% c("Male", "Female")) %>% 
#   ggplot(aes(age, proportion, fill = Sex)) +
#   geom_bar(stat = "identity",
#            position = "dodge") +
#   # position = position_dodge(preserve = "single")) +
#   scale_x_continuous(breaks = seq(min(agecomps$age), max(agecomps$age), 4), 
#                      labels =  seq(min(agecomps$age), max(agecomps$age), 4)) +
#   labs(x = "\nAge", y = "Proportion\n") +
#   theme(legend.position = c(0.9, 0.7))
# 
# ggsave(paste0("figures/agecomp_bargraph_", YEAR, ".png"), 
#        dpi=300, height=3, width=9, units="in")

# Format tables

v2018 <- c(exp_n, exp_b, F50, Q50s) #, Q45s, Q40s) 

# From 2017 assessment

v2017 <- c(N_MR_sex, 13502591, 0.0683, 850113)

format(round((v2018 - v2017)/v2017 * 100, 1),  nsmall=1) -> perc_change

v2017[c(1, 2, 4)] <- lapply(v2017[c(1, 2, 4)], prettyNum, trim=TRUE, big.mark=",")
v2017[3] <- lapply(v2017[3], format, nsmall=3, digits=3)

v2018[c(1, 2, 4)] <- lapply(v2018[c(1, 2, 4)], prettyNum, trim=TRUE, big.mark=",")
v2018[3] <- lapply(v2018[3], format, nsmall=3, digits=3)

rbind(v2017, v2018, perc_change) %>% data.frame() -> forecast

save(forecast, file = paste0("output/forecast_table_", YEAR+1, ".rda"))

# 2017 exploitable biomass ---- Re-calculate this for the updated summary table
# since the 2017 exploitable biomass was so low.

N_MR_sex

AGE <- 2:42
Nm <- 1:41
Nf <- 1:41

for(i in 1:41){
  
  Nm[i] <- (N_MR_sex * male_p * m[i]) / m_sel[i]
  Nf[i] <- (N_MR_sex * female_p * f[i]) / f_sel[i]
  
}

sum(Nf+Nm) 

upd2017_expb <- ktp * sum((Nm * wt_f_f  * f_sel) + (Nf * wt_f_m * m_sel))

# Adjust for high recruitment ----

# Justification for using a different quantile of the N posterior: Option 1: (1)
# determine what proportion of fish are under 7 (a50 = 6.4), (2) take the 50th +
# that percent under age-7 to adjust the quantile used from the N posterior.
# Conservative because a much greater proportion of the exploitable pop is
# immature than in previous years. - did not choose this method.

Nme <- 1:41
Nfe <- 1:41

for(i in 1:41){
  Nme[i] <- (N_MR_sex * male_p * m[i]) 
  Nfe[i] <- (N_MR_sex * female_p * f[i]) 
}

# Option 1: take an "immaturity" adjustment
data.frame(age = AGE,
           Nme = Nme,
           Nfe = Nfe) %>% 
  mutate(N = Nme + Nfe) %>% 
  mutate(total = sum(N)) %>% 
  filter(age <= 7) %>% 
  mutate(suba50 = sum(N),
         prop_suba50 = suba50/total) %>% 
  distinct(prop_suba50) %>% 
  pull(prop_suba50) -> imm_adj # immaturity adjustment 

# Option 2: just choose a percentile. I chose this method.
imm_adj <- 0.35 

results %>% 
  filter(year == YEAR) %>%
  mutate(N.avg = N.avg / 1000000,
         q025 = quantile(N.avg, 0.025),
         q975 = quantile(N.avg, 0.975),
         imm_adj = quantile(N.avg, .5 - imm_adj),
         ci = ifelse(N.avg >= q025 & N.avg <=q975, 1, 0),
         median = median(N.avg)) -> adj_results 

adj_results %>% 
  ggplot(aes(N.avg)) + 
  geom_histogram(fill = "white", alpha = 0.9, bins = 100, color = 'black') + 
  geom_histogram(data = . %>% filter(ci==1), 
                 aes(N.avg), fill = "grey50", alpha = 0.9, bins = 100) +
  geom_vline(aes(xintercept = median), col = "black", size = 1) +
  geom_vline(aes(xintercept = imm_adj), col = "black", lty = 2, size = 1) +
  labs(x = "\nNumber of sablefish in millions",
       y = "Posterior distribution\n")

ggsave(paste0("figures/mod1_Nposterior_adjustment.png"), 
       dpi=300,  height=4, width=7,  units="in")

adj_N_MR_sex <- adj_results %>% 
  distinct(imm_adj) %>% 
  transmute(imm_adj = imm_adj * 1e6) %>% 
  pull

# Calculate adjusted 2017 exploitable biomass

Nm <- 1:41
Nf <- 1:41

for(i in 1:41){
  
  Nm[i] <- (adj_N_MR_sex * male_p * m[i]) / m_sel[i]
  Nf[i] <- (adj_N_MR_sex * female_p * f[i]) / f_sel[i]
  
}

sum(Nf+Nm) 

adj_upd2017_expb <- ktp * sum((Nm * wt_f_f  * f_sel) + (Nf * wt_f_m * m_sel))


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
adj_N_MR_sex

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


adj_Q50s <- sum((N_fp * wt_f_f * ktp)* ((F50 * f_sel) / (((F50 * f_sel) + mort)) * (1 - exp(-((F50 * f_sel) + mort))))+
              (N_mp * wt_f_m * ktp) * ((F50 * m_sel)/(((F50 * m_sel) + mort)) * (1 - exp(-((F50 * m_sel) + mort)))))

# THIS IS TOTAL EXPLOITED BIOMASS - TOTAL ABUNDANCE PARTITIONED INTO COHORTS * 
# FISHERY WEIGHT * SELECTIVITY (REFER: Q&D bottom page 339)

adj_2018_exp_b <- ktp * sum((N_fp * wt_f_f * f_sel) + (N_mp * wt_f_m * m_sel))


adj_2018_exp_n<-sum((N_fp*f_sel)+(N_mp*m_sel))

adj_2018_exp_n
adj_2018_exp_b
adj_Q50s
F50

# Updated summary table ----

data.frame("Quantity" = c("Exploited abundance (2017 value from last year)",
                          "Exploited abundance",
                          "Exploited abundance (adjusted for uncertainty in recruitment)",
                          "Exploited biomass (2017 value from last year)",
                          "Exploited biomass",
                          "Exploited biomass (adjusted for uncertainty in recruitment)",
                          "$F_{ABC}=F_{50}$",
                          "$ABC$ (round lbs)",
                          "$ABC_{adj}$ (round lbs)"),
           "Y2017" = c(1564409, N_MR_sex, adj_N_MR_sex,
                       13502591, upd2017_expb, adj_upd2017_expb,
                       0.0683, 850113, 850113),
           "Y2018" = c(adj_2018_exp_n, exp_n, adj_2018_exp_n,
                       adj_2018_exp_b, exp_b, adj_2018_exp_b,
                       F50, Q50s, adj_Q50s)) %>% 
   write_csv("output/2018_summary_table.csv")

# Updated retrospective plot ----

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
              select(year, `Previous estimate` = abundance_age2plus), 
            by = "year") %>% 
  # bind_rows(fcast) %>% 
  ungroup() %>% 
  mutate(year = as.Date(as.character(year), format = "%Y")) %>% 
  pad(interval = "year") %>% 
  mutate(year = year(year),
         Year = factor(year),
         `Current estimate adjusted` = ifelse(year == YEAR, adj_N_MR_sex, `Current estimate`)) %>% 
  # Add forecasted year
  bind_rows(data.frame(year = YEAR + 1,
                       time_period = "N.avg", 
                       "Current estimate" = exp_n, 
                       q025 = NA, 
                       q975 = NA, 
                       "Previous estimate" = NA,
                       Year = factor(YEAR + 1),
                       "Current estimate adjusted" = adj_2018_exp_n,
                       check.names = FALSE)) %>% 
  gather("Abundance", "N", `Previous estimate`, `Current estimate`, `Current estimate adjusted`) %>% 
  mutate(N = N / 1000000,
         # interpolate the CI in missing years for plotting purposes
         q025 = zoo::na.approx(q025 / 1000000, maxgap = 20, rule = 2),
         q975 = zoo::na.approx(q975 / 1000000, maxgap = 20, rule = 2),
         # get rid of interpolated values for forecast
         q025 = ifelse(year == YEAR + 1, NA, q025),
         q975 = ifelse(year == YEAR + 1, NA, q975)) -> forec_plot

axis <- tickr(forec_plot, year, 2)

ggplot(data = forec_plot) +
  geom_point(aes(x = year, y = N, col = Abundance, shape = Abundance), size = 2) +
  geom_smooth(aes(x = year, y = N, col = Abundance, linetype = Abundance), 
              se = FALSE) +
  geom_ribbon(aes(x = year, ymin = q025, ymax = q975), 
              alpha = 0.2, fill = "grey70") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  scale_color_manual(values = c("black", "black", "grey75")) + 
  scale_linetype_manual(values = c(4, 1, 2)) + 
  ylim(c(1, 3.5)) +
  labs(x = "", y = "Number of sablefish (millions)\n",
       colour = NULL, shape = NULL, linetype = NULL) +
  theme(legend.position = c(.8, .8))

ggsave(paste0("figures/model1_N_retrospective_", FIRST_YEAR, "_", YEAR, ".png"), 
       dpi=300,  height=4, width=7,  units="in")
