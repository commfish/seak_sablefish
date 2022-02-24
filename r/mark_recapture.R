# Mark-recapture analysis
# Author: Jane Sullivan
# Contact: jane.sullivan@noaa.gov
# Last edited: Feb 2021

# Conduct MR annual estimates using data >= 2005. 2003 countbacks were partially
# lost, 2004 used PIT tags and batch 15 (2004) recoveries aren't in database.
# Other MR data go back to 1997, but these data haven't been recovered.

# NOTE: The mark-recap models take awhile to run. Allot some time to this.

# Past release: https://github.com/commfish/seak_sablefish/releases/tag/mark-recapture-pre-2019-assessment
# Issue on survey countbacks: https://github.com/commfish/seak_sablefish/issues/39

# Variable definitions ----

# N.0 = number of sablefish in Chatham Strait at time of marking
# K   = number of marks released
# K.0 = number of marks released after tagging of small fish has been accounted for 
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

# Set up ----

source("r/helper.r")
source("r/functions.r")
if(!require("rjags"))   install.packages("rjags") # run jags/bugs models
if(!require("purrr"))   install.packages("purrr") # clean up posterior output

FIRST_YEAR <- 2005 
#Note: no marking in 2021 due to covid 
#'22 analysis of '21 year is just a rerun of 2020 data 
YEAR <- 2021 # Current assessment year, most recent year of data
NO_MARK_SRV <- c(2011, 2014, 2016, 2021) # years without a marking survey

# Past assessments ----

# FLAG TO DO! A summary of past estimated abundance in a given year - I use the
# abundance_age2plus to help inform the prior of the starting abundance (it will
# still be vague - see Priors and starting values section). Note that these
# values aren't very consistent because the MR age2plus abundance is actually
# exploitable not total abundance... some abd are from MR estimates, others are
# from an ASA model or the YPR/SPR model (lots of overturn in biometricians.  Currently I add this value manually
# to the .csv until I can figure out a slicker way to update it each year.
read_csv("data/chatham_sablefish_abd_index.csv") -> assessment_summary
view(assessment_summary)

unique(assessment_summary$year)
# Past years of mark-recapture variables, summarized to best of my ability based
# on going through old Excel files on the server. Used for comparison purposes.
read_csv("data/fishery/raw_data/mr_variable_summary.csv") -> mr_summary
view(mr_summary)
# Released tags ----

# Released fish. Each year has a unique batch_no.

read_csv(paste0("data/survey/tag_releases_2003_", YEAR-1, ".csv"), 
#read_csv(paste0("data/survey/tag_releases_2003_", YEAR, ".csv"), 
         guess_max = 50000) %>% 
  filter(year >= FIRST_YEAR) -> releases
str(releases)

# Check - these should be all tagged and released, which is TRUE pre-2020, but a
# bunch of "retained", "retained; bio sample", and "released; already tagged by
# ADFG" are in the 2020 data. I'm especially confused by the retained tags...
releases %>% filter(year < YEAR) %>% group_by(discard_status) %>% dplyr::summarise(n_distinct(tag_no)) 
releases %>% filter(year == 2020) %>% group_by(discard_status) %>% dplyr::summarise(n_distinct(tag_no)) 
# releases %>% filter(discard_status %in% c("Retained", "Retained; Bio. sample")) %>% write_csv("tag_release_errors_2020.csv") # sent to A Baldwin 20210224
releases <- releases %>% filter(discard_status != "Released; already tagged by ADFG")

# Lookup table for year and tag batch no combos
releases %>% 
  mutate(year_batch = paste0(year, "_", tag_batch_no)) %>% 
  filter(year >= FIRST_YEAR) %>% 
  distinct(year_batch) -> tag_summary

# Recovered tags ----

# Recaptured fish. Match up to the daily tag accounting (countback) data in the
# fishery to determine dead tags, number caught in survey, etc.

read_csv(paste0("data/fishery/tag_recoveries_2003_", YEAR, ".csv"), 
         guess_max = 50000) -> recoveries
unique(recoveries$year)
str(recoveries)

par(mfrow=c(3,1))
hist(recoveries$length); hist(releases$length)
# project codes: query zprod: "select new_project_code, project_code, project from
# lookup.project_conversion where category_code = 'g'"

# Filter out all recoveries that are not from the current year's tag batch
# (i.e., get rid of recovered tags that were deployed in a past year)
recoveries %>% 
  mutate(year_batch = paste0(year, "_", tag_batch_no),
         year_trip = paste0(year, "_", trip_no),
         # use landing_date (same as the countbacks - see section below), otherwise catch_date
         date = as.Date(ifelse(is.na(landing_date), catch_date, landing_date))) %>% 
  filter(year >= FIRST_YEAR & year_batch %in% tag_summary$year_batch) -> recoveries
nrow(recoveries[recoveries$year == 2020,])
# Check for Project_cde NAs and if there are any check with A. Baldwin or
# whoever is leading the tagging project. In the past there have been occasional
# tags that were recovered in the personal use or commercial fishery that were
# left as NAs and need to be fixed
filter(recoveries, is.na(Project_cde)) %>% 
  distinct(year, date, catch_date, landing_date, returned_by, Project_cde, tag_no, comments) #%>% distinct(comments) #write_csv("tag_recovery_issues_2020.csv")

# Project_cde NA that are personal use trips should be code
# "27" - change it to appropriate code
recoveries %>% 
  mutate(Project_cde = ifelse(is.na(Project_cde) &
                                grepl(c("Personal Use|subsistance|sport"), comments), 
                              "27", Project_cde)) -> recoveries
nrow(recoveries[recoveries$year == 2020,])
# Size selectivity differences ----

# Check range of data
releases %>% 
  group_by(year) %>% 
  dplyr::summarize(min = min(length, na.rm = TRUE),
            max = max(length, na.rm = TRUE))

# *FLAG* Contacted A Bladwin 20200124 about T-096264 length = 30 cm. The paper
# form also says it was 30 cm, but he doesn't think they would have tagged a
# fish that small. Don't know how I should treat this... leave it in for now.
releases %>% filter(length <= 30) %>% distinct(year, Project_cde, length, tag_no, release_condition_cde, discard_status) 

unique(recoveries$measurer_type)
with(recoveries, table(year,measurer_type))

recoveries %>% 
  filter(measurer_type == "Scientific staff") %>% 
  group_by(year) %>% 
  dplyr::summarize(min = min(length, na.rm = TRUE),
            max = max(length, na.rm = TRUE))

# Create bins. Used 5-cm bins b/c the data is a little sparse
releases %>% 
  filter(!is.na(length)) %>% 
  mutate(length_bin = cut(length, breaks = seq(32.5, 117.5, 5),
                          labels = paste(seq(35, 115, 5)))) %>% 
  select(year, rel_date = date, rel_stat = Stat, tag_no, tag_batch_no, rel_len = length, 
         rel_bin = length_bin) -> rel_sel
par(mfrow=c(2,1))
hist(releases$length); hist(rel_sel$rel_len)
nrow(releases); nrow(rel_sel)

# Same for recoveries, only use measurements from scientific staff
# pj22: only using sci staff culls 75+% of lengths !!! - OK here bc looking for most reliable
# smallest fish. Mind weather this is an issue below (geographic exam...)
recoveries %>% 
  filter(!is.na(length) &
           measurer_type == "Scientific staff") %>% 
  mutate(length_bin = cut(length, breaks = seq(32.5, 117.5, 5),
                          labels = paste(seq(35, 115, 5)))) %>% 
  select(year, rec_date = date, rec_stat = Stat, tag_no, tag_batch_no, rec_len = length, 
         rec_bin = length_bin) -> rec_sel
hist(recoveries$length); hist(rec_sel$rec_len)
nrow(recoveries); nrow(rec_sel)

#make similar data frame but keep in other measurements where we're examinng non-length
#based assumptions... 
recoveries %>% 
  filter(!is.na(length) & !is.na(Stat)) %>% 
         #  measurer_type == "Scientific staff") %>% 
  mutate(length_bin = cut(length, breaks = seq(32.5, 117.5, 5),
                          labels = paste(seq(35, 115, 5)))) %>% 
  select(year, rec_date = date, rec_stat = Stat, tag_no, tag_batch_no, rec_len = length, 
         rec_bin = length_bin) -> rec_mvt

# Growth in tagged individuals by bin
merge(rel_sel, rec_sel, by = c("year", "tag_no", "tag_batch_no")) %>% 
  mutate(growth = rec_len - rel_len) %>% 
  # lots of negative growth... 
  filter(!growth < 0) -> growth
growth[1:10,]

# Lots of unrealistic outliers, omit growth over 5 cm
# PJ2022, just checking trends... would argue the %increase is better culling metric?
hist(growth$growth, breaks=50)
quantile(growth$growth, c(0.001,0.01,0.05,0.95,0.99,0.999))
plot(growth$growth~growth$rel_len)
growth$perc_inc<-growth$growth/growth$rel_len
hist(growth$perc_inc, breaks=50)
plot(growth$perc_inc~growth$rel_len)
#or a schwaggy look at increase in mass based on cube relationship...
growth <-growth %>%
  mutate(rel_length3 = rel_len^3,
         rec_length3 = rec_len^3,
         growth3 = rec_length3 - rel_length3,
         perc_inc3 = growth3/rel_length3); growth[1,]
quantile(growth$perc_inc3, c(0.001,0.01,0.05,0.95,0.99,0.999))
hist(growth$perc_inc3, breaks=100)

growth %>% 
  filter(growth < 5) %>% 
#  filter(perc_inc3 < 0.25) %>%    #PJ2022 - alternative culling based on % increase in schwaggy mass of over 10%
                                  # gives quite different answers... max growth in larger age size
                                  #thinking this is more realistic/appropriate... 
  arrange(year, rel_bin) %>% 
  group_by(rel_bin) %>% 
  dplyr::summarize(n = length(tag_no),
            mean_g = round(mean(growth), 2),
            max_g = max(growth),
            sd_g = round(sd(growth), 2)) %>% 
  # Deal with outliers in shoulder length bins: if the sample size of less than
  # 25, assume 0 growth
  mutate(g = ifelse(n < 25, 0, mean_g)) %>% 
  select(rel_bin, g) %>% 
  # join back in any bins that did not have growth estimated, assign these as 0
  # growth
  full_join(data.frame(rel_bin = paste0(seq(35, 115, 5)))) -> growth

growth[is.na(growth)] <- 0


# Rearrange
growth %>% 
  mutate(tmp = as.numeric(rel_bin)) %>% 
  arrange(tmp) %>% 
  select(-tmp) -> growth
growth

write_csv(growth, paste0("output/tag_estimated_growth_PJmeth_", min(rel_sel$year), 
                         "_", max(rel_sel$year), ".csv"))

# Add growth to released fish then recalculate the bins
merge(rel_sel, growth, by = "rel_bin") %>%
  mutate(growth_len = rel_len + g,
         growth_bin = cut(growth_len, breaks = seq(32.5, 117.5, 5),
                          labels = paste(seq(35, 115, 5)))) %>% 
  select(year, tag_no, growth_len, growth_bin) -> growth_sel
growth_sel[1:10,]

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
  facet_wrap(~year, ncol = 2, dir = "v") +
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
  facet_wrap(~year, ncol = 2, dir = "v") +
  scale_x_discrete(breaks = seq(40, 110, 10), 
                     labels = seq(40, 110, 10)) +
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
#PJ22 Seems like we might be getting rid of a few too many fish using bins to cull.  Why not use
# raw lengths for the culling part? Redid growth to keep raw lengths in there... 
# using raw lengths actually gets rid of quite a bit more fish... should explore effects on
# estimates...

rec_sel %>% 
  # *FLAG* removing this single tag that NOAA returned to ADFG on 20190328 that
  # is officially the smallest recovered fish that we have since 2005 from the
  # fishery and was recaptured outside of Chatham in 365630.
  filter(!tag_no %in% "T-089116") %>% 
  group_by(year) %>% 
  dplyr::summarize(cutoff = min(as.numeric(as.character(rec_bin)))) %>% 
  right_join(growth_sel, "year") %>% 
  mutate(growth_bin = as.numeric(as.character(growth_bin))) %>% #View()
  filter(growth_bin < cutoff) %>% 
  distinct(year, tag_no) -> throw_out

throw_out %>% group_by(year) %>% dplyr::summarize(n_distinct(tag_no)) # number tags to throw out by year

str(rec_sel)
str(growth_sel)

#do second thow_out using raw lengths as opposed to length bins... 
rec_sel %>% 
  filter(!tag_no %in% "T-089116") %>% 
  group_by(year) %>% 
  dplyr::summarize(cutoff = min(as.numeric(as.character(rec_len)))) %>% 
  right_join(growth_sel, "year") %>% 
  mutate(growth_len = as.numeric(as.character(growth_len))) %>% #View()
  filter(growth_len < cutoff) %>% 
  distinct(year, tag_no) -> throw_out_raw

throw_out_raw %>% group_by(year) %>% dplyr::summarize(n_distinct(tag_no)) # number tags to throw out by year

#Phil's primitive loop to look at this a little closer; had to figure out if I get the same
#results.  Got it now.  
{

small<-data.frame()
YRS<-unique(rec_sel$year)
j<-1
for (i in YRS) {     #i<-2020
  dat<-rec_sel[rec_sel$year == i,]
  dat$rec_bin<-as.numeric(dat$rec_bin)
  dat2<-growth_sel[growth_sel$year == i,]
  dat2$growth_bin<-as.numeric(dat2$growth_bin)
  small[j,"year"]<-i
  small[j,"smallest"]<-min(dat$rec_len)
  small[j,"no.too.small"]<-nrow(dat2[dat2$growth_len < min(dat$rec_len),])
  small[j,"smallest.bin"]<-min(dat$rec_bin) 
  small[j,"no.too.small.bin"]<-nrow(dat2[dat2$growth_bin < min(dat$rec_bin),])
  j<-j+1
}
small #bins mostly matches Jane - except 2018... 
   #however, very different results with raw lengths! yuck...
   #throw out a lot more marks using raw lengths... 

i<-2018
dat<-rec_sel[rec_sel$year == i,]
dat$rec_bin<-as.numeric(as.character(dat$rec_bin))
dat2<-growth_sel[growth_sel$year == i,]
dat2$growth_bin<-as.numeric(as.character(dat2$growth_bin))
par(mfrow=c(2,1))
hist(dat$rec_len); min(dat$rec_len)
#co<-min(as.numeric(as.character(dat$rec_bin)))
hist(dat2$growth_len)
length(dat2$growth_len[dat2$growth_len < min(dat$rec_len)]) #hmmm...need to go back and check how rec_len and growth_len derived...
hist(dat$rec_bin); min(dat$rec_bin)
hist(dat2$growth_bin)
length(dat2$growth_bin[dat2$growth_bin < min(dat$rec_bin)])

dat3<-rec_sel[rec_sel$tag_no != "T-089116" & rec_sel$year == i,]
min(dat3$rec_len)
min(as.numeric(as.character(dat3$rec_bin)))

small2<-data.frame()
YRS<-unique(rec_sel$year)
j<-1
for (i in YRS) {     #i<-2020
  dat<-rec_sel[rec_sel$year == i & rec_sel$tag_no != "T-089116",]
  dat$rec_bin<-as.numeric(dat$rec_bin)
  dat2<-growth_sel[growth_sel$year == i,]
  dat2$growth_bin<-as.numeric(dat2$growth_bin)
  small2[j,"year"]<-i
  small2[j,"smallest"]<-min(dat$rec_len)
  small2[j,"no.too.small"]<-nrow(dat2[dat2$growth_len < min(dat$rec_len),])
  small2[j,"smallest.bin"]<-min(dat$rec_bin) 
  small2[j,"no.too.small.bin"]<-nrow(dat2[dat2$growth_bin < min(dat$rec_bin),])
  j<-j+1
}
small2

to18<-throw_out[throw_out$year == 2018,]
head(to18); nrow(to18)

rec_sel %>% 
  filter(!tag_no %in% "T-089116" & year %in% 2018) %>% 
  group_by(year) %>% 
  dplyr::summarize(cutoff = min(as.numeric(as.character(rec_bin)))) %>% 
  right_join(growth_sel, "year") %>% 
  mutate(growth_bin = as.numeric(as.character(growth_bin))) %>% #View()
  filter(growth_bin < cutoff) %>% 
  distinct(year, tag_no) -> throw_out18
}
# Total number tagged and released by year plus define length of time period 1
releases %>% 
  group_by(year, tag_batch_no) %>% 
  dplyr::summarize(K = n_distinct(tag_no), # total fish tagged
            potsrv_beg = min(date),
            potsrv_end = max(date),
            # Mueter (2007) defined the length of time period 1 (t.1) as
            # the number of days between the middle of the pot survey and the
            # middle of the LL survey, and t.2 as the middle of the LL survey to
            # the end of the first fishing period. We changed this 
            # such that t.1 goes from the middle of the pot survey to the day
            # before the fishery begins. We set it up this way to account for
            # instantaneous natural mortality (and functionally emigration)
            # during this period.
            potsrv_middle = (potsrv_end - potsrv_beg) / 2 + potsrv_beg) %>% 
  mutate(year_batch = paste0(year, "_", tag_batch_no)) -> tag_summary

# Remove tags that fall below the cutoff value to account for size-selectivity 
# reviewed 2-21-22.  Getting rid of too small tags good; Phil approved! 
releases %>%  filter(!tag_no %in% throw_out$tag_no) -> releases_bin
releases %>%  filter(!tag_no %in% throw_out_raw$tag_no) -> releases_raw

str(releases)
# Add these to tag_summary as K.0
releases_bin %>% 
  group_by(year, tag_batch_no) %>% 
  dplyr::summarize(K.0 = n_distinct(tag_no)) %>% left_join(tag_summary) -> tag_summary_bin

releases_raw %>% 
  group_by(year, tag_batch_no) %>% 
  dplyr::summarize(K.0 = n_distinct(tag_no)) %>% left_join(tag_summary) -> tag_summary_raw

#-------------------------------------------------------------------------------
# Movement in Chatham ----
merge(rel_sel, 
#      rec_sel %>%  #nrow(rec_sel)   #why used culled recoveries here???
        #these are culled based on who measured them, but does that effect the movement 
        #piece of the puzzle
     rec_mvt %>% 
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
         # probability of being recaptured in an area if released in an area
         Probability = Freq/N) -> move
str(move)
head(move, 30)

ggplot(move, aes(x = releases, y = recaptures)) +
  geom_tile(aes(fill = Probability), colour = "grey") +
  facet_wrap(~ year, ncol = 2, dir = "v") +
  scale_fill_gradient(low = "white", high = "black", space = "Lab",
                      na.value = "white", guide = "colourbar",
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

# Alternative movement graphic that shows sample size
ggplot(move, aes(x = releases, y = recaptures)) +
  geom_tile(aes(fill = Probability), colour = "grey") +
  geom_point(aes(size = Freq), shape = 21, colour = "black") +
  scale_size(limits = c(1, max(move$Freq)), range = c(0, 4.5)) +  
  facet_wrap(~ year, ncol = 2, dir = "v") +
  labs(x = '\nAge', y = '') +
  # guides(size = FALSE) +
  scale_fill_gradient(low = "white", high = "grey35", space = "Lab",
                      na.value = "white", guide = "colourbar",
                      name = "Proportion") +
  labs(x = "\nRelease Stat Area", y = "Recapture Stat Area\n", size = "No. of\ntagged fish")

ggsave(paste0("figures/movement_matrix_with_N_", 
              FIRST_YEAR, "_", YEAR, ".png"), 
       dpi=300, height=8.5, width=7.5, units="in")
# PJ22: mixing "looks" better when we use all the data (not just scientific samplers).  Still need
# proper diagnostics
#===============================================================================================================
# Recaps and Second Event Sample
# LL survey ----

# 1. Countbacks

# Prior to the 2019 assessment, it was assumed all fish on the LL survey were
# checked for marks. Only 2008 and 2010 surveys had countbacks. See Issue #39
# for documentation.
srv_count <- read_csv("data/survey/nsei_sable_llsurvey_countbacks.csv")

# !!! clarify which valid mark data frame you are using, raw or binned length culling...
tag_summary<-tag_summary_bin  #tag_summary_raw

# Expand grid, sum within years, and apply 0s for NAs
srv_count %>% 
  group_by(year) %>% 
  dplyr::summarize(n.1 = sum(number_unmarked, number_marked),  # number checked for marks
            k.1 = sum(number_marked)) %>%  # number marked
  right_join(data.frame(year = FIRST_YEAR:YEAR)) %>% 
  mutate(n.1 = ifelse(is.na(n.1), 0, n.1),
         k.1 = ifelse(is.na(k.1), 0, k.1)) %>% 
  right_join(tag_summary, by = "year") -> tag_summary  

# 2. Recovered tags in LL survey

# Add LL survey recoveries to the summary (survey is time period i = 1)
head(recoveries)
recoveries %>% 
  filter(Project_cde == "03") %>% 
  group_by(year, tag_batch_no) %>%
  dplyr::summarize(D.1 = n_distinct(tag_no)) %>% # number of tags to remove before fishery starts = tags removed in ll survey
  left_join(tag_summary, by = c("year", "tag_batch_no")) -> tag_summary

# Remove tags recovered in the survey from the releases df so they don't get
# double-counted by accident.
# pj22: Jane does this at each step of accounting for recaptures.
#save recoveries for diagnostics before we start stripping this data away
all_recoveries<-recoveries

nrow(recoveries); head(recoveries)
unique(recoveries$Project_cde)
recoveries %>% filter(Project_cde != "03") -> recoveries
nrow(recoveries2)

# 3. LL survey catch in numbers (C.1)

read_csv(paste0("data/survey/llsrv_by_condition_1988_", YEAR, ".csv"), guess_max = 50000) %>% 
  filter(year >= FIRST_YEAR) %>% 
  mutate(sablefish_retained = ifelse(discard_status_cde == "01", hooks_sablefish, 0)) %>% 
  group_by(year) %>% 
  dplyr::summarize(llsrv_beg = min(date),
            llsrv_end = max(date),
            C.1 = sum(hooks_sablefish)) %>% # catch in numbers
  right_join(tag_summary, by = "year") -> tag_summary   

# 4. LL survey mean weight

read_csv(paste0("data/survey/llsrv_bio_1988_", YEAR,".csv"), 
         guess_max = 50000) %>% 
  filter(year >= FIRST_YEAR & !is.na(weight)) %>% 
  group_by(year) %>% 
  dplyr::summarize(mean_weight_1 = mean(weight)) -> srv_mean_weights

#--------------------------------------------------------------------------
# LL fishery ----

# 1. Fish tickets

# From IFDB database. These will help us check the countback daily accounting
# sheets and also account for fishing mortality in the LL survey.

read_csv(paste0("data/fishery/nseiharvest_ifdb_1985_", YEAR,".csv"), 
         guess_max = 50000) %>% 
  filter(year >= FIRST_YEAR) %>% 
  mutate(whole_kg = whole_pounds * 0.453592,
         year_trip = paste0(year, "_", trip_no)) -> fsh_tx
str(fsh_tx)

# 2. Fishery countbacks   

# Daily tag accounting data in the fishery, includes catch. Note that many of
# the comments indicate that not all fish were observed, so be careful using
# these to estimate mean weight. Filter out language like missed, Missed, not
# counted, did not observe. There is no difference between dressed and round fish for
# detecting marks, but we shouldn't be using it for estimating weight.

#PJ22!! Note that no survey in '21 so YEAR-1 below; change after next abundance estiamte
#read_csv(paste0("data/fishery/nsei_daily_tag_accounting_2004_", YEAR, ".csv")) -> marks
read_csv(paste0("data/fishery/nsei_daily_tag_accounting_2004_", YEAR-1, ".csv")) -> marks  #marks = recaps
view(marks)
marks$marked
marks$unmarked

marks %>% 
  filter(year >= FIRST_YEAR &
    !year %in% NO_MARK_SRV) %>% 
  mutate(all_observed = ifelse(
    !grepl(c("Missing|missing|Missed|missed|eastern|Eastern|not counted|
             Did not observe|did not observe|dressed|Dressed"), comments) & 
                                 observed_flag == "Yes", "Yes", "No"),
    mean_weight = ifelse(all_observed == "Yes", whole_kg/total_obs, NA),
    year_trip = paste0(year, "_", trip_no)) -> marks
nrow(marks)

# 3. Fishery mean weight

# Biological data to get mean weight to get numbers estimated on unobserved catch. 
read_csv(paste0("data/fishery/fishery_bio_2000_", YEAR,".csv"), 
         guess_max = 50000) %>%
  filter(!is.na(weight)) %>% 
  mutate(date = ymd(as.Date(date, "%m/%d/%Y"))) %>% 
  select(date, trip_no, weight) %>% 
  group_by(date, trip_no) %>% 
  dplyr::summarize(mean_weight_bios = mean(weight)) -> fsh_bio
view(fsh_bio)

# Join the mark sampling with the biological sampling. If random bio samples
# were taken, use those as the mean weight, if not use the estimated mean weight
# from the total weight of the catch and the number of fish sampled
left_join(marks, fsh_bio, by = c("date", "trip_no")) %>% 
  mutate(mean_weight = ifelse(!is.na(mean_weight_bios), mean_weight_bios, mean_weight)) %>% 
  select(-mean_weight_bios) -> marks

head(marks, 10)

#hmmm... looks like if there are marks there are unmarked... 
nrow(marks)
nrow(marks[!is.na(marks$marked) & is.na(marks$unmarked),])
nrow(marks[is.na(marks$marked),])
nrow(marks[is.na(marks$unmarked),])
nrow(marks[marks$observed_flag == "No",])
nrow(marks[marks$all_observed == "No",])
nrow(marks[marks$all_observed == "Yes",])
866+557
# 4. Fishery CPUE

# FLAG! Starting with 2020 data, you will get an error here. In 2020, the
# Groundfish Project embarked on a fishery CPUE revamp, including a new data
# entry application and data re-entry project. New scripts will need to be
# developed to match fishery logbooks to fish tickets, allocating poundage to
# the numbers typically reported on logbooks. This is a big, but low priority
# project. As a stop gap in 2020 and until the new fishery CPUE data can be
# developed, use mean fishery CPUE for the entire time period. The model that
# uses this index doesn't end up getting used for management, so again this is
# low priority.

# CPUE data - use nominal CPUE for now
#read_csv(paste0("data/fishery/fishery_cpue_1997_", YEAR,".csv"), 
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
  group_by(year, trip_no) %>% 
  dplyr::summarize(WPUE = mean(WPUE)) -> fsh_cpue # legacy code, YOU WILL GET AN ERROR!
#PJ2022: Fixed and good to go!  
str(fsh_cpue)
unique(fsh_cpue$year) # !FLAG! Received from Justin 1/31/22 
                      #once have that data can get rid of code below 

# Stop gap: use long term (1997-2019) mean for interim years (2020-whenever
# fishery CPUE project is finished). Remove the following code chunks once the
# new fishery CPUE index is developed:
#read_csv(paste0("data/fishery/fishery_cpue_1997_2019.csv"), 
#         guess_max = 50000) %>% 
#  filter(Spp_cde == "710") %>% 
#  mutate(sable_kg_set = sable_lbs_set * 0.45359237, # conversion lb to kg
#         std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (0.0254 * hook_space))), #standardize hook spacing (Sigler & Lunsford 2001, CJFAS)
         # kg sablefish/1000 hooks, following Mueter 2007
#         WPUE = sable_kg_set / (std_hooks / 1000)) %>% 
#  filter(!is.na(date) & 
#           !is.na(sable_lbs_set) &
           # omit special projects before/after fishery
#           julian_day > 226 & julian_day < 322) %>% 
#  group_by(year, trip_no) %>% 
#  dplyr::summarize(WPUE = mean(WPUE)) -> fsh_cpue #

# here I fill in all trips in 2020 (or later) with the long-term fishery CPUE
# mean. this will need to be deleted at some point!
#fsh_cpue <- marks %>% 
#  filter(date > 2020-01-01) %>% 
#  mutate(year = year(date)) %>% 
#  select(year, trip_no) %>% 
#  mutate(WPUE = mean(fsh_cpue$WPUE, na.rm = TRUE)) %>% 
#  bind_rows(fsh_cpue) %>% 
#  arrange(year)
  
# Join the mark sampling with the fishery cpue - add wpue column
head(marks,20)
left_join(marks, fsh_cpue, by = c("year", "trip_no")) -> marks2
view(marks2)

# 5. Timing of the fishery
marks<-marks2
marks %>% 
  group_by(year) %>% 
  dplyr::summarize(fishery_beg = min(date),
            fishery_end = max(date)) %>% 
  left_join(tag_summary, by = "year") %>% 
  # See earlier notes on changes in methods from Franz Mueter in the Released
  # tags section - number of days to incur natural mortality
  mutate(t.1 = as.numeric((fishery_beg - 1) - potsrv_middle)) -> tag_summary

unique(tag_summary$year)
# 6. Tags from fishery

# Join the recovered trips from a specific year-trip combination to the daily
# accounting dataframe. This will ultimately go into a D calculation. IMPORTANT
# NOTE from Mike Vaughn 2018-02-22: trip_nos are year and project specific.
# Canadian-recovered tags get a trip_no if there was an otolith collected.
unique(recoveries$Project_cde)
#!!!FLAG - in coming years make sure 602 accounted for, new standard.  Set up code
# currently keep 602 -> 02 to keep consistent with Jane's code.  But something to 
# be mindful of in coming years

recoveries %>% 
  filter(year_trip %in% marks$year_trip & Project_cde == "02") %>%  
  group_by(year_trip) %>% 
  dplyr::summarize(tags_from_fishery = n_distinct(tag_no)) %>% 
  right_join(marks, by = "year_trip") -> marks

unique(recoveries$year)
unique(marks$year)
# Remove these matched tags from the releases df so they don't get double-counted by accident. 
# pj22 - same as other removal; Jane says releases df, but its not in here??? 
# from recoveries data frame, remove as you go ... 
nrow(recoveries)
recoveries %>% filter(!c(year_trip %in% marks$year_trip & Project_cde == "02")) -> recoveries
str(recoveries)

# See Issue 41 - fishery recovered tag discrepancies:
# https://github.com/commfish/seak_sablefish/issues/41. Some year_trip fishery
# combos in recoveries df are not in marks df. These will be tallied with other
# tags recovered during the fishery (part 2 of Daily summary). This issue is
# tracked but will likely never be resolved. 
anti_join(recoveries %>% 
            filter(!trip_no %in% c(1, 2, 3) &  # pot and longline surveys
                     !is.na(trip_no) & 
                     Project_cde == "02"), 
          marks, by = "year_trip") %>% 
  group_by(year_trip, Mgmt_area, date) %>% 
  dplyr::summarize(tags_from_fishery = n_distinct(tag_no)) -> no_match 
# 7 trips from 2008, all in the NSEI; 3 from 2018; 1 trips in 2020... 2020_116
# was part a tendered delivery, so no surprise it was weird.

#--------------------------------------------------------------------------------
# Daily summary ----

# 1. Summary of observed (n), marks[recaps] (k), and recovered tags by day during the
# directed NSEI season.
# PJ!!! here is where some numbers are interpolated from weight data !!! 
# this could make length based stratification problematic... will need to think on this
# will need to interpolate length distributions ... yucko maybe
view(marks)

marks %>% 
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
         julian_day = yday(date)) -> daily_marks

view(daily_marks)
# 2. Other recovered tags from the fishery (D)

# Join remaining recovered tags that have a matching data with the daily marks 
# df. These could be from other fisheries in or outside the NSEI or from Canada.
# Get fishery_D, the final count of tags to be decremented for the next time
# period.
# PJnote to self; removing marks that are recovered elsewhere
recoveries %>% 
  filter(date %in% daily_marks$date) %>% 
  group_by(date) %>% 
  dplyr::summarize(other_tags = n_distinct(tag_no)) %>% 
  right_join(daily_marks, by = "date") %>% 
  # padr::fill_ replaces NAs with 0 for specified cols
  fill_by_value(other_tags, value = 0) %>% 
  # If tags observed in countback (tags_from_fishery?) > marks (total_marked), 
  # the D for the day is the difference
  # between tags and marks plus other tags from the recoveries df that haven't
  # been accounted for yet. This assumes any tags recovered at time of countback
  # are potentially linked with a marked fish.
  mutate(fishery_D = ifelse(tags_from_fishery - total_marked > 0, 
                            tags_from_fishery - total_marked + other_tags, 
                            other_tags)) -> daily_marks
view(daily_marks)
# Remove these matched tags from the releases df so they don't get double-counted by accident. 
recoveries %>% filter(!c(date %in% daily_marks$date)) -> recoveries

# Remaining tag loss ----

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
  select(-D.1.x, -D.1.y) -> tag_summary

# Currently n.1 and k.1 reflect observed and marked in the longline survey. Add
# similar columns for the fishery
daily_marks %>% 
  group_by(year) %>% 
  dplyr::summarize(k_fishery = sum(total_marked),
            n_fishery = sum(total_obs),
            D_fishery = sum(fishery_D)) %>% 
  left_join(tag_summary) -> tag_summary

view(tag_summary)
#=======================================================================================================
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
  facet_wrap(~ year, dir = "v") +
  labs(x = "Julian Day", y = "Mean Individual Weight (kg)")

ggplot(daily_marks,
       aes(x = julian_day, y = mean_npue)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~ year, scales = "free", ncol = 2, dir = "v") +
  labs(x = "Julian Day", y = "Number sablefish per 1000 hooks\n")

#*** If doing PJ diagnostics here is where you go to MR_exam_code_pj22.R

#===============================================================================================================
#model prep

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
# end model loop
#======================================================================================

#Calls rbind on each list item to convert the list of dfs into one long df
N_summary <- do.call("rbind", N_summary_ls); view(N_summary)
dic_summary <- do.call("rbind", dic_ls)
convergence_summary <- do.call("rbind", converge_ls)

# Save relevant files for later use - with 50K iterations and the default
# thinning rate it takes about an hour to run.

save(list = c("dic_summary", "N_summary", "convergence_summary",
              "mod1_posterior_ls", "mod2_posterior_ls", 
              "mod3_posterior_ls", "mod4_posterior_ls",
              "data_ls", "data_df"),
     file = paste0("output/mark_recap_model_selection_", YEAR, ".Rdata"))

# Model selection ----

load(paste0("output/mark_recap_model_selection_", YEAR, ".Rdata"))

convergence_summary %>% 
  mutate(mod_version = paste(year, model, P, sep = "_")) %>% 
  filter(# Convergance diagnostic: A factor of 1 means that between variance and within chain
    # variance are equal, larger values mean that there is still a notable
    # difference between chains. General rule: everything below 1.1 or so
    # is ok.
    Point.est. >= 1.1) -> not_converged

not_converged %>% distinct(model, P) # do not include models that estimate q unless they have > 6 periods

# Models with P>=6
dic_summary %>% 
#  filter(P >= 6 & year == YEAR) %>% 
  filter(P >= 6 & year == YEAR-1) %>%
  group_by(year) %>% 
  mutate(min_DIC = min(DIC)) %>% 
  ungroup() %>% 
  mutate(delta_DIC = round(DIC - min_DIC, 2),
         mod_version = paste(year, model, P, sep = "_")) %>%
  arrange(year, delta_DIC) %>% 
  group_by(model) %>% 
  filter(between(delta_DIC, min(delta_DIC), min(delta_DIC) + 2)) -> top_models
  # filter(delta_DIC == min(delta_DIC)) -> top_models

bind_rows(mod1_posterior_ls[[5]] %>% select(N.avg, year, model, P),
          mod2_posterior_ls[[5]] %>% select(N.avg, year, model, P),
          mod3_posterior_ls[[5]] %>% select(N.avg, year, model, P),
          mod4_posterior_ls[[5]] %>% select(N.avg, year, model, P)) %>% 
  group_by(model, year, P) %>% 
  # mutate(N.avg = N.avg / 1000000) %>% 
  dplyr::summarize(mean = mean(N.avg),
            q025 = quantile(N.avg, 0.025),
            q975 = quantile(N.avg, 0.975)) -> N_summary 
view(N_summary)

N_summary %>% filter(model == "Model1") %>% write_csv(paste0("output/N_summary_tagsremoved_", YEAR, ".csv"))

N_summary %>% 
  mutate(mod_version = paste(year, model, P, sep = "_"),
         mod_version2 = paste(model, P, sep = "_")) -> N_summary

N_summary %>% 
  filter(mod_version %in% top_models$mod_version) %>% 
  left_join(top_models, by = c("year", "model", "P", "mod_version")) %>% 
  select(year, model, P, Estimate = mean, q025, q975, deviance, parameter_penalty, delta_DIC) -> top_models 
  
tag_summary %>% 
  mutate(K = K.0 - D.0 - D.1, # marks released & available to NSEI survey and fishery
         n = n.1 + n_fishery, # fish examined for marks in NSEI survey and fishery
         k = k.1 + k_fishery, # fish caught with marks in NSEI survey and fishery
         Estimate = ( (K + 1) * (n + 1) /  (k + 1) ) - 1, # Simple Chapman
         var = ( (K + 1) * (n + 1) * (K - k) * (n - k) ) / ( (k + 1)^2 * (k + 2) ), # variance
         q975 = Estimate + 1.965 * sqrt(var), 
         q025 = Estimate - 1.965 * sqrt(var),
         model = "Model0",
         P = 1) %>% 
  select(year, model, P, Estimate, q975, q025) %>% 
  filter(year == YEAR) %>% 
  full_join(top_models) %>% 
  write_csv(paste0("output/top_models_", YEAR, ".csv"))

# ggplot() +
#   geom_histogram(data = N_summary %>% 
#                    # leave out models that did not converge
#                    filter(!mod_version %in% not_converged$mod_version), 
#                  aes(median),  binwidth = 0.05) +
#   # geom_vline(data = N_summary %>%
#   #              filter(mod_version2 %in% top_mods2$mod_version2),
#   #            aes(colour = model, linetype = model,
#   #                xintercept = N.avg),
#   #            size = 1) +
#   facet_wrap(~year, ncol = 2) +
#   labs(x = "\nAbundance estimate (millions)", 
#        y = "Frequency\n") +
#   scale_color_brewer(palette = "BrBG") +
#   theme(legend.position = "bottom",
#         legend.title = element_blank(),
#         legend.text = element_text(size = 14))
# 
# # Histogram shows point estimates for all model versions (P = 2-9), except the
# # ones that did not converge. Coloured vertical bars show point estimate of
# # "best fit" model by DIC.
# ggsave(paste0("figures/Nest_range_histogram", 
#               FIRST_YEAR, "_", YEAR, ".png"), 
#        dpi=300, height=8.5, width=7.5, units="in")

# Summary of posterior distributions of quantities of interest, including
# period-specific estimates of N (N[]), estimates of mean abundance (N.avg), net
# migration (r) which can be positive or negative to indicate net immigration or
# emigration, respectively, and catchability (q): map() applies the summary
# functions after the ~ to each df in the list. do.call() rbinds the output
# together. Repeat this for each model and rbind into one summary df.
bind_rows(
  do.call("rbind", map(mod1_posterior_ls, 
                       ~select(.x, year, P, contains("N["), N.avg) %>%
                         melt(id.vars = c("year", "P")) %>% 
                         group_by(year, P, variable) %>% 
                         dplyr::summarize(median = median(value),
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
                         dplyr::summarize(median = median(value),
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
                         dplyr::summarize(median = median(value),
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
                         dplyr::summarize(median = median(value),
                                   sd = sd(value),
                                   q025 = quantile(value, 0.025),
                                   q975 = quantile(value, 0.975)))) %>% 
    arrange(year, variable) %>% 
    mutate(model = "Model4",
           mod_version = paste(year, model, P, sep = "_"))) -> post_sums

# Model 1 P = 6 Results ----

# After reviewing all models for several years, Model 1 with 6 time periods
# consistently was the "best model" used for management. See the 2018 assessment
# for more details on model selection. This can be revisited periodically but to
# streamline the assessment process, I've put the code to output Model 1 P=6 up
# front.
results <- mod1_posterior_ls[[5]]
model_years <- unique(tag_summary$year)

write_csv(results, paste0("output/final_mod_posterior_", 
                          min(results$year), "_", max(results$year), ".csv"))

# Credibility intervals N and p, N by time period
df <- data.frame(year = FIRST_YEAR:YEAR)
# axis <- tickr(df, year, 2)

results %>% 
  gather("time_period", "N.avg", contains("N.avg")) %>% 
  group_by(year, time_period) %>% 
  dplyr::summarize(`Current estimate` = median(N.avg),
                   q025 = quantile(N.avg, 0.025),
                   q975 = quantile(N.avg, 0.975)) %>% 
  arrange(year, time_period) %>% 
  ungroup() %>% 
  mutate(year = as.Date(as.character(year), format = "%Y")) %>% 
  pad(interval = "year") %>% 
  mutate(year = year(year),
         Year = factor(year)) %>%
  left_join(assessment_summary %>%
              select(year, `Previous estimate` = abundance_age2plus) %>%
              mutate(`Previous estimate` = ifelse(year == YEAR, NA, `Previous estimate`)),
            by = "year") %>%
  gather("Abundance", "N", c(`Current estimate`, `Previous estimate`)) %>% 
  mutate(N = N / 1e6,
         # interpolate the CI in missing years for plotting purposes
         q025 = zoo::na.approx(q025 / 1e6, maxgap = 20, rule = 2),
         q975 = zoo::na.approx(q975 / 1e6, maxgap = 20, rule = 2)) -> df
view(df)

ggplot(df) +
  geom_point(aes(x = year, y = N, col = Abundance, shape = Abundance), 
             size = 1.5) +
  geom_line(data = df %>% filter(!is.na(N)), 
            aes(x = year, y = N, col = Abundance, linetype = Abundance)) +
  geom_ribbon(aes(x = year, ymin = q025, ymax = q975), 
              alpha = 0.2, fill = "grey") +
  scale_x_continuous(breaks = seq(min(model_years), max(model_years), 2), 
                     labels = seq(min(model_years), max(model_years), 2)) +
  geom_point(data = assessment_summary %>%
               filter(year %in% c(YEAR)) %>%
               mutate(est = abundance_age2plus / 1e6),
             aes(x = year, y = est),
             shape = 8, size = 1.5, colour = "grey") +
  scale_colour_grey() +
  ylim(c(1, 4.5)) +
  labs(x = "", y = "Number of sablefish (millions)\n",
       colour = NULL, shape = NULL, linetype = NULL) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  theme(legend.position = c(.7, .9))

ggsave(paste0("figures/model1_N_retro_noforec_",
              FIRST_YEAR, "_", YEAR, ".png"),
       dpi=300, height=5, width=8, units="in")

# Write results to file for ASA
results %>% 
  gather("time_period", "N.avg", contains("N.avg")) %>% 
  group_by(year) %>% 
  dplyr::summarize(estimate = mean(N.avg) / 1e6,
                   sd = sd(N.avg) / 1e6,
                   q025 = quantile(N.avg, 0.025) / 1e6,
                   q975 = quantile(N.avg, 0.975) / 1e6) %>% 
  write_csv("output/mr_index_newsrvcountbackassumptions.csv")

# Compare new and old results for survey countback assumptions 
assumptions <- read_csv("output/mr_index_newsrvcountbackassumptions.csv") %>% 
  mutate(`Assumptions for survey countbacks` = "New (fish only checked for marks in 2008 and 2010)") %>% 
  bind_rows(read_csv("output/mr_index.csv") %>% 
              mutate(`Assumptions for survey countbacks` = "Old (all fish checked for marks)"))

ggplot(assumptions, aes(x = factor(year), y = estimate, fill = `Assumptions for survey countbacks`)) +
  scale_fill_manual(values = c("grey90", "darkgrey")) +
  geom_bar(position = position_dodge(), stat = "identity", col = "black") +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.2, position = position_dodge(.9)) +
  labs(x = NULL, y = "Number of sablefish in millions") +
  theme(legend.position = "top") +
  guides(fill = guide_legend(nrow = 2, byrow=TRUE))

ggsave(paste0("figures/llsrv_countback_assumptions_",
              FIRST_YEAR, "_", YEAR, ".png"),
       dpi=300, height=4, width=6, units="in")

results %>% 
  gather("time_period", "N.avg", contains("N.avg")) %>% 
  group_by(year) %>% 
  dplyr::summarize(estimate = mean(N.avg) / 1e6,
                   sd = sd(N.avg) / 1e6,
                   q025 = quantile(N.avg, 0.025) / 1e6,
                   q975 = quantile(N.avg, 0.975) / 1e6) %>% 
  write_csv(paste0("output/mr_index_", YEAR, ".csv"))

# Posterior distributions from the last several years with MR data
results %>% 
  filter(year > 2012) %>%
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
  facet_wrap(~ year, dir = "v") +
  labs(x = "Number of sablefish in millions",
       y = "Posterior distribution")

results %>% 
  group_by(year) %>% 
  dplyr::summarize(N_current = mean(N.avg) ,
                   q025 = quantile(N.avg, 0.025),
                   q975 = quantile(N.avg, 0.975))  %>% 
  left_join(assessment_summary) -> assessment_summary

write_csv(assessment_summary, paste0("output/assessment_summary_", YEAR, ".csv"))

results %>% 
  group_by(year) %>% 
  dplyr::summarize(N = mean(N.avg) ,
                   q025 = quantile(N.avg, 0.025),
                   q975 = quantile(N.avg, 0.975)) -> ci

ci[c(2:4)] <- lapply(ci[,c(2:4)], prettyNum, trim=TRUE, big.mark=",")

ci %>% #filter(year == YEAR) %>% 
  select(`Estimate N` = N, `Lower CI` = q025, 
         `Upper CI` = q975) %>% 
  kable()

# Tag summary

tag_summary %>% 
  select(Year = year, `$K$` = K, `$K_0$` = K.0, `$D_0$` = D.0, 
         `$k_{srv}$` = k.1, `$n_{srv}$` = n.1, `$D_{srv}$` = D.1,
         `$k_{fsh}$` = k_fishery, `$n_{fsh}$` = n_fishery, `$D_{fsh}$` = D_fishery) %>% 
  write_csv(paste0("output/tag_summary_report_", YEAR, ".csv"))

# Examination of other models ----

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
  facet_wrap(~ year, ncol = 2, dir = "v")

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
# as the within model examination... more striking are the consistent between
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
  facet_wrap(~ year, ncol = 2, dir = "v")

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
  facet_wrap(~ year, ncol = 2, dir = "v")

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
  facet_wrap(~ year, ncol = 2, dir = "v")

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
  facet_wrap(~ year, ncol = 2, dir = "v")

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
  facet_wrap(~ year, ncol = 2, dir = "v")

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

#now just looking at model2?  -pj2022
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
  facet_wrap(~ year, ncol = 2, dir = "v") +
  labs(x = "\nNet migration of sablefish (x 1000)",
       y = "\nPosterior distribution") +
  xlim(c(-30, 40))

ggsave(paste0("figures/net_migration_estimates", YEAR, ".png"), 
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

# Plot catchability and obs vs. fitted NPUE
# results <- mod3_posterior_ls[[5]]
results <- mod4_posterior_ls[[5]]

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
  facet_wrap(~ year, ncol = 2, dir = "v") +
  labs(x = "Catchability (q)",
       y = "Posterior distribution") +
  xlim(c(0.00002, 0.00009))

results %>% 
  group_by(year) %>% 
  dplyr::summarize(median = median(q),
            q025 = quantile(q, 0.025),
            q975 = quantile(q, 0.975))

data_df[[5]] %>% 
  select(year, P = catch_strata, NPUE) %>% 
  mutate(Type = "Observed") -> NPUE

results %>% 
  select(year, P, contains("npue.hat")) %>% 
  reshape2::melt(id.vars = c("year", "P")) %>% 
  group_by(year, variable) %>% 
  dplyr::summarize(NPUE = median(value),
            q025 = quantile(value, 0.025),
            q975 = quantile(value, 0.975)) %>% 
  mutate(Type = "Estimated",
         P = factor(variable,
                    levels = c("npue.hat[2]",
                               "npue.hat[3]",
                               "npue.hat[4]",
                               "npue.hat[5]",
                               "npue.hat[6]"),
                    labels = c("2", "3", "4", "5", "6"),
                    ordered = TRUE)) %>% 

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
  facet_wrap(~ year, ncol = 2, dir = "v") +
  labs(x = "\nTime period",
       y = "CPUE (sablefish per 1000 hooks)\n") +
  theme(legend.position = "none")

ggsave(paste0("figures/NPUE_obsvsfitted_mod3_", YEAR, ".png"),
       dpi=300, height=8.5, width=7.5, units="in")

ggsave(paste0("figures/NPUE_obsvsfitted_mod4_", YEAR, ".png"), 
       dpi=300, height=8.5, width=7.5, units="in")
                        
# In 2018 the models with NPUE only fit the data well when migration was included:

#NPUE <- mod3_posterior_ls[[5]] %>% filter(year == YEAR) %>% 
#  bind_rows(mod4_posterior_ls[[5]] %>% filter(year == YEAR)) %>% 
NPUE <- mod3_posterior_ls[[5]] %>% filter(year == YEAR-1) %>% 
  bind_rows(mod4_posterior_ls[[5]] %>% filter(year == YEAR-1)) %>% 
  select(model, year, P, contains("npue.hat")) %>% 
  reshape2::melt(id.vars = c("model", "year", "P")) %>% 
  group_by(model, year, variable) %>% 
  dplyr::summarize(NPUE = median(value),
            q025 = quantile(value, 0.025),
            q975 = quantile(value, 0.975)) %>% 
  mutate(Type = "Estimated",
         P = factor(variable,
                    levels = c("npue.hat[2]",
                               "npue.hat[3]",
                               "npue.hat[4]",
                               "npue.hat[5]",
                               "npue.hat[6]"),
                    labels = c("2", "3", "4", "5", "6"),
                    ordered = TRUE),
         Model = factor(model,
                        labels = c("Model 3 (includes fishery CPUE)",
                                   "Model 4 (includes fishery CPUE and migration)"),
                        levels = c("Model3", "Model4"))) %>% 
                        # `Model 3 (includes fishery CPUE)` = "Model3",
                        # `Model 4 (includes fishery CPUE and migration)` = "Model4")) %>% 
  select(-variable)

NPUE_dat <- data_df[[5]] %>% 
#  filter(year == YEAR) %>% 
  filter(year == YEAR-1) %>%   #YEAR-1 for Phil's 2022 review
  select(P = catch_strata, NPUE)

ggplot() +
  geom_point(data = NPUE_dat,
             aes(x = P, y = NPUE)) +
  geom_smooth(data = NPUE %>% filter(Type == "Estimated"),
              aes(x = P, y = NPUE, group = Type), colour = "grey") +
  geom_ribbon(data = NPUE %>% filter(Type == "Estimated"),
              aes(x = P, ymin = q025, ymax = q975, 
                  group = Type), 
              alpha = 0.2, fill = "grey") +
  scale_colour_manual(values = c("grey", "black")) +
  facet_wrap(~ Model, ncol = 1) +
  labs(x = "\nTime period",
       y = "CPUE (sablefish per 1000 hooks)\n") +
  theme(legend.position = "none")

ggsave(paste0("figures/NPUE_mod3_vs_mod4_", YEAR, ".png"), 
       dpi=300, height=4, width=6, units="in")

