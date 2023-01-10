
# Fishery CPUE for 2020
# Author: Jane Sullivan
# Contact: jane.sullivan@noaa.gov
# Last edited: Apr 29

source("r/helper.r")
source("r/functions.r")

# if(!require("rms"))   install.packages("rms") # simple bootstrap confidence intervals

# Most recent year of data
YEAR <- 2021
lyr<-2021
# Logbook/CPUE data  ----

# Read in data, standardize cpue, etc.
read_csv(paste0("data/fishery/fishery_cpue_1997_2019.csv"), 
         guess_max = 50000) %>% names()

cpue <- read_csv(paste0("data/fishery/raw_data/sablefish_cpue_final_view_2020_nsei.csv"), 
         guess_max = 50000) %>% 
  rename_all(tolower)

names(cpue)
cpue %>% 
  mutate(date = ymd(as.Date(time_set)), #ISO 8601 format
         julian_day = yday(date)) %>% 
  filter(!is.na(date) & !is.na(hook_spacing) & !is.na(sable_lbs_per_set ) &
           # !is.na(start_longitude_decimal_degree) & !is.na(start_latitude_decimal_degrees) & !is.na(soak) & !is.na(depth) &
           !is.na(hook_size) & hook_size != "MIX" &
           # soak > 0 & !is.na(soak) & # soak time in hrs
           julian_day > 226 & # if there were special projects before the fishery opened
           number_of_hooks < 15000 & # 15000 in Kray's scripts - 14370 is the 75th percentile
           # limit analysis to Chatham Strait and Frederick Sounds where the
           # majority of fishing occurs
           g_stat_area %in% c("345603", "345631", "345702",
                       "335701", "345701", "345731", "345803")) %>% 
  mutate(Year = factor(year), 
         Gear = factor(longline_system_code),
         Adfg = factor(adfg_no),
         Stat = fct_relevel(factor(g_stat_area),
                            "345702", "335701", # Frederick Sound
                            # Chatham south to north
                            "345603", "345631", "345701", "345731", "345803"),
         # 01=Conventional, 02=Snap On, 05=Mixed, 06=Autobaiter -> 01, 02, 05
         # show no strong differences. main difference with autobaiters, which
         # have lwr cpue than conventional gear
         Gear = derivedFactor("AB" = Gear == "06",
                              "CS" = Gear %in% c("01","02","05")),
         Hook_size = factor(hook_size),
         # standardize hook spacing (Sigler & Lunsford 2001, CJFAS), 1 m = 39.37 in
         std_hooks = 2.2 * number_of_hooks * (1 - exp(-0.57 * (hook_spacing / 39.37))), 
         std_cpue = sable_lbs_per_set  / std_hooks,
         # dummy varbs, for prediction with random effects
         dum = 1, 
         dumstat = 1) %>% 
  #"sets" (aka effort_no) is the set identifier. Currently Martina's scripts
  #filter out sets that Kamala identifies as halibut target sets. Create a new
  #column that is the total number of sablefish target sets in a trip (trip_no's
  #only unique within a year)
  group_by(year, trip_no) %>% 
  mutate(no_sets = n_distinct(effort_no)) %>% 
  group_by(year) %>% 
  mutate(
    #The number of vessels participating in the fishery has descreased by 50% from
    #1997-2015. create new column is the total number of active vessels
    #participating in a given year
    total_vessels = n_distinct(Adfg),
    # Total unique trips per year
    total_trips = n_distinct(trip_no)) %>% 
  ungroup() -> fsh_cpue

# Consolidation of fishery - number of vessels fishing and total number of trips
# in Chatham over time
# axis <- tickr(fsh_cpue, year, 5)

fsh_cpue %>% 
  select(year, Vessels = total_vessels, Trips = total_trips) %>% 
  gather(Variable, Count, -year) %>% 
  distinct() %>%
  ggplot(aes(x = year, y = Count)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~ Variable, ncol = 1, scales = "free") +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "", y = "") +
  ylim(0, NA) -> trips_vessels
trips_vessels
# ggsave(plot = trips_vessels, paste0("figures/fishery_tripandvessel_trends_1997_", YEAR, ".png"), 
       # dpi=300, height=6, width=5, units="in")

# Nominal CPUE ----

fsh_cpue %>% 
  group_by(year) %>% 
  dplyr::summarise(fsh_cpue = mean(std_cpue),
                   sd = sd(std_cpue),
                   n = length(std_cpue),
                   se = sd / (n ^ (1/2)),
                   var = var(std_cpue),
                   cv = sd / fsh_cpue,
                   upper = fsh_cpue + (2 * se),
                   lower = fsh_cpue - (2 * se)) -> fsh_sum 

fsh_sum %>% print(n=Inf)

# Percent change in fishery nominal cpue compared to a ten year rolling average
fsh_sum %>% 
  filter(year > YEAR - 10) %>% 
  mutate(lt_mean = mean(fsh_cpue),
         perc_change_lt = (fsh_cpue - lt_mean) / lt_mean * 100) 

# Percent change in fishery nominal cpue from last year
fsh_sum %>% 
  filter(year >= YEAR - 1) %>%
  select(year, fsh_cpue) %>% 
  reshape2::dcast("fsh_cpue" ~ year) -> perc_ch

names(perc_ch) <- c("cpue", "last_year", "this_year") 
perc_ch %>% mutate(perc_change_ly = (`this_year` - `last_year`) / `last_year` * 100)

ggplot(fsh_sum) +
  geom_point(aes(year, fsh_cpue)) +
  geom_line(aes(year, fsh_cpue)) +
  geom_ribbon(aes(year, ymin = fsh_cpue - sd, 
                  ymax = fsh_cpue + sd),
              alpha = 0.2,  fill = "grey") +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") 

ggsave(paste0("figures/fshcpue_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

# # Write to file
# write_csv(cpue_ts, paste0("output/fshcpue_", min(cpue_ts$year), "_", YEAR, ".csv"))
# # write_csv(cpue_ts, paste0("output/nominalwpue_var_llfsh_", min(cpue_ts$year), "_", YEAR, ".csv"))

pastcpue <- read_csv("data/tmb_inputs/abd_indices_2020.csv")

pastcpue %>% 
  select(year, fsh_cpue) %>% 
  filter(year >= 1997) %>% 
  mutate(fsh_cpue = fsh_cpue * 2.20462,
         version = "past") %>% 
  bind_rows(fsh_sum %>% 
              select(year,fsh_cpue) %>% 
              mutate(version = "current"))  %>% 
  ggplot(aes(x = year, y = fsh_cpue, col = version, lty = version, shape = version)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Fishery CPUE (round lb per hook)\n") 
  

# length comp densities ----

# Fishery biological data
read_csv(paste0("data/fishery/fishery_bio_2000_", lyr,".csv"), 
         guess_max = 50000) %>%
  mutate(Year = factor(year),
         Sex = factor(Sex)) -> fsh_bio

# Survey biological data
read_csv(paste0("data/survey/llsrv_bio_1988_", lyr,".csv"), 
         guess_max = 50000) %>%
  mutate(Year = factor(year),
         Sex = factor(Sex)) -> srv_bio

# Lengths ----

# Fishery by sex
fsh_bio %>% 
  filter(!is.na(length) & !is.na(Sex)) %>% 
  select(year, Sex, length) %>% 
  mutate(Source = "Fishery") %>% 
  bind_rows(srv_bio %>% 
              filter(!is.na(length) & !is.na(Sex)) %>% 
              select(year, Sex, length) %>% 
              mutate(Source = "Survey")) %>% 
  group_by(Source, year, Sex) %>% 
  summarize(ecdf = 1 - ecdf(length)(63)) %>% 
  group_by(Source, Sex) %>% 
  mutate(std = (ecdf - mean(ecdf)) / sd(ecdf)) %>% 
  ggplot(aes(x = year, y = ecdf, col = Sex, lty = Source, group = interaction(Sex, Source))) +
  # geom_hline(yintercept = 0, lty = 2) +
  geom_line() +
  geom_point() +
  # geom_hline(yintercept = 0, lty = 2) +
  labs(x = "Year", y = "Proportion of fish greater than 63 cm") +
  facet_wrap(~Sex)

# Weight ----
fsh_bio %>% 
  filter(!is.na(weight)) %>% 
  select(year,  weight) %>% 
  mutate(Source = "Fishery") %>% 
  bind_rows(srv_bio %>% 
              filter(!is.na(weight) ) %>% 
              select(year, weight) %>% 
              mutate(Source = "Survey")) %>% 
  filter(year >= 2005) %>%
  group_by(Source, year) %>% 
  summarize(ecdf = (ecdf(weight)(3 / 0.63 / 2.20462)) * 100) %>% 
  group_by(Source) %>% 
  mutate(std = (ecdf - mean(ecdf)) / sd(ecdf),
         mean = mean(ecdf)) %>% 
  ggplot(aes(x = year, y = ecdf,col = Source)) +
  # ggplot(aes(x = year, y = std,lty = Source)) +
  # geom_hline(yintercept = 0, lty = 2, col = "grey") +
  geom_line() +
  geom_point() +
  geom_line(aes(y = mean, col = Source), lty = 2) +
  # labs(x = NULL, y = NULL) +
  labs(x = NULL, y = "Percent under 3 dressed lb")

ggsave("figures/small_fish_2002.png", width = 8, height = 3.5, dpi = 400, units = "in")

# Lengths -----
fsh_bio %>% 
  filter(!is.na(length)) %>% 
  select(year,  length) %>% 
  mutate(Source = "Fishery (landed catch)") %>% 
  bind_rows(srv_bio %>% 
              filter(!is.na(length) ) %>% 
              select(year, length) %>% 
              mutate(Source = "Survey (total catch)")) %>% 
  filter(year >= 2005) %>%
  group_by(Source, year) %>% 
  summarize(ecdf = (ecdf(length)(59))) %>% 
  group_by(Source) %>% 
  mutate(std = (ecdf - mean(ecdf)) / sd(ecdf),
         mean = mean(ecdf)) %>% #print(n=Inf)

  ggplot(aes(x = year, y = ecdf,col = Source, shape = Source)) +
  # ggplot(aes(x = year, y = std,lty = Source)) +
  # geom_hline(yintercept = 0, lty = 2, col = "grey") +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_line(aes(y = mean, col = Source), lty = 2) +
  # labs(x = NULL, y = NULL) +
  labs(x = NULL, y = NULL,
       #y = "Percent\nof fish \u2264 3\ndressed lb", 
       shape = NULL, col = NULL,
       title = "Percent of sablefish \u2264 3 dressed lb") +
  theme(axis.title.y = element_text(angle=0, vjust = 0.5),
        legend.position = c(.2,.8))

ggsave("figures/small_fish_2021.png", width = 8, height = 4, dpi = 400, units = "in")

# Fishery
fsh_bio %>% 
  filter(!is.na(age)) %>% 
  select(year,  age) %>% 
  mutate(Source = "Fishery") %>% 
  bind_rows(srv_bio %>% 
              filter(!is.na(age) ) %>% 
              select(year, age) %>% 
              mutate(Source = "Survey")) %>% 
  filter(year >= 2005) %>%
  group_by(Source, year) %>% 
  summarize(ecdf = (ecdf(age)(6)) * 100) %>% 
  group_by(Source) %>% 
  mutate(std = (ecdf - mean(ecdf)) / sd(ecdf),
         mean = mean(ecdf)) %>% 
  ggplot(aes(x = year, y = ecdf,col = Source)) +
  # ggplot(aes(x = year, y = std,lty = Source)) +
  # geom_hline(yintercept = 0, lty = 2, col = "grey") +
  geom_line() +
  geom_point() +
  geom_line(aes(y = mean, col = Source), lty = 2) +
  # labs(x = NULL, y = NULL) +
  labs(x = NULL, y = "Percent <= 6 yrs old")

ggsave("figures/small_fish_age_2021.png", width = 8, height = 3.5, dpi = 400, units = "in")

fsh_bio %>% 
  filter(!is.na(length)) %>% 
  group_by(year) %>% 
  summarize(ecdf =  ecdf(length)(63)) %>% 
  ungroup() %>% 
  mutate(std = (ecdf - mean(ecdf)) / sd(ecdf)) %>% 
  ggplot(aes(x = year, y = std)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Proportion fish <= 63 cm")
