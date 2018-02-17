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

# Summary of marking effort 2004 to present. This could be wrong. I tried to
# pull from the original assessments but the numbers Kray used to estimate past
# years were different from the numbers in those years.

read_csv("data/fishery/raw_data/mr_variable_summary.csv") -> mr_summary

# Daily tag accounting data in the fishery, includes catch. Note that many of 
# the comments indicate that not all fish were observed, so be careful using 
# these to estimate mean weight. Filter out language like missed, Missed, not
# counted, dressed, did not observe
read_csv(paste0("data/fishery/nsei_daily_tag_accounting_2004_", YEAR, ".csv")) -> marks

marks %>% 
  filter(!year %in% NO_MARK_SRV) %>% 
  mutate(all_observed = ifelse(
    !grepl(c("Missing|missing|Missed|missed|eastern|Eastern|not counted|Did not observe|did not observe|dressed|Dressed"), comments) & 
                                 observed_flag == "Yes", "Yes", "No"),
    mean_weight = ifelse(all_observed == "Yes", whole_kg/total_obs, NA)) -> marks

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
  select(-mean_weight_bios, -comments) -> marks

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

  # # Estimated total number of fish caught in a trip (total weight of catch divided by mean weight of fish)
  # mutate(est_tot_number = whole_kg / mean_weight,
  #        npue = ifelse
  #        -> marks 

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
