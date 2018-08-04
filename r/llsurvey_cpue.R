# Longline Survey cpue
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2017-10-09

# load ----
source("r/helper.r")
source("r/functions.r")
YEAR <- 2017

# Explore hook standardization relationship ----

# Standardizing number of hooks based off hook spacing (Sigler & Lunsford 2001,
# CJFAS)

# range of hook number seen in fishery/survey
no_hooks <- c(100, 200, 800, 1000, 2000, 4000, 6000, 8000) 
# spacings observed in fishery/survey - in inches (converted to meters in the
# formula below)
hook_space <- seq(20, 120, by = 0.1) 
hk_stand <- expand.grid(no_hooks = no_hooks, hook_space = hook_space)

hk_stand %>% 
  mutate(std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * hook_space*0.0254))
         ) -> hk_stand

ggplot(hk_stand, aes(x = hook_space, y = std_hooks, col = factor(no_hooks))) +
  geom_line(size = 2) +
  xlab("\nHook spacing (inches)") +
  ylab("\nStandardized number of hooks") +
  scale_color_brewer(palette = "Greys", "Number of\nHooks") 

# other, probably clearer way of visualizing this

# range of hook number seen in fishery/survey
no_hooks <- seq(100, 8000, by = 100) 
# spacings observed in fishery/survey - in inches (converted to meters in the
# formula below)
hook_space <- seq(20, 180, by = 30) 
hk_stand <- expand.grid(no_hooks = no_hooks, hook_space = hook_space)

hk_stand %>% 
  mutate(std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * hook_space*0.0254))
         ) -> hk_stand

ggplot(hk_stand, aes(x = no_hooks, y = std_hooks, col = factor(hook_space))) +
  geom_line(size = 2) +
  xlab("\nNumber of hooks") +
  ylab("\nStandardized number of hooks") +
  scale_color_brewer(palette = "Greys", "Hook spacing (in.)") +
  scale_x_continuous(breaks = seq(0, 15000, by = 1000)) +
  scale_y_continuous(breaks = seq(0, 15000, by = 1000)) +
  geom_abline(slope = 1, col = "red", linetype = 2) 

# data -----
srv_cpue <- read_csv(paste0("data/survey/llsrv_cpue_1985_", YEAR, "2.csv"),
                     guess_max = 500000)

glimpse(srv_cpue)

srv_cpue  %>% 
  filter(year >= 1997 & 
           # Mike Vaughn 2018-03-06: Sets (aka subsets with 12 or more invalid hooks are subset condition code "02" or invalid)
           subset_condition_cde != "02") %>% 
  mutate(Year = factor(year),
         Stat = factor(Stat),
         #standardize hook spacing (Sigler & Lunsford 2001, CJFAS) changes in 
         #hook spacing. pers. comm. with aaron.baldwin@alaska.gov: 1995 & 1996 -
         #118 in; 1997 - 72 in.; 1998 & 1999 - 64; 2000-present - 78". This is
         #different from KVK's code (he assumed 3 m before 1997, 2 m in 1997 and
         #after)
         hooks_bare = ifelse(is.na(hooks_bare), 0, hooks_bare),
         hooks_bait = ifelse(is.na(hooks_bait), 0, hooks_bait),
         hook_invalid = ifelse(is.na(hook_invalid), 0, hook_invalid),
         no_hooks = no_hooks - hook_invalid,
         std_hooks = ifelse(year <= 1996, 2.2 * no_hooks * (1 - exp(-0.57 * (118 * 0.0254))),
                            ifelse(year == 1997, 2.2 * no_hooks * (1 - exp(-0.57 * (72 * 0.0254))),
                                   ifelse( year %in% c(1998, 1999), 2.2 * no_hooks * (1 - exp(-0.57 * (64 * 0.0254))),
                                           2.2 * no_hooks * (1 - exp(-0.57 * (78 * 0.0254)))))),
         # std_hooks = ifelse(year < 1997, 2.2 * no_hooks * (1 - exp(-0.57 * 3)),
         #                    2.2 * no_hooks * (1 - exp(-0.57 * 2))),
         # sablefish_retained = ifelse(discard_status_cde == "01", hooks_sablefish, 0),
         # sablefish_retained = hooks_sablefish,
         sablefish_retained = replace(hooks_sablefish, is.na(hooks_sablefish), 0), # make any NAs 0 values
         std_cpue = sablefish_retained/std_hooks #*FLAG* this is NPUE, the fishery is a WPUE
         # raw_cpue = sablefish_retained/no_hooks
  ) -> srv_cpue

hist(srv_cpue$std_cpue)
srv_cpue %>% 
  group_by(year) %>% 
  # mutate(
  #   #mean annual cpue
  #   annual_cpue = mean(NPUE)
  summarise(annual_cpue = round(mean(std_cpue), 2),
         # nn = length(std_cpue),
         sdev = sd(std_cpue),
         # std_error = sdev / sqrt(nn),
         CIupper = annual_cpue + (sdev * 2),
         CIlower = annual_cpue - (sdev * 2)
         ) -> srv_sum

write_csv(srv_sum,
          paste0("output/srvcpue_", min(srv_cpue$year), "_", YEAR, ".csv"))

# figures

axis <- tickr(srv_sum, year, 3)
ggplot(data = srv_sum) +
  # geom_jitter() + 
  geom_point(aes(year, annual_cpue)) +
  geom_line(aes(year, annual_cpue)) +
  # geom_ribbon(aes(year, ymin = CIlower, ymax = CIupper),
  geom_ribbon(aes(year, ymin = annual_cpue - sdev, ymax = annual_cpue + sdev),
              alpha = 0.2, col = "white", fill = "grey") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  lims(y = c(0, 0.4)) +
  ylab("Survey CPUE (number of sablefish per hook)\n") +
  xlab("") #+
  # theme(plot.title = element_text(hjust = .5)) +
  # geom_vline(xintercept = 9.5, linetype = 2, col = "grey")

ggsave("figures/npue_llsrv.png", 
       dpi=300, height=4, width=7, units="in")

