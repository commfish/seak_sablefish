# Compare 2021 projections using the 2019 and 2020 assessment models.

compare_dir <- file.path(root, "output/tmb/compare_mods_2020") #
source("r/helper.R")

# Data ----
deriv <- read_csv(paste0(compare_dir, "/2019am/derived_ts_imperial_2020.csv")) %>% 
  mutate(model = "2019_assess_mod") %>% 
  bind_rows(read_csv(paste0(compare_dir, "/2020am/derived_ts_imperial_2020.csv")) %>% 
              mutate(model = "2020_assess_mod"))

rec <- read_csv(paste0(compare_dir, "/2019am/pred_rec_2020.csv")) %>% 
  mutate(model = "2019_assess_mod") %>% 
  bind_rows(read_csv(paste0(compare_dir, "/2020am/pred_rec_2020.csv")) %>% 
              mutate(model = "2020_assess_mod"))

ts <- read_csv(paste0(compare_dir, "/2019am/ts_pred_imperial_2020.csv")) %>% 
  mutate(model = "2019_assess_mod") %>% 
  bind_rows(read_csv(paste0(compare_dir, "/2020am/ts_pred_imperial_2020.csv")) %>% 
              mutate(model = "2020_assess_mod"))

res <- read_csv(paste0(compare_dir, "/2019am/abc_wastage_2020.csv")) %>% 
  mutate(model = "2019_assess_mod") %>% 
  bind_rows(read_csv(paste0(compare_dir, "/2020am/abc_wastage_2020.csv")) %>% 
              mutate(model = "2020_assess_mod"))

slx <- read_csv(paste0(compare_dir, "/2019am/selectivity_2020.csv")) %>% 
  mutate(model = "2019_assess_mod") %>% 
  bind_rows(read_csv(paste0(compare_dir, "/2020am/selectivity_2020.csv")) %>% 
              mutate(model = "2020_assess_mod"))

# Biom/adb trajectories ----

(p1 <- ggplot(deriv, aes(year, spawn_biom, col = model, lty = model, shape = model)) +
  geom_point() +
  geom_line() +
  labs(y = "spawning biomass (million lb)") +
  theme(legend.position = c(.85, .85)))

(p2 <- ggplot(deriv, aes(year, Fmort, col = model, lty = model, shape = model)) +
  geom_point() +
  geom_line() +
  labs(y = "fishing mortality") +
  theme(legend.position = c(.85, .85)))

(p3 <- ggplot(deriv, aes(year, biom, col = model, lty = model, shape = model)) +
  geom_point() +
  geom_line() +
  labs(y = "total biomass (million lb)") +
  theme(legend.position = c(.85, .85)))

plot_grid(p1, p3, p2, ncol = 1)

ggsave(paste0(compare_dir, "/ssb_biom_fmort.png"),
       units = "in", height = 11, width = 8, dpi = 400)

# recruitment ----

(p4 <- ggplot(rec, aes(year_class, est, fill = model, col = model, 
                       shape = model, lty = model)) +
   geom_point() +
   geom_line() +
   # geom_errorbar(aes(ymin = lower, ymax = upper)))
   geom_ribbon(aes(ymin = lower, ymax = upper), col = "white", alpha = 0.2) +
   labs(x = "year class", y = "recruitment (millions)")  +
   theme(legend.position = c(.15, .85)))
   
ggsave(paste0(compare_dir, "/rec.png"),
       units = "in", height = 4, width = 8, dpi = 400)

# abc ----

(p5 <- ggplot(res %>% filter(Fxx == 0.5), aes(year, ABC / 1e6, col = model, lty = model, shape = model)) +
   geom_point() +
   geom_line() +
   geom_line(data = ts, aes(year, catch * 2204.62 / 1e6), col = "grey") +
   labs(y = "ABC (million lb)") +
   theme(legend.position = c(.85, .85)))

(p6 <- ggplot(res %>% filter(Fxx == 0.5), aes(year, discarded / 1e6, col = model, lty = model, shape = model)) +
    geom_point() +
    geom_line() +
    labs(y = "Mortality from releases (million lb)") +
    theme(legend.position = c(.85, .85)))

plot_grid(p5, p6, ncol = 1)

ggsave(paste0(compare_dir, "/abc_mort_catch.png"),
       units = "in", height = 8, width = 8, dpi = 400)

# selectivity ----

slx <-  slx %>%   
  filter(Age <= 10) %>% 
  mutate(`Time blocks` = paste0(start_year, "-", end_year),
         age = as.factor(Age)) 

(p7 <- ggplot(slx %>% filter(Selectivity == "Fishery"), 
       aes(x = age, y = proportion, colour = model, 
           shape = model, lty = model, group = model)) +
  geom_point() +
  geom_line() +
  facet_grid(`Time blocks` ~ Sex)  +
  labs(x = "age", y = "selectivity", title = "Fishery selectivity") +
  theme(legend.position = "top") )

(p8 <- ggplot(slx %>% filter(Selectivity == "Survey"), 
       aes(x = age, y = proportion, colour = model, 
           shape = model, lty = model, group = model)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Sex)  +
  labs(x = "age", y = "selectivity", title = "Survey selectivity") +
  theme(legend.position = "none") )

plot_grid(p7, p8, ncol = 1, rel_heights = c(3/5, 2/5)) 

ggsave(paste0(compare_dir, "/selectivity.png"),
       units = "in", height = 12, width = 8, dpi = 400)

