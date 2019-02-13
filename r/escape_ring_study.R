# Escape ring preliminary analysis
# Jane Sullivan (jane.sullivan1@alaska.gov)
# February 2018

source("r/helper.r")
source("r/functions.r")

# Data ----

# Digitilized data points from Haist et al 2004 Figure N.2a - Sablefish in
# British Columbia, Canada: Stock Assessment for 2003 and Advice to Managers for
# 2004). https://apps.automeris.io/wpd/

len_gir <- read_csv("data/survey/length_girth_2001bc.csv")

# Data from the digistized figure to reproduce linear regression
fit_may <- lm(girth_mm ~ length_mm, data = filter(len_gir, month == "May"))
fit_oct <- lm(girth_mm ~ length_mm, data = filter(len_gir, month == "October"))

pred <- data.frame(length_mm = seq(450, 750, 1))
pred$May <- predict(fit_may, newdata = pred, type = "response")
pred$Oct <- predict(fit_oct, newdata = pred, type = "response")
pred %>% mutate(length_cm = round(length_mm / 10, 0)) -> pred

# Basic design with 4 escape ring treatments: control, 4" diameter, 3.75", and 3.5"
escape <- data.frame(diam_in = c(4.0, 3.75, 3.5))

escape %>% mutate(`Escape ring` = as.factor(paste0(format(diam_in, nsmall = 2), '"')),
                  diam_mm = diam_in * 25.4,
                  circ_mm = diam_mm * pi,
                  # estimated length given regression from Haist et al.
                  length_mm = (circ_mm - coef(fit_may)[1]) / coef(fit_may)[2]) -> escape

# Pot biological data
potbio <- read_csv("data/survey/potsrv_bio_1981_2018.csv", guess_max = 50000) %>% 
  select(year, Stat, set, depth, length) %>% 
  filter(!is.na(length) & year >= 2005)

potbio %>% 
  # filter(year >= 2010) %>% 
  mutate(length2 = ifelse(length < 40, 40,
                          ifelse(length > 110, 110, length)),
         length_bin = factor(cut(length2, breaks = seq(35, 110, 5),
                          labels = paste(seq(40, 110, 5))), ordered = TRUE)) %>% 
  select(-length2) -> potbio 

# Length comps by year
potbio %>% 
  count(year, length_bin) %>%
  group_by(year) %>% 
  mutate(proportion = round( n / sum(n), 4)) -> lencomps

potbio %>% 
  group_by(year) %>% 
  # number of sets in each year
  summarize(n_sets = max(set)) %>% 
  left_join(lencomps, by = "year") %>% 
  # number of fish in a bin that were sampled per set
  mutate(n_per_set = n / n_sets) -> lencomps

# Graphics ----

axis <- tickr(pred, length_cm, 5)
axisy <- pred %>% mutate(girth_cm = round(May / 10,0))
axisy <- tickr(axisy, girth_cm, 5)

pred %>% gather("Month", "girth_mm", -c(length_mm, length_cm)) %>% 
  ggplot() +
  geom_hline(yintercept = escape$circ_mm / 10, colour = "grey70", lty = 4, alpha = 0.8) +
  geom_vline(xintercept = escape$length_mm / 10, colour = "grey70", lty = 4, alpha = 0.8) +
  geom_line(aes(x = length_mm / 10, y = girth_mm / 10, colour = Month, 
                group = Month, linetype = Month), size = 1.5) +
  geom_text(data = escape, aes(x = 47, y = (circ_mm + 5) / 10 , label = `Escape ring`),
            family = "Times") +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  scale_y_continuous(breaks = axisy$breaks, labels = axisy$labels) + 
  scale_color_grey() +
  labs(x = "\nLength (cm)", y = "Girth (cm)\n") +
  theme(legend.position = c(0.8, 0.2))

lencomps %>% 
  group_by(year) %>% 
  summarize(N = sum(n),
            label = paste0("n = ", prettyNum(N, big.mark = ","))) %>% 
  ungroup() %>% 
  mutate(length_bin = "95", proportion = 0.18) -> labels 

# Length comps
ggplot(data = lencomps, aes(x = length_bin, y = proportion)) + 
  geom_bar(stat = "identity", colour = "lightgrey", fill = "lightgrey", width = 0.8) +
  # Compare all past years to this year
  geom_line(data = lencomps %>% ungroup() %>% filter(year == 2018) %>% select(-year),
            aes(x = length_bin, y = proportion, group = 1),
            colour = "black") +
  geom_text(data = labels, aes(x = length_bin, y = proportion, label = label),
            size = 4, family = "Times") +
  scale_x_discrete(breaks = seq(40, 110, 10),
                   labels = seq(40, 110, 10)) +
  facet_wrap(~ year) +
  labs(x = "\nFork length (cm)", y = "Proportion-at-length\n") +
  theme(strip.placement = "outside") 

# Alternative binning for weight/girth sampling ----

# Pot biological data
potbio <- read_csv("data/survey/potsrv_bio_1981_2018.csv", guess_max = 50000) %>% 
  select(year, Stat, set, depth, length) %>% 
  filter(!is.na(length) & year >= 2005)

potbio %>% 
  mutate(length_bin = derivedFactor("< 50" = length < 50,
                                    "[50 - 55)" = length >= 50 & length < 55,
                                    "[55 - 60)" = length >= 55 & length < 60,
                                    "[60 - 65)" = length >= 60 & length < 65,
                                    "[65 - 70)" = length >= 65 & length < 70,
                                    ">= 70" = length >= 70)) -> potbio

# Length comps by year
potbio %>% 
  count(year, length_bin) %>%
  group_by(year) %>% 
  mutate(proportion = round( n / sum(n), 4)) -> lencomps

potbio %>% 
  group_by(year) %>% 
  # number of sets in each year
  summarize(n_sets = max(set)) %>% 
  left_join(lencomps, by = "year") %>% 
  # number of fish in a bin that were sampled per set
  mutate(n_per_set = n / n_sets) -> lencomps

target <- 5 # of of fish to sample in each bin per set

lencomps %>% 
  group_by(length_bin) %>% 
  mutate(mu_nset = mean(n_per_set)) %>% 
  ungroup() %>% 
  mutate(no_sampled = target * n_sets,
         rate = target / n_per_set,
         mu_rate = target / mu_nset) %>% 
  group_by(year) %>% 
  mutate(tot_sampled = sum(no_sampled),
         N = sum(n)) -> lencomps


lencomps %>% 
  group_by(year) %>% 
  distinct(n_sets, N, tot_sampled)

ggplot(data = lencomps, aes(x = length_bin, y = n_per_set, fill = factor(year))) +
  scale_fill_grey(start = 0.3, end = 0.8) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "total")) +
  geom_errorbar(aes(ymax = mu_nset, ymin = mu_nset, colour = "Mean"), lwd = 1, lty = 2) +
  labs(x = "\nLength bins", y = "Number sampled per set\n", fill = NULL, colour = NULL)

ggplot(data = lencomps, aes(x = factor(year), y = rate, fill = factor(year))) +
  scale_fill_grey(start = 0.3, end = 0.8) +
  facet_wrap(~length_bin, scales = "free", nrow = 1) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "total")) +
  geom_errorbar(aes(ymax = mu_rate, ymin = mu_rate, colour = "Mean"), lwd = 1, lty = 2) +
  labs(x = "\nLength bins", y = "Sampling rate per set\n", fill = NULL, colour = NULL) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
