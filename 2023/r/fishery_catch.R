
#** Prior to 2023 this script was part of fishery_catch_cpue.R
#* In 2023 moved to its own script because other scripts got out of control.. 
#* 
# Fishery catch 1985-present, 
# Author: Jane Sullivan & Phil Joy
# Contact: jane.sullivan@noaa.gov & philip.joy@alaska.gov
# Last edited: Feb 2023

source("r_helper/helper.r")
source("r_helper/functions.r")

# if(!require("rms"))   install.packages("rms") # simple bootstrap confidence intervals

# Most recent year of data
YEAR <- 2022

# Harvest ----

# There are multiple sources of catch data. The IFDB (Region I database) was
# used in the past, but J. Shriver recommends using the Gross Earnings File
# (GEF) for catch histories, which go back to 1975. The problem is the GEF isn't
# updated until data has been finalized, so it's never ready for the current
# year's assessment. The IFDB and GEF are the same from 1985-present, so use
# that. The SCAA model relies on Carlile et al 2002 published estimates of NSEI
# catch 1975-1984.

catch_ifdb <- read_csv(paste0(YEAR+1,"/data/fishery/nseiharvest_ifdb_1985_", YEAR,".csv"), 
                       guess_max = 50000) #%>% 

#exvessel_value <- read.csv("data/exvessel_value.csv") # request from Aaron.baldwin@alaska.gov
#update from OceanAK report for future standardization; 2022 onward will use this report...
# link:

exvessel_value <- read_csv(paste0("legacy_data/exvessel_value_",
                YEAR,".csv",sep="")) %>% 
  mutate(year=Year_Landed, exvessel_mil_usd = CFEC_Value/1000000)

#exvessel_value <- read_csv(paste0(YEAR+1,"/data/exvessel_value_22ud.csv")) %>% 
#  mutate(year=Year_Landed, exvessel_mil_usd = CFEC_Value/1000000)
#format like Jane's old for consistency with code... just year and value

#Add 2021 do n 2023 only.  Should not be necessary next year!!!
exvessel_value[nrow(exvessel_value)+1,]<-list(2021,NA,NA, NA, 2021, 2.821949)

#Add new year
#OceanAK for 2022 exvessel: https://oceanak.dfg.alaska.local/analytics/saw.dll?PortalGo&Action=prompt&path=%2Fshared%2FCommercial%20Fisheries%2FRegion%20I%2FGroundFish%2FUser%20Reports%2FPhil%27s%20Sablefish%2FExvessel%20value%20for%202022%20estimated%20from%20fish%20ticket%20data

exves_new <- read_csv(paste0(YEAR+1,"/data/fishery/raw_data/Exvessel_value_for_2022_estimated_from_fish_ticket_data.csv")) %>% 
  data.frame()

exvessel_value[nrow(exvessel_value)+1,]<-list(YEAR,
                                              sum(exves_new$Whole.Weight..lbs.),
                                              length(unique(exves_new$CFEC)), 
                                              sum(exves_new$Exvessel.value), 
                                              YEAR, 
                                              sum(exves_new$Exvessel.value)/1000000)
#save for next year
write_csv(exvessel_value, paste0("legacy_data/exvessel_value_",
                          YEAR+1, ".csv"))

exvessel_value<-exvessel_value[,c(5,6)]
#if new year not available, add in best est. from Aaron and groundfish crew....
#2021 exvessel value ... add in manually and

view(exvessel_value)

catch_ifdb %>% 
  filter(year > 2013) %>% 
  group_by(year, julian_day) %>% 
  dplyr::summarise(pounds = sum(whole_pounds)) %>% 
  mutate(cum_pounds = cumsum(pounds)) %>% 
  group_by(year) %>% 
  mutate(tot_pounds = sum(pounds)) %>% 
  ungroup() %>% 
  mutate(cum_pounds = cum_pounds / tot_pounds) -> catch_plot

# Cumulative catch over the fishery seasons
ggplot(catch_plot, aes(x = julian_day, colour = factor(year), size = factor(year), group = factor(year))) +
  geom_line(aes(y = cum_pounds)) +
  scale_color_stata() +
  scale_size_manual(values = c(rep(1, length(unique(catch_plot$year))-1), 2)) +
  # facet_wrap(~ year, ncol = 1) +
  labs(x = "Julian Day", y = "Cumulative catch", col = NULL, size = NULL) +
  ylim(0, 1)

# Total catch by year
catch_ifdb %>% 
  group_by(year) %>% 
  dplyr::summarize(total_pounds = sum(whole_pounds)) -> sum_catch #view(sum_catch)

# axis <- tickr(sum_catch, year, 5)
ggplot(sum_catch %>% 
         filter(year >= 1985), 
       aes(x = year, y = total_pounds/1e6)) +
  geom_line(group=1) +
  geom_point() +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  scale_x_continuous(breaks = seq(1985,2022,5), labels = seq(1985,2022,5)) + 
  scale_y_continuous(breaks = seq(0, 6, 1), limits = c(0, 6), labels = seq(0, 6, 1)) +
  # add a line for EQS starting in 1994 (1997 in the SSEI).
  geom_vline(xintercept = 1993.5, lty = 5, colour = "grey") +
  labs(x = NULL, y = "Catch (million round lb)\n") -> catch

write_csv(sum_catch, paste0(YEAR+1,"/output/harvest_1985_", YEAR, ".csv"))

catch
ggsave(paste0(YEAR+1,"/figures/fishery_harvest_1985_", YEAR, ".png"), 
       dpi=300,  height=4, width=7,  units="in")

# Catch by ports
'%ni%' <- Negate('%in%')

catch_ifdb %>% filter(year == YEAR) %>% distinct(Port)
catch_ifdb %>% filter(year == YEAR) %>% distinct(Vessel)
# catch_ifdb %>% filter(year == YEAR & Port == "HOM") 
catch_ifdb %>% 
  mutate(Port = derivedFactor(`SIT` = Port == "SIT",
                              `JNU` = Port == "JNU",
                              `PBG` = Port == "PBG",
                              `OTH` = Port %ni% c("SIT", "JNU", "PBG"))) %>% 
  filter(year >= 1985) %>% 
  group_by(year, Port) %>% 
  dplyr::summarise(pounds = sum(whole_pounds),
                   n_cfec = n_distinct(Cfec_permit),
                   n_vessels = n_distinct(Vessel)) %>% 
  filter(n_vessels > 3) %>% 
  group_by(year) %>% 
  mutate(tot_pounds = sum(pounds),
         perc = pounds/tot_pounds * 100) -> port_catch

ggplot(port_catch, aes(x = year, y = pounds/1e6, colour = Port, group = Port)) +
  geom_line()

ggplot(port_catch, aes(x = year, y = perc, fill = Port)) +
  geom_bar(stat = "identity", width = 1, colour = "black") +
  scale_fill_grey(start = 0.15, end = 1) +
  scale_x_continuous(breaks = seq(1985,2022,5), labels = seq(1985,2022,5)) +
  # add a line for EQS starting in 1994 (1997 in the SSEI).
  # geom_vline(xintercept = 1993.5, lty = 5, colour = "grey") +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  theme(legend.position = c(0.8,0.8),
        legend.background = element_rect(color = "black", 
                                         fill = "white", 
                                         linetype = "solid")) +
  labs(x = NULL, y = "Percent landings by port", fill = NULL) -> port 

plot_grid(catch, port, ncol = 1, align = 'hv')

ggsave(paste0(YEAR+1,"/figures/catch_byport_", YEAR, ".png"),
       dpi=300, height=10, width=7, units="in")

# Exvessel value ----
exvessel <- ggplot(exvessel_value, aes(x = year, y = exvessel_mil_usd)) +
  #exvessel <- ggplot(exvessel_value, aes(x = Year.Landed, y = CFEC.Value)) +
  geom_point() +
  geom_line() +
  # add a line for EQS starting in 1994 (1997 in the SSEI).
  # geom_vline(xintercept = 1993.5, lty = 5, colour = "grey") +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  scale_x_continuous(breaks = seq(1985,2022,5), labels = seq(1985,2022,5)) +
  labs(x = NULL, y = "Ex-vessel value (million USD)\n") +
  ylim(c(0, 12.5))
exvessel
ggsave(paste0(YEAR+1,"/figures/exvessel_value_1985_", YEAR, ".png"), 
       dpi=300,  height=4, width=7,  units="in")

plot_grid(catch, port, exvessel, ncol = 1)#, align = 'hv')

ggsave(paste0(YEAR+1,"/figures/catch_exvesselvalue_", YEAR, "v3.png"),
       dpi=300, height=10, width=7, units="in")
# View(port_catch)

data.frame(exvessel_value)

# Relationship between ex-vessel price and catch
if(!require("ggrepel"))   install.packages("ggrepel") 
sum_catch %>% 
  left_join(exvessel_value) %>% 
  mutate(flag = ifelse(year %in% c(YEAR, YEAR-1, YEAR-2), "a", "b")) %>% 
  ggplot(aes(x = total_pounds / 1e6, y = exvessel_mil_usd, col = flag)) +
  geom_smooth(method = "lm", se = FALSE, col = "grey") +
  geom_point() + 
  ggrepel::geom_text_repel(aes(label = year), max.overlaps = Inf) +
  scale_colour_manual(values = c("red", "black"), guide = FALSE) +
  labs(x = "\nCatch (million round lb)", y = "Ex-vessel value (million USD)") 

ggsave(paste0(YEAR+1,"/figures/exvessel_catch_correlation_1985_", YEAR, ".png"), 
       dpi=300,  height=4, width=7,  units="in")

#------------------------------------------------------------------------------
#_______________________________________________________________________________