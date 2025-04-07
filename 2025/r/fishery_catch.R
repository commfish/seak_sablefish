
#** Prior to 2023 this script was part of fishery_catch_cpue.R
#* In 2023 moved to its own script because other scripts got out of control.. 
#* 
# Fishery catch 1985-present, 
# Author: Spencer Weinstein & Alex Reich
# Contact: spencer.weinstein@alaska.gov
# Last edited: March 2025

source("r_helper/helper.r")
source("r_helper/functions.r")

# if(!require("rms"))   install.packages("rms") # simple bootstrap confidence intervals

# Most recent year of data
YEAR <- 2024

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
colnames(catch_ifdb)
#exvessel_value <- read.csv("data/exvessel_value.csv") # request from Aaron.baldwin@alaska.gov
#update from OceanAK report for future standardization; 2022 onward will use this report...
# link:

exvessel_value <- read_csv(paste0("legacy_data/exvessel_value_",
                YEAR,".csv",sep="")) %>% 
  mutate(year=Year_Landed, exvessel_mil_usd = CFEC_Value/1000000)

#exvessel_value <- read_csv(paste0(YEAR+1,"/data/exvessel_value_22ud.csv")) %>% 
#  mutate(year=Year_Landed, exvessel_mil_usd = CFEC_Value/1000000)
#format like Jane's old for consistency with code... just year and value

View(exvessel_value)
#Add new year from Rhea's querry: 
#OceanAK exvessel: https://oceanak.dfg.alaska.local/analytics/saw.dll?PortalGo&Action=prompt&path=%2Fshared%2FCommercial%20Fisheries%2FRegion%20I%2FGroundFish%2FUser%20Reports%2FPhil%27s%20Sablefish%2FExvessel%20value%20estimated%20from%20fish%20ticket%20data
# Save in YEAR/data/fishery/raw_data/Exvessel_value_for_YEAR_estimated_from_fish_ticket_data.csv

exves_new <- read_csv(paste0(YEAR+1,"/data/fishery/raw_data/Exvessel value estimated from fish ticket data",YEAR,".csv")) %>% 
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

str(catch_ifdb)

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
  dplyr::summarize(total_pounds = sum(whole_pounds)) %>%
  mutate(perc_ch = (total_pounds-lag(total_pounds,default = first(total_pounds)))/
           lag(total_pounds,default = first(total_pounds))) -> sum_catch #view(sum_catch)

if (sum_catch$perc_ch[sum_catch$year == YEAR] > 0) {
  pchange<-paste0(round(sum_catch$perc_ch[sum_catch$year == YEAR]*100,1),"% increase")
} else {
  pchange<-paste0(round(sum_catch$perc_ch[sum_catch$year == YEAR]*100,1),"% decrease")
}

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
  labs(x = NULL, y = "Catch (million round lb)\n") +
  annotate("text", x = 2015, y = 5, 
           label = pchange,
           size=8)-> catch

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
exvessel_value<-exvessel_value %>% 
  mutate(perc_ch = (exvessel_mil_usd-lag(exvessel_mil_usd,default = first(exvessel_mil_usd)))/
         lag(exvessel_mil_usd,default = first(exvessel_mil_usd))) #view(sum_catch)

if (exvessel_value$perc_ch[exvessel_value$year == YEAR] > 0) {
  pchange_evv<-paste0(round(exvessel_value$perc_ch[exvessel_value$year == YEAR]*100,1),"% increase")
} else {
  pchange_evv<-paste0(round(exvessel_value$perc_ch[exvessel_value$year == YEAR]*100,1),"% decrease")
}

exvessel <- ggplot(exvessel_value, aes(x = year, y = exvessel_mil_usd)) +
  #exvessel <- ggplot(exvessel_value, aes(x = Year.Landed, y = CFEC.Value)) +
  geom_point() +
  geom_line() +
  # add a line for EQS starting in 1994 (1997 in the SSEI).
  # geom_vline(xintercept = 1993.5, lty = 5, colour = "grey") +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  scale_x_continuous(breaks = seq(1985,2022,5), labels = seq(1985,2022,5)) +
  labs(x = NULL, y = "Ex-vessel value (million USD)\n") +
  ylim(c(0, 12.5)) +
  annotate("text", x = YEAR-7, y = 10, 
           label = pchange_evv,
           size=8)
exvessel
ggsave(paste0(YEAR+1,"/figures/exvessel_value_1985_", YEAR, ".png"), 
       dpi=300,  height=4, width=7,  units="in")

plot_grid(catch, port, exvessel, ncol = 1)#, align = 'hv') 

ggsave(paste0(YEAR+1,"/figures/catch_exvesselvalue_", YEAR, "v3.png"),
       dpi=300, height=10, width=7, units="in")
# View(port_catch)

data.frame(exvessel_value)

# Catch by gear: 
unique(catch_ifdb$Gear)
catch_ifdb %>% 
  mutate(Gear = ifelse(is.na(Gear),"Longline",Gear)) %>% 
  filter(year >= 1985) %>% 
  group_by(year, Gear) %>% 
  dplyr::summarise(pounds = sum(whole_pounds),
                   n_cfec = n_distinct(Cfec_permit),
                   n_vessels = n_distinct(Vessel)) %>% 
  filter(n_vessels > 3) %>% 
  group_by(year) %>% 
  mutate(tot_pounds = sum(pounds),
         perc = pounds/tot_pounds * 100) -> gear_catch

ggplot(gear_catch, aes(x = year, y = pounds/1e6, colour = Gear, group = Gear)) +
  geom_line() + geom_point()

# This plot is lame but will be cooler as the pot fishery develops
ggplot(gear_catch, aes(x = year, y = perc, fill = Gear)) +
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
  labs(x = NULL, y = "Percent landings by port", fill = NULL) -> gear 

ggplot(sum_catch %>% 
         filter(year >= 1985), 
       aes(x = year, y = total_pounds/1e6)) +
  geom_line(group=1) +
  geom_point() +
  geom_line(data = gear_catch %>% filter(year >=2021),
            aes(x = year, y = pounds/1e6, colour = Gear, group = Gear)) +
  geom_point(data = gear_catch %>% filter(year >=2021),
            aes(x = year, y = pounds/1e6, colour = Gear, group = Gear)) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  scale_x_continuous(breaks = seq(1985,2022,5), labels = seq(1985,2022,5)) + 
  scale_y_continuous(breaks = seq(0, 6, 1), limits = c(0, 6), labels = seq(0, 6, 1)) +
  # add a line for EQS starting in 1994 (1997 in the SSEI).
  geom_vline(xintercept = 1993.5, lty = 5, colour = "grey") +
  labs(x = NULL, y = "Catch (million round lb)\n") +
  annotate("text", x = 2015, y = 5, 
           label = pchange,
           size=8)-> catch_bygear

myColors <- c("blue","black","darkcyan")

ggplot(rbind(sum_catch %>% mutate(Gear = "Longline & Pot") %>% 
               select(-perc_ch) %>% filter(year >= 2021),
             gear_catch %>% #filter(year >= 2021) %>% 
               select(year,total_pounds = pounds, Gear)), 
       aes(x = year, y = total_pounds/1e6, col = Gear, group = Gear, fill = Gear)) +
  geom_line() +
  geom_point(aes(col = Gear)) +
  scale_colour_manual(values = myColors) +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  scale_x_continuous(breaks = seq(1985,2022,5), labels = seq(1985,2022,5)) + 
  scale_y_continuous(breaks = seq(0, 6, 1), limits = c(0, 6), labels = seq(0, 6, 1)) +
  # add a line for EQS starting in 1994 (1997 in the SSEI).
  geom_vline(xintercept = 1993.5, lty = 5, colour = "grey") +
  labs(x = NULL, y = "Catch (million round lb)\n") +
  annotate("text", x = 2015, y = 5, 
           label = pchange,
           size=8) +
  theme(legend.position = c(.2, .2)) -> catch_bygear

plot_grid(catch_bygear, port, exvessel,  ncol = 1, align = 'hv')

ggsave(paste0(YEAR+1,"/figures/catch_exvesselvalue_", YEAR, "v34.png"),
       dpi=300, height=10, width=7, units="in") # figure to use in assessment 2025

# Relationship between ex-vessel price and catch
if(!require("ggrepel"))   install.packages("ggrepel") 

full_join(sum_catch,exvessel_value,by="year") %>%
  mutate(flag = ifelse(year %in% c(YEAR, YEAR-1, YEAR-2), "a", "b")) %>% 
  ggplot(aes(x = total_pounds / 1e6, y = exvessel_mil_usd, col = flag)) +
  geom_smooth(method = "lm", se = FALSE, col = "grey") +
  geom_point() + 
  ggrepel::geom_text_repel(aes(label = year), max.overlaps = Inf) +
  scale_colour_manual(values = c("red", "black"), guide = FALSE) +
  labs(x = "\nCatch (million round lb)", y = "Ex-vessel value (million USD)") +
  scale_y_continuous(limits = c(0,12.5))

ggsave(paste0(YEAR+1,"/figures/exvessel_catch_correlation_1985_", YEAR, ".png"), 
       dpi=300,  height=4, width=7,  units="in")

full_join(sum_catch,exvessel_value,by="year") %>%
  filter(year > 2000) %>%
  mutate(flag = ifelse(year %in% c(YEAR, YEAR-1, YEAR-2), "a", "b")) %>% 
  ggplot(aes(x = total_pounds / 1e6, y = exvessel_mil_usd, col = flag)) +
  geom_smooth(method = "lm", se = FALSE, col = "grey") +
  geom_point() + 
  ggrepel::geom_text_repel(aes(label = year), max.overlaps = Inf) +
  scale_colour_manual(values = c("red", "black"), guide = FALSE) +
  labs(x = "\nCatch (million round lb)", y = "Ex-vessel value (million USD)") +
  scale_y_continuous(limits = c(0,6))

#------------------------------------------------------------------------------
#_______________________________________________________________________________
