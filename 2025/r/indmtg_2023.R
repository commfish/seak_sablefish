source("r_helper/helper.r")
source("r_helper/functions.r")

# if(!require("rms"))   install.packages("rms") # simple bootstrap confidence intervals

# Most recent year of data
YEAR <- 2023
lyr<-2023

fsh_cpue <- read_csv(paste0(YEAR+1,"/output/ll_cpue_fullstand_1980_", YEAR, ".csv"))

fsh_cpue %>% 
  filter(year >= YEAR - 1) %>%
  select(year, fsh_cpue) %>% 
  reshape2::dcast("fsh_cpue" ~ year) -> perc_ch

# Percent change from last year
fsh_cpue %>% 
  #rename(srv_cpue = std_cpue) %>%
  filter(year >= YEAR - 1 & year <= YEAR) %>%
  select(year, fsh_cpue) %>% 
  mutate(year2 = ifelse(year == YEAR, "thisyr", "lastyr")) %>% 
  reshape2::dcast("fsh_cpue" ~ year2, value.var = "fsh_cpue") %>% 
  mutate(perc_change_ly = (thisyr - lastyr) / lastyr * 100,
         eval_ly = ifelse(perc_change_ly < 0, "decreased", "increased")) -> fsh_ly

names(perc_ch) <- c("cpue", "last_year", "this_year") 
perc_ch %>% mutate(perc_change_ly = (`this_year` - `last_year`) / `last_year` * 100) ->pchange
change <- as.character(round(pchange[4],1))

ggplot(fsh_cpue %>% filter(year >=2000)) +
  geom_point(aes(year, fsh_cpue)) +
  geom_line(aes(year, fsh_cpue)) +
  geom_ribbon(aes(year, ymin = lower, 
                  ymax = upper),
              alpha = 0.2,  fill = "grey") +
  labs(x = "", y = "Fishery CPUE (round lb per hook)\n") + 
#  annotate("text", x = YEAR-2, y = max(fsh_cpue$fsh_cpue[fsh_cpue$year >= 2000])*1.1, label = paste0("",change,"%"), size = 7) + 
  geom_text(x = YEAR-2, 
            y = max(fsh_cpue$fsh_cpue[fsh_cpue$year > 2000]) * 1.05,
            label = paste0(ifelse(fsh_ly$eval_ly == "increased", "+", ""),
                           sprintf("%.0f%%", fsh_ly$perc_change_ly)), 
            size = 7) +
  theme(axis.text = element_text(size=15)) -> fishery_trends

ggsave(paste0(YEAR+1,"/figures/fshcpue_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

#---------------------------------------------------------------------------
mr_ests <- read_csv(paste0(YEAR+1,"/output/mr_index_", YEAR, ".csv"))

last_est<-max(mr_ests$year)-max(mr_ests$year[mr_ests$year !=YEAR])

mr_ests %>% 
  filter(year >= YEAR - last_est) %>%
  select(year, estimate) %>% 
  reshape2::dcast("mr" ~ year) -> perc_ch_mr

names(perc_ch_mr) <- c("estimate", "last_est", "this_year") 
perc_ch_mr %>% mutate(perc_change_ly = (`this_year` - `last_est`) / `last_est` * 100) ->pchange
change <- as.character(round(pchange[4],0))

ggplot(mr_ests %>% filter(year >=2000)) +
  geom_point(aes(year, estimate)) +
  geom_line(aes(year, estimate)) +
  geom_ribbon(aes(year, ymin = q025, 
                  ymax = q975),
              alpha = 0.2,  fill = "grey") +
  labs(x = "", y = "Abundance (millions)\n") + 
#  annotate("text", x = YEAR-2, y = max(mr_ests$estimate)*1.1, label = paste0("",change,"%"), size = 7) + 
  scale_x_continuous(limits=c(2000,YEAR)) +
  theme(axis.text = element_text(size=15)) -> mr_trends

ggsave(paste0(YEAR+1,"/figures/mr_1997_", YEAR, ".png"),
       dpi=300, height=4, width=7, units="in")

#------------------------------------------------------------------------------
srv_cpue <- read_csv(paste0(YEAR+1,"/output/srvcpue_1997_", YEAR, ".csv"))


# Percent change from last year
srv_cpue %>% 
  rename(srv_cpue = std_cpue) %>%
  filter(year >= YEAR - 1 & year <= YEAR) %>%
  select(year, srv_cpue) %>% 
  mutate(year2 = ifelse(year == YEAR, "thisyr", "lastyr")) %>% 
  reshape2::dcast("srv_cpue" ~ year2, value.var = "srv_cpue") %>% 
  mutate(perc_change_ly = (thisyr - lastyr) / lastyr * 100,
         eval_ly = ifelse(perc_change_ly < 0, "decreased", "increased")) -> srv_ly; srv_ly

# figures
# axis <- tickr(data = srv_sum, var = year, to = 3)
srv_cpue2000 <- srv_cpue %>% filter(year >= 2000)

ggplot(data = srv_cpue2000, aes(x = year)) +
  geom_point(aes(y = std_cpue)) +
  geom_line(aes(y = std_cpue)) +
  geom_ribbon(aes(year, ymin = std_cpue - sd, ymax = std_cpue + sd),
              alpha = 0.2, col = "white", fill = "grey") +
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  # lims(y = c(0, 0.45)) + 
  geom_text(x = YEAR-2, 
            y = 1.1 * max(srv_cpue2000$std_cpue),
            label = paste0(ifelse(srv_ly$eval_ly == "increased", "+", ""),
                           sprintf("%.0f%%", srv_ly$perc_change_ly)), 
            size = 7) +
  theme(axis.text = element_text(size=15)) +
  labs(x = NULL, y = "Survey CPUE (number per hook)\n") -> srv_trends

#ggsave(paste0(YEAR+1,"/figures/npue_llsrv_", YEAR, ".png"), 
#       dpi=300, height=4, width=7, units="in")
#-------------------------------------------------------------------------------
plot_grid(srv_trends, mr_trends, fishery_trends, ncol = 1, align = 'hv') -> trends#, 
          #labels = c('(A)', '(B)', '(C)')) 

ggsave(plot = trends, filename = paste0(YEAR+1, "/figures/abd_trends_", YEAR, ".png"), 
         # dpi = 300, height = 7, width = 6, units = "in")
         dpi = 300, height = 10, width = 7, units = "in")

#-------------------------------------------------------------------------------
# Fishery biological data
read_csv(paste0(YEAR+1,"/data/fishery/fishery_bio_2000_", lyr,".csv"), 
         guess_max = 50000) %>%
  mutate(Year = factor(year),
         Sex = factor(Sex)) -> fsh_bio

# Survey biological data
read_csv(paste0(YEAR+1,"/data/survey/llsrv_bio_1988_", lyr,".csv"), 
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
         mean = mean(ecdf)) -> df_fb 

ggplot(df_fb,aes(x = year, y = ecdf,col = Source)) +
  # ggplot(aes(x = year, y = std,lty = Source)) +
  # geom_hline(yintercept = 0, lty = 2, col = "grey") +
  geom_line() +
  geom_point() +
  geom_line(aes(y = mean, col = Source), lty = 2) +
  annotate("text", x = YEAR, y = df_fb$ecdf[df_fb$year == YEAR]*1.1, 
           label = paste0(round(df_fb$ecdf[df_fb$year == YEAR],0),"%"), 
           size = 4, col = c("red","darkcyan")) + 
  # labs(x = NULL, y = NULL) +
  labs(x = NULL, y = "Percent under 3 dressed lb")

ggsave(paste0(YEAR+1,"/figures/small_fish_",YEAR,".png"), width = 8, height = 3.5, dpi = 400, units = "in")

# Lengths -----
library("scales")
library("plotrix")
library("ggforce")
hex <- hue_pal()(2)

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
         mean = mean(ecdf)) -> smallfish #print(n=Inf)

round(smallfish %>% filter(year == YEAR & Source == "Fishery (landed catch)")%>% data.frame() %>% select(ecdf)*100,1)  

prop_sm_fsh<-round(smallfish %>% filter(year == YEAR & Source == "Fishery (landed catch)")%>% data.frame() %>% select(ecdf)*100,1) 
prop_sm_srv<-round(smallfish %>% filter(year == YEAR & Source == "Survey (total catch)")%>% data.frame() %>% select(ecdf)*100,1) 

smallfish %>%  ggplot(aes(x = year, y = ecdf,col = Source, shape = Source)) +
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
        legend.position = c(.2,.8)) +
  annotate("text", x = 2015, y = 0.22, label = "20%", col = hex[2]) +
  annotate("text", x = 2015, y = 0.062, label = "5%", col = hex[1]) +
  annotate("text", x = YEAR, y = as.numeric(prop_sm_srv/100)-0.02, 
           label = paste0(prop_sm_srv,"%"), col = hex[2]) +
  annotate("text", x = YEAR, y = as.numeric(prop_sm_fsh/100) -0.02, 
           label = paste0(prop_sm_fsh,"%"), col = hex[1]) #+
  #geom_circle(data=data.frame(x0=c(2015,YEAR),y0=c(0.2,as.numeric(prop_sm_srv/100)),r=c(1,1)),
  #            aes(x0=x0,y0=y0,r=r, col = hex[2]))

ggsave(paste0(YEAR+1,"/figures/small_fish_",YEAR,".png"), width = 8, height = 4, dpi = 400, units = "in")

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

ggsave(paste0(YEAR+1,"/figures/small_fish_age_",YEAR,".png"), width = 8, height = 3.5, dpi = 400, units = "in")

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
