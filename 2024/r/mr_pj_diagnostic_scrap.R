ugh<-marks[1:20,]
dys<-unique(ugh$date)
unique(ugh$year)

ugh$whole_kg
ugh$total_obs
ugh$marked
ugh$tags_from_fishery
ugh$mean_weight
ugh$WPUE

ugh$interp_mean = 
ugh$mean_npue <- ugh$WPUE/ugh$mean_weight

cumsum(ugh$tags_from_fishery)

for (d in dys){
  Dat<-Ugh[Ugh$date == d,]
  Dat$whole_kg = sum(whole_kg),
  total_obs = sum(total_obs),
  total_marked = sum(marked),
  tags_from_fishery = sum(tags_from_fishery),
  mean_weight = mean(mean_weight),
  mean_wpue = mean(WPUE)
}

fshbio<-read.csv(paste0("data/fishery/fishery_bio_2000_", YEAR,".csv"))
str(fshbio)



read_csv(paste0("data/fishery/fishery_bio_2000_", YEAR,".csv"), 
         guess_max = 50000) %>%
  filter(!is.na(weight)) %>% 
  mutate(date = ymd(as.Date(date, "%m/%d/%Y"))) %>% 
  select(date, trip_no, weight, Stat) %>% 
  group_by(date, trip_no) %>% 
  dplyr::summarize(mean_weight_bios = mean(weight)) -> fsh_bio2

view(fsh_bio2)

read_csv(paste0("data/fishery/fishery_bio_2000_", YEAR,".csv"), 
         guess_max = 50000) %>%
  #filter(!is.na(weight)) %>% 
  mutate(date = ymd(as.Date(date, "%m/%d/%Y"))) %>% 
  select(date, trip_no, Stat) %>% 
  group_by(date, trip_no) -> fsh_bio3 #%>% 
#  dplyr::summarize(mean_weight_bios = mean(weight)) 


view(fsh_bio3)
unique(fsh_bio3$Stat)

left_join(marks, fsh_bio3, by = c("date", "trip_no"))-> marks2
view(marks2)

left_join(marks, fsh_bio3, by = c("date", "trip_no")) %>% 
  #mutate(mean_weight = ifelse(!is.na(mean_weight_bios), mean_weight_bios, mean_weight)) %>% 
  select(-mean_weight_bios) -> marks2

read_csv(paste0("data/fishery/nsei_daily_tag_accounting_2004_", YEAR-1, ".csv")) -> marks3 

marks3 %>% 
  filter(year >= FIRST_YEAR &
           !year %in% NO_MARK_SRV) %>% 
  mutate(all_observed = ifelse(
    !grepl(c("Missing|missing|Missed|missed|eastern|Eastern|not counted|
             Did not observe|did not observe|dressed|Dressed"), comments) & 
      observed_flag == "Yes", "Yes", "No"),
    mean_weight = ifelse(all_observed == "Yes", whole_kg/total_obs, NA),
    year_trip = paste0(year, "_", trip_no)) -> marks3

#left_join(marks3, fsh_tx, by = c("date", "trip_no"))-> marks2
left_join(marks3, fsh_tx, by = c("date", "year_trip"))-> marks2

view(marks2)

ex5<-marks3[marks3$year == 2005,]
tx5<-fsh_tx[fsh_tx$year == 2005,]

view(ex5)
unique(ex5$year_trip)
unique(tx5$year_trip)
unique(tx5$Stat)

left_join(ex5, tx5 %>%
            select(year_trip, Stat), 
          by = c("year_trip"))-> ex5.2
ex5.2<-distinct(ex5.2)

view(ex5.2)
nrow(ex5.2)
nrow(distinct(ex5.2))

tx5$Stat[tx5$year_trip == "2005_9501"]
ex5.2[ex5.2$year_trip == "2005_9501",]

fsh_tx[fsh_tx$year_trip=="2005_9501",]
fsh_tx[fsh_tx$trip_no =="9501",]

nostat<-marks3[is.na(marks3$Stat),]
view(nostat)

with(nostat, table(year))
nrow(marks3)

fsh_tx[fsh_tx$trip_no == 9301,]

rawtx<-read.csv(paste0("data/fishery/nseiharvest_ifdb_1985_", YEAR,".csv"))

rawtx[rawtx$trip_no == 9301,]

rawtx[rawtx$date == "2020-09-19",]
unique(rawtx$trip_no[rawtx$year == 2020])

#check fishery CPUE for missing trip numbers...
str(fsh_cpue)

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
  dplyr::summarize(WPUE = mean(WPUE)) -> fsh_cpue2
str(fsh_cpue2)

fsh_cpue2<-read.csv(paste0("data/fishery/fishery_cpue_2022reboot_1997_", YEAR,".csv"))
fsh_cpue2$year_trip = paste0(fsh_cpue2$year, "_", fsh_cpue2$trip_no)

fsh_cpue2[fsh_cpue2$year_trip == "2020_9301",]
fsh_cpue2[fsh_cpue2$trip_no == 9302,]

marks3[marks3$trip_no == 9302,]

fsh_cpue2[fsh_cpue2$year_trip == "2020_101",]

rawtx[rawtx$year == 2020 & is.na(rawtx$trip_no),]

#===============================================================================
read_csv(paste0("data/fishery/nsei_daily_tag_accounting_2004_", YEAR-1, ".csv")) -> marks4 

marks4 %>% 
  filter(year >= FIRST_YEAR &
           !year %in% NO_MARK_SRV) %>% 
  mutate(all_observed = ifelse(
    !grepl(c("Missing|missing|Missed|missed|eastern|Eastern|not counted|
             Did not observe|did not observe|dressed|Dressed"), comments) & 
      observed_flag == "Yes", "Yes", "No"),
    mean_weight = ifelse(all_observed == "Yes", whole_kg/total_obs, NA),
    year_trip = paste0(year, "_", trip_no)) -> marks4
nrow(marks4)

left_join(marks4, fsh_cpue2 %>%
            select(year_trip, Stat), 
          by = c("year_trip"))-> marks4
view(marks4)

marks4<-distinct(marks4)

nostat4<-marks4[is.na(marks4$Stat),]
view(nostat4)   #some missing trip numbers not present in fishery cpue or fish_tx data!!! 

with(nostat4, table(year))
nrow(marks4)

3327/1423
head(mtry,20)
view(mtry[1:6,])
head(marks3)
view(mtry[mtry$year_trip == "2005_106",])
view(fsh_tx[fsh_tx$year_trip == "2005_106",])
view(marks[marks$year_trip == "2005_106",])
1397.5+4125

marks[marks$trip_no == 106 & marks$year == 2005,]

str(fsh_tx)

view(fsh_tx[fsh_tx$year_trip == "2005_106",])
view(mtry)

test<-mtry[mtry$year == 2005,][1:10,]

test %>% 
  # padr::pad fills in missing dates with NAs, grouping by years.
  pad(group = "year") %>% 
  group_by(year, date) %>% 
  dplyr::summarize(whole_kg = sum(whole_kg.y),
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
         julian_day = yday(date)) -> t3

daily_marks3[daily_marks3$year_trip == "2005_106",]

view(test)
view(t3)
mksub<-marks3[marks3$year_trip == "2005_2003" |
                marks3$year_trip == "2005_2006" |
                marks3$year_trip == "2005_58" |
                marks3$year_trip == "2005_59",]
view(mksub)

philcpue<-read.csv(paste0("data/fishery/fishery_cpue_2022reboot_1997_", YEAR,".csv"))
view(philcpue)
view(philcpue[philcpue$year == 2005 & philcpue$trip_no == 58,])

tcpue<-philcpue[philcpue$year == 2005 & philcpue$trip_no == 58,]

tcpue %>% filter(Spp_cde == "710") %>% 
  mutate(sable_kg_set = sable_lbs_set * 0.45359237, # conversion lb to kg
         std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (0.0254 * hook_space))), #standardize hook spacing (Sigler & Lunsford 2001, CJFAS)
         # kg sablefish/1000 hooks, following Mueter 2007
         WPUE = sable_kg_set / (std_hooks / 1000)) %>% 
  filter(!is.na(date) & 
           !is.na(sable_lbs_set) &
           # omit special projects before/after fishery
           julian_day > 226 & julian_day < 322) %>% 
  group_by(year, trip_no, Stat) %>% 
  dplyr::summarize(WPUE = mean(WPUE)) -> pcpue2

view(pcpue1)
view(pcpue2)

view(fsh_cpue_stat[fsh_cpue_stat$year == 2005 & fsh_cpue_stat$trip_no == 58,])
view(marks4[marks4$year_trip == "2005_58",])

view(marks5[marks5$year == 2005 & marks5$trip_no == 58,])
view(marks3[marks3$year_trip == "2005_58",])

view(fsh_cpue[fsh_cpue$year == 2005 & fsh_cpue$trip_no == 58,])
view(fsh_cpue_stat)

fsh_cpue[fsh_cpue$year == 2005 & fsh_cpue$trip_no == 58,]
fsh_cpue_stat[fsh_cpue_stat$year == 2005 & fsh_cpue_stat$trip_no == 58,]

view(marks)
view(marks2)
view(marks5)

















