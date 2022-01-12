

Dat<-read.csv(paste0("data/fishery/raw_data/nseiharvest_ifdb_",
                     YEAR, ".csv"))
head(Dat)

Dat<-Dat %>% 
  mutate(date = ymd(parse_date_time(CATCH_DATE, c("%Y-%m-%d %H:%M:%S"))), #ISO 8601 format
         julian_day = yday(date),
         sell_date = ymd(parse_date_time(SELL_DATE, c("%Y-%m-%d %H:%M:%S"))),
         whole_pounds = ifelse(ROUND_POUNDS == 0, POUNDS, ROUND_POUNDS)) %>% 
  select(year = ï..YEAR, date, julian_day, Mgmt_area = G_MANAGEMENT_AREA_CODE, Stat = G_STAT_AREA,
                Adfg = ADFG_NO, trip_no = TRIP_NO, sell_date, Vessel = VESSEL_NAME, Port = PORT_CODE,  
                Cfec_permit = G_CFEC_FISHERY, Delivery_cde = DELIVERY_CODE, 
                Harvest = HARVEST, Harvest_cde = HARVEST_CODE, Spp_cde = SPECIES_CODE, 
                whole_pounds, pounds = POUNDS) %>% 
           mutate(Stat = as.character(Stat),
                  Harvest_cde = as.character(Harvest_cde))

read_csv(paste0("data/fishery/raw_data/nseiharvest_ifdb_",
                YEAR, ".csv"), 
        guess_max = 500000) %>% 
  mutate(date = ymd(parse_date_time(CATCH_DATE, c("%Y-%m-%d %H:%M:%S"))), #ISO 8601 format
         julian_day = yday(date),
         sell_date = ymd(parse_date_time(SELL_DATE, c("%Y-%m-%d %H:%M:%S"))), #
         # BEWARE: Landings are always entered as POUNDS and then converted to
         # WHOLE_POUNDS using a conversion factor related to the disposition code.
         # Prior to 1985 there was no disposition code for landings, which is why
         # WHOLE_POUNDS is not populated prior to 1985. Here we assume fish were
         # delivered whole prior to 1985, because that's the best we have.
         whole_pounds = ifelse(ROUND_POUNDS == 0, POUNDS, ROUND_POUNDS)) %>% 
  select(year = YEAR,#ï..YEAR, #YEAR, 
         date, julian_day, Mgmt_area = G_MANAGEMENT_AREA_CODE, Stat = G_STAT_AREA,
         Adfg = ADFG_NO, trip_no = TRIP_NO, sell_date, Vessel = VESSEL_NAME, Port = PORT_CODE,  
         Cfec_permit = G_CFEC_FISHERY, Delivery_cde = DELIVERY_CODE, 
         Harvest = HARVEST, Harvest_cde = HARVEST_CODE, Spp_cde = SPECIES_CODE, 
         whole_pounds, 
         pounds = POUNDS) %>% 
  mutate(Stat = as.character(Stat),
         Harvest_cde = as.character(Harvest_cde)) -> ifdb_catch

str(past_catch,5)
str(ifdb_catch,5)

#================================================================================================
Dat<-read.csv(paste0("data/fishery/raw_data/fishery_bio_", 
                     YEAR, ".csv"))
#
{str(Dat)

Dat<-Dat %>% 
  mutate(date = ymd(parse_date_time(SELL_DATE, c("%Y-%m-%d %H:%M:%S"))), #ISO 8601 format
         julian_day = yday(date))

ex<-Dat$SELL_DATE[1]
exp<-parse_date_time(ex, c("%Y-%m-%d %H:%M:%S"))

exp<-parse_date_time(ex, c("%Y-%m-%d %H:%M:%S"))

str(exp)
ymd(exp, truncated=2)

yday(exp)

year(exp)
day(exp)
month(exp)

format(exp,"%Y-%m-%d")

Dat<-Dat %>% 
  mutate(date = format(parse_date_time(SELL_DATE, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d"), #ISO 8601 format
         julian_day = yday(date))
}

Dat<-Dat %>% 
  mutate(date = format(parse_date_time(SELL_DATE, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d"), #ISO 8601 format
         julian_day = yday(date),
         Sex = derivedFactor("Male" = SEX_CODE == "01",
                             "Female" = SEX_CODE == "02",
                             .default = NA),
         Maturity = derivedFactor("0" = MATURITY_CODE %in% c("01", "02"), 
                                  "1" = MATURITY_CODE %in% c("03", "04", "05", "06", "07"),
                                  .default = NA),
         Gear = "Longline") %>% #,
  select(year = ï..YEAR, Project_cde = PROJECT_CODE, trip_no = TRIP_NO, 
            Adfg = ADFG_NO, Vessel = VESSEL_NAME, date, julian_day,
            Stat = G_STAT_AREA, Mgmt_area = G_MANAGEMENT_AREA_CODE,
            Sample_type = SAMPLE_TYPE, Spp_cde = SPECIES_CODE, 
            length = LENGTH, weight = WEIGHT_KILOGRAMS,
            age = AGE, Sex, Maturity) %>% 
  mutate(Adfg = as.character(Adfg))

str(Dat); unique(Dat$PROJECT_CODE)

ymd(exp)

ymd(parse_date_time(ex, c("%Y-%m-%d %H:%M:%S")))

read_csv(paste0("data/fishery/raw_data/fishery_bio_", 
                YEAR, ".csv"), 
         guess_max = 50000) %>% 
  mutate(date = format(as.Date(parse_date_time(SELL_DATE, c("%Y-%m-%d %H:%M:%S")),"%Y-%m-%d")), #ISO 8601 format
         julian_day = yday(date),
         Sex = derivedFactor("Male" = SEX_CODE == "01",
                             "Female" = SEX_CODE == "02",
                             .default = NA),
         Maturity = derivedFactor("0" = MATURITY_CODE %in% c("01", "02"), 
                                  "1" = MATURITY_CODE %in% c("03", "04", "05", "06", "07"),
                                  .default = NA),
         Gear = "Longline") %>% #,
  select(year = YEAR, Project_cde = PROJECT_CODE, trip_no = TRIP_NO, 
         Adfg = ADFG_NO, Vessel = VESSEL_NAME, date, julian_day,
         Stat = G_STAT_AREA, Mgmt_area = G_MANAGEMENT_AREA_CODE,
         Sample_type = SAMPLE_TYPE, Spp_cde = SPECIES_CODE, 
         length = LENGTH, weight = WEIGHT_KILOGRAMS,
         age = AGE, Sex, Maturity) %>% 
  mutate(Adfg = as.character(Adfg)) -> fsh_bio

str(fsh_bio)
fsh_bio$date<-as.Date(fsh_bio$date)
fsh_bio$Sex<-as.character(fsh_bio$Sex)
fsh_bio$Maturity<-as.character(fsh_bio$Maturity)
fsh_bio$Project_cde<-as.character(fsh_bio$Project_cde)
str(past_fsh_bio)

tst<-bind_rows(past_fsh_bio, fsh_bio)





















