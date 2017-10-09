# work up of biological data
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2017-10-09

# load ----
source("r_code/helper.R")

# data -----
srv_bio <- read_csv("data/survey/survey_bio_1988_2016.csv")

glimpse(srv_bio)
View(srv_bio)
srv_bio %>% 
  mutate(Year = factor(year),
         Project = factor(Project),
         )
