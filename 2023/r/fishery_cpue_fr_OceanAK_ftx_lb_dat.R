# 2. Fishery cpue ---- 2023

# This script will replace past (2022 and earlier) CPUE calculation that relied
# on a long sql script that was not documented in written format.  When Rhea and 
# staff reworked the logbook data that script became even more problematic and 
# required programmer Justin Daily to run the scripts on the revamped logbook
# data.  Rather than continue this undocumented and "black box" calculation of
# CPUE this script will utilize data pulled directly from OceanAK.  

#-----HISTORY------------------------------------------------------------------
# HISTORY details from Jane Sullivan's original 2021 analysis:
# from JANE SULLIVAN in 2021 Current documentation: In 2020, Rhea Ehresmann 
# and GF project staff worked
# with Karl Wood to create a new cpue data input application. As part, they
# redefined how set targets are defined. This means Kallenburger's scripts that
# match fish ticket poundage data to logbook data are likely depricated and will
# need a revamp. If this is the case, my recommendation is to work with Justin
# Daily and the new biometrician to develop new methods (ideally in R). In the
# mean time, the statistical catch-at-age model will accept NULLS for this data
# component. This part of the model likelihood receives a relatively low weight,
# so I anticipate this will have little influence on model results. The full
# time series will need to be redeveloped once this has happened.

# Past documentation: Kamala Carroll pulls the IFDB view out_g_log_longline_c_e,
# populates new columns effort_target_species_code and effort_comments, and
# sends to Scott Johnson. Scott runs a series of Martina Kallenburger's sql
# scripts (https://github.com/commfish/seak_sablefish/issues/7) that match fish
# tickets pounds back to set based on Kamala's set target designation based on
# some undocumented methodology (proportional to numbers/pounds in logbook?). It
# lives in a view called sablefish_cpue_final_view in scottj's IFDB schema,
# though he has made a public synonym for it. This output doesn't contain
# information on sets designated as halibut targets. Note that it is missing
# effort_no's (effort_no's = individual sets).
#----------------------------------------------------------------------------------
# most recent year of data
YEAR <- 2022

# Load ----
source("r_helper/helper.r")
source("r_helper/functions.r")

#---------------------------------------------------------------------------------
# Ocean AK filters and data sources:
# 1) Logbook Data
#   A) Longline :Longline_logbook_sable_and_halibut_target_1997-2005_for_CPUE.csv
#                Longline_logbook_sable_and_halibut_target_2006-now_for_CPUE.csv
#     i) Year >= 2006
#     ii) Trip primary target = sablefish & halibut
#     iii) Groundfish Management Area Code = NSEI
#   B) Pot: Pot_logbook_sable_and_halibut_target_2006-now_for_CPUE.csv
#     i) Year >= 2006
#     ii) Trip primary target = sablefish & halibut
#     iii) Groundfish Management Area Code = NSEI
# 2) V1 Fish ticket data: NSEI_ftx_sablefish_halibut_pot_longline_1997-now.csv
#     i) Year >=1997
#     ii) Gear = longline or pot
#     iii) species = halibut or sablefish
#     iv) Mgt Area District = NSEI
#     v) CFEC Code = B05B;B06B;B06Z;B06ZL;B09B;B25B;B26B;B61B;B61ZL;B91B;BAKE;BO6B;C61A; 0000, 9998, 9999
#         *0000, 9998 & 9999 for missing values
#         *halibut must be landed on a "B" permit, and Chatham sablefish must be 
#          landed on "C61A" permit. Otherwise any other C-permit is IFQ or Clarence 
#          sablefish, I-permits = directed lingcod, M-permits = misc finfish like 
#          Pcod and older rockfish landings, hagfish, etc., S-permits = salmon, 
#          Y-permits = directed DSR.
#
# NSEI stat areas = 325700; 335631, 335634, 335701, 335702, 335703, 335704, 335705
#                     335731, 335732, 335733, 335734, 335735, 345534, 345603, 345605
#                     345631, 345632, 345701, 345702, 345703, 345704, 345705, 345706
#                     345731, 345732, 345801, 345802, 345803, 345830, 355707, 355731
#                     355732, 355733, 355801, 355802, 355830, 355900, 365804, 365830
# 3

#ll_lb<-rbind(read.csv(paste0(YEAR+1,"/data/fishery/raw_data/Longline_logbook_sable_and_halibut_target_1997-2005_for_CPUE.csv")),
#             read.csv(paste0(YEAR+1,"/data/fishery/raw_data/Longline_logbook_sable_and_halibut_target_2006-now_for_CPUE.csv")))

ll_lb<-read.csv(paste0(YEAR+1,"/data/fishery/raw_data/Longline logbook sable and halibut target 1997-now for CPUE.csv"))
str(ll_lb)
colnames(ll_lb)

pot_lb<-read.csv(paste0(YEAR+1,"/data/fishery/raw_data/Pot_logbook_sable_and_halibut_target_2006-now_for_CPUE.csv"))

ftx<-read.csv(paste0(YEAR+1,"/data/fishery/raw_data/NSEI_ftx_sablefish_halibut_pot_longline_1997-now.csv"))
str(ftx)
#----------------------------------------------------------------------------------
str(ll_lb)
unique(ll_lb$Gear)
unique(ll_lb$Longline.System)
unique(ll_lb$Config.List)

ll_lb<-ll_lb %>% 
  mutate(CONFIG_TARGET_SP_CODE=NA, HOOK_SIZE=NA, HOOK_SPACING=NA, 
         HOOKS_PER_SKATE = NA, NUMBER_OF_SKATES = NA, NUMBER_OF_HOOKS = NA,
         CONFIG_TARGET_SP_CODE_2=NA, HOOK_SIZE_2=NA, HOOK_SPACING_2=NA, 
         HOOKS_PER_SKATE_2 = NA, NUMBER_OF_SKATES_2 = NA, NUMBER_OF_HOOKS_2 = NA)

# 1) Split up ll_lb$Config.List into Target Species, Hook Size,Spacing, Hooks/Skate, Num Skates, Spacing
grab6<-round(runif(6,1,nrow(ll_lb)),0)
sam<-as.data.frame(cbind(ll_lb$Year[c(grab6)],   #sam will get replaced by ll_lb
                         ll_lb$Config.List[c(grab6)]))
colnames(sam)<-c("Year","Shit")

ridiculous8<-ll_lb[stringr::str_count(ll_lb$Config.List, ",") == 8,]
#ridiculous8$Config.List
grab3<-round(runif(3,1,nrow(ridiculous8)),0)
ridiculous8<-as.data.frame(cbind(ridiculous8$Year[c(grab3)],
                                 ridiculous8$Config.List[c(grab3)]))
colnames(ridiculous8)<-c("Year","Shit")

ridiculous9<-ll_lb[stringr::str_count(ll_lb$Config.List, ",") == 9,]
ridiculous9$Config.List
grab3<-round(runif(3,1,nrow(ridiculous9)),0)
ridiculous9<-as.data.frame(cbind(ridiculous9$Year[c(grab3)],ridiculous9$Config.List[c(grab3)]))
colnames(ridiculous9)<-c("Year","Shit")

ridiculous12<-ll_lb[stringr::str_count(ll_lb$Config.List, ",") == 12,]
ridiculous12$Config.List
ridiculous12<-as.data.frame(cbind(ridiculous12$Year[c(1:2)],ridiculous12$Config.List[c(1:2)]))
colnames(ridiculous12)<-c("Year","Shit")

sam<-rbind(sam, ridiculous8,ridiculous9,ridiculous12)
#Argh... need to divid up around "---" and deal with two configuration lists... 

n_col<-max(stringr::str_count(sam$Shit, ","))+1
n_precol<-max(stringr::str_count(sam$Shit, "---"))+1

sam %>% separate(Shit, into = paste0("precol",1:n_precol),sep = "---") -> prestep
n_col1<-max(stringr::str_count(prestep$precol1, ":"))+1
n_col2<-max(stringr::str_count(prestep$precol2[!is.na(prestep$precol2)], ":"))+1

prestep %>% mutate(Shit1 = gsub(")","",sub(".","",precol1))) %>%
  mutate(Shit2 = gsub(")","",sub(".","",precol2))) %>%
  mutate(Shit2 = sub(".","",Shit2)) %>%
  select(Year, Shit1, Shit2) -> prestep

Shit1cols<-max(stringr::str_count(prestep$Shit1, ":"), na.rm=T)+1
Shit2cols<-max(stringr::str_count(prestep$Shit2, ":"), na.rm=T)+1
 
prestep %>%        
  separate(Shit1, into = paste0("col", 1:(Shit1cols)),sep = ",") %>%
  separate(Shit2, into = paste0("col", (1+Shit1cols):(1+Shit1cols+Shit2cols)),sep = ",") -> Step0
#1-7 = first config
#8-14 = second config
#  mutate(across(everything(),~dplyr::na_if(.,""))) -> Step0 #%>%

Step0<-Step0 %>% 
  mutate(CONFIG_TARGET_SP_CODE=NA, HOOK_SIZE=NA, HOOK_SPACING=NA, 
         HOOKS_PER_SKATE = NA, NUMBER_OF_SKATES = NA, NUMBER_OF_HOOKS = NA,
         CONFIG_TARGET_SP_CODE_2=NA, HOOK_SIZE_2=NA, HOOK_SPACING_2=NA, 
         HOOKS_PER_SKATE_2 = NA, NUMBER_OF_SKATES_2 = NA, NUMBER_OF_HOOKS_2 = NA)

for (i in 1:nrow(Step0)) {  #i<-21
  #1st Config list
  vals<-vector()
  #need to identify when it switches to second set of set configuration...???
  #da<-Step0[i,]
  da<-select(Step0[i,],contains("col"))
  cats<-sub("(.*?)[\\.|:].*", "\\1", da)
  targ_refs<-which(cats=="Target Species")  #column numbers 
  
  if (length(targ_refs) > 1) {
    second.config <- targ_refs[2]
  } else {
    second.config <- length(cats[!is.na(cats)])+1
  }
  
  #1st Gear Configuration Breakdown
  for (j in 1:(second.config-1)){   #j<-3
    val<-Step0[i,which(colnames(Step0)==paste0("col",j))]
    vals[j]<-val
    #val reference if it's just a number so we know where to add it... 
    if (!is.na(as.numeric(val))) {
      if (is.na(as.numeric(vals[j-1]))) {
        ref<-sub("(.*?)[\\.|:].*", "\\1", vals[j-1])
      } else {ref<-sub("(.*?)[\\.|:].*", "\\1", vals[j-2])}
      
    } else {ref<-NA}
    
    #Target
    if (grepl("Target Species",val) & !is.na(val)) {
      Step0[i,"CONFIG_TARGET_SP_CODE"] = as.numeric(gsub("[^0-9]","",val))
    } else {}
    
    #Hook Size 
    if (grepl("Hook Size",val) & !is.na(val)) {
      Step0[i,"HOOK_SIZE"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Hook Size",ref)) {
        Step0[i,"HOOK_SIZE"] = paste0(Step0[i,"HOOK_SIZE"],",",val)
      } else {}
    }
    
    # Spacing
    if (grepl("Spacing",val) & !is.na(val)) {
      Step0[i,"HOOK_SPACING"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Spacing",ref)) {
        Step0[i,"HOOK_SPACING"] = paste0(Step0[i,"HOOK_SPACING"],",",val)
      } else {}
    }  
    
    #Hooks per skate
    if (grepl("Hooks/Skate",val) & !is.na(val)) {
      Step0[i,"HOOKS_PER_SKATE"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Hooks/Skate",ref)) {
        Step0[i,"HOOKS_PER_SKATE"] = paste0(Step0[i,"HOOKS_PER_SKATE"],",",val)
      } else {}
    } 
    
    #number of hooks
    if (grepl("Num Hooks",val) & !is.na(val)) {
      Step0[i,"NUMBER_OF_HOOKS"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Num Skates",ref)) {
        Step0[i,"NUMBER_OF_HOOKS"] = paste0(Step0[i,"NUMBER_OF_HOOKS"],",",val)
      } else {}
    }  
    
    #number of skates
    if (grepl("Num Skates",val) & !is.na(val)) {
      Step0[i,"NUMBER_OF_SKATES"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Num Skates",ref)) {
        Step0[i,"NUMBER_OF_SKATES"] = paste0(Step0[i,"NUMBER_OF_SKATES"],",",val)
      } else {}
    }   
  }
  
  #2nd Gear Configuration list
  if (length(targ_refs) > 1) {
    for (j in targ_refs:(length(cats))){   #j<-1
      val<-Step0[i,which(colnames(Step0)==paste0("col",j))]
      #Target
      if (grepl("Target Species",val) & !is.na(val)) {
        Step0[i,"CONFIG_TARGET_SP_CODE_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {}
      #Hook Size 
      if (grepl("Hook Size",val) & !is.na(val)) {
        Step0[i,"HOOK_SIZE_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Hook Size",ref)) {
          Step0[i,"HOOK_SIZE_2"] = paste0(Step0[i,"HOOK_SIZE_2"],",",val)
        } else {}
      }
      
      # Spacing
      if (grepl("Spacing",val) & !is.na(val)) {
        Step0[i,"HOOK_SPACING_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Spacing",ref)) {
          Step0[i,"HOOK_SPACING_2"] = paste0(Step0[i,"HOOK_SPACING_2"],",",val)
        } else {}
      }  
      
      #Hooks per skate
      if (grepl("Hooks/Skate",val) & !is.na(val)) {
        Step0[i,"HOOKS_PER_SKATE_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Hooks/Skate",ref)) {
          Step0[i,"HOOKS_PER_SKATE_2"] = paste0(Step0[i,"HOOKS_PER_SKATE_2"],",",val)
        } else {}
      } 
      
      #number of hooks
      if (grepl("Num Hooks",val) & !is.na(val)) {
        Step0[i,"NUMBER_OF_HOOKS2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Num Skates",ref)) {
          Step0[i,"NUMBER_OF_HOOKS2"] = paste0(Step0[i,"NUMBER_OF_HOOKS2"],",",val)
        } else {}
      }  
      
      #number of skates
      if (grepl("Num Skates",val) & !is.na(val)) {
        Step0[i,"NUMBER_OF_SKATES_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Num Skates",ref)) {
          Step0[i,"NUMBER_OF_SKATES_2"] = paste0(Step0[i,"NUMBER_OF_SKATES_2"],",",val)
        } else {}
      }   
    }
  } else {}
  
}



#1) Whittle the data down to what's needed for RAW file
ll_lb<-ll_lb %>% 
  mutate(Date = as.Date(Time.Hauled),
         Set_Length = Set.Length..mi.,
         Sell_Date = as.Date(Sell.Date),
         DATE_LEFT_PORT = as.Date(Date.Left.Port),
         AVERAGE_DEPTH_METERS = 0.54680665*Average.Depth.Fathoms) %>% 
  select(YEAR = Year, PROJECT_CODE = Project.Code, TRIP_NO = Trip.Number, 
         ADFG_NO = ADFG.Number, LONGLINE_SYSTEM = Longline.System, 
         DATE_LEFT_PORT,
         SELL_DATE = Sell_Date, LONGLINE_SYSTEM_CODE = Longline.System.Code,
         SOAK_TIME = Soak.Time.Hours,
         
         #HOOK_SIZE = NA, HOOK_SPACING = NA, HOOKS_PER_SKATE = NA, 
         #NUMBER_OF_SKATES = NA, NUMBER_OF_HOOKS = NA, 
         
         AVERAGE_DEPTH_METERS, 
         G_MANAGEMENT_AREA_CODE = Groundfish.Management.Area.Code,
         G_STAT_AREA = Groundfish.Stat.Area, 
         TRIP_TARGET = Trip.Primary.Target.Species,
         TRIP_TARGET2 = Trip.Secondary.Target.Species,
         SET_TARGET = Effort.Primary.Target.Species,
         SET_TARGET2 = Effort.Secondary.Target.Species,
         EFFORT_NO = Effort.Number, 
         
         #SABLE_LOG_LBS = ,
         #SABLE_NUMBERS = ,
         #SABLE_TKT_ROUND_LBS = ,
         #SABLE_LBS_PER_SET = ,
         #HALIBUT_LOG_LBS = ,
         #HALIBUT_NUMBERS = ,
         #HALIBUT_ROUND_LBS = ,
         #HALIBUT_LBS_PER_SET = ,
         
         SET_COMMENT = Effort.Comments, 
         #TRIP_COMMENT = NA, 
         TIME_SET = Time.Set,
         TIME_HAULED = Time.Hauled,
         START_LATITUDE_DECIMAL_DEGREES = Start.Latitude.Decimal.Degrees,
         START_LONGITUDE_DECIMAL_DEGREE = Start.Longitude.Decimal.Degrees) 

str(ll_lb)

str(ftx)
unique(ftx$Trip.Number)
unique(ftx$Gear.Code)
unique(ftx$Gear.Name)

ftx<-ftx %>% 
  mutate() %>%
  select(YEAR = Year,     #**Join
         PROJECT_CODE = NA,
         TRIP_NO = Trip.Number, #**Join
         ADFG_NO = ADFG,        #**Join
         LONGLINE_SYSTEM = NA, 
         DATE_LEFT_PORT = NA,
         SELL_DATE = Sell_Date, 
         LONGLINE_SYSTEM_CODE = Longline.System.Code,
         SOAK_TIME = Soak.Time.Hours,
         
         #HOOK_SIZE = NA, HOOK_SPACING = NA, HOOKS_PER_SKATE = NA, 
         #NUMBER_OF_SKATES = NA, NUMBER_OF_HOOKS = NA, 
         
         AVERAGE_DEPTH_METERS, 
         G_MANAGEMENT_AREA_CODE = Groundfish.Management.Area.Code,
         G_STAT_AREA = Groundfish.Stat.Area, 
         TRIP_TARGET = Trip.Primary.Target.Species,
         TRIP_TARGET2 = Trip.Secondary.Target.Species,
         SET_TARGET = Effort.Primary.Target.Species,
         SET_TARGET2 = Effort.Secondary.Target.Species,
         EFFORT_NO = Effort.Number, 
         
         #SABLE_LOG_LBS = ,
         #SABLE_NUMBERS = ,
         #SABLE_TKT_ROUND_LBS = ,
         #SABLE_LBS_PER_SET = ,
         #HALIBUT_LOG_LBS = ,
         #HALIBUT_NUMBERS = ,
         #HALIBUT_ROUND_LBS = ,
         #HALIBUT_LBS_PER_SET = ,
         
         SET_COMMENT = Effort.Comments, 
         #TRIP_COMMENT = NA, 
         TIME_SET = Time.Set,
         TIME_HAULED = Time.Hauled,
         START_LATITUDE_DECIMAL_DEGREES = Start.Latitude.Decimal.Degrees,
         START_LONGITUDE_DECIMAL_DEGREE = Start.Longitude.Decimal.Degrees)

#--------------------------------------------------------------------------------  
  select(Date, Year, Trip.Number, Effort_no = Effort.Number, Ticket, CFEC.Permit.Number, ADFG.Number,
         Set_Length, Depth = Average.Depth.Fathoms, Soak = Soak.Time.Hours,
         Species, Species.Code, Groundfish.Stat.Area, Groundfish.Management.Area.Code, 
         Disposition, Pounds, Numbers, 
         Effort.Primary.Target.Species, Effort.Secondary.Target.Species, 
         Sablefish.Discards, 
         Depredation, Number.of.Skates.Impacted.by.Depredation, 
         Sell_Date,
         Port, Vessel.Name, 
         Longline.System.Code, Longline.System, 
         Gear.Code, Gear.Class.Code, Gear, 
         Project, Project.Code, 
         Start.Longitude.Decimal.Degrees, Start.Latitude.Decimal.Degrees,
         End.Longitude.Decimal.Degrees, End.Latitude.Decimal.Degrees )

str(ll_lb)

eg<-ll_lb$Time.Set[1:4]

as.


##################################################################################
## SCRAP
##################################################################################

grepl("Hook Size",Step0[i,which(colnames(Step0)==paste0("col",j))])


Step0 %>% filter(sub("(.*?)[\\.|:].*", "\\1", col1) == "Hook Size") %>%
  mutate(HOOK_SIZE = gsub("[^0-9]","",col1)) %>% unfilter()




Step0 %>% mutate(col1_txt = sub("(.*?)[\\.|:].*", "\\1", col1),
                 col1_no = gsub("[^0-9]","",col1)) -> Step0.1


Step0.1 %>% pivot_wider(names_from=col1_txt,values_from=col1_no)

#----
varNames <- c("var1", "var2", "var3", "var4", "var5")
df <- data.frame(VariableID = rep(varNames, 5),
                 Serial = rep(1:5, 5),
                 Response = runif(25, 1, 10))

df
newdf <- df %>% 
  arrange(Serial) %>%
  group_by(Serial) %>% mutate(id=row_number()) %>% 
  pivot_wider(names_from=VariableID,values_from=Response) %>% select(-id)
#-----

Step0 %>% mutate(LL_gear = as.factor(sub("(.*?)[\\.|:].*", "\\1", value)))

Step0 %>% pivot_longer(-Year) %>%
  mutate(LL_gear = as.factor(sub("(.*?)[\\.|:].*", "\\1", value))) %>%
  arrange(LL_gear) %>%
  group_by(LL_gear) %>% mutate(id=row_number()) %>%
  pivot_wider(names_from=LL_gear, values_from=value) -> Step0.1

str(Step0.1)
unique(Step0.1)

pivot_wider(Year = Year, names_from = LL_gear, values_from = gsub("[^0-9]","",value))

Step0 %>% pivot_wider(Year = Year, names_from = value) %>%
  pivot_

Step0 %>% mutate(HOOK_SIZE = ifelse(sub("(.*?)[\\.|:].*", "\\1", col1) %in% c("Hook Size"),
                                    gsub("[^0-9]","",col1),
                                    ifelse(sub("(.*?)[\\.|:].*", "\\1", col2) %in% c("Hook Size"),
                                           gsub("[^0-9]","",col2),NA
                                    )))

Step0 %>% mutate(Ex_txt = sub("(.*?)[\\.|:].*", "\\1", col1),
                 Ex_no = gsub("[^0-9]","",col1))

select(Step0,contains("Hook Size"))

mutate(Hook_size = select(contains("Hook Size")))

mutate(Hook_size = gsub("[^0-9]","",select(contains("Hook Size"))))

mutate(Hook_size = gsub("[^0-9]","",col1))




pivot_longer(-Year) %>% 
  select(-name) %>% 
  drop_na() -> step1

step1<-as.data.frame(step1)
n_col2<-max(stringr::str_count(step1[,2], ":"))+1  
colnames(step1)<-c("Year","Shit") 

step1 %>% separate(Shit, into = paste0("col", 1:n_col),sep = ":") %>%
  mutate(across(everything(),~dplyr::na_if(.,""))) %>%
  pivot_longer(-Year) %>% 
  select(-name) %>% 
  drop_na() -> step2  


pivot_wider(Year = Year, names_from = hooks)








