# 2. Fishery cpue ---- 2023

# This script will replace past (2022 and earlier) CPUE calculations that relied
# on a long and mysterious sql script that was not documented in written format.  
# When Rhea and staff reworked the logbook data that script became even more 
# problematic and required programmer Justin Daily to run the scripts on the 
# revamped logbook data.  Rather than continue this undocumented and "black box" 
# calculation of CPUE this script will utilize data pulled directly from OceanAK.  

#-----Pre-2023-HISTORY------------------------------------------------------------------
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
# The column Config.List has all the information on hook size, spacing, # of skates
# etc.  All in one clunky, inconsistent column.  A better coder could likey use 
# dplyr::pivot_whatever to get this split out.  But not being a better coder
# I would up witing a loop that takes the data frame apart line by line to separate
# out all the data in that column.  It takes a couple of minutes to run (embarrassing) 
# but that's what I have for now.  The loop is at least annotated and recorded now... 

#Prep ll_lb to rip apart the Config.List column
{n_col<-max(stringr::str_count(ll_lb$Config.List, ","))+1      #Config.List replaced Shit from development code
n_precol<-max(stringr::str_count(ll_lb$Config.List, "---"))+1

ll_lb %>% separate(Config.List, into = paste0("precol",1:n_precol),sep = "---") -> ll_lb #prestep
n_col1<-max(stringr::str_count(ll_lb$precol1, ":"))+1
n_col2<-max(stringr::str_count(ll_lb$precol2[!is.na(ll_lb$precol2)], ":"))+1

ll_lb %>% mutate(junk1 = gsub(")","",sub(".","",precol1))) %>%   #junk1 and junk2
  mutate(junk2 = gsub(")","",sub(".","",precol2))) %>%
  mutate(junk2 = sub(".","",junk2)) %>%
  select(-precol1, -precol2) -> ll_lb #prestep

junk1cols<-max(stringr::str_count(ll_lb$junk1, ":"), na.rm=T)+1
junk2cols<-max(stringr::str_count(ll_lb$junk2, ":"), na.rm=T)+1
 
ll_lb %>%        
  separate(junk1, into = paste0("col", 1:(junk1cols)),sep = ",") %>%
  separate(junk2, into = paste0("col", (1+junk1cols):(1+junk1cols+junk2cols)),sep = ",") -> ll_lb}

# This loop will pull apart the Config.List column and break it up into the different
# pieces of longline gear info... 
for (i in 1:nrow(ll_lb)) {  #i<-21
  #1st Config list
  vals<-vector()
  #need to identify when it switches to second set of set configuration...???
  #da<-Step0[i,]
  da<-select(ll_lb[i,],contains("col"))
  cats<-sub("(.*?)[\\.|:].*", "\\1", da)
  targ_refs<-which(cats=="Target Species")  #column numbers 
  
  if (length(targ_refs) > 1) {
    second.config <- targ_refs[2]
  } else {
    second.config <- length(cats[!is.na(cats)])+1
  }
  
  #1st Gear Configuration Breakdown
  for (j in 1:(second.config-1)){   #j<-3
    val<-ll_lb[i,which(colnames(ll_lb)==paste0("col",j))]
    vals[j]<-val
    #val reference if it's just a number so we know where to add it... 
    if (!is.na(as.numeric(val))) {
      if (is.na(as.numeric(vals[j-1]))) {
        ref<-sub("(.*?)[\\.|:].*", "\\1", vals[j-1])
      } else {ref<-sub("(.*?)[\\.|:].*", "\\1", vals[j-2])}
      
    } else {ref<-NA}
    
    #Target
    if (grepl("Target Species",val) & !is.na(val)) {
      ll_lb[i,"CONFIG_TARGET_SP_CODE"] = as.numeric(gsub("[^0-9]","",val))
    } else {}
    
    #Hook Size 
    if (grepl("Hook Size",val) & !is.na(val)) {
      ll_lb[i,"HOOK_SIZE"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Hook Size",ref)) {
        ll_lb[i,"HOOK_SIZE"] = paste0(ll_lb[i,"HOOK_SIZE"],",",val)
      } else {}
    }
    
    # Spacing
    if (grepl("Spacing",val) & !is.na(val)) {
      ll_lb[i,"HOOK_SPACING"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Spacing",ref)) {
        ll_lb[i,"HOOK_SPACING"] = paste0(ll_lb[i,"HOOK_SPACING"],",",val)
      } else {}
    }  
    
    #Hooks per skate
    if (grepl("Hooks/Skate",val) & !is.na(val)) {
      ll_lb[i,"HOOKS_PER_SKATE"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Hooks/Skate",ref)) {
        ll_lb[i,"HOOKS_PER_SKATE"] = paste0(ll_lb[i,"HOOKS_PER_SKATE"],",",val)
      } else {}
    } 
    
    #number of hooks
    if (grepl("Num Hooks",val) & !is.na(val)) {
      ll_lb[i,"NUMBER_OF_HOOKS"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Num Skates",ref)) {
        ll_lb[i,"NUMBER_OF_HOOKS"] = paste0(ll_lb[i,"NUMBER_OF_HOOKS"],",",val)
      } else {}
    }  
    
    #number of skates
    if (grepl("Num Skates",val) & !is.na(val)) {
      ll_lb[i,"NUMBER_OF_SKATES"] = as.numeric(gsub("[^0-9]","",val))
    } else {
      if(!is.na(as.numeric(val)) & grepl("Num Skates",ref)) {
        ll_lb[i,"NUMBER_OF_SKATES"] = paste0(ll_lb[i,"NUMBER_OF_SKATES"],",",val)
      } else {}
    }   
  }
  
  #2nd Gear Configuration list
  if (length(targ_refs) > 1) {
    for (j in second.config:(length(cats))){   #j<-1
      val<-ll_lb[i,which(colnames(ll_lb)==paste0("col",j))]
      vals[j]<-val
      
      if (!is.na(as.numeric(val))) {
        if (is.na(as.numeric(vals[j-1]))) {
          ref<-sub("(.*?)[\\.|:].*", "\\1", vals[j-1])
        } else {ref<-sub("(.*?)[\\.|:].*", "\\1", vals[j-2])}
        
      } else {ref<-NA}
      
      #Target
      if (grepl("Target Species",val) & !is.na(val)) {
        ll_lb[i,"CONFIG_TARGET_SP_CODE_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {}
      #Hook Size 
      if (grepl("Hook Size",val) & !is.na(val)) {
        ll_lb[i,"HOOK_SIZE_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Hook Size",ref)) {
          ll_lb[i,"HOOK_SIZE_2"] = paste0(ll_lb[i,"HOOK_SIZE_2"],",",val)
        } else {}
      }
      
      # Spacing
      if (grepl("Spacing",val) & !is.na(val)) {
        ll_lb[i,"HOOK_SPACING_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Spacing",ref)) {
          ll_lb[i,"HOOK_SPACING_2"] = paste0(ll_lb[i,"HOOK_SPACING_2"],",",val)
        } else {}
      }  
      
      #Hooks per skate
      if (grepl("Hooks/Skate",val) & !is.na(val)) {
        ll_lb[i,"HOOKS_PER_SKATE_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Hooks/Skate",ref)) {
          ll_lb[i,"HOOKS_PER_SKATE_2"] = paste0(ll_lb[i,"HOOKS_PER_SKATE_2"],",",val)
        } else {}
      } 
      
      #number of hooks
      if (grepl("Num Hooks",val) & !is.na(val)) {
        ll_lb[i,"NUMBER_OF_HOOKS2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Num Skates",ref)) {
          ll_lb[i,"NUMBER_OF_HOOKS2"] = paste0(ll_lb[i,"NUMBER_OF_HOOKS2"],",",val)
        } else {}
      }  
      
      #number of skates
      if (grepl("Num Skates",val) & !is.na(val)) {
        ll_lb[i,"NUMBER_OF_SKATES_2"] = as.numeric(gsub("[^0-9]","",val))
      } else {
        if(!is.na(as.numeric(val)) & grepl("Num Skates",ref)) {
          ll_lb[i,"NUMBER_OF_SKATES_2"] = paste0(ll_lb[i,"NUMBER_OF_SKATES_2"],",",val)
        } else {}
      }   
    }
  } else {}
  
}

# examine some lines to see if this worked...
ll_lb[24189,]

grab10<-round(runif(10,1,nrow(ll_lb)))
ll_lb[grab10,]

doubles<-ll_lb[!is.na(ll_lb$CONFIG_TARGET_SP_CODE_2),]
grab5<-round(runif(5,1,nrow(doubles)))
doubles[grab5,]

#Should save this ll_lb so that don't need to rerun this shitty loop... 
colnames(ll_lb)

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
         
         CONFIG_TARGET_SP_CODE,
         HOOK_SIZE, HOOK_SPACING, HOOKS_PER_SKATE, 
         NUMBER_OF_SKATES, NUMBER_OF_HOOKS, 
         
         CONFIG_TARGET_SP_CODE_2,
         HOOK_SIZE_2, HOOK_SPACING_2, HOOKS_PER_SKATE_2, 
         NUMBER_OF_SKATES_2, NUMBER_OF_HOOKS_2,
         
         AVERAGE_DEPTH_METERS, 
         G_MANAGEMENT_AREA_CODE = Groundfish.Management.Area.Code,
         G_STAT_AREA = Groundfish.Stat.Area, 
         TRIP_TARGET = Trip.Primary.Target.Species,
         TRIP_TARGET2 = Trip.Secondary.Target.Species,
         SET_TARGET = Effort.Primary.Target.Species,
         SET_TARGET2 = Effort.Secondary.Target.Species,
         
         TRIP_TARGET_CODE = Trip.Primary.Target.Species.Code,
         TRIP_TARGET2_CODE = Trip.Secondary.Target.Species.Code,
         SET_TARGET_CODE = Effort.Primary.Target.Species.Code,
         SET_TARGET2_CODE = Effort.Secondary.Target.Species.Code,
         
         EFFORT_NO = Effort.Number, 
         
         SET_COMMENT = Effort.Comments, 
         #TRIP_COMMENT = NA, 
         TIME_SET = Time.Set,
         TIME_HAULED = Time.Hauled,
         START_LATITUDE_DECIMAL_DEGREES = Start.Latitude.Decimal.Degrees,
         START_LONGITUDE_DECIMAL_DEGREE = Start.Longitude.Decimal.Degrees) 

str(ll_lb)
nrow(ll_lb)
nrow(unique(ll_lb))
#lots of duplicate entries... :( 
ll_lb<-unique(ll_lb)
#------------------------------------------------------------------------------
# ftx
str(ftx)
unique(ftx$Trip.Number)
unique(ftx$Gear.Code)
unique(ftx$Gear.Name)


nrow(ftx[is.na(ftx$Trip.Number),])/nrow(ftx)
length(unique(ftx$Year.Office.BN))/nrow(ftx)
length(unique(ftx$CFEC))/nrow(ftx)
length(unique(ftx$Ticket.Count))/nrow(ftx)
unique(ftx$Ticket.Count)
length(unique(ftx$CFEC.1))/nrow(ftx)
length(unique(ftx$Batch.Number))/nrow(ftx)
length(unique(ftx$Trip.Number))/nrow(ftx)
length(unique(ftx$Sequential.Number))/nrow(ftx)
length(unique(ftx$Record.ID))/nrow(ftx)

unique(ftx$Year)
unique(ftx$Permit.Year)
unique(ftx$CFEC.Fishery.Code)
unique(ftx$Disposition.Name)
unique(ftx$Delivery.Condition.Name)
unique(ftx$Species.Name)
unique(ftx$Harvest.Name)
unique(ftx$Sold)
unique(ftx$Discard.at.Sea)
unique(ftx$Gear.Name)
unique(ftx$Port)
unique(ftx$Processor.Type.Name)
unique(ftx$Office.Name)

unique(ftx$Region)
unique(ftx$Mgt.Area.District)
unique(ftx$Regulation.Mgt.Area)
unique(ftx$FMP.Area)
unique(ftx$Stat.Area)

colnames(ftx)

eg<-ftx$Date.of.Landing..MM.DD.

ll_ftx<-ftx %>% 
  filter(Gear.Name == "Longline") %>%
  mutate(DATE_FISHING_BEGAN = as.Date(Date.Fishing.Began),
         DATE_LANDING = as.Date(Date.of.Landing),
         PERMIT_START_DATE = as.Date(Start.Date),
         PERMIT_END_DATE = as.Date(End.Date)
         ) %>%
  select(YEAR = Year,     #**Join
         DATE_FISHING_BEGAN, DATE_LANDING,
         PERMIT_START_DATE, PERMIT_END_DATE,
         #PROJECT_CODE = NA,  #from lb
         #identifiers:
         TRIP_NO = Trip.Number, #**Join
         ADFG_NO = ADFG,        #**Join
         SEQU_NO = Sequential.Number,
         RECORD_ID = Record.ID,
         CFEC = CFEC,
         CFEC_CODE = CFEC.Fishery.Code,
         G_STAT_AREA = Stat.Area,
         DATE_FISHING_BEGAN, DATE_LANDING,
         
         SPECIES = Species.Name,
         SPECIES_CODE = Species.Code,
         HARVEST_NAME = Harvest.Name,
         HARVEST_CODE = Harvest.Code,
         
         DISPOSITION_NAME = Disposition.Name,
         DISPOSITION_CODE = Disposition.Code,
         
         DELIVERY_COND = Delivery.Condition.Name,
         DELIVERY_COND_CODE = Delivery.Condition.Code,
         
         DISCARD_AT_SEA = Discard.at.Sea,
         
         LANDED_WEIGHT_SUM_lbs = Landed.Weight..sum.,
         WHOLE_WEIGHT_SUM_lbs = Whole.Weight..sum.,
         NUMBER_OF_FISH = Number.of.Animals..sum.
         )

str(ll_ftx); str(ll_lb)
unique(ll_ftx$SPECIES)

##FLAG !! Need to get rid of longline survey data from fish tickets... (Test fishery, HARVEST_CODE = 43
ll_ftx<-ll_ftx %>% filter(HARVEST_CODE != 43)

#fish tickets broken down by Species
sab_ll_ftx<-ll_ftx %>% filter (SPECIES_CODE == 710)
hal_ll_ftx<-ll_ftx %>% filter(SPECIES_CODE == 200)

#logbook broken down by TRIP TARGET
sab_ll_lb<-ll_lb %>% filter(TRIP_TARGET_CODE == 710)
hal_ll_lb<-ll_lb %>% filter(TRIP_TARGET_CODE == 200)
#subset small chunk to play with joins...

sa<-sample(unique(ll_lb$G_STAT_AREA),1)

{Y<-sample(unique(ll_lb$YEAR),1) #2005
an<-sample(unique(ll_lb$ADFG_NO[!is.na(ll_lb$ADFG_NO) & ll_lb$YEAR == Y]),1)

tix_eg<-ll_ftx %>% filter (YEAR == Y & ADFG_NO == an); nrow(tix_eg)
s_tix_eg<-sab_ll_ftx %>% filter (YEAR == Y & ADFG_NO == an); nrow(s_tix_eg)
h_tix_eg<-hal_ll_ftx %>% filter (YEAR == Y & ADFG_NO == an); nrow(h_tix_eg)

lb_eg<-ll_lb %>% filter (YEAR == Y & ADFG_NO == an); nrow(lb_eg)
s_lb_eg<-sab_ll_lb %>% filter (YEAR == Y & ADFG_NO == an); nrow(s_lb_eg)
h_lb_eg<-hal_ll_lb %>% filter (YEAR == Y & ADFG_NO == an); nrow(h_lb_eg)}

head(tix_eg)
#head(lb_eg)
unique(lb_eg$TRIP_NO) #does the number of unique trips match the number of unique
                      #landing dates
length(unique(lb_eg$TRIP_NO)); length(unique(tix_eg$DATE_LANDING))
length(unique(s_tix_eg$DATE_LANDING)); length(unique(h_tix_eg$DATE_LANDING))
length(unique(s_lb_eg$TRIP_NO));length(unique(h_lb_eg$TRIP_NO))

sort(unique(lb_eg$SELL_DATE)); sort(unique(tix_eg$DATE_LANDING))   #should match DATE_LANDING
sort(unique(s_tix_eg$DATE_LANDING));sort(unique(h_tix_eg$DATE_LANDING))
sort(unique(s_lb_eg$SELL_DATE));sort(unique(h_lb_eg$SELL_DATE))

sort(unique(lb_eg$G_STAT_AREA)); sort(unique(tix_eg$G_STAT_AREA)) #do they match?
sort(unique(s_tix_eg$G_STAT_AREA)); sort(unique(h_tix_eg$G_STAT_AREA))
sort(unique(s_lb_eg$G_STAT_AREA)); sort(unique(h_lb_eg$G_STAT_AREA))

unique(lb_eg$TRIP_TARGET)
unique(lb_eg$TRIP_TARGET2)
unique(lb_eg$SET_TARGET)
unique(lb_eg$SET_TARGET2)
unique(lb_eg$CONFIG_TARGET_SP_CODE)
unique(lb_eg$CONFIG_TARGET_SP_CODE_2)

tix_eg$SPECIES_CODE
lb_eg$TRIP_TARGET_CODE
lb_eg$TRIP_TARGET2_CODE
lb_eg$SET_TARGET_CODE
lb_eg$SET_TARGET2_CODE
lb_eg$CONFIG_TARGET_SP_CODE
lb_eg$CONFIG_TARGET_SP_CODE_2

#OK... now to see how many sets per trip... 
sort(unique(lb_eg$EFFORT_NO))

lb_eg[lb_eg$EFFORT_NO]

lb_eg$trip_effort<-paste0(lb_eg$TRIP_NO,"-",lb_eg$EFFORT_NO)
unique(lb_eg[lb_eg$TRIP_NO == 4072,])

unique(lb_eg)

#-------------------------------------------------------------------------------
# JOIN PLAN and NOTES...
## I think we can leave ftx with sab and hal combined for now and then calculate
## species specific stuff once they are joined... need to be careful about labels
## and not getting things confused... 

## OK, need to fish ticket species code = 710? do not want or need fish tickets from 
## halibut landings... OR maybe not? 

# Step 1) Split ftx up by SPECIES 
# Step 2) rename split ftx columns to be species specific
# Step 3) join both ftx into lb data frame... then you will have the amount landed
#         for each species in each trip
# Step 4) At that point it will be time to look at TARGET and relative numbers
#         caught by species and make call on which to include in calculating
#         CPUE statistics

# So the Join outline:
# 1) lb$SELL_DATE = ftx$DATE_LANDING
# 2) lb$G_STAT_AREA = ftx$G_STAT_AREA
# 

#################################################################################
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
# Code development for splitting apart gear configuration column

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

#for trouble shooting...
ll_lb<-read.csv(paste0(YEAR+1,"/data/fishery/raw_data/Longline logbook sable and halibut target 1997-now for CPUE.csv"))
{sam<-ll_lb[24189,c("Year","Config.List")]
colnames(sam)<-c("Year","junk")
#Argh... need to divid up around "---" and deal with two configuration lists... 

n_col<-max(stringr::str_count(sam$junk, ","))+1
n_precol<-max(stringr::str_count(sam$junk, "---"))+1

sam %>% separate(junk, into = paste0("precol",1:n_precol),sep = "---") -> prestep
n_col1<-max(stringr::str_count(prestep$precol1, ":"))+1
n_col2<-max(stringr::str_count(prestep$precol2[!is.na(prestep$precol2)], ":"))+1

prestep %>% mutate(junk1 = gsub(")","",sub(".","",precol1))) %>%
  mutate(junk2 = gsub(")","",sub(".","",precol2))) %>%
  mutate(junk2 = sub(".","",junk2)) %>%
  select(Year, junk1, junk2) -> prestep

junk1cols<-max(stringr::str_count(prestep$junk1, ":"), na.rm=T)+1
junk2cols<-max(stringr::str_count(prestep$junk2, ":"), na.rm=T)+1

prestep %>%        
  separate(junk1, into = paste0("col", 1:(junk1cols)),sep = ",") %>%
  separate(junk2, into = paste0("col", (1+junk1cols):(1+junk1cols+junk2cols)),sep = ",") -> Step0
#1-7 = first config
#8-14 = second config
#  mutate(across(everything(),~dplyr::na_if(.,""))) -> Step0 #%>%

Step0<-Step0 %>% 
  mutate(CONFIG_TARGET_SP_CODE=NA, HOOK_SIZE=NA, HOOK_SPACING=NA, 
         HOOKS_PER_SKATE = NA, NUMBER_OF_SKATES = NA, NUMBER_OF_HOOKS = NA,
         CONFIG_TARGET_SP_CODE_2=NA, HOOK_SIZE_2=NA, HOOK_SPACING_2=NA, 
         HOOKS_PER_SKATE_2 = NA, NUMBER_OF_SKATES_2 = NA, NUMBER_OF_HOOKS_2 = NA)
}

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
    
    for (j in second.config:(length(cats))){   #j<-second.config+2
      
      val<-Step0[i,which(colnames(Step0)==paste0("col",j))]
      vals[j]<-val
      
      if (!is.na(as.numeric(val))) {
        if (is.na(as.numeric(vals[j-1]))) {
          ref<-sub("(.*?)[\\.|:].*", "\\1", vals[j-1])
        } else {ref<-sub("(.*?)[\\.|:].*", "\\1", vals[j-2])}
        
      } else {ref<-NA}
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


#--------------------------------------------------------------------------------
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








