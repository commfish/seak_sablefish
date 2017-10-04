
# Fishery catch 1980-present, fishery CPUE 1997-present
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2017-10-03


# Also used for requested vessel-specific cpue

# Kray's old notes: These data are from a spreadsheet sent out by Scott Johnson each year.
# The sheet is the output from a SQL script written by Martina to condition
# each set to a target species, as opposed to the overall trip target. Logbooks were not included in IFDB until 1997. Commercial fishery CPUE
# values prior to 1997, for use in the ASA or other medium, are LEGACY VALUES
# found in ..data/legacy_fishery_cpue.csv

# Kray's original cpue_fish_vessel.r script used fsh_cpue.csv and 
# non_gam_cpue.csv. Ben Williams and I decided 2017-09-29 to use cpue_fsh.csv 
# because it goes back to  there was no documentation
# for the legacy cpue values


source("r_code/helper.r")

library(broom)

# data ----

#Vessel of interest if looking at cpue by vessel for a private request
VESSEL_REQUESTED <- "21465" #adfg number


cpue <- read.csv("data/fishery/raw_data/cpue_fsh.csv", header=TRUE, sep=",")

# rename, define factors, remove mixed hook sizes; calculate stanardized no. of 
# hooks and cpue
cpue <- cpue %>% 
  mutate(Year = factor(YEAR),
         Adfg = factor(ADFG_NO),
         Spp_cde = factor(TRIP_TARGET),
         date = dmy(SELL_DATE), #ISO 8601 format
         julian_day = yday(date),
         Gear = factor(LONGLINE_SYSTEM_CODE),
         Hook_size = HOOK_SIZE,
         Size = factor(as.numeric(gsub("[^0-9]","",Hook_size))),
         hook_space = HOOK_SPACING,
         hooks_per_skate = HOOKS_PER_SKATE,
         Stat = factor(G_STAT_AREA),
         depth = AVERAGE_DEPTH_METERS,
         sets = EFFORT_NO, #number of sets
         no_hooks = NUMBER_OF_HOOKS,         
         std_hooks = 2.2*no_hooks*(1-exp(-0.57*(2.54*hook_space/100))), #standardize hook spacing (Sigler & Lunsford 2001, CJFAS)
         sable_wt_set = SABLE_LBS_PER_SET, 
         std_cpue = sable_wt_set/std_hooks) %>% #standardized cpue
  filter(!is.na(date) & !is.na(hook_space) & 
           !is.na(hooks_per_skate) & 
           !is.na(sable_wt_set) &
           Spp_cde == "710" & #if there are any other spp (710=sablefish)
           julian_day > 226 & #if there were special projects before the fishery opened
           no_hooks < 15000) %>%  #*FLAG* where does 15000 come from? 14370 is the 75th percentile
  select(Year, Adfg, Spp_cde, date, julian_day, Gear, Hook_size,
         Size, hook_space, hooks_per_skate, Stat, depth, sets, no_hooks, 
         std_hooks, sable_wt_set, std_cpue)

#Get mean cpue, compare it with vector from Kray.
overall_cpue <- cpue %>% 
  group_by(Year) %>% 
  summarise(overall_cpue = mean(std_cpue)) 

#individual vessel cpue
vessel_cpue <- cpue %>% filter(Adfg == VESSEL_REQUESTED)



setwd("~/seak_sablefish/data/fishery")
annual_catch <- read.csv("annual_catch.csv") #old from ALEX
annual_catch2 <- read.csv("annual_catch2.csv") #new from OceanAK

annual_catch2 %>% 
  select(dressed_lbs = `Whole.Pounds..sum.`, #*FLAG* check in OceanAK what exactly Whole.Pounds..sum. is
         year = Batch.Year,
         harvest_code = Harvest.Code.And.Name) %>% 
  filter(!harvest_code %in% c("42 - Test fishery - special study",
                              "43 - Test fishery - long term stock assessment")) %>% 
  mutate(round_lbs = dressed_lbs/0.68, #*FLAG* needs citation
        round_mt = round_lbs*0.00045359) %>% 
  group_by(year) %>% 
  dplyr::summarise(catch_mt = sum(round_mt)) -> catch

#Historical legacy data (1980-1986), total annual catch in metric tons. No associated query available. 
hist_catch <- 
  data.frame(year = c(1980:1986),
             catch_mt = c(300.85,106.91,185.09,169.56,340.18,1410.21,1904.98))

catch <- rbind(hist_catch, catch)


annual_catch %>% arrange(YEAR) %>% select(YEAR, POUNDS, ROUND_POUNDS) %>% View()

#What are the differences in catch usuing the old output from ALEX vs. the new
#output from OceanAK?
annual_catch %>% 
  #prior to 1985 there is no 'ROUND_POUNDS' just POUNDS. After 1984, POUNDS is 
  #ROUND_POUNDS rounded to the nearest whole number. Keep the precision of
  #ROUND_POUNDS for later years, and fill in ROUND_POUNDS with POUNDS for past
  #years
  # mutate(POUNDS = ifelse(ROUND_POUNDS==0, POUNDS, ROUND_POUNDS)) %>% 
  select(year = YEAR,
         harvest_code = HARVEST_CODE,
         round_lbs = POUNDS) %>% 
  filter(!harvest_code %in% c(42,43)) %>% 
  mutate(round_mt = round_lbs*0.00045359) %>% 
  group_by(year) %>% 
  dplyr::summarise(catch_mt = sum(round_mt)) -> catch2

annual_catch %>% 
  #prior to 1985 there is no 'ROUND_POUNDS' just POUNDS. After 1984, POUNDS is 
  #ROUND_POUNDS rounded to the nearest whole number. Keep the precision of
  #ROUND_POUNDS for later years, and fill in ROUND_POUNDS with POUNDS for past
  #years
  # mutate(POUNDS = ifelse(ROUND_POUNDS==0, POUNDS, ROUND_POUNDS)) %>% 
  select(year = YEAR,
         harvest_code = HARVEST_CODE,
         round_lbs = ROUND_POUNDS) %>% 
  filter(!harvest_code %in% c(42,43)) %>% 
  mutate(round_mt = round_lbs*0.00045359) %>% 
  group_by(year) %>% 
  dplyr::summarise(catch_mt = sum(round_mt)) -> catch3

#Exploratory figs
setwd("~/seak_sablefish/figs/eda")
png("eda_annual_catch.png", height = 5,
    width = 11, units = 'in', res=300)
plot(catch$year, catch$catch_mt, type="o",
     xlab="Year", ylab="Commercial catch (mt)", pch=17)
abline(v=1985, lty=2, col="grey")
abline(v=1986, lty=2, col="grey")
abline(v=1997, lty=2, col="grey")
abline(v=2006, lty=2, col="grey")
points(catch2$year, catch2$catch_mt, col="magenta", pch=20)
lines(catch2$year, catch2$catch_mt, col="magenta")
points(catch3$year, catch3$catch_mt, col="blue", pch=20)
lines(catch3$year, catch3$catch_mt, col="blue")
legend(x = 2001, y = 4000, 
       legend = c("2016 assessment (OceanAK) w/ uncited values 1980-1986",
         "ROUND_POUNDS column (ALEX)",
         "POUNDS column (ALEX)"),
       lty = c(1,1,1), pch = c(17,20,20), 
       col = c("black","blue","magenta"), box.col="white", cex=.8)
dev.off()
setwd("~/seak_sablefish/data/fishery")

#Relationship between POUNDS (catch2, red) and ROUND_POUNDS (catch3, blue) cols. 1980-1984 
#ROUND_POUNDS=0, POUNDS populated; 1985-1996 POUNDS/ROUND_POUNDS = 0.63-0.65; 
#1997-2006 POUNDS/ROUND_POUNDS = 0.96-0.97; 2007-2015 POUNDS/ROUND_POUNDS > 0.99
#*FLAG* 1985-1996 are particularly confusing
data.frame(year = 1980:2015, 
           ratio = catch2$catch_mt/catch3$catch_mt) 

#Relationship between ROUND_POUNDS catch_mt and catch_mt that Kray used in 2017
#assessment from OceanAK (up to 2015). 1980-1984 ROUND_POUNDS=0; 1985-1986 1:1; 1987-2015
#0.68 *FLAG* this is the conversion factor from dressed to round weight. It
#looks like Kray took the `Whole.Pounds..sum.` column from OceanAK and treated
#it like dressed weight when it was round weight, ex
data.frame(year = 1980:2015,
           ratio = catch3$catch_mt / (filter(catch, year<2016) %$% catch_mt))

#Relationship between POUNDS catch_mt and Kray's historical catch values from
#1980-1984: all ~0.63 (same as the relationship between POUNDS and ROUND_POUNDS
#1985-1996)
data.frame(year = 1980:1984,
           ratio = filter(catch2, year<1985) %$% catch_mt / (filter(catch, year<1985) %$% catch_mt))


# figures ----

# for most figs, the `data`` could be inter-changed for vessel_cpue or cpue depending on your interest.

# cpue over time
ggplot() +
  geom_point(data = cpue, aes(Year, std_cpue)) + 
  geom_line(data = overall_cpue,  aes(Year, overall_cpue, group=1)) +
  # geom_jitter(width=.15, height=0) +
  eda_theme

# catch by julian day
ggplot(cpue, aes(julian_day, sable_wt_set)) +
  geom_jitter() +
  stat_smooth(method='lm', se=FALSE) + 
  # facet_wrap(~Year) + 
  eda_facet

# cpue by julian day, stat area
# png(file='figures/fsh_cpue_day.png', res=300, width=7, height=3.5, units ="in", bg="transparent")  
ggplot(vessel_cpue, aes(julian_day, std_cpue, col=Stat, group=1)) + 
  geom_jitter() +
  stat_smooth(aes(julian_day, std_cpue), alpha=.2, method='lm') +
  xlab("\nJulian day (from start of year)") +
  ylab("CPUE") +  
  eda_theme
# dev.off()

# cpue by depth, stat area
# png(file='figures/fsh_cpue_depth.png', res=300, width=7, height=3.5, units ="in", bg="transparent")  
ggplot(cpue, aes(depth, std_cpue, col=Stat, group=1)) +
  geom_jitter() +
  stat_smooth(alpha=.2, method='gam', formula= y ~ s(x, k=4)) +
  xlab("\nDepth (meters)") + # *FLAG* - check units
  ylab("CPUE") +
  eda_theme
# dev.off()

# catch by depth and hook size
ggplot(vessel_cpue, aes(depth, sable_wt_set, col=Size, group=1)) +
  geom_jitter()+
  stat_smooth(alpha=.2, method='gam', formula= y ~ s(x, k=4)) +
  xlab("\nDepth (meters)") + 
  ylab("Catch (lbs)") + 
  eda_theme 

# Hook size and cpue - looks like most of the hooks used are 13s and 14s and
# they don't have a measurable difference in cpue
ggplot(cpue, aes(Size,std_cpue )) +
  geom_boxplot() +
  # facet_wrap(~Stat) +
  xlab("\nHook Size") +
  ylab("CPUE") +
  eda_theme
table(cpue$Size)

# Compare vessel cpue with longterm average cpue *FLAG* - this doesn't 
# png(file='figures/vessel.png', res=300, width=7, height=3.5, units ="in", bg="transparent")  
ggplot() + 
  geom_boxplot(data=cpue, aes(Year, std_cpue), fill="lightgrey") + 
  geom_line(data=overall_cpue, aes(Year, overall_cpue, col="Overall CPUE", group=1), lwd=1) +
  stat_summary(data=vessel_cpue, aes(Year, std_cpue, col="Vessel CPUE"), 
               fun.y='mean', geom="line", group=1, lwd=1.5, lty=2) +
  xlab("\nYear") +
  ylab("CPUE") +
  eda_theme 
# dev.off()

# CPUE by stat area
# png(file='figures/fsh_cpue_stat.png', res=300, width=7, height=3.5, units ="in", bg="transparent")  
ggplot(cpue, aes(x=(reorder(Stat, std_cpue)), y=std_cpue)) +
  geom_boxplot() +
  xlab("Stat area")+
  ylab("CPUE") +
  eda_theme
# dev.off()
table(cpue$Stat)

#Plot trends in cpue and catch in core stat areas
core_areas <- c("335701", "345603", "345631", "345701",
                "345702", "345731", "345803")

cpue_year_area <- cpue %>% 
  filter(Stat %in% core_areas) %>% 
  group_by(Stat, Year) %>% 
  summarise(catch = sum(sable_wt_set),
            cpue = mean(std_cpue))

ggplot(cpue_year_area, aes(Year, catch, group=1)) +
  geom_point() +
  geom_line() + 
  facet_wrap(~Stat) +
  eda_facet

ggplot(cpue_year_area, aes(Year, cpue, group=1)) +
  geom_point() +
  geom_line() + 
  facet_wrap(~Stat) +
  eda_facet

#Number of sets by
ggplot(cpue %>% filter(Stat %in% core_areas), aes(Year)) +
  geom_bar() + 
  facet_wrap(~Stat) +
  eda_facet

cpue %>% group_by(Year) %>% 
  summarise(vessels = n_distinct(Adfg))

cpue %>% filter(Adfg=="23214", Year=="2013")
cpue %>% distinct(Adfg, Year, julian_day)
# CPUE and catch by vessel
plot_cpue <- ggplot(cpue, aes(x=(reorder(Adfg, std_cpue)), y=std_cpue)) +
  geom_boxplot() +
  xlab("Individual vessels in the Chatham Strait longline fishery")+
  ylab("CPUE") +
  eda_theme

plot_catch <- ggplot(cpue, aes(x=(reorder(Adfg, sable_wt_set)), y=sable_wt_set)) +
  geom_boxplot() +
  xlab("Individual vessels in the Chatham Strait longline fishery")+
  ylab("Catch (lbs)") + #*FLAG* check units
  eda_theme

grid.arrange(plot_cpue, plot_catch)

# catch and number of hooks in a set
ggplot(cpue, aes(no_hooks, sable_wt_set, group=1)) +
  geom_point() +
  stat_smooth(method='gam', formula=y~s(x, k=4)) +
  xlab("\nTotal Number of Hooks") +
  ylab("Catch (lbs)") + #*FLAG check units
  eda_theme

# cpue and number of hooks in a set
ggplot(cpue, aes(no_hooks, std_cpue, group=1)) +
  geom_point() +
  stat_smooth(method='gam', formula=y~s(x, k=4)) +
  xlab("\nTotal Number of Hooks") +
  ylab("CPUE") + #*FLAG check units
  eda_theme

# catch and number of sets
ggplot(cpue, aes(sets, sable_wt_set, group=1)) +
  geom_point() +
  stat_smooth(method='gam', formula=y~s(x, k=4)) +
  xlab("\nTotal Number of Sets") +
  ylab("Catch (lbs)") + #*FLAG check units
  eda_theme

# catch and number of sets
ggplot(cpue, aes(sets, std_cpue, group=1)) +
  geom_point() +
  stat_smooth(method='gam', formula=y~s(x, k=4)) +
  xlab("\nTotal Number of Sets") +
  ylab("CPUE") + #*FLAG check units
  eda_theme

# CPUE pdf 
ggplot(cpue, aes(std_cpue)) +
  geom_density(fill=4, alpha=.5)

# CPUE pdfs by stat areas 
ggplot(cpue, aes(std_cpue, fill=Stat)) +
  geom_density(alpha=.3)

# GAM cpue ----

# fake data
n <- 50
sig <- 2
dat <- gamSim(1,n=n,scale=sig)

# P-spline smoothers (with lambda=0.6) used for x1 and x2; x3 is parametric.
b1 <- mgcv::gam(y ~ s(x1, bs='ps', sp=0.6) + s(x2, bs='ps', sp=0.6) + x3, data = dat)
summary(b1)
plot(b1, scale=0, pages=1)


b2 <- mgcv::gam(y ~ s(x1) + s(x2) + x3, data = dat)
summary(b2,pages=1)
plot(b2, scale=0, pages=1)

# plot the smooth predictor function for x1 with ggplot to get a nicer looking graph
p <- predict(b1, type="lpmatrix")
beta <- coef(b1)[grepl("x1", names(coef(b1)))]
s <- p[,grepl("x1", colnames(p))] %*% beta
ggplot(data=cbind.data.frame(s, dat$x1), aes(x=dat$x1, y=s)) + geom_line()


# predict
newdf <- gamSim(1,n=n,scale=sig)
f <- predict(b1, newdata=newdf)

cpue <- cpue %>% mutate(year = as.numeric(as.character(Year)))

fit <- gam(std_cpue ~ s(depth, k=4) + s(julian_day, k=4) + s(year), 
           
           data=cpue) #, gamma=1.4
plot(fit, shade=TRUE, residuals=TRUE, page=1)


newd <- expand.grid(julian_day = min(cpue$julian_day), min(cpue$julian_day), 
                    depth = seq(min(cpue$depth), max(cpue$depth), by=10),
                    Year = levels(cpue$Year), dum=0)

# newdB <- expand.grid(day = min(dat$day), max(dat$day), depth = seq(min(dat$depth),max(dat$depth), by=10),sets=1:17,
#                      ADFG = "55900", Stat = "345701", Size=levels(dat$Size), 
#                      Year = levels(dat$Year), dum=0)
# 
# newdB <- expand.grid(day = min(median(dat$day)-25), max(median(dat$day)+25),depth = min(median(dat$depth)-147), max(median(dat$depth)+147),
#                      sets=median(dat$sets),
#                      ADFG = "55900", Stat = "345701", Size=levels(dat$Size), 
#                      Year = levels(dat$Year), dum=0)

summary(fit)




cpue$fit <- predict(fit, cpue, type="response")
newd$fit <- predict(fit, newd)


#png(file='figures/fsh_cpue_comparison.png', res=300, width=7, height=3.5, units ="in", bg="transparent") 


ggplot(cpue, aes(Year, fit, group = 1)) +
  stat_summary(fun.data='mean_cl_boot',geom="smooth",aes(Year, fit, colour="GAM"), fill="grey70")+
  stat_summary(data=dat, fun.data='mean_cl_boot',geom="smooth",aes(Year,cpue,colour="Simple ratio"),fill="blue",alpha=0.1)+
  stat_summary(fun.y='mean',geom="point",aes(Year,fit,colour="GAM"))+
  stat_summary(data=dat, fun.y='mean',geom="point",aes(Year,cpue,colour="Simple ratio"))+
  xlab("Year")+
  ylab("CPUE")+
  scale_colour_manual(name="CPUE",values=c("Commercial fisheries CPUE - GAM" = "black", "Commercial fisheries CPUE - simple ratio" = "blue"))+
  theme_eda


dev.off()

#stat_sub <- dat[is.element(dat$Stat,c("335701","345603","345631","345701","345702","345731","345803")),]
stat_sub <- dat[is.element(dat$Stat,c("345701","345631","345603","345731")),]

ggplot(stat_sub, aes(Stat,cpue, fill=factor(Stat)))+geom_boxplot()

vis.gam(fit,c("depth","sets"), plot.type="contour",type="response",color="topo", too.far = 0.1)
vis.gam(fit,c("depth","day"), plot.type="contour",type="response",color="topo", too.far = 0.1)
vis.gam(fit,c("sets","day"), plot.type="contour",type="response",color="topo", too.far = 0.1)


# Check normality of fit
histogram(~fit|Year, data=dat, breaks=15,strip = strip.custom(bg="grey"))
histogram(~fit|Year, data=newd, breaks=15,strip = strip.custom(bg="grey"))

a1<-ggplot(dat, aes(Year,fit, fill=Stat))+geom_boxplot()
b1<-ggplot(newd, aes(Year,fit, fill=Stat))+geom_boxplot()
grid.arrange(a1,b1,ncol=2)

#-----------------------------------------------------
# Tukey's test for differences
#-----------------------------------------------------
tukey_fit<-(aov(cpue ~ Stat, data=stat_sub))
summary(tukey_fit)
plot(TukeyHSD(tukey_fit))

tukey.edit <- function (x, ...) 
  # changed main = NULL to main = ""
{
  for (i in seq_along(x)) {
    xi <- x[[i]][, -4, drop = FALSE]
    yvals <- nrow(xi):1
    dev.hold()
    on.exit(dev.flush())
    plot(c(xi[, "lwr"], xi[, "upr"]), rep.int(yvals, 2),
         type = "n", axes = FALSE, xlab = "", ylab = "", 
         main = "", 
         ...)
    axis(1, at=c(-1.0,-0.5,0,0.5,1),labels=c("-1.0","-0.5","0","0.5","1.0"),cex.axis=1,...)
    axis(2, at = nrow(xi):1, labels = dimnames(xi)[[1L]], 
         srt = 0, las=2,cex.axis=1)
    abline(h = yvals, lty = 1, lwd = 0.5, col = "lightgrey")
    abline(v = 0, lty = 2, lwd = 0.5,...)
    segments(xi[, "lwr"], yvals, xi[, "upr"], yvals, lwd=2,...)
    segments(as.vector(xi), rep.int(yvals - 0.3, 3), as.vector(xi), 
             rep.int(yvals + 0.3, 3), lwd=3,...)
    title(xlab = paste(""))# removed main from here
    box()
    for(j in 1:nrow(xi)){
      if(xi[j,][["upr"]] < 0)
        segments(xi[j,][["lwr"]], yvals[j], xi[j,][["upr"]], yvals[j], lwd=3, col="red",...)}
    for(j in 1:nrow(xi)){
      if(xi[j,][["lwr"]] > 0)
        segments(xi[j,][["lwr"]], yvals[j], xi[j,][["upr"]], yvals[j], lwd=3, col="red",...)}
  }
}

par(oma=c(3,13,0,1))
tukey.edit(TukeyHSD(tukey_fit))

par(oma=c(3,13,0,1))
plot(tukey.edit(TukeyHSD(tukey_fit)))

#Mean cpue and and variance for each year 1997 - present
dat %>% 
  group_by(Year) %>% 
  summarize(mfit=mean(fit))-> cpue_gam
cpue<-as.numeric(round(cpue_gam$mfit,4))   

newd %>% 
  group_by(Year) %>% 
  summarize(var=var(fit))-> cpue_var
var<-as.numeric(round(cpue_var$var,4)) 

#Check
x<-with(dat, tapply(dat$cpue,dat$year,mean, na.rm=TRUE))
round(x,4)
cpue

#Check
y<-with(dat, tapply(dat$cpue,dat$year, var, na.rm=TRUE))
round(y,4)
var


#------------------------------------------------------------------------------
# Secondary estimate for standard graphic for depth < 450
#------------------------------------------------------------------------------
cpue.fsh %>% 
  mutate(Year = factor(year), ADFG = factor(adfg), date = dmy(sell_date) ,
         day = yday(date),catch = sqrt(sable_wt_set), gear1 = factor(gear), Stat = factor(stat_area)) -> dat450

dat450$size<-as.numeric(gsub("[^0-9]","",dat450$Hook_size))

dat450 %>%
  mutate(Size = factor(size)) -> dat450

dat450<- subset(dat450, day > 226 & no_hooks < 15000 & depth > 450)
dat450$standard=dat450$no_hooks*2.2*(1-exp(-0.57*(dat450$hook_space*2.54/100)))
dat450$cpue<-dat450$sable_wt_set/dat450$standard
dat450$dum <- 1


fit450 <- gam(cpue ~ s(day, k=4) + s(depth, k=4) + s(sets, k=3) + 
                s(ADFG, bs='re', by=dum) +s(Stat, bs='re',by=dum) + Year + Size, data=dat450, gamma=1.4)

newd450 <- expand.grid(day = min(dat450$day), max(dat450$day), depth = seq(min(dat450$depth),max(dat450$depth), by=10),sets=1:29,
                       ADFG = "55900", Stat = "345701", Size=levels(dat450$Size), 
                       Year = levels(dat450$Year), dum=0)


dat450$fit <- predict(fit450,dat450, type="response")
newd450$fit <- predict(fit450, newd450)


ggplot(newd450, aes(Year,fit, group=1))+
  stat_summary(fun.data='mean_cl_boot',geom="smooth",aes(Year,fit),colour=2)+
  stat_summary(data=dat, fun.data='mean_cl_boot',geom="smooth",aes(Year,fit),colour="green")


#Mean cpue and and variance for each year 1997 - present
newd450 %>% 
  group_by(Year) %>% 
  summarize(mfit=mean(fit))-> cpue450
cpue450<-as.numeric(round(cpue450$mfit,4))   

newd450 %>% 
  group_by(Year) %>% 
  summarize(var=var(fit))-> cpue_var
var450<-as.numeric(round(cpue_var$var,4)) 

#------------------------------------------------------------------------------
# Write to file
#
# Note that we are using the GAM-derived estimates of variance, which are
#  smaller than the direct y calculation
# This is up for discussion
#
# We are back-calculating the variance for the legacy CPUE values 1980 - 1996
#  by taking the mean CV of 1997 - present CPUE and applying that to the legacy
#  CPUE values
#------------------------------------------------------------------------------

#Calculate mean CV for 1997 - present poundage CPUE
mean_cv<-mean(sqrt(var)/cpue)
mean_cv

# Legacy fishery cpue values and back-calculation of variance
hist<-as.numeric(read.csv("data/legacy_fishery_cpue.csv",header=FALSE,sep=","))
legacy_fishery_cpue_var <- (mean_cv * hist)^2

# Merge vectors
cpue_full<-c(hist,cpue)
cpue_full_450<-c(hist,cpue450)
var_full<-c(legacy_fishery_cpue_var,var)

#Write to file
write.table(round(rbind(cpue_full),4),"output/fish_cpue_gam_pounds.csv",row.names=FALSE,col.names = FALSE,sep=",")
write.table(round(rbind(cpue_full_450),4),"output/fish_cpue_gam_pounds_450.csv",row.names=FALSE,col.names = FALSE,sep=",")
write.table(round(rbind(var_full),4),"output/fish_cpue_gam_var.csv",row.names=FALSE,col.names = FALSE,sep=",")
write.table(round(rbind(x),4),"output/non_gam_cpue.csv",row.names=FALSE,col.names = FALSE,sep=",")

