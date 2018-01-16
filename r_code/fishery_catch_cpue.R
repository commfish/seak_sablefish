
# Fishery catch 1980-present, fishery CPUE 1997-present
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2018-01-12


# Also used for requested vessel-specific cpue

# Kray's old notes: These data are from a spreadsheet sent out by Scott Johnson
# each year. The sheet is the output from a SQL script written by Martina to
# condition each set to a target species, as opposed to the overall trip target.
# Logbooks were not included in IFDB until 1997. Commercial fishery CPUE values
# prior to 1997, for use in the ASA or other medium, are LEGACY VALUES found in
# ..data/legacy_fishery_cpue.csv

# Kray's original cpue_fish_vessel.r script used fsh_cpue.csv and 
# non_gam_cpue.csv. Ben Williams and I decided 2017-09-29 to use cpue_fsh.csv
# (now fishery_cpue_1997_2015.csv) because it goes back to 1997. There was no
# documentation for the legacy cpue values

# most recent year of data
YEAR <- 2017

# Load ----

source("r_code/helper.R")

read_csv(paste0("data/fishery/fishery_cpue_1997_", YEAR,".csv"), 
                 guess_max = 50000) %>% 
  mutate(Year = factor(year), 
         Adfg = factor(Adfg),
         Spp_cde = factor(Spp_cde), 
         Gear = factor(Gear),
         Hook_size = factor(Hook_size), 
         Size = factor(Size),
         Stat = factor(Stat),
         std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (0.0254 * hook_space))), #standardize hook spacing (Sigler & Lunsford 2001, CJFAS)
         std_cpue = sable_lbs_set/std_hooks #standardized cpue (lbs of sablefish/hook)
  ) %>% 
  #"sets" (aka effort_no) is the set identifier. Currently Martina's scripts
  #filter out sets that Kamala identifies as halibut target sets. Create a new
  #column that is the total number of sablefish target sets in a trip
  group_by(trip_no) %>% 
  mutate(no_sets = n_distinct(sets)) %>% 
  group_by(year) %>% 
  mutate(
    #The number of vessels participating in the fishery has descreased by 50% from
    #1997-2015. create new column is the total number of active vessels
    #participating in a given year
    total_vessels = n_distinct(Adfg),
    # Total unique trips per year
    total_trips = n_distinct(trip_no),
    #mean annual cpue
    annual_cpue = mean(std_cpue)) -> cpue

View(cpue)

# There are multiple sources of catch data. The IFDB (Region I database) was
# used in the past, but J. Shriver recommends using the Gross Earnings File
# (GEF) for catch histories. The problem is the GEF isn't updated until data has
# been finalized, so 2017 isn't ready yet. The IFDB data goes back to 1969,
# while the GEF data only goes back to 1975.


catch_gef <- read_csv(paste0("data/fishery/nseiharvest_gef_1975_", YEAR-1,".csv"), 
                      guess_max = 50000)

names(catch_gef)

catch_ifdb <- read_csv(paste0("data/fishery/nseiharvest_ifdb_1969_", YEAR,".csv"), 
                      guess_max = 50000)
names(catch_ifdb)
# clean up data structures, get cpue
cpue %>% 
  mutate(Year = factor(year), 
         Adfg = factor(Adfg),
         Spp_cde = factor(Spp_cde), 
         Gear = factor(Gear),
         Hook_size = factor(Hook_size), 
         Size = factor(Size),
         Stat = factor(Stat),
         std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (0.0254 * hook_space))), #standardize hook spacing (Sigler & Lunsford 2001, CJFAS)
         std_cpue = sable_wt_set/std_hooks #standardized cpue
  ) %>% 
  #'sets' is a sequence of integers within a unique date/Adfg, the maximum of 
  #which in the number of total sets in a trip. create new column that is the
  #number of sets in a trip
  group_by(date, Adfg) %>% 
  mutate(no_sets = max(sets)) %>% 
  group_by(year) %>% 
  mutate(
    #The number of vessels participating in the fishery has descreased by 50% from
    #1997-2015. create new column is the total number of active vessels
    #participating in a given year
    total_vessels = n_distinct(Adfg),
    #mean annual cpue
    annual_cpue = mean(std_cpue),
    annual_catch = sum(sable_wt_set) # lbs
    ) -> cpue


#Vessel of interest if looking at cpue by vessel for a private request
VESSEL_REQUESTED <- "21465" # adfg number

#individual vessel cpue
vessel_cpue <- cpue %>% filter(Adfg == VESSEL_REQUESTED)


# figures ----

# for most figs, the `data` could be inter-changed for vessel_cpue or cpue depending on your interest.

# cpue over time
ggplot() +
  geom_point(data = cpue, aes(Year, annual_cpue)) +
  geom_line(data = cpue,  aes(Year, annual_cpue, group=1)) +
  ylab('Fishery CPUE\n') +
  xlab('') +
  theme(axis.text.x = element_text(size=10, angle=45, hjust=1))

# catch by julian day
ggplot(cpue, aes(julian_day, sable_wt_set)) +
  geom_jitter() +
  stat_smooth(method='lm', se=FALSE) + 
  ylab('Catch (lbs)\n') +
  xlab('\nJulian day') 

# cpue by julian day
ggplot(cpue, aes(julian_day, std_cpue)) +
  geom_jitter() +
  stat_smooth(method='lm', se=FALSE) + 
  ylab('Fishery CPUE\n') +
  xlab('\nJulian day') 

# cpue by stat area
ggplot(cpue, aes(Stat, std_cpue)) +
  geom_boxplot() +
  ylab('Fishery CPUE\n') +
  xlab('\nStat area') +
  theme(axis.text.x = element_text(size=10, angle=45, hjust=1))
table(cpue$Stat)

#Plot trends in cpue and catch in core stat areas
core_areas <- c("335701", "345603", "345631", "345701",
                "345702", "345731", "345803")
#areas with strong trends in catch over time
core_areas <- c("345603", "345631", "345701",
                "345731", "345803")

cpue_year_area <- cpue %>% 
  filter(Stat %in% core_areas) %>% 
  group_by(Stat, Year) %>% 
  summarise(catch = sum(sable_lbs_set),
            cpue = mean(std_cpue))

p_catch <- ggplot(cpue_year_area, aes(Year, catch, group=1)) +
  geom_point() +
  geom_line() + 
  facet_wrap(~Stat) +
  theme(axis.text.x = element_text(size=10, angle=45, hjust=1))

p_cpue <- ggplot(cpue_year_area, aes(Year, cpue, group=1)) +
  geom_point() +
  geom_line() + 
  facet_wrap(~Stat) +
  theme(axis.text.x = element_text(size=10, angle=45, hjust=1))

grid.arrange(p_catch, p_cpue)

ggplot(cpue_year_area, aes(cpue, catch, group=Stat, col=Stat)) +
  geom_point() +
  geom_smooth(method='gam',  alpha=0.1) +#formula= y ~ s(x, k=12), 
  xlab("CPUE") +
  ylab("Catch (lbs)")

# cpue by julian day, stat area
# png(file='figures/fsh_cpue_day.png', res=300, width=7, height=3.5, units ="in", bg="transparent")  
ggplot(vessel_cpue, aes(julian_day, std_cpue, col=Stat, group=1)) + 
  geom_jitter() +
  stat_smooth(aes(julian_day, std_cpue), alpha=.2, method='lm') +
  xlab("\nJulian day (from start of year)") +
  ylab("CPUE") 
# dev.off()

# cpue by depth
ggplot(cpue, aes(depth, std_cpue)) +
  geom_jitter() +
  stat_smooth(method='gam', formula= y ~ s(x, k=4)) + 
  xlab("\nDepth (meters)") + # *FLAG* - check units
  ylab("CPUE") 

# cpue by depth, stat area
# png(file='figures/fsh_cpue_depth.png', res=300, width=7, height=3.5, units ="in", bg="transparent")  
ggplot(cpue, aes(depth, std_cpue, col=Stat, group=1)) +
  geom_jitter() +
  stat_smooth(alpha=.2, method='gam', formula= y ~ s(x, k=4)) +
  xlab("\nDepth (meters)") + # *FLAG* - check units
  ylab("CPUE") 
# dev.off()

# cpue by number of sets
ggplot(cpue, aes(no_sets, std_cpue, group=1)) +
  geom_jitter() +
  stat_smooth(alpha=.2, method='gam', formula= y ~ s(x, k=4)) +
  xlab("\nNumber of sets") + # *FLAG* - check units
  ylab("CPUE")

# catch by depth and hook size
ggplot(vessel_cpue, aes(depth, sable_wt_set, col=Size, group=1)) +
  geom_jitter()+
  stat_smooth(alpha=.2, method='gam', formula= y ~ s(x, k=4)) +
  xlab("\nDepth (meters)") + 
  ylab("Catch (lbs)")

# Hook size and cpue - looks like most of the hooks used are 13s and 14s and
# they don't have a measurable difference in cpue
ggplot(cpue, aes(Size, std_cpue)) +
  geom_boxplot() +
  # facet_wrap(~Stat) +
  xlab("\nHook Size") +
  ylab("CPUE") 
table(cpue$Size)

# Compare vessel cpue with longterm average cpue *FLAG* - this doesn't 
# png(file='figures/vessel.png', res=300, width=7, height=3.5, units ="in", bg="transparent")  
ggplot() + 
  geom_boxplot(data=cpue, aes(Year, std_cpue), fill="lightgrey") + 
  geom_line(data=overall_cpue, aes(Year, overall_cpue, col="Overall CPUE", group=1), lwd=1) +
  stat_summary(data=vessel_cpue, aes(Year, std_cpue, col="Vessel CPUE"), 
               fun.y='mean', geom="line", group=1, lwd=1.5, lty=2) +
  xlab("\nYear") +
  ylab("CPUE") 
# dev.off()

#Number of sets by
ggplot(cpue %>% filter(Stat %in% core_areas), aes(Year)) +
  geom_bar() + 
  facet_wrap(~Stat) +
  theme(axis.text.x = element_text(size=10, angle=45, hjust=1))

# CPUE and catch by vessel
plot_cpue <- ggplot(cpue, aes(x=(reorder(Adfg, std_cpue)), y=std_cpue)) +
  geom_boxplot() +
  xlab("Individual vessels in the Chatham Strait longline fishery")+
  ylab("CPUE") +
  theme(axis.text.x = element_text(size=10, angle=45, hjust=1))

plot_catch <- ggplot(cpue, aes(x=(reorder(Adfg, sable_lbs_set)), y=sable_lbs_set)) +
  geom_boxplot() +
  xlab("Individual vessels in the Chatham Strait longline fishery")+
  ylab("Catch (lbs)") +#*FLAG* check units
  theme(axis.text.x = element_text(size=10, angle=45, hjust=1))  

grid.arrange(plot_cpue, plot_catch)

# catch and number of hooks in a set
ggplot(cpue, aes(no_hooks, sable_lbs_set, group=1)) +
  geom_point() +
  stat_smooth(method='gam', formula=y~s(x, k=4)) +
  xlab("\nTotal Number of Hooks") +
  ylab("Catch (lbs)")  #*FLAG check units
  

# cpue and number of hooks in a set
ggplot(cpue, aes(no_hooks, std_cpue, group=1)) +
  geom_point() +
  stat_smooth(method='gam', formula=y~s(x, k=4)) +
  xlab("\nTotal Number of Hooks") +
  ylab("CPUE")


# catch and number of sets
ggplot(cpue, aes(sets, sable_lbs_set, group=1)) +
  geom_point() +
  stat_smooth(method='gam', formula=y~s(x, k=4)) +
  xlab("\nTotal Number of Sets") +
  ylab("Catch (lbs)") #*FLAG check units

# catch and number of sets
ggplot(cpue, aes(sets, std_cpue, group=1)) +
  geom_point() +
  stat_smooth(method='gam', formula=y~s(x, k=4)) +
  xlab("\nTotal Number of Sets") +
  ylab("CPUE") 

# CPUE pdf 
ggplot(cpue, aes(std_cpue)) +
  geom_density(fill=4, alpha=.5)

# CPUE pdfs by stat areas 
ggplot(cpue, aes(std_cpue, fill=Stat)) +
  geom_density(alpha=.3)


# GAM cpue ----

fit <- gam(std_cpue ~ s(depth, k=4) + s(julian_day, k=4) +  s(total_vessels, k=4) +
             s(no_sets, k=4) + s(Year, bs="re")  + s(Stat, bs="re"), 
           data = cpue) #, gamma=1.4
           
plot(fit, shade=TRUE, residuals=TRUE, page=1, all=TRUE)
summary(fit)

fit2 <- gam(std_cpue ~ s(depth, k=4) + s(julian_day, k=4) +  s(total_vessels, k=4) +
             s(no_sets, k=4) + s(year) + Stat, 
           data = cpue) #, gamma=1.4

plot(fit2, shade=TRUE, residuals=TRUE, page=1, all=TRUE)
summary(fit2)


newd <- expand.grid(julian_day = min(cpue$julian_day), max(cpue$julian_day), 
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
