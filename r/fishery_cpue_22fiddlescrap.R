str(fsh_cpue)
fy<-fsh_cpue[fsh_cpue$year == 1997,]
cp97<-smean.cl.boot(fy$std_cpue)

f20<-fsh_cpue[fsh_cpue$year == 2020,]
f21<-fsh_cpue[fsh_cpue$year == 2021,]
cp20<-smean.cl.boot(f20$std_cpue)
cp21<-smean.cl.boot(f21$std_cpue)

hist(f20$std_cpue)
hist(f21$std_cpue)
#some very 

view(f20)

f20n<-f20[f20$std_cpue > 1 & f20$std_cpue <1.2,]
f20o<-f20[f20$std_cpue > 6,]

quantile(f20o$std_cpue, c(0.001,0.01,0.05,0.5, 0.95, 0.99, 0.999))
quantile(f20n$std_cpue, c(0.001,0.01,0.05,0.5, 0.95, 0.99, 0.999))
par(mfrow=c(2,1))
hist(f20o$std_cpue, xlim=c(0,10), breaks=20)
hist(f20n$std_cpue, xlim=c(0,10), breaks=20)
hist(f20o$no_hooks, xlim=c(0,10000), breaks=20)
hist(f20n$no_hooks, xlim=c(0,10000), breaks=20)
unique(f20o$Hook_size)
unique(f20n$Hook_size)
hist(f20o$hook_space, xlim=c(0,200), breaks=20)
hist(f20n$hook_space, xlim=c(0,200), breaks=20)
unique(f20o$Stat)
unique(f20n$Stat)
hist(f20o$soak, xlim=c(0,20), breaks=20)
hist(f20n$soak, xlim=c(0,20), breaks=20)
hist(f20o$depth, xlim=c(0,1000), breaks=20)
hist(f20n$depth, xlim=c(0,1000), breaks=20)
hist(f20o$sets, xlim=c(0,10), breaks=20)
hist(f20n$sets, xlim=c(0,10), breaks=20)
hist(f20o$start_lat, xlim=c(55,58), breaks=20)
hist(f20n$start_lat, xlim=c(55,58), breaks=20)
hist(f20o$start_lon, xlim=c(-133,-135), breaks=20)
hist(f20n$start_lon, xlim=c(-133,-135), breaks=20)
hist(f20o$julian_day, xlim=c(230,350), breaks=20)
hist(f20n$julian_day, xlim=c(230,350), breaks=20)

f20n$no_hooks
f20o$no_hooks
f20o$Stat
f20n$Stat

unique(f20o$Adfg)
unique(f20n$Adfg)

killer<-f20[f20$Adfg == 60299,]
soso<-f20[f20$Adfg == 26017,]

quantile(killer$std_cpue, c(0.001,0.01,0.05,0.5, 0.95, 0.99, 0.999))
quantile(soso$std_cpue, c(0.001,0.01,0.05,0.5, 0.95, 0.99, 0.999))
par(mfrow=c(2,1))
hist(killer$std_cpue, xlim=c(0,10), breaks=20)
hist(soso$std_cpue, xlim=c(0,10), breaks=20)
hist(killer$no_hooks, xlim=c(0,10000), breaks=20)
hist(soso$no_hooks, xlim=c(0,10000), breaks=20)


hist(fy$no_hooks)
hist(f20$no_hooks)
hist(f21$no_hooks)

quantile(fy$no_hooks, c(0.001,0.01,0.05,0.5))
quantile(f20$no_hooks, c(0.001,0.01,0.05,0.5))
quantile(f21$no_hooks, c(0.001,0.01,0.05,0.5))

quantile(fy$std_cpue, c(0.001,0.01,0.05,0.5, 0.95, 0.99, 0.999))
quantile(f20$std_cpue, c(0.001,0.01,0.05,0.5, 0.95, 0.99, 0.999))
quantile(f21$std_cpue, c(0.001,0.01,0.05,0.5, 0.95, 0.99, 0.999))

