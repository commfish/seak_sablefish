mod0 <- bam(set_cpue ~ s(soak, k = 4) + s(depth, k = 4) + s(slope, k = 4) + 
              s(bait, k = 4) + 
              te(end_lon, end_lat) + Year + s(Adfg, bs='re') + 1, 
            data = sable, gamma=1.4, family=Gamma(link=log), select = TRUE,
            subset = soak < 10 & slope < 300 & bait < 900)
#s = smooth; te = ??
summary(mod0)
anova(mod0)
print(plot(getViz(mod0), allTerms = TRUE) + l_fitRaster() + l_fitContour() + 
        l_points(color = "grey60", size = 0.5)+ l_fitLine() + l_ciLine() +
        l_ciBar(linetype = 2) + l_fitPoints(size = 1), pages = 8)

par(mfrow=c(2, 2), cex=1.1); gam.check(mod0)

str(mod0)

nrow(srv_cpue)
nrow(sable)

sable.f<-sable[sable$soak <10 & sable$slope < 300 & sable$bait < 900,]
nrow(sable.f); str(sable.f)

sable.f<-sable.f[complete.cases(sable.f),]
nrow(sable.f)

mod0f<-bam(set_cpue ~ s(soak, k = 4) + s(depth, k = 4) + s(slope, k = 4) + 
             s(bait, k = 4) + 
             te(end_lon, end_lat) + Year + s(Adfg, bs='re') + 1, 
           data = sable.f, gamma=1.4, family=Gamma(link=log), select = TRUE)
str(mod0f)
getViz(mod0)

preds<-predict.bam(mod0, type="response"); str(preds); head(preds)
preds.f<-predict.bam(mod0f, type="response", se.fit=T); str(preds.f); head(preds.f, 20)
nrow(preds.f$fit)

preds.f2<-predict.bam(mod0f, type="terms", se.fit=T, terms=c("set_cpue"))
str(preds.f2); head(preds.f2)

length(predicted)
length(sable.f$set_cpue)

plot(preds.f$fit); points(sable.f$set_cpue, col="red")

plot(preds.f$fit~sable.f$set_cpue)

sable.f$pred.cset.cpue<-preds.f$fit  #do this to get various model predictions for
                                     # comparisons... 
#======== 

preds2<-predict.bam(mod0, type="lpmatrix"); str(preds2); head(preds2)

nrow(preds)
str(preds)
summary(preds)
?predict.bam
