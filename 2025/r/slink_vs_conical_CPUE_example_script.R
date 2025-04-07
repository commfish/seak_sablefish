#We've got two groups of fish caught in two gears.  We fished 100 pots for each 
# group.
#in the first group we caught 500 fish that averaged 50 cm in length: 
set.seed(1457)
fish1<-round(rnorm(500,50,10),0); hist(fish1)
CPUE1<-500/100
#In the second group we caught 450 fish that averaged 54 cm in length: 
fish2<-round(rnorm(450,54,7),0); hist(fish2)
CPUE2<-450/100

#This is a simple example.  You should calculate the CPUE for each pot and then
#calculate the average CPUE across all pots/sets and get the variance and standard deviation
# which will allow you to see if they are significantly different.  You can repeat that at
# the set level as well.  

#In this simple example we know we caught more fish in group 1, but we want to know
# did we catch more fish of all length categories or did we just catch more small 
# fish and the same number of big fish?  

plot(density(fish1), xlim=c(10,90)); lines(density(fish2), col="blue")

#are the length distributions different?
ks.test(fish1,fish2)
#results show that the length distributions are significantly different! (P value < 0.05)

#lets plot the cumulative distribution functions of the data 
cdf1<-ecdf(fish1)
cdf2<-ecdf(fish2) 
plot(cdf1, verticals=TRUE, do.points=FALSE, main="cdf", xlab="length")
plot(cdf2, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")

#At this point it should be obvious that there are more little fish in group 1

#So, let's identify the length that we should stratify the data.  We do this by 
#finding the length where the two distributions are the furthest apart.  
#Here is a cute function to do that... 

D.func<-function(x,y){  #x<-C; y<-M
  n.x <- length(x)
  n.y <- length(y)
  n <- n.x * n.y/(n.x + n.y)
  w <- c(x, y)
  z <- cumsum(ifelse(order(w) <= n.x, 1/n.x, -1/n.y))
  max.at <- sort(w)[which(abs(z) == max(abs(z)))]
  return(max.at)
  
}

D.func(fish1,fish2)  #this tells you that the greatest distance between the distributions 
                     #is at the number you see (random numbers so might be different... 
                      # 46 last time I ran this)
abline(v=D.func(fish1,fish2), col="red")

#So, 50 is the length we should stratify around.  Divide both groups into fish
#greater and less than 51cm.

bp1<-D.func(fish1,fish2)

f1_small<-fish1[fish1 <=bp1]
f1_big<-fish1[fish1 >bp1]

f2_small<-fish2[fish2 <=bp1]
f2_big<-fish2[fish2 >bp1]

#Lets compare the length distribution of big fish... 
cdf1_big<-ecdf(f1_big)
cdf2_big<-ecdf(f2_big) 
plot(cdf1_big, verticals=TRUE, do.points=FALSE, main="cdf")
plot(cdf2_big, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")

ks.test(f1_big,f2_big)
# No difference in the length distribution of big fish. 
# Go ahead and compare the CPUE of big fish...
length(f1_big)/100 #<-CPUE of big fish in first gear
length(f2_big)/100 #<-CPUE of big fish in second gear
#For the full data set, you'll calculate by set and average across sets so you get
# variance and SD and can see if they are significantly different... 


## Lets look at the smaller fish first.  Are those distributions different?
cdf1_small<-ecdf(f1_small)
cdf2_small<-ecdf(f2_small) 
plot(cdf1_small, verticals=TRUE, do.points=FALSE, main="cdf")
plot(cdf2_small, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")

ks.test(f1_small,f2_small)
# these distributions are different. 
# At this point you could compare the CPUE of fish <= "break point" in the two groups.
# But lets find the next break point...

D.func(f1_small,f2_small)
bp2<-D.func(f1_small,f2_small)
abline(v=D.func(f1_small,f2_small), col="red")
# stratify at break point 2 (bp2).  In this example we are probably getting into sample size issues...
# But we can see that those smaller fish are disappearing from sample 2.  

f1_ltbp2<-f1_small[f1_small <=bp2]
f1_bp1_bp2<-f1_small[f1_small >bp2]

f2_ltbp2<-f2_small[f2_small <=bp2]
f2_bp1_bp2<-f2_small[f2_small >bp2]

#Compare the length distribution of fish bp-50cm 
cdf1_bp1_bp2<-ecdf(f1_bp1_bp2)
cdf2_bp1_bp2<-ecdf(f2_bp1_bp2) 
plot(cdf1_bp1_bp2, verticals=TRUE, do.points=FALSE, main="cdf")
plot(cdf2_bp1_bp2, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")

ks.test(fish1_bp_50,fish2_bp_50)
#same length distributions.  Compare the CPUE of fish in this length category.

#what about fish less than bp2... 
cdf1_ltbp<-ecdf(f1_ltbp2)
cdf2_ltbp<-ecdf(f2_ltbp2) 
plot(cdf1_ltbp, verticals=TRUE, do.points=FALSE, main="cdf")
plot(cdf2_ltbp, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")

ks.test(f1_ltbp2,f2_ltbp2)
abline(v=D.func(f1_ltbp2,f2_ltbp2), col="red")
#Still significant difference, so you could repeat or call it good at this point.
#You can see there are no fish in the second group less than a certain size (changes a little bit
# every time you run the simulations)
# make a break point there and compare the CPUE of fish between the minimum length 
# of group 2 and bp2 above and note that gear 2 did not catch any fish less than 
# the smallest fish 




##modify









