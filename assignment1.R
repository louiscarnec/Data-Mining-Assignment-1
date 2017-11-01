#1.
oj <- read.csv("/Users/Carnec/Desktop/Business_Analytics/Data Mining/Assignment1/oj.csv")
attach(oj)
#2.
dim(oj)
# 28947 records and 17 attributes

#3.
summary(oj$price)
mean(oj$price)
# mean = 2.2825
sd(oj$price)
# sd = 0.6480
min(oj$price)
# min = 0.52
max(oj$price)
# max = 3.87

#4.
median(oj$logmove)
# median = 9.03408

#5.
brands <- factor(oj$brand)
table(brands)

#6.his
pdf("dominicksprice.pdf")
hist(oj$price[brands=="dominicks"],main="Dominicks Prices",xlab="Price",breaks=15)
graphics.off()

pdf("minute.maid.hist.pdf")
hist(oj$price[brands=="minute.maid"],main="Minute Maid Prices",xlab="Price",breaks=15)
graphics.off()

pdf("tropicana.hist.pdf")
hist(oj$price[brands=="tropicana"],main="Tropicana Prices",xlab="Price",breaks=15)
graphics.off()

#7.
pdf("boxplot.pricebrands.pdf")
boxplot(oj$price~oj$brand, data=oj)
graphics.off()

#9.

pdf("colorscatter.pdf")
plot(oj$logmove,oj$price,pch = 20, col = oj$brand, main = "Price and Number of units sold",xlab="Log of the number of units sold",ylab="price",)
par(xpd=TRUE)
legend('topright', legend = levels(oj$brand), col = 1:3, cex = 0.8, pch = 1)
graphics.off()

#11.

pdf("colorscatter.pdf")
week.price <- tapply(oj$price,oj$week,FUN=mean,na.rm=TRUE)
meanweekprice <- as.data.frame(week.price)
print(weekprice)
pdf("timeseries.pdf")
plot.ts(meanweekprice,main="Mean price of orange juice sold each week",ylab="Mean Price",xlab = "Weeks")
graphics.off()

#12.
meanpriceperbrand <- t(as.data.frame(tapply(oj$price,INDEX=list(oj$brand,oj$week),FUN=mean,na.rm=TRUE)))

#13.
pdf("pricestimeseries.pdf")
plot.ts(meanpriceperbrand,plot.type=c("single"),ylab="Weekly Price",main="Weekly Prices",col=c("darkgoldenrod1","aquamarine2","deeppink4"))
lines(weekprice,col=c("coral2"))
legend(90,3.6,cex=0.6,legend=c("Mean","dominicks","tropicana","minute maid"),lty=c(1,1),col=c("coral2","darkgoldenrod1","deeppink4","aquamarine2"))
graphics.off()

#14.
advertisement <-factor(oj$feat)

tapply(oj$logmove,advertisement,FUN=mean)

aov.out = aov(oj$logmove~advertisement)
summary(aov.out)

#15.
pdf("advetisementlineplot.pdf")
logmove_feat <- tapply(oj$logmove,INDEX=list(oj$week,oj$feat),FUN=mean,na.rm=TRUE)
plot.ts(logmove_feat,plot.type="single",ylab="Mean weekly units sold", col=c("seagreen3","mediumorchid1"))
legend("topright",cex=0.6,legend=c("no adevertisement","advertisement"),lty=c(1,1),col=c("seagreen3","mediumorchid1"))
graphics.off()

pdf("advetisementlineplot.pdf")
logmove_feat <- tapply(oj$logmove,INDEX=list(oj$week,oj$feat),FUN=mean,na.rm=TRUE)
plot(logmove_feat,plot.type="single",ylab="Mean weekly units sold", col=c("seagreen3","mediumorchid1"))
legend("topright",cex=0.6,legend=c("no adevertisement","advertisement"),lty=c(1,1),col=c("seagreen3","mediumorchid1"))
graphics.off()

#16.
# Descriptive statistics
attributes(oj)

#Summary of variables
summary(oj)

###Interested in number of unit sold!
summary(logmove)
tapply(logmove,brand,summary)
tapply(logmove,brand,median)
tapply(logmove,brand,sd)

###AGE###
summary(AGE60)
oj$percAGE <- cut(AGE60,breaks=c(0,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50))
summary(oj$percAGE)

#mean units sold to fraction of customers in age bracket over 60
tapply(logmove,list(oj$percAGE,brand),mean)

#mean units sold to customers in age bracket over 60 per brand
tapply(logmove,list(oj$AGE60,brand),mean)
pdf("boxplotlogmoveover60.pdf")
boxplot(tapply(logmove,list(oj$AGE60,brand),mean),ylab="logmove",main="Boxplot of logmove to >60 per brand")
graphics.off()

#median units sold to fraction of customers in age bracket over 60
tapply(logmove,list(percAGE,brand),median)

#standard deviation of units sold to fraction of customers in age bracket over 60
tapply(logmove,list(AGE_BIN,brand),sd)

#Correlation between mean logmove per store and mean AGE60 per store
cor(tapply(logmove,store,FUN=mean,na.rm=TRUE),tapply(AGE60,store,FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean AGE60 per store for dominicks
cor(tapply(logmove[brand=="dominicks"],store[brand=="dominicks"],FUN=mean,na.rm=TRUE),tapply(AGE60[brand=="dominicks"],store[brand=="dominicks"],FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean AGE60 per store for minute.maid
cor(tapply(logmove[brand=="minute.maid"],store[brand=="minute.maid"],FUN=mean,na.rm=TRUE),tapply(AGE60[brand=="minute.maid"],store[brand=="minute.maid"],FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean AGE60 per store for tropicana
cor(tapply(logmove[brand=="tropicana"],store[brand=="tropicana"],FUN=mean,na.rm=TRUE),tapply(AGE60[brand=="tropicana"],store[brand=="tropicana"],FUN=mean,na.rm=TRUE))

###EDUC###
summary(EDUC)
oj$percEDUC <- cut(EDUC,breaks=c(0,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60))
summary(percEDUC)

#mean units sold to fraction of customers with a college degree
tapply(logmove, list(percEDUC,brand),mean)

#median units sold to fraction of customers with a college degree
tapply(logmove,list(percEDUC,brand),median)

#standard deviation of units sold to fraction of customers with a college degree
tapply(logmove,list(percEDUC,brand),sd)

#Correlation between mean logmove per store and mean EDUC per store
cor(tapply(logmove,store,FUN=mean,na.rm=TRUE),tapply(EDUC,store,FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean EDUC per store for dominicks
cor(tapply(logmove[brand=="dominicks"],store[brand=="dominicks"],FUN=mean,na.rm=TRUE),tapply(EDUC[brand=="dominicks"],store[brand=="dominicks"],FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean EDUC per store for Minute MAid
cor(tapply(logmove[brand=="minute.maid"],store[brand=="minute.maid"],FUN=mean,na.rm=TRUE),tapply(EDUC[brand=="minute.maid"],store[brand=="minute.maid"],FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean EDUC per store for tropicana
cor(tapply(logmove[brand=="tropicana"],store[brand=="tropicana"],FUN=mean,na.rm=TRUE),tapply(EDUC[brand=="tropicana"],store[brand=="tropicana"],FUN=mean,na.rm=TRUE))

###ETHNIC###

summary(ETHNIC)
oj$percETHNIC <- cut(ETHNIC,breaks=c(0,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,1.00))
summary(percETHNIC) #OUTLIER

pdf("ETHNIChist.pdf")
hist(tapply(ETHNIC,store,mean),xlab="percent of population that is black or hispanic")
graphics.off()


#mean units sold to ethnic fraction of customers 
tapply(logmove, list(percETHNIC,brand),mean)

#median units sold to ethnic fraction of customers
tapply(logmove, list(percETHNIC,brand),mean)

#standard deviation of units sold to ethnic fraction of customers
tapply(logmove,list(percETHNIC,brand),sd)

#Correlation between mean logmove per store and mean ETHNIC per store
cor(tapply(logmove,store,FUN=mean,na.rm=TRUE),tapply(ETHNIC,store,FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean ETHNIC per store for dominicks
cor(tapply(logmove[brand=="dominicks"],store[brand=="dominicks"],FUN=mean,na.rm=TRUE),tapply(ETHNIC[brand=="dominicks"],store[brand=="dominicks"],FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean ETHNIC per store for Minute MAid
cor(tapply(logmove[brand=="minute.maid"],store[brand=="minute.maid"],FUN=mean,na.rm=TRUE),tapply(ETHNIC[brand=="minute.maid"],store[brand=="minute.maid"],FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean ETHNIC per store for tropicana
cor(tapply(logmove[brand=="tropicana"],store[brand=="tropicana"],FUN=mean,na.rm=TRUE),tapply(ETHNIC[brand=="tropicana"],store[brand=="tropicana"],FUN=mean,na.rm=TRUE))

###INCOME###
summary(INCOME)
hist(INCOME)
hist(tapply(INCOME,store,mean))

#mean log units sold for different customer incomes
tapply(logmove, list(INCOME,brand),mean)

#median log units sold for different customer incomes
tapply(logmove, list(INCOME,brand),median)

#standard deviation of log units sold for different customer incomes
tapply(logmove, list(INCOME,brand),sd)

#Correlation between mean logmove per store and mean INCOME per store
cor(tapply(logmove,store,FUN=mean,na.rm=TRUE),tapply(INCOME,store,FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean INCOME per store for dominicks
cor(tapply(logmove[brand=="dominicks"],store[brand=="dominicks"],FUN=mean,na.rm=TRUE),tapply(INCOME[brand=="dominicks"],store[brand=="dominicks"],FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean INCOME per store for Minute MAid
cor(tapply(logmove[brand=="minute.maid"],store[brand=="minute.maid"],FUN=mean,na.rm=TRUE),tapply(INCOME[brand=="minute.maid"],store[brand=="minute.maid"],FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean INCOME per store for tropicana
cor(tapply(logmove[brand=="tropicana"],store[brand=="tropicana"],FUN=mean,na.rm=TRUE),tapply(INCOME[brand=="tropicana"],store[brand=="tropicana"],FUN=mean,na.rm=TRUE))

###HHLARGE###
summary(HHLARGE)
oj$percHHLARGE <- cut(HHLARGE,breaks=c(0.05,0.1,0.15,0.20,0.25))
summary(percHHLARGE)
hist(tapply(HHLARGE,store,mean))

#mean log units sold to percentage of households with more than 5 people
tapply(logmove,list(percHHLARGE,brand),mean)

#median log units sold to percentage of households with more than 5 people
tapply(logmove,list(percHHLARGE,brand),median)

#sd of log units sold to percentage of households with more than 5 people
tapply(logmove,list(percHHLARGE,brand),sd)

#Correlation between mean logmove per store and mean HHLARGE per store
cor(tapply(logmove,store,FUN=mean,na.rm=TRUE),tapply(HHLARGE,store,FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean HHLARGE per store for dominicks
cor(tapply(logmove[brand=="dominicks"],store[brand=="dominicks"],FUN=mean,na.rm=TRUE),tapply(HHLARGE[brand=="dominicks"],store[brand=="dominicks"],FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean HHLARGE per store for Minute MAid
cor(tapply(logmove[brand=="minute.maid"],store[brand=="minute.maid"],FUN=mean,na.rm=TRUE),tapply(HHLARGE[brand=="minute.maid"],store[brand=="minute.maid"],FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean HHLARGE per store for tropicana
cor(tapply(logmove[brand=="tropicana"],store[brand=="tropicana"],FUN=mean,na.rm=TRUE),tapply(HHLARGE[brand=="tropicana"],store[brand=="tropicana"],FUN=mean,na.rm=TRUE))

###WORKWOM###

summary(WORKWOM)
oj$percWORKWOM <- cut(WORKWOM,breaks=c(0,0.05,0.1,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50))
summary(percWORKWOM)
hist(tapply(WORKWOM,store,mean))

#mean log units sold to percentage of women in full-time jobs
tapply(logmove,list(percWORKWOM,brand),mean)

#median log units sold to percentage of women in full-time jobs
tapply(logmove,list(percWORKWOM,brand),median)

#sd of log units sold to percentage of women in full-time jobs
tapply(logmove,list(percWORKWOM,brand),sd)

#Correlation between mean logmove per store and mean WORKWOM per store
cor(tapply(logmove,store,FUN=mean,na.rm=TRUE),tapply(WORKWOM,store,FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean WORKWOM per store for dominicks
cor(tapply(logmove[brand=="dominicks"],store[brand=="dominicks"],FUN=mean,na.rm=TRUE),tapply(WORKWOM[brand=="dominicks"],store[brand=="dominicks"],FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean WORKWOM per store for Minute MAid
cor(tapply(logmove[brand=="minute.maid"],store[brand=="minute.maid"],FUN=mean,na.rm=TRUE),tapply(WORKWOM[brand=="minute.maid"],store[brand=="minute.maid"],FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean WORKWOM per store for tropicana
cor(tapply(logmove[brand=="tropicana"],store[brand=="tropicana"],FUN=mean,na.rm=TRUE),tapply(WORKWOM[brand=="tropicana"],store[brand=="tropicana"],FUN=mean,na.rm=TRUE))

###HVAL150###
summary(HVAL150)
oj$percHVAL150 <- cut(HVAL150,breaks=c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00))
summary(percHVAL150)
hist(tapply(HVAL150,store,mean))

#mean log units sold to percentage HH worth > 150,000
tapply(logmove,list(percHVAL150,brand),mean)

#median log units sold to percentage of HH worth > 150,000
tapply(logmove,list(percHVAL150,brand),median)

#sd of log units sold to percentage of HH worth > 150,000
tapply(logmove,list(percHVAL150,brand),sd)

#Correlation between mean logmove per store and mean HVAL150 per store
cor(tapply(logmove,store,FUN=mean,na.rm=TRUE),tapply(HVAL150,store,FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean HVAL150 per store for dominicks
cor(tapply(logmove[brand=="dominicks"],store[brand=="dominicks"],FUN=mean,na.rm=TRUE),tapply(HVAL150[brand=="dominicks"],store[brand=="dominicks"],FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean HVAL150 per store for Minute MAid
cor(tapply(logmove[brand=="minute.maid"],store[brand=="minute.maid"],FUN=mean,na.rm=TRUE),tapply(HVAL150[brand=="minute.maid"],store[brand=="minute.maid"],FUN=mean,na.rm=TRUE))

#Correlation between mean logmove per store and mean HVAL150 per store for tropicana
cor(tapply(logmove[brand=="tropicana"],store[brand=="tropicana"],FUN=mean,na.rm=TRUE),tapply(HVAL150[brand=="tropicana"],store[brand=="tropicana"],FUN=mean,na.rm=TRUE))

###STRDIST###

# Market Share:
tapply(logmove,INDEX=list(week,brand),FUN=mean)

sd(oj$logmove)
sd(movebrands)
sd(movebrands[,"dominicks"])
sd(movebrands[,"minute.maid"])
sd(movebrands[,"tropicana"])

pdf("lovemovebrandsts.pdf")
plot.ts(tapply(logmove,INDEX=list(week,brand),FUN=mean),plot.type=("single"),ylab="log of the number of units sold",col=c("red","blue","green"))
legend("topright",cex=0.6,legend=c("Dominicks","Minute Maid","Tropicana"),lty=c(1,1),col=c("red","blue","green"))
graphics.off()

install.packages("stargazer")
library(stargazer)

summary(oj)
ojdescrive <-describe(oj)
stargazer(ojdescrive,summary=FALSE)

hist(tapply(logmove,INDEX=list(AGE60,brand),FUN=mean))

lmout = lm(formula = logmove ~ EDUC + INCOME, data = oj)
summary(lmout)
stargazer(lmout)

#education: 
hist(oj$EDUC,breaks=15)

#education: 

logmoveHH <- as.data.frame(tapply(HHLARGE,INDEX=list(brands,logmove), FUN=mean))

#CPDIST5:
meandistperstore <- tapply(CPDIST5,store,FUN=mean)
hist(meandistperstore)




