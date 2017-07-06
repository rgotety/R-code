# Linear Regression - other issues #

# Salary #
sd<-read.csv("Salary.csv",head=TRUE)

Salary=sd[,2]
Age=sd[,3]

Genderfactor=as.factor(sd[,4])

head(sd)

res1=lm(Salary~Age+Genderfactor)
summary(res1)

res2=lm(Salary~Genderfactor+Age:Genderfactor)
summary(res2)


AIC(res1)
AIC(res2)


# Amusement Park #
sat.df<-read.csv("http://goo.gl/HKnl74")
dim(sat.df)
str(sat.df)
names(sat.df)

library(gpairs)
gpairs(sat.df)

sat.df$logdist <- log(sat.df$distance)
sat.dflogdist <- log(sat.df$distance)

head(sat.df)
sat.df$logdist <- log(sat.df$distance)
head(sat.df)

hist(sat.df$logdist)

library(corrplot)

corrplot.mixed(cor(sat.df[,c(2,4:9)]),upper="ellipse")
attach(sat.df)

m1=lm(overall~rides)

sum_m1=summary(m1)
sum_m1
names(sum_m1)

plot(overall~rides,data=sat.df,xlab="Satisfaction with Rides",ylabel="Overall Satisfaction")
abline(m1)

dev.off()
plot(m1$fitted.values,m1$residuals)

sum_m1$r.squared

anova(m1)
sum_m1$coefficients

betas=m1$coefficients
betas

new_value=95

sum(betas[1]+betas[2]*new_value)

confint(m1)


#multiple regression#

re=read.csv("RealEstate_ExampleData.csv",head=TRUE)

head(re)
Y=re[,2]
sqft=as.numeric(re[,3])
age=as.numeric(re[,4])

m2_1 <- lm(Y~sqft+age)
m2_1
summary(m2_1)

m2 <- lm(overall~rides + games + wait + clean ,data=sat.df)
m2


library(coefplot)

coefplot(m2, intercept=FALSE, ylab="Features",xlab="Relationships with DV")


sat.std<-sat.df[,-3]
sat.std[,3:8]<-scale(sat.std[,3:8])
sat.std[,3:8]
head(sat.std)

m3<-lm(overall~rides+games+wait+clean+weekend+logdist+num.child,data=sat.std)

sat.std<-sat.df[,-3]
sat.std

m3<-lm(overall~rides+games+wait+clean+weekend+logdist+num.child,data=sat.std)
m3

summary(m3)

factor(sat.std$num.child)


sat.std$num.child.factor <- factor(sat.std$num.child)
sat.std$num.child.factor

#Discrete Independent Variable#

m4<-lm(overall~rides+games+wait+clean+weekend+logdist+num.child.factor,data=sat.std)
m4
m3

summary(m4)

AIC(m2);AIC(m3);AIC(m4)
AIC(m2)

BIC(m2);BIC(m3);BIC(m4)

sat.std$has.child<-factor(sat.std$num.child>0)

m5 <- lm(overall ~ rides+games+wait+clean+weekend+logdist+has.child,data=sat.std)
summary(m5)
m4
m3
m5
AIC(m4)

m6 <- lm(overall~rides+games+wait+clean+weekend+logdist+has.child+rides:has.child+games:has.child+wait:has.child+clean:has.child+rides:weekend+games:weekend+wait:weekend+clean:weekend,data=sat.std)
summary(m6)


m7 <- lm(overall ~ rides+games+wait+clean+logdist+has.child+wait:has.child,data=sat.std)
summary(m7)

AIC(m6);AIC(m7)




### Regression with Variable Selection ###

dis=read.csv("Discover_step.csv",head=TRUE)
head(dis)

Sat=dis[,2]
IVs=as.matrix(dis[,3:15])

cor(IVs)

library(Hmisc)
rcorr(IVs)

Res_cor=lm(Sat~IVs)

summary(Res_cor)

library(MASS)

IVs=scale(IVs)

x1=IVs[,1];x2=IVs[,2];x3=IVs[,3];x4=IVs[,4];x5=IVs[,5];x13=IVs[,13];x6=IVs[,6];x7=IVs[,7];x8=IVs[,8];x9=IVs[,9];x10=IVs[,10];x11=IVs[,11];x12=IVs[,12];

fit=lm(Sat~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13)

summary(fit)

step_both=stepAIC(fit, direction="both")
?stepAIC

summary(step_both)

step_back=stepAIC(fit,direction="backward")
summary(step_back)

step_forward=stepAIC(fit,direction="forward")
summary(step_forward)

library(Rcmdr)

stepwise(fit)



# LASSO
library(lars)
res_lasso=lars(IVs, Sat, type="lasso")
plot(res_lasso)

dim(res_lasso$beta)

res_lasso$beta[3,] #non-zero result show that particular variable is significant
