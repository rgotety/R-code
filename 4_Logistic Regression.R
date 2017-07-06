######################### Logistic / Logit

XBeta = -10
exp(XBeta)/(exp(XBeta)+1)

plogis(-10)
plogis(-Inf)

XB=seq(-10,10,by=0.01)
XB
logist=plogis(XB)

plot(logist,type="l",xlab="XB",ylab="Prob")


############## Catalog Example
Catalog=read.csv("Catalog.csv",head=TRUE)
attach(Catalog)

res_Catalog=glm(Choice~Recency+Frequency+Monetary, family=binomial("logit"))
summary(res_Catalog)
?glm
n=dim(Catalog)[1]
n
pred=predict.glm(res_Catalog,Catalog,type="response")
pred

pred.C=rep(0,n)
pred.C

pred.C[pred>0.5]=1
table(pred.C)

table(Catalog[,5],pred.C) #Classification Table

betas=coefficients(res_Catalog)
betas
odds_ratio=betas[1]+betas[2]*4+betas[3]*9+betas[4]*66.71
odds_ratio
plogis(odds_ratio)


NewData1=data.frame(4,9,66.71)
colnames(NewData1)=c("Recency","Frequency","Monetary")
predict(res_Catalog,NewData1,type="link")
predict(res_Catalog,NewData1,type="response")


NewData2=data.frame(5,8,70)
colnames(NewData2)=c("Recency","Frequency","Monetary")
predict(res_Catalog,NewData2,type="link")
predict(res_Catalog,NewData2,type="response")

################################ Customer Acquisition

jdp=read.csv("JDPowers.csv",head=TRUE)
attach(jdp)

Email=as.factor(Email)
Coupon=as.factor(Coupon)

res_JD=glm(Customer~Distance+Billboard+Email+Coupon,family=binomial)
summary(res_JD)

NewData3=data.frame(2,30,as.factor(0),as.factor(1))
colnames(NewData3)=c("Distance","Billboard","Email","Coupon")
predict(res_JD,NewData3,type="link")
predict(res_JD,NewData3,type="response")


#Following does not give accurate result 
betas=coefficients(res_JD)
betas
sum_1=betas[1]+betas[2]*2+betas[3]*30+betas[4]*0+betas[5]*1
plogis(sum_1)


NewData2=data.frame(2,30,as.factor(0),as.factor(0))
colnames(NewData2)=c("Distance","Billboard","Email","Coupon")
predict(res_JD, NewData2,type="link")
predict(res_JD,NewData2,type="response")

(plogis(0.403)-plogis(0))/plogis(0)
(exp(0.403)-exp(0))/exp(0)

NewData4=data.frame(2,30,as.factor(1),as.factor(0))
colnames(NewData4)=c("Distance","Billboard","Email","Coupon")
predict(res_JD, NewData4,type="link")
predict(res_JD,NewData4,type="response")

###########################################

bank=read.csv("bank-additional-full.csv",header=TRUE)

dim(bank)

train.prop=0.75
train.cases=sample(nrow(bank),nrow(bank),train.prop)

length(train.cases)
head(train.cases)

head(bank)
dim(bank)

class.train=bank[train.cases,c(1,3,6,11,14,21)]
class.valid=bank[-train.cases,c(1,3,6,11,14,21)]

head(class.valid)
dim(class.valid)

class.train$marital=as.factor(class.train[,2])
class.train$housing=as.factor(class.train[,3])

Y.train=class.train[,6]
X.train=class.train[,-6]

n=dim(X.train)[1]
n
dim(Y.train)[2]


fit.logit=glm(y~age+marital+housing+duration,,data=class.train,family=binomial("logit"))
summary(fit.logit)

pred.train=predict(fit.logit,class.train[,-6],type='response')
pred.train=ifelse(pred.train>0.5,1,0)
(ct=table(Y.train,pred.train))

diag(prop.table(ct,1))
sum(diag(prop.table(ct)))

class.valid$marital=as.factor(class.valid[,2])
class.valid$housing=as.factor(class.valid[,3])

pred.valid=predict(fit.logit,class.valid[,-6],type='response')
pred.valid=ifelse(pred.valid>0.5,1,0)

(ctv=table(class.valid[,6],pred.valid))

diag(prop.table(ctv,1))
sum(diag(prop.table(ctv)))

#Cross tab

table(bank[,21])/dim(bank)[1]

library(gmodels)
CrossTv=CrossTable(class.valid[,6],pred.valid)
CrossTv[2,2]/(dim(class.valid)[1]-CrossTv[2,1])

##########################################

plogis(+Inf)

qlogis(1)

pass.df <- read.csv("http://goo.gl/J8MH6A")
head(pass.df)
summary(pass.df)
pass.df$Promo = factor(pass.df$Promo,c("NoBundle","Bundle"))
summary(pass.df)

pass.tab <- c(242, 639, 38, 359, 284, 27, 449, 223, 83, 278, 49, 485)

dim(pass.tab)<-c(3,2,2)
class(pass.tab)="table"

table

dimnames(pass.tab)=list(Channel=c("Mail","Park","Email"),Promo=c("Bundle","NoBundle"),Pass=c("YesPass","NoPass"))
pass.tab

################################################
class(pass.tab)
str(pass.tab)

names(pass.tab)
dim(pass.tab)
class(pass.tab)
dimnames(pass.tab)
attributes(pass.tab)

####################################

library(vcdExtra)

pass.df=expand.dft(pass.tab)
str(pass.df)
pass.tab

table(pass.df$Pass,pass.df$Promo)

pass.df$Promo=factor(pass.df$Promo,levels=c("NoBundle","Bundle"))
table(pass.df$Pass,pass.df$Promo)

#############
pass.ml=glm(Pass~Promo,data=pass.df,family=binomial)
pass.ml
summary(pass.ml)

exp(coef((pass.ml)))

#########################
table(pass.df$Pass,pass.df$Channel)

library(vcd)

doubledecker(table(pass.df))

pass.m2=glm(Pass~Promo+Channel,data=pass.df,family=binomial)
pass.m2
summary(pass.m2)

exp(coef(pass.m2))

pass.m3=glm(Pass~Promo+Channel+Promo:Channel,data=pass.df,family=binomial)
pass.m3
summary(pass.m3)
exp(coef(pass.m3))
