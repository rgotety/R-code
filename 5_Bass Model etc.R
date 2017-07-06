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


fit.logit=glm(y~age+marital+housing+duration,data=class.train,family=binomial("logit"))
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



########################################################## BASS MODEL

# 1. iphone example
idata=read.csv("sample_iphone_sales.csv",head=TRUE)

head(idata)

?ts
#start=C(2007,3) indicates that it will start at the year 2007
sales=ts(idata$Sales, start = c(2007,3),freq=4)
sales
# 2. Samsung
sdata=read.csv("Gallaxy.csv",head=TRUE)
head(sdata)
sales=ts(sdata$Sales,start=c(2007,1), freq=4)
sales

#3. In class
excel_sales=read.csv("Sales_excel.csv",head=TRUE)
head(excel_sales)
#In the below code, since no frequency is included, there is no need to define the second element of start vector.
sales=ts(excel_sales$Sales,start=c(2004))
sales


##### BASS Model
?plot
plot(sales,type="l",lty=2,col="red")#lty indicates the line type
points(sales,pch=20,col="blue")
title("Quarterly iphone")


Y=cumsum(sales)
Y=ts(Y,start=c(2007,3),freq=4)
plot(Y,type="l",lty=2,col="red")
?points
points(Y,pch=20,col="blue")# pch indicates the plotting character
title("Cumulative iphone")
length(Y)
Y=c(0,Y[1:(length(Y)-1)])
Ysq=Y^2
out=lm(sales~Y+Ysq)
summary(out)
length(Y)

a=out$coef[1]
b=out$coef[2]
c=out$coef[3]
a
b
c

mminus=(-b-sqrt(b^2-4*a*c))/(2*c)
m=mminus
p=a/m
q=b+p

###########Bass 2################
### External/Internal
Ext=NULL

for (t in 1:length(Y))
{Ext=c(Ext,p*(m-Y[t]))
Ext}

plot(Ext,type="l")
length(Y)



Int=NULL

for (t in 1:length(Y)){Int=c(Int,(q*Y[t]/m)*(m-Y[t]))}

plot(Int,type="l")


Bass_model=function(p,q,m,T=50){
  S=double(T)
  Y=double((T+1))
  Y[1]=0
  
  for (t in 1:T){
    S[t]=p*m+(q-p)*Y[t]-(q/m)*Y[t]^2
    Y[t+1]=Y[t]+S[t]
  }
  
  return(list(sales=S, cumSales=cumsum(S)))
}

#####################Bass 3 ############
#Pred & Plots

#1.
Spred=Bass_model(p,q,m,T=25)$sales
Spred







Spred=ts(Spred,start=c(2007,3),freq=4)
ts.plot(sales,Spred,col=c("blue","red"))

Spred=Bass_model(p,q,m,T=26)$sales
CumSpred=ts(cumsum(Spred),start=c(2007,3),freq=4)
CumSales=ts(cumsum(sales),start=c(2007,3),freq=4)

ts.plot(CumSales,CumSpred,col=c("blue","red"))

#2. Samsung
??Bass_model
Spred=Bass_model(p,q,m,T=25)$sales
Spred
Spred=ts(Spred,start=c(2007,1),freq=4)


ts.plot(sales,Spred,col=c("blue","red"))

Spred=Bass_model(p,q,m)$sales
CumSpred=ts(cumsum(Spred),start=c(2007,1),freq=4)
CumSales=ts(cumsum(sales),start=c(2007,1),freq=4)
ts.plot(CumSales,CumSpred,col=c("blue","red"))


#3. Sales
Spred=Bass_model(p,q,m,T=15)$sales
Spred
Spred=ts(Spred,start=2004)
ts.plot(sales,Spred,col=c("blue","red"))

Spred=Bass_model(p,q,m)$sales
CumSpred=ts(cumsum(Spred),start=2004)
CumSales=ts(cumsum(sales),start=2004)
ts.plot(CumSales,CumSpred,col=c("blue","red"))




###CORRECTIONS IN BELOW
Spred=Bass_model(p,q,m)$sales
CumSpred=ts(cumsum(Spred),start=c(2007,1),freq=4)
CumSales=ts(cumsum(sales),start=c(2007,1),freq=4)
ts.plot(CumSales,CumSpred,col=c("blue","red"))

# 3.
Spred=Bass_model(p,q,m,T=25)$sales
SPred=ts(Spred,start=2004)
#CumSpred=ts(cumsum(Spred),start=c(2007,1),freq=4)
#SumSales=ts(cumsum(sales),start=c(2007,1),freq=4)
ts.plot(sales,Spred,col=c("blue","red"))


#4
Spred=Bass_model(p,q,m)$sales
CumSpred=ts(cumsum(Spred),start=2004)
CumSales=ts(cumsum(sales),start=2004)
ts.plot(CumSales,CumSpred,col=c("blue","red"))
