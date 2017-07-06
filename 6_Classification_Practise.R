#Classification

#1
set.seed(100)

seg.raw=read.csv("http://goo.gl/qw303p")

train.prop=0.65
train.cases=sample(nrow(seg.raw),nrow(seg.raw)*train.prop)

length(train.cases)

seg.df.train=seg.raw[train.cases,]
seg.df.valid=seg.raw[-train.cases,]


#2
export=read.csv("Clients.csv",head=TRUE)
dim(export)

n=dim(export)[2]

Class=export[,1]
X.size=export[,2]
X.rev=export[,3]
X.year=export[,4]
X.prod=export[,5]

head(Class)

#3
bank=read.csv("bank-additional-full.csv",head=TRUE)
train.prop=0.75
train.cases=sample(nrow(bank),nrow(bank)*train.prop)

length(bank)

class.train=bank[train.cases,c(1,3,6,11,14,21)]
class.valid=bank[-train.cases,c(1,3,6,11,14,21)]

class.train$marital=as.factor(class.train[,2])
class.train$housing=as.factor(class.train[,3])

library(MASS)
library(ggplot2)
library(gmodels)


#2
fit.lda<-lda(Class ~ X.size +X.rev +X.year+X.prod,CV=TRUE)

(ct=table(Class,fit.lda$class))
CrossTable(Class,fit.lda$class)
#sum(diag(prop.table(ct))

fit.lda<-lda(Class ~ X.size +X.rev +X.year+X.prod)
plot(fit.lda, dimen=1,type="both")

#1
fit.seg=lda(Segment ~ .,data=seg.df.train,CV=TRUE)
summary(fit.seg)

### Bayes
library(e1071)
#2: Bank Data

seg.nb=naiveBayes(y~age+housing,data=class.train)
set.seed(100)

pred.nb=predict(seg.nb,class.valid)

table(pred.nb,class.valid$y)
prop.table(table(pred.nb))
library(cluster)

clusplot(class.valid[,-6],pred.nb,color=TRUE,shade=TRUE,labels=4, lines=0,main="Bla")
Cross1=CrossTable(class.valid$y,pred.nb)

CrossTv=table(class.valid$y,pred.nb)
CrossTv[2,2]/(dim(class.valid)[1]-CrossTv[1,1])


#Seg.raw
seg.nb=naiveBayes(Segment~ .,,data=seg.df.train)
set.seed(100)

(seg.nb.class=predict(seg.nb,seg.df.valid))

table(seg.nb.class)
prop.table(table(seg.nb.class))
library(cluster)

clusplot(seg.df.valid[,-7],seg.nb.class,color=TRUE,shade=TRUE,labels=4, lines=0,main="Bla")
mean(seg.df.valid$Segment==seg.nb.class)

table(seg.df.valid$Segment,seg.nb.class)

Cross1=CrossTable(seg.df.valid$Segment,seg.nb.class)

CrossTv=table(seg.df.valid$Segment,seg.nb.class)
CrossTv[2,2]/(dim(seg.df.valid)[1]-CrossTv[1,1])

########################## CART
library(rpart)
set.seed(100)

#1: Client

CART_fit.inv=rpart(Class ~ X.size+X.rev+X.year+X.prod, method="class")

printcp(CART_fit.inv)
summary(CART_fit.inv)
pred.rpart=predict(CART_fit.inv,type="class")

table(Class,pred.rpart)

plot(CART_fit.inv,uniform = TRUE, main="bla")
text(CART_fit.inv,use.n=TRUE,all=TRUE,cex=.8)

#2: Seg.raw
CART_fit=rpart(Segment ~ ., method="class",data=seg.df.train)

printcp(CART_fit)
summary(CART_fit)
pred.rpart=predict(CART_fit,type="class")


plot(CART_fit,uniform = TRUE, main="bla")
text(CART_fit,use.n=TRUE,all=TRUE,cex=.8)

#3: Bank
CART_fit.bank=rpart(y~age+duration+previous,data=class.train, method="class")

printcp(CART_fit.bank)
summary(CART_fit.bank)
pred.rpart=predict(CART_fit.bank,type="class")

table(y,pred.rpart)

plot(CART_fit.bank,uniform = TRUE, main="bla")
text(CART_fit.bank,use.n=TRUE,all=TRUE,cex=.8)



################# RANDOM FOREST
library(randomForest)

#1: Seg.rf
(seg.rf=randomForest(Segment~.,data=seg.df.train,ntree=1000))
library(cluster)

seg.rf.class.all=predict(seg.rf, seg.df.valid, predict.all = TRUE)
n=dim(seg.df.valid)[1]
seg.rf.class=rep(0,n)

for ( i in 1:n)
  {tmp=table(seg.rf.class.all$individual[i,])
  seg.rf.class[i]=names(tmp)[which.max(tmp)]
}

table(seg.df.valid$Segment,seg.rf.class)
CrossTable(seg.df.valid$Segment,seg.rf.class)

clusplot(seg.df.valid[,-7],seg.rf.class,color=TRUE, shade=TRUE, labels=4, lines=0)

(seg.rf=randomForest(Segment ~.,data=seg.df.train,ntree=1000,importance=TRUE))
importance(seg.rf)

varImpPlot(seg.rf)

library(gplots)
library(RColorBrewer)

heatmap.2(t(importance(seg.rf)[,1:4]),col=brewer.pal(9,"Blues"),dend="none",trace="none",key=FALSE,margins=c(10,10),main="bla")


#1: Bank
(RF_fit.bank=randomForest(Segment~.,data=seg.df.train,ntree=1000))
library(cluster)

bank.predict=predict(seg.rf, seg.df.valid, predict.all = TRUE)
n=dim(class.valid)[1]
seg.rf.class=rep(0,n)

for ( i in 1:n)
{tmp=table(bank.predict$individual[i,])
seg.rf.class[i]=names(tmp)[which.max(tmp)]
}

table(seg.df.valid$Segment,seg.rf.class)
CrossTable(seg.df.valid$Segment,seg.rf.class)

clusplot(seg.df.valid[,6],seg.rf.class,color=TRUE, shade=TRUE, labels=4, lines=0)

(seg.rf=randomForest(Segment ~.,data=seg.df.train,ntree=1000,importance=TRUE))
importance(seg.rf)


Cross1=CrossTable(class.valid$y,seg.rf.class)

CrossTv=table(class.valid$y,seg.rf.class)
CrossTv[2,2]/(dim(class.valid)[1]-CrossTv[1,1])
