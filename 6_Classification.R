#CLASSIFICATION #


#1
set.seed(100)
seg.raw=read.csv("http://goo.gl/qw303p")
head(seg.raw)
dim(seg.raw)

train.prop=0.65
?sample
train.cases=sample(nrow(seg.raw),nrow(seg.raw)*train.prop)
length((train.cases))
head(train.cases)

seg.df.train=seg.raw[train.cases,]
dim(seg.df.train)
seg.df.valid=seg.raw[-train.cases,]


#2
export=read.csv("Clients.csv",head=TRUE)
dim(export)
n=dim(export)[2]
head(export)

class=export[,1] #DV
X.size=export[,2]
X.rev=export[,3]
X.year=export[,4]
X.prod=export[,5]


#3
bank=read.csv("bank-additional-full.csv",head=TRUE)
head(bank)

train.prop=0.75
train.cases=sample(nrow(bank),nrow(bank)*train.prop)

length(bank)
length(train.cases)

class.train=bank[train.cases,c(1,3,6,11,14,21)]
class.valid=bank[-train.cases,c(1,3,6,11,14,21)]

class.train$marital=as.factor(class.train[,2])
class.train$housing=as.factor(class.train[,3])

library(MASS)
library(ggplot2)
library(gmodels)


##1
fit.lda=lda(class~X.size+X.rev+X.year+X.prod, CV=TRUE)
names(fit.lda)
fit.lda$class
(ct=table(class,fit.lda$class)) # to calculate Jackard Similarity
CrossTable(class,fit.lda$class)
prop.table(ct)
diag(prop.table(ct,1))
sum(diag(prop.table(ct))) # For Hit Rate

fit.lda=lda(class~X.size+X.rev+X.year+X.prod)
fit.lda
plot(fit.lda,dimen = 1, type="both")




##2
fit.seg=lda(Segment ~ .,data=seg.df.train,CV=TRUE)
(ct=table(seg.df.train$Segment,fit.seg$class))
?diag
diag(prop.table(ct,1))
sum(diag(prop.table(ct)))

CrossTable(seg.df.train$Segment,fit.seg$class)

fit.seg=lda(Segment ~ .,data=seg.df.train)
fit.seg
pre_class=predict(fit.seg,seg.df.valid)$class

(ct1=table(seg.df.valid$Segment,pre_class))
diag(prop.table(ct1,1))
sum(diag(prop.table(ct1)))


### Naives Bayes
library(e1071)
#2: Bank Data

seg.nb=naiveBayes(y~age+housing,data=class.train)
set.seed(100)

pred.nb=predict(seg.nb,class.valid)

table(pred.nb,class.valid$y)
prop.table(table(pred.nb))
library(cluster)
?clusplot
clusplot(class.valid[,-6],pred.nb,color=TRUE,shade=TRUE,labels=4, lines=0,main="Bla")
Cross1=CrossTable(class.valid$y,pred.nb)

CrossTv=table(class.valid$y,pred.nb)
CrossTv[2,2]/(dim(class.valid)[1]-CrossTv[1,1])#Jaccard


#Seg.raw
seg.nb=naiveBayes(Segment~ .,data=seg.df.train)
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

### CART Modeling
library(rpart)
set.seed(100)

#1
CART_fit.inv=rpart(class~X.size+X.rev+X.year+X.prod,method="class")
printcp(CART_fit.inv)
summary(CART_fit.inv)
pred.rpart=predict(CART_fit.inv, type="class")

table(class,pred.rpart) # Classification Table

plot(CART_fit.inv, uniform=TRUE,main="Investmnet Classification Tree")
text(CART_fit.inv,use.n=TRUE, all=TRUE, cex=.8)

#2: Bank Example
CART_fit.bank=rpart(y~age+duration+previous,data=class.train,method="class")
printcp(CART_fit.bank)
summary(CART_fit.bank)

plot(CART_fit.bank, uniform=TRUE,main="Classification Tree")
text(CART_fit.bank,use.n=TRUE, all=TRUE, cex=.8)

pred.cart=predict(CART_fit.bank,newdata=class.valid,type="class")

Cross1=CrossTable(class.valid$y,pred.cart)

CrossTv=table(class.valid$y,pred.cart)
CrossTv[2,2]/(dim(class.valid)[1]-CrossTv[1,1])



#Random Forest Classification

library(randomForest)

(seg.rf=randomForest(Segment ~., data=seg.df.train,ntree=1000))

library(cluster)

seg.rf.class.all=predict(seg.rf,seg.df.valid,predict.all=TRUE)
n=dim(seg.df.valid)[1]
n

seg.rf.class=rep(0,n)
for ( i in 1:n){tmp=table(seg.rf.class.all$individual[i,])
seg.rf.class[i]=names(tmp)[which.max(tmp)]
}

table(seg.df.valid$Segment,seg.rf.class)
CrossTable(seg.df.valid$Segment, seg.rf.class)

clusplot(seg.df.valid[,-7], seg.rf.class, color=TRUE, shade=TRUE, labels=4, lines=0, main="Rain Forest, holdout data")

(seg.rf=randomForest(Segment ~.,data=seg.df.train, ntree=1000,importance=TRUE))

importance(seg.rf)

varImpPlot(seg.rf, main="Var importance")

library(gplots)
library(RColorBrewer)

heatmap.2(t(importance(seg.rf)[,1:4]),col=brewer.pal(9,"Blues"),dend="none",trace="none",key=FALSE, margins = c(10,10),main="VAR...")



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
