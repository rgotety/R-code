### Basic Segmentation

seg.raw=read.csv("http://goo.gl/qw303p",head=TRUE)
seg.raw
seg.df=seg.raw[,-7]
summary(seg.df)
seg.df
#computing segment level mean values
seg.summ=function(data,groups){
  aggregate(data,list(groups),function(x)mean(as.numeric(x)))}

## Hierarchial clustering - distance based
library(cluster)
seg.dist=daisy(seg.df)
head(seg.dist)

as.matrix(seg.dist)[1:5,1:5]
dim(as.matrix(seg.dist))

seg.hc=hclust(seg.dist,method="complete")
plot(seg.hc)

cut(as.dendrogram(seg.hc),h=0.7)
?cut
cut(as.dendrogram(seg.hc),h=0.5)

plot(cut(as.dendrogram(seg.hc),h=0.5)$lower[[1]])
plot(cut(as.dendrogram(seg.hc),h=0.7)$lower[[1]])
plot(cut(as.dendrogram(seg.hc),h=1.0)$lower[[1]])

# Check Similarity

seg.df[c(101,107),]

##
plot(seg.hc)

rect.hclust(seg.hc, k=4, border="red")

seg.hc.segment = cutree(seg.hc, k=4)

seg.hc.segment[1:100]
table(seg.hc.segment)

seg.summ(seg.df, seg.hc.segment)#computing segment level mean values

plot(jitter(as.numeric(seg.df$gender))~jitter(as.numeric(seg.df$subscribe)),col=seg.hc.segment, yaxt="n",xaxt="n",ylab = "",xlab="")
?jitter
axis(1,at=c(1,2), labels=c("Subscribe: No", "Subscribe:Yes"))
axis(2, at=c(1,2), labels=levels(seg.df$gender))


### K-means (Distance based)

seg.df.num=seg.df
seg.df.num$gender=ifelse(seg.df$gender=="Male",0,1)
seg.df.num$ownHome=ifelse(seg.df$ownHome=="ownNo", 0,1)
seg.df.num$subscribe=ifelse(seg.df$subscribe=="subNo",0,1)

seg.k=kmeans(seg.df.num, centers=4)
seg.k$cluster
?kmeans
seg.summ(seg.df, seg.k$cluster) # to compute average values
# seg.df (1,2) is used, not seg.df.num (0,1)

boxplot(seg.df.num$income~seg.k$cluster, ylab="Income", xlab="cluster")


### TOOTHPASTE EXAMPLE ###
Tooth=read.csv("Toothpaste.csv",head=TRUE)

tooth.dist=daisy(Tooth)
as.matrix(tooth.dist)[1:5,1:5]
dim(as.matrix(tooth.dist))

tooth.hc=hclust(tooth.dist,method="complete")
plot(tooth.hc)

rect.hclust(tooth.hc, k=3, border="red")

tooth.hc.segment = cutree(tooth.hc, k=3)

tooth.hc.segment
table(tooth.hc.segment)


# Kmeans

tooth.k=kmeans(Tooth, centers=3)
tooth.k$cluster
table(tooth.k$cluster)

boxplot(Tooth$Prevent~tooth.k$cluster, ylab="prevent", xlab="cluster")
seg.summ(Tooth, tooth.k$cluster)

########################
# Latent Class Regression


library(flexmix)
library(MASS)
source=("Reordering.R")

#Simulation Study
p=10; n=200; K=2
K
d_t=rep(1/K,K)
?rep
X=matrix(0,n,p)
tau2_t=1
z_t=matrix(0,p,K)
mu_t=matrix(0,p,K)

z_t[,1]=c(rep(1,2),rep(0,8))
z_t[,2]=c(rep(0,2),rep(1,2),rep(0,6))

mu_t[,1]=c(rep(1,4),rep(0,6))#True coefficient values
mu_t[,2]=c(rep(0,4),rep(1.5,4),rep(0,2))
#mu_t[,3]=c(rep(1.5,2),rep(0.4,1),rep(0,18))
?sample
H_t=sample(1:K,n,replace = TRUE,d_t)
mu_t;H_t;z_t;d_t;
newp<-reorder(mu_t,H_t,z_t,d_t);
?reorder

mu_t=newp$mu
H_t=newp$H
z_t=newp$z
d_t=newp$d

Y=rep(0,n)
X=mvrnorm(n,rep(0,p),diag(1,p))
?mvrnorm
sig2_t=0.1

for(i in 1:n)
  Y[i]=X[i,]%*%mu_t[,H_t[i]]+rnorm(1,0,sqrt(sig2_t))

result1=lm(Y~0+X)#runninf linear models
summary(result1)

m1=flexmix(Y~0+X,k=1)
rm1=refit(m1)
summary(rm1)

m2=flexmix(Y~0+X,k=2)
rm2=refit(m2)
summary(rm2)
posterior(m2)

m3=flexmix(Y~0+X,k=3)
rm3=refit(m3)
summary(rm3)
?rnorm
