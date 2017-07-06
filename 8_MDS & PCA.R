### PCA


brand.ratings=read.csv("http://goo.gl/IQl8nc")
head(brand.ratings)

x=1:1000
x.sc=(x-mean(x))/sd(x)
summary(x.sc)
plot(x.sc)

brand.sc=brand.ratings
brand.sc[,1:9]=scale(brand.ratings[,1:9])

summary(brand.sc)
library(corrplot)
corrplot(cor(brand.sc[,1:9]),order="hclust")
cor(brand.sc[,1:9])


#PCA with Simple Simulation
xvar=sample(1:10,100,replace=TRUE)
yvar=xvar
yvar[sample(1:length(yvar),50)]
zvar=yvar
zvar[sample(1:length(zvar),50)]
my.vars=cbind(xvar,yvar,zvar)

cor(my.vars)
plot(yvar ~ xvar, data=jitter(my.vars))
plot(yvar ~ xvar, data=(my.vars))

my.pca=prcomp(my.vars,scores=TRUE)
summary(my.pca)

pca.scores=prcomp(my.vars,scale=FALSE)
biplot(my.pca)


#Perceptal Map
brand.pc=prcomp(brand.sc[,1:9])
summary(brand.pc)
brand.pc

plot(brand.pc, type="l")
biplot(brand.pc, cex=c(0.5,1))

brand.mean=aggregate(.~brand, data=brand.sc, mean)

brand.mean #mean of each attribute for a particular brand
brand.mean=brand.mean[,-1] #to remove 'brand' column from brand.mean
rownames(brand.mean)=paste("",letters,sep="")[1:10]
brand.mu.pc=prcomp(brand.mean,scale=TRUE)
summary(brand.mean)

biplot(brand.mu.pc,main="Brand",cex=c(1,1))
cor(brand.sc[,1:9])
brand.mean[1,]-brand.mean[10,] #to check which brands have large/small diff between their attributes
# a and j in above case, which have small diff



### MDS by cmdscale
brand.dist=dist(brand.mean) #to calculate distances
(brand.mds=cmdscale(brand.dist)) ### HOW to interpret its output
plot(brand.mds,type="n")
rownames(brand.mds)=paste("",letters,sep="")[1:10]
text(brand.mds,rownames(brand.mds),cex=0.7)


### Infiniti case
Infiniti=read.csv("Infiniti.csv",head=TRUE)
infi=Infiniti[,2:20]
rownames(infi)=as.character(Infiniti[,1])

infi.dist=dist(infi)
(infi.mds=cmdscale(infi.dist))

plot(infi.mds,type="n")
rownames(infi.mds)=as.character(Infiniti[,1])
text(infi.mds,rownames(infi.mds),cex=0.7)
