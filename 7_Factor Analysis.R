### Dimension Reduction

factor.bank=read.csv("Bank.csv",head=TRUE)

brand.ratings=read.csv("http://goo.gl/IQl8nc")

x=1:1000
x.sc=(x-mean(x))/sd(x)
summary(x.sc)
plot(x.sc)

brand.sc=brand.ratings
brand.sc[,1:9]=scale(brand.ratings[,1:9])

summary(brand.sc)
library(corrplot)
corrplot(cor(brand.sc[,1:9]),order="hclust")


# Factor Analysis

library(nFactors)
library(Hmisc)
library(GPArotation)

#1: Bank
rcorr(as.matrix(factor.bank[,2:6]),type="pearson")
?nScree
nScree(factor.bank[,2:6])
eigen(cor(factor.bank[,2:6]))
?factanal
factanal(factor.bank[,2:6],factors=2)
factanal(factor.bank[,2:6],factors=2,rotation="varimax")
(bank_res=factanal(factor.bank[,2:6],factors=2,rotation="oblimin"))

factor_res=factanal(factor.bank[,2:6],factors=2,scores="Bartlett")
dim(factor_res$scores)
factor_res$scores #showing 'scores' (library function) of each of 15 values

names(factor_res)

X=as.matrix(factor.bank[,2:6]) #shows 15 values and 5 Xs
X

fX=as.matrix(factor_res$scores) #shows 15 values and 2 Factors

fX

Y=factor.bank[,7]

factor_res$scores
Y

summary(lm(Y~X)) # none of 5Xs is fairly significant

summary(lm(Y~fX)) # both Factors are significant


#2. Rextbook example (Brand Rating)
rcorr(as.matrix(brand.sc[,1:9]),type="pearson")

head(brand.sc)
nScree(brand.sc[,1:9])
eigen(cor(brand.sc[,1:9])) # total of 9 columns, therefore 9 Eigen Values

factanal(brand.sc[,1:9],factors=3)

factor_res=factanal(brand.sc[,1:9],factors=3,scores="regression")
dim(factor_res$scores)
head(factor_res$scores)


# Following cannot work as DV is categorical variable
X=as.matrix(brand.sc[,1:9])
X

fX=as.matrix(factor_res$scores)
fX

Y=brand.sc[,10]

factor_res$scores

summary(lm(Y~X))

summary(lm(Y~fX))
##

## Rotation ##
(brand.fa.ob=factanal(brand.sc[,1:9],factors=3,rotation="oblimin"))
library(semPlot)
semPaths(brand.fa.ob)
semPaths() #NO NEED OF THIS, AS WRONG LINKAGE IS DRAWN


## Dupont
DuPont=read.csv("Dupont.csv",head=TRUE)

nScree(DuPont[,7:25])
eigen(cor(DuPont[,7:25]))

factanal(DuPont[,7:25],factors=4,rotation = "varimax")
factanal(DuPont[,7:25],factors=4,rotation = "oblimin")
