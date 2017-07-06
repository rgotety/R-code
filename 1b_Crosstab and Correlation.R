CT=read.csv("CrossTab_Example.csv", head=TRUE)

AccountSize=table(CT[,2])
dimnames(AccountSize)=list(c("Small","Medium","Large"))

Recomm=table(CT[,3])


library(car)

n=dim(CT)

table=t(table(CT[,2],CT[,3]))
chisq.test(table)

library(gmodels)

CrossTable(CT[,2],CT[,3])

CrossTable(CT[,2],CT[,3],chisq=TRUE,expected = TRUE)

AccountSize=CT[,2]
AccountSize[AccountSize==1]="Small"
AccountSize[AccountSize==2]="Medium"
AccountSize[AccountSize==3]="Large"

Recomm=CT[,3]
Recomm[Recomm==0]="Not Recommended"
Recomm[Recomm==1]="Recommended"

CrossTable(AccountSize,Recomm,chisq=TRUE,expected = TRUE)


#Correlation

corr=read.csv("file:///C:/Users/kamil217/Documents/Correlation_Example.csv",head=TRUE)

price=corr[,2]
sales=corr[,3]

plot(price,sales)
cor(price,sales)

library(Hmisc)

rcorr(cbind(price,sales),type="pearson")
