print("Hello world")
setwd("C:\\Users\\jyoti\\OneDrive\\Documents\\R Sessions\\R Day1")
getwd()

getwd()

library(rpivotTable)


myData = read.csv("C:\\Users\\jyoti\\OneDrive\\Documents\\R Sessions\\R Day2\\CardioGoodFitness.csv")
myData

setwd("C:\\Users\\jyoti\\OneDrive\\Documents\\R Sessions\\R Day2")

names(myData)
mean(myData$Miles)
attach(myData)
mean(Miles)


summary(myData)
str(myData)

boxplot(myData$Education)

#install.packages("lattice")
library(lattice)

cardio_subset = myData[ ,c(myData$Product)]
View(cardio_subset)


Cardio = c(myData$Product)
cardio = myData[,1:3]
View(cardio)

table(myData$Product)

temp=myData[myData$Product=="TM195",]
summary(temp)
View(temp)

str(temp)
temp$Product=as.character(temp$Product)
summary(temp)


sd(myData$Miles)
sd(Miles)
hist(Miles)

boxplot(Miles)


by(myData, INDICES = Product, FUN = summary)
histogram(~Miles | Product)
boxplot(Miles ~Product)
boxplot(Age ~Product)
boxplot(Income ~Product)


rpivotTable(myData)

View(myData)

summary(myData)
str(myData)

by(myData, INDICES = list(Product,Gender), FUN = Summary)

by(myData, INDICES = list(Product,Gender), FUN = summary)


qbinom(p = 0.18,size = 500,prob = 0.18,lower.tail = TRUE)




dbinom(1,10,0.6,0)


sd(40, 45, 50, 45, 40, 35, 55, 40, 60)

x= c(40,45,50,45,40,35,55,40,60)
sd(x)

y = c(35, 55, 65, 75, 45, 50, 70, 65, 30)
sd(y)

z = c(2, 6, 5, 3, 8, 2, 1, 9, 7, 7, 6, 4)
median(z)


b = c(3, 5, 10, 10, 11, 12, 12, 14, 14, 14, 19)
boxplot(b, horizontal = TRUE)

m = c(2, 6, 5, 3, 8, 2, 1, 9, 7, 7, 6, 4)
summary(m)

summary(b)

dbinom(x = 0,size = 100,prob = 0.03)
