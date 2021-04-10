Titanic

titanic = read.csv("titanic\\train.csv")
View(titanic)

summary(titanic)
str(titanic)
View(titanic)

by(titanic, INDICES = list(titanic$Pclass,titanic$Sex), FUN = sum)

table(titanic$Sex)
prop.table(table(titanic$Sex))*100

prop.table(table(titanic$Pclass,titanic$Survived),1)*100

df2 = round(prop.table(table(titanic$Pclass,titanic$Survived),1)*100)

df1 = prop.table(table(titanic$Pclass,titanic$Sex,titanic$Survived),1)*100
df1
df2
class(df2)

x = c(1:10)
y = c(10:99)
countEven = function(x){
  cnt=0
  for (i in x){
    if(i%%2 == 0){
      cnt=cnt+1
          } 
  }
 return (cnt)

}
countEven(x)
countEven(y)


x = c(1,5,9,NA)

is.na(x)

x[which(is.na(x))] = 10
x
x[is.na(x)] = 4

sum(is.na(titanic$Age))
table(is.na(titanic$Age))

mean(titanic$Age)
mean(titanic$Age, na.rm=T)

titanic$Age[is.na(titanic$Age)] = 29.69

titanic[is.na(titanic$Age), "Age"] = mean(titanic$Age, na.rm=T)

sum(is.na(titanic$Age))

histogram(~titanic$Age)      
hist(titanic$Age)


table(titanic$Embarked)

is.blank(titanic$Embarked)

titanic$Embarked[which(titanic$Embarked=="")] = "S"

str(titanic)

titanic$Embarked = as.character(titanic$Embarked)

mode(titanic$Age)

summary(titanic)

table(titanic$Embarked)


modeest(titanic$Embarked)

modeest::titanic$Embarked

help = "modeest"

library(help = "modeest")

mlv(titanic$Age)


library(modeest)

mlv(titanic$Age, method="mfv")


strsplit("A|B","|")

z = strsplit(titanic$Name,",")


titanic$Name = as.character(titanic$Name)
z = strsplit(titanic$Name,",")
z



y = strsplit(z[[1]][2], ". ")

y
class(y)
s=y[[1]][1]
s

result = c()
for (i in titanic$Name){
  y=strsplit(i,", ")[[1]][2]
  y2=strsplit(y,". ")[[1]][1]
  result = c(result,y2)
}
print(result)

length(result)

titanic$Title = result
View(Titanic)

View(titanic)

table(titanic$Title, is.na(titanic$Age))
