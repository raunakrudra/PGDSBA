#**********************DATA FRAME****************
country=c("India","USA","CHINA")
gdp=c(20,70,50)
population=c(130,60,145)
df=data.frame(country,gdp,population)
df
class(df)
dim(df) #dimesnion
nrow(df)
ncol(df)
colnames(df) #all the columns used
#Access individual columns...
df[,3] #indexing works..
df$population
#indexing in a dataframe
df[ ,1] # to get all rows and first column
df$country #to access columns using $
df$gdp
#using matrix like indexing in df also works
df[2,3]
df[2,]

iris
View(iris)
dim(iris)
str(iris)

iris$Species=as.character(iris$Species)
str(iris)
iris[1,5] = "something"
View(iris)

iris$Species=as.factor(iris$Species)
str(iris)
sum(iris$Sepal.Length>5)

summary(iris)

boxplot(iris)

sd(iris$Sepal.Length)
hist(iris$Sepal.Length)
scatter.smooth(iris$Sepal.Length)
hist(iris$Sepal.Length)
hist(iris$Sepal.Length,breaks = 6, col = "yellow1", xlab = 'length', ylab = "count", main = "Hist of sepal length")

plot(iris$Sepal.Length,iris$Sepal.Width)
scatter.smooth(iris$Sepal.Length,iris$Sepal.Width)

quantile(iris$Sepal.Width,0.25)
q3=quantile(iris$Sepal.Width,0.75)
IQR(iris$Sepal.Width)

sum(iris$Sepal.Width>(1.5 * IQR(iris$Sepal.Width)+q3))

sum(iris$Sepal.Length>5 & iris$Sepal.Width<4)

dim(iris[(iris$Sepal.Length>5)&(iris$Sepal.Width<4), ])
summary(iris[(iris$Sepal.Length>5)&(iris$Sepal.Width<4), ])

table(iris[(iris$Sepal.Length>5)&(iris$Sepal.Width<4), ])
table(iris$Species)

temp = iris[(iris$Sepal.Length>5)&(iris$Sepal.Width<4), ]
table(temp$Species)    
