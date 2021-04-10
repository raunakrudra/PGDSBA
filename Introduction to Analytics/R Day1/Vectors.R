#fix your working directory
setwd("XXXgive your own path hereXXX")
#data types in R
#Vectors
x=c(1,2,3,4,2,1,1)
print(x)
course_name=c("Zero2","Hero")
print(course_name)
class(course_name)#to check the dtype of an object
temp=c(1,234,"Bangalore")
print(temp)
class(temp)
temp[2] #indexing in R, starts from 1
x=c(2,11,22,33,44)
x[3:5]
x[c(3,4,5)]
x[-1]#all but the first index
#Important vector functions
sum(x)
length(x)
max(x)
min(x)
mean(x)
median(x)
x==4

x=c(1:10)
print(x)
y=seq(10,100,25)
print(y)

count.fields(y>5)

sum(y>5)

count(y>5)
length(y>5)
sum(y>10)
length(y>10)
y>5

y3 = (y>10&y<20)
length(y3)

y = seq(5,50,10)
y3 = (y>5&y<20)
sum(y3)

which(y==25)

which(max(y))
max(y)
which(y==max(y))

x = c("GL","PGP", "BABI")

grep("A",x)

y="BABI"
grep("A",y)
grep("q",y)
