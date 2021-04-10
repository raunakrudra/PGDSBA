#list in R
x=list(A=c(1,2,3),B=c("bangalore","New Delhi"))
#list is a collection of vectors..
class(x)
print(x)
#indexing in a list
#we use $ notation to access individual vectors
x$A
x$B #etc...
#double square brackets...
x[[1]]
x[[2]]
x[[2]][2]
#to remane a list
names(x)=c("X","Y")


x = c(1:10)
df = iris
y = matrix(1:10, nrow = 5)
mylist = list(x,df,y)
mylist
mylist[[1]]

mylist[[1]][2]

mylist[[2]][5,3]


cnt=0
for (i in y[,1]){
  if(i%%2 == 0){
  cnt=cnt+1
  print(i)
} 
}
print(cnt)

y

cnt=0
for (i in mylist[[1]]){
  if(i%%2 == 0){
    cnt=cnt+1
    print(i)
  } 
}
print(cnt)

cnt=0
for (i in df[,1]){
  if(i%%2 != 0){
    cnt=cnt+1
    print(i)
  } 
}
print(paste0("the count is ",cnt))

df
df[,1]
