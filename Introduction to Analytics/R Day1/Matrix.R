#matrix
x=matrix(1:10,nrow = 5,byrow = T)#by default byrow=F
print(x)

x2=matrix(1:10,nrow=3)
x2
x3=matrix(1:10,nrow=5,ncol = 4)
print(x3)

#indexing in 2D
x2[2,1] # 2nd row and 1st col
#to get all rows in 1st col
x2[,1]
class(x2)
#if you want all columns in 3rd row
x2[3,]
colnames(x2)=c("A","B","C","D")
print(x2)
rownames(x2)=c('P','Q','R','S','Y')
print(x2)
x2[2,3]       # using indexes
x2["S",]      #using Row names
x2[,"A"]      #using column names
#************************
x=matrix(1:15,nrow = 3)
x
x>10
x[x>5]
x[(x<14) &(x>10)]
colSums(x)
rowSums(x)

y =matrix(1:20, nrow=5)
print(y)
z=matrix(1:20, nrow=5, ncol=4)
print(z)

z[3,1:4]

v=matrix(1:10,nrow = 5)
print(v)

sum(z[,3])

colsums(z)
colSums(z)

sum(z>10)
