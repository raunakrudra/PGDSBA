setwd("C:\\Users\\jyoti\\OneDrive\\Documents\\PGPBABI\\Data Mining")
data = read.csv("2.csv")
str(data)
data
datascale = data[,-c(1)]

View(datascale)
dist_euc1 = dist(data, method = "euclidean")
dist_euc1

clus2 <- hclust(dist_euc1, method = "average")
clus2
plot(clus2)
rect.hclust(clus2, k=3, border="red")

kc3 = kmeans(x=datascale, centers = 3)
kc3$cluster

data$kc3 <- kc3$cluster

write.csv(data, "clusterdata.csv")
