setwd("C:\\Users\\jyoti\\OneDrive\\Documents\\PGPBABI\\Advance Stats\\Group Assignment")
leslidata = read_excel('Dataset_LeslieSalt.xlsx')
str(leslidata)
dim(leslidata)
leslidata$County = as.factor(leslidata$County)
leslidata$Flood = as.factor(leslidata$Flood)
str(leslidata)
#leslidata$Flood = factor(x = leslidata$Flood,levels = c("0","1"), labels = c("No","Yes"))
#leslidata$County = factor(x=leslidata$County, levels = c("0","1"), labels = c('San Mateo','Santa Clara'))
library(DataExplorer)
create_report(leslidata)

library(ggplot2)
##Load the psych and lmtest Libraries
library(psych)
library(lmtest)
library(zoo)

#Univariate Analysis of Numerical Variables.

with(leslidata, boxplot(Size, main="Size"))


with(leslidata, boxplot(Elevation, main="Elevation"))

with(leslidata, boxplot(Elevation, main="Sewer"))

with(leslidata, boxplot(Elevation, main="Date"))

with(leslidata, boxplot(Elevation, main="Distance"))
#table for categorical variables

View(table(leslidata$County))
View(table(leslidata$Flood))

#Bivariate Analysis
with(leslidata, plot(Size, Price, pch=19, cex=0.6))

with(leslidata, plot(Elevation, Price, pch=19, cex=0.6))

with(leslidata, plot(Sewer, Price, pch=19, cex=0.6))

with(leslidata, plot(Date, Price, pch=19, cex=0.6))

with(leslidata, plot(Distance, Price, pch=19, cex=0.6))

ggplot(leslidata, aes(x=County, y=Price)) + 
  geom_boxplot()

ggplot(leslidata, aes(x=Flood, y=Price)) + 
  geom_boxplot()

##Creating a Correlation coefficient to identify any linear relationship, removing categorical variables.
cor(leslidata[,-c(2,7)])
cor(leslidata$Price,leslidata$Distance)


#An additional plot to check for correlation - instead of using the matrix
install.packages("corrplot")
library(corrplot)

corrplot(cor(leslidata[,-c(2,7)]))

#Scaling the numeric data to eliminate biasing and then doing cbind with factor columns.

leslidata_scale = data.frame(scale(leslidata[,-c(2,7)]))
View(leslidata_scale)

leslidata_final = cbind(leslidata_scale,leslidata$County,leslidata$Flood)
View(leslidata_final)
names(leslidata_final)
RMSE <- function(error){ sqrt(mean(error^2))}

#-----------------------creating full model with all independent variables and without removing outlier. Adj R2 of 67%---------------------------------
m1 = lm(Price ~ leslidata$County+Sewer+Elevation+Date+Size+leslidata$Flood+Distance,leslidata_final)
m1
summary(m1)

#-----------------------creating full model with all independent variables and removing outlier. Adj R2 of 67%------------------------------------------

leslidata_out = leslidata_final[-26,]
View(leslidata_out)
names(leslidata_out)
m4 = lm(Price ~ leslidata_out$`leslidata$County`+Sewer+Elevation+Date+Size+leslidata_out$`leslidata$Flood`+Distance,leslidata_out)
m4
summary(m1)

#-----------------------creating  model without distance. Adj R2 of 68%--------------------------------------------------------------------------------

m2 = lm(Price ~ leslidata$County+Sewer+Elevation+Date+Size+leslidata$Flood, data = leslidata_final)
m2
summary(m2)

#-----------------------creating  model without distance and outlier. Adj R2 of 72.56%-----------------------------------------------------------------

m5 = lm(Price ~ leslidata_out$`leslidata$County`+Sewer+Elevation+Date+Size+leslidata_out$`leslidata$Flood`, data = leslidata_out)
m5
summary(m5)

#------------------------creating  model without distance and size. Adj R2 of 65%---------------------------------------------------------------------

m3 = lm(Price ~ leslidata$County+Sewer+Elevation+Date+leslidata$Flood, data = leslidata_final)
m3
summary(m3)
RMSE(m3$residuals)
#------------------------creating  model without distance and size and outlier. Adj R2 of 72.78-------------------------------------------------------

m6 = lm(Price ~ leslidata_out$`leslidata$County`+Sewer+Elevation+Date+leslidata_out$`leslidata$Flood`, data = leslidata_out)
m6
summary(m6)
RMSE(m6$residuals)

Testlm_residuals3 = residuals(m6)
Testlm_fitted3 = fitted(m6)
Leslie_reg6 = cbind(leslidata_out,Testlm_residuals3, Testlm_fitted3)
with(Leslie_reg6, plot(Testlm_fitted3,Testlm_residuals3, pch=19, cex=0.6)) # check for homoscedasticity
abline(a=0,b=0)

vif(m6)
dwtest(m6)
gqtest(m6)
