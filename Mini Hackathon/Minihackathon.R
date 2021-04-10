setwd("C:\\Users\\jyoti\\OneDrive\\Documents\\PGPBABI\\Mini Hackathon")
cardata = read.csv('Training Data Set1.csv')
str(cardata)
dim(cardata)
View(cardata)
attach(cardata)

#library(DataExplorer)
#create_report(cardata)
summary(cardata)

#treating distance missing values using KNN
#install.packages(VIM)
library(VIM)
?kNN

cardata_f = kNN(cardata, variable = c("Distance", "engine_power"), k = 6)
summary(cardata_f)
str(cardata_f)
View(cardata_f)
names(cardata_f)
#Removing ID, manufacture_year, imputation column
cardata_final = cardata_f[,-c(1,7,18,19)]
View(cardata_final)
str(cardata_final)
cardata_final$Vroom.Audit.Rating = as.factor(cardata_final$Vroom.Audit.Rating)
cardata_final$door_count = as.factor(cardata_final$door_count)
cardata_final$seat_count = as.factor(cardata_final$seat_count)
attach(cardata_final)

#An additional plot to check for correlation - instead of using the matrix
install.packages("corrplot")
library(corrplot)
names(cardata_final)
corrplot(cor(cardata_final[,c(4,6,7,8,15)]))

#Scale the numerical columns
cardat_s = data.frame(scale(cardata_final[,c(4,6,7,8,15)]))
View(cardat_s)
cardata_scale = cbind(cardat_s,cardata_final$Maker,cardata_final$model,cardata_final$Location,cardata_final$Owner.Type,cardata_final$body_type,cardata_final$Vroom.Audit.Rating,cardata_final$transmission,cardata_final$door_count,cardata_final$seat_count,cardata_final$fuel_type)
View(cardata_scale)
#split the data into train and validation
library(caTools)

set.seed(123)
split = sample.split(cardata_scale$Price, SplitRatio = 0.7)
dt1 = subset(cardata_scale, split == TRUE)
dv1 = subset(cardata_scale, split == FALSE)

#Create a full linear regression model with all parameters. Adjusted r2 73%
str(cardata_final)
m.full=lm(Price~., cardata_final)
m.full
summary(m.full)

#predicting based on full model
car_test = read.csv('Test Data Set.csv')
View(car_test)
names(car_test)
car_test$Vroom.Audit.Rating = as.factor(car_test$Vroom.Audit.Rating)
car_test$door_count = as.factor(car_test$door_count)
car_test$seat_count = as.factor(car_test$seat_count)
car_test$predict <- predict(m.full, newdata = car_test)
car_test$predict

## Data Frame creation for submission file
predictions_df1 <- data.frame( Id = car_test$Id, Price = car_test$predict) 
View(predictions_df1)


write.csv(predictions_df1, "prediction1.csv", row.names=FALSE)
write.csv(predictions_df1, "prediction2.csv", row.names=FALSE)


RMSE <- function(error){ sqrt(mean(error^2))}
RMSE(m2$residuals)


#-----------------------Second Model with age of car and engine power-------------------------------#
names(cardata_final)
m1=lm(Price~Age.of.car+engine_power, cardata_final)
m1
summary(m1)
names(car_test)
car_test$Vroom.Audit.Rating = as.factor(car_test$Vroom.Audit.Rating)
car_test$door_count = as.factor(car_test$door_count)
car_test$seat_count = as.factor(car_test$seat_count)
car_test$predict1 <- predict(m1, newdata = car_test)
car_test$predict1

predictions_df2 <- data.frame( ID = car_test$Id, Price = car_test$predict1) 
View(predictions_df2)


write.csv(predictions_df2, "prediction3.csv", row.names=FALSE)

#-----------------------third Model with Maker, Location, Distance ,age of car and engine power-------------------------------#
names(cardata_final)

m2=lm(Price~Distance+Age.of.car+engine_power+Maker+model+body_type+transmission+Vroom.Audit.Rating+fuel_type, cardata_final)
m2
summary(m2)
RMSE(m2$residuals)
names(car_test)
car_test$Vroom.Audit.Rating = as.factor(car_test$Vroom.Audit.Rating)
car_test$door_count = as.factor(car_test$door_count)
car_test$seat_count = as.factor(car_test$seat_count)
car_test$predict2 <- predict(m1, newdata = car_test)
car_test$predict2

predictions_df3 <- data.frame( ID = car_test$Id, Price = car_test$predict2) 
View(predictions_df3)


write.csv(predictions_df3, "prediction6.csv", row.names=FALSE)

