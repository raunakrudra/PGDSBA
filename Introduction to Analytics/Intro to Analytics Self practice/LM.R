
                                                          #Importing Data

#install readr package
install.packages(readr)

#load readr package into session
library(readr)

#setting the current working directory
getwd()
setwd("C:\\Users\\jyoti\\OneDrive\\Documents\\R Sessions\\Intro to Analytics Self practice")
getwd()

#import .txt file as a dataframe with space seperated.

titanic = read.table("Titanic_space_separated-1.txt", header = TRUE)
class(titanic)
View(titanic)


#View top 10 or last 10 data
head(titanic,10)
tail(titanic,10)

#dataframe overview functions

str(titanic)

            #Convert column sex to character instead of factor

titanic$Sex = as.character(titanic$Sex)
str(titanic)

summary(titanic)

#import .txt file as a dataframe with comma seperated

orange = read.table("Orange_comma_separated.txt", header = TRUE, sep = ",")
View(orange)

orange$Tree = as.factor(orange$Tree)
str(orange)

summary(orange)

#import csv file

library(readr)

ctrypopulation = read.csv("Countries Population.csv")
View(ctrypopulation)

summary(ctrypopulation)

ctrypopulation$Country.Name  = as.factor(ctrypopulation$Country.Name)
ctrypopulation$Country.Code = as.factor(ctrypopulation$Country.Code)

summary(ctrypopulation)

#import excel file

install.packages("readxl")
library(readxl)

ctryregion = read_excel("Countries Region Mapping.xlsx")
View(ctryregion)

class(ctryregion)

    #convert the class to dataframe

ctryregion = as.data.frame(ctryregion)
class(ctryregion)


                                                    #Data Manipulation



#Get to know about functions

?colnames
?make.names


##change the col name in country population to remove spaces

colnames(ctrypopulation) = make.names(colnames(ctrypopulation))

colnames(ctrypopulation)


summary(ctrypopulation)


# obtain the coutries where population is more that 10 million

ctrypopulation[ctrypopulation$Total.Population.2017>10000000,]

bigctry = ctrypopulation[ctrypopulation$Total.Population.2017>10000000,]
bigctry

# obtain the coutries where population is more that 10 million
smallctry = ctrypopulation[ctrypopulation$Total.Population.2017<200000,]

# obtain the coutries where population is less than 10 million and more than 2 million

medctry = ctrypopulation[ctrypopulation$Total.Population.2017>200000 & ctrypopulation$Total.Population.2017<10000000,]


#Binding rows - appending rows

big.and.small.ctry = rbind(bigctry,smallctry)

# Binding columns - adding flag for Big ctry as TRUE and small country will automatically have false the flag column is merged with data.

big.ctry.flag = big.and.small.ctry$Total.Population.2017>10000000

big.and.small.ctry = cbind(big.and.small.ctry,big.ctry.flag)
big.and.small.ctry
View(big.and.small.ctry)

summary(big.and.small.ctry)


#Column transformation

big.and.small.ctry$X = log(big.and.small.ctry$Total.Population.2017)
big.and.small.ctry$X.1 = exp(big.and.small.ctry$X)
head(big.and.small.ctry,10)


## Sorting

# sorting based on country population ascending order

ctrypopulation = ctrypopulation[order(ctrypopulation$Total.Population.2017),]
View(ctrypopulation)


# sorting based on country population descending order


ctrypopulation = ctrypopulation[order(-ctrypopulation$Total.Population.2017),]


                                                #Sorting and Merging

##Use previously imported and manipulated dataframes

summary(ctrypopulation)
summary(ctryregion)


ctryregion$Country.Code = as.factor(ctryregion$Country.Code)
ctryregion$Region = as.factor(ctryregion$Region)
ctryregion$IncomeGroup = as.factor(ctryregion$IncomeGroup)

summary(ctryregion)


ctryregion$IncomeGroup = ordered(ctryregion$IncomeGroup,levels = c("Low income",
                                                                   "Lower middle income",
                                                                   "Upper middle income",
                                                                   "High income"))

#import 3rd dataset

ctryinfo = read.csv("Countries Indicators.csv")
summary(ctryinfo)
ctryinfo$Country.Code = as.factor(ctryinfo$Country.Code)


#Joins

ctry.pop.region = merge(ctrypopulation,ctryregion, by = "Country.Code")

ctry.all = merge(ctry.pop.region,ctryinfo, by = "Country.Code")     ##Inner Join
rm(ctry.all)

ctry.all = merge(ctry.pop.region,ctryinfo, by = "Country.Code", all.x = TRUE)   ## Left outer Join


                                                        #Data Summaries

region_vs_income = table(ctry.all$Region, ctry.all$IncomeGroup)
region_vs_income

prop.table(region_vs_income, 1)

prop.table(region_vs_income, 2)
View(ctry.all)
str(ctry.all)

#group_by dplyr

install.packages("dplyr")
library(dplyr)

# Apply group by in ctry.all by region and summarise

region.sum = ctry.all %>% group_by(ctry.all$Region) %>% summarise()
region.sum

# Apply group by function in ctry.all by region and income group

region.income = ctry.all %>% group_by(ctry.all$Region,ctry.all$IncomeGroup) %>% summarise()
region.income

#Apply group by function in ctry.all by region and income group and extract count in each categories

region.income = ctry.all %>% group_by(ctry.all$Region,ctry.all$IncomeGroup) %>% summarise(Count =n())
View(region.income)

#group by function to extract number of disticnt income groups within a region

region.sum = ctry.all %>% group_by(Region) %>% summarise(dist.income.count = n_distinct(IncomeGroup))
View(region.sum)
