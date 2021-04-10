# Importing libraries
library(DataExplorer)
library(ggplot2)
library(forecast)
library (esquisse)
library(tseries)
library(Metrics)

install.packages("Metrics")


# Reading csv file
setwd( "C:\\Users\\jyoti\\OneDrive\\Documents\\PGPBABI\\TSF\\Group Assignment")
fancy = read.csv("Fancy.csv")
names(fancy)
summary(fancy)
str(fancy)

fancy[1,]

#creating the time series for the data

fancy.ts = ts(fancy[,2], start = c(1987,1), frequency = 12)
fancy.ts


############################## Doing the initial EDA on the time series######################################

plot(fancy.ts)  # we see an upward trend in general and there seems to be seasonality in the data.

ggmonthplot(fancy.ts)  # Decomposed original time series into 12 monthly time series and looking at data from month over month perspective. For every month over years we
# see an upward trend in general barring few randomness. There is slight increase in average number of sales for month of March and then since July there is an 
# increase in sales which is peaking in month of december(average sales closing to 50,000). This suggests strong seasonality towards the holiday season.

ggseasonplot(fancy.ts, year.labels = TRUE) # As the year goes by the increase in number of sales seems to be going up in multiclipative way. Also the plot suggests that there is some
# irregular component present in time series as well. There was some drop in year 1990

# Decompose the time series in Trend, Seasonality and random part based on additive approach
fancy_decom_add = decompose(fancy.ts, type = "a")
fancy_decom_add
plot(fancy_decom_add) 

# we observe that there is an upward trend for moving averages and decomposing asserts our assumption that there is stroong seasonality towards
# year end when the seasonal component is positive. The randon component indicates that we were over estimating the sale in December from '87 to '90 and then underestimated
# the sales in 1992

# Decompose the time series in Trend, Seasonality and random part based on multiplicative approach
fancy_decom_mult = decompose(fancy.ts, type = "m")
fancy_decom_mult
plot(fancy_decom_mult) 

# we observe that there is an upward trend for moving averages and decomposing asserts our assumption that there is stroong seasonality towards
# year end when the seasonal component is positive. This decomposition also looks very fitted to the time series as the randomness is distributed between range 0.7 to 1.3
# The randon component indicates that it is doing a better job in estimating the sales for November and December and there doesn't seems to be a trend in multiplicative.
# The seasonality effect is also better represented as percentages
# in case of this time series.

#check the stationarity of time series

fancy_st = adf.test(fancy.ts)
fancy_st  

# p-value of 0.5 suggests that time series is not stationary and we will have to look at the change of sales to see if that is stationary

fancy_diff_f = diff(fancy.ts)  # change of sales time series
fancy_st_f = adf.test(fancy_diff_f) # Dickey-Fuller test to check the stationarity of time series.
fancy_st_f  

# p-value is less that 0.05 which suggests that change of sales time series is stationary, plotting the change of sales times series to visualize.
plot(diff(fancy.ts))

########################################################   Building the winter holt model ##########################################################

# Since there is both trend and seasonality in the time series we will be using all the 3 smoothening parameters alpha, beta & gamma and use tripe smoothening method.

#######################creating a basic holt winter's model with additive method##########################

fancy_model_hw = hw(fancy.ts)
fancy_model_hw
plot(fancy_model_hw)
summary(fancy_model_hw)

#increasing the horizon to 5 years

fancy_model_hw5 = hw(fancy.ts, h = 60)
fancy_model_hw5
plot(fancy_model_hw5)
summary(fancy_model_hw5)





#creating a test set with 3 years and trains set of 4 years
fancy.train = window(fancy.ts, end=c(1990,12))
fancy.train

fancy.test = window(fancy.ts, start =c(1991,1))
fancy.test

fancy_model_train4 = hw(fancy.train, h=36)
fancy_model_train4
summary(fancy_model_train4)

fancy.forecast.add = fancy_model_train4$mean
fancy.forecast.add

plot(fancy.test)
lines(fancy.forecast.add, col=2)
fancy.test - fancy.forecast.add

RMSE_add = sqrt(sum((fancy.test - fancy.forecast.add)^2))
RMSE_add

MAPE_add = mape(fancy.test,fancy.forecast.add)# Calculating MAPE of multiplicative model.
MAPE_add

#We will now use the obtained smoothing parameters to predict for horizon of 5 years.

fancy_model_train4_hw5 = hw(fancy.ts, alpha = 0.0002, beta = 0.0001, gamma = 0.0001, h=60)
fancy_model_train4_hw5


fancy.forecast.add_hw5 = fancy_model_train4_hw5$mean
fancy.forecast.add_hw5

plot(fancy.forecast.add_hw5)
plot(fancy_model_train4_hw5)

#######################creating a basic holt winter's model with multiplicative method#####################
fancy_model_mult_hw5 = hw(fancy.ts, h=60, seasonal = "m")
fancy_model_mult_hw5
plot(fancy_model_mult_hw5)
summary(fancy_model_mult_hw5)  # both RMSe and MAPE of multiplicative model suggests that this method fits better to the time series.

#creating a test set with 2 years and train set of 5 years to take into account the recent explosion in sales. Taking a train of 4 years data will result in negative trends.

fancy.train_mult = window(fancy.ts, end=c(1991,12))
fancy.train_mult

fancy.test_mult = window(fancy.ts, start =c(1992,1))
fancy.test_mult

fancy.forecast.mult = hw(fancy.train_mult, h=24, seasonal = "m")$mean
fancy.forecast.mult

summary(hw(fancy.train_mult, h=24, seasonal = "m"))

plot(fancy.test_mult)
lines(fancy.forecast.mult, col=2)  



#underforecasting but better then additive model.
fancy.test_mult - fancy.forecast.mult

RMSE_mult = sqrt(sum((fancy.test_mult - fancy.forecast.mult)^2))
RMSE_mult

MAPE_mult = mape(fancy.test_mult, fancy.forecast.mult)  # Calculating MAPE of multiplicative model.
MAPE_mult # MAPE of multiplicative is better than additive



############################# Creating a hw model after optimizing the smoothening parameters ################################
#### current smoeethening parameters  


fancy.forecast.opt = hw(fancy.train_mult, h=24, alpha = 0.2178, beta = 0.04, gamma = 0.0001, seasonal = "m")$mean
fancy.forecast.opt
plot(fancy.test_mult)
lines(fancy.forecast.opt, col=2)

fancy.test_mult - fancy.forecast.opt


RMSE_opt = sqrt(sum((fancy.test_mult - fancy.forecast.opt)^2))
RMSE_opt

MAPE_mult = mape(fancy.test_mult, fancy.forecast.opt)  # Calculating MAPE of optimized model.
MAPE_mult

####################Building the final model ###########################
fancy.forecast.opt_5 = hw(fancy.ts, h=60, alpha = 0.2178, beta = 0.04, gamma = 0.0001, seasonal = "m")
fancy.forecast.opt_5$mean
plot(fancy.forecast.opt_5)

