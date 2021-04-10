setwd("C:\\Users\\jyoti\\OneDrive\\Documents\\PGPBABI\\TSF\\data")
GDPIndia = read.csv('GDPIndia.csv')
View(GDPIndia)
GDPIndia.ts = ts(GDPIndia$GDPpercapita,start = 1960)
GDPIndia.ts
plot(GDPIndia.ts)



Petrol= read.csv('Petrol.csv')

Petrol.ts = ts(Petrol$Consumption, start = c(2001,1), frequency = 4)
Petrol.ts               


TractorSales = read.csv('TractorSales.csv')
TractorSales.ts = ts(TractorSales$Number.of.Tractor.Sold, start = c(2003,1), frequency = 12)
TractorSales.ts

library(ggplot2)
library(forecast)
ggmonthplot(TractorSales.ts)
ggseasonplot(TractorSales.ts)


decompose(TractorSales.ts, type = "a")
plot(decompose(TractorSales.ts, type = "a"))

decompose(TractorSales.ts, type = "m")
plot(decompose(TractorSales.ts, type = "m"))


holt(GDPIndia.ts)
plot(holt(GDPIndia.ts))

ses(GDPIndia.ts)
plot(ses(GDPIndia.ts))
