library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)
library(lubridate)
library(forecast)

# read in greenhouse gas data from reservoirs
ghg <- read.csv("/cloud/project/hwk5/Deemer_GHG_Data.csv")
ETdat <- read.csv("/cloud/project/hwk5/ETdata.csv")

ETall <- ETdat %>%
  group_by(date, crop) %>%
  summarise(ET.in = mean (Ensemble.ET))

Pistachios <- ETall %>%
  filter(crop == "Pistachios")


Pistachio_ts <- na.omit(ts(Pistachios$ET.in,
                           start = c(2016, 1), frequency = 12))

Pistachio_dec <- decompose(Pistachio_ts)
plot(Pistachio_dec)

acf(Pistachio_ts, lag.max=24)

pacf(Pistachio_ts, lag.max=24)

model1 <- arima(Pistachio_ts, order = c(1, 0, 0))
model1
