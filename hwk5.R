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


#HomeWork 5
# Question 1

ghg$CO2_transform <- 1/ (ghg$co2 + 1000)

ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)
ghg$BorealV <- ifelse(ghg$Region == "Boreal",1,0)
ghg$TropicalV <- ifelse(ghg$Region == "Tropical",1,0)
ghg$AlpineV <- ifelse(ghg$Alpine == "yes",1,0)


mod.full <- lm(CO2_transform ~ airTemp +
                 runoff + 
                 log.DIP +
                 log.precip +
                 TropicalV +
                 mean.depth, data=ghg)
summary(mod.full)

res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)

qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)

shapiro.test(res.full)

plot(fit.full, res.full, pch =19, col="grey50")
abline(h=0)


reg.data <- data.frame(ghg$airTemp,
                         ghg$runoff,
                         ghg$log.DIP,
                         ghg$log.precip,
                         ghg$TropicalV,
                         ghg$mean.depth)

# make a correlation matrix 
chart.Correlation(reg.data, histogram=TRUE, pch=19)
summary(mod.full)
summ(mod.full)
full.step <- ols_step_forward_aic(mod.full)

full.step$predictors
full.step$model
plot(full.step)

#question 2

Pistachios <- ETall %>%
  filter(crop == "Pistachios")


Pistachio_ts <- na.omit(ts(Pistachios$ET.in,
                           start = c(2016, 1), frequency = 12))

Pistachio_dec <- decompose(Pistachio_ts)

Almonds <- ETall %>%
  filter(crop == "Almonds")


Almonds_ts <- na.omit(ts(Almonds$ET.in,
                           start = c(2016, 1), frequency = 12))

Almonds_dec <- decompose(Almonds_ts)

Fallow <- ETall %>%
  filter(crop == "Fallow/Idle Cropland")


Fallow_ts <- na.omit(ts(Fallow$ET.in,
                           start = c(2016, 1), frequency = 12))
Fallow_dec <- decompose(Fallow_ts)

Corn <- ETall %>%
  filter(crop == "Corn")


Corn_ts <- na.omit(ts(Corn$ET.in,
                           start = c(2016, 1), frequency = 12))

Corn_dec <- decompose(Corn_ts)

Grapes <- ETall %>%
  filter(crop == "Grapes (Table/Raisin)")


Grapes_ts <- na.omit(ts(Grapes$ET.in,
                           start = c(2016, 1), frequency = 12))

Grapes_dec <- decompose(Grapes_ts)

plot(Pistachio_dec)
plot(Almonds_dec)
plot(Fallow_dec)
plot(Corn_dec)
plot(Grapes_dec)

Pistachio_ar <- arima(Pistachio_ts , # data 
                               order = c(4,0,0))
Pistachio_ar

Almond_ar <- arima(Almonds_ts , # data 
                             order = c(4,0,0))
Almond_ar

Fallow_ar <- arima(Fallow_ts , # data 
                   order = c(4,0,0))
Fallow_ar

Corn_ar <- arima(Corn_ts, order=c(5,0,0))
Corn_ar

Grapes_ar <- arima(Grapes_ts, order=c(4,0,0))
Grapes_ar

Pistachio_fit <- Pistachio_ts - residuals(Pistachio_ar)
Almond_fit <- Almonds_ts - residuals(Almond_ar)
Fallow_fit <- Fallow_ts - residuals(Fallow_ar)
Corn_fit <- Corn_ts - residuals(Corn_ar)
Grapes_fit <- Grapes_ts - residuals(Grapes_ar)

New_Pistachio <- forecast(Pistachio_ar)
New_Almond <- forecast(Almond_ar)
New_Fallow <- forecast(Fallow_ar)
New_Corn <- forecast(Corn_ar)
New_Grapes <- forecast(Grapes_ar)

New_PistachioDF <- data.frame(New_Pistachio)
New_AlmondDF <- data.frame(New_Almond)
New_FallowDF <- data.frame(New_Fallow)
New_CornDF <- data.frame(New_Corn)
New_GrapesDF <- data.frame(New_Grapes)

years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))

New_PistachioDF$dateF <- ymd(paste(years,"/",month,"/",1))
New_AlmondDF$dateF <- ymd(paste(years,"/",month,"/",1))
New_FallowDF$dateF <- ymd(paste(years,"/",month,"/",1))
New_CornDF$dateF <- ymd(paste(years,"/",month,"/",1))
New_GrapesDF$dateF <- ymd(paste(years,"/",month,"/",1))


ggplot() +
  geom_line(data = Almonds, aes(x = ymd(date), y = ET.in))+
  ggtitle("Almond Evapotranspiration")+
  xlim(ymd(Almonds$date[1]), New_AlmondDF$dateF[24])+  # Plotting original data
  geom_line(data = New_AlmondDF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=New_AlmondDF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

ggplot() +
  geom_line(data = Pistachios, aes(x = ymd(date), y = ET.in))+
  ggtitle("Pistachio Evapotranspiration")+
  xlim(ymd(Pistachios$date[1]), New_PistachioDF$dateF[24])+  # Plotting original data
  geom_line(data = New_PistachioDF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=New_PistachioDF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

ggplot() +
  geom_line(data = Fallow, aes(x = ymd(date), y = ET.in))+
  ggtitle("Fallow Evapotranspiration")+
  xlim(ymd(Fallow$date[1]), New_FallowDF$dateF[24])+  # Plotting original data
  geom_line(data = New_FallowDF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=New_FallowDF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

ggplot() +
  geom_line(data = Corn, aes(x = ymd(date), y = ET.in))+
  ggtitle("Corn Evapotranspiration")+
  xlim(ymd(Corn$date[1]), New_CornDF$dateF[24])+  # Plotting original data
  geom_line(data = New_CornDF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=New_CornDF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

ggplot() +
  geom_line(data = Grapes, aes(x = ymd(date), y = ET.in))+
  ggtitle("Grapes Evapotranspiration")+
  xlim(ymd(Grapes$date[1]), New_GrapesDF$dateF[24])+  # Plotting original data
  geom_line(data = New_GrapesDF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=New_GrapesDF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")
