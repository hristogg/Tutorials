head(AirPassengers)
library(forecast)
library(ggplot2)

a = ts(AirPassengers, start=1949, end=1960, frequency = 12)
seasonplot(a, year.labels = TRUE, col=rainbow(12), labelgap = 0.35, cex=0.75)
g?seasonplot
ggseasonplot(a)

b <- read.csv('irregular-sensor.csv', header = FALSE)
class(b$V1)

library(zoo)
library(tidyr)

?separate

irreg.split = separate(b, col = V1, into = c('date', 'time'), sep=8, remove = T)

sensor.date = strptime(irreg.split$date, '%m/%d/%y')

irregts.df = data.frame(date=as.Date(sensor.date), measurement = b$V2)

irreg.date = zoo(irregts.df$measurement, order.by = irregts.df$date)
ag.aggr = aggregate(irreg.date, as.Date, mean)


###############  Outliers and Missing Values ###############  
tsoutliers()  #lib forecast
ts() #lib tsoutliers
#missing data imputation
#zoo na.fill() na.locf() na.trim()
#forecast  na.interp(), tsclean()


#Stationarity - no trend, mean stays constant, variance stays constant
#Heteroscedasticty - same mean, but different Variance  - non stationary
#trend - mean is not constant, variance is - non-stationary
#Seasonal - variance and mean the same, spikes with same distance
#exponential trend - mean not constant, variance non constant - non-stationary


###############  simple methods ###############  

# naive() last observation snaive()
# meanf() rwf()


#AIC compares complexity of differnet models, penalizes the more complex models - lower the better
#accuracy()   

#ideally we want random residuals ( all the patern should be in the model), they should be uncorrelated
#normally distributed, mean of 0 and constant variance

#non zero mean can be taken care of with addition/substraction
#correlation- modelling tools e.g. differencing
#normal distribtion - transformations e.g. logs


#Stationarity - has the data the same statistical properties throughtout the time series (variance, mean,  autocorrelation)
#unit root test - fUnitroots adn urca?

###############  Exercise ###############  

set.seed(54)
myts <- ts(c(rnorm(50,34,10), rnorm(67,7,1), runif(23,3,14)))
#myts <- log(myts)
#myts <- diff(myts)
plot(myts)

acf(myts)

summary(myts)

acf(myts)
library(tseries)
adf.test(myts)

#seems like the dataset is non-stationary - has different variance and mean overtime
#we need to make the dataset stationary
plot(diff(myts))
acf(diff(myts))

myts_n <- naive(myts, 10)
plot(myts_n)
myts_m <- meanf(myts, 10)
myts_r <- rwf(myts,10, drif= T)

legend('bottomleft', lty=1, col=c('blue','green','red'), bty='n', cex=0.75, legend=c('Naive', 'Mean', 'Drift'))

length(myts)
myts_train <- myts[1:112]
myts_test <- myts[113:140]

myts_naive <- naive(myts_train, 28)
myts_mean <- meanf(myts_train, 28)
myts_drift <- rwf(myts_train, drift = T, 28)

plot(myts)
lines(myts_naive$mean, col = 'red')
lines(myts_mean$mean, col = 'green')
lines(myts_drift$mean, col = 'green')

accuracy(myts_naive, myts_test)
accuracy(myts_mean, myts_test)
accuracy(myts_drift, myts_test)

#checking residuals
plot(myts_n$residuals)#heteroscedasticity is prsent - variance is not the same
#
mean(myts_n$residuals[2:140])
hist(myts_n$residuals) 
#normality test:
shapiro.test(myts_n$residuals) #null hypothesis is the it is a normal distribution - less than 0.05 so we can reject it

acf(myts_n$residuals[2:139])

#we can do  a log transformation to which would improve greatly the residual performanc


#nnetar nfor libraries for neural nets


##################### Seasonal Decomposition ########################


#drawbacks - slow to catch changes and assumes constant seasonality
#can be either additive or multiplicative


#decomposing the AirPassengers dataset
plot(AirPassengers)
frequency(AirPassengers)
#we see a trend and seasonal pattern
#increasing amplitude of seasons-  probably multiplicative decomposition?

air_ad <- decompose(AirPassengers, type='additive')
air_mu <- decompose(AirPassengers, type='multiplicative')


#in the multiplicative more seems to have gone to the random 

plot(air_ad)
plot(air_mu)
plot(air_ad$trend + air_ad$random)



##################### Simple Moving Average ########################
#define the number of observations and take their average
library('TTR')
x=c(1,2,3,4,5,6,7)
plot(x,type='l')
SMA(x,n=3)
plot(SMA(x,n=3))

#trying with lynx
lynxsmooth = SMA(lynx, n=4)
plot(lynx)
plot(lynxsmooth)
lines(lynxsmooth, col='red')


##################### Exponential Smoothign ##################### 

#ses() for datasets without trend and seasonality
# holt() with trend without seasonality; damp for declining trend over time
#hw() trend + seasonality + Damp

#ets() - automated model selection  - based on information criterion
#models are based on coefficients, reactive - coefficient close to 1 puts really high weight on the more recent observations
#coefficient closer to 0 is smoother taking into accoutn old data as well

# alpha - initial level; beta - trend; gamma - seasonality; 
#default zzz - autoselection - A addivtive, M multiplicatve, N non-present


#example:
library(forecast)
etsmodel = ets(nottem)
etsmodel

#model - error - Additive, trend - None, Seasonality - Additive

#alpha and gamam seem close to 0, more of a smoother model

plot(nottem,lwd=2)
lines(etsmodel$fitted, col='red')
plot(forecast(etsmodel, h=12))

#manually choosing error and seasonality to be multiplicative, letting R decide on trend
etsmodmult = ets(nottem,model='MZM')
etsmodmult
plot(nottem,lwd=2)
lines(etsmodmult$fitted, col='red')
 

##################### ARIMA ##################### 

#autoregressive integrated moving average
# (p,d,q)
# p - autoregressive part (AR)
# d - degree of differencing (Integrated)
# q - 
#ARAIMA requires stationary time series! if we start we non-stationary one it will be differenced until its 
#stationary

#auto.arima() vs arima() - base arima is manual, whereas autoarima automatically calculates the parameters, wheres
#in arima we have to mainly use pacf and acf to choose some of the parametrs


#Seasonal datasets are easier to model with exponential smoothing or seasonal decomposition?

#ARIMA(1,0,0) = first order lag of AR
# Yt = Constant + Coefficient * Yt-1 + Error_Term_t
#ARIMA(0,0,1) = first order MA

#ARIMA(1,0,1) - ARMA
#Yt = Constant + Coefficient * Yt-1 + Coefficient * ErrorTerm_T-1 + Error_Term_t


### auto.arima - popular and easy to use, but there exists a danger of producing uninformed, low quality models
#good as a benchmark
#example:


plot(lynx)
#no seasonality, cyclical; logically based on the nature of the dataset if we extract above a certain number
#of lynx, next season we will have less

#checking for autoregression (p) with acf and pacf - 
tsdisplay(lynx)
tsdisplay(lynx)
#the dataset looks like it is stationary, the plot looks harmonic, so d might be 0 

adf.test(lynx)
#the test confirms it   

auto.arima(lynx)
#2,0,2 ARIMA

auto.arima(lynx, stepwise = FALSE, trace= TRUE, approximation = F)
#4,0,0  

##################### Reproducing ARIMA model manuallu ##################### 

#using lynx dataset
#model 2,0,0 

myarima = arima(lynx, order= c(2,0,0))
myarima


#Yt = Constant + Coefficient_1 * Y_t-1 + Coefficient_2 * Y_t-2 + Error_Term_t
#Yt - mean = Coefficient_1(Y_t-1 - mean) + Coefficient_2(Yt-2 - mean) + Error_term_t

#how does R calculate the the last values...

tail(lynx)
tail(residuals(myarima))
#manually calculating for the last value
(2657 - 1545.4458) * 1.1474 + (1590 - 1545.4458) * (-0.5997) + 601.8380
3396 - 1545.4458


#manually calculating for MA(2) model (0,0,2) ARIMA
myarima = arima(lynx, order=c(0,0,2))

#Yt= Constant + Error_Term_t-1 * Coefficient_T-1 + Error_Term_t-2 * Coefficient_T-2 + Error_Term_T
myarima
tail(residuals(myarima))
844.7122 * 1.1407 + 255.91108 * 0.4697 + 766.83050
3396 - 1545.37



##################### ARIMA simulation ##################### 
set.seed(123)
asim <- arima.sim(model = list(order = c(1,0,1) , ar= c(0.4), ma=c(0.3)), n = 1000) + 10
plot(asim)
library(zoo)
plot(rollmean(asim,50))
plot(rollmean(asim,25))

#stationarity
adf.test(asim)
#dataset is stationary

#autoregression
tsdisplay(asim)

#checking if auto.arima will cofnrim the parameters we have seleection
auto.arima(asim, trace = T, stepwise = F, approximation = F)
#1,0,1 is indeed chosen ar1 = 0.31 we indicated 0.4 ma1 = 0.33 - closer to the 0.3 we indicated and mean 10 

##################### Manuam Model Selection ##################### 

#R base arima() does not calculate a constant if there is a differencing step 
# Arima() from forecast packag could be used then

#sample with lynx dataset
#testing for stationarity
adf.test(lynx)

#seems like the dataset is stationary, se we should not need any differencing
#d = 0

#testing for autoregression based on acf and pacf
tsdisplay(lynx)
#generally the acf tells us more about the MA part - q, whereas the pacf tells us more about the AR part - p

#acf is significant at almost every lag
#pacf is is significant at 1,2 and possibly 4
#we can check what happens with an AR(2) model 
myarima <- Arima(lynx, order=c(2,0,0))
#checking the info criterion - aICC 1878

#we would expect there to be no autoregression in the residuals and they to be normally distributed 
checkresiduals(myarima)

#ACF still shows autoregreesion, generally if it is at the beginning it is a clear sign that 
#there still is autoregression present, if it is towards the end it could be by chance
#however we can still try to adjust the model and see if we can improve

myarima <- Arima(lynx, order=c(3,0,0))
#aic - 1880 - worse than the previous one
checkresiduals(myarima)
#problematic lags are still outside the threshold

myarima <- Arima(lynx, order=c(4,0,0))
#aic 1874 - best one so far
checkresiduals(myarima)
#autoregression seems to be eliminated
#although residuals are still not exactly normal, although close

#with p = 5, we get worse results than AR(4)
#therefore AR(4) seems to be the model we could choose

### exaple of MA model
set.seed(123)
myts <- arima.sim(model = list(order = c(0,0,2), ma=c(0.3, 0.7)), n=1000) + 10
#checking for stationarity
adf.test(myts)
#we do not need differencing
tsdisplay(myts)
#we can start with the 2 lags outside of boundary on the ACF -> MA part
myarima <- Arima(myts, order=c(0,0,2))
#aic = 2828
checkresiduals(myarima)
#normally distributed residuals 
#autocorrelation seems fine as well - only one lag outside of boundary whichs is fine under 0.95 assumption

#with MA(3) the mdoel shows no improvement

#####################  Forecasting ##################### 
#r base predict() forecast package - forecast()
myarima <- auto.arima(lynx, stepwise = F, approximation = F)
#10 years forecast
arimafore <- forecast(myarima, h = 10)
plot(arimafore)

#forecasted results
arimafore$mean

#zooming on the forecast
plot(arimafore, xlim=c(1930, 1944))

#ets for comparison
myets <- ets(lynx)
etsfore <- forecast(myets, h=10)

#comparing the two models
library(ggplot2)
autoplot(lynx) + forecast::autolayer(etsfore$mean, series='ETS Model') +
                 forecast::autolayer(arimafore$mean, series='ARIMA Model') +
        xlab('year') + ylab('Lynx Trappings') + guides(colour=guide_legend(title='Forecast Method')) +
        theme(legend.position = c(0.8,0.8))
##################### Adding explonotary (predictors) to ARIMA) ##################### 
setwd('C://Users/hgospodinov/Documents/Data Science Training/R Stuff/timeseries')
cyprinidae <- read.csv('cyprinidae.csv')
#do cyprinade (small fish) answer with a hormonal reaction to the presence of a predotary catfish

ggplot(cyprinidae, aes(y= concentration, x = X)) + geom_point() + aes(color=predator_presence)
#seems like there is a strong concentration between the two

x = ts(cyprinidae$concentration)
y = cyprinidae$predator_presence
mymodel <- auto.arima(x, xreg= y, stepwise = F, approximation = F)
#xreg is always explanatory variable
mymodel
#xreg will be added as soon as we have a TRUE in predatory presence g
checkresiduals(mymodel)

#forecasting
y1 = c(T,T,F,F,F,F,T,F,T,F)

plot(forecast(mymodel, xreg=y1))
#zooming in
plot(forecast(mymodel, xreg=y1),xlim=c(230,260))
