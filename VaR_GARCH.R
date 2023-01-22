library(tidyverse)
library(ggthemes)
library(forecast)
library(tseries)
library(gridExtra)
library(parallel)
library(rugarch)
# visulaization of each coin
# close price
price = read.csv('C:\\Users\\ASUS\\Downloads\\qa\\project\\price.csv' , header = T)
price1 = price$BTC
p1=qplot(x = 1:length(price1) , y = price1 , geom = 'line') + geom_line(color = 'darkblue') + 
  labs(x = 'BTC' , y = 'Close Price')

price2 = price$ETH
p2=qplot(x = 1:length(price2) , y = price2 , geom = 'line') + geom_line(color = 'darkblue') + 
  labs(x = 'ETH' , y = 'Close Price')

price3 = price$XRP
p3=qplot(x = 1:length(price3) , y = price3 , geom = 'line') + geom_line(color = 'darkblue') + 
  labs(x = 'XRP' , y = 'Close Price')

price4 = price$LTC
p4=qplot(x = 1:length(price4) , y = price4 , geom = 'line') + geom_line(color = 'darkblue') + 
  labs(x = 'LTC' , y = 'Close Price')

price5 = price$DOGE
p5=qplot(x = 1:length(price5) , y = price5 , geom = 'line') + geom_line(color = 'darkblue') + 
  labs(x = 'DOGE' , y = 'Close Price')

price6 = price$LINK
p6=qplot(x = 1:length(price6) , y = price6 , geom = 'line') + geom_line(color = 'darkblue') + 
  labs(x = 'LINK' , y = 'Close Price')

grid.arrange(p1 , p2 ,p3, p4,p5,p6, nrow=3,ncol=2)
# daily log return
crypoto = read.csv('C:\\Users\\ASUS\\Downloads\\qa\\project\\logreturn.csv' , header = T)
rets1 = crypoto$BTC
p1=qplot(x = 1:length(rets1) , y = rets1 , geom = 'line') + geom_line(color = 'darkblue') + 
  geom_hline(yintercept = mean(rets1) , color = 'red' , size = 1) + 
  labs(x = 'BTC' , y = 'Daily Returns')

rets2 = crypoto$ETH
p2=qplot(x = 1:length(rets2) , y = rets2 , geom = 'line') + geom_line(color = 'darkblue') + 
  geom_hline(yintercept = mean(rets2) , color = 'red' , size = 1) + 
  labs(x = 'ETH' , y = 'Daily Returns')

rets3 = crypoto$XRP
p3=qplot(x = 1:length(rets3) , y = rets3 , geom = 'line') + geom_line(color = 'darkblue') + 
  geom_hline(yintercept = mean(rets3) , color = 'red' , size = 1) + 
  labs(x = 'XRP' , y = 'Daily Returns')

rets4 = crypoto$LTC
p4=qplot(x = 1:length(rets4) , y = rets4 , geom = 'line') + geom_line(color = 'darkblue') + 
  geom_hline(yintercept = mean(rets4) , color = 'red' , size = 1) + 
  labs(x = 'LTC' , y = 'Daily Returns')

rets5 = crypoto$DOGE
p5=qplot(x = 1:length(rets5) , y = rets5 , geom = 'line') + geom_line(color = 'darkblue') + 
  geom_hline(yintercept = mean(rets5) , color = 'red' , size = 1) + 
  labs(x = 'DOGE' , y = 'Daily Returns')

rets6 = crypoto$LINK
p6=qplot(x = 1:length(rets6) , y = rets6 , geom = 'line') + geom_line(color = 'darkblue') + 
  geom_hline(yintercept = mean(rets6) , color = 'red' , size = 1) + 
  labs(x = 'LINK' , y = 'Daily Returns')

grid.arrange(p1 , p2 ,p3, p4,p5,p6, nrow=3,ncol=2)

#load data
stocks = read.csv('C:\\Users\\ASUS\\Downloads\\qa\\project\\portfolio.csv' , header = T)
rets = stocks$logreturn

# arima order
adf.test(rets)
model.arima = auto.arima(rets , max.order = c(5 , 0 ,5) , stationary = TRUE , trace = T , ic = 'aicc')
model.arima
checkresiduals(model.arima)


# find best distribution
model_train <- function(data_, m_name, m_dist) {
  infor_ratio <- NULL
  for (i in m_name) {
    sub_model <- NULL
    main_model <- i
    if (i == "TGARCH" | i == "NAGARCH") {
      sub_model <- i
      main_model <- "fGARCH"
    }
    
    for (j in m_dist) {
      model_name <- paste0('ARMA(1,1)', '-',
                           i, '-', j,
                           sep = "")
      if (main_model == "fGARCH") {
        model_name <- paste0('ARMA(1,1)', '-',
                             sub_model, '-', j,
                             sep = "")
      }
      print(model_name)

      mean.spec <- list(armaOrder = c(1, 1), include.mean = F,
                        archm = F, archpow = 1, arfima = F,
                        external.regressors = NULL)
      var.spec <- list(model = main_model, garchOrder = c(1, 1),
                       submodel = sub_model,
                       external.regressors = NULL,
                       variance.targeting = F)
      dist.spec <- j
      my_spec <- ugarchspec(mean.model = mean.spec,
                            variance.model = var.spec,
                            distribution.model = dist.spec)
 
      my_fit <- ugarchfit(data = data_, spec = my_spec)
      infor_ratio <- rbind(infor_ratio, c(list(ModelName = model_name),
                          list(LogL = likelihood(my_fit)),
                          list(AIC = infocriteria(my_fit)[1]),
                          list(BIC = infocriteria(my_fit)[2]),
                          list(RMSE = sqrt(mean(residuals(my_fit)^2))),
                          list(md = my_fit)))
    }
  }
  return(infor_ratio)
}


md_dist <- c("norm", "std", "sstd", "ged", "jsu")
infor_ratio <- model_train(rets, "sGARCH", md_dist)

fitdist(distribution = 'sstd' , x = rets)$pars

#find best GARCH model
model_test <- function(data_, m_name, m_dist) {
  for (i in m_name) {
    sub_model <- NULL
    main_model <- i
    if (i == "TGARCH" | i == "NAGARCH") {
      sub_model <- i
      main_model <- "fGARCH"
    }
    
    for (j in m_dist) {
      model_name <- paste0('ARMA(1,1)', '-',
                           i, '-', j,
                           sep = "")
      if (main_model == "fGARCH") {
        model_name <- paste0('ARMA(1,1)', '-',
                             sub_model, '-', j,
                             sep = "")
      }
      print(model_name)

      mean.spec <- list(armaOrder = c(1, 1), include.mean = F,
                        archm = F, archpow = 1, arfima = F,
                        external.regressors = NULL)
      var.spec <- list(model = main_model, garchOrder = c(1, 1),
                       submodel = sub_model,
                       external.regressors = NULL,
                       variance.targeting = F)
      dist.spec <- j
      my_spec <- ugarchspec(mean.model = mean.spec,
                            variance.model = var.spec,
                            distribution.model = dist.spec)
     
      my_fit <- ugarchfit(data = data_, spec = my_spec)
      model_roll <- ugarchroll(my_spec,data_,
                               n.start = 876, refit.every = 1, refit.window = "moving",
      )
      report(model_roll, type = "VaR",conf.level = 0.95,VaR.alpha=0.05)
      
    }
  }
}

md_name <- c("sGARCH", "eGARCH", "gjrGARCH","TGARCH", "NAGARCH")
model_test(rets, md_name, 'sstd')

# sgarch
mean.spec <- list(armaOrder = c(1, 1), include.mean = F,
                  archm = F, archpow = 1, arfima = F,
                  external.regressors = NULL)
var.spec <- list(model = "sGARCH", garchOrder = c(1, 1),
                 submodel = NULL,
                 external.regressors = NULL,
                 variance.targeting = F)
model.spec <- ugarchspec(mean.model = mean.spec,
                         variance.model = var.spec,
                         distribution.model = 'sstd')
model.fit = ugarchfit(spec = model.spec , data = rets , solver = 'solnp')
model_roll <- ugarchroll(model.spec, rets,
                         n.start = 876, refit.every = 1, refit.window = "moving",
)
report(model_roll, type = "VaR",conf.level = 0.95,VaR.alpha=0.05)
sigma_sgarch <- ugarchforecast(model.fit, n.ahead = 1)@forecast$sigmaFor
s = fitdist(distribution = 'sstd' , x = rets)$pars['shape']
VaR_sgarch <- mean(rets) + sigma_sgarch * qdist(distribution='sstd', shape=s, p=0.05)
# eGARCH
mean.spec <- list(armaOrder = c(1, 1), include.mean = F,
                  archm = F, archpow = 1, arfima = F,
                  external.regressors = NULL)
var.spec <- list(model = "eGARCH", garchOrder = c(1, 1),
                 submodel = NULL,
                 external.regressors = NULL,
                 variance.targeting = F)
model.spec <- ugarchspec(mean.model = mean.spec,
                         variance.model = var.spec,
                         distribution.model = 'sstd')
model.fit = ugarchfit(spec = model.spec , data = rets , solver = 'solnp')
model.roll = ugarchroll(spec = model.spec , data = rets , n.start = 876 , refit.every = 50 ,
                        refit.window = 'moving')
report(model.roll, type = "VaR",conf.level = 0.95,VaR.alpha=0.05)
sigma_egarch <- ugarchforecast(model.fit, n.ahead = 1)@forecast$sigmaFor
s = fitdist(distribution = 'sstd' , x = rets)$pars['shape']
VaR_egarch <- mean(rets) + sigma_egarch * qdist(distribution='sstd', shape=s, p=0.05)
# grjGARCH
mean.spec <- list(armaOrder = c(1, 1), include.mean = F,
                  archm = F, archpow = 1, arfima = F,
                  external.regressors = NULL)
var.spec <- list(model = "gjrGARCH", garchOrder = c(1, 1),
                 submodel = NULL,
                 external.regressors = NULL,
                 variance.targeting = F)
model.spec <- ugarchspec(mean.model = mean.spec,
                         variance.model = var.spec,
                         distribution.model = 'sstd')
model.fit = ugarchfit(spec = model.spec , data = rets , solver = 'solnp')
model.roll = ugarchroll(spec = model.spec , data = rets , n.start = 876 , refit.every = 50 ,
                        refit.window = 'moving')
report(model.roll, type = "VaR",conf.level = 0.95,VaR.alpha=0.05)
sigma_gjrgarch <- ugarchforecast(model.fit, n.ahead = 1)@forecast$sigmaFor
s = fitdist(distribution = 'sstd' , x = rets)$pars['shape']
VaR_gjrgarch <- mean(rets) + sigma_gjrgarch * qdist(distribution='sstd', shape=s, p=0.05)

# TGARCH
mean.spec <- list(armaOrder = c(1, 1), include.mean = F,
                  archm = F, archpow = 1, arfima = F,
                  external.regressors = NULL)
var.spec <- list(model = "fGARCH", garchOrder = c(1, 1),
                 submodel = "TGARCH",
                 external.regressors = NULL,
                 variance.targeting = F)
model.spec <- ugarchspec(mean.model = mean.spec,
                         variance.model = var.spec,
                         distribution.model = 'sstd')
model.fit = ugarchfit(spec = model.spec , data = rets , solver = 'solnp')
model.roll = ugarchroll(spec = model.spec , data = rets , n.start = 876 , refit.every = 50 ,
                        refit.window = 'moving')
report(model.roll, type = "VaR",conf.level = 0.95,VaR.alpha=0.05)
sigma_tgarch <- ugarchforecast(model.fit, n.ahead = 1)@forecast$sigmaFor
s = fitdist(distribution = 'sstd' , x = rets)$pars['shape']
VaR_tgarch <- mean(rets) + sigma_tgarch * qdist(distribution='sstd', shape=s, p=0.05)

# NAGARCH
mean.spec <- list(armaOrder = c(1, 1), include.mean = F,
                  archm = F, archpow = 1, arfima = F,
                  external.regressors = NULL)
var.spec <- list(model = "fGARCH", garchOrder = c(1, 1),
                 submodel = "NAGARCH",
                 external.regressors = NULL,
                 variance.targeting = F)
model.spec <- ugarchspec(mean.model = mean.spec,
                         variance.model = var.spec,
                         distribution.model = 'sstd')
model.fit = ugarchfit(spec = model.spec , data = rets , solver = 'solnp')
model.roll = ugarchroll(spec = model.spec , data = rets , n.start = 876 , refit.every = 50 ,
                        refit.window = 'moving')
report(model.roll, type = "VaR",conf.level = 0.95,VaR.alpha=0.05)
sigma_nagarch <- ugarchforecast(model.fit, n.ahead = 1)@forecast$sigmaFor
s = fitdist(distribution = 'sstd' , x = rets)$pars['shape']
VaR_nagarch <- mean(rets) + sigma_nagarch*qdist(distribution='sstd', shape=s, p=0.05)

# monte carlo simulation to find optimal weights
crypto = read.csv('C:\\Users\\ASUS\\Downloads\\qa\\project\\logreturn.csv' , header = T)
btc = crypto$BTC
eth	= crypto$ETH
xrp = crypto$XRP
ltc = crypto$LTC
doge = crypto$DOGE
link = crypto$LINK

mean.spec <- list(armaOrder = c(1, 1), include.mean = F,
                  archm = F, archpow = 1, arfima = F,
                  external.regressors = NULL)
var.spec <- list(model = "eGARCH", garchOrder = c(1, 1),
                 external.regressors = NULL,
                 variance.targeting = F)
model.spec <- ugarchspec(mean.model = mean.spec,
                         variance.model = var.spec,
                         distribution.model = 'sstd')


ratio <- 0
weight <- c(0,0,0,0,0,0)
for (i in 1: 100000){
  print(i)
  w = runif(6,0,1)
  real_w = w/sum(w)
  rets = real_w[1]*btc + real_w[2]*eth + real_w[3]*xrp + real_w[4]*ltc + real_w[5]*doge + real_w[6]*link
  model.fit = ugarchfit(spec = model.spec , data = rets , solver = 'solnp')
  sigma_f <- ugarchforecast(model.fit, n.ahead = 1)@forecast$sigmaFor
  s = fitdist(distribution = 'sstd' , x = rets)$pars['shape']
  VaR_f <- mean(rets) + sigma_f*qdist(distribution='sstd', shape=s, p=0.05)
  if (exp(cumsum(rets)/3)[1096]/VaR_f < ratio){
    ratio = exp(cumsum(rets)/3)[1096]/VaR_f
    weight = w
  }}
weight
