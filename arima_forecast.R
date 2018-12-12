#must run entire code, function is called at the bottom. User can change the amount for each trade and the lookback period. 
#This code will use arima to forecast into the future one period.  Remember, depending on the data periods could be weekly, daily, etc...
#after the forecast is done, the code runs though a basic trading strategy based on those forecasts.
#rules: buy if not in position and forecast is higher
#       buy if short the stock but forecast is higher
#       hold if long the stock and forecast is higher
#       short if not in position and forecast is lower
#       short if long the stock and forecast is lower
#       hold if short the stock and forecast is lower
rm(list=ls())
#install.packages(c('forecast','lmtest','dynlm','useful','quantmod'))#After code is run once, comment this out
#call on libaries (if doesnt call you must install which is done in prior line)
library(lmtest)
library(forecast)
library(dynlm)
library(useful)
library(quantmod)
#download data
getSymbols("^GSPC",periodicity="daily",from = '2015/01/01')#load sp500
ticker = GSPC
ticker = data.frame(ticker[,4])

#function for arima backtest, called at the bottom
arima.backtest<-function(ticker,position,lookback){

###########################################################
#Forecast one period into the future and align next to data
###########################################################

for(i in lookback:nrow(ticker))#align one period forecast into thefuture with current time period. can change lookback period
{
  data = ticker[(i-lookback):i,1]#slice the data
  arma = auto.arima(data,trace=F,ic='aicc',stepwise = T)#perform arima
  armaf = forecast(arma,h=1)#forecast off arima
  if(i != nrow(ticker)){ticker[i+1,2] = armaf$mean[1]}#assign forecast into the future
  ticker[i,3] = armaf$mean[1]#assign next periods forecast to current time frame (for trade decision)
}

names(ticker)[2] = 'forecast'#change name

##################
#backtest strategy
##################
#initial settings
ticker['profit line'] = 0#start at 0 profit
profit = 0#running profit
win = 0#wins
trade = 0#total trades
p = 0#position long or short
for(i in lookback:nrow(ticker))
{
  #f = forecast, c = current, p = position (long or short)
  #f = ticker[i,3]
  c = ticker[i,1]
  f2 = ticker[i,3]#next period forecast aligned with current period ( for trade decision )
  #calculate first trades short or ong
  #long
  if(f2 > c && p == 0)
  {
    p = 1#initialize position to long
    entry_share = round(position/c)
    enter = entry_share * c
  }
  #short
  if(f2 < c && p == 0)
  {
    p = -1
    entry_share = round(position/c)
    enter = entry_share * c#initilize position to short
  }
  #if positions are open make trade decisions
  if(f2 >= c && p == 1) #forecast is higher, position is currently long
  {
    p = 1#do nothing
  }
  if(f2 < c && p == 1)#forecast is lower, current position is long.  exit position and short.
  {
    closed = entry_share * c#close position
    net = closed - enter#net amount
    profit = profit + net#update running profit
    ticker[i,4] = profit #store profit for profit line
    if(net > 0){win = win + 1}#update win counter
    trade = trade + 1 #update trade counter
    p=-1#update position for shorting
    entry_share = round(position/c)
    enter = entry_share * c#enter trade short
  }
  if(f2 <= c && p == -1)#forecast is lower than current, already short
  {
    p = -1#do nothing
  }
  if(f2 > c && p == -1)#forecast is higher, currently short
  {
    closed = entry_share * c#close price
    net = enter - closed#calculate profit from short position
    profit = profit + net#update running profit
    ticker[i,4] = profit #store profit for profit line
    if(net>0){win = win + 1}#add to wincounter
    trade = trade + 1
    p = 1#update position for long
    entry_share = round(position/c)
    enter = entry_share * c #enter trade long
  } 
}
#adjust data to align forecast and profit line
ticker[,4][ticker[,4]==0]<-NA
ticker[1,4] = 0
ticker[,4] = na.locf(ticker[,4])
ticker = na.omit(ticker)
#calculate error for forecast
e<- (ticker[,1] - ticker[,2])#errors
MSE<- mean(e^2)#total mean square error for entire period forecast
#caclculate trade stats
ratio = win / trade#win to loss ratio


#plots for profit line and forcast data
par(mfrow=c(2,1))
plot(ticker[,4],col = 'blue',type='l', main = 'profit line')#plot profit line
ts.plot(cbind(ticker[,1],ticker[,2]), main = 'forecast and actual')
cor=cor(ticker[,1],ticker[,2])
#returns in percent
return.sp500 = (ticker[nrow(ticker),1] - ticker[1,1]) / ticker[1,1]
return.strat =  profit / position
return(c(ratio,MSE,cor,profit,trade,return.sp500,return.strat))
}
#user inputs
position = 10000#position amount purchase amount
lookback = 150#training period for arima model
back.result<-arima.backtest(ticker,position,lookback)
#results
win.ratio=back.result[1]
MSE=back.result[2]
correlation=back.result[3]
net.profit = back.result[4]
trades = back.result[5]
return.strat = back.result[7]
return.sp500 = back.result[6]
out = rbind(paste('win ratio :',round(win.ratio,2)),
paste('MSE :',round(MSE,2)),
paste('corr:',round(correlation,2)),
paste('net prof:',round(net.profit,2)),
paste('trades:',trades),
paste('return strategy %:',round(100*return.strat,2)),
paste('return sp500 %:',round(100*return.sp500,2)))
out
