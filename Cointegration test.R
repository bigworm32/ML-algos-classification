#tests for cointegration between two assets using johansen in a particular order:
#1)Tests if both assets are same difference stationary
#2)Tests for Trend, Constant, and no trend or constant cointegration models
#3)if Trend of Constant model, tests the statistical signficance for these constants or trends added
#4)Tests if the cointegrating vector scaled with the data is stationary

#the output will either be 1 if cointegrated with user defined input and lag specifications, or 0 otherwise. The output vector is:

#1 = 1 is cointegration 0 is none
#2 = johansen test output
#3 = p value for const or trend from t-test (NA for none)
#4 = adjustment speed stk 1
#5 = adjustment speed stk 2
#6 = p value for adf.test on scaled cointegration vector
#7 = model type (trend, const, none)
#8 = residual of stk1
#9 = residual of stk2
#10 = cointegration vectors scaled with data
#11 = unit root test stk1 undifferenced
#12 = unit root test stk2 undifferenced
#13 = unit root stk1 I(1)
#14 = unit root stk2 I(1)


rm(list=ls())#start fresh
#install.packages(c('urca','tseries','quantmod'))#comment out once installed once
library(urca)
library(tseries)
library(quantmod)
options(scipen=999)

#data call with close price
getSymbols("XEL",periodicity="daily",from = '2014/01/01',to = '2016/01/01')#load XEL
getSymbols("CMS",periodicity="daily",from = '2014/01/01',to = '2016/01/01')#load CMS
XEL = as.vector(XEL[,4])
CMS = as.vector(CMS[,4])
ts.plot(cbind(XEL,CMS))#plot data

###########################################
#call functions for  cointegration testing
###########################################
#function to calculate the pvalue from t test of cointegration vector components (only for r = 1)
extract.cajo_beta <- function(cajo, orls) {
  alpha <- coef(orls$rlm)[1, ];
  resids <- resid(orls$rlm);
  N <- nrow(resids);
  sigma <- crossprod(resids) / N;
  # get standard errors and p-values
  beta <- orls$beta
  beta.se <- sqrt(diag(kronecker(solve(crossprod(cajo@RK[, -1])), solve(t(alpha) %*% solve(sigma) %*% alpha))));
  beta.se2 <- c(NA, beta.se);
  beta.t <- c(NA, beta[-1] / beta.se);
  names(beta.t)<- rownames(beta)
  beta.pval <- dt(beta.t, df= orls$rlm$df.residual)
  return(beta.pval);
}

trend.johan<- function(pair,min.crit,maxlag)
#johansen test with trend(trace statistic). user inputs min criticial value and the maxlag
{
  jo1<-ca.jo(pair, type = 'trace', ecdet="trend",K = maxlag,spec="transitory")#johansen test with lag held constant at 2
  sum.jo <- summary(jo1)
  resid.jo <- data.frame(jo1@R0)
  if(jo1@teststat[1] < jo1@cval[1,1] & jo1@teststat[2] >= jo1@cval[2,min.crit])#if test was passed with trend, test its significance
  {
    vecm1 <- cajorls(jo1,r=1)#restricted model on OLS form (to conduct inference on cointegration vector)
    x = extract.cajo_beta(jo1,vecm1)#get the pvalues
    p.val.trend = round(x[3],2)#pvalue of the trend component
    if(p.val.trend <= .05)#if the trend component is significant
    {
      alpha.1 = round(jo1@W[1,1],2)#speed of adjustment of stock 1
      alpha.2 = round(jo1@W[2,1],2)#speed of adjustment of stock 2
      scaled.coin.vec = pair[,1] + (pair[,2]*jo1@V[2,1])#scale the data to cointegration vector
      scale.coin.adf = adf.test(scaled.coin.vec,k = maxlag)#test for stationarity from the scaled data from cointegration vector
      pval.coin.vec = scale.coin.adf$p.value
      if(pval.coin.vec <= .05)#if the scaled cointegration vector is stationary
      {
        return.vector = c(1, sum.jo, p.val.trend, alpha.1, alpha.2, pval.coin.vec,'trend',resid.jo, data.frame(scaled.coin.vec))#return 1 for cointegration pass
        return(return.vector)
      }else{return(c(0,0))}
    }else{return(c(0,0))}
  }else{return(c(0,0))}
}

const.johan<- function(pair,min.crit,maxlag)
#johansen test with constant (trace statistic). user inputs min criticial value and the maxlag
{
  jo1<-ca.jo(pair, type = 'trace', ecdet="const",K = maxlag,spec="transitory")#johansen test with lag held constant at 2
  sum.jo <- summary(jo1)
  resid.jo <- data.frame(jo1@R0)
  if(jo1@teststat[1] < jo1@cval[1,1] & jo1@teststat[2] >= jo1@cval[2,min.crit])#if test was passed with trend, test its significance
  {
    vecm1 <- cajorls(jo1,r=1)#restricted model on OLS form (to conduct inference on cointegration vector)
    x = extract.cajo_beta(jo1,vecm1)#get the pvalues
    p.val.const = round(x[3],2)#pvalue of the const component
    if(p.val.const <= .05)#if the const component is significant
    {
      alpha.1 = round(jo1@W[1,1],2)#speed of adjustment of stock 1
      alpha.2 = round(jo1@W[2,1],2)#speed of adjustment of stock 2
      scaled.coin.vec = pair[,1] + (pair[,2]*jo1@V[2,1])#scale the data to cointegration vector
      scale.coin.adf = adf.test(scaled.coin.vec,k = maxlag)#test for stationarity from the scaled data from cointegration vector
      pval.coin.vec = scale.coin.adf$p.value
      if(pval.coin.vec <= .05)#if the scaled cointegration vector is stationary
      {
        return.vector = c(1, sum.jo, p.val.const, alpha.1, alpha.2, pval.coin.vec,'const',resid.jo, data.frame(scaled.coin.vec))#return 1 for cointegration pass
        return(return.vector)
      }else{return(c(0,0))}
    }else{return(c(0,0))}
  }else{return(c(0,0))}
}

none.johan<- function(pair,min.crit,maxlag)
  #johansen test with constant (trace statistic). user inputs min criticial value and the maxlag
{
  jo1<-ca.jo(pair, type = 'trace', ecdet="none",K = maxlag,spec="transitory")#johansen test with lag held constant at 2
  sum.jo <- summary(jo1)
  resid.jo <- data.frame(as.matrix(jo1@R0))
  if(jo1@teststat[1] < jo1@cval[1,1] & jo1@teststat[2] >= jo1@cval[2,min.crit])#if test was passed with trend, test its significance
  {
    alpha.1 = round(jo1@W[1,1],2)#speed of adjustment of stock 1
    alpha.2 = round(jo1@W[2,1],2)#speed of adjustment of stock 2
    scaled.coin.vec = pair[,1] + (pair[,2]*jo1@V[2,1])#scale the data to cointegration vector
    scale.coin.adf = adf.test(scaled.coin.vec,k = maxlag)#test for stationarity from the scaled data from cointegration vector
    pval.coin.vec = scale.coin.adf$p.value
    if(pval.coin.vec <= .05)#if the scaled cointegration vector is stationary
    {
      return.vector = c(1, sum.jo, NA, alpha.1, alpha.2, scale.coin.adf$p.value,'none', resid.jo, data.frame(scaled.coin.vec) )#return 1 for cointegration pass
      return(return.vector)
    }else{return(c(0,0))}
  }else{return(c(0,0))}
}


cointegration.test <- function(stk1,stk2,max.precent,maxlag)
{
  pair<- cbind(stk1,stk2)
  #thesevalues are used in the minimum critical values used in johansen
  if(max.percent == 90){min.crit = 1}
  if(max.percent == 95){min.crit = 2}
  if(max.percent == 99){min.crit = 3}
  #lags
  if(maxlag > 2){maxlag = maxlag}else{maxlag = 2}#johansen test requires minimum lag of 2
  ###Difference Stationary###
  stk1.ur = adf.test(stk1, k = maxlag)#output
  stk2.ur = adf.test(stk2, k = maxlag)#output
  if(stk1.ur$p.value > .10 & stk2.ur$p.value > .10)#if stock is not stationary
  {
    stk1.d.ur = adf.test(diff(stk1),k = maxlag)#output
    stk2.d.ur = adf.test(diff(stk2),k = maxlag)#output
    ##if both same difference stationary###
    if(stk1.d.ur$p.value <= .05 & stk2.d.ur$p.value <= .05)#both stationary after one difference I(1)
    {
      t.jo.out<- trend.johan(pair,min.crit,maxlag)#johansen trend test
      if(t.jo.out[1] != 0)#if cointegration was found with all the filter requirements
      {
        out<- c(t.jo.out, stk1.ur$p.value, stk2.ur$p.value, stk1.d.ur$p.value, stk2.d.ur$p.value)
        return(out)
      }else#cointegration wasnt found proceed with const johansen test
      { 
        c.jo.out<- const.johan(pair,min.crit,maxlag)#johansen test for const
        if(c.jo.out[1] != 0)#if cointegration found
        {
          out<- c(c.jo.out,stk1.ur$p.value, stk2.ur$p.value, stk1.d.ur$p.value, stk2.d.ur$p.value )
          return(out)
        }else#if cointegration wasnt found proceed with no constant
        {
          n.jo.out<-none.johan(pair,min.crit,maxlag)#johansen test no drift no trend
          if(n.jo.out[1] != 0)#no cointegration found
          {
            out<- c(n.jo.out, stk1.ur$p.value, stk2.ur$p.value, stk1.d.ur$p.value, stk2.d.ur$p.value)
            return(out)
          }else{return(c(0,0))}#cointegration wasnt found for all three models
        }
      }
    }else{return(c(0,0))}
  }else{return(c(0,0))}
}

maxlag = 5
max.percent = 90
coin<- cointegration.test(stk1=XEL,stk2=CMS,max.precent = max.percent, maxlag = maxlag)
#print the cointegrating vector scaled with data
ts.plot(unlist(coin[10]))
