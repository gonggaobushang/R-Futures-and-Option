setwd("E:\\许捷\\R 工作\\亚式期权")

# RQuantLib package
library(RQuantLib)

AsianOption("geometric", "put", underlying=80, strike=85, div=-0.03,
            riskFree=0.05, maturity=0.25, vol=0.2)
AsianOption("arithmetic", "put", underlying=80, strike=85, div=-0.03,
            riskFree=0.05, maturity=0.25, vol=0.2)

# 算术平均只返回价格
# first,length,fixings 只适用于算数平均 
# 开始时间 时间长度 除数权重


# 画图
# y=value,delta,gamma,vega,theta,rho,divRho
# x=underlying,strike
Asianplot <- function(averageType, type, underlying, strike,dividendYield, riskFreeRate, maturity,volatility,
                      first,length,fixings,
                      x,y
){
 
  if ("first" %in% ls() == TRUE){ first =0}
  if ("length" %in% ls()== TRUE){ length = 11.0/12.0}
  if ("fixings" %in% ls() == TRUE){ fixings = 26}

  #x="delta"
  
  if(averageType == "arithmetic"){
    y="value"
  }
    
    if ( x == "underlying" ){
      asian_x <-  c(1:(2*underlying))
      y2 <-c()
      for (i in 1:(2*underlying)){
        eval(parse(text = paste0(
          "asian_y <-  AsianOption(averageType=averageType,type=type,underlying=",i,",strike=strike,",
          "dividendYield=dividendYield,riskFreeRate=riskFreeRate,maturity=maturity,volatility=volatility,",
          "first=first,length=length,fixings=fixings)$",y
        )))
        y2 <- rbind(y2,asian_y)
      }
    }else if ( x == "strike" ){
      asian_x <-  c(1:(2*strike))
      y2 <- c()
      for (i in 1:(2*strike)){
        eval(parse(text = paste0(
          "asian_y <-  AsianOption(averageType=averageType,type=type,underlying=underlying,strike=",i,",",
          "dividendYield=dividendYield,riskFreeRate=riskFreeRate,maturity=maturity,volatility=volatility,",
          "first=first,length=length,fixings=fixings)$",y
        )))
        y2 <- rbind(y2,asian_y)
      }
    }else if ( x == "dividendYield" ){
      asian_x <-  seq((-2*abs(dividendYield)),(2*abs(dividendYield)),0.01)
      y2 <- c()
      for (i in seq((-2*abs(dividendYield)),(2*abs(dividendYield)),0.01) ){
        eval(parse(text = paste0(
          "asian_y <-  AsianOption(averageType=averageType,type=type,underlying=underlying,strike=strike,",
          "dividendYield=",i,",riskFreeRate=riskFreeRate,maturity=maturity,volatility=volatility,",
          "first=first,length=length,fixings=fixings)$",y
        )))
        y2 <- rbind(y2,asian_y)
      }
    }else if ( x == "riskFreeRate" ){
      asian_x <-  seq((-2*abs(riskFreeRate)),(2*abs(riskFreeRate)),0.01)
      y2 <-c()
      for (i in seq((-2*abs(riskFreeRate)),(2*abs(riskFreeRate)),0.01) ){
        eval(parse(text = paste0(
          "asian_y <-  AsianOption(averageType=averageType,type=type,underlying=underlying,strike=strike,",
          "dividendYield=dividendYield,riskFreeRate=",i,",maturity=maturity,volatility=volatility,",
          "first=first,length=length,fixings=fixings)$",y
        )))
        y2 <- rbind(y2,asian_y)
      }
    }else if ( x == "maturity" ){
      asian_x <- seq(1/365,2*maturity,1/365)
      y2 <- c()
      for (i in seq(1/365,2*maturity,1/365) ){
        eval(parse(text = paste0(
          "asian_y <-  AsianOption(averageType=averageType,type=type,underlying=underlying,strike=strike,",
          "dividendYield=dividendYield,riskFreeRate=riskFreeRate,maturity=",i,",volatility=volatility,",
          "first=first,length=length,fixings=fixings)$",y
        )))
        y2 <- rbind(y2,asian_y)
      }
    }else if ( x == "volatility" ){
      asian_x <- seq(0.01,2*volatility,0.01)
      y2 <- c()
      for (i in seq(0.01,2*volatility,0.01) ){
        eval(parse(text = paste0(
          "asian_y<-  AsianOption(averageType=averageType,type=type,underlying=underlying,strike=strike,",
          "dividendYield=dividendYield,riskFreeRate=riskFreeRate,maturity=maturity,volatility=",i,",",
          "first=first,length=length,fixings=fixings)$",y
        )))
        y2 <- rbind(y2,asian_y)
      }
    }
    
  

  
  
  
  
  plot(x=asian_x,y=y2,type="l",xlab = x,ylab = y)
  
  
  
  
}

Asianplot("geometric", "put", underlying=80, strike=85, dividendYield=-0.03,
          riskFreeRate=0.05, maturity=0.25, volatility=0.2,y="divRho",x="underlying")

Asianplot("geometric", "put", underlying=80, strike=85, dividendYield=-0.03,
          riskFreeRate=0.05, maturity=0.25, volatility=0.2,y="theta",x="strike")

Asianplot("geometric", "put", underlying=80, strike=85, dividendYield=-0.03,
          riskFreeRate=0.05, maturity=0.25, volatility=0.2,y="value",x="dividendYield")

Asianplot("geometric", "put", underlying=80, strike=85, dividendYield=-0.03,
          riskFreeRate=0.05, maturity=0.25, volatility=0.2,y="delta",x="riskFreeRate")

Asianplot("geometric", "put", underlying=80, strike=85, dividendYield=-0.03,
          riskFreeRate=0.05, maturity=0.25, volatility=0.2,y="value",x="maturity")

Asianplot("geometric", "put", underlying=80, strike=85, dividendYield=-0.03,
          riskFreeRate=0.05, maturity=0.25, volatility=0.2,y="vega",x="volatility")


Asianplot("arithmetic", "put", underlying=80, strike=85, dividendYield=-0.03,
          riskFreeRate=0.05, maturity=0.25, volatility=0.2,y="gamma",x="strike")

Asianplot("arithmetic", "put", underlying=80, strike=85, dividendYield=-0.03,
          riskFreeRate=0.05, maturity=0.25, volatility=0.2,y="gamma",x="volatility")
