library(quantmod)


# load APPLE data
getSymbols("AAPL")





startpoints<- function(x,on="days",k=1){
  head(endpoints(x,on,k)+1,-1)
}


profit<-function(per,sigma){


bb<-BBands(Cl(AAPL),n=per,sd=sigma)


bb.up<-bb$up
bb.dn<-bb$dn

m<-merge(Cl(AAPL),BBands(Cl(AAPL),maType="SMA"))
m$sig[with(m,AAPL.Close>up)&index(m) %in% index(m)[startpoints(m,on="days")]]<-0
m$sig[with(m,AAPL.Close<up)&index(m) %in% index(m)[startpoints(m,on="days")]]<-1
#m$sig[1:19]<-1
position<-na.locf(m)

return<-ROC(Cl(AAPL))*position$sig
return<-return["2012-03-30/2013-12-31"]
return<-exp(cumsum(return))


return(return)
}


plot(return)
