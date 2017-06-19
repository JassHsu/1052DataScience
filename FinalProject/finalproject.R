library(quantmod)


# load APPLE data
stock<-get(getSymbols("IBM"))

# set the date
date<-"2012-03-30/2013-12-31"


#apply the starpoints function to pick the first trading day
startpoints<- function(x,on="days",k=1){
  head(endpoints(x,on,k)+1,-1)
}

# create the profit function
profit<-function(per,sigma){

bb<-BBands(Cl(stock),n=per,sd=sigma)

bb.up<-bb$up
bb.dn<-bb$dn

m<-merge(Cl(stock),BBands(Cl(stock),maType="SMA"))
m$sig[with(m,stock[,4]>bb.up)&index(m) %in% index(m)[startpoints(m,on="days")]]<-1
m$sig[with(m,stock[,4]<bb.dn)&index(m) %in% index(m)[startpoints(m,on="days")]]<-0
position<-na.locf(m)

return<-ROC(Cl(stock))*position$sig
return<-return[date]
return<-exp(cumsum(return))


return(return)
}


