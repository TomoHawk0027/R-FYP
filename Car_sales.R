dataset=na.omit(train)
#View(dataset)
summary(dataset)

class(dataset)
nrow(dataset)

b=c(train$offer_request)
mean=mean(b)
median=median(b)
stdDev=sd(b)
Var=var(b)

mean
median
stdDev
Var

mode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
mode=mode(b)

length(b)
range(b)
range=max(b)-min(b)

CentralMeasures <- data.frame(mean,median,mode)
VariationalMeasures <-data.frame(stdDev,Var,range)