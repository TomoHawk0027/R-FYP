data=na.omit(car_price)
#View(data)
summary(data)

class(data)
nrow(data)

b2=c(data$price)
len=length(b2)
mean2=sum(b2[1]:b2[len])/len
median2=median(b2[1]:b2[len])
stdDev2=sd(b2[1]:b2[len])
Var2=var(b2[1]:b2[len])

mean2
median2
stdDev2
Var2

mode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
mode2=mode(b2[1]:b2[len])

length(b2)
range(b2[1]:b2[len])
range2=max(b2[1]:b2[len])-min(b2[1]:b2[len])

CentralMeasures2 <- data.frame(mean2,median2,mode2)
VariationalMeasures2 <-data.frame(stdDev2,Var2,range2)

table(b2)

q=quantile(b2)
ql=q[2];qu=q[4];iqr=qu-ql # upper, lower quartile and interquartile range
lw=ql-1.5*iqr;uw=qu+1.5*iqr # lower and upper whisker
out=c()
for(j in 1:length(b2)){
  if(b2[j]<lw|b2[j]>uw){o=b2[j]}else{o=NULL} # empty vector 'out', gets populated with values that do not meet
  # the criteria in the logic statement
  out=c(out,o)
}
out

boxplot(b2)
class(out)
length(out)

#########################################################
#Train the model using 80% and suggest an appropriate GLM

data=na.omit(car_price)

price=data$price;car_age=data$age;mileage=data$odometres #integer output, poisson model
dataset=data.frame(price,car_age,mileage)
#set.seed(1453)
n=nrow(dataset)
indexes = sample(n,n*(80/100))
trainset = dataset[indexes,]
testset = dataset[-indexes,]

trainset.glm <- glm(price ~.,trainset, family="poisson") 
res=predict(trainset.glm,testset, type="response")



###########################################################
fit=glm(price~car_age+mileage,data = data,family = "poisson")
summary(fit)
#significant variables

#parameter estimates
coef(fit)
#( (Intercept)            car_age            mileage 
#-1.798208e+02  9.433481e-02  4.014825e-07
#all are significant.

#q2c predict test data using model

newdata=c(20000,3,2312) 
thati=coef(fit)*newdata
predictedvalue=sum(thati) #price value given betas and xi's
predictedvalue
#this predicted value is way off

#messy
# accuracyP=mean(predictedvalue==testset[,3])
# pred=predict(trainset.glm,testset, type="response")
# predictedvalues=rep(0,nrow(testset))
# predictedvalues[pred>0.5]=1 ; predictedvalues[pred<0.5]=0
# 
# #ind=grep("homekick", colnames(data))
# 
# #q2d confusion matrix and prob of correct predictions
# mc=1000; acc=0
# for (i in 1:mc) {
#   n=nrow(dataset)
#   indexes = sample(n,n*(80/100))
#   trainset = dataset[indexes,]
#   testset = dataset[-indexes,]
#   
#   trainset.glm <- glm(y ~.,trainset, family="binomial") 
#   pred=predict(trainset.glm,testset, type="response")
#   
#   predictedvalues=rep(0,nrow(testset))
#   predictedvalues[pred>0.5]=1 ; predictedvalues[pred<0.5]=0  # probability of Homekick being 1, if p<0.5 then homekick=0
#   actual=testset[,4]
#   df=data.frame(actual,predictedvalues)
#   
#   confusion_matrix=table(predictedvalues, actualvalues=testset[,4])
#   accuracy=mean(predictedvalues==testset[,4]) 
#   acc=acc+(1/mc)*accuracy
# }
# acc
# confusion_matrix=table(predictedvalues, actualvalues=testset[,4])
# confusion_matrix

############################################################
data.lda=data
price=data$price;car_age=data$age;mileage=data$odometres #integer output, poisson model
library(MASS)
data.lda = lda(price ~ car_age+mileage, data=data)
data.lda
#############################################################
#2 Apply PCA
datasetPCA=cbind(car_age,mileage)
dataset_NoNull=na.omit(datasetPCA)
fit <- princomp(na.omit(datasetPCA),cor=TRUE)
summary(fit)

loadings(fit)
plot(fit,type="lines") #scree plot
################################################################
library(CCA)
X=cbind(car_age,mileage) 

cor(X,price)
##############################################################
# k.means.fit <- kmeans(dataset_NoNull,4)
# attributes(k.means.fit)
# 
# # Centroids(arithmetic mean)
# k.means.fit$centers
# # Cluster size:
# k.means.fit$size
# 
# var=ncol(dataset_NoNull)
# 
# wssplot <- function(dataset_NoNull, nc=10, seed=1234){
#   wss <- (nrow(dataset_NoNull)-1)*sum(apply(dataset_NoNull,2,var))
#   for (i in 2:nc){
#     set.seed(seed)
#     wss[i] <- sum(kmeans(dataset_NoNull, centers=i)$withinss)}
#   plot(1:nc, wss, type="b", xlab="Number of Clusters",
#        ylab="Within groups sum of squares")}
# 
# wssplot(dataset_NoNull, nc=10) 