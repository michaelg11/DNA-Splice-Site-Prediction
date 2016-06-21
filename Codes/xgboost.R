install.packages("xgboost")
library(xgboost)
require(Matrix)
require(data.table)
require(caret)
require(dplyr)

load("features.Rd")
part<-createDataPartition(f$response,p=0.7,list=F,1)
train<-f[part,]
test<-f[-part,]
st<-strata(train,stratanames = "response",size=c(150,150),method = "srswr")
s<-getdata(train,st)
s<-select(s,-c(762:764))

features<-as.matrix(train[,-which(names(f)=='response')]) 
output<-as.matrix(train[,which(names(f)=='response')])
output<-as.factor(output)
levels(output)[levels(output)=="-1"] <- "0"
output <- as.numeric(levels(output))[output]

xg_base<-xgboost(data = features,label = output,objective="binary:logistic",eta=1,nthreads=2,nrounds = 100
                 , verbose = T, print.every.n = 50)

outcome_name<-'response'
feature_name<-setdiff(names(train),outcome_name)
var_imp<-xgb.importance(feature_name,model = xg_base)
best<-filter(var_imp,Gain>0.001)
lst<-best$Feature
  
feature_selec<-as.matrix(train[,which(names(train) %in% lst)])

x<-as.matrix(feature_selec)
param<-list("objective"="binary:logistic","eval_metric"="auc","num_class"=2,nrounds=50,nfold=10)
best.cv<-xgboost(data = feature_selec, label = output,objective="binary:logistic", eta=1,nthreads=2
                 ,nrounds = 1000, verbose = T, print.every.n = 500, params = param)

x_test<-as.matrix(test[,-which(names(test)=='response')])
y_test<-test[,which(names(test)=='response')]
pred<-predict(best.cv,x_test) 
valid<-rep("-1",length(y_test)) 
valid[pred[1:length(y_test)]>0.1]="1" 
table(valid,test[,which(names(test)=="response")]) 

sn<-subset(pred[,2],y_test=='-1')
sp<-subset(pred[,2],y_test=='1')
prf<-pr.curve(sp,sn,curve = T)
plot(prf)
