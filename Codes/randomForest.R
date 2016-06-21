install.packages("varSelRF")
install.packages("DMwR")
install.packages("PRROC")
install.packages("sampling")
library(varSelRF)
library(DMwR)
library(ROCR)
library(h2o)
library(PRROC)
library(sampling)
require(caret)
require(randomForest)
require(doMC)
require(pROC)
registerDoMC(cores = 2)
set.seed(1234)
h2o.init(nthreads = -1)

load("features.Rd")
features.hex<-as.h2o(f)

outcome_name<-'response'
feature_names<-setdiff(names(features.hex),outcome_name)

rf_selection<-h2o.randomForest(x=feature_names,y=outcome_name,training_frame = features.hex, 
                     ignore_const_cols = T, mtries=-1, nfolds = 10, score_each_iteration = F,
                     max_runtime_secs = 1000, stopping_metric = "AUC")
importance<-h2o.varimp(rf_selection)
selected<-subset(importance, scaled_importance > 0.20)
selected<-selected[,1]

feat_filtered<-f[,which(names(f) %in% selected)]
class<-f[,which(names(f)=="response")]

selec<-varSelRF(xdata=feat_filtered,Class=class,c.sd=0,ntree=100,vars.drop.frac = 0.2,whole.range = F,
                recompute.var.imp = F,verbose = T)

lst<-selec$selected.vars
feat<-feat_filtered[,which(names(feat_filtered) %in% lst)]
feat_selec<-cbind(feat,class)

part<-createDataPartition(feat_selec$class,p=0.7,list=F,1)
train<-feat_selec[part,]
test<-feat_selec[-part,]
prop.table(table(train$class))

control<-trainControl(method="cv",number = 10,allowParallel = T)
x<-train[,-which(names(train)=="class")]
y<-train[,which(names(train)=="class")]
x_test<-test[,-which(names(test)=="class")]
y_test<-test[,which(names(test)=="class")]

rf_model<-randomForest(x,y,importance=T,replace = T,ntree = 5000)

pred<-predict(rf_model,x_test,type="prob")
valid<-rep("-1",length(y_test))
valid[pred[,1]<0.9]="1"
table(valid,test[,which(names(test)=="class")])

sn<-subset(pred[,2],y_test=='-1')
sp<-subset(pred[,2],y_test=='1')
prf<-pr.curve(sp,sn,curve = T)
plot(prf)

train_smote<-SMOTE(class~.,train,perc.over = 900,perc.under = 120)
prop.table(table(train_smote$class))
x_smote<-train_smote[,-which(names(train_smote)=="class")]
y_smote<-train_smote[,which(names(train_smote)=="class")]

rf_smote<-randomForest(x_smote,y_smote,importance=T,replace = T,ntree = 5000)
pred_smote<-predict(rf_smote,x_test,type="prob")
valid<-rep("-1",length(y_test))
valid[pred_smote[,1]<0.6]="1"
table(valid,test[,which(names(test)=="class")])

sn<-subset(pred_smote[,2],y_test=='-1')
sp<-subset(pred_smote[,2],y_test=='1')
prf<-pr.curve(sp,sn,curve = T)
plot(prf)
