# L-1 regression

install.packages("glmnet")
library(glmnet)
install.packages("caTools")
library(caTools)
load('features.Rd')
sample = sample.split(f, SplitRatio = 0.7)
train = subset(f, sample == TRUE)
test =( subset(f, sample == FALSE))

x=as.matrix(trainx[,-which(colnames(f)=="response")])
y=as.matrix(trainx[,which(colnames(f)=="response")])
xt=as.matrix(test[,-which(colnames(f)=="response")])
yt=as.matrix(test[,which(colnames(f)=="response")])
fit<-glmnet(x,y,family = 'binomial')
plot(fit)
plot(fit, xvar = "dev", label = TRUE)
cvfit = cv.glmnet(x, y, family = "binomial",nfolds = 5, type.measure = "auc")
plot(cvfit)
test.res=predict(cvfit,xt,type='response')
testpred=rep("-1",length(test[,1]))
testpred[test.res>0.5]="1"
table(testpred,test[,757])
roc(yt,test.res,plot=T)

sn<-subset(test.res[,2],y_test=='-1')
sp<-subset(test.res[,2],y_test=='1')
prf<-pr.curve(sp,sn,curve = T)
plot(prf)


#one class SVM
library(e1071)
df=subset(train,response=="1")
dx=subset(trainx,select = -response)
dy=trainx$response
dx <- df[,!names(df) %in% c("response","V423","V433","V434","V435","V436","V437","V438","V439","V440")]
svmfit=svm(dx,dy,type = "one-classification")
svm.probs=predict(svmfit,newdata=test,type='response')
svm.pred=rep("-1",length(test$V2))
svm.pred[svm.probs>0]="1"
table(svm.probs,test$response)
mean(svm.pred!=Test.impensData$V2)
roc(Test$V2,svm.probs,plot = T)

model<-svm(x=dx,y=dy,cross=3,probability=T)

sn<-subset(svm.probs[,2],y_test=='-1')
sp<-subset(svm.probs[,2],y_test=='1')
prf<-pr.curve(sp,sn,curve = T)
plot(prf)


