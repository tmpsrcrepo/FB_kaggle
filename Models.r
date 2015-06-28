agg_df<-read.csv('agg_df.csv')
summary(agg_df)
dim(agg_df)
#split into train and test
library(foreach)
train_df2=foreach(i = 1:nrow(train),.combine=rbind) %do% {
  line = agg_df[which(agg_df$Group.1==as.character(train[i,]$bidder_id)),]
  line
}

test_df2=foreach(i = 1:nrow(test),.combine=rbind) %do% {
  
  line = agg_df[which(agg_df$Group.1==as.character(test[i,]$bidder_id)),]
  if(nrow(line)==0) {
    a = agg_df[1,]
    a$Group.1=as.character(test[i,]$bidder_id)
    a$bidder_id =as.character(test[i,]$bidder_id)
    colnames(a)=colnames(agg_df)
    line =a
  }
  line
}


summary(train_avg_tmp)
write.table(train_avg_tmp,'train_df_num.csv',sep=',')
tmp_train<-read.csv('train_df1.csv')
summary(tmp_train)
tmp_train = tmp_train[,-11]
summary(tmp_train)

summary(submit)

summary(test_avg)
dim(test_avg)
#split 
positions <- sample(nrow(tmp_train),size=floor((nrow(train)/3)*2))

library(randomForest)
library(gbm)
library(pROC)
library(caret)

#model exploration
#feature set1:
rf1_<-randomForest(train_avg_tmp[positions,-10],as.factor(train_avg_tmp[positions,10]),ntree=5000,mtry=3)
pred_c=predict(rf1,train_avg_tmp[-positions,],'prob')
roc(train_avg_tmp[-positions,10],(pred_c[,2]))

gbm1<-gbm((outcome)~.,data=train_avg_tmp[positions,],n.trees=5000,interaction.depth=3,shrinkage=0.1,bag.fraction = 0.3)

pred_gbm=predict(gbm1,train_avg_tmp[-positions,],n.trees=5000,type="response")
roc(train_avg_tmp[-positions,10],(pred_gbm))

w=0.01
b=0.5
summary(train_avg_tmp)
roc(train_avg_tmp[-positions,10],w*(pred_gbm)+(1-w-b)*pred_c[,2]+b*((attr(svm1_1.pred_, "probabilities"))[,2]))
w=0.01
b=0.72
roc(train_avg_tmp[-positions,10],w*(pred_gbm)+(1-w-b)*pred_c[,2]+b*((attr(svm1_1.pred_, "probabilities"))[,2]))

roc(train_avg_tmp[-positions,10],pred_c[,2])

#ada boosting 
fitControl <- trainControl(number = 3,method = "cv",classProbs=TRUE,summaryFunction = twoClassSummary)
adaFit<-train(as.factor(outcome)~.,data=train_avg_tmp,method='ada',trControl=fitControl,preProcess=c('scale'),tuneLength=8,metric='ROC')
adaFit$final
ada.pred_ <- predict(adaFit$final, train_avg_tmp[-positions,],'prob')
roc(train_avg_tmp[-positions,10],ada.pred_[,2])

library(e1071)
fitControl <- trainControl(number = 5,method = "cv",classProbs=TRUE,summaryFunction = twoClassSummary)
svmFit<-train(as.factor(outcome)~.,data=train_avg_tmp,method='svmRadial',trControl=fitControl,
              preProcess=c('center','scale'),tuneLength=8,metric='ROC')
svm1_1 <- svm(factor(outcome)~., data=train_avg_tmp[positions,], kernel='radial', sigma=1.213097,cost = 2,probability=TRUE)
print(svm1_1)
svm1_1.pred_ <- predict(svm1_1, train_avg_tmp[-positions,],probability=TRUE)
roc(train_avg_tmp[-positions,10],(attr(svm1_1.pred_, "probabilities"))[,2])

#feature set 2:
summary(train_df2)
train_df2_=train_df2[,c(3,5:13)]
train_df2_$outcome = train_avg_tmp[,10]
dim(train_df2_)
test_df2_ =test_df2[,c(3,5:13)]

positions <- sample(nrow(tmp_train),size=floor((nrow(train)/3)*2))
rf2_<-randomForest(train_df2_[positions,-11],as.factor(train_df2_[positions,11]),ntree=5000,mtry=2)
pred_c2=predict(rf2_,train_df2_[-positions,],'prob')
roc(train_df2_[-positions,11],(pred_c2[,2]))

fitControl <- trainControl(number = 5,method = "cv",classProbs=TRUE,summaryFunction = twoClassSummary)
svmFit2<-train(as.factor(outcome)~.,data=train_df2_,method='svmRadial',trControl=fitControl,
              preProcess=c('center','scale'),tuneLength=8,metric='ROC')




#train on the full data set
rf_full<-randomForest(train_avg_tmp[,-10],as.factor(train_avg_tmp[,10]),ntree=5000,mtry=3)
rf2_full<-randomForest(train_df2_[,-11],as.factor(train_df2_[,11]),ntree=5000,mtry=2)
gbm_full<-gbm((outcome)~.,data=train_avg_tmp,n.trees=5000,interaction.depth=3,shrinkage=0.1,bag.fraction = 0.1)
svm_full <- svm(factor(outcome)~., data=train_avg_tmp, kernel='radial', sigma=1.213097,cost = 2,probability=TRUE)
adaFit$final



pred_rf=predict(rf_full,test_avg,'prob')
pred_rf2=predict(rf2_full,test_df2_,'prob')
pred_gb=predict(gbm_full,test_avg,n.trees=5000,type="response")
pred_svm=predict(svm_full,test_avg,probability=TRUE)
pred_svm_prob=((attr(pred_svm,"probabilities"))[,2])
pred_ada_prob =predict(adaFit$final,test_avg,'prob')


w=0.01 
b=0.005

w=0.01
b=0.7
submit$prediction=0.01*pred_ada_prob[,2]+w*pred_rf2[,2]+b*(pred_svm_prob)+(1-w-b-0.01)*pred_rf[,2]



w_list=c(0.15,0.2,0.3)
b_list=c(0.6,0.62,0.65,0.67)

start=1
end=1
lis=rep(0,(length(w_list)*length(b_list)))

#4-fold cross validation
foreach(i =c(1:4)) %do% {
  if(i<4){
    end = as.integer(start+nrow(train_avg_tmp)/4)
    
  }
  else{
    end =nrow(train_avg_tmp)
  }
  
  index = 1
  foreach(j = w_list) %do% {
    foreach(k = b_list) %do% {
      pred_rf=predict(rf_full,train_avg_tmp[start:end,],'prob')
      pred_rf2=predict(rf2_full,train_df2_[start:end,],'prob')
      pred_svm=predict(svm_full,train_avg_tmp[start:end,],probability=TRUE)
      pred_svm_prob=((attr(pred_svm,"probabilities"))[,2])
      pred_ada_prob =predict(adaFit$final,train_avg_tmp[start:end,],'prob')
      prediction=0.02*pred_ada_prob[,2]+j*pred_rf2[,2]+k*(pred_svm_prob)+(1-j-k-0.02)*pred_rf[,2]
      roc_=auc(train_avg_tmp[start:end,10],prediction)
      if(i==1){
        lis[index]=roc_
      }
      else{
        lis[index]=(lis[index]+roc_)
      }
      index=index+1
    }
  }
  start = end +1

  
}

lis/4
w=0.1
b=0.7
submit$prediction=0.02*pred_ada_prob[,2]+0.3*pred_rf2[,2]+0.65*(pred_svm_prob)+(1-w-b-0.02)*pred_rf[,2]






write.csv(submit,'ensemble__0.02_0.3_0.65.csv')


library(doParallel)
cluster=makeCluster(3)
registerDoParallel(cluster)

stopCluster(cluster)

#parameter tuning
gbmGrid <-  expand.grid(interaction.depth = c(1,2,3),n.trees = c(2000,3000,5000),shrinkage = c(0.01,0.1,0.5),
                        n.minobsinnode = c(1,5,10))

gbmFit1 <- train(as.factor(outcome) ~ ., data = train_avg_tmp[positions,],method = "gbm",trControl = fitControl,
                 preprocess=c('scale'),verbose = FALSE)




gbm2<-gbm(factor(outcome)~.,data=train_avg_tmp[positions,],ntrees=1000)
summary(gbm2,n.trees=5000)
summary(gbm3,n.trees=5000)
best.iter <- gbm.perf(gbm3,method="cv")
print(best.iter)


