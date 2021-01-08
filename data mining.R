#데이터처리 및 라이브러리 생성

library(AUC)
library(ROCR)
library(e1071)
library(randomForest)
library(gbm)
library(MASS)
library(tree)
library(dplyr)
library(leaps)

wine = read.csv('winequality-white.csv',header=T)
wine2=wine #wine은 원본으로 남겨두고 wine2,wine3으로
wine3=wine
wine2$quality <- as.factor(ifelse(wine2$quality>=6,'Good','Bad'))
wine3$quality <- as.numeric(ifelse(wine3$quality>=6,1,0))
table(wine2$quality)
table(wine3$quality)

#자료를 k등분해주는 함수 생성
kfold= function(data,k){
  valid=list()
  dat1 = 1:nrow(data); datt1= 1:nrow(data)
  for (i in 1:k){
    valid[[i]]= sample(datt1,length(dat1)/k,replace=F)
    datt1=setdiff(datt1,valid[[i]])
  } #이렇게 10등분하면 4880개가 10개로 분할 남은 8개자료도 랜덤분할해야함
  yy=sample(1:length(valid),length(datt1),replace=F) 
  for (i in 1:length(datt1)){
    valid[[yy[i]]]=c(valid[[yy[i]]],datt1[i])
  }
  valid
}
set.seed(777)
cv_list = kfold(wine2,10)
cv_list


##cv_train set, cv_valid set
for(i in 1:length(cv_list)) {
  valid_index <- cv_list[[i]]
  
  # K-fold 에서의 test 데이터
  cv_valid_set <- wine2[valid_index,]
  
  # K-fold 에서의 train 데이터
  cv_train_set <- wine2[-valid_index,]
}


#로지스틱회귀
cv_logistic= function(cv_list){
  flash=list()
  for(i in 1:length(cv_list)) {
    valid_index <- cv_list[[i]]
    
    # K-fold 에서의 test 데이터
    cv_valid_set <- wine2[valid_index,]
    
    # K-fold 에서의 train 데이터
    cv_train_set <- wine2[-valid_index,]
    
    # 로지스틱 모델 생성
    model.fit <- glm(quality~., data = cv_train_set,family=binomial(link='logit'))
    
    # predict 
    model.pred <- predict(model.fit, newdata = cv_valid_set, type = "response")
    model.pred[model.pred>0.5] = 'Good'
    model.pred[model.pred<=0.5] = 'Bad'
    # model acurracy 생성
    model.table=table(model.pred,cv_valid_set$quality)
    flash[[i]]= c(sum(cv_valid_set$quality == model.pred)/nrow(cv_valid_set),model.table[2,2]/sum(model.table[,2]),model.table[1,1]/sum(model.table[,1]))
  }
  flash
  mean(unlist(flash)[c(seq(1,length(unlist(flash)),3))]) #10 fold cv값의 분류율 평균
}

Logistic=cv_logistic(cv_list) #0.748745


#lda
cv_lda= function(cv_list){
  flash=list()
  for(i in 1:length(cv_list)) {
    valid_index <- cv_list[[i]]
    
    # K-fold 에서의 test 데이터
    cv_valid_set <- wine2[valid_index,]
    
    # K-fold 에서의 train 데이터
    cv_train_set <- wine2[-valid_index,]
    
    # lda 모델 생성
    model.fit <- lda(quality~., data = cv_train_set)
    
    # predict 
    model.pred <- predict(model.fit, newdata = cv_valid_set, type = "class")
    
    # model acurracy 생성
    model.table=table(model.pred$class,cv_valid_set$quality)
    flash[[i]]= c(sum(cv_valid_set$quality == model.pred$class)/nrow(cv_valid_set),model.table[2,2]/sum(model.table[,2]),model.table[1,1]/sum(model.table[,1]))
  }
  
  flash
  mean(unlist(flash)[c(seq(1,length(unlist(flash)),3))]) #10 fold cv값의 분류율 평균
}

lda=cv_lda(cv_list)#0.7499028
lda
#qda

cv_qda = function(cv_list){
  flash=list()
  for(i in 1:length(cv_list)) {
    valid_index <- cv_list[[i]]
    
    # K-fold 에서의 test 데이터
    cv_valid_set <- wine2[valid_index,]
    
    # K-fold 에서의 train 데이터
    cv_train_set <- wine2[-valid_index,]
    
    # qda 모델 생성
    model.fit <- qda(quality~., data = cv_train_set)
    
    # predict 
    model.pred <- predict(model.fit, newdata = cv_valid_set, type = "class")
    
    # model acurracy 생성 
    model.table=table(model.pred$class,cv_valid_set$quality)
    flash[[i]]= c(sum(cv_valid_set$quality == model.pred$class)/nrow(cv_valid_set),model.table[2,2]/sum(model.table[,2]),model.table[1,1]/sum(model.table[,1]))
  }
  
  flash
  mean(unlist(flash)[c(seq(1,length(unlist(flash)),3))]) #10 fold cv값의 분류율 평균
}

qda= cv_qda(cv_list) #0.7456166
qda 


#tree
cv_tree = function(cv_list){
  flash=list()
  for(i in 1:length(cv_list)) {
    valid_index <- cv_list[[i]]
    
    # K-fold 에서의 test 데이터
    cv_valid_set <- wine2[valid_index,]
    
    # K-fold 에서의 train 데이터
    cv_train_set <- wine2[-valid_index,]
    
    # Decision Tree 모델 생성
    model.fit <- tree(quality~., data = cv_train_set)
    
    # predict 
    tree.pred <- predict(model.fit, newdata = cv_valid_set, type = "class")
    
    # model acurracy 생성
    tree.table=table(tree.pred,cv_valid_set$quality)
    flash[[i]]= c(sum(cv_valid_set$quality == tree.pred)/nrow(cv_valid_set),tree.table[2,2]/sum(tree.table[,2]),tree.table[1,1]/sum(tree.table[,1]))
  }
  flash
  mean(unlist(flash)[c(seq(1,length(unlist(flash)),3))]) #10 fold cv값의 분류율 평균
}
tree= cv_tree(cv_list)
tree #0.7339744

# Tree 시각화

model.fit <- tree(quality~., data = cv_train_set)
model.pred <- predict(model.fit, newdata = cv_valid_set, type = "class")
plot(model.fit)
text(model.fit, pretty=0, cex=0.8)



#randomForest
cv_randomForest = function(cv_list){
  flash=list()
  for(i in 1:length(cv_list)) {
    valid_index <- cv_list[[i]]
    
    # K-fold 에서의 test 데이터
    cv_valid_set <- wine2[valid_index,]
    
    # K-fold 에서의 train 데이터
    cv_train_set <- wine2[-valid_index,]
    
    # randomForest 모델 생성
    model.fit <- randomForest(quality~., data = cv_train_set,mtry=6,importance=T)
    
    # predict 
    model.pred <- predict(model.fit, newdata = cv_valid_set, type = "class")
    
    # model acurracy 생성
    model.table=table(model.pred,cv_valid_set$quality)
    flash[[i]]= c(sum(cv_valid_set$quality == model.pred)/nrow(cv_valid_set),model.table[2,2]/sum(model.table[,2]),model.table[1,1]/sum(model.table[,1]))
  }
  flash
  mean(unlist(flash)[c(seq(1,length(unlist(flash)),3))]) #10 fold cv값의 분류율 평균
}
randomForest= cv_randomForest(cv_list)
randomForest #0.8432073 


## hyper parameter tunning (Random Forest)

control <- trainControl(method="repeatedcv")
seed <- 7
metric <- "Accuracy"
set.seed(seed)
tunegrid2 <- expand.grid(.mtry=c(3:5))
rf_default2 <- train(quality~., data=wine2,  method="rf", tuneGrid=tunegrid2, trControl=control)
print(rf_default2)
plot(rf_default2)

# random forest model.fit error 시각화
model.fit <- randomForest(quality~., data = cv_train_set,mtry=sqrt(12),importance=T)
plot(model.fit)


# random forest 변수 중요도 시각화
varImpPlot(model.fit)


# ROC Curve
# Random Forest
##


rf.prediction.best <- predict(model.fit, cv_valid_set, type="prob")
rf.prediction.values <- rf.prediction.best[,2]
predictions <- prediction(rf.prediction.values, cv_valid_set$quality)
par(mfrow=c(1,1))
plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf,lty=1, lwd=2, col="black",
       main=title.text, cex.main=0.6, cex.lab=0.8,xaxs="i", yaxs="i")
  abline(0,1, col="red")
  auc <- performance(predictions,"auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,2)
  legend(0.4,0.4,legend=c(paste0("AUC: ",auc)),cex=0.6,bty = "n",box.col = "white")
}
plot.roc.curve(predictions, title.text = "RF ROC Curve")


#boost
cv_boost = function(cv_list){
  flash=list()
  for(i in 1:length(cv_list)) {
    valid_index <- cv_list[[i]]
    
    # K-fold 에서의 test 데이터
    cv_valid_set <- wine3[valid_index,] #여기서만 1,0으로 구분하는 wine3사용
    
    # K-fold 에서의 train 데이터
    cv_train_set <- wine3[-valid_index,]
    
    # 부스팅 모델 생성
    model.fit <- gbm(quality~.,data= cv_train_set,distribution='bernoulli',verbose=F,n.trees=5000,interaction.depth=1)
    
    # predict 
    model.boost <- predict(model.fit, newdata = cv_valid_set,n.trees = 5000 ,type = "response")
    model.pred = ifelse(model.boost>=0.5,1,0)
    
    # model acurracy 생성
    model.table=table(model.pred,cv_valid_set$quality)
    flash[[i]]= c(sum(cv_valid_set$quality == model.pred)/nrow(cv_valid_set),model.table[2,2]/sum(model.table[,2]),model.table[1,1]/sum(model.table[,1]))
  }
  flash
  mean(unlist(flash)[c(seq(1,length(unlist(flash)),3))]) #10 fold cv값의 분류율 평균
}


boosting = cv_boost(cv_list)
boosting # 0.7754284


##0,1의 종속 변수를 가지고 있는 wine3의 데이터
for(i in 1:length(cv_list)) {
  valid_index <- cv_list[[i]]
  
  # K-fold 에서의 test 데이터
  cv_valid_set2 <- wine3[valid_index,] #여기서만 1,0으로 구분하는 wine3사용
  
  # K-fold 에서의 train 데이터
  cv_train_set2 <- wine3[-valid_index,]
}


model.fit <- gbm(quality~.,data= cv_train_set2,distribution='bernoulli',verbose=F,n.trees=5000,interaction.depth=1)
rf.prediction.best <- predict(model.fit, cv_valid_set2, type="response")
rf.prediction.values <- rf.prediction.best
predictions <- prediction(rf.prediction.values, cv_valid_set2$quality)

###
plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf,lty=1, lwd=2, add=TRUE, col="blue",
       main=title.text, cex.main=0.6, cex.lab=0.8,xaxs="i", yaxs="i")
}
plot.roc.curve(predictions, title.text = "RF ROC Curve")



#Support Vector Classifier

#서포트벡터분류기
cv_svc = function(cv_list){
  flash=list()
  for(i in 1:length(cv_list)) {
    valid_index <- cv_list[[i]]
    
    # K-fold 에서의 test 데이터
    cv_valid_set <- wine2[valid_index,]
    
    # K-fold 에서의 train 데이터
    cv_train_set <- wine2[-valid_index,]
    
    # SVM 모델 생성
    model.fit <- svm(quality~.,data=cv_train_set,kernel='linear',cost=1,scale=F)
    
    # predict 
    model.pred <- predict(model.fit, newdata = cv_valid_set, type = "class")
    
    # model acurracy 생성
    model.table=table(model.pred,cv_valid_set$quality)
    flash[[i]]= c(sum(cv_valid_set$quality == model.pred)/nrow(cv_valid_set),model.table[2,2]/sum(model.table[,2]),model.table[1,1]/sum(model.table[,1]))
  }
  flash
  mean(unlist(flash)[c(seq(1,length(unlist(flash)),3))]) #10 fold cv값의 분류율 평균
}

svc=cv_svc(cv_list) 
svc 
#0.7527599

#서포트벡터머신

cv_svm= function(cv_list){
  flash= list()
  for(i in 1:length(cv_list)) {
    valid_index <- cv_list[[i]]
    
    # K-fold 에서의 test 데이터
    cv_valid_set <- wine2[valid_index,]
    
    # K-fold 에서의 train 데이터
    cv_train_set <- wine2[-valid_index,]
    
    # SVM 모델 생성
    model.fit <- svm(quality~.,data=cv_train_set,kernel='radial',gamma=0.5,cost=1,scale=F) 
    
    # predict 
    model.pred <- predict(model.fit, newdata = cv_valid_set,type='class')
    
    # model acurracy 생성
    model.table=table(model.pred,cv_valid_set$quality)
    flash[[i]]= c(sum(cv_valid_set$quality == model.pred)/nrow(cv_valid_set),model.table[2,2]/sum(model.table[,2]),model.table[1,1]/sum(model.table[,1]))
  }
  flash
  mean(unlist(flash)[c(seq(1,length(unlist(flash)),3))]) #10 fold cv값의 분류율 평균
}
svm= cv_svm(cv_list)
svm #[1] 0.7676591

##svm plot
rocplot = function(predict,truth,...){
  predob = prediction(predict,truth)
  perf = performance(predob,'tpr','fpr')
  plot(perf,...)
}
svm.fit.opt = svm(quality~.,data=cv_train_set,kernel='radial',gamma=0.5,cost=1, decision.values=T)
fitted = attributes(predict(svm.fit.opt,cv_valid_set,decision.values=T))$decision.values
rocplot(fitted,cv_valid_set$quality, add=T, lwd=2, col="red")
legend(0.7,0.5,c("RF","Boosting","SVM"),pch=1,col=c("black", "blue", "red"))



accuracy=data.frame(c(Logistic,lda,qda,tree,randomForest,boosting,svc,svm),1,8)
accuracy
plot(accuracy,type='l',ylim=c(0.6,1),col='blue')
colnames(accuracy)=c('Logistic','lda','qda','tree','randomForest','boosting,svc,svm')


reg.fit.data11 = regsubsets(quality~.,data=wine2,nvmax=11)
summary(reg.fit.data11)
reg.fit.data12 = regsubsets(quality~.,data=wine2,nvmax=11,)
reg.fit.data12 = regsubsets(quality~.,data=wine2,nvmax=11,method='forward')
summary(reg.fit.data12)

reg.fit.data13 = regsubsets(quality~.,data=data1,nvmax=11,method='backward')
summary(reg.fit.data13)


##visualization


acid1 <- qplot(data = wine2, x = wine2$fixed.acidity, binwidth=0.1, 
               color = I('#555555'), fill = I('#F79420'), 
               xlab = 'Fixed Acidity', ylab = 'Sample Count') +
  xlim(quantile(wine2$fixed.acidity, 0.01), 
       quantile(wine2$fixed.acidity, 0.99)) +
  ggtitle('Distribution of Fixed Acidity Contents in Collected Samples')

acid2 <- qplot(data = wine2, x = wine2$volatile.acidity, binwidth=0.01, 
               color = I('#555555'), fill = I('#F79420'), 
               xlab = 'Volatile Acidity', ylab = 'Sample Count') +
  xlim(quantile(wine2$volatile.acidity, 0.01), 
       quantile(wine2$volatile.acidity, 0.99)) +
  ggtitle('Distribution of Volatile Acidity Contents in Collected Samples')

acid3 <- qplot(data =wine2, x = wine2$citric.acid, binwidth=0.01, 
               color = I('#555555'), fill = I('#F79420'), 
               xlab = 'Citric Acid', ylab = 'Sample Count') +
  xlim(quantile(wine2$citric.acid, 0.01), 
       quantile(wine2$citric.acid, 0.99)) +
  ggtitle('Distribution of Citric Acid Contents in Collected Samples')

acid4 <- qplot(data = wine2, x = wine2$pH, binwidth=0.01, 
               color = I('#555555'), fill = I('#F79420'), 
               xlab = 'pH', ylab = 'Sample Count') +
  xlim(quantile(wine2$pH, 0.01), 
       quantile(wine2$pH, 0.99)) +
  ggtitle('Distribution of pH Contents in Collected Samples')


install.packages("gridExtra")
library(gridExtra)
grid.arrange(acid1, acid2, acid3, acid4, ncol=1)

summary(subset(wine2, select = c('fixed.acidity', 
                                 'volatile.acidity', 
                                 'citric.acid', 
                                 'pH')))


sulfur1 <- qplot(data = wine2, x = wine2$free.sulfur.dioxide, binwidth=1, 
                 color = I('#555555'), fill = I('#5760AB'), 
                 xlab = 'Free Sulfur Dioxide', ylab = 'Sample Count') +
  xlim(quantile(wine2$free.sulfur.dioxide, 0.01), 
       quantile(wine2$free.sulfur.dioxide, 0.99)) +
  ggtitle('Distribution of Free Sulfur Dioxide Contents in Collected Samples')

sulfur2 <- qplot(data = wine2, x = wine2$total.sulfur.dioxide, binwidth=5, 
                 color = I('#555555'), fill = I('#5760AB'), 
                 xlab = 'Total Sulfur Dioxide', ylab = 'Sample Count') +
  xlim(quantile(wine2$total.sulfur.dioxide, 0.01), 
       quantile(wine2$total.sulfur.dioxide, 0.99)) +
  ggtitle('Distribution of Total Sulfur Dioxide Contents in Collected Samples')

sulfur3 <- qplot(data = wine2, x = wine2$sulphates, binwidth=0.01, 
                 color = I('#555555'), fill = I('#5760AB'), 
                 xlab = 'Sulphates', ylab = 'Sample Count') +
  xlim(quantile(wine2$sulphates, 0.01), 
       quantile(wine2$sulphates, 0.99)) +
  ggtitle('Distribution of pH Sulphates Contents in Collected Samples')
grid.arrange(sulfur1, sulfur2, sulfur3, ncol=1)
summary(subset(wine2, select = c('free.sulfur.dioxide', 
                                 'total.sulfur.dioxide', 
                                 'sulphates')))



random1 <- qplot(data = wine2, x = wine2$residual.sugar, binwidth=0.1, 
                 color = I('#555555'), fill = I('#28B463'), 
                 xlab = 'Residual Sugar', ylab = 'Sample Count') +
  xlim(quantile(wine2$residual.sugar, 0.01), 
       quantile(wine2$residual.sugar, 0.99)) +
  ggtitle('Distribution of Residual Sugar Contents in Collected Samples')

random2 <- qplot(data = wine2, x = wine2$chlorides, binwidth=0.001, 
                 color = I('#555555'), fill = I('#28B463'), 
                 xlab = 'Chlorides', ylab = 'Sample Count') +
  xlim(quantile(wine2$chlorides, 0.01), 
       quantile(wine2$chlorides, 0.99)) +
  ggtitle('Distribution of Chlorides Contents in Collected Samples')

random3 <- qplot(data = wine2, x = wine2$density, binwidth=0.0001, 
                 color = I('#555555'), fill = I('#28B463'), 
                 xlab = 'Density', ylab = 'Sample Count') +
  xlim(quantile(wine2$density, 0.01), 
       quantile(wine2$density, 0.99)) +
  ggtitle('Distribution of Density in Collected Samples')
grid.arrange(random1, random2, random3, ncol=1)
summary(subset(wine2, select = c('residual.sugar', 
                                 'chlorides', 
                                 'density')))


thecor <- round(cor(wine2[,sort(c("fixed.acidity", "volatile.acidity", 
                                  "citric.acid", "residual.sugar", 
                                  "chlorides", "free.sulfur.dioxide", 
                                  "total.sulfur.dioxide", "density", 
                                  "pH", "sulphates", "alcohol"))],
                    method="pearson", use="pairwise.complete.obs"),3)

thecor[lower.tri(thecor)] <- NA
lower.tri(thecor)
thecor
library(reshape2)
thecor <- melt(thecor)
thecor
thecor$X1 <- as.character(thecor$Var1)
thecor$X2 <- as.character(thecor$Var2)
thecor <- na.omit(thecor)
(thecor)
ggplot(thecor, aes(X2, X1))+
  geom_tile(data=thecor, aes(fill=value), color="white")+
  scale_fill_gradient2(low="#943126", high="#1A5276", mid="white", 
                       midpoint=0, limit=c(-1,1),name="Correlation")+
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1))+
  coord_equal()



plot1 <- ggplot(aes(x = quality, y = alcohol), data = wine2) +
  geom_point(color = '#1A5276', alpha = 1/5, position = 'jitter') +
  stat_smooth(method = 'lm', color = '#943126') +
  scale_y_continuous(lim = c(quantile(wine2$alcohol, 0.01), 
                             quantile(wine2$alcohol, 0.99))) +
  labs(x = "Quality", y = "% Alcohol", 
       title = "Corrolation Between Alcohol and Quality (r = 0.436)")
plot1


ggplot(aes(x = quality, y = density), data = wine2) +
  geom_point(color = '#1A5276', alpha = 1/5, position = 'jitter') +
  stat_smooth(method = 'lm', color = '#943126') +
  scale_y_continuous(lim = c(quantile(wine2$density, 0.01), 
                             quantile(wine2$density, 0.99))) + 
  labs(x = "Quality", y = "Density", 
       title = "Corrolation Between Density and Quality (r = -0.307)")

ggplot(aes(x = quality, y = chlorides), data = wine2) +
  geom_point(color = '#1A5276', alpha = 1/5, position = 'jitter') +
  stat_smooth(method = 'lm', color = '#943126') +
  scale_y_continuous(lim = c(quantile(wine2$chlorides, 0.05), 
                             quantile(wine2$chlorides, 0.95))) +
  labs(x = "Quality", y = "Chlorides", 
       title = "Corrolation Between Chlorides and Quality (r = -0.210)")



acid1 <- ggplot(aes(x = quality, y = fixed.acidity), data = wine2) +
  geom_point(color = '#F79420', alpha = 1/5, position = 'jitter') +
  stat_smooth(method = 'lm', color = '#000000') +
  scale_y_continuous(lim = c(quantile(wine2$fixed.acidity, 0.01),
                             quantile(wine2$fixed.acidity, 0.99)))

acid2 <- ggplot(aes(x = quality, y = volatile.acidity), data = wine2) +
  geom_point(color = '#F79420', alpha = 1/5, position = 'jitter') +
  stat_smooth(method = 'lm', color = '#000000') +
  scale_y_continuous(lim = c(quantile(wine2$volatile.acidity, 0.01),
                             quantile(wine2$volatile.acidity, 0.99)))
grid.arrange(acid1, acid2, ncol=1)
acid3 <- ggplot(aes(x = quality, y = pH), data = wine2) +
  geom_point(color = '#F79420', alpha = 1/5, position = 'jitter') +
  stat_smooth(method = 'lm', color = '#000000') +
  scale_y_continuous(lim = c(quantile(wine2$pH, 0.01), 
                             quantile(wine2$pH, 0.99)))

sulfur1 <- ggplot(aes(x = quality, y = free.sulfur.dioxide), data =wine2) +
  geom_point(color = '#5760AB', alpha = 1/5, position = 'jitter') +
  stat_smooth(method = 'lm', color = '#000000') +
  scale_y_continuous(lim = c(quantile(wine2$free.sulfur.dioxide, 0.01), 
                             quantile(wine2$free.sulfur.dioxide, 0.99)))
grid.arrange(acid3, sulfur1, ncol=1)
sulfur2 <- ggplot(aes(x = quality, y = total.sulfur.dioxide), data = wine2) +
  geom_point(color = '#5760AB', alpha = 1/5, position = 'jitter') +
  stat_smooth(method = 'lm', color = '#000000') +
  scale_y_continuous(lim = c(quantile(wine2$total.sulfur.dioxide, 0.01), 
                             quantile(wine2$total.sulfur.dioxide, 0.99)))

sulfur3 <- ggplot(aes(x = quality, y = sulphates), data = wine2) +
  geom_point(color = '#5760AB', alpha = 1/5, position = 'jitter') +
  stat_smooth(method = 'lm', color = '#000000') +
  scale_y_continuous(lim = c(quantile(wine2$sulphates, 0.01), 
                             quantile(wine2$sulphates, 0.99)))
grid.arrange(sulfur2, sulfur3, ncol=1)
random1 <- ggplot(aes(x = quality, y = residual.sugar), data = wine2) +
  geom_point(color = '#28B463', alpha = 1/5, position = 'jitter') +
  stat_smooth(method = 'lm', color = '#000000') +
  scale_y_continuous(lim = c(quantile(wine2$residual.sugar, 0.01), 
                             quantile(wine2$residual.sugar, 0.99)))

random2 <- ggplot(aes(x = quality, y = citric.acid), data = wine2) +
  geom_point(color = '#28B463', alpha = 1/5, position = 'jitter') +
  stat_smooth(method = 'lm', color = '#000000') +
  scale_y_continuous(lim = c(quantile(wine2$citric.acid, 0.01), 
                             quantile(wine2$citric.acid, 0.99)))
grid.arrange(random1, random2, ncol=1)

library(dplyr)
attach(wine2)
wine.by_quality <- wine2 %>%
  group_by(quality) %>%
  summarise(mean_chlorides = mean(chlorides),
            mean_density = mean(density),
            mean_alcohol = mean(alcohol),
            n = n()) %>%
  ungroup() %>%
  arrange(quality)
wine.by_quality

ggplot(wine2, aes(density, chlorides, color=factor(quality)))+
  geom_point(size = 4, alpha = 1/2, position = 'jitter') +
  scale_color_brewer(palette ="Blues") +
  # scale_x_continuous(lim = c(0.987,1.0005)) +
  # scale_y_continuous(lim = c(0,22)) +
  scale_x_continuous(lim = c(quantile(wine2$density, 0.01), 
                             quantile(wine2$density, 0.99))) +
  scale_y_continuous(lim = c(quantile(wine2$chlorides, 0.01), 
                             quantile(wine2$chlorides, 0.99))) +
  labs(x = "Density", y = "Chlorides",
       title = "Relationship Between Density, Chlorides and Quality")
plot2 <- ggplot(wine2, aes(alcohol, density, color=factor(quality)))+
  geom_point(size = 4, alpha = 1/2, position = 'jitter') +
  scale_color_brewer(palette ="Purples") +
  scale_y_continuous(lim = c(quantile(wine2$density, 0.01), 
                             quantile(wine2$density, 0.99))) +
  labs(x = "% Alcohol", y = "Density",
       title = "Relationship Between Density, Alcohol and Quality")
plot2
ggplot(wine2, aes(alcohol, chlorides, color=factor(quality))) +
  geom_point(size = 4, alpha = 1/2, position = 'jitter') + 
  scale_color_brewer(palette ="Greens") +
  #scale_x_continuous(lim = c(6,16)) +
  #scale_y_continuous(lim = c(0,22)) +
  scale_y_continuous(lim = c(quantile(wine2$chlorides, 0.01), 
                             quantile(wine2$chlorides, 0.99))) +
  labs(x = "% Alcohol", y = "Chlorides",
       title = "Relationship Between Alcohol, Chlorides and Quality")
