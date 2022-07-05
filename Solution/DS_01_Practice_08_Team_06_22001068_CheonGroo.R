#########################################
### Course: Data Science              ###
### Semester: 2022-1                  ###
### Title: Practice 8                 ###
### Team: 06                          ###
### Member: 22001068 Cheon Groo       ###
###         21800643 Jeon Mingyu      ###
###         Byun Yongkyung            ###
#########################################


load(file="/Users/groo/Desktop/R/PRSA_set.RData")
PRSA_set
set.seed(2022)

### Question.1

#pm2.5가 핵심 변수이기 때문에 결측치가 존재하면 안됨
#(1)PRSA_set의 행의 갯수를 확인하고, PRSA_set의 pm2.5 열에 NA값이 있는 행이 몇개인지를 출력한다. NA가 아닌 행들만 PRSA_set에 저장하여 NA 행이 삭제된 것을 확인한다.
nrow(PRSA_set)
nrow(PRSA_set[is.na(PRSA_set$pm2.5)==TRUE,])
PRSA_set<-PRSA_set[is.na(PRSA_set$pm2.5)==FALSE,]
nrow(PRSA_set)
#(2)PRSA_set의 pm2.5열의 값이 75 초과인 경우 TRUE를 반환하고, 아닐경우 FALSE를 반환하여 PRSA_set의 bad_air 열에 저장해준다.
#TRUE로 놓으면 자동으로 내가 만들고자 하는 모델의 타겟을 설정해줄 수 있음
PRSA_set$bad_air <- ifelse(PRSA_set$pm2.5 > 75, TRUE, FALSE)
table(PRSA_set$bad_air)
#(3)PRSA_set에서 year열이 2014보다 작은 경우 PRSA.train에 저장하고, 2014인 경우 PRSA.test에 나누어 저장한다.
head(PRSA_set)
PRSA.train <- subset(PRSA_set,year<2014)
PRSA.test <- subset(PRSA_set,year==2014)

### Question.2

#정보의 편향이 일어나지 않게 random sampling을 해야 하지만 연도에 따라 동일하게 관측되었다는 가정하에 기존의 데이터에서 샘플을 추출해도 가능하다 판단했다.
#2010~2013의 데이터를 기반으로 예측 모델을 설계하고, 이 모델을 통해 2014년 데이터를 예측하여 얼마나 잘 맞는지를 확인한다.
#(1)PRSA.train데이터에서 year, month, day, hour과 같은 시간과 관련된 변수들을 제거하고, 핵심 변수인 pm2.5도 제외한 열에 대해 bad_air의 의사 결정 트리를 생성한다. 이 모델과 PRSA.train 데이터에 대한 예측값을 PRSA.train 데이터의 pred열에 추가시킨다. 이 예측값과 PRSA.train의 bad_air에 대한 예측 일치 여부를 확인하는 confusion matrix를 생성하고 Accuracy, Precision, recall, F1을 계산한다.
library(rpart)
bad_air_model <- rpart(bad_air ~ .-(year + month + day + hour + pm2.5), data = PRSA.train, method = "class",control = rpart.control())
PRSA.train$pred <- predict(bad_air_model, PRSA.train,type = 'class')
bad.conf.table.train <- table(actual=PRSA.train$bad_air,pred=PRSA.train$pred)
bad.conf.table.train
#ACCURACY
bad.accuracy.train<-sum(diag(bad.conf.table.train))/sum(bad.conf.table.train)
bad.accuracy.train
#PRECISION
bad.precision.train<-bad.conf.table.train[2,2] / sum(bad.conf.table.train[,2])
bad.precision.train
#RECALL
bad.recall.train<-bad.conf.table.train[2,2] / sum(bad.conf.table.train[2,])
bad.recall.train
#F1
F1Score <- 2*((bad.precision.train * bad.recall.train)/(bad.precision.train + bad.recall.train))
F1Score

### Question.3

#2번에서 생성한 모델을 PRSA.test에 사용하여 PRSA.test의 pred 열에 결과값을 저장하고 2번과 같은 방법으로 confusion matrix를 생성하여 Accuracy, precision, recall, F1을 계산한다.
PRSA.test$pred <- predict(bad_air_model, PRSA.test,type = 'class')
bad.conf.table.test <- table(actual=PRSA.test$bad_air,pred=PRSA.test$pred)
bad.conf.table.test
#ACCURACY
bad.accuracy.test<-sum(diag(bad.conf.table.test))/sum(bad.conf.table.test)
bad.accuracy.test
#PRECISION
bad.precision.test<-bad.conf.table.test[2,2] / sum(bad.conf.table.test[,2])
bad.precision.test
#RECALL
bad.recall.test<-bad.conf.table.test[2,2] / sum(bad.conf.table.test[2,])
bad.recall.test
#F1
F1Score <- 2*((bad.precision.test * bad.recall.test)/(bad.precision.test + bad.recall.test))
F1Score
#Accuracy of 0.7225647 on PRSA.train is not similar 0.667244 on PRSA.test 즉, overfitting issue: YES
#cp=0.01로 simple하게 만든 모델이기 떄문에 0.05정도의 차이는 심각한 overfitting이라 생각할 수 없다. 보통 10%의 차이면 overfitting이라 생각하지 않음.

### Question.4

#overfitting을 해결하기 위한 pre-pruning approach - 첫번째 방법. maxdepth
#pre-pruning할 때는 cp=0을 설정해주어야 한다.
#plots에서 제일 위에 글자는 다수인 녀석, 밑에 0.49는 positive인 비율, 시나리오에 해당하는 비율이 전체의 19%다 
#maxdepth의 갯수를 3으로 설정하여 2번과 같은 방법으로 PRSA.train에 대한 의사결정트리를 생성한다. 이 모델과 PRSA.test에 대한 예측값을 PRSA.test 데이터의 pred_depth열에 추가시킨다. bad_air열의 값과 pred_depth열의 값이 같은 것들의 평균, 즉 accuracy를 구한다. 이 모델을 rpart.plot 함수로 확인해보면 시나리오가 총 4개인 의사결정트리가 생성된 것을 확인할 수 있다.
bad_air_model_predepth <- rpart(bad_air ~ .-(year + month + day + hour + pm2.5 + pred), data = PRSA.train, method = "class",control = rpart.control(maxdepth = 3))
PRSA.test$pred_depth <- predict(bad_air_model_predepth,PRSA.test,type = 'class')
mean(PRSA.test$bad_air == PRSA.test$pred_depth)
library(rpart.plot)
rpart.plot(bad_air_model_predepth)

### Question.5

#overfitting을 해결하기 위한 pre-pruning approach - 두번째 방법. minsplit
#minsplit을 15000으로 설정하여 2번과 같은 방법으로 PRSA.train에 대한 의사결정트리를 생성한다. 이 모델과 PRSA.test에 대한 예측값을 PRSA.test 데이터의 pred_split열에 추가시킨다. bad_air열의 값과 pred_split열의 값이 같은 것들의 평균, 즉 accuracy를 구한다. 
bad_air_model_presplit <- rpart(bad_air ~ .-(year + month + day + hour + pm2.5 + pred),data = PRSA.train, method = "class",control = rpart.control(minsplit = 15000))
PRSA.test$pred_split <- predict(bad_air_model_presplit, PRSA.test, type = 'class')
mean(PRSA.test$bad_air == PRSA.test$pred_split)

PRSA.train$pred_split <- predict(bad_air_model_presplit, PRSA.train, type = 'class')
mean(PRSA.train$bad_air == PRSA.train$pred_split)
#그 결과 2번에서 train set이 0.7225647, test set이 0.667244로 overfitting issue가 있었기 때문에 4번에서 maxdepth를 3개로 설정하여 의사결정 구조를 단순화시켰고, 5번에서 minsplit을 15000으로 설정하여 노드를 분할하기 위해 필요한 데이터의 갯수를 조정해주었다. 그 결과 train의 accuracy가 0.6812908, test set이 0.6197899으로 오히려 줄어든 것을 확인할 수 있었다. 여전히 두개 사이의 차이가 0.05이상 나기 때문에 overfitting issue는 해결되지 않았다고 할 수 있다.


### Question.6

#overfitting을 해결하기 위한 post-pruning approach - 방법. cp 설정
#nsplit 가지, rel error 완성된 모형의 에러, xerror 
#cp=0 즉 비용이 들지 않을 때 까지 가지치기를 진행하도록 하여 2번과 같은 방법으로 PRSA.train에 대해 의사결정모델을 생성해준다. 이 모델에 대한 plotcp를 그린 결과, root node와 주요 노드에 대해 에러가 큰폭으로 감소하다가 cp가 0.00021부근이 되는 순간 선 이하로 내려가는 최저점이 되는것을 확인할 수 있다. 선 아래 범위 안에서는 유의미한 차이가 거의 없기 때문에 complexity를 고려하여 선(최솟값+표준편차)을 기준으로 size of tree가 가장 작은 값인 cp=0.00021를 선택하였다. cp에 값을 대입하여 모델을 생성한 결과, accuracy가 0.7059231로 기존에 비해 높은 값을 띄는것을 확인할 수 있었다.
bad_air_model_post <- rpart(bad_air ~ .-(year + month + day + hour + pm2.5 + pred), data = PRSA.train, method = "class",control = rpart.control(cp=0))
plotcp(bad_air_model_post)
cptable.sample<-data.frame(printcp(bad_air_model_post))

bad_air_model_pruned <- prune(bad_air_model_post,cp=0.00021  )
PRSA.test$pred <- predict(bad_air_model_pruned,PRSA.test,type = 'class')
mean(PRSA.test$bad_air == PRSA.test$pred)
###
#abline(h=cptable.sample['xerror',]+cptable.sample['xstd',],col="red")
#


### Question.7


library(ROCR)
calAUC <- function(preCol, targetCol){
  perf <- performance(prediction(preCol, targetCol), 'auc') 
  as.numeric(perf@y.values) #AUC = peformance 데이터에서 y.values
}
#2번에서 생성한 모델로 ROC와 AUC 측정 
bad_air_model
PRSA.train$pred_prob <- predict(bad_air_model, PRSA.train, type="prob")

plot(performance(prediction(PRSA.train$pred_prob[,2], PRSA.train$bad_air), 'tpr', 'fpr'))
calAUC(PRSA.train$pred_prob[,2],PRSA.train$bad_air)

#6번에서 생성한 모델로 ROC와 AUC 측정
bad_air_model_pruned
PRSA.train$pred_prob <- predict(bad_air_model_pruned, PRSA.train, type="prob")

plot(performance(prediction(PRSA.train$pred_prob[,2], PRSA.train$bad_air), 'tpr', 'fpr'))
calAUC(PRSA.train$pred_prob[,2],PRSA.train$bad_air)

#2번 모델에서의 AUC가 0.765299이고 6번 모델에서의 AUC가 0.8448379로 측정된 것을 보아, 6번 모델에서 cp값을 설정하여 post-pruning approach를 사용함으로써 1에 더욱 근접한 결과가 나온것을 확인할 수 있었다. 또한 ROC 그래프를 비교했을 때, 아무것도 하지 않은 2번 원본 모델에 비해 cp값을 설정한 6번모델에서 비교적 더 좌상향 된것을 확인할 수 있었다. 위 두 결과를 종합했을 때, post-pruning approach를 통해 complexity를 조절해주어 TPR, 즉 예측율이 올라갔다는 것을 도출할 수 있다.
