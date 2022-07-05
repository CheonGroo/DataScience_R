#########################################
### Course: Data Science              ###
### Semester: 2022-1                  ###
### Title: Practice 6                 ###
### Team: 06                          ###
### Member: 22001068 Cheon Groo       ###
###         21800643 Jeon Mingyu      ###
###         Byun Yongkyung            ###
#########################################

load(file = "/Users/groo/Desktop/R/PRSA_data_class.RData")
PRSA_data_class

### Question.1

#is.na를 이용해 데이터에 결측값이 존재하는지 여부와 sum을 이용해 몇개가 있는지 확인, NA인 값들을 인덱싱하여 해당 행을 확인. 우리가 하고자 하는건 Month에 대한 미세먼지 농도를 예측하는 모델이기에 pm2.5는 핵심 변수이고 값이 반드시 존재해야 한다. 그러므로 NA값을 가지고 있는 pm2.5열을 데이터에서 삭제시켜준 PRSA_DATA를 사용하여야 한다.
sum(is.na(PRSA_data_class))
PRSA_data_class[is.na(PRSA_data_class$pm2.5),]

PRSA_DATA<-na.omit(PRSA_data_class)
sum(is.na(PRSA_DATA))
head(PRSA_DATA)

### Question.2

#PRSA_data.ls라는 리스트를 생성하고, PRSA_DATA의 type이 train인 행들을 train열을 추가하여 저장해준다. 같은 방법으로 type이 test인 행들을 test열에 추가한다. PRSA_data.ls 를 lapply연산하여 리스트 각 element의 행의 갯수를 반환하다.
PRSA_data.ls<- list()
PRSA_data.ls$train<-subset(PRSA_DATA, PRSA_DATA$type=="train")
PRSA_data.ls$test<-subset(PRSA_DATA, PRSA_DATA$type=="test")
PRSA_data.ls
lapply(PRSA_data.ls,nrow)

### Question.3

PRSA.train<-PRSA_data.ls[['train']]
PRSA.test<-PRSA_data.ls[['test']]
#보고자 하는 변수 월(month)과 미세먼지농도(hign/low) 빈도분포 확인
tble<-table(PRSA.train$Month,PRSA.train$pm2.5)
tble
#prop.table 통해 빈도를 비율로 계산
prop.table(tble,margin = 1)
# 월과 미세먼지농도의 빈도를 비율로 계산한것 중 미세먼지가 high인것만 추출
PRSA_model<-prop.table(tble,margin = 1)[,1]
sort(PRSA_model, decreasing = T)

PRSA_model
PRSA.train$fine_dust_concentration<-PRSA_model[PRSA.train$Month]
head(PRSA.train[,c('Month','fine_dust_concentration','pm2.5')],10)

#예측을 위해 threshold 설정
threshold<-0.5
#(미세먼지가 high일 확률이 0.5보다 높을지[T]일지 낮을지[F])예상되는 결과를 변수로 생성하기
PRSA.train$dust_prediction<-PRSA.train$fine_dust_concentration > threshold
head(PRSA.train[,c('Month','fine_dust_concentration','dust_prediction','pm2.5')],10)
#얼마나 정확하게 예측했는지 확인해보기
dust.conf.table.train<-table(pred=PRSA.train$dust_prediction,
                             actual=PRSA.train$pm2.5)
dust.conf.table.train

dust.accuracy.train<-sum(diag(dust.conf.table.train)) / sum(dust.conf.table.train)
dust.accuracy.train

#같은방식으로 test set 예측률 구하기
PRSA.test$fine_dust_concentration<-PRSA_model[PRSA.test$Month]
PRSA.test$dust_prediction<-PRSA.test$fine_dust_concentration>threshold
head(PRSA.test[,c('Month','fine_dust_concentration','dust_prediction','pm2.5')],10)

dust.conf.table.test<-table(pred=PRSA.test$dust_prediction,
                            actual=PRSA.test$pm2.5)
dust.conf.table.test
dust.accuracy.test<-sum(diag(dust.conf.table.test)) / sum(dust.conf.table.test)
dust.accuracy.test

#
get_accuracy<-function(pred, actual){
  tble<-table(pred, actual)
  return(round(sum(diag(tble)) / sum(tble),3))
}

threshold<-0.5
PRSA.train$dust_prediction<-PRSA.train$fine_dust_concentration>threshold
print(paste("Accuracy of train set : ", 1- get_accuracy(PRSA.train$dust_prediction,
                                                        PRSA.train$pm2.5)))

PRSA.test$dust_prediction<-PRSA.test$fine_dust_concentration>threshold
print(paste("Accuracy of test set : ", 1- get_accuracy(PRSA.test$dust_prediction,
                                                       PRSA.test$pm2.5)))
#최종적으로 두 값이 비슷하다는 것을 알 수 있다.
#simple한 모델에서는 overfitting issue가 없다


### Question.4

#performance 함수와 prediction 함수를 이용한 ROC 곡선 생성과 AUC의 넓이를 계산해 주었다.
#먼저 auc의 값을 구하기 위해 prediction 함수를 이용해서 ROCR 함수를 적용하기에 적합한 형태로 만들어주었다. prediction 객체를 이용하여 performance를 계산하여 표현하였다.

library(ROCR)
plot(performance(prediction(PRSA.test$fine_dust_concentration, PRSA.test$pm2.5), 'tpr','fpr'))

#
perf<-performance(prediction(PRSA.train$fine_dust_concentration,PRSA.train$pm2.5),'auc')
perf@y.values
#y.values는 auc를 측정한 값

calAUC<- function (predCol, targetCol){
  perf<-performance(prediction(predCol,targetCol),'auc')
  as.numeric(perf@y.values)
}
calAUC(PRSA.train$fine_dust_concentration,PRSA.train$pm2.5)
calAUC(PRSA.test$fine_dust_concentration,PRSA.test$pm2.5)


### Question.5

#i가 0.45부터 0.02의 간격으로 0.55까지 늘어나는 for loop를 작성한다. for loop안에는 threshold를 i로 넣어 매 루프마다 재설정 되게 하였으며, threshold값 이상인 fine_dust_concentration을 추출하여 테이블화 시켜주는 작업을 한다. 이렇게 도출된 actual/predect 테이블을 이용하여 precision과 recall을 계산해주고, rbind를 이용해 threshold와 precision,recall값을 df1에 추가해주어 데이터 프레임을 완성시킨다. for loop가 끝나면 0.55까지 채워져있는 데이터 프레임의 열 이름을 지정하여 출력해준다.

df1<-data.frame()
for (i in seq(0.45,0.55,by=0.02)){
  threshold<-i
  PRSA.train$dust_prediction<-PRSA.train$fine_dust_concentration > threshold
  dust.conf.table.train<-table(pred=PRSA.train$dust_prediction,
                               actual=PRSA.train$pm2.5)
  dust.accuracy.train<- (dust.conf.table.train[2,1] + dust.conf.table.train[1,2]) / sum(dust.conf.table.train)
  precision<-dust.conf.table.train[2,1] / (dust.conf.table.train[2,2] +dust.conf.table.train[2,1])
  recall<- dust.conf.table.train[2,1] / sum(dust.conf.table.train[,1])
  df1=rbind(df1,c(i,precision,recall))
}
names(df1)=c("threshold","precision","recall")
df1
#Threshold를 높이면 FP가 낮아지기 때문에 precision이 올라간다

### Question.6

#threshold를 증가하면 precision은 작은 비율로 증가하고
#recall은 큰 비율로 감소하는 것을 볼 수 있다.
#주어진 식을 사용하여 계산한 값을 보면
#threshold가 증가하면 F1Score는 감소한다.
#이것을 해석해보면 theshold가 증가하면
#F1Score 식의 분모는 감소하고 F1Score의 분자는 증가하기 때문에 
#F1Score는 threshold가 증가함에 따라 크게 감소한다.

F1Score <- 2*((precision * recall)/(precision + recall))
F1Score
#모형을 평가하는 지표, 조화평균, 우리가 쓰는 평균은 극단값의 영향을 많이 받음
#두 값이 고르게 영향을 받는 평균치 - 우리가 쓴건 값이 두개일때 구하는 방법 공식


#7

#PRSA_data.ls의 train element의 TEMP열에 대하여 음의 무한대와 -10,0,10,20,30, 양의 무한대로 구간을 나눈 값을 TEMP_group 열에 추가해준다. table 함수를 이용해 앞의 결과와 train element의 pm2.5에 대한 테이블을 cont_table에 저장해준다. count_table의 LOW에 대하여 prop함수로 확률을 계산한 값을 sv_model_TEMP에 저장해준다.
#3,4번과 같은 방식으로 TMP에 대한 미세먼지 농도를 구한다.

summary(PRSA_DATA$TEMP)

PRSA_data.ls[['train']]$TEMP_group <-cut(PRSA_data.ls[['train']]$TEMP,
      breaks = c(-Inf,-10,0,10,20,30,Inf),
      labels = c('under -10','- 10 ~ 0',' 0 ~ 10', '10 ~ 20', '20 ~ 30', 'over 30' ))
#고르게 분포할 수 있는 구간을 쪼개는게 skill

table(PRSA_data.ls[['train']]$TEMP_group)

cont_table <- table(PRSA_data.ls[['train']]$TEMP_group,
                    PRSA_data.ls[['train']]$pm2.5)

sv_model_TEMP <- prop.table(cont_table, margin = 1)[,2]

PRSA_data.ls[['train']]$TEMP_est_prob <- sv_model_TEMP[PRSA_data.ls[['train']]$TEMP_group]
PRSA_data.ls[['train']]$TEMP_predict <- ifelse(PRSA_data.ls[['train']]$TEMP_est_prob > threshold,'HIGH','LOW')

PRSA_data.ls[["train"]]$TEMP_predict <- factor(PRSA_data.ls[["train"]]$TEMP_predict,
                                               levels = c("LOW","HIGH"),
                                               ordered = T)

TEMP.conf.table.train <- table(pred = PRSA_data.ls[["train"]]$TEMP_predict,
                               actual = PRSA_data.ls[["train"]]$pm2.5)

TEMP.conf.table.train

sum(diag(TEMP.conf.table.train)) / sum(TEMP.conf.table.train)


### Question.8

# AUC를 비교해봤을 때 그 값이 높은 모델이 더 좋은 모델이다 두가지 종류의 모델을 비교해 봤을때 유의미한 의미를 가지는가를 현실적으로 봤을때 의미있는 모델이냐?절대적으로 보았을때 두가지 모두 의미가 없는 모델이다.그 이유는 AUC가 0.5 이하이기 때문이다. 
# 그렇다면 미세먼지 농도와 관련된 다른 모델을 살펴보았을때 직접적으로 연관되어있는 대기중 산란도, 대기중 매연 배출량이 적절할 것 같다.

plot(performance(prediction(PRSA.test$fine_dust_concentration, PRSA.test$pm2.5), 'tpr','fpr'))

head(PRSA_data.ls[["train"]],10)
