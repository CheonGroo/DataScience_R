#########################################
### Course: Data Science              ###
### Semester: 2022-1                  ###
### Title: Practice 6                 ###
### Team: 06                          ###
### Member: 22001068 Cheon Groo       ###
###         21800643 Jeon Mingyu      ###
###         Byun Yongkyung, Lee Subin ###
#########################################

load(file="/Users/groo/Desktop/R/UserConsump.RData")
AirBnB<-read.csv("/Users/groo/Desktop/R/AirBnB_data.csv")
setwd("/Users/groo/Desktop/R/")
head(UserConsump)
head(AirBnB)

library(lubridate)
library(tidyr)
library(stringr)

### Question.1

#a.nrow와 unique 사용하여 df의 전체 row와 안겹치는 row 차이 확인하기(1249vs973), #b.어떤 column이 중복되었는지 확인하기 위해 table() 사용, #c.ip 빈도분포를 df로 만들어 중복되는 ip들 출력
nrow(UserConsump)
length(unique(UserConsump$src_ip_numeric))
freq.df <- data.frame(table(UserConsump$src_ip_numeric))
freq.df[freq.df$Freq>1,]$Var1

### Question.2

#%in%을 사용해서 UserConsump의 ip 열이 1번에서 구한 중복되는 값에 해당하는 경우를 dup 벡터에 저장한다. which 함수를 이용해 중복치에 해당하는 값의 인덱스를 찾아내고, UserConsump의 해당행을 데이터로 가진다. 이 데이터에서 모든 열을 ip열에 대해 더해주는 aggregate를 하고, 이 값을 UserConsump_New에 저장해준다. cluster에는 방금과 같은 방식으로 sum 대신 max를 이용해 최대값을 구하여 이를 cluster에 저장해준다. UserConsump에서 중복이 아닌 행들을 UserConsump에 저장해주고, rbind를 이용해 중복인 것과 아닌 것을 합해준다.
dup <- UserConsump$src_ip_numeric %in% freq.df[freq.df$Freq>1,]$Var1
UserConsump_New <- aggregate(.~src_ip_numeric, UserConsump[which(dup == TRUE),], sum)
cluster <- aggregate(cluster~src_ip_numeric, UserConsump[which(dup == TRUE),], max)
UserConsump_New$cluster <- cluster
UserConsump <- UserConsump[-which(dup==TRUE),]
UserConsump <- rbind(UserConsump, UserConsump_New)

length(unique(UserConsump$src_ip_numeric))
nrow(UserConsump)

### Question.3

#str_detect 활용하여 google과 data가 들어가는 colnames 확인하기, 위 조건에 해당하는 칼럼만을 추출하여 UserConsump.Google이라는 Variable 생성, UserConsump.Google의 첫번째 row 확인
UserConsump.Google <- UserConsump[,(str_detect(names(UserConsump), "Google") & str_detect(names(UserConsump), "data"))]
head(UserConsump.Google,1)

### Question.4

#summary를 확인해본 결과, 0으로 나타나는 항목이 있어 값이 0인 항목들을 뽑아 확인을 해보았다. Google Hangout은 미국 및 캐나다 내에서 무료통화를 지원하는 메신저이다. Google Hangout을 사용하지 않는 사람들은 WhatsApp data 사용량이 높은 것으로 보아, 구글 행아웃의 경쟁사인 왓츠앱을 사용하는 유럽 유저임을 알 수 있었고, 아시안 국가의 경우 위챗, 카카오톡, 라인등의 메신저가 존재하기에 구글 행아웃을 거의 사용하지 않아 0으로 나타나는 값은 충분히 도출 가능한 결과라고 할 수 있다. 
summary(UserConsump.Google$GoogleHangoutDuo_data_occupation)
UserConsump[UserConsump$GoogleHangoutDuo_data_occupation==0,]

### Question.5

#최솟값에서 1사분위에 해당하는 영역보다 3사분위에서 최댓값에 해당하는 영역이 상대적으로 넓은것으로 보아 사용시간이 많은 사람 간에도 사용시간 차이가 있을것으로 판단됨. 또한 중위값이 상자안에서 약간 아래쪽에 있는 것으로 보아 전반적으로 사용시간이 적은 사람이 많은 것을 알 수 있음. 그러나 중위수가 11,999인것과 달리 평균이 27,596에 이른것은 outlier가 너무 많기 때문이라 판단됨, 고사용자일수록 사용자별 편차가 매우 심함 + 소수의 과사용자가 매우 많은 시간을 사용함
GoogleServices_box <- boxplot(UserConsump.Google$GoogleServices_data_occupation)
GoogleServices_box <- hist(UserConsump.Google$GoogleServices_data_occupation, breaks = 1000)
summary(UserConsump.Google$GoogleServices_data_occupation)
str(UserConsump.Google$GoogleServices_data_occupation)

### Question.6

#전체 데이터는 7833개, 중복치를 제거한 값은 6378개로 다른 값이 나온다. 데이터를 확인해 본 결과, 한명의 호스트가 여러개의 방을 운영하는 경우, 중복된 host_id를 가지는 행이 발생하는 것을 알 수 있다. 
nrow(AirBnB)
length(unique(AirBnB$host_id))
head(AirBnB,20)


### Question.7

#중복된 host_id와 매물별 고유 id를 -로 이어 identifier로 사용하고자 했으나, nrow와 length(unique)를 사용한 결과 값이 달라 중복치가 여전히 존재함을 확인할 수 있었다. 중복치가 존재하는 행을 인덱싱하여 확인해본 결과, host_id와 id뿐만 아니라 모든열의 모든 값이 중복으로 입력된 데이터임을 확인할 수 있었다. 그래서 우리는 이를 정리하기 위해 unique함수를 사용해 data frame의 중복값을 없애주려 했지만, 10번 문제의 영향을 받아 정리하지 않은 상태로 남겨두었다. id는 고객 아이디
library(tidyr)
AirBnB$host_cust<-paste(AirBnB$host_id,AirBnB$id,sep="-")
AirBnB

nrow(AirBnB)
length(unique(AirBnB$host_cust))
freq.df <- data.frame(table(AirBnB$host_cust))
freq.df[freq.df$Freq>=2,]$Var1

#AirBnB <- unique(AirBnB)
#nrow(AirBnB) == length(unique(AirBnB$host_cust))

### Question.8

#각 년도별 신규 AirBnB제공자를 계산하기 위해 ID, Year, Name이 모두 겹치는 row 제거한 변수생성
#(년도가 달라지면 이전 AirBnB 제공자도 신규 제공자로 인식)
#이에 따라 AirBnB_name_year에 AirBnB 중복 제공자가 한번만 카운트 된 명단생성됨
#table함수를 사용하여 년도별로 신규 제공자를 세보면 다음과 같은 결과 얻게됨 
table(unique(AirBnB[,c("host_id","host_name","host_since_year")])$host_since_year)
sort(AirBnB[AirBnB$host_since_year=='2015',]$host_since_anniversary,decreasing = T)
#시간의 흐름에 따라 제공자 수가 지속적으로 증가하던것과 달리 2015년 신규 AirBnB수가 급격하게 감소한것은 확인해볼 필요가 있는 문제이다. 이를 확인하기 위해 2015년 AirBnB 신규 제공자의 데이터를 확인한 결과, host_since_anniversray가 4월까지만 집계되었음을 확인하였다. 즉, 5월~12월 신규 제공자들은 해당 데이터에 포함되지 않았음을 의미한다.


### Question.9

#AirBnB의 host_since_year과 host_since_anniversary를 합하고, ymd형태로 변환하여 anni_date column에 추가해준다. /형태를 -로 바꿔주기 위해 str_replace를 이용해 anni_date열의 형식을 바꿔준다. AirBnB의 anni_date의 날짜를 현재 날짜에서 빼주어 서비스가 얼마나 되었는지 값을 구하고, 이 값과 "days" 단어를 paste 함수로 연결하여 AirBnB의 age_days column에 추가해준다.
head(AirBnB,2)
AirBnB$anni_date<-ymd(paste(AirBnB$host_since_year,AirBnB$host_since_anniversary))
str_replace(AirBnB$anni_date,"/","-")
AirBnB$age_days<-paste(as.integer(Sys.Date() - as.Date(AirBnB$anni_date)),"days")
head(AirBnB[,c("host_id","host_since_year","host_since_anniversary","anni_date","age_days")])

### Question.10

#str_detect 함수를 활용하여 colnames 중에 review가 들어간 것들 찾아주었다. 이 중에서 colSums, is.na를 통해 결측치가 몇개 있는지 확인하였다. (Before) na.omit을 통해 결측치를 없애주고 colSums으로 없어진 것을 확인할 수 있었다. (After)
AirBnB_1<-AirBnB[,str_detect(colnames(AirBnB),"review")]
colSums(is.na(AirBnB_1))

AirBnB_1 <- na.omit(AirBnB_1)
colSums(is.na(AirBnB_1))
