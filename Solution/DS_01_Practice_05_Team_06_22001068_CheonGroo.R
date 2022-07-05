#########################################
### Course: Data Science              ###
### Semester: 2022-1                  ###
### Title: Practice 5                 ###
### Team: 06                          ###
### Member: 22001068 Cheon Groo       ###
###         21800643 Jeon Mingyu      ###
###         Byun Yongkyung, Lee Subin ###
#########################################

#gather함수를 사용하기 위해 필요한 tidyr패키지와 아래에서 필요한 stringr 패키지를 설치하고 라이브러리로 불러온다.
install.packages("tidyr")
install.packages("stringr")
#UserConsump_ed 데이터와 daily.attend 데이터를 로드하고 디렉토리를 지정해준다.
load(file="/Users/groo/Desktop/R/UserConsump_ed.RData")
load(file="/Users/groo/Desktop/R/daily.attend.RData")
setwd("/Users/groo/Desktop/R/")
UserConsump_ed
daily.attend

### Question.1

#(1)UserConsump_ed의 행과 열의 갯수를 파악하기 위해 nrow함수와 ncol함수를 이용하였다. 
nrow(UserConsump_ed)
ncol(UserConsump_ed)
#(2)각 열은 사용자들의 IP와 OTT들의 종류에 따른 time occupation과 data occupation을 나타낸다. cluster는 평균 소비 시간을 Low,Mid,High로 나누어 분류한 값을 나타낸다.
colnames(UserConsump_ed)
str(UserConsump_ed)
summary(UserConsump_ed)
head(UserConsump_ed)

### Question.2a

#구글과 유튜브 데이터 소비 빈도를 비교하려한다.auntitle함수를 사용하여 0%, 25%, 50%, 75%, 100% 총 네가지 구간으로 나누어 Low,Mid-Low,Mid-High,High로 구분해 주었고, levels 함수를 이용하여 구글과 유튜브 각각에 대한 범위에 맞는 값을 넣어주었다.
Google_cut_point<-quantile(UserConsump_ed$Google_data_occupation)
YouTube_cut_point<-quantile(UserConsump_ed$YouTube_data_occupation)
UserConsump_ed$Google_data_d<-cut(UserConsump_ed$Google_data_occupation,breaks = Google_cut_point, include.lowest = T)
UserConsump_ed$YouTube_data_d<-cut(UserConsump_ed$YouTube_data_occupation,breaks = YouTube_cut_point, include.lowest = T)
levels(UserConsump_ed$Google_data_d)<-c("GG_low","GG_mid_low","GG_mid_high","GG_high")
levels(UserConsump_ed$YouTube_data_d)<-c("YT_low","YT_mid_low","YT_mid_high","YT_high")
head(UserConsump_ed[,c("src_ip_numeric","YouTube_data_d","Google_data_d")])


#교수님은 구간별 오른쪽값 right=true 

### Question.2b

#유튜브 데이터 사용량이 적은 소비자들은 구글의 데이터 사용량도 적고, 유튜브 데이터 사용량이 큰 소비자들은 구글 데이터 사용량이 큰 것을 확인할 수 있다. 유튜브는 구글 계열사의 서비스로 연동되기 때문에, 위의 정보를 통해 유튜브 사용을 거의 하지 않는 소비자들은 구글 아이디가 없는 등의 이유로 데이터 사용량이 적다는 것을 추론할 수 있다.
table(UserConsump_ed$YouTube_data_d,UserConsump_ed$Google_data_d)

#선형의 관계에 있는지 확인하기, 유튜브에서 검색하면 구글의 검색량이 증가하기 떄문에 구글 사용량도 올라간다.

### Question.3

library(tidyr)
library(stringr)
#gather 함수를 이용하여 wide data format 형태의 데이터를 long data format으로 바꿔주었다. 또한 value부분에 있어서 character형식이었던 것을 numeric 형식으로 바꿔주면서 소숫점 4번째 자리에서 반올림을 하여 예상 결과값과 동일하게 만들어주었다.
UserConsump.long<-gather(UserConsump_ed, variable, value,-c(src_ip_numeric,cluster))
UserConsump.long$value<-round(as.numeric(UserConsump.long$value),3)
head(UserConsump.long)


### Question.4

#교수님 답
unique(UserConsump.long$variable)
UserConsump.long<-gather(UserConsump_ed,variable,value,-c("src_ip_numeric","cluster"))
unique(userConsump.ling$variable)
UserConsump.long$type<-ifelse(str_detect(UserConsump.long$variable,"data")==TRUE,"data","time")
#조금 구체적인 조건을 걸어서 엄격한 기준으로 찾는다. data라는 이름이 서비스 명에도 들어갈 수 있음
str_detect(names(UserConsump_ed),"_time_occupation")
head(UserConsump.long)
UserConsump.long$variable<-str_replace(UserConsump.long$variable,"_time_occupation","")
UserConsump.long$variable<-str_replace(UserConsump.long$variable,"_data_occupation","")


#(1)variable에서 _occupation은 중복되고 의미없는 문자열이기 때문에 gsub함수를 사용해 빈 스트링으로 대체하여 삭제해준다. _을 기준으로 separate하는 경우 HTTP_Proxy라는 항목이 있어 잘못된 값이 들어갈 우려가 있기 때문에 _data와 _time의 _를 gsub 함수를 이용해 -형태로 바꿔준다. str_detect 함수를 이용해 variable열의 element에 "time"이라는 스트링이 포함되어 있으면 type이라는 새로운 column에 "time"값을 추가하고, 그렇지 않으면 "data"값을 추가해준다.
UserConsump.long$variable <- gsub("_occupation","",UserConsump.long$variable)
UserConsump.long$variable <- gsub("_data","-data",UserConsump.long$variable)
UserConsump.long$variable <- gsub("_time", "-time", UserConsump.long$variable)
UserConsump.long$type<-ifelse(str_detect(UserConsump.long$variable,"-time"),"time","data")
head(UserConsump.long)
#(2)-형태로 이어져 있는 variable을 -를 기준으로 구분해주고, 구분한 값을 variable이라는 column에 저장해준다.
UserConsump.long<-separate(UserConsump.long, col=variable, "variable",sep='-')
head(UserConsump.long)
#(3)gsub함수를 이용해 variable열의 이름을 OTT로 바꿔주어 colname으로 다시 저장해준다. 
colnames(UserConsump.long)<-gsub("variable","OTT",colnames(UserConsump.long))
head(UserConsump.long)

### Question.5

#(1)aggregate 함수를 이용하여 long data type의 UserConsump.long 데이터에서 type열이 time인 행을 바탕으로 value값의 합을 OTT별로 계산하여 Top10_time에 저장해준다. Top10_time의 value열에 해당하는 값을 기준으로 내림차순으로 정렬하여 10개의 행을 보여주는 작업을 수행한다.
Top10_time<-aggregate(value ~ OTT, UserConsump.long[UserConsump.long$type=="time",], FUN = sum )
head(Top10_time[order(Top10_time$value,decreasing = TRUE),],10)
#(2)aggregate 함수를 이용하여 long data type의 UserConsump.long 데이터에서 type열이 data인 행을 바탕으로 value값의 합을 OTT별로 계산하여 Top10_data에 저장해준다. Top10_data의 value열에 해당하는 값을 기준으로 내림차순으로 정렬하여 10개의 행을 보여주는 작업을 수행한다.
Top10_data<-aggregate(value ~ OTT, UserConsump.long[UserConsump.long$type=="data",], FUN = sum )
head(Top10_data[order(Top10_data$value,decreasing = TRUE),],10)


### Question.6

#교수님 답-> UserConsump.long$high랑 long에 저장하고 names(UserConsump.long.high)[2]<-"High"해서merge(UserConsump.long.low,UserConsump.long.high) 해줌

#(1)UserConsump.long 데이터에서 type열의 값이 time이고 cluster가 0인(평균 시간 소비가 Low에 해당하는) 데이터를 바탕으로 value값의 평균을 OTT별로 계산하여 Low에 저장해준다. type열의 값이 time이고 cluster가 2인(평균 소비가 High에 해당하는) 데이터를 바탕으로 value값의 평균을 OTT별로 계산하여 High에 저장해준다. Low 데이터 프레임의 첫번째 열을 OTT열로 가지고, Low에서 계산한 평균 값을 소숫점 여섯째자리에서 반올림하여 보여주는 값을 Low 열로 가지며, High에서 계산한 평균 값을 소숫점 여섯째자리에서 반올림하여 보여주는 값을 High 열로 가지는 데이터 프레임을 만들어 UserConsump.long.lh에 저장하고 출력한다.
Low<-aggregate(value~OTT,data=subset(UserConsump.long,type=="time"&cluster==0,select=c(OTT,value)),mean)
High<-aggregate(value~OTT,data=subset(UserConsump.long,type=="time"&cluster==2,select=c(OTT,value)),mean)
UserConsump.long.lh<-data.frame("OTT"=Low[,1],"Low"=round(Low$value,6),"High"=round(High$value,6))
UserConsump.long.lh
#(2)Low열의 값들의 평균과, High열의 값들의 평균을 colMeans 함수를 이용해 계산한다. 이렇게 도출된 평균인 Low=1083.878, High=2298.072는 초단위이기 때문에 60으로 나누어 분단위로 변환하면 Low=18.0463, High=38.30120인 것을 확인할 수 있다. 이를 통해 평균 시간 소비가 적은 Low 그룹들은 OTT에 평균 18분을 소비하고, 평균 시간 소비가 큰 High 그룹들은 OTT에 평균 38분을 소비한다는 결과를 도출할 수 있다.
colMeans(UserConsump.long.lh[,2:3])/60

### Question.7

#tidyr::separate 함수를 이용하여 월/일/년도 사이에 연결되어 있는 '/'를 기준으로, date로 묶여있던 month,day,year들을 각각 분리하여 column에 추가해주었다.
daily.attend<-tidyr::separate(daily.attend, col = Date, into=c("month", "day", "year"), sep = '/')
head(daily.attend)

### Question.8

#substr함수를 이용하여 SchoolYear열에 있는 element 개별 string의 1번째부터 4번째까지 분리를 시키고, 다시 substr을 이용해 5번째부터 8번째로 분리해 두 덩어리로 나누었다. 두가지로 나눈 덩어리를 paste함수를 이용해 '-'를 기준으로 양쪽에 넣어 합쳐주었다. 
daily.attend$SchoolYear<-paste(substr(daily.attend$SchoolYear,1,4),
                               substr(daily.attend$SchoolYear,5,9),sep='-')
head(daily.attend)

### Question.9

#교수님 답->nrow()로 데이터 행 몇개인지 출력하고, is.na해서 미리 결측치 있는지 확인하고 시작

#입학생 수는 출석한 학생과 결석한 학생 그리고 퇴학 학생수의 합이어야 하는데 그렇지 않은 케이스가 있는지 확인하기 위해 다음과 같은 함수를 사용하였다. 출석한 학생과 결석한 학생 그리고 퇴학 학생수의 합이 입학생 수와 같지 않다면 TRUE가 나오게 하고 이것들의 갯수를 알기 위해 sum함수를 사용한 결과 0이 나온 것을 확인할 수 있다. 이를 통해 모든 데이터의 입학생 수는 출석+결석+퇴학 학생 수의 합을 만족하는 정상적인 데이터라는 사실을 도출할 수 있다.
daily.attend[daily.attend$Enrolled!=rowSums(daily.attend[,7:9]),]
sum(daily.attend$Enrolled !=(daily.attend$Present+daily.attend$Released+daily.attend$Absent))

### Question.10

#aggregate 함수를 이용하여 각 month마다의 결석 수 평균을 구하여 표시하고 order 함수를 이용하여 내림차순으로 결석수 평균이 높은 month순으로 나타내었다.
daily.attend<-aggregate(Absent~month,data=daily.attend,FUN=mean)
daily.attend
daily.attend[order(daily.attend$Absent,decreasing = TRUE),]
#데이터 상에서 7,8월이 없는 이유는 방학으로 인해 데이터가 없는 것으로 보이고 6월초부터 7,8월까지 방학인 미국일 가능성이 높을 것이며 그런 이유에서 방학이 시작하는 6월이 결석률이 높을 것이다. 9월에 학기가 시작해서 결석율이 낮다가 점점 높아지고 6월에 헤이해져서 피크를 찍음, 미리 여행을 갈수도, 코로나 이전이기 때문

