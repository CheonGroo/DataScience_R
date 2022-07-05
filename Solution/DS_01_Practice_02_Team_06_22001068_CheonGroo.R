#########################################
### Course: Data Science              ###
### Semester: 2022-1                  ###
### Title: Practice 2                 ###
### Team: 06                          ###
### Member: 22001068 Cheon Groo       ###
###         21800643 Jeon Mingyu      ###
###         Byun Yongkyung, Lee Subin ###
#########################################

load(file="/Users/groo/Desktop/R/country_set.RData")
setwd("/Users/groo/Desktop/R/")
country_set

### Question.1

#(1)불러온 country_set 데이터에서, head 함수에 3을 넣어 위에서부터 3개의 행을 출력한다.
head(country_set,3)
#(2)country_set 데이터에서, 10번째부터 13번째까지에 해당되는 행이 모두 출력되게 한다.
country_set[10:13,]

### Question.2

#(1)colnames 함수를 이용하여 country_set의 열의 이름에 해당하는 변수들을 출력한다.
colnames(country_set)
#(2)ncol 함수를 이용하여 country_set 데이터의 열 갯수를 알아내고, 마지막에 위치한 열의 이름에 해당하는 변수를 출력한다.
colnames(country_set[ncol(country_set)])

### Question.3

#(1)str 함수를 이용하여 country_set 데이터의 구조를 출력해준다.
str(country_set)
#(2)summary 함수를 이용하여 country_set 데이터 정보를 요약하여 출력해준다.
summary(country_set)
#summary에서 4열부터 마지막 열까지 Min 데이터가 모두 0인 것을 볼 떄, 데이터 샘플 중에 0으로만 채워진 행이 존재하고, 나라의 이름과 대륙 정보가 없는 것을 보아 입력 실수로 엉터리 쓰레기 값이 들어가 있다는 것을 추측할 수 있다.

### Question.4

#(1)is.na함수를 이용해 country_set의 code 열에 NA 결측치가 있는지 여부를 확인하고 sum 함수를 이용해 한눈에 확인할 수 있게 한다. sum이 0이면 is.na의 TRUE 값이 없다는 뜻이기 때문에 결측치가 없다는 것을 알 수 있고, sum이 1이상이면 is.na의 TRUE 값이 존재한다는 뜻이기 때문에 결측치가 존재한다는 것을 알 수 있다.
sum(is.na(country_set$code))
#(2)country_set에서 NA가 존재하는 행이 몇번째 행에 위치하고 있는지 위치를 인덱싱 할 수 있게끔 nrow를 이용해 확인한다.
nrow(country_set[NA,])
#(3)na.omit 함수를 이용해 country_set에서 NA가 위치한 행 전체를 삭제해주고, 그 값을 country_set에 저장해준다. tail함수를 이용해 127번쨰 줄이 삭제된 것을 확인할 수 있다.
country_set<-na.omit(country_set)
tail(country_set)

### Question.5

#(1)country_set의 continent 열에 저장된 값들을 unique 함수를 이용하여 중복값을 제외한 벡터로 정리하여 표현해준다.
unique(country_set$continent)
#(2)table함수를 이용하여 country_set 데이터의 continent 열에 해당하는 값들의 빈도를 계산하고, 이를 값 별로 정리하여 표현해준다.
table(country_set$continent)

### Quetion.6

#country_set의 life_expect의 최소값과 최대값을 계산하여 Maxmin 벡터에 저장하고, 기대수명 열에서 이 벡터값과 일치하는 행이 있으면 행 전체를 출력한다.
Maxmin<-c(min(country_set$life_expect),max(country_set$life_expect))
country_set[country_set$life_expect==Maxmin,]

### Question.7

#(1)GDP, life_expect, population, child.per.woman 을 Mean 변수를 생성하여 벡터화 시키고, country_set데이터에서 이 벡터값과 일치하는 열이 있으면 해당 열의 평균을 colMeans 함수를 이용해 계산한다. 계산한 값은 average.values 변수를 생성하여 저장해준다.
average.values<-colMeans(country_set[,c("GDP","life_expect","population","child.per.woman")])
average.values
#(2)as.data.frame 함수를 이용해 7-(1)에서 생성한 average.values를 행이 4개이고, Mean 벡터를 행으로 가지며, "average.values"를 열로 가지는 dataframe으로 변환시켜준다.
as.data.frame(average.values,nrow=4)
### Question.8

#country_set의 continent 열이 "Asia"이고, battle_death열이 0.0의 값을 가지는 행을 인덱싱으로 찾아 해당 행의 county_name을 반환하여 출력해준다.
country_set[country_set$continent=="Asia" & country_set$battle_death==0.0,"country_name"]
country_set[country_set$continent=="Asia" & country_set$battle_death==0.0,]$country_name
### Quetion.9

#country_name에 여섯개의 대륙의 이름을 element로 갖는 벡터를 저장하고, 해당 백터에서 첫번째 인덱스에 해당하는 "Asia"와 country_set의 continent열의 값이 일치하는 경우 리스트의 첫번째 인덱스에 추가한다. country_name의 두번째, 세번째, ... ,여섯번째 인덱스에 해당하는 대륙 이름과 country_set의 continent열의 값이 일치하는 경우 리스트 각각의 인덱스에 해당 대륙들의 정보를 추가한다.
country_name <- c("Asia","North America", "South America", "Africa", "Europe", "Oceania")
country_list <- list(country_set[country_set$continent==country_name[1],],
                     country_set[country_set$continent==country_name[2],],
                     country_set[country_set$continent==country_name[3],],
                     country_set[country_set$continent==country_name[4],],
                     country_set[country_set$continent==country_name[5],],
                     country_set[country_set$continent==country_name[6],])
country_list

### Question.10

#9번 문제에서 생성한 대륙의 이름이 담긴 country_name 벡터를 name함수를 이용하여 country_list의 인덱스 element로 지정해준다.
names(country_list)<-country_name
country_list

