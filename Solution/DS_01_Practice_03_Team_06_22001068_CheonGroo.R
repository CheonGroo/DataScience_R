#########################################
### Course: Data Science              ###
### Semester: 2022-1                  ###
### Title: Practice 3                 ###
### Team: 06                          ###
### Member: 22001068 Cheon Groo       ###
###         21800643 Jeon Mingyu      ###
###         Byun Yongkyung, Lee Subin ###
#########################################

#load함수를 이용해 country 데이터를 로드해주고, 워킹 디렉토리를 변경하여 설정해준다.
load(file="/Users/groo/Desktop/R/country.RData")
setwd("/Users/groo/Desktop/R/")
country

### Quetion.1

#(1)tapply 함수를 이용해 country 데이터의 continent에 대한 GDP열의 평균을 계산하여 출력한다.
tapply(country$GDP, country$continent, mean)
#(2)tapply 함수를 이용해 country 데이터의 continent에 대한 CO2열의 평균을 계산하여 출력한다.
tapply(country$CO2, country$continent, mean)
#(3)(1)과 (2)의 결과값을 각각 "GDP"와 "CO2"로 저장하고, 이를 cbind함수를 이용하여 출력한다.
data.frame(GDP=tapply(country$GDP, country$continent, mean),
           CO2=tapply(country$CO2, country$continent, mean))


### Question.2

#(1)for을 이용해 country 데이터의 행의 갯수만큼 반복문을 실행한다. 반복문 내에는 if 조건문을 이용해 country 데이터의 country_name 열의 i번째 값(i번째 행=현재까지 반복문 실행 횟수)이 "South Korea"와 같은지 색인한다. 색인 값이 "South Korea"와 같다면 country데이터에 BTS_country_d라는 column의 i번째 행에 "BTS_Home"을 저장하고, 같지 않다면 NA 값을 저장한다.
for (i in 1:nrow(country)){
  if (country$country_name[i]=='South Korea'){
    country$BTS_country_d[i] <- "BTS_Home"}
  else{country$BTS_country_d[i] <- NA}
}
country$BTS_country_d
#(2)BTS_country_d라는 column이 추가된 country데이터의 country_name열의 값이 "South Korea"이거나, "China"인 행값을 찾고 해당 행을 전부 출력한다.
country[country$country_name=="South Korea"| country$country_name=="China",] 


### Question.3

#(1)vectorized operation인 ifelse를 이용하여 country데이터의 country_name 열이 "South Korea"와 일치하는지 조건을 검증하여 이 값이 TRUE이면(일치하면) "BTS_Home"을, FALSE이면(일치하지 않으면) NA를 BTS_country에 저장해준다. country데이터에 BTS_country라는 column을 생성하여 ifelse를 통해 만들어진 BTS_country 값을 저장해준다.
country$BTS_country<-ifelse(country$country_name=="South Korea","BTS_Home",NA)
country$BTS_country
#(2)2-(2)에 추가적으로 BTS_country라는 column이 추가된 country데이터의 country_name열의 값이 "South Korea"이거나, "China"인 행값을 찾고 해당 행을 전부 출력한다.
country[country$country_name=="South Korea"| country$country_name=="China",]

### Question.4

#is.na 함수를 이용해 BTS_country 열에 NA 값이 있는지 확인하고, TRUE 혹은 FALSE로 반환된 값을 Replace라는 벡터에 저장해준다. ifelse를 이용해 Replace 벡터의 값이 TRUE인 경우(NA인 경우) "Others"로 정의하여 NA를 대체해주고, FALSE인 경우(BTS_Home인 경우) "BTS_Home"을 저장해준다.
country$BTS_country<-ifelse(is.na(country$BTS_country),"Others","BTS_Home")
country$BTS_country

### Question.5

#(1)mean 함수를 이용해 country데이터의 GDP 열의 평균값을 계산하여 avg 변수에 저장해준다. ifelse를 이용해 country데이터의 GDP 열의 값이 평균보다 작은지 조건을 검증하고 이 값이 TRUE면(평균보다 작으면) "Low"를, FALSE면(평균보다 크거나 같으면) "High"를 반환하여 compare 벡터에 저장해준다. country데이터에 GDP_dummy라는 새로운 column을 추가하여 compare 벡터의 값을 저장해준다. head 함수에 2를 넣어 완성된 country 데이터의 위에서부터 2개의 행을 출력한다.
country$GDP_dummy<-ifelse(country$GDP<mean(country$GDP),"Low","High")
head(country,2)
#(2)table 함수를 이용하여 country데이터의 GDP_dummy 열의 case와 그 빈도수를 출력해준다.
table(country$GDP_dummy)

### Question.6

#(1)country_list를 생성하여 리스트의 첫번째 인덱스에는 country데이터의 GDP_dummy 열의 값이 "Low"인 값을 찾아 해당하는 행을 전부 출력한 값을 저장하고, 리스트의 두번째 인덱스에는 country데이터의 GDP_dummy 열의 값이 "High"인 값을 찾아 해당하는 행을 전부 출력한 값을 저장한다. 
country_list <- list(country[country$GDP_dummy=="Low",],
                     country[country$GDP_dummy=="High",])
lapply(country_list,head)
#(2)country 데이터의 GDP_dummy가 "Low"인 값의 갯수의 합과 "High"인 값의 갯수의 합을 list의 형태로 출력한다.
list(sum(country$GDP_dummy=="Low"),
     sum(country$GDP_dummy=="High"))

### Question.7

#tapply를 이용하여 country 데이터의 continent별로 GDP열이 30000이상인지 여부를 계산하고, 반환된 값을 sapply 함수의 데이터로 사용하여 TRUE의 값을 더하여 simplify한 형태로 출력한다.
sapply(tapply(country$GDP, country$continent, function(x){x>30000}),sum)

### Question.8

#(1)sapply함수를 이용하여 country데이터의 column별 데이터 타입에 대한 정보를 데이터 타입의 형태로 country.data.type에 저장해주고, 이 데이터 프레임의 열 이름을 data.type으로 저장해준다.
country.data.type <- data.frame(data.type=sapply(country, typeof))
country.data.type
#(2)country.data.type의 data.type열의 값이 character인 경우 numeric.dummy 열에 "non-numeric"값을, 그외의 경우 "numeric"값을 넣어 새로운 column을 추가해준다.
country.data.type$numeric.dummy<-ifelse(country.data.type$data.type=="character","non-numeric","numeric")
country.data.type

### Question.9

#(1)Avgpop 이라는 function을 만들어 country 데이터의 continent열의 값이 입력값과 같은 경우 해당 행의 population 열의 평균 값을 반환하도록 작성한 후, Avgpop function에 "Asia"를 대입하여 아시아 대륙의 인구수의 평균을 출력해준다.
Avgpop<-function(x){
  mean(country[country$continent==x,]$population)
}
Avgpop("Asia")
#(2)Avgpop 이라는 function을 만들어 country 데이터의 continent열의 값이 입력값과 같고, GDP열의 값이 전체 GDP열의 평균보다 큰 경우 해당 행의 country 열의 값을 반환하도록 작성한 후, Avgpop function에 "Asia"를 대입하여 GDP가 평균이상인 아시아 대륙의 나라명을 출력해준다.
AvgpopList<-function(x){
  country[country$continent==x & country$GDP>mean(country$GDP),]$country
}
AvgpopList("Asia")

### Question.10

#tapply를 이용하여 country데이터의 전체 인구 평균보다 큰 인구수를 가진 행에 대한  GDP열의 평균을 구하고 FALSE(크지 않음)를 나타내는 첫번째 인덱스 값을 avg.gdp.low.pop에 저장한다. TRUE(큼)을 나타내는 두번째 인덱스 값을 avg.gdp.high.pop에 저장하여 이 둘의 값을 data frame의 형태로 gdp.pop.df에 저장하여 나타낸다.
avg.gdp.low.pop<- tapply(country$GDP, country$population > mean(country$population), mean)[1]
avg.gdp.high.pop<- tapply(country$GDP, country$population > mean(country$population), mean)[2]
gdp.pop.df<-data.frame(avg.gdp.low.pop, avg.gdp.high.pop, row.names = NULL)
gdp.pop.df


##교수님 답
gdp.pop<-tapply(country$GDP,country$population>mean(country$population), mean)
gdp.pop.df<-data.frame(t(gdp.pop))
names(gdp.pop.df)<-c("avg.gdp.low.pop","avg.gdp.high.pop")
gdp.pop.df
