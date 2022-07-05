#########################################
### Course: Data Science              ###
### Semester: 2022-1                  ###
### Title: Practice 4                 ###
### Team: 06                          ###
### Member: 22001068 Cheon Groo       ###
###         21800643 Jeon Mingyu      ###
###         Byun Yongkyung, Lee Subin ###
#########################################

#load함수를 이용해 covid.set 데이터를 로드해주고, 워킹 디렉토리를 변경하여 설정해준다.
load(file="/Users/groo/Desktop/R/covid.set.RData")
setwd("/Users/groo/Desktop/R/")
covid.set

### Question.1

#(1)head를 이용해 데이터의 가장 앞에 있는 6가지의 데이터를 확인해주고 nrow함수를 통해 데이터의 행이 총 몇개인지 출력한다.
head(covid.set)
nrow(covid.set)
#(2)ncol 함수를 통해 데이터의 열이 총 몇개인지 출력한다.
ncol(covid.set)
#(3)주의 갯수를 확인하기 위해 unique 함수를 이용해 중복된 값을 제외해주고 length를 이용해 총 갯수를 구해준다.
length(unique(factor(covid.set$ST_Name)))
#(4)Countyname의 갯수를 확인하기 위해 unique 함수를 이용해 중복된 값을 제외해주고 length를 이용해 총 갯수를 구해준다. 
length(unique(factor(covid.set$Countyname)))

### Question.2

#(1)NewCases를 Population으로 나누어 NewCases.pop 변수를 만들어주었다
covid.set$NewCases.pop<-(covid.set$NewCases / covid.set$Population)
head(covid.set)
#(2)2021년의 Alabama라는 나라의 Autauga와 Baldwin 도시를 서로 비교하기 위해 subset 함수를 사용하여 총 3가지의 조건을 넣어 비교하여 결과를 도출하였다 
subset(covid.set, covid.set$year==2021 & (covid.set$Countyname=="Autauga"|covid.set$Countyname=="Baldwin") & covid.set$ST_Name=="Alabama")
#Baldwin이 Autauga에 비해 인구가 더 크고 신규 확진자도 많지만, 인구 수 대비 확진율인 NewCases.pop을 확인했을때, 더 낮다는 것을 발견할 수 있다. size-effect는 절대적인 확진자 수가 많다는 것에 초점을 둔 오류이다.

### Question.3

#
head(covid.set[covid.set$NewCases.pop=="Inf",])
head(covid.set[covid.set$NewCases.pop=="NaN",])
head(covid.set[covid.set$NewCases.pop=="-Inf",])
##왜 이렇게 될까?
##Inf(Infinite): 즉 분모가 0이어서 무한대로 나옴
##-Inf(-Infinite): 즉 분모가 0이고 분자가 -인 경우 음의 무한대로 나옴
##NaN(Not a Number): 0/0처럼 수학적으로 정의되지 않음

summary(covid.set)
covid.set<-covid.set[covid.set$Population>0&covid.set$NewCases>=0,]
summary(covid.set)
#분모인 Population이 0인 주는 존재할 수 없기 때문에 이 값들을 제외한 나머지 데이터를 covid.set에 저장하여 fix해준다.

### Question.4

#aggregate 함수를 사용하여 각년도별 NewCases와Deaths를 cbind를 통해 서로 묶어서 비교해 보았다. 
aggregate(cbind(NewCases, Deaths) ~ year, data=covid.set,FUN=mean)

### Qustion.5

#which()function과 sort 함수를 이용하여 년도가 2020년이고, 평균보다 높은 Confirmed을 가지는 지역과 평균보다 높은 Death를 가지는 지역, 그리고 평균보다 낮은 NewCases를 가진 지역을 비교해 찾아내었다. 추가적으로 sort,decreasing=FALSE를 이용하여 알파벳 순으로 정렬하였다.
covid.set<- covid.set[covid.set$year==2020,]
sort(covid.set[which(covid.set$Confirmed>mean(covid.set$Confirmed)
                       &covid.set$Deaths>mean(covid.set$Deaths)
                       &covid.set$NewCases<mean(covid.set$NewCases)),"Countyname"], decreasing = FALSE)

#교수님 방식 intersect.cases<-intersect(which(covid.set$Confirmed>mean(covid.set))) 

### Question.6

Crime<-read.csv("/Users/groo/Desktop/R/Crime.csv")
head(Crime)
#colname에 해당하는 변수들이 존재하지 않고, 행의 첫번째에 존재해야 할 데이터가 colname 위치에 올라와 있으며, 숫자값 앞에 X가 붙어 데이터가 변형되었다.또한 첫번째 열에 의미없는 데이터가 들어가 있다.

### Qustion.7

#문제6번을 통해 발견한 문제를 고치는 문제로 파일을 불러올 때 column 명을 넣기 위해 header=F를 이용하였다. 첫번째열의 의미없는 데이터를 빼기 위해서 subset 함수의 select을 이용해 첫번째 열을 삭제하였다. 그러고 나서 column 명을 넣어주었다.
Crime<-read.csv("/Users/groo/Desktop/R/Crime.csv",header = F)
head(Crime)
Crime<-subset(Crime, select = -1)
head(Crime)
names(Crime)<-c( "MSA", "ViolentCrime", "Murder", "Rape", "Robbery", "AggravatedAssault", "PropertyCrime", "Burglary", "Theft", "MotorVehicleTheft", "State", "City")
head(Crime)

### Qustion.8

#order 함수(내림차순)를 이용해 Crime Murder 순으로 정렬하였고 head를 통해 가장 많은 6개의 정보를 도출하였다.#Detroit,New Orleans,Monroe, Savannah,Philadelphia, Myrtle Beach가 top 6 도시들이다.
head(Crime[order(Crime$Murder,decreasing = TRUE),])


### Question.9

#(1)set.seed함수를 이용해 2022로 랜덤함수의 seed를 고정해 값이 변하지 않게 해주었다. gsub 함수를 통해 Crime$ViolentCrime에 ","를 없애주었다.substr함수를 사용해 value들의 길이를 통일 시켜주었고 character였던 값들을 소수점 표현이 가능하도록 as.double를 통해 실수로 변경해주었다. sample함수를 이용해 Crime 데이터에서 100개의 난수를 추출하여 Crime1에 저장해주고,같은 방법으로 Crime2도 저장해주었다.
set.seed(2022)
Crime$ViolentCrime<-gsub(",","",Crime$ViolentCrime)
Crime$ViolentCrime<-as.double(substr(Crime$ViolentCrime,1,6))
Crime1<-Crime[sample(1:nrow(Crime),100),]
Crime1
Crime2<-Crime[sample(1:nrow(Crime),100),]
Crime2
#(2)Crime1의 ViolentCrime열의 NA값들을 삭제한 후 as.double을 이용해 실수로 변환하여 평균값을 계산해주었다. 같은 방법으로 Crime2의 평균값을 계산해주었다.
mean(as.double(Crime1$ViolentCrime),na.rm = TRUE)
mean(as.double(Crime2$ViolentCrime),na.rm = TRUE)
#Crime1의 평균은 380.903이고 Crime2의 평균은 357.273인 것으로 보아 비슷하다고 생각 할 수 있다. 보다 정확히 비교하기 위해 전체 Crime데이터의 ViolentCrime열의 평균값을 도출한 결과, 368.4268이 나온것으로 보아 두 값이 평균으로부터 비슷한 위치에 떨어져 있다는 것과 평균값과 근사한 값이 나왔으므로 similar하다는 분석 결과를 도출할 수 있다.
mean(as.double(Crime$ViolentCrime),na.rm = TRUE)

### Question.10

#(1)gsub 함수를 통해 Crime$PropertyCrime에 ","를 없애주었다.substr함수를 사용해 value들의 길이를 통일 시켜주었고 character였던 값들을 소수점 표현이 가능하도록 as.double를 통해 실수로 변경해주었다. Crime 데이터의 PropertyCrime 열의 75%에 대한 정규화를 NA값을 제외하고 계산해주고, 이 값(3229.7)보다 PropertyCrime 열의 값이 큰 데이터들을 첫번째 열을 제외하고 데이터 프레임의 형태로 추출하여 PropertyCrime 변수에 저장해준다. PropertyCrime 데이터 프레임의 PropertyCrime 열을 내림차순의 형태로 인덱싱하여 반환된 값을 PropertyCrime 데이터 프레임에 다시 대입해 정렬해주고 위에서 6개의 행만 출력해준다. 
Crime$PropertyCrime<-gsub(",","",Crime$PropertyCrime)
Crime$PropertyCrime<-as.double(substr(Crime$PropertyCrime,1,6))
PropertyCrime<-subset(Crime,PropertyCrime>quantile(Crime$PropertyCrime,0.75, na.rm = TRUE),select=-1)
head(PropertyCrime[order(PropertyCrime$PropertyCrime,decreasing = TRUE),])
#(2)PropertyCrime 데이터 프레임의 State열 별로 table함수를 이용해 분류했고, 이를 sort함수를 이용해 내림차순으로 정렬했다.
sort(table(PropertyCrime$State),decreasing = TRUE)

### Question.11


#which.max함수를 이용해Crime 데이터의 ViolentCrime열의 최댓값이 존재하는 인덱스를 반환하고, 이 인덱스 값을 갖는 행의 "State"값을 반환해 주었다. 이를 통해 LA가 the worst state in the overall numver of violent crime 인 것을 도출 할 수 있었다.
Crime[which.max(Crime$ViolentCrime),"State"]

