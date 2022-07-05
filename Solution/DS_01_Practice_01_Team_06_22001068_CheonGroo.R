#########################################
### Course: Data Science              ###
### Semester: 2022-1                  ###
### Title: Practice 1                 ###
### Team: 06                          ###
### Member: 22001068 Cheon Groo       ###
###         21800643 Jeon Mingyu      ###
###         Byun Yongkyung, Lee Subin ###
#########################################

### Question.1

#fc 벡터를 만들어 축구팀 이름을 element 값으로 넣어준다.
fc<-c("Manchester United","Manchester City","Tottenham","FC JYP","PSG","Barcelona","Real Madrid","FC Wonderwoman")
fc

### Question.2

#my_rating 벡터를 만들어 각 축구팀별 점수를 element 값으로 넣어준다.
my_rating<-c(4.8,0.0,5.0,NA,4.1,3.2,3.8,4.9)
my_rating

### Question.3

#fifa_rating 벡터를 만들어 각 축구팀별 점수를 element 값으로 넣어준다.
fifa_rating<-c(3.4,4.8,2.7,NA,4.7,4.1,3.8,NA)
fifa_rating

### Question.4

#멤버들의 점수를 element로 가지는 새로운 벡터를 만든다. 행의 갯수가 4개이고 가로로 값이 채워지는 byrow 행렬을 만들어 각 멤버의 벡터를 행으로 넣어준다.
Byun<-c(3.8,1.8,0.1,NA,0.4,3.4,1.2,NA)
Cheon<-c(4.8,0.8,1.1,NA,1.4,4.4,2.2,NA)
Jeon<-c(1.3,4.2,4.3,NA,4.1,3.2,3.6,4.1)
Lee<-c(2.3,3.2,3.3,NA,3.1,2.2,2.6,3.1)
team_rating<-matrix(c(Byun,Cheon,Jeon,Lee),nrow=4, byrow=TRUE)
team_rating

### Question.5

#team_rating에서 NA인 element의 값을 0.0으로 정의하고, 각 행의 평균을 계산하여 stu_mean 벡터에 저장한다.
stu_mean<-rowMeans(team_rating,is.na(team_rating)==0.0)
stu_mean

### Question.6

#team_rating에서 NA인 element의 값을 0.0으로 정의하고, 각 열의 합을 계산하여 team_sum 벡터에 저장한다.
team_sum<-colSums(team_rating, is.na(team_rating)==0.0)
team_sum

### Question.7

#team_rating 행렬의 마지막 열 오른쪽에 stu_mean을 추가해 붙여준다. 
team_matrix_ed<-cbind(team_rating,stu_mean)
team_matrix_ed

### Question.8

#7번에서 만든 행렬에 team_sum 벡터를 행렬의 맨 마지막끝에 새로운 행으로 추가하여 새로운 team_matrix_ed 행렬 변수에 저장해준다.
team_matrix_ed<-rbind(team_matrix_ed,team_sum)
#7번에서 cbind를 실행한 후의 team_matrix_ed와 team_sum의 column 갯수가 일치하지 않아 warning 메시지가 뜬다.
#Warning message:
#In rbind(team_matrix_ed, team_sum) :
#  number of columns of result is not a multiple of vector length (arg 2)
team_matrix_ed

### Question.9

#8번에서 만든 행렬에서, 5행 9열의 값은 stu_mean의 합이므로 무의미한 데이터이기 때문에 NA로 대체하여 정의해준다.
team_matrix_ed[5,9]<-NA
team_matrix_ed

### Question.10

#team_matrix_ed_1이라는 새로운 행렬 변수에 9번에서 만든 team_matrix_ed 행렬을 복사해준다. team_name은 앞서 정의한 fc 벡터와 stu_mean 벡터를 element 값으로 갖도록 정의하고, 이렇게 만들어진 team_name을 colname으로 지정해준다. rowname은 멤버들의 이름과 team_sum을 element 값으로 갖는 student_name 벡터를 새로 정의하여 설정해준다.
team_matrix_ed_1<-team_matrix_ed
team_name<-c(fc,"stu_mean")
colnames(team_matrix_ed_1)<-team_name
student_name<-c("Byun","Cheon","Jeon","Lee","team_sum")
rownames(team_matrix_ed_1)<-student_name
team_matrix_ed_1

### Question.11

#10번에서 만든 행렬에서, 학생들의 정보만 나올수 있게 team_sum이 포함된 5번째 행을 제외시켜 저장해준다.
team_matrix_ed_1<-team_matrix_ed_1[-5,]
team_matrix_ed_1
