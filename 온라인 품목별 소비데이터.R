# 0. Packages --------------------------------------------------------------
library(data.table)
library(dplyr)
library(stringr)

oo <- fread("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/신설법인 데이터.csv", encoding = "UTF-8") 
# 1. Read data ------------------------------------------------------------
##데이터는 집계(3월, 9월) 데이터이기 때문에 코로나 전, 후로 데이터를 분석할 예정. 
d1 <- fread("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/온라인 품목별 소비데이터.csv") 

colSums(is.na(d1)) #결측치는 존재하지 않음. 
d1$기준년 <- str_sub(d1$기준년월, 1, 4)
d1$품목대분류코드 <- as.numeric(d1$품목대분류코드)

cov_before <- d1 %>% 
  filter(기준년 == "2019") %>% 
  select(-c(2,4))
cov_after <- d1 %>% 
  filter(기준년 == "2020" | 기준년 == "2021") %>% 
  select(-c(2,4))

d2 <- fread("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/업종중분류코드.csv", encoding = "UTF-8")
d2$중분류코드 <- str_sub(d2$중분류, 2,3)

#d3 <- merge(d1, d2, by.x = d1$품목대분류코드, by.y = d2$중분류코드)
######################기업별로 산업별 중소기업 유무 판단 필요!!!!##########################

####지역도 나눠서 진행!!!!!!!

#########성별과 연령을 고려한 조합도 확인 필요!!!!
# 2. EDA ------------------------------------------------------------------
##코로나 발생 이전
#1.성별 비율 확인
sort(round(prop.table(table(cov_before$성별))*100,2)) #여성이 63%, 남성이 37%

#1-1. 성별에 따른 중분류명 트렌드 파악
#남성
t1 <- cov_before %>% 
  filter(성별 == "남성") 
o1<- as.data.frame(sort(round(prop.table(table(t1$품목중분류명))*100,2))) %>% 
  arrange(desc(Freq))
#o2o 서비스(배달의 민족, 직방, 카카오택시), 생활용품, 여행, e머니/상품권 순임. 

#여성
t2 <- cov_before %>% 
  filter(성별 == "여성") 
o2<- as.data.frame(sort(round(prop.table(table(t2$품목중분류명))*100,2))) %>% 
  arrange(desc(Freq))
#o2o 서비스(배달의 민족, 직방, 카카오택시), 생활용품, 신선/요리재료, 취미/특기, 건강식품순임. 


#2.연령 비율 확인 
sort(round(prop.table(table(cov_before$연령))*100,2)) # 30대, 40대, 20대, 50대 순임.

#2.1 연령에 따른 중분류명 트렌드 파악 
t3 <- cov_before %>% 
  filter(연령 == "20세 미만")
o3 <- as.data.frame(sort(round(prop.table(table(t3$품목중분류명))*100,2))) %>% 
  arrange(desc(Freq))
# o2o서비스, 취미/특기, e머니/상품권, 디지털, 스포츠의류



table(d1$연령)

#3.가구생애주기 비율 확인 
sort(round(prop.table(table(cov_before$가구생애주기))*100,2)) #신혼영융아가구, 1인가구, 초중고자녀가구, 성인자녀가구 순임.

#4.매출금액 요약통계량
summary(cov_before$매출금액)

#5.매출 건수 요약통계량
summary(cov_before$매출건수)

#6. 대분류명에 비율 확인
sort(round(prop.table(table(cov_before$품목대분류명))*100,2)) #e상품/서비스, 여가/스포츠, 식품, 출산/육아

#. 대분류명에 따른 중분류명 트렌드 파악
t1 <- cov_before %>% 
  filter(품목대분류명 == "e상품/서비스")
sort(round(prop.table(table(t1$품목중분류명))*100,2)) #o2o 서비스(배달의 민족, 직방, 카카오택시), e머니/상품권, 모바일상품 순임.

t2 <- cov_before %>% 
  filter(품목대분류명 == "여가/스포츠")
sort(round(prop.table(table(t2$품목중분류명))*100,2)) #취미/특기, 여행, 문화, 스포츠, 레저 순임.

t3 <- cov_before %>% 
  filter(품목대분류명 == "식품")
sort(round(prop.table(table(t3$품목중분류명))*100,2)) #신선/요리재료, 가공식품, 음료 순임.

t4 <- cov_before %>% 
  filter(품목대분류명 == "출산/육아")
sort(round(prop.table(table(t4$품목중분류명))*100,2)) #어린이용품서비스, 육아용품서비스, 임산부용품서비스, 베이비용품서비스 순임.  

#8. 성별에 따른 중분류명 트렌드 파악 


##코로나 발생 이후
#성별 비율 확인
sort(round(prop.table(table(cov_after$성별))*100,2)) #여성이 64%, 남성이 36%

#연령 비율 확인 
sort(round(prop.table(table(cov_after$연령))*100,2)) # 40대, 30대, 20대, 50대 순임.

#가구생애주기 비율 확인 
sort(round(prop.table(table(cov_after$가구생애주기))*100,2)) #신혼영융아가구, 1인가구, 초중고자녀가구, 성인자녀가구 순임.

#대분류명에 따른 중분류명 트렌드 파악
sort(round(prop.table(table(cov_after$품목대분류명))*100,2)) #e상품/서비스, 식품, 여가/스포츠, 출산/육아 순임.

t5 <- cov_after %>% 
  filter(품목대분류명 == "e상품/서비스")
sort(round(prop.table(table(t5$품목중분류명))*100,2)) #o2o 서비스(배달의 민족, 직방, 카카오택시), e머니/상품권, 모바일상품 순임.

t6 <- cov_after %>% 
  filter(품목대분류명 == "여가/스포츠")
sort(round(prop.table(table(t6$품목중분류명))*100,2)) #취미/특기, 여행, 스포츠, 문화, 레저 순임.

t7 <- cov_after %>% 
  filter(품목대분류명 == "식품")
sort(round(prop.table(table(t7$품목중분류명))*100,2)) #신선/요리재료, 가공식품, 음료 순임.

t8 <- cov_after %>% 
  filter(품목대분류명 == "출산/육아")
sort(round(prop.table(table(t8$품목중분류명))*100,2)) #육아용품서비스, 어린이용품서비스, 임산부용품서비스, 베이비용품서비스 순임.  














