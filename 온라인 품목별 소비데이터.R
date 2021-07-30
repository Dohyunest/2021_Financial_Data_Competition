# 0. Packages --------------------------------------------------------------
library(data.table)
library(dplyr)
library(stringr)


# 1. Read data ------------------------------------------------------------
d1 <- fread("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/온라인 품목별 소비데이터.csv") 

colSums(is.na(d1)) #결측치는 존재하지 않음. 
d1$기준년 <- str_sub(d1$기준년월, 1, 4)


d2019 <- d1 %>% 
  filter(기준년 == "2019") %>% 
  select(-c(2,4))

d2020 <- d1 %>% 
  filter(기준년 == "2020") %>% 
  select(-c(2,4))

d2021 <- d1 %>% 
  filter(기준년 == "2021") %>% 
  select(-c(2,4))


# 2. EDA ------------------------------------------------------------------
#데이터는 집계(3월, 9월) 데이터이기 때문에 코로나 전, 후로 데이터를 분석할 예정. 
##2019년
sort(round(prop.table(table(d2019$품목대분류명))*100,2)) #e상품/서비스, 여가/스포츠, 식품, 출산/육아 순임. 
a1 <- d2019 %>% 
  filter(품목대분류명 == "e상품/서비스")
sort(round(prop.table(table(a1$품목중분류명))*100,2)) #o2o 서비스, e머니/상품권, 모바일상품 순임.

a2 <- d2019 %>% 
  filter(품목대분류명 == "여가/스포츠")
sort(round(prop.table(table(a2$품목중분류명))*100,2)) #취미/특기, 여행, 문화, 스포츠, 레저 순임.

a3 <- d2019 %>% 
  filter(품목대분류명 == "식품")
sort(round(prop.table(table(a3$품목중분류명))*100,2)) #신선/요리재료, 가공식품, 음료 순임.

a4 <- d2019 %>% 
  filter(품목대분류명 == "출산/육아")
sort(round(prop.table(table(a4$품목중분류명))*100,2)) #어린이용품서비스, 육아용품서비스, 임산부용품서비스, 베이비용품서비스 순임.  

#온라인과 오프라인의 경계를 무너뜨리고 서로 연동하는 결합형 비즈니스로, 인터넷이나 스마트폰을 이용해 오프라인 매장으로 고객을 끌어오고자 하는 서비스입니다.
#ex) 배달의 민족, 직방, 카카오택시 

#e머니란 상품등록. 유료옵션비. 상품구입시에 사용할 수 있는 사이버머니



sort(round(prop.table(table(d2019$성별))*100,2)) #여성이 63%, 남성이 37%
sort(round(prop.table(table(d2019$연령))*100,2)) # 30대, 40대, 20대 순임.
sort(round(prop.table(table(d2019$가구생애주기))*100,2)) #신혼영융아가구, 1인가구, 초중고자녀가구 순임.





#2020년
sort(round(prop.table(table(d2020$품목대분류명))*100,2)) #e상품/서비스, 식품, 여가/스포츠, 출산/육아 순임. ->  

sort(round(prop.table(table(d2020$성별))*100,2)) #여성이 64%, 남성이 36%
sort(round(prop.table(table(d2020$연령))*100,2)) # 40대, 30대, 20대 순임.
sort(round(prop.table(table(d2020$가구생애주기))*100,2)) #신혼영융아가구, 1인가구, 초중고자녀가구 순임.

#2021년
sort(round(prop.table(table(d2019$품목대분류명))*100,2)) #e상품/서비스, 여가/스포츠, 식품, 출산/육아  순임. ->

sort(round(prop.table(table(d2019$성별))*100,2)) #여성이 63%, 남성이 37%
sort(round(prop.table(table(d2019$연령))*100,2)) # 30대, 40대, 20대 순임.
sort(round(prop.table(table(d2019$가구생애주기))*100,2)) #신혼영융아가구, 1인가구, 초중고자녀가구 순임.









