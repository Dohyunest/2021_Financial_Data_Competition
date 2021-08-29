# 0. Packages --------------------------------------------------------------
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(readxl)

# 1. Read data ------------------------------------------------------------
data1 <- fread("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/1.신설법인 데이터.csv", encoding = "UTF-8", na.strings = c("", NA)) 
data2 <- fread("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/5.업종중분류코드.csv", encoding = "UTF-8", na.strings = c("", NA)) 
#na.strings를 통해 공백을 na로 다 처리

data3 <- read_xlsx("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/6.인구_2018_수정.xlsx") 
data3$지역 <- str_trim(data3$지역, side = "both")
data3$지역 <- str_sub(data3$지역, 1,2)
data3$년도 <- rep(2018, nrow(data3))

data4 <- read_xlsx("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/7.인구_2019_수정.xlsx") 
data4$지역 <- str_trim(data4$지역, side = "both")
data4$지역 <- str_sub(data4$지역, 1,2)
data4$년도 <- rep(2019, nrow(data4))

data5 <- read_xlsx("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/8.인구_2020_수정.xlsx") 
data5$지역 <- str_trim(data5$지역, side = "both")
data5$지역 <- str_sub(data5$지역, 1,2)
data5$년도 <- rep(2020, nrow(data5))

data6 <- rbind(data3, data4, data5)
data6$년도 <- as.character(data6$년도)

data7 <- fread("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/9.물가지수.csv", encoding = "UTF-8")
data7$년도 <- as.character(data7$년도)

data8 <- fread("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/10.상업용 부동산 임대동향.csv", encoding = "UTF-8")
data8$년도 <- as.character(data8$년도)

data9 <- fread("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/11.폐업자수.csv", encoding = "UTF-8")
data9$년도 <- as.character(data9$년도)

data10 <- fread("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/4.업종대분류코드.csv", encoding = "UTF-8")

data11 <- fread("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/12.지역별 폐업사유.csv", encoding = "UTF-8")
data11$년도 <- as.character(data11$년도)
data11$카운트 <- str_trim(data11$카운트, side = c("both"))
data11$카운트 <- as.numeric(data11$카운트)

data12 <- fread("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/13.대분류업종별 폐업사유.csv", encoding = "UTF-8")
data12$년도 <- as.character(data12$년도)
data12$카운트 <- as.numeric(data12$카운트)

# 2. EDA ------------------------------------------------------------------
#2-1. 주식수와 주당금액이 있는데 자본금이 없는 데이터가 존재. -> 주당금액이 모두 있는 데이터 정제 필요
#원데이터에서 자본금이 결측치인 자료 추출 -> 주식수와 주당금액을 곱해서 자본금 계산 -> 자본금이 결측치인 데이터 제거
d2 <- data1 %>% 
  filter(is.na(자본금) == TRUE)

d2$자본금 <- d2$주식수 * d2$주당금액

d3 <- d2 %>% 
  filter(is.na(자본금) == FALSE)

#원데이터에서 자본금이 결측치가 아닌 자료 추출 
d4 <- data1 %>% 
  filter(is.na(자본금) == FALSE)

#원데이터에서 자본금이 결측치가 아닌 자료 + 자본금을 구한 데이터 추가  = 자본금이 모두 있는 데이터 생성
d5 <- rbind(d3, d4)

#2-2.시도 결측치인 자료 제거
d6 <- d5 %>% 
  filter(is.na(시도) == FALSE)
colSums(is.na(d6))

#2-3. 대분류 코드 추가
d7 <- d6 %>% 
  mutate(업종코드_new = str_sub(업종코드,1,3))
d8 <- merge(d7, data2, by.x = "업종코드_new", by.y = "중분류")
d9 <- d8[,c(2,3,4,5,1,21,6:20)] 

#2-4. 정제 데이터에 인구 데이터 추가
d9$설립일자 <- str_sub(d9$설립일자, 1, 4)

d10 <- merge(d9, data6, by.x = c("시도", "설립일자"), by.y = c("지역","년도"))
d11 <- d10[,c(3:11, 2,1,12:24)]
colnames(d11)[6] <- "중분류명"


#2-5. 소비자물가지수, 생활물가지수 추가
d12 <- merge(d11, data7, by.x = c("시도","설립일자"), by.y = c("지역","년도"))
d13 <- d12[,c(3:11,2,1,12:26)]


#2-6. 부동산 임대료 추가
d14 <- merge(d13, data8, by.x = c("시도","설립일자"), by.y = c("지역","년도"))
d15 <- d14[,c(3:11,2,1,12:30)]
d15$오피스임대료 <- d15$오피스임대료*1000
d15$중대형상가임대료 <- d15$중대형상가임대료*1000
d15$소규모상가임대료 <- d15$소규모상가임대료*1000
d15$집합상가임대료 <- d15$집합상가임대료*1000


#2-7. 폐업자 수 
d16 <- merge(d15, data9, by.x = c("시도","설립일자"), by.y = c("지역","년도"))
d17 <- d16[,c(3:11, 2, 1, 12:31)]
d17$업종코드_new_new <- str_sub(d17$업종코드_new, 1, 1)


#2-8. 대분류 추가 
d18 <- merge(d17, data10, by.x = "업종코드_new_new", by.y = "대분류")
d19 <- d18[,c(2:5, 1,33,6:32)]
colnames(d19)[5] <- "업종대분류코드"
colnames(d19)[6] <- "대분류명"
colnames(d19)[7] <- "업종중분류코드"

#2-9. 시도별, 년도별, 중분류명 카운트 확인 
o1 <- d19 %>% 
  group_by(시도, 설립일자, 대분류명, 중분류명) %>% 
  summarise(n = n()) %>% 
  mutate(percent = round(n / sum(n) * 100,2))

#2-10. 시도별, 년도별,중분류별 평균자본금 확인 
o2 <- d19 %>% 
  group_by(시도,설립일자,대분류명,중분류명) %>% 
  summarise(mean_money = mean(자본금)) %>% 
  arrange(시도, 설립일자,desc(mean_money))

#2-11. 시도별, 년도별,중분류별 총자본금 확인 
o3 <- d19 %>% 
  group_by(시도,설립일자,대분류명,중분류명) %>% 
  summarise(sum_money = sum(자본금)) %>% 
  arrange(시도, 설립일자,desc(sum_money))


#2-11. 시도별, 년도별,중분류별 성별 비율 확인 
d20 <- d19[which(is.na(d19$성별구분) == FALSE),]
o4 <- d20 %>% 
  group_by(시도,설립일자,대분류명,중분류명, 성별구분) %>% 
  summarise(n = n()) %>% 
  mutate(percent = round(n / sum(n) * 100,2))


# 3. 트렌드 확인 ---------------------------------------------------------------
#3-1.지역별 폐업 사유
ggplot(data11, aes(x = 년도, y = 카운트, group = 폐업사유)) +
  geom_line(aes(color = 폐업사유)) +
  geom_point() +
  facet_grid( ~ data11$지역) +
  labs(title = "지역에 따른 폐업 사유", 
       x = "기준년월",
       y = "Count")


#3-2.대분류업종별 폐업 사유
ggplot(data12, aes(x = 년도, y = 카운트, group = 폐업사유)) +
  geom_line(aes(color = 폐업사유)) +
  geom_point() +
  facet_grid( ~ data12$지역) +
  labs(title = "대분류 업종별에 따른 폐업 사유", 
       x = "기준년월",
       y = "Count")

#data11은 폐업 기업 수 이며, data9는 폐업자 수 (즉 실직자 수)


#년도별, 대분류별  n수파악, 평균자본금 
