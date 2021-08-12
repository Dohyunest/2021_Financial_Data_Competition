# 0. Packages --------------------------------------------------------------
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(readxl)

# 1. Read data ------------------------------------------------------------
data1 <- fread("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/신설법인 데이터.csv", encoding = "UTF-8", na.strings = c("", NA)) 
data2 <- fread("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/업종중분류코드.csv", encoding = "UTF-8", na.strings = c("", NA)) 
#na.strings를 통해 공백을 na로 다 처리

data3 <- read_xlsx("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/인구_2018_수정.xlsx") 
data3$지역 <- str_trim(data3$지역, side = "both")
data3$지역 <- str_sub(data3$지역, 1,2)
data3$년도 <- rep(2018, nrow(data3))

data4 <- read_xlsx("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/인구_2019_수정.xlsx") 
data4$지역 <- str_trim(data4$지역, side = "both")
data4$지역 <- str_sub(data4$지역, 1,2)
data4$년도 <- rep(2019, nrow(data4))

data5 <- read_xlsx("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/인구_2020_수정.xlsx") 
data5$지역 <- str_trim(data5$지역, side = "both")
data5$지역 <- str_sub(data5$지역, 1,2)
data5$년도 <- rep(2020, nrow(data5))

data6 <- rbind(data3, data4, data5)
data6$년도 <- as.character(data6$년도)



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

#3. 정제 데이터에 인구 데이터 추가
d9$설립일자 <- str_sub(d9$설립일자, 1, 4)

d10 <- merge(d9, data6, by.x = c("시도", "설립일자"), by.y = c("지역","년도"))
d11 <- d10[,c(3:11, 2,1,12:24)]
colnames(d11)[6] <- "중분류명"


#4. 시도별, 년도별, 중분류명 카운트 확인 
o1 <- d11 %>% 
  group_by(시도,설립일자,중분류명) %>% 
  summarise(count = n()) %>% 
  arrange(시도, 설립일자,desc(count))


#5. 시도별, 년도별,중분류별 자본금 확인 
o2 <- d11 %>% 
  group_by(시도,설립일자,중분류명) %>% 
  summarise(mean_money = mean(자본금)) %>% 
  arrange(시도, 설립일자,desc(mean_money)) 

write.csv(d11,"C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/신설법인 데이터_전처리.csv") 


#####할 것.
#최소 자본금이 필요한 업종 매핑 필요!!! 근데 확인하는게 쉽지 않음 
#최소 자본금을 설정한 기업과 설정하지 못한 기업 비율 확인!!!! (나중에 ui/ux 구현시 현황 같이 제시)
#지역별 지표 추가 (엑셀에서 정하기)
#

table(d11$시도)

