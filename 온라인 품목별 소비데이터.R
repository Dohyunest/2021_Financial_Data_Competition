# 0. Packages --------------------------------------------------------------
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

# 1. Read data ------------------------------------------------------------
##데이터는 집계(3월, 9월) 데이터이기 때문에 코로나 전, 후로 데이터를 분석할 예정. 
d1 <- fread("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/2.온라인 품목별 소비데이터.csv") 

colSums(is.na(d1)) #결측치는 존재하지 않음. 
d1$기준년 <- str_sub(d1$기준년월, 1, 4)
d1$품목대분류코드 <- as.numeric(d1$품목대분류코드)
d1$기준년월 <- as.character(d1$기준년월)
#해당 데이터의 대분류,중분류명은 표준산업코드가 아닌 BC카드에서 자체적으로 만든 분류명으로 표준산업분류코드와 같이 활용하기 어려움. 


# 2. 지역별 EDA ------------------------------------------------------------------
#2-0. 년도별, 대분류명별, 중분류명별 평균매출금액(총 매출금액) / (총 매출건수) 계산
r1 <- d1 %>% 
  group_by(기준년월, 고객소재지_광역시도, 품목대분류명) %>% 
  summarise(총매출건수 = sum(매출건수), 총매출금액 = sum(매출금액)) %>% 
  mutate(평균매출금액 = 총매출금액 / 총매출건수) 

  
r2 <- d1 %>% 
  group_by(기준년월, 고객소재지_광역시도, 품목대분류명, 품목중분류명) %>% 
  summarise(총매출건수 = sum(매출건수), 총매출금액 = sum(매출금액)) %>% 
  mutate(평균매출금액 = 총매출금액 / 총매출건수) 


#2-1. 년도별, 대분류명별,중분류명별 평균매출 건수
r3 <- d1 %>% 
  group_by(기준년월, 고객소재지_광역시도, 품목대분류명) %>% 
  summarise(평균매출건수 = mean(매출건수))


r4 <- d1 %>% 
  group_by(기준년월, 고객소재지_광역시도, 품목대분류명, 품목중분류명) %>% 
  summarise(평균매출건수 = mean(매출건수))


#2-2. 년도별, 대분류별, 중분류별 성별 비율 확인
r5 <- d1 %>% 
  group_by(기준년월, 고객소재지_광역시도, 품목대분류명, 성별) %>% 
  summarise(n = n()) %>% 
  mutate(percent = round(n / sum(n) * 100,2))

r6 <- d1 %>% 
  group_by(기준년월, 고객소재지_광역시도, 품목대분류명, 품목중분류명,성별) %>% 
  summarise(n = n()) %>% 
  mutate(percent = round(n / sum(n) * 100,2))

#2-3. 년도별, 대분류별,중분류명별 가구생애주기 비율 확인 
r7 <- d1 %>% 
  group_by(기준년월, 고객소재지_광역시도, 품목대분류명, 가구생애주기) %>% 
  summarise(n = n()) %>% 
  mutate(percent = round(n / sum(n) * 100,2))


r8 <- d1 %>% 
  group_by(기준년월, 고객소재지_광역시도, 품목대분류명, 품목중분류명, 가구생애주기) %>% 
  summarise(n = n()) %>% 
  mutate(percent = round(n / sum(n) * 100,2))

#2-4. 년도별, 대분류별,중분류명별 연령 비율 확인 
r9 <- d1 %>% 
  group_by(기준년월, 고객소재지_광역시도, 품목대분류명, 연령) %>% 
  summarise(n = n()) %>% 
  mutate(percent = round(n / sum(n) * 100,2))

r10 <- d1 %>% 
  group_by(기준년월, 고객소재지_광역시도, 품목대분류명, 품목중분류명, 연령) %>% 
  summarise(n = n()) %>% 
  mutate(percent = round(n / sum(n) * 100,2))

#2-5. 201903과 202103의 중분류별 총매출금액, 총매출건수 증감률 계산
r11 <- r2 %>% 
  filter(기준년월 == "201903" | 기준년월 == "202103") %>% 
  arrange(기준년월, 품목대분류명, 품목중분류명)

r11_seoul <- r11 %>% 
  filter(고객소재지_광역시도 == "서울특별시")
r11_gyeonggido <- r11 %>% 
  filter(고객소재지_광역시도 == "경기도")
r11_incheon <- r11 %>% 
  filter(고객소재지_광역시도 == "인천광역시")

#서울 증감률
a1 <- r11_seoul %>% 
  filter(기준년월 == "201903")
a2 <- r11_seoul %>% 
  filter(기준년월 == "202103") 
a2[which(a2$품목중분류명 %in% a1$품목중분류명 == FALSE),]
#서울 데이터 중에서 대분류 건강 중에서 중분류 다이어트가 201903에 없음. 

r11_seoul_f <- r11_seoul %>% 
  filter(품목중분류명 != "다이어트")
r11_seoul_result <- data.frame(rep(NA, nrow(r11_seoul_f)/2), rep(NA, nrow(r11_seoul_f)/2), rep(NA, nrow(r11_seoul_f)/2),rep(NA, nrow(r11_seoul_f)/2))

for (i in 1:(nrow(r11_seoul_f)/2)) {
  if (r11_seoul_f[i,4] == r11_seoul_f[i+49,4]) {
    r11_seoul_result[i,1] <- r11_seoul_f[i,3]
    r11_seoul_result[i,2] <- r11_seoul_f[i,4]
    r11_seoul_result[i,3] <- round((r11_seoul_f[i+49,5] - r11_seoul_f[i, 5]) / r11_seoul_f[i, 5] * 100,2)
    r11_seoul_result[i,4] <- round((r11_seoul_f[i+49,6] - r11_seoul_f[i, 6]) / r11_seoul_f[i, 6] * 100,2)
  }
}

colnames(r11_seoul_result)[1] <- "품목대분류명"
colnames(r11_seoul_result)[2] <- "품목중분류명"
colnames(r11_seoul_result)[3] <- "총매출건수 증감률"
colnames(r11_seoul_result)[4] <- "총매출금액 증감률"

r11_seoul_result$고객소재지_광역시도 <- rep("서울특별시", nrow(r11_seoul_result))
r11_seoul_result <- r11_seoul_result[,c(5,1:4)]

#경기도 증감률
a3 <- r11_gyeonggido %>% 
  filter(기준년월 == "201903")
a4 <- r11_gyeonggido %>% 
  filter(기준년월 == "202103") 
a4[which(a4$품목중분류명 %in% a3$품목중분류명 == FALSE),]
#경기도 데이터 중에서 대분류 건강 중에서 중분류 다이어트가 201903에 없음. 

r11_gyeonggido_f <- r11_gyeonggido %>% 
  filter(품목중분류명 != "다이어트")

r11_gyeonggido_result <- data.frame(rep(NA, nrow(r11_gyeonggido_f)/2), rep(NA, nrow(r11_gyeonggido_f)/2), rep(NA, nrow(r11_gyeonggido_f)/2),rep(NA, nrow(r11_gyeonggido_f)/2))

for (i in 1:(nrow(r11_gyeonggido_f)/2)) {
  if (r11_gyeonggido_f[i,4] == r11_gyeonggido_f[i+49,4]) {
    r11_gyeonggido_result[i,1] <- r11_gyeonggido_f[i,3]
    r11_gyeonggido_result[i,2] <- r11_gyeonggido_f[i,4]
    r11_gyeonggido_result[i,3] <- round((r11_gyeonggido_f[i+49,5] - r11_gyeonggido_f[i, 5]) / r11_gyeonggido_f[i, 5] * 100,2)
    r11_gyeonggido_result[i,4] <- round((r11_gyeonggido_f[i+49,6] - r11_gyeonggido_f[i, 6]) / r11_gyeonggido_f[i, 6] * 100,2)
  }
}

colnames(r11_gyeonggido_result)[1] <- "품목대분류명"
colnames(r11_gyeonggido_result)[2] <- "품목중분류명"
colnames(r11_gyeonggido_result)[3] <- "총매출건수 증감률"
colnames(r11_gyeonggido_result)[4] <- "총매출금액 증감률"

r11_gyeonggido_result$고객소재지_광역시도 <- rep("경기도", nrow(r11_gyeonggido_result))
r11_gyeonggido_result <- r11_gyeonggido_result[,c(5,1:4)]

#인천 증감률
a5 <- r11_incheon %>% 
  filter(기준년월 == "201903")
a6 <- r11_incheon %>% 
  filter(기준년월 == "202103") 
a6[which(a6$품목중분류명 %in% a5$품목중분류명 == FALSE),]
#인천 데이터 중에서 대분류 건강 중에서 중분류 다이어트가 201903에 없음 & 인천 데이터 중에서 대분류 기타 중에서 중분류 기부/후원이 201903에 없음

r11_incheon_f <- r11_incheon %>% 
  filter(품목중분류명 != "다이어트" | 품목중분류명 != "기부/후원")

r11_incheon_result <- data.frame(rep(NA, nrow(r11_incheon_f)/2), rep(NA, nrow(r11_incheon_f)/2), rep(NA, nrow(r11_incheon_f)/2), rep(NA, nrow(r11_incheon_f)/2))

for (i in 1:(nrow(r11_incheon_f)/2)) {
  if (r11_incheon_f[i,4] == r11_incheon_f[i+48,4]) {
    r11_incheon_result[i,1] <- r11_incheon_f[i,3]
    r11_incheon_result[i,2] <- r11_incheon_f[i,4]
    r11_incheon_result[i,3] <- round((r11_incheon_f[i+48,5] - r11_incheon_f[i, 5]) / r11_incheon_f[i, 5] * 100,2)
    r11_incheon_result[i,4] <- round((r11_incheon_f[i+48,6] - r11_incheon_f[i, 6]) / r11_incheon_f[i, 6] * 100,2)
  }
}
colnames(r11_incheon_result)[1] <- "품목대분류명"
colnames(r11_incheon_result)[2] <- "품목중분류명"
colnames(r11_incheon_result)[3] <- "총매출금액 증감률"
colnames(r11_incheon_result)[4] <- "총매출건수 증감률"
r11_incheon_result$고객소재지_광역시도 <- rep("인천광역시", nrow(r11_incheon_result))
r11_incheon_result <- r11_incheon_result[,c(5,1:4)]

r11_result <- rbind(r11_seoul_result, r11_gyeonggido_result, r11_incheon_result)


# 3. 전국 EDA ------------------------------------------------------------------
#3-0. 년도별, 대분류명별, 중분류명별 평균매출금액(총 매출금액) / (총 매출건수) 계산
w1 <- d1 %>% 
  group_by(기준년월, 품목대분류명) %>% 
  summarise(총매출건수 = sum(매출건수), 총매출금액 = sum(매출금액)) %>% 
  mutate(평균매출금액 = 총매출금액 / 총매출건수) 


w2 <- d1 %>% 
  group_by(기준년월, 품목대분류명, 품목중분류명) %>% 
  summarise(총매출건수 = sum(매출건수), 총매출금액 = sum(매출금액)) %>% 
  mutate(평균매출금액 = 총매출금액 / 총매출건수) 


#3-1. 년도별, 대분류명별,중분류명별 평균매출 건수
w3 <- d1 %>% 
  group_by(기준년월, 품목대분류명) %>% 
  summarise(평균매출건수 = mean(매출건수))

w4 <- d1 %>% 
  group_by(기준년월, 품목대분류명, 품목중분류명) %>% 
  summarise(평균매출건수 = mean(매출건수))


#3-2. 년도별, 대분류별, 중분류별 성별 비율 확인
w5 <- d1 %>% 
  group_by(기준년월, 품목대분류명, 성별) %>% 
  summarise(n = n()) %>% 
  mutate(percent = round(n / sum(n) * 100,2))

w6 <- d1 %>% 
  group_by(기준년월, 품목대분류명, 품목중분류명,성별) %>% 
  summarise(n = n()) %>% 
  mutate(percent = round(n / sum(n) * 100,2))

#3-3. 년도별, 대분류별,중분류명별 가구생애주기 비율 확인 
w7 <- d1 %>% 
  group_by(기준년월, 품목대분류명, 가구생애주기) %>% 
  summarise(n = n()) %>% 
  mutate(percent = round(n / sum(n) * 100,2))

w8 <- d1 %>% 
  group_by(기준년월, 품목대분류명, 품목중분류명, 가구생애주기) %>% 
  summarise(n = n()) %>% 
  mutate(percent = round(n / sum(n) * 100,2))

#3-4. 년도별, 대분류별,중분류명별 연령 비율 확인 
w9 <- d1 %>% 
  group_by(기준년월, 품목대분류명, 연령) %>% 
  summarise(n = n()) %>% 
  mutate(percent = round(n / sum(n) * 100,2))

w10 <- d1 %>% 
  group_by(기준년월, 품목대분류명, 품목중분류명, 연령) %>% 
  summarise(n = n()) %>% 
  mutate(percent = round(n / sum(n) * 100,2))

#3-5. 201903과 202103의 총매출금액, 총매출건수 증감률 계산
w11 <- w2 %>% 
  filter(기준년월 == "201903" | 기준년월 == "202103") %>% 
  arrange(기준년월, 품목대분류명, 품목중분류명)

a7 <- w11 %>% 
  filter(기준년월 == "201903")
a8 <- w11 %>% 
  filter(기준년월 == "202103") 
a8[which(a8$품목중분류명 %in% a7$품목중분류명 == FALSE),]
#전국 데이터 중에서 대분류 건강 중에서 중분류 다이어트가 201903에 없음.

w11_f <- w11 %>% 
  filter(품목중분류명 != "다이어트")

w11_result <- data.frame(rep(NA, nrow(w11_f)/2), rep(NA, nrow(w11_f)/2), rep(NA, nrow(w11_f)/2), rep(NA, nrow(w11_f)/2))
for (i in 1:(nrow(w11_f)/2)) {
  if (w11_f[i,3] == w11_f[i+49,3]) {
    w11_result[i,1] <- w11_f[i,2]
    w11_result[i,2] <- w11_f[i,3]
    w11_result[i,3] <- round((w11_f[i+49,4] - w11_f[i, 4]) / w11_f[i, 4] * 100,2)
    w11_result[i,4] <- round((w11_f[i+49,5] - w11_f[i, 5]) / w11_f[i, 5] * 100,2)
  }
}
colnames(w11_result)[1] <- "품목대분류명"
colnames(w11_result)[2] <- "품목중분류명"
colnames(w11_result)[3] <- "총매출금액 증감률"
colnames(w11_result)[4] <- "총매출건수 증감률"


# 4.시각화 -------------------------------------------------------------------
#1.지역에 따른 성별 비율 확인
p1 <- d1 %>% 
  group_by(기준년월, 고객소재지_광역시도, 성별) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  arrange(기준년월, 고객소재지_광역시도, 성별, desc(percent))
p1$percent <- round(p1$percent, 2)

#1-1. 지역에 따른 온라인 소비 성별 비율 시각화
ggplot(p1,aes(x = 기준년월, y = percent, group = 성별)) +
  geom_line(aes(color = 성별)) +
  geom_point() + 
  labs(title = "지역에 따른 온라인 소비 성별 비율 확인", 
       x = "기준년월", 
       y = "Percent(%)") +
  scale_y_continuous(limits = c(0,100)) +
  facet_grid(. ~ p1$고객소재지_광역시도)
  

#2. 지역별, 성별에 따른 중분류명 트렌드 파악
trend_sex <- d1 %>% 
  group_by(기준년월,고객소재지_광역시도, 성별, 품목중분류명) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  arrange(기준년월,고객소재지_광역시도, 성별, desc(percent))
trend_sex$percent <- round(trend_sex$percent,2)
# 중분류명으로는 시각화 불가하여 대분류명으로 대체


#2-1 지역별, 성별에 따른 대분류명 트렌드 파악 및 시각화 
trend_sex_f <- d1 %>% 
  group_by(기준년월,고객소재지_광역시도, 성별, 품목대분류명) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  arrange(기준년월,고객소재지_광역시도, 성별, desc(percent))
trend_sex_f$percent <- round(trend_sex_f$percent,2)

ggplot(trend_sex_f,aes(x = 기준년월, y = percent, group = 품목대분류명)) +
  geom_line(aes(color = 품목대분류명)) +
  geom_point() + 
  labs(title = "지역에 따른 온라인 소비 품목 트렌드 확인", 
       x = "기준년월", 
       y = "Percent(%)") +
  scale_y_continuous(limits = c(0,25)) +
  facet_grid(trend_sex_f$성별 ~ trend_sex_f$고객소재지_광역시도)


#3.지역에 따른 온라인 소비 연령 비율 확인 
p2 <- d1 %>% 
  group_by(기준년월,고객소재지_광역시도, 연령) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  arrange(기준년월,고객소재지_광역시도, 연령, desc(percent))
p2$percent <- round(p2$percent,2)


ggplot(p2,aes(x = 기준년월, y = percent, group = 연령)) +
  geom_line(aes(color = 연령)) +
  geom_point() + 
  labs(title = "지역에 따른 온라인 소비 성별 비율 확인", 
       x = "기준년월", 
       y = "Percent(%)") +
  scale_y_continuous(limits = c(0,100)) +
  facet_grid(. ~ p2$고객소재지_광역시도)


#3-1 지역별, 연령에 따른 중분류명 트렌드 파악 
trend_age <- d1 %>% 
  group_by(기준년월,고객소재지_광역시도, 연령, 품목중분류명) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  arrange(기준년월,고객소재지_광역시도, 연령, desc(percent))
trend_age$percent <- round(trend_age$percent,2)
# 중분류명으로는 시각화 불가하여 대분류명으로 대체

trend_age_f <- d1 %>% 
  group_by(기준년월,고객소재지_광역시도, 연령, 품목대분류명) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  arrange(기준년월,고객소재지_광역시도, 연령, desc(percent))
trend_age_f$percent <- round(trend_age_f$percent,2)

ggplot(trend_age_f,aes(x = 기준년월, y = percent, group = 품목대분류명)) +
  geom_line(aes(color = 품목대분류명)) +
  geom_point() + 
  labs(title = "지역에 따른 온라인 소비 품목 트렌드 확인", 
       x = "기준년월", 
       y = "Percent(%)") +
  scale_y_continuous(limits = c(0,25)) +
  facet_grid(trend_age_f$연령 ~ trend_age_f$고객소재지_광역시도)
