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

d2 <- fread("C:/Users/user/Desktop/2021_Financial_Data_Competition/Data/5.업종중분류코드.csv", encoding = "UTF-8")
d2$중분류코드 <- str_sub(d2$중분류, 2,3)

#d3 <- merge(d1, d2, by.x = d1$품목대분류코드, by.y = d2$중분류코드)
######################기업별로 산업별 중소기업 유무 판단 필요!!!!##########################


# 2. EDA ------------------------------------------------------------------
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


###############################################################################

#3. 지역별, 성별, 연령에 따른 중분류명 트렌드 파악 
trend_sex_age <- d1 %>% 
  group_by(기준년월,성별,연령, 품목중분류명) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  arrange(기준년월,성별, 연령, desc(percent))
trend_sex_age$percent <- round(trend_sex_age$percent,2)


#4.지역에 따른 가구생애주기 비율 확인 
p3 <- d1 %>% 
  group_by(기준년월,고객소재지_광역시도, 가구생애주기) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  arrange(기준년월,고객소재지_광역시도, 가구생애주기, desc(percent))
p3$percent <- round(p3$percent,2)


#4.1 지역별, 가구생애주기에 따른 중분류명 트렌드 파악
trend_family <- d1 %>% 
  group_by(기준년월, 가구생애주기, 품목중분류명) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  arrange(기준년월, 가구생애주기, desc(percent))
trend_family$percent <- round(trend_family$percent,2)


#5-1.지역별, 품복중분류명에 따른 매출금액 요약통계량
trend_sales <- d1 %>% 
  group_by(기준년월, 고객소재지_광역시도,품목중분류명) %>% 
  summarise(sales_mean = mean(매출금액)) %>% 
  arrange(기준년월, 고객소재지_광역시도,desc(sales_mean))
trend_sales$sales_mean <- round(trend_sales$sales_mean,2)


#6-1.지역별, 품복중분류명에 따른 매출건수 요약통계량
trend_sales_count <- d1 %>% 
  group_by(기준년월, 고객소재지_광역시도,품목중분류명) %>% 
  summarise(sales_count_mean = mean(매출건수)) %>% 
  arrange(기준년월, 고객소재지_광역시도,desc(sales_count_mean))
trend_sales_count$sales_count_mean <- round(trend_sales_count$sales_count_mean,2)











