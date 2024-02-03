install.packages("tidyverse")
install.packages("reshape")
install.packages("caret")
library(reshape)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
getwd()
setwd("C:/Users/FACDAMM_founder/Desktop/R_programing_work")
getwd()

traffic1923 <- readRDS('traffic1923.rds')
head(traffic1923)

traffic1923_tcs <- traffic1923 %>% 
                    filter(tcshi_name == 'TCS')
head(traffic1923_tcs)

traffic1923_hi <- traffic1923 %>% 
  filter(tcshi_name == 'hi-pass')

head(traffic1923_hi)
#---------------------------------------------------------차종을 승용차와 트럭으로 구분하기 위해 합계 변수 생성 (car, truck)

#---------------------------------------------------------car, truck 변수 생성(하이패스 통행량 테이블)
traffic1923_hi <- traffic1923_hi %>% 
  mutate(car = vetype1 + vetype6)
traffic1923_hi <- traffic1923_hi %>% 
  mutate(truck = vetype2 + vetype3 + vetype4 + vetype5)
head(traffic1923_hi)

#---------------------------------------------------------car, truck 변수 생성(tcs 통행량 테이블)
traffic1923_tcs <- traffic1923_tcs %>% 
  mutate(car = vetype1 + vetype6)
traffic1923_tcs <- traffic1923_tcs %>% 
  mutate(truck = vetype2 + vetype3 + vetype4 + vetype5)
head(traffic1923_tcs)

#---------------------------------------------------------통행량 상위 10개 추출하여 내림차순 정렬 데이터프레임이름바꿈

#---------------------------------------------------------tcs

traffic1923_tcs_10 <- traffic1923_tcs %>%
  group_by(tname) %>%
  arrange(desc(totalve)) %>% 
  distinct(tname, .keep_all = T) %>% 
  head(10)

head(traffic1923_tcs_10)
traffic1923_tcs_10



#---------------------------------------------------------hi-pass
traffic1923_hi_10 <- traffic1923_hi %>%
  group_by(tname) %>%
  arrange(desc(totalve)) %>% 
  distinct(tname, .keep_all = T) %>% 
  head(10)

head(traffic1923_hi_10)
traffic1923_hi_10


#---------------------------------------------------------필요한 변수만 추출하여 테이블 새로 저장
#---------------------------------------------------------tcs
traffic1923_tcs_10_plot <- traffic1923_tcs_10 %>% 
  select(adate, code, tname, car, truck)
traffic1923_tcs_10_plot
#---------------------------------------------------------상위 10개 요금소 4년간 tcs 교통량(승용차, 화물차) df

#---------------------------------------------------------adate 컬럼 문자열에서 날짜형식으로 변환
traffic1923_tcs$adate <- as.Date(traffic1923_tcs$adate, "%Y%m%d")
str(traffic1923_tcs)

#---------------------------------------------------------1. 서울(code = 101, car), 이상치(outlier) 확인
                
traffic1923_tcs_101 <- traffic1923_tcs %>%
    filter(code == 101 | tname == '서울') %>% 
    select(adate, code, tname, car, truck)
head(traffic1923_tcs_101)
qplot(traffic1923_tcs_101$car, geom="boxplot")
qplot(traffic1923_tcs_101$truck, geom="boxplot")
#---------------------------------------------------------1. 서울(code = 101) 이상치(outlier) 제거
traffic1923_tcs_101$car <- ifelse(traffic1923_tcs_101$car >= 17500, NA,  traffic1923_tcs_101$car)
traffic1923_tcs_101$truck <- ifelse(traffic1923_tcs_101$truck >= 3000, NA,  traffic1923_tcs_101$truck)
str(traffic1923_tcs_101)
View(traffic1923_tcs_101)
model_minmax <- preProcess(x = traffic1923_tcs_101, method = 'range')
traffic1923_tcs_101_ratio <- predict(model_minmax, traffic1923_tcs_101)

#---------------------------------------------------------1. 서울(code = 101) 교통량 차트작성
ggplot(data = traffic1923_tcs_101_ratio, aes(x=adate, y=car, color = traffic1923_tcs_101_ratio$car)) + geom_point() + 
  geom_smooth(method = 'loess', color = 'yellow', span = 2) #---- car
ggplot(data = traffic1923_tcs_101, aes(x=adate, y=truck, color = traffic1923_tcs_101$truck)) + geom_point()#---- truck

#---------------------------------------------------------1. 서울(code = 101) 교통량 회귀분석
#adate 변수가 톨게이트 코드 하나당 2개씩(입, 출입구) 연결되어 있어 순차적 변수로 구성되어 있지 않음
# 현재 테이블로는 회귀분석시 오류 발생생

#---------------------------------------------------------2. 황간(code = 120)

traffic1923_tcs_120 <- traffic1923_tcs %>%
  filter(code == 120 | tname == '황간') %>% 
  select(adate, code, tname, car, truck)
head(traffic1923_tcs_120)
traffic1923_tcs_120 %>% 
  arrange(desc(truck))
qplot(traffic1923_tcs_120$car, geom="boxplot")
qplot(traffic1923_tcs_120$truck, geom="boxplot")

#---------------------------------------------------------2. 황간(code = 120) 이상치(outlier) 제거
traffic1923_tcs_120$car <- ifelse(traffic1923_tcs_120$car >= 1500, NA,  traffic1923_tcs_120$car)
traffic1923_tcs_120$truck <- ifelse(traffic1923_tcs_120$truck >= 150, NA,  traffic1923_tcs_120$truck)
#---------------------------------------------------------2. 황간(code = 120) 최종확인
head(traffic1923_tcs_120)
#---------------------------------------------------------2. 황간(code = 120) 교통량 차트작성
ggplot(data = traffic1923_tcs_120, aes(x=adate, y=car, color = traffic1923_tcs_120$car)) + geom_point() #----car
ggplot(data = traffic1923_tcs_120, aes(x=adate, y=truck, color = traffic1923_tcs_120$truck)) + geom_point() #----truck


#---------------------------------------------------------3. 신탄진(code = 113)

traffic1923_tcs_113 <- traffic1923_tcs %>%
  filter(code == 113 | tname == '신탄진') %>% 
  select(adate, code, tname, car, truck)
head(traffic1923_tcs_113)
traffic1923_tcs_113 %>% 
  arrange(desc(truck))
qplot(traffic1923_tcs_113$car, geom="boxplot")
qplot(traffic1923_tcs_113$truck, geom="boxplot")

#---------------------------------------------------------3. 신탄진(code = 113) 이상치(outlier) 제거
traffic1923_tcs_113$car <- ifelse(traffic1923_tcs_113$car >= 4000, NA,  traffic1923_tcs_113$car)
traffic1923_tcs_113$truck <- ifelse(traffic1923_tcs_113$truck >= 1200, NA,  traffic1923_tcs_113$truck)
#---------------------------------------------------------3. 신탄진(code = 113) 최종확인
head(traffic1923_tcs_113)
#---------------------------------------------------------3. 신탄진(code = 113) 교통량 차트작성
ggplot(data = traffic1923_tcs_113, aes(x=adate, y=car, color = traffic1923_tcs_113$car)) + geom_point() #----car
ggplot(data = traffic1923_tcs_113, aes(x=adate, y=truck, color = traffic1923_tcs_113$truck)) + geom_point() #----truck


#---------------------------------------------------------4. 북부산(code = 150)

traffic1923_tcs_150 <- traffic1923_tcs %>%
  filter(code == 150 | tname == '북부산') %>% 
  select(adate, code, tname, car, truck)
head(traffic1923_tcs_150)
traffic1923_tcs_150 %>% 
  arrange(desc(car))
qplot(traffic1923_tcs_150$car, geom="boxplot")
qplot(traffic1923_tcs_150$truck, geom="boxplot")

#---------------------------------------------------------4. 북부산(code = 150) 이상치(outlier) 제거
traffic1923_tcs_150$car <- ifelse(traffic1923_tcs_150$car >= 7000, NA,  traffic1923_tcs_150$car)
traffic1923_tcs_150$truck <- ifelse(traffic1923_tcs_150$truck >= 1000, NA,  traffic1923_tcs_150$truck)
#---------------------------------------------------------4. 북부산(code = 150) 최종확인
head(traffic1923_tcs_150)
#---------------------------------------------------------4. 북부산(code = 150) 교통량 차트작성
ggplot(data = traffic1923_tcs_150, aes(x=adate, y=car, color = traffic1923_tcs_150$car)) + geom_point() #----car
ggplot(data = traffic1923_tcs_150, aes(x=adate, y=truck, color = traffic1923_tcs_150$truck)) + geom_point() #----truck


#---------------------------------------------------------5. 군포(code = 215)

traffic1923_tcs_215 <- traffic1923_tcs %>%
  filter(code == 215 | tname == '군포') %>% 
  select(adate, code, tname, car, truck)
head(traffic1923_tcs_215)
traffic1923_tcs_215 %>% 
  arrange(desc(truck))
qplot(traffic1923_tcs_215$car, geom="boxplot")
qplot(traffic1923_tcs_215$truck, geom="boxplot")

#---------------------------------------------------------5. 군포(code = 215) 이상치(outlier) 제거
traffic1923_tcs_215$car <- ifelse(traffic1923_tcs_215$car >= 5000, NA,  traffic1923_tcs_215$car)
traffic1923_tcs_215$truck <- ifelse(traffic1923_tcs_215$truck >= 600, NA,  traffic1923_tcs_215$truck)
#---------------------------------------------------------5. 군포(code = 215) 최종확인
head(traffic1923_tcs_215)
#---------------------------------------------------------5. 군포(code = 215) 교통량 차트작성
ggplot(data = traffic1923_tcs_215, aes(x=adate, y=car, color = traffic1923_tcs_215$car)) + geom_point() #----car
ggplot(data = traffic1923_tcs_215, aes(x=adate, y=truck, color = traffic1923_tcs_215$truck)) + geom_point() #----truck


#---------------------------------------------------------6. 구리남양주(code = 62)

traffic1923_tcs_62 <- traffic1923_tcs %>%
  filter(code == 62 | tname == '구리남양주') %>% 
  select(adate, code, tname, car, truck)
head(traffic1923_tcs_62)
traffic1923_tcs_62 %>% 
  arrange(desc(truck))
qplot(traffic1923_tcs_62$car, geom="boxplot")
qplot(traffic1923_tcs_62$truck, geom="boxplot")

#---------------------------------------------------------6. 구리남양주(code = 62) 이상치(outlier) 제거
traffic1923_tcs_62$car <- ifelse(traffic1923_tcs_62$car >= 20000, NA,  traffic1923_tcs_62$car)
traffic1923_tcs_62$truck <- ifelse(traffic1923_tcs_62$truck >= 4000, NA,  traffic1923_tcs_62$truck)
#---------------------------------------------------------6. 구리남양주(code = 62) 최종확인
head(traffic1923_tcs_62)
#---------------------------------------------------------6. 구리남양주(code = 62) 교통량 차트작성
ggplot(data = traffic1923_tcs_62, aes(x=adate, y=car, color = traffic1923_tcs_62$car)) + geom_point() #----car
ggplot(data = traffic1923_tcs_62, aes(x=adate, y=truck, color = traffic1923_tcs_62$truck)) + geom_point() #----truck

#---------------------------------------------------------7. 김포(code = 68)

traffic1923_tcs_68 <- traffic1923_tcs %>%
  filter(code == 68 | tname == '김포') %>% 
  select(adate, code, tname, car, truck)
head(traffic1923_tcs_68)
traffic1923_tcs_68 %>% 
  arrange(desc(truck))
qplot(traffic1923_tcs_68$car, geom="boxplot")
qplot(traffic1923_tcs_68$truck, geom="boxplot")

#---------------------------------------------------------7. 김포(code = 68) 이상치(outlier) 제거
traffic1923_tcs_68$car <- ifelse(traffic1923_tcs_68$car >= 18200, NA,  traffic1923_tcs_68$car)
traffic1923_tcs_68$truck <- ifelse(traffic1923_tcs_68$truck >= 5000, NA,  traffic1923_tcs_68$truck)
#---------------------------------------------------------7. 김포(code = 68) 최종확인
head(traffic1923_tcs_68)
#---------------------------------------------------------7. 김포(code = 68) 교통량 차트작성
ggplot(data = traffic1923_tcs_68, aes(x=adate, y=car, color = traffic1923_tcs_68$car)) + geom_point() #----car
ggplot(data = traffic1923_tcs_68, aes(x=adate, y=truck, color = traffic1923_tcs_68$truck)) + geom_point() #----truck

#---------------------------------------------------------8. 서서울(code = 253)

traffic1923_tcs_253 <- traffic1923_tcs %>%
  filter(code == 253 | tname == '서서울') %>% 
  select(adate, code, tname, car, truck)
head(traffic1923_tcs_253)
traffic1923_tcs_253 %>% 
  arrange(desc(car))
qplot(traffic1923_tcs_253$car, geom="boxplot")
qplot(traffic1923_tcs_253$truck, geom="boxplot")

#---------------------------------------------------------8. 서서울(code = 253) 이상치(outlier) 제거
traffic1923_tcs_253$car <- ifelse(traffic1923_tcs_253$car >= 20000, NA,  traffic1923_tcs_253$car)
traffic1923_tcs_253$truck <- ifelse(traffic1923_tcs_253$truck >= 3500, NA,  traffic1923_tcs_253$truck)
#---------------------------------------------------------8. 서서울(code = 253) 최종확인
head(traffic1923_tcs_253)
#---------------------------------------------------------8. 서서울(code = 68) 교통량 차트작성
ggplot(data = traffic1923_tcs_253, aes(x=adate, y=car, color = traffic1923_tcs_253$car)) + geom_point() #----car
ggplot(data = traffic1923_tcs_253, aes(x=adate, y=truck, color = traffic1923_tcs_253$truck)) + geom_point() #----truck


#---------------------------------------------------------9. 수원신갈(code = 103)

traffic1923_tcs_103 <- traffic1923_tcs %>%
  filter(code == 103 | tname == '수원신갈') %>% 
  select(adate, code, tname, car, truck)
head(traffic1923_tcs_103)
traffic1923_tcs_103 %>% 
  arrange(desc(car))
qplot(traffic1923_tcs_103$car, geom="boxplot")
qplot(traffic1923_tcs_103$truck, geom="boxplot")

#---------------------------------------------------------9. 수원신갈(code = 103) 이상치(outlier) 제거
traffic1923_tcs_103$car <- ifelse(traffic1923_tcs_103$car >= 8000, NA,  traffic1923_tcs_103$car)
traffic1923_tcs_103$truck <- ifelse(traffic1923_tcs_103$truck >= 2000, NA,  traffic1923_tcs_103$truck)
#---------------------------------------------------------9. 수원신갈(code = 103) 최종확인
head(traffic1923_tcs_103)
#---------------------------------------------------------9. 수원신갈(code = 103) 교통량 차트작성
ggplot(data = traffic1923_tcs_103, aes(x=adate, y=car, color = traffic1923_tcs_103$car)) + geom_point() #----car
ggplot(data = traffic1923_tcs_103, aes(x=adate, y=truck, color = traffic1923_tcs_103$truck)) + geom_point() #----truck


#---------------------------------------------------------10. 동서울(code = 190)

traffic1923_tcs_190 <- traffic1923_tcs %>%
  filter(code == 190 | tname == '동서울') %>% 
  select(adate, code, tname, car, truck)
head(traffic1923_tcs_190)
traffic1923_tcs_190 %>% 
  arrange(desc(truck))
qplot(traffic1923_tcs_190$car, geom="boxplot")
qplot(traffic1923_tcs_190$truck, geom="boxplot")

#---------------------------------------------------------10. 동서울(code = 190) 이상치(outlier) 제거
traffic1923_tcs_190$car <- ifelse(traffic1923_tcs_190$car >= 15000, NA,  traffic1923_tcs_190$car)
traffic1923_tcs_190$truck <- ifelse(traffic1923_tcs_190$truck >= 2900, NA,  traffic1923_tcs_190$truck)
#---------------------------------------------------------10. 동서울(code = 190) 최종확인
head(traffic1923_tcs_190)
#---------------------------------------------------------10. 동서울(code = 190) 교통량 차트작성
ggplot(data = traffic1923_tcs_190, aes(x=adate, y=car, color = traffic1923_tcs_190$car)) + geom_point() #----car
ggplot(data = traffic1923_tcs_190, aes(x=adate, y=truck, color = traffic1923_tcs_190$truck)) + geom_point() #----truck



#---------------------------------------------------------hi-pass
traffic1923_hi_10_plot <- traffic1923_hi_10 %>% 
  select(adate, code, tname, car, truck)
traffic1923_hi_10_plot

#---------------------------------------------------------y축 자르기 사전작업 (car)
traffic1923_tcs_10_plot$car <- as.numeric(traffic1923_tcs_10_plot$car)
vec_diff = 20000:max(traffic1923_tcs_10_plot$car) 
traffic1923_tcs_10_plot_1 = traffic1923_tcs_10_plot
traffic1923_tcs_10_plot_1[1, "car"] = quantile(vec_diff, probs = 0.3)#--------------변수가 integer라서 double로 고쳐야 함


#---------------------------------------------------------y축 자르기 사전작업 (truck)
traffic1923_tcs_10_plot$truck <- as.numeric(traffic1923_tcs_10_plot$truck)
vec_diff1 = 500000:max(traffic1923_tcs_10_plot$truck) 
traffic1923_tcs_10_plot_1 = traffic1923_tcs_10_plot
traffic1923_tcs_10_plot_1[1, "truck"] = quantile(vec_diff1, probs = 0.3)


#---------------------------------------------------------tcs 차종별 교통량 순위 그래프 그려보기(car)
ggplot(data = traffic1923_tcs_10_plot_1, aes(x=reorder(tname, -car), y=car, fill=tname)) + geom_col() + 
  scale_y_continuous(limits = c(0, max(traffic1923_hi_10_plot$car))) + annotate(geom = "rect", xmin=0.5, xmax=1.4,
                                                                                ymin = quantile(vec_diff, probs = 0.45),
                                                                                ymax = max(traffic1923_hi_10_plot$car),
                                                                                fill = "blue")

#---------------------------------------------------------tcs 차종별 교통량 순위 그래프 그려보기(truck)
ggplot(data = traffic1923_tcs_10_plot_1, aes(x=reorder(tname, -truck), y=car, fill=tname)) + geom_col() + 
  scale_y_continuous(limits = c(0, max(traffic1923_hi_10_plot$truck))) + annotate(geom = "rect", xmin=0.5, xmax=1.4,
                                                                                ymin = quantile(vec_diff1, probs = 0.45),
                                                                                ymax = max(traffic1923_hi_10_plot$truck),
                                                                                fill = "green")

#---------------------------------------------------------hi-pass 차종별 교통량 순위 그래프 그려보기
ggplot(data = traffic1923_hi_10_plot, aes(x=reorder(tname, -car), y=car, fill=tname)) + geom_col()
ggplot(data = traffic1923_hi_10_plot, aes(x=reorder(tname, -truck), y=truck, fill=tname)) + geom_col()

