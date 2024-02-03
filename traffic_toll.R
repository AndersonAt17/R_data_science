
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
traffic20233 <- read.csv("traffic20233.csv", fileEncoding = "euc-kr")
traffic20233
head(traffic20233)
str(traffic20233)

traffic20233_mod <- traffic20233
str(traffic20233_mod)

traffic20233_mod <- rename(traffic20233_mod, adate=집계일자, code=영업소코드,
                           tname=영업소명, ex=입출구구분코드, exname=입출구명,
                           tcshi=TCS하이패스구분코드, tcshi_name=TCS하이패스명,
                           aa=고속도로운영기관구분코드, bb=고속도로운영기관명,
                           tollcode=영업형태구분코드, tollname=영업형태명,
                           vetype1=X1종교통량, vetype2=X2종교통량, vetype3=X3종교통량,
                           vetype4=X4종교통량, vetype5=X5종교통량, vetype6=X6종교통량,
                           totalve=총교통량)
str(traffic20233_mod)
traffic20233qt <- traffic20233_mod
traffic20233qt$ex <- NULL
traffic20233qt$exname <- NULL
traffic20233qt$aa <- NULL
traffic20233qt$bb <- NULL
str(traffic20233qt)
table(traffic20233qt$tollcode)

saveRDS(traffic20233qt, file="traffic20233qt.rds")
traffic20233qt <- readRDS("traffic20233qt.rds")
str(traffic20233qt)



#-------------------------------- whole table binding

traffic1923 <- rbind(traffic20191qt, traffic20192qt, traffic20193qt, traffic20194qt, 
                     traffic20201qt, traffic20202qt, traffic20203qt, traffic20204qt,
                     traffic20211qt, traffic20212qt, traffic20213qt, traffic20214qt,
                     traffic20221qt, traffic20222qt, traffic20223qt, traffic20224qt,
                     traffic20231qt, traffic20232qt, traffic20233qt)
head(traffic1923)
str(traffic1923)
saveRDS(traffic1923, file="traffic1923.rds")
#---------------------------------- open rds file as traffic1923
traffic1923 <- readRDS("traffic1923.rds")
str(traffic1923)
head(traffic1923)
tail(traffic1923)
max(traffic1923$totalve)
min(traffic1923$totalve)
mean(traffic1923$totalve)
median(traffic1923$totalve)
table(traffic1923$tcshi)

#---------------------------------- filter by tcs or hipass and make separate each column and table


#---------------------------------- tcs table
traffic1923_tcs <- subset(traffic1923, tcshi==1)
head(traffic1923_tcs, 11)
str(traffic1923_tcs)
tail(traffic1923_tcs)
table(traffic1923_tcs$tname)
table(traffic1923_tcs$adate)
traffic1923_tcs$adate <- as.Date(as.character(traffic1923_tcs$adate), format = "%Y%m%d")
summary(traffic1923_tcs)
str(traffic1923_tcs)
head(traffic1923_tcs)


#----------------------------------- hipass table

traffic1923_hi <- subset(traffic1923, tcshi==2)
head(traffic1923_hi, 11)
traffic1923_hi$adate <- as.Date(traffic1923_hi$adate, "%Y%m%d")
head(traffic1923_hi)
tail(traffic1923_hi)
str(traffic1923)


typeof(traffic1923$adate)


adate <- as.Date(traffic1923$adate)


