install.packages("foreign")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")
install.packages("tibble")
install.packages("openxlsx")
install.packages("plotrix")
install.packages("VIM")
# install.packages("mice")
# install.packages("missForest", dependencies = TRUE)
# install.packages("randomForest")

library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
library(tibble)
library(openxlsx)
library(plotrix)
library(VIM)
# library(mice)
# library(missForest)
# library("randomForest")

rm(list=ls())

# 데이터 불러오기
raw_data_2020 <- read.spss(file = "theSurvey2020.sav", reencode='utf-8', use.value.label = F, to.data.frame = T)

# 복사본 만들기
fare_data_2020 <- raw_data_2020

# head(fare_data)
# tail(fare_data)
# #view(fare_data)
# dim(fare_data)
# str(fare_data)
# summary(fare_data)

# ### 행 번호를 새로운 변수로 만들기
# # RowId 생성
# fare_data_rowid <- rowid_to_column(fare_data_2014)
# colnames(fare_data_rowid)[1]<-"Person_ID"

### 결과 변수, 설명 변수 추출 데이터 셋
final_2020 <- select(fare_data_2020, ANS_TYPE, H25, S5_1, HTYPE, B2_3, B8, B11_1, B11_2, B11_4, B11_5, B11_6, B11_7, B11_8, B11_9, B11_10, C1_2, C2_2, C3_2, C8, J2, J6a_4, F4, F6, F16_1, F16_2, H1, H12, H16_1, H16_2, H16_3, H16_4, H16_5, H16_6, H19_1, H19_2, H19_3, H19_4, H19_5, H19_6, H19_7, H19_8, H19_etc, H14_1_1, H14_1_3, H14_1_4, H14_1_5, H14_1_6, Q1, E1)
# 추가 E1(현재 경제활동 여부)


### 변수 검토
# 응답자 유형 (본인, 동거 대리자, 비동거 대리자)
class(fare_data_2020$ANS_TYPE)
table(fare_data_2020$ANS_TYPE)
# 응답자 유형 0만 추출
final_2020 <- final_2020[final_2020$ANS_TYPE == 0,]
# 결측치 확인
sum(is.na(fare_data_2020))
sum(is.na(final_2020))
# 이상치 결측 처리하기
#final$B2 <- ifelse(final$B2 >= 8, NA, final$B2)
final_2020$C1_2 <- ifelse(final_2020$C1_2 >= 8, NA, final_2020$C1_2)
final_2020$C2_2 <- ifelse(final_2020$C2_2 >= 8, NA, final_2020$C2_2)
final_2020$C3_2 <- ifelse(final_2020$C3_2 >= 8, NA, final_2020$C3_2)

final_2020$C8 <- ifelse(final_2020$C8 >= 8, NA, final_2020$C8)
#final$G3_2_4_a <- ifelse(final$G3_2_4_a >= 8, NA, final$G3_2_4_a)

final_2020$F4 <- ifelse(final_2020$F4 >= 8, NA, final_2020$F4)
final_2020$F6 <- ifelse(final_2020$F6 >= 8, NA, final_2020$F6)
#final$H10 <- ifelse(final$H10 >= 8, NA, final$H10)

final_2020$F16_1 <- ifelse(final_2020$F16_1 >= 98, NA, final_2020$F16_1)
final_2020$F16_2 <- ifelse(final_2020$F16_2 >= 98, NA, final_2020$F16_2)

final_2020$H16_1 <- ifelse(final_2020$H16_1 >= 8, NA, final_2020$H16_1)
final_2020$H16_2 <- ifelse(final_2020$H16_2 >= 8, NA, final_2020$H16_2)
final_2020$H16_3 <- ifelse(final_2020$H16_3 >= 8, NA, final_2020$H16_3)
final_2020$H16_4 <- ifelse(final_2020$H16_4 >= 8, NA, final_2020$H16_4)
final_2020$H16_5 <- ifelse(final_2020$H16_5 >= 8, NA, final_2020$H16_5)
final_2020$H16_6 <- ifelse(final_2020$H16_6 >= 8, NA, final_2020$H16_6)

final_2020$H19_1 <- ifelse(final_2020$H19_1 >= 8, NA, final_2020$H19_1)
final_2020$H19_2 <- ifelse(final_2020$H19_2 >= 8, NA, final_2020$H19_2)
final_2020$H19_3 <- ifelse(final_2020$H19_3 >= 8, NA, final_2020$H19_3)
final_2020$H19_4 <- ifelse(final_2020$H19_4 >= 8, NA, final_2020$H19_4)
final_2020$H19_5 <- ifelse(final_2020$H19_5 >= 8, NA, final_2020$H19_5)
final_2020$H19_6 <- ifelse(final_2020$H19_6 >= 8, NA, final_2020$H19_6)
final_2020$H19_7 <- ifelse(final_2020$H19_7 >= 8, NA, final_2020$H19_7)
final_2020$H19_8 <- ifelse(final_2020$H19_8 >= 8, NA, final_2020$H19_8)
final_2020$H19_etc <- ifelse(final_2020$H19_etc >= 8, NA, final_2020$H19_etc)

final_2020$H14_1_1 <- ifelse(final_2020$H14_1_1 >= 8, NA, final_2020$H14_1_1)
final_2020$H14_1_3 <- ifelse(final_2020$H14_1_3 >= 8, NA, final_2020$H14_1_3)
final_2020$H14_1_4 <- ifelse(final_2020$H14_1_4 >= 8, NA, final_2020$H14_1_4)
final_2020$H14_1_5 <- ifelse(final_2020$H14_1_5 >= 8, NA, final_2020$H14_1_5)
final_2020$H14_1_6 <- ifelse(final_2020$H14_1_6 >= 8, NA, final_2020$H14_1_6)

final_2020$E1 <- ifelse(final_2020$E1 >= 8, NA, final_2020$E1)
#final$L6 <- ifelse(final$L6 >= 8, NA, final$L6)
# 결측치를 포함한 행 제거
#final <- final[complete.cases(final),]
# 결측치 확인
sum(is.na(final_2020))
# 데이터 셋에서 결측치가 차지하는 비중 구하기
mean(is.na(final_2020))

final_copy_2020 <- final_2020

### 결과변수
# 자살 생각 여부
# 자살 생각 여부
class(final_2020$H25)
table(final_2020$H25)
# 1) 자살 생각 여부 변수 가공
## (1, 있다) -> 'Yes', (2, 없다) -> 'No'
final_2020$H25 = ifelse(final_2020$H25 == 1, 'Yes', 'No')

# 결과변수 table 만들기
class(final_2020$H25)
table(final_2020$H25)
thought_yes = 99
thought_no = 660
rate_yes = round(99 / (99 + 660) * 100, 2)
rate_no = round(660 / (99 + 660) * 100, 2)
#표 셀 생성
C11 = paste0(thought_no)
C12 = paste0(rate_no,'(%)')
C21 = paste0(thought_yes)
C22 = paste0(rate_yes,'(%)')
C31 = paste0(thought_no + thought_yes)
C32 = paste0(100,'(%)')
#표 생성
my_table_2020=rbind(data.frame(A=C11,B=C12),
                    data.frame(A=C21,B=C22),
                    data.frame(A=C31,B=C32))
colnames(my_table_2020)=c("Frequency(people)","Ratio(%)")
rownames(my_table_2020)=c("Suicidal idea(No)","Suicidal idea(Yes)", "Total")
## 표 png 파일로 저장
setwd("D:\\TeamProject")
png("Suicide_ideas_frequency_table_2020.png",width=1200,height=350,res=150)
#빈 plot 생성
plot.new()
plot.window(xlim=c(0.7,5),ylim=c(115,200))
#표출력
addtable2plot(0 ,5, my_table_2020,bty="o",display.rownames=TRUE,hlines=TRUE,
              vlines=TRUE,cex=2)
dev.off()


### 설명변수
# 1) 총 가구원 수 변수 가공
## 1 2 3 4 5 6 >=7
final_2020$S5_1 = ifelse(final_2020$S5_1 >= 7, '>= 7', final_2020$S5_1)

# 2) 노인 가구형태 변수 가공
## 노인독거가구 1 노인부부가구 2 3 자녀동거 노인가구 11 - 25 기타 노인가구 29 -
final_2020$HTYPE = ifelse(final_2020$HTYPE >= 29, '기타 노인가구',
                          ifelse(final_2020$HTYPE >= 11,'자녀동거 노인가구',
                                 ifelse(final_2020$HTYPE >= 2,'노인부부가구','노인독거가구')))
class(final_2020$HTYPE) # 확인용
table(final_2020$HTYPE)

# 3) 의사진단 만성질환 총 수 변수 가공
## [0] ->  ‘0’ , [1]-[3] -> ‘1 ~ 3’, [4] ~ [30] -> ‘>= 4’
final_2020$B2_3 = ifelse(final_2020$B2_3 >= 4, '>= 4',
                         ifelse(final_2020$B2_3 >= 1,'1 ~ 3', '0'))

# 4) (건강행태) 지난 1년 간 음주 빈도 변수 가공
## [0] -> ‘No’ (지난 1년간 음주 안함) , [1] - [7] -> ‘Yes’ (지난 1년간 음주 함)
final_2020$B8 = ifelse(final_2020$B8 >= 1, 'YES', 'No')

# 5) 영양상태 관리 변수 가공
## nutrient 새로운 data frame 만들기
nutrient_2020 <- select(final_copy_2020, B11_1, B11_2, B11_4, B11_5, B11_6, B11_7, B11_8, B11_9, B11_10)
## yes = 1 개수 구하기
nutrient_2020 <- rowSums(nutrient_2020[1:9] == 1, na.rm = TRUE)
## nutrientYesNum 변수 열 만들기
nutrient_2020 <- as.data.frame(nutrient_2020)
# nutrient <- cbind(nutrient, C5)
## [0] - [2] -> ‘영양상태 양호함’, [3] - -> ‘영양관리요함’
nutrient_2020 = ifelse(nutrient_2020 >= 3, '영양관리요함', '영양상태 양호함')

# 6) (간병 수발) 일상생활수행에 있어 도움 받음 여부 변수 가공
## (1, 있다) -> 'Yes', (2, 없다) -> 'No'
final_2020$C8 = ifelse(final_2020$C8 == 1, 'Yes', 'No')

# 7) (경제 상태) 국민기초생활보장수급자 여부 변수 가공
## [1] -> ‘Yes’, [2] - [3] - -> ‘No’
final_2020$J2 = ifelse(final_2020$J2 == 1, 'Yes', 'No')

# 8) (경제 상태) 본인 부채 유무 변수 가공
## (1, 있다) -> 'Yes', (2, 없다) -> 'No'
final_2020$J6a_4 = ifelse(final_2020$J6a_4 == 1, 'Yes', 'No')

# # 9) (경제 상태) 배우자 부채 유무 변수 가공
# ## (1, 있다) -> 'Yes', (2, 없다) -> 'No'
# final$G3_2_4_a = ifelse(final$G3_2_4_a == 1, 'Yes', 'No')

# # 10) (가구 경제 상태) 가구 부채 유무 변수 가공
# ## (1, 있다) -> 'Yes', (2, 없다) -> 'No'
# final$N5_4_1 = ifelse(final$N5_4_1 == 1, 'Yes', 'No')

# 11) (자녀와의 관계 및 가구 형태) 지난 1년간 자녀와의 갈등 경험 변수 가공
## (1, 있다) -> 'Yes', (2, 없다) -> 'No'
final_2020$F4 = ifelse(final_2020$F4 == 1, 'Yes', 'No')

# # 12) (배우자와의 관계 및 가구 형태) 지난 1개월간 부부 간 갈등 경험 변수 가공
# ## (1, 있다) -> 'Yes', (2, 없다) -> 'No'
# final$H10 = ifelse(final$H10 == 1, 'Yes', 'No')

# 13) 가깝게 지내는 형제/자매를 포함한 친/인척 수(존재 여부) 변수 가공
## [0] -> ‘No’ , [1] - -> ‘Yes’
final_2020$F16_1 = ifelse(final_2020$F16_1 == 0, 'No', 'Yes')

# 14) 가깝게 지내는 친구/이웃/지인 수(존재 여부) 변수 가공
## [0] -> ‘No’ , [1] - -> ‘Yes’
final_2020$F16_2 = ifelse(final_2020$F16_2 == 0, 'No', 'Yes')

# 15) (생활환경) 거주형태(가구) 변수 가공
## [1] -> ‘자가’, [2] -> ‘전세’,  [3] - [4] -> ‘월세’, [5] -> ‘무상’
final_2020$H1 = ifelse(final_2020$H1 == 5, '무상',
                       ifelse(final_2020$H1 >= 3,'월세',
                              ifelse(final_2020$H1 == 2,'전세','자가')))

#16) (생활환경) 낙상, 지난 1년간 경험 유무 변수 가공
## (1, 있다) -> 'Yes', (2, 없다) -> 'No'
final_2020$H12 = ifelse(final_2020$H12 == 1, 'Yes', 'No')

# 17) (노후 생활과 삶의 질) 일상생활 차별 경험 유무 변수 가공
## discrimination 새로운 data frame 만들기
discrimination_2020 <- select(final_copy_2020, H19_1, H19_2, H19_3, H19_4, H19_5, H19_6, H19_7, H19_8, H19_etc)
## yes = 1 개수 구하기
discrimination_2020 <- rowSums(discrimination_2020[1:9] == 1, na.rm = TRUE)
## discrimination 변수 열 만들기
discrimination_2020 <- as.data.frame(discrimination_2020)
## [0] -> ‘No’, [1] - -> ‘Yes’
discrimination_2020 = ifelse(discrimination_2020 >= 1, 'Yes', 'No')


# 18) 타인으로부터 신체적 고통을 당함 여부 변수 가공
## (1, 있다) -> 'Yes', (2, 없다) -> 'No'
final_2020$H14_1_1 = ifelse(final_2020$H14_1_1 == 1, 'Yes', 'No')

# 19) 타인으로 인해 감정을 상함 여부 변수 가공
## (1, 있다) -> 'Yes', (2, 없다) -> 'No'
final_2020$H14_1_3 = ifelse(final_2020$H14_1_3 == 1, 'Yes', 'No')

# 20) 타인으로부터 금전적 피해를 입음 여부 변수 가공
## (1, 있다) -> 'Yes', (2, 없다) -> 'No'
final_2020$H14_1_4 = ifelse(final_2020$H14_1_4 == 1, 'Yes', 'No')

# 21) 가족이나 보호자가 돌봐주지 않음 여부 변수 가공
## (1, 있다) -> 'Yes', (2, 없다) -> 'No'
final_2020$H14_1_5 = ifelse(final_2020$H14_1_5 == 1, 'Yes', 'No')

# 22) 가족이나 보호자의 방임 및 생활비 미지원 여부 변수 가공
## (1, 있다) -> 'Yes', (2, 없다) -> 'No'
final_2020$H14_1_6 = ifelse(final_2020$H14_1_6 == 1, 'Yes', 'No')

# # 23) 노인보호전문기관 인지 여부 변수 가공
# ## [1] -> '알고 있다', [2] -> '모른다'
# final$L6 = ifelse(final$L6 == 1, '알고 있다', '모른다')

# 24) 주택 종류 변수 가공
## [1] -> ‘단독주택’, [2] -> ‘아파트’,  [3] -> ‘연릭/다세대 주택’, [4] -> ‘기타’
final_2020$Q1 = ifelse(final_2020$Q1 == 4, '기타',
                       ifelse(final_2020$Q1 == 3,'연릭/다세대 주택',
                              ifelse(final_2020$Q1 == 2,'아파트','단독주택')))

# 25) 현재 경제활동 여부 변수 가공
## [1] -> 'Yes', [2]- -> 'No'
final_2020$E1 = ifelse(final_2020$E1 >= 2, 'No', 'Yes')


### 변수 가공한 거 합쳐서 최종 data frame 만들기
# nutrient, final에 합치기기
final_2020 <- cbind(final_2020, nutrient_2020)
final_2020 <- cbind(final_2020, discrimination_2020)

# 필요없는 변수 없애기
final_2020 <- select(final_2020, H25, S5_1, HTYPE, B2_3, B8, nutrient_2020, C1_2, C2_2, C3_2, C8, J2, J6a_4, F4, F6, F16_1, F16_2, H1, H12, H16_1, H16_2, H16_3, H16_4, H16_5, H16_6, discrimination_2020, H14_1_1, H14_1_3, H14_1_4, H14_1_5, H14_1_6, Q1, E1)

### 범주형 변수 Factor 로 바꾸기
final_2020$H25 <- as.factor(final_2020$H25)
final_2020$S5_1 <- as.factor(final_2020$S5_1)
final_2020$HTYPE <- as.factor(final_2020$HTYPE)
final_2020$B2_3 <- as.factor(final_2020$B2_3)
final_2020$B8 <- as.factor(final_2020$B8)
final_2020$nutrient_2020 <- as.factor(final_2020$nutrient_2020)
final_2020$C8 <- as.factor(final_2020$C8)
final_2020$J2 <- as.factor(final_2020$J2)
final_2020$J6a_4 <- as.factor(final_2020$J6a_4)
#final$G3_2_4_a <- as.factor(final$G3_2_4_a)
#final$N5_4_1 <- as.factor(final$N5_4_1)
final_2020$F4 <- as.factor(final_2020$F4)
#final$H10 <- as.factor(final$H10)
final_2020$F16_1 <- as.factor(final_2020$F16_1)
final_2020$F16_2 <- as.factor(final_2020$F16_2)
final_2020$H1 <- as.factor(final_2020$H1)
final_2020$H12 <- as.factor(final_2020$H12)
final_2020$discrimination_2020 <- as.factor(final_2020$discrimination_2020)
final_2020$H14_1_1 <- as.factor(final_2020$H14_1_1)
final_2020$H14_1_3 <- as.factor(final_2020$H14_1_3)
final_2020$H14_1_4 <- as.factor(final_2020$H14_1_4)
final_2020$H14_1_5 <- as.factor(final_2020$H14_1_5)
final_2020$H14_1_6 <- as.factor(final_2020$H14_1_6)
#final$L6 <- as.factor(final$L6)
final_2020$Q1 <- as.factor(final_2020$Q1)

final_2020$E1 <- as.factor(final_2020$E1)

# 결측치를 포함한 상태로 분석하면 NA 로 return
# 결측치 분포 확인
summary(final_2020)

# 처음 결측치 시각화
aggr(final_2020, cex.axis=.4, gap=3, prop=FALSE, numbers=TRUE, ylab=c("Number of Missings","Combinations"))
title(sub = "< 2020 Missing Data >")

# 가장 많은 결측치를 가지고 있는 변수 제거
final_2020 <- subset(final_2020, select=-C8)
summary(final_2020)
# 결측치를 포함한 행 제거
final_2020 <- final_2020[complete.cases(final_2020),]
# 결측치 확인
sum(is.na(final_2020))

# 최종 결측치 시각화
aggr(final_2020, cex.axis=.4, gap=3, prop=FALSE, numbers=TRUE, ylab=c("Number of Missings","Combinations"))
title(sub = "< 2020 Missing Data >")


write.xlsx(final_2020, sheetName="sheet1", file="2020_dataset.xlsx")


# 자살 생각 빈도와 비율표 계산
# 결과변수 table 만들기
class(final_2020$H25)
table(final_2020$H25)
thought_yes = 79
thought_no = 5629
rate_yes = round(thought_yes / (thought_yes + thought_no) * 100, 2)
rate_no = round(thought_no / (thought_yes + thought_no) * 100, 2)
#표 셀 생성
C11 = paste0(thought_no)
C12 = paste0(rate_no,'(%)')
C21 = paste0(thought_yes)
C22 = paste0(rate_yes,'(%)')
C31 = paste0(thought_no + thought_yes)
C32 = paste0(100,'(%)')
#표 생성
my_table_2020=rbind(data.frame(A=C11,B=C12),
                    data.frame(A=C21,B=C22),
                    data.frame(A=C31,B=C32))
colnames(my_table_2020)=c("Frequency(people)","Ratio(%)")
rownames(my_table_2020)=c("Suicidal idea(No)","Suicidal idea(Yes)", "Total")
## 표 png 파일로 저장
setwd("D:\\TeamProject")
png("Suicide_ideas_frequency_table_2020.png",width=1200,height=350,res=150)
#빈 plot 생성
plot.new()
plot.window(xlim=c(0.7,5),ylim=c(115,200))
#표출력
addtable2plot(0 ,5, my_table_2020,bty="o",display.rownames=TRUE,hlines=TRUE,
              vlines=TRUE,cex=2)
dev.off()