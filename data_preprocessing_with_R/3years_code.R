# 2014, 2017, 2020 데이터프레임 합치기

# 2017, 2020 열 이름 바꾸기
# 모든 컬럼의 이름 바꾸기.
names(final_2017) <- c("L11", "total_ga", "ga_h", "B3_4", "C2", "nutrient_2014", "D1_2", "D2_2", "D3_2", "G2", "G3_1_4_a", "H4", "H6", "I6_1", "I6_2", "K1", "K10", "L1_1", "L1_2", "L1_3", "L1_4", "L1_5", "L1_6", "L4", "L5_1", "L5_2", "L5_3", "L5_4", "L5_5", "jo1", "F1")
names(final_2020) <- c("L11", "total_ga", "ga_h", "B3_4", "C2", "nutrient_2014", "D1_2", "D2_2", "D3_2", "G2", "G3_1_4_a", "H4", "H6", "I6_1", "I6_2", "K1", "K10", "L1_1", "L1_2", "L1_3", "L1_4", "L1_5", "L1_6", "L4", "L5_1", "L5_2", "L5_3", "L5_4", "L5_5", "jo1", "F1")
# 출처: https://realab.tistory.com/8 [안녕!]


# 합치기
final <- rbind(final_2014, final_2017, final_2020)
# 출처: https://rfriend.tistory.com/51 [R, Python 분석과 프로그래밍의 친구 (by R Friend)]

write.xlsx(final, sheetName="sheet1", file="3years_dataset.xlsx")

# 자살 생각 빈도와 비율표 계산
# 결과변수 table 만들기
class(final$L11)
table(final$L11)
thought_yes = 855
thought_no = 17403
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
my_table_3years=rbind(data.frame(A=C11,B=C12),
                      data.frame(A=C21,B=C22),
                      data.frame(A=C31,B=C32))
colnames(my_table_3years)=c("Frequency(people)","Ratio(%)")
rownames(my_table_3years)=c("Suicidal idea(No)","Suicidal idea(Yes)", "Total")
## 표 png 파일로 저장
setwd("D:\\TeamProject")
png("Suicide_ideas_frequency_table_3years.png",width=1200,height=350,res=150)
#빈 plot 생성
plot.new()
plot.window(xlim=c(0.7,5),ylim=c(115,200))
#표출력
addtable2plot(0 ,5, my_table_3years,bty="o",display.rownames=TRUE,hlines=TRUE,
              vlines=TRUE,cex=2)
dev.off()