library(readr)
library(stringr)
library(dplyr)
library(moonBook)
library(plyr)
library(readxl)
library(car)

library(leaps)

df<-read_csv("data/2_강도살인안전도.csv")
View(df)
df


#변수 눈 부터 끝까지
dt<-df[3:64]
dt


#-------------------------------------------------------------------

#더미변수 중 1개 drop->미상 드롭
#+crm_wthr_미상 ,+crm_clue_기타 ,+vic_sx_3 , +vic_age_미상, +crm_tm_미상
#na 값 : +화재_인명피해_계, +화재_재산피해_천원, #+기초수급_총합
fit2<-lm(score_강살~crm_wthr_눈+crm_wthr_만월+crm_wthr_맑음+crm_wthr_비
         +crm_wthr_암흑+crm_wthr_흐림+crm_clue_고발+crm_clue_고소
         +crm_clue_변사체+crm_clue_불심검문+crm_clue_여죄+crm_clue_자수
         +crm_clue_진정+crm_clue_타인신고+crm_clue_탐문정보+crm_clue_피해자신고
         +crm_clue_현행범+vic_sx_1+vic_sx_2+vic_age_0_12세+vic_age_10대
         +vic_age_2_30대+vic_age_4_50대+vic_age_60세초과
         +crm_tm_새벽+crm_tm_오전+crm_tm_오후+crm_tm_저녁+강도살인_발생수
         +강도살인_검거수+강도살인_검거율+화재건수+화재_사망+화재_부상+화재_인명피해_계
         +화재_부동산피해_천원+화재_동산피해_천원+화재_재산피해_천원
         +화재_재산피해_건당천원+cctv_개수+공원_개수+배치인원_수+보안등_개수
         +비상벨_개수+유흥업소_개수+일인가구수+자살건수
         +자살_사망률_10만명당+자살_연령표준화사망률_10만명당+총_인구수
         +기초수급_19세이하+기초수급_20_34세+기초수급_35_59세+기초수급_60세이상
         +기초수급_총합+외국인수, data = dt)
summary(fit2)

#다중공산성
vif(fit2)


#-------------------------------------------------------


####***모든변수 활용####
#all subset regression
#na오류 나는 변수 제외, 이유는 모름
#변수가 많아 시간이 오래걸림 오류남 ->really.big=T로 오류해결
#nvmax, 변수개수 지정
result2<-regsubsets(score_강살~crm_wthr_눈+crm_wthr_만월+crm_wthr_맑음+crm_wthr_비
                    +crm_wthr_암흑+crm_wthr_흐림+crm_clue_고발+crm_clue_고소
                    +crm_clue_변사체+crm_clue_불심검문+crm_clue_여죄+crm_clue_자수
                    +crm_clue_진정+crm_clue_타인신고+crm_clue_탐문정보+crm_clue_피해자신고
                    +crm_clue_현행범+vic_sx_1+vic_sx_2+vic_age_0_12세+vic_age_10대
                    +vic_age_2_30대+vic_age_4_50대+vic_age_60세초과
                    +crm_tm_새벽+crm_tm_오전+crm_tm_오후+crm_tm_저녁+강도살인_발생수
                    +강도살인_검거수+강도살인_검거율+화재건수+화재_사망+화재_부상#+화재_인명피해_계
                    +화재_부동산피해_천원+화재_동산피해_천원#+화재_재산피해_천원
                    +화재_재산피해_건당천원+cctv_개수+공원_개수+배치인원_수+보안등_개수
                    +비상벨_개수+유흥업소_개수+일인가구수+자살건수
                    +자살_사망률_10만명당+자살_연령표준화사망률_10만명당+총_인구수
                    +기초수급_19세이하+기초수급_20_34세+기초수급_35_59세+기초수급_60세이상
                    #+기초수급_총합
                    +외국인수, data = dt,really.big=T)

#그래프로 확인
plot(result2, scale="adjr2")

#그래프 너무 오래걸리니까 직접확인_ 맨 아래것만 보면됨
summary(result2)




#leaps한 후 ols확인, 다중공산성 확인
fit5<- lm(score_강살~+crm_clue_변사체+crm_clue_현행범+강도살인_검거수
          +공원_개수+자살_사망률_10만명당+자살_연령표준화사망률_10만명당
          +기초수급_19세이하+외국인수,data=dt)
summary(fit5)

#다중공산성 문제없음
vif(fit5)




#-------------------------------------------------------


####***모든변수 활용####
#all subset regression
#na오류 나는 변수 제외, 이유는 모름
#변수가 많아 시간이 오래걸림 오류남 ->really.big=T로 오류해결
#nvmax, 변수개수 지정
result3<-regsubsets(score_강살~crm_wthr_눈+crm_wthr_만월+crm_wthr_맑음+crm_wthr_비
                    +crm_wthr_암흑+crm_wthr_흐림+crm_clue_고발+crm_clue_고소
                    +crm_clue_변사체+crm_clue_불심검문+crm_clue_여죄+crm_clue_자수
                    +crm_clue_진정+crm_clue_타인신고+crm_clue_탐문정보+crm_clue_피해자신고
                    +crm_clue_현행범+vic_sx_1+vic_sx_2+vic_age_0_12세+vic_age_10대
                    +vic_age_2_30대+vic_age_4_50대+vic_age_60세초과
                    +crm_tm_새벽+crm_tm_오전+crm_tm_오후+crm_tm_저녁+강도살인_발생수
                    +강도살인_검거수+강도살인_검거율+화재건수+화재_사망+화재_부상#+화재_인명피해_계
                    +화재_부동산피해_천원+화재_동산피해_천원#+화재_재산피해_천원
                    +화재_재산피해_건당천원+cctv_개수+공원_개수+배치인원_수+보안등_개수
                    +비상벨_개수+유흥업소_개수+일인가구수+자살건수
                    +자살_사망률_10만명당+자살_연령표준화사망률_10만명당+총_인구수
                    +기초수급_19세이하+기초수급_20_34세+기초수급_35_59세+기초수급_60세이상
                    #+기초수급_총합
                    +외국인수, data = dt,really.big=T, nvmax = 15)

#그래프로 확인
plot(result3, scale="adjr2")

#그래프 너무 오래걸리니까 직접확인_ 맨 아래것만 보면됨
summary(result3)




#leaps한 후 ols확인, 다중공산성 확인
fit6<- lm(score_강살~+crm_clue_변사체+crm_clue_자수+crm_clue_진정+crm_clue_현행범
          +vic_sx_1+강도살인_검거수+화재_사망+화재_부상+화재_부동산피해_천원
          +공원_개수+자살_사망률_10만명당+자살_연령표준화사망률_10만명당
          +기초수급_35_59세+기초수급_60세이상+외국인수,data=dt)
summary(fit6)

#다중공산성 문제없음
vif(fit6)
