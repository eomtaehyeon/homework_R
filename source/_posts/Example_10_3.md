---
title: "Example_10_3"
author: "Eom_Taehyeon"
date: '2022-03-13'
output: 
  html_document:
    keep_md: true 
---



# 사례분석 10-3 한국인의 정신건강 분석

## 1. 데이터 전처리

### 1) 분석 파일을 R로 불러오기


```r
library(dplyr)
```

```
## Warning: 패키지 'dplyr'는 R 버전 4.1.3에서 작성되었습니다
```

```
## 
## 다음의 패키지를 부착합니다: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

```
## Warning: 패키지 'ggplot2'는 R 버전 4.1.3에서 작성되었습니다
```

```r
library(foreign)
mental<-read.spss("한국행정연구원_사회통합실태조사_데이터_2019.sav",to.data.frame=T)

class(mental)
```

```
## [1] "data.frame"
```
### 2) 분석 변수 추출하고 이름 변경

<추출변수(변수 설명, 척도 범위) : 영어 변수이름>
q32_2(자살하고 싶은 생각이 드는 정도, 4점 척도) : suicide
q1_4(요즘 느끼는 삶에 대한 만족도, 등현등간척도, 11점) : satisfaction 
q32_1(외롭다고 느끼는 정도, 4점 척도) : loneliness 
q34_1(본인의 가족에 대한 신뢰도, 4점척도) : family_belief 
q52(본인 경제상황의 안정도, 등현등간척도, 0~10점) : wealth 
d17(건강상태, 5점 척도) : health
d1(성별) : sex 
d2(연령대, 5등 등급) : age 
ara(지역) : area

<척도 값>
4점 척도 : 1=전혀 아니다, 2=별로 아니다, 3=약간 그렇다, 4=매우 그렇다
5점척도 : 1=매우 나쁘다, 2=나쁘다, 3=보통이다, 4=좋다, 5=매우 좋다
등현등간척도 : 0~10점으로 구성, 5점(보통)보다 낮을수록 부정적이고 높을수록 긍정적


```r
# 추출변수(변수설명, 척도 범위) : 영어변수이름
# 변수 추출 후 이름 변경
mental <- mental%>%
  select(q32_2, q1_4, q32_1, q34_1, q52, d17, d1, d2, ara) %>% 
  rename(suicide=q32_2,
         satisfaction=q1_4,
         loneliness=q32_1,
         family_belief=q34_1,
         wealth=q52,
         health=d17,
         sex=d1,
         age=d2,
         area=ara)
```

### 3) 변수 유형 변경과 정리


```r
str(mental)
```

```
## 'data.frame':	8000 obs. of  9 variables:
##  $ suicide      : Factor w/ 4 levels "전혀 그렇지 않다",..: 2 2 2 1 1 2 3 3 1 2 ...
##  $ satisfaction : Factor w/ 12 levels "0점 전혀 만족하지 않는다",..: 5 5 5 4 7 5 6 6 5 5 ...
##  $ loneliness   : Factor w/ 4 levels "전혀 그렇지 않다",..: 2 2 2 2 1 2 3 3 1 2 ...
##  $ family_belief: Factor w/ 4 levels "전혀 신뢰하지 않는다",..: 3 3 2 2 4 2 2 2 4 3 ...
##  $ wealth       : Factor w/ 11 levels "0점 전혀 안정적이지 않다",..: 4 7 6 6 7 7 6 5 4 6 ...
##  $ health       : Factor w/ 5 levels "매우 나쁘다",..: 4 3 4 4 4 4 4 4 4 4 ...
##  $ sex          : Factor w/ 2 levels "남성","여성": 2 2 1 2 1 2 2 1 2 1 ...
##  $ age          : Factor w/ 5 levels "19~29세","30대",..: 5 5 4 4 3 3 1 5 5 3 ...
##  $ area         : Factor w/ 17 levels "서울","부산",..: 1 1 1 1 1 1 1 1 1 1 ...
##  - attr(*, "variable.labels")= Named chr [1:276] "데이터 ID" "지역" "모수추정 가중치" "표준화 가중치" ...
##   ..- attr(*, "names")= chr [1:276] "id" "ara" "wt1" "wt2" ...
##  - attr(*, "codepage")= int 51949
```


```r
table(mental$suicide)
```

```
## 
## 전혀 그렇지 않다 별로 그렇지 않다      약간 그렇다      매우 그렇다 
##             5592             1862              479               67
```


```r
table(mental$health)
```

```
## 
## 매우 나쁘다 나쁜 편이다    보통이다 좋은 편이다   매우 좋다 
##          87         509        2413        3730        1261
```


```r
table(mental$satisfaction)
```

```
## 
## 0점 전혀 만족하지 않는다                      1점                      2점 
##                       49                       79                      170 
##                      3점                      4점                 5점 보통 
##                      302                      440                     2053 
##                      6점                      7점                      8점 
##                     1611                     1761                     1040 
##                      9점       10점 매우 만족한다              모름/무응답 
##                      321                      174                        0
```


```r
mental$suicide <- as.integer(mental$suicide)
mental$satisfaction <- as.integer(mental$satisfaction)
mental$loneliness <- as.integer(mental$loneliness)
mental$family_belief <- as.integer(mental$family_belief)
mental$wealth <- as.integer(mental$wealth)
mental$health <- as.integer(mental$health)

table(mental$suicide)
```

```
## 
##    1    2    3    4 
## 5592 1862  479   67
```

```r
table(mental$health)
```

```
## 
##    1    2    3    4    5 
##   87  509 2413 3730 1261
```

```r
table(mental$satisfaction)
```

```
## 
##    1    2    3    4    5    6    7    8    9   10   11 
##   49   79  170  302  440 2053 1611 1761 1040  321  174
```



```r
mental$satisfaction <- mental$satisfaction-1
mental$wealth <- mental$wealth-1

table(mental$satisfaction)
```

```
## 
##    0    1    2    3    4    5    6    7    8    9   10 
##   49   79  170  302  440 2053 1611 1761 1040  321  174
```

```r
mental$age <- as.character(mental$age)
mental$sex <- as.character(mental$sex)
mental$area <- as.character(mental$area)
table(mental$sex)
```

```
## 
## 남성 여성 
## 4011 3989
```

```r
table(mental$age)
```

```
## 
## 19~29세    30대    40대    50대 60~69세 
##    1542    1516    1769    1821    1352
```

```r
table(mental$area)
```

```
## 
## 강원 경기 경남 경북 광주 대구 대전 부산 서울 세종 울산 인천 전남 전북 제주 충남 
##  388 1103  527  466  353  464  356  539  965  162  324  522  395  381  267  425 
## 충북 
##  363
```

```r
mental$age <- ifelse(mental$age=="19~29세","20대",
                     ifelse(mental$age=="60~69세","60대",mental$age))
```

### 4) 결측치, 이상치 확인하기


```r
summary(mental)
```

```
##     suicide       satisfaction      loneliness    family_belief  
##  Min.   :1.000   Min.   : 0.000   Min.   :1.000   Min.   :1.000  
##  1st Qu.:1.000   1st Qu.: 5.000   1st Qu.:1.000   1st Qu.:3.000  
##  Median :1.000   Median : 6.000   Median :2.000   Median :4.000  
##  Mean   :1.378   Mean   : 6.037   Mean   :1.795   Mean   :3.576  
##  3rd Qu.:2.000   3rd Qu.: 7.000   3rd Qu.:2.000   3rd Qu.:4.000  
##  Max.   :4.000   Max.   :10.000   Max.   :4.000   Max.   :4.000  
##      wealth           health          sex                age           
##  Min.   : 0.000   Min.   :1.000   Length:8000        Length:8000       
##  1st Qu.: 4.000   1st Qu.:3.000   Class :character   Class :character  
##  Median : 5.000   Median :4.000   Mode  :character   Mode  :character  
##  Mean   : 4.985   Mean   :3.696                                        
##  3rd Qu.: 6.000   3rd Qu.:4.000                                        
##  Max.   :10.000   Max.   :5.000                                        
##      area          
##  Length:8000       
##  Class :character  
##  Mode  :character  
##                    
##                    
## 
```

## 2. 데이터 분석

### 1) 빈도분석


```r
# 성별 빈도분석
mental %>% 
  group_by(sex) %>% 
  summarise(n=n()) %>% 
  mutate(total=sum(n),
         pct=round(n/total*100,1))
```

```
## # A tibble: 2 x 4
##   sex       n total   pct
##   <chr> <int> <int> <dbl>
## 1 남성   4011  8000  50.1
## 2 여성   3989  8000  49.9
```



```r
# 연령대별 빈도분석
mental %>% 
  group_by(age) %>% 
  summarise(n=n()) %>% 
  mutate(total=sum(n),
         pct=round(n/total*100,1))
```

```
## # A tibble: 5 x 4
##   age       n total   pct
##   <chr> <int> <int> <dbl>
## 1 20대   1542  8000  19.3
## 2 30대   1516  8000  19  
## 3 40대   1769  8000  22.1
## 4 50대   1821  8000  22.8
## 5 60대   1352  8000  16.9
```

### 2) 교차분석


```r
# 성과 연령대의 교차 빈도 구하기
table(mental$sex, mental$age)
```

```
##       
##        20대 30대 40대 50대 60대
##   남성  822  745  900  891  653
##   여성  720  771  869  930  699
```


```r
# 성과 연령대의 교차 백분율 구하기. 행별로 100% 기준. 소수점 한자리
round(prop.table(table(mental$sex, mental$age),1)*100,1)
```

```
##       
##        20대 30대 40대 50대 60대
##   남성 20.5 18.6 22.4 22.2 16.3
##   여성 18.0 19.3 21.8 23.3 17.5
```

```r
# -> 앞의 숫자 1은 행별로 비율의 총합이 100이 되도록 계산하라는 명령이고, 뒤의 숫자 1은 소수점 한자리까지 구하라는 명령이다.
```


```r
# 교차분석 검정
chisq.test(mental$sex, mental$age)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  mental$sex and mental$age
## X-squared = 10.076, df = 4, p-value = 0.03916
```

- 유의수준이 0.03916으로 p<0.5입니다. 따라서 남성과 여성의 연령대 분포 비율은 다소 차이가 있다고 할 수 있습니다.
- 남성에서는 40대와 50대 비율이 높고, 여성에서는 50대의 비율이 가장 높습니다.

### 3) 평균 분석


```r
mental %>% 
  summarise(m1=mean(suicide), m2=mean(satisfaction), m3=mean(loneliness), m4=mean(family_belief), m5=mean(wealth), m6=mean(health))
```

```
##         m1     m2    m3       m4       m5       m6
## 1 1.377625 6.0365 1.795 3.576375 4.985125 3.696125
```

- 자살충동(suicide) : 4점 척도에서 1.38점으로 '아니다~별로 아니다' 있습니다.
- 삶의 만족도(satisfactio) : 0~10점 척도에서 6.04점입니다. 보통(5점)보다 위에 있지만 높은 수준은 아닙니다.
- 외로움(loneliness) : 4점 척도에서 1.8점으로 비교적 낮은 수준입니다.
- 가족신뢰도(family_belief) : 4점 척도에서 3.58점으로 높은 수준입니다.
- 경제 안정도(wealth) : 0~10점 척도에서 4.99점으로 보통(5점)보다 조금 낮은 수준입니다.
- 건강상태(health) : 5점 척도에서 3.7점으로 좋은 수준입니다.

### 4) 연구문제 1 : 삶의 만족도와 외로움이 자살충동에 미치는 영향

- 삶의 만족도와 외로움을 독립변수로 하고, 자살충동을 종속변수로 하는 다중회귀분석을 수행


```r
RA <- lm(data=mental, suicide~satisfaction+loneliness) #다중회귀분석
summary(RA) #분석 결과 상세히 보기기
```

```
## 
## Call:
## lm(formula = suicide ~ satisfaction + loneliness, data = mental)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.50517 -0.40228 -0.03487  0.17773  3.07029 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   1.035551   0.029823   34.72   <2e-16 ***
## satisfaction -0.052583   0.003614  -14.55   <2e-16 ***
## loneliness    0.367405   0.007987   46.00   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5451 on 7997 degrees of freedom
## Multiple R-squared:  0.2668,	Adjusted R-squared:  0.2666 
## F-statistic:  1455 on 2 and 7997 DF,  p-value: < 2.2e-16
```

- 이 회귀식의 회귀모델은 p<0.001로 적합.
- 2개 독립변수 회귀계수를 보면 satisfaction은 -0.052583이고, loneliness 0.367405이다.
- Pr 에서 유의수준은 모두 0.001보다 작다. 
- 회귀식은 suicide = 1.035551 - 0.052583 * satisfaction + 0.367405 * loneliness
- 삶의 만족도가 1단위 높아지면 자살충동이 -0.052583단위 감소하고, 외로움이 1단위 높아지면 자살충동은 0.367405단위 증가합니다.
- 이 회귀식의 수정된 결정설명력(Adjusted R-squared)은 0.2666

### 5) 연구문제 2 : 삶의 만족도와 외로움의 상관관계

- 삶의 만족도와 외로움은 자살충동에 상반된 영향을 주었습니다. 두 변수가 상관관계가 있는지 알아보자.


```r
cor.test(mental$satisfaction, mental$loneliness)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  mental$satisfaction and mental$loneliness
## t = -25.374, df = 7998, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.2931116 -0.2525481
## sample estimates:
##        cor 
## -0.2729512
```

- 유의수준은 0.001보다 작기 때문에 통계적으로 유의미.
- 상관계수(r)는 -0.27 따라서 삶의 만족도와 외로움은 약한 수준에서 부적인 상관관계에 있다.
- 삶의 만족도가 높아지면 외로움이 약간 줄어드는 관계.

### 6) 연구문제 3 : 가족신뢰도, 경제안정도, 건강상태가 삶의 만족도와 외로움에 미치는 영향

- 가족신뢰도, 경제안정도, 건강상태를 독립변수, 삶의 만족도를 종속변수로 하는 다중회귀분석을 수행.
- 같은 방법으로 외로움을 족속변수로 다중회귀분석 수행.

#### (1) 3개 독립변수가 삶의 만족도에 미치는 영향


```r
RA1 <- lm(data=mental, satisfaction~family_belief+wealth+health)

summary(RA1)
```

```
## 
## Call:
## lm(formula = satisfaction ~ family_belief + wealth + health, 
##     data = mental)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.8274 -0.9431 -0.0425  1.0569  6.1986 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    2.07613    0.13765   15.08   <2e-16 ***
## family_belief  0.36851    0.03196   11.53   <2e-16 ***
## wealth         0.26016    0.01089   23.88   <2e-16 ***
## health         0.36403    0.02206   16.50   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.627 on 7996 degrees of freedom
## Multiple R-squared:  0.1386,	Adjusted R-squared:  0.1383 
## F-statistic: 428.8 on 3 and 7996 DF,  p-value: < 2.2e-16
```

satisfaction = 2.07613 + 0.36851 * family_belief + 0.26016 * wealth + 0.36403 * health

영향력은 가족신뢰도 > 건강상태 > 경제안정도

#### (2) 3개 독립변수가 외로움에 미치는 영향


```r
RA2 <- lm(data=mental, loneliness~family_belief+wealth+health)

summary(RA2)
```

```
## 
## Call:
## lm(formula = loneliness ~ family_belief + wealth + health, data = mental)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.24066 -0.64247  0.01863  0.43022  2.83959 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    3.652247   0.063109   57.87   <2e-16 ***
## family_belief -0.220274   0.014654  -15.03   <2e-16 ***
## wealth        -0.072686   0.004995  -14.55   <2e-16 ***
## health        -0.191313   0.010116  -18.91   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.746 on 7996 degrees of freedom
## Multiple R-squared:  0.1157,	Adjusted R-squared:  0.1154 
## F-statistic: 348.9 on 3 and 7996 DF,  p-value: < 2.2e-16
```

loneliness = 3.652247 - 0.220274 * family_belief - 0.072686 * wealth - 0.191313 * health

영향력은 가족신뢰도 > 건강상태 > 경제안정도 

- 결론적으로 3개 독립변수가 삶의 만족도와 외로움에 미치는 영향력을 가진다. 
- 공통적으로 가족신뢰도가 가장 큰 영향을 준다.
- 삶의 만족도와 외로움은 자살충동에 영향을 준다.
- 자살률을 줄이는 데는 가족 간 신뢰를 회복하는 것이 가장 중요하다.

### 7) 연구문제 : 4 성, 연령, 지역별 삶의 만족도 차이

#### (1) 성별 삶의 만족도 차이

- 성별로 삶의 만족도에서 차이가 있는가를 독립표본 t검정으로 알아보자.


```r
t.test(data=mental,satisfaction~sex)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  satisfaction by sex
## t = -3.7719, df = 7997.6, p-value = 0.0001632
## alternative hypothesis: true difference in means between group 남성 and group 여성 is not equal to 0
## 95 percent confidence interval:
##  -0.22446298 -0.07094075
## sample estimates:
## mean in group 남성 mean in group 여성 
##           5.962852           6.110554
```

평균 남성이 5.96 여성이 6.11 여성이 남성보다 높다.

#### (2) 연령대별 삶의 만족도 차이

- 연령대는 5개로 분류되어 있기 때문에 평균만 알아보자.


```r
mental %>% 
  group_by(age) %>%   # age 변수를 범주별로 분류
  summarise(m=mean(satisfaction)) %>%    # age 범주별 평균 구하기
  arrange(desc(m))    # 평균값을 내림차순으로 정렬
```

```
## # A tibble: 5 x 2
##   age       m
##   <chr> <dbl>
## 1 30대   6.13
## 2 50대   6.08
## 3 40대   6.05
## 4 20대   6.04
## 5 60대   5.84
```

30대가 6.13점으로 가장 높다. 60대를 제외하고 모두 6점대이다.

#### (3) 지역별 삶의 만족도 분석과 그래프 그리기

- 지역이 많기 때문에 coord_flip() 함수를 이용해서 지역 이름을 y축에 놓고 막대를 x축과 수평으로 그린다.


```r
area_satisfaction <- mental %>% 
  group_by(area) %>% 
  summarise(m=mean(satisfaction)) %>% 
  arrange(desc(m))

ggplot(data = area_satisfaction, aes(x=reorder(area,m),y=m))+
  geom_col()+
  ggtitle("지역별 만족도")+
  xlab("지역")+
  ylab("만족도")+
  coord_flip()
```

![](/images/Example_10_3_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

제주, 강원, 세종 지역의 만족도가 높고, 부산, 대구, 인천이 낮다.

## 연습문제 10-3

### 1. 앞에서 성별, 연령대별 삶의 만족도를 각각 분석 했습니다. 연령대별로 남녀로 분류해서 삶의 만족도 평균을 분석하고, 평균이 높은 상위 4개 집단을 출력합니다.


```r
mental %>% 
  group_by(age, sex) %>%   # age, sex 변수를 범주별로 분류
  summarise(m=mean(satisfaction)) %>%    # age, sex 범주별 평균 구하기
  arrange(desc(m)) %>%   # 평균값을 내림차순으로 정렬
  head(4)   # 상위 4개 데이터 출력
```

```
## `summarise()` has grouped output by 'age'. You can override using the `.groups`
## argument.
```

```
## # A tibble: 4 x 3
## # Groups:   age [4]
##   age   sex       m
##   <chr> <chr> <dbl>
## 1 30대  여성   6.24
## 2 50대  여성   6.19
## 3 40대  여성   6.13
## 4 20대  여성   6.06
```

### 2. 지역, 연령대, 성별로 분류해서 삶의 만족도 평균을 분석하고, 평균이 높은 상위 5개 집단을 출력합니다. 3개 변수를 교차해서 분류하고 분석하는 문제.


```r
mental %>% 
  group_by(area, age, sex) %>%   # area, age, sex 변수를 범주별로 분류
  summarise(m=mean(satisfaction)) %>%    # area, age, sex 범주별 평균 구하기
  arrange(desc(m)) %>%   # 평균값을 내림차순으로 정렬
  head(5)  # 상위 5개 데이터 출력
```

```
## `summarise()` has grouped output by 'area', 'age'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 5 x 4
## # Groups:   area, age [5]
##   area  age   sex       m
##   <chr> <chr> <chr> <dbl>
## 1 제주  40대  남성   6.94
## 2 제주  30대  여성   6.86
## 3 제주  50대  남성   6.82
## 4 충북  30대  여성   6.81
## 5 세종  30대  여성   6.81
```

### 3. 앞에서 분석한 연령대별 삶의 만족도 평균을 보면 30대가 가장 높고, 60대가 가장 낮았습니다. 30대와 60대의 만족도가 통계적으로 차이가 있는가를 검정.


```r
# age_30_60 객체 만들기
age_30_60 <- mental %>% 
  filter(age %in% c("30대","60대"))

# 30대와 60대의 평균 차이 검정
t.test(data=age_30_60, satisfaction~age)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  satisfaction by age
## t = 4.2675, df = 2811.3, p-value = 2.043e-05
## alternative hypothesis: true difference in means between group 30대 and group 60대 is not equal to 0
## 95 percent confidence interval:
##  0.1562378 0.4218640
## sample estimates:
## mean in group 30대 mean in group 60대 
##           6.129288           5.840237
```

유의수준이 0.001보다 작아서 30대와 60대의 평균의 차이가 있다.


### 4. 앞에서 가족신뢰도, 경제안정도, 건강상태가 삶의 만족도와 외로움에 직접적인 영향을 주는 것으로 확인되었습니다. 그런데 가족신뢰도, 경제안정도, 건강상태는 자살충동에 직접적인 영향을 줄 수도 있습니다 .이를 확인하기 바랍니다.



```r
RA3 <- lm(data=mental, suicide~family_belief+wealth+health)

summary(RA3)
```

```
## 
## Call:
## lm(formula = suicide ~ family_belief + wealth + health, data = mental)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.5342 -0.3821 -0.2426  0.4095  3.0650 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    2.882060   0.050986   56.53   <2e-16 ***
## family_belief -0.238659   0.011839  -20.16   <2e-16 ***
## wealth        -0.049597   0.004035  -12.29   <2e-16 ***
## health        -0.109209   0.008172  -13.36   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.6027 on 7996 degrees of freedom
## Multiple R-squared:  0.1037,	Adjusted R-squared:  0.1034 
## F-statistic: 308.4 on 3 and 7996 DF,  p-value: < 2.2e-16
```

유의수준이 0.001보다 작아 통계적으로 유의미하다.

세 변수 모두 자살충동에 부적인 영향을 주고 있다.

영향력은 가족신뢰도 > 건강상태 > 경제안정도 순이다.

회귀식의 수정된 설명력은 0.1034 이다.






















