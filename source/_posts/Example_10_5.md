---
title: "Example_10_5"
author: "Eom_Taehyeon"
date: '2022-03-12'
output: 
  html_document :
    keep_md: true 
---



# 사례분석 10 - 5 한국인의 급여 실태 분석

## R로 불러와서 분석 객체 만들기

### 1) R로 불러오기


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

koweps19 <- read.spss("koweps_hpwc14_2019_beta1.sav")
koweps19 <- as.data.frame(koweps19)  # 데이터프레임 형태로 변환
```

### 2) 분석 변수 추출과 이름 변경


```r
# 분석할 변수 추출해서 새 객체에 입력

welfare19 <- koweps19 %>% 
  select(h14_g3, h14_g4, h14_g6, h14_reg5, h14_eco9, h14_inc2, h14_inc3)

# welfare19의 변수 이름 변경
welfare19 <- welfare19 %>% 
  rename(sex=h14_g3,
         birth=h14_g4,
         edu=h14_g6,
         region=h14_reg5,
         job_code=h14_eco9,
         p_salary=h14_inc2,
         t_salary=h14_inc3)
```


## 데이터 전처리

### 1) 변수 유형 확인하기


```r
str(welfare19)
```

```
## 'data.frame':	14418 obs. of  7 variables:
##  $ sex     : num  2 1 1 1 2 2 1 2 2 2 ...
##  $ birth   : num  1945 1948 1942 1962 1963 ...
##  $ edu     : num  4 3 7 6 5 4 4 4 3 5 ...
##  $ region  : num  1 1 1 1 1 1 1 1 3 1 ...
##  $ job_code: num  NA NA 762 855 NA NA NA 941 999 NA ...
##  $ p_salary: num  NA NA NA 2304 NA ...
##  $ t_salary: num  NA NA 1284 NA NA ...
```

### 2) 결측치, 이상치 확인하기


```r
summary(welfare19)
```

```
##       sex            birth           edu            region     
##  Min.   :1.000   Min.   :1907   Min.   :1.000   Min.   :1.000  
##  1st Qu.:1.000   1st Qu.:1948   1st Qu.:3.000   1st Qu.:2.000  
##  Median :2.000   Median :1968   Median :5.000   Median :3.000  
##  Mean   :1.549   Mean   :1969   Mean   :4.565   Mean   :2.659  
##  3rd Qu.:2.000   3rd Qu.:1990   3rd Qu.:6.000   3rd Qu.:3.000  
##  Max.   :2.000   Max.   :2018   Max.   :9.000   Max.   :5.000  
##                                                                
##     job_code         p_salary        t_salary      
##  Min.   : 111.0   Min.   :    0   Min.   :    0.0  
##  1st Qu.: 313.0   1st Qu.: 2448   1st Qu.:  391.5  
##  Median : 611.0   Median : 3540   Median : 1116.0  
##  Mean   : 587.5   Mean   : 4141   Mean   : 1389.4  
##  3rd Qu.: 873.0   3rd Qu.: 5378   3rd Qu.: 2040.0  
##  Max.   :1009.0   Max.   :22700   Max.   :11500.0  
##  NA's   :7540     NA's   :11759   NA's   :11087
```


```r
# 상용직, 일용직 근로자의 총급여가 0이면 결측치로 처리

welfare19$p_salary <- ifelse(welfare19$p_salary==0, NA, welfare19$p_salary)
welfare19$t_salary <- ifelse(welfare19$t_salary==0, NA, welfare19$t_salary)

table(is.na(welfare19$p_salary))  # 결측치 6개 증가
```

```
## 
## FALSE  TRUE 
##  2653 11765
```

```r
table(is.na(welfare19$t_salary))   # 결측치 1개 증가
```

```
## 
## FALSE  TRUE 
##  3330 11088
```

### 3) 변수 기초 정리

#### (1) 성별 변수


```r
table(welfare19$sex)
```

```
## 
##    1    2 
## 6506 7912
```

```r
welfare19$sex <- ifelse(welfare19$sex==1,"male", "female")

table(welfare19$sex)
```

```
## 
## female   male 
##   7912   6506
```

#### (2) 출생연도 변수


```r
welfare19$age <- 2019 - welfare19$birth + 1

range(welfare19$age)
```

```
## [1]   2 113
```

#### (3) 교욱수준 변수


```r
# 교육수준 등급 재분류

welfare19$edu_grade <- ifelse(welfare19$edu %in% c(1,2,3,4), "중학 이하",
                              ifelse(welfare19$edu==5, "고교",
                                     ifelse(welfare19$edu==6, "전문대","대학 이상")))
# 교육수준 등급별 빈도 보기
table(welfare19$edu_grade)
```

```
## 
##      고교 대학 이상    전문대 중학 이하 
##      3849      2728      1317      6524
```


#### (4) 권역 변수


```r
# region_name 만들기

region_name <- data.frame(region=c(1,2,3,4,5),
                          region1=c("서울", "광역시", "시", "구", "도농복합군"))

# welfare19dp region_name 결합
welfare19 <- left_join(welfare19, region_name, id= "region")
```

```
## Joining, by = "region"
```

#### (5) 직종 변수


```r
library(readxl)
```

```
## Warning: 패키지 'readxl'는 R 버전 4.1.3에서 작성되었습니다
```

```r
# job_name 객체 만들기
job_name <- read_excel("(2019년 14차 한국복지패널조사) 조사설계서-가구용(beta1).xlsx", sheet=6)

str(job_name)
```

```
## tibble [156 x 2] (S3: tbl_df/tbl/data.frame)
##  $ job_code: num [1:156] 111 112 121 122 131 132 133 134 135 139 ...
##  $ job     : chr [1:156] "의회 의원<U+2219>고위 공무원 및 공공단체 임원" "기업 고위 임원" "행정 및 경영 지원 관리자" "마케팅 및 광고<U+2219>홍보 관리자" ...
```

```r
# welfare19에 job_name을 결합
welfare19 <- left_join(welfare19, job_name, id="job_code")
```

```
## Joining, by = "job_code"
```

```r
str(welfare19)
```

```
## 'data.frame':	14418 obs. of  11 variables:
##  $ sex      : chr  "female" "male" "male" "male" ...
##  $ birth    : num  1945 1948 1942 1962 1963 ...
##  $ edu      : num  4 3 7 6 5 4 4 4 3 5 ...
##  $ region   : num  1 1 1 1 1 1 1 1 3 1 ...
##  $ job_code : num  NA NA 762 855 NA NA NA 941 999 NA ...
##  $ p_salary : num  NA NA NA 2304 NA ...
##  $ t_salary : num  NA NA 1284 NA NA ...
##  $ age      : num  75 72 78 58 57 17 93 86 80 50 ...
##  $ edu_grade: chr  "중학 이하" "중학 이하" "대학 이상" "전문대" ...
##  $ region1  : chr  "서울" "서울" "서울" "서울" ...
##  $ job      : chr  NA NA "전기공" "금속기계 부품 조립원" ...
```

```r
# 분석 변수만 순서대로 정리
welfare19 <- welfare19 %>% select(sex, age, edu, edu_grade, region1, job, p_salary, t_salary)
```

## 분석하기

### 1) 상용직과 일용직의 평균 총급여 비교


```r
mean(welfare19$p_salary, na.rm = T) # 상용직 평균
```

```
## [1] 4150.747
```

```r
mean(welfare19$t_salary, na.rm = T) # 일용직 평균
```

```
## [1] 1389.858
```

상용직 평균은 4150.7만원, 일용직 평균은 1389.858 만원 

### 2) 성별 평균 총급여 차이 검정


```r
# 상용직 남녀 평균 총급여 차이 통계 분석
t.test(data=welfare19, p_salary~sex)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  p_salary by sex
## t = -21.912, df = 2650.7, p-value < 2.2e-16
## alternative hypothesis: true difference in means between group female and group male is not equal to 0
## 95 percent confidence interval:
##  -2079.007 -1737.475
## sample estimates:
## mean in group female   mean in group male 
##             2979.045             4887.286
```

  유의 수준이 <0.001이어서 유의미 하다.
  남성 평균은 4887.3만원, 여성 평균 2979만원이다.
  남성이 여성보다 약 64% 더 많다.
  
### 3) 최대 총급여 상용직 근로자 찾기


```r
welfare19 %>% 
  filter(!is.na(p_salary)) %>% 
  group_by(sex) %>% 
  filter(p_salary==max(p_salary)) %>% 
  select(sex, age, edu, edu_grade, region1, job, p_salary)
```

```
## # A tibble: 2 x 7
## # Groups:   sex [2]
##   sex      age   edu edu_grade region1 job                            p_salary
##   <chr>  <dbl> <dbl> <chr>     <chr>   <chr>                             <dbl>
## 1 male      48     7 대학 이상 광역시  기계<U+2219>로봇공학 기술자 및 시험원    22700
## 2 female    52     5 고교      시      보험 및 금융 관리자               12096
```

### 4) 연령별 평균 총급여


```r
# age_salary1 만들기
age_salary1 <- welfare19 %>% 
  filter(!is.na(p_salary)) %>% 
  group_by(age) %>% 
  summarise(m=mean(p_salary))

# 상위 3개 데이터 추출
age_salary1 %>% 
  arrange(desc(m)) %>% 
  head(3)
```

```
## # A tibble: 3 x 2
##     age     m
##   <dbl> <dbl>
## 1    58 5586.
## 2    53 5558.
## 3    57 5387.
```

### 5) 연령별 평균 총급여 그래프 그리기


```r
ggplot(data = age_salary1, aes(x=age, y=m))+
  geom_line()+
  xlab("연령")+
  ylab("총급여")
```

![](/images/Eample_10_5_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

### 6) 연령별 남녀 평균 총급여 그래프 그리기


```r
# age_salary2 만들기
age_salary2 <- welfare19 %>% 
  filter(!is.na(p_salary)) %>% 
  group_by(age, sex) %>% 
  summarise(m=mean(p_salary))
```

```
## `summarise()` has grouped output by 'age'. You can override using the `.groups`
## argument.
```

```r
# 연령별로 남성과 여성의 평균 총급여 선그래프 그리기
ggplot(data=age_salary2, aes(x=age, y=m, col=sex))+
  geom_line()+
  xlab("연령")+
  ylab("총급여")
```

![](/images/Eample_10_5_files/figure-html/unnamed-chunk-17-1.png)<!-- -->


### 7) 교육수준별 상용직 평균 총급여 비교


```r
# 교육수준별 상용직 근로자 평균 총급여 구하기
edu_salary1 <- welfare19 %>% 
  filter(!is.na(p_salary)) %>% 
  group_by(edu_grade) %>% 
  summarise(m=mean(p_salary))

# 교육수준별 상용직 평균 총급여 기준 내림차순 정렬
edu_salary1 %>% arrange(desc(m))
```

```
## # A tibble: 4 x 2
##   edu_grade     m
##   <chr>     <dbl>
## 1 대학 이상 4889.
## 2 고교      3670.
## 3 전문대    3640.
## 4 중학 이하 2672.
```

```r
# 그래프 그리기
ggplot(data = edu_salary1, aes(x=reorder(edu_grade, m), y=m))+geom_col()
```

![](/images/Eample_10_5_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

### 8) 상용직 근로자의 교육수준과 성에 따른 총급여 분석


```r
# 상용직 근로자의 교육수준별, 성별 총급여 구하기
edu_salary2 <- welfare19 %>% 
  filter(!is.na(p_salary)) %>% 
  group_by(edu_grade, sex) %>% 
  summarise(m=mean(p_salary))
```

```
## `summarise()` has grouped output by 'edu_grade'. You can override using the
## `.groups` argument.
```

```r
# 총급여 기준 내림차순 정렬
edu_salary2 %>% arrange(desc(m))
```

```
## # A tibble: 8 x 3
## # Groups:   edu_grade [4]
##   edu_grade sex        m
##   <chr>     <chr>  <dbl>
## 1 대학 이상 male   5632.
## 2 전문대    male   4382.
## 3 고교      male   4345.
## 4 대학 이상 female 3547.
## 5 중학 이하 male   3129.
## 6 전문대    female 2666.
## 7 고교      female 2631.
## 8 중학 이하 female 1996.
```

```r
# 막대 그래프 그리기
ggplot(data = edu_salary2, aes(x=edu_grade, y=m, fill=sex))+
  geom_col(position = "dodge")+
  scale_x_discrete(limits=c("중학 이하","고교","전문대","대학 이상"))
```

![](/images/Eample_10_5_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

### 9) 권역별 평균 총급여 비교


```r
# 권역별 상용직 근로자 평균 총급여 구하기
region_salary <- welfare19 %>% 
  filter(!is.na(p_salary)) %>% 
  group_by(region1) %>% 
  summarise(m=mean(p_salary))

# 총급여 기준 내림차순 정렬

region_salary %>% arrange(desc(m))
```

```
## # A tibble: 5 x 2
##   region1        m
##   <chr>      <dbl>
## 1 도농복합군 4634.
## 2 서울       4371.
## 3 시         4163.
## 4 광역시     4070.
## 5 구         3702.
```

```r
# 총급여 기준 내림차순으로 막대그래프 그리기
ggplot(data = region_salary, aes(x=reorder(region1, -m), y=m))+geom_col()
```

![](/images/Eample_10_5_files/figure-html/unnamed-chunk-20-1.png)<!-- -->


### 10) 직종별 평균 총급여 구하기


```r
# job_salary 만들기
job_salary <- welfare19 %>% 
  filter(!is.na(p_salary)) %>% 
  group_by(job) %>% 
  summarise(m=mean(p_salary))

# 총급여 10위 내림차순
job_salary %>% 
  arrange(desc(m)) %>% 
  head(10)
```

```
## # A tibble: 10 x 2
##    job                                    m
##    <chr>                              <dbl>
##  1 보험 및 금융 관리자                9876.
##  2 법률 전문가                        9416 
##  3 행정 및 경영 지원 관리자           9306.
##  4 의료 진료 전문가                   9237.
##  5 기업 고위 임원                     8622 
##  6 기계<U+2219>로봇공학 기술자 및 시험원     8283.
##  7 재활용 처리 및 소각로 조작원       8261 
##  8 컴퓨터 하드웨어 및 통신공학 전문가 8211 
##  9 대학교수 및 강사                   7976.
## 10 건설<U+2219>전기 및 생산 관련 관리자      7892.
```

```r
# 그래프 그리기
job_salary20 <- job_salary %>% 
  arrange(desc(m)) %>% 
  head(20)

ggplot(data=job_salary20, aes(x=reorder(job,m), y=m))+
  geom_col()+
  coord_flip()
```

![](/images/Eample_10_5_files/figure-html/unnamed-chunk-21-1.png)<!-- -->


















