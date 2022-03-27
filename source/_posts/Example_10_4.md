---
title: "Example_10_4"
author: "Eom_Taehyeon"
date: '2022-03-12'
output: 
  html_document:
    keep_md: true 
---



# 사례 분석 10-4 서울의 음식점 현황 분석

## 4. 데이터 전처리

### 10 분석 파일을 R로 불러오기


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
# 엑셀 파일 불러오기. 공란은 결측치로 처리.
foodshop <- read.csv("6110000_서울특별시_07_24_04_P_일반음식점.csv", na=" ", stringsAsFactors=F) #stringsAsFactors=F 파라미터로 문자형으로 변경

str(foodshop)
```

```
## 'data.frame':	458263 obs. of  47 variables:
##  $ 번호              : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ 개방서비스명      : chr  "일반음식점" "일반음식점" "일반음식점" "일반음식점" ...
##  $ 개방서비스id      : chr  "07_24_04_P" "07_24_04_P" "07_24_04_P" "07_24_04_P" ...
##  $ 개방자치단체코드  : int  3150000 3150000 3220000 3220000 3220000 3220000 3220000 3220000 3040000 3040000 ...
##  $ 관리번호          : chr  "3150000-101-2020-00448" "3150000-101-2020-00449" "3220000-101-2020-00851" "3220000-101-2020-00852" ...
##  $ 인허가일자        : int  20200803 20200803 20200803 20200803 20200803 20200803 20200803 20200803 20200803 20200803 ...
##  $ 인허가취소일자    : logi  NA NA NA NA NA NA ...
##  $ 영업상태구분코드  : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ 영업상태명        : chr  "영업/정상" "영업/정상" "영업/정상" "영업/정상" ...
##  $ 상세영업상태코드  : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ 상세영업상태명    : chr  "영업" "영업" "영업" "영업" ...
##  $ 폐업일자          : chr  "" "" "" "" ...
##  $ 휴업시작일자      : logi  NA NA NA NA NA NA ...
##  $ 휴업종료일자      : logi  NA NA NA NA NA NA ...
##  $ 재개업일자        : logi  NA NA NA NA NA NA ...
##  $ 소재지전화        : chr  "" "264089282" "" "" ...
##  $ 소재지면적        : chr  "35.19" "50.37" "13.65" "13.81" ...
##  $ 소재지우편번호    : int  157210 157900 135918 135880 135840 135812 135730 135955 143200 143890 ...
##  $ 소재지전체주소    : chr  "서울특별시 강서구 마곡동 757 두산더랜드파크 B동 207호" "서울특별시 강서구 화곡동 827-2 1층" "서울특별시 강남구 역삼동 707-34 한신인터밸리24빌딩" "서울특별시 강남구 삼성동 157-18" ...
##  $ 도로명전체주소    : chr  "서울특별시 강서구 마곡중앙로 161-8, 두산더랜드파크 B동 2층 207호 (마곡동)" "서울특별시 강서구 등촌로13다길 35, 1층 (화곡동)" "서울특별시 강남구 테헤란로 322, 한신인터밸리24빌딩 지하2층 B130호 (역삼동)" "서울특별시 강남구 테헤란로83길 11, 지하2층 비201호, 주방8호 (삼성동)" ...
##  $ 도로명우편번호    : int  7788 7740 6211 6168 6197 6040 6164 6011 5116 4931 ...
##  $ 사업장명          : chr  "혼밥대왕 마곡점" "꾸어가게생선구이화곡점" "인생갈비탕" "할랄가이즈" ...
##  $ 최종수정시점      : chr  "2.02009E+13" "2.02009E+13" "2.02008E+13" "2.02008E+13" ...
##  $ 데이터갱신구분    : chr  "U" "U" "I" "I" ...
##  $ 데이터갱신일자    : chr  "40:00.0" "40:00.0" "23:26.0" "23:26.0" ...
##  $ 업태구분명        : chr  "한식" "한식" "한식" "경양식" ...
##  $ 좌표정보.x.       : num  NA 187499 204045 205012 204603 ...
##  $ 좌표정보.y.       : chr  "" "448069.5835" "444529.8404" "445106.4575" ...
##  $ 위생업태명        : chr  "한식" "한식" "한식" "경양식" ...
##  $ 남성종사자수      : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ 여성종사자수      : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ 영업장주변구분명  : chr  "" "" "" "" ...
##  $ 등급구분명        : chr  "" "" "" "" ...
##  $ 급수시설구분명    : chr  "상수도전용" "상수도전용" "" "" ...
##  $ 총종업원수        : logi  NA NA NA NA NA NA ...
##  $ 본사종업원수      : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ 공장사무직종업원수: int  NA NA NA NA NA NA NA NA NA NA ...
##  $ 공장판매직종업원수: int  NA NA NA NA NA NA NA NA NA NA ...
##  $ 공장생산직종업원수: int  NA NA NA NA NA NA NA NA NA NA ...
##  $ 건물소유구분명    : chr  "" "" "" "" ...
##  $ 보증액            : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ 월세액            : chr  "" "" "" "" ...
##  $ 다중이용업소여부  : chr  "N" "N" "N" "N" ...
##  $ 시설총규모        : num  35.2 50.4 13.6 13.8 123.3 ...
##  $ 전통업소지정번호  : chr  "" "" "" "" ...
##  $ 전통업소주된음식  : chr  "" "" "" "" ...
##  $ 홈페이지          : logi  NA NA NA NA NA NA ...
```

### 2) 분석 변수 추출, 변수이름 변경


```r
# foodshop에서 분석할 변수이름 변경하고 추출하기(먼저 추출하고 이름을 변경해도 됨)
foodshop <- foodshop %>% 
  rename(open_date=인허가일자,
         status=상세영업상태명,
         close_date=폐업일자,
         name=사업장명,
         type=업태구분명,
         address=소재지전체주소) %>% 
  select("name","type","status","open_date","close_date","address") # 분석 변수 추출

str(foodshop)
```

```
## 'data.frame':	458263 obs. of  6 variables:
##  $ name      : chr  "혼밥대왕 마곡점" "꾸어가게생선구이화곡점" "인생갈비탕" "할랄가이즈" ...
##  $ type      : chr  "한식" "한식" "한식" "경양식" ...
##  $ status    : chr  "영업" "영업" "영업" "영업" ...
##  $ open_date : int  20200803 20200803 20200803 20200803 20200803 20200803 20200803 20200803 20200803 20200803 ...
##  $ close_date: chr  "" "" "" "" ...
##  $ address   : chr  "서울특별시 강서구 마곡동 757 두산더랜드파크 B동 207호" "서울특별시 강서구 화곡동 827-2 1층" "서울특별시 강남구 역삼동 707-34 한신인터밸리24빌딩" "서울특별시 강남구 삼성동 157-18" ...
```

```r
foodshop$close_date <- as.integer(foodshop$close_date) #정수형으로 변환
```

```
## Warning: 강제형변환에 의해 생성된 NA 입니다
```

### 3) 변수의 결측치 확인


```r
summary(is.na(foodshop))
```

```
##     name            type           status        open_date      
##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
##  FALSE:458263    FALSE:458263    FALSE:458263    FALSE:458263   
##                                                                 
##  close_date       address       
##  Mode :logical   Mode :logical  
##  FALSE:335187    FALSE:458263   
##  TRUE :123076
```

### 4) 변수를 분석에 맞게 정리하고 파생변수 만들기

#### (1) type 변수


```r
foodshop$type <- ifelse(foodshop$type %in% c("까페","다방","라이브카페","커피숍"),
                        "카페",foodshop$type)
foodshop$type <- ifelse(foodshop$type %in% c("통닭(치킨)","호프/통닭"),
                        "치킨",foodshop$type)
foodshop$type <- ifelse(foodshop$type %in% c("일식","회집","횟집"),
                        "회집",foodshop$type)
foodshop$type <- ifelse(foodshop$type %in% c("경양식","패밀리레스토랑"),
                        "레스토랑",foodshop$type)
foodshop$type <- ifelse(foodshop$type=="정종/대포집/소주방",
                        "소주방",foodshop$type)
foodshop$type <- ifelse(foodshop$type=="외국음식전문점(인도,태국등)",
                        "외국음식전문점",foodshop$type)
foodshop$type <- ifelse(foodshop$type %in% c("기타","193959.1505"),
                        "NA",foodshop$type)
```

#### (2) status 변수


```r
table(foodshop$status)
```

```
## 
##   영업   폐업 
## 123075 335188
```

```r
# 문제 없음
```

#### (3) open_date 변수

이상한 날짜가 있는지를 확인하고, 있으면 결측치(NA)로 처리합니다.

연도별 개업 현황을 알아보기 위해 substr() 함수로 open_date 변수에서 앞의 4자리를 분리해서 연도만 있는 새 변수에 입력.


```r
range(foodshop$open_date) # open_date의 범위 확인
```

```
## [1] 11981207 39920706
```

```r
foodshop$open_date <- ifelse(foodshop$open_date < 19480815 |
                               foodshop$open_date > 20201231 , NA, foodshop$open_date) # 이상치 처리

table(is.na(foodshop$open_date)) # 결측치 빈도 확인
```

```
## 
##  FALSE   TRUE 
## 458123    140
```

```r
foodshop$open_year <- substr(foodshop$open_date, 1, 4) # foodshop$open_year 변수 만들기
```

#### (4) close_date 변수


```r
range(foodshop$close_date, na.rm = T) # 범위 구하기, na.rm = T를 지정
```

```
## [1]    20723 50080306
```

```r
foodshop$close_date <- ifelse(foodshop$close_date < 19480815 |
                               foodshop$close_date > 20201231 , NA, foodshop$close_date) # 이상치 처리

table(is.na(foodshop$close_date)) # 결측치 빈도 확인
```

```
## 
##  FALSE   TRUE 
## 334265 123998
```

```r
foodshop$close_year <- substr(foodshop$close_date, 1, 4) # foodshop$close_year 변수 만들기
```

#### (5) address 변수


```r
foodshop$district <- substr(foodshop$address, 7, 9)

table(foodshop$district)
```

```
## 
##        강남구 강동구 강북구 강서구 관악구 광진구 구로구 금천구 노원구  도 제 
##    238  42617  20620  15072  19338  19741  15745  17241  11983  13984      1 
## 도봉구 동대문 동작구 마포구  번지  서대문 서초구 성동구 성북구 송파구 수영구 
##  10774  19421  12866  23700      1  15938  24054  12277  15787  25955      1 
##  시 분  시 수  시 원  시 일 양천구 영등포 용산구 은평구 종로구  중구  중랑구 
##      1      1      1      2  15694  23432  13339  15148  18257  19418  15616
```

```r
foodshop$district <-
  ifelse(foodshop$district %in% c("도 제", "번지 ", "시 분", "시 수", "시 원", "시 일"),
         NA, foodshop$district)
```

### 5) 분석 데이터 최종 확인


```r
str(foodshop)
```

```
## 'data.frame':	458263 obs. of  9 variables:
##  $ name      : chr  "혼밥대왕 마곡점" "꾸어가게생선구이화곡점" "인생갈비탕" "할랄가이즈" ...
##  $ type      : chr  "한식" "한식" "한식" "레스토랑" ...
##  $ status    : chr  "영업" "영업" "영업" "영업" ...
##  $ open_date : int  20200803 20200803 20200803 20200803 20200803 20200803 20200803 20200803 20200803 20200803 ...
##  $ close_date: int  NA NA NA NA NA NA NA NA NA NA ...
##  $ address   : chr  "서울특별시 강서구 마곡동 757 두산더랜드파크 B동 207호" "서울특별시 강서구 화곡동 827-2 1층" "서울특별시 강남구 역삼동 707-34 한신인터밸리24빌딩" "서울특별시 강남구 삼성동 157-18" ...
##  $ open_year : chr  "2020" "2020" "2020" "2020" ...
##  $ close_year: chr  NA NA NA NA ...
##  $ district  : chr  "강서구" "강서구" "강남구" "강남구" ...
```

```r
foodshop$open_year <- as.integer(foodshop$open_year)

foodshop$close_year <- as.integer(foodshop$close_year)
```

## 5. 데이터 분석

### 1) 오래된 음식점 찾아보기

#### (1) 가장 오래 영업 중인 음식점


```r
foodshop %>% 
  filter(!is.na(open_date) & status=="영업") %>%  # 결측치 제거, 영업 데이터 추출
  filter(open_date==min(open_date)) %>%  # 개업일이 가장 빠른 데이터 추출
  select(name, type, open_date, address) # 4개 변수 추출
```

```
##     name type open_date                          address
## 1 혜심정 한식  19630422 서울특별시 강북구 우이동 300번지
```

#### (2) 주요 업종별로 가장 오래 영업 중인 음식점


```r
foodshop %>% 
  filter(!is.na(open_date) & status=="영업") %>%  # 결측치 제거, 영업 데이터 추출
  filter(type %in% c("분식", "레스토랑", "치킨", "회집", "중국식", "카페", "패스트푸드")) %>% 
  group_by(type) %>% 
  filter(open_date==min(open_date)) %>%  # 개업일이 가장 빠른 데이터 추출
  arrange(open_date) %>%  # 개업일 기준 오름차순 정렬
  select(type, name, open_date, address)
```

```
## # A tibble: 7 x 4
## # Groups:   type [7]
##   type       name                  open_date address                            
##   <chr>      <chr>                     <int> <chr>                              
## 1 카페       커핀그루나루 대학로점  19650208 서울특별시 종로구 명륜4가 88-2번지 
## 2 레스토랑   사카                   19651010 서울특별시 중구 초동 40-4번지      
## 3 중국식     태화관                 19651010 서울특별시 용산구 후암동 244-60번~ 
## 4 회집       교동 전선생            19660806 서울특별시 중구 회현동1가 92-1번지 
## 5 분식       동명삼계탕             19670127 서울특별시 종로구 중학동 91-0번지  
## 6 치킨       또봉이통닭 대학로점    19670327 서울특별시 종로구 명륜4가 46-1     
## 7 패스트푸드 59피자                 19791119 서울특별시 강서구 등촌동 513-1번지~
```

### 2) 개업, 영업, 폐업 현황 알아보기

#### (1) 업종별 개업 비율


```r
foodshop %>% 
  filter(!is.na(open_date) & !is.na(type) & !is.na(district)) %>%  # 결측지 제외
  group_by(type) %>%   # type 변수의 범주별로 분류
  summarise(n=n()) %>%   # 범주별 빈도 구하기
  mutate(total=sum(n),
         pct=round(n/total*100,1)) %>% 
  arrange(desc(n)) %>% 
  head(10)
```

```
## # A tibble: 10 x 4
##    type            n  total   pct
##    <chr>       <int>  <int> <dbl>
##  1 한식       194378 458116  42.4
##  2 분식        78229 458116  17.1
##  3 레스토랑    46615 458116  10.2
##  4 치킨        43581 458116   9.5
##  5 NA          28618 458116   6.2
##  6 회집        18078 458116   3.9
##  7 중국식      14192 458116   3.1
##  8 소주방      12741 458116   2.8
##  9 카페         8172 458116   1.8
## 10 패스트푸드   3925 458116   0.9
```

한식이 전체 42만9273개 중 45.3%인 19만 4289개로 압도적으로 많다.

#### (2) 영업 중인 음식점의 업종별 비율


```r
foodshop %>% 
  filter(!is.na(open_date) & !is.na(type) & !is.na(district)) %>%  # 결측지 제외
  filter(status=="영업") %>% 
  group_by(type) %>% 
  summarise(n=n()) %>% 
  mutate(total=sum(n),
         pct=round(n/total*100,1)) %>%
  arrange(desc(pct)) %>% 
  head(5)
```

```
## # A tibble: 5 x 4
##   type         n  total   pct
##   <chr>    <int>  <int> <dbl>
## 1 한식     53555 123057  43.5
## 2 NA       16106 123057  13.1
## 3 치킨     13882 123057  11.3
## 4 분식      9686 123057   7.9
## 5 레스토랑  9115 123057   7.4
```

영업 중인 전체 음식점은 10만 6855개 이 중 한식이 50.1%로 가장 많다.

#### (3) 전체 음식점의 영업과 폐업 비율 알아보기


```r
foodshop %>% 
  filter(!is.na(open_date) & !is.na(type) & !is.na(district)) %>% 
  group_by(status) %>% 
  summarise(n=n()) %>% 
  mutate(total=sum(n),
         pct=round(n/total*100,1))
```

```
## # A tibble: 2 x 4
##   status      n  total   pct
##   <chr>   <int>  <int> <dbl>
## 1 영업   123057 458116  26.9
## 2 폐업   335059 458116  73.1
```

42만 9273개 음식점 가운데 75.1%가 폐업하고, 24.9%만 영업중이다.

#### (4) 주요 업종별 영업과 폐업 비율 알아보기


```r
foodshop %>% 
  filter(!is.na(open_date) & !is.na(type) & !is.na(district)) %>%  # 결측지 제외
  filter(type %in% c("한식", "분식", "레스토랑", "치킨", "회집", "카페")) %>% 
  group_by(type, status) %>% 
  summarise(n=n()) %>% 
  mutate(total=sum(n),
        pct=round(n/total*100,1)) %>%
  filter(status=="영업") %>% 
  arrange(desc(pct))
```

```
## `summarise()` has grouped output by 'type'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 6 x 5
## # Groups:   type [6]
##   type     status     n  total   pct
##   <chr>    <chr>  <int>  <int> <dbl>
## 1 회집     영업    6399  18078  35.4
## 2 카페     영업    2624   8172  32.1
## 3 치킨     영업   13882  43581  31.9
## 4 한식     영업   53555 194378  27.6
## 5 레스토랑 영업    9115  46615  19.6
## 6 분식     영업    9686  78229  12.4
```

  회집의 영업률이 35.4%로 가장 높다. 분식은 12.4%로 매우 낮다.

### 3) 연도별 분석

#### (1) 개업이 많았던 연도


```r
foodshop %>% 
  filter(!is.na(open_date) & !is.na(district)) %>% 
  group_by(open_year) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(5)
```

```
## # A tibble: 5 x 2
##   open_year     n
##       <int> <int>
## 1      2001 18858
## 2      1994 18062
## 3      1999 17921
## 4      2000 16276
## 5      1993 16210
```

  1999년 ~ 2001년과 1993년 ~ 1994년에 많이 개업했습니다.

#### (2) 폐업이 많았던 연도


```r
foodshop %>% 
  filter(!is.na(close_date) & !is.na(district)) %>% 
  group_by(close_year) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(5)
```

```
## # A tibble: 5 x 2
##   close_year     n
##        <int> <int>
## 1       1999 15868
## 2       2000 15782
## 3       2005 14945
## 4       2002 14137
## 5       2001 13637
```

1999년부터 2005년 사이에 폐업이 가장 많았다.

#### (3) 개업 음식점수와 폐업 음식점수를 그래프로 그리기

##### (1) 연도별 개업 음식점수 그래프


```r
# 연도별 개업 음식점수 구하기

open_trend <- foodshop %>% 
  filter(!is.na(open_date) & !is.na(district)) %>% 
  group_by(open_year) %>% 
  summarise(open_n=n())

# open_trend의 구조 보기
str(open_trend)
```

```
## tibble [66 x 2] (S3: tbl_df/tbl/data.frame)
##  $ open_year: int [1:66] 1951 1953 1954 1955 1957 1958 1960 1962 1963 1964 ...
##  $ open_n   : int [1:66] 1 1 2 2 1 1 3 1 1 3 ...
```

```r
# 연도별 개업 음식점수 막대그래프 그리기
ggplot(data = open_trend, aes(x=open_year, y=open_n))+
  geom_col()+
  xlab("연도")+
  ylab("개업수")
```

![](/images/Example_10_4_files/figure-html/unnamed-chunk-19-1.png)<!-- -->


##### (2) 연도별 폐업 음식점수 그래프


```r
# 연도별 폐업 음식점수 구하기

close_trend <- foodshop %>% 
  filter(!is.na(close_date) & !is.na(district)) %>% 
  group_by(close_year) %>% 
  summarise(close_n=n())

# open_trend의 구조 보기
str(close_trend)
```

```
## tibble [47 x 2] (S3: tbl_df/tbl/data.frame)
##  $ close_year: int [1:47] 1963 1965 1975 1976 1977 1978 1980 1981 1982 1983 ...
##  $ close_n   : int [1:47] 1 1 1 4 1 1 1 2 3 13 ...
```

```r
# 연도별 개업 음식점수 막대그래프 그리기
ggplot(data = close_trend, aes(x=close_year, y=close_n))+
  geom_col()+
  xlab("연도")+
  ylab("개업수")
```

![](/images/Example_10_4_files/figure-html/unnamed-chunk-20-1.png)<!-- -->


##### (3) 개업과 폐업 음식점수를 통합해서 그리기


```r
open_trend1 <- rename(open_trend, year= open_year)
close_trend1 <- rename(close_trend, year= close_year)

open_close_trend <- left_join(open_trend1, close_trend1, by="year")

# 통합 그래프 그리기

ggplot()+
  geom_line(data=open_close_trend, aes(year, open_n))+
  geom_line(data=open_close_trend, aes(year, close_n, color="red"))+
  xlab("연도")+
  ylab("개수")
```

```
## Warning: Removed 8 row(s) containing missing values (geom_path).
```

![](/images/Example_10_4_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

```r
# 다음과 같이 해도 됩니다.
# ggplot(data=open_close_trend)+
#   geom_line(aes(year, open_n))+
#   geom_line(aes(year, close_n, color="red))+
```

##### (4) 폐업 음식점수가 개업 음식점수보다 많았던 기간 확인하기


```r
open_close_trend %>%  filter(close_n>open_n)
```

```
## # A tibble: 6 x 3
##    year open_n close_n
##   <int>  <int>   <int>
## 1  2005  12505   14945
## 2  2006  10968   13472
## 3  2007  10790   12000
## 4  2008   9797   11305
## 5  2011  11112   11403
## 6  2013  10373   10632
```

  2005년 ~ 2008년 까지 개업보다 폐업이 많았다.
  
### 4) 지역별 분석

#### (1) 영업 중인 음식점수가 가장 많은 5개 구를 알아보기


```r
# 구별로 영업 중인 음식점 빈도를 구해서 새 객체 만들기

district_business <- foodshop %>% 
  filter(!is.na(open_date) & !is.na(district) & status=="영업") %>% 
  group_by(district) %>% 
  summarise(n=n())

# 영업 중인 음식점수가 많은 5개 구를 출력하기
district_business %>% 
  arrange(desc(n)) %>% 
  head(5)
```

```
## # A tibble: 5 x 2
##   district     n
##   <chr>    <int>
## 1 강남구   11933
## 2 마포구    7996
## 3 종로구    7094
## 4 송파구    6991
## 5 영등포    6486
```

  강남구가 가장 많다.
  
#### (2) 25개 구의 음식점수를 막대그래프로 그리기


```r
ggplot(data=district_business, aes(x=reorder(district, n), y=n))+
  geom_col()+
  coord_flip()+
  xlab("구 이름")+
  ylab("영업 음식점 수")
```

![](/images/Example_10_4_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

#### (3) 주요 업종별로 가장 많은 구를 알아보기


```r
foodshop %>% 
  filter(!is.na(open_date) & !is.na(district)) %>% 
  filter(type %in% c("한식", "분식", "치킨", "레스토랑", "회집", "중국식", "카페", "패스트푸드")) %>% 
  filter(status=="영업") %>% 
  group_by(type, district) %>% 
  summarise(n=n()) %>% 
  mutate(total=sum(n),
         pct=round(n/100,1)) %>% 
  group_by(type) %>% 
  filter(pct==max(pct))
```

```
## `summarise()` has grouped output by 'type'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 8 x 5
## # Groups:   type [8]
##   type       district     n total   pct
##   <chr>      <chr>    <int> <int> <dbl>
## 1 레스토랑   강남구    2237  9115  22.4
## 2 분식       강남구     935  9686   9.3
## 3 중국식     영등포     500  4439   5  
## 4 치킨       구로구     882 13882   8.8
## 5 카페       마포구     541  2624   5.4
## 6 패스트푸드 강남구     213   799   2.1
## 7 한식       강남구    4830 53555  48.3
## 8 회집       강남구     865  6399   8.7
```

  레스토랑, 분식, 패스트푸드, 한식, 회집은 강남구에 가장 많습니다.



## 연습 문제 10 - 4

### 1. 종로구에서 현재 영업하고 있는 업종의 비율을 상위 5위까지 알아보세요.


```r
foodshop %>% 
  filter(!is.na(open_date) & !is.na(type) & !is.na(district)) %>% 
  filter(district=="종로구") %>% 
  filter(status=="영업") %>% 
  group_by(type) %>% 
  summarise(n=n()) %>% 
  mutate(total=sum(n),
         oct=round(n/total*100,1)) %>% 
  arrange(desc(n)) %>% 
  head(5)
```

```
## # A tibble: 5 x 4
##   type         n total   oct
##   <chr>    <int> <int> <dbl>
## 1 한식      3032  7094  42.7
## 2 NA        1153  7094  16.3
## 3 레스토랑   993  7094  14  
## 4 분식       773  7094  10.9
## 5 회집       294  7094   4.1
```

### 2. 2020년은 코로나19로 경제가 매우 어려웠습니다. 이런 상황에서 2020년에 개업한 음식점수와 영업, 폐업의 비율을 알아보세요.


```r
foodshop %>% 
  filter(!is.na(open_date) & !is.na(district) & !is.na(type)) %>% 
  filter(open_year==2020) %>% 
  group_by(status) %>% 
  summarise(n=n()) %>% 
  mutate(total=sum(n),
         pct=round(n/100,1))
```

```
## # A tibble: 2 x 4
##   status     n total   pct
##   <chr>  <int> <int> <dbl>
## 1 영업   12667 13384 127. 
## 2 폐업     717 13384   7.2
```


### 3. 2020년에 개업한 음식점 가운데 비율이 높은 5개 업종의 음식점수와 비율을 구해보세요.


```r
foodshop %>% 
  filter(!is.na(open_date) & !is.na(district) & !is.na(type)) %>% 
  filter(open_year==2020) %>% 
  group_by(type) %>% 
  summarise(n=n()) %>% 
  mutate(total=sum(n),
         pct=round(n/100,1)) %>% 
  arrange(desc(n)) %>% 
  head(5)
```

```
## # A tibble: 5 x 4
##   type         n total   pct
##   <chr>    <int> <int> <dbl>
## 1 한식      4967 13384  49.7
## 2 NA        3714 13384  37.1
## 3 레스토랑  1137 13384  11.4
## 4 치킨      1027 13384  10.3
## 5 분식       744 13384   7.4
```




