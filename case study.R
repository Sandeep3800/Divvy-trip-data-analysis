install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(conflicted)
library(conflicted)
library(dplyr)
filter(mtcars, cyl == 8)
library(lubridate)
q()
# Install from CRAN
install.packages('rmarkdown')
# Or if you want to test the development version,
# install from GitHub
if (!requireNamespace("devtools"))
  install.packages('devtools')
devtools::install_github('rstudio/rmarkdown')
q()
library("tidyverse")
install.packages("tidyverse")
library("tidyverse")
install.packages("lubridate")
library("lubridate")
library("hms")
library("data.table")
library("dplyr")
jan01_df <- read_csv('202201-divvy-tripdata.csv')
jan01_df <- read_csv("202201-divvy-tripdata.csv")
jan01_df <- read_csv("202201-divvy-tripdata.csv")
feb02_df <- read_csv("202202-divvy-tripdata.csv")
mar03_df <- read_csv("202203-divvy-tripdata.csv")
apr04_df <- read_csv("202204-divvy-tripdata.csv")
may05_df <- read_csv("202205-divvy-tripdata.csv")
jun06_df <- read_csv("202206-divvy-tripdata.csv")
jul07_df <- read_csv("202207-divvy-tripdata.csv")
aug08_df <- read_csv("202208-divvy-tripdata.csv")
sep09_df <- read_csv('202209-divvy-publictripdata.csv')
oct10_df <- read_csv('202210-divvy-tripdata.csv')
nov11_df <- read_csv("202211-divvy-tripdata.csv")
dec12_df <- read_csv('202212-divvy-tripdata.csv')

# merge all of the data frames into one year
cyclistic_df <- rbind(jan01_df, feb02_df, mar03_df, apr04_df, may05_df, jun06_df, jul07_df, aug08_df, sep09_df, oct10_df, nov11_df, dec12_df)

# remove individual month frames to make space
remove(jan01_df, feb02_df, mar03_df, apr04_df, may05_df, jun06_df, jul07_df, aug08_df, sep09_df, oct10_df, nov11_df, dec12_df)

# create a new data frame for new columns
cyclistic_data <- cyclistic_df

#calculating ride length by substracting end_at_time and started_at_time 

cyclistic_data$ride_length <- difftime(cyclistic_df$ended_at, cyclistic_df$started_at, units = 'mins')

#create individual column for date, time, month, day, year

cyclistic_data$date <- as.Date(cyclistic_data$started_at)

> cyclistic_data$day_of_week <- wday(cyclistic_data$started_at)

> cyclistic_data$day_of_week <- format(as.Date(cyclistic_data$date), "%A")

> cyclistic_data$month <- format(as.Date(cyclistic_data$date), '%m')

> cyclistic_data$day <- format(as.Date(cyclistic_data$day), '%d')
# remove individual data frames to make space 
> remove(jan_df, feb_df, mar_df, apr_df, may_df, jun_df, jul_df, aug_df, sep_df, oct_df, nov_df, dec_df)
> 
> # create a new data frame for new columns
> 
> cyclistic_data <- cyclistic_df
> 
> # calculate ride length by substracting end_at_time and started_at_time 
> 
> cyclistic_data$ride_length <- difftime(cyclistic_df$ended_at, cyclistic_df$started_at, units = 'mins')
> 
> # create a individual column for date, time, month, day, year.
  
> cyclistic_data$date <- as.Date(cyclistic_data$started_at)

> cyclistic_data$day_of_week <- wday(cyclistic_df$started_at)

> cyclistic_data$day_of_week <- format(as.Date(cyclistic_data$date), "%A")

> cyclistic_data$day_of_week <- format(as.Date(cyclistic_data$date), "%A")

> cyclistic_data$month <- format(as.Date(cyclistic_data$date), '%m') 

 cyclistic_data$day <- format(as.Date(cyclistic_data$date), '%d')
 
 cyclistic_data$year <-format(as.Date(cyclistic_data$date), '%y')

> cyclistic_data$time <- format(as.Date(cyclistic_data$date), '%h:%m:%s')

> cyclistic_data$time <- as_hms(cyclistic_data$started_at)
> cyclistic_data$hour <- hour(cyclistic_data$time)
> 
> #create columns for different seasons
> 
> cyclistic_data <- cyclistic_data 
> cyclistic_data <- cyclistic_data  %>% mutate(season = 
+                                              case_when(month == "03" ~ "Spring",
+                                                        month == "04" ~ "Spring",
+                                                        month == "05" ~ "Spring",
+                                                        month == "06"  ~ "Summer",
+                                                        month == "07"  ~ "Summer",
+                                                        month == "08"  ~ "Summer",
+                                                        month == "09" ~ "Fall",
+                                                        month == "10" ~ "Fall",
+                                                        month == "11" ~ "Fall",
+                                                        month == "12" ~ "Winter",
+                                                        month == "01" ~ "Winter",
+                                                        month == "02" ~ "Winter")
+ )


> cyclistic_data <- cyclistic_data %>% mutate(time_of_day = 
                                             case_when(hour == "0" ~ "Night",
                                                       hour == "1" ~ "Night",
                                                        hour == "2" ~ "Night",
                                                        hour == "3" ~ "Night",
                                                        hour == "4" ~ "Night",
                                                        hour == "5" ~ "Night",
                                                        hour == "6" ~ "Morning",
                                                        hour == "7" ~ "Morning",
                                                        hour == "8" ~ "Morning",
                                                        hour == "9" ~ "Morning",
                                                        hour == "10" ~ "Morning",
                                                        hour == "11" ~ "Morning",
                                                        hour == "12" ~ "Afternoon",
                                                        hour == "13" ~ "Afternoon",
                                                        hour == "14" ~ "Afternoon",
                                                        hour == "15" ~ "Afternoon",
                                                        hour == "16" ~ "Afternoon",
                                                        hour == "17" ~ "Afternoon",
                                                        hour == "18" ~ "Evening",
                                                        hour == "19" ~ "Evening",
                                                        hour == "20" ~ "Evening",
                                                        hour == "21" ~ "Evening",
                                                        hour == "22" ~ "Evening",
                                                        hour == "23" ~ "Evening")
 )
 
 # clean data
 cyclistic_data <- na.omit(cyclistic_data)
 cyclistic_data <- distinct(cyclistic_data)
 cyclistic_data <- cyclistic_data[!(cyclistic_data$ride_length <= 0),]
 cyclistic_data <- cyclistic_data %>% 
+    select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng)) 
 
 view(cyclistic_data)
 
 # total rides by member type
 rides_member_type <- cyclistic_date %>%
+ group_by(rideable_type) %>% 
+ count(rideable_type)
# A tibble: 3 × 2
# Groups:   rideable_type [3]
  rideable_type       n
  <chr>           <int>
1 classic_bike  2597301
2 docked_bike    174852
3 electric_bike 1596899
 
 # total rides by member type
> cyclistic_data %>% 
+ group_by(member_casual) %>%
+ count(hour) %>% 
+ print(n = 48)
# A tibble: 48 × 3
# Groups:   member_casual [2]
   member_casual  hour      n
   <chr>         <int>  <int>
 1 casual            0  33310
 2 casual            1  21320
 3 casual            2  12672
 4 casual            3   7199
 5 casual            4   4660
 6 casual            5   8418
 7 casual            6  21677
 8 casual            7  37864
 9 casual            8  52233
10 casual            9  54610
11 casual           10  72352
12 casual           11  94712
13 casual           12 111665
14 casual           13 115910
15 casual           14 122697
16 casual           15 135558
17 casual           16 151990
18 casual           17 169885
19 casual           18 150217
20 casual           19 114078
21 casual           20  83087
22 casual           21  71172
23 casual           22  63872
24 casual           23  46889
25 member            0  25221
26 member            1  15554
27 member            2   8593
28 member            3   5243
29 member            4   6160
30 member            5  26083
31 member            6  75873
32 member            7 143245
33 member            8 167012
34 member            9 112432
35 member           10 104175
36 member           11 124950
37 member           12 144064
38 member           13 141621
39 member           14 141249
40 member           15 172033
41 member           16 232567
42 member           17 281223
43 member           18 223727
44 member           19 159558
45 member           20 110449
46 member           21  85431
47 member           22  63909
48 member           23  40633
 
 # total no. of hours
 
> cyclistic_data %>% 
+ count(hour)%>%
+ print(n = 2500)
# A tibble: 24 × 2
    hour      n
   <int>  <int>
 1     0  58531
 2     1  36874
 3     2  21265
 4     3  12442
 5     4  10820
 6     5  34501
 7     6  97550
 8     7 181109
 9     8 219245
10     9 167042
11    10 176527
12    11 219662
13    12 255729
14    13 257531
15    14 263946
16    15 307591
17    16 384557
18    17 451108
19    18 373944
20    19 273636
21    20 193536
22    21 156603
23    22 127781
24    23  87522
 
 # morning
 # total rides by membertype
 
 cyclistic_data %>% 
+ group_by(member_casual) %>%
+ filter(time_of_day == "Morning") %>% 
+   count(time_of_day)
# A tibble: 2 × 3
# Groups:   member_casual [2]
  member_casual time_of_day      n
  <chr>         <chr>        <int>
1 casual        Morning     333448
2 member        Morning     727687
 
 cyclistic_data %>% 
+ group_by(member_casual) %>%
+ count(time_of_day)
# A tibble: 8 × 3
# Groups:   member_casual [2]
  member_casual time_of_day       n
  <chr>         <chr>         <int>
1 casual        Afternoon    807705
2 casual        Evening      529315
3 casual        Morning      333448
4 casual        Night         87579
5 member        Afternoon   1112757
6 member        Evening      683707
7 member        Morning      727687
8 member        Night         86854
 
 cyclistic_data %>% 
+ group_by(member_casual %>%
+ filter(time_of_day == "Afternoon") %>%
+ count(time_of_day)
+ 
+ cyclistic_data %>% 
Error: unexpected symbol in:
"
cyclistic_data"
> cyclistic_data %>% 
+ filter(time_of_day == "Afternoon") %>%
+ count(time_of_day)
# A tibble: 1 × 2
  time_of_day       n
  <chr>         <int>
1 Afternoon   1920462
 
 cyclistic_data %>% 
+ group_by(member_casual) %>% 
+ filter(time_of_day == "Evening") %>%
+ count(time_of_day)
# A tibble: 2 × 3
# Groups:   member_casual [2]
  member_casual time_of_day      n
  <chr>         <chr>        <int>
1 casual        Evening     529315
2 member        Evening     683707
 
 cyclistic_data %>% 
+ filter(time_of_day == 'Evening') %>%
+ count(time_of_day)
# A tibble: 1 × 2
  time_of_day       n
  <chr>         <int>
1 Evening     1213022
 
 cyclistic_data %>% 
+ filter(time_of_day == "Night") %>%
+ count(time_of_day)
# A tibble: 1 × 2
  time_of_day      n
  <chr>        <int>
1 Night       174433

 cyclistic_data %>% 
+ group_by(member_casual) %>%
+ filter(time_of_day == "Night") %>% 
+ count(time_of_day)
# A tibble: 2 × 3
# Groups:   member_casual [2]
  member_casual time_of_day     n
  <chr>         <chr>       <int>
1 casual        Night       87579
2 member        Night       86854
 
 cyclistic_data %>% 
+ group_by(member_casual) %>%
+ count(time_of_day)
# A tibble: 8 × 3
# Groups:   member_casual [2]
  member_casual time_of_day       n
  <chr>         <chr>         <int>
1 casual        Afternoon    807705
2 casual        Evening      529315
3 casual        Morning      333448
4 casual        Night         87579
5 member        Afternoon   1112757
6 member        Evening      683707
7 member        Morning      727687
8 member        Night         86854
> 
> cyclistic_data %>% 
+ group_by(time_of_day) %>% 
+ count(time_of_day)
# A tibble: 4 × 2
# Groups:   time_of_day [4]
  time_of_day       n
  <chr>         <int>
1 Afternoon   1920462
2 Evening     1213022
3 Morning     1061135
4 Night        174433
> 
> cyclistic_data %>%
+ group_by(member_casual) %>% 
+ count(day_of_week)
# A tibble: 14 × 3
# Groups:   member_casual [2]
   member_casual day_of_week      n
   <chr>         <chr>        <int>
 1 casual        Friday      248785
 2 casual        Monday      210746
 3 casual        Saturday    367310
 4 casual        Sunday      301278
 5 casual        Thursday    229993
 6 casual        Tuesday     196367
 7 casual        Wednesday   203568
 8 member        Friday      360029
 9 member        Monday      375151
10 member        Saturday    338255
11 member        Sunday      297707
12 member        Thursday    415862
13 member        Tuesday     411226
14 member        Wednesday   412775
> 
> cyclistic_data %>%
+ count(day_of_week)
# A tibble: 7 × 2
  day_of_week      n
  <chr>        <int>
1 Friday      608814
2 Monday      585897
3 Saturday    705565
4 Sunday      598985
5 Thursday    645855
6 Tuesday     607593
7 Wednesday   616343
> 
> cyclistic_data %>% 
+ group_by(member_casual) %>%
+ count(day) %>%
+ print(n = 62)
# A tibble: 62 × 3
# Groups:   member_casual [2]
   member_casual day       n
   <chr>         <chr> <int>
 1 casual        01    53936
 2 casual        02    59280
 3 casual        03    61697
 4 casual        04    55100
 5 casual        05    61510
 6 casual        06    48302
 7 casual        07    47145
 8 casual        08    43846
 9 casual        09    68491
10 casual        10    71505
11 casual        11    53605
12 casual        12    54948
13 casual        13    59077
14 casual        14    61585
15 casual        15    49373
16 casual        16    66468
17 casual        17    59281
18 casual        18    54056
19 casual        19    57111
20 casual        20    57099
21 casual        21    60703
22 casual        22    58044
23 casual        23    70844
24 casual        24    58903
25 casual        25    42333
26 casual        26    55015
27 casual        27    48537
28 casual        28    58579
29 casual        29    64557
30 casual        30    64337
31 casual        31    32780
32 member        01    90427
33 member        02    87657
34 member        03    81346
35 member        04    81300
36 member        05    82586
37 member        06    79702
38 member        07    84070
39 member        08    81544
40 member        09    95176
41 member        10    97083
42 member        11    83148
43 member        12    88277
44 member        13    89437
45 member        14    92247
46 member        15    79795
47 member        16    91855
48 member        17    82921
49 member        18    78672
50 member        19    85893
51 member        20    85294
52 member        21    95240
53 member        22    86485
54 member        23    89821
55 member        24    79678
56 member        25    68806
57 member        26    83511
58 member        27    81148
59 member        28    88984
60 member        29    88050
61 member        30    79039
62 member        31    51813
> 
> 
> cyclistic_data %>%
+ group_by(member_casual) %>%
+ count(month) %>%
+ print(n = 24)
# A tibble: 24 × 3
# Groups:   member_casual [2]
   member_casual month      n
   <chr>         <chr>  <int>
 1 casual        01     12605
 2 casual        02     15143
 3 casual        03     67150
 4 casual        04     91889
 5 casual        05    220232
 6 casual        06    292053
 7 casual        07    311649
 8 casual        08    270074
 9 casual        09    220905
10 casual        10    151312
11 casual        11     73533
12 casual        12     31502
13 member        01     67523
14 member        02     74031
15 member        03    148821
16 member        04    180657
17 member        05    282284
18 member        06    328258
19 member        07    330980
20 member        08    335201
21 member        09    314214
22 member        10    262926
23 member        11    182219
24 member        12    103891
> 
> cyclistic_data %>% 
+ group_by(member_casual) %>%
+ filter(season == "Spring") %>%
+ count(season)
# A tibble: 2 × 3
# Groups:   member_casual [2]
  member_casual season      n
  <chr>         <chr>   <int>
1 casual        Spring 379271
2 member        Spring 611762
> cyclistic_data %>%
+ filter(season == "Spring") %>%
+ count(season)
# A tibble: 1 × 2
  season      n
  <chr>   <int>
1 Spring 991033
> 
> cyclistic_data %>%
+ group_by(member_casual)%>%
+ filter(season == "Summer") %>%
+ count(season)
# A tibble: 2 × 3
# Groups:   member_casual [2]
  member_casual season      n
  <chr>         <chr>   <int>
1 casual        Summer 873776
2 member        Summer 994439
> cyclistic_data %>%
+ filter(season == "Summer") %>%
+ count(season)
# A tibble: 1 × 2
  season       n
  <chr>    <int>
1 Summer 1868215
> 
> cyclistic_data %>%
+ group_by(member_casual) %>%
+ filter(season == "Fall") %>%
+ count(season)
# A tibble: 2 × 3
# Groups:   member_casual [2]
  member_casual season      n
  <chr>         <chr>   <int>
1 casual        Fall   445750
2 member        Fall   759359
> 
> cyclistic_data %>%
+ filter(season == 'Fall') %>%
+ count(season)
# A tibble: 1 × 2
  season       n
  <chr>    <int>
1 Fall   1205109
> 
> cyclistic_data %>%
+ group_by(member_casual) %>%
+ filter(season == 'Winter') %>%
+ count(season)
# A tibble: 2 × 3
# Groups:   member_casual [2]
  member_casual season      n
  <chr>         <chr>   <int>
1 casual        Winter  59250
2 member        Winter 245445
> 
> cyclistic_data %>% 
+ filter(season == 'Winter') %>% 
+ count(season)
# A tibble: 1 × 2
  season      n
  <chr>   <int>
1 Winter 304695
> 
> cyclistic_data %>%
+ group_by(season, member_casual) %>%
+ count(season)
# A tibble: 8 × 3
# Groups:   season, member_casual [8]
  season member_casual      n
  <chr>  <chr>          <int>
1 Fall   casual        445750
2 Fall   member        759359
3 Spring casual        379271
4 Spring member        611762
5 Summer casual        873776
6 Summer member        994439
7 Winter casual         59250
8 Winter member        245445
> 
> cyclistic_data %>%
+ group_by(season) %>%
+ count(season)
# A tibble: 4 × 2
# Groups:   season [4]
  season       n
  <chr>    <int>
1 Fall   1205109
2 Spring  991033
3 Summer 1868215
4 Winter  304695
> 
> cyclistic_avgride <- mean(cyclistic_data$ride_length)
> print(cyclistic_avgride)
Time difference of 17.09677 mins
> 
> cyclistic_data %>% group_by( member_casual) %>% 
+   summarise_at(vars(ride_length),
+                list(time = mean))
# A tibble: 2 × 2
  member_casual time         
  <chr>         <drtn>       
1 casual        23.99450 mins
2 member        12.45238 mins
> 
> # type of bike
> cyclistic_data %>% group_by(member_casual, rideable_type) %>% 
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 5 × 3
# Groups:   member_casual [2]
  member_casual rideable_type time         
  <chr>         <chr>         <drtn>       
1 casual        classic_bike  24.44771 mins
2 casual        docked_bike   50.70956 mins
3 casual        electric_bike 16.68823 mins
4 member        classic_bike  13.23431 mins
5 member        electric_bike 10.97195 mins
> 
> cyclistic_data %>% group_by(rideable_type) %>% 
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 3 × 2
  rideable_type time         
  <chr>         <drtn>       
1 classic_bike  17.07124 mins
2 docked_bike   50.70956 mins
3 electric_bike 13.45788 mins
> 
> cyclistic_data %>% 
+ group_by(hour, member_casual) %>%
+ summarise_at(vars(ride_length), list(time = mean)) %>% 
+ print(n = 48) # lets you view entire tibble
# A tibble: 48 × 3
# Groups:   hour [24]
    hour member_casual time         
   <int> <chr>         <drtn>       
 1     0 casual        21.21345 mins
 2     0 member        12.41811 mins
 3     1 casual        24.04439 mins
 4     1 member        12.30597 mins
 5     2 casual        22.43488 mins
 6     2 member        12.39141 mins
 7     3 casual        22.00207 mins
 8     3 member        12.25813 mins
 9     4 casual        19.67710 mins
10     4 member        12.46609 mins
11     5 casual        16.54682 mins
12     5 member        10.34195 mins
13     6 casual        17.07450 mins
14     6 member        10.82999 mins
15     7 casual        15.42928 mins
16     7 member        11.37031 mins
17     8 casual        17.93963 mins
18     8 member        11.16202 mins
19     9 casual        23.84361 mins
20     9 member        11.44289 mins
21    10 casual        28.23764 mins
22    10 member        12.24412 mins
23    11 casual        28.49636 mins
24    11 member        12.56537 mins
25    12 casual        27.72916 mins
26    12 member        12.25057 mins
27    13 casual        28.07127 mins
28    13 member        12.47765 mins
29    14 casual        27.70400 mins
30    14 member        12.82825 mins
31    15 casual        26.33648 mins
32    15 member        12.72806 mins
33    16 casual        24.15263 mins
34    16 member        12.94037 mins
35    17 casual        22.35433 mins
36    17 member        13.21970 mins
37    18 casual        22.26418 mins
38    18 member        13.15526 mins
39    19 casual        22.76690 mins
40    19 member        12.96705 mins
41    20 casual        22.08176 mins
42    20 member        12.76230 mins
43    21 casual        20.88157 mins
44    21 member        12.44297 mins
45    22 casual        20.36093 mins
46    22 member        12.38050 mins
47    23 casual        21.41145 mins
48    23 member        12.49728 mins
> 
> # morning rides
> library(dplyr)
> 
> cyclistic_data %>%
+   group_by(member_casual) %>%
+   filter(time_of_day == 'Morning') %>%
+   summarise(time = mean(ride_length))
# A tibble: 2 × 2
  member_casual time         
  <chr>         <drtn>       
1 casual        23.79824 mins
2 member        11.60768 mins
> cyclistic_data %>%
+ group_by(member_casual) %>%
+ filter(time_of_day == 'Morning') %>%
+ summarise(time = mean(ride_length)) %>%
+ print(n = 4)
# A tibble: 2 × 2
  member_casual time         
  <chr>         <drtn>       
1 casual        23.79824 mins
2 member        11.60768 mins
> 
> 
> cyclistic_data %>%
+ filter(time_of_day == 'Morning') %>%
+ summarise_at(vars(ride_length), list(time = mean)) 
# A tibble: 1 × 1
  time         
  <drtn>       
1 15.43841 mins
> 
> # afternoon rides
> cyclistic_data %>%
+ group_by(member_casual) %>%
+ filter(time_of_day == 'Afternoon') %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 2 × 2
  member_casual time         
  <chr>         <drtn>       
1 casual        25.73719 mins
2 member        12.81571 mins
> 
> cyclistic_data %>%
+ filter(time_of_day == 'Afternoon') %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 1 × 1
  time         
  <drtn>       
1 18.25021 mins
> 
> # evening rides
> cyclistic_data %>%
+ group_by(member_casual) %>%
+ filter(time_of_day == 'Evening') %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 2 × 2
  member_casual time         
  <chr>         <drtn>       
1 casual        21.85279 mins
2 member        12.84733 mins
> 
> cyclistic_data %>%
+ filter(time_of_day == 'Evening') %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 1 × 1
  time         
  <drtn>       
1 16.77696 mins
> 
> # Night rides
> cyclistic_data %>%
+ group_by(member_casual) %>%
+ filter(time_of_day == 'Night') %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 2 × 2
  member_casual time         
  <chr>         <drtn>       
1 casual        21.61386 mins
2 member        11.76564 mins
> 
> cyclistic_data %>%
+ filter(time_of_day == 'Night') %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 1 × 1
  time         
  <drtn>       
1 16.71022 mins
> 
> # all times of day
> cyclistic_data %>%
+ group_by(time_of_day, member_casual) %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 8 × 3
# Groups:   time_of_day [4]
  time_of_day member_casual time         
  <chr>       <chr>         <drtn>       
1 Afternoon   casual        25.73719 mins
2 Afternoon   member        12.81571 mins
3 Evening     casual        21.85279 mins
4 Evening     member        12.84733 mins
5 Morning     casual        23.79824 mins
6 Morning     member        11.60768 mins
7 Night       casual        21.61386 mins
8 Night       member        11.76564 mins
> 
> # average ride length
> cyclistic_data %>%
+ group_by(time_of_day) %>%
+ summarise(time = mean(ride_length))
# A tibble: 4 × 2
  time_of_day time         
  <chr>       <drtn>       
1 Afternoon   18.25021 mins
2 Evening     16.77696 mins
3 Morning     15.43841 mins
4 Night       16.71022 mins
> 
> cyclistic_data %>%
+ group_by(time_of_day) %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 4 × 2
  time_of_day time         
  <chr>       <drtn>       
1 Afternoon   18.25021 mins
2 Evening     16.77696 mins
3 Morning     15.43841 mins
4 Night       16.71022 mins
> 
> # day of week
> cyclistic_data %>% 
+ group_by(day_of_week, member_casual) %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 14 × 3
# Groups:   day_of_week [7]
   day_of_week member_casual time         
   <chr>       <chr>         <drtn>       
 1 Friday      casual        22.35811 mins
 2 Friday      member        12.22776 mins
 3 Monday      casual        24.83541 mins
 4 Monday      member        12.03342 mins
 5 Saturday    casual        26.76825 mins
 6 Saturday    member        13.98234 mins
 7 Sunday      casual        27.22911 mins
 8 Sunday      member        13.85147 mins
 9 Thursday    casual        21.40456 mins
10 Thursday    member        12.03258 mins
11 Tuesday     casual        21.44340 mins
12 Tuesday     member        11.79166 mins
13 Wednesday   casual        20.71879 mins
14 Wednesday   member        11.84745 mins
> 
> # average ride length
> cyclistic_data %>%
+ group_by(day_of_week) %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 7 × 2
  day_of_week time         
  <chr>       <drtn>       
1 Friday      16.36741 mins
2 Monday      16.63827 mins
3 Saturday    20.63855 mins
4 Sunday      20.58017 mins
5 Thursday    15.37000 mins
6 Tuesday     14.91099 mins
7 Wednesday   14.77751 mins
> 
> # day of month
> cyclistic_data %>%
+ group_by(day, member_casual) %>%
+ summarise_at(vars(ride_length), list(time = mean)) %>%
+ print(n = 62)
# A tibble: 62 × 3
# Groups:   day [31]
   day   member_casual time         
   <chr> <chr>         <drtn>       
 1 01    casual        22.45288 mins
 2 01    member        12.13226 mins
 3 02    casual        24.31105 mins
 4 02    member        12.43581 mins
 5 03    casual        25.04179 mins
 6 03    member        12.44855 mins
 7 04    casual        24.40341 mins
 8 04    member        12.40058 mins
 9 05    casual        26.04097 mins
10 05    member        12.65082 mins
11 06    casual        23.13452 mins
12 06    member        11.92372 mins
13 07    casual        21.55620 mins
14 07    member        12.18003 mins
15 08    casual        21.18400 mins
16 08    member        11.75364 mins
17 09    casual        25.20616 mins
18 09    member        13.05012 mins
19 10    casual        25.41925 mins
20 10    member        13.02587 mins
21 11    casual        23.90736 mins
22 11    member        12.37232 mins
23 12    casual        24.12315 mins
24 12    member        12.34273 mins
25 13    casual        24.38902 mins
26 13    member        12.38330 mins
27 14    casual        24.92903 mins
28 14    member        12.59093 mins
29 15    casual        22.52148 mins
30 15    member        12.05115 mins
31 16    casual        24.57396 mins
32 16    member        12.79009 mins
33 17    casual        23.35665 mins
34 17    member        12.36689 mins
35 18    casual        23.69188 mins
36 18    member        12.41944 mins
37 19    casual        24.00956 mins
38 19    member        12.42646 mins
39 20    casual        23.37221 mins
40 20    member        12.32704 mins
41 21    casual        23.93628 mins
42 21    member        12.67568 mins
43 22    casual        23.83028 mins
44 22    member        12.57097 mins
45 23    casual        26.29472 mins
46 23    member        13.16419 mins
47 24    casual        23.83061 mins
48 24    member        12.62531 mins
49 25    casual        22.12982 mins
50 25    member        12.15657 mins
51 26    casual        23.30614 mins
52 26    member        12.44922 mins
53 27    casual        22.36173 mins
54 27    member        11.97898 mins
55 28    casual        23.92508 mins
56 28    member        12.19900 mins
57 29    casual        24.40760 mins
58 29    member        12.47118 mins
59 30    casual        24.96212 mins
60 30    member        12.77384 mins
61 31    casual        22.68213 mins
62 31    member        12.57957 mins
> 
> # average ride length
> cyclistic_data %>%
+ group_by(day) %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 31 × 2
   day   time         
   <chr> <drtn>       
 1 01    15.98818 mins
 2 02    17.22674 mins
 3 03    17.88024 mins
 4 04    17.24923 mins
 5 05    18.36665 mins
 6 06    16.15409 mins
 7 07    15.54885 mins
 8 08    15.05122 mins
 9 09    18.13715 mins
10 10    18.28241 mins
# ℹ 21 more rows
# ℹ Use `print(n = ...)` to see more rows
> cyclistic_data %>%
+ group_by(day) %>%
+ summarise_at(vars(ride_length), list( time = mean)) %>%
+ print(n = 31)
# A tibble: 31 × 2
   day   time         
   <chr> <drtn>       
 1 01    15.98818 mins
 2 02    17.22674 mins
 3 03    17.88024 mins
 4 04    17.24923 mins
 5 05    18.36665 mins
 6 06    16.15409 mins
 7 07    15.54885 mins
 8 08    15.05122 mins
 9 09    18.13715 mins
10 10    18.28241 mins
11 11    16.89387 mins
12 12    16.86227 mins
13 13    17.15902 mins
14 14    17.53036 mins
15 15    16.05331 mins
16 16    17.73725 mins
17 17    16.94829 mins
18 18    17.01035 mins
19 19    17.05236 mins
20 20    16.75611 mins
21 21    17.05903 mins
22 22    17.09280 mins
23 23    18.95399 mins
24 24    17.38805 mins
25 25    15.95539 mins
26 26    16.76100 mins
27 27    15.86492 mins
28 28    16.85397 mins
29 29    17.52062 mins
30 30    18.24308 mins
31 31    16.49434 mins
> 
> # rides by season
> cyclistic_data %>%
+ group_by(member_casual) %>%
+ filter(season == 'Spring') %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 2 × 2
  member_casual time         
  <chr>         <drtn>       
1 casual        27.41043 mins
2 member        12.43592 mins
> 
> # average ride length
> cyclistic_data %>%
+ filter(season == 'Spring') %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 1 × 1
  time        
  <drtn>      
1 18.1667 mins
> 
> cyclistic_data %>%
+ group_by(member_casual) %>%
+ filter(season == 'Summer') %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 2 × 2
  member_casual time         
  <chr>         <drtn>       
1 casual        24.51078 mins
2 member        13.42974 mins
> 
> cyclistic_data %>%
+ filter(season == 'Summer') %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 1 × 1
  time         
  <drtn>       
1 18.61241 mins
> 
> cyclistic_data %>%
+ group_by(member_casual) %>%
+ filter(season == 'Fall') %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 2 × 2
  member_casual time         
  <chr>         <drtn>       
1 casual        20.59958 mins
2 member        11.86398 mins
> 
> # average ride length
> cyclistic_data %>%
+ filter(season == 'Fall') %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 1 × 1
  time         
  <drtn>       
1 15.09514 mins
> 
> cyclistic_data %>%
+ group_by(member_casual) %>%
+ filter(season == 'Spring') %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 2 × 2
  member_casual time         
  <chr>         <drtn>       
1 casual        27.41043 mins
2 member        12.43592 mins
> 
> # average ride length
> cyclistic_data %>%
+ filter(season == 'Spring') %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 1 × 1
  time        
  <drtn>      
1 18.1667 mins
> 
> # all seasons 
> cyclistic_data %>%
+ group_by(season, member_casual) %>%
+ summarise_at(vars(ride_length), list(time = mean))
# A tibble: 8 × 3
# Groups:   season [4]
  season member_casual time         
  <chr>  <chr>         <drtn>       
1 Fall   casual        20.59958 mins
2 Fall   member        11.86398 mins
3 Spring casual        27.41043 mins
4 Spring member        12.43592 mins
5 Summer casual        24.51078 mins
6 Summer member        13.42974 mins
7 Winter casual        20.05550 mins
8 Winter member        10.35396 mins
> 
> library(ggplot2)
> 
