---
title: "Reproducible Research: Peer Assessment 1"
author: "Ray Bem"
date: "8/24/2020"
output: 
  html_document: 
    keep_md: yes
---



Need dplyr/ggplot2 for manipulation


```r
require(dplyr)
```

```
## Loading required package: dplyr
```

```
## 
## Attaching package: 'dplyr'
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
require(ggplot2)
```

```
## Loading required package: ggplot2
```

## Loading and preprocessing the data
## unzip file if not there...

```r
ifelse(file.exists("activity.csv")==TRUE, 
       "activity.csv available",
       unzip("activity.zip", junkpaths=TRUE, exdir = "."))
```

```
## [1] "activity.csv available"
```

```r
raw_data_in <- read.csv("activity.csv")
head(raw_data_in)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(raw_data_in$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```

```r
summary(raw_data_in$interval)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0   588.8  1177.5  1177.5  1766.2  2355.0
```

```r
nrow(raw_data_in)
```

```
## [1] 17568
```

```r
count(raw_data_in,interval)
```

```
##     interval  n
## 1          0 61
## 2          5 61
## 3         10 61
## 4         15 61
## 5         20 61
## 6         25 61
## 7         30 61
## 8         35 61
## 9         40 61
## 10        45 61
## 11        50 61
## 12        55 61
## 13       100 61
## 14       105 61
## 15       110 61
## 16       115 61
## 17       120 61
## 18       125 61
## 19       130 61
## 20       135 61
## 21       140 61
## 22       145 61
## 23       150 61
## 24       155 61
## 25       200 61
## 26       205 61
## 27       210 61
## 28       215 61
## 29       220 61
## 30       225 61
## 31       230 61
## 32       235 61
## 33       240 61
## 34       245 61
## 35       250 61
## 36       255 61
## 37       300 61
## 38       305 61
## 39       310 61
## 40       315 61
## 41       320 61
## 42       325 61
## 43       330 61
## 44       335 61
## 45       340 61
## 46       345 61
## 47       350 61
## 48       355 61
## 49       400 61
## 50       405 61
## 51       410 61
## 52       415 61
## 53       420 61
## 54       425 61
## 55       430 61
## 56       435 61
## 57       440 61
## 58       445 61
## 59       450 61
## 60       455 61
## 61       500 61
## 62       505 61
## 63       510 61
## 64       515 61
## 65       520 61
## 66       525 61
## 67       530 61
## 68       535 61
## 69       540 61
## 70       545 61
## 71       550 61
## 72       555 61
## 73       600 61
## 74       605 61
## 75       610 61
## 76       615 61
## 77       620 61
## 78       625 61
## 79       630 61
## 80       635 61
## 81       640 61
## 82       645 61
## 83       650 61
## 84       655 61
## 85       700 61
## 86       705 61
## 87       710 61
## 88       715 61
## 89       720 61
## 90       725 61
## 91       730 61
## 92       735 61
## 93       740 61
## 94       745 61
## 95       750 61
## 96       755 61
## 97       800 61
## 98       805 61
## 99       810 61
## 100      815 61
## 101      820 61
## 102      825 61
## 103      830 61
## 104      835 61
## 105      840 61
## 106      845 61
## 107      850 61
## 108      855 61
## 109      900 61
## 110      905 61
## 111      910 61
## 112      915 61
## 113      920 61
## 114      925 61
## 115      930 61
## 116      935 61
## 117      940 61
## 118      945 61
## 119      950 61
## 120      955 61
## 121     1000 61
## 122     1005 61
## 123     1010 61
## 124     1015 61
## 125     1020 61
## 126     1025 61
## 127     1030 61
## 128     1035 61
## 129     1040 61
## 130     1045 61
## 131     1050 61
## 132     1055 61
## 133     1100 61
## 134     1105 61
## 135     1110 61
## 136     1115 61
## 137     1120 61
## 138     1125 61
## 139     1130 61
## 140     1135 61
## 141     1140 61
## 142     1145 61
## 143     1150 61
## 144     1155 61
## 145     1200 61
## 146     1205 61
## 147     1210 61
## 148     1215 61
## 149     1220 61
## 150     1225 61
## 151     1230 61
## 152     1235 61
## 153     1240 61
## 154     1245 61
## 155     1250 61
## 156     1255 61
## 157     1300 61
## 158     1305 61
## 159     1310 61
## 160     1315 61
## 161     1320 61
## 162     1325 61
## 163     1330 61
## 164     1335 61
## 165     1340 61
## 166     1345 61
## 167     1350 61
## 168     1355 61
## 169     1400 61
## 170     1405 61
## 171     1410 61
## 172     1415 61
## 173     1420 61
## 174     1425 61
## 175     1430 61
## 176     1435 61
## 177     1440 61
## 178     1445 61
## 179     1450 61
## 180     1455 61
## 181     1500 61
## 182     1505 61
## 183     1510 61
## 184     1515 61
## 185     1520 61
## 186     1525 61
## 187     1530 61
## 188     1535 61
## 189     1540 61
## 190     1545 61
## 191     1550 61
## 192     1555 61
## 193     1600 61
## 194     1605 61
## 195     1610 61
## 196     1615 61
## 197     1620 61
## 198     1625 61
## 199     1630 61
## 200     1635 61
## 201     1640 61
## 202     1645 61
## 203     1650 61
## 204     1655 61
## 205     1700 61
## 206     1705 61
## 207     1710 61
## 208     1715 61
## 209     1720 61
## 210     1725 61
## 211     1730 61
## 212     1735 61
## 213     1740 61
## 214     1745 61
## 215     1750 61
## 216     1755 61
## 217     1800 61
## 218     1805 61
## 219     1810 61
## 220     1815 61
## 221     1820 61
## 222     1825 61
## 223     1830 61
## 224     1835 61
## 225     1840 61
## 226     1845 61
## 227     1850 61
## 228     1855 61
## 229     1900 61
## 230     1905 61
## 231     1910 61
## 232     1915 61
## 233     1920 61
## 234     1925 61
## 235     1930 61
## 236     1935 61
## 237     1940 61
## 238     1945 61
## 239     1950 61
## 240     1955 61
## 241     2000 61
## 242     2005 61
## 243     2010 61
## 244     2015 61
## 245     2020 61
## 246     2025 61
## 247     2030 61
## 248     2035 61
## 249     2040 61
## 250     2045 61
## 251     2050 61
## 252     2055 61
## 253     2100 61
## 254     2105 61
## 255     2110 61
## 256     2115 61
## 257     2120 61
## 258     2125 61
## 259     2130 61
## 260     2135 61
## 261     2140 61
## 262     2145 61
## 263     2150 61
## 264     2155 61
## 265     2200 61
## 266     2205 61
## 267     2210 61
## 268     2215 61
## 269     2220 61
## 270     2225 61
## 271     2230 61
## 272     2235 61
## 273     2240 61
## 274     2245 61
## 275     2250 61
## 276     2255 61
## 277     2300 61
## 278     2305 61
## 279     2310 61
## 280     2315 61
## 281     2320 61
## 282     2325 61
## 283     2330 61
## 284     2335 61
## 285     2340 61
## 286     2345 61
## 287     2350 61
## 288     2355 61
```

```r
count(raw_data_in,date)
```

```
##          date   n
## 1  2012-10-01 288
## 2  2012-10-02 288
## 3  2012-10-03 288
## 4  2012-10-04 288
## 5  2012-10-05 288
## 6  2012-10-06 288
## 7  2012-10-07 288
## 8  2012-10-08 288
## 9  2012-10-09 288
## 10 2012-10-10 288
## 11 2012-10-11 288
## 12 2012-10-12 288
## 13 2012-10-13 288
## 14 2012-10-14 288
## 15 2012-10-15 288
## 16 2012-10-16 288
## 17 2012-10-17 288
## 18 2012-10-18 288
## 19 2012-10-19 288
## 20 2012-10-20 288
## 21 2012-10-21 288
## 22 2012-10-22 288
## 23 2012-10-23 288
## 24 2012-10-24 288
## 25 2012-10-25 288
## 26 2012-10-26 288
## 27 2012-10-27 288
## 28 2012-10-28 288
## 29 2012-10-29 288
## 30 2012-10-30 288
## 31 2012-10-31 288
## 32 2012-11-01 288
## 33 2012-11-02 288
## 34 2012-11-03 288
## 35 2012-11-04 288
## 36 2012-11-05 288
## 37 2012-11-06 288
## 38 2012-11-07 288
## 39 2012-11-08 288
## 40 2012-11-09 288
## 41 2012-11-10 288
## 42 2012-11-11 288
## 43 2012-11-12 288
## 44 2012-11-13 288
## 45 2012-11-14 288
## 46 2012-11-15 288
## 47 2012-11-16 288
## 48 2012-11-17 288
## 49 2012-11-18 288
## 50 2012-11-19 288
## 51 2012-11-20 288
## 52 2012-11-21 288
## 53 2012-11-22 288
## 54 2012-11-23 288
## 55 2012-11-24 288
## 56 2012-11-25 288
## 57 2012-11-26 288
## 58 2012-11-27 288
## 59 2012-11-28 288
## 60 2012-11-29 288
## 61 2012-11-30 288
```


## What is mean total number of steps taken per day?

```r
activity_data <-raw_data_in %>%
	mutate(newdate=as.Date(date)) %>% 
	group_by(newdate) %>%
	mutate(total_steps_per_day=sum(steps, na.rm = TRUE))

activity_data %>% distinct(newdate, total_steps_per_day) %>%
	ungroup %>%
	summarise(
	mean_steps_per_day=mean(total_steps_per_day, na.rm = TRUE),
	median_steps_per_day=median(total_steps_per_day, na.rm = TRUE))
```

```
## # A tibble: 1 x 2
##   mean_steps_per_day median_steps_per_day
##                <dbl>                <int>
## 1              9354.                10395
```

```r
ggplot(data = activity_data) +
	geom_col(mapping = aes(x=newdate, y=steps))
```

```
## Warning: Removed 2304 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## What is the average daily activity pattern?

```r
interval_activity <- activity_data %>% 
	ungroup %>% 
	group_by(interval) %>% 
	summarise(mean_interval_steps_per_day=mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(interval_activity) +
	geom_line(mapping = aes(x=interval, y=mean_interval_steps_per_day))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
max<-interval_activity %>% ungroup %>% mutate(max=max(mean_interval_steps_per_day)) %>% distinct(max)
str(max)
```

```
## tibble [1 × 1] (S3: tbl_df/tbl/data.frame)
##  $ max: num 206
```

```r
max
```

```
## # A tibble: 1 x 1
##     max
##   <dbl>
## 1  206.
```

```r
interval_activity %>% filter(mean_interval_steps_per_day==max(interval_activity$mean_interval_steps_per_day))
```

```
## # A tibble: 1 x 2
##   interval mean_interval_steps_per_day
##      <int>                       <dbl>
## 1      835                        206.
```

## Imputing missing values


```r
activity_data2 <- activity_data %>% ungroup %>%
	left_join(interval_activity, by = "interval") %>% 
#	mutate(newtotalsteps=ifelse(is.na(steps)==TRUE, mean_interval_steps_per_day, total_steps_per_day))
	mutate(newtotalsteps=ifelse(is.na(steps)==TRUE, mean_interval_steps_per_day, steps),
	       imputed_flag=ifelse(is.na(steps)==TRUE, "imputed (mean for interval)", "untouched")) %>% ungroup

## looking at the NA's for steps, along with the zero values...
tx<-activity_data%>%
	ungroup%>% 
	mutate(na_steps=is.na(steps), zero_steps=(steps==0)) %>% group_by(newdate)
#	count(is.na(steps),steps==0)
tx%>%filter(na_steps==TRUE)%>%distinct(newdate)
```

```
## # A tibble: 8 x 1
## # Groups:   newdate [8]
##   newdate   
##   <date>    
## 1 2012-10-01
## 2 2012-10-08
## 3 2012-11-01
## 4 2012-11-04
## 5 2012-11-09
## 6 2012-11-10
## 7 2012-11-14
## 8 2012-11-30
```

```r
ggplot(data = activity_data2) +
#	geom_col(mapping = aes(x=newdate, y=newtotalsteps, fill=imputed_flag)) +
#	geom_col(data=activity_data, mapping = aes(x=newdate, y=steps)) +
	geom_col(mapping = aes(x=newdate, y=newtotalsteps, fill=imputed_flag)) 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
# +
# 	geom_col(data=activity_data, mapping = aes(x=newdate, y=steps), alpha=.5, color="black")

ggplot(data = activity_data2) +
#	geom_col(mapping = aes(x=newdate, y=newtotalsteps, fill=imputed_flag)) +
#	geom_col(data=activity_data, mapping = aes(x=newdate, y=steps)) +
#	geom_jitter(mapping = aes(x=newdate, y=newtotalsteps, fill=imputed_flag, color=imputed_flag), alpha=.5) +
	geom_point(data=activity_data, mapping = aes(x=newdate, y=steps), alpha=.7, color="black", size=3) +
	geom_point(mapping = aes(x=newdate, y=newtotalsteps, fill=imputed_flag, color=imputed_flag), alpha=.5, size=1.5)
```

```
## Warning: Removed 2304 rows containing missing values (geom_point).
```

![](PA1_template_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```r
activity_data2 %>% ungroup %>% distinct(newdate, newtotalsteps) %>%
	#ungroup %>%
	summarise(
	mean_steps_per_day=mean(newtotalsteps, na.rm = TRUE),
	median_steps_per_day=median(newtotalsteps, na.rm = TRUE))
```

```
## # A tibble: 1 x 2
##   mean_steps_per_day median_steps_per_day
##                <dbl>                <dbl>
## 1               112.                 50.0
```

# This concludes the work (finish later)...
