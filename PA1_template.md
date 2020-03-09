


```r
library(data.table)
library(dplyr)
library(lattice)
```

##### 1. Reading in the dataset and/or processing the data


```r
act_dt <- fread("activity.csv")
act_dt$date <- as.Date(act_dt$date)
```

##### 2. Histogram of the total number of steps taken each day


```r
steps_day <- act_dt %>% group_by(date) %>% summarise(steps_per_day = sum(steps, na.rm = TRUE))
with(steps_day, hist(steps_per_day))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

##### 3. Mean and median of total number of steps each day


```r
mean_steps = round(mean(steps_day$steps_per_day), 0)
median_steps = median(steps_day$steps_per_day)
tbl <- as.table(matrix(c(mean_steps, median_steps), ncol = 2))
colnames(tbl) <- c("mean", "median")
rownames(tbl) <- c("")
tbl
```

```
##   mean median
##   9354  10395
```

  
##### 4. Time series plot of the average number of steps taken


```r
act_dt_avg <- act_dt %>% group_by(interval) %>% summarise(avg_steps = mean(steps, na.rm = TRUE))
with(act_dt_avg, plot(interval, avg_steps, type = "l"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

##### 5. The 5-minute interval that, on average, contains the maximum number of steps


```r
max_step_int <- act_dt_avg[which.max(act_dt_avg$avg_steps),]
max_step_int
```

```
## # A tibble: 1 x 2
##   interval avg_steps
##      <int>     <dbl>
## 1      835      206.
```


The average of steps gets maximum at interval 835 (around 2PM of the day).

##### 6. Code to describe and show a strategy for imputing missing data


```r
# count number of rows with missing data (NA) in any column
NAs <- sum(apply(act_dt,MARGIN = 1, function(x){any(is.na(x))})) #MARGIN = 1 for row
NAs
```

```
## [1] 2304
```

Let fill in the missing values(NAs) of the dataset using the mean for that 5-minute interval. In question 4 the dataset act_dt_avg contains the mean of each interval except that the values are not yet rounded. In reality, the step numbers should be intergers. However, for the sake of that the values are reused in the coming calculations for mean and median, let not round them at this step.


```r
act_dt_fillNA <- left_join(act_dt, act_dt_avg, by = "interval")
act_dt_fillNA$steps <- ifelse(is.na(act_dt_fillNA$steps), act_dt_fillNA$avg_steps, act_dt_fillNA$steps)
head(act_dt_fillNA)
```

```
##       steps       date interval avg_steps
## 1 1.7169811 2012-10-01        0 1.7169811
## 2 0.3396226 2012-10-01        5 0.3396226
## 3 0.1320755 2012-10-01       10 0.1320755
## 4 0.1509434 2012-10-01       15 0.1509434
## 5 0.0754717 2012-10-01       20 0.0754717
## 6 2.0943396 2012-10-01       25 2.0943396
```

##### 7. Histogram of the total number of steps taken each day after missing values are imputed.


```r
act_day_steps <- act_dt_fillNA %>% group_by(date) %>% summarise(steps_per_day = sum(steps))
with(act_day_steps, hist(steps_per_day))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

Calculate mean and median total number of steps taken per day.


```r
mean_day_steps <- round(mean(act_day_steps$steps_per_day))
median_day_steps = round(median(act_day_steps$steps_per_day))
tbl2 <- as.table(matrix(c(mean_day_steps, median_day_steps), ncol = 2))
colnames(tbl2) <- c("mean", "median")
rownames(tbl2) <- c("")
tbl2
```

```
##   mean median
##  10766  10766
```

After filling in the missing values, the mean and median of total steps each day increase. Mean changes from 9354 to 10766 and median changes from 10395 to 10766.

##### 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends.


```r
#add variable wd of weekday or weekend
act_dt_fillNA <- act_dt_fillNA %>% mutate(wd = ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekend", "weekday"))
act_dt_avg2 <- act_dt_fillNA %>% group_by(interval, wd) %>% summarise(steps_avg = mean(steps))
xyplot(steps_avg~interval | as.factor(wd), data = act_dt_avg2, layout = c(1,2), type = "l", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)



