##Reproducible Research
##======================
    
# Project 1
    
## Loading and preprocessing the data
### Load raw data from CSV file.

##```{r, load_data}
#cwd <- "C:\\Users\\martin-trejo\\Dropbox\\Coursera_Specializations\\Data_Science_Specialization\\05_Reproducible Research\\project_1\\"
cwd<- "/home/martin-trejo/Dropbox/Coursera_Specializations/Data_Science_Specialization/05_Reproducible_Research/project_1/data/"
filename <- "activity.csv"
raw_data <- read.csv(paste0(cwd, filename), stringsAsFactors = TRUE)
##```

### Clean data, remove NA values from dataset.
##```{r, Clean_data}
clean_data <- raw_data[complete.cases(raw_data),]
clean_data <- clean_data[c(2, 3, 1)]
num_clean_records <- nrow(clean_data); num_clean_records
##```

### Processing the data

##```{r, Process_data}
total_steps_per_day <- tapply(clean_data$steps, clean_data$date, FUN = sum, 
                              na.rm = TRUE, default = 0, simplify = TRUE)
total_steps_per_day_df <- as.data.frame(total_steps_per_day)
total_steps_per_day_df <- cbind(rownames(total_steps_per_day_df), total_steps_per_day_df )
colnames(total_steps_per_day_df)<- c("date","num_steps")

hist(total_steps_per_day_df$num_steps, main = "HISTOGRAM OF TOTAL NUMBER OF STEPS PER DAY", 
     xlab= "Date", ylab= "Steps taken")

steps_per_day_summary1 <-c(mean(total_steps_per_day_df$num_steps), 
                           median(total_steps_per_day_df$num_steps))
names(steps_per_day_summary1) <- c("mean","median")
steps_per_day_summary1

##```


## What is the average daily activity pattern?
## Make a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type="l") 
## of the 5-minute interval (x-axis) and the average number of steps taken, averaged 
## across all days (y-axis).

tmp_data <- data.frame(clean_data)
tmp_data$interval <- rep(seq(from = 0, to = 55, by= 5), nrow(clean_data)/12)

steps_per_interval <- tapply(tmp_data$steps, tmp_data$interval, FUN = mean, 
                              na.rm = TRUE, default = 0, simplify = TRUE)

time_series_df <- as.data.frame(steps_per_interval)
time_series_df <- cbind(seq(from = 0, to = 55, by= 5), time_series_df )

colnames(time_series_df)<- c("interval","num_steps")

plot(time_series_df, type = "l", xlab= "5 Minutes interval", ylab= "average steps taken")


## Which 5-minute interval, on average across all the days in the dataset, 
## contains the maximum number of steps?

max_steps <- time_series_df$interval[time_series_df$num_steps == max(time_series_df$num_steps)]


## Imputing missing values

## Calculate and report the total number of missing values in the dataset 
## (i.e. the total number of rows with NA\color{red}{\verb|NA|}NAs).

missing_values <- sum(!complete.cases(raw_data))
missing_values

## Devise a strategy for filling in all of the missing values in the dataset. 
## The strategy does not need to be sophisticated. For example, you could use 
## the mean/median for that day, or the mean for that 5-minute interval, etc.

## Sample the clean dataset to fill the missing values with randomly selected ones.
replacement_samples <- sample(x =clean_data$steps, 
size = missing_values,
replace = TRUE)

## Create a new dataset that is equal to the original dataset but with the 4
## missing data filled in. 

complete_data <- data.frame(raw_data)
complete_data$steps[!complete.cases(raw_data)] <- replacement_samples

## Make a histogram of the total number of steps taken each day and Calculate and 
## report the mean and median total number of steps taken per day. 
## Do these values differ from the estimates from the first part of the assignment? 
##  yes the mean and median results differ.
## What is the impact of imputing missing data on the estimates of the total daily
## number of steps?
##  The resulting histogram of the values better resembles a normal distribution.
##  the mean and the median values become closer.


total_steps_per_day <- tapply(complete_data$steps, complete_data$date, FUN = sum, 
                              na.rm = TRUE, default = 0, simplify = TRUE)
total_steps_per_day_df <- as.data.frame(total_steps_per_day)
total_steps_per_day_df <- cbind(rownames(total_steps_per_day_df), total_steps_per_day_df )
colnames(total_steps_per_day_df)<- c("date","num_steps")

hist(total_steps_per_day_df$num_steps, main = "HISTOGRAM OF TOTAL NUMBER OF STEPS PER DAY", 
     xlab= "Date", ylab= "Steps taken")

steps_per_day_summary1 <-c(mean(total_steps_per_day_df$num_steps), 
                           median(total_steps_per_day_df$num_steps))
names(steps_per_day_summary1) <- c("mean","median")
steps_per_day_summary1


## Are there differences in activity patterns between weekdays and weekends?

## Create a new factor variable in the dataset with two levels – “weekday” 
## and “weekend” indicating whether a given date is a weekday or weekend day.

complete_data$date <- as.Date(complete_data$date ) 
complete_data_day_type <- cbind(complete_data, day_type = sapply(X = complete_data$date, 
                                                        FUN = function(x){
                                                            day = weekdays(x, abbreviate=TRUE)
                                                            day
                                                            if ((day== "Sat") ||
                                                                (day == "Sun"))
                                                                day = "Weekend"
                                                            else
                                                                day = "Weekday"
                                                        }, 
                                                        simplify = TRUE))

## Make a panel plot containing a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

## tapply(X, INDEX, FUN = NULL, ..., default = NA, simplify = TRUE)
library(plyr)
library(lattice)

steps_summary <- ddply(complete_data_day_type, .(day_type,interval), summarize, 
                       mean_steps = mean(steps))

xyplot(mean_steps~interval|day_type, data = steps_summary, type = "l", 
       layout= c(1,2), xlab ="Interval", ylab = "Number of steps")


