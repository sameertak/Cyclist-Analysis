install.packages('lubridate')
install.packages('tidyquant')
install.packages('timetk')

library(tidyverse)
df = read_csv('last_year_trip.csv')
df = head(df1)
head(df)
colnames(df)
str(df)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyquant)
library(timetk)
library(scales)

df
write.csv(x=df, file="data_name.csv")

df <- df1[!duplicated(df1$ride_id), ]
print(paste("Removed", nrow(df1) - nrow(df), "duplicated rows"))

df$started_at <- as.POSIXct(df$started_at, format = "%Y-%m-%d %H:%M:%S")
df$ended_at <- as.POSIXct(df$ended_at, format = "%Y-%m-%d %H:%M:%S")

df$started_at
df <- df %>%
  mutate(year_month = paste(strftime(df$started_at, "%Y"),
                            "-",
                            strftime(df$started_at, "%m"),
                            paste("(",strftime(df$started_at, "%b"), ")", sep="")))
unique(df$year_month)
df<-df[!(df$year_month=="2022 - 08 (Aug)"),]

df[,c("Unnamed: 0", "Unnamed: 0.1", "Unnamed: 0.1.1", "File_Name", "year")] <- list(NULL)

df %>%
  filter(member_casual == "member") %>%
  group_by(member_casual)

ggplot(data = df) +
  geom_bar(mapping = aes(x = member_casual, fill = member_casual)) +
  geom_text(stat='count', aes(label= ..count.., x = member_casual, vjust=-0.5)) +
  ggtitle('Total Casual and Member') +
  xlab("Subscriptions")

ggplot(data = df) +
  geom_bar(mapping = aes(x = rideable_type, fill = rideable_type)) +
  ggtitle('Rideables VS Total') +
  geom_text(stat='count', aes(label= ..count.., x=rideable_type, vjust=-0.5)) +
  xlab("Bikes")

colnames(df)


ggplot(data = df) +
  geom_bar(mapping = aes(x = year_month, fill=member_casual)) +
  geom_text(stat='count', aes(label= ..count.., x=year_month, vjust=-0.5)) +
  ggtitle('Customers VS Months') +
  xlab("Months") + 
  ylab("Costumers")

df$time_str = str_sub(df$total_time,-8)
df$time = as.POSIXct(df$time_str, format = "%H:%M:%S")

df$time

a <- df$time %>% 
  between_time(as.POSIXct("00:00:00", format = "%H:%M:%S"), as.POSIXct("00:10:00", format = "%H:%M:%S")) %>% sum()
b <- df$time %>% 
  between_time(as.POSIXct("00:10:00", format = "%H:%M:%S"), as.POSIXct("00:20:00", format = "%H:%M:%S")) %>% sum()
c <- df$time %>% 
  between_time(as.POSIXct("00:20:00", format = "%H:%M:%S"), as.POSIXct("00:30:00", format = "%H:%M:%S")) %>% sum()
d <- df$time %>% 
  between_time(as.POSIXct("00:30:00", format = "%H:%M:%S"), as.POSIXct("00:40:00", format = "%H:%M:%S")) %>% sum()
e <- df$time %>% 
  between_time(as.POSIXct("00:40:00", format = "%H:%M:%S"), as.POSIXct("00:50:00", format = "%H:%M:%S")) %>% sum()
f <- df$time %>% 
  between_time(as.POSIXct("00:50:00", format = "%H:%M:%S"), as.POSIXct("01:00:00", format = "%H:%M:%S")) %>% sum()
g <- df$time %>% 
  between_time(as.POSIXct("01:00:00", format = "%H:%M:%S"), as.POSIXct("01:10:00", format = "%H:%M:%S")) %>% sum()
h <- df$time %>% 
  between_time(as.POSIXct("01:10:00", format = "%H:%M:%S"), as.POSIXct("01:20:00", format = "%H:%M:%S")) %>% sum()
i <- df$time %>% 
  between_time(as.POSIXct("01:20:00", format = "%H:%M:%S"), as.POSIXct("01:30:00", format = "%H:%M:%S")) %>% sum()

df_time <- data.frame(
  time_range <- c("0", "10", "20", "30", "40", "50", "60", "70", "80"),
  frequency<-c(a,b,c,d,e,f,g,h,i)
)

ggplot(data = df_time) +
  geom_smooth(mapping = aes(x = time_range, y=frequency, group=1)) +
  scale_y_continuous(labels = comma) +
  xlab("Minutes") + 
  ylab("Costumers") +
  ggtitle('Duration VS Frequency')

df$weekday <- weekdays(df$started_at)                
df$weekday

ggplot(data = df) +
  geom_bar(mapping = aes(x = weekday, fill=member_casual)) +
  geom_text(stat='count', aes(label= ..count.., x=weekday, vjust=-0.5)) +
  ggtitle('Customers VS Weekdays') +
  xlab("Weekday") + 
  ylab("Costumers")

format(mean(strptime(df$time, "%H:%M:%S")), "%H:%M:%S")
format(max(strptime(df$time, "%H:%M:%S")), "%H:%M:%S")

df$time

ggplot(data = df) +
  geom_col(mapping = aes(x = weekday, y=time, fill=member_casual)) +
  ggtitle('Customers VS Weekdays') +
  xlab("Weekday") + 
  ylab("Costumers")



y <- sort(table(df1$start_station_name), decreasing = FALSE)[1:5]
sd <- data.frame(y)
ggplot(data = sd) +
  geom_col(mapping = aes(x = Var1, y=Freq)) +
  ggtitle('Least Frequent Locations') +
  xlab("Locations") + 
  ylab("Frequencies")

df1 <- transform(df1, freq=ave(seq(nrow(df1)), start_station_name, FUN=length))
df <- na.omit(df1)
df<-df[order(df$freq, decreasing = TRUE), ]
head(new_df)
new_df <- df[!duplicated(df$start_station_name), ]
new_df <- head(new_df)

ggplot(data = new_df) +
  geom_col(mapping = aes(x = start_station_name, y=freq, fill=start_station_name)) +
  ggtitle('Most Frequent Locations') +
  xlab("Locations") + 
  ylab("Frequencies")
new_df$freq
df1

df = read_csv('data_name.csv')
df <- transform(df, freq=ave(seq(nrow(df)), start_station_name, FUN=length))
df1 <- df[!(is.na(df$start_station_name)),]
df1<-df1[order(df1$freq, decreasing = TRUE), ]
df1
new_df <- df1[!duplicated(df1$start_station_name), ]
new_df <- head(new_df)

  ggplot(data = new_df) +
    geom_col(mapping = aes(x = start_station_name, y=freq, fill=start_station_name)) +
    ggtitle('Most Frequent Locations') +
    xlab("Locations") + 
    ylab("Frequencies")
  
df
df <- df[!(is.na(df$year_month)), ]
