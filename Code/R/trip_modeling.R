# loading require package 
library(tidyverse)
library(data.table)
library(chron)
library(GGally)
#Loading Trip data
apr_18<-fread("Apr18.csv")  # use path under " "
may_18<-fread("May18.csv")
jun_18<-fread("Jun18.csv")

#Loading weather data
apr_w18<-fread("w_apr18.csv") %>%
         mutate(pic_date=as.Date(Date, "%d-%m-%Y"))%>%
         select(pic_date,avg_temp)

may_w18<-fread("w_may18.csv") %>%
  mutate(pic_date=as.Date(Date, "%d-%m-%Y"))%>%
  select(pic_date,avg_temp)

jun_w18<-fread("w_jun18.csv")%>%
  mutate(pic_date=as.Date(Date, "%d-%m-%Y"))%>%
  select(pic_date,avg_temp)
wet_data<-rbind(apr_w18,may_w18,jun_w18)

#Loading Income data
income<-fread("income_data.csv")%>%
  mutate(PULocationID=LocationID)%>%
  select(PULocationID,Borough, 'Median H/income')


# Combine Trip weather and income data from April-18 to June-18
trip_data<-apr_18 #rbind(apr_18[sample(1:dim(apr_18)[1],5000)],may_18[sample(1:dim(may_18)[1],5000)],jun_18[sample(1:dim(jun_18)[1],5000)])

trip_data2<-trip_data[,-3] %>% 
  mutate(tpep_pickup_datetime=as.POSIXct(tpep_pickup_datetime,tz = "America/New_York"),
         pic_hr=as.numeric(format(tpep_pickup_datetime, "%H")),
         pic_month=month.abb[as.numeric(format(tpep_pickup_datetime, "%m"))],
         pic_date=as.Date(tpep_pickup_datetime))%>%
         left_join(wet_data, by = "pic_date")%>%
        left_join(income, by = "PULocationID")%>%
         select(-c(V1,tpep_pickup_datetime, DOLocationID))
head(trip_data2,10)
         
# Boxplot of tip percentage by month
bp <- ggplot(trip_data2, aes(x=pic_month, y=tip_perc, fill=pic_month)) + 
  geom_boxplot()+labs(x="Month (2018)", y = "Tip Percentage")
bp 

# Pic hour vs Mean tip percentage
pichr_tip<-trip_data2 %>%
  group_by(pic_hr) %>%
  summarize(mean_tip=mean(tip_perc))

ggplot(pichr_tip, aes(x = pic_hr, y = mean_tip)) +
  geom_point(alpha = 0.5)+
  geom_line(col="deepskyblue",size=0.6)+
  labs(x="Pickup's hours of day", y="Tip Percentage")

# Pic hour vs Mean speed
pichr_speed<-trip_data2 %>%
  group_by(pic_hr) %>%
  summarize(mean_speed=mean(speed))

ggplot(pichr_speed, aes(x = pic_hr, y = mean_speed)) +
  geom_point(alpha = 0.5)+
  geom_line(col="deepskyblue",size=0.6)+
  labs(x="Pickup's hours of day", y="speed(Miles/hr)")

# Tip percentage vs Mean speed
tip_speed<-trip_data2 %>%
  filter(speed<50) %>%
  mutate(speed=round(speed,0)) %>%
  group_by(speed) %>%
  select(tip_perc, speed) %>%
  summarize(mean_tips=mean(tip_perc))   
  
ggplot(tip_speed, aes(x = speed, y = mean_tips)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE)+
  labs(x="Speed (miles/hr)", y="Tip (%)")

# Tip percentage vs No. of trip
tip_trip<-trip_data2 %>%
  filter(tip_perc<40)%>%
  mutate(tip_perc=round(tip_perc,0)) %>%
  group_by(tip_perc) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count) )  
ggplot(tip_trip, aes(x = tip_perc, y = prop)) +
  geom_col() +
  labs(x="Tip (%)", y="% of trips")

# Holyday plot
holiday<-as.Date("2018-05-27")
trip_data3<-trip_data2 %>%
           mutate(holydays = ifelse(pic_date == holiday,"Holiday","Weekday"))
         
holi_tip<-trip_data3 %>%
  select(holydays,tip_perc)%>%
  group_by(holydays)%>%
  summarise(mean_tips=mean(tip_perc))
 
ggplot(holi_tip, aes(x=holydays, y=mean_tips, fill=holydays)) +
  geom_bar(stat="identity")+
  labs(x="Days", y="Tip (%)")

#  weekend plot

week_tip<-trip_data2 %>%
  mutate(weekends = ifelse(as.numeric(is.weekend(pic_date))==1,"Weekend","Weekday"))%>%
  select(weekends,tip_perc)%>%
  group_by(weekends)%>%
  summarise(mean_tips=mean(tip_perc))

ggplot(week_tip, aes(x=weekends, y=mean_tips, fill=weekends)) +
  geom_bar(stat="identity")+
  labs(x="Days", y="Tip (%)")

 
# weather plot
may_w18<-may_w18 %>%
      mutate(pic_date=as.Date(Date))%>%
      select(pic_date,avg_temp)

wet_tip<-trip_data2%>%
         mutate(avg_temp=round(avg_temp,0))%>%
         group_by(avg_temp) %>%
         select(tip_perc, avg_temp) %>%
         summarize(mean_tips=mean(tip_perc))   

ggplot(wet_tip, aes(x = avg_temp, y = mean_tips)) +
  geom_col()+
  labs(x="Average tempareture", y="Tip (%)")

# Income effect
income_tip<-trip_data2%>%
  group_by(Borough) %>%
  select(tip_perc, Borough) %>%
  summarize(mean_tips=mean(tip_perc))   

ggplot(income_tip, aes(x = Borough, y = mean_tips)) +
  geom_col()+
  labs(x="Borough", y="Tip (%)")


# modeling 
dd<-trip_data2%>%
  filter(fare_amount != 52) %>%
  mutate(tip_amount=total_amount/(100/tip_perc+1),
         passenger_count=as.factor(passenger_count),
         income=as.factor(`Median H/income`))%>%
select(-c(total_amount,tip_perc,`Median H/income`,PULocationID,pic_hr,pic_month,pic_date, Borough))

# Pairs data plot for subsety of data 
dd[sample(1:dim(dd)[1],5000),]%>%
  ggpairs()
  
mod<-lm(tip_amount~ .,data=dd)
summary(mod)
