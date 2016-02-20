#Homework4
print("Jingchi YAN")
print(1503639)
#1
flights <- read.csv("/var/folders/7j/1dt1n8bs051gfqx5k_y6g66m0000gn/T//Rtmp2JC8jq/data13f3656b733", stringsAsFactors=FALSE)
planes <- read.csv("/var/folders/7j/1dt1n8bs051gfqx5k_y6g66m0000gn/T//Rtmp2JC8jq/data13f77159123", stringsAsFactors=FALSE)
weather <- read.csv("/var/folders/7j/1dt1n8bs051gfqx5k_y6g66m0000gn/T//Rtmp2JC8jq/data13f28ebcde9", stringsAsFactors=FALSE)
airports <- read.csv("/var/folders/7j/1dt1n8bs051gfqx5k_y6g66m0000gn/T//Rtmp2JC8jq/data13f111b90fc", stringsAsFactors=FALSE)
#2
flights$date<-as.Date(flights$date)
weather$date <- as.Date(weather$date,"%Y-%m-%d")
#3
flights.2a<-subset(flights, dest=="SFO"|dest=="OAK")
nrow(flights.2a)
flights.2b<-subset(flights, dep_delay>=60)
nrow(flights.2b)
flights.2c<-subset(flights, arr_delay>=2*dep_delay)
nrow(flights.2c)
#4
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
select(flights, dep_delay, arr_delay)
select(flights, ends_with("delay"))
select(flights, contains("delay"))
#5
#a
arrange(flights, desc(arr_delay)) %>%head(5)
#b
arrange(flights,desc(dep_delay - arr_delay)) %>% head(5)
#6
flights <- mutate(flights, speed = dist/(time/60))
flights <- mutate(flights, delta = dep_delay - arr_delay)
View(flights)
#a
arrange(flights,desc(speed)) %>% head(5)
#b
arrange(flights,desc(delta)) %>% head(5)
#c
arrange(flights,delta) %>% head(1)
#7
flights.7a <- flights %>% group_by(carrier) %>% summarise(
  cancelled = sum(cancelled),
  total_flights = n(),
  cancelled_percent = cancelled/total_flights,
  min = min(delta, na.rm = T),
  quantile_1st = quantile(delta, 0.25, na.rm = T),
  mean = mean(delta, na.rm = T),
  median = median(delta, na.rm = T),
  quantile_3rd = quantile(delta, 0.75, na.rm = T),
  quantile_90th = quantile(delta, 0.90, na.rm = T),
  max = max(delta, na.rm = T)
)
print(arrange(flights.7a,desc(cancelled_percent)))
day_delay <- flights %>% dplyr::filter(!is.na(dep_delay)) %>% group_by(date) %>%
  summarise(delay = mean(dep_delay), n = n()) %>% dplyr::filter(n > 10)
cat('filter the data of dep_delay with no NA, then find the average of dep_delay
    and flights,finally summarize the result with more than 10 fights in each day')
#8
day_delay <- day_delay %>% mutate(diff = delay - lag(delay))
arrange(day_delay,desc(diff)) %>% head(5)
#9
dest_delay <- flights %>% group_by(dest) %>%
  summarise (
    avg_arr_delay = mean(arr_delay, na.rm = T),
    number_flights = n()
  )
airports <- airports %>% 
  select(dest = iata, name = airport, city, state, lat, long)
df.9a <-left_join(airports,dest_delay, by="dest")
arrange(df.9a,desc(avg_arr_delay)) %>% head(5)
df.9b<-inner_join(airports,dest_delay, by="dest")
print('The left join does not match inner join')
df.9c<-right_join(airports,dest_delay, by="dest")
print('There are 116 observations. No NA appear in avg_arr_delay')
df.9d<- full_join(airports,dest_delay, by="dest")
sum(is.na(df.9d$avg_arr_delay))
print('There are 3378 observations and 3262 NA of avg_arr_delay.The reason is the
different rows of two tables.')
#10
hourly_delay <- filter(flights,!is.na(dep_delay)) %>% group_by(date,hour) %>%
  summarise(delay = mean(dep_delay), n = n())
hourly_delay %>% inner_join(weather) %>% group_by(conditions) %>% 
  summarise(max_delay = max(delay, na.rm=T)) %>% arrange(desc(max_delay))
#11
#a
library("tidyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
df <- data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))
df
df %>%
gather(subject, value, -treatment) %>% 
mutate(subject = subject %>% substr(8,9)) %>% 
select(subject, treatment, value)
#b
df <- data.frame(subject = c(1,1,2,2),treatment = c("a","b","a","b"),value = c(3,4,5,6))
df
df %>% 
spread( key = subject, value = value) %>%
rename(subject1 = `1`, subject2 = `2`)
#c
df <- data.frame(
  subject = c(1,2,3,4),
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  value = c(3,4,5,6)
  )
df
separate(df,demo, into = c('sex','age','state') , sep = '_')
#d
df <- data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m",NA),
  age = c(11,55,65,NA),
  city = c("DC","NY","WA",NA),
  value = c(3,4,5,6)
)
df
df <-unite(df,"demo", c(sex, age, city),sep = '.')
df[4,2] = NA
df




