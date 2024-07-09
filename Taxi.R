
library(ggplot2)
library(tidyverse)
library(dbscan)
library(dplyr)
library(data.table)
library(scattermore)
library(outliers)



taxi <- fread("taxi.csv")

ggplot(taxi, aes(x = Longitude, y = Latitude)) +
  geom_scattermore() +
  labs(title = "Location Points of Taxi", x = "Longitude", y = "Latitude")




#compute is there any invalid points
sum(is.na(taxi))

#Rome, Italy
#West 42.157626, 11.733559
#East 41.948640, 13.296241
#South 41.407578, 12.764920
#North 42.296137, 12.535792


#date and time:  from 01/Feb/2014 until 02/March/2014
#时区转换为罗马/意大利时间
as.POSIXct(taxi$`Date and Time`,tz="CET")

a=taxi$Longitude
b=taxi$Latitude
df<-data.frame(a,b)
df<-data.table(df)


df_noInvalid_ab<-df[which(df$b>=40&df$a>=0)]
ggplot(df_noInvalid_ab, aes(x = a, y = b)) +
  geom_scattermore() +
  labs(title = "Location Points of Taxi", x = "Longitude", y = "Latitude")


#find outliers by boxplot
boxplot(df_noInvalid_ab$a)
boxplot(df_noInvalid_ab$b)


aoutliers<-which(a %in% boxplot.stats(df_noInvalid_ab$a)$out)
boutliers<-which(b %in% boxplot.stats(df_noInvalid_ab$b)$out)
position_outliers<-union(aoutliers,boutliers)
poutliers<-intersect(aoutliers,boutliers)
df_noInvalid_ab[!position_outliers,]
ggplot(df, aes(x = a, y = b)) +
  geom_scattermore() +
  labs(title = "Location Points of Taxi", x = "Longitude", y = "Latitude")

boxplot.stats(df$b)
boxplot.stats(df$a)
aoutliers<-which(a %in% boxplot.stats(df$a)$out)
boutliers<-which(b %in% boxplot.stats(df$b)$out)
position_outliers<-union(aoutliers,boutliers)


df[!position_outliers,]
ggplot(df[!position_outliers], aes(x = a, y = b)) +
  geom_scattermore() +
  labs(title = "Location Points of Taxi", x = "Longitude", y = "Latitude")

ggplot(df[position_outliers,], aes(x = a, y = b, col="red")) +
  geom_scattermore() +
  labs(title = "Location Points of Taxi", x = "Longitude", y = "Latitude")

df_nooutliers<-df[!position_outliers,]

# Minimum, maximum, and mean location values
min_latitude <- min(df_nooutliers$b)
max_latitude <- max(df_nooutliers$b)
mean_latitude <- mean(df_nooutliers$b)

min_longitude <- min(df_nooutliers$a)
max_longitude <- max(df_nooutliers$a)
mean_longitude <- mean(df_nooutliers$a)

x_outliers<-which(a %in% boxplot.stats(a)$out)
x_outliers

y_outliers<-which(b %in% boxplot.stats(b)$out)
y_outliers

x1 <- boxplot.stats(a)
x_outliers <- x1$out
y1<-boxplot.stats(b)
y_outliers<-y1$out
position_outliers<-union(x_outliers,y_outliers)

#find the point which a,b are both outliers
p1<-intersect(x_outliers,y_outliers)
position_outliers<-union(x_outliers,y_outliers)

df[p1,]

ggplot(df[p1,],aes(x=a,y=b))+
  geom_scattermore()+
  labs(title = "Location Points of Taxi", x = "Longitude", y = "Latitude")
  
plot(df) 

ggplot(df, aes(x = a, y = b)) +
  geom_scattermore() +
  labs(title = "Location Points of Taxi", x = "Longitude", y = "Latitude")+
  geom_scattermore(data = df[p1,],aes(col="red",pch="x"))
  #points(df[position_outliers,],col="red",pch="x",cex=1)





#find outliers by DBSCAN
EPS = 7
cluster.dbscan <- dbscan(df, eps = EPS, minPts = 500, borderPoints = T, 
                         search = "kdtree")

#find best eps
kNNdistplot(df,k=50)
abline(h=0.15,lty=2)


result_dbscan<-dbscan(df,eps=0.05,minPts = 5)
clusters<-result_dbscan$cluster
noise<-result_dbscan$cluster==0

plot(df,col=clusters,pch = 20, main = "DBSCAN Clustering", xlab = "Latitude", ylab = "Longitude")
points(df[noise, ], col = "red", pch = 20)
legend("topright", legend = c("Cluster ", "Noise"), col = c("black","red"), pch = 20)


# Remove noise points from the dataset
taxi <- taxi[!noise, ]
ggplot(taxi, aes(x = Longitude, y = Latitude)) +
  geom_point(color = "firebrick", size = 0.5) +
  xlab("Longtitude")+
  ylab("Latitude")+
  ggtitle("Location Points of Taxi")+
  theme_set(theme_bw())

#############################3
a=taxi$Longitude
b=taxi$Latitude
outlier(a)
outlier(b)
taxi<-taxi[which(b>=40&a>=0&b<=51),]

aoutliers<-which(a %in% boxplot.stats(a)$out)
boutliers<-which(b %in% boxplot.stats(b)$out)
position_outliers<-union(aoutliers,boutliers)
taxi<-taxi[!position_outliers,]


mean_latitude <- mean(taxi$Latitude)
min_latitude <- min(taxi$Latitude)
max_latitude <- max(taxi$Latitude)

mean_longitude <- mean(taxi$Longitude)
min_longitude <- min(taxi$Longitude)
max_longitude <- max(taxi$Longitude)


taxi_activity <- taxi %>%
  group_by(DriveNo) %>%
  summarise(totalTime = difftime(max(`Date and Time`), min(`Date and Time`), units = "secs"))


most_active <- taxi_activity[which.max(taxi_activity$totalTime), ]
least_active <- taxi_activity[which.min(taxi_activity$totalTime), ]
average_time_driven <- sum(taxi_activity$totalTime)/315
average_time_driven



#timezone_adjustment <- substr(taxi$Date.and.Time, nchar(taxi$Date.and.Time) - 2, nchar(taxi$Date.and.Time))

# Remove the time zone adjustment from the Date.and.Time column
#taxi$Date.and.Time <- sub("\\+\\d{2}$", "", taxi$Date.and.Time)

# Convert Date and Time column to POSIXct format without altering the values
#taxi$Date.and.Time <- strptime(taxi$Date.and.Time, format = "%Y-%m-%d %H:%M:%OS")

# Add the time zone adjustment back to the Date.and.Time column
#attr(taxi$Date.and.Time, "tzone") <- timezone_adjustment


#timelist <- as.POSIXct(taxi$Date.and.Time, format = "%Y-%m-%d %H:%M:%OS")

# Calculate total time driven for each taxi driver
timedata <- taxi %>% 
  group_by(ID) %>% 
  summarise(total_time_driven = difftime(max(Date.and.Time), min(Date.and.Time), units = "hours"))

# Find the most active taxi driver
most_active <- timedata[which.max(timedata$total_time_driven), ]

# Find the least active taxi driver
least_active <- timedata[which.min(timedata$total_time_driven), ]

# Calculate average time driven
average_time_driven <- summarise(timedata$total_time_driven)/315






# Assuming the dataset includes timestamps for each trip
# Calculate total time driven for each taxi driver
activity <- data %>%
  group_by(DriveNo) %>%
  summarise(total_time_driven = sum(duration, na.rm = TRUE))


# Most active taxi driver (most time driven)
most_active <- activity[which.max(activity$total_time_driven), ]

# Least active taxi driver (least time driven)
least_active <- activity[which.min(activity$total_time_driven), ]

# Average activity
average_activity <- mean(activity$total_time_driven)



#My first_and_3_last_digits_of_student_number and TaxiID are 8119 44

taxi_44 <- taxi %>%
  filter(DriveNo == 44)

ggplot(taxi_44, aes(x = Longitude, y = Latitude)) +
  geom_point(size = 0.5, alpha = 0.5) +
  labs(title = "Location Points for Taxi 44", x = "Longitude", y = "Latitude") +
  theme_minimal()

mean_latitude_44 <- mean(taxi_44$Latitude)
min_latitude_44 <- min(taxi_44$Latitude)
max_latitude_44 <- max(taxi_44$Latitude)

mean_longitude_44 <- mean(taxi_44$Longitude)
min_longitude_44 <- min(taxi_44$Longitude)
max_longitude_44 <- max(taxi_44$Longitude)

x<-c(mean_longitude_44,min_longitude_44,max_longitude_44)
y<-c(mean_latitude_44,min_latitude_44,max_latitude_44)
x1<-c(mean_longitude,min_longitude,max_longitude)
y1<-c(mean_latitude,min_latitude,max_latitude)
plot(x,y)
points(x1,y1,col="red")
par(no.readonly=FALSE)

# Compare total time driven by taxi=ID with global mean, min, and max values
total_time_taxi_44 <- taxi_44 %>% 
  summarise(difftime(max(`Date and Time`), min(`Date and Time`), units = "secs"))
global_mean_time <- mean(taxi_activity$total_time_driven)
global_min_time <- min(taxi_activity$total_time_driven)
global_max_time <- max(taxi_activity$total_time_driven)




dlon <- diff(range(taxi_44$Longitude))
dlat <- diff(range(taxi_44$Latitude))
lat1 <- min(taxi_44$Latitude)
lat2 <- max(taxi_44$Latitude)
lon1 <- min(taxi_44$Longitude)
lon2 <- max(taxi_44$Longitude)
R <- 6371000  # Radius of the Earth in meters
a <- (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2 
c <- 2 * atan2(sqrt(a), sqrt(1-a))
distance_44 <- R * c
cat("Distance Traveled by Taxi 44:", distance_44, "meters\n")

R <- 6371000  # Radius of the Earth in meters
lat1 <- taxi_44$Latitude[1]
lon1 <- taxi_44$Longitude[1]
lat2 <- taxi_44$Latitude[nrow(taxi_44)]
lon2 <- taxi_44$Longitude[nrow(taxi_44)]

dlon <- lon2 - lon1
dlat <- lat2 - lat1
a <- (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2
c <- 2 * atan2(sqrt(a), sqrt(1-a))
distance <- R * c



# Initialize a variable to store the total distance.
total_distance <- 0
# The Earth's radius, in meters.
R <- 6371000
# Calculate the distance.
for (i in 1:(nrow(taxi_44) - 1)) {
  lat1 <- taxi_44$Latitude[i]* pi / 180
  lat2 <- taxi_44$Latitude[i + 1] * pi / 180
  dlat <- (taxi_44$Latitude[i + 1] - taxi_44$Latitude[i]) * pi / 180
  dlon <- (taxi_44$Longitude[i + 1] - taxi_44$Longitude[i]) * pi/180
  a <- (sin(dlat / 2))^2 + cos(lat1) * cos(lat2) * (sin(dlon / 2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  total_distance <- total_distance + R * c
}
print(total_distance)
