
#insatlling the required packages

install.packages("dplyr")
install.packages("stringr")
install.packages("ggplot2")

#loading the required packages

library("dplyr")
library("stringr")
library("ggplot2")


#-------Reading the input file---------------------------------------------

uber_request <- read.csv("Uber Request Data.csv",stringsAsFactors = F)


#------------------------------------Data Cleaning-------------------------------------------------------------------------------

# Checking for duplicate entry

uber_duplicated <- duplicated(uber_request)

#Since sum is zero hence there is no duplicate entries
sum(uber_duplicated)


#Checking for NAs

# Finding out the NAs in the data set

uber_request %>%
 select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))

#Based on the above query we found that only Driver.id and Drop.timestamp are having NA values

uber_request_na_driverId <- uber_request[which(is.na(uber_request$Driver.id)),]

unique_status <- unique(uber_request_na_driverId$Status)

# Based on the above query it is clear that- for all the request where driver id is NA- the status of trip is"no cars available"

no_cars_available <- uber_request_na_driverId[colSums(!is.na(uber_request_na_driverId))>0]
View(no_cars_available)

#The request in which the driverID is not NA but Drop.timestamp is NA

uber_request_na_droptime <-uber_request[which(!is.na(uber_request$Driver.id) & (is.na(uber_request$Drop.timestamp))),]
View(uber_request_na_droptime)

unique_status2 <- unique(uber_request_na_droptime$Status)
View(unique_status2)

#based on the above query it is clear that these are cancelled request

cancelled_request <- uber_request_na_droptime[colSums(!is.na(uber_request_na_droptime))>0]
View(cancelled_request)

#Hence all the NAs are because of valid business scenarios.There is no NA because of wrong data entry


#----------------------------------------Data Preperation------------------------------------------------------------------------------

# converting all the strings to uniform case

uber_request <- data.frame(lapply(uber_request, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}),stringsAsFactors = F)


#converting all date-time columns to a uniform format

uber_request$Request.timestamp <- str_replace_all(uber_request$Request.timestamp, "[/]",  "-")
uber_request$Drop.timestamp <- str_replace_all(uber_request$Drop.timestamp, "[/]",  "-")

# convert time columns to datetime oject
uber_request$Request.timestamp <- as.POSIXct(uber_request$Request.timestamp, format = "%d-%m-%Y %H:%M")
uber_request$Drop.timestamp <- as.POSIXct(uber_request$Drop.timestamp, format = "%d-%m-%Y %H:%M")
View(uber_request)

# Finding the day of the request 
uber_request$Request_day <- weekdays(uber_request$Request.timestamp)

# Finding the hour of the request and drop
uber_request$reqhour <- format(uber_request$Request.timestamp,"%H")
uber_request$drophour <- format(uber_request$Drop.timestamp, "%H")

#Trip duration
uber_request$tripduration <- difftime(uber_request$Drop.timestamp,uber_request$Request.timestamp,units="mins")


#Categorising the reqhour into various time slots

uber_request$reqhour <- as.numeric(uber_request$reqhour)
uber_request$time_slot = ifelse((uber_request$reqhour >1 & uber_request$reqhour< 6), "earlymorning", 
                                ifelse((uber_request$reqhour>5 & uber_request$reqhour< 11),"morning",
                                       ifelse((uber_request$reqhour > 10 & uber_request$reqhour < 16) ,"daytime",
                                              ifelse((uber_request$reqhour>16 & uber_request$reqhour < 22),"evening","latenight"))))


View(uber_request)

#---------------------------------------------------------Plotting the data to get feel of the issue--------------------------------


# plotting the request based on the staus and the time slot

ggplot(uber_request, aes(x = as.factor(time_slot), fill= as.factor(uber_request$Status))) + geom_bar(position = "dodge")+labs(x = "Time Slot", y = "Number of Requests", fill = "Status" )


#comparing the pattern of request on various days

unique_request_days <- unique(uber_request$Request_day)
uber_city2airport_request <- subset(uber_request,uber_request$Pickup.point=="city")
uber_airport2city_request <- subset(uber_request,uber_request$Pickup.point=="airport")

# plot of the request for city to airport trip on hourly basis
plot_day_wise <- ggplot(uber_city2airport_request, aes(x = as.factor(uber_city2airport_request$reqhour),fill = Status))+geom_bar(position = "dodge")
plot_day_wise + facet_wrap( ~ uber_city2airport_request$Request_day, nrow =nrow(unique_request_days), ncol = 1) + labs(x = "Hour", y = "Number of Requests", fill = "Status" )

# plot of the request from airport to city trip on hourly basis
plot_day_wise <- ggplot(uber_airport2city_request, aes(x = as.factor(uber_airport2city_request$reqhour),fill = Status))+geom_bar(position = "dodge")
plot_day_wise + facet_wrap( ~ uber_airport2city_request$Request_day, nrow =nrow(unique_request_days), ncol = 1) + labs(x = "Hour", y = "Number of Requests", fill = "Status" )


# Request status distribution across the days of the week

plot_day_wise <- ggplot(uber_request, aes(x = as.factor(uber_request$time_slot),fill = Status))+geom_bar(position = "fill")
plot_day_wise + facet_wrap( ~ uber_request$Request_day, nrow =5, ncol = 1) + labs(x = "Time Slot", y = "Status Distribution", fill = "Status" )


#comparing the pattern of request based on pickup point

plot_pickup <- ggplot(uber_request,aes(x=as.factor(time_slot),fill=Status))+geom_bar(position = "dodge")
plot_pickup +facet_wrap(~uber_request$Pickup.point,nrow = 2,ncol=1)+labs(x="Time Slot",y="Number Of Requests",fill="Status")

# Based on the above plots it is clear that there are two problems:
#   1. Higher Cancellations During Morning for the city to Airport trip
#   2. No cars available during Evening for the  Airport to city trip

#NOTE: These two problems are same on all the day hence ignoring the aspect of "day-effect" on the trip status.




#----------------------------- Problem 1: Higher Cancellations During Morning for the city to Airport trip---------------------------------

# Measuring problem1

morning_city2airport_request <- subset(uber_request,uber_request$time_slot=="morning" & uber_request$Pickup.point=="city" )

View(morning_city2airport_request)

requestStaus_count <- count(morning_city2airport_request,morning_city2airport_request$Status)

names(requestStaus_count) <- c("status","count")

View(requestStaus_count)

#Plotting the request count and status for the city to airport trip

ggplot(requestStaus_count,aes(x=requestStaus_count$status,y=requestStaus_count$count))+geom_bar(stat="identity",fill="lightblue") +geom_text(aes(label=requestStaus_count$count))+labs(title="Morning City to Airport Trip",x = "status",y="request count")


#----------------------------------------------Problem 2:No cars available during Evening for the  Airport to city trip--------------------

# measuring problem

evening_airport2city_request <-subset(uber_request,uber_request$time_slot=="evening" & uber_request$Pickup.point=="airport" )

evening_requestStaus_count <- count(evening_airport2city_request,evening_airport2city_request$Status)

names(evening_requestStaus_count) <- c("status","count")

#Plotting the request count and status for the airport to city trip
ggplot(evening_requestStaus_count,aes(x=evening_requestStaus_count$status,y=evening_requestStaus_count$count))+geom_bar(stat="identity",fill="lightblue") +geom_text(aes(label=evening_requestStaus_count$count))+labs(title="Evening Airport to City Trip",x = "status",y="request count")



#----------------------------------------Root Cause Analysis-----------------------------------------------------------

# 1. Long distance between city and Airport--

#  finding completed trips

completed_trips <- uber_request[uber_request$Status=="trip completed",]
morning_completedtrip <- completed_trips[as.character(completed_trips$time_slot)=="morning",]

# Average time of the city to Airport(52.7 min) 
avg_tripduration <- mean(morning_completedtrip$tripduration)
View(avg_tripduration)


#2. There is less demand from Airport to city trip  till evening and few cars in evening at Airport

uber_request_airport <- uber_request[uber_request$Pickup.point=="airport",]
View(uber_request_airport)
ggplot(uber_request_airport, aes(x = as.factor(time_slot), fill= as.factor(uber_request_airport$Status))) + geom_bar()+labs(x = "Time Slot", y = "Number of Requests", fill = "Status" )


uber_request_city <- subset(uber_request,uber_request$Pickup.point=="city")
ggplot(uber_request_city, aes(x = as.factor(time_slot), fill= as.factor(uber_request_city$Status))) + geom_bar()+labs(x = "Time Slot", y = "Number of Requests", fill = "Status" )

#-----------------------------------------Suggesting ways to solve the Issue------------------------------

# Finding the top 5 drivers who cancel alot of trips form city to airport during Morning

cancelled_city2airport_morn <- subset(uber_request,uber_request$Status=="cancelled" & uber_request$time_slot=="morning" & uber_request$Pickup.point=="city")

Drivers_cancellation_count <- count(cancelled_city2airport_morn,cancelled_city2airport_morn$Driver.id)

names(Drivers_cancellation_count) <- c("DriverID","count")

top_5_trip_canceller <- top_n(Drivers_cancellation_count,5,Drivers_cancellation_count$count)
 View(top_5_trip_canceller)

 
# Finding the top 5 drivers who completes the trips from city to airport during Morning
 
 
 completed_city2airport_morn <- subset(uber_request,uber_request$Status=="trip completed" & uber_request$time_slot=="morning" & uber_request$Pickup.point=="city")
 
 Drivers_completion_count <- count(completed_city2airport_morn,completed_city2airport_morn$Driver.id)
 
 names(Drivers_completion_count) <- c("DriverID","count")
 
 top_5_trip_completer <- top_n(Drivers_completion_count,5,Drivers_completion_count$count)
 View(top_5_trip_completer)
 
 
 # Finding the top 5 drivers who completes the trips from airport to city during evening
 
 
 completed_airport2city_even <- subset(uber_request,uber_request$Status=="trip completed" & uber_request$time_slot=="evening" & uber_request$Pickup.point=="airport")
 
 Drivers_completion_count <- count(completed_city2airport_morn,completed_city2airport_morn$Driver.id)
 
 names(Drivers_completion_count) <- c("DriverID","count")
 
 top_5_trip_completer <- top_n(Drivers_completion_count,5,Drivers_completion_count$count)
 View(top_5_trip_completer)
 