#  Load SparkR
spark_path <- '/usr/local/spark'
if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = spark_path)
}

library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

##Loading other libraries

library(dplyr)
library(stringr)
library(ggplot2)

# Initialise the sparkR session

sparkR.session(master = "yarn-client", sparkConfig = list(spark.driver.memory = "1g"))

# Before executing any hive-sql query from RStudio, you need to add a jar file in RStudio 

sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

# Reading the three years data

nyc_tkt_2015 <- SparkR::read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2015.csv","CSV",header="true",inferschema="true")
nyc_tkt_2016 <- SparkR::read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2016.csv","CSV",header="true",inferschema="true")
nyc_tkt_2017 <- SparkR::read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2017.csv","CSV",header="true",inferschema="true")

#Getting the feel of data

head(nyc_tkt_2015)
dim(nyc_tkt_2015)

#2015 Dataset has Rows: 11,809,233 | Columns: 51

head(nyc_tkt_2016)
dim(nyc_tkt_2016)

#2016 Dataset has Rows: 10,626,899 | Columns: 51

head(nyc_tkt_2017)
dim(nyc_tkt_2017)

#2017 Dataset has Rows: 10,803,028 | Columns: 43

###We observed that the Column names are Seperated by Space and this could lead to problems hence
##Seperating it via _

colnames(nyc_tkt_2015)<- str_trim(colnames(nyc_tkt_2015), side= "both")
colnames(nyc_tkt_2015)<- str_replace_all(colnames(nyc_tkt_2015), pattern=" ", replacement = "_")
colnames(nyc_tkt_2015)<- str_replace_all(colnames(nyc_tkt_2015), pattern="\\?", replacement = "")
colnames(nyc_tkt_2015)

colnames(nyc_tkt_2016)<- str_trim(colnames(nyc_tkt_2015), side= "both")
colnames(nyc_tkt_2016)<- str_replace_all(colnames(nyc_tkt_2015), pattern=" ", replacement = "_")
colnames(nyc_tkt_2016)<- str_replace_all(colnames(nyc_tkt_2015), pattern="\\?", replacement = "")
colnames(nyc_tkt_2016)

colnames(nyc_tkt_2017)<- str_trim(colnames(nyc_tkt_2017), side= "both")
colnames(nyc_tkt_2017)<- str_replace_all(colnames(nyc_tkt_2017), pattern=" ", replacement = "_")
colnames(nyc_tkt_2017)<- str_replace_all(colnames(nyc_tkt_2017), pattern="\\?", replacement = "")
colnames(nyc_tkt_2017)

######Detailed data Quality Check of 2015 data #################
##Removing any duplicates wrt Summon_Number

nyc_tkt_2015 <- dropDuplicates(nyc_tkt_2015,"Summons_Number")
dim(nyc_tkt_2015)

# Now The dimensions are as Follows: Rows: 10,951,256 | Columns: 51

########Checking Data issue wrt to Issue date#################

#Create Temp View
createOrReplaceTempView(nyc_tkt_2015,"nyc_tkt_2015_tbl")

#Checking if the Issue date is null- From the below query the count is 0 hence no null for issue date

nyc_2015_IssueDateNull <- SparkR::sql("select count(1) from nyc_tkt_2015_tbl where Issue_Date is null ")

head(nyc_2015_IssueDateNull)

#Converting date data types to suitable format

nyc_tkt_2015$Issue_Date <- SparkR::to_date(nyc_tkt_2015$Issue_Date, 'MM/dd/yyyy')
nyc_tkt_2015$Vehicle_Expiration_Date <- SparkR::to_date(nyc_tkt_2015$Vehicle_Expiration_Date, 'yyyyMMdd')
nyc_tkt_2015$Date_First_Observed <- SparkR::to_date(nyc_tkt_2015$Date_First_Observed, 'yyyyMMdd')

# Understand the Range of ticket Issue Dates Available in the Dataset
createOrReplaceTempView(nyc_tkt_2015, "tkt_2015_nyc")

Range_Issue_Date_2015 <- SparkR::sql("SELECT min(issue_date)as Min_IssueDate_2015,
                                     max(issue_date)as Max_IssueDate_2015
                                     FROM tkt_2015_nyc")
head(Range_Issue_Date_2015)
# Min_IssueDate_2015 : 1985-07-16
# Max_IssueDate_2015 : 2015-06-30
# The Issue Tickets range between 16th July 1985 to 30th June 2015. Clearly this is Nonconforming. 

#Create Additional Columns in the Dataset that Correspond to the Year and Month of Ticket Issue

nyc_tkt_2015$Issue_Year <- year(nyc_tkt_2015$Issue_Date)
nyc_tkt_2015$Issue_Month <- month(nyc_tkt_2015$Issue_Date)

#Now let's observe the Distribution of Issue Date.

createOrReplaceTempView(nyc_tkt_2015, "tkt_2015_nyc")

Grouped_Issue_Date_2015 <- SparkR::sql("SELECT Issue_Year,
                                       Issue_Month,
                                       count(1)as Num_of_Records
                                       FROM tkt_2015_nyc
                                       GROUP BY Issue_Year,
                                       Issue_Month
                                       ORDER BY 1,2")


dfgrouped_issue_ym_2015 <- data.frame(head(Grouped_Issue_Date_2015, nrow(Grouped_Issue_Date_2015)))
View(dfgrouped_issue_ym_2015)

#Subsetting the DataFrame according to the Fiscal Year
# Considering A Fiscal Year to extend from the July of Pervious Year to June of the Current Year.
nyc_tkt_2015 <- nyc_tkt_2015[
  nyc_tkt_2015$Issue_Date >= "2014-07-01" & 
    nyc_tkt_2015$Issue_Date <= "2015-06-30"]

nrow(nyc_tkt_2015)

# 10,598,035 records in filtered dataset
##The columns "Latitude", "Longitude", "Community_Board", "Community_Council", "Census_Tract", "BIN", "BBL" and "NTA" are logged for the fiscal year 2015 and 2016 but not for 2017. We will Check the Number of Null values in the aforementioned columns in nyc_tkt_2015 

createOrReplaceTempView(nyc_tkt_2015, "tkt_2015_nyc")

CountNull_ExtraCol_2015 <- SparkR::sql("SELECT COUNT(*) Num_of_Rows,
                                       SUM(CASE WHEN Plate_ID IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Plate_ID,
                                       SUM(CASE WHEN No_Standing_or_Stopping_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_stand_stop,
                                       SUM(CASE WHEN Hydrant_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Hydrant_Violation,
                                       SUM(CASE WHEN Double_Parking_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Double_Parking_Violation,
                                       SUM(CASE WHEN Latitude IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Latitude,
                                       SUM(CASE WHEN Longitude IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Longitude,
                                       SUM(CASE WHEN Community_Board IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Community_Board,
                                       SUM(CASE WHEN Community_Council IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Community_Council,
                                       SUM(CASE WHEN Census_Tract IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Census_Tract,
                                       SUM(CASE WHEN BIN IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_BIN,
                                       SUM(CASE WHEN BBL IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_BBL,
                                       SUM(CASE WHEN NTA IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_NTA     
                                       FROM tkt_2015_nyc")
head(CountNull_ExtraCol_2015)

#All the additional columns have only null values therefore they will be dropped to standardize the dataset between the years.
#Removing these Null Columns.

nyc_tkt_2015<- drop(nyc_tkt_2015, c("No_Standing_or_Stopping_Violation", "Hydrant_Violation", "Double_Parking_Violation", "Latitude","Longitude","Community_Board", "Community_Council","Census_Tract","BIN" , "BBL",  "NTA") )
colnames(nyc_tkt_2015)

#1.8.5 Fixing the format for the Violation Time [Time First Observed, From Hours in Effect and To Hours in Effect are also converted.]
#We can observe that the string format of the time attributes include only a partial component of AM/PM. Therefore we will append M to the end of each time attribute before converting it into a timestamp.

nyc_tkt_2015$Concat_M <- "M"
nyc_tkt_2015$Violation_Time<-concat(nyc_tkt_2015$Violation_Time,  nyc_tkt_2015$Concat_M)
nyc_tkt_2015$Time_First_Observed<- concat(nyc_tkt_2015$Time_First_Observed, nyc_tkt_2015$Concat_M)
nyc_tkt_2015$From_Hours_In_Effect<- concat(nyc_tkt_2015$From_Hours_In_Effect, nyc_tkt_2015$Concat_M)
nyc_tkt_2015$To_Hours_In_Effect<- concat(nyc_tkt_2015$To_Hours_In_Effect, nyc_tkt_2015$Concat_M)
nyc_tkt_2015<- drop(nyc_tkt_2015, c("Concat_M"))

head(nyc_tkt_2015)


#Extracting Violation Hour, Violation Minute and Part of Day.

nyc_tkt_2015$Violation_Hour <- substr(nyc_tkt_2015$Violation_Time, 1, 2)
nyc_tkt_2015$Violation_Minute <- substr(nyc_tkt_2015$Violation_Time, 4, 5)
nyc_tkt_2015$Violation_AMPM <- substr(nyc_tkt_2015$Violation_Time, 6, 7)

#We've observed that there are records that have both 00xxAM as well as 12xxAM. Therefore we will replace all 00xxAM with 12xxAM

nyc_tkt_2015$Violation_Hour <- regexp_replace(x = nyc_tkt_2015$Violation_Hour,pattern = "\\00",replacement = "12")

#Concatenating the components into a standardized Violation Time.
nyc_tkt_2015$Violation_Time <- concat(nyc_tkt_2015$Violation_Hour, nyc_tkt_2015$Violation_Minute, nyc_tkt_2015$Violation_AMPM)

#Converting Violation Time into a TimeStamp
nyc_tkt_2015$Violation_Time<-to_timestamp(x = nyc_tkt_2015$Violation_Time, format = "hhmma")

#Converting the other time attributes into a TimeStamp.

nyc_tkt_2015$Time_First_Observed<- to_timestamp(x= nyc_tkt_2015$Time_First_Observed, format = "hhmma")
nyc_tkt_2015$From_Hours_In_Effect<- to_timestamp(x= nyc_tkt_2015$From_Hours_In_Effect, format = "hhmma")
nyc_tkt_2015$To_Hours_In_Effect<- to_timestamp(x= nyc_tkt_2015$To_Hours_In_Effect, format = "hhmma")

#The dimensions of Formatted and Cleaned Dataset that will be used for Analysis:
head(nyc_tkt_2015)
dim(nyc_tkt_2015)

#2015 Dataset has Rows: 10,598,035 | Columns: 45
printSchema(nyc_tkt_2015)

#All attributes are in standardized formats now 

####################Data-2016####################

#Detailed Data Quality Verification of 2016 NYC Parking Ticket Dataset
#Removing any duplicate rows in the dataset [Two or More rows having the same Summons_Number ]

nyc_tkt_2016<- dropDuplicates(nyc_tkt_2016, "Summons_Number")
dim(nyc_tkt_2016)

# After Removing Duplicate records of 2016 dataset. The dimensions- Rows: 10,626,899 | Columns: 51

# Let us check if there are any missing values in the Issue Date Parameter.
createOrReplaceTempView(nyc_tkt_2016, "tkt_2016_nyc")
CountNull_IssueDate_2016 <- SparkR::sql("SELECT SUM(CASE WHEN Issue_Date IS NULL
                                        THEN 1
                                        ELSE 0
                                        END) nulls_Issue_Date,
                                        COUNT(*) Num_of_Rows
                                        FROM tkt_2016_nyc")
head(CountNull_IssueDate_2016)

#There are no records with missing or null Issue Dates.

#Converting the Date Paramters[Issue Date, Vehicle Expiration Date and Date First Observed] to a suitable format for Analysis.
nyc_tkt_2016$Issue_Date <- SparkR::to_date(nyc_tkt_2016$Issue_Date, 'MM/dd/yyyy')
nyc_tkt_2016$Vehicle_Expiration_Date <- SparkR::to_date(cast(nyc_tkt_2016$Vehicle_Expiration_Date,"string"), 'yyyyMMdd')
nyc_tkt_2016$Date_First_Observed <- SparkR::to_date(cast(nyc_tkt_2016$Date_First_Observed,"string"), 'yyyyMMdd')

#The Range of ticket Issue Dates Available in the Dataset
createOrReplaceTempView(nyc_tkt_2016, "tkt_2016_nyc")
Range_Issue_Date_2016 <- SparkR::sql("SELECT min(issue_date)as Min_IssueDate_2016,
                                     max(issue_date)as Max_IssueDate_2016
                                     FROM tkt_2016_nyc")
head(Range_Issue_Date_2016)
# Min_IssueDate_2016 : 1970-04-13
# Max_IssueDate_2016 : 2069-10-02
# The Issue Tickets range between 13th April 1970 to 2nd October 2069. Clearly this is Nonconforming. 

#We Will create Additional Columns in the Dataset that Correspond to the Year and Month of Ticket Issue
nyc_tkt_2016$Issue_Year <- year(nyc_tkt_2016$Issue_Date)
nyc_tkt_2016$Issue_Month <- month(nyc_tkt_2016$Issue_Date)

#Now let's observe the Distribution of Issue Date.
createOrReplaceTempView(nyc_tkt_2016, "tkt_2016_nyc")
Grouped_Issue_Date_2016 <- SparkR::sql("SELECT Issue_Year,
                                       Issue_Month,
                                       count(*)as Num_of_Records
                                       FROM tkt_2016_nyc
                                       GROUP BY Issue_Year,
                                       Issue_Month
                                       ORDER BY 1,2")



dfgrouped_issue_ym_2016 <- data.frame(head(Grouped_Issue_Date_2016, nrow(Grouped_Issue_Date_2016)))
View(dfgrouped_issue_ym_2016)

#Subsetting the DataFrame according to the Fiscal Year.
# Considering A Fiscal Year to extend from the July of Pervious Year to June of the Current Year.

nyc_tkt_2016 <- nyc_tkt_2016[
  nyc_tkt_2016$Issue_Date >= "2015-07-01" & 
    nyc_tkt_2016$Issue_Date <= "2016-06-30"]

nrow(nyc_tkt_2016)

# 10,396,894 records in the filtered dataset

#The columns "Latitude", "Longitude", "Community_Board", "Community_Council", "Census_Tract", "BIN", "BBL" and "NTA" are logged for the fiscal year 2015 and 2016 but not for 2017. We will Check the Number of Null values in the aforementioned columns in tkt_2016_nycparking. 
createOrReplaceTempView(nyc_tkt_2016, "tkt_2016_nyc")
CountNull_ExtraCol_2016 <- SparkR::sql("SELECT COUNT(*) Num_of_Rows,
                                       SUM(CASE WHEN Plate_ID IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Plate_ID,
                                       SUM(CASE WHEN No_Standing_or_Stopping_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_stand_stop,
                                       SUM(CASE WHEN Hydrant_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Hydrant_Violation,
                                       SUM(CASE WHEN Double_Parking_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Double_Parking_Violation,
                                       SUM(CASE WHEN Latitude IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Latitude,
                                       SUM(CASE WHEN Longitude IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Longitude,
                                       SUM(CASE WHEN Community_Board IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Community_Board,
                                       SUM(CASE WHEN Community_Council IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Community_Council,
                                       SUM(CASE WHEN Census_Tract IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_Census_Tract,
                                       SUM(CASE WHEN BIN IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_BIN,
                                       SUM(CASE WHEN BBL IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_BBL,
                                       SUM(CASE WHEN NTA IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) Nulls_NTA     
                                       FROM tkt_2016_nyc")
head(CountNull_ExtraCol_2016)

#All the additional columns have only null values therefore they will be dropped to standardize the dataset between the years.

#Removing these Null Columns.
nyc_tkt_2016<- drop(nyc_tkt_2016, c("No_Standing_or_Stopping_Violation", "Hydrant_Violation", "Double_Parking_Violation", "Latitude","Longitude","Community_Board", "Community_Council","Census_Tract","BIN" , "BBL",  "NTA") )
colnames(nyc_tkt_2016)

#We can observe that the string format of the time attributes include only a partial component of AM/PM. Therefore we will append M to the end of each time attribute before converting it into a timestamp.
nyc_tkt_2016$Concat_M <- "M"
nyc_tkt_2016$Violation_Time<-concat(nyc_tkt_2016$Violation_Time,  nyc_tkt_2016$Concat_M)
nyc_tkt_2016$Time_First_Observed<- concat(nyc_tkt_2016$Time_First_Observed, nyc_tkt_2016$Concat_M)
nyc_tkt_2016$From_Hours_In_Effect<- concat(nyc_tkt_2016$From_Hours_In_Effect, nyc_tkt_2016$Concat_M)
nyc_tkt_2016$To_Hours_In_Effect<- concat(nyc_tkt_2016$To_Hours_In_Effect, nyc_tkt_2016$Concat_M)
nyc_tkt_2016<- drop(nyc_tkt_2016, c("Concat_M"))

#Since we are conducting an Analysis with Violation Time we will look into this attribute a littl closer.
#Extracting Violation Hour, Violation Minute and Part of Day.
nyc_tkt_2016$Violation_Hour <- substr(nyc_tkt_2016$Violation_Time, 1, 2)
nyc_tkt_2016$Violation_Minute <- substr(nyc_tkt_2016$Violation_Time, 4, 5)
nyc_tkt_2016$Violation_AMPM <- substr(nyc_tkt_2016$Violation_Time, 6, 7)

# replace all 00xxAM with 12xxAM

nyc_tkt_2016$Violation_Hour <- regexp_replace(x = nyc_tkt_2016$Violation_Hour,pattern = "\\00",replacement = "12")

#Concatenating the components into a standardized Violation Time.
nyc_tkt_2016$Violation_Time <- concat(nyc_tkt_2016$Violation_Hour, nyc_tkt_2016$Violation_Minute, nyc_tkt_2016$Violation_AMPM)

#Converting Violation Time into a TimeStamp
nyc_tkt_2016$Violation_Time<-to_timestamp(x = nyc_tkt_2016$Violation_Time, format = "hhmma")

#Converting the other time attributes into a TimeStamp.
nyc_tkt_2016$Time_First_Observed<- to_timestamp(x= nyc_tkt_2016$Time_First_Observed, format = "hhmma")
nyc_tkt_2016$From_Hours_In_Effect<- to_timestamp(x= nyc_tkt_2016$From_Hours_In_Effect, format = "hhmma")
nyc_tkt_2016$To_Hours_In_Effect<- to_timestamp(x= nyc_tkt_2016$To_Hours_In_Effect, format = "hhmma")

#The dimensions of Formatted and Cleaned Dataset that will be used for Analysis:
head(nyc_tkt_2016)
dim(nyc_tkt_2016)

#2016 Dataset has Rows: 10,396,894 | Columns: 45
printSchema(tkt_2016_nycparking)

#All attributes are in standardized formats

####################Data-2017####################

#Detailed Data Quality Verification of 2017 NYC Parking Ticket Dataset
#Removing any duplicate rows in the dataset [Two or More rows having the same Summons_Number ]

nyc_tkt_2017<- dropDuplicates(nyc_tkt_2017, "Summons_Number")
dim(nyc_tkt_2017)

# After Removing Duplicate records of 2017 dataset. The dimensions- Rows: 10,803,028 | Columns: 43

# Let us check if there are any missing values in the Issue Date Parameter.
createOrReplaceTempView(nyc_tkt_2017, "tkt_2017_nyc")
CountNull_IssueDate_2017 <- SparkR::sql("SELECT SUM(CASE WHEN Issue_Date IS NULL
                                        THEN 1
                                        ELSE 0
                                        END) nulls_Issue_Date,
                                        COUNT(*) Num_of_Rows
                                        FROM tkt_2017_nyc")
head(CountNull_IssueDate_2017)

#There are no records with missing or null Issue Dates.

#Converting the Date Paramters[Issue Date, Vehicle Expiration Date and Date First Observed] to a suitable format for Analysis.
nyc_tkt_2017$Issue_Date <- SparkR::to_date(nyc_tkt_2017$Issue_Date, 'MM/dd/yyyy')
nyc_tkt_2017$Vehicle_Expiration_Date <- SparkR::to_date(cast(nyc_tkt_2017$Vehicle_Expiration_Date,"string"), 'yyyyMMdd')
nyc_tkt_2017$Date_First_Observed <- SparkR::to_date(cast(nyc_tkt_2017$Date_First_Observed,"string"), 'yyyyMMdd')

#The Range of ticket Issue Dates Available in the Dataset
createOrReplaceTempView(nyc_tkt_2017, "tkt_2017_nyc")
Range_Issue_Date_2017 <- SparkR::sql("SELECT min(issue_date)as Min_IssueDate_2017,
                                     max(issue_date)as Max_IssueDate_2017
                                     FROM tkt_2017_nyc")
head(Range_Issue_Date_2017)
# Min_IssueDate_2017 : 1972-03-30
# Max_IssueDate_2017 : 2069-11-19
# The Issue Tickets range between 30th March 1972 to 19th November 2069. Clearly this is Nonconforming. 

#We Will create Additional Columns in the Dataset that Correspond to the Year and Month of Ticket Issue

nyc_tkt_2017$Issue_Year <- year(nyc_tkt_2017$Issue_Date)
nyc_tkt_2017$Issue_Month <- month(nyc_tkt_2017$Issue_Date)

#Now let's observe the Distribution of Issue Date.

createOrReplaceTempView(nyc_tkt_2017, "tkt_2017_nyc")
Grouped_Issue_Date_2017 <- SparkR::sql("SELECT Issue_Year,
                                       Issue_Month,
                                       count(*)as Num_of_Records
                                       FROM tkt_2017_nyc
                                       GROUP BY Issue_Year,
                                       Issue_Month
                                       ORDER BY 1,2")



dfgrouped_issue_ym_2017 <- data.frame(head(Grouped_Issue_Date_2017, nrow(Grouped_Issue_Date_2017)))
View(dfgrouped_issue_ym_2017)

#Subsetting the DataFrame according to the Fiscal Year.
# Considering A Fiscal Year to extend from the July of Pervious Year to June of the Current Year.

nyc_tkt_2017 <- nyc_tkt_2017[
  nyc_tkt_2017$Issue_Date >= "2016-07-01" &
    nyc_tkt_2017$Issue_Date <= "2017-06-30"]

nrow(nyc_tkt_2017)

# 10,539,563 records in the filtered dataset

#The columns "Latitude", "Longitude", "Community_Board", "Community_Council", "Census_Tract", "BIN", "BBL" and "NTA" are logged for the fiscal year 2015 and 2017 but not for 2017. We will Check the Number of Null values in the aforementioned columns in tkt_2017_nycparking. 

createOrReplaceTempView(nyc_tkt_2017, "tkt_2017_nyc")
CountNull_ExtraCol_2017 <- SparkR::sql("SELECT COUNT(*) Num_of_Rows,
                                       SUM(CASE WHEN Plate_ID IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Plate_ID,
                                       SUM(CASE WHEN No_Standing_or_Stopping_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_stand_stop,
                                       SUM(CASE WHEN Hydrant_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Hydrant_Violation,
                                       SUM(CASE WHEN Double_Parking_Violation IS NULL
                                       THEN 1
                                       ELSE 0
                                       END) nulls_Double_Parking_Violation
                                       from tkt_2017_nyc")
head(CountNull_ExtraCol_2017)

#All the additional columns have only null values therefore they will be dropped to standardize the dataset between the years.

#Removing these Null Columns.
nyc_tkt_2017<- drop(nyc_tkt_2017, c("No_Standing_or_Stopping_Violation", "Hydrant_Violation", "Double_Parking_Violation") )
colnames(nyc_tkt_2017)

#We can observe that the string format of the time attributes include only a partial component of AM/PM. Therefore we will append M to the end of each time attribute before converting it into a timestamp.

nyc_tkt_2017$Concat_M <- "M"
nyc_tkt_2017$Violation_Time<-concat(nyc_tkt_2017$Violation_Time,  nyc_tkt_2017$Concat_M)
nyc_tkt_2017$Time_First_Observed<- concat(nyc_tkt_2017$Time_First_Observed, nyc_tkt_2017$Concat_M)
nyc_tkt_2017$From_Hours_In_Effect<- concat(nyc_tkt_2017$From_Hours_In_Effect, nyc_tkt_2017$Concat_M)
nyc_tkt_2017$To_Hours_In_Effect<- concat(nyc_tkt_2017$To_Hours_In_Effect, nyc_tkt_2017$Concat_M)
nyc_tkt_2017<- drop(nyc_tkt_2017, c("Concat_M"))

#Since we are conducting an Analysis with Violation Time we will look into this attribute a littl closer.
#Extracting Violation Hour, Violation Minute and Part of Day.
nyc_tkt_2017$Violation_Hour <- substr(nyc_tkt_2017$Violation_Time, 1, 2)
nyc_tkt_2017$Violation_Minute <- substr(nyc_tkt_2017$Violation_Time, 4, 5)
nyc_tkt_2017$Violation_AMPM <- substr(nyc_tkt_2017$Violation_Time, 6, 7)

# replace all 00xxAM with 12xxAM

nyc_tkt_2017$Violation_Hour <- regexp_replace(x = nyc_tkt_2017$Violation_Hour,pattern = "\\00",replacement = "12")

#Concatenating the components into a standardized Violation Time.
nyc_tkt_2017$Violation_Time <- concat(nyc_tkt_2017$Violation_Hour, nyc_tkt_2017$Violation_Minute, nyc_tkt_2017$Violation_AMPM)

#Converting Violation Time into a TimeStamp
nyc_tkt_2017$Violation_Time<-to_timestamp(x = nyc_tkt_2017$Violation_Time, format = "hhmma")

#Converting the other time attributes into a TimeStamp.
nyc_tkt_2017$Time_First_Observed<- to_timestamp(x= nyc_tkt_2017$Time_First_Observed, format = "hhmma")
nyc_tkt_2017$From_Hours_In_Effect<- to_timestamp(x= nyc_tkt_2017$From_Hours_In_Effect, format = "hhmma")
nyc_tkt_2017$To_Hours_In_Effect<- to_timestamp(x= nyc_tkt_2017$To_Hours_In_Effect, format = "hhmma")

#The dimensions of Formatted and Cleaned Dataset that will be used for Analysis:
head(nyc_tkt_2017)
dim(nyc_tkt_2017)

#2017 Dataset has Rows: 10,539,563 | Columns: 45
printSchema(tkt_2017_nycparking)

#All attributes are in standardized formats


#************************ Overview and Examining the dataset ************************#
#Creating temp views of each table

createOrReplaceTempView(nyc_tkt_2015, "tkt_2015_nyc")
createOrReplaceTempView(nyc_tkt_2016, "tkt_2016_nyc")
createOrReplaceTempView(nyc_tkt_2017, "tkt_2017_nyc")

#************ Q1 ************#
# Find total number of tickets for each year!

Number_of_Tickets<- c(nrow(nyc_tkt_2015), nrow(nyc_tkt_2016), nrow(nyc_tkt_2017))
Year_Labels<- c("FY_2015", "FY_2016", "FY_2017")
tickets_vs_year<- data.frame(Number_of_Tickets, Year_Labels)
tickets_vs_year
ggplot(tickets_vs_year, aes(x=Year_Labels, y=Number_of_Tickets))+ geom_col() + xlab("Fiscal Year") + ylab("Number of Tickets") + ggtitle("Plot1. Fiscal Year vs. Number of Tickets") + geom_text(aes(label=Number_of_Tickets),vjust=-0.3)

#************  Q2 ************#
# Find out how many unique states the cars which got parking tickets came from!

# Registration State Distribution for 2015
reg_st_2015<- SparkR::sql("SELECT Registration_State, count(*)as Number_of_Tickets 
                          from tkt_2015_nyc 
                          group by Registration_State
                          order by Number_of_Tickets desc")
head((reg_st_2015),nrow(reg_st_2015))

#Registration State Distribution for 2016

reg_st_2016<- SparkR::sql("SELECT Registration_State, count(*)as Number_of_Tickets 
                          from tkt_2016_nyc 
                          group by Registration_State
                          order by Number_of_Tickets desc")
head((reg_st_2016),nrow(reg_st_2016))

#Registration State Distribution for 2017

reg_st_2017<- SparkR::sql("SELECT Registration_State, count(*)as Number_of_Tickets 
                          from tkt_2017_nyc 
                          group by Registration_State
                          order by Number_of_Tickets desc")
head((reg_st_2017),nrow(reg_st_2017))

#Comparison Unique Registration States of cars issued with a Parking Ticket Vs FY 2015, 2016 and 2017

Uniq_Reg_State_Count<- c(nrow(reg_st_2015), nrow(reg_st_2016), nrow(reg_st_2017))
Year_Labels<- c("FY_2015", "FY_2016", "FY_2017")
uniqregstate_vs_year<- data.frame(Uniq_Reg_State_Count, Year_Labels)
head(uniqregstate_vs_year)

#Let's see the results on a graph.

ggplot(uniqregstate_vs_year, aes(x=Year_Labels, y=Uniq_Reg_State_Count))+ geom_col() + xlab("Fiscal Year") + ylab("Number of Unique Registration States") + ggtitle("Plot2. Fiscal Year vs. Number of Unique Registration States") + geom_text(aes(label=Uniq_Reg_State_Count),vjust=-0.3)


#************  Q3 ************#
# Some parking tickets don't have addresses on them, which is cause for concern. Find out how many such tickets there are!

# Missing Address in 2015
Missing_Address_2015 <- SparkR::sql("SELECT count(*) as Total_Num_Records, 
                                    SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL 
                                    THEN 1 
                                    ELSE 0 END)as Num_Tickets_2015_with_MissingAddress, 
                                    100*SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL
                                    THEN 1 
                                    ELSE 0 
                                    END)/count(*) as Percent_Tickets_2015_with_MissingAddress
                                    from tkt_2015_nyc")
head(Missing_Address_2015)

msingadd_2015<- data.frame(head(Missing_Address_2015))
msingadd_2015$Fiscal_Year<- 2015
colnames(msingadd_2015)<- c("Total_Num_Records", "Count_Tickets_with_MissingAddress", "Percent_Tickets_with_MissingAddress", "Fiscal_Year")


#Missing Address in 2016
Missing_Address_2016 <- SparkR::sql("SELECT count(*) as Total_Num_Records, 
                                    SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL 
                                    THEN 1 
                                    ELSE 0 END)as Num_Tickets_2016_with_MissingAddress, 
                                    100*SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL
                                    THEN 1 
                                    ELSE 0 
                                    END)/count(*) as Percent_Tickets_2016_with_MissingAddress
                                    from tkt_2016_nyc")
head(Missing_Address_2016)
msingadd_2016<- data.frame(head(Missing_Address_2016))
msingadd_2016$Fiscal_Year<- 2016
colnames(msingadd_2016)<- c("Total_Num_Records", "Count_Tickets_with_MissingAddress", "Percent_Tickets_with_MissingAddress", "Fiscal_Year")


#Missing Address in 2017
Missing_Address_2017 <- SparkR::sql("SELECT count(*) as Total_Num_Records, 
                                    SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL 
                                    THEN 1 
                                    ELSE 0 END)as Num_Tickets_2017_with_MissingAddress, 
                                    100*SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL
                                    THEN 1 
                                    ELSE 0 
                                    END)/count(*) as Percent_Tickets_2017_with_MissingAddress
                                    from tkt_2017_nyc")
head(Missing_Address_2017)
msingadd_2017<- data.frame(head(Missing_Address_2017))
msingadd_2017$Fiscal_Year<- 2017
colnames(msingadd_2017)<- c("Total_Num_Records", "Count_Tickets_with_MissingAddress", "Percent_Tickets_with_MissingAddress", "Fiscal_Year")


#Comparison of Missing Addresses
msingadd_combined<- rbind(msingadd_2015, msingadd_2016, msingadd_2017)
msingadd_combined
ggplot(msingadd_combined, aes(x=Fiscal_Year, y=Count_Tickets_with_MissingAddress))+ geom_col() + xlab("Fiscal Year") + ylab("Number of tickets with Missing Address") + ggtitle("Plot3. Fiscal Year vs. Number of Missing Address Tickets") + geom_text(aes(label=Count_Tickets_with_MissingAddress),vjust=-0.3)

#Percentage of Tickets with Missing Addresses Comparison

ggplot(msingadd_combined, aes(x=Fiscal_Year, y=Percent_Tickets_with_MissingAddress))+ geom_col() + xlab("Fiscal Year") + ylab("Percentage of tickets with Missing Address") + ggtitle("Plot3B. Fiscal Year vs. Percentage of Missing Address Tickets") + geom_text(aes(label=Percent_Tickets_with_MissingAddress),vjust=-0.3)

#************************ Stage 3: Deriving and Comparing Metrics through Aggregation Tasks. ************************#

#************ Q1 ************

# How often does each violation code occur? (frequency of violation codes - find the top 5)!

#Top Violation Codes for 2015
#2015

violationcd_frequency_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from tkt_2015_nyc 
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2015,5)

viocd_2015_top5<- data.frame(head(violationcd_frequency_2015,5))

ggplot(viocd_2015_top5, aes(x=as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col() + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot4A. 2015 Top 5 Violation Code vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

# Top Violation Codes for 2016

violationcd_frequency_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from tkt_2016_nyc 
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2016,5)

viocd_2016_top5<- data.frame(head(violationcd_frequency_2016,5))

ggplot(viocd_2016_top5, aes(x=as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col() + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot4B. 2016 Top 5 Violation Code vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


#Top Violation Codes for 2017

violationcd_frequency_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from tkt_2017_nyc 
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2017,5)

viocd_2017_top5<- data.frame(head(violationcd_frequency_2017,5))

ggplot(viocd_2017_top5, aes(x=as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col() + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot4C. 2017 Top 5 Violation Code vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

# Combined Comparison of Top-5 Violation Codes
viocd_combined<- rbind(viocd_2015_top5, viocd_2016_top5, viocd_2017_top5)
viocd_combined$Year_Labels<- c("2015","2015","2015","2015", "2015","2016","2016","2016","2016", "2016","2017","2017","2017","2017", "2017")
ggplot(viocd_combined, aes(x=as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Year_Labels) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot4. Comparison of Top 5 Violation Code vs Frequency of Ticket between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#************ Stage 3: Q2 ************#

#How often does each vehicle body type get a parking ticket? How about the vehicle make? (find the top 5 for both)

#Top Vehicle Body Type for 2015

vehbdty_frequency_2015<- SparkR::sql("SELECT Vehicle_Body_Type, count(*)as Frequency_of_Tickets
                                     from tkt_2015_nyc 
                                     group by Vehicle_Body_Type
                                     order by Frequency_of_Tickets desc")
head(vehbdty_frequency_2015,5)

vehbdty_2015_top5<- data.frame(head(vehbdty_frequency_2015,5))

ggplot(vehbdty_2015_top5, aes(x=as.factor(Vehicle_Body_Type), y=Frequency_of_Tickets))+ geom_col() + xlab("Vehicle Body Type") + ylab("Frequency of Tickets") + ggtitle("Plot5A. 2015 Top 5 Vehicle Body Type vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#Top Vehicle Body Type for 2016

vehbdty_frequency_2016<- SparkR::sql("SELECT Vehicle_Body_Type, count(*)as Frequency_of_Tickets
                                     from tkt_2016_nyc 
                                     group by Vehicle_Body_Type
                                     order by Frequency_of_Tickets desc")
head(vehbdty_frequency_2016,5)

vehbdty_2016_top5<- data.frame(head(vehbdty_frequency_2016,5))

ggplot(vehbdty_2016_top5, aes(x=as.factor(Vehicle_Body_Type), y=Frequency_of_Tickets))+ geom_col() + xlab("Vehicle Body Type") + ylab("Frequency of Tickets") + ggtitle("Plot5B. 2016 Top 5 Vehicle Body Type vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#Top Vehicle Body Type for 2017
vehbdty_frequency_2017<- SparkR::sql("SELECT Vehicle_Body_Type, count(*)as Frequency_of_Tickets
                                     from tkt_2017_nyc 
                                     group by Vehicle_Body_Type
                                     order by Frequency_of_Tickets desc")
head(vehbdty_frequency_2017,5)

vehbdty_2017_top5<- data.frame(head(vehbdty_frequency_2017,5))

ggplot(vehbdty_2017_top5, aes(x=as.factor(Vehicle_Body_Type), y=Frequency_of_Tickets))+ geom_col() + xlab("Vehicle Body Type") + ylab("Frequency of Tickets") + ggtitle("Plot5C. 2017 Top 5 Vehicle Body Type vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#Combined Comparison for Top-5 Body types
vehbdty_combined<- rbind(vehbdty_2015_top5, vehbdty_2016_top5, vehbdty_2017_top5)
vehbdty_combined$Year_Labels<- c("2015","2015","2015","2015", "2015","2016","2016","2016","2016", "2016","2017","2017","2017","2017", "2017")
ggplot(vehbdty_combined, aes(x=as.factor(Vehicle_Body_Type), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Year_Labels) + xlab("Vehicle Body Type") + ylab("Frequency of Tickets") + ggtitle("Plot5. Comparison of Top 5 Violation Code vs Frequency of Ticket between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

####Analysis based on Vehicle Make ######


#3.Top Vehicle Make for 2015
vehmake_frequency_2015<- SparkR::sql("SELECT Vehicle_Make, count(*)as Frequency_of_Tickets
                                     from tkt_2015_nyc 
                                     group by Vehicle_Make
                                     order by Frequency_of_Tickets desc")
head(vehmake_frequency_2015, 5)

vehmake_2015_top5<- data.frame(head(vehmake_frequency_2015, 5))

ggplot(vehmake_2015_top5, aes(x=as.factor(Vehicle_Make), y=Frequency_of_Tickets))+ geom_col() + xlab("Vehicle Make") + ylab("Frequency of Tickets") + ggtitle("Plot6A. 2015 Top 5 Vehicle Make vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#3.Top Vehicle Make for 2016

vehmake_frequency_2016<- SparkR::sql("SELECT Vehicle_Make, count(*)as Frequency_of_Tickets
                                     from tkt_2016_nyc 
                                     group by Vehicle_Make
                                     order by Frequency_of_Tickets desc")
head(vehmake_frequency_2016, 5)

vehmake_2016_top5<- data.frame(head(vehmake_frequency_2016, 5))

ggplot(vehmake_2016_top5, aes(x=as.factor(Vehicle_Make), y=Frequency_of_Tickets))+ geom_col() + xlab("Vehicle Make") + ylab("Frequency of Tickets") + ggtitle("Plot6B. 2016 Top 5 Vehicle Make vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#Top Vehicle Make for 2017

vehmake_frequency_2017<- SparkR::sql("SELECT Vehicle_Make, count(*)as Frequency_of_Tickets
                                     from tkt_2017_nyc 
                                     group by Vehicle_Make
                                     order by Frequency_of_Tickets desc")
head(vehmake_frequency_2017, 5)

vehmake_2017_top5<- data.frame(head(vehmake_frequency_2017, 5))

ggplot(vehmake_2017_top5, aes(x=as.factor(Vehicle_Make), y=Frequency_of_Tickets))+ geom_col() + xlab("Vehicle Make") + ylab("Frequency of Tickets") + ggtitle("Plot6C. 2017 Top 5 Vehicle Make vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#Combined Comparison for Top-5 Body Makes

vehmake_combined<- rbind(vehmake_2015_top5, vehmake_2016_top5, vehmake_2017_top5)
vehmake_combined$Year_Labels<- c("2015","2015","2015","2015", "2015","2016","2016","2016","2016", "2016","2017","2017","2017","2017", "2017")
ggplot(vehmake_combined, aes(x=as.factor(Vehicle_Make), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Year_Labels) + xlab("Vehicle Make") + ylab("Frequency of Tickets") + ggtitle("Plot6. Comparison of Top 5 Vehicle Make vs Frequency of Ticket between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#************ Stage 3: Q3 ************#


#Violation Precinct vs Frequency of Tickets
# Top Violation Precinct for 2015

vioprect_frequency_2015<- SparkR::sql("SELECT Violation_Precinct, count(*)as Frequency_of_Tickets
                                      from tkt_2015_nyc 
                                      group by Violation_Precinct
                                      order by Frequency_of_Tickets desc")
head(vioprect_frequency_2015,5)

vioprect_2015_top5<- data.frame(head(vioprect_frequency_2015,5))

ggplot(vioprect_2015_top5, aes(x=as.factor(Violation_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Violation Precinct") + ylab("Frequency of Tickets") + ggtitle("Plot7A. 2015 Top 5 Violation Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

# Top Violation Precinct for 2016

vioprect_frequency_2016<- SparkR::sql("SELECT Violation_Precinct, count(*)as Frequency_of_Tickets
                                      from tkt_2016_nyc 
                                      group by Violation_Precinct
                                      order by Frequency_of_Tickets desc")
head(vioprect_frequency_2016,5)

vioprect_2016_top5<- data.frame(head(vioprect_frequency_2016,5))

ggplot(vioprect_2016_top5, aes(x=as.factor(Violation_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Violation Precinct") + ylab("Frequency of Tickets") + ggtitle("Plot7B. 2016 Top 5 Violation Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#Top Violation Precinct for 2017

vioprect_frequency_2017<- SparkR::sql("SELECT Violation_Precinct, count(*)as Frequency_of_Tickets
                                      from tkt_2017_nyc 
                                      group by Violation_Precinct
                                      order by Frequency_of_Tickets desc")
head(vioprect_frequency_2017,5)

vioprect_2017_top5<- data.frame(head(vioprect_frequency_2017,5))

ggplot(vioprect_2017_top5, aes(x=as.factor(Violation_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Violation Precinct") + ylab("Frequency of Tickets") + ggtitle("Plot7C. 2017 Top 5 Violation Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#Combined Comparison for Violation Precinct

vioprect_combined<- rbind(vioprect_2015_top5,vioprect_2016_top5,vioprect_2017_top5)
vioprect_combined$Year_Labels<- c("2015","2015","2015","2015", "2015","2016","2016","2016","2016", "2016","2017","2017","2017","2017", "2017")
ggplot(vioprect_combined, aes(x=as.factor(Violation_Precinct), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Year_Labels) + xlab("Violation Precinct") + ylab("Frequency of Tickets") + ggtitle("Plot7. Comparison of Top 5 Violation Precinct vs Frequency of Ticket between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


# Issuer Precinct vs Frequency of Tickets
# Top Issuer Precinct for 2015

isuprect_frequency_2015<- SparkR::sql("SELECT Issuer_Precinct, count(*)as Frequency_of_Tickets
                                      from tkt_2015_nyc 
                                      group by Issuer_Precinct
                                      order by Frequency_of_Tickets desc")
head(isuprect_frequency_2015,5)

isuprect_2015_top5<- data.frame(head(isuprect_frequency_2015,5))

ggplot(isuprect_2015_top5, aes(x=as.factor(Issuer_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Issuer Precinct") + ylab("Frequency of Tickets") + ggtitle("Plot8A. 2015 Top 5 Issuer Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


#Top Issuer Precinct for 2016

isuprect_frequency_2016<- SparkR::sql("SELECT Issuer_Precinct, count(*)as Frequency_of_Tickets
                                      from tkt_2016_nyc 
                                      group by Issuer_Precinct
                                      order by Frequency_of_Tickets desc")
head(isuprect_frequency_2016,5)

isuprect_2016_top5<- data.frame(head(isuprect_frequency_2016,5))

ggplot(isuprect_2016_top5, aes(x=as.factor(Issuer_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Issuer Precinct") + ylab("Frequency of Tickets") + ggtitle("Plot8B. 2016 Top 5 Issuer Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#Top Issuer Precinct for 2017

isuprect_frequency_2017<- SparkR::sql("SELECT Issuer_Precinct, count(*)as Frequency_of_Tickets
                                      from tkt_2017_nyc 
                                      group by Issuer_Precinct
                                      order by Frequency_of_Tickets desc")
head(isuprect_frequency_2017,5)

isuprect_2017_top5<- data.frame(head(isuprect_frequency_2017,5))

ggplot(isuprect_2017_top5, aes(x=as.factor(Issuer_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Issuer Precinct") + ylab("Frequency of Tickets") + ggtitle("Plot8C. 2017 Top 5 Issuer Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#Combined Comparison for Issuer Precinct

isuprect_combined<- rbind(isuprect_2015_top5, isuprect_2016_top5, isuprect_2017_top5)
isuprect_combined$Year_Labels<- c("2015","2015","2015","2015", "2015","2016","2016","2016","2016", "2016","2017","2017","2017","2017", "2017")
ggplot(isuprect_combined, aes(x=as.factor(Issuer_Precinct), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Year_Labels) + xlab("Issuer Precinct") + ylab("Frequency of Tickets") + ggtitle("Plot8. Comparison of Top 5 Issuer Precinct vs Frequency of Ticket between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#************ Stage 3: Q4 ************#
#Find the violation code frequency across 3 precincts which have issued the most number of tickets - do these precinct zones have an exceptionally high frequency of certain violation codes? Are these codes common across precincts?

#In Year 2015 [Top Three Issuer Precinct's : 0, 19 and 18]

#Violation Code Distribution in Issuer Precinct 0

one_isuprect_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                from tkt_2015_nyc 
                                where Issuer_Precinct = 0
                                group by Violation_Code, Issuer_Precinct
                                order by Frequency_of_Tickets desc")
head(one_isuprect_2015, 5)

one_isuprect_top5_2015<- data.frame(head(one_isuprect_2015, 5))

#Violation Code Distribution in Issuer Precinct 19
two_isuprect_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                from tkt_2015_nyc 
                                where Issuer_Precinct = 19
                                group by Violation_Code, Issuer_Precinct
                                order by Frequency_of_Tickets desc")
head(two_isuprect_2015, 5)

two_isuprect_top5_2015<- data.frame(head(two_isuprect_2015, 5))

#Violation Code Distribution in Issuer Precinct 18

three_isuprect_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                  from tkt_2015_nyc 
                                  where Issuer_Precinct = 18
                                  group by Violation_Code, Issuer_Precinct
                                  order by Frequency_of_Tickets desc")
head(three_isuprect_2015,5)

three_isuprect_top5_2015<- data.frame(head(three_isuprect_2015,5))

#Violation Code Distribution in Other Issuer Precincts

other_isuprect_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                  from tkt_2015_nyc 
                                  where Issuer_Precinct NOT IN (0,19,18)
                                  group by Violation_Code
                                  order by Frequency_of_Tickets desc")
head(other_isuprect_2015,5)

other_isuprect_top5_2015<- data.frame(head(other_isuprect_2015,5))
other_isuprect_top5_2015$Issuer_Precinct<- c("Other","Other","Other","Other","Other")

#Combined Violation Code Distribution vs Issuer Precincts in 2015

vioisuprect_2015_combined<- rbind(one_isuprect_top5_2015, two_isuprect_top5_2015, three_isuprect_top5_2015, other_isuprect_top5_2015)

ggplot(vioisuprect_2015_combined, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Issuer_Precinct) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot9A. 2015 Comparison of Violation Code Distribution vs. Top Issuer Precinct") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#In Year 2016 [Top Three Issuer Precinct's : 0, 19 and 18]

#Violation Code Distribution in Issuer Precinct 0

one_isuprect_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                from tkt_2016_nyc 
                                where Issuer_Precinct = 0
                                group by Violation_Code, Issuer_Precinct
                                order by Frequency_of_Tickets desc")
head(one_isuprect_2016, 5)

one_isuprect_top5_2016<- data.frame(head(one_isuprect_2016, 5))

#Violation Code Distribution in Issuer Precinct 19

two_isuprect_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                from tkt_2016_nyc 
                                where Issuer_Precinct = 19
                                group by Violation_Code, Issuer_Precinct
                                order by Frequency_of_Tickets desc")
head(two_isuprect_2016, 5)

two_isuprect_top5_2016<- data.frame(head(two_isuprect_2016, 5))

#Violation Code Distribution in Issuer Precinct 18

three_isuprect_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                  from tkt_2016_nyc 
                                  where Issuer_Precinct = 18
                                  group by Violation_Code, Issuer_Precinct
                                  order by Frequency_of_Tickets desc")
head(three_isuprect_2016,5)

three_isuprect_top5_2016<- data.frame(head(three_isuprect_2016,5))

#Violation Code Distribution in Other Issuer Precincts

other_isuprect_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                  from tkt_2016_nyc 
                                  where Issuer_Precinct NOT IN (0,19,18)
                                  group by Violation_Code
                                  order by Frequency_of_Tickets desc")
head(other_isuprect_2016,5)

other_isuprect_top5_2016<- data.frame(head(other_isuprect_2016,5))
other_isuprect_top5_2016$Issuer_Precinct<- c("Other","Other","Other","Other","Other")

#Combined Violation Code Distribution vs Issuer Precincts in 2016

vioisuprect_2016_combined<- rbind(one_isuprect_top5_2016, two_isuprect_top5_2016, three_isuprect_top5_2016, other_isuprect_top5_2016)

ggplot(vioisuprect_2016_combined, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Issuer_Precinct) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot9B. 2016 Comparison of Violation Code Distribution vs. Top Issuer Precinct") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


#In Year 2017 [Top Three Issuer Precinct's : 0, 19 and 14]

#Violation Code Distribution in Issuer Precinct 0
one_isuprect_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                from tkt_2017_nyc 
                                where Issuer_Precinct = 0
                                group by Violation_Code, Issuer_Precinct
                                order by Frequency_of_Tickets desc")
head(one_isuprect_2017, 5)

one_isuprect_top5_2017<- data.frame(head(one_isuprect_2017, 5))

#Violation Code Distribution in Issuer Precinct 19

two_isuprect_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                from tkt_2017_nyc 
                                where Issuer_Precinct = 19
                                group by Violation_Code, Issuer_Precinct
                                order by Frequency_of_Tickets desc")
head(two_isuprect_2017, 5)

two_isuprect_top5_2017<- data.frame(head(two_isuprect_2017, 5))

#Violation Code Distribution in Issuer Precinct 14
three_isuprect_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                  from tkt_2017_nyc 
                                  where Issuer_Precinct = 14
                                  group by Violation_Code, Issuer_Precinct
                                  order by Frequency_of_Tickets desc")
head(three_isuprect_2017,5)

three_isuprect_top5_2017<- data.frame(head(three_isuprect_2017,5))

#Violation Code Distribution in Other Issuer Precincts
other_isuprect_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                  from tkt_2017_nyc 
                                  where Issuer_Precinct NOT IN (0,19,14)
                                  group by Violation_Code
                                  order by Frequency_of_Tickets desc")
head(other_isuprect_2017,5)

other_isuprect_top5_2017<- data.frame(head(other_isuprect_2017,5))
other_isuprect_top5_2017$Issuer_Precinct<- c("Other","Other","Other","Other","Other")

#Combined Violation Code Distribution vs Issuer Precincts in 2017

vioisuprect_2017_combined<- rbind(one_isuprect_top5_2017, two_isuprect_top5_2017, three_isuprect_top5_2017, other_isuprect_top5_2017)

ggplot(vioisuprect_2017_combined, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Issuer_Precinct) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot9C. 2017 Comparison of Violation Code Distribution vs. Top Issuer Precinct") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#************ Stage 3: Q5 ************#

#You'd want to find out the properties of parking violations across different times of the day:
#Already Converted the Violation Time into a standardized timestamp format. 


#******************** Part 1: Violation Bin Group vs Violation Code [Top-3] ********************#

#2015 Dataset: Violation Time Bin vs. Violation Code Analysis
#Find a way to deal with missing values, if any.

null_violat_times_2015<- SparkR::sql("SELECT count(*)as Total_Num_of_Rows, 
                                     SUM(CASE WHEN Violation_Time is NULL
                                     THEN 1 ELSE 0 END)as Nulls_Violation_Time,
                                     100*SUM(CASE WHEN Violation_Time IS NULL
                                     THEN 1 ELSE 0 END)/count(*) as Percent_Tickets_2015_ViolationTimeMissing
                                     from tkt_2016_nyc")
head(null_violat_times_2015)

#2015 dataset 0.5812% records with Missing Violation Time is Negligable and will therefore be removed before analysis.

adjusted_tkt_2015_nycparking<- subset(nyc_tkt_2015, isNotNull(nyc_tkt_2015$Violation_Time))
adjusted_tkt_2015_nycparking$Violation_Hour <- hour(cast(adjusted_tkt_2016_nycparking$Violation_Time,dataType = "string"))
createOrReplaceTempView(adjusted_tkt_2015_nycparking, "violt_2015")


#Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 
violation_hour_bin_2015 <- SparkR::sql("SELECT Violation_Hour,
                                       Violation_Code,
                                       CASE WHEN Violation_Hour BETWEEN 0 AND 3
                                       THEN '0_3'
                                       WHEN Violation_Hour BETWEEN 4 AND 7
                                       THEN '4_7'
                                       WHEN Violation_Hour BETWEEN 8 AND 11
                                       THEN '8_11'
                                       WHEN Violation_Hour BETWEEN 12 AND 15
                                       THEN '12_15' 
                                       WHEN Violation_Hour BETWEEN 16 AND 19
                                       THEN '16_19' 
                                       WHEN Violation_Hour BETWEEN 20 AND 23
                                       THEN '20_23' 
                                       END AS Violation_Hour_Bin
                                       FROM violt_2015")

createOrReplaceTempView(violation_hour_bin_2015, "violt_hour_2015_nyc")

hour_bin_tkts_2015 <- SparkR::sql("SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets,
                                  dense_rank() over (partition by Violation_Hour_Bin order by Frequency_of_Tickets desc) Rnk
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  count(*)as Frequency_of_Tickets
                                  FROM violt_hour_2015_nyc
                                  GROUP BY Violation_Hour_Bin,
                                  Violation_Code))
                                  WHERE Rnk <= 3")

df_hour_bin_tkts_2015 <- data.frame(head(hour_bin_tkts_2015, nrow(hour_bin_tkts_2015)))

ggplot(df_hour_bin_tkts_2015, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Violation_Hour_Bin) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot13A. 2015 Comparison of Violation Code Distribution vs. Violation_Hour_Bin") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#2016 Dataset: Violation Time Bin vs. Violation Code Analysis

#Find a way to deal with missing values, if any.
null_violat_times_2016<- SparkR::sql("SELECT count(*)as Total_Num_of_Rows, 
                                     SUM(CASE WHEN Violation_Time is NULL
                                     THEN 1 ELSE 0 END)as Nulls_Violation_Time,
                                     100*SUM(CASE WHEN Violation_Time IS NULL
                                     THEN 1 ELSE 0 END)/count(*) as Percent_Tickets_2016_ViolationTimeMissing
                                     from tkt_2016_nyc")
head(null_violat_times_2016)
#2016 dataset 0.6114% records with Missing Violation Time is Negligable and will therefore be removed before analysis.

adjusted_tkt_2016_nycparking<- subset(nyc_tkt_2016, isNotNull(nyc_tkt_2016$Violation_Time))
adjusted_tkt_2016_nycparking$Violation_Hour <- hour(cast(adjusted_tkt_2016_nycparking$Violation_Time,dataType = "string"))
createOrReplaceTempView(adjusted_tkt_2016_nycparking, "violt_2016")


#Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 
violation_hour_bin_2016 <- SparkR::sql("SELECT Violation_Hour,
                                       Violation_Code,
                                       CASE WHEN Violation_Hour BETWEEN 0 AND 3
                                       THEN '0_3'
                                       WHEN Violation_Hour BETWEEN 4 AND 7
                                       THEN '4_7'
                                       WHEN Violation_Hour BETWEEN 8 AND 11
                                       THEN '8_11'
                                       WHEN Violation_Hour BETWEEN 12 AND 15
                                       THEN '12_15' 
                                       WHEN Violation_Hour BETWEEN 16 AND 19
                                       THEN '16_19' 
                                       WHEN Violation_Hour BETWEEN 20 AND 23
                                       THEN '20_23' 
                                       END AS Violation_Hour_Bin
                                       FROM violt_2016")

createOrReplaceTempView(violation_hour_bin_2016, "violt_hour_2016_nyc")

hour_bin_tkts_2016 <- SparkR::sql("SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets,
                                  dense_rank() over (partition by Violation_Hour_Bin order by Frequency_of_Tickets desc) Rnk
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  count(*)as Frequency_of_Tickets
                                  FROM violt_hour_2016_nyc
                                  GROUP BY Violation_Hour_Bin,
                                  Violation_Code))
                                  WHERE Rnk <= 3")

df_hour_bin_tkts_2016 <- data.frame(head(hour_bin_tkts_2016, nrow(hour_bin_tkts_2016)))

ggplot(df_hour_bin_tkts_2016, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Violation_Hour_Bin) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot13B. 2016 Comparison of Violation Code Distribution vs. Violation_Hour_Bin") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#2017 Dataset: Violation Time Bin vs. Violation Code Analysis

#Find a way to deal with missing values, if any.
null_violat_times_2017<- SparkR::sql("SELECT count(*)as Total_Num_of_Rows, 
                                     SUM(CASE WHEN Violation_Time is NULL
                                     THEN 1 ELSE 0 END)as Nulls_Violation_Time,
                                     100*SUM(CASE WHEN Violation_Time IS NULL
                                     THEN 1 ELSE 0 END)/count(*) as Percent_Tickets_2017_ViolationTimeMissing
                                     from tkt_2017_nyc")
head(null_violat_times_2017)
#2017 dataset 0.6114% records with Missing Violation Time is Negligable and will therefore be removed before analysis.

adjusted_tkt_2017_nycparking<- subset(nyc_tkt_2017, isNotNull(nyc_tkt_2017$Violation_Time))
adjusted_tkt_2017_nycparking$Violation_Hour <- hour(cast(adjusted_tkt_2017_nycparking$Violation_Time,dataType = "string"))
createOrReplaceTempView(adjusted_tkt_2017_nycparking, "violt_2017")


#Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 
violation_hour_bin_2017 <- SparkR::sql("SELECT Violation_Hour,
                                       Violation_Code,
                                       CASE WHEN Violation_Hour BETWEEN 0 AND 3
                                       THEN '0_3'
                                       WHEN Violation_Hour BETWEEN 4 AND 7
                                       THEN '4_7'
                                       WHEN Violation_Hour BETWEEN 8 AND 11
                                       THEN '8_11'
                                       WHEN Violation_Hour BETWEEN 12 AND 15
                                       THEN '12_15' 
                                       WHEN Violation_Hour BETWEEN 16 AND 19
                                       THEN '16_19' 
                                       WHEN Violation_Hour BETWEEN 20 AND 23
                                       THEN '20_23' 
                                       END AS Violation_Hour_Bin
                                       FROM violt_2017")

createOrReplaceTempView(violation_hour_bin_2017, "violt_hour_2017_nyc")

hour_bin_tkts_2017 <- SparkR::sql("SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  Frequency_of_Tickets,
                                  dense_rank() over (partition by Violation_Hour_Bin order by Frequency_of_Tickets desc) Rnk
                                  FROM (SELECT Violation_Hour_Bin,
                                  Violation_Code,
                                  count(*)as Frequency_of_Tickets
                                  FROM violt_hour_2017_nyc
                                  GROUP BY Violation_Hour_Bin,
                                  Violation_Code))
                                  WHERE Rnk <= 3")

df_hour_bin_tkts_2017 <- data.frame(head(hour_bin_tkts_2017, nrow(hour_bin_tkts_2017)))

ggplot(df_hour_bin_tkts_2017, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Violation_Hour_Bin) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot13C. 2017 Comparison of Violation Code Distribution vs. Violation_Hour_Bin") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#*******************************************************************************************************#

#Now, try another direction. For the 3 most commonly occurring violation codes, find the most common times of day (in terms of the bins from the previous part)

#******************** Part 2: Violation Code [Top-3] vs. Violation Time Bin Distribution ********************#
#2015 Dataset: Top-3 Violation Code vs. Violation Time Bin Analysis

top_3_violations_2015 <- SparkR::sql("SELECT Violation_Code,
                                     count(*) no_of_tickets
                                     FROM violt_hour_2015_nyc
                                     GROUP BY Violation_Code
                                     ORDER BY no_of_tickets desc")

head(top_3_violations_2015,3)
#Top-3 Violation Code for 2015 are 21, 38 and 14

common_times_2015 <- SparkR::sql("SELECT Violation_Code,
                                 Violation_Hour_Bin,
                                 count(*) no_of_tickets
                                 FROM violt_hour_2015_nyc
                                 WHERE violation_code IN (21,38,14)
                                 GROUP BY Violation_Code, 
                                 Violation_Hour_Bin
                                 ORDER BY Violation_Code, 
                                 Violation_Hour_Bin,
                                 no_of_tickets desc")	

df_common_times_viol_2015 <- data.frame(head(common_times_2015, nrow(common_times_2015)))

ggplot(df_common_times_viol_2015, aes(x= as.factor(Violation_Hour_Bin), y=no_of_tickets))+ geom_col()+ facet_grid(~Violation_Code) + xlab("Violation Hour Bin") + ylab("Frequency of Tickets") + ggtitle("Plot14A. 2015 Comparison of Violation_Hour_Bin vs. No_of_tickets") + geom_text(aes(label=no_of_tickets),vjust=-0.3)

#2016 Dataset: Top-3 Violation Code vs. Violation Time Bin Analysis
top_3_violations_2016 <- SparkR::sql("SELECT Violation_Code,
                                     count(*) no_of_tickets
                                     FROM violt_hour_2016_nyc
                                     GROUP BY Violation_Code
                                     ORDER BY no_of_tickets desc")

head(top_3_violations_2016,3)
#Top-3 Violation Codes for 2016 are 21, 36 and 38.

common_times_2016 <- SparkR::sql("SELECT Violation_Code,
                                 Violation_Hour_Bin,
                                 count(*) no_of_tickets
                                 FROM violt_hour_2016_nyc
                                 WHERE violation_code IN (21,36,38)
                                 GROUP BY Violation_Code, 
                                 Violation_Hour_Bin
                                 ORDER BY Violation_Code, 
                                 Violation_Hour_Bin,
                                 no_of_tickets desc")	

df_common_times_viol_2016 <- data.frame(head(common_times_2016, nrow(common_times_2016)))

ggplot(df_common_times_viol_2016, aes(x= as.factor(Violation_Hour_Bin), y=no_of_tickets))+ geom_col()+ facet_grid(~Violation_Code) + xlab("Violation Hour Bin") + ylab("Frequency of Tickets") + ggtitle("Plot14B. 2016 Comparison of Violation_Hour_Bin vs. No_of_tickets") + geom_text(aes(label=no_of_tickets),vjust=-0.3)

#2017 Dataset: Top-3 Violation Code vs. Violation Time Bin Analysis
top_3_violations_2017 <- SparkR::sql("SELECT Violation_Code,
                                     count(*) no_of_tickets
                                     FROM violt_hour_2017_nyc
                                     GROUP BY Violation_Code
                                     ORDER BY no_of_tickets desc")

head(top_3_violations_2017,3)
#Top-3 Violation Codes for 2017 are 21, 36 and 38.

common_times_2017 <- SparkR::sql("SELECT Violation_Code,
                                 Violation_Hour_Bin,
                                 count(*) no_of_tickets
                                 FROM violt_hour_2017_nyc
                                 WHERE violation_code IN (21,36,38)
                                 GROUP BY Violation_Code, 
                                 Violation_Hour_Bin
                                 ORDER BY Violation_Code, 
                                 Violation_Hour_Bin,
                                 no_of_tickets desc")	

df_common_times_viol_2017 <- data.frame(head(common_times_2017, nrow(common_times_2017)))

ggplot(df_common_times_viol_2017, aes(x= as.factor(Violation_Hour_Bin), y=no_of_tickets))+ geom_col()+ facet_grid(~Violation_Code) + xlab("Violation Hour Bin") + ylab("Frequency of Tickets") + ggtitle("Plot14C. 2017 Comparison of Violation_Hour_Bin vs. No_of_tickets") + geom_text(aes(label=no_of_tickets),vjust=-0.3)

#************ Stage 3: Q6 ************#
# Checking for seasonality in dataset

#Season vs. Frequency Analysis

#2015 Season vs. Frequency Analysis
Season_Binning_2015 <- SparkR::sql("SELECT Summons_Number,
                                   Violation_Code,
                                   CASE WHEN Issue_Month IN (1,2,12)
                                   THEN 'Winter'
                                   WHEN Issue_Month BETWEEN 3 AND 5
                                   THEN 'Spring'
                                   WHEN Issue_Month BETWEEN 6 AND 8
                                   THEN 'Summer'
                                   WHEN Issue_Month BETWEEN 9 AND 12
                                   THEN 'Fall' 
                                   END AS Season
                                   FROM tkt_2015_nyc")
createOrReplaceTempView(Season_Binning_2015, "season_tkt_2015_nyc")

tktseason_2015<- SparkR::sql("SELECT Season,
                             Count(*)as Frequency_of_Tickets
                             FROM season_tkt_2015_nyc
                             GROUP BY Season
                             ORDER BY Frequency_of_Tickets desc")
head(tktseason_2015)

freq_tktseason_2015<- data.frame(head(tktseason_2015))
freq_tktseason_2015$Fiscal_Year<- c(2015,2015,2015,2015)
freq_tktseason_2015

#2016 Season vs. Frequency Analysis
Season_Binning_2016 <- SparkR::sql("SELECT Summons_Number,
                                   Violation_Code,
                                   CASE WHEN Issue_Month IN (1,2,12)
                                   THEN 'Winter'
                                   WHEN Issue_Month BETWEEN 3 AND 5
                                   THEN 'Spring'
                                   WHEN Issue_Month BETWEEN 6 AND 8
                                   THEN 'Summer'
                                   WHEN Issue_Month BETWEEN 9 AND 12
                                   THEN 'Fall' 
                                   END AS Season
                                   FROM tkt_2016_nyc")
createOrReplaceTempView(Season_Binning_2016, "season_tkt_2016_nyc")

tktseason_2016<- SparkR::sql("SELECT Season,
                             Count(*)as Frequency_of_Tickets
                             FROM season_tkt_2016_nyc
                             GROUP BY Season
                             ORDER BY Frequency_of_Tickets desc")
head(tktseason_2016)

freq_tktseason_2016<- data.frame(head(tktseason_2016))
freq_tktseason_2016$Fiscal_Year<- c(2016,2016,2016,2016)
freq_tktseason_2016

#2017 Season vs. Frequency Analysis
Season_Binning_2017 <- SparkR::sql("SELECT Summons_Number,
                                   Violation_Code,
                                   CASE WHEN Issue_Month IN (1,2,12)
                                   THEN 'Winter'
                                   WHEN Issue_Month BETWEEN 3 AND 5
                                   THEN 'Spring'
                                   WHEN Issue_Month BETWEEN 6 AND 8
                                   THEN 'Summer'
                                   WHEN Issue_Month BETWEEN 9 AND 12
                                   THEN 'Fall' 
                                   END AS Season
                                   FROM tkt_2017_nyc")
createOrReplaceTempView(Season_Binning_2017, "season_tkt_2017_nyc")

tktseason_2017<- SparkR::sql("SELECT Season,
                             Count(*)as Frequency_of_Tickets
                             FROM season_tkt_2017_nyc
                             GROUP BY Season
                             ORDER BY Frequency_of_Tickets desc")
head(tktseason_2017)

freq_tktseason_2017<- data.frame(head(tktseason_2017))
freq_tktseason_2017$Fiscal_Year<- c(2017,2017,2017,2017)
freq_tktseason_2017

#Comparison of Season vs. Frequency of Tickets ocer the Years
freq_tktseason_combined<- rbind(freq_tktseason_2015, freq_tktseason_2016, freq_tktseason_2017)

ggplot(freq_tktseason_combined, aes(x= as.factor(Season), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Fiscal_Year) + xlab("Seasons of Year") + ylab("Frequency of Tickets") + ggtitle("Plot11A. Comparison of Seasons vs. Frequency of Tickets between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


# Season vs. Violation Code Distribution Analysis

#2015 Season vs. Violation Code Distribution Analysis

season_violation_2015 <- SparkR::sql("SELECT  Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT dense_rank() over (partition by Season order by Frequency_of_Tickets desc) rk,
                                     Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT Season,
                                     Violation_Code,
                                     Count(*) Frequency_of_Tickets
                                     FROM season_tkt_2015_nyc
                                     GROUP BY Season, Violation_Code))
                                     WHERE rk <= 3
                                     ORDER BY Season, Frequency_of_Tickets desc")

df_season_violation_2015 <-  data.frame(head(season_violation_2015, nrow(season_violation_2015)))
df_season_violation_2015

#Seasonwise Violation Code Distribution 2015
ggplot(df_season_violation_2015, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Season) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot12A. 2015 Comparison of Seasons vs. Frequency of Violation Codes") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#2016 Season vs. Violation Code Distribution Analysis

season_violation_2016 <- SparkR::sql("SELECT  Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT dense_rank() over (partition by Season order by Frequency_of_Tickets desc) rk,
                                     Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT Season,
                                     Violation_Code,
                                     Count(*) Frequency_of_Tickets
                                     FROM season_tkt_2016_nyc
                                     GROUP BY Season, Violation_Code))
                                     WHERE rk <= 3
                                     ORDER BY Season, Frequency_of_Tickets desc")

df_season_violation_2016 <-  data.frame(head(season_violation_2016, nrow(season_violation_2016)))
df_season_violation_2016

#Seasonwise Violation Code Distribution 2016
ggplot(df_season_violation_2016, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Season) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot12B. 2016 Comparison of Seasons vs. Frequency of Violation Codes") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#2017 Season vs. Violation Code Distribution Analysis

season_violation_2017 <- SparkR::sql("SELECT  Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT dense_rank() over (partition by Season order by Frequency_of_Tickets desc) rk,
                                     Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT Season,
                                     Violation_Code,
                                     Count(*) Frequency_of_Tickets
                                     FROM season_tkt_2017_nyc
                                     GROUP BY Season, Violation_Code))
                                     WHERE rk <= 3
                                     ORDER BY Season, Frequency_of_Tickets desc")

df_season_violation_2017 <-  data.frame(head(season_violation_2017, nrow(season_violation_2017)))
df_season_violation_2017

#Seasonwise Violation Code Distribution 2017
ggplot(df_season_violation_2017, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Season) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot12C. 2017 Comparison of Seasons vs. Frequency of Violation Codes") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


#************ Stage 3: Q7 ************#

#2015
violationcd_frequency_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from tkt_2015_nyc 
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2015,3)

fine_top3_2015<- data.frame(head(violationcd_frequency_2015,3))
fine_top3_2015$Fiscal_Year <- c(2015,2015,2015)
fine_top3_2015$Average_Fine_PerTicket<- c(55,50,115)
fine_top3_2015$Total_Fine_Amount<- fine_top3_2015$Frequency_of_Tickets * fine_top3_2015$Average_Fine_PerTicket
fine_top3_2015

#2016
violationcd_frequency_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from tkt_2016_nyc 
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2016,3)

fine_top3_2016<- data.frame(head(violationcd_frequency_2016,3))
fine_top3_2016$Fiscal_Year <- c(2016,2016,2016)
fine_top3_2016$Average_Fine_PerTicket<- c(55,50,50)
fine_top3_2016$Total_Fine_Amount<- fine_top3_2016$Frequency_of_Tickets * fine_top3_2016$Average_Fine_PerTicket
fine_top3_2016

#2017
violationcd_frequency_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from tkt_2017_nyc 
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2017,3)

fine_top3_2017<- data.frame(head(violationcd_frequency_2017,3))
fine_top3_2017$Fiscal_Year <- c(2017,2017,2017)
fine_top3_2017$Average_Fine_PerTicket<- c(55,50,50)
fine_top3_2017$Total_Fine_Amount<- fine_top3_2017$Frequency_of_Tickets * fine_top3_2017$Average_Fine_PerTicket
fine_top3_2017

fine_top3_combined<- rbind(fine_top3_2015, fine_top3_2016, fine_top3_2017)

ggplot(fine_top3_combined, aes(x=as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Fiscal_Year) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Plot10A. Comparison of Top 3 Violation Code vs Frequency of Ticket between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

ggplot(fine_top3_combined, aes(x=as.factor(Violation_Code), y=Total_Fine_Amount))+ geom_col()+ facet_grid(~Fiscal_Year) + xlab("Violation Code") + ylab("Total Fine Amount") + ggtitle("Plot10B. Comparison of Top 3 Violation Code vs Total Fine Amount between Fiscal Years") + geom_text(aes(label=Total_Fine_Amount),vjust=-0.3)












