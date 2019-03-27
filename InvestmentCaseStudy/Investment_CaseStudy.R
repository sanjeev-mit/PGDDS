#Loading the required package for data cleaning and manipulation

library(dplyr)
library(tidyr)

# CheckPoint 1 :Data Cleaning 1

# Loading the companies.txt to data frame

companies <- read.delim("companies.txt",header = T,stringsAsFactors =F)

#Loading the rounds2 to data frame

rounds2 <- read.csv("rounds2.csv",header=T,stringsAsFactors = F)

# converting the permalink to uniform case 
companies$permalink <- toupper(companies$permalink)
rounds2$company_permalink <- toupper(rounds2$company_permalink)

#Unique companies present in rounds2 dataframe
unique_companies_rounds2 <-count(distinct(rounds2,company_permalink))

#Unique companies present in companies dataframe
unique_companies<- count(distinct(companies,permalink))

# Updating column name of companies data frame to same as in rounds2 data frame
colnames(companies)[colnames(companies)=="permalink"] <- "company_permalink"

#Count of companies present in rounds2 but not in companies dataset
count_notinrounds2 <- count(anti_join(companies,rounds2))

#merging the two data frames

master_frame <- merge(rounds2,companies,by="company_permalink")
master_frame <- na.omit(master_frame)


#Checkpoint 2

# creating vectors of desired funding type
funding_type <- c("seed","venture","private_equity","angel")

#filtering  master_frame 
filtered_master_frame <- filter(master_frame,funding_round_type %in% funding_type)

#grouping the filtered_master_frame by funding type
funding_grouped_byfundingtype <- group_by(filtered_master_frame,funding_round_type)
funding_average_byfundingtype <- summarise(funding_grouped_byfundingtype,avg_funding=mean(raised_amount_usd,na.rm = T))

#Best funding type for the Spark foundation

best_funding_details <- funding_average_byfundingtype %>%
                    filter(avg_funding>=5000000, avg_funding<=15000000)%>%
                    arrange(desc(avg_funding))%>%
                    head(.,n=1)

best_funding_type <- best_funding_details$funding_round_type

#Checkpoint 3:country Analysis

# filtering master data frame for the venture funding type

bestfundingtype_master_frame <- filter(master_frame,funding_round_type==best_funding_type)

#top 9 countries by the total funding received

top9  <- bestfundingtype_master_frame %>%
         group_by(country_code) %>%
         summarise(totalfunding= sum(raised_amount_usd,na.rm = T))%>%
         arrange(desc(totalfunding))%>%
         filter(country_code != '')%>%
         head(.,n=9)

# Checkpoint 4: Sector Analysis 1


#Reading the mapping data set
mappings <- read.csv("mapping.csv",header = TRUE,sep = ",")

#Converting the wide data format to long data format.
mappings <- gather(mappings,sector,isPresent,-1) 
mappings <- mappings[!(mappings$isPresent==0),]
mappings <- mappings[,-3]

#Renaming the col names of final_mappings
colnames(mappings) <- c("Primary_Sector","Main_Sector")

#Extracting the primary sector from the category_list column
Primary_Sector <- sapply(strsplit(master_frame$category_list,"\\|"), function(x) x[1])
master_frame_sectored <- cbind(master_frame,Primary_Sector)

master_frame_sectored$Primary_Sector <- tolower(master_frame_sectored$Primary_Sector)
mappings$Primary_Sector <- tolower(mappings$Primary_Sector)

# Merging the mapping data with the master data to obtain the Main Sector of the corresponding Primary sector.
master_frame_sectored <- merge(master_frame_sectored,mappings,by="Primary_Sector", all.x=T)
master_frame_sectored <- na.omit(master_frame_sectored)


# Checkpoint 5: Sector Analysis 2


#master_frame data for the funding type as venture

master_frame_sectored_venture <- filter(master_frame_sectored,funding_round_type==best_funding_type)


#master-frame data for meeting the criteria of investement amount bracket for USA

USA_Data <- master_frame_sectored_venture %>%
    filter(raised_amount_usd>=5000000, raised_amount_usd<=15000000,country_code=="USA")


#master-frame data for meeting the criteria of investement amount bracket for GBR
GBR_Data <- master_frame_sectored_venture %>%
  filter(raised_amount_usd>=5000000, raised_amount_usd<=15000000,country_code=="GBR")


#master-frame data for meeting the criteria of investement amount bracket for IND
IND_Data <- master_frame_sectored_venture %>%
  filter(raised_amount_usd>=5000000, raised_amount_usd<=15000000,country_code=="IND")


# Sector wise investment count and total investment for USA

D1_Sector_Investment <- USA_Data %>%
                        group_by(Main_Sector) %>%
                        summarise(Investment_count=n(),Investment_Total=sum(raised_amount_usd))

# Sector wise investment count and total investment for GBR
D2_Sector_Investment <- GBR_Data %>%
                        group_by(Main_Sector) %>%
                        summarise(Investment_count=n(),Investment_Total=sum(raised_amount_usd))

# Sector wise investment count and total investment for IND  
D3_Sector_Investment <- IND_Data %>%
                        group_by(Main_Sector) %>%
                        summarise(Investment_count=n(),Investment_Total=sum(raised_amount_usd))  

#Merging for final data

#USA result set
D1 <- merge(USA_Data,D1_Sector_Investment,by="Main_Sector")

#GBR result set
D2 <- merge(GBR_Data,D2_Sector_Investment,by="Main_Sector")

#IND result set
D3 <- merge(IND_Data,D3_Sector_Investment,by="Main_Sector")


#No of Investment in USA for venture funding
USA_Venture_Invesment_Count <- sum(D1_Sector_Investment$Investment_count)

#No of Investment in GBR for venture funding
GBR_Venture_Invesment_Count <- sum(D2_Sector_Investment$Investment_count)

#No of Investment in IND for venture funding
IND_Venture_Invesment_Count <- sum(D3_Sector_Investment$Investment_count)


#Total Investment amount in USA for venture funding
USA_Venture_Total_Investment <- sum(D1_Sector_Investment$Investment_Total)

#Total Investment amount in GBR for venture funding
GBR_Venture_Total_Investment <- sum(D2_Sector_Investment$Investment_Total)

#Total Investment amount in IND for venture funding
IND_Venture_Total_Investment <- sum(D3_Sector_Investment$Investment_Total)

# Finding top 3 sectors for each country on the basis of number of investments

USA_TOP3_Sectors <- head(arrange(D1_Sector_Investment,desc(Investment_count)),n=3)
GBR_TOP3_Sectors <- head(arrange(D2_Sector_Investment,desc(Investment_count)),n=3)
IND_TOP3_Sectors <- head(arrange(D3_Sector_Investment,desc(Investment_count)),n=3)

# Finding the number of investment across top 3 sectors

#USA Data
USA_Invesment_Count_TopSector <- USA_TOP3_Sectors[1,2]
USA_Invesment_Count_SecondSector <- USA_TOP3_Sectors[2,2]
USA_Invesment_Count_ThirdSector <-USA_TOP3_Sectors[3,2]

#GBR Data
GBR_Invesment_Count_TopSector <- GBR_TOP3_Sectors[1,2]
GBR_Invesment_Count_SecondSector <- GBR_TOP3_Sectors[2,2]
GBR_Invesment_Count_ThirdSector <- GBR_TOP3_Sectors[3,2]

# IND Data 
IND_Invesment_Count_TopSector <- IND_TOP3_Sectors[1,2]
IND_Invesment_Count_SecondSector <- IND_TOP3_Sectors[2,2]
IND_Invesment_Count_ThirdSector <- IND_TOP3_Sectors[3,2]


#Top company in USA 
USA_Top_1_Company_wise <- subset(USA_Data,USA_Data$Main_Sector==USA_TOP3_Sectors$Main_Sector[1]) %>%
                          group_by(company_permalink) %>%
                          summarise(total=sum(raised_amount_usd))%>%
                          arrange(desc(total)) %>%
                          head(.,n=1)
#Top company in GBR         
GBR_Top_1_Company_wise <- subset(GBR_Data,GBR_Data$Main_Sector==GBR_TOP3_Sectors$Main_Sector[1]) %>%
                          group_by(company_permalink) %>%
                          summarise(total=sum(raised_amount_usd))%>%
                          arrange(desc(total)) %>%
                          head(.,n=1)

#Top company in IND
IND_Top_1_Company_wise <- subset(IND_Data,IND_Data$Main_Sector==IND_TOP3_Sectors$Main_Sector[1]) %>%
                          group_by(company_permalink) %>%
                          summarise(total=sum(raised_amount_usd))%>%
                          arrange(desc(total)) %>%
                          head(.,n=1)


#Second best in USA
USA_Top_2_Company_wise <- subset(USA_Data,USA_Data$Main_Sector==USA_TOP3_Sectors$Main_Sector[2]) %>%
                          group_by(company_permalink) %>%
                          summarise(total=sum(raised_amount_usd))%>%
                          arrange(desc(total))%>%
                          head(.,n=1)

#Second best sector in GBR
GBR_Top_2_Company_wise <- subset(GBR_Data,GBR_Data$Main_Sector==GBR_TOP3_Sectors$Main_Sector[2]) %>%
                          group_by(company_permalink) %>%
                          summarise(total=sum(raised_amount_usd))%>%
                          arrange(desc(total))%>%
                          head(.,n=1)

#Second best sector in IND
IND_Top_2_Company_wise <- subset(IND_Data,IND_Data$Main_Sector==IND_TOP3_Sectors$Main_Sector[2]) %>%
                          group_by(company_permalink) %>%
                          summarise(total=sum(raised_amount_usd))%>%
                          arrange(desc(total))%>%
                          head(.,n=1)


