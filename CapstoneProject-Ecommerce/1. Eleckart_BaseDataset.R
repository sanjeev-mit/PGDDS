## Loading required packages 

requiredPackages = c('cowplot','DataCombine','scales','glmnet','DAAG','caret','GGally','corrplot','lubridate','gdata','ggplot2','dplyr','reshape','tidyr','data.table','MASS','car','rstudioapi')

#Installing the required packages
for(item in requiredPackages){
  if(!require(item,character.only = TRUE)){
    install.packages(item)
  }
  library(item, character.only = TRUE)
}

####setting source directory as working directory#####

sourcedir <- getActiveDocumentContext()$path 
setwd(dirname(sourcedir))

##Disabling the scientific notation

options(scipen=999)

############################################################################################
#### Reading the advt spend details 
###########################################################################################
advt_details <- read.xls("Media data and other information.xlsx", sheet = 2, header = TRUE)
advt_details$year_month <- paste(advt_details$Year , advt_details$Month , sep = '-')
advt_details$Year <- as.numeric(advt_details$Year)
advt_details$Month <- as.numeric(advt_details$Month)
#View(advt_details)

advt_details$year_month <- factor(advt_details$year_month, levels = advt_details$year_month[order(advt_details$Year , advt_details$Month)])

###In long format
advt_details_long <- gather(advt_details, Medium, Spend, 3:12)

##Year wise spend
p_theme <-theme(axis.title = element_text(colour="grey20",size=10,face="bold")
                ,axis.text.x = element_text(angle = 90,face="bold", color="#993333",size=8)
                ,axis.text.y = element_text(face="bold", color="#993333",size=8)
                ,title= element_text(colour="Orange",size=15,face="bold")) 

ggplot(advt_details, aes(x = year_month, y = Total.Investment)) + geom_bar(stat = "identity") +labs(x="yyyy-MM",y="Total Investment") + ggtitle("The Monthwise Advt Spend") +p_theme
  
###Observation:
## 1.Spend  is lowest for aug
## 2.Spend is high in Sep,Oct,Dec,Mar

#For Investments in Radio and others- spend is assigned 0 for NA values
sapply(advt_details , function(x) sum(is.na(x)))
advt_details[which(is.na(advt_details$Radio)), "Radio"] <- 0 
advt_details[which(is.na(advt_details$Other)), "Other"] <- 0

#1.Removing Total investments from data, 
#2.converted to crore value
advt_details$Total.Investment <- NULL
advt_details[,3:11] <- advt_details[,3:11] * 10000000

##########################################################################################
# Importing  Eleckart order data  details
##########################################################################################
Eleckart_order_data <- read.csv ( "ConsumerElectronics.csv" , header = T , stringsAsFactors = F)
nrow(Eleckart_order_data)  ##1648824
str(Eleckart_order_data)

######################################################################################
## Data Quality checks 
#####################################################################################

## There is no issue with case sensitivity
sapply(Eleckart_order_data, function(x) length(unique(toupper(x)))-length(unique(tolower(x)))) 

## NA values treatment
sapply(Eleckart_order_data, function(x){sum(is.na(x))}) ##NA values in cust_id,pin_code and gmv

###Business issues check:

#1. Negative GMV
nrow(filter(Eleckart_order_data , Eleckart_order_data$gmv < 0 ) ) ##0 rows
#2. Zero GMV
nrow(filter(Eleckart_order_data , Eleckart_order_data$gmv == 0 )) ## 1349 rows 
#3. Negative units sold
nrow(filter(Eleckart_order_data , Eleckart_order_data$units < 0 ) ) ## 0 row
#4.Zero units sold
nrow(filter(Eleckart_order_data , Eleckart_order_data$units == 0 )) ## 0 row

##Analysis on deliverybdays and deliverycdays
table(Eleckart_order_data$deliverybdays)  ## rows with negative as well as very high deliverybdays 
table(Eleckart_order_data$deliverycdays)  ## rows with negative as well as very high deliverycdasy

###Payment Types
unique(Eleckart_order_data$s1_fact.order_payment_type)  ## Two payment type available - COD , Prepaid 
table(Eleckart_order_data$s1_fact.order_payment_type)  ## COD is preferred one 

###SLA 
table(Eleckart_order_data$sla)  ## rows with 0 sla. They are same day delivery. Few rows with high sla

## Unique Pincodes
length(unique(Eleckart_order_data$pincode)) ## 7565 unique values 
length(unique(Eleckart_order_data$cust_id)) ## 1201090 distinct customers 

##Remaining fields
unique(Eleckart_order_data$product_analytic_super_category) ## Single value CE 
unique(Eleckart_order_data$product_analytic_sub_category)   ## 14 distinct values 
unique(Eleckart_order_data$product_analytic_category)       ## 5 distinct values 
unique(Eleckart_order_data$product_analytic_vertical)      ## 74 distinct values 

##Negative MRP
filter(Eleckart_order_data , Eleckart_order_data$product_mrp < 0 )  ## 0 row
nrow(filter(Eleckart_order_data , Eleckart_order_data$product_mrp == 0 )) ## 5308 row

###Order date
min(Eleckart_order_data$order_date)   ## "2015-05-19 13:42:09"
max (Eleckart_order_data$order_date ) ## "2016-07-25 01:19:45"

## Create an Engineered variable year-month  
Eleckart_order_data$year_month <- paste(Eleckart_order_data$Year, Eleckart_order_data$Month , sep = '-')
Eleckart_order_data$order_date <- as.Date(Eleckart_order_data$order_date)

##Getting the start week date for the order date
Eleckart_order_data$start_week_date <-   floor_date(as.Date(Eleckart_order_data$order_date), unit="week" , week_start = getOption("lubridate.week.start", 1))
Eleckart_order_data[which(Eleckart_order_data$start_week_date < '2015-07-01'),"start_week_date"] <- '2015-07-01'
View(Eleckart_order_data)


## Filtering the order details for  date range- July 2015 to June 2016
Eleckart_order_data <- subset(Eleckart_order_data, order_date >= '2015-07-01')
Eleckart_order_data <- subset(Eleckart_order_data,order_date < '2016-07-01')

## Filter out the rows  having NA values 
rows_with_na <- apply(Eleckart_order_data, 1, function(x){any(is.na(x))})
sum(rows_with_na) #4904

## Remove the missing values 
Eleckart_order_data <- Eleckart_order_data[!rows_with_na,]


## Add a varaiable to specify month number as per week start date 
Eleckart_order_data$month_wrt_wkstart_date <- format(Eleckart_order_data$start_week_date , "%m")
Eleckart_order_data$month_wrt_wkstart_date <- as.numeric(Eleckart_order_data$month_wrt_wkstart_date)

##Finding the week num corresponding to the order.Assumptions:
## 1. Number the week from 1 to 53
## 2. July 2015 1st week will be 1 and june 2016 last week will be 53 

Eleckart_order_data$week_no <-strftime( Eleckart_order_data$start_week_date ,format="%V")
Eleckart_order_data$week_no <- as.numeric(Eleckart_order_data$week_no)

Eleckart_order_data$week_no[Eleckart_order_data$Year == 2015 ] <- (Eleckart_order_data$week_no[Eleckart_order_data$Year == 2015 ]) -26
Eleckart_order_data$week_no[Eleckart_order_data$Year == 2016 & Eleckart_order_data$week_no !=53 ] <- (Eleckart_order_data$week_no[Eleckart_order_data$Year == 2016 & Eleckart_order_data$week_no !=53 ]) +27
Eleckart_order_data[which(Eleckart_order_data$Year == 2016 & Eleckart_order_data$Month==1 & Eleckart_order_data$week_no == 53 ), "week_no"] <- Eleckart_order_data[which(Eleckart_order_data$Year == 2016 & Eleckart_order_data$Month==1 & Eleckart_order_data$week_no == 53 ), "week_no"] - 26

#View(unique(Eleckart_order_data$week_no))


## Filter out the rows having mrp value 0 

Eleckart_order_data <- Eleckart_order_data[!Eleckart_order_data$product_mrp == 0,]
Eleckart_order_data$deliverybdays[Eleckart_order_data$deliverybdays < 0] = 0
Eleckart_order_data$deliverycdays[Eleckart_order_data$deliverycdays < 0] = 0
Eleckart_order_data$product_procurement_sla [Eleckart_order_data$product_procurement_sla <0 ] =0

###Deliverybdays and deliverycdays
Eleckart_order_data$deliverybdays <- as.numeric(Eleckart_order_data$deliverybdays)
Eleckart_order_data$deliverycdays <- as.numeric(Eleckart_order_data$deliverycdays)
Eleckart_order_data$sla <- as.numeric(Eleckart_order_data$sla)

Eleckart_order_data$delivery_on_time <- Eleckart_order_data $sla - (Eleckart_order_data$deliverybdays+Eleckart_order_data$deliverycdays+Eleckart_order_data$product_procurement_sla)
Eleckart_order_data$delivery_status[Eleckart_order_data$delivery_on_time < 0] <- 'Delayed'
Eleckart_order_data$delivery_status[Eleckart_order_data$delivery_on_time == 0] <- 'On time'
Eleckart_order_data$delivery_status[Eleckart_order_data$delivery_on_time > 0] <- 'Early'


###########################################################################
## Promotional details 
##########################################################################

promo_details <- read.xls("Media data and other information.xlsx", sheet = 3, header = TRUE ,stringsAsFactors = F   )
promo_details$Year[1:6] <- 2015
promo_details$Year[7:12] <- 2016
promo_details$X <- NULL

#Week number Derived manually from holidays
promo_details$start_week_no <- c(3,7,9,16,19,26,30,32,34,33,37,48)
promo_details$end_week_no <- c(3,8,9,16,20,27,30,32,34,34,37,48)
promo_details$promotion_type <- trim(sapply(promo_details$Sales.Calendar, function(x) substr(x , 1,  (regexpr("\\(", x[1])-1 ))))
promo_details$Sales.Calendar <- NULL
promo_details_long <- gather ( promo_details , week_type , week_no , 2:3)
promo_details_long$week_type <- NULL
#View(promo_details_long)

##############################################################################
## Reading the NPS
#############################################################################

nps_score <- read.xls("Media data and other information.xlsx", sheet = 4, header = TRUE)
str(nps_score)
nps_score <- nps_score[2:13]
t_nps_score <- transpose(nps_score)
t_nps_score$Month <- c(seq(7,12,1),seq(1,6,1))
colnames(t_nps_score)[1] <- "NPS"
#View(t_nps_score)

#############################################################################
### Merging the multiple dataset to create a master dataset at weekly level 
#############################################################################

## Grouping the data at weekly level 
weekly_Eleckart_order_data <- Eleckart_order_data %>% group_by ( Year, month_wrt_wkstart_date,  product_analytic_category,product_analytic_sub_category, product_analytic_vertical,year_month , week_no)%>% summarise( prepaid_cnt =  sum(ifelse (s1_fact.order_payment_type =='Prepaid' , 1 , 0)) ,cod_cnt =  sum(ifelse (s1_fact.order_payment_type =='COD' , 1,0)) ,delayed_delivery_cnt =sum(ifelse (delivery_status =='Delayed' , 1 , 0)), early_delivery_cnt =sum(ifelse (delivery_status =='Early' , 1 , 0)), onetime_delivery_cnt =sum(ifelse (delivery_status =='On time' , 1 , 0)), tot_gmv = sum(gmv) , tot_units = sum(units) , tot_product_mrp = sum( as.numeric (product_mrp)), avg_gmv = mean(gmv) , avg_mrp = mean(product_mrp) , no_of_customer = length(unique(cust_id)), no_of_orders = length(unique(order_id)) , list_price = (tot_gmv/tot_units) , avg_price = mean(list_price) )
colnames(weekly_Eleckart_order_data)[2] <- "Month"

## Merging the adv data with weekly data 
weekly_Eleckart_order_data <- merge(weekly_Eleckart_order_data ,advt_details , by=c("Year" , "Month"))

## dont need this variable any more
weekly_Eleckart_order_data$year_month.y <- NULL 
View(weekly_Eleckart_order_data)

## Merging the NPS data 
weekly_Eleckart_order_data <- merge(weekly_Eleckart_order_data ,t_nps_score , by=c("Month"))

## Number of entries in a month  
week_in_a_month <- weekly_Eleckart_order_data %>% group_by( Month ) %>% summarize (  tot_week = length(unique(week_no)) )

weekly_Eleckart_order_data <- merge(weekly_Eleckart_order_data ,week_in_a_month, by = c ( "Month") )

rows_ina_week <- weekly_Eleckart_order_data %>% group_by( week_no ) %>% summarize ( total_row = n())

weekly_Eleckart_order_data <- merge(weekly_Eleckart_order_data ,rows_ina_week, by = c ( "week_no") )

## Converting monthly ad spend into weekly ad spend 

weekly_Eleckart_order_data[,c(22:30)] <- weekly_Eleckart_order_data[,c(22:30)]/(weekly_Eleckart_order_data$tot_week*weekly_Eleckart_order_data$total_row)

## Adding the promotional sale name in dataset

weekly_Eleckart_order_data$promotion_type <- NULL

for (row_no  in 1:nrow(promo_details) ) {
  for (week in promo_details[row_no,2] : promo_details[row_no,3] ){
    #print(paste("The week is", week))
    weekly_Eleckart_order_data[which(weekly_Eleckart_order_data$week_no==week),"promotion_type"]  <-   promo_details[row_no,4]
  }
}

#############################################################################################
## Engineered variables 
###############################################################################################
weekly_Eleckart_order_data$discount_over_mrp <- (weekly_Eleckart_order_data$tot_product_mrp-weekly_Eleckart_order_data$tot_gmv)/weekly_Eleckart_order_data$tot_product_mrp
weekly_Eleckart_order_data$Holiday_week <- ifelse (is.na(weekly_Eleckart_order_data$promotion_type) , 'N','Y' )
weekly_Eleckart_order_data[which(is.na(weekly_Eleckart_order_data$promotion_type)), "promotion_type"] <- "No_promotion"
weekly_Eleckart_order_data$value_per_visitor <- weekly_Eleckart_order_data$tot_gmv/weekly_Eleckart_order_data$no_of_customer

#############################################################################
### EDA analysis on weekly_Eleckart_order_data and advt_details
## EDA indicates we need different ad spend for each product category 
#############################################################################

## Monthly advt spend
View(advt_details_long) 
advt_details_long <- advt_details_long [-c(which(advt_details_long$Medium =="Total.Investment")),]
ggplot(advt_details_long, aes (x = Month, y = Spend, colour = Medium)) + geom_line() + 
  scale_x_discrete(name="Months July 2015-June 2016", limits=seq(1,12,1)) +ggtitle("Monthly Advt Spend") +p_theme

##Week wise sale and Advt spend details for various sub category level 

advt_sales_details <- weekly_Eleckart_order_data %>% group_by (product_analytic_sub_category, week_no)%>% 
  summarise(tot_sales = sum(tot_gmv) ,
            tot_tv_spend = sum (TV), tot_dig_spend = sum (Digital), 
            tot_spon_spend = sum(Sponsorship) , tot_content_spend = sum(Content.Marketing),
            tot_online_spend = sum(Online.marketing) ,tot_aff_spend = sum(X.Affiliates),
            tot_sem_spend = sum(SEM) ,tot_radio_spend = sum(Radio), 
            tot_other_spend = sum(Other))


## weekly sale details for different sub category 
advt_sales_details_1 <-subset(advt_sales_details , product_analytic_sub_category %in% c("CameraAccessory","GamingAccessory","HomeAudio") )

p <- ggplot(advt_sales_details_1 , aes ( x = week_no , y = tot_sales))+ geom_line(size=0.8,color="blue") #geom_bar(stat = "identity")
p + facet_grid(product_analytic_sub_category ~ . ) + labs(x = "week", y = "Sales " ) + ggtitle ( " Weekly Sale") +p_theme +scale_x_continuous(breaks=seq(1,54,by=1)) 
  
require(gridExtra)


###EDA media spend week wise
plot_grid(
  ggplot(advt_sales_details , aes ( x =week_no , y = tot_tv_spend))+ geom_line(color="red")  + scale_x_continuous(breaks=seq(1,54,by=1)) 
  ,ggplot(advt_sales_details , aes ( x = week_no , y = tot_dig_spend))+ geom_line(color="blue") +  scale_x_continuous(breaks=seq(1,54,by=1)) 
  ,ggplot(advt_sales_details , aes ( x = week_no , y = tot_spon_spend))+ geom_line(color="pink") +  scale_x_continuous(breaks=seq(1,54,by=1)) 
  ,ggplot(advt_sales_details , aes ( x = week_no , y = tot_content_spend))+ geom_line(color="orange") + scale_x_continuous(breaks=seq(1,54,by=1)) 
  ,ggplot(advt_sales_details , aes ( x = week_no , y = tot_online_spend))+ geom_line(color="violet") +  scale_x_continuous(breaks=seq(1,54,by=1)) 
  ,ggplot(advt_sales_details , aes ( x = week_no , y = tot_aff_spend))+ geom_line(color="grey") +  scale_x_continuous(breaks=seq(1,54,by=1)) 
  ,ggplot(advt_sales_details , aes ( x = week_no , y = tot_sem_spend))+ geom_line(color="orange") +  scale_x_continuous(breaks=seq(1,54,by=1)) 
  ,ggplot(advt_sales_details , aes ( x = week_no , y = tot_radio_spend))+ geom_line(color="blue") + scale_x_continuous(breaks=seq(1,54,by=1)) 
  ,nrow=8
)

###EDA scope to channel wise:
## 1.Total TV advt spend vs sales details

p <- ggplot(subset(advt_sales_details_1 , product_analytic_sub_category %in% c("CameraAccessory","GamingAccessory","HomeAudio") ) , aes ( x = tot_tv_spend , y = tot_sales))+ geom_line()
p + facet_grid(product_analytic_sub_category ~ .) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs TV Advt") +p_theme

###2.Sale vs Digital Advt
p <- ggplot(advt_sales_details_1 , aes ( x = tot_dig_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ advt_sales_details_1$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Digital Advt") +p_theme

###3. Sale vs Sponsor Advt
p <- ggplot(advt_sales_details_1 , aes ( x = tot_spon_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ advt_sales_details_1$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Sponsor Advt") +p_theme

###4.Sale vs Content Advt
p <- ggplot(advt_sales_details_1 , aes ( x = tot_content_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ advt_sales_details_1$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Content Advt") +p_theme

###5.Sale vs Online Advt
p <- ggplot(advt_sales_details_1 , aes ( x = tot_online_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ advt_sales_details_1$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Online Advt") +p_theme

##6.Sale vs Affiliate Advt
p <- ggplot(advt_sales_details_1 , aes ( x = tot_aff_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ advt_sales_details_1$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Affiliate Advt") +p_theme

### 7. Sale vs SEM Advt
p <- ggplot(advt_sales_details_1 , aes ( x = tot_sem_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ advt_sales_details_1$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs SEM Advt")+p_theme

### 8.Sale vs Radio Ad
p <- ggplot(advt_sales_details_1 , aes ( x = tot_radio_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ advt_sales_details_1$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Radio Advt") +p_theme

###9. Sale vs  Other Advt
p <- ggplot(advt_sales_details_1 , aes ( x = tot_other_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ advt_sales_details_1$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Other Advt")+p_theme


## Average Sale wrt different promotional and non promotional weeks
weekly_Eleckart_order_data %>% group_by ( promotion_type) %>% summarise( Avg_sale = mean(tot_gmv)) %>% ggplot( aes ( x=promotion_type, y =Avg_sale  )) + geom_bar(stat = "identity") +ggtitle("Avg Sale wrt different promotions") +p_theme

## Avg Sale wrt different promotional weeks for 3 sub categories 
weekly_Eleckart_order_data %>% filter(product_analytic_sub_category %in% c("CameraAccessory","GamingAccessory","HomeAudio") )%>% group_by ( product_analytic_sub_category ,promotion_type) %>% summarise( Avg_sale = mean(tot_gmv)) %>% ggplot( aes ( x=promotion_type, y =Avg_sale  )) + geom_bar(stat = "identity") +facet_wrap( ~ product_analytic_sub_category, nrow =2, ncol = 7)+theme(axis.text.x=element_text(angle = -90, hjust = 0))

###Sales Vs Advt Spend

sale_Vs_Advt <- subset(weekly_Eleckart_order_data , product_analytic_sub_category %in% c("CameraAccessory","GamingAccessory","HomeAudio") ) %>% group_by ( week_no) %>% summarise(tot_sales = sum(tot_gmv) , advt_spend = sum(TV+Digital+Sponsorship+Content.Marketing+Online.marketing+X.Affiliates+SEM+Radio+Other))
sale_Vs_Advt_long <- gather(sale_Vs_Advt, Type, Spend, 2:3)
ggplot(sale_Vs_Advt_long, aes ( x= week_no , y = Spend , color = Type))+geom_line()+ggtitle("Sales Vs Advt Spend")+p_theme + scale_x_continuous(breaks=seq(1,54,by=1)) 


### Disocunt vs average sales

discount_vs_sales <- weekly_Eleckart_order_data[,c("tot_gmv" , "discount_over_mrp")]

##discount range

discount_vs_sales$discount_range <- ifelse (discount_vs_sales$discount_over_mrp <= .1 , 'upto 10', 
                                            ifelse ( discount_vs_sales$discount_over_mrp > .1 & discount_vs_sales$discount_over_mrp <= .3 ,'upto 30', 
                                                     ifelse(discount_vs_sales$discount_over_mrp > .3 & discount_vs_sales$discount_over_mrp <= .5 , 'upto 50',
                                                            ' more than 50') ))
discount_vs_sales %>% group_by(discount_range) %>% summarise( avg_sale = mean(tot_gmv)) %>% ggplot(aes ( x=discount_range , y =avg_sale ,fill=discount_range )) + geom_bar(stat = "identity")+ggtitle("Discount vs Sale")+p_theme

## Avg discount wrt different promotional week
weekly_Eleckart_order_data %>% group_by(promotion_type) %>% summarise(avg_discount = mean(discount_over_mrp)) %>% ggplot(aes(x= promotion_type, y =avg_discount, fill=promotion_type )) + geom_bar(stat = "identity")+ggtitle("Discount wrt promotion week") +p_theme

## Nps vs week
weekly_Eleckart_order_data %>% group_by(week_no) %>% summarise(nps = mean(NPS)) %>% ggplot(aes(x= week_no, y =nps )) + geom_bar(stat = "identity")+ggtitle("Nps Vs Week") +p_theme
## payment type vs number of orders
plot_grid(
Eleckart_order_data %>% filter(product_analytic_sub_category %in% c("CameraAccessory","GamingAccessory","HomeAudio") )%>% group_by ( product_analytic_sub_category,s1_fact.order_payment_type) %>% summarise(Sale = sum(gmv)) %>% ggplot(aes(x= s1_fact.order_payment_type, y =Sale)) + geom_bar(stat = "identity", fill="chocolate1")+ facet_wrap(~product_analytic_sub_category ) +labs(x="payment type",y="Total Sale ") +p_theme + ggtitle("payment type vs number of orders and Total Sale") + theme(legend.position="none")
,Eleckart_order_data %>% filter(product_analytic_sub_category %in% c("CameraAccessory","GamingAccessory","HomeAudio") )%>% group_by ( product_analytic_sub_category,s1_fact.order_payment_type) %>% summarise(order_count = n()) %>% ggplot(aes(x= s1_fact.order_payment_type, y =order_count )) + geom_bar(stat = "identity", fill="chocolate1")+ facet_wrap(~product_analytic_sub_category ) +labs(x="payment type",y="Total Orders") +p_theme + theme(legend.position="none")
, nrow=2
)
## delivery_status  vs number of orders
Eleckart_order_data %>% group_by ( delivery_status) %>% summarise(order_count = n()) %>% ggplot(aes(x= delivery_status, y =order_count )) + geom_bar(stat = "identity")+labs(x="Delivery Status",y="Total Orders") + ggtitle("Delivery status Orders Count")+p_theme

##Delivery count for different delivery statuses
plot_grid(
weekly_Eleckart_order_data %>% group_by ( product_analytic_sub_category, week_no)  %>% summarise(Early_delivery_count = sum(early_delivery_cnt))  %>%ggplot(aes(x= week_no, y =Early_delivery_count ,fill="chocolate1" )) + geom_bar(stat = "identity")  +p_theme + scale_x_continuous(breaks=seq(1,54,by=1)) + theme(legend.position="none") +  ggtitle("Delivery count for different delivery statuses") 
,weekly_Eleckart_order_data %>% group_by ( product_analytic_sub_category, week_no)  %>% summarise(Ontime_delivery_count = sum(onetime_delivery_cnt))  %>%ggplot(aes(x= week_no, y =Ontime_delivery_count ,fill="coral4" )) + geom_bar(stat = "identity") +  p_theme + scale_x_continuous(breaks=seq(1,54,by=1)) + theme(legend.position="none")
,weekly_Eleckart_order_data %>% group_by ( product_analytic_sub_category, week_no)  %>% summarise(delayed_delivery_count = sum(delayed_delivery_cnt))  %>%ggplot(aes(x= week_no, y =delayed_delivery_count ,fill="seagreen1" )) + geom_bar(stat = "identity") + p_theme + scale_x_continuous(breaks=seq(1,54,by=1)) + theme(legend.position="none")
, nrow=3)

#Customer distribution across weeks
weekly_Eleckart_order_data%>% filter(product_analytic_sub_category %in% c("CameraAccessory","GamingAccessory","HomeAudio") )%>% group_by ( product_analytic_sub_category, week_no) %>% summarise(customer_count = sum(no_of_customer))  %>% ggplot(aes(x= week_no, y =customer_count )) + geom_bar(stat = "identity",  color ="darkred" ,fill="seagreen4") + facet_grid(product_analytic_sub_category~. ) + labs(x="week",y="customer Count") +p_theme + scale_x_continuous(breaks=seq(1,54,by=1)) +  ggtitle("Customer distribution across weeks")

#summarise(Early_delivery_count = sum(early_delivery_cnt))
############################################################################
###createing 3 different data set & adding engineered kpis to them
#############################################################################

unique(weekly_Eleckart_order_data$product_analytic_category)

## Since model needs to be built at sub category level, this varaiable is not needed 
weekly_Eleckart_order_data$product_analytic_category <- NULL 
weekly_Eleckart_order_data$year_month.x <- NULL 

## Filtering data to have only targeted sub categories
weekly_Eleckart_order_data <- filter ( weekly_Eleckart_order_data ,weekly_Eleckart_order_data$product_analytic_sub_category %in% c("CameraAccessory", "GamingAccessory", "HomeAudio")) 
View(weekly_Eleckart_order_data)

## Dummy variable creation for character data types 
weekly_Eleckart_order_data_chr <- weekly_Eleckart_order_data[,c(4,5,32,34)]

###Converting to factor
weekly_Eleckart_order_data_fact <- data.frame(sapply(weekly_Eleckart_order_data_chr, function(x) factor(x)))
str(weekly_Eleckart_order_data_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(weekly_Eleckart_order_data_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =weekly_Eleckart_order_data_fact))[,-1]))

## Create master data set by appending dummies with main data set 
weekly_Eleckart_order_data_overall <- cbind(weekly_Eleckart_order_data[,c(1:3,6:31,33,35)],dummies) 
View(weekly_Eleckart_order_data_overall) 

###############################################################################################
### Outlier treatment 
###############################################################################################


boxplot(weekly_Eleckart_order_data_overall$tot_gmv )
boxplot(weekly_Eleckart_order_data_overall$tot_units)
boxplot(weekly_Eleckart_order_data_overall$tot_product_mrp)
boxplot(weekly_Eleckart_order_data_overall$TV)
boxplot(weekly_Eleckart_order_data_overall$Digital)
boxplot(weekly_Eleckart_order_data_overall$Sponsorship)
boxplot(weekly_Eleckart_order_data_overall$Content.Marketing)
boxplot(weekly_Eleckart_order_data_overall$Online.marketing)
boxplot(weekly_Eleckart_order_data_overall$X.Affiliates)
boxplot(weekly_Eleckart_order_data_overall$SEM)
boxplot(weekly_Eleckart_order_data_overall$Radio)
boxplot(weekly_Eleckart_order_data_overall$Other)


## Since there are lots of outliers  in dataset, they cant be removed. 


overall_quantile <- sapply(weekly_Eleckart_order_data_overall[,c("tot_gmv","tot_units", "tot_product_mrp" , "TV" ,"Digital",
                                                           "Sponsorship", "Content.Marketing", "Online.marketing" ,"X.Affiliates", "SEM" ,"Radio" , "Other" )], 
                           function(x) quantile(x,seq(0,1,.01),na.rm = T)) 

## remove_outliers function for capping the value to specific quantile

remove_outliers <- function(x , lower_quantile, upper_quantile) {
  qnt <- quantile(x, probs=c(lower_quantile, upper_quantile), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  y <- x
  y[x < qnt[1]] <- qnt[1]
  y[x > qnt[2]] <- qnt[2]
  y
}

weekly_Eleckart_order_data_overall$tot_gmv <- remove_outliers (weekly_Eleckart_order_data_overall$tot_gmv,0, .97 ) 
weekly_Eleckart_order_data_overall$tot_units <- remove_outliers (weekly_Eleckart_order_data_overall$tot_units,0, .97 ) 
weekly_Eleckart_order_data_overall$tot_product_mrp <- remove_outliers (weekly_Eleckart_order_data_overall$tot_product_mrp,0, .97 ) 
weekly_Eleckart_order_data_overall$TV <- remove_outliers (weekly_Eleckart_order_data_overall$TV,0, .98 ) 
weekly_Eleckart_order_data_overall$Digital <- remove_outliers (weekly_Eleckart_order_data_overall$Digital,0, .95 ) 
weekly_Eleckart_order_data_overall$Sponsorship <- remove_outliers (weekly_Eleckart_order_data_overall$Sponsorship,0, .95 ) 
weekly_Eleckart_order_data_overall$Content.Marketing <- remove_outliers (weekly_Eleckart_order_data_overall$Content.Marketing,0, .95 ) 
weekly_Eleckart_order_data_overall$SEM <- remove_outliers (weekly_Eleckart_order_data_overall$SEM,0, .95 ) 
weekly_Eleckart_order_data_overall$Radio <- remove_outliers (weekly_Eleckart_order_data_overall$Radio,0, .95 ) 
weekly_Eleckart_order_data_overall$Other <- remove_outliers (weekly_Eleckart_order_data_overall$Other,0, .95 )

## Removing extra column
weekly_Eleckart_order_data_overall$total_row <- NULL
weekly_Eleckart_order_data_overall$tot_week <- NULL

##  Taking back up of master dataset weekly_Eleckart_order_data_overall
weekly_Eleckart_order_data_overall_bkp <- weekly_Eleckart_order_data_overall

###Correlation calculation
corr <- cor(weekly_Eleckart_order_data_overall)

##Depending on higher corelation or since there vars are direct proxy to sales , so taking them out
weekly_Eleckart_order_data_overall$avg_mrp <- NULL
weekly_Eleckart_order_data_overall$avg_price <- NULL
weekly_Eleckart_order_data_overall$tot_units <- NULL
weekly_Eleckart_order_data_overall$no_of_orders <- NULL
weekly_Eleckart_order_data_overall$tot_product_mrp <- NULL
weekly_Eleckart_order_data_overall$avg_gmv <- NULL
weekly_Eleckart_order_data_overall$value_per_visitor <- NULL
weekly_Eleckart_order_data_overall$Year <- NULL
weekly_Eleckart_order_data_overall$no_of_customer <- NULL
weekly_Eleckart_order_data_overall$delayed_delivery_cnt <- NULL
weekly_Eleckart_order_data_overall$early_delivery_cnt <- NULL
weekly_Eleckart_order_data_overall$onetime_delivery_cnt <- NULL
weekly_Eleckart_order_data_overall$cod_cnt <- NULL
weekly_Eleckart_order_data_overall$prepaid_cnt <- NULL



## Create 3 data set HomeAudio, GamingAccessory and CameraAccessory  for model building. 

CameraAccessory <- filter(weekly_Eleckart_order_data_overall,
                          weekly_Eleckart_order_data_overall$product_analytic_sub_category.xGamingAccessory==0,
                          weekly_Eleckart_order_data_overall$product_analytic_sub_category.xHomeAudio==0)

CameraAccessory <<- CameraAccessory[,-which(names(CameraAccessory) %in% c("product_analytic_sub_category.xGamingAccessory","product_analytic_sub_category.xHomeAudio"))]

HomeAudio <- filter(weekly_Eleckart_order_data_overall,
                          weekly_Eleckart_order_data_overall$product_analytic_sub_category.xGamingAccessory==0,
                          weekly_Eleckart_order_data_overall$product_analytic_sub_category.xHomeAudio==1)


HomeAudio <<- HomeAudio[,-which(names(HomeAudio) %in% c("product_analytic_sub_category.xGamingAccessory","product_analytic_sub_category.xHomeAudio"))]

GamingAccessory <- filter(weekly_Eleckart_order_data_overall,
                          weekly_Eleckart_order_data_overall$product_analytic_sub_category.xGamingAccessory==1,
                          weekly_Eleckart_order_data_overall$product_analytic_sub_category.xHomeAudio==0)

GamingAccessory <<- GamingAccessory[,-which(names(GamingAccessory) %in% c("product_analytic_sub_category.xGamingAccessory","product_analytic_sub_category.xHomeAudio"))]

str(HomeAudio)
str(GamingAccessory)
str(CameraAccessory)

nrow(HomeAudio)
nrow(GamingAccessory)
nrow(CameraAccessory)



