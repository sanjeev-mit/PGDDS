install.packages("MASS")
install.packages("car")
install.packages("stringr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("DescTools")
install.packages("corrplot")
install.packages("gridExtra")


#loading the required packages

library(MASS)
library(car)
library(stringr)
library(dplyr)
library(DescTools)
library(corrplot) 
library(ggplot2)
library(gridExtra)


###########Reading the data
US_Car <- read.csv("CarPrice_Assignment.csv",stringsAsFactors = F)

#Checking the data quality 

#1. Checking for Duplicated carID- No duplicate
sum(duplicated(US_Car$car_ID))

#2. Checking for na - No missing values
sum(is.na(US_Car))


##########Data preparation

#1. Getting the company name

US_Car$carModel <- str_split_fixed(US_Car$CarName,"[ ]",2)[,2]

US_Car$CarName <- str_split_fixed(US_Car$CarName,"[ ]",2)[,1]

#Renaming the column to Company
names(US_Car)[3] <-"Company"


#2. converting all the character to a common case(lower case)

US_Car <- data.frame(lapply(US_Car, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))

#3. Symboling Analysis

table(factor(US_Car$symboling))
Desc(US_Car$symboling) ### -2 having only 3 Variable (0.01%)


table(US_Car$Company) #This indicates there are various Company name issues


#4.Checking for Unique Company Name to fix any spelling or case issue

company_names <- unique(US_Car$Company)
View(company_names)

# a. Since for toyouta and toyota same model name is there hence same company

US_Car %>%select(Company,carModel) %>% filter((Company=="toyouta" | Company=="toyota")& carModel=="tercel")

US_Car$Company[which(US_Car$Company=="toyouta")]= "toyota"

# b. "porcshce and porsche" 

US_Car$Company[which(US_Car$Company=="porcshce")] = "porsche"

#c. "vokswagen,volkswagen and vw"

US_Car$Company[which(US_Car$Company=="vokswagen" |US_Car$Company=="vw")] = "volkswagen"

#d. maxda renamed to mazda, Because Carmodel maxda is matched with mazda

US_Car$Company[which(US_Car$Company=="maxda")] = "mazda"

#Summary based on the car name
US_Car$Company <- as.factor(US_Car$Company)
Desc(US_Car$Company)
summary(factor(US_Car$Company))


#5. fueltype :
table(US_Car$fueltype) ### No Issue

#6. aspiration :
table(US_Car$aspiration)  ### No Issue

#7. doornumber :

table(US_Car$doornumber) 

# Updating the doornumber to integer value
US_Car$doornumber = as.character(US_Car$doornumber)
US_Car$doornumber[which(US_Car$doornumber=="four")] = 4
US_Car$doornumber[which(US_Car$doornumber=="two")] = 2
US_Car$doornumber = as.integer(US_Car$doornumber)

#8. carbody :
table(US_Car$carbody) ### No Issue

#9. drivewheel
table(US_Car$drivewheel) ### No Issue

#10. enginelocation
table(US_Car$enginelocation) ### No Issue


#11. wheelbase
summary(US_Car$wheelbase)
Desc(US_Car$wheelbase) ### Data Having Outliers
quantile(US_Car$wheelbase,probs = seq(0,1,0.1))

## Finding the Outliers wheelbase

Q1 = quantile(US_Car$wheelbase,probs = c(0.25))
Q3 = quantile(US_Car$wheelbase,probs = c(0.75))
IQR = Q3-Q1
Upper_outlier = Q3+(IQR*1.5)
range(US_Car$wheelbase)
US_Car[US_Car$wheelbase >Upper_outlier, ] ## Total we have 3 outliers-Hence neglecting

#12. carlength
Desc(US_Car$carlength) ### No Issue


#13. carwidth

Desc(US_Car$carwidth) ### Data Having Outliers-but could be valid width
Q1 = quantile(US_Car$carwidth,probs = c(0.25))
Q3 = quantile(US_Car$carwidth,probs = c(0.75))
IQR = Q3-Q1
Upper_outlier = Q3+(IQR*1.5)
range(US_Car$carwidth)
US_Car[US_Car$carwidth >Upper_outlier, ]


#14. carheight
Desc(US_Car$carheight) ### No Issue

#15. curbweight
Desc(US_Car$curbweight) ### No Issue

#16. enginetype
Desc(US_Car$enginetype) ### dohcv Engine frequency is only 1 on checking dohcv type is not available, We changed "dohcv to dohv"

US_Car$enginetype = as.character(US_Car$enginetype)
US_Car$enginetype[which(US_Car$enginetype=="dohcv")] = "dohc"
US_Car$enginetype = as.factor(US_Car$enginetype)

#17. cylindernumber
Desc(US_Car$cylindernumber)  #### Jagure XK having 12 cylenders

US_Car$cylindernumber=as.character(US_Car$cylindernumber)

#Updating cyllinder number to integer value for model creation

US_Car$cylindernumber[which(US_Car$cylindernumber=="four")] = 4
US_Car$cylindernumber[which(US_Car$cylindernumber=="six")] = 6
US_Car$cylindernumber[which(US_Car$cylindernumber=="five")] = 5
US_Car$cylindernumber[which(US_Car$cylindernumber=="eight")] = 8
US_Car$cylindernumber[which(US_Car$cylindernumber=="two")] = 2
US_Car$cylindernumber[which(US_Car$cylindernumber=="three")] = 3
US_Car$cylindernumber[which(US_Car$cylindernumber=="twelve")] = 12
US_Car$cylindernumber=as.integer(US_Car$cylindernumber)


#18. enginesize

Desc(US_Car$enginesize)  #### Data Having Outliers
range(US_Car$enginesize)
Q1ES = quantile(US_Car$enginesize,probs = c(0.25))
Q3ES = quantile(US_Car$enginesize,probs = c(0.75))
IQR = Q3ES-Q1ES
Upper_outlier_ES = Q3ES+(IQR*1.5)
US_Car[US_Car$enginesize >Upper_outlier_ES, ] # Ther are 10 outliers but not much effect

#19.fuelsystem
Desc(US_Car$fuelsystem) ### No Issue

#20. boreratio 
Desc(US_Car$boreratio) ### No Issue

#21. stroke 
Desc(US_Car$stroke) ### Data Having Outliers

#22. compressionratio
Desc(US_Car$compressionratio) ### Data Having Outlier

#23. horsepower
Desc(US_Car$horsepower) ### Data Having Outlier

#24. peakrpm
Desc(US_Car$peakrpm) ### Data Having Outlier

#25. citympg
Desc(US_Car$citympg) ### Data Having Outlier

#26.highwaympg
Desc(US_Car$highwaympg) ### Data Having Outlier

#27.price(Dependent variable)
Desc(US_Car$price) ### Data Having Outlier-But count is not significant
Q1P = quantile(US_Car$price,probs = c(0.25))
Q3P = quantile(US_Car$price,probs = c(0.75))
IQR = Q3P-Q1P
Upper_outlier = Q3P+(IQR*1.5)
range(US_Car$price)
US_Car[US_Car$price >Upper_outlier, ]


########### EDA For numerical Variables and Categorical Variables#########

#Check the Pattern for Category variable and dependent  Variables

US_Car_Numeric = US_Car[,sapply(US_Car,is.numeric)]
dim(US_Car_Numeric)

US_Car_Category =  US_Car[,sapply(US_Car,is.factor)]
dim(US_Car_Category)

str(US_Car_Category)

# Correlation on Numerical data
#Based on the correlation Plot we are able to identify the Positive & negative Correlated variables
#Cylindernumber,horsepower,enginesize,price,boreratio,wheelbase,carwidth,carlength,curbweight are positive Correlaction and remaining Variables are negitive corelaction
corrplot(cor(US_Car_Numeric),method = "circle",type="full",
         outline = T,addgrid.col = "darkgray",order="hclust",
         mar = c(2,0,1,0),title = "Numeric - Correlaction") 


### Check the relation between Categorical and Price using the boxplot.

colnames(US_Car_Category)
colnames(US_Car_Numeric)

#### Fueltype Vs Price

FP = ggplot(US_Car_Category,aes(x = fueltype,y = US_Car_Numeric$price,fill=fueltype))+
  geom_boxplot(outlier.colour = "red",outlier.size = 1)+scale_y_continuous(name = "Price")+
  ggtitle(label = "Fueltype vs Price")+ theme_gray()

#### aspiration Vs Price

AP = ggplot(US_Car_Category,aes(x = aspiration,y = US_Car_Numeric$price,fill=aspiration))+
  geom_boxplot(outlier.colour = "red",outlier.size = 1)+scale_y_continuous(name = "Price")+
  ggtitle(label = "Aspiration vs Price")+ theme_gray()

#### carbody Vs Price

CP = ggplot(US_Car_Category,aes(x = carbody,y = US_Car_Numeric$price,fill=carbody))+
  geom_boxplot(outlier.colour = "red",outlier.size = 1)+scale_y_continuous(name = "Price")+
  ggtitle(label = "Carbody vs Price")+ theme_gray()

#### drivewheel Vs Price

DP = ggplot(US_Car_Category,aes(x = drivewheel,y = US_Car_Numeric$price,fill=drivewheel))+
  geom_boxplot(outlier.colour = "red",outlier.size = 1)+scale_y_continuous(name = "Price")+
  ggtitle(label = "Drivewheel vs Price")+ theme_gray()

#### enginelocation Vs Price

EP = ggplot(US_Car_Category,aes(x = enginelocation,y = US_Car_Numeric$price,fill=enginelocation))+
  geom_boxplot(outlier.colour = "red",outlier.size = 1)+scale_y_continuous(name = "Price")+
  ggtitle(label = "Enginelocation vs Price")+ theme_gray()

##### enginetype Vs Price

ETP = ggplot(US_Car_Category,aes(x = enginetype,y = US_Car_Numeric$price,fill=enginetype))+
  geom_boxplot(outlier.colour = "red",outlier.size = 1)+scale_y_continuous(name = "Price")+
  ggtitle(label = "Enginetype vs Price")+ theme_gray()

#### Fuelsystem Vs Price

FSP = ggplot(US_Car_Category,aes(x = fuelsystem,y = US_Car_Numeric$price,fill=fuelsystem))+
  geom_boxplot(outlier.colour = "red",outlier.size = 1)+scale_y_continuous(name = "Price")+
  ggtitle(label = "Fuelsystem vs Price")+ theme_gray()

#### CompanyName Vs Price##############

CMP = ggplot(US_Car_Category,aes(x =Company ,y = US_Car_Numeric$price,fill=Company))+
  geom_boxplot(outlier.colour = "red",outlier.size = 1)+scale_y_continuous(name = "Price")+
  ggtitle(label = "CarName_NEW vs Price")+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

grid.arrange(FP,AP,CP,DP,top="Categorical data Vs Price Pattren-1") ### Looks Like Variables are in Different Median, So These Variables are effecting the Price Variables

grid.arrange(EP,CMP,FSP,ETP, top="Categorical data Vs Price Pattren-2") ### Looks Like Variables are in Different Median, So These Variables are effecting the Price Variables

# converting multilevel variables to dummy and then to numbers
#1. carbody
dummy_cb <- data.frame(model.matrix( ~carbody, data = US_Car))
View(dummy_cb)
dummy_cb <- dummy_cb[,-1]


#2. drivewheel
dummy_dw <- data.frame(model.matrix( ~drivewheel, data = US_Car))
View(dummy_dw)
dummy_dw <- dummy_dw[,-1]

#3. enginetype
dummy_et <- data.frame(model.matrix( ~enginetype, data = US_Car))
View(dummy_et)
dummy_et <- dummy_et[,-1]


#4. fuelsystem
dummy_fsys <- data.frame(model.matrix( ~fuelsystem, data = US_Car))
View(dummy_fsys)
dummy_fsys <- dummy_fsys[,-1]


#5. Company name
dummy_comp <- data.frame(model.matrix( ~Company, data = US_Car))
View(dummy_comp)
dummy_comp <- dummy_comp[,-1]

## removing car models and Combining dummy variable in data set called US_Car_Exp
US_Car_Exp <- cbind(US_Car[,setdiff(names(US_Car),
                                           c("Type","carbody","drivewheel","enginetype"
                                             ,"fuelsystem","company"))], 
                    dummy_cb, dummy_dw, dummy_et, dummy_fsys,dummy_comp)
View(US_Car_Exp)
str(US_Car_Exp)


## Derived metrices

#1. Overall mpg
US_Car_Exp$Ompg <- round(mean(US_Car_Exp$citympg + US_Car_Exp$highwaympg),2)

#2. Stroke2Bore Ratio
US_Car_Exp$sbr <- round(US_Car_Exp$stroke/US_Car_Exp$boreratio,2)

#4. Overall mpg to Horsepower ratio
US_Car_Exp$Ohp <- round(US_Car_Exp$Ompg/US_Car_Exp$horsepower, 2)

#5. Overall mpg to curbweight ratio (FE)
US_Car_Exp$FE <- round(US_Car_Exp$Ompg/US_Car_Exp$curbweight, 4)

## Setting seed to achieve reproducibility
set.seed(9999)

## avoiding scientific notation to increase comparability
options(scipen = 999)

## seperating Training and test datasets
trainindices= sample(1:nrow(US_Car_Exp), 0.7*nrow(US_Car_Exp))
car_training = US_Car_Exp[trainindices,]
car_testing = US_Car_Exp[-trainindices,]

##### Creating the regression models using StepAIC.

model_1 = lm(price~.,data=car_training)
summary(model_1)

### Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9897 


## using stepAIC to estimate the model
step <- stepAIC(model_1, direction = "both")

## using last step of AIC for finalisation of our model
model_2 <- lm(price ~ car_ID + fueltype + aspiration + doornumber + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                enginesize + stroke + peakrpm + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd + enginetypel + 
                enginetypeohcf + cylindernumber + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                Companybmw + Companychevrolet + Companydodge + Companyisuzu + 
                Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                Companyplymouth + Companyrenault + Companytoyota + Companyvolkswagen + 
                Companyvolvo + sbr + Ohp, data = car_training)
summary(model_2)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_2)

## droping car_id, curbweight, carlength, enginesize, carbodysedan, carwidth, Wheelbase
## stroke, carbodysedan as VIF is too high 

model_3 <- lm(price ~  aspiration + doornumber + enginelocation + 
                carheight + curbweight + 
                peakrpm + carbodyhardtop + carbodyhatchback + 
                carbodywagon + drivewheelrwd + enginetypel + 
                enginetypeohcf + cylindernumber + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                Companybmw + Companychevrolet + Companydodge + Companyisuzu + 
                Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                Companyplymouth + Companyrenault + Companytoyota + Companyvolkswagen + 
                Companyvolvo + sbr + Ohp, data = car_training)
summary(model_3)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_3)

## peakrpm, doornumber, carheight, drivewheelrwd, companymercury & sbr low significance

model_4 <- lm(price ~  aspiration + enginelocation + curbweight + 
                carbodyhardtop + carbodyhatchback + 
                carbodywagon  + enginetypel + 
                enginetypeohcf + cylindernumber + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                Companybmw + Companychevrolet + Companydodge + Companyisuzu + 
                Companymazda + Companymitsubishi + Companynissan + 
                Companyplymouth + Companyrenault + Companytoyota + Companyvolkswagen + 
                Companyvolvo  + Ohp, data = car_training)
summary(model_4)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_4)

## droping carbodyhatchback,fuelsystemspdi, companychevrolet as it has high VIF
## and low significance

model_5 <- lm(price ~  aspiration + enginelocation + curbweight + 
                carbodyhardtop  +  carbodywagon  + enginetypel +
                enginetypeohcf + cylindernumber + fuelsystem2bbl + fuelsystemmpfi + 
                Companybmw + Companydodge + Companyisuzu + 
                Companymazda + Companymitsubishi + Companynissan + 
                Companyplymouth + Companyrenault + Companytoyota + Companyvolkswagen + 
                Companyvolvo  + Ohp, data = car_training)
summary(model_5)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_5)

## droping companyisuzu, companydodge as these have low significance

model_6 <- lm(price ~  aspiration + enginelocation + curbweight + 
                carbodyhardtop  +  carbodywagon  + enginetypel +
                enginetypeohcf + cylindernumber+ fuelsystem2bbl + fuelsystemmpfi + 
                Companybmw + Companymazda + Companymitsubishi + Companynissan +
                Companyplymouth + Companyrenault + Companytoyota + Companyvolkswagen + 
                Companyvolvo  + Ohp, data = car_training)
summary(model_6)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_6)

## droping aspirations as this low significance

model_7 <- lm(price ~ enginelocation + curbweight + 
                carbodyhardtop  +  carbodywagon  + enginetypel +
                enginetypeohcf + cylindernumber+ fuelsystem2bbl + fuelsystemmpfi + 
                Companybmw + Companymazda + Companymitsubishi + Companynissan +
                Companyplymouth + Companyrenault + Companytoyota + Companyvolkswagen + 
                Companyvolvo  + Ohp, data = car_training)
summary(model_7)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_7)

## droping fuelsystemmpfi as it has high VIF low significance

model_8 <- lm(price ~ enginelocation + curbweight + 
                carbodyhardtop  +  carbodywagon  + enginetypel +
                enginetypeohcf + cylindernumber+ fuelsystem2bbl + 
                Companybmw + Companymazda + Companymitsubishi + Companynissan +
                Companyplymouth + Companyrenault + Companytoyota + Companyvolkswagen + 
                Companyvolvo  + Ohp, data = car_training)
summary(model_8)


## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_8)

## droping fuelsystem2bbl and company - renault,mazda, volvo, & Ohp 
## as they have low significance 

model_9 <- lm(price ~ enginelocation + curbweight + 
                carbodyhardtop  +  carbodywagon  + enginetypel +
                enginetypeohcf + cylindernumber + 
                Companybmw + Companymitsubishi + Companynissan +
                Companyplymouth + Companytoyota + Companyvolkswagen 
                , data = car_training)
summary(model_9)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_9)

## droping enginetypel,Companyplymouth,Companyvolkswagen as it has low significance and slightly higher VIF

model_10 <- lm(price ~ enginelocation + curbweight + 
                carbodyhardtop  +  carbodywagon   +
                enginetypeohcf + cylindernumber + 
                Companybmw + Companymitsubishi + Companynissan 
                 + Companytoyota 
              , data = car_training)

summary(model_10)


## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_10)

## droping enginetypeohcf as it has low significance 
## and slightly higher VIF
model_11 <- lm(price ~ enginelocation + curbweight + 
                 carbodyhardtop  +  carbodywagon   
                  + cylindernumber + 
                 Companybmw + Companymitsubishi + Companynissan 
               + Companytoyota 
               , data = car_training)
summary(model_11)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_11)


## As now our model has only significant parameters in our apart from company so 
## we can use this model for our prediction. 

## NOTE: Company names have not been altered as the manufacturer may have their
## sales based on their brand values too which is not covered in this model and 
## prices are mostly brand specific.

# predicting the results in test dataset

Predict_1 <- predict(model_11,car_testing[,-20])
car_testing$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(car_testing$price,car_testing$test_price)
rsquared <- cor(car_testing$price,car_testing$test_price)^2
rsquared
