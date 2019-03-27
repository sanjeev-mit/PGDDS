# Creating Base Data set

#lag sale for last three weeks
wkly_order_ad_data_with_lag <- weekly_Eleckart_order_data%>% arrange ( week_no) %>% group_by(product_analytic_sub_category ,product_analytic_vertical) %>%  mutate(lag_1_gmv = lag(tot_gmv, 1))  %>%  mutate(lag_2_gmv = lag(tot_gmv, 2)) %>%  mutate(lag_3_gmv = lag(tot_gmv, 3)) 

#change in sale in last three weeks
wkly_order_ad_data_with_lag <- wkly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category ,product_analytic_vertical ) %>%  mutate(change_wrt_w1_gmv = (tot_gmv-lag(tot_gmv, 1))/tot_gmv) %>%  mutate(change_wrt_w2_gmv = (tot_gmv-lag(tot_gmv, 2))/tot_gmv) %>%  mutate(change_wrt_w3_gmv = (tot_gmv-lag(tot_gmv, 3))/tot_gmv) 

## lag list price for last 3 weeks
wkly_order_ad_data_with_lag <- wkly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(lag_1_lp = lag(list_price, 1))  %>%  mutate(lag_2_lp = lag(list_price, 2)) %>%  mutate(lag_3_lp = lag(list_price, 3)) 

##  change in list price change for last 3 weeks
wkly_order_ad_data_with_lag <- wkly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(price_change_wrt_w1 = (list_price-lag(list_price, 1))/list_price) %>%  mutate(price_change_wrt_w2 = (list_price-lag(list_price, 2))/list_price) %>%  mutate(price_change_wrt_w3 = (list_price-lag(list_price, 3))/list_price)

#### TV ad stcok - 60% effect of current week is propagating to next week
wkly_order_ad_data_with_lag <- wkly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(tv_ad_stock = TV+ if_else ( is.na (lag(TV, 1)*.6),0, lag(TV, 1)*.6) + if_else ( is.na (lag(TV, 2)*.36),0, lag(TV, 2)*.36) + if_else ( is.na (lag(TV, 3)*.22),0, lag(TV, 3)*.22) + if_else ( is.na (lag(TV, 4)*.13),0, lag(TV, 4)*.13)  + if_else ( is.na (lag(TV, 5)*.07),0, lag(TV, 5)*.07)  + if_else ( is.na (lag(TV, 6)*.05),0, lag(TV, 6)*.05) ) 

####20% effect of current week is propagating to next week for other advertisments
## Digital ad stcok.
wkly_order_ad_data_with_lag <- wkly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(dig_ad_stock = Digital+ if_else ( is.na (lag(Digital, 1)*.2),0, lag(Digital, 1)*.2) + if_else ( is.na (lag(Digital, 2)*.04),0, lag(Digital, 2)*.04)   )

##sponsorship ad stcok. 
wkly_order_ad_data_with_lag <- wkly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(spon_ad_stock = Sponsorship+ if_else ( is.na (lag(Sponsorship, 1)*.2),0, lag(Sponsorship, 1)*.2) + if_else ( is.na (lag(Sponsorship, 2)*.04),0, lag(Sponsorship, 2)*.04)   )  

##content marketing ad stcok. 
wkly_order_ad_data_with_lag <- wkly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(content_ad_stock = Content.Marketing+ if_else ( is.na (lag(Content.Marketing, 1)*.2),0, lag(Content.Marketing, 1)*.2) + if_else ( is.na (lag(Content.Marketing, 2)*.04),0, lag(Content.Marketing, 2)*.04)   ) 

## online marketing ad stcok. 
wkly_order_ad_data_with_lag <- wkly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(online_ad_stock = Online.marketing+ if_else ( is.na (lag(Online.marketing, 1)*.2),0, lag(Online.marketing, 1)*.2) + if_else ( is.na (lag(Online.marketing, 2)*.04),0, lag(Online.marketing, 2)*.04)   )

##Radio ad stcok. 
wkly_order_ad_data_with_lag <- wkly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(radio_ad_stock = Radio+ if_else ( is.na (lag(SEM, 1)*.2),0, lag(Radio, 1)*.2) + if_else ( is.na (lag(Radio, 2)*.04),0, lag(Radio, 2)*.04)   )

##SEM ad stcok. 
wkly_order_ad_data_with_lag <- wkly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(sem_ad_stock = SEM+ if_else ( is.na (lag(SEM, 1)*.2),0, lag(SEM, 1)*.2) + if_else ( is.na (lag(SEM, 2)*.04),0, lag(SEM, 2)*.04)   )

##Affiliate  ad stcok. 
wkly_order_ad_data_with_lag <- wkly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(affiliate_ad_stock = X.Affiliates+ if_else ( is.na (lag(X.Affiliates, 1)*.2),0, lag(X.Affiliates, 1)*.2) + if_else ( is.na (lag(X.Affiliates, 2)*.04),0, lag(X.Affiliates, 2)*.04)   )

## Filter out CameraAccessory", GamingAccessory and  HomeAudio sub category
wkly_order_ad_data_with_lag <- wkly_order_ad_data_with_lag %>% filter ( product_analytic_sub_category %in% c("CameraAccessory", "GamingAccessory", "HomeAudio"))
wkly_order_ad_data_with_lag <- as.data.frame(wkly_order_ad_data_with_lag)



str(wkly_order_ad_data_with_lag)
## Dummy variable creation for character data types
wkly_order_ad_data_with_lag_chr <- wkly_order_ad_data_with_lag[,c(5,32,34)]

###Converting to factor
wkly_order_ad_data_with_lag_fact <- data.frame(sapply(wkly_order_ad_data_with_lag_chr, function(x) factor(x)))
str(wkly_order_ad_data_with_lag_fact)

# creating dummy variables for factor attributes
dummies_DLM<- data.frame(sapply(wkly_order_ad_data_with_lag_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =wkly_order_ad_data_with_lag_fact))[,-1]))

## Create master data set by appending dummies with main data set 
wkly_order_ad_data_with_lag_overall <- cbind ( wkly_order_ad_data_with_lag[, -c(5,32,34)], dummies_DLM )  
View(wkly_order_ad_data_with_lag_overall) 


###Correlation Check
cor_var_DLM <- cor ( wkly_order_ad_data_with_lag_overall[-4])
## Since these variables are highly corelated  or direct proxy to sales , so taking them out 
wkly_order_ad_data_with_lag_overall$tot_week <- NULL 
wkly_order_ad_data_with_lag_overall$total_row <- NULL
wkly_order_ad_data_with_lag_overall$avg_mrp <- NULL
wkly_order_ad_data_with_lag_overall$avg_price <- NULL
wkly_order_ad_data_with_lag_overall$tot_units <- NULL
wkly_order_ad_data_with_lag_overall$no_of_orders <- NULL
wkly_order_ad_data_with_lag_overall$tot_product_mrp <- NULL
wkly_order_ad_data_with_lag_overall$avg_gmv <- NULL
wkly_order_ad_data_with_lag_overall$value_per_visitor <- NULL
wkly_order_ad_data_with_lag_overall$Year <- NULL
wkly_order_ad_data_with_lag_overall$no_of_customer <- NULL
wkly_order_ad_data_with_lag_overall$delayed_delivery_cnt <- NULL
wkly_order_ad_data_with_lag_overall$early_delivery_cnt <- NULL
wkly_order_ad_data_with_lag_overall$onetime_delivery_cnt <- NULL
wkly_order_ad_data_with_lag_overall$cod_cnt <- NULL
wkly_order_ad_data_with_lag_overall$prepaid_cnt <- NULL


## Create 3 data set for 3 subcategories HomeAudio, GamingAccessory and CameraAccessory  

Camera_DLM <- filter ( wkly_order_ad_data_with_lag_overall ,wkly_order_ad_data_with_lag_overall$product_analytic_sub_category %in% c("CameraAccessory")) 
Camera_DLM$product_analytic_sub_category <- NULL

GamingAccessory_DLM <- filter ( wkly_order_ad_data_with_lag_overall ,wkly_order_ad_data_with_lag_overall$product_analytic_sub_category %in% c("GamingAccessory")) 
GamingAccessory_DLM$product_analytic_sub_category <- NULL

HomeAudio_DLM <- filter ( wkly_order_ad_data_with_lag_overall ,wkly_order_ad_data_with_lag_overall$product_analytic_sub_category %in% c("HomeAudio")) 
HomeAudio_DLM$product_analytic_sub_category <- NULL


#==================================================================================================================
#                                    DISTRIBUTED LAG MODEL 
#==================================================================================================================


################################################################################
#                                      CameraAccessory
################################################################################
Camera_DLM1<- Camera_DLM
Camera_DLM <-na.omit(Camera_DLM)
Camera_DLM$week_no <- NULL
Camera_DLM$Month <- NULL
#Scaling the dataset
Camera_DLM.df <-Camera_DLM
Camera_DLM.df[,1:33] <- data.frame(scale(Camera_DLM.df[,1:33], center = TRUE))

########## separate training and testing data
set.seed(100)
trainindices_cam_dlm= sample(1:nrow(Camera_DLM.df), 0.7*nrow(Camera_DLM.df))
train_CameraAccessory_dlm = Camera_DLM.df[trainindices_cam_dlm,]
test_CameraAccessory_dlm = Camera_DLM.df[-trainindices_cam_dlm,]
nrow(train_CameraAccessory_dlm) #614 obs
nrow(test_CameraAccessory_dlm) #264 obs.

########## Build model 1 containing all variables
cam_dlm_model_1 <-lm(tot_gmv~.,data=train_CameraAccessory_dlm)
summary(cam_dlm_model_1) # Adjusted R-squared:  0.7623

##########Use stepAIC 
step_cam_dlm <- stepAIC(cam_dlm_model_1, direction="both")
#Step:  AIC=-876.71
summary(step_cam_dlm) #0.7668 
vif(step_cam_dlm)
alias(step_cam_dlm)

cam_dlm_model_2 <- lm(tot_gmv ~ list_price + TV + Digital + Sponsorship + Content.Marketing + 
                        Online.marketing + X.Affiliates + discount_over_mrp + lag_2_gmv + 
                        lag_3_gmv + lag_1_lp + dig_ad_stock + online_ad_stock + sem_ad_stock + 
                        affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                        product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                        product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                        product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale + 
                        change_wrt_w1_gmv + price_change_wrt_w1,data=train_CameraAccessory_dlm)


summary(cam_dlm_model_2) #0.7668 
vif(cam_dlm_model_2)

#Removing online_ad_stock having high p value and high VIF
cam_dlm_model_3 <- lm(tot_gmv ~ list_price + TV + Digital + Sponsorship + Content.Marketing + 
                        Online.marketing + X.Affiliates + discount_over_mrp + lag_2_gmv + 
                        lag_3_gmv + lag_1_lp + dig_ad_stock + sem_ad_stock + 
                        affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                        product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                        product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                        product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale + 
                        change_wrt_w1_gmv + price_change_wrt_w1,data=train_CameraAccessory_dlm)


summary(cam_dlm_model_3)
vif(cam_dlm_model_3)

#Removing Online.marketing having high p value and high VIF
cam_dlm_model_4 <- lm(tot_gmv ~ list_price + TV + Digital + Sponsorship + Content.Marketing + 
                        X.Affiliates + discount_over_mrp + lag_2_gmv + 
                        lag_3_gmv + lag_1_lp + dig_ad_stock + sem_ad_stock + 
                        affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                        product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                        product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                        product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale + 
                        change_wrt_w1_gmv + price_change_wrt_w1,data=train_CameraAccessory_dlm)


summary(cam_dlm_model_4) 
vif(cam_dlm_model_4)

#Removing affiliate_ad_stock having high p value and high VIF
cam_dlm_model_5 <- lm(tot_gmv ~ list_price + TV + Digital + Sponsorship + Content.Marketing + 
                        X.Affiliates + discount_over_mrp + lag_2_gmv + 
                        lag_3_gmv + lag_1_lp + dig_ad_stock + sem_ad_stock + 
                        product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                        product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                        product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                        product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale + 
                        change_wrt_w1_gmv + price_change_wrt_w1,data=train_CameraAccessory_dlm)


summary(cam_dlm_model_5) 
vif(cam_dlm_model_5)


#Removing sem_ad_stock having high p value and high VIF
cam_dlm_model_6 <- lm(tot_gmv ~ list_price + TV + Digital + Sponsorship + Content.Marketing + 
                        X.Affiliates + discount_over_mrp + lag_2_gmv + 
                        lag_3_gmv + lag_1_lp + dig_ad_stock +  
                        product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                        product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                        product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                        product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale + 
                        change_wrt_w1_gmv + price_change_wrt_w1,data=train_CameraAccessory_dlm)


summary(cam_dlm_model_6) 
vif(cam_dlm_model_6)



#Removing dig_ad_stock having high p value and high VIF
cam_dlm_model_7 <- lm(tot_gmv ~ list_price + TV + Digital + Sponsorship + Content.Marketing + 
                        X.Affiliates + discount_over_mrp + lag_2_gmv + 
                        lag_3_gmv + lag_1_lp +   
                        product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                        product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                        product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                        product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale + 
                        change_wrt_w1_gmv + price_change_wrt_w1,data=train_CameraAccessory_dlm)


summary(cam_dlm_model_7) 
vif(cam_dlm_model_7)

cor(train_CameraAccessory_dlm$Digital ,train_CameraAccessory_dlm$Content.Marketing) #0.909 highly correlated


#Removing Digital as it is highly correlated to  Content.Marketing andhence as hIGH VIF
cam_dlm_model_8 <- lm(tot_gmv ~ list_price + TV + Sponsorship + Content.Marketing + 
                        X.Affiliates + discount_over_mrp + lag_2_gmv + 
                        lag_3_gmv + lag_1_lp +   
                        product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                        product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                        product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                        product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale + 
                        change_wrt_w1_gmv + price_change_wrt_w1,data=train_CameraAccessory_dlm)


summary(cam_dlm_model_8) 
vif(cam_dlm_model_8)

#Removing lag_1_lp having high p and high VIF 
cam_dlm_model_9 <- lm(tot_gmv ~ list_price + TV + Sponsorship + Content.Marketing + 
                        X.Affiliates + discount_over_mrp + lag_2_gmv + lag_3_gmv +    
                        product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                        product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                        product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                        product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale + 
                        change_wrt_w1_gmv + price_change_wrt_w1,data=train_CameraAccessory_dlm)


summary(cam_dlm_model_9) 
vif(cam_dlm_model_9)


#Removing price_change_wrt_w1 having high p 
cam_dlm_model_10 <- lm(tot_gmv ~ list_price + TV + Sponsorship + Content.Marketing + 
                         X.Affiliates + discount_over_mrp + lag_2_gmv + lag_3_gmv +    
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBag + product_analytic_vertical.xCameraBattery + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale + 
                         change_wrt_w1_gmv ,data=train_CameraAccessory_dlm)


summary(cam_dlm_model_10) 
vif(cam_dlm_model_10)

#Removing list_price having high p 
cam_dlm_model_11 <- lm(tot_gmv ~ TV + Sponsorship + Content.Marketing + 
                         X.Affiliates + discount_over_mrp + lag_2_gmv + lag_3_gmv +    
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBag + product_analytic_vertical.xCameraBattery + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale + 
                         change_wrt_w1_gmv ,data=train_CameraAccessory_dlm)

summary(cam_dlm_model_11) 
vif(cam_dlm_model_11)


#Removing product_analytic_vertical.xCameraBag having high p 
cam_dlm_model_12 <- lm(tot_gmv ~ TV + Sponsorship + Content.Marketing + 
                         X.Affiliates + discount_over_mrp + lag_2_gmv + lag_3_gmv +    
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBattery + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale + 
                         change_wrt_w1_gmv ,data=train_CameraAccessory_dlm)


summary(cam_dlm_model_12) 
vif(cam_dlm_model_12)


#Removing change_wrt_w1_gmv ,product_analytic_vertical.xCameraBattery  having high p 
cam_dlm_model_13 <- lm(tot_gmv ~ TV + Sponsorship + Content.Marketing + 
                         X.Affiliates + discount_over_mrp + lag_2_gmv + lag_3_gmv +    
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale ,data=train_CameraAccessory_dlm)


summary(cam_dlm_model_13) 
vif(cam_dlm_model_13)


#Removing product_analytic_vertical.xFlash  having high p value
cam_dlm_model_14 <- lm(tot_gmv ~ Digital + Sponsorship + 
                         discount_over_mrp + lag_2_gmv + lag_3_gmv +   
                         affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale + 
                         change_wrt_w1_gmv ,data=train_CameraAccessory_dlm)


summary(cam_dlm_model_14) 
vif(cam_dlm_model_14)

#Removing affiliate_ad_stock highly  having high p value
cam_dlm_model_15 <- lm(tot_gmv ~ Digital + Sponsorship + 
                         discount_over_mrp + lag_2_gmv + lag_3_gmv +   
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale + 
                         change_wrt_w1_gmv ,data=train_CameraAccessory_dlm)


summary(cam_dlm_model_15) 
vif(cam_dlm_model_15)

#Removing discount_over_mrp ,change_wrt_w1_gmv  having high p value and high VIF
cam_dlm_model_16 <- lm(tot_gmv ~ Digital + Sponsorship + 
                          lag_2_gmv + lag_3_gmv +   
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale ,data=train_CameraAccessory_dlm)


summary(cam_dlm_model_16) 
vif(cam_dlm_model_16)

#Removing product_analytic_vertical.xCameraTripod having high p value
cam_dlm_model_17 <- lm(tot_gmv ~ Digital + Sponsorship + 
                         lag_2_gmv + lag_3_gmv +   
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale ,data=train_CameraAccessory_dlm)


summary(cam_dlm_model_17) 
vif(cam_dlm_model_17)

#Removing lag_2_gmv having high p value
cam_dlm_model_18 <- lm(tot_gmv ~ Digital + Sponsorship + lag_3_gmv +   
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale ,data=train_CameraAccessory_dlm)


summary(cam_dlm_model_18) #0.754
vif(cam_dlm_model_18)



Final_camaccesory_dlm<-cam_dlm_model_18

##################Test the model on test data#############################
#######  taking final model as cam_dlm_model_18 ##############
#value of Adjusted R-squared of cam_dlm_model_18 is ~ 75%.

#Now, let us move forward to test the model on test data. 
Predict<-predict(Final_camaccesory_dlm, test_CameraAccessory_dlm[,-1])
#r-squared value between the predicted and actual values of sales
(cor(test_CameraAccessory_dlm$tot_gmv,Predict))^2 # 72.13% ~72%


########## Calculate Elaticity ################################
#To calculate Price Elasticity of Demand we use the formula:
#  PE = (??Q/??P) * (P/Q)
#(??Q/??P) is determined by the coefficient in our regression formula.
#To determine (P/Q) we will use the mean of independent variable.

elasticity <- function(var) {
  x <- as.numeric(Final_camaccesory_dlm$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(Final_camaccesory_dlm$coefficients)) {
  var_list[i-1] <- elasticity(names(Final_camaccesory_dlm$coefficients)[i])
}

elasticity.outputs <- data.frame(names(Final_camaccesory_dlm$coefficients[2:length(Final_camaccesory_dlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)

ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory- Lag Model") +xlab("Variables")

###################  Cross validation ###############################


cam_dlm_cv <- cv.lm(data = train_CameraAccessory_dlm, form.lm = formula(tot_gmv ~ Digital + Sponsorship + lag_3_gmv +   
                                                                          product_analytic_vertical.xCameraAccessory + 
                                                                          product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                                                                          product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                                                                          product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                                                                          product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                                                                          product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                                                                          product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                                                                          product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                                                                          promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                                                                          promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale ), m =10)

#ms = 0.275


################################################################################
#                                      HomeAudio
################################################################################
HomeAudio_DLM1 <- HomeAudio_DLM
HomeAudio_DLM <-na.omit(HomeAudio_DLM)
HomeAudio_DLM$week_no <- NULL
HomeAudio_DLM$Month <- NULL
#Scaling the dataset
HomeAudio_DLM.df <-HomeAudio_DLM
HomeAudio_DLM.df[,1:33] <- data.frame(scale(HomeAudio_DLM.df[,1:33], center = TRUE))

########## separate training and testing data
set.seed(100)
trainindices_HA_dlm= sample(1:nrow(HomeAudio_DLM.df), 0.7*nrow(HomeAudio_DLM.df))
train_HomeAudio_dlm = HomeAudio_DLM.df[trainindices_HA_dlm,]
test_HomeAudio_dlm = HomeAudio_DLM.df[-trainindices_HA_dlm,]
nrow(train_HomeAudio_dlm) #300 obs
nrow(test_HomeAudio_dlm) #129 obs.

########## Build model 1 containing all variables
HA_dlm_model_1 <-lm(tot_gmv~.,data=train_HomeAudio_dlm)
summary(HA_dlm_model_1) # Adjusted R-squared:  0.5871

##########Use stepAIC 
step_HA_dlm <- stepAIC(HA_dlm_model_1, direction="both")
#Step:  AIC=-287.35
summary(step_HA_dlm) #0.6228 
vif(step_HA_dlm)
alias(step_HA_dlm)

HA_dlm_model_2 <-lm(tot_gmv ~ TV + Digital + SEM + Radio + Other + discount_over_mrp + 
                      lag_3_gmv + change_wrt_w1_gmv + change_wrt_w2_gmv + product_analytic_vertical.xBoomBox + 
                      product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                      product_analytic_vertical.xHomeAudioSpeaker + promotion_type.xChristmas...New.Year.Sale + 
                      promotion_type.xDaussera.sale + promotion_type.xNo_promotion,data=train_HomeAudio_dlm)

summary(HA_dlm_model_2) 
vif(HA_dlm_model_2)

#Removing Digital having high p value and high vif
HA_dlm_model_3 <-lm(tot_gmv ~ TV + SEM + Radio + Other + discount_over_mrp + 
                      lag_3_gmv + change_wrt_w1_gmv + change_wrt_w2_gmv + product_analytic_vertical.xBoomBox + 
                      product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                      product_analytic_vertical.xHomeAudioSpeaker + promotion_type.xChristmas...New.Year.Sale + 
                      promotion_type.xDaussera.sale + promotion_type.xNo_promotion,data=train_HomeAudio_dlm)

summary(HA_dlm_model_3) 
vif(HA_dlm_model_3)

cor(train_HomeAudio_dlm$change_wrt_w1_gmv , train_HomeAudio_dlm$change_wrt_w2_gmv) # 0.99 ighly correlated

#removing change_wrt_w1_gmv highly corrlated to change_wrt_w2_gmv
HA_dlm_model_4 <-lm(tot_gmv ~ TV + SEM + Radio + Other + discount_over_mrp + 
                      lag_3_gmv + change_wrt_w2_gmv + product_analytic_vertical.xBoomBox + 
                      product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                      product_analytic_vertical.xHomeAudioSpeaker + promotion_type.xChristmas...New.Year.Sale + 
                      promotion_type.xDaussera.sale + promotion_type.xNo_promotion,data=train_HomeAudio_dlm)

summary(HA_dlm_model_4) 
vif(HA_dlm_model_4)


#removing other having high p value and high vif
HA_dlm_model_5 <-lm(tot_gmv ~ TV + SEM + Radio + discount_over_mrp + 
                      lag_3_gmv + change_wrt_w2_gmv + product_analytic_vertical.xBoomBox + 
                      product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                      product_analytic_vertical.xHomeAudioSpeaker + promotion_type.xChristmas...New.Year.Sale + 
                      promotion_type.xDaussera.sale + promotion_type.xNo_promotion,data=train_HomeAudio_dlm)

summary(HA_dlm_model_5) 
vif(HA_dlm_model_5)

#removing Radio having high p value and high vif
HA_dlm_model_6 <-lm(tot_gmv ~ TV + SEM + discount_over_mrp + 
                      lag_3_gmv + change_wrt_w2_gmv + product_analytic_vertical.xBoomBox + 
                      product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                      product_analytic_vertical.xHomeAudioSpeaker + promotion_type.xChristmas...New.Year.Sale + 
                      promotion_type.xDaussera.sale + promotion_type.xNo_promotion,data=train_HomeAudio_dlm)

summary(HA_dlm_model_6) 
vif(HA_dlm_model_6)


#removing promotion_type.xChristmas...New.Year.Sale having high p value and high vif
HA_dlm_model_7 <-lm(tot_gmv ~ TV + SEM + discount_over_mrp + 
                      lag_3_gmv + change_wrt_w2_gmv + product_analytic_vertical.xBoomBox + 
                      product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                      product_analytic_vertical.xHomeAudioSpeaker + 
                      promotion_type.xDaussera.sale + promotion_type.xNo_promotion,data=train_HomeAudio_dlm)

summary(HA_dlm_model_7) 
vif(HA_dlm_model_7)

#removing TV having high p value and high vif
HA_dlm_model_8 <-lm(tot_gmv ~ SEM + discount_over_mrp + 
                      lag_3_gmv + change_wrt_w2_gmv + product_analytic_vertical.xBoomBox + 
                      product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                      product_analytic_vertical.xHomeAudioSpeaker + 
                      promotion_type.xDaussera.sale + promotion_type.xNo_promotion,data=train_HomeAudio_dlm)

summary(HA_dlm_model_8) 
vif(HA_dlm_model_8)

#removing SEM having high p value 
HA_dlm_model_9 <-lm(tot_gmv ~ discount_over_mrp + 
                      lag_3_gmv + change_wrt_w2_gmv + product_analytic_vertical.xBoomBox + 
                      product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                      product_analytic_vertical.xHomeAudioSpeaker + 
                      promotion_type.xDaussera.sale + promotion_type.xNo_promotion,data=train_HomeAudio_dlm)

summary(HA_dlm_model_9) 
vif(HA_dlm_model_9)

#removing lag_3_gmv having high p value 
HA_dlm_model_10 <-lm(tot_gmv ~ discount_over_mrp + 
                      change_wrt_w2_gmv + product_analytic_vertical.xBoomBox + 
                      product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                      product_analytic_vertical.xHomeAudioSpeaker + 
                      promotion_type.xDaussera.sale + promotion_type.xNo_promotion,data=train_HomeAudio_dlm)

summary(HA_dlm_model_10) 
vif(HA_dlm_model_10)



#removing product_analytic_vertical.xBoomBox having high p value 
HA_dlm_model_11 <-lm(tot_gmv ~ discount_over_mrp + 
                       change_wrt_w2_gmv +
                       product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                       product_analytic_vertical.xHomeAudioSpeaker + 
                       promotion_type.xDaussera.sale + promotion_type.xNo_promotion,data=train_HomeAudio_dlm)

summary(HA_dlm_model_11) 
vif(HA_dlm_model_11)


#removing product_analytic_vertical.xFMRadio having high p value 
HA_dlm_model_12 <-lm(tot_gmv ~ discount_over_mrp + 
                       change_wrt_w2_gmv +
                       product_analytic_vertical.xDock + 
                       product_analytic_vertical.xHomeAudioSpeaker + 
                       promotion_type.xDaussera.sale + promotion_type.xNo_promotion,data=train_HomeAudio_dlm)

summary(HA_dlm_model_12) 
vif(HA_dlm_model_12)

#removing promotion_type.xNo_promotion having high p value 
HA_dlm_model_13 <-lm(tot_gmv ~ discount_over_mrp + 
                       change_wrt_w2_gmv +
                       product_analytic_vertical.xDock + 
                       product_analytic_vertical.xHomeAudioSpeaker + 
                       promotion_type.xDaussera.sale ,data=train_HomeAudio_dlm)

summary(HA_dlm_model_13) 
vif(HA_dlm_model_13)


#removing discount_over_mrp ,product_analytic_vertical.xDock having high p value 
HA_dlm_model_14 <-lm(tot_gmv ~ change_wrt_w2_gmv +product_analytic_vertical.xHomeAudioSpeaker + 
                       promotion_type.xDaussera.sale ,data=train_HomeAudio_dlm)

summary(HA_dlm_model_14)  #0.5988 
vif(HA_dlm_model_14)

Final_HA_dlm<-HA_dlm_model_14

##################Test the model on test data#############################
#######  taking final model as HA_dlm_model_14 ##############
#value of Adjusted R-squared of HA_dlm_model_14 is ~ 60%.

#Now, let us move forward to test the model on test data. 
Predict<-predict(Final_HA_dlm, test_HomeAudio_dlm[,-1])
#r-squared value between the predicted and actual values of sales
(cor(test_HomeAudio_dlm$tot_gmv,Predict))^2 # 71.2% ~71%


########## Calculate Elaticity ################################
#To calculate Price Elasticity of Demand we use the formula:
#  PE = (??Q/??P) * (P/Q)
#(??Q/??P) is determined by the coefficient in our regression formula.
#To determine (P/Q) we will use the mean of independent variable.
elasticity <- function(var) {
  x <- as.numeric(Final_HA_dlm$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(Final_HA_dlm$coefficients)) {
  var_list[i-1] <- elasticity(names(Final_HA_dlm$coefficients)[i])
}

elasticity.outputs <- data.frame(names(Final_HA_dlm$coefficients[2:length(Final_HA_dlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)

ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio- Lag Model") +xlab("Variables")


###################  Cross validation ###############################


HA_dlm_cv <- cv.lm(data = train_HomeAudio_dlm, form.lm = formula(tot_gmv ~ change_wrt_w2_gmv +product_analytic_vertical.xHomeAudioSpeaker + 
                                                                          promotion_type.xDaussera.sale), m =10)

#ms = 1.26


################################################################################
#                                      GamingAccessories
################################################################################
GamingAccessory_DLM1 <- GamingAccessory_DLM
GamingAccessory_DLM <-na.omit(GamingAccessory_DLM)
GamingAccessory_DLM$week_no <- NULL
GamingAccessory_DLM$Month <- NULL
#Scaling the dataset
GamingAccessory_DLM.df <-GamingAccessory_DLM
GamingAccessory_DLM.df[,1:33] <- data.frame(scale(GamingAccessory_DLM.df[,1:33], center = TRUE))

########## separate training and testing data
set.seed(100)
trainindices_GA_dlm= sample(1:nrow(GamingAccessory_DLM.df), 0.7*nrow(GamingAccessory_DLM.df))
train_GA_dlm = GamingAccessory_DLM.df[trainindices_GA_dlm,]
test_GA_dlm = GamingAccessory_DLM.df[-trainindices_GA_dlm,]
nrow(train_GA_dlm) #467 obs
nrow(test_GA_dlm) #201 obs.

########## Build model 1 containing all variables
GA_dlm_model_1 <-lm(tot_gmv~.,data=train_GA_dlm)
summary(GA_dlm_model_1) # Adjusted R-squared:  0.75

##########Use stepAIC 
step_GA_dlm <- stepAIC(GA_dlm_model_1, direction="both")
#Step:  AIC=-605
summary(step_GA_dlm) #0.6228 
vif(step_GA_dlm)
alias(step_GA_dlm)

GA_dlm_model_2 <-lm(tot_gmv ~ TV + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                      X.Affiliates + Other + NPS + lag_3_gmv + change_wrt_w1_gmv + 
                      change_wrt_w2_gmv + dig_ad_stock + content_ad_stock + online_ad_stock + 
                      radio_ad_stock + sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                      product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                      product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                      product_analytic_vertical.xJoystickGamingWheel + promotion_type.xChristmas...New.Year.Sale + 
                      promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                      promotion_type.xPacman + promotion_type.xRakshabandhan.Sale + 
                      promotion_type.xValentine.s.Day + product_analytic_vertical.xGamingAccessoryKit,data=train_GA_dlm)

summary(GA_dlm_model_2) 
vif(GA_dlm_model_2)

 #Removing affiliate_ad_stock due to its extremely high VIF
GA_dlm_model_3 <-lm(tot_gmv ~ TV + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                      X.Affiliates + Other + NPS + lag_3_gmv + change_wrt_w1_gmv + 
                      change_wrt_w2_gmv + dig_ad_stock + content_ad_stock + online_ad_stock + 
                      radio_ad_stock + sem_ad_stock + product_analytic_vertical.xGamePad + 
                      product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                      product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                      product_analytic_vertical.xJoystickGamingWheel + promotion_type.xChristmas...New.Year.Sale + 
                      promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                      promotion_type.xPacman + promotion_type.xRakshabandhan.Sale + 
                      promotion_type.xValentine.s.Day + product_analytic_vertical.xGamingAccessoryKit,data=train_GA_dlm)

summary(GA_dlm_model_3) 
vif(GA_dlm_model_3)

#Removing X.Affiliates due to its extremely high VIF and high p value
GA_dlm_model_4 <-lm(tot_gmv ~ TV + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                      Other + NPS + lag_3_gmv + change_wrt_w1_gmv + 
                      change_wrt_w2_gmv + dig_ad_stock + content_ad_stock + online_ad_stock + 
                      radio_ad_stock + sem_ad_stock + product_analytic_vertical.xGamePad + 
                      product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                      product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                      product_analytic_vertical.xJoystickGamingWheel + promotion_type.xChristmas...New.Year.Sale + 
                      promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                      promotion_type.xPacman + promotion_type.xRakshabandhan.Sale + 
                      promotion_type.xValentine.s.Day + product_analytic_vertical.xGamingAccessoryKit,data=train_GA_dlm)

summary(GA_dlm_model_4) 
vif(GA_dlm_model_4)

#Removing dig_ad_stock due to its extremely high VIF 
GA_dlm_model_5 <-lm(tot_gmv ~ TV + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                      Other + NPS + lag_3_gmv + change_wrt_w1_gmv + 
                      change_wrt_w2_gmv + content_ad_stock + online_ad_stock + 
                      radio_ad_stock + sem_ad_stock + product_analytic_vertical.xGamePad + 
                      product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                      product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                      product_analytic_vertical.xJoystickGamingWheel + promotion_type.xChristmas...New.Year.Sale + 
                      promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                      promotion_type.xPacman + promotion_type.xRakshabandhan.Sale + 
                      promotion_type.xValentine.s.Day + product_analytic_vertical.xGamingAccessoryKit,data=train_GA_dlm)

summary(GA_dlm_model_5) 
vif(GA_dlm_model_5)

#Removing Digital due to its extremely high VIF  and igh p value
GA_dlm_model_6 <-lm(tot_gmv ~ TV + Sponsorship + Content.Marketing + Online.marketing + 
                      Other + NPS + lag_3_gmv + change_wrt_w1_gmv + 
                      change_wrt_w2_gmv + content_ad_stock + online_ad_stock + 
                      radio_ad_stock + sem_ad_stock + product_analytic_vertical.xGamePad + 
                      product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                      product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                      product_analytic_vertical.xJoystickGamingWheel + promotion_type.xChristmas...New.Year.Sale + 
                      promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                      promotion_type.xPacman + promotion_type.xRakshabandhan.Sale + 
                      promotion_type.xValentine.s.Day + product_analytic_vertical.xGamingAccessoryKit,data=train_GA_dlm)

summary(GA_dlm_model_6) 
vif(GA_dlm_model_6)

#Removing other due to its extremely high VIF  and igh p value
GA_dlm_model_7 <-lm(tot_gmv ~ TV + Sponsorship + Content.Marketing + Online.marketing + 
                      NPS + lag_3_gmv + change_wrt_w1_gmv + 
                      change_wrt_w2_gmv + content_ad_stock + online_ad_stock + 
                      radio_ad_stock + sem_ad_stock + product_analytic_vertical.xGamePad + 
                      product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                      product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                      product_analytic_vertical.xJoystickGamingWheel + promotion_type.xChristmas...New.Year.Sale + 
                      promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                      promotion_type.xPacman + promotion_type.xRakshabandhan.Sale + 
                      promotion_type.xValentine.s.Day + product_analytic_vertical.xGamingAccessoryKit,data=train_GA_dlm)

summary(GA_dlm_model_7) 
vif(GA_dlm_model_7)


#Removing content_ad_stock due to its extremely high VIF  and igh p value
GA_dlm_model_8 <-lm(tot_gmv ~ TV + Sponsorship + Content.Marketing + Online.marketing + 
                      NPS + lag_3_gmv + change_wrt_w1_gmv + 
                      change_wrt_w2_gmv + online_ad_stock + 
                      radio_ad_stock + sem_ad_stock + product_analytic_vertical.xGamePad + 
                      product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                      product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                      product_analytic_vertical.xJoystickGamingWheel + promotion_type.xChristmas...New.Year.Sale + 
                      promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                      promotion_type.xPacman + promotion_type.xRakshabandhan.Sale + 
                      promotion_type.xValentine.s.Day + product_analytic_vertical.xGamingAccessoryKit,data=train_GA_dlm)

summary(GA_dlm_model_8) 
vif(GA_dlm_model_8)


#Removing Content.Marketing due to its extremely high VIF  and high p value
GA_dlm_model_9 <-lm(tot_gmv ~ TV + Sponsorship + Online.marketing + 
                      NPS + lag_3_gmv + change_wrt_w1_gmv + change_wrt_w2_gmv + online_ad_stock + 
                      radio_ad_stock + sem_ad_stock + product_analytic_vertical.xGamePad + 
                      product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                      product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                      product_analytic_vertical.xJoystickGamingWheel + promotion_type.xChristmas...New.Year.Sale + 
                      promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                      promotion_type.xPacman + promotion_type.xRakshabandhan.Sale + 
                      promotion_type.xValentine.s.Day + product_analytic_vertical.xGamingAccessoryKit,data=train_GA_dlm)

summary(GA_dlm_model_9) 
vif(GA_dlm_model_9)

#Removing online_ad_stock due to its extremely high VIF  and high p value
GA_dlm_model_10 <-lm(tot_gmv ~ TV + Sponsorship + Online.marketing + 
                      NPS + lag_3_gmv + change_wrt_w1_gmv + change_wrt_w2_gmv +  
                      radio_ad_stock + sem_ad_stock + product_analytic_vertical.xGamePad + 
                      product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                      product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                      product_analytic_vertical.xJoystickGamingWheel + promotion_type.xChristmas...New.Year.Sale + 
                      promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                      promotion_type.xPacman + promotion_type.xRakshabandhan.Sale + 
                      promotion_type.xValentine.s.Day + product_analytic_vertical.xGamingAccessoryKit,data=train_GA_dlm)

summary(GA_dlm_model_10) 
vif(GA_dlm_model_10)

#Removing promotion_type.xValentine.s.Day due to its extremely high VIF  and high p value
GA_dlm_model_11 <-lm(tot_gmv ~ TV + Sponsorship + Online.marketing + 
                       NPS + lag_3_gmv + change_wrt_w1_gmv + change_wrt_w2_gmv +  
                       radio_ad_stock + sem_ad_stock + product_analytic_vertical.xGamePad + 
                       product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                       product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                       product_analytic_vertical.xJoystickGamingWheel + promotion_type.xChristmas...New.Year.Sale + 
                       promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                       promotion_type.xPacman + promotion_type.xRakshabandhan.Sale + 
                       product_analytic_vertical.xGamingAccessoryKit,data=train_GA_dlm)

summary(GA_dlm_model_11) 
vif(GA_dlm_model_11)

#Removing product_analytic_vertical.xGamingSpeaker due to its  high p value
GA_dlm_model_12 <-lm(tot_gmv ~ TV + Sponsorship + Online.marketing + 
                       NPS + lag_3_gmv + change_wrt_w1_gmv + change_wrt_w2_gmv +  
                       radio_ad_stock + sem_ad_stock + product_analytic_vertical.xGamePad + 
                       product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                       product_analytic_vertical.xGamingMouse +  
                       product_analytic_vertical.xJoystickGamingWheel + promotion_type.xChristmas...New.Year.Sale + 
                       promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                       promotion_type.xPacman + promotion_type.xRakshabandhan.Sale + 
                       product_analytic_vertical.xGamingAccessoryKit,data=train_GA_dlm)

summary(GA_dlm_model_12) 
vif(GA_dlm_model_12)

#Removing product_analytic_vertical.xGamingAccessoryKit due to its  high p value
GA_dlm_model_13 <-lm(tot_gmv ~ TV + Sponsorship + Online.marketing + 
                       NPS + lag_3_gmv + change_wrt_w1_gmv + change_wrt_w2_gmv +  
                       radio_ad_stock + sem_ad_stock + product_analytic_vertical.xGamePad + 
                       product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                       product_analytic_vertical.xGamingMouse +  
                       product_analytic_vertical.xJoystickGamingWheel + promotion_type.xChristmas...New.Year.Sale + 
                       promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                       promotion_type.xPacman + promotion_type.xRakshabandhan.Sale ,data=train_GA_dlm)

summary(GA_dlm_model_13) 
vif(GA_dlm_model_13)

#Removing promotion_type.xPacman due to its  high p value
GA_dlm_model_14 <-lm(tot_gmv ~ TV + Sponsorship + Online.marketing + 
                       NPS + lag_3_gmv + change_wrt_w1_gmv + change_wrt_w2_gmv +  
                       radio_ad_stock + sem_ad_stock + product_analytic_vertical.xGamePad + 
                       product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                       product_analytic_vertical.xGamingMouse +  
                       product_analytic_vertical.xJoystickGamingWheel + promotion_type.xChristmas...New.Year.Sale + 
                       promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                       promotion_type.xRakshabandhan.Sale ,data=train_GA_dlm)

summary(GA_dlm_model_14) 
vif(GA_dlm_model_14)

#Removing radio_ad_stock due to its  high p value
GA_dlm_model_15 <-lm(tot_gmv ~ TV + Sponsorship + Online.marketing + 
                       NPS + lag_3_gmv + change_wrt_w1_gmv + change_wrt_w2_gmv +  
                       sem_ad_stock + product_analytic_vertical.xGamePad + 
                       product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                       product_analytic_vertical.xGamingMouse +  
                       product_analytic_vertical.xJoystickGamingWheel + promotion_type.xChristmas...New.Year.Sale + 
                       promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                       promotion_type.xRakshabandhan.Sale ,data=train_GA_dlm)

summary(GA_dlm_model_15) 
vif(GA_dlm_model_15)


#Removing product_analytic_vertical.xJoystickGamingWheel due to its  high p value
GA_dlm_model_16 <-lm(tot_gmv ~ TV + Sponsorship + Online.marketing + 
                       NPS + lag_3_gmv + change_wrt_w1_gmv + change_wrt_w2_gmv +  
                       sem_ad_stock + product_analytic_vertical.xGamePad + 
                       product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                       product_analytic_vertical.xGamingMouse +  
                       promotion_type.xChristmas...New.Year.Sale + 
                       promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                       promotion_type.xRakshabandhan.Sale ,data=train_GA_dlm)

summary(GA_dlm_model_16) 
vif(GA_dlm_model_16)

#Removing promotion_type.xChristmas...New.Year.Sale due to its  high p value
GA_dlm_model_17 <-lm(tot_gmv ~ TV + Sponsorship + Online.marketing + 
                       NPS + lag_3_gmv + change_wrt_w1_gmv + change_wrt_w2_gmv +  
                       sem_ad_stock + product_analytic_vertical.xGamePad + 
                       product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                       product_analytic_vertical.xGamingMouse + 
                       promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                       promotion_type.xRakshabandhan.Sale ,data=train_GA_dlm)

summary(GA_dlm_model_17) 
vif(GA_dlm_model_17)

#Removing change_wrt_w2_gmv due to its  high p value
GA_dlm_model_18 <-lm(tot_gmv ~ TV + Sponsorship + Online.marketing + 
                       NPS + lag_3_gmv + change_wrt_w1_gmv +   
                       sem_ad_stock + product_analytic_vertical.xGamePad + 
                       product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                       product_analytic_vertical.xGamingMouse + 
                       promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                       promotion_type.xRakshabandhan.Sale ,data=train_GA_dlm)

summary(GA_dlm_model_18) 
vif(GA_dlm_model_18)

#Removing change_wrt_w1_gmv due to its  high p value
GA_dlm_model_19 <-lm(tot_gmv ~ TV + Sponsorship + Online.marketing +NPS + lag_3_gmv +   
                       sem_ad_stock + product_analytic_vertical.xGamePad + 
                       product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                       product_analytic_vertical.xGamingMouse + 
                       promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                       promotion_type.xRakshabandhan.Sale ,data=train_GA_dlm)

summary(GA_dlm_model_19) 
vif(GA_dlm_model_19)

#Removing NPS due to its  high p value
GA_dlm_model_20 <-lm(tot_gmv ~ TV + Sponsorship + Online.marketing +lag_3_gmv +   
                       sem_ad_stock + product_analytic_vertical.xGamePad + 
                       product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                       product_analytic_vertical.xGamingMouse + 
                       promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                       promotion_type.xRakshabandhan.Sale ,data=train_GA_dlm)

summary(GA_dlm_model_20) 
vif(GA_dlm_model_20)

#Removing TV due to its  high p value
GA_dlm_model_21 <-lm(tot_gmv ~ Sponsorship + Online.marketing +lag_3_gmv +   
                       sem_ad_stock + product_analytic_vertical.xGamePad + 
                       product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                       product_analytic_vertical.xGamingMouse + 
                       promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                       promotion_type.xRakshabandhan.Sale ,data=train_GA_dlm)

summary(GA_dlm_model_21) 
vif(GA_dlm_model_21)


#Removing lag_3_gmv due to its  high p value
GA_dlm_model_22 <-lm(tot_gmv ~ Sponsorship + Online.marketing +   
                       sem_ad_stock + product_analytic_vertical.xGamePad + 
                       product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                       product_analytic_vertical.xGamingMouse + 
                       promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                       promotion_type.xRakshabandhan.Sale ,data=train_GA_dlm)

summary(GA_dlm_model_22) 
vif(GA_dlm_model_22)

#Removing promotion_type.xIndependence.Sale due to its  high p value
GA_dlm_model_23 <-lm(tot_gmv ~ Sponsorship + Online.marketing +   
                       sem_ad_stock + product_analytic_vertical.xGamePad + 
                       product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                       product_analytic_vertical.xGamingMouse +promotion_type.xDaussera.sale +  
                       promotion_type.xRakshabandhan.Sale ,data=train_GA_dlm)

summary(GA_dlm_model_23) 
vif(GA_dlm_model_23)


#Removing product_analytic_vertical.xGamingKeyboard due to its  high p value
GA_dlm_model_24 <-lm(tot_gmv ~ Sponsorship + Online.marketing +   
                       sem_ad_stock + product_analytic_vertical.xGamePad + 
                       product_analytic_vertical.xGamingHeadset +  
                       product_analytic_vertical.xGamingMouse +promotion_type.xDaussera.sale +  
                       promotion_type.xRakshabandhan.Sale ,data=train_GA_dlm)

summary(GA_dlm_model_24)  # 0.717
vif(GA_dlm_model_24)

Final_GA_dlm<-GA_dlm_model_24

##################Test the model on test data#############################
#######  taking final model as GA_dlm_model_24 ##############
#value of Adjusted R-squared of GA_dlm_model_24 is ~ 71%.

#Now, let us move forward to test the model on test data. 
Predict<-predict(Final_GA_dlm, test_GA_dlm[,-1])
#r-squared value between the predicted and actual values of sales
(cor(test_GA_dlm$tot_gmv,Predict))^2 # 67.8% ~68%


########## Calculate Elaticity ################################
#To calculate Price Elasticity of Demand we use the formula:
#  PE = (??Q/??P) * (P/Q)
#(??Q/??P) is determined by the coefficient in our regression formula.
#To determine (P/Q) we will use the mean of independent variable.

elasticity <- function(var) {
  x <- as.numeric(Final_GA_dlm$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(Final_GA_dlm$coefficients)) {
  var_list[i-1] <- elasticity(names(Final_GA_dlm$coefficients)[i])
}

elasticity.outputs <- data.frame(names(Final_GA_dlm$coefficients[2:length(Final_GA_dlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)

ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity", fill="palevioletred4") + 
  coord_flip() +
  ggtitle("Gaming Accessory - Lag Model") +xlab("Variables") +p_theme

###################  Cross validation ###############################


GA_dlm_cv <- cv.lm(data = train_GA_dlm, form.lm = formula(tot_gmv ~ Sponsorship + Online.marketing +   
                                                            sem_ad_stock + product_analytic_vertical.xGamePad + 
                                                            product_analytic_vertical.xGamingHeadset +  
                                                            product_analytic_vertical.xGamingMouse +promotion_type.xDaussera.sale +  
                                                            promotion_type.xRakshabandhan.Sale), m =10)

#ms = 0.333



