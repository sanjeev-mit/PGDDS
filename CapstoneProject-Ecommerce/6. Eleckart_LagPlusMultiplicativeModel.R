#######################################################################
##Building multipiactive lag model for camera accessory 
#######################################################################
weekly_LPML_cam <- Camera_DLM1
weekly_LPML_tmp  <- lapply (weekly_LPML_cam[,1:35] ,function(x) ifelse ( x==0 , 0, log(abs(x)))*sign(x) )

weekly_LPML_cam <- data.frame( cbind( weekly_LPML_tmp ,weekly_LPML_cam[,36:97] ))
weekly_LPML_cam <- na.omit(weekly_LPML_cam)

set.seed(200)
###Creating training and test data
trainindices= sample(1:nrow(weekly_LPML_cam), 0.7*nrow(weekly_LPML_cam))

#Generating the train data set
train_LPML_cam = weekly_LPML_cam[trainindices,]

#Generating the test data set
test_LPML_cam = weekly_LPML_cam[-trainindices,]


####Building lienar model using all attributes
cam_LPML_model1 <- lm(tot_gmv~. , data = train_LPML_cam)
summary(cam_LPML_model1)
##Multiple R-squared:  0.896,	Adjusted R-squared:  0.884 


### StepAIC Modellig
step_LPML_cam_model1 <- stepAIC(cam_LPML_model1, direction = "both")
summary(step_LPML_cam_model1)
##Multiple R-squared:  0.895,	Adjusted R-squared:  0.885 
vif (step_LPML_cam_model1 )

## online_ad_stock has high vif -Hence removing it
cam_LPML_model12 <- lm(formula = tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                         Sponsorship + Content.Marketing + Online.marketing + X.Affiliates + 
                         SEM + Radio + Other + NPS + discount_over_mrp + lag_2_gmv + 
                         change_wrt_w1_gmv + change_wrt_w2_gmv + change_wrt_w3_gmv + 
                         lag_2_lp + tv_ad_stock + dig_ad_stock + spon_ad_stock + content_ad_stock + 
                         sem_ad_stock + product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                         promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xNo_promotion + 
                         promotion_type.xPacman + promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                       data = train_LPML_cam)

summary(cam_LPML_model12)
##Multiple R-squared:  0.888,	Adjusted R-squared:  0.879 
vif (cam_LPML_model12 )




## Other has high vif -Hence removing it
cam_LPML_model13 <- lm(formula = tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                   Sponsorship + Content.Marketing + Online.marketing + X.Affiliates + 
                   SEM + Radio + NPS + discount_over_mrp + lag_2_gmv + 
                   change_wrt_w1_gmv + change_wrt_w2_gmv + change_wrt_w3_gmv + 
                   lag_2_lp + tv_ad_stock + dig_ad_stock + spon_ad_stock + content_ad_stock + 
                   sem_ad_stock + product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraBatteryCharger + 
                   product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                   product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                   product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xExtensionTube + 
                   product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                   product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                   product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                   promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                   promotion_type.xChristmas...New.Year.Sale + promotion_type.xNo_promotion + 
                   promotion_type.xPacman + promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                 data = train_LPML_cam)

summary(cam_LPML_model13)
##Multiple R-squared:  0.877,	Adjusted R-squared:  0.867 
vif (cam_LPML_model13 )



## X.Affiliates  has high vif -Hence removing it
cam_LPML_model14 <- lm(formula = tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                      Sponsorship + Content.Marketing + Online.marketing + 
                      SEM + Radio + NPS + discount_over_mrp + lag_2_gmv + 
                      change_wrt_w1_gmv + change_wrt_w2_gmv + change_wrt_w3_gmv + 
                      lag_2_lp + tv_ad_stock + dig_ad_stock + spon_ad_stock + content_ad_stock + 
                      sem_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraBatteryCharger + 
                      product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                      product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                      product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                      product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xExtensionTube + 
                      product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                      promotion_type.xChristmas...New.Year.Sale + promotion_type.xNo_promotion + 
                      promotion_type.xPacman + promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                    data = train_LPML_cam)

summary(cam_LPML_model14)

##Multiple R-squared:  0.865,	Adjusted R-squared:  0.854 
vif (cam_LPML_model14 )



## spon_ad_stock has high vif -Hence removing it
cam_LPML_model15 <- lm(formula = tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                         Sponsorship + Content.Marketing + Online.marketing + 
                         SEM + Radio + NPS + discount_over_mrp + lag_2_gmv + 
                         change_wrt_w1_gmv + change_wrt_w2_gmv + change_wrt_w3_gmv + 
                         lag_2_lp + tv_ad_stock + dig_ad_stock +content_ad_stock + 
                         sem_ad_stock + product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                         promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xNo_promotion + 
                         promotion_type.xPacman + promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                       data = train_LPML_cam)


summary(cam_LPML_model15)
###Multiple R-squared:  0.858,	Adjusted R-squared:  0.846
vif (cam_LPML_model15 )



## sem_ad_stock has high vif -Hence removing it
cam_LPML_model16 <- lm(formula = tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                         Sponsorship + Content.Marketing + Online.marketing + 
                         SEM + Radio + NPS + discount_over_mrp + lag_2_gmv + 
                         change_wrt_w1_gmv + change_wrt_w2_gmv + change_wrt_w3_gmv + 
                         lag_2_lp + tv_ad_stock + dig_ad_stock +content_ad_stock + 
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                         promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xNo_promotion + 
                         promotion_type.xPacman + promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                       data = train_LPML_cam)

summary(cam_LPML_model16)
##Multiple R-squared:  0.857,	Adjusted R-squared:  0.846 
vif (cam_LPML_model16 )



## Month has high p value hence removing it
cam_LPML_model17 <- lm(formula = tot_gmv ~ week_no + list_price + TV + Digital + 
                         Sponsorship + Content.Marketing + Online.marketing + 
                         SEM + Radio + NPS + discount_over_mrp + lag_2_gmv + 
                         change_wrt_w1_gmv + change_wrt_w2_gmv + change_wrt_w3_gmv + 
                         lag_2_lp + tv_ad_stock + dig_ad_stock +content_ad_stock + 
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                         promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xNo_promotion + 
                         promotion_type.xPacman + promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                       data = train_LPML_cam)
summary(cam_LPML_model17)
##Multiple R-squared:  0.857,	Adjusted R-squared:  0.846
vif (cam_LPML_model17 )


## promotion_type.xBig.Diwali.Sale  has high p value hence removing it
cam_LPML_model18 <- lm(formula = tot_gmv ~ week_no + list_price + TV + Digital + 
                         Sponsorship + Content.Marketing + Online.marketing + 
                         SEM + Radio + NPS + discount_over_mrp + lag_2_gmv + 
                         change_wrt_w1_gmv + change_wrt_w2_gmv + change_wrt_w3_gmv + 
                         lag_2_lp + tv_ad_stock + dig_ad_stock +content_ad_stock + 
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                         promotion_type.xBSD.5 + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xNo_promotion + 
                         promotion_type.xPacman + promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                       data = train_LPML_cam)
summary(cam_LPML_model18)
##Multiple R-squared:  0.857,	Adjusted R-squared:  0.847
vif (cam_LPML_model18 )


## promotion_type.xBSD.5   has high p value hence removing it
cam_LPML_model19 <- lm(formula = tot_gmv ~ week_no + list_price + TV + Digital + 
                         Sponsorship + Content.Marketing + Online.marketing + 
                         SEM + Radio + NPS + discount_over_mrp + lag_2_gmv + 
                         change_wrt_w1_gmv + change_wrt_w2_gmv + change_wrt_w3_gmv + 
                         lag_2_lp + tv_ad_stock + dig_ad_stock +content_ad_stock + 
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xNo_promotion + 
                         promotion_type.xPacman + promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                       data = train_LPML_cam)

summary(cam_LPML_model19)
##Multiple R-squared:  0.857,	Adjusted R-squared:  0.847 
vif (cam_LPML_model19 )


##promotion_type.xPacman,promotion_type.xRepublic.Day,promotion_type.xValentine.s.Day  has high p value hence removing it
cam_LPML_model110 <- lm(formula = tot_gmv ~ week_no + list_price + TV + Digital + 
                         Sponsorship + Content.Marketing + Online.marketing + 
                         SEM + Radio + NPS + discount_over_mrp + lag_2_gmv + 
                         change_wrt_w1_gmv + change_wrt_w2_gmv + change_wrt_w3_gmv + 
                         lag_2_lp + tv_ad_stock + dig_ad_stock +content_ad_stock + 
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xNo_promotion , 
                         data = train_LPML_cam)

summary(cam_LPML_model110)
##Multiple R-squared:  0.857,	Adjusted R-squared:  0.847
vif (cam_LPML_model110 )


##tv_ad_stock,dig_ad_stock,discount_over_mrp,lag_2_gmv  has high p value hence removing it
cam_LPML_model111 <- lm(formula = tot_gmv ~ week_no + list_price + TV + Digital + 
                          Sponsorship + Content.Marketing + Online.marketing + 
                          SEM + Radio + NPS +
                          change_wrt_w1_gmv + change_wrt_w2_gmv + change_wrt_w3_gmv + 
                          lag_2_lp +content_ad_stock + 
                          product_analytic_vertical.xCameraAccessory + 
                          product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                          product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                          product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                          product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xExtensionTube + 
                          product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                          product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                          product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                          promotion_type.xChristmas...New.Year.Sale + promotion_type.xNo_promotion , 
                        data = train_LPML_cam)

summary(cam_LPML_model111)
##Multiple R-squared:  0.856,	Adjusted R-squared:  0.848 
vif (cam_LPML_model111 )


## Removed  TV,Online.Marketing,SEM,Digital,NPS,Content.Marketing  due to high vif 
cam_LPML_model112 <- lm(formula = tot_gmv ~ week_no + list_price + 
                          Sponsorship +
                          Radio +
                          change_wrt_w1_gmv + change_wrt_w2_gmv + change_wrt_w3_gmv + 
                          lag_2_lp +content_ad_stock + 
                          product_analytic_vertical.xCameraAccessory + 
                          product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                          product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                          product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                          product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xExtensionTube + 
                          product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                          product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                          product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                          promotion_type.xChristmas...New.Year.Sale + promotion_type.xNo_promotion , 
                        data = train_LPML_cam)

summary(cam_LPML_model112)
##Multiple R-squared:  0.836,	Adjusted R-squared:  0.828 
vif (cam_LPML_model112 )


## Removed  week_no,Radio,lag_2_lp,content_ad_stock,product_analytic_vertical.xCameraBattery,product_analytic_vertical.xFlash as high p value
cam_LPML_model113 <- lm(formula = tot_gmv ~ list_price + 
                          Sponsorship +
                          change_wrt_w1_gmv + change_wrt_w2_gmv + change_wrt_w3_gmv + 
                          product_analytic_vertical.xCameraAccessory + 
                          product_analytic_vertical.xCameraBatteryCharger + 
                          product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                          product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                          product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                          product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xExtensionTube + 
                          product_analytic_vertical.xFilter +
                          product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                          product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                          promotion_type.xChristmas...New.Year.Sale + promotion_type.xNo_promotion , 
                        data = train_LPML_cam)

summary(cam_LPML_model113)
##Multiple R-squared:  0.827,	Adjusted R-squared:  0.821 
vif (cam_LPML_model113 )

cam_LPML_model_final<- cam_LPML_model113



## Model evalution on test data 

cam_gmv_predicted_value <- predict(cam_LPML_model_final, test_LPML_cam[,-3])
test_LPML_cam$predicted_gmv <- cam_gmv_predicted_value

###Calculating R- square
LPML_cam_r <- cor(test_LPML_cam$tot_gmv, test_LPML_cam$predicted_gmv  )
LPML_cam_rsquared <- LPML_cam_r^2
LPML_cam_rsquared   ####Rsquare on test = 0.863

##cross validation - Model Evaluation
LPML_cam_cv <- cv.lm(data =weekly_LPML_cam , form.lm = cam_LPML_model_final, m =10)
###   ms 
###  0.693 


###Calculating Elasticity
elasticity <- function(var) {
  x <- as.numeric(cam_LPML_model_final$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(cam_LPML_model_final$coefficients)) {
  var_list[i-1] <- elasticity(names(cam_LPML_model_final$coefficients)[i])
}

elasticity.outputs <- data.frame(names(cam_LPML_model_final$coefficients[2:length(cam_LPML_model_final$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)
ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Lag Distributed Multipicative  Model For Camera  Accessory ") +xlab("Variables")

###################################################################################################
##Building multipiactive lag model for gaming accessory 
##################################################################################################

weekly_LPML_game <- GamingAccessory_DLM1
weekly_LPML_game <-na.omit(weekly_LPML_game)

weekly_LPML_game_tmp  <- lapply (weekly_LPML_game[,1:35] ,function(x) ifelse ( x==0 , 0, log(abs(x)))*sign(x) )
weekly_LPML_game <- data.frame( cbind( weekly_LPML_game_tmp ,weekly_LPML_game[,36:97] ))


####Creating test and train data
set.seed(200)
trainindices= sample(1:nrow(weekly_LPML_game), 0.7*nrow(weekly_LPML_game))

#Generate the train data set
train_LPML_game = weekly_LPML_game[trainindices,]

#.Generate the test data set
test_LPML_game = weekly_LPML_game[-trainindices,]



###Building models with all attributes

game_LPML_model1 <- lm(tot_gmv~. , data = train_LPML_game)
summary(game_LPML_model1)
##Multiple R-squared:  0.855,	Adjusted R-squared:  0.835


##StepAIC 
step_game_LPML_model1 <- stepAIC(game_LPML_model1, direction = "both")
summary(step_game_LPML_model1)
##Multiple R-squared:  0.854,	Adjusted R-squared:  0.839 
vif (step_game_LPML_model1 )

## Removed Radio,Online.Marketing,,X.Affliates,Other  due to very high vif 
game_LPML_model1 <- lm ( tot_gmv ~ week_no + Month +list_price + TV + Content.Marketing 
                         + SEM + NPS + X.Affiliates +
                           discount_over_mrp + lag_1_gmv + lag_2_gmv + lag_3_gmv + change_wrt_w1_gmv + 
                           change_wrt_w2_gmv + change_wrt_w3_gmv + lag_2_lp + price_change_wrt_w1 + 
                           price_change_wrt_w2 + tv_ad_stock + dig_ad_stock + spon_ad_stock + 
                           content_ad_stock + affiliate_ad_stock + product_analytic_vertical.xCoolingPad + 
                           product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                           product_analytic_vertical.xGamingAdapter + product_analytic_vertical.xGamingChargingStation + 
                           product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                           product_analytic_vertical.xGamingMemoryCard + product_analytic_vertical.xGamingMouse + 
                           product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xMotionController + 
                           promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                           promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                           promotion_type.xPacman + promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day,
                          data = train_LPML_game)
summary(game_LPML_model1)
##Multiple R-squared:  0.797,	Adjusted R-squared:  0.778 
vif (game_LPML_model1 )

## Remove promotion_type.xNo_promotion,promotion_type.xValentine.s.Day,promotion_type.xChristmas...New.Year.Sale,promotion_type.xRepublic.Day,promotion_type.xBSD.5 as p vale is high
game_LPML_model2 <- lm ( tot_gmv ~ week_no + Month +list_price + TV + Content.Marketing 
                         + SEM + NPS + X.Affiliates +
                           discount_over_mrp + lag_1_gmv + lag_2_gmv + lag_3_gmv + change_wrt_w1_gmv + 
                           change_wrt_w2_gmv + change_wrt_w3_gmv + lag_2_lp + price_change_wrt_w1 + 
                           price_change_wrt_w2 + tv_ad_stock + dig_ad_stock + spon_ad_stock + 
                           content_ad_stock + affiliate_ad_stock + product_analytic_vertical.xCoolingPad + 
                           product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                           product_analytic_vertical.xGamingAdapter + product_analytic_vertical.xGamingChargingStation + 
                           product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                           product_analytic_vertical.xGamingMemoryCard + product_analytic_vertical.xGamingMouse + 
                           product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xMotionController + 
                           promotion_type.xDaussera.sale + 
                           promotion_type.xPacman ,
                         data = train_LPML_game)
summary(game_LPML_model2)
##Multiple R-squared:  0.795,	Adjusted R-squared:  0.778 
vif (game_LPML_model2 )

## Removed SEM,affiliate_ad_stock due to high vif 
game_LPML_model3 <- lm ( tot_gmv ~ week_no + Month +list_price + TV + Content.Marketing 
                         + NPS + X.Affiliates +
                           discount_over_mrp + lag_1_gmv + lag_2_gmv + lag_3_gmv + change_wrt_w1_gmv + 
                           change_wrt_w2_gmv + change_wrt_w3_gmv + lag_2_lp + price_change_wrt_w1 + 
                           price_change_wrt_w2 + tv_ad_stock + dig_ad_stock + spon_ad_stock + 
                           content_ad_stock +product_analytic_vertical.xCoolingPad + 
                           product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                           product_analytic_vertical.xGamingAdapter + product_analytic_vertical.xGamingChargingStation + 
                           product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                           product_analytic_vertical.xGamingMemoryCard + product_analytic_vertical.xGamingMouse + 
                           product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xMotionController + 
                           promotion_type.xDaussera.sale + 
                           promotion_type.xPacman ,
                         data = train_LPML_game)
summary(game_LPML_model3)
##Multiple R-squared:  0.784,	Adjusted R-squared:  0.767 
vif (game_LPML_model3 )

## Removed  Month,Week_no,TV,Content.Marketing as P value is very high
game_LPML_model4 <- lm ( tot_gmv ~ list_price
                         + NPS + X.Affiliates +
                           discount_over_mrp + lag_1_gmv + lag_2_gmv + lag_3_gmv + change_wrt_w1_gmv + 
                           change_wrt_w2_gmv + change_wrt_w3_gmv + lag_2_lp + price_change_wrt_w1 + 
                           price_change_wrt_w2 + tv_ad_stock + dig_ad_stock + spon_ad_stock + 
                           content_ad_stock +product_analytic_vertical.xCoolingPad + 
                           product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                           product_analytic_vertical.xGamingAdapter + product_analytic_vertical.xGamingChargingStation + 
                           product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                           product_analytic_vertical.xGamingMemoryCard + product_analytic_vertical.xGamingMouse + 
                           product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xMotionController + 
                           promotion_type.xDaussera.sale + 
                           promotion_type.xPacman ,
                         data = train_LPML_game)
summary(game_LPML_model4)
##Multiple R-squared:  0.78,	Adjusted R-squared:  0.765 
vif (game_LPML_model4 )

## removed lag_1_gmv, lag_2_gmv as p value is high

game_LPML_model5 <- lm ( tot_gmv ~ list_price
                         + NPS + X.Affiliates +
                           discount_over_mrp +lag_3_gmv + change_wrt_w1_gmv + 
                           change_wrt_w2_gmv + change_wrt_w3_gmv + lag_2_lp + price_change_wrt_w1 + 
                           price_change_wrt_w2 + tv_ad_stock + dig_ad_stock + spon_ad_stock + 
                           content_ad_stock +product_analytic_vertical.xCoolingPad + 
                           product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                           product_analytic_vertical.xGamingAdapter + product_analytic_vertical.xGamingChargingStation + 
                           product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                           product_analytic_vertical.xGamingMemoryCard + product_analytic_vertical.xGamingMouse + 
                           product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xMotionController + 
                           promotion_type.xDaussera.sale + 
                           promotion_type.xPacman ,
                         data = train_LPML_game)
summary(game_LPML_model5)
##Multiple R-squared:  0.764,	Adjusted R-squared:  0.748 
vif (game_LPML_model5)


## removed price_change_wrt_w1,tv_ad_stock due to high p- value
game_LPML_model6 <- lm ( tot_gmv ~ list_price
                         + NPS + X.Affiliates +
                           discount_over_mrp +lag_3_gmv + change_wrt_w1_gmv + 
                           change_wrt_w2_gmv + change_wrt_w3_gmv + lag_2_lp +
                           price_change_wrt_w2 +dig_ad_stock + spon_ad_stock + 
                           content_ad_stock +product_analytic_vertical.xCoolingPad + 
                           product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                           product_analytic_vertical.xGamingAdapter + product_analytic_vertical.xGamingChargingStation + 
                           product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                           product_analytic_vertical.xGamingMemoryCard + product_analytic_vertical.xGamingMouse + 
                           product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xMotionController + 
                           promotion_type.xDaussera.sale + 
                           promotion_type.xPacman ,
                         data = train_LPML_game)
summary(game_LPML_model6)
##Multiple R-squared:  0.763,	Adjusted R-squared:  0.749 
vif (game_LPML_model6)

## removed lag_3_gmv,product_analytic_vertical.xGamingMemoryCard  due to high p value 
game_LPML_model7 <- lm ( tot_gmv ~ list_price
                         + NPS + X.Affiliates +
                           discount_over_mrp + change_wrt_w1_gmv + 
                           change_wrt_w2_gmv + change_wrt_w3_gmv + lag_2_lp +
                           price_change_wrt_w2 +dig_ad_stock + spon_ad_stock + 
                           content_ad_stock +product_analytic_vertical.xCoolingPad + 
                           product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                           product_analytic_vertical.xGamingAdapter + product_analytic_vertical.xGamingChargingStation + 
                           product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                           product_analytic_vertical.xGamingMouse + 
                           product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xMotionController + 
                           promotion_type.xDaussera.sale + 
                           promotion_type.xPacman ,
                         data = train_LPML_game)
summary(game_LPML_model7)

##Multiple R-squared:  0.763,	Adjusted R-squared:  0.75 
vif (game_LPML_model7)


## removed NPS as high vif

game_LPML_model8 <- lm ( tot_gmv ~ list_price
                         +  X.Affiliates +
                           discount_over_mrp + change_wrt_w1_gmv + 
                           change_wrt_w2_gmv + change_wrt_w3_gmv + lag_2_lp +
                           price_change_wrt_w2 +dig_ad_stock + spon_ad_stock + 
                           content_ad_stock +product_analytic_vertical.xCoolingPad + 
                           product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                           product_analytic_vertical.xGamingAdapter + product_analytic_vertical.xGamingChargingStation + 
                           product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                           product_analytic_vertical.xGamingMouse + 
                           product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xMotionController + 
                           promotion_type.xDaussera.sale + 
                           promotion_type.xPacman ,
                         data = train_LPML_game)
summary(game_LPML_model8)
##Multiple R-squared:  0.76,	Adjusted R-squared:  0.748
vif (game_LPML_model8)

## removed lag_2_lp,content_ad_stock,product_analytic_vertical.xGamingAdapter due to high p-value 

game_LPML_model9 <- lm ( tot_gmv ~ list_price
                          +  X.Affiliates +
                            discount_over_mrp + change_wrt_w1_gmv + 
                            change_wrt_w2_gmv + change_wrt_w3_gmv +
                            price_change_wrt_w2 +dig_ad_stock + spon_ad_stock + 
                            product_analytic_vertical.xCoolingPad + 
                            product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                            product_analytic_vertical.xGamingChargingStation + 
                            product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                            product_analytic_vertical.xGamingMouse + 
                            product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xMotionController + 
                            promotion_type.xDaussera.sale + 
                            promotion_type.xPacman ,
                          data = train_LPML_game)
summary(game_LPML_model9)
##Multiple R-squared:  0.752,	Adjusted R-squared:  0.741 
vif (game_LPML_model9)

game_LPML_FinalModel <- game_LPML_model9

## Validation on Test Data 
game_gmv_prediction <- predict(game_LPML_FinalModel, test_LPML_game[,-3])
test_LPML_game$predicted_gmv <- game_gmv_prediction

###Calculating R-sqaured value
game_r <- cor(test_LPML_game$tot_gmv, test_LPML_game$predicted_gmv  )
game_rsquared <- game_r^2
game_rsquared   ####Rsquare on test = 0.776

## CV on of the Model
cv_lm_game <- cv.lm(data =weekly_LPML_game  , form.lm = game_LPML_FinalModel, m =10)

##ms - 0.858 

##########################################################################################
###Plotting Elascticity for multipicative lag model for gaming accessories 
########################################################################################


elasticity <- function(var) {
  x <- as.numeric(game_LPML_FinalModel$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(game_LPML_FinalModel$coefficients)) {
  var_list[i-1] <- elasticity(names(game_LPML_FinalModel$coefficients)[i])
}

elasticity.outputs <- data.frame(names(game_LPML_FinalModel$coefficients[2:length(game_LPML_FinalModel$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)

ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Lag Distributed Multipicative  Model for Gaming Accessory") +xlab("Variables")



#################################################################################################
### Building distributed lag multipicative model for home audio
################################################################################################
weekly_LPML_Home  <-HomeAudio_DLM1 
weekly_LPML_Home <-na.omit(weekly_LPML_Home)

weekly_LPML_Home_tmp  <- lapply (weekly_LPML_Home[,1:35] ,function(x) ifelse ( x==0 , 0, log(abs(x)))*sign(x) )
weekly_LPML_Home <- data.frame( cbind( weekly_LPML_Home_tmp ,weekly_LPML_Home[,36:97] ))

###Creating test and train data

set.seed(200)
trainindices= sample(1:nrow(weekly_LPML_Home), 0.7*nrow(weekly_LPML_Home))

#Generate the train data set
train_LPML_home = weekly_LPML_Home[trainindices,]

#Generate the test data set.
test_LPML_home = weekly_LPML_Home[-trainindices,]

## Creating model with all attributes

home_LPML_model1 <- lm(tot_gmv~. , data = train_LPML_home)
summary(home_LPML_model1)
##Multiple R-squared:  0.911,	Adjusted R-squared:  0.893 

##StepAIC 
step_home_LPML_model1 <- stepAIC(home_LPML_model1, direction = "both")
summary(step_home_LPML_model1)
##Multiple R-squared:  0.907,	Adjusted R-squared:  0.896 
vif (step_home_LPML_model1 )

## Removed Radio,Online.marketing,Other as very high vif 
home_LPML_model2 <- lm( tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                    Sponsorship + Content.Marketing + X.Affiliates + 
                    lag_1_gmv + change_wrt_w1_gmv + change_wrt_w2_gmv + 
                    change_wrt_w3_gmv + lag_1_lp + lag_2_lp + price_change_wrt_w2 + 
                    price_change_wrt_w3 + content_ad_stock + affiliate_ad_stock + 
                    product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                    product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                    product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                    product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                    promotion_type.xPacman, data = train_LPML_home)
summary(home_LPML_model2)
##Multiple R-squared:  0.874,	Adjusted R-squared:  0.861 
vif(home_LPML_model2)

## Removed TV,Digital,Sponsorship as very high p-value 
home_LPML_model3 <- lm( tot_gmv ~ week_no + Month + list_price +
                          Content.Marketing + X.Affiliates + 
                          lag_1_gmv + change_wrt_w1_gmv + change_wrt_w2_gmv + 
                          change_wrt_w3_gmv + lag_1_lp + lag_2_lp + price_change_wrt_w2 + 
                          price_change_wrt_w3 + content_ad_stock + affiliate_ad_stock + 
                          product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                          product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                          product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                          promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                          promotion_type.xPacman, data = train_LPML_home)
summary(home_LPML_model3)
##Multiple R-squared:  0.873,	Adjusted R-squared:  0.861 
vif(home_LPML_model3)

## Removed affiliate_ad_stock as very high vif 

home_LPML_model4 <- lm( tot_gmv ~ week_no + Month + list_price +
                     Content.Marketing + X.Affiliates + 
                     lag_1_gmv + change_wrt_w1_gmv + change_wrt_w2_gmv + 
                     change_wrt_w3_gmv + lag_1_lp + lag_2_lp + price_change_wrt_w2 + 
                     price_change_wrt_w3 + content_ad_stock +
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                     promotion_type.xPacman, data = train_LPML_home)
summary(home_LPML_model4)

##Multiple R-squared:  0.868,	Adjusted R-squared:  0.856 
vif(home_LPML_model4)


## Removed promotion_type.xPacman,promotion_type.xNo_promotion  and promotion_type.xChristmas...New.Year.Sale   very high p-value 
home_LPML_model5 <- lm( tot_gmv ~ week_no + Month + list_price +
                          Content.Marketing + X.Affiliates + 
                          lag_1_gmv + change_wrt_w1_gmv + change_wrt_w2_gmv + 
                          change_wrt_w3_gmv + lag_1_lp + lag_2_lp + price_change_wrt_w2 + 
                          price_change_wrt_w3 + content_ad_stock +
                          product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                          product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                          product_analytic_vertical.xSoundMixer + 
                          promotion_type.xDaussera.sale , data = train_LPML_home)
summary(home_LPML_model5)
##Multiple R-squared:  0.868,	Adjusted R-squared:  0.857 
vif(home_LPML_model5)


## Removed Month,Week very high vif 
home_LPML_model6 <- lm( tot_gmv ~ list_price +
                          Content.Marketing + X.Affiliates + 
                          lag_1_gmv + change_wrt_w1_gmv + change_wrt_w2_gmv + 
                          change_wrt_w3_gmv + lag_1_lp + lag_2_lp + price_change_wrt_w2 + 
                          price_change_wrt_w3 + content_ad_stock +
                          product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                          product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                          product_analytic_vertical.xSoundMixer + 
                          promotion_type.xDaussera.sale , data = train_LPML_home)
summary(home_LPML_model6)
##Multiple R-squared:  0.858,	Adjusted R-squared:  0.848
vif(home_LPML_model6)


## Removed lag_1_lp,lag_2_lp very high vif 

home_LPML_model7 <- lm( tot_gmv ~ list_price +
                          Content.Marketing + X.Affiliates + 
                          lag_1_gmv + change_wrt_w1_gmv + change_wrt_w2_gmv + 
                          change_wrt_w3_gmv + price_change_wrt_w2 + 
                          price_change_wrt_w3 + content_ad_stock +
                          product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                          product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                          product_analytic_vertical.xSoundMixer + 
                          promotion_type.xDaussera.sale , data = train_LPML_home)

summary(home_LPML_model7)
##Multiple R-squared:  0.847,	Adjusted R-squared:  0.837 
vif(home_LPML_model7)


## Removed lag_1_gmv as high p- value
home_LPML_model8 <- lm( tot_gmv ~ list_price +
                          Content.Marketing + X.Affiliates + 
                          change_wrt_w1_gmv + change_wrt_w2_gmv + 
                          change_wrt_w3_gmv + price_change_wrt_w2 + 
                          price_change_wrt_w3 + content_ad_stock +
                          product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                          product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                          product_analytic_vertical.xSoundMixer + 
                          promotion_type.xDaussera.sale , data = train_LPML_home)
summary(home_LPML_model8)
##Multiple R-squared:  0.847,	Adjusted R-squared:  0.838 
vif(home_LPML_model8)


## Removed price_change_wrt_w2,price_change_wrt_w3 and content_ad_stock  as  high p-value 
home_LPML_model9 <- lm( tot_gmv ~ list_price +
                          Content.Marketing + X.Affiliates + 
                          change_wrt_w1_gmv + change_wrt_w2_gmv + 
                          change_wrt_w3_gmv + 
                          product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                          product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                          product_analytic_vertical.xSoundMixer + 
                          promotion_type.xDaussera.sale , data = train_LPML_home)
summary(home_LPML_model9)
##Multiple R-squared:  0.844,	Adjusted R-squared:  0.836 
vif(home_LPML_model9)


## Removed Content.Marketing  very high P-value 
home_LPML_model10 <- lm( tot_gmv ~ list_price +
                           X.Affiliates + 
                          change_wrt_w1_gmv + change_wrt_w2_gmv + 
                          change_wrt_w3_gmv + 
                          product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                          product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                          product_analytic_vertical.xSoundMixer + 
                          promotion_type.xDaussera.sale , data = train_LPML_home)
summary(home_LPML_model10)
##Multiple R-squared:  0.844,	Adjusted R-squared:  0.837
vif(home_LPML_model10)

home_LPML_model_final <- home_LPML_model10

###Predict the gmv using test data and the model

homeaudio_gmv_prediction <- predict(home_LPML_model_final, test_LPML_home[,-3])
test_LPML_home$predicted_gmv <- homeaudio_gmv_prediction


###Calculating the Rsquared value
homeAudio_r <- cor(test_LPML_home$tot_gmv, test_LPML_home$predicted_gmv  )
homeAudio_rsquared <- homeAudio_r^2
homeAudio_rsquared   #### Rsquare on testdata =  0.772

### 10 fold-CV using the model
home_LPML_cv <- cv.lm(data = weekly_LPML_Home , form.lm = home_LPML_model_final, m =10)

##0.752

#### Plotting Elasticity for the Home Audio

elasticity <- function(var) {
  x <- as.numeric(home_LPML_model_final$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(home_LPML_model_final$coefficients)) {
  var_list[i-1] <- elasticity(names(home_LPML_model_final$coefficients)[i])
}

elasticity.outputs <- data.frame(names(home_LPML_model_final$coefficients[2:length(home_LPML_model_final$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)

ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle(" Lag Distributed Multipicative  Model For Home Audio") +xlab("Variables")



