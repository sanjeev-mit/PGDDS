#==================================================================================================================
#                                    MULTIPLICATIVE MODEL
#==================================================================================================================


################################################################################
#                                      CameraAccessory
################################################################################

Camera_Multipicative_Model.df <-CameraAccessory
Camera_Multipicative_Model.df$week_no <- NULL
Camera_Multipicative_Model.df$Month <- NULL


#The Cliffs Notes version is that for a model like
#ln(Y)=a+b???ln(X)+c???D+??,
#where X is a continuous regressor, and D is a zero-one dummy variable.
#If D switches from 0 to 1, the % impact of D on Y is 100???(exp(c)???1).
#If D switches from 1 to 0, the % impact of D on Y is 100???(exp(???c)???1).

## Hence leave a 0/1 coded dummy variable as it is

######## Treatment of continuous regressor having zero value
sapply(Camera_Multipicative_Model.df[,c(1:13)], function(x) length(which(x==0)))
#TV,Content.Marketing, Radio,other have zero values
Camera_Multipicative_Model.df$TV[which(Camera_Multipicative_Model.df$TV == 0)] <- 0.01
Camera_Multipicative_Model.df$Radio[which(Camera_Multipicative_Model.df$Radio == 0)] <- 0.01
Camera_Multipicative_Model.df$Other[which(Camera_Multipicative_Model.df$Other == 0)] <- 0.01
Camera_Multipicative_Model.df$Content.Marketing[which(Camera_Multipicative_Model.df$Content.Marketing == 0)] <- 0.01

#Log of the numerical variables
Camera_Multipicative_Model.df[,1:13] <- data.frame(sign(Camera_Multipicative_Model.df[,1:13])*log(abs(Camera_Multipicative_Model.df[,1:13])))

########## separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(Camera_Multipicative_Model.df), 0.7*nrow(Camera_Multipicative_Model.df))
train_CameraAccessory_mm = Camera_Multipicative_Model.df[trainindices,]
test_CameraAccessory_mm = Camera_Multipicative_Model.df[-trainindices,]
nrow(train_CameraAccessory_mm) #662 obs
nrow(test_CameraAccessory_mm) #284 obs.


########## Build model 1 containing all variables
cam_multi_model_1 <-lm(tot_gmv~.,data=train_CameraAccessory_mm)
summary(cam_multi_model_1) # Adjusted R-squared:  0.8315

##########Use stepAIC 
step_cam_mm <- stepAIC(cam_multi_model_1, direction="both")
#Step:  AIC=-8143.14
summary(step_cam_mm) #0.8332
vif(step_cam_mm)
alias(step_cam_mm)


cam_multi_model_2 <-lm(tot_gmv~list_price + TV + Sponsorship + Content.Marketing + 
                         Online.marketing + X.Affiliates + Radio + Other + NPS + discount_over_mrp + 
                         product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                         promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                         promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory_mm)

summary(cam_multi_model_2)
vif(cam_multi_model_2)

cor(train_CameraAccessory_mm$Radio,train_CameraAccessory_mm$Radio) #1 hence remove with high VIF

#removing promotion_type.xBSD.5 hvaing higH P VALUE
cam_multi_model_3 <-lm(tot_gmv~list_price + TV + Sponsorship + Content.Marketing + 
                         Online.marketing + X.Affiliates + Radio + Other + NPS + discount_over_mrp + 
                         product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                         promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory_mm)

summary(cam_multi_model_3)
vif(cam_multi_model_3)

#removing promotion_type.xNo_promotion hvaing higH VIF
cam_multi_model_4 <-lm(tot_gmv~list_price + TV + Sponsorship + Content.Marketing + 
                         Online.marketing + X.Affiliates + Radio + Other + NPS + discount_over_mrp + 
                         product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                         promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory_mm)

summary(cam_multi_model_4)
vif(cam_multi_model_4)

#removing promotion_type.xChristmas...New.Year.Sale hvaing higH VIF
cam_multi_model_5 <-lm(tot_gmv~list_price + TV + Sponsorship + Content.Marketing + 
                         Online.marketing + X.Affiliates + Radio + Other + NPS + discount_over_mrp + 
                         product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                         promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                         promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory_mm)

summary(cam_multi_model_5)
vif(cam_multi_model_5)


#removing discount_over_mrp hvaing higH VIF
cam_multi_model_6 <-lm(tot_gmv~list_price + TV + Sponsorship + Content.Marketing + 
                         Online.marketing + X.Affiliates + Radio + Other + NPS + 
                         product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                         promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                         promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory_mm)

summary(cam_multi_model_6)
vif(cam_multi_model_6)

#removing promotion_type.xDaussera.sale ,promotion_type.xRakshabandhan.Sale hvaing higH VIF
cam_multi_model_7 <-lm(tot_gmv~list_price + TV + Sponsorship + Content.Marketing + 
                         Online.marketing + X.Affiliates + Radio + Other + NPS + 
                         product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                          promotion_type.xIndependence.Sale ,data=train_CameraAccessory_mm)

summary(cam_multi_model_7)
vif(cam_multi_model_7)

#removing product_analytic_vertical.xLens hvaing higH VIF
cam_multi_model_8 <-lm(tot_gmv~list_price + TV + Sponsorship + Content.Marketing + 
                         Online.marketing + X.Affiliates + Radio + Other + NPS + 
                         product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                         product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                         promotion_type.xIndependence.Sale ,data=train_CameraAccessory_mm)

summary(cam_multi_model_8)
vif(cam_multi_model_8)
# there are some variable with high VIF,  lets check their correlation

cor(train_CameraAccessory_mm$Radio,train_CameraAccessory_mm$Other) #0.99
cor(train_CameraAccessory_mm$Radio,train_CameraAccessory_mm$Online.marketing) #0.0.24
cor(train_CameraAccessory_mm$Radio,train_CameraAccessory_mm$X.Affiliates) #0.0.245
cor(train_CameraAccessory_mm$Online.marketing,train_CameraAccessory_mm$X.Affiliates) #0.99
cor(train_CameraAccessory_mm$X.Affiliates,train_CameraAccessory_mm$Content.Marketing) #0.58
cor(train_CameraAccessory_mm$X.Affiliates,train_CameraAccessory_mm$TV) #0.84
cor(train_CameraAccessory_mm$Content.Marketing,train_CameraAccessory_mm$TV) #0.38

# REMOVING Other ,Online.marketing , TV having high VIF and high correlation
cam_multi_model_9 <-lm(tot_gmv~list_price + Sponsorship + Content.Marketing + 
                          X.Affiliates + Radio +  NPS + 
                         product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                         product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                         promotion_type.xIndependence.Sale ,data=train_CameraAccessory_mm)

summary(cam_multi_model_9)
vif(cam_multi_model_9)


#removing Content.Marketing hvaing higH p-value
cam_multi_model_10 <-lm(tot_gmv~list_price + Sponsorship + 
                         X.Affiliates + Radio +  NPS + 
                         product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                         product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                         promotion_type.xIndependence.Sale ,data=train_CameraAccessory_mm)

summary(cam_multi_model_10)
vif(cam_multi_model_10)


#removing Radio having higH p-value
cam_multi_model_11 <-lm(tot_gmv~list_price + Sponsorship + 
                          X.Affiliates +NPS + 
                          product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                          product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                          product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                          product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                          product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                          product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                          product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                          product_analytic_vertical.xReflectorUmbrella + 
                          product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                          product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                          promotion_type.xIndependence.Sale ,data=train_CameraAccessory_mm)

summary(cam_multi_model_11)
vif(cam_multi_model_11)

#removing Sponsorship having higH p-value
cam_multi_model_12 <-lm(tot_gmv~list_price + X.Affiliates +NPS + 
                          product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                          product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                          product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                          product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                          product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                          product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                          product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                          product_analytic_vertical.xReflectorUmbrella + 
                          product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                          product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                          promotion_type.xIndependence.Sale ,data=train_CameraAccessory_mm)

summary(cam_multi_model_12)
vif(cam_multi_model_12)

#removing NPS having higH p-value
cam_multi_model_13 <-lm(tot_gmv~list_price + X.Affiliates + 
                          product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                          product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                          product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                          product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                          product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                          product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                          product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                          product_analytic_vertical.xReflectorUmbrella + 
                          product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                          product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                          promotion_type.xIndependence.Sale ,data=train_CameraAccessory_mm)

summary(cam_multi_model_13) #0.8137
vif(cam_multi_model_13)

Final_camaccesory_multi_model<-cam_multi_model_13

##################Test the model on test data#############################
#######  taking final model as cam_multi_model_13 ##############
#value of Adjusted R-squared of cam_multi_model_13 is ~ 81.37%.

#Now, let us move forward to test the model on test data. 
Predict<-predict(Final_camaccesory_multi_model, test_CameraAccessory_mm)
#r-squared value between the predicted and actual values of sales
(cor(test_CameraAccessory_mm$tot_gmv,Predict))^2 # 73.433%


########## Calculate Elaticity ################################
#For Multiplicative model elasticity is directly equals to coefficient of variables

elasticity <- function(var) {
  x <- as.numeric(Final_camaccesory_multi_model$coefficients[var])
  return(x)
}

varlist.mul.cam <- list()

for(i in 2:length(Final_camaccesory_multi_model$coefficients)) {
  varlist.mul.cam[i-1] <- elasticity(names(Final_camaccesory_multi_model$coefficients)[i])
}

elasticity.cam.mul <- data.frame(names(Final_camaccesory_multi_model$coefficients[2:length(Final_camaccesory_multi_model$coefficients)]))
elasticity.cam.mul <- cbind(elasticity.cam.mul,do.call(rbind.data.frame, varlist.mul.cam))
colnames(elasticity.cam.mul) <- c("Variable","Elasticity")

elasticity.cam.mul$direction <- ifelse(elasticity.cam.mul$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.cam.mul, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" , fill="sandybrown") + 
  coord_flip() +
  ggtitle("Camera Accessory - Multiplicative Model") +xlab("Variables") +p_theme

###################  Cross validation ###############################


cam_lm_cv <- cv.lm(data = train_CameraAccessory_mm, form.lm = formula(tot_gmv~list_price + X.Affiliates + 
                                                                        product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                                                                        product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                                                                        product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                                                                        product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                                                                        product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                                                                        product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                                                                        product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                                                                        product_analytic_vertical.xReflectorUmbrella + 
                                                                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                                                                        product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                                                                        promotion_type.xIndependence.Sale ), m =10)

#ms = 0.994
################################################################################
#                                Gaming Accessories
################################################################################

Gaming_Multipicative_Model.df <-GamingAccessory
Gaming_Multipicative_Model.df$week_no <- NULL
Gaming_Multipicative_Model.df$Month <- NULL


#The Cliffs Notes version is that for a model like
#ln(Y)=a+b???ln(X)+c???D+??,
#where X is a continuous regressor, and D is a zero-one dummy variable.
#If D switches from 0 to 1, the % impact of D on Y is 100???(exp(c)???1).
#If D switches from 1 to 0, the % impact of D on Y is 100???(exp(???c)???1).

## Hence leave a 0/1 coded dummy variable as it is

######## Treatment of continuous regressor having zero value
sapply(Gaming_Multipicative_Model.df[,c(1:13)], function(x) length(which(x==0)))
#TV,Content.Marketing, Radio,other have zero values
Gaming_Multipicative_Model.df$TV[which(Gaming_Multipicative_Model.df$TV == 0)] <- 0.01
Gaming_Multipicative_Model.df$Radio[which(Gaming_Multipicative_Model.df$Radio == 0)] <- 0.01
Gaming_Multipicative_Model.df$Other[which(Gaming_Multipicative_Model.df$Other == 0)] <- 0.01
Gaming_Multipicative_Model.df$Content.Marketing[which(Gaming_Multipicative_Model.df$Content.Marketing == 0)] <- 0.01

#Log of the numerical variables
Gaming_Multipicative_Model.df[,1:13] <- data.frame(sign(Gaming_Multipicative_Model.df[,1:13])*log(abs(Gaming_Multipicative_Model.df[,1:13])))

########## separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(Gaming_Multipicative_Model.df), 0.7*nrow(Gaming_Multipicative_Model.df))
train_GA_mm = Gaming_Multipicative_Model.df[trainindices,]
test_GA_mm = Gaming_Multipicative_Model.df[-trainindices,]
nrow(train_GA_mm) #499 obs
nrow(test_GA_mm) #214 obs.


########## Build model 1 containing all variables
gaming_multi_model_1 <-lm(tot_gmv~.,data=train_GA_mm)
summary(gaming_multi_model_1) # Adjusted R-squared:  0.746

##########Use stepAIC 
step_GA_mm <- stepAIC(gaming_multi_model_1, direction="both")
#Step:  AIC=-4.25
summary(step_GA_mm) #0.8332
vif(step_GA_mm)
alias(step_GA_mm)


gaming_multi_model_2 <-lm(tot_gmv ~ list_price + TV + Digital + Sponsorship + Content.Marketing + 
                            Online.marketing + X.Affiliates + SEM + Radio + Other + discount_over_mrp + 
                            product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGameControlMount + 
                            product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                            product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingGun + 
                            product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                            product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingMousePad + 
                            product_analytic_vertical.xGamingSpeaker + promotion_type.xBig.Diwali.Sale + 
                            promotion_type.xBSD.5 + promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                            promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale,data=train_GA_mm)

summary(gaming_multi_model_2)
vif(gaming_multi_model_2)


#removing Digital having high p-value and high VIF
gaming_multi_model_3 <-lm(tot_gmv ~ list_price + TV + Sponsorship + Content.Marketing + 
                            Online.marketing + X.Affiliates + SEM + Radio + Other + discount_over_mrp + 
                            product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGameControlMount + 
                            product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                            product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingGun + 
                            product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                            product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingMousePad + 
                            product_analytic_vertical.xGamingSpeaker + promotion_type.xBig.Diwali.Sale + 
                            promotion_type.xBSD.5 + promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                            promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale,data=train_GA_mm)

summary(gaming_multi_model_3)
vif(gaming_multi_model_3)


#removing promotion_type.xBSD.5 having high p-value 
gaming_multi_model_4 <-lm(tot_gmv ~ list_price + TV + Sponsorship + Content.Marketing + 
                            Online.marketing + X.Affiliates + SEM + Radio + Other + discount_over_mrp + 
                            product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGameControlMount + 
                            product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                            product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingGun + 
                            product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                            product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingMousePad + 
                            product_analytic_vertical.xGamingSpeaker + promotion_type.xBig.Diwali.Sale + 
                            promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                            promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale,data=train_GA_mm)

summary(gaming_multi_model_4)
vif(gaming_multi_model_4)

#removing Content.Marketing having high p-value  and high VIF
gaming_multi_model_5 <-lm(tot_gmv ~ list_price + TV + Sponsorship + 
                            Online.marketing + X.Affiliates + SEM + Radio + Other + discount_over_mrp + 
                            product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGameControlMount + 
                            product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                            product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingGun + 
                            product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                            product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingMousePad + 
                            product_analytic_vertical.xGamingSpeaker + promotion_type.xBig.Diwali.Sale + 
                            promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                            promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale,data=train_GA_mm)

summary(gaming_multi_model_5)
vif(gaming_multi_model_5)

#removing promotion_type.xBig.Diwali.Sale having high p-value  and high VIF
gaming_multi_model_6 <-lm(tot_gmv ~ list_price + TV + Sponsorship + 
                            Online.marketing + X.Affiliates + SEM + Radio + Other + discount_over_mrp + 
                            product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGameControlMount + 
                            product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                            product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingGun + 
                            product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                            product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingMousePad + 
                            product_analytic_vertical.xGamingSpeaker + 
                            promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                            promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale,data=train_GA_mm)

summary(gaming_multi_model_6)
vif(gaming_multi_model_6)

#removing product_analytic_vertical.xGamingMousePad having high p-value  and high VIF
gaming_multi_model_7 <-lm(tot_gmv ~ list_price + TV + Sponsorship + 
                            Online.marketing + X.Affiliates + SEM + Radio + Other + discount_over_mrp + 
                            product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGameControlMount + 
                            product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                            product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingGun + 
                            product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                            product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                            promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                            promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale,data=train_GA_mm)

summary(gaming_multi_model_7)
vif(gaming_multi_model_7)

#removing discount_over_mrp having high p-value  and high VIF
gaming_multi_model_8 <-lm(tot_gmv ~ list_price + TV + Sponsorship + 
                            Online.marketing + X.Affiliates + SEM + Radio + Other +  
                            product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGameControlMount + 
                            product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                            product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingGun + 
                            product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                            product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                            promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                            promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale,data=train_GA_mm)

summary(gaming_multi_model_8)
vif(gaming_multi_model_8)


#removing promotion_type.xDaussera.sale having high p-value  and high VIF
gaming_multi_model_9 <-lm(tot_gmv ~ list_price + TV + Sponsorship + 
                            Online.marketing + X.Affiliates + SEM + Radio + Other +  
                            product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGameControlMount + 
                            product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                            product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingGun + 
                            product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                            product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                            promotion_type.xIndependence.Sale + 
                            promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale,data=train_GA_mm)

summary(gaming_multi_model_9)
vif(gaming_multi_model_9)

#There are some variables with high VIF .Lets check their correlation
cor(Gaming_Multipicative_Model.df$Radio,Gaming_Multipicative_Model.df$Other) #0.99
cor(Gaming_Multipicative_Model.df$Radio,Gaming_Multipicative_Model.df$X.Affiliates) #0.252
cor(Gaming_Multipicative_Model.df$Radio,Gaming_Multipicative_Model.df$Online.marketing) #0.251
cor(Gaming_Multipicative_Model.df$Radio,Gaming_Multipicative_Model.df$TV) #0.14
cor(Gaming_Multipicative_Model.df$ X.Affiliates,Gaming_Multipicative_Model.df$Online.marketing) #0.99
cor(Gaming_Multipicative_Model.df$ X.Affiliates,Gaming_Multipicative_Model.df$TV) #0.817

# Removing TV and Online.marketing having high p value and low vif

gaming_multi_model_10 <-lm(tot_gmv ~ list_price + Sponsorship + 
                            X.Affiliates + SEM + Radio + Other +  
                            product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGameControlMount + 
                            product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                            product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingGun + 
                            product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                            product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                            promotion_type.xIndependence.Sale + 
                            promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale,data=train_GA_mm)

summary(gaming_multi_model_10)
vif(gaming_multi_model_10)

# Removing other having high p value and low vif

gaming_multi_model_11 <-lm(tot_gmv ~ list_price + Sponsorship + X.Affiliates + SEM + Radio +  
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGameControlMount + 
                             product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingGun + 
                             product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             promotion_type.xIndependence.Sale + 
                             promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale,data=train_GA_mm)

summary(gaming_multi_model_11)
vif(gaming_multi_model_11)

# Removing Sponsorship,SEM,Radio,product_analytic_vertical.xGameControlMount having high p value 

gaming_multi_model_12 <-lm(tot_gmv ~ list_price + X.Affiliates +  
                             product_analytic_vertical.xCoolingPad +
                             product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingGun + 
                             product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             promotion_type.xIndependence.Sale + 
                             promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale,data=train_GA_mm)

summary(gaming_multi_model_12)
vif(gaming_multi_model_12)

# Removing promotion_type.xNo_promotion having high p value 

gaming_multi_model_13 <-lm(tot_gmv ~ list_price + X.Affiliates +  
                             product_analytic_vertical.xCoolingPad +
                             product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingGun + 
                             product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             promotion_type.xIndependence.Sale + 
                             promotion_type.xRakshabandhan.Sale,data=train_GA_mm)

summary(gaming_multi_model_13) #0.773
vif(gaming_multi_model_13)

Final_GA_multi_model<-gaming_multi_model_13

##################Test the model on test data#############################
#######  taking final model as Final_GA_multi_model ##############
#value of Adjusted R-squared of Final_GA_multi_model is ~ 77%.

#Now, let us move forward to test the model on test data. 
Predict<-predict(Final_GA_multi_model, test_GA_mm)
#r-squared value between the predicted and actual values of sales
(cor(test_GA_mm$tot_gmv,Predict))^2 # 60%

########## Calculate Elaticity ################################
#For multiplicative model we take elasticity as directly coefficients of variables

elasticity <- function(var) {
  x <- as.numeric(Final_GA_multi_model$coefficients[var])
  return(x)
}

varlist.mul.game <- list()

for(i in 2:length(Final_GA_multi_model$coefficients)) {
  varlist.mul.game[i-1] <- elasticity(names(Final_GA_multi_model$coefficients)[i])
}

elasticity.game.mul <- data.frame(names(Final_GA_multi_model$coefficients[2:length(Final_GA_multi_model$coefficients)]))
elasticity.game.mul <- cbind(elasticity.game.mul,do.call(rbind.data.frame, varlist.mul.game))
colnames(elasticity.game.mul) <- c("Variable","Elasticity")

elasticity.game.mul$direction <- ifelse(elasticity.game.mul$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.game.mul, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" ,fill="darkslategray4") + 
  coord_flip() +
  ggtitle("Game Accessory - Multiplicative Model") +xlab("Variables") +p_theme


###################  Cross validation ###############################


GMA_mm_cv <- cv.lm(data = train_GA_mm, form.lm = formula(tot_gmv ~ list_price + X.Affiliates +  
                                                                   product_analytic_vertical.xCoolingPad +
                                                                   product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                                                                   product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingGun + 
                                                                   product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                                                                   product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                                                                   promotion_type.xIndependence.Sale + 
                                                                   promotion_type.xRakshabandhan.Sale), m =10)

#ms = 0.792





################################################################################
#                                HomeAudio
################################################################################
HomeAudio_Multipicative_Model.df <-HomeAudio
HomeAudio_Multipicative_Model.df$week_no <- NULL
HomeAudio_Multipicative_Model.df$Month <- NULL


#The Cliffs Notes version is that for a model like
#ln(Y)=a+b???ln(X)+c???D+??,
#where X is a continuous regressor, and D is a zero-one dummy variable.
#If D switches from 0 to 1, the % impact of D on Y is 100???(exp(c)???1).
#If D switches from 1 to 0, the % impact of D on Y is 100???(exp(???c)???1).

## Hence leave a 0/1 coded dummy variable as it is

######## Treatment of continuous regressor having zero value
sapply(HomeAudio_Multipicative_Model.df[,c(1:13)], function(x) length(which(x==0)))
#TV,Content.Marketing, Radio,other have zero values
HomeAudio_Multipicative_Model.df$TV[which(HomeAudio_Multipicative_Model.df$TV == 0)] <- 0.01
HomeAudio_Multipicative_Model.df$Radio[which(HomeAudio_Multipicative_Model.df$Radio == 0)] <- 0.01
HomeAudio_Multipicative_Model.df$Other[which(HomeAudio_Multipicative_Model.df$Other == 0)] <- 0.01
HomeAudio_Multipicative_Model.df$Content.Marketing[which(HomeAudio_Multipicative_Model.df$Content.Marketing == 0)] <- 0.01

#Log of the numerical variables
HomeAudio_Multipicative_Model.df[,1:13] <- data.frame(sign(HomeAudio_Multipicative_Model.df[,1:13])*log(abs(HomeAudio_Multipicative_Model.df[,1:13])))

########## separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(HomeAudio_Multipicative_Model.df), 0.7*nrow(HomeAudio_Multipicative_Model.df))
train_HA_mm = HomeAudio_Multipicative_Model.df[trainindices,]
test_HA_mm = HomeAudio_Multipicative_Model.df[-trainindices,]
nrow(train_HA_mm) #322 obs
nrow(test_HA_mm) #138 obs.


########## Build model 1 containing all variables
HA_multi_model_1 <-lm(tot_gmv~.,data=train_HA_mm)
summary(HA_multi_model_1) # Adjusted R-squared:  0.744

##########Use stepAIC 
step_HA_mm <- stepAIC(HA_multi_model_1, direction="both")
#Step:  AIC=-6.27
summary(step_HA_mm) #0.8332
vif(step_HA_mm)
alias(step_HA_mm)

HA_multi_model_2 <-lm(tot_gmv ~ list_price + TV + Sponsorship + Content.Marketing + 
                        Online.marketing + X.Affiliates + Radio + Other + discount_over_mrp + 
                        product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                        product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                        product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                        product_analytic_vertical.xKaraokePlayer + product_analytic_vertical.xSlingBox + 
                        product_analytic_vertical.xSoundMixer + promotion_type.xDaussera.sale,data=train_HA_mm)
summary(HA_multi_model_2) 
vif(HA_multi_model_2)

# Removing promotion_type.xDaussera.sale havinG  HIGH P-VALUE
HA_multi_model_3 <-lm(tot_gmv ~ list_price + TV + Sponsorship + Content.Marketing + 
                        Online.marketing + X.Affiliates + Radio + Other + discount_over_mrp + 
                        product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                        product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                        product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                        product_analytic_vertical.xKaraokePlayer + product_analytic_vertical.xSlingBox + 
                        product_analytic_vertical.xSoundMixer ,data=train_HA_mm)
summary(HA_multi_model_3) 
vif(HA_multi_model_3)

#There are variables having hig VIF,lETS check their correlation
cor(train_HA_mm$Radio,train_HA_mm$Other) #0.999
cor(train_HA_mm$Other,train_HA_mm$X.Affiliates) #0.251
cor(train_HA_mm$Other,train_HA_mm$Online.marketing) #0.244
cor(train_HA_mm$Other,train_HA_mm$TV) #0.136
cor(train_HA_mm$Radio,train_HA_mm$X.Affiliates) #0.251
cor(train_HA_mm$Radio,train_HA_mm$Online.marketing) #0.244
cor(train_HA_mm$Radio,train_HA_mm$TV) #0.136
cor(train_HA_mm$X.Affiliates,train_HA_mm$Online.marketing) #0.995
cor(train_HA_mm$Online.marketing,train_HA_mm$TV) #0.892

# Removing OTHER,X.Affiliates ,TV havinG  HIGH P-VALUE
HA_multi_model_4 <-lm(tot_gmv ~ list_price +Sponsorship + Content.Marketing + 
                        Online.marketing +Radio + discount_over_mrp + 
                        product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                        product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                        product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                        product_analytic_vertical.xKaraokePlayer + product_analytic_vertical.xSlingBox + 
                        product_analytic_vertical.xSoundMixer ,data=train_HA_mm)
summary(HA_multi_model_4) 
vif(HA_multi_model_4)

# Removing Content.Marketing  HIGH P-VALUE
HA_multi_model_5 <-lm(tot_gmv ~ list_price +Sponsorship +  
                        Online.marketing +Radio + discount_over_mrp + 
                        product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                        product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                        product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                        product_analytic_vertical.xKaraokePlayer + product_analytic_vertical.xSlingBox + 
                        product_analytic_vertical.xSoundMixer ,data=train_HA_mm)
summary(HA_multi_model_5) 
vif(HA_multi_model_5)

# Removing Sponsorship  HIGH P-VALUE
HA_multi_model_6 <-lm(tot_gmv ~ list_price +  
                        Online.marketing +Radio + discount_over_mrp + 
                        product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                        product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                        product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                        product_analytic_vertical.xKaraokePlayer + product_analytic_vertical.xSlingBox + 
                        product_analytic_vertical.xSoundMixer ,data=train_HA_mm)
summary(HA_multi_model_6) 
vif(HA_multi_model_6)

# Removing Radio  HIGH P-VALUE
HA_multi_model_7 <-lm(tot_gmv ~ list_price +  
                        Online.marketing +discount_over_mrp + 
                        product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                        product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                        product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                        product_analytic_vertical.xKaraokePlayer + product_analytic_vertical.xSlingBox + 
                        product_analytic_vertical.xSoundMixer ,data=train_HA_mm)
summary(HA_multi_model_7) 
vif(HA_multi_model_7)

# Removing discount_over_mrp  HIGH P-VALUE
HA_multi_model_8 <-lm(tot_gmv ~ list_price +  
                        Online.marketing + 
                        product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                        product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                        product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                        product_analytic_vertical.xKaraokePlayer + product_analytic_vertical.xSlingBox + 
                        product_analytic_vertical.xSoundMixer ,data=train_HA_mm)
summary(HA_multi_model_8) 
vif(HA_multi_model_8)

# Removing product_analytic_vertical.xDock   HIGH P-VALUE
HA_multi_model_9 <-lm(tot_gmv ~ list_price +  
                        Online.marketing + product_analytic_vertical.xDJController + 
                        product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                        product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                        product_analytic_vertical.xKaraokePlayer + product_analytic_vertical.xSlingBox + 
                        product_analytic_vertical.xSoundMixer ,data=train_HA_mm)
summary(HA_multi_model_9) 
vif(HA_multi_model_9)

# Removing product_analytic_vertical.xHiFiSystem   HIGH P-VALUE
HA_multi_model_10 <-lm(tot_gmv ~ list_price +  
                        Online.marketing + product_analytic_vertical.xDJController + 
                        product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                        product_analytic_vertical.xHomeAudioSpeaker + 
                        product_analytic_vertical.xKaraokePlayer + product_analytic_vertical.xSlingBox + 
                        product_analytic_vertical.xSoundMixer ,data=train_HA_mm)
summary(HA_multi_model_10)  #Adjusted R-squared:  0.767
vif(HA_multi_model_10)

Final_HA_multi_model<-HA_multi_model_10

##################Test the model on test data#############################
#######  taking final model as HA_multi_model_10 ##############
#value of Adjusted R-squared of HA_multi_model_10 is ~ 77%.

#Now, let us move forward to test the model on test data. 
Predict<-predict(Final_HA_multi_model, test_HA_mm)
#r-squared value between the predicted and actual values of sales
(cor(test_HA_mm$tot_gmv,Predict))^2 # 0.656


########## Calculate Elaticity ################################
#To calculate Price Elasticity of Demand we use the formula:
#  take coefficients directly as elasticity

elasticity <- function(var) {
  x <- as.numeric(Final_HA_multi_model$coefficients[var])
  return(x)
}

varlist.mul.home <- list()

for(i in 2:length(Final_HA_multi_model$coefficients)) {
  varlist.mul.home[i-1] <- elasticity(names(Final_HA_multi_model$coefficients)[i])
}

elasticity.home.mul <- data.frame(names(Final_HA_multi_model$coefficients[2:length(Final_HA_multi_model$coefficients)]))
elasticity.home.mul <- cbind(elasticity.home.mul,do.call(rbind.data.frame, varlist.mul.home))
colnames(elasticity.home.mul) <- c("Variable","Elasticity")

elasticity.home.mul$direction <- ifelse(elasticity.home.mul$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.home.mul, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" ,fill="indianred4") + 
  coord_flip() +
  ggtitle("Home Accessory - Multiplicative Model") +xlab("Variables") +p_theme

###################  Cross validation ###############################


HA_mm_cv <- cv.lm(data = train_HA_mm, form.lm = formula(tot_gmv ~ list_price +  
                                                              Online.marketing + product_analytic_vertical.xDJController + 
                                                              product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                                                              product_analytic_vertical.xHomeAudioSpeaker + 
                                                              product_analytic_vertical.xKaraokePlayer + product_analytic_vertical.xSlingBox + 
                                                              product_analytic_vertical.xSoundMixer ), m =10)

#msE = .903




