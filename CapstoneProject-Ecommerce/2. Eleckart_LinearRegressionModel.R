#==================================================================================================================
#                                    LINEAR REGRESSION 
#==================================================================================================================


################################################################################
#                                      CameraAccessory
################################################################################

Camera_linear.df <-CameraAccessory
Camera_linear.df$week_no <- NULL
Camera_linear.df$Month <- NULL
#Scaling the dataset
Camera_linear.df[,1:13] <- data.frame(scale(Camera_linear.df[,1:13], center = TRUE))

########## separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(Camera_linear.df), 0.7*nrow(Camera_linear.df))
train_CameraAccessory = Camera_linear.df[trainindices,]
test_CameraAccessory = Camera_linear.df[-trainindices,]
nrow(train_CameraAccessory) #662 obs
nrow(test_CameraAccessory) #284 obs.

########## Build model 1 containing all variables
cam_model_1 <-lm(tot_gmv~.,data=train_CameraAccessory)
summary(cam_model_1) # Adjusted R-squared:  0.7924

##########Use stepAIC 
step_cam <- stepAIC(cam_model_1, direction="both")
#Step:  AIC=--1023.4
summary(step_cam) #0.7947 
vif(step_cam)
alias(step_cam)

cam_model_2 <-lm(tot_gmv ~ list_price + TV + Sponsorship + Content.Marketing + 
                   X.Affiliates + SEM + NPS + discount_over_mrp + product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBag + product_analytic_vertical.xCameraBattery + 
                   product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                   product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                   product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                   product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                   promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                   promotion_type.xEid...Rathayatra.sale + promotion_type.xNo_promotion + 
                   promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory)

summary(cam_model_2)
vif(cam_model_2)

#removing Content.Marketing having high p value and high VIF and hgly correlated to tot_list_price

cam_model_3 <-lm(tot_gmv ~ list_price + TV + Sponsorship + 
                   X.Affiliates + SEM + NPS + discount_over_mrp + product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBag + product_analytic_vertical.xCameraBattery + 
                   product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                   product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                   product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                   product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                   promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                   promotion_type.xEid...Rathayatra.sale + promotion_type.xNo_promotion + 
                   promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory)

summary(cam_model_3)
vif(cam_model_3)

#removing product_analytic_vertical.xCameraBag having high p value 

cam_model_4 <-lm(tot_gmv ~ list_price + TV + Sponsorship + 
                   X.Affiliates + SEM + NPS + discount_over_mrp + product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBattery + 
                   product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                   product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                   product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                   product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                   promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                   promotion_type.xEid...Rathayatra.sale + promotion_type.xNo_promotion + 
                   promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory)

summary(cam_model_4)
vif(cam_model_4)


#removing product_analytic_vertical.xCameraTripod having high p value 

cam_model_5 <-lm(tot_gmv ~ list_price + TV + Sponsorship + 
                   X.Affiliates + SEM + NPS + discount_over_mrp + product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBattery + 
                   product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                   product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                   product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                   promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                   promotion_type.xEid...Rathayatra.sale + promotion_type.xNo_promotion + 
                   promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory)

summary(cam_model_5)
vif(cam_model_5)

#removing product_analytic_vertical.xCameraTripod having high p value 

cam_model_6 <-lm(tot_gmv ~ list_price + TV + Sponsorship + 
                   X.Affiliates + SEM + NPS + discount_over_mrp + product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                   product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                   product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                   promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                   promotion_type.xEid...Rathayatra.sale + promotion_type.xNo_promotion + 
                   promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory)

summary(cam_model_6)
vif(cam_model_6)

#removing product_analytic_vertical.xFlash having high p value 

cam_model_7 <-lm(tot_gmv ~ list_price + TV + Sponsorship + 
                   X.Affiliates + SEM + NPS + discount_over_mrp + product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                   product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                   promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                   promotion_type.xEid...Rathayatra.sale + promotion_type.xNo_promotion + 
                   promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory)

summary(cam_model_7)
vif(cam_model_7)

#removing promotion_type.xEid...Rathayatra.sale having high p value 

cam_model_8 <-lm(tot_gmv ~ list_price + TV + Sponsorship + 
                   X.Affiliates + SEM + NPS + discount_over_mrp + product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                   product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                   promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                    promotion_type.xNo_promotion + 
                   promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory)

summary(cam_model_8)
vif(cam_model_8)

#removing discount_over_mrp having high p value 

cam_model_9 <-lm(tot_gmv ~ list_price + TV + Sponsorship + 
                   X.Affiliates + SEM + NPS + product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                   product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                   promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                   promotion_type.xNo_promotion + 
                   promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory)

summary(cam_model_9)
vif(cam_model_9)


#removing NPS having high p value 

cam_model_10 <-lm(tot_gmv ~ list_price + TV + Sponsorship + 
                   X.Affiliates + SEM +product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                   product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                   promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                   promotion_type.xNo_promotion + 
                   promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory)

summary(cam_model_10)
vif(cam_model_10)

#removing NPS having high p value 

cam_model_11 <-lm(tot_gmv ~ TV + Sponsorship + 
                    X.Affiliates + SEM +product_analytic_vertical.xCameraAccessory + 
                    product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                    product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                    product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraLEDLight + 
                    product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                    product_analytic_vertical.xCameraRemoteControl + 
                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + product_analytic_vertical.xFlashShoeAdapter + 
                    product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                    product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                    product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                    promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                    promotion_type.xNo_promotion + 
                    promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory)

summary(cam_model_11)
vif(cam_model_11)

#removing product_analytic_vertical.xTeleconverter having high p value 

cam_model_12 <-lm(tot_gmv ~ TV + Sponsorship + 
                    X.Affiliates + SEM +product_analytic_vertical.xCameraAccessory + 
                    product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                    product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                    product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraLEDLight + 
                    product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                    product_analytic_vertical.xCameraRemoteControl + 
                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + product_analytic_vertical.xFlashShoeAdapter + 
                    product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                    product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                    product_analytic_vertical.xTelescope + 
                    promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                    promotion_type.xNo_promotion + 
                    promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory)

summary(cam_model_12)
vif(cam_model_12)

#removing product_analytic_vertical.xFlashShoeAdapter having high p value 

cam_model_13 <-lm(tot_gmv ~ TV + Sponsorship + 
                    X.Affiliates + SEM +product_analytic_vertical.xCameraAccessory + 
                    product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                    product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                    product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraLEDLight + 
                    product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                    product_analytic_vertical.xCameraRemoteControl + 
                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                    product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                    product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                    product_analytic_vertical.xTelescope + 
                    promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                    promotion_type.xNo_promotion + 
                    promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory)

summary(cam_model_13)
vif(cam_model_13)

#removing product_analytic_vertical.xCameraLEDLight having high p value 

cam_model_14 <-lm(tot_gmv ~ TV + Sponsorship + 
                    X.Affiliates + SEM +product_analytic_vertical.xCameraAccessory + 
                    product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                    product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                    product_analytic_vertical.xCameraHousing + 
                    product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                    product_analytic_vertical.xCameraRemoteControl + 
                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                    product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                    product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                    product_analytic_vertical.xTelescope + 
                    promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                    promotion_type.xNo_promotion + 
                    promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory)

summary(cam_model_14)
vif(cam_model_14)

#removing product_analytic_vertical.xReflectorUmbrella having high p value 

cam_model_15 <-lm(tot_gmv ~ TV + Sponsorship + 
                    X.Affiliates + SEM +product_analytic_vertical.xCameraAccessory + 
                    product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                    product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                    product_analytic_vertical.xCameraHousing + 
                    product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                    product_analytic_vertical.xCameraRemoteControl + 
                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                    product_analytic_vertical.xLens + 
                    product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                    product_analytic_vertical.xTelescope + 
                    promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                    promotion_type.xNo_promotion + 
                    promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory)

summary(cam_model_15)
vif(cam_model_15)

#removing promotion_type.xDaussera.sale ,promotion_type.xNo_promotion having high p value 

cam_model_16 <-lm(tot_gmv ~ TV + Sponsorship + 
                    X.Affiliates + SEM +product_analytic_vertical.xCameraAccessory + 
                    product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                    product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                    product_analytic_vertical.xCameraHousing + 
                    product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                    product_analytic_vertical.xCameraRemoteControl + 
                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                    product_analytic_vertical.xLens + 
                    product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                    product_analytic_vertical.xTelescope + 
                    promotion_type.xChristmas...New.Year.Sale +
                    promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory)

summary(cam_model_16) #0.7801
vif(cam_model_16)

#removing promotion_type.xChristmas...New.Year.Sale having high p value 

cam_model_17 <-lm(tot_gmv ~ TV + Sponsorship + 
                    X.Affiliates + SEM +product_analytic_vertical.xCameraAccessory + 
                    product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                    product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                    product_analytic_vertical.xCameraHousing + 
                    product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                    product_analytic_vertical.xCameraRemoteControl + 
                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                    product_analytic_vertical.xLens + 
                    product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                    product_analytic_vertical.xTelescope + 
                    promotion_type.xRakshabandhan.Sale,data=train_CameraAccessory)

summary(cam_model_17) #0.7732
vif(cam_model_17)



Final_camaccessory_model<-cam_model_17

##################Test the model on test data#############################
#######  taking final model as cam_model_17 ##############
#value of Adjusted R-squared of cam_model_17 is ~ 77%.

#Now, let us move forward to test the model on test data. 
Predict<-predict(Final_camaccessory_model, test_CameraAccessory)
#r-squared value between the predicted and actual values of sales
(cor(test_CameraAccessory$tot_gmv,Predict))^2 # 83.03% ~83%


########## Calculate Elaticity ################################
#To calculate Price Elasticity of Demand we use the formula:
#  PE = (??Q/??P) * (P/Q)
#(??Q/??P) is determined by the coefficient in our regression formula.
#To determine (P/Q) we will use the mean of independent variable.



elasticity <- function(var) {
  x <- as.numeric(Final_camaccessory_model$coefficients[var] * mean(train_CameraAccessory[,var])/mean(train_CameraAccessory$tot_gmv))
  return(x)
}

var_list_cam <- list()

for(i in 2:length(Final_camaccessory_model$coefficients)) {
  var_list_cam[i-1] <- elasticity(names(Final_camaccessory_model$coefficients)[i])
}

elasticity.outputs.cam <- data.frame(names(Final_camaccessory_model$coefficients[2:length(Final_camaccessory_model$coefficients)]))
elasticity.outputs.cam <- cbind(elasticity.outputs.cam,do.call(rbind.data.frame, var_list_cam))
colnames(elasticity.outputs.cam) <- c("Variable","Elasticity")

elasticity.outputs.cam$direction <- ifelse(elasticity.outputs.cam$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs.cam, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity", fill="palevioletred4") + 
  coord_flip() +
  ggtitle("Camera Accessory - Linear Model") +xlab("Variables") +p_theme


###################  Cross validation ###############################


cam_lm_cv <- cv.lm(data = train_CameraAccessory, form.lm = formula(tot_gmv ~ TV + Sponsorship + 
                                                                     X.Affiliates + SEM +product_analytic_vertical.xCameraAccessory + 
                                                                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                                                                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                                                                     product_analytic_vertical.xCameraHousing + 
                                                                     product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                                                                     product_analytic_vertical.xCameraRemoteControl + 
                                                                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                                                                     product_analytic_vertical.xLens + 
                                                                     product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                                                                     product_analytic_vertical.xTelescope + 
                                                                     promotion_type.xRakshabandhan.Sale), m =10)

#ms = 0.236

################################################################################
#                                Gaming Accessories
################################################################################

GameAccessory_linear.df <-GamingAccessory
GameAccessory_linear.df$week_no <- NULL
GameAccessory_linear.df$Month <- NULL


#Scaling the dataset
GameAccessory_linear.df[,1:13] <- data.frame(scale(GameAccessory_linear.df[,1:13], center = TRUE))

########## separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(GameAccessory_linear.df), 0.7*nrow(GameAccessory_linear.df))
train_GameAccessory = GameAccessory_linear.df[trainindices,]
test_GameAccessory = GameAccessory_linear.df[-trainindices,]
nrow(train_GameAccessory) #499 obs
nrow(test_GameAccessory) #214 obs.

########## Build model 1 containing all variables
GMA_model_1 <-lm(tot_gmv~.,data=train_GameAccessory)
summary(GMA_model_1) # Adjusted R-squared:  0.753 

##########Use stepAIC 
step_GMA <- stepAIC(GMA_model_1, direction="both")
#Step:  AIC=-723
summary(step_GMA) #0.757 
vif(step_GMA)
alias(step_GMA)

GMA_model_2 <-lm(tot_gmv ~ TV + Digital + Sponsorship + X.Affiliates + SEM + Radio + 
                   NPS + discount_over_mrp + product_analytic_vertical.xGamePad + 
                   product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingChargingStation + 
                   product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                   product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                   product_analytic_vertical.xJoystickGamingWheel + product_analytic_vertical.xMotionController + 
                   promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                   promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                   promotion_type.xRakshabandhan.Sale,data=train_GameAccessory)

summary(GMA_model_2)
vif(GMA_model_2)

#removing Digital having high pvalue and high VIF
GMA_model_3 <-lm(tot_gmv ~ TV + Sponsorship + X.Affiliates + SEM + Radio + 
                   NPS + discount_over_mrp + product_analytic_vertical.xGamePad + 
                   product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingChargingStation + 
                   product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                   product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                   product_analytic_vertical.xJoystickGamingWheel + product_analytic_vertical.xMotionController + 
                   promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                   promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                   promotion_type.xRakshabandhan.Sale,data=train_GameAccessory)

summary(GMA_model_3)
vif(GMA_model_3)


#removing Radio having high pvalue 
GMA_model_4 <-lm(tot_gmv ~ TV + Sponsorship + X.Affiliates + SEM + 
                   NPS + discount_over_mrp + product_analytic_vertical.xGamePad + 
                   product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingChargingStation + 
                   product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                   product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                   product_analytic_vertical.xJoystickGamingWheel + product_analytic_vertical.xMotionController + 
                   promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                   promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                   promotion_type.xRakshabandhan.Sale,data=train_GameAccessory)

summary(GMA_model_4)
vif(GMA_model_4)

#removing product_analytic_vertical.xGamingAccessoryKit having high pvalue 
GMA_model_5 <-lm(tot_gmv ~ TV + Sponsorship + X.Affiliates + SEM + 
                   NPS + discount_over_mrp + product_analytic_vertical.xGamePad + 
                   product_analytic_vertical.xGamingChargingStation + 
                   product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                   product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                   product_analytic_vertical.xJoystickGamingWheel + product_analytic_vertical.xMotionController + 
                   promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                   promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                   promotion_type.xRakshabandhan.Sale,data=train_GameAccessory)

summary(GMA_model_5)
vif(GMA_model_5)

#removing product_analytic_vertical.xMotionController having high pvalue 
GMA_model_6 <-lm(tot_gmv ~ TV + Sponsorship + X.Affiliates + SEM + 
                   NPS + discount_over_mrp + product_analytic_vertical.xGamePad + 
                   product_analytic_vertical.xGamingChargingStation + 
                   product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                   product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                   product_analytic_vertical.xJoystickGamingWheel + 
                   promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                   promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                   promotion_type.xRakshabandhan.Sale,data=train_GameAccessory)

summary(GMA_model_6)
vif(GMA_model_6)


#removing product_analytic_vertical.xJoystickGamingWheel having high pvalue 
GMA_model_7 <-lm(tot_gmv ~ TV + Sponsorship + X.Affiliates + SEM + 
                   NPS + discount_over_mrp + product_analytic_vertical.xGamePad + 
                   product_analytic_vertical.xGamingChargingStation + 
                   product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                   product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                   promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                   promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                   promotion_type.xRakshabandhan.Sale,data=train_GameAccessory)

summary(GMA_model_7)
vif(GMA_model_7)

#removing discount_over_mrp having high pvalue 
GMA_model_8 <-lm(tot_gmv ~ TV + Sponsorship + X.Affiliates + SEM + 
                   NPS + product_analytic_vertical.xGamePad + 
                   product_analytic_vertical.xGamingChargingStation + 
                   product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                   product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                   promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                   promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                   promotion_type.xRakshabandhan.Sale,data=train_GameAccessory)

summary(GMA_model_8)
vif(GMA_model_8)

#removing product_analytic_vertical.xGamingSpeaker + having high pvalue 
GMA_model_9 <-lm(tot_gmv ~ TV + Sponsorship + X.Affiliates + SEM + 
                   NPS + product_analytic_vertical.xGamePad +
                   product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                   product_analytic_vertical.xGamingMouse +  
                   promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                   promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                   promotion_type.xRakshabandhan.Sale,data=train_GameAccessory)

summary(GMA_model_9)
vif(GMA_model_9)


#removing promotion_type.xDaussera.sale + having high pvalue 
GMA_model_10 <-lm(tot_gmv ~ TV + Sponsorship + X.Affiliates + SEM + 
                   NPS + product_analytic_vertical.xGamePad +
                   product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                   product_analytic_vertical.xGamingMouse +  
                   promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                   promotion_type.xIndependence.Sale + 
                   promotion_type.xRakshabandhan.Sale,data=train_GameAccessory)

summary(GMA_model_10)
vif(GMA_model_10)

#removing promotion_type.xBSD.5 having high pvalue 
GMA_model_11 <-lm(tot_gmv ~ TV + Sponsorship + X.Affiliates + SEM + 
                    NPS + product_analytic_vertical.xGamePad +
                    product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                    product_analytic_vertical.xGamingMouse +  
                    promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xIndependence.Sale + 
                    promotion_type.xRakshabandhan.Sale,data=train_GameAccessory)

summary(GMA_model_11)
vif(GMA_model_11)


#removing TV having high pvalue 
GMA_model_12 <-lm(tot_gmv ~ Sponsorship + X.Affiliates + SEM + 
                    NPS + product_analytic_vertical.xGamePad +
                    product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                    product_analytic_vertical.xGamingMouse +  
                    promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xIndependence.Sale + 
                    promotion_type.xRakshabandhan.Sale,data=train_GameAccessory)

summary(GMA_model_12)
vif(GMA_model_12)

#removing SEM having high pvalue 
GMA_model_13 <-lm(tot_gmv ~ Sponsorship + X.Affiliates + 
                    NPS + product_analytic_vertical.xGamePad +
                    product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                    product_analytic_vertical.xGamingMouse +  
                    promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xIndependence.Sale + 
                    promotion_type.xRakshabandhan.Sale,data=train_GameAccessory)

summary(GMA_model_13)
vif(GMA_model_13)

#removing Sponsorship having high pvalue 
GMA_model_14 <-lm(tot_gmv ~ X.Affiliates + 
                    NPS + product_analytic_vertical.xGamePad +
                    product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                    product_analytic_vertical.xGamingMouse +  
                    promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xIndependence.Sale + 
                    promotion_type.xRakshabandhan.Sale,data=train_GameAccessory)

summary(GMA_model_14)
vif(GMA_model_14)

#removing promotion_type.xChristmas...New.Year.Sale having high pvalue 
GMA_model_15 <-lm(tot_gmv ~ X.Affiliates + 
                    NPS + product_analytic_vertical.xGamePad +
                    product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                    product_analytic_vertical.xGamingMouse +
                    promotion_type.xIndependence.Sale + 
                    promotion_type.xRakshabandhan.Sale,data=train_GameAccessory)

summary(GMA_model_15)
vif(GMA_model_15)

#removing NPS having high pvalue 
GMA_model_16 <-lm(tot_gmv ~ X.Affiliates + 
                    product_analytic_vertical.xGamePad +
                    product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                    product_analytic_vertical.xGamingMouse +
                    promotion_type.xIndependence.Sale + 
                    promotion_type.xRakshabandhan.Sale,data=train_GameAccessory)

summary(GMA_model_16)
vif(GMA_model_16)

Final_GamingAccessory_model<-GMA_model_16

##################Test the model on test data#############################
#######  taking final model as GMA_model_16 ##############
#value of Adjusted R-squared of GMA_model_16 is ~ 73%.

#Now, let us move forward to test the model on test data. 
Predict<-predict(Final_GamingAccessory_model, test_GameAccessory)
#r-squared value between the predicted and actual values of sales
(cor(test_GameAccessory$tot_gmv,Predict))^2 # 60% 


########## Calculate Elaticity ################################
#To calculate Price Elasticity of Demand we use the formula:
#  PE = (??Q/??P) * (P/Q)
#(??Q/??P) is determined by the coefficient in our regression formula.
#To determine (P/Q) we will use the mean of independent variable.


elasticity <- function(var) {
  x <- as.numeric(Final_GamingAccessory_model$coefficients[var])
  return(x)
}

var_list_game <- list()

for(i in 2:length(Final_GamingAccessory_model$coefficients)) {
  var_list_game[i-1] <- elasticity(names(Final_GamingAccessory_model$coefficients)[i])
}

elasticity.outputs.game <- data.frame(names(Final_GamingAccessory_model$coefficients[2:length(Final_GamingAccessory_model$coefficients)]))
elasticity.outputs.game <- cbind(elasticity.outputs.game,do.call(rbind.data.frame, var_list_game))
colnames(elasticity.outputs.game) <- c("Variable","Elasticity")

elasticity.outputs.game$direction <- ifelse(elasticity.outputs.game$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs.game, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" ,fill="tomato2") + 
  coord_flip() +
  ggtitle("Game Accessory - Linear Model") +xlab("Variables") +p_theme



###################  Cross validation ###############################


GMA_lm_cv <- cv.lm(data = train_GameAccessory, form.lm = formula(tot_gmv ~ X.Affiliates + 
                                                                   product_analytic_vertical.xGamePad +
                                                                   product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                                                                   product_analytic_vertical.xGamingMouse +
                                                                   promotion_type.xIndependence.Sale + 
                                                                   promotion_type.xRakshabandhan.Sale), m =10)

#ms = 0.265





################################################################################
#                                HomeAudio
################################################################################

HomeAudio_linear.df <-HomeAudio
HomeAudio_linear.df$week_no <- NULL
HomeAudio_linear.df$Month <- NULL

#Scaling the dataset
HomeAudio_linear.df[,1:13] <- data.frame(scale(HomeAudio_linear.df[,1:13], center = TRUE))

########## separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(HomeAudio_linear.df), 0.7*nrow(HomeAudio_linear.df))
train_HomeAudio = HomeAudio_linear.df[trainindices,]
test_HomeAudio = HomeAudio_linear.df[-trainindices,]
nrow(train_HomeAudio) #322 obs
nrow(test_HomeAudio) #138 obs.

########## Build model 1 containing all variables
HA_model_1 <-lm(tot_gmv~.,data=train_HomeAudio)
summary(HA_model_1) # Adjusted R-squared:  0.8545

##########Use stepAIC 
step_HA <- stepAIC(HA_model_1, direction="both")
#Step:  AIC=-612
summary(step_HA) #0.8582 
vif(step_HA)
alias(step_HA)

HA_model_2 <-lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + Online.marketing + 
                  X.Affiliates + Other + NPS + product_analytic_vertical.xDJController + 
                  product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                  product_analytic_vertical.xFMRadio + product_analytic_vertical.xHiFiSystem + 
                  product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xSoundMixer + 
                  promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                  promotion_type.xEid...Rathayatra.sale,data=train_HomeAudio)

summary(HA_model_2)
vif(HA_model_2)

# Removing Online.marketing having high p value and high VIF
HA_model_3 <-lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                  X.Affiliates + Other + NPS + product_analytic_vertical.xDJController + 
                  product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                  product_analytic_vertical.xFMRadio + product_analytic_vertical.xHiFiSystem + 
                  product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xSoundMixer + 
                  promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                  promotion_type.xEid...Rathayatra.sale,data=train_HomeAudio)

summary(HA_model_3)
vif(HA_model_3)

# Removing X.Affiliates having high p value and high VIF
HA_model_4 <-lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                  Other + NPS + product_analytic_vertical.xDJController + 
                  product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                  product_analytic_vertical.xFMRadio + product_analytic_vertical.xHiFiSystem + 
                  product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xSoundMixer + 
                  promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                  promotion_type.xEid...Rathayatra.sale,data=train_HomeAudio)

summary(HA_model_4)
vif(HA_model_4)

# Removing Other having high p value and high VIF
HA_model_5 <-lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                  NPS + product_analytic_vertical.xDJController + 
                  product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                  product_analytic_vertical.xFMRadio + product_analytic_vertical.xHiFiSystem + 
                  product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xSoundMixer + 
                  promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                  promotion_type.xEid...Rathayatra.sale,data=train_HomeAudio)

summary(HA_model_5)
vif(HA_model_5)


# Removing promotion_type.xBSD.5 having high p value and high VIF
HA_model_6 <-lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                  NPS + product_analytic_vertical.xDJController + 
                  product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                  product_analytic_vertical.xFMRadio + product_analytic_vertical.xHiFiSystem + 
                  product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xSoundMixer + 
                  promotion_type.xBig.Diwali.Sale + 
                  promotion_type.xEid...Rathayatra.sale,data=train_HomeAudio)

summary(HA_model_6)
vif(HA_model_6)

# Removing product_analytic_vertical.xHiFiSystem having high p value 
HA_model_7 <-lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                  NPS + product_analytic_vertical.xDJController + 
                  product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                  product_analytic_vertical.xFMRadio + 
                  product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xSoundMixer + 
                  promotion_type.xBig.Diwali.Sale + 
                  promotion_type.xEid...Rathayatra.sale,data=train_HomeAudio)

summary(HA_model_7)
vif(HA_model_7)



# Removing product_analytic_vertical.xSoundMixer having high p value 
HA_model_8 <-lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                  NPS + product_analytic_vertical.xDJController + 
                  product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                  product_analytic_vertical.xFMRadio + 
                  product_analytic_vertical.xHomeAudioSpeaker + 
                  promotion_type.xBig.Diwali.Sale + 
                  promotion_type.xEid...Rathayatra.sale,data=train_HomeAudio)

summary(HA_model_8)
vif(HA_model_8)

# Removing product_analytic_vertical.xDJController having high p value 
HA_model_9 <-lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                  NPS +product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                  product_analytic_vertical.xFMRadio + 
                  product_analytic_vertical.xHomeAudioSpeaker + 
                  promotion_type.xBig.Diwali.Sale + 
                  promotion_type.xEid...Rathayatra.sale,data=train_HomeAudio)

summary(HA_model_9)
vif(HA_model_9)

# Removing NPS having high p value 
HA_model_10 <-lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                  product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                  product_analytic_vertical.xFMRadio + 
                  product_analytic_vertical.xHomeAudioSpeaker + 
                  promotion_type.xBig.Diwali.Sale + 
                  promotion_type.xEid...Rathayatra.sale,data=train_HomeAudio)

summary(HA_model_10)
vif(HA_model_10)

# Removing promotion_type.xBig.Diwali.Sale
 
HA_model_11 <-lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                   product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                   product_analytic_vertical.xFMRadio + 
                   product_analytic_vertical.xHomeAudioSpeaker + 
                   promotion_type.xEid...Rathayatra.sale,data=train_HomeAudio)

summary(HA_model_11)
vif(HA_model_11)

# Removing product_analytic_vertical.xDock
HA_model_12 <-lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                   product_analytic_vertical.xDockingStation + 
                   product_analytic_vertical.xFMRadio + 
                   product_analytic_vertical.xHomeAudioSpeaker + 
                   promotion_type.xEid...Rathayatra.sale,data=train_HomeAudio)

summary(HA_model_12)
vif(HA_model_12)

# Removing product_analytic_vertical.xDockingStation
HA_model_13 <-lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                   product_analytic_vertical.xFMRadio + 
                   product_analytic_vertical.xHomeAudioSpeaker + 
                   promotion_type.xEid...Rathayatra.sale,data=train_HomeAudio)

summary(HA_model_13)
vif(HA_model_13)

# Removing promotion_type.xEid...Rathayatra.sale
HA_model_14 <-lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                   product_analytic_vertical.xFMRadio + 
                   product_analytic_vertical.xHomeAudioSpeaker ,data=train_HomeAudio)

summary(HA_model_14)
vif(HA_model_14)

# Removing promotion_type.xEid...Rathayatra.sale
HA_model_15 <-lm(tot_gmv ~ Digital + Sponsorship +  
                   product_analytic_vertical.xFMRadio + 
                   product_analytic_vertical.xHomeAudioSpeaker ,data=train_HomeAudio)

summary(HA_model_15)
vif(HA_model_15)


# Removing promotion_type.xEid...Rathayatra.sale
HA_model_16 <-lm(tot_gmv ~ Sponsorship +  
                   product_analytic_vertical.xFMRadio + 
                   product_analytic_vertical.xHomeAudioSpeaker ,data=train_HomeAudio)

summary(HA_model_16)
vif(HA_model_16)

Final_HomeAudio_model<-HA_model_16

##################Test the model on test data#############################
#######  taking final model as HA_model_16 ##############
#value of Adjusted R-squared of HA_model_16 is ~ 84%.

#Now, let us move forward to test the model on test data. 
Predict<-predict(Final_HomeAudio_model, test_HomeAudio)
#r-squared value between the predicted and actual values of sales
(cor(test_HomeAudio$tot_gmv,Predict))^2 # 81.16% ~81%


########## Calculate Elaticity ################################
#To calculate Price Elasticity of Demand we use the formula:
#  PE = (??Q/??P) * (P/Q)
#(??Q/??P) is determined by the coefficient in our regression formula.
#To determine (P/Q) we will use the mean of independent variable.

elasticity <- function(var) {
  x <- as.numeric(Final_HomeAudio_model$coefficients[var])
   return(x)
}

var_list_home <- list()

for(i in 2:length(Final_HomeAudio_model$coefficients)) {
  var_list_home[i-1] <- elasticity(names(Final_HomeAudio_model$coefficients)[i])
}

elasticity.outputs.home <- data.frame(names(Final_HomeAudio_model$coefficients[2:length(Final_HomeAudio_model$coefficients)]))
elasticity.outputs.home <- cbind(elasticity.outputs.home,do.call(rbind.data.frame, var_list_home))
colnames(elasticity.outputs.home) <- c("Variable","Elasticity")

elasticity.outputs.home$direction <- ifelse(elasticity.outputs.home$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs.home, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity", fill="tomato2") + 
  coord_flip() +
  ggtitle("Home Accessory - Linear Model") +xlab("Variables") +p_theme



###################  Cross validation ###############################


HA_lm_cv <- cv.lm(data = train_HomeAudio, form.lm = formula(tot_gmv ~ Sponsorship +  
                                                              product_analytic_vertical.xFMRadio + 
                                                              product_analytic_vertical.xHomeAudioSpeaker), m =10)

#ms = 0.163


