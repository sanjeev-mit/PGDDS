##################Camera Accessory#####################

###Checking NA value##############
sum(is.na(CameraAccessory)) 

###Creating variable for Koyck modelling### 
Koyck_CameraAccessory <- CameraAccessory

library(DataCombine)

# Creating Lag variable with lag 1 for the dependent variable tot_gmv

Koyck_CameraAccessory <- slide(Koyck_CameraAccessory, Var = "tot_gmv",slideBy = -1)


###Removing NA value
koyck_Camera <- na.omit(Koyck_CameraAccessory)
View(koyck_Camera)

###Scaling the variables####

koyck_Camera <- cbind(data.frame(scale(koyck_Camera[,c(1:15)])),
                      data.frame(koyck_Camera[,16:77]),
                      data.frame(scale(koyck_Camera[,78])))

View(koyck_Camera)

# removing month and year columns
koyck_Camera <- koyck_Camera[,-c(1,2)]


# Creating train and test sets

set.seed(100)

training<- sample(1:nrow(koyck_Camera), 0.6*nrow(koyck_Camera))

train_data_cam <-  koyck_Camera[training,]
test_data_cam <- koyck_Camera[-training,]

## First overall model
Cam_Koyck_model1 <- lm(tot_gmv~.,train_data_cam)
summary(Cam_Koyck_model1)

###R-squared:  0.8362,	Adjusted R-squared:  0.822 

# Evaluating the first models for significant predictors

Cam_Koyck_model2 <- stepAIC(Cam_Koyck_model1,direction = "both")
summary(Cam_Koyck_model2)
vif(Cam_Koyck_model2)

##Multiple R-squared:  0.8342,	Adjusted R-squared:  0.8246

# removing Radio as vif is very high
Cam_Koyck_model3<- lm(formula = tot_gmv ~ list_price + TV + Digital + Sponsorship + 
                    Content.Marketing + X.Affiliates +NPS + 
                    product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                    product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                    product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                    product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                    product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                    product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                    product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                    product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                    promotion_type.xBig.Diwali.Sale + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                  data = train_data_cam)


summary(Cam_Koyck_model3)
###Multiple R-squared:  0.8313,	Adjusted R-squared:  0.8222 
vif(Cam_Koyck_model3)

# removing TV as p-value is very high
Cam_Koyck_model4<- lm(formula = tot_gmv ~ list_price + Digital + Sponsorship + 
                    Content.Marketing + X.Affiliates +
                    product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                    product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                    product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                    product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                    product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                    product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                    product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                    product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                    promotion_type.xBig.Diwali.Sale + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                  data =train_data_cam )

summary(Cam_Koyck_model4)

##Multiple R-squared:  0.8236,	Adjusted R-squared:  0.8148

vif(Cam_Koyck_model4)

# removing Content.Marketing as vif is high

Cam_Koyck_model5 <- lm(formula = tot_gmv ~ list_price + Digital + Sponsorship + 
                     X.Affiliates +
                     product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                     product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                     product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                     product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                     product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                   data =train_data_cam )

summary(Cam_Koyck_model5)

###Multiple R-squared:  0.8233,	Adjusted R-squared:  0.8148

vif(Cam_Koyck_model5)

# removing promotion_type.xBig.Diwali.Sale as high p-value
Cam_Koyck_model6 <-  lm(formula = tot_gmv ~ list_price + Digital + Sponsorship + 
                      X.Affiliates +
                      product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                      product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                      product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                      product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                      product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xChristmas...New.Year.Sale + 
                      promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                    data =train_data_cam )

summary(Cam_Koyck_model6)
##Multiple R-squared:  0.8228,	Adjusted R-squared:  0.8146 
vif(Cam_Koyck_model6)

# removing promotion_type.xChristmas...New.Year.Sale as it has relatively high p-value
Cam_Koyck_model7 <- lm(formula = tot_gmv ~ list_price + Digital + Sponsorship + X.Affiliates +
                     product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                     product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                     product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                     product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                     product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                   data =train_data_cam )
summary(Cam_Koyck_model7)
vif(Cam_Koyck_model7)

# final model after AIC and VIF tuning
final_koyck_camera <- Cam_Koyck_model7
summary(final_koyck_camera)
##Multiple R-squared:  0.8215,	Adjusted R-squared:  0.8136 
vif(final_koyck_camera)

par(mfrow = c(2,2))
plot(final_koyck_camera, main = "Final Camera Accessory - Koyck Model")
dev.off()

## Cross validation
cross_val <- cv.lm(data =train_data_cam, form.lm = formula(final_koyck_camera),m = 10,
                  main = "Cross Validation  of Camera Accessory Model")

View(cross_val)
?attr

attr(cross_val, "ms")
# Cross validation(ms) = 0.232

# Predicting Values on the test data using the final Model

pred_cam <- predict(final_koyck_camera, test_data_cam)
RMSE(test_data_cam$tot_gmv,pred_cam)


##Finding the R- square
cam_cor_koyck <- cor(test_data_cam$tot_gmv, pred_cam)
cam_rsquared <- cam_cor_koyck^2

#Rsquared on test: 0.731

###################################
#Finding elasticity of the Model

cam_model <- final_koyck_camera
View(cam_model)

elasticity <- function(x) {
  els <- as.numeric(cam_model$coefficients[x] * mean(train_data_cam[,x])/mean(train_data_cam$tot_gmv))
  return(els)
}

var_list <- list()

for(i in 2:length(cam_model$coefficients)) {
  var_list[i-1] <- elasticity(names(cam_model$coefficients)[i])
}

elasticity_OP <- data.frame(names(cam_model$coefficients[2:length(cam_model$coefficients)]))
elasticity_OP  <- cbind(elasticity_OP ,do.call(rbind.data.frame, var_list))
colnames(elasticity_OP) <- c("Variable","Elasticity")
View(elasticity_OP)

elasticity_OP$direction <- ifelse(elasticity_OP$Elasticity > 0, "Positive", "Negative")

###Plotting the elasticity 
ggplot(data=elasticity_OP, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Elasticity:Camera Accessory - Koyck Model") +xlab("Variables")


################## Gaming Accessory#####################

###Checking NA values ############

sum(is.na(GamingAccessory))
Koyck_GamingAccessory <- GamingAccessory
library(DataCombine)

# Creating Lag variable 
Koyck_GamingAccessory <- slide(Koyck_GamingAccessory, Var = "tot_gmv",slideBy = -1)

Koyck_GamingAccessory <- na.omit(Koyck_GamingAccessory)

str(Koyck_GamingAccessory)


###Scaling the variables####

Koyck_GamingAccessory <- cbind(data.frame(scale(Koyck_GamingAccessory[,c(1:15)])),
                      data.frame(Koyck_GamingAccessory[,16:77]),
                      data.frame(scale(Koyck_GamingAccessory[,78])))

str(Koyck_GamingAccessory)

# removing month and year columns
Koyck_GamingAccessory <- Koyck_GamingAccessory[,-c(1,2)]

# Training and test data creation

set.seed(100)
trainindices= sample(1:nrow(Koyck_GamingAccessory), 0.6*nrow(Koyck_GamingAccessory))
train_game = Koyck_GamingAccessory[trainindices,]
test_game = Koyck_GamingAccessory[-trainindices,]

## building first model with all fields
Koyck_model1 <- lm(tot_gmv~.,train_game)
summary(Koyck_model1)
##Multiple R-squared:  0.775,	Adjusted R-squared:  0.753 

# StepAIC over the first moodel for significant predictors
Koyck_model2 <- stepAIC(Koyck_model1,direction = "both")
summary(Koyck_model2)
vif(Koyck_model2)

#SEM has vif which is very high
Koyck_model3<- lm(formula = tot_gmv ~ TV + Digital + Sponsorship + X.Affiliates + 
                    NPS + product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingHeadset + 
                    product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                    product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xJoystickGamingWheel + 
                    promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                    promotion_type.xRakshabandhan.Sale, data = train_game)
summary(Koyck_model3)
##Multiple R-squared:  0.747,	Adjusted R-squared:  0.737
vif(Koyck_model3)

# removing product_analytic_vertical.xJoystickGamingWheel as p-value is moderately high
Koyck_model4<- lm(formula = tot_gmv ~ TV + Digital + Sponsorship + X.Affiliates + 
                    NPS + product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingHeadset + 
                    product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                    product_analytic_vertical.xGamingSpeaker + 
                    promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                    promotion_type.xRakshabandhan.Sale, data = train_game)
                    
summary(Koyck_model4)
##Multiple R-squared:  0.745,	Adjusted R-squared:  0.736 

vif(Koyck_model4)



# final model after AIC and VIF tuning
koyck_gaming_model <-  Koyck_model4
summary(koyck_gaming_model)
vif(koyck_gaming_model)

par(mfrow = c(2,2))
plot(koyck_gaming_model, main = "Gaming Accessory - Koyck Model")
dev.off()

## Performing 10 fold Cross validation
crossval <- cv.lm(data = train_game, form.lm = formula(koyck_gaming_model),m = 10,
                  main = "CV Gaming Accessory")
attr(crossval, "ms")
# Cross validation (ms) = 0.319

# Predicting test data
pred_cam <- predict(koyck_gaming_model, test_game)
RMSE(test_game$tot_gmv,pred_cam) ##0.579

game_cor <- cor(test_game$tot_gmv, pred_cam)
game_rsquared <- game_cor^2
game_rsquared

#Rsquared on test: 0.669

###################################
#Estimating elasticity 


elasticity <- function(x) {
  els <- as.numeric(koyck_gaming_model$coefficients[x] * mean(train_game[,x])/mean(train_game$tot_gmv))
  return(els)
}

var_list <- list()

for(i in 2:length(koyck_gaming_model$coefficients)) {
  var_list[i-1] <- elasticity(names(koyck_gaming_model$coefficients)[i])
}

elasticity_OP <- data.frame(names(koyck_gaming_model$coefficients[2:length(koyck_gaming_model$coefficients)]))
elasticity_OP <- cbind(elasticity_OP,do.call(rbind.data.frame, var_list))
colnames(elasticity_OP) <- c("Variable","Elasticity")
elasticity_OP$direction <- ifelse(elasticity_OP$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity_OP, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming Accessory - Koyck Model") +xlab("Variables")


################## Home Audio#####################

##NA values
sum(is.na(HomeAudio))
HomeAudio_Koyck <- HomeAudio

# Creating Lag variable 
HomeAudio_Koyck <- slide(HomeAudio_Koyck, Var = "tot_gmv",slideBy = -1)
str(HomeAudio_Koyck)
HomeAudio_Koyck<- na.omit(HomeAudio_Koyck)

HomeAudio_Koyck <- cbind(data.frame(scale(HomeAudio_Koyck[,c(1:15)])),
                               data.frame(HomeAudio_Koyck[,16:77]),
                               data.frame(scale(HomeAudio_Koyck[,78])))

str(HomeAudio_Koyck)
# removing month and year columns
HomeAudio_Koyck <- HomeAudio_Koyck[,-c(1,2)]

# splitting train & test data

trainindices= sample(1:nrow(HomeAudio_Koyck), 0.6*nrow(HomeAudio_Koyck))
train_home = HomeAudio_Koyck[trainindices,]
test_home = HomeAudio_Koyck[-trainindices,]

## building first lm model with all fields
Koyck_model1 <- lm(tot_gmv~.,train_home)

summary(Koyck_model1)
###Multiple R-squared:  0.792,	Adjusted R-squared:  0.765  

# Performing StepAIC on  first model for significant predictors
Koyck_model2 <- stepAIC(Koyck_model1,direction = "both")
summary(Koyck_model2)
##Multiple R-squared:  0.785,	Adjusted R-squared:  0.774 
vif(Koyck_model2)

# removing Online.marketing  as vif is very high
Koyck_model3<- lm(formula = tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                    X.Affiliates + discount_over_mrp + product_analytic_vertical.xDJController + 
                    product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                    product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + 
                    promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                  data = train_home)
summary(Koyck_model3)
##Multiple R-squared:  0.78,	Adjusted R-squared:  0.77 
vif(Koyck_model3)

# removing X.Affiliates as p-value is high
Koyck_model4 <-  lm(formula = tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                      discount_over_mrp + product_analytic_vertical.xDJController + 
                      product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                      product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + 
                      promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                    data = train_home)
summary(Koyck_model4)
##Multiple R-squared:  0.78,	Adjusted R-squared:  0.77
vif(Koyck_model4)

# removing promotion_type.xEid...Rathayatra.sale as it has high p-value
Koyck_model5 <-  lm(formula = tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                      discount_over_mrp + product_analytic_vertical.xDJController + 
                      product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                      product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + 
                      promotion_type.xDaussera.sale, 
                    data = train_home)
summary(Koyck_model5)
##Multiple R-squared:  0.778,	Adjusted R-squared:  0.77 
vif(Koyck_model5)


# removing product_analytic_vertical.xDJController as it has high p-value
Koyck_model6 <-  lm(formula = tot_gmv ~ Digital + Sponsorship +  
                      discount_over_mrp + product_analytic_vertical.xDock +  
                      product_analytic_vertical.xDockingStation + 
                      product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + 
                      promotion_type.xDaussera.sale, 
                    data = train_home)
summary(Koyck_model6)
##Multiple R-squared:  0.773,	Adjusted R-squared:  0.766 
vif(Koyck_model6)


# final model after AIC and VIF tuning
koyck_homeAudio <-  Koyck_model6

summary(koyck_homeAudio)
vif(koyck_homeAudio)
par(mfrow = c(2,2))
plot(koyck_homeAudio, main = "Home Audio - Koyck Model")
dev.off()

## Performing 10 fold  Cross validation
crossval <- cv.lm(data = train_home, form.lm = formula(koyck_homeAudio),m = 10, 
                  main = "Cross Validation Home Audio Model")
attr(crossval, "ms")


# Predicting Values of test data
pred_homeAudio <- predict(koyck_homeAudio, test_home)
RMSE(koyck_homeAudio$tot_gmv,pred_homeAudio)

homeA_corr <- cor(test_home$tot_gmv, pred_homeAudio)
homeA_rsquared <- homeA_corr^2
homeA_rsquared
#Rsquared on test: 0.896

###################################
#Estimating elasticity 


elasticity <- function(x) {
  els <- as.numeric(koyck_homeAudio$coefficients[x] * mean(train_home[,x])/mean(train_home$tot_gmv))
  return(els)
}

var_list <- list()

for(i in 2:length(koyck_homeAudio$coefficients)) {
  var_list[i-1] <- elasticity(names(koyck_homeAudio$coefficients)[i])
}

elasticity_OP <- data.frame(names(koyck_homeAudio$coefficients[2:length(koyck_homeAudio$coefficients)]))
elasticity_OP <- cbind(elasticity_OP,do.call(rbind.data.frame, var_list))
colnames(elasticity_OP) <- c("Variable","Elasticity")

elasticity_OP$direction <- ifelse(elasticity_OP$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity_OP, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" , fill="palevioletred4") + 
  coord_flip() +
  ggtitle("Home Audio - Koyck Model") +xlab("Variables") +p_theme
###########################################

