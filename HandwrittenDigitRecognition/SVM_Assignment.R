###Loading the required packages

library(gridExtra)
library(readr)
library(dplyr)
library(e1071)
library(caret)
library(kernlab)
library(ggplot2)
library(MASS)
library(caTools)
library(doParallel)
###reading the input files

input_data <- read.csv("mnist_train.csv", stringsAsFactors = F,header = F)
test_data <- read.csv("mnist_test.csv", stringsAsFactors = F,header = F)

# Checking missing value, No missing value
sapply(input_data, function(x) sum(is.na(x)))

# Checking outlier value, No outlier values
sapply(input_data, function(x) quantile(x, seq(0, 1, 0.01)))


####Taking 15 % of total data for training

set.seed(1)
sample.indices <- sample(1:nrow(input_data), 0.15 * nrow(input_data))
sample_data <- input_data[sample.indices,]
View(sample_data)

####renaming the first column to Digit

colnames(sample_data)[1] <- 'Digit'
colnames(test_data)[1] <- 'Digit'


##Converting the Digit as factor

sample_data$Digit <- factor(sample_data$Digit)
test_data$Digit <- factor(test_data$Digit)


##Scale all the fields except the Digit

#temporary dataframe for Digit
keep <- as.data.frame(sample_data$Digit)
keeptest <- as.data.frame(test_data$Digit)

#null Digit variable in both datasets
sample_data$Digit <- NA
test_data$Digit <- NA


sample_data <- as.data.frame(scale(sample_data))
sample_data[is.na(sample_data)] <- 0 #replace NA with 0
test_data <- as.data.frame(scale(test_data))
test_data[is.na(test_data)] <- 0

#add back Digit
sample_data$Digit <- keep$`sample_data$Digit`
test_data$Digit <- keeptest$`test_data$Digit`
View(sample_data)

###Creating Model 

#1. Using Linear Kernel
Model_linear <- ksvm(Digit ~ ., data = sample_data, scaled =F , kernel = "vanilladot")
Eval_linear <- predict(Model_linear, test_data)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear, test_data$Digit)

#2. Using rbf Kernel

Model_RBF <- ksvm(Digit ~ ., data = sample_data, scaled = F, kernel = "rbfdot")

Eval_RBF <- predict(Model_RBF, test_data)

#confusion matrix - rbf Kernel
confusionMatrix(Eval_RBF, test_data$Digit)


############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 5 implies Number of folds in CV.

trainControl <- trainControl(method = "cv", number = 5,verboseIter = T)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(7)
grid <- expand.grid(.sigma = seq(0.01, 0.05,by=0.01), .C = seq(1,5,by=1))

View(grid)

#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

cl <- makeCluster(detectCores())
registerDoParallel(cl)
fit.svm <- train(Digit ~ ., data = sample_data, method = "svmRadial", metric = metric, allowParallel=T,tuneGrid = grid, trControl = trainControl)
stopCluster(cl)

print(fit.svm)

plot(fit.svm)

###Based on the plotfound that for C=2 and Sigma=0.001 we get the best accuracy

##Creating model using the above C and sigm value
Model_RBF_Final <- ksvm(Digit ~ ., data = sample_data, scaled = F, kernel = "rbfdot",C = 2, sigma = 0.01)
Eval_RBF_Final <- predict(Model_RBF_Final, test_data)

#confusion matrix - rbf Kernel
confusionMatrix(Eval_RBF_Final, test_data$Digit)

####The Accuracy is-0.9456################

#Statistics by Class:

#                     Class:0 Class:1 Class:2 Class:3 Class:4 Class:5 Class:6 Class:7 Class:8 Class:9
#Sensitivity           0.9796 0.9903 0.9525   0.9337  0.9460  0.9327  0.9478  0.9300  0.9158  0.9207
#Specificity           0.9967 0.9966 0.9852   0.9952  0.9947  0.9937  0.9964  0.9932  0.9938  0.9941
