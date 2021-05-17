library(tidyverse)
library(neuralnet)
library(caret)
library(GGally)
library(bslib)
library(DT)
data <- readRDS("OHIE_Final_Selection.RDS")

# imputation
data_nNA1 <- drop_na(data)

#Visualization of data, need to export as pdf and set size larger for data to be 
#to figure out relationship between features.
#ggpairs(data_nNA1, title = "Scatterplot Matrix of the Features of the OHIE Data Set")

### Normalize data 
summary(data_nNA1)
str(data_nNA1)
data_nNA1$sex <- as.numeric(data_nNA1$sex)
data_nNA1 <- mutate_if(data_nNA1, is.logical, as.numeric)

#Min Max scaling
preproc1 <- preProcess(data_nNA1, method = c("range"))
data_standardized <- predict(preproc1, data_nNA1)
#data_standardized <- bind_cols(data_standardized, data_nNA1[,13])
str(data_standardized)

#Test and Train Data
n <- nrow(data_standardized)
set.seed(69)
index <- sample(1:n, size = round(0.7*n), replace=FALSE)
train <- data_standardized[index,]
test <-data_standardized[-index,]

####### nn 1 #########
#1 Hidden layer, 1 Neuron ANN, logistic activation, threshold 0.01
#computation time ≈30sec
nn1 <- neuralnet(charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + 
                   any_ed_chronic_condition + any_ed_injury + any_ed_skin_condition + 
                   any_ed_abdominal_pain + any_ed_back_pain + any_ed_heart_or_chest_pain + 
                   any_ed_headache + any_ed_depression + charge_food_assistance +
                   charge_temporary_assistance, data = train, stepmax = 1e+07, threshold = 0.01,
                 act.fct = "logistic", lifesign = "full")
plot(nn1, rep = 'best')

#SSE of train
nn1_Train_SSE <- sum((nn1$net.result - train[, 13])^2)/2
paste("SSE train: ", round(nn1_Train_SSE, 4))

#SSE of test
Test_nn2_output <- predict(nn1, test[, c(1:12, 14,15)])
nn1_Test_SSE <- sum((Test_nn1_output - test[13])^2)/2
paste("SSE test: ", round(nn1_Test_SSE, 4))
######### nn 2 #########
#1 Hidden Layer, 1 Neuron ANN, sofplus activation, threshold 0.02
#computation time ≈8.6min
#activation function 
#would love to use ReLU but non-differentiability not supported by neuralnet package.
#softplus as smooth approximation of ReLU
softplus <- function(x) log(1+exp(x))
nn2 <- neuralnet(charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + 
                  any_ed_chronic_condition + any_ed_injury + any_ed_skin_condition + 
                  any_ed_abdominal_pain + any_ed_back_pain + any_ed_heart_or_chest_pain + 
                  any_ed_headache + any_ed_depression + charge_food_assistance + 
                  charge_temporary_assistance, data = train, stepmax = 1e+07, threshold = 0.02,
                 act.fct = softplus, lifesign = "full")
plot(nn2, rep = 'best')

#SSE of train
nn2_Train_SSE <- sum((nn2$net.result - train[, 13])^2)/2
paste("SSE train: ", round(nn2_Train_SSE, 4))

#SSE of test
Test_nn2_output <- predict(nn2, test[, c(1:12, 14,15)])
nn2_Test_SSE <- sum((Test_nn2_output - test[13])^2)/2
paste("SSE test: ", round(nn2_Test_SSE, 4))


####### nn 3 #########
#9 Hidden layer (aggregation of 2/3 of input layer.), 1 Neuron ANN, logistic activation, threshold 0.02
#computation time ≈30sec
nn3 <- neuralnet(charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + 
                  any_ed_chronic_condition + any_ed_injury + any_ed_skin_condition + 
                  any_ed_abdominal_pain + any_ed_back_pain + any_ed_heart_or_chest_pain + 
                  any_ed_headache + any_ed_depression + charge_food_assistance +
                  charge_temporary_assistance, data = train, hidden = c(9,1), stepmax = 1e+07, threshold = 0.02,
                  act.fct = "logistic", lifesign = "full")
plot(nn3, rep = 'best')

#SSE of train
nn3_Train_SSE <- sum((nn3$net.result - train[, 13])^2)/2
paste("SSE train: ", round(nn3_Train_SSE, 4))

#SSE of test
Test_nn3_output <- predict(nn3, test[, c(1:13, 14,15)])
nn3_Test_SSE <- sum((Test_nn3_output - test[13])^2)/2
paste("SSE test: ", round(nn3_Test_SSE, 4))

######## nn 4 #########
#9 Hidden layer, 1 Neuron ANN, softplus activation, threshold = 0.02
#computation time ≈9.72min
nn4 <- neuralnet(charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + 
                  any_ed_chronic_condition + any_ed_injury + any_ed_skin_condition + 
                  any_ed_abdominal_pain + any_ed_back_pain + any_ed_heart_or_chest_pain + 
                  any_ed_headache + any_ed_depression + charge_food_assistance +
                  charge_temporary_assistance, data = train, hidden = c(9,1), stepmax = 1e+07, threshold = 0.02,
                  act.fct = softplus, lifesign = "full")
plot(nn4, rep = 'best')

#SSE of train
nn4_Train_SSE <- sum((nn4$net.result - train[, 13])^2)/2
paste("SSE train: ", round(nn4_Train_SSE, 4))

#SSE of test
Test_nn4_output <- predict(nn4, test[, c(1:13, 14,15)])
nn4_Test_SSE <- sum((Test_nn4_output - test[13])^2)/2
paste("SSE test: ", round(nn4_Test_SSE, 4))

Regression_nn_Errors <- tibble(Network = rep(c("nn1", "nn2", "nn3", "nn4"), each = 2), 
                               DataSet = rep(c("Train", "Test"), time = 4), 
                               SSE = c(nn1_Train_SSE, nn1_Test_SSE, 
                                       nn2_Train_SSE, nn2_Test_SSE, 
                                       nn3_Train_SSE, nn3_Test_SSE, 
                                       nn4_Train_SSE, nn4_Test_SSE))

Regression_nn_Errors %>% 
  ggplot(aes(Network, SSE, fill = DataSet)) + 
  geom_col(position = "dodge") + 
  ggtitle("Regression ANN's SSE")

#the more complex our NN , the worse it actually gets.
#NN is having a really hard time working through the sparse underlying dataset. 
save(nn2, file="nn2.Rdata")
save(nn4, file="nn4.Rdata")

