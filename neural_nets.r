library(tidyverse)
library(neuralnet)
library(caret)
library(GGally)

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

#1 Hidden Layer, 1 Neuron ANN, sofplus activation
#activation function 
#would love to use ReLU but non-differentiability not supported by neuralnet package.
#softplus as smooth approximation of ReLU
softplus <- function(x) log(1+exp(x))
nn1 <- neuralnet(charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + 
                  any_ed_chronic_condition + any_ed_injury + any_ed_skin_condition + 
                  any_ed_abdominal_pain + any_ed_back_pain + any_ed_heart_or_chest_pain + 
                  any_ed_headache + any_ed_depression + charge_food_assistance + 
                  charge_temporary_assistance, data = train, stepmax = 1e+07, threshold = 0.02,
                 act.fct = softplus, lifesign = "full")
plot(nn1, rep = 'best')

#SSE of train
NN1_Train_SSE <- sum((nn1$net.result - train[, 13])^2)/2
paste("SSE: ", round(NN1_Train_SSE, 4))

test[13]
#SSE of test
Test_nn1_output <- predict(nn1, test[, c(1:12, 14,15)])
NN1_Test_SSE <- sum((Test_nn1_output - test[13])^2)/2
NN1_Test_SSE
