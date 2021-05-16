library(tidyverse)
library(neuralnet)
library(caret)

data <- readRDS("OHIE_Final_Selection.RDS")

# imputation
data_nNA1 <- drop_na(data)
### Normalize data 

summary(data_nNA1)
str(data_nNA1)
data_nNA1$sex <- as.numeric(data_nNA1$sex)
data_nNA1 <- mutate_if(data_nNA1, is.logical, as.numeric)

#Min Max scaling
preproc1 <- preProcess(data_nNA1[,c(1:12,14:15)], method = c("range"))
data_standardized <- predict(preproc1, data_nNA1[,c(1:12,14:15)])
data_standardized <- bind_cols(data_standardized, data_nNA1[,13])
str(data_standardized)

#Test and Train Data
n <- nrow(data_standardized)
set.seed(69)
index <- sample(1:n, size = round(0.7*n), replace=FALSE)
train <- data_standardized[index,]
test <-data_standardized[-index,]

### formula for NN
feats <- names(data_standardized[,1:14])

# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste("charge_total ~",f)

# Convert to formula
f <- as.formula(f)

#First NN
nn1 <- neuralnet(charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + 
                  any_ed_chronic_condition + any_ed_injury + any_ed_skin_condition + 
                  any_ed_abdominal_pain + any_ed_back_pain + any_ed_heart_or_chest_pain + 
                  any_ed_headache + any_ed_depression + charge_food_assistance + 
                  charge_temporary_assistance, data = train)
plot(nn1, rep = 'best')
