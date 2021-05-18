#OHIE_analysis

library(Metrics)
library(tidyverse)
library(corrplot)
library(Hmisc)

library(dlstats) #package to check download statistics of packages

library(tree)
library(randomForest)
library(e1071)
library(xgboost)

library(MuMIn)


#Data----
setwd("~/GitHub/BAna-DS")
ohie <- readRDS("OHIE_Wrangled.RDS")



#Variables

#dependent variable: charge_total <- includes all charges and food assistance is not paid for by insurance
formula <- "charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + any_ed_chronic_condition + 
any_ed_injury + any_ed_skin_condition + any_ed_abdominal_pain + any_ed_back_pain + 
  any_ed_heart_or_chest_pain + any_ed_headache + any_ed_depression + 
  any_ed_psychiatric_condition_or_substance_abuse + food_assistance + charge_food_assistance + 
  temporary_assistance + charge_temporary_assistance"

#dataframe with all variables that could generally be included in the model
ohieVariables <- ohie %>% 
  select(!c("person_id", "treatment", "dt_retro_coverage", "numhh_list", "numhh_list",
                                "ed_charge_total", "zip_msa_list")) #zip_msa_list would generally work as predictor but it
#contains only one possible value, which renders it useless for actual predictions

ohieVariables$sex <- as.numeric(ohieVariables$sex=="Female")


#Creating train and test data with different imputation methods

#no imputation
set.seed(1111)
index <- sample(x=c(1:length(ohie$person_id)), size=trunc(length(ohie$person_id)*0.7), replace = FALSE)
train <- ohieVariables[index,]
test <- ohieVariables[-index,]

#listwise deletion
ohieVariables_nNA <- drop_na(ohieVariables)

#dataframe with variables removed, that showed to decrease model performance
ohieVariables2 <- select(ohieVariables, !c("any_ed_psychiatric_condition_or_substance_abuse", "food_assistance",
                                           "temporary_assistance"))
ohieVariables_nNA2 <- drop_na(ohieVariables2)
#does not result in any more observations i.e. does not make a difference for model performance if 
#imputation method is listwise deletion
set.seed(1111)
index_nNA <- sample(x=c(1:length(ohieVariables_nNA$age)), size=trunc(length(ohieVariables_nNA$age)*0.7),
                    replace = FALSE)
train_nNA <- ohieVariables_nNA[index_nNA,]
test_nNA <- ohieVariables_nNA[-index_nNA,]

#mean substitution (with Hmisc)


ohieVariables_DepnNA <- drop_na(ohieVariables, any_of("charge_total"))

#ohieVariables_mean <- 




#Exploration----

cor_ohieVariables_nNA <- cor(ohieVariables_nNA)

corplot_ohieVariables_nNA <- corrplot(corr = cor_ohieVariables_nNA)



#Linear regression----

#Initial model
m.lm <- lm(charge_total ~ ., train_nNA, na.action = "na.fail")
#Multiple R-squared:  0.2681,	Adjusted R-squared:  0.2672     pretty bad...
p.lm <- predict.lm(m.lm, newdata = test_nNA)
rmse(test_nNA$charge_total, p.lm) #[1] 16709.41


#without logical values in conditional branches of survey
m.lm_nl <- lm(charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + any_ed_chronic_condition + 
                any_ed_injury + any_ed_skin_condition + any_ed_abdominal_pain + any_ed_back_pain + 
                any_ed_heart_or_chest_pain + any_ed_headache + any_ed_depression + 
                any_ed_psychiatric_condition_or_substance_abuse + charge_food_assistance + 
                charge_temporary_assistance, train_nNA, na.action = "na.fail")
p.lm_nl <- predict.lm(m.lm_nl, newdata = test_nNA)
rmse(test_nNA$charge_total, p.lm_nl) #[1] 16700.6

#without numerical values in conditional branches of survey
m.lm_nn <- lm(charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + any_ed_chronic_condition + 
                any_ed_injury + any_ed_skin_condition + any_ed_abdominal_pain + any_ed_back_pain + 
                any_ed_heart_or_chest_pain + any_ed_headache + any_ed_depression + 
                any_ed_psychiatric_condition_or_substance_abuse + food_assistance + 
                temporary_assistance, train_nNA, na.action = "na.fail")
p.lm_nn <- predict.lm(m.lm_nn, newdata = test_nNA)
rmse(test_nNA$charge_total, p.lm_nn) #[1] 16709.32
#Interpretation: The logical values resulting from the conditional branches of the survey do not add valuable information
#if the numerical value is in the dataset <- test with random forest shows the same


Sys.time()
#dred.lm <- dredge(global.model = m.lm_nn) #should take between 32 and 44 minutes; took only around 15min
#Restult: The model with the lowest AIC score (among of the possible models without the two variables for logical values in 
 #conditional branches of the survey) is the model with all remaining variables, except of 
 #"forany_ed_psychiatric_condition_or_substance_abuse"
Sys.time()

#model with lowest AICc
m.lm_aic <- lm(charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + any_ed_chronic_condition + 
                 any_ed_injury + any_ed_skin_condition + any_ed_abdominal_pain + any_ed_back_pain + 
                 any_ed_heart_or_chest_pain + any_ed_headache + any_ed_depression + charge_food_assistance + 
                 charge_temporary_assistance, train_nNA, na.action = "na.fail")
p.lm_aic <- predict.lm(m.lm_aic, newdata = test_nNA)
rmse(test_nNA$charge_total, p.lm_aic) #[1] 16700.77

#adding interaction terms with variable sex to highest AICc model
m.lm_it <- lm(charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + any_ed_chronic_condition + 
                 any_ed_injury + any_ed_skin_condition + any_ed_abdominal_pain + any_ed_back_pain + 
                 any_ed_heart_or_chest_pain + any_ed_headache + any_ed_depression + charge_food_assistance + 
                 charge_temporary_assistance + sex:any_ed_headache + sex:preperiod_any_visits +
                sex:age + sex:any_ed_visits + sex:any_ed_chronic_condition + sex:any_ed_injury +
                sex:any_ed_back_pain, train_nNA)
p.lm_it <- predict.lm(m.lm_it, newdata = test_nNA)
rmse(test_nNA$charge_total, p.lm_it) #16649.42
#Interaction terms with sex that led to worse results: any_ed_depressio, any_ed_skin_condition, any_ed_abdominal_pain
 #any_ed_heart_or_chest_pain, charge_food_assistance
 #did not change it at all: charge_temporary_assistance
#Result: Interaction terms with variable sex could improve the result a bit, but the model is still a bit worse than
 #the random forest model

#Support Vector Regression-----

#Initial model
m.sv <- svm(charge_total ~ ., train_nNA)
p.sv <- predict(m.sv, newdata = test_nNA)
rmse(test_nNA$charge_total, p.sv) #[1] 20437.4

#Logical features removed if both logical and numeric is available
m.sv_nn <- svm(charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + any_ed_chronic_condition + 
                 any_ed_injury + any_ed_skin_condition + any_ed_abdominal_pain + any_ed_back_pain + 
                 any_ed_heart_or_chest_pain + any_ed_headache + any_ed_depression + 
                 any_ed_psychiatric_condition_or_substance_abuse + food_assistance + 
                 temporary_assistance, train_nNA)
p.sv_nn <- predict(m.sv_nn, newdata = test_nNA)
rmse(test_nNA$charge_total, p.sv_nn) #[1] 17439.04

#features that were used in the model with the highest AIC in lm
m.sv_aic <- svm(charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + any_ed_chronic_condition + 
                  any_ed_injury + any_ed_skin_condition + any_ed_abdominal_pain + any_ed_back_pain + 
                  any_ed_heart_or_chest_pain + any_ed_headache + any_ed_depression + charge_food_assistance + 
                  charge_temporary_assistance, train_nNA)
p.sv_aic <- predict(m.sv_aic, newdata = test_nNA)
rmse(test_nNA$charge_total, p.sv_aic) #[1] 17409.03


#Tree based methods----


#
#Single Tree
m.tr <- tree(charge_total ~ ., train_nNA)
p.tr <- predict(m.tr, newdata = test_nNA)
rmse(test_nNA$charge_total, p.tr) #[1] 17029.64



plot(m.tr)
text(m.tr, pretty = 0)

#
#Random forest
m.fo <- randomForest(charge_total ~ ., train_nNA, ntree = 2000)

p.fo <- predict(m.fo, newdata = test_nNA)

rmse(test_nNA$charge_total, p.fo) #[1] 16834.86

#without logical values in conditional branches of survey
m.fo_nl <- randomForest(charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + any_ed_chronic_condition + 
                any_ed_injury + any_ed_skin_condition + any_ed_abdominal_pain + any_ed_back_pain + 
                any_ed_heart_or_chest_pain + any_ed_headache + any_ed_depression + 
                any_ed_psychiatric_condition_or_substance_abuse + charge_food_assistance + 
                charge_temporary_assistance, train_nNA, na.action = "na.fail")
p.fo_nl <- predict(m.fo_nl, newdata = test_nNA)
rmse(test_nNA$charge_total, p.fo_nl) #[1] 16740.19

#without numerical values in conditional branches of survey
m.fo_nn <- randomForest(charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + any_ed_chronic_condition + 
                any_ed_injury + any_ed_skin_condition + any_ed_abdominal_pain + any_ed_back_pain + 
                any_ed_heart_or_chest_pain + any_ed_headache + any_ed_depression + 
                any_ed_psychiatric_condition_or_substance_abuse + food_assistance + 
                temporary_assistance, train_nNA, na.action = "na.fail")
p.fo_nn <- predict(m.fo_nn, newdata = test_nNA)
rmse(test_nNA$charge_total, p.fo_nn) #[1] 16659.06

#features that were used in the model with the highest AIC in lm
m.fo_aic <- randomForest(charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + any_ed_chronic_condition + 
                 any_ed_injury + any_ed_skin_condition + any_ed_abdominal_pain + any_ed_back_pain + 
                 any_ed_heart_or_chest_pain + any_ed_headache + any_ed_depression + charge_food_assistance + 
                 charge_temporary_assistance, train_nNA, na.action = "na.fail")
p.fo_aic <- predict(m.fo_aic, newdata = test_nNA)
rmse(test_nNA$charge_total, p.fo_aic) #[1] 16550.61
#Visualization of error rate


#adding esxplicit interactions, namely those interactions with sex that were found to be helpful in the lm model
m.fo_it <- randomForest(charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + any_ed_chronic_condition + 
                any_ed_injury + any_ed_skin_condition + any_ed_abdominal_pain + any_ed_back_pain + 
                any_ed_heart_or_chest_pain + any_ed_headache + any_ed_depression + charge_food_assistance + 
                charge_temporary_assistance + sex:any_ed_headache + sex:preperiod_any_visits +
                sex:age + sex:any_ed_visits + sex:any_ed_chronic_condition + sex:any_ed_injury +
                sex:any_ed_back_pain, train_nNA)
p.fo_it <- predict(m.fo_it, newdata = test_nNA)
rmse(test_nNA$charge_total, p.fo_it) #[1] 16564.99
#Result: the result was actually worse
#improvements were not really to be expected, since due to their structure random forests should implicitly model
#interactions, by themselves, fairly well



#
#Boosted trees

#took the following tutorial as guidance: http://uc-r.github.io/gbm_regression

#Initial model
featuresBt <- as.matrix(select(ohieVariables_nNA, !c("charge_total")))
set.seed(1111)
m.bt <- xgb.cv(
  data = featuresBt,
  label = ohieVariables_nNA$charge_total,
  nrounds = 1400,
  nfold = 5,    #one is for testing the rest is used for training
  objective = "reg:squarederror",  # for regression models, linear is depreciated
  verbose = 0   # silent,
)

#number of trees that minimize error
m.bt$evaluation_log %>%
  dplyr::summarise(
    ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
    rmse.train   = min(train_rmse_mean),
    ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
    rmse.test   = min(test_rmse_mean),
  )
#  ntrees.train rmse.train ntrees.test rmse.test
#          1400   3088.657           7  16189.93

# plot error vs number trees
ggplot(m.bt$evaluation_log) +
  geom_line(aes(iter, train_rmse_mean), color = "red") +
  geom_line(aes(iter, test_rmse_mean), color = "blue")


#Aci model
featuresBt_aci <- as.matrix(select(ohieVariables_nNA, !c("any_ed_psychiatric_condition_or_substance_abuse", "food_assistance",
                                                      "temporary_assistance", "charge_total")))
set.seed(1111)
m.bt_aci <- xgb.cv(
  data = featuresBt_aci,
  label = ohieVariables_nNA$charge_total,
  nrounds = 1400,
  nfold = 5,    #one is for testing the rest is used for training
  objective = "reg:squarederror",  # for regression models, linear is depreciated
  verbose = 0   # silent,
)
#test_rmse_std: 1st iter: 1242.8520; 1400th iter: 658.1926

#number of trees that minimize error
m.bt_aci$evaluation_log %>%
  dplyr::summarise(
    ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
    rmse.train   = min(train_rmse_mean),
    ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
    rmse.test   = min(test_rmse_mean),
  )
#   ntrees.train rmse.train ntrees.test rmse.test
#           1400   3679.516           6  16121.63
# ln-aci optimized versions performs slightly better
saveRDS(m.bt_aci, file="~/GitHub/BAna-DS/mbt_aci.RDS")

# plot error vs number trees
ggplot(m.bt_aci$evaluation_log, aes(x=iter)) +
  geom_line(aes(y=train_rmse_mean), color = "red", show.legend = TRUE) +
  geom_line(aes(y=test_rmse_mean), color = "blue", show.legend = TRUE) + 
  annotate(geom = "text", x = 1000, y = 4800, label = "In sample error", color = "red") +
  annotate(geom = "text", x = 1000, y = 18000, label = "Out of sample error", color = "blue")

#Result: Whilst the training rmse decreases continously with the number iterations,
 #the test rmse actually increases after reaching its minimum after merely 6 trees
 #hyothesis: the data is very noisy and the model is overfitting
 #see: https://www.cs.cmu.edu/afs/cs/project/jair/pub/volume11/opitz99a-html/node14.html

#Varying tree depth
 #since overfitting seems to be a big problem, smaller trees might increase performance

depthList_m.bt_aci <- vector(mode = "list", length = 9)
for (i in 1:9)
{
  set.seed(1111)
  depthList_m.bt_aci[[i]] <- xgb.cv(
    data = featuresBt_aci,
    label = ohieVariables_nNA$charge_total,
    nrounds = 800,
    nfold = 5,    #one is for testing the rest is used for training
    objective = "reg:squarederror",  # for regression models, linear is depreciated
    verbose = 0,   # silent,
    max_depth = i
  )
}

depthList_evaluation <- data.frame(matrix(data = NA, nrow = 9, ncol = 4))
for (i in 1:9)
{
  depthList_evaluation[i,] <- depthList_m.bt_aci[[i]]$evaluation_log %>%
    dplyr::summarise(
      ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
      rmse.train   = min(train_rmse_mean),
      ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
      rmse.test   = min(test_rmse_mean),
    )
}
depthList_evaluation <- cbind(c(1:9), depthList_evaluation)
names(depthList_evaluation) <- c("treeDepth", "ntrees.train", "rmse.train", "ntrees.test", "rmse.test")
#Results: The best performing model has depth of only 3
 #it performs best at 29 iterations with a test-rmse of 15940.39
saveRDS(depthList_evaluation , file="~/GitHub/BAna-DS/depthList_evaluation.RDS")

#Barplot to vizualize minimum error per depth
ggplot(depthList_evaluation, aes(x=treeDepth, y=rmse.test)) +
  geom_col() +
  labs(title = "Smallest out of sample error per tree depth") +
  scale_x_continuous("Tree Depth", breaks = c(1:9), labels = as.character(c(1:9))) +
  coord_cartesian(ylim = c(1.5*10^4, 1.8*10^4))


#Could theoretically add a visualisation of all error rates <- worth it ??


#
#Testing performance of model without the additional ed_visit specifics
featuresBt_nEDsp <- as.matrix(select(ohieVariables_nNA, !c("any_ed_psychiatric_condition_or_substance_abuse", "food_assistance",
                                                         "temporary_assistance", "charge_total", "any_ed_chronic_condition", "any_ed_injury",                                 
                                                         "any_ed_skin_condition", "any_ed_abdominal_pain", "any_ed_back_pain",
                                                         "any_ed_heart_or_chest_pain", "any_ed_headache", "any_ed_depression")))
Sys.time()
depthList_m.bt_nEDsp <- vector(mode = "list", length = 9)
for (i in 1:9)
{
  set.seed(1111)
  depthList_m.bt_nEDsp[[i]] <- xgb.cv(
    data = featuresBt_aci,
    label = ohieVariables_nNA$charge_total,
    nrounds = 800,
    nfold = 5,    #one is for testing the rest is used for training
    objective = "reg:squarederror",  # for regression models, linear is depreciated
    verbose = 0,   # silent,
    max_depth = i
  )
}
Sys.time()

depthList_evaluation_nEDsp <- data.frame(matrix(data = NA, nrow = 9, ncol = 4))
for (i in 1:9)
{
  depthList_evaluation_nEDsp[i,] <- depthList_m.bt__nEDsp[[i]]$evaluation_log %>%
    dplyr::summarise(
      ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
      rmse.train   = min(train_rmse_mean),
      ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
      rmse.test   = min(test_rmse_mean),
    )
}
depthList_evaluation_nEDsp <- cbind(c(1:9), depthList_evaluation_nEDsp)
names(depthList_evaluation_nEDsp) <- c("Tree Depth", "ntrees.train", "rmse.train", "ntrees.test", "rmse.test")
#Results: almost the same result as before
 #it performs best at 29 iterations with a test-rmse of 15940.39


#Recalculating the currently best performing model to export it
featuresBt_aci <- as.matrix(select(ohieVariables_nNA, !c("any_ed_psychiatric_condition_or_substance_abuse", "food_assistance",
                                                         "temporary_assistance", "charge_total")))
set.seed(1111)
m.bt_3d <- xgb.cv(
  data = featuresBt_aci,
  label = ohieVariables_nNA$charge_total,
  nrounds = 100,
  nfold = 5,    #one is for testing the rest is used for training
  objective = "reg:squarederror",  # for regression models, linear is depreciated
  verbose = 0,   # silent,
  max_depth = 3)
saveRDS(m.bt_3d, file="~/GitHub/BAna-DS/ohieBoostingModel.RDS")

trainBt_aci <- as.matrix(select(train_nNA, !c("any_ed_psychiatric_condition_or_substance_abuse", "food_assistance",
                                                         "temporary_assistance", "charge_total")))
testBt_aci <- as.matrix(select(test_nNA, !c("any_ed_psychiatric_condition_or_substance_abuse", "food_assistance",
                                              "temporary_assistance", "charge_total")))
set.seed(1113)
m.bt_3d.2 <- xgboost(
  data = trainBt_aci,
  label = train_nNA$charge_total,
  nrounds = 29,
  #nfold = 5,    #one is for testing the rest is used for training
  objective = "reg:squarederror",  # for regression models, linear is depreciated
  verbose = 0,   # silent,
  max_depth = 3)
pr.bt_3d.2 <- predict(m.bt_3d.2, newdata = testBt_aci)
rmse(test_nNA$charge_total, pr.bt_3d.2) #[1] 16519.78 #why????
saveRDS(m.bt_3d.2, file="~/GitHub/BAna-DS/ohieBoostingModel.RDS")

#matrix(unlist(), ncol = 14, byrow = TRUE)


#create object of gradient boosting that can be used with predict()
#scale the charge variable into one year beforehand



#Possible further algorithms----
 #support vecor machiene <- mainly used for classification problems, but regression can also be done
  #do not yet know about usefullness in this setting
 #neural nets
 #extensions to linear model:
  #interaction terms

