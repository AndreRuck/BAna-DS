#OHIE_analysis

library(Metrics)
library(tidyverse)
library(corrplot)

library(tree)
library(randomForest)
library(e1071)

library(MuMIn)

#Data----
setwd("~/GitHub/BAna-DS")
ohie <- readRDS("OHIE_Wrangled.RDS")



#Variables----

#dependent variable: charge_total <- includes all charges and food assistance is not paid for by insurance
formula <- "charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + any_ed_chronic_condition + 
any_ed_injury + any_ed_skin_condition + any_ed_abdominal_pain + any_ed_back_pain + 
  any_ed_heart_or_chest_pain + any_ed_headache + any_ed_depression + 
  any_ed_psychiatric_condition_or_substance_abuse + food_assistance + charge_food_assistance + 
  temporary_assistance + charge_temporary_assistance"
ohieVariables <- ohie %>% 
  select(!c("person_id", "treatment", "dt_retro_coverage", "numhh_list", "numhh_list",
                                "ed_charge_total", "zip_msa_list")) #%>%  #zip_msa_list would generally work as predictor but it
#contains only one possible value, which renders it useless for actual predictions
  #select(!c())
  
ohieVariables$sex <- as.numeric(ohieVariables$sex=="Female")
  
ohieVariables_nNA <- drop_na(ohieVariables)

set.seed(1111)
index <- sample(x=c(1:length(ohie$person_id)), size=trunc(length(ohie$person_id)*0.7), replace = FALSE)
train <- ohieVariables[index,]
test <- ohieVariables[-index,]

set.seed(1111)
index_nNA <- sample(x=c(1:length(ohieVariables_nNA$age)), size=trunc(length(ohieVariables_nNA$age)*0.7),
                    replace = FALSE)
train_nNA <- ohieVariables_nNA[index_nNA,]
test_nNA <- ohieVariables_nNA[-index_nNA,]



#Exploration----

cor_ohieVariables_nNA <- cor(ohieVariables_nNA)

corplot_ohieVariables_nNA <- corrplot(corr = cor_ohieVariables_nNA)



#Initial linear regression----
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

#model with highest AICc
m.lm_aic <- lm(charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + any_ed_chronic_condition + 
                 any_ed_injury + any_ed_skin_condition + any_ed_abdominal_pain + any_ed_back_pain + 
                 any_ed_heart_or_chest_pain + any_ed_headache + any_ed_depression + charge_food_assistance + 
                 charge_temporary_assistance, train_nNA, na.action = "na.fail")
p.lm_aic <- predict.lm(m.lm_aic, newdata = test_nNA)
rmse(test_nNA$charge_total, p.lm_aic) #[1] 16700.77

#Initial tree based methods----

#Single Tree
m.tr <- tree(charge_total ~ ., train_nNA)

p.tr <- predict(m.tr, newdata = test_nNA)

rmse(test_nNA$charge_total, p.tr) #[1] 17029.64

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
rmse(test_nNA$charge_total, p.fo_nn) #[1] 16659.06 ;  sqrt=129.07



#Support Vector Regression-----

m.sv <- svm(charge_total ~ ., train_nNA)
p.sv <- predict(m.sv, newdata = test_nNA)
rmse(test_nNA$charge_total, p.sv) #[1] 20437.4

m.sv_nn <- svm(charge_total ~ preperiod_any_visits + age + sex + any_ed_visits + any_ed_chronic_condition + 
                 any_ed_injury + any_ed_skin_condition + any_ed_abdominal_pain + any_ed_back_pain + 
                 any_ed_heart_or_chest_pain + any_ed_headache + any_ed_depression + 
                 any_ed_psychiatric_condition_or_substance_abuse + food_assistance + 
                 temporary_assistance, train_nNA)
p.sv_nn <- predict(m.sv_nn, newdata = test_nNA)
rmse(test_nNA$charge_total, p.sv_nn) #[1] 17439.04


#Possible further algorithms----
 #support vecor machiene <- mainly used for classification problems, but regression can also be done
  #do not yet know about usefullness in this setting
 #neural nets
 #extensions to linear model:
  #interaction terms

