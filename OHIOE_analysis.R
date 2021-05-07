#OHIE_analysis

setwd("~/GitHub/BAna-DS")

ohie <- readRDS("OHIE_Wrangled.RDS")
set.seed(1111)
index <- sample(x=c(1:length(ohie$person_id)), size=trunc(length(ohie$person_id)*0.7), replace = FALSE)
train <- ohie[index,]
test <- ohie[-index,]


#Initial linear regression----
m.lm <- lm()


#Initial tree based methods----


#Initial support vecor machiene----