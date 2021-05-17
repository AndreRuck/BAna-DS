library(viridis)
library(ggridges)
library(shiny)
library(ggplot2)
library(data.table)
library(DT)
library(shiny)
library(ggplot2)
library(data.table)
library(DT)
library(caret)
library(xgboost)
library(tree)
library(randomForest)

insurance = fread("insurance.csv")
insurance$smoker=as.factor(insurance$smoker)
insurance$region=as.factor(insurance$region)
insurance$sex=as.factor(insurance$sex)

ohie= fread("ohie.plot.csv")

ohie$age <- as.numeric(ohie$age)
ohie$any_ed_visits <- as.factor(ohie$any_ed_visits)
ohie$any_ed_chronic_condition <- as.factor(ohie$any_ed_chronic_condition)
ohie$any_ed_injury <- as.factor(ohie$any_ed_injury)
ohie$any_ed_skin_condition <- as.factor(ohie$any_ed_skin_condition)
ohie$any_ed_abdominal_pain <- as.factor(ohie$any_ed_abdominal_pain )
ohie$any_ed_back_pain <- as.factor(ohie$any_ed_back_pain)
ohie$any_ed_heart_or_chest_pain <- as.factor(ohie$any_ed_heart_or_chest_pain)
ohie$any_ed_headache <- as.factor(ohie$any_ed_headache)
ohie$any_ed_depression <- as.factor(ohie$any_ed_depression )
ohie$charge_total <- as.numeric(ohie$charge_total) 
ohie$charge_food_assistance <-  as.numeric(ohie$charge_food_assistance)
ohie$charge_temporary_assistance <- as.numeric(ohie$charge_temporary_assistance)
ohie$preperiod_any_visits <- as.factor(ohie$preperiod_any_visits )
ohie$sex <- as.factor(ohie$sex)

if (interactive()) {
    
    shinyServer(function(input, output, session){
        
        filtered = reactive({
            filtered=insurance[age >= input$ageInput[1] & age <= input$ageInput[2]]
            filtered
        })
        
        output$plot = renderPlot({
            
            y_axis = input$selectedColsy
            
            ggplot(filtered(), aes_string(x="bmi", y=y_axis))+
                geom_point(aes(color=age), 
                           alpha=(input$ageInput[2]/80)) + scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"))
        })
        
        
        filteredd = reactive({
            filteredd=insurance[age >= input$ageInputt[1] & age <= input$ageInputt[2] & bmi >= input$bmiInputt[1] & bmi <= input$bmiInputt[2] &
                                    sex %in% input$genderInputt,,]
            filteredd
        })
        
        output$plot2 = renderPlot({
            
            y_axis = input$selectedColsyy
            x_axis = input$selectedColsxx
            
            ggplot(filteredd(), aes_string(x=x_axis, y=y_axis)) +
                geom_point(aes(color=smoker), 
                           alpha=(input$ageInput[2]/80))
        })
        
        filtereddd = reactive({
            filtereddd=insurance[age >= input$ageInputtt[1] & age <= input$ageInputtt[2] & bmi >= input$bmiInputtt[1] &
                                     bmi <= input$bmiInputtt[2] & children >= input$childrenInputtt[1] & children <= input$childrenInputtt[2] &
                                     expenses >= input$expensesInputtt[1] & expenses <= input$expensesInputtt[2] &
                                     sex %in% input$genderInputtt,,]
            filtereddd
        })
        
        output$plot3 = renderPlot({
            
            y_axis = input$selectedColsyyy
            x_axis = input$selectedColsxxx
            
            ggplot(filtereddd(), aes_string(x=x_axis, y=y_axis)) +
                geom_point(aes(color=age), 
                           alpha=(input$ageInputtt[2]/80)) + scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"))
        
        })
        output$plot4 = renderPlot({
            
            y_axis = input$selectedColsyyy
            x_axis = input$selectedColsxxx
            
            ggplot(filtereddd(), aes_string(x=x_axis)) +
                geom_histogram(fill="skyblue", color = "white")
        })
        
        output$results = renderDataTable({
            filtereddd()
        }, options=list(searching=TRUE, paging=TRUE), rownames=TRUE, filter="top")
        
        
        filter = reactive({
            filter=data.frame("age"=input$age_Input, "sex"= input$gender_Input,
                              "bmi" = (input$weights_Input / ((input$heights_Input/100)^2)),
                              "children" = input$children_Input,"smoker" = input$smoker_Input, "region" = input$region_Input)
            filter
        })
        
        output$result = renderTable({
            filter()
        })
        
        fit <- step(lm(expenses ~ age + sex + bmi + children + smoker + region, data=insurance))
        
        fitpred <- reactive({
            age_Input <- input$age_Input
            gender_Input <- as.factor(input$gender_Input)
            bmi_Input <- ((input$weights_Input) / ((input$heights_Input/100)^2))
            children_Input <- input$children_Input
            smoker_Input <- input$smoker_Input
            region_Input <- input$region_Input
            
            
            predict(fit, newdata= 
                        data.frame(age = age_Input,
                                   sex = gender_Input,
                                   bmi = bmi_Input,
                                   children = children_Input,
                                   smoker = smoker_Input,
                                   region = region_Input
                        ))
        })
        
        output$fit <- renderText({
            fitpred()
        })
        output$value <- renderText({
            req(input$go)
            isolate(input$password)
        })
        
        filter1 = reactive({
            filter1 = ohie[age >= input$ageINPUT[1] & age <= input$ageINPUT[2] &
                               charge_total >= input$expensesINPUT[1] & charge_total <= input$expensesINPUT[2] &
                               charge_food_assistance >= input$cfoodINPUT[1] & charge_food_assistance <= input$cfoodINPUT[2] &
                               charge_temporary_assistance >= input$ctemporaryINPUT[1] & age <= input$ctemporaryINPUT[2] &
                               sex %in% input$sexINPUT & preperiod_any_visits  %in% input$preperiodINPUT &
                               any_ed_visits %in% input$visitsINPUT &
                               any_ed_chronic_condition %in% input$chronicINPUT &
                               any_ed_injury %in% input$injuryINPUT &
                               any_ed_skin_condition %in% input$skinINPUT &
                               any_ed_abdominal_pain %in% input$abdominalINPUT &
                               any_ed_back_pain %in% input$backINPUT &
                               any_ed_heart_or_chest_pain %in% input$heartINPUT &
                               any_ed_headache %in% input$headacheINPUT &
                               any_ed_depression %in% input$depressionINPUT,,]
            filter1
        })
        
        output$pl = renderPlot({
            
            y__axis = input$selectedCooly
            x__axis = input$selectedCoolx
            
            ggplot(filter1(), aes_string(x=x__axis, y=y__axis)) +
                geom_jitter(aes(color=sex), 
                            alpha=(input$ageINPUT[2]/80)) + theme_bw()
        })
        
        output$pl1 = renderPlot({
            
            y.axis = input$selectedCooly
            x__axis = input$selectedCoolx
            
            ggplot(filter1(), aes_string(x=x__axis, y=y.axis))+
                geom_bar(stat="identity", fill="blue3", 
                         alpha=(input$ageINPUT[2]/80))  + theme_bw()
        })
        
        
        output$pl2 = renderPlot({
            
            y___axis = input$selectedCooly
            x___axis = input$selectedCoolx
            
            ggplot(filter1(), aes_string(x=x___axis)) +
                geom_histogram(fill="skyblue", color = "white", bins =  20) + theme_bw()
        })
        
        output$pl3 = renderPlot({
            
            y___axis = input$selectedCooly
            x___axis = input$selectedCoolx
            
            filter1() %>%
                ggplot(filter1(), aes_string(x=x___axis, y=y___axis, fill = x___axis )) +
                geom_boxplot() +
                scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
                theme_ipsum() +
                theme(
                    legend.position="none",
                    plot.title = element_text(size=11)
                ) +
                ggtitle("Basic boxplot") +
                xlab("")
        })
        
        fit1 = readRDS("ohieBoostingModel.RDS")
        
        fit1pred <- reactive({
            Data <- data.frame(age  = input$ageINPUTt,
                               charge_food_assistance = input$cfoodINPUTt,
                               charge_temporary_assistance = input$ctemporaryINPUTt,
                               sex =  input$sexINPUTt,
                               preperiod_any_visits =  input$preperiodINPUTt,
                               any_ed_visits =  input$visitsINPUTt,
                               any_ed_chronic_condition =  input$chronicINPUTt,
                               any_ed_injury =  input$injuryINPUTt,
                               any_ed_skin_condition =  input$skinINPUTt,
                               any_ed_abdominal_pain =  input$abdominalINPUTt,
                               any_ed_back_pain =  input$backINPUTt,
                               any_ed_heart_or_chest_pain =  input$heartINPUTt,
                               any_ed_headache =  input$headacheINPUTt,
                               any_ed_depression =  input$depressionINPUTt)
            
            df_matrix <- data.matrix(Data)
            mdata <- xgb.DMatrix(df_matrix)
            predict(fit, newdata = mdata)
        })
        
        
        output$fit1 <- renderText({
            fit1pred()
        })
        
        fit2 <- readRDS("rf2.RDS")
        
        fit2pred <- eventReactive(input$submit,{
            
            predict(fit2, newdata= 
                        data.frame(age  = input$ageINPUTt,
                                   charge_food_assistance = input$cfoodINPUTt,
                                   charge_temporary_assistance = input$ctemporaryINPUTt,
                                   sex =  as.numeric(input$sexINPUTt),
                                   preperiod_any_visits =  as.numeric(input$preperiodINPUTt),
                                   any_ed_visits =  as.logical(input$visitsINPUTt),
                                   any_ed_chronic_condition =  as.logical(input$chronicINPUTt),
                                   any_ed_injury =  as.logical(input$injuryINPUTt),
                                   any_ed_skin_condition =  as.logical(input$skinINPUTt),
                                   any_ed_abdominal_pain =  as.logical(input$abdominalINPUTt),
                                   any_ed_back_pain =  as.logical(input$backINPUTt),
                                   any_ed_heart_or_chest_pain =  as.logical(input$heartINPUTt),
                                   any_ed_headache =  as.logical(input$headacheINPUTt),
                                   any_ed_depression =  as.logical(input$depressionINPUTt))
            )
        })
        
        output$fit2 <- renderText({
            fit2pred()
            
        })
        
})}
