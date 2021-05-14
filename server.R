library(hrbrthemes)
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

insurance = fread("insurance.csv")
insurance$smoker=as.factor(insurance$smoker)
insurance$region=as.factor(insurance$region)
insurance$sex=as.factor(insurance$sex)

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
            geom_point(aes(color=bmi), 
                       alpha=(input$ageInput[2]/80))
    })
    output$plot4 = renderPlot({
        
        y_axis = input$selectedColsyyy
        x_axis = input$selectedColsxxx
        
        ggplot(filtereddd(), aes_string(x=x_axis)) +
            geom_histogram(fill="skyblue", color = "white")
    })
    
    output$results = renderDataTable({
        filtered()
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
    
    prediction = reactive({
        model = readRDS("model.step.rds")
        insuranceDT = (insurance)
    })
    
    output$prediction1 = renderTable({
        insuranceDT = setDT(insurance)
        insuranceDT[, c("expenses")]
    })

})}

