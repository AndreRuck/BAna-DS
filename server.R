library(hrbrthemes)
library(viridis)
library(ggridges)
library(shiny)
library(ggplot2)
library(data.table)
library(DT)

insurance = fread("insurance.csv")
insurance$smoker=as.factor(insurance$smoker)

shinyServer(function(input, output){
    
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
})
