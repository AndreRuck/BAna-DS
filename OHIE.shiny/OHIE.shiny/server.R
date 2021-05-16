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

ohie=fread("ohie.plot.csv")

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

shinyServer(function(input, output, session){
    
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
})