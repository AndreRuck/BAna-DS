#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(ggplot2)

insurance = fread("insurance.csv")
insurance$sex <- factor(insurance$sex)
insurance$smoker <- factor(insurance$smoker)
insurance$children <- as.factor(insurance$children)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$plot1 = renderPlot({
    # x_axis1 = input$ageInput
    # ggplot(insurance, aes(x=x_axis1, fill=smoker)) +
    #   geom_density(alpha=0.4) + theme(legend.position="bottom") + labs(title="expenses / smoker")
    bmi1 = input$bmiInput
    children1 = input$childrenInput
    smoker1 = factor(input$smokeInput)
    # 
    # ggplot(insurance, aes(x = bmi1, y = children1, fill = children1)) + 
    #   geom_density_ridges() + theme_ridges() + ylab("children")+ xlab("bmi") + scale_y_discrete(limits=c("0","1", "2", "3", "4", "5"))
    
    ggplot(insurance, aes(x=bmi1, y=expenses)) + geom_point(aes(color=factor(smoker1)))
    

  })
  prediction = reactive({
    model = readRDS("model.RDS")
    insuranceDT = (insurance)
  })
  
  output$prediction1 = renderTable({
    insuranceDT = setDT(insurance)
    insuranceDT[, c("expenses")]
  })
    
})
