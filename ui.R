library(shiny)


shinyUI(fluidPage(
    navbarPage("Insurance Prediction",
               tabPanel("Questionnaire",
                        tabsetPanel(
                            tabPanel("General Questionnaire",
                            verticalLayout(
                                sidebarPanel(
                                    numericInput("ageInput", "Enter your age", min=0, max=90,
                                                 value=c(45)
                                    ),
                                    radioButtons("genderInput", "Choose your gender:", 
                                                 c("Male" = "male",
                                                   "Female" = "female",
                                                   "None" = "")
                                    ),
                                    numericInput("bmiInput", "Enter your BMI", min=10, max=50,
                                                 value=c(30.5)
                                    ),
                                    numericInput("childrenInput", "How many children do you have", min = 0, max = 10,
                                                 value = c(1)
                                    ),
                                    radioButtons("smokeInput", "Do you smoke?", 
                                                 c("Yes" = "yes",
                                                   "No" = "no")
                                    ),
                                ),
                                mainPanel(
                                    plotOutput("plot1"),
                                    br(), br(),
                                    tableOutput("results1")
                                )
                        ),

                            )
                        )
                        ),
               tabPanel("Your results",
               verticalLayout(
                   tableOutput("prediction1")
               ),
                   
               ),
               tabPanel("More info on the models",
                        verticalLayout(
                            includeHTML("statistik.html")
                    
                        )))
    
))