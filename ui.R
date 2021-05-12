library(shiny)
library(DT)

insurancecols=read.csv("insurance.csv")
insurancecols=colnames(insurancecols)

shinyUI(fluidPage(
  navbarPage("Preddly",
             tabPanel("Basic Knowledge",
                      tabsetPanel(
                        tabPanel("Easy",
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(
                                       "selectedColsy",
                                       "y-axis",
                                       choices=insurancecols[c(1,7)],
                                       selected=insurancecols[7]
                                     ),
                                     sliderInput("ageInput", "Age", min=0, max=100,
                                                 value=c(0,100)
                                     )),
                                   mainPanel(
                                     plotOutput("plot"),
                                     plotOutput("plot1"),
                                     br(),br()
                                   )
                                 )
                        ),
                        tabPanel("Advanced",
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(
                                       "selectedColsyy",
                                       "y-axis",
                                       choices=insurancecols,
                                       selected=insurancecols[7]
                                     ),
                                     selectInput(
                                       "selectedColsxx",
                                       "x-axis",
                                       choices=insurancecols,
                                       selected=insurancecols[3]
                                     ),
                                     sliderInput("ageInputt", "Age", min=0, max=100,
                                                 value=c(0,100)
                                     ),
                                     sliderInput("bmiInputt", "BMI", min=10, max=50,
                                                 value=c(10,50)
                                     ),
                                     checkboxGroupInput("genderInputt", "Gender",
                                                        choiceNames = list("male", "female"),
                                                        choiceValues = list("male", "female"),
                                                        selected=list("male","female")
                                     )),
                                   mainPanel(
                                     plotOutput("plot2"),
                                     br(),br(),
                                   )
                                 )
                        )
                      )),
             tabPanel("Your Expenses",
                      tabsetPanel(
                        tabPanel("General Questionnaire",
                                 verticalLayout(
                                   sidebarPanel(
                                     radioButtons("genderInput", "Choose your gender", 
                                                  c("Male" = "male",
                                                    "Female" = "female")
                                     ),
                                     numericInput("ageInput", "Enter your age", min=0, max=90,
                                                  value=c(45)
                                     ),
                                     numericInput("weightsInput", "Enter your weights (kg)", min=40, max=150,
                                                  value=c(75)
                                     ),
                                     numericInput("heightsInput", "Enter your heights (cm)", min=130, max=220,
                                                  value=c(170)
                                     ),
                                     numericInput("childrenInput", "How many children do you have", min = 0, max = 10,
                                                  value = c(1)
                                     ),
                                     radioButtons("smokeInput", "Do you smoke?", 
                                                  c("Yes" = "yes",
                                                    "No" = "no")
                                     ),
                                     selectInput("regionInput", "Region", c("northwest", "northeast", "southeast", "southwest")
                                    ),
                                    numericInput("zipInput", "ZIP", min = 00601, max = 99950,
                                                               value = ""
                                    ),
                                    textInput("StreetInput", "Street",
                                                 value = ""
                                    ),
                                    numericInput("NumberInput", "Number", min = 1, max = 1000,
                                                 value = ""
                                    ),
                                    textInput("emailInput", "E-Mail",
                                              value = ""
                                    ),
                                    numericInput("phoneInput", "Phone", min = 10000000, max = 100000000,
                                                 value = ""
                                    ))
                                  
                                 )
                        )
                      )
             )
  )
))
