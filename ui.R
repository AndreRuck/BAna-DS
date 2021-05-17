library(shiny)
library(DT)
library(bslib)

insurancecols=read.csv("insurance.csv")
insurancecols=colnames(insurancecols)

shinyUI(fluidPage(
    navbarPage("Preddly",
               theme = bs_theme(version = 4,
                                bg = "#FFFFFF",
                                fg = "#0A5192",
                                primary = "#85878C",
                                base_font = font_google("Oswald"),
                                bootswatch = "minty"
                                ),
               tabPanel("Explore Expenses",
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
                                             sliderInput("ageInput", "Age", min=18, max=70,
                                                         value=c(18,70)
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
                                             sliderInput("ageInputt", "Age", min=18, max=70,
                                                         value=c(18,70)
                                             ),
                                             sliderInput("bmiInputt", "BMI", min=18, max=50,
                                                         value=c(18,50)
                                             ),
                                             checkboxGroupInput("genderInputt", "Gender",
                                                                choiceNames = list("male", "female"),
                                                                choiceValues = list("male", "female"),
                                                                selected=list("male","female")
                                             )),
                                         mainPanel(
                                             plotOutput("plot2"),
                                             br(),br()
                                         )
                                     )
                            ),
                            tabPanel("Professional",
                                     br(),
                                     sidebarLayout(
                                         sidebarPanel(
                                             selectInput(
                                                 "selectedColsyyy",
                                                 "y-axis",
                                                 choices=insurancecols,
                                                 selected=insurancecols[3]
                                             ),
                                             selectInput(
                                                 "selectedColsxxx",
                                                 "x-axis",
                                                 choices=insurancecols,
                                                 selected=insurancecols[7]
                                             ),
                                             sliderInput("ageInputtt", "Age", min=18, max=70,
                                                         value=c(18,70)
                                             ),
                                             sliderInput("bmiInputtt", "BMI", min=10, max=50,
                                                         value=c(10,50)
                                             ),
                                             sliderInput("childrenInputtt", "Children", min=0, max=8,
                                                         value=c(0,8)
                                             ),
                                             sliderInput("expensesInputtt", "Expenses", min=1000, max=70000,
                                                         value=c(1000,70000)
                                             ),
                                             checkboxGroupInput("genderInputtt", "Gender",
                                                                choiceNames = list("male", "female"),
                                                                choiceValues = list("male", "female"),
                                                                selected=list("male","female")
                                             )),
                                         mainPanel(
                                             plotOutput("plot3"),
                                             plotOutput("plot4"),
                                             br(),br()
                                         )
                                     )
                            ),
                            tabPanel("Search ",
                                     verticalLayout(
                                         mainPanel(
                                             dataTableOutput("results")
                                         )
                                     )
                            ))),
               tabPanel("Predict YOUR Expenses ",
                        tabsetPanel(
                            tabPanel("Insurance Information",
                                     br(),
                                     sidebarLayout(
                                         sidebarPanel(
                                             radioButtons("gender_Input", "Choose your gender", 
                                                          c("Male" = "male",
                                                            "Female" = "female")
                                             ),
                                             numericInput("age_Input", "Enter your age", min=0, max=90,
                                                          value=c(0)
                                             ),
                                             numericInput("weights_Input", "Enter your weights (kg)", min=40, max=150,
                                                          value=c(0)
                                             ),
                                             numericInput("heights_Input", "Enter your heights (cm)", min=130, max=220,
                                                          value=c(0)
                                             ),
                                             numericInput("children_Input", "How many children do you have", min = 0, max = 10,
                                                          value = c(1)
                                             ),
                                             radioButtons("smoker_Input", "Do you smoke?", 
                                                          c("Yes" = "yes",
                                                            "No" = "no")
                                             ),
                                             selectInput("region_Input", "Region", c("northwest", "northeast", "southeast", "southwest")
                                             )),
                                         mainPanel(
                                             h1("Prediction"),
                                             h3(textOutput("fit1")),
                                             tableOutput("result"))
                                         
                                     )
                            )
                        )
               ),
               tabPanel("Optimize YOUR Expenses ",
                        tabsetPanel(
                            tabPanel("Register",
                                     verticalLayout(
                                         sidebarPanel(
                                             radioButtons("Saluations_Input", "Dear", 
                                                          c("Mr" = "yes",
                                                            "Ms" = "no")
                                             ),
                                             textInput("Firstname_Input", "First name",
                                                       value = ""
                                             ),
                                             textInput("Surname_Input", "Surname",
                                                       value = ""
                                             ),
                                             numericInput("zip_Input", "ZIP", min = 00601, max = 99950,
                                                          value = ""
                                             ),
                                             textInput("Street_Input", "Street",
                                                       value = ""
                                             ),
                                             numericInput("Number_Input", "Number", min = 1, max = 1000,
                                                          value = ""
                                             ),
                                             textInput("email_Input", "E-Mail",
                                                       value = ""
                                             ),
                                             numericInput("phone_Input", "Phone", min = 10000000, max = 100000000,
                                                          value = ""),
                                             passwordInput("password", "Password:"),
                                             actionButton("go", "Login"),
                                             verbatimTextOutput("value")
                                         ))
                            ),
                            tabPanel("Compare Expenses",
                                     splitLayout(
                                         sidebarPanel(
                                             titlePanel("Profil 1"),
                                             radioButtons("gender_Input", "Choose your sex", 
                                                          c("male" = "male",
                                                            "female" = "female")
                                             ),
                                             numericInput("age.Input", "Enter your age", min=0, max=90,
                                                          value=c(0)
                                             ),
                                             numericInput("weights.Input", "Enter your weights (kg)", min=40, max=150,
                                                          value=c(0)
                                             ),
                                             numericInput("heights.Input", "Enter your heights (cm)", min=130, max=220,
                                                          value=c(0)
                                             ),
                                             numericInput("children.Input", "Children", min = 0, max = 10,
                                                          value = c(0)
                                             ),
                                             radioButtons("smoker.Input", "Do you smoke?", 
                                                          c("Yes" = "yes",
                                                            "No" = "no")
                                             )),
                                         sidebarPanel(
                                             titlePanel("Profil 2"),
                                             radioButtons("gender__Input", "Choose your sex", 
                                                          c("male" = "male",
                                                            "female" = "female")
                                             ),
                                             numericInput("age__Input", "Enter your age", min=0, max=90,
                                                          value=c(0)
                                             ),
                                             numericInput("weights__Input", "Enter your weights (kg)", min=40, max=150,
                                                          value=c(0)
                                             ),
                                             numericInput("heights__Input", "Enter your heights (cm)", min=130, max=220,
                                                          value=c(0)
                                             ),
                                             numericInput("children__Input", "Children", min = 0, max = 10,
                                                          value = c(0)
                                             ),
                                             radioButtons("smoker__Input", "Do you smoke?", 
                                                          c("Yes" = "yes",
                                                            "No" = "no"))
                                         ),
                                         mainPanel(
                                           wellPanel("Recommendation for healthy habits from our Partners"),
                                           br(),
                                           tags$a(href="https://blog.fitbit.com/", img(src='fitbit-logo2.png', height = "50%", width = "50%", align = "center")),
                                           br(),
                                           tags$a(href="https://www.eatingwell.com/recipes/", img(src = 'eatingwell-logo.jpg', height = "50%", width = "50%", align = "center")
                                           ),
                                           br(),
                                           tags$a(href="https://smokefreeapp.com/",img(src='smoke-logo.png', height = "50%", width = "50%", align = "center")
                                           ),
                                           br(),
                                           tags$a(href="https://kidshealth.org/", img(src = 'kidshealth-logo.jpg' , height = "50%", width = "50%", align = "center")
                                           ),
                                           br(),
                                           tags$a(href="https://www.gaiam.com/", img(src = 'gaiam-logo.jpg' , height = "50%", width = "50%", align = "center")
                                           ),
                                           br(),
                                           tags$a(href="https://www.headspace.com/mindfulness/mindful-eating", img(src = 'headspace-logo.jpg' , height = "50%", width = "50%", align = "center")
                                           ),
                                           
                                           
                                         )
                                         
                                     )))))))
