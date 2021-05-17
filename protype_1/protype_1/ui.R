library(shiny)
library(DT)
library(bslib)

insurancecols=read.csv("insurance.csv")
insurancecols=colnames(insurancecols)

options(scipen=999)
ohie= fread("ohie.plot.csv")
ohie <- ohie[,-1]

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

ohiecols = ohie
ohiecols=colnames(ohiecols)


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
                                         )
                                     ),
                                     sidebarPanel(
                                       selectInput(
                                         "selectedColsy",
                                         "y-axis",
                                         choices=insurancecols[c(1,7)],
                                         selected=insurancecols[7]
                                       ),
                                       sliderInput("ageInput", "Age", min=18, max=70,
                                                   value=c(18,70)
                                       ))
                                     
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
                                                         value=c(0,70000)
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
                                             h3(textOutput("fit")),
                                             tableOutput("result"))
                                         
                                     )
                            )
                        )
               ),
               tabPanel("Optimize YOUR Expenses ",
                        tabsetPanel(
                            tabPanel("Register",
                                     br(),
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
                                     br(),
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
                                           br(),
                                           tags$a(href= "https://www.24hourfitness.com/" , img(src = 'zok.png' , height = "50%", width = "50%", align = "center")
                                           )      
                                           
                                         
                                     ))),
                            
                            tabPanel("Visualization",
                                     tabsetPanel(
                                       tabPanel("Visualation",
                                                br(),
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    selectInput(
                                                      "selectedCooly",
                                                      "y-axis",
                                                      choices=ohiecols,
                                                      selected= ohiecols[13]
                                                    ),
                                                    selectInput(
                                                      "selectedCoolx",
                                                      "x-axis",
                                                      choices=ohiecols,
                                                      selected=ohiecols[2]
                                                    ),
                                                    sliderInput("ageINPUT", "Age", min=18, max=100,
                                                                value=c(18,100)
                                                    ),
                                                    sliderInput("expensesINPUT", "Expenses", min=0, max=400000,
                                                                value=c(0,400000)
                                                    ),
                                                    sliderInput("cfoodINPUT", "Value in SNAP", min=0, max=80000,
                                                                value=c(0,80000)
                                                    ),
                                                    sliderInput("ctemporaryINPUT", "Value in TANF ", min=0, max=70000,
                                                                value=c(0,70000)
                                                    ),
                                                    checkboxGroupInput("sexINPUT", "Sex",
                                                                       choiceNames = list("Female", "Male"),
                                                                       choiceValues = list("Female", "Male"),
                                                                       selected=list("Female","Male")
                                                    ),
                                                    checkboxGroupInput("preperiodINPUT", "Any doctors visits",
                                                                       choiceNames = list("No", "Yes"),
                                                                       choiceValues = list(0,1),
                                                                       selected=list(0,1)
                                                    ),
                                                    checkboxGroupInput("visitsINPUT", "Visits",
                                                                       choiceNames = list("No", "Yes"),
                                                                       choiceValues = list("FALSE", "TRUE"),
                                                                       selected=list("FALSE","TRUE")
                                                    ),
                                                    checkboxGroupInput("chronicINPUT", "Visits for a chronic decision",
                                                                       choiceNames = list("No", "Yes"),
                                                                       choiceValues = list("FALSE", "TRUE"),
                                                                       selected=list("FALSE","TRUE")
                                                    ),
                                                    checkboxGroupInput("injuryINPUT", "Visits for a injury condition",
                                                                       choiceNames = list("No", "Yes"),
                                                                       choiceValues = list("FALSE", "TRUE"),
                                                                       selected=list("FALSE","TRUE")
                                                    ),
                                                    checkboxGroupInput("skinINPUT", "Visits for skin condition",
                                                                       choiceNames = list("No", "Yes"),
                                                                       choiceValues = list("FALSE", "TRUE"),
                                                                       selected=list("FALSE","TRUE")
                                                    ),
                                                    checkboxGroupInput("abdominalINPUT", "Visits for abdominal pain",
                                                                       choiceNames = list("No", "Yes"),
                                                                       choiceValues = list("FALSE", "TRUE"),
                                                                       selected=list("FALSE","TRUE")
                                                    ),
                                                    checkboxGroupInput("backINPUT", "Visits for back pain",
                                                                       choiceNames = list("No", "Yes"),
                                                                       choiceValues = list("FALSE", "TRUE"),
                                                                       selected=list("FALSE","TRUE")
                                                    ),
                                                    checkboxGroupInput("heartINPUT", "Visits for hear or chest pain",
                                                                       choiceNames = list("No", "Yes"),
                                                                       choiceValues = list("FALSE", "TRUE"),
                                                                       selected=list("FALSE","TRUE")
                                                    ),
                                                    checkboxGroupInput("headacheINPUT", "Visits for hear a headache",
                                                                       choiceNames = list("No", "Yes"),
                                                                       choiceValues = list("FALSE", "TRUE"),
                                                                       selected=list("FALSE","TRUE")
                                                    ),
                                                    checkboxGroupInput("depressionINPUT", "Visits for dpression",
                                                                       choiceNames = list("No", "Yes"),
                                                                       choiceValues = list("FALSE", "TRUE"),
                                                                       selected=list("FALSE","TRUE")
                                                    )),
                                                  mainPanel(
                                                    plotOutput("pl"),
                                                    br(),br(),
                                                    plotOutput("pl1"),
                                                    br(),br(),
                                                    plotOutput("pl2"),
                                                    br(),br()
                                                  )
                                                )
                                       )
                                     )),
                            tabPanel("Predictions",
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("ageINPUTt", "Enter your age", min=0, max=90,
                                                      value=c(0)
                                         ),
                                         numericInput("cfoodINPUTt", "Value in SNAP", min=0, max=90000,
                                                      value=c(0)
                                         ),
                                         numericInput("ctemporaryINPUTt", "Value in TANF", min=0, max=90000,
                                                      value=c(0)
                                         ),
                                         selectInput("sexINPUTt", "Sex",
                                                     choices = c("Female" = 0, "Male" = 1), selected=0
                                                     
                                         ),
                                         radioButtons("preperiodINPUTt", "Any doctors visits",
                                                      c("Yes" = 1,
                                                        "No" = 0), selected = 0
                                         ),
                                         radioButtons("visitsINPUTt", "Visits",
                                                      c("Yes" = TRUE,
                                                        "No" = FALSE), selected = FALSE
                                         ),
                                         radioButtons("chronicINPUTt", "Visits for a chronic decision",
                                                      c("Yes" = TRUE,
                                                        "No" = FALSE), selected = FALSE
                                         ),
                                         radioButtons("injuryINPUTt", "Visits for a injury condition",
                                                      c("Yes" = TRUE,
                                                        "No" = FALSE), selected = FALSE
                                         ),
                                         radioButtons("skinINPUTt", "Visits for skin condition",
                                                      c("Yes" = TRUE,
                                                        "No" = FALSE), selected = FALSE
                                         ),
                                         radioButtons("abdominalINPUTt", "Visits for abdominal pain",
                                                      c("Yes" = TRUE,
                                                        "No" = FALSE), selected = FALSE
                                         ),
                                         radioButtons("backINPUTt", "Visits for back pain",
                                                      c("Yes" = TRUE,
                                                        "No" = FALSE), selected = FALSE
                                         ),
                                         radioButtons("heartINPUTt", "Visits for hear or chest pain",
                                                      c("Yes" = TRUE,
                                                        "No" = FALSE), selected = FALSE
                                         ),
                                         radioButtons("headacheINPUTt", "Visits for hear a headache",
                                                      c("Yes" = TRUE,
                                                        "No" = FALSE), selected = FALSE
                                         ),
                                         radioButtons("depressionINPUTt", "Visits for depression",
                                                      c("Yes" = TRUE,
                                                        "No" = FALSE), selected = FALSE
                                         ),
                                         actionButton("submit","Submit")),
                                       mainPanel(
                                         h1("Your Predictions"),
                                         h3(textOutput("fit2"))
                            ))))))))