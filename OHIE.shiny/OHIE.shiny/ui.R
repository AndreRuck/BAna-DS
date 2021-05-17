library(shiny)
library(DT)

options(scipen=999)
ohie=fread("ohie.plot.csv")
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
    navbarPage("Ohie",
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
                                            sliderInput("expensesINPUT", "Expenses", min=0, max=40000,
                                                                     value=c(0,40000)
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
)))))