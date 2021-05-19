# General Github instructions:
your files are only shared if you commit AND push to origin afterwards
  
if committing/pushing results in a conflict only try to resolve it yourself if it is small/straightforward to do so and you are confident that you wont be breaking the code,
otherwise simply use another branch


# Overall structure of the submitted documents
We decided to split the overall solution in parts to structure our solution. Nevertheless, if you solely run the folder with the shiny app, everything will be included. This split rather allows to better understand the models we used and worked with in more depth and more comments.

Part 1 is about the Medical Cost Personal datasets which we use for the basic needs prediction
Part 2 is about the Oregon Health Insurance Experiment which we use for the specific needs prediction and give an in-depth overview of our models
Part 3 is the whole shiny app, which include both datasets and every document needed to run the application and present our overall solution.


# Info on the current files in the repo: (please add your explanation if you add a file)

OHIE_features.xlsx = guide for shiny implementation: an excel document explaining all the variables used in the ohie model

OHIE_wrangle.R = the r script containing all data wrangling on the OHIE dataset

OHIE_wrangled.RDS = the cleaned dataframe. This forms the base of our OHIE analysis. (24 variables)

OHIE_Final_Selection.RDS = cleaned dataframe without unique identifier's and statistically unimpactful variables. (15 variables)

OHIE_analysis.R = forecasting models on OHIE dataset, which is imported from OHIE_wrangled

neural_nets.R = Neural Nets R code.

NN.RData = Neural Nets models saved.

model_step_rds = predictions multi model regression save (step model)

ui and server: 1. step genrating by visualization done. 2. making predictions based on the inputs is not finished yet.

dredlm.rds & dredlm.RData = files containing the ACI scores of different ohie predictions model (calculations take forever so thats why they are saved there)

ui & server_1: main framework for the linear regression analysis done (if enough time, comparing different profil as an added value for the customer and fine tuning:icon, color)

ohie shiny app: ui/server prototyp for analysis. additionally to this folder new csv file without NA's value for the vizualization. 

xb_predictions: UI&server with predictions model with a bug. is in process.

prototype1. please to download the rf model and put it into your folder while you use the shiny app. as the xboost did not work yet, I used the rf. 
--> to do: optimize the tab, make better comparison and then icons and color. of course adding xboost if it works. 

prototype2: different way of tabs. provide simultanously both solution as a process. 

prototype4: ready to provide (more or less). xbooost works finally :)

Part_1_kaggle: Everything regarding kaggle (dataset, modelling, statistics)

Part_2_OHIE: Everything regarding OHIE (datasets, wrangled datasets, code, preliminary datasets for further extrapolations)

Part_3_shiny: All data and models for shiny implementation.
