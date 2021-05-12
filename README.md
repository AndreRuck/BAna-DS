# General:
your files are only shared if you commit AND push to origin afterwards
 
if committing/pushing results in a conflict only try to resolve it yourself if it is small/straightforward to do so and you are confident that you wont be breaking the code,
otherwise simply use another branch


# Info on the current files in the repo: (please add your explanation if you add a file)

OHIE_wrangle = the r script containing all data wrangling on the OHIE dataset

OHIE_wrangled.RDS = the cleaned dataframe. This forms the base of our OHIE analysis.

OHIE_featureSelection = depreciated OHIE data wrangling script

OHIE_analysis.R = forecasting models on OHIE dataset, which is imported from OHIE_wrangled

model_step_rds = predictions multi model regression save (step model)

ui and server: 1. step genrating by visualization done. 2. making predictions based on the inputs is not finished yet.
