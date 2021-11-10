# Load Libraries
library(tidyverse)
require(stargazer)

# Loading data
load('./combined_df.Rda') #Candidate and API response data
load('./image_df.Rda') #Image data - we need resolution from here


#Merging image properties to df_reg
df_reg <- merge(combined_df,image_df,by="image_name")

# Removing Third Gender entries from the dataset for regression
df_reg <- droplevels(df_reg[(df_reg$Gender!='T'),])


# ----- Regression Models -----
## prints output in Latex 
## To print output in any other format - change the type to either html or text
capture.output(
  stargazer::stargazer(list(
  d_1 = lm(detection_success~Gender+Age+ln_res,data=df_reg),
  d_2 = lm(detection_success~Gender+Age+ln_res+tool,data=df_reg),
  g_1 = lm(gender_success~Gender+Age+ln_res,data=df_reg),
  g_2 = lm(gender_success~Gender+Age+ln_res+tool,data=df_reg),
  a_1 = lm(age_error~Gender+Age+ln_res,data=df_reg),
  a_2 = lm(age_error~Gender+Age+ln_res+tool,data=df_reg)
  ),
  type = "latex",title="Regression Results",label = "reg_results",
  no.space = T,model.names = T,align = T,dep.var.labels=c("Face Detection","Gender Classification","Age Prediction Error"),
  covariate.labels=c("Gender(Male)","Age (years)","log(resoultion)"),
  style="aer",omit.stat = c('ser','f')),
 file = "./Output/Tables/Regression_table.tex")


