#Checking for dependencies and installing them 

packages <- c('tidyverse','stargazer','xtable','tikzDevice')

lapply(packages, function(x){
  if(!require(x,character.only = T)){
    install.packages(x,dependencies = T)
  }
})

# Checking for availability of R source and datafiles needed 
# Note: this is only needed if Rscript called directly (make will check this already)
file_list <- c("2_Results_Analysis.R","3_Regression.R","4_additional_plots.R",
               "./API_response/amazon_test_results_v20210222.csv",
               "./API_response/Facepp_results.Rda",
               "./API_response/facex_output.Rda",
               "./API_response/microsoft_result.Rda") 
file_reponse <- sapply(file_list,file.exists)
if(any(!file_reponse)){
  print("Files missing")
  stop(paste("Following files missing",file_list[!files_reponse]))
}
  

# Running the R scripts
source('2_Results_Analysis.R')
source('3_Regression.R')
source('4_additional_plots.R')