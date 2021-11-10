#---- Loading Libraries ----
library(tidyverse)
require(xtable)
#---- Reading Files ----

#Loading Candidate Data (without any PII)
load("./Cleaned_data_no_PII.Rda")
# This has only Gender, Age, State and image_name (that acts as unique identifier)

#Face++
load('./API_response/Facepp_results.Rda')
facepp_test <- output
rm(output)

#Amazon
amazon_test <- read.csv('./API_response/amazon_test_results_v20210222.csv')
amazon_test <- amazon_test[,-1]

#FaceX
load('./API_response/facex_output.Rda')
facex_test <- output
rm(output)

#Microsoft
load('./API_response/microsoft_result.Rda')
microsoft_test <- output
rm(output)

#Removing 15 images that showed error for any one of the tools from all tools
to_be_removed <- c(1184,2888,10856,21142,21148,30730,31364,31486,31528,31599,
                   10856,14268,15031,15364,16339,24137)
amazon_test <- amazon_test[-to_be_removed,]
facex_test <- facex_test[-to_be_removed,]
microsoft_test <- microsoft_test[-to_be_removed,]
facepp_test <- facepp_test[-to_be_removed,]

#---- Cleaning Tool resposes -----

## ----- Cleaning Amazon ----
# - As Amazon was analysed using python, the data is saved in CSV file
#   and the cleaning is slightly different than other tools

#Printing percentage of faces correctly detected
print(sum(amazon_test$number_faces==1)/nrow(amazon_test))

# Setting values to NA for cases where number of faces is not 1 (failed detection)
amazon_test[which((amazon_test$number_faces!=1)),c(4:7)] <- NA

#Correct Image address to align with the candidate data
amazon_test$Image <- sapply(amazon_test$Image,function(x) 
  gsub("\\\\", "/", x))
amazon_test$image_name <- paste0("./IMG/",substring(amazon_test$Image,first = 35))

#Cleaning Gender
amazon_test$Gender_Pred <- factor(amazon_test$Gender)
levels(amazon_test$Gender_Pred) <- c('F','M')

#Cleaning Age
amazon_test$low_age <- as.numeric(sapply(amazon_test$low_age,function(x) 
  gsub(x = x,pattern = ".*?([0-9]+).*$",replacement = "\\1")))
amazon_test$high_age <- as.numeric(sapply(amazon_test$high_age,function(x) 
  gsub(x = x,pattern = ".*?([0-9]+).*$",replacement = "\\1")))
amazon_test$mean_age <- 
  (amazon_test$low_age + amazon_test$high_age)/2
names(amazon_test)[1] <- "image_list"

#Adding tool name
amazon_test$tool <- "Amazon"

## ----- Cleaning FaceX -----

# Printing percentage of faces correctly detected
print(sum(facex_test$number_faces==1)/nrow(facex_test))

# Correct Image address to align with the candidate data
facex_test$image_name <- paste0("./",substring(facex_test$image_list,first = 31))

# Cleaning Gender
facex_test$Gender_Pred <- factor(facex_test$Gender)
levels(facex_test$Gender_Pred) <- c('F','M')

# Cleaning Age
facex_test$low_age <- as.numeric(sapply(facex_test$age, function(x)
  str_split_fixed(x,pattern = '-',n = 2)[1]))
facex_test$high_age <- as.numeric(sapply(facex_test$age, function(x)
  str_split_fixed(x,pattern = '-',n = 2)[2]))
facex_test$mean_age <- 
  (facex_test$low_age + facex_test$high_age)/2

# Adding tool name
facex_test$tool <- "FaceX"

## ------ Cleaning Microsoft -----

# Printing percentage of faces correctly detected
print(sum(microsoft_test$number_faces==1)/nrow(microsoft_test))

# Correct Image address to align with the candidate data
microsoft_test$image_name <- paste0("./",substring(microsoft_test$image_list,first = 31))

# Cleaning Gender
microsoft_test$Gender_Pred <- factor(microsoft_test$Gender)
levels(microsoft_test$Gender_Pred) <- c('F','M')

# Cleaning Age
microsoft_test$mean_age <- as.numeric(microsoft_test$age)

# Adding tool name
microsoft_test$tool <- "Microsoft"

## ------ Cleaning Face++ -------

# Correct Image address to align with the candidate data
print(sum(facepp_test$number_faces==1)/nrow(facepp_test))

# Correct Image address to align with the candidate data
facepp_test$image_name <- paste0("./",substring(facepp_test$image_list,first = 31))

# Correcting Gender
facepp_test$Gender_Pred <- factor(facepp_test$Gender)
levels(facepp_test$Gender_Pred) <- c('F','M')

# Cleaning Age - adding low and high to avoid NAs - not used later in analysis
facepp_test$mean_age <- as.numeric(facepp_test$age)

# Adding tool name
facepp_test$tool <- "Face++"

# ----- Creating a single overall results data frame ------
col_list <- c("image_name","tool","image_list","number_faces","mean_age","Gender_Pred")
test_results <- rbind(amazon_test[,col_list],facex_test[,col_list],microsoft_test[,col_list],facepp_test[,col_list])

#clean candidate data - Gender levels
levels(candidate_data$Gender) <- c('F','M','T')

## Note: Image quality variable is added when doing regressions 

# Combining the dataframe into an single results dataset
combined_df <- merge(candidate_data,test_results,by="image_name")

# ----- Defining Success and Error Terms -----
# -- For face detection: Success is defined as one face detected for a given image
combined_df$detection_success <- combined_df$number_faces==1
combined_df$detection_error <- !combined_df$detection_success

# -- For Gender classification: Success is the classified gender being same as the self-described gender
combined_df$gender_success <- as.character(combined_df$Gender)==as.character(combined_df$Gender_Pred)
combined_df$gender_error <- !combined_df$gender_success

# -- For Age Detection: Success definition varies as no discreate measure of success
#    Rather a continious definition of error use which is = Predicted Age - Actual Age
combined_df$age_error <- combined_df$mean_age - combined_df$Age

# Saving the combined df as Rda to be used as input for other scripts
save(combined_df, file = "./combined_df.Rda")

# Removing unused variables
rm(test_results,to_be_removed)

#'Function to do a 2 sided t-test and print result for adding into table
#'
#' @param x First side to a t-test
#' @param y Second side to a t-test
#' @param style style indicating if the star system has to be according to AER or custom
#' @return T statistic value with p-value indicated by '*' similar to regression
#' 
print_t.test <- function(x,y,style="default"){
  test <- t.test(x,y)
  if(style=="default"){
  if(test$p.value<=0.001){
    return(paste0(round(test$statistic,digits = 2),"***"))
  }else if(test$p.value<0.01){
    return(paste0(round(test$statistic,digits = 2),"**"))
  }else if(test$p.value<0.05){
    return(paste0(round(test$statistic,digits = 2),"*"))
  }else if(test$p.value<0.1){
    return(paste0(round(test$statistic,digits = 2),"."))
  }else{
    return(paste0(round(test$statistic,digits = 2),""))
  }
    
  }else if(Stlye=="aer"){
    if(test$p.value<0.01){
      return(paste0(round(test$statistic,digits = 2),"***"))
    }else if(test$p.value<0.05){
      return(paste0(round(test$statistic,digits = 2),"**"))
    }else if(test$p.value<0.1){
      return(paste0(round(test$statistic,digits = 2),"*"))
    }else{
      return(paste0(round(test$statistic,digits = 2),""))
    }
    }
}


#' Function to create a summary dump of results covering absolute numbers for face detection, gender classification and age analysis. 
#' 
#' \code{print_analysis_summary} prints results summary of input API response.
#' 
#' @param x The API response to be used for analysis. It should be standardised/cleaned prior to use here. 
#' 
#' @param confusion_matrix Logical - indicating whether the results should include a confusion matrix type response for Gender classification. 
#' 
#' @return Prints the results outlining detailed results for face detection, gender classification and age prediction.
#' 
print_analysis_summary<-function(x,confusion_matrix=F){
  print("Summary of Analysis")
  print("========================================================",right = T,quote = F)
  print(paste("Total number of images analysed",nrow(x)))
  print("---",right = T,quote = F)
  print(paste("No Faces or Multiple Faces identified:",sum(!(x$number_faces==1))))
  print(paste("No Faces identified:",sum(x$number_faces==0)))
  print(paste("Multiple Faces identified:",sum(x$number_faces>1)))
  print("",right = T,quote = F)
  print("-------------------------------",right = T,quote = F)
  
  df <- merge(x,candidate_data,by='image_name',all.x = T)
  print("Analysing Gender and Age")
  print("",right = T,quote = F)
  print("-----",right = T,quote = F)
  df <- df[which(df$number_faces==1),]
  print(paste("Total Cases for Gender Analysis",sum(!is.na(df$Gender))))
  if(confusion_matrix){
    caret::confusionMatrix(df$Gender_Pred,df$Gender)
  }else{
  df$Gender <- as.character(df$Gender)
  df$Gender_Pred <- as.character((df$Gender_Pred))
  print(paste("Total cases of missidentified Gender:",sum((df$Gender!=df$Gender_Pred),na.rm = T)))
  print(
    paste("Out of the mis-identified - Male identified as Female:" ,
          sum((((df$Gender!=df$Gender_Pred)) & (df$Gender=='M')),na.rm=T)))
  print(paste("Total Number of Males",sum(df$Gender=='M',na.rm = T)))
  print(
    paste("Out of the mis-identified - Female identified as Male:",
          sum((((df$Gender!=df$Gender_Pred)) & (df$Gender=='F')),na.rm=T)))
  print(paste("Total Number of Females",sum(df$Gender=='F',na.rm = T)))
  }
  print("------------------------------")
  print('Misindentified Age - analysis')
  print("",right = T,quote = F)
  print(paste("Cases with self described age:",
              sum(!is.na(df$Age)),"observations"))
  print(paste("Cases of incorrectly predicted age (5 year bin)",sum((df$Age>(df$mean_age+2)) | (df$Age<(df$mean_age-2)),na.rm = T)))
  print(paste("Cases of incorrectly predicted age (11 year bin)",sum((df$Age>(df$mean_age+5)) | (df$Age<(df$mean_age-5)),na.rm = T)))
  print(paste("Cases of incorrectly predicted age (21 year bin)",sum((df$Age>(df$mean_age+10)) | (df$Age<(df$mean_age-10)),na.rm = T)))
  print("----")
  df$age_error <- df$mean_age - df$Age
  print("Summary of Continuous measure of error in age prediction")
  print(summary(df$age_error))
  print("Standard Deviation")
  print(sd(df$age_error,na.rm = T))
  print("=======================================================",right = T,quote = F)
  print("End of Analysis")
  print("*******************************************************",right = T,quote = F)
  print("",right = T,quote = F)
}

#Creating the directory output to save output dump files
dir.create("./Output")

capture.output({
print("Running Analysis for Amazon",quote = F)
print_analysis_summary(amazon_test[,col_list])
print("Running Analysis for Microsoft",quote = F)
print_analysis_summary(microsoft_test[,col_list])
print("Running Analysis for FaceX",quote = F)
print_analysis_summary(facex_test[,col_list])
print("Running Analysis for Face++",quote = F)
print_analysis_summary(facepp_test[,col_list])
},file = "./Output/Error_analysis.txt",append = F)

## ------ Creating tables for the paper ------

#Creating Directory to save the generated tables 
dir.create("./Output/Tables")

# Table 1 -- data - summary
data <- combined_df[!duplicated(combined_df[,c("image_name","Gender","Age","profile_state")]),c("image_name","Gender","Age","profile_state")]
tbl1 <- data.frame(matrix(data=NA,nrow=1,ncol=4))
names(tbl1)<-c("Variable","N","Unique N","Percentage")
tbl1[1,] <- c("State/UT",nrow(data),length(unique(data$profile_state)),"-")
tbl1[2,] <- c("Gender","","","")
tbl1[3,] <- c(" - Female",sum(data$Gender=='F'),"-",
              paste0(round(100*sum(data$Gender=='F')/nrow(data),digits=2),"%"))
tbl1[4,] <- c(" - Male",sum(data$Gender=='M'),"-",
              paste0(round(100*sum(data$Gender=='M')/nrow(data),digits=2),"%"))
tbl1[5,] <- c(" - Third Gender",sum(data$Gender=='T'),"-",
              paste0(round(100*sum(data$Gender=='T')/nrow(data),digits = 2),"%"))
tbl1[6,] <- c("Age Brackets","","","")
#Setting low and high age for the brackets - first bracket is till 25 and then it increases by 15 years
low_age <- min(data$Age)
high_age <- 25
max_age <- max(data$Age)
row_position = 7
while(high_age<max_age | high_age <= 100){
 
  tbl1[row_position,] <- c(paste(" - ",low_age,"-",high_age,"years"),sum((data$Age>=low_age) & (data$Age<high_age)),
                "",paste0(round(100*sum((data$Age>=low_age) & (data$Age<high_age))/nrow(data),digits = 2),"%"))
  row_position <- row_position + 1
  low_age <- high_age
  high_age <- high_age+15
}
#print CSV and Latex output
write.csv(tbl1,file = "./Output/Tables/Table_1.csv")
print(xtable::xtable(tbl1,caption="Descriptive statistics about the dataset",label="image_df_stat"),file = "./Output/Tables/image_df_stat.tex")
## --------------------------------------

#Creating a list of unique tools analysed for rest of the tables
tool_list <- sort(unique(combined_df$tool))

# --- Table 2a  - Detection error with Gender subgroups ---
tbl2a <- data.frame(matrix(data=NA,nrow=1,ncol=7))
names(tbl2a)<-c("Tool","N","Overall","Female","Male","Third","t(between F and M)")

for(i in 1:length(tool_list)){
  tl <- tool_list[i]
  tmp <- combined_df[which(combined_df$tool==tl),]
  tbl2a[i,] <- (c(tl,nrow(tmp),
                 paste0(round(100*mean(tmp$detection_error),digits = 2),"%"),
               paste0(round(100*mean(tmp$detection_error[tmp$Gender=="F"]),digits = 2),"%"),
               paste0(round(100*mean(tmp$detection_error[tmp$Gender=="M"]),digits = 2),"%"),
               paste0(round(100*mean(tmp$detection_error[tmp$Gender=="T"]),digits = 2),"%"),
               print_t.test(tmp$detection_error,tmp$detection_error[tmp$Gender=="M"])
               ))
  rm(tmp)
}
rm(i)

#Printing csv and Latex output
write.csv(tbl2a,file = "./Output/Tables/Table_2a.csv")
print(xtable::xtable(tbl2a,caption="Error Rates in face detection across FPT tools",
                     label="detection_error_rate_a"),file = "./Output/Tables/detection_error_rate_a.tex")
## --------------------------------------

# --- Table 2b  - Detection error without Gender subgroups ---
tbl2b <- data.frame(matrix(data=NA,nrow=1,ncol=6))
names(tbl2b)<-c("Tool","N","No Faces","Multiple Faces","One Face","Overall")

for(i in 1:length(tool_list)){
  tl <- tool_list[i]
  tmp <- combined_df[which(combined_df$tool==tl),]
  tbl2b[i,] <- c(tl,nrow(tmp),
                  sum(tmp$number_faces==0),
                  sum(tmp$number_faces>1),
                  sum(tmp$number_faces==1),
                  paste0(round(100*mean(tmp$detection_error),digits = 2),"%"))
  rm(tmp)
}
rm(i)

#Printing csv and Latex output
write.csv(tbl2b,file = "./Output/Tables/Table_2b.csv")
print(xtable::xtable(tbl2b,caption="Error Rates in face detection across FPT tools",
                     label="detection_error_rate"),file = "./Output/Tables/detection_error_rate_b.tex")
## --------------------------------------

# --- Table 2c  - Detection error with one sided t-test ---
tbl2c <- data.frame(matrix(data=NA,nrow=1,ncol=7))
names(tbl2c)<-c("Tool","N","No Faces","Multiple Faces","One Face","Overall","t-test")

for(i in 1:length(tool_list)){
  tl <- tool_list[i]
  tmp <- combined_df[which(combined_df$tool==tl),]
  tbl2c[i,] <- c(tl,nrow(tmp),
                 sum(tmp$number_faces==0),
                 sum(tmp$number_faces>1),
                 sum(tmp$number_faces==1),
                 paste0(round(100*mean(tmp$detection_error),digits = 2),"%"),
                 print_t.test(tmp$detection_error[tmp$Gender=="F"],NULL)
                 )
  rm(tmp)
}
rm(i)

#Printing csv and Latex output
write.csv(tbl2c,file = "./Output/Tables/Table_2C.csv")
print(xtable::xtable(tbl2c,caption="Error Rates in face detection across FPT tools",
                     label="detection_error_rate_c"),file = "./Output/Tables/detection_error_rate_c.tex")
## --------------------------------------


# ----  Table 3a - Gender error - absolute numbers -----
tbl3a <- data.frame(matrix(data=NA,nrow=1,ncol=5))
names(tbl3a)<-c("Tool","N","F","M","T")

for(i in 1:length(tool_list)){
  tl <- tool_list[i]
  tmp <- combined_df[which(combined_df$tool==tl & combined_df$number_faces==1),]
  tbl3a[i,] <- (c(tl,nrow(tmp),
                  sum(tmp$Gender=="F",na.rm = T),
                  sum(tmp$Gender=="M",na.rm = T),
                  sum(tmp$Gender=="T",na.rm = T)))
  rm(tmp)
}
rm(i)

#Printing csv and Latex output
write.csv(tbl3a,file = "./Output/Tables/Table_3a.csv")
print(xtable::xtable(tbl3a,caption="Successfully detected faces for facial analysis",
                     label="detection_success"),file = "./Output/Tables/detection_success.tex")
## --------------------------------------


# ----  Table 3b - Gender error - absolute numbers -----
tbl3b <- data.frame(matrix(data=NA,nrow=1,ncol=8))
names(tbl3b)<-c("Tool","N","F - F","F - M","M - F","M -M","T - F","T - M")

for(i in 1:length(tool_list)){
  tl <- tool_list[i]
  tmp <- combined_df[which(combined_df$tool==tl & combined_df$number_faces==1),]
  tbl3b[i,] <- (c(tl,nrow(tmp),
                 sum(tmp$Gender=="F" & tmp$Gender_Pred=="F",na.rm = T),
                 sum(tmp$Gender=="F" & tmp$Gender_Pred=="M",na.rm = T),
                 sum(tmp$Gender=="M" & tmp$Gender_Pred=="F",na.rm = T),
                 sum(tmp$Gender=="M" & tmp$Gender_Pred=="M",na.rm = T),
                 sum(tmp$Gender=="T" & tmp$Gender_Pred=="F",na.rm = T),
                 sum(tmp$Gender=="T" & tmp$Gender_Pred=="M",na.rm = T)
                ))
  rm(tmp)
}
rm(i)

#Printing csv and Latex output
write.csv(tbl3b,file = "./Output/Tables/Table_3b.csv")
print(xtable::xtable(tbl3b,caption="Gender classification across FPT tools",
                     label="gender_result"),file = "./Output/Tables/gender_result.tex")
## --------------------------------------


# --- Table 4   - Gender error ---
tbl4 <- data.frame(matrix(data=NA,nrow=1,ncol=7))
names(tbl4)<-c("Tool","N","Overall","Female","Male","F-M","t")

for(i in 1:length(tool_list)){
  tl <- tool_list[i]
  tmp <- (combined_df[which(combined_df$tool==tl & combined_df$number_faces==1),])
  tmp <- droplevels(tmp[(tmp$Gender!='T'),])
  tbl4[i,] <- (c(tl,nrow(tmp),
                 paste0(round((100*mean(tmp$gender_error)),digits = 2),"%"),
                 paste0(round((100*mean(tmp$gender_error[which(tmp$Gender=="F")])),digits = 2),"%"),
                 paste0(round((100*mean(tmp$gender_error[which(tmp$Gender=="M")])),digits = 2),"%"),
                 paste0(round(((100*mean(tmp$gender_error[which(tmp$Gender=="F")]))-
                                 (100*mean(tmp$gender_error[which(tmp$Gender=="M")]))),digits = 2),"%")
                ,print_t.test(tmp$gender_error[which(tmp$Gender=="F")],tmp$gender_error[which(tmp$Gender=="M")])
                 ))
  rm(tmp,tl)
}
rm(i)

#Printing csv and Latex output
write.csv(tbl4,file = "./Output/Tables/Table_4.csv")
print(xtable::xtable(tbl4,caption="Error rates for gender classifcation across FPT tools",
                     label="gender_error_rate"),
                 file = "./Output/Tables/gender_error_rate.tex")
## --------------------------------------


# --- Table 5 - Age error ---
tbl5 <- data.frame(matrix(data=NA,nrow=1,ncol=5))
names(tbl5)<-c("Tool","N","5 Year","11 Year","21 Years")

for(i in 1:length(tool_list)){
  tl <- tool_list[i]
  tmp <- droplevels(combined_df[which(combined_df$tool==tl & combined_df$number_faces==1),])
  tbl5[i,] <- (c(tl,nrow(tmp),
                 paste0(round((100*sum((tmp$Age>(tmp$mean_age+2)) | (tmp$Age<(tmp$mean_age-2)),na.rm = T)/nrow(tmp)),digits = 2),"%"),
                 paste0(round((100*sum((tmp$Age>(tmp$mean_age+5)) | (tmp$Age<(tmp$mean_age-5)),na.rm = T)/nrow(tmp)),digits = 2),"%"),
                 paste0(round((100*sum((tmp$Age>(tmp$mean_age+10)) | (tmp$Age<(tmp$mean_age-10)),na.rm = T)/nrow(tmp)),digits = 2),"%")
                 ))
  rm(tmp)
}
rm(i)

#Printing csv and Latex output
write.csv(tbl5,file = "./Output/Tables/Table_5.csv")
print(xtable::xtable(tbl5,caption="Error rates in age prediction across FPT tools",label="age_error_rate"),file = "./Output/Tables/age_error_rate.tex")
## --------------------------------------

# --- Table 6 - Age error Correlation matrix ---
tbl6 <- data.frame(matrix(data=NA,nrow=1,ncol=3))
names(tbl6)<-c("Tool","N","Correlation")

for(i in 1:length(tool_list)){
  tl <- tool_list[i]
  tmp <- droplevels(combined_df[which(combined_df$tool==tl & combined_df$number_faces==1),])
  tbl6[i,] <- (c(tl,nrow(tmp),
                 round(cor(x=tmp$age_error,y=tmp$Age),digits = 2)  
                 ))
  rm(tmp)
}
rm(i)

#Printing csv and Latex output
write.csv(tbl6,file = "./Output/Tables/Table_6.csv")
print(xtable::xtable(tbl6,caption="Correlation between difference in predicted age and self described age with the self described age",
                     label="age_error_correlation"),file = "./Output/Tables/age_error_correlation.tex")
## --------------------------------------

