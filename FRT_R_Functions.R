library(tidyverse)
library(httr)

#' Function to get API response from Face++
#' 
#' @param image_list List with address of images
#' @param filename Name of file where the response is to be saved
#' @param save Logical if the response should be paid out in CSV
#' 
#' @return Saves a RDa (and CSV) file with response for API call with 
#' details including number of faces detected, age predicted, gender classified. 
#' 
#' @note Remember to update the API KEY and API SECRET in the API call
facepp_analysis<-function(image_list,file_name,save=F){
  # Creating a dataframe to save the API response
  output <- data.frame(image_list)
  output$response <- NA
  output$age <- NA
  output$Gender <- NA
  output$number_faces <- NA
  for(i in 1:nrow(output)){
    Sys.sleep(0.33)
    # Doing the API call
    r <- POST(url = "https://api-us.faceplusplus.com/facepp/v3/detect",
              body= list(api_key = "YOUR API KEY GOES HERE",
                         api_secret = "YOUR API SECRET GOES HERE",
                         return_landmark=0,
                         return_attributes="gender,age,smiling,headpose,facequality,blur,eyestatus,emotion,ethnicity,beauty,mouthstatus,eyegaze,skinstatus",
                         image_file=upload_file(output$image_list[i])),encode = c("multipart"),verbose())
    output$response[i] <- content(r,as='text')
    # Finding the number of faces detected
    output$number_faces[i] <- length(content(r)$faces)
    # Finding age and value
    if(output$number_faces[i]>0){
      output$age[i] <- content(r)$faces[[1]]$attributes$age$value
      output$Gender[i] <- content(r)$faces[[1]]$attributes$gender$value
    }
    rm(r)
  }
  # Saving final data frame
  if(save){  write.csv(output,paste0(file_name,".csv"))
    save(output,file = paste0(file_name,".Rda"))
  }
  return(output)
}

#' Function to get API response from Microsoft
#' 
#' @param image_list List with address of images
#' @param filename Name of file where the response is to be saved
#' @param save Logical if the response should be paid out in CSV
#' 
#' @return Saves a RDa (and CSV) file with response for API call with 
#' details including number of faces detected, age predicted, gender classified. 
#'
#' @note Remember to update the Subscription Key in the API call
microsoft_analysis<-function(image_list,file_name,save=F){
  # Creating a dataframe to save the API response
  output <- data.frame(image_list)
  output$response <- NA
  output$age <- NA
  output$Gender <- NA
  output$number_faces <- NA
  for(i in 1:nrow(output)){
    Sys.sleep(0.1)
    tryCatch(expr = {
      # Doing API call
      r <- POST(url = "https://ai-bias-new.cognitiveservices.azure.com/face/v1.0/detect?detectionModel=detection_01&returnFaceId=true&returnFaceLandmarks=false&returnFaceAttributes=age,gender,headPose,smile,facialHair,glasses,emotion,hair,makeup,occlusion,accessories,blur,exposure,noise",
                add_headers(.headers=c("Content-Type" = "application/octet-stream",
                                       "Ocp-Apim-Subscription-Key" = "YOUR SUBSCRIPTION KEY GOES HERE")),
                body= upload_file(output$image_list[i]),encode = c("multipart"))
      output$response[i] <- content(r,as='text')
      # Finding the number of faces detected
      output$number_faces[i] <- length(content(r))
      #Finding age and value
      if(output$number_faces[i]>0){
        output$age[i] <- content(r)[[1]]$faceAttributes$age
        output$Gender[i] <- content(r)[[1]]$faceAttributes$gender
      }
      rm(r)
    },
    error = function(cond){
      message("There was an error reading this file",output$image_list[i])
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    }
    ) 
  }
  # Saving final data frame
  if(save){  
    write.csv(output,paste0(file_name,".csv"))
    save(output,file = paste0(file_name,".Rda"))
  }
  return(output)
}

#' Function to get API response from FaceX
#' 
#' @param image_list List with address of images
#' @param filename Name of file where the response is to be saved
#' @param save Logical if the response should be paid out in CSV
#' 
#' @return Saves a RDa (and CSV) file with response for API call with 
#' details including number of faces detected, age predicted, gender classified. 
#'
#' @note Remember to update the USER ID in the API call
facex_analysis<-function(image_list,file_name,save=F){
  # Creating a dataframe to save the API response
  output <- data.frame(image_list)
  output$response <- NA
  output$age <- NA
  output$Gender <- NA
  output$number_faces <- NA
  output$Gender_confidence <- NA
  for(i in 1:nrow(output)){
    #Sys.sleep(3.33)
    tryCatch(
      #API call
      expr = {r <- POST(url = "http://facexapi.com/get_image_attr",
                        add_headers(.headers=c("user_id" = "YOUR USER ID GOES HERE")),
                        body= list(image_attr=upload_file(output$image_list[i])),encode = c("multipart"),verbose())
      output$response[i] <- content(r,as='text')
      #Filtering out number of faces
      output$number_faces[i] <- length(content(r)$data$attributes)-1
      # Filtering out age, gender and confidence
      if(output$number_faces[i]>0){
        output$age[i] <- content(r)$data$attributes$face_id_0$age
        output$Gender[i] <- content(r)$data$attributes$face_id_0$gender
        output$Gender_confidence[i] <- content(r)$data$attributes$face_id_0$gender_confidence
      }
      rm(r)},
      error = function(cond){
        message("There was an error reading this file",output$image_list[i])
        message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        return(NA)
      })
  }
  # Aligning face numbering with others API responses
  output$number_faces <- ifelse(output$number_faces==-1,0,output$number_faces )
  # Saving final data frame
  if(save){  
    write.csv(output,paste0(file_name,".csv"))
    save(output,file = paste0(file_name,".Rda"))
  }
  return(output)
}
