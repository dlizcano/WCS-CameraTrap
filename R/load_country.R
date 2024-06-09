

#################################
### Function to read by country
#################################
require(readxl)
data_by_country <- function(country="Argentina"){
  # Identify file path string names
  pais <- paste("C:/CodigoR/WCS_2024/camera_trap/data", country, sep="/")
  recIDs <- list.files(pais,  recursive = FALSE, pattern = ".xlsx")
  i.strings <- paste0(pais, "/", recIDs, sep="")
  
  # make a list with all tables
  deployment_pais<-lapply(i.strings, function(x) read_excel(x, sheet = "Deployment", 
                                                            col_types = c("numeric", 
                                                                          "text", "text", "numeric", "numeric", 
                                                                          "text", "text", "text", "text", "text", 
                                                                          "text", "text", "text", "text", "text", 
                                                                          "text"), col_names = TRUE))
  
  image_pais<-lapply(i.strings, function(x) read_excel(x, sheet = "Image", skip = 1,
                                                       col_types = c("text", 
                                                                     "text", "text", "text", "text", "text", 
                                                                     "text", "text", "numeric", "text", 
                                                                     "numeric", "text", "text", "text", 
                                                                     "numeric", "text", "text"), col_names = TRUE))
  
  
  # extract names... 
  deployment_pais1<-list() #empty list
  # add file name at the end
  for(i in 1:length(recIDs)) {
    ExcelFile<-rep(recIDs[i],nrow(deployment_pais[[i]]))
    deployment_pais1[[i]]<-cbind(deployment_pais[[i]], ExcelFile)
    colnames(deployment_pais1[[i]])[ncol(deployment_pais)]<-"ExcelFile"
    #### 
    print(i)
    print(names(deployment_pais[[i]]))
    
  } # end loop
  
  
  
  ##############################
  # loop to checking problems...
  # remove comment to activate
  ##############################
  for(h in 1:length(recIDs)){
    archivo <- read_excel(i.strings[h], sheet = "Image", skip = 1,
                          col_types = c("text",
                                        "text", "text", "text", "text", "text",
                                        "text", "text", "numeric", "text",
                                        "numeric", "text", "text", "text",
                                        "numeric", "text", "text"), col_names = TRUE)
    print (paste("archivo: ", recIDs[h], "cols: ", dim(archivo)[2], sep=""))
    if(is.character(archivo$`Date_Time Captured`)==FALSE){
      print(paste("date problem in image sheet in file:"), recIDs[h] )
      
    }
  }
  
  
  
  ### convert to dataframe
  # deployment_Pais<- deployment_pais1 %>%  do.call(rbind, .) 
  deployment_Pais<-  bind_rows(deployment_pais1) %>% select("Deployment ID",
                                                            "Longitude Resolution",
                                                            "Latitude Resolution",
                                                            "Camera Deployment Begin Date",
                                                            "Camera Deployment End Date",
                                                            "Bait Type",
                                                            "Bait Description",
                                                            "Camera Id",
                                                            "Camera Type",
                                                            "ExcelFile") 
  # %>% write.csv(file=paste0("G:/Panama_Audubon/result/formated/pegadas/",
  
  deployment_Pais$year <- year(as.Date(deployment_Pais$`Camera Deployment Begin Date`))
  
  # Make dataframe binding selected rows
  image_Pais<-  bind_rows(image_pais ) %>% select("Deployment ID",
                                                  "Photo Type",
                                                  "Genus Species",
                                                  "Date_Time Captured",
                                                  "Independent event",
                                                  "Age",
                                                  "Sex",
                                                  "Count")  |> left_join(deployment_Pais)
  
  return(image_Pais) # return the join table
  
} ### end of function



