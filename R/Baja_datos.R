

# baja datos 
# ejemplo con venezuela



nombrecol <- c("ID", "DeploymentID", 
               "ImageId", "Location",  
               "PhotoType", "Identifiedby",	
               "Point", "GenusSpecies", 
               "NEWSpecies", "IUCNID", "Date_Time", 
               "Independent", "Age", "Sex",	 
               "IndividualID", "Count", "recognizable")# , 	
# "notes")



mapview(Venezuela_cameras, zcol = c("year"),  burst = TRUE)

# Identify file path string names
pais <- "C:/WCS_2024/camera_trap/data/Venezuela/"
recIDs <- list.files(pais,  recursive = TRUE)#, pattern = ".csv")
i.strings <- paste0(pais, recIDs)

# make a list with all tables
deployment_venezuela <- lapply(i.strings, function(x) read_excel(x, sheet = "Deployment", col_names = TRUE))
# get images in a list
images_venezuela <- lapply(i.strings, function(x) read_excel(x, range = "Image!A5:F8"))

images_venezuela <- lapply(i.strings, function(x) read_excel(x, sheet = "Image", skip = 2, n_max = 17, col_names = TRUE))

# extrae names... empty list
deployment_venezuela1 <- list()
# add file name at the end
for (i in 1:3) {
  origin <- rep(recIDs[i], nrow(deployment_venezuela[[i]]))
  #### Deployment
  deployment_venezuela1[[i]] <- cbind(deployment_venezuela[[i]], origin)
  colnames(deployment_venezuela1[[i]])[ncol(deployment_venezuela)] <- "origin"
}

deployment_Venezuela<- deployment_venezuela1 %>%  do.call(rbind, .) # %>% write.csv(file=paste0("G:/Panama_Audubon/result/formated/pegadas/",
images_venezuela <- images_venezuela %>% do.call(rbind, .)
