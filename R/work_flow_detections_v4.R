
# library(lubridate)
library(sf)
library(readxl)
library(dplyr)
library(hms)
library(tidyr)
library(tidyverse)
# library(camtrapR)
 
# load custom functions
# Adjust path to the file (organiza_datos_v4) in your hard disk
source("F:/WCS-CameraTrap/R/organiza_datos_v4.R")

###########################
### Camera Trap Work flow
###########################

archivo_excel <- "VEN-003_Caura2014.xlsx" #10a # put the name of file (campaign) to process
path_to_file <- paste("F:/WCS-CameraTrap/data/BDcorregidas/Venezuela/", # ends in / modify to your hard disk
                      archivo_excel, sep="")

# load data
full_table <-loadproject (path_to_file)

# Detection history creation
full_history <- wcs.det_history.creator(data=full_table)

# date
dates_history <- wcs.date_history.creator(data=full_table)

# look at the species
sort(names(full_history)) 


# get sites and covariates
sites <- get.sites(path_to_file) |> 
  mutate(excel_file=archivo_excel) #|> # add excel file
sites <- sites |> mutate(longitude = sf::st_coordinates(sites)[,1], # add lat long
                         latitude = sf::st_coordinates(sites)[,2]) |> 
  st_drop_geometry() #|> # drop geometry


##########################
# look for jaguar
sp_number <- which(names(full_history)=="Panthera onca")
# get jaguar detection history
y_jaguar <-  as.data.frame (full_history[[sp_number]])
date_jaguar <-  as.data.frame (dates_history[[sp_number]])


# join two tables: detection history and sites 
Jaguar_Hist_Det <- cbind(sites, y_jaguar)
Jaguar_Hist_Date <- cbind(sites, date_jaguar)


# write to csv file
output_dir <- "F:/WCS-CameraTrap/data/BDcorregidas/Ecuador/Jaguar/" # it has / at the end
write.csv(Jaguar_Hist_Det, paste (output_dir, archivo_excel, "_Jaguar.csv", sep=""))
write.csv(Jaguar_Hist_Date, paste (output_dir, archivo_excel, "_Jaguar_date.csv", sep=""))

# save to R data
# save(Jaguar_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))



######################
# look for Puma
sp_number <- which(names(full_history)=="Puma concolor")
# get jaguar detection history
y_Puma <-  as.data.frame (full_history[[sp_number]])
date_Puma <-  as.data.frame (dates_history[[sp_number]])
  

# join two tables: detection history and sites 
Puma_Hist_Det <- cbind(sites, y_Puma)
Puma_Hist_Date <- cbind(sites, date_Puma)


# write to csv file
output_dir <- "F:/WCS-CameraTrap/data/BDcorregidas/Venezuela/Puma/" # it has / at the end
write.csv(Puma_Hist_Det, paste (output_dir, archivo_excel, "_Puma.csv", sep=""))
write.csv(Puma_Hist_Date, paste (output_dir, archivo_excel, "_Puma_date.csv", sep=""))
# save to R data
# save(Puma_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))


#######################
# look for Tropero
sp_number <- which(names(full_history)=="Tayassu pecari")
# get jaguar detection history
y_Tayassu <-  as.data.frame (full_history[[sp_number]])
date_Tayassu <-  as.data.frame (dates_history[[sp_number]])


# join two tables: detection history and sites 
Tayassu_Hist_Det <- cbind(sites, y_Tayassu)
Tayassu_Hist_Date <- cbind(sites, date_Tayassu)

# write to csv file
output_dir <- "F:/WCS-CameraTrap/data/BDcorregidas/Ecuador/Pecari/" # it has / at the end
write.csv(Tayassu_Hist_Det, paste (output_dir, archivo_excel, "_T_pecari.csv", sep=""))
write.csv(Tayassu_Hist_Date, paste (output_dir, archivo_excel, "_T_pecari_date.csv", sep=""))
# save to R data
# save(Tayassu_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))




#######################
# look for Speothos
sp_number <- which(names(full_history)=="Speothos venaticus")
# get jaguar detection history
y_Speothos <-  as.data.frame (full_history[[sp_number]])
date_Speothos <-  as.data.frame (dates_history[[sp_number]])


# join two tables: detection history and sites 
Speothos_Hist_Det <- cbind(sites, y_Speothos)
Speothos_Hist_Date <- cbind(sites, date_Speothos)


# write to csv file
output_dir <- "F:/WCS-CameraTrap/data/BDcorregidas/Ecuador/Speothos/" # it has / at the end
write.csv(Speothos_Hist_Det, paste (output_dir, archivo_excel, "_Speothos.csv", sep=""))
write.csv(Speothos_Hist_Date, paste (output_dir, archivo_excel, "_Speothos_date.csv", sep=""))
# save to R data
# save(Tayassu_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))



#######################
# look for Atelocynus
sp_number <- which(names(full_history)=="Atelocynus microtis")
# get jaguar detection history
y_Atelocynus <-  as.data.frame (full_history[[sp_number]])
date_Atelocynus <-  as.data.frame (dates_history[[sp_number]])


# join two tables: detection history and sites 
Atelocynus_Hist_Det <- cbind(sites, y_Atelocynus)
Atelocynus_Hist_Date <- cbind(sites, date_Atelocynus)

# write to csv file
output_dir <- "F:/WCS-CameraTrap/data/BDcorregidas/Ecuador/Atelocynus/" # it has / at the end
write.csv(Atelocynus_Hist_Det, paste (output_dir, archivo_excel, "_Atelocynus.csv", sep=""))
write.csv(Atelocynus_Hist_Date, paste (output_dir, archivo_excel, "_Atelocynus_date.csv", sep=""))
# save to R data
# save(Tayassu_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))


