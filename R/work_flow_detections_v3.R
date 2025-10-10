
# library(lubridate)
library(sf)
library(readxl)
library(dplyr)
library(hms)
library(tidyr)

library(tidyverse)


# load custom functions
# Adjust path to the file (organiza_datos_v3) in your hard disk
source("C:/CodigoR/WCS-CameraTrap/R/organiza_datos_v3.R")

###########################
### Camera Trap Work flow
###########################

archivo_excel <- "VEN-001_Caura2011_editedDL.xlsx" #10a # put the name of file (campaign) to process
path_to_file <- paste("F:/WCS-CameraTrap/data/BDcorregidas/Venezuela/", # modify to your hard disk
                      archivo_excel, sep="")

# load data
full_table <-loadproject (path_to_file)

# Detection history creation
full_history <- wcs.det_history.creator(data=full_table)
names(full_history)


##########################
# look for jaguar
sp_number <- which(names(full_history)=="Panthera onca")
# get jaguar detection history
y_jaguar <-  as.data.frame (full_history[[sp_number]])

# get sites and covariates
jaguar_sites <- get.sites(path_to_file) |> 
          mutate(excel_file=archivo_excel) #|> # add excel file
jaguar_sites <- jaguar_sites |> mutate(longitude = sf::st_coordinates(jaguar_sites)[,1], # add lat long
                 latitude = sf::st_coordinates(jaguar_sites)[,2]) |> 
                st_drop_geometry() #|> # drop geometry


# join two tables: detection history and sites 
Jaguar_Hist_Det <- cbind(y_jaguar, jaguar_sites)

# write to csv file
output_dir <- "G:/WCS-CameraTrap/data/BDcorregidas/Venezuela/Jaguar/" # it has / at the end
write.csv(Jaguar_Hist_Det, paste (output_dir, archivo_excel, "_Jaguar.csv", sep=""))
# save to R data
# save(Jaguar_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))


######################
# look for Puma
sp_number <- which(names(full_history)=="Puma concolor")
# get jaguar detection history
y_Puma <-  as.data.frame (full_history[[sp_number]])

# get sites
Puma_sites <- get.sites(path_to_file) |> 
  mutate(excel_file=archivo_excel) #|> # add excel file
Puma_sites <- Puma_sites |> mutate(longitude = sf::st_coordinates(Puma_sites)[,1], # add lat long
                                       latitude = sf::st_coordinates(Puma_sites)[,2]) |> 
  st_drop_geometry() #|> # drop geometry


# join two tables: detection history and sites 
Puma_Hist_Det <- cbind(y_Puma, Puma_sites)

# write to csv file
output_dir <- "G:/WCS-CameraTrap/data/BDcorregidas/Venezuela/Puma/" # it has / at the end
write.csv(Puma_Hist_Det, paste (output_dir, archivo_excel, "_Puma.csv", sep=""))
# save to R data
# save(Puma_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))


#######################
# look for Tropero
sp_number <- which(names(full_history)=="Tayassu pecari")
# get jaguar detection history
y_Tayassu <-  as.data.frame (full_history[[sp_number]])

# get sites
Tayassu_sites <- get.sites(path_to_file) |> 
  mutate(excel_file=archivo_excel) #|> # add excel file
Tayassu_sites <- Tayassu_sites |> mutate(longitude = sf::st_coordinates(Tayassu_sites)[,1], # add lat long
                                   latitude = sf::st_coordinates(Tayassu_sites)[,2]) |> 
  st_drop_geometry() #|> # drop geometry


# join two tables: detection history and sites 
Tayassu_Hist_Det <- cbind(y_Tayassu, Tayassu_sites)

# write to csv file
output_dir <- "G:/WCS-CameraTrap/data/BDcorregidas/Venezuela/Pecari/" # it has / at the end
write.csv(Tayassu_Hist_Det, paste (output_dir, archivo_excel, "_T_pecari.csv", sep=""))
# save to R data
# save(Tayassu_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))




