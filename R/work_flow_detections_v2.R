
library(lubridate)
library(sf)
library(readxl)
library(dplyr)
library(hms)
library(tidyr)


# load custom functions
# Adjust path to the file (organiza_datos_v2) in your hard disk
source("C:/CodigoR/WCS-CameraTrap/R/organiza_datos_v2.R")

###########################
### Camera Trap Work flow
###########################

archivo_excel <- "BOL-015.xlsx" #10a # put the name of file (campaign) to process
path_to_file <- paste("C:/CodigoR/WCS-CameraTrap/data/Bolivia/", # modify to your har disk
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
y_Jaguar <-  as.data.frame (full_history[[sp_number]])

# get sites
Jaguar_sites <- get.sites(path_to_file)

# join two tables
Jaguar_Hist_Det <- cbind(jaguar_sites, y_jaguar)

# write to csv file
output_dir <- "C:/CodigoR/WCS-CameraTrap/data/output/test/" # it has / at the end
write.csv(Jaguar_Hist_Det, paste (output_dir, archivo_excel, "_.csv", sep=""))
# save to R data
# save(Jaguar_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))


######################
# look for Puma
sp_number <- which(names(full_history)=="Puma concolor")
# get jaguar detection history
y_Puma <-  as.data.frame (full_history[[sp_number]])

# get sites
Puma_sites <- get.sites(path_to_file)

# join two tables
Puma_Hist_Det <- cbind(Puma_sites, y_Puma)

# write to csv file
output_dir <- "C:/CodigoR/WCS-CameraTrap/data/output/test/" # it has / at the end
write.csv(Puma_Hist_Det, paste (output_dir, archivo_excel, "_Puma.csv", sep=""))
# save to R data
# save(Puma_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))


#######################
# look for Tropero
sp_number <- which(names(full_history)=="Tayassu pecari")
# get jaguar detection history
y_Tayassu <-  as.data.frame (full_history[[sp_number]])

# get sites
Tayassu_sites <- get.sites(path_to_file)

# join two tables
Tayassu_Hist_Det <- cbind(Tayassu_sites, y_Tayassu)

# write to csv file
output_dir <- "C:/CodigoR/WCS-CameraTrap/data/output/test/" # it has / at the end
write.csv(Tayassu_Hist_Det, paste (output_dir, archivo_excel, "_Tayassu pecari.csv", sep=""))
# save to R data
# save(Tayassu_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))




