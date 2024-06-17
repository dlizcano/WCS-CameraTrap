library(lubridate)
library(sf)
require(readxl)
require(dplyr)
require(hms)
require(tidyr)


# load custom functions
source("D:/CORREGIDAS/organiza_datos.R")

###########################
### Camera Trap Work flow
###########################

archivo_excel <- "BOL-015.xlsx"
path_to_file <- paste("D:/CORREGIDAS/Bolivia/",
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

# get sites
jaguar_sites <- get.sites(path_to_file)

# join two tables
Jaguar_Hist_Det <- cbind(jaguar_sites, y_jaguar)

# write to csv file
output_dir <- "D:/CORREGIDAS/output/"
write.csv(Jaguar_Hist_Det, paste (output_dir, archivo_excel, "_.csv", sep=""))
# save to R data
save(Jaguar_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))

######################
# look for Puma
sp_number <- which(names(full_history)=="Puma concolor")
# get jaguar detection history
y_jaguar <-  as.data.frame (full_history[[sp_number]])

# get sites
jaguar_sites <- get.sites(path_to_file)

# join two tables
Jaguar_Hist_Det <- cbind(jaguar_sites, y_jaguar)

# write to csv file
output_dir <- "D:/CORREGIDAS/output/"
write.csv(Jaguar_Hist_Det, paste (output_dir, archivo_excel, "_Puma.csv", sep=""))
# save to R data
save(Jaguar_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))


#######################
# look for Tropero
sp_number <- which(names(full_history)=="Tayassu pecari")
# get jaguar detection history
y_jaguar <-  as.data.frame (full_history[[sp_number]])

# get sites
jaguar_sites <- get.sites(path_to_file)

# join two tables
Jaguar_Hist_Det <- cbind(jaguar_sites, y_jaguar)

# write to csv file
output_dir <- "D:/CORREGIDAS/output/"
write.csv(Jaguar_Hist_Det, paste (output_dir, archivo_excel, "_Tayassu pecari.csv", sep=""))
# save to R data
save(Jaguar_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))



###########################################
# look for ocelot
sp_number <- which(names(full_history)=="Leopardus pardalis")
# get jaguar detection history
y_jaguar <-  as.data.frame (full_history[[sp_number]])

# get sites
jaguar_sites <- get.sites(path_to_file)

# join two tables
Jaguar_Hist_Det <- cbind(jaguar_sites, y_jaguar)

# write to csv file
output_dir <- "D:/CORREGIDAS/output/"
write.csv(Jaguar_Hist_Det, paste (output_dir, archivo_excel, "_ocelot.csv", sep=""))
# save to R data
save(Jaguar_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))

###########################################
# look for Taitetu
sp_number <- which(names(full_history)=="Pecari tajacu")
# get jaguar detection history
y_jaguar <-  as.data.frame (full_history[[sp_number]])

# get sites
jaguar_sites <- get.sites(path_to_file)

# join two tables
Jaguar_Hist_Det <- cbind(jaguar_sites, y_jaguar)

# write to csv file
output_dir <- "D:/CORREGIDAS/output/"
write.csv(Jaguar_Hist_Det, paste (output_dir, archivo_excel, "_taitetu.csv", sep=""))
# save to R data
save(Jaguar_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))



###########################################
# look for huaso
sp_number <- which(names(full_history)=="Mazama americana")
# get jaguar detection history
y_jaguar <-  as.data.frame (full_history[[sp_number]])

# get sites
jaguar_sites <- get.sites(path_to_file)

# join two tables
Jaguar_Hist_Det <- cbind(jaguar_sites, y_jaguar)

# write to csv file
output_dir <- "D:/CORREGIDAS/output/"
write.csv(Jaguar_Hist_Det, paste (output_dir, archivo_excel, "_huaso.csv", sep=""))
# save to R data
save(Jaguar_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))



###########################################
# look for urina
sp_number <- which(names(full_history)=="Mazama gouazoubira")
# get jaguar detection history
y_jaguar <-  as.data.frame (full_history[[sp_number]])

# get sites
jaguar_sites <- get.sites(path_to_file)

# join two tables
Jaguar_Hist_Det <- cbind(jaguar_sites, y_jaguar)

# write to csv file
output_dir <- "D:/CORREGIDAS/output/"
write.csv(Jaguar_Hist_Det, paste (output_dir, archivo_excel, "_urina.csv", sep=""))
# save to R data
save(Jaguar_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))




###########################################
# look for tatu
sp_number <- which(names(full_history)=="Dasypus novemcinctus")
# get jaguar detection history
y_jaguar <-  as.data.frame (full_history[[sp_number]])

# get sites
jaguar_sites <- get.sites(path_to_file)

# join two tables
Jaguar_Hist_Det <- cbind(jaguar_sites, y_jaguar)

# write to csv file
output_dir <- "D:/CORREGIDAS/output/"
write.csv(Jaguar_Hist_Det, paste (output_dir, archivo_excel, "_tatu.csv", sep=""))
# save to R data
save(Jaguar_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))


###########################################
# look for 15kilos
sp_number <- which(names(full_history)=="Dasypus kappleri")
# get jaguar detection history
y_jaguar <-  as.data.frame (full_history[[sp_number]])

# get sites
jaguar_sites <- get.sites(path_to_file)

# join two tables
Jaguar_Hist_Det <- cbind(jaguar_sites, y_jaguar)

# write to csv file
output_dir <- "D:/CORREGIDAS/output/"
write.csv(Jaguar_Hist_Det, paste (output_dir, archivo_excel, "_15k.csv", sep=""))
# save to R data
save(Jaguar_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))


###########################################

# look for jochipintado
sp_number <- which(names(full_history)=="Cuniculus paca")
# get jaguar detection history
y_jaguar <-  as.data.frame (full_history[[sp_number]])

# get sites
jaguar_sites <- get.sites(path_to_file)

# join two tables
Jaguar_Hist_Det <- cbind(jaguar_sites, y_jaguar)

# write to csv file
output_dir <- "D:/CORREGIDAS/output/"
write.csv(Jaguar_Hist_Det, paste (output_dir, archivo_excel, "_jochipintado.csv", sep=""))
# save to R data
save(Jaguar_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))


###########################################

# look for jochicolorado
sp_number <- which(names(full_history)=="Dasyprocta punctata")
# get jaguar detection history
y_jaguar <-  as.data.frame (full_history[[sp_number]])

# get sites
jaguar_sites <- get.sites(path_to_file)

# join two tables
Jaguar_Hist_Det <- cbind(jaguar_sites, y_jaguar)

# write to csv file
output_dir <- "D:/CORREGIDAS/output/"
write.csv(Jaguar_Hist_Det, paste (output_dir, archivo_excel, "_jochicolorado.csv", sep=""))
# save to R data
save(Jaguar_Hist_Det, file = paste (output_dir, archivo_excel, "_R.Rdata",sep=""))
