
library(readxl)
library(tidyverse)
library(ggmap)
library(osmdata)
library(sf)
library(mapview)

# Identify file path string names
pais <- "C:/CodigoR/WCS_2024/camera_trap/data/Bolivia/"
recIDs <- list.files(pais,  recursive = TRUE)#, pattern = ".csv")
i.strings <- paste0(pais, recIDs)

# make a list with all tables
deployment <- lapply(i.strings[1:49], function(x) read_excel(x, sheet = "Deployment", col_names = TRUE,
                                                           skip = 1))
# extrae names... empty list
deployment1 <- list()
# add file name at the end
for(i in 1:49) { 
  origin <- rep(recIDs[i],nrow(deployment[[i]]))
  deployment1[[i]] <- cbind(deployment[[i]], origin)
  colnames(deployment1[[i]])[ncol(deployment)] <- "origin"
}

############# lee 2 da parte
# make a list with all tables
deployment_b<-lapply(i.strings[50:62], function(x) read_excel(x, sheet = "Deployment", col_names = TRUE
                                                           ))
# empty list
deployment2<-list()
# add file name at the end
for(i in 1:13) {
  origin<-rep(recIDs[i+49],nrow(deployment_b[[i]]))
  deployment2[[i]]<-cbind(deployment_b[[i]], origin)
  colnames(deployment2[[i]])[ncol(deployment_b)]<-"origin"
}


# make dataframe
deployment_pais1 <- deployment1 %>%  do.call(rbind, .) # %>% write.csv(file=paste0("G:/Panama_Audubon/result/formated/pegadas/",
#                                               "Parita_01_ElAgallito_05.csv"))
deployment_pais2 <- deployment2 %>%  do.call(rbind, .) # %>% write.csv(file=paste0("G:/Panama_Audubon/result/formated/pegadas/",


#deployment_pais2$year <- year(as.Date(deployment_pais2$`Camera Deployment Begin Date`))
deployment_pais2$year <- year(as.Date(deployment_pais2$`Camera Deployment Begin Date`, "%Y-%m-%d"))
deployment_pais1$year <- year(as.Date(deployment_pais1$`Camera Deployment Begin Date`, "%Y/%m/%d"))


# # check equal names
# for (i in 50:length(i.strings)) {
#   a <- read_excel(i.strings[i],
#                   sheet = "Deployment",
#                   col_names = TRUE
#                   #skip = 1
#                   ) 
#   print(names (a))
#   print(i)
#   print(recIDs[i])
#   #print(a)
# }

deployment_pais <- rbind(deployment_pais1, deployment_pais2)

# convert NONE to NA
index <- which(deployment_pais$"Camera Deployment Begin Date"== "NONE")
deployment_pais$`Camera Deployment Begin Date`[index]=NA


Bolivia_bb <- getbb("Bolivia")

projlatlon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

Bolivia_cameras <- st_as_sf(x = deployment_pais,                         
               coords = c("Longitude Resolution", "Latitude Resolution"),
               crs = projlatlon)
### fix dates
Bolivia_cameras$`Camera Deployment Begin Date` <- as.Date(Bolivia_cameras$`Camera Deployment Begin Date`)
Bolivia_cameras$`Camera Deployment End Date` <- as.Date(Bolivia_cameras$`Camera Deployment End Date`)


mapview(Bolivia_cameras, zcol = c("year"),  burst = TRUE) # burst = TRUE prouce uniques
### Notas algunas camaras en Argentina movida por diego. Fechas sin año y BOLCH-010.xlsx no tiene coordenada Longitud 

########################
### PERU 
########################

# Identify file path string names
pais <- "C:/CodigoR/WCS_2024/camera_trap/data/Peru/"
recIDs <- list.files(pais,  recursive = TRUE, pattern = ".xlsx")
i.strings <- paste0(pais, recIDs)

# make a list with all tables
deployment_peru<-lapply(i.strings, function(x) read_excel(x, sheet = "Deployment", col_names = TRUE
                                                          ))
# extrae names... empty list
deployment_peru1<-list()
# add file name at the end
for(i in 1:4) {
  origin<-rep(recIDs[i],nrow(deployment_peru[[i]]))
  deployment_peru1[[i]]<-cbind(deployment_peru[[i]], origin)
  colnames(deployment_peru1[[i]])[ncol(deployment_peru)]<-"origin"
}
deployment_Peru<- deployment_peru1 %>%  do.call(rbind, .) # %>% write.csv(file=paste0("G:/Panama_Audubon/result/formated/pegadas/",

deployment_Peru$year <- year(as.Date(deployment_Peru$`Camera Deployment Begin Date`))

Peru_cameras <- st_as_sf(x = deployment_Peru,                         
                            coords = c("Longitude Resolution", "Latitude Resolution"),
                            crs = projlatlon)
mapview(Peru_cameras, zcol = c("year"),  burst = TRUE) # burst = TRUE prouce uniques



########### READ DATA Images

images_peru<-lapply(i.strings, function(x) read_excel(x, sheet = "Image", col_names = TRUE))



                                                          

########################
### COLOMBIA 
########################

# Identify file path string names
pais <- "C:/CodigoR/WCS_2024/camera_trap/data/Colombia/"
recIDs <- list.files(pais,  recursive = TRUE, pattern = ".xlsx")
i.strings <- paste0(pais, recIDs)

# make a list with all tables
deployment_colombia<-lapply(i.strings, function(x) read_excel(x, sheet = "Deployment", col_names = TRUE))
# extrae names... empty list
deployment_colombia1<-list()
# add file name at the end
for(i in 1:6) {
  origin<-rep(recIDs[i],nrow(deployment_colombia[[i]]))
  deployment_colombia1[[i]]<-cbind(deployment_colombia[[i]], origin)
  colnames(deployment_colombia1[[i]])[ncol(deployment_colombia)]<-"origin"
}
deployment_Colombia<- deployment_colombia1 %>%  do.call(rbind, .) # %>% write.csv(file=paste0("G:/Panama_Audubon/result/formated/pegadas/",

deployment_Colombia$year <- year(as.Date(deployment_Colombia$`Camera Deployment Begin Date`))

projlatlon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
Colombia_cameras <- st_as_sf(x = deployment_Colombia,                         
                         coords = c("Longitude Resolution", "Latitude Resolution"),
                         crs = projlatlon)

mapview(Colombia_cameras, zcol = c("year"),  burst = TRUE) # burst = TRUE prouce uniques

# Notas COL-005_Mag2016_WCS y COL-006-Mag2017_WCS tiene coordenadas invertidas


# mapview(Colombia_cameras, zcol = c("year"),  burst = TRUE) + mapview(Peru_cameras, zcol = c("year"),  burst = TRUE) + mapview(Bolivia_cameras, zcol = c("year"),  burst = TRUE) # burst = TRUE prouce uniques

########################
### VENEZUELA 
########################

# Identify file path string names
pais <- "C:/CodigoR/WCS_2024/camera_trap/data/Venezuela/"
recIDs <- list.files(pais,  recursive = TRUE)#, pattern = ".csv")
i.strings <- paste0(pais, recIDs)

# make a list with all tables
deployment_venezuela<-lapply(i.strings, function(x) read_excel(x, sheet = "Deployment", col_names = TRUE))
# extrae names... empty list
deployment_venezuela1<-list()
# add file name at the end
for(i in 1:3) {
  origin<-rep(recIDs[i],nrow(deployment_venezuela[[i]]))
  deployment_venezuela1[[i]]<-cbind(deployment_venezuela[[i]], origin)
  colnames(deployment_venezuela1[[i]])[ncol(deployment_venezuela)]<-"origin"
}
deployment_Venezuela<- deployment_venezuela1 %>%  do.call(rbind, .) # %>% write.csv(file=paste0("G:/Panama_Audubon/result/formated/pegadas/",

deployment_Venezuela$year <- year(as.Date(deployment_Venezuela$`Camera Deployment Begin Date`))

projlatlon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
Venezuela_cameras <- st_as_sf(x = deployment_Venezuela,                         
                             coords = c("Longitude Resolution", "Latitude Resolution"),
                             crs = projlatlon)

mapview(Venezuela_cameras, zcol = c("year"),  burst = TRUE) # burst = TRUE prouce uniques

mapview(Colombia_cameras, zcol = c("year"),  burst = TRUE) + mapview(Peru_cameras, zcol = c("year"),  burst = TRUE) + mapview(Bolivia_cameras, zcol = c("year"),  burst = TRUE) + mapview(Venezuela_cameras, zcol = c("year"),  burst = TRUE)



########################
### ECUADOR 
########################

# Identify file path string names
pais <- "C:/CodigoR/WCS_2024/camera_trap/data/Ecuador/csv/Deployment/"
recIDs <- list.files(pais,  recursive = TRUE)#, pattern = ".xlsx")
i.strings <- paste0(pais, recIDs)

# make a list with all tables
deployment_ecuador<-lapply(i.strings, function(x) read_delim(x, 
                                                             delim = ";", 
                                                             escape_double = FALSE, 
                                                             trim_ws = TRUE))
# extrae names... empty list
deployment_ecuador1<-list()
# add file name at the end
for(i in 1:length(deployment_ecuador)) {
  origin<-rep(recIDs[i],nrow(deployment_ecuador[[i]]))
  deployment_ecuador1[[i]]<-cbind(deployment_ecuador[[i]], origin)
  colnames(deployment_ecuador1[[i]])[ncol(deployment_ecuador)]<-"origin"
}
deployment_Ecuador<- deployment_ecuador1 %>%  do.call(rbind, .) # %>% write.csv(file=paste0("G:/Panama_Audubon/result/formated/pegadas/",

deployment_Ecuador$year <- year(as.Date(deployment_Ecuador$`Camera Deployment Begin Date`))

projlatlon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
Ecuador_cameras <- st_as_sf(x = deployment_Ecuador,                         
                              coords = c("Longitude Resolution", "Latitude Resolution"),
                              crs = projlatlon)

mapview(Ecuador_cameras, zcol = c("year"),  burst = TRUE) # burst = TRUE prouce uniques

# mapview(Colombia_cameras, zcol = c("year"),  burst = TRUE) + mapview(Peru_cameras, zcol = c("year"),  burst = TRUE) + mapview(Bolivia_cameras, zcol = c("year"),  burst = TRUE) + mapview(Venezuela_cameras, zcol = c("year"),  burst = TRUE)

# verificar ECU_011_17MAR15_Final coordenadas de 10 ultimas camaras


########################
### BRAZIl 
########################

# Identify file path string names
pais <- "C:/CodigoR/WCS_2024/camera_trap/data/Brazil/"
recIDs <- list.files(pais,  recursive = TRUE)#, pattern = ".csv")
i.strings <- paste0(pais, recIDs)

# make a list with all tables
deployment_brazil<-lapply(i.strings, function(x) read_excel(x, sheet = "Deployment", col_names = TRUE))
# extrae names... empty list
deployment_brazil1<-list()
# add file name at the end
for(i in 1:5) {
  origin<-rep(recIDs[i],nrow(deployment_brazil[[i]]))
  deployment_brazil1[[i]]<-cbind(deployment_brazil[[i]], origin)
  colnames(deployment_brazil1[[i]])[ncol(deployment_brazil)]<-"origin"
}
deployment_Brazil<- deployment_brazil1 %>%  do.call(rbind, .) # %>% write.csv(file=paste0("G:/Panama_Audubon/result/formated/pegadas/",

deployment_Brazil$year <- year(as.Date(deployment_Brazil$`Camera Deployment Begin Date`))

projlatlon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
Brazil_cameras <- st_as_sf(x = deployment_Brazil,                         
                              coords = c("Longitude Resolution", "Latitude Resolution"),
                              crs = projlatlon)

mapview(Brazil_cameras, zcol = c("year"),  burst = TRUE) # burst = TRUE prouce uniques

# Notas COL-005_Mag2016_WCS y COL-006-Mag2017_WCS tiene coordenadas invertidas


mapview(Colombia_cameras, zcol = c("year"), burst = TRUE) + 
  mapview(Peru_cameras, zcol = c("year"),  burst = TRUE) + 
  mapview(Bolivia_cameras, zcol = c("year"),  burst = TRUE) + 
  mapview(Venezuela_cameras, zcol = c("year"),  burst = TRUE) +
  mapview(Brazil_cameras, zcol = c("year"),  burst = TRUE)




########################
### ARGENTINA 
########################

# Identify file path string names
pais <- "C:/CodigoR/WCS_2024/camera_trap/data/Argentina/"
recIDs <- list.files(pais,  recursive = TRUE)#, pattern = ".csv")
i.strings <- paste0(pais, recIDs)

# make a list with all tables
# deployment_argentina<-lapply(i.strings, function(x) read_excel(x, sheet = "Deployment", col_names = TRUE))
# fix date problem
deployment_argentina <- list(read_excel("data/Argentina/ARG-001_Final_1MAR2016.xlsx", 
                                     sheet = "Deployment", col_types = c("numeric", 
                                                                         "text", "numeric", "numeric", "numeric", 
                                                                         "date", "date", "text", "text", "text", 
                                                                         "numeric", "text", "text", "numeric", 
                                                                         "text", "text")))
# extrae names... empty list
deployment_argentina1<-list()
# add file name at the end
for(i in 1:1) {
  origin<-rep(recIDs[i],nrow(deployment_argentina[[i]]))
  deployment_argentina1[[i]]<-cbind(deployment_argentina[[i]], origin)
  colnames(deployment_argentina1[[i]])[ncol(deployment_argentina)]<-"origin"
}
deployment_Argentina<- deployment_argentina1 %>%  do.call(rbind, .) # %>% write.csv(file=paste0("G:/Panama_Audubon/result/formated/pegadas/",

deployment_Argentina$year <- year(as.Date(deployment_Argentina$`Camera Deployment Begin Date`))

projlatlon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
Argentina_cameras <- st_as_sf(x = deployment_Argentina,                         
                           coords = c("Longitude Resolution", "Latitude Resolution"),
                           crs = projlatlon)

mapview(Argentina_cameras, zcol = c("year"),  burst = TRUE) # burst = TRUE prouce uniques

# Notas COL-005_Mag2016_WCS y COL-006-Mag2017_WCS tiene coordenadas invertidas


mapview(Colombia_cameras, zcol = c("year"), burst = TRUE) + 
  mapview(Peru_cameras, zcol = c("year"),  burst = TRUE) + 
  mapview(Bolivia_cameras, zcol = c("year"),  burst = TRUE) + 
  mapview(Venezuela_cameras, zcol = c("year"),  burst = TRUE) +
  mapview(Brazil_cameras, zcol = c("year"),  burst = TRUE) + 
  mapview(Argentina_cameras, zcol = c("year"),  burst = TRUE)


########################
### PARAGUAY 
########################

# Identify file path string names
pais <- "C:/CodigoR/WCS_2024/camera_trap/data/Paraguay/"
recIDs <- list.files(pais,  recursive = TRUE)#, pattern = ".csv")
i.strings <- paste0(pais, recIDs)

# make a list with all tables
deployment_paraguay<-lapply(i.strings, function(x) read_excel(x, sheet = "Deployment", col_names = TRUE))
# extrae names... empty list
deployment_paraguay1<-list()
# add file name at the end
for(i in 1:1) {
  origin<-rep(recIDs[i],nrow(deployment_paraguay[[i]]))
  deployment_paraguay1[[i]]<-cbind(deployment_paraguay[[i]], origin)
  colnames(deployment_paraguay1[[i]])[ncol(deployment_paraguay)]<-"origin"
}
deployment_Paraguay<- deployment_paraguay1 %>%  do.call(rbind, .) # %>% write.csv(file=paste0("G:/Panama_Audubon/result/formated/pegadas/",

deployment_Paraguay$year <- year(as.Date(deployment_Paraguay$`Camera Deployment Begin Date`))

projlatlon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
Paraguay_cameras <- st_as_sf(x = deployment_Paraguay,                         
                              coords = c("Longitude Resolution", "Latitude Resolution"),
                              crs = projlatlon)

mapview(Paraguay_cameras, zcol = c("year"),  burst = TRUE) # burst = TRUE prouce uniques

# Notas COL-005_Mag2016_WCS y COL-006-Mag2017_WCS tiene coordenadas invertidas


mapview(Colombia_cameras, zcol = c("year"), burst = TRUE) + 
  mapview(Argentina_cameras, zcol = c("year"),  burst = TRUE) +
  mapview(Paraguay_cameras, zcol = c("year"),  burst = TRUE) +
  mapview(Peru_cameras, zcol = c("year"),  burst = TRUE) + 
  mapview(Bolivia_cameras, zcol = c("year"),  burst = TRUE) + 
  mapview(Venezuela_cameras, zcol = c("year"),  burst = TRUE) +
  mapview(Brazil_cameras, zcol = c("year"),  burst = TRUE)  
  
#### join tables 
part_cameras <- rbind(Colombia_cameras, Paraguay_cameras, #, 
                      Peru_cameras, Venezuela_cameras,
                      Brazil_cameras, Argentina_cameras)

### very dirty fix for dates cameras
Bolivia_cameras$`Camera Deployment Begin Date` <- as.Date(Bolivia_cameras$`Camera Deployment Begin Date`)
part_cameras$`Camera Deployment Begin Date` <- as.Date (part_cameras$`Camera Deployment Begin Date`)
part_cameras$`Camera Deployment End Date` <- as.Date (part_cameras$`Camera Deployment End Date`)

all_cameras <- rbind(part_cameras,Bolivia_cameras)

mapview(all_cameras, zcol = c("year"), burst = TRUE)

###################
### Tmap
###################

library(tmap)
tm_shape(World) +
  tm_borders()

SAmerica <-  World[World$continent == "South America", ]

tm_basemap("Esri.WorldImagery", alpha = 0.5) + 
  tm_shape(SAmerica) + tm_borders() + 
  # tm_facets("sovereignt",  free.coords = FALSE)+
  tm_shape(all_cameras) + #tm_facets_grid("origin") +
    tm_symbols(size = 0.1, col = "red", col_alpha=0.5, fill_alpha=0.5) 
  
  






