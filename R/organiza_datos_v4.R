# helper functions
# function to make matrix per species per day, extracting and averaging hour 
# Modified from CI-TEAM Network code by Diego Lizcano
# July 2024
# https://github.com/dlizcano/WCS-CameraTrap/tree/main/R

require(camtrapR) # new in v2

###################################
# Creates a nested list by species.
###################################
# One list is presence (detection history) another list is hour to make hour of detection as a covariate

dev.det_history.creator<-function(data){
  require(lubridate)
  require(hms)
  #results object
  res <- list()
  
  #get the dimensions of the matrix
  
  #list if sampling units
  cams<-unique(data$locationID)
  cams<-sort(cams)
  rows<-length(cams)
  species<-unique(data$scientificName)
  # make some equivalents
  data$eventDate <- as_date(data$`Date_Time_Captured`)
  data$time <- as_hms(ymd_hms(as_datetime(data$`Date_Time_Captured`)))
  
  # start and end dates of sampling periods
  # data<-data[data$Sampling.Period==year,]
  min<-min(as.Date(as.character(data$start_date), "%Y/%m/%d"))
  max<-max(as.Date(as.character(data$end_date), "%Y/%m/%d"))
  cols<-max-min+1
  
  #sampling period
  date.header<-seq(from=min,to=max, by="days")
  
  mat<-matrix(NA,rows,cols,dimnames=list(cams,as.character(date.header)))
  
  #for all cameras, determine the open and close date and mark in the matrix
  start.dates<-tapply(as.character(data$start_date),data$locationID,unique)
  nms<-names(start.dates)
  # start.dates<-ymd(start.dates)
  names(start.dates)<-nms
  end.dates<-tapply(as.character(data$end_date),data$locationID,unique)
  # end.dates<-ymd(end.dates)
  names(end.dates)<-nms
  
  #outline the sampling periods for each camera j
  for(j in 1:length(start.dates)){
    #for each camera beginning and end of sampling
    low<-which(date.header==min(start.dates[[j]])) # ojo es una lista
    hi<-which(date.header==max(end.dates[[j]])) # ojo es una lista de varios
    if(length(low)+length(hi)>0){
      indx<-seq(from=low,to=hi)
      mat[names(start.dates)[j],indx]<- 0
    } else next
  }
  mat.template<-mat 
  mat_h <- mat
  #get the species
  #species<-unique(data$bin)
  #construct the matrix for each species i
  for(i in 1:length(species)){
    indx<-which(data$scientificName==species[i])
    #dates and cameras when/where the species was photographed
    dates<-data$eventDate[indx]
    cameras<-data$locationID[indx]
    hora <- data$time[indx]
    
    dates.cameras <- data.frame(dates, cameras)
    hours.cameras <- data.frame(hora, cameras)
    #unique combination of dates and cameras 
    dates.cameras<-unique(dates.cameras)
    hours.cameras <- unique(hours.cameras)
    #fill in the obs matrix
    for(j in 1:length(indx)){
      col<-which(date.header==as.character( dates.cameras[j,1]))
      row<-which(cams==as.character( dates.cameras[j,2]))
      mat[row,col]<-1 # here puts 1
    }
    mat.nas<-is.na(mat)
    sum.nas<-apply(mat.nas,2,sum)
    indx.nas<-which(sum.nas==rows)
    if(length(indx.nas)>0){
      mat<-mat[,-indx.nas]
    }
    
    
    #fill in the hour matrix
    for(j in 1:length(dates.cameras[,1])){
      col<-which(date.header==as.character( dates.cameras[j,1]))
      row<-which(cams==as.character( dates.cameras[j,2]))
      mat_h[row,col]<- as.character( hours.cameras[j,1]) # add date
    }
    mat_h.nas<-is.na(mat)
    sum.nas<-apply(mat_h.nas,2,mean)# instead of sum
    indx.nas<-which(sum.nas==rows)
    if(length(indx.nas)>0){
      mat_h<-mat_h[,-indx.nas]
     }
    
    
    res<-c(res,list(mat))
    occur<-c(res,list(mat)) #lista anidada
    dete <- c(dete,list(mat_h))
    
    # res<-list(occur=list(mat), hora=list(mat_h), sp=list(species)) #lista anidada
    #return the matrix to its original form
    mat<-mat.template
    
  }
  
  # names(occur)<-species
  names(dete)<-species
  #res<-lapply(res,f.dum)
  return(list(occur=occur, dete=dete))
  
}

########################################


#code to shrink the matrix to exactly 9 columns: collapsing by 3 days
f.shrink.matrix.to9<-function(matrix){
  nc<-dim(matrix)[2]
  if(!nc%%9){ # of the number of columns is exactly divisible by 9
    newc<-nc%/%9
    old.cols<-seq(1,nc,newc)
    new.matrix<-matrix(NA,nr=nrow(matrix),nc=9)
    for(i in 1:9){
      new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,max,na.rm=T)
    }
  } else{
    rem<-nc%%9
    newc<-nc%/%9
    old.cols<-seq(1,nc-rem,newc)
    new.matrix<-matrix(NA,nr=nrow(matrix),nc=9)
    for(i in 1:8) # notice here is one less
      new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,max,na.rm=T)
    new.matrix[,9]<-apply(matrix[,old.cols[9]:nc],1,max,na.rm=T) 
  }
  new.matrix[new.matrix=="-Inf"]<-NA
  rownames(new.matrix)<-rownames(matrix)
  new.matrix
}


#code to shrink the hour matrix to exactly 9 columns: collapsing by 3 days
f.shrink.matrix.h.to9<-function(matrix){
  nc<-dim(matrix)[2]
  if(!nc%%9){ # of the number of columns is exactly divisible by 9
    newc<-nc%/%9
    old.cols<-seq(1,nc,newc)
    new.matrix<-matrix(NA,nr=nrow(matrix),nc=9)
    for(i in 1:9){
      new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,mean,na.rm=T)
    }
  } else{
    rem<-nc%%9
    newc<-nc%/%9
    old.cols<-seq(1,nc-rem,newc)
    new.matrix<-matrix(NA,nr=nrow(matrix),nc=9)
    for(i in 1:8)
      new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,mean,na.rm=T)
    new.matrix[,9]<-apply(matrix[,old.cols[9]:nc],1,mean,na.rm=T) 
  }
  new.matrix[new.matrix=="-Inf"]<-NA
  rownames(new.matrix)<-rownames(matrix)
  new.matrix
}

####################################################
## load a single WCS excel file to make a dataframe 
## linking: cameras, deployment and image
####################################################

loadproject <- function(path_to_file){
  require(readxl)
  require(dplyr)
  require(tidyr)
  
  project <- read_excel(path_to_file, 
                        sheet = "Project", range = "M1:M2")
  
  cameras <- read_excel(path_to_file, 
                        sheet = "Cameras") |> 
    dplyr::rename("Camera_Id"= "Camera id") |> 
    dplyr::rename("Year_Purchased"= "Year Purchased") 
  
  cam_in_cameras <- length(unique(cameras$Camera_Id))
  
  deployment <- read_excel(path_to_file, 
                        sheet = "Deployment") |> select(!c(ID)) |> 
    dplyr::rename("Longitude" = "Longitude Resolution") |> 
    dplyr::rename("Latitude" = "Latitude Resolution") |> 
    dplyr::rename("start_date" = "Camera Deployment Begin Date") |> 
    dplyr::rename("end_date" = "Camera Deployment End Date") |> 
    dplyr::rename("Deployment_ID" = "Deployment ID") |> 
    dplyr::rename("Camera_Id" = "Camera Id") |>
    mutate(locationID = Point)
    
  cam_in_deployment <- length(unique(deployment$Camera_Id))
  deployments_in_deployment<- length(unique(deployment$Deployment_ID))
  point_in_deployment <- length(unique(deployment$locationID))
  
  image <- read_excel(path_to_file, skip = 1,
                        sheet = "Image") |> 
    select(!c(ID, Location, "IUCN Identification Number", "Animal recognizable", "individual Animal notes"  )) |> 
    dplyr::rename("scientificName" = "Genus Species") |> 
    dplyr::rename("Date_Time_Captured" = "Date_Time Captured") |> 
    dplyr::rename("Deployment_ID" = "Deployment ID") 
  
  cam_in_image <- length(unique(image$Deployment_ID))
  point_in_image <- length(unique(image$Point))
  
  cat(paste(cam_in_cameras, "cameras in Cameras. \n", 
            cam_in_deployment, "cameras in Deployment. \n",
            deployments_in_deployment, "deployments in Deployment. \n",
            point_in_deployment, "points in Deployment. \n",
            cam_in_image, "cameras in Images. \n",
            point_in_image, "points in Images. \n"))
  
  data1 <-  cameras |> left_join(deployment) # join first two
  by <- join_by("Deployment_ID") 
  data <- left_join(data1, image, by) # join by "Deployment ID"
  
  #### add Jauar_Design and yr
  data$Jaguar_Design <- project$Jaguar_Design
  #### remove NONES 
  ind <- which(data$Date_Time_Captured=="NONE")
  if(class(ind)=="integer"){print("dates Ok")}else(data <- data[-c(ind),])
  #data <- data[-c(ind),]
  data$Date_Time_Captured <- as.POSIXlt(data$Date_Time_Captured, format="%Y/%m/%d %H:%M:%S") #as.Date
  # remove NA in hour
  # remove NA in Date_Time_Captured
  data <- data %>% drop_na(Date_Time_Captured)
  data$year <- lubridate::year(data$Date_Time_Captured) 
  # remove NA in sp
  # data <- data %>% drop_na(scientificName)

  # remove blank in sp
  # ind <- which(data$scientificName=="Blank")
  # data <- data[-c(ind),]
  
  
  cat(paste("year:",unique(data$year), "\n",
              "Jaguar_Design:", unique(data$Jaguar_Design)),"\n") #Just to check
  
  return (data)
  
} # end of loadproject


#################################
# get the sites to add covariates
#################################
get.sites <- function(path_to_file){
  suppressWarnings({ # no warnings
  require(readxl)
  require(dplyr)
  require(sf)
  
  project <- read_excel(path_to_file, 
                           sheet = "Project", range = "M1:M2")
  ##############################
  # if Jaguar_Design == yes 
  ##############################
  if (project$Jaguar_Design == "yes"){
  
    cameras <- read_excel(path_to_file, 
                          sheet = "Cameras") |> 
      dplyr::rename("Camera_Id"= "Camera id") |> 
      dplyr::rename("Year_Purchased"= "Year Purchased") 
    
    deployment <- read_excel(path_to_file, 
                             sheet = "Deployment") |> select(!c(ID)) |> 
      dplyr::rename("Longitude" = "Longitude Resolution") |> 
      dplyr::rename("Latitude" = "Latitude Resolution") |> 
      dplyr::rename("start_date" = "Camera Deployment Begin Date") |> 
      dplyr::rename("end_date" = "Camera Deployment End Date") |> 
      dplyr::rename("bait"="Bait Description") |> 
      dplyr::rename("CamType"="Camera Type")  |> 
      dplyr::rename("Deployment_ID" = "Deployment ID") |> 
      dplyr::rename("Camera_Id"= "Camera Id")
    
    # extract covs
    bait <- deployment |> # can produce error if is a mix 
      distinct(Point, Longitude, Latitude, bait,
               season, rio_playa,	arroyo,	camino,	senda_animal,	senda_gente,	salitral,	pozo_agua,	bosque,	sabana,	intermedio,	intervalo_trigger) |> 
               # Deployment_ID )|> #, Camera_Id) |> 
      mutate(Point = as.character(Point))
    
    # add year selecting  the min yr per point
    year_purchased <- deployment |> left_join(cameras) #|> # as table
      # select("Year_Purchased") |> rename("year_Purchased" = "Year_Purchased")# join first two
    # by <- join_by("Camera_Id") 
    # datatemp <- left_join(deployment, cameras, by) # join by "Deployment ID"
    min_yr_purcha_by_pt <-  year_purchased |> group_by(Point) |> summarise(year_purchased=min(Year_Purchased)) |> ungroup()
    cam_type1 <-  year_purchased |> group_by(Point) |> summarise(camtype=first(CamType)) |> ungroup()
    
    
    # count camera models
    camtypes <- deployment |> mutate(site=Point) |> 
      distinct(Point, CamType) 
    CamTypes <-  as.data.frame(cbind(CamTypes=table(camtypes$Point),  Point=names(table(camtypes$Point))))
    
    CamTypes$Jaguar_Design <- project$Jaguar_Design
    a1 <- left_join(bait, CamTypes) |>  # add min year
      mutate(across(c(CamTypes), as.factor)) |> 
      mutate(Point= as.numeric(Point)) # convert Point to numeric to next join
    
    a <- left_join(a1, min_yr_purcha_by_pt) |> left_join(cam_type1) |>
    mutate(year_sampling=year(min(deployment$start_date))) # add sampling year
    
  }# close Jaguar_Design == yes 
  
  ##############################
  # if Jaguar_Design == no 
  ##############################
  if (project$Jaguar_Design == "no"){

    cameras <- read_excel(path_to_file, 
                          sheet = "Cameras") |> 
      dplyr::rename("Camera_Id"= "Camera id") |> 
      dplyr::rename("Year_Purchased"= "Year Purchased") 
    
    deployment <- read_excel(path_to_file, 
                             sheet = "Deployment") |> select(!c(ID)) |> 
      dplyr::rename("Longitude" = "Longitude Resolution") |> 
      dplyr::rename("Latitude" = "Latitude Resolution") |> 
      dplyr::rename("start_date" = "Camera Deployment Begin Date") |> 
      dplyr::rename("end_date" = "Camera Deployment End Date") |> 
      dplyr::rename("bait"="Bait Description") |> 
      dplyr::rename("CamType"="Camera Type")  |> 
      dplyr::rename("Deployment_ID" = "Deployment ID") |> 
      dplyr::rename("Camera_Id"= "Camera Id")
    
    # extract covs
    bait <- deployment |> # can produce error if is a mix 
      distinct(Point, Longitude, Latitude, bait,
               season, rio_playa,	arroyo,	camino,	senda_animal,	senda_gente,	salitral,	pozo_agua,	bosque,	sabana,	intermedio,	intervalo_trigger) |> 
      # Deployment_ID )|> #, Camera_Id) |> 
      mutate(Point = as.character(Point))
    
    # add year
    # add year selecting  the min yr per point
    year_purchased <- deployment |> left_join(cameras) |>  #|> # as table
    mutate(Point = as.character(Point))
    # select("Year_Purchased") |> rename("year_Purchased" = "Year_Purchased")# join first two
    # by <- join_by("Camera_Id") 
    # datatemp <- left_join(deployment, cameras, by) # join by "Deployment ID"
    
    # sometimes problematic if have two cameras per point
    min_yr_purcha_by_pt <-  year_purchased # |> group_by(Point) |> summarise(year_purchased=min(Year_Purchased)) |> ungroup()
    cam_type1 <-  year_purchased # |> group_by(Point) |> summarise(camtype=first(CamType)) |> ungroup()
    # remove extra columns
    
    
    # count camera models
    camtypes <- deployment |> mutate(site=as.character(Point)) |> 
          distinct(Point, CamType) 
    camtypes$Point <- as.character(camtypes$Point)
    # add Jaguar design
    camtypes$Jaguar_Design <- project$Jaguar_Design
    a1 <- left_join(bait, camtypes) |>  # add min year
      mutate(across(c(CamType), as.factor)) # |> # uncomment 4 Ecua
      # mutate(Point= as.numeric(Point)) # convert Point to numeric to next join
    b <- a1 |> left_join(min_yr_purcha_by_pt) |> left_join(cam_type1)
    # remove extra columns
    c <- b[,-c(19:32)]
    # add, reorder and remove cols to make it equal to yes
    a <- c |> dplyr::mutate (CamTypes=1) |> 
      dplyr::mutate (camtype=CamType) |> #b[,31] )  
      dplyr::mutate (year_purchased=Year_Purchased) |>
      dplyr::select (-c(CamType, Year_Purchased)) |>
      dplyr::relocate (camtype, .after = year_purchased) |>
      dplyr::relocate (CamTypes, .after = intervalo_trigger) |>
      mutate(year_sampling=year(min(deployment$start_date))) # add sampling year
    
  }# close Jaguar_Design == no 


    # make sf
    sites <- st_as_sf(a, coords = c("Longitude","Latitude"))   #crs="EPSG:4326")
    #--- set CRS ---#
    st_crs(sites) <- 4326
  
  return (sites)
  }) # end supress warning
} # end get.sites function



#############################################
### Historias de detección con fecha... may be a problem
#############################################

f.det_history.creator<-function(data){
  #results object
  res <- list()
  require(lubridate)
  require(hms)
  #get the dimensions of the matrix
  
  #list if sanpling units
  cams<-unique(data$locationID)
  cams<-sort(cams)
  rows<-length(cams)
  species<-unique(data$scientificName)
  # make some equivalents
  data$eventDate <- as_date(data$`Date_Time_Captured`)
  data$time <- as_hms(ymd_hms(as_datetime(data$`Date_Time_Captured`)))
  
  # start and end dates of sampling periods
  # data<-data[data$Sampling.Period==year,]
  min<-min(as.Date(as.character(data$start_date), "%Y/%m/%d"))
  max<-max(as.Date(as.character(data$end_date), "%Y/%m/%d"))
  cols<-max-min+1
  
  #sampling period
  date.header<-seq(from=min,to=max, by="days")
  mat<-matrix(NA,rows,cols,dimnames=list(cams,as.character(date.header)))
  
  #for all cameras, determine the open and close date and mark in the matrix
  start.dates<-as_date(tapply((data$start_date), data$locationID, unique))
  nms<-names(start.dates)
  # start.dates<-ymd(start.dates)
  names(start.dates)<-nms
  end.dates<-as_date(tapply((data$end_date),data$locationID,unique))
  # end.dates<-ymd(end.dates)
  names(end.dates)<-nms
  
  # CHK for error date
  # if(which(is.na(end.dates))>=1){print("Bad date format in Deployment")}
  # if(which(is.na(start.dates))>=1){print("Bad date format in Deployment")}
  
  
  #outline the sampling periods for each camera j
  for(j in 1:length(start.dates)){
    #for each camera beginning and end of sampling
    low<-which(date.header==min(start.dates[[j]])) # ojo es una lista
    hi<-which(date.header==max(end.dates[[j]])) # ojo es una lista de varios
    if(length(low)+length(hi)>0){
      indx<-seq(from=low,to=hi)
      mat[names(start.dates)[j],indx]<- 0
    } else next
  }
  mat.template<-mat
  #get the species
  #species<-unique(data$bin)
  #construct the matrix for each species i
  for(i in 1:length(species)){
    indx<-which(data$scientificName==species[i])
    #dates and cameras when/where the species was photographed
    dates<-data$eventDate[indx]
    cameras<-data$locationID[indx]
    dates.cameras<-data.frame(dates,cameras)
    #unique combination of dates and cameras 
    dates.cameras<-unique(dates.cameras)
    #fill in the matrix
    for(j in 1:length(dates.cameras[,1])){
      col<-which(date.header==as.character( dates.cameras[j,1]))
      row<-which(cams==as.character( dates.cameras[j,2]))
      mat[row,col]<-1
    }
    mat.nas<-is.na(mat)
    sum.nas<-apply(mat.nas,2,sum)
    indx.nas<-which(sum.nas==rows)
    if(length(indx.nas)>0){
      mat<-mat[,-indx.nas]
    }
    
    res<-c(res,list(mat))
    #return the matrix to its original form
    mat<-mat.template
  }
  
  names(res)<-species
  #res<-lapply(res,f.dum)
  res
  
}


#############################################
### Historias de deteccion sin importar anio
#############################################

wcs.det_history.creator<-function(data){
  #results object
  res<-list()
  require(lubridate)
  require(hms)
  require(tidyr)
  # require(hablar) # to get the max with NA
  #get the dimensions of the matrix
  
  # # Check if dates in image are in range of deployment
  # index_cams_id <- unique(data$Camera_Id)
  # fotomin <- data |> group_by(Camera_Id) |> summarize (minphoto=min(Date_Time_Captured))# |> min(Date_Time_Captured)
  # cam_deploy <- data |> group_by(Camera_Id) |> distinct(start_date)
  # 
  # for (i in 1:length(index_cams_id)) {
  #   fotomin <- data |> filter(Camera_Id == index_cams_id[i]) |> select(Date_Time_Captured)  
  #   minc <- 
  # }
  # 
  
  #list if sampling units
  cams<-unique(data$locationID)
  cams<-sort(cams)
  rows<-length(cams)
  species<-unique(data$scientificName)
  # make some equivalents
  data$eventDate <- as_date(data$`Date_Time_Captured`)
  data$time <- as_hms(ymd_hms(as_datetime(data$`Date_Time_Captured`)))
  
  # start and end dates of sampling periods
  # data<-data[data$Sampling.Period==year,]
  min<-min(as.Date(as.character(data$start_date), format = "%Y/%m/%d"))
  # max<-max(s(as.Date(as.character(data$time), "%Y/%m/%d"))) #max(s(data$column1))
  max<-max(as.Date(as.character(data$end_date), format = "%Y/%m/%d"))
  
  ### warning error date  ###
  if(is.na(min)==TRUE){ stop("dates as number in Deployment") } 
  if(is.na(max)==TRUE){ stop("dates as number in Deployment") } 
  
  cols<-max-min+1
  ### print number of days and sites ####
  cat(paste( format(cols), "of sampling effort. \n" , 
             rows, "sampling sites. \n" ,
             length(species), "species. \n"
            ))
  
  #sampling period
  date.header<-seq(from=min,to=max, by="days")
  mat<-matrix(NA,rows,cols,dimnames=list(cams,as.character(date.header)))
  
  #for all cameras, determine the open and close date and mark in the matrix
  start.dates<-tapply(as_date(data$start_date),data$locationID,unique)
  nms<-names(start.dates)
  # start.dates<-ymd(start.dates)
  names(start.dates)<-nms
  end.dates<-tapply(as_date(data$end_date),data$locationID,unique)
  # end.dates<-ymd(end.dates)
  names(end.dates)<-nms
  
  #outline the sampling periods for each camera j
  for(j in 1:length(start.dates)){
    #for each camera beginning and end of sampling
    low<-which(date.header==min(start.dates[[j]])) # ojo es una lista
    hi<-which(date.header==max(end.dates[[j]])) # ojo es una lista de varios
    if(length(low)+length(hi)>0){
      indx<-seq(from=low,to=hi)
      mat[names(start.dates)[j],indx]<- 0
    } else next
  }
  mat.template<-mat
  #get the species
  #species<-unique(data$bin)
  #construct the matrix for each species i
  for(i in 1:length(species)){
    indx<-which(data$scientificName==species[i])
    #dates and cameras when/where the species was photographed
    dates<-data$eventDate[indx]
    cameras<-data$locationID[indx]
    dates.cameras<-data.frame(dates,cameras)
    #unique combination of dates and cameras 
    dates.cameras<-unique(dates.cameras)
    #fill in the matrix
    for(j in 1:length(dates.cameras[,1])){
      col<-which(date.header==as.character( dates.cameras[j,1]))
      row<-which(cams==as.character( dates.cameras[j,2]))
      mat[row,col]<-1
    }
    mat.nas<-is.na(mat)
    sum.nas<-apply(mat.nas,2,sum)
    indx.nas<-which(sum.nas==rows)
    if(length(indx.nas)>0){
    #  mat<-mat[,-indx.nas] ##### OJO... Coment para Venezuela
    }
    ###################################
    ## get mat starting day 1
    ###################################
    # empty mat
      mat2<-matrix(NA,rows,cols)
      # transponse
      res2 <- mat |> t() 
      for (i in 1: rows){
        #get diference
        difere <- length(mat2[i,]) - length(na.omit(res2[,i]))
        mat2[i,] <- c(na.omit(res2[,i]), rep(NA,difere))
      }
    ##################################
    # res<-c(res,list(mat)) # original  with dates 
    # chk missing days
      miscam <- length(1:dim(mat)[2]) - dim(mat2)[1] 
     # if(miscam >=1){stop(paste(miscam, " missing days. Photos after pickup date")) } 
      
    colnames(mat2) <- c(1:dim(mat)[2]) # put column name 
    res<-c(res,list(mat2)) # modify no day starting day1
    #return the matrix to its original form
    mat<-mat.template
  }
  
  names(res)<-species
  #res<-lapply(res,f.dum)
  res
  
}




#############################################
### Historias de deteccion con fecha
#############################################

wcs.date_history.creator<-function(data){
  #results object
  res<-list()
  require(lubridate)
  require(hms)
  require(tidyr)
  # require(hablar) # to get the max with NA
  #get the dimensions of the matrix
  
  # # Check if dates in image are in range of deployment
  # index_cams_id <- unique(data$Camera_Id)
  # fotomin <- data |> group_by(Camera_Id) |> summarize (minphoto=min(Date_Time_Captured))# |> min(Date_Time_Captured)
  # cam_deploy <- data |> group_by(Camera_Id) |> distinct(start_date)
  # 
  # for (i in 1:length(index_cams_id)) {
  #   fotomin <- data |> filter(Camera_Id == index_cams_id[i]) |> select(Date_Time_Captured)  
  #   minc <- 
  # }
  # 
  
  #list if sampling units
  cams<-unique(data$locationID)
  cams<-sort(cams)
  rows<-length(cams)
  species<-unique(data$scientificName)
  # make some equivalents
  data$eventDate <- as_date(data$`Date_Time_Captured`)
  data$time <- as_hms(ymd_hms(as_datetime(data$`Date_Time_Captured`)))
  
  # start and end dates of sampling periods
  # data<-data[data$Sampling.Period==year,]
  min<-min(as.Date(as.character(data$start_date), "%Y/%m/%d"))
  # max<-max(s(as.Date(as.character(data$time), "%Y/%m/%d"))) #max(s(data$column1))
  max<-max(as.Date(as.character(data$end_date), "%Y/%m/%d"))
  
  ### warning error date  ###
  if(is.na(min)==TRUE){ stop("dates as number in Deployment") } 
  if(is.na(max)==TRUE){ stop("dates as number in Deployment") } 
  
  cols<-max-min+1
  ### print number of days and sites ####
  cat(paste( format(cols), "of sampling effort. \n" , 
             rows, "sampling sites. \n" ,
             length(species), "species. \n"
  ))
  
  #sampling period
  date.header<-seq(from=min,to=max, by="days")
  mat<-matrix(NA,rows,cols,dimnames=list(cams,as.character(date.header)))
  
  #for all cameras, determine the open and close date and mark in the matrix
  start.dates<-tapply(as_date(data$start_date),data$locationID,unique)
  nms<-names(start.dates)
  # start.dates<-ymd(start.dates)
  names(start.dates)<-nms
  end.dates<-tapply(as_date(data$end_date),data$locationID,unique)
  # end.dates<-ymd(end.dates)
  names(end.dates)<-nms
  
  #outline the sampling periods for each camera j
  for(j in 1:length(start.dates)){
    #for each camera beginning and end of sampling
    low<-which(date.header==min(start.dates[[j]])) # ojo es una lista
    hi<-which(date.header==max(end.dates[[j]])) # ojo es una lista de varios
    if(length(low)+length(hi)>0){
      indx<-seq(from=low,to=hi)
      mat[names(start.dates)[j],indx]<- 0
    } else next
  }
  mat.template<-mat
  #get the species
  #species<-unique(data$bin)
  #construct the matrix for each species i
  for(i in 1:length(species)){
    indx<-which(data$scientificName==species[i])
    #dates and cameras when/where the species was photographed
    dates<-data$eventDate[indx]
    cameras<-data$locationID[indx]
    dates.cameras<-data.frame(dates,cameras)
    #unique combination of dates and cameras 
    dates.cameras<-unique(dates.cameras)
    #fill in the matrix
    for(j in 1:length(dates.cameras[,1])){
      col<-which(date.header==as.character( dates.cameras[j,1]))
      row<-which(cams==as.character( dates.cameras[j,2]))
      
      mat[row,col]<- colnames(mat)[col] # here put 1
      
    }
    mat.nas<-is.na(mat)
    sum.nas<-apply(mat.nas,2,sum)
    indx.nas<-which(sum.nas==rows)
    if(length(indx.nas)>0){
      #  mat<-mat[,-indx.nas] ##### OJO... Coment para Venezuela
    }
    ###################################
    ## get mat starting day 1
    ###################################
    # empty mat
    mat2<-matrix(NA,rows,cols)
    # transponse
    res2 <- mat |> t() 
    for (i in 1: rows){
      #get diference
      difere <- length(mat2[i,]) - length(na.omit(res2[,i]))
      mat2[i,] <- c(na.omit(res2[,i]), rep(NA,difere))
    }
    ##################################
    # res<-c(res,list(mat)) # original  with dates 
    # chk missing days
    miscam <- length(1:dim(mat)[2]) - dim(mat2)[1] 
    # if(miscam >=1){stop(paste(miscam, " missing days. Photos after pickup date")) } 
    
    colnames(mat2) <- c(1:dim(mat)[2]) # put column name 
    res<-c(res,list(mat2)) # modify no day starting day1
    #return the matrix to its original form
    mat<-mat.template
  }
  
  names(res)<-species
  #res<-lapply(res,f.dum)
  res
  
}







##############################################
### Function to read by country
### Notice only reads deployment and image
##############################################
data_by_country <- function(path_to_files, country="Argentina"){
  # Identify file path string names
  
  pais <- paste(path_to_files, country, sep="/")
  recIDs <- list.files(pais,  recursive = FALSE, pattern = ".xlsx")
  i.strings <- paste0(pais, "/", recIDs, sep="")
  
  # make a list with all tables
  deployment_pais<-lapply(i.strings, function(x) read_excel(x, sheet = "Deployment", # including  Robs new columns
                                                            col_types = c("numeric", 
                                                                          "text", "text", "numeric", "numeric", 
                                                                          "text", "text", "text", "text", "text", 
                                                                          "text", "text", "text", "text", "text", 
                                                                          "text", "text", "text", "text", "text", 
                                                                          "text", "text", "text", "text", "text",
                                                                          "text", "text", "text"), col_names = TRUE))
  
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
  
  ## get the Project id
  
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
  
}

####################################################
# code to collapse the WCS matrix from 40 days to 7 days 
####################################################
f.collapse.matrix.to6<-function(matrix){
  nc<-dim(matrix)[2]
  if(!nc%%6){ # of the number of columns is exactly divisible by 6
    newc<-nc%/%6
    old.cols<-seq(1,nc,newc)
    new.matrix<-matrix(NA,nr=nrow(matrix),nc=6)
    for(i in 1:6){
      new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,max,na.rm=T)
    }
  } else{
    rem<-nc%%6
    newc<-nc%/%6
    old.cols<-seq(1,nc-rem,newc)
    new.matrix<-matrix(NA,nr=nrow(matrix),nc=6)
    for(i in 1:5) # notice here is one less
      new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,max,na.rm=T)
    new.matrix[,6]<-apply(matrix[,old.cols[6]:nc],1,max,na.rm=T) 
  }
  new.matrix[new.matrix=="-Inf"]<-NA
  rownames(new.matrix)<-rownames(matrix)
  new.matrix
}
