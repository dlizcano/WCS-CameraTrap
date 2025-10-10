

#### read packages

library(readr)
library(unmarked)
library(terra)
library(mapview)
library(sf)
library(stars)
library(elevatr)
library(ubms)

# load custom functions
# Adjust path to the file (organiza_datos_v4) in your hard disk
source("F:/WCS-CameraTrap/R/organiza_datos_v4.R")

# load data
Puma_Regional <- read_delim("E:/detection_Puma_Bolivia_Ecuador_Paraguay_Peru_Guatemala_Venezuela.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

y_reg <- Puma_Regional[,26:67] # select sampling occasions to 42 days

# Count zeros in each row
row_zeros_1 <- apply(as.matrix(y_reg), 1, function(x) sum(x == 0, na.rm = TRUE))
ind <- which(row_zeros_1<=10) # which are less than 10 days
# remove cameras with less than 10 days
y_reg2 <- y_reg[-ind,]
Puma_Regional2 <- Puma_Regional[-ind, ]

# collapse to seven days
y_reg3 <- f.collapse.matrix.to6(y_reg2)


# remove lees than two sampling occasions
row_zeros_2 <- apply(as.matrix(y_reg3), 1, function(x) sum(x == 0, na.rm = TRUE))
ind2 <- which(row_zeros_2<=2) # which are less than 2 sampling occasions
y_reg3 <- y_reg3[-ind2,]
Puma_Regional3 <- Puma_Regional2[-ind2, ]


#load raster
per_tree_cov <- rast("F:/WCS-CameraTrap/raster/latlon/Veg_Cont_Fields_Yearly_250m_v61/Perc_TreeCov/MOD44B_Perc_TreeCov_2010_065.tif")
road_den <- rast("F:/WCS-CameraTrap/raster/latlon/RoadDensity/grip4_total_dens_m_km2.asc")
# elev <- rast("F:/WCS-CameraTrap/raster/latlon/elevation_z7.tif")
landcov <- rast("F:/WCS-CameraTrap/raster/latlon/LandCover_Type_Yearly_500m_v61/LC1/MCD12Q1_LC1_2010_001.tif") 
cattle <- rast("F:/WCS-CameraTrap/raster/latlon/Global cattle distribution/5_Ct_2010_Da.tif")
#river <- st_read("F:/WCS-CameraTrap/shp/DensidadRios/MCD12Q1_LC1_2001_001_RECLASS_MASK_GRID_3600m_DensDrenSouthAmer.shp")


#Puma_Regional <- read_csv("G:/detections_Puma_Bolivia_Venezuela_Peru_Guatemala_Paraguay_Ecuador.csv")
#y_reg <- Puma_Regional[,9:58]

#Puma_Bolivia <- detections_Puma_Bolivia <- read_csv("F:/BDcorregidas/Bolivia/Guido/Puma/detections_Puma_Bolivia.csv")
# y_reg <- Puma_Regional[,27:86]

# fix one coord
# Puma_Regional3$longitude[2351] <- -73.182

# make sf
sites_puma <- st_as_sf(Puma_Regional3, coords = c("longitude","latitude"))   #crs="EPSG:4326")
#--- set CRS ---#
st_crs(sites_puma) <- 4326

######################
## get elevation map
######################

# elevation_detailed <- rast(get_elev_raster(sites_puma, z = 8, 
#                                             clip="bbox", 
#                                             neg_to_na=TRUE,
#                                             override_size_check = TRUE))
 
elevation_detailed <- get_elev_point (sites_puma, 
                                      src="aws", 
                                      z = 5, # 7=1.2km, 8=600m
                                      overwrite=TRUE)

# # Code to split into a loop and grab a point at a time.
# # This is usually faster for z>8 at points that are spread apart 
# # but it is a slow loop for many points
# elev <- vector("numeric", length = nrow(sites_puma))
# for(i in seq_along(sites_puma)){
#   elev[i]<-get_elev_point(locations = sites_puma[i,], 
#                           prj = 4326, 
#                           src = "aws", 
#                           z = 8)$elevation
#   }# end loop



# mts_elev <- cbind(sites_puma, elev)
# mts_elev

###################
### Put elevation
# eleva <- terra::extract(elev, sites_puma)
sites_puma$elevation <- elevation_detailed$elevation #eleva$file3be898018c3 # elevation_detailed$elevation
####################

##### Filter by 2000 meters
# ind4 <- which(sites_puma$elevation >= 2000)
# Puma_Regional4 <- Puma_Regional3[-ind4,]
# y_reg4 <- y_reg3[-ind4,]
# sites_puma <- sites_puma[-ind4,]

Puma_Regional4 <- Puma_Regional3
y_reg4 <- y_reg3

# extract covs using points and add to sites_puma
# covs <- cbind(sites_puma, terra::extract(SiteCovsRast, sites_puma))
per_tre <- terra::extract(per_tree_cov, sites_puma)
roads <- terra::extract(road_den, sites_puma)

land_cov <- terra::extract(landcov, sites_puma)
cattle_den <-  terra::extract(cattle, sites_puma)


# remove decimals convert to factor
sites_puma$land_cover <-  factor(land_cov$MCD12Q1_LC1_2010_001)
# sites_puma$elevation <-  eleva$file3be898018c3
sites_puma$per_tree_cov <- per_tre$MOD44B_Perc_TreeCov_2010_065 
#  fix 200 isue
ind <- which(sites_puma$per_tree_cov== 200)
sites_puma$per_tree_cov[ind] <- 0

#  fix No in  Jaguar_Design issue
# ind <- which(Puma_Regional$Jaguar_Design=="No")
# Puma_Regional$Jaguar_Design[ind] <- "no"


# put covs  
sites_puma$roads <- roads$grip4_total_dens_m_km2
sites_puma$cattle <- cattle_den[,2]




# camera_age
Puma_Regional4$camera_age <- Puma_Regional4$year_sampling - as.numeric(Puma_Regional4$year_purchased)
which(Puma_Regional4$camera_age<0)

# arrange detection observations

x4_bait <- cbind(as.data.frame(Puma_Regional4$bait), replicate(6, Puma_Regional4$bait))
# x4_bait2 <- cbind(as.data.frame(Puma_Regional4$bait2), replicate(6, Puma_Regional4$bait2))
x4_lat <- cbind(as.data.frame(Puma_Regional4$latitude ), replicate(6, Puma_Regional4$latitude))
x4_country <- cbind(as.data.frame(Puma_Regional4$Pais), replicate(6, Puma_Regional4$Pais))
x4_CamType <- cbind(as.data.frame(Puma_Regional4$cam_brand), replicate(6, Puma_Regional4$cam_brand))
x4_Jaguar_Design <- cbind(as.data.frame(Puma_Regional4$Jaguar_Design), replicate(6, Puma_Regional4$Jaguar_Design))
x4n_cameras <- cbind(as.data.frame((Puma_Regional4$CamTypes)), replicate(6, (Puma_Regional4$CamTypes)))
x4n_season <- cbind(as.data.frame(as.factor(Puma_Regional4$season)), replicate(6, as.factor(Puma_Regional4$season)))
x4n_rio_playa <- cbind(as.data.frame(as.factor(Puma_Regional4$rio_playa)), replicate(6, as.factor(Puma_Regional4$rio_playa)))
x4n_arroyo <- cbind(as.data.frame(as.factor(Puma_Regional4$arroyo)), replicate(6, as.factor(Puma_Regional4$arroyo)))
x4n_camino <- cbind(as.data.frame(as.factor(Puma_Regional4$camino)), replicate(6, as.factor(Puma_Regional4$camino)))
x4n_senda_animal <- cbind(as.data.frame(as.factor(Puma_Regional4$senda_animal)), replicate(6, as.factor(Puma_Regional4$senda_animal)))
x4n_senda_gente  <- cbind(as.data.frame(as.factor(Puma_Regional4$senda_gente)), replicate(6, as.factor(Puma_Regional4$senda_gente)))
x4n_salitral  <- cbind(as.data.frame(as.factor(Puma_Regional4$salitral)), replicate(6, as.factor(Puma_Regional4$salitral )))
x4n_pozo_agua  <- cbind(as.data.frame(as.factor(Puma_Regional4$pozo_agua)), replicate(6, as.factor(Puma_Regional4$pozo_agua )))
x4n_bosque  <- cbind(as.data.frame(as.factor(Puma_Regional4$bosque)), replicate(6, as.factor(Puma_Regional4$bosque )))
x4n_sabana  <- cbind(as.data.frame(as.factor(Puma_Regional4$sabana )), replicate(6, as.factor(Puma_Regional4$sabana )))
x4n_intermedio  <- cbind(as.data.frame(as.factor(Puma_Regional4$intermedio)), replicate(6, as.factor(Puma_Regional4$intermedio )))
x4n_camera_age  <- cbind(as.data.frame(as.numeric(Puma_Regional4$camera_age)), replicate(6, as.numeric(Puma_Regional4$camera_age )))
x4n_trigger_speed  <- cbind(as.data.frame(as.numeric(Puma_Regional4$trigger_speed)), replicate(6, as.numeric(Puma_Regional4$trigger_speed )))




x4_ObsCovs_list <- list(bait= as.data.frame(x4_bait[,2:7]),
                        #bait2= as.data.frame(x4_bait2[,2:7]),
                        CamTypes= as.data.frame(x4_CamType[,2:7]),
                        lat= as.data.frame(x4_lat[,2:7]),
                        country=as.data.frame(x4_country[,2:7]),
                        Jaguar_Design=as.data.frame(x4_Jaguar_Design[,2:7]),
                        n_cameras = as.data.frame(x4n_cameras[,2:7]),
                        season = as.data.frame(x4n_season[,2:7]),
                        rio_playa = as.data.frame(x4n_rio_playa[,2:7],),
                        arroyo = as.data.frame(x4n_arroyo[,2:7],),
                        camino = as.data.frame(x4n_camino[,2:7],),
                        senda_animal = as.data.frame(x4n_senda_animal[,2:7],),
                        senda_gente =as.data.frame(x4n_senda_gente[,2:7],),
                        salitral =as.data.frame(x4n_salitral[,2:7],),
                        pozo_agua =as.data.frame(x4n_pozo_agua[,2:7],),
                        bosque =as.data.frame(x4n_bosque[,2:7],),
                        sabana =as.data.frame(x4n_sabana[,2:7],),
                        intermedio =as.data.frame(x4n_intermedio[,2:7],),
                        camera_age =as.data.frame(x4n_camera_age[,2:7],),
                        trigger_speed=as.data.frame(x4n_trigger_speed[,2:7]) 
                        )

site_covs <- data.frame(#country=x4_country[,2], 
                        lat=x4_lat[,2], 
                        elevation=sites_puma$elevation,
                        land_cover=sites_puma$land_cover,
                        per_tree_cov=sites_puma$per_tree_cov,
                        #elevation=sites_puma$elevation,
                        road_den=sites_puma$roads,
                        cattle=sites_puma$cattle
                        )



# Make UMF object
umf <- unmarkedFrameOccu(y= y_reg4,
                         siteCovs = site_covs,
                         obsCovs = x4_ObsCovs_list)

# saveRDS(umf, "C:/CodigoR/WCS_2024/camera_trap/R/umf.rds")

plot(umf, main="Puma concolor")  



# fit unmarked models
fit_1 <- unmarked::occu(~1~1, data=umf) # ok!
fit_2 <- unmarked::occu(~scale(camera_age) ~1, data=umf) # It work!
fit_3 <- unmarked::occu(~factor(CamTypes) ~1, data=umf) # It work!
fit_4 <- unmarked::occu(~factor(Jaguar_Design)~1, data=umf) # It work
fit_5 <- unmarked::occu(~scale(n_cameras)~1, data=umf) # it work!
fit_6 <- unmarked::occu(~factor(rio_playa)~1, data=umf)# It work!
fit_7 <- unmarked::occu(~factor(arroyo)~1, data=umf)# It work!
fit_8 <- unmarked::occu(~factor(camino)~1, data=umf)# It work!
fit_9 <- unmarked::occu(~factor(senda_animal)~1, data=umf)# It work!
fit_10 <- unmarked::occu(~factor(senda_gente)~1, data=umf)# It work!
fit_11 <- unmarked::occu(~scale(trigger_speed)~1, data=umf)# It work!
fit_12 <- unmarked::occu(~factor(bait)~1, data=umf)# It work!



fit_21 <- unmarked::occu(~factor(camino)+factor(rio_playa)~1, data=umf)# It work!
fit_22 <- unmarked::occu(~factor(senda_animal)+factor(rio_playa)~1, data=umf)# It work!
fit_23 <- unmarked::occu(~factor(camino)+factor(senda_animal)~1, data=umf)# It work!
fit_24 <- unmarked::occu(~factor(camino)+factor(senda_animal)+factor(rio_playa)~1, data=umf)# It work!
fit_25 <- unmarked::occu(~factor(camino)+factor(senda_animal)+factor(rio_playa)+factor(Jaguar_Design)~1, data=umf)# It work!
#fit_26 <- unmarked::occu(~factor(camino)+factor(senda_animal)+factor(rio_playa)+factor(Jaguar_Design)~scale(elevation), data=umf)# It work!
fit_27 <- unmarked::occu(~factor(camino)+factor(CamTypes)~1, data=umf)# It work!
fit_28 <- unmarked::occu(~factor(camino)+factor(senda_gente)~1, data=umf)# It work!
fit_29 <- unmarked::occu(~factor(camino)+factor(bait)~1, data=umf)# It work!



# model names
# fit list for detection
fms1<-fitList("p(.) Ocu(.)"=            fit_1,
              "p(camino_SendaGente) Ocu(.)"=   fit_28, 
              "p(CamType) Ocu(.)"=      fit_3, #, 
              #"p(camino_senda_animal) Ocu(.)"=      fit_23, #, 
              "p(Jaguar_Design) Ocu(.)"=fit_4,#,
              "p(camino_rio_playa) Ocu(.)"=    fit_21,
              "p(rio_playa) Ocu(.)"    =fit_6,
              "p(bait) Ocu(.)"= fit_12,
              # "p(camino_senda_animal) Ocu(.)"= fit_24,
              #"p(camino_bait) Ocu(.)"= fit_29,
              "p(camino) Ocu(.)"=       fit_8,
              "p(senda_animal) Ocu(.)"= fit_9,
#             "p(camino_senda_animal_rio_playa_Jaguar_Design) Ocu(elevation)"= fit_26, #, 
              "p(senda_gente) Ocu(.)"=  fit_10)

# model selection detection unmarked
ms1<- modSel(fms1)
ms1



# Function returning the fit-statistics.
fitstats <- function(fm, na.rm=TRUE) {
  observed <- getY(fm@data)
  expected <- fitted(fm)
  # resids <- residuals(fm)
  # sse <- sum(resids^2, na.rm=na.rm)
  chisq <- sum((observed - expected)^2 / expected, na.rm=na.rm)
  # freeTuke <- sum((sqrt(observed) - sqrt(expected))^2, na.rm=na.rm)
  out <- c(Chisq=chisq) #c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
  return(out)
}

pb8 <- parboot(fit_8, fitstats, nsim=300, report=1)
pb8
plot(pb8, main="p(trigger_speed) Ocu(.)")

pb27 <- parboot(fit_27, fitstats, nsim=300, report=1)
pb27
plot(pb27, main="p(camino_CamTypes) Ocu(.)")

pb28 <- parboot(fit_28, fitstats, nsim=300, report=1)
pb28
plot(pb28, main="p(camino_Senda_Gente) Ocu(.)")

pb29 <- parboot(fit_29, fitstats, nsim=300, report=1)
pb29
plot(pb29, main="p(camino_Senda_Gente) Ocu(.)")


pb12 <- parboot(fit_12, fitstats, nsim=300, report=1)
pb12
plot(pb12, main="p(camino_Senda_Gente) Ocu(.)")

pb2 <- parboot(fit_2, fitstats, nsim=300, report=1)
pb2
plot(pb2, main="p(camera_age) Ocu(.)")

pb25 <- parboot(fit_25, fitstats, nsim=300, report=1)
pb25
plot(pb25, main="p(best_model) Ocu(.)")

pb24 <- parboot(fit_24, fitstats, nsim=150, report=1)
pb24
plot(pb24, main="p(best_model) Ocu(.)")


############################
# prediction using best model
###########################
# Create individual vectors
camino <- c("yes", "yes", "yes", "no")
senda_animal <- c("yes", "yes", "yes", "no")
rio_playa <- c("yes", "yes", "yes", "no")
Jaguar_Design <- c("yes", "yes", "yes", "no")

# Combine vectors into a data frame
newdata1 <- data.frame(camino = camino, senda_animal = senda_animal, rio_playa = rio_playa, Jaguar_Design=Jaguar_Design)

predict(fit_25, "det", newdata1 )


##########################################
## Bayesian model
##########################################

library(ubms)

# define number of iterations
itera=500

# fit stan (Bayesian models) using the package ubms
fit_stan_1_puma <- stan_occu(~1~1, data=umf, chains=4, iter=itera, cores=8)
fit_stan_2_puma <- stan_occu(~factor(bait)~1, data=umf, chains=4, iter=itera, cores=8)
fit_stan_2b_puma <- stan_occu(~factor(Jaguar_Design)~1, data=umf, chains=4, iter=itera, cores=8)
fit_stan_3_puma <- stan_occu(~factor(CamTypes)~1, data=umf, chains=4, iter=itera, cores=8)
fit_stan_3a_puma <- stan_occu(~factor(n_cameras)~1, data=umf, chains=4, iter=itera, cores=8)
fit_stan_4_puma <- stan_occu(~factor(rio_playa)~1, data=umf, chains=4, iter=itera, cores=8)
fit_stan_5_puma <- stan_occu(~factor(arroyo)~1, data=umf, chains=4, iter=itera, cores=8)
fit_stan_6_puma <- stan_occu(~factor(camino)~1, data=umf, chains=4, iter=itera, cores=8)
fit_stan_7_puma <- stan_occu(~factor(senda_animal)~1, data=umf, chains=4, iter=itera, cores=8)
fit_stan_8_puma <- stan_occu(~factor(senda_gente)~1, data=umf, chains=4, iter=itera, cores=8)
fit_stan_9_puma <- stan_occu(~factor(salitral)~1, data=umf, chains=4, iter=itera, cores=8)
fit_stan_10_puma <- stan_occu(~factor(pozo_agua)~1, data=umf, chains=4, iter=itera, cores=8)
fit_stan_11_puma <- stan_occu(~factor(bosque)~1, data=umf, chains=4, iter=itera, cores=8)
fit_stan_12_puma <- stan_occu(~factor(sabana)~1, data=umf, chains=4, iter=itera, cores=8)
fit_stan_13_puma <- stan_occu(~factor(intermedio)~1, data=umf, chains=4, iter=itera, cores=8)
fit_stan_14-puma <- stan_occu(~factor(country)~1, data=umf, chains=4, iter=itera, cores=8)
fit_stan_15_puma <- stan_occu(~scale(camera_age)~1, data=umf, chains=4, iter=itera, cores=8)
fit_stan_16_puma <- stan_occu(~factor(season)~1, data=umf, chains=4, iter=itera, cores=8)
fit_stan_17_puma <- stan_occu(~scale(trigger_speed)~1, data=umf, chains=4, iter=itera, cores=8)



# fit_stan_20 <- stan_occu(~factor(camino)+factor(senda_animal)~1, data=umf, chains=4, iter=itera, cores=8)
# fit_stan_21 <- stan_occu(~factor(camino)+factor(rio_playa)~1, data=umf, chains=4, iter=itera, cores=8)
# fit_stan_22 <- stan_occu(~factor(Jaguar_Design)+factor(camino)~1, data=umf, chains=4, iter=itera, cores=8)
# fit_stan_23 <- stan_occu(~factor(Jaguar_Design)+factor(bait)~1, data=umf, chains=4, iter=itera, cores=8)
# fit_stan_23b <- stan_occu(~factor(Jaguar_Design):factor(bait)~1, data=umf, chains=4, iter=itera, cores=8)
# fit_stan_24 <- stan_occu(~factor(Jaguar_Design)+factor(CamTypes)~1, data=umf, chains=4, iter=itera, cores=8)


# fit_stan_25 <- stan_occu(~factor(Jaguar_Design)+factor(camino)+factor(bait)+factor(rio_playa)~1, data=umf, chains=4, iter=itera, cores=8)


### Ocu

# fit_stan_5 <- stan_occu(~factor(country)~1, data=umf, chains=4, iter=itera, cores=8)
# fit_stan_6 <- stan_occu(~1~factor(country), data=umf, chains=4, iter=itera, cores=8)
# fit_stan_7 <- stan_occu(~1~scale(lat), data=umf, chains=4, iter=itera, cores=8)
# fit_stan_8 <- stan_occu(~1~scale(elevation), data=umf, chains=4, iter=itera, cores=8)
# fit_stan_9 <- stan_occu(~1~factor(land_cover), data=umf, chains=4, iter=itera, cores=8)
# fit_stan_10 <- stan_occu(~1~scale(per_tree_cov), data=umf, chains=4, iter=itera, cores=8)
# fit_stan_11 <- stan_occu(~1~scale(road_den), data=umf, chains=4, iter=itera, cores=8)
# fit_stan_12 <- stan_occu(~1~scale(cattle), data=umf, chains=4, iter=itera, cores=8)
# fit_stan_13 <- stan_occu(~1~scale(river), data=umf, chains=4, iter=itera, cores=8)
# 


# fit_stan_4 <- stan_occu(~1~scale(elevation), data=umf, chains=3, iter=itera, cores=3)
# fit_stan_5 <- stan_occu(~1~factor(land_cover), data=umf, chains=3, iter=itera, cores=3)
# fit_stan_6 <- stan_occu(~1~scale(per_tre_cover), data=umf, chains=3, iter=itera, cores=3)
# fit_stan_7 <- stan_occu(~1~scale(elevation) + factor(land_cover), data=umf, chains=3, iter=itera, cores=3)

# put name to the models
stan_mods <- fitList("p(.) Ocu(.)" =           fit_stan_1_puma,
                     "p(bait) Ocu(.)"=         fit_stan_2_puma,
                     "p(Jaguar_Design) Ocu(.)"=fit_stan_2b_puma,
                     "p(CamTypes) Ocu(.)"=     fit_stan_3_puma,
                     "p(n_cameras) Ocu(.)"=    fit_stan_3a_puma,
                     "p(rio_playa) Ocu(.)"=    fit_stan_4_puma, #,
                     "p(arroyo) Ocu(.)"=       fit_stan_5_puma,#, 
                     "p(camino) Ocu(.)"=       fit_stan_6_puma,
                     "p(senda_animal) Ocu(.)"= fit_stan_7_puma)#,
                     #"p(senda_gente) Ocu(.)"=    fit_stan_8
                     #"p(salitral) Ocu(.)"=   fit_stan_9#,
                     #"p(pozo_agua) Ocu(.)"= fit_stan_10,
                     #"p(bosque) Ocu(.)"=     fit_stan_11,#,
                     #"p(sabana) Ocu(.)"=       fit_stan_12,#,
                     #"p(intermedio) Ocu(.)"=       fit_stan_13
                     #"p(camino+rio_playa) Ocu(.)"=       fit_stan_20)#,
                     #"p(camera_age) Ocu(.)"=       fit_stan_15,
                     #"p(season) Ocu(.)"=    fit_stan_16
                     
                     )


# model selection 
# El modelo con el elpd más grande tuvo el mejor rendimiento
ms2 <- round(modSel(stan_mods), 3) #AGB_Spawn
ms2
# La columna elpd_diff muestra la diferencia en elpd entre un modelo y el modelo superior; Si esta diferencia es varias veces mayor que el error estándar de la diferencia (se_diff), estamos seguros de que el modelo con el elpd más grande tuvo un mejor desempeño.


### 
waic(fit_stan_2_puma)


########  Model Evaluation #########
# look at chains
# traceplot(fit_stan_4, pars=c("beta_state"))

# Evaluate model fit
# Statistic (p) should be near 0.5 if the model fits well.
# fit_top_gof <- ubms::gof(fit_stan_15, draws=100, quiet=TRUE)
# fit_top_gof
# plot(fit_top_gof)

# simulate new datasets
sim_y <- posterior_predict(fit_stan_3, "y", draws=100)
#dim(sim_y)
prop0 <- apply(sim_y, 1, function(x) mean(x==0, na.rm=TRUE))
# compare that to the proportion of zeros in the actual dataset.
actual_prop0 <- mean(getY(fit_stan_3) == 0, na.rm=TRUE)
#Compare
hist(prop0, col='gray')
abline(v=actual_prop0, col='red', lwd=2)





(fit_top_gof <- gof(fit_stan_2_puma, draws=100, quiet=TRUE))
plot(fit_top_gof)

library(ggplot2)

# (fit_stan_2)
ubms::plot_effects(fit_stan_2_puma, "det") + ggtitle("bait")# Detection
ubms::plot_effects(fit_stan_2b_puma, "det") + ggtitle("Jaguar Design")# Detection
ubms::plot_effects(fit_stan_3_puma, "det") + ggtitle("Camera Type")# Detection
ubms::plot_effects(fit_stan_4_puma, "det") + ggtitle("Rio Playa")# Detection
ubms::plot_effects(fit_stan_5_puma, "det") + ggtitle("Arroyo")# Detection
ubms::plot_effects(fit_stan_6_puma, "det") + ggtitle("Camino")# Detection
ubms::plot_effects(fit_stan_7_puma, "det") + ggtitle("Senda Animal")# Detection
ubms::plot_effects(fit_stan_8_puma, "det") + ggtitle("Senda Gente")# Detection
ubms::plot_effects(fit_stan_9_puma, "det") + ggtitle("Salitral")# Detection
ubms::plot_effects(fit_stan_10_puma, "det") + ggtitle("Pozo Agua")# Detection
ubms::plot_effects(fit_stan_11_puma, "det") + ggtitle("Bosque")# Detection
ubms::plot_effects(fit_stan_12_puma, "det") + ggtitle("Sabana")# Detection
ubms::plot_effects(fit_stan_13_puma, "det") + ggtitle("Intermedio")# Detection
ubms::plot_effects(fit_stan_14_puma, "det") + ggtitle("Pais")# Detection
ubms::plot_effects(fit_stan_15_puma, "det") + ggtitle("Camera Age") # Detection
ubms::plot_effects(fit_stan_16_puma, "det") + ggtitle("Season") # Detection
ubms::plot_effects(fit_stan_17_puma, "det") + ggtitle("Trigger Speed") # Detection



ubms::plot_effects(fit_stan_9_puma, "state") + ggtitle("Land cover") # Occu
ubms::plot_effects(fit_stan_10-puma, "state") + ggtitle("Percent tree cover") # Occu
ubms::plot_effects(fit_stan_11_puma, "state") + ggtitle("Road density") # Occu
ubms::plot_effects(fit_stan_12_puma, "state") + ggtitle("Cattle density") # Occu
ubms::plot_effects(fit_stan_13_puma, "state") + ggtitle("River") # Occu


# 
# plot_effects(fit_stan_5, "det") # Detection
# plot_effects(fit_stan_6, "state") # Occupancy
# plot_effects(fit_stan_7, "state") # Occupancy
# plot_effects(fit_stan_8, "state") # Occupancy
# plot_effects(fit_stan_9, "state") # Occupancy
# plot_effects(fit_stan_10, "state") # Occupancy
# plot_effects(fit_stan_11, "state") # Occupancy
# plot_effects(fit_stan_12, "state") # Occupancy



