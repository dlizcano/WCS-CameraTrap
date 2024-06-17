

#### read packages

library(readr)
library(unmarked)
library(terra)
library(mapview)
library(sf)
library(stars)
library(elevatr)
library(ubms)


#load raster
per_tree_cov <- rast("E:/WCS-CameraTrap/raster/latlon/Veg_Cont_Fields_Yearly_250m_v61/Perc_TreeCov/MOD44B_Perc_TreeCov_2010_065.tif")
road_den <- rast("E:/WCS-CameraTrap/raster/latlon/RoadDensity/grip4_total_dens_m_km2.asc")
elev <- rast("D:/CORREGIDAS/elevation_z7.tif")
landcov <- rast("E:/WCS-CameraTrap/raster/latlon/LandCover_Type_Yearly_500m_v61/LC1/MCD12Q1_LC1_2010_001.tif") 
cattle <- rast("E:/WCS-CameraTrap/raster/latlon/Global cattle distribution/5_Ct_2010_Da.tif")

#load shp
river <- st_read("E:/WCS-CameraTrap/shp/DensidadRios/MCD12Q1_LC1_2001_001_RECLASS_MASK_GRID_3600m_DensDrenSouthAmer.shp")


Puma_Regional <- read_csv("D:/CORREGIDAS/PumaRegional50def_v2.csv")
y_reg <- Puma_Regional[,9:58]


# make sf
sites <- st_as_sf(Puma_Regional, coords = c("Longitud","Latitud"))   #crs="EPSG:4326")
#--- set CRS ---#
st_crs(sites) <- 4326

# get elevation map
# elevation_detailed <- rast(get_elev_raster(sites, z = 9))


# extract covs using points and add to sites
# covs <- cbind(sites, terra::extract(SiteCovsRast, sites))
per_tre <- terra::extract(per_tree_cov, sites)
roads <- terra::extract(road_den, sites)
eleva <- terra::extract(elev, sites)
land_cov <- terra::extract(landcov, sites)
cattle_den <-  terra::extract(cattle, sites)


# remove decimals convert to factor
sites$land_cover <-  factor(land_cov$MCD12Q1_LC1_2010_001)
sites$elevation <-  eleva$file3be898018c3
sites$per_tree_cov <- per_tre$MOD44B_Perc_TreeCov_2010_065
sites$elevation <- eleva$file3be898018c3
sites$roads <- roads$grip4_total_dens_m_km2
sites$cattle <- cattle_den[,2]

#arrange detections observations

x4_bait <- cbind(as.data.frame(Puma_Regional$bait), replicate(60, Puma_Regional$bait))
x4_lat <- cbind(as.data.frame(Puma_Regional$Latitud), replicate(60, Puma_Regional$Latitud))
x4_country <- cbind(as.data.frame(Puma_Regional$Country), replicate(60, Puma_Regional$Country))
x4_CamType <- cbind(as.data.frame(Puma_Regional$CamType), replicate(60, Puma_Regional$CamType))
x4_ObsCovs_list <- list(bait= as.data.frame(x4_bait[,2:51]),
                        CamTypes= as.data.frame(x4_CamType[,2:51]),
                        lat= as.data.frame(x4_lat[,2:51]),
                        country=as.data.frame(x4_country[,2:51]))

site_covs <- data.frame(country=x4_country[,2], 
                        lat=x4_lat[,2], 
                        elevation=sites$elevation,
                        land_cover=sites$land_cover,
                        per_tree_cov=sites$per_tree_cov,
                        elevation=sites$elevation,
                        road_den=sites$roads,
                        cattle=sites$cattle
                        )



# Make UMF object
umf <- unmarkedFrameOccu(y= y_reg,
                         siteCovs = site_covs,
                         obsCovs = x4_ObsCovs_list)

# saveRDS(umf, "C:/CodigoR/WCS_2024/camera_trap/R/umf.rds")

plot(umf, main="Puma")  



# fit unmarked models
fit_1 <- unmarked::occu(~1~1, data=umf) # ok!
fit_2 <- unmarked::occu(~factor(bait) ~1, data=umf) # It work!
fit_3 <- unmarked::occu(~factor(CamTypes) ~1, data=umf) # It work!
fit_4 <- unmarked::occu(~scale(lat)~1, data=umf) # It work, but warnings
fit_5 <- unmarked::occu(~factor(country)~1, data=umf) # loong time!
# fit_6 <- unmarked::occu(~1~scale(per_tre_cover), data=umf)# It work!


# model names
# fit list for detection
fms1<-fitList("p(.) Ocu(.)"=            fit_1,
              "p(bait) Ocu(.)"=         fit_2)#, 
#              "p(CamType) Ocu(.)"=      fit_3) #, 
#              "p(.) Ocu(elevation)"=    fit_4,
#              "p(.) Ocu(land_cover)"=   fit_5,
#              "p(.) Ocu(per_tre_cover)"=fit_6)

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

pb <- parboot(fit_2, fitstats, nsim=300, report=1)
plot(pb, main="p(CamType) Ocu(.)")



library(ubms)

# define number of iterations
itera=500

# fit stan (Bayesian models) using the package ubms
fit_stan_1P <- stan_occu(~1~1, data=umf, chains=3, iter=itera, cores=12)
fit_stan_2P <- stan_occu(~factor(bait)~1, data=umf, chains=3, iter=itera, cores=12)
fit_stan_3P <- stan_occu(~factor(CamTypes)~1, data=umf, chains=3, iter=itera, cores=12)
fit_stan_4P <- stan_occu(~scale(lat)~1, data=umf, chains=3, iter=itera, cores=12)
fit_stan_5P <- stan_occu(~factor(country)~1, data=umf, chains=3, iter=itera, cores=12)
fit_stan_6P <- stan_occu(~1~factor(country), data=umf, chains=3, iter=itera, cores=12)
fit_stan_7P <- stan_occu(~1~scale(lat), data=umf, chains=3, iter=itera, cores=12)
fit_stan_8P <- stan_occu(~1~scale(elevation), data=umf, chains=3, iter=itera, cores=12)
fit_stan_9P <- stan_occu(~1~factor(land_cover), data=umf, chains=3, iter=itera, cores=12)
fit_stan_10P <- stan_occu(~1~scale(per_tree_cov), data=umf, chains=3, iter=itera, cores=12)
fit_stan_11P <- stan_occu(~1~scale(road_den), data=umf, chains=3, iter=itera, cores=12)
fit_stan_12P <- stan_occu(~1~scale(cattle), data=umf, chains=3, iter=itera, cores=12)



# fit_stan_4 <- stan_occu(~1~scale(elevation), data=umf, chains=3, iter=itera, cores=3)
# fit_stan_5 <- stan_occu(~1~factor(land_cover), data=umf, chains=3, iter=itera, cores=3)
# fit_stan_6 <- stan_occu(~1~scale(per_tre_cover), data=umf, chains=3, iter=itera, cores=3)
# fit_stan_7 <- stan_occu(~1~scale(elevation) + factor(land_cover), data=umf, chains=3, iter=itera, cores=3)

# put name to the models
stan_mods <- fitList("p(.) Ocu(.)" =           fit_stan_1T,
                     "p(bait) Ocu(.)"=         fit_stan_2T,
                     # "p(CamType) Ocu(.)"=      fit_stan_3,
                     "p(lat) Ocu(.)"=          fit_stan_4T,
                     "p(country) Ocu(.)"=      fit_stan_5T, 
                     "p(.) Ocu(country)"=      fit_stan_6T,
                     "p(.) Ocu(lat)"=          fit_stan_7T,
                     "p(.) Ocu(elevation)"=    fit_stan_8T,
                     "p(.) Ocu(land_cover)"=    fit_stan_9T)


# model selection 
# El modelo con el elpd más grande tuvo el mejor rendimiento
ms2 <- round(modSel(stan_mods), 3) #AGB_Spawn
ms2
# La columna elpd_diff muestra la diferencia en elpd entre un modelo y el modelo superior; Si esta diferencia es varias veces mayor que el error estándar de la diferencia (se_diff), estamos seguros de que el modelo con el elpd más grande tuvo un mejor desempeño.


(fit_top_gof <- gof(fit_stan_2T, draws=100, quiet=TRUE))
plot(fit_top_gof)

# (fit_stan_2)
plot_effects(fit_stan_2P, "det") # Detection
plot_effects(fit_stan_3P, "det") # Detection
plot_effects(fit_stan_4P, "det") # Detection
plot_effects(fit_stan_5P, "det") # Detection
plot_effects(fit_stan_6P, "state") # Occupancy
plot_effects(fit_stan_7P, "state") # Occupancy
plot_effects(fit_stan_8P, "state") # Occupancy
plot_effects(fit_stan_9P, "state") # Occupancy
plot_effects(fit_stan_10P, "state") # Occupancy
plot_effects(fit_stan_11P, "state") # Occupancy
plot_effects(fit_stan_12P, "state") # Occupancy



