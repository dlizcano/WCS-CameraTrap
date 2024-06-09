
library(terra)
library(mapview)
library(sf)
library(rnaturalearth)
library(tidyverse)
library(tidyterra)
library(tmap)
        

# Base maps
# world and Latin America
world <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf') %>% st_set_crs("EPSG:4326")
n_america <- world |> dplyr::filter(continent==c("North America")) %>% filter(!adm0_a3_us=="USA")
s_america <- world |> dplyr::filter(continent==c("South America"))
france <- world |> dplyr::filter(name_en =="France") # add  French Guyana 
america <- rbind(n_america, s_america, france) # mosaic sf and french guyana
bbox_Latam_unprojected <- c(xmin=-118.5, ymin=-55.89170, xmax=-34.1, ymax= 32.5)
Latam_unprojected <- america %>% st_crop(bbox_Latam_unprojected) %>% st_make_valid

# equal area projection (Equatorial Lambert azimuthal equal-area) 
equalareaCRS <-  '+proj=laea +lon_0=-73.125 +lat_0=0 +datum=WGS84 +units=m +no_defs'
Latam_projected <- sf::st_transform(Latam_unprojected, crs=equalareaCRS) %>% st_cast() %>% st_make_valid
# Latam_projected <- Latam_projected %>% 
#  mutate(iso_a2=ifelse(is.na(iso_a2) & name=='France', 'GF', iso_a2)) %>% 
#  mutate(name_en=ifelse(iso_a2=='GF' & name_en=='France', 'French Guiana', name_en))

Latam <- st_as_sf(st_union(Latam_projected)) 


################################
##  reproject MODIS to 36 km
################################

# load veg cover yr 2001
vcov2001 <- rast("C:/CodigoR/WCS_2024/camera_trap/raster/lambert/LandCover_Type_Yearly_500m_v61/LC1/MCD12Q1_LC1_2001_001.tif")

# Identify file path string names
ruta <- "C:/CodigoR/WCS_2024/camera_trap/raster/lambert/LandCover_Type_Yearly_500m_v61/LC1/"
sp_IDs <- list.files(ruta, pattern = ".tif")
i.strings2 <- paste0(ruta, sp_IDs)

#### Loop to reproject

for (i in 1:length(i.strings2)){
  rastermap500m <- rast(i.strings2[i])
  # 1st aggregate to 10
  # r2 <- aggregate(vcov2001, fact = 12) # aprox 5 km pixel
  r2 <- aggregate(rastermap500m, fact = 12) # aprox 5 km pixel
  r2_masked <- terra::mask(r2, Latam) # add latam mask
  # mapview(r2_masked)

  # 2nd then resample to exact resolution 3600 = 36 km2
  # r2 <- vcov2001
  res(r2) <- 3600
  r2 <- resample(r2_masked, r2)
  # mapview(r2)
  terra::writeRaster(r2, paste("C:/CodigoR/WCS_2024/camera_trap/raster/lambert/LandCover_Type_Yearly_36km_v61/", sp_IDs[i], sep=""), overwrite=TRUE)

  # Just in case use also
  # reproject and resample (50m pixels)
  # pl.grid50 <- terra::project(x = rs.raster, y = crs(elev.raster), method="near", res=50)
}












