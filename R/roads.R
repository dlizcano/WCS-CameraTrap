

library(terra)
library(mapview)
library(sf)

### Roads https://www.globio.info/download-grip-dataset
## https://zenodo.org/records/6420961
# pixel 8 km2
road <- rast("C:/CodigoR/WCS_2024/camera_trap/raster/latlon/RoadDensity/grip4_total_dens_m_km2.asc")
plot(road)
mapview(road)

#shp 700 megas
road_shp <- st_read("C:/CodigoR/WCS_2024/camera_trap/shp/RoadDensity/GRIP4_region2.shp")
