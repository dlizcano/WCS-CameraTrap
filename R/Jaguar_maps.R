

library(tmap)
library(tmaptools)
library(sf)
library(sf)
library(tidyverse)
library(readxl)
library(maps)

# elev <- rast("F:/WCS-CameraTrap/raster/latlon/elevation_z7.tif")
world1 <- sf::st_as_sf(map(database = 'world', plot = FALSE, fill = TRUE))
toplot = world1[c(32,243,189,179,92,63),]


# load data
Jaguar_Regional <- read_csv("H:/detections_Jaguar_Bolivia_Venezuela_Peru_Guatemala_Paraguay_Ecuador_triger.csv")
y_reg <- Jaguar_Regional[,27:68] # select sampling occasions to 42 days

# sum records
Jaguar_Regional$records <- apply(y_reg, 1,sum, na.omit=T)

recordmap <- Jaguar_Regional |> 
select(c("year_sampling", "excel_file", "longitude", "latitude", "records", "Pais")) |> 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) |> 
  mutate(excel_file=as.factor(excel_file))
 
# mapview
mapview(recordmap, 
        alpha = 0,
        map.types = "Esri.WorldImagery",
        cex = "records")

# bounding boxes
bb_Guatemala = tmaptools::bb(recordmap[recordmap$Pais == "Guatemala",], ext = 1.1)
bb_Venezuela = tmaptools::bb(recordmap[recordmap$Pais == "Venezuela",], ext = 1.1)
bb_Ecuador = tmaptools::bb(recordmap[recordmap$Pais == "Ecuador",], ext = 1.1)
bb_Peru = tmaptools::bb(recordmap[recordmap$Pais == "Peru",], ext = 1.1)
bb_Bolivia = tmaptools::bb(recordmap[recordmap$Pais == "Bolivia",], ext = 1.1)


tm_shape(recordmap) +
  # tm_basemap("Esri.WorldImagery") + # usa basemap
  tm_symbols(shape = 1, col = "red", fill = "red",size =0.2) + # punto rojo
  tm_basemap("Esri.WorldImagery") #+
  # tm_inset(tmaptools::bb("bb_Guatemala")) +
  # tm_inset(tmaptools::bb("bb_Venezuela")) +
  # tm_inset(tmaptools::bb("bb_Ecuador"))+
  # tm_inset(tmaptools::bb("bb_Peru"))+
  # tm_inset(tmaptools::bb("bb_Bolivia"))
  # 


tm_shape(toplot) +
  tm_borders(lwd = 1, col ="gray80") +
  tm_shape(recordmap) +
  # tm_basemap("Esri.WorldImagery") + # usa basemap
  tm_symbols(shape = 1, col = "red", fill = "red",size =0.2) + # punto rojo
  tm_basemap("Esri.WorldTerrain") +
  tm_title("Guatemala", group_id = 2) +
  tm_inset(tmaptools::bb("bb_Guatemala", ext = 1.5), group_id = 2) +
  tm_title("Venezuela", group_id = 2) +
  tm_inset(tmaptools::bb("bb_Venezuela", ext = 1.5), group_id = 2) +
  tm_title("Ecuador", group_id = 2) +
  tm_inset(tmaptools::bb("bb_Ecuador", ext = 1.5), group_id = 2) +
  tm_title("Peru", group_id = 2) +
  tm_inset(tmaptools::bb("bb_Peru", ext = 1.5), group_id = 2) +
  tm_title("Bolivia", group_id = 2) +
  tm_inset(tmaptools::bb("bb_Bolivia", ext = 1.5), group_id = 2) +
  tm_components(2, position = c("left", "bottom"), frame_combine =T) # +
  # tm_components(2, position = c("left", "top"))



  tm_shape(toplot) +
    tm_borders(lwd = 1, col ="gray80")  +
  tm_shape(recordmap) +
  # tm_basemap("Esri.WorldImagery") + # usa basemap
  tm_symbols(shape = 1, col = "red", fill = "red",size =0.2) + # punto rojo
  tm_basemap("Esri.WorldImagery") 


# tm_shape(recordmap.g, bbox = tmaptools::bb(recordmap.g, ext = 1.5))  + 
#   tm_basemap("Esri.WorldImagery") + # usa basemap
  # tm_symbols(shape = 1, col = "black", fill = "black",size =0.2) + #punto negro
tm_shape(toplot) +# usa basemap 
  tm_borders(lwd = 1, col ="gray80") +
  tm_basemap("Esri.WorldImagery") +
tm_shape(recordmap, is.main = TRUE)  + 
    tm_symbols(shape = 1, col = "red", fill = "red", size =0.2) +
    #tm_symbols(shape = 1, col = "red", fill = "red",size =0.3) + #punto negro
  # tm_bubbles(fill = "red", col = "red", size = "records", scale = 1.5) +
    tm_facets(by = "Pais", ncol = 2) +
  # tm_tiles("Esri_WorldImagery") +
  tm_legend_hide() 


