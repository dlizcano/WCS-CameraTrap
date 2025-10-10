


# library(lubridate)
library(sf)
library(readxl)
library(dplyr)
library(hms)
library(tidyr)

library(tidyverse)
library(grateful)

library(mapview)
library(maps)
library(tmap)

library(ggTimeSeries) # to make calendar
library(patchwork) # combine ggplots




# load custom functions
# Adjust path to the file (organiza_datos_v3) in your hard disk
source("C:/CodigoR/WCS-CameraTrap/R/organiza_datos_v3.R")

###########################
### Camera Trap Work flow
###########################

archivo_excel <- "PRY-001_10AUG15_Final.xlsx" #10a # put the name of file (campaign) to process
path_to_file <- paste("G:/WCS-CameraTrap/data/Paraguay/", # modify to your hard disk
                      archivo_excel, sep="")

# load data
full_table <-loadproject (path_to_file)

# Detection history creation
full_history <- wcs.det_history.creator(data=full_table)
names(full_history)


###########################
### make a calendar
###########################

#Load data by country
path <- "F:/WCS-CameraTrap/data/BDcorregidas"
Guatemala <- data_by_country(path, country = "Guatemala") 

dtData <- Guatemala  |> as.data.frame() |> 
  mutate(Date_Time=as_date(`Date_Time Captured`)) |> 
  count(Date_Time) |> na.omit()

# base plot
p1 = ggplot_calendar_heatmap(
  dtData,
  'Date_Time',
  'n',
  dayBorderSize = 0.1,
  monthBorderSize = 0.7
)

# adding some formatting
p1 +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_continuous(low = 'cyan', high = 'red') +
  facet_wrap(~Year, ncol = 1) # number of columns

###########################
### make a map per country
###########################

Country_to_map <- Guatemala |>  distinct(ExcelFile, year, `Deployment ID`, `Longitude Resolution`, `Latitude Resolution`) %>% 
  drop_na() # drop na in coord

projlatlon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# remove NA in coords

Country_map <- st_as_sf(x = Country_to_map,                         
                        coords = c("Longitude Resolution", 
                                   "Latitude Resolution"),
                        crs = projlatlon)


mapview(Country_map, zcol = c("year"),  burst = TRUE) # burst = TRUE prouce uniques


###########################
### make a calendar and map per excel
###########################

excelid <- unique(na.omit(Guatemala$ExcelFile)) # hay unos NA

for(i in 1:5){
  tablex <- filter(Guatemala, ExcelFile==excelid[i]) 
  exceltomap <- filter(Country_map, ExcelFile ==excelid[i])
    
  dtData <- tablex |> as.data.frame() |> 
    mutate(Date_Time=as_date(`Date_Time Captured`)) |> 
    count(Date_Time) |> na.omit() #%>% filter(Date_Time >= 2003)
  
  # base plot
  p1 = ggplot_calendar_heatmap(
    dtData,
    'Date_Time',
    'n',
    dayBorderSize = 0.1,
    monthBorderSize = 0.7
  )
  
  # adding some formatting
  p2 <-  p1+
    ggtitle(label = excelid[i]) +
    xlab(NULL) +
    ylab(NULL) +
    scale_fill_continuous(low = 'cyan', high = 'red') +
    facet_wrap(~Year, ncol = 1) # number of columns
  
  ### Map
  mapa <- tm_basemap("OpenTopoMap") +
    tm_shape(exceltomap, is.main = TRUE) + 
    tm_dots(col = "red") +
    # tm_facets(ncol = 1) +
    tm_scalebar(breaks = c(0, 3, 5), 
                text.size = 1,
                position = c("left", "bottom")) +
    tm_layout(panel.labels = excelid[i])
  
  # print calendar and map
  print(p2)
  print(mapa)

  
}






