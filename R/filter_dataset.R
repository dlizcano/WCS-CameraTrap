

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
Jaguar_Regional <- read_csv("E:/detections_Jaguar_Bolivia_Venezuela_Peru_Guatemala_Paraguay_Ecuador_triger.csv")
y_reg <- Jaguar_Regional[,27:68] # select sampling occasions to 42 days

# Count zeros in each row
row_zeros_1 <- apply(as.matrix(y_reg), 1, function(x) sum(x == 0, na.rm = TRUE))
ind <- which(row_zeros_1<=10) # which are less than 10 days
# remove cameras with less than 10 days
y_reg2 <- y_reg[-ind,]
Jaguar_Regional2 <- Jaguar_Regional[-ind, ]

# collapse to seven days
y_reg3 <- f.collapse.matrix.to6(y_reg2)


# remove lees than two sampling occasions
row_zeros_2 <- apply(as.matrix(y_reg3), 1, function(x) sum(x == 0, na.rm = TRUE))
ind2 <- which(row_zeros_2<=2) # which are less than 2 sampling occasions
y_reg3 <- y_reg3[-ind2,]
Jaguar_Regional3 <- Jaguar_Regional2[-ind2, ]



# save
# write_csv(y_reg3, file="E:/Jaguar_Bolivia_Venezuela_Peru_Guatemala_Paraguay_Ecuador_collapsedto6.csv")



