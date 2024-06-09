


library (sf)
puntos <- st_read("C:/CodigoR/WCS_2024/camera_trap/data/Bolivia/Est_LlanosMoxos2021.kml")

punt <- matrix(ncol = 2, nrow = 47)
for(j in 1:47){
  print(puntos$geometry[[j]][c(1,2)])
  punt[j,] <- puntos$geometry[[j]][c(1,2)]
}
write.csv(punt,"C:/CodigoR/WCS_2024/camera_trap/data/Bolivia/Est_LlanosMoxos2021.csv")
