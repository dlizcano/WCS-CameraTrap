

#### read packages

library(readr)
library(unmarked)
library(terra)
library(mapview)
library(sf)
library(stars)
library(elevatr)
library(ubms)
library(ggplot)


#load raster
per_tree_cov <- rast("E:/WCS-CameraTrap/raster/latlon/Veg_Cont_Fields_Yearly_250m_v61/Perc_TreeCov/MOD44B_Perc_TreeCov_2010_065.tif")
road_den <- rast("E:/WCS-CameraTrap/raster/latlon/RoadDensity/grip4_total_dens_m_km2.asc")
elev <- rast("D:/CORREGIDAS/elevation_z7.tif")
landcov <- rast("E:/WCS-CameraTrap/raster/latlon/LandCover_Type_Yearly_500m_v61/LC1/MCD12Q1_LC1_2010_001.tif") 
cattle <- rast("E:/WCS-CameraTrap/raster/latlon/Global cattle distribution/5_Ct_2010_Da.tif")


# make sf
sites <- st_as_sf(Tropero_Regional, coords = c("Longitud","Latitud"))   #crs="EPSG:4326")
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

# load data
Jaguar_Regional <- read_csv("D:/CORREGIDAS/JaguarRegional50def_v2.csv")
y_regJ <- Jaguar_Regional[,9:58]

Tropero_Regional <- read_csv("D:/CORREGIDAS/TroperoRegional50def_v2.csv")
y_regT <- Tropero_Regional[,9:58]

#arrange detections Tropero observations
x4_baitT <- cbind(as.data.frame(Tropero_Regional$bait), replicate(60, Tropero_Regional$bait))
x4_latT <- cbind(as.data.frame(Tropero_Regional$Latitud), replicate(60, Tropero_Regional$Latitud))
x4_countryT <- cbind(as.data.frame(Tropero_Regional$Country), replicate(60, Tropero_Regional$Country))
x4_CamTypeT <- cbind(as.data.frame(Tropero_Regional$CamType), replicate(60, Tropero_Regional$CamType))
x4_ObsCovs_listT <- list(bait= as.data.frame(x4_baitT[,2:51]),
                        CamTypes= as.data.frame(x4_CamTypeT[,2:51]),
                        lat= as.data.frame(x4_latT[,2:51]),
                        country=as.data.frame(x4_countryT[,2:51]))


#arrange detections Jaguar observations
x4_baitJ <- cbind(as.data.frame(Jaguar_Regional$bait), replicate(60, Jaguar_Regional$bait))
x4_latJ <- cbind(as.data.frame(Jaguar_Regional$Lat), replicate(60, Jaguar_Regional$Lat))
x4_countryJ <- cbind(as.data.frame(Jaguar_Regional$Country), replicate(60, Jaguar_Regional$Country))
x4_CamTypeJ <- cbind(as.data.frame(Jaguar_Regional$CamType), replicate(60, Jaguar_Regional$CamType))
x4_ObsCovs_listJ <- list(bait= as.data.frame(x4_baitJ[,2:51]),
                        CamTypes= as.data.frame(x4_CamTypeJ[,2:51]),
                        lat= as.data.frame(x4_latJ[,2:51]),
                        country=as.data.frame(x4_countryJ[,2:51]))

site_covs <- data.frame(country=x4_countryJ[,2], 
                        lat=x4_latJ[,2], 
                        elevation=sites$elevation,
                        land_cover=sites$land_cover,
                        per_tree_cov=sites$per_tree_cov,
                        elevation=sites$elevation,
                        road_den=sites$roads,
                        cattle=sites$cattle
)



# put sp in a list
y <- list(as.matrix(y_regJ), as.matrix(y_regT))

names(y) <- c('Jaguar','Tropero')

#Create the unmarked data object
data_coOc = unmarkedFrameOccuMulti(y=y,
                              siteCovs=site_covs)
                              #obsCovs=det_covs)

#Summary of data object
summary(data_coOc)
(data_coOc)

# Look at f parameter design matrix
data@fDesign

# Formulas for state and detection processes

# Length should match number/order of columns in fDesign
occFormulas <- c('~per_tree_cov', "~per_tree_cov ","~1")

#Length should match number/order of species in data@ylist
detFormulas <- c('~1','~1')

fit1 <- occuMulti(#detFormulas,
                  stateformulas=occFormulas,
                  data=data_coOc)

#update model
occFormulas <- c('~per_tree_cov', "~elevation ",'~per_tree_cov+elevation','~1','~1','~1','~1')

# fit2 <- update(fit1,
#                stateformulas=occFormulas)

#List of fitted models
# fl <- fitList(fit1,fit2)
#coef(fl)

#Look at output
fit1


#Model selection
# modSel(fit1)

bt <- parboot(fit1, nsim=50) # takes time
plot(bt)


#Plot predicted marginal occupancy as a function of road_den
r <- range(sites$per_tree_cov)
x1 <- seq(r[1],r[2],length.out=100)
x_scale <- (x1-mean(sites$per_tree_cov))/sd(sites$per_tree_cov)

r2 <- range(sites$elevation)
x2 <- seq(r2[1],r2[2],length.out=100)
x2_scale <- (x2-mean(sites$elevation))/sd(sites$elevation)

nd <- matrix(NA, 100, 2)
nd <- data.frame(per_tree_cov=x_scale, elevation= x2_scale)


Jaguar_pred <- unmarked::predict(fit1, "state", 
                                 species="Jaguar", 
                                 newdata=nd)

Jaguar_pred$Species <- "Jaguar"

Tropero_pred <- predict(fit1, "state", 
                        species="Tropero", 
                        newdata=nd)

Tropero_pred$Species <- "Tropero"

# ocelote_pred <- predict(fit2, "state", species="ocelote", newdata=nd)
# ocelote_pred$Species <- "ocelote"


plot_dat1 <- rbind(Jaguar_pred, Tropero_pred)#, ocelote_pred)

ggplot(data=plot_dat1, aes(x=rep(x1,2), y=Predicted)) + # change to 3 sp and x2 to distance
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=Species), alpha=0.3) +
  geom_line(aes(col=Species)) +
  labs(x="per_tree_cov", y="Marginal occupancy") +
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.text=element_text(size=12), axis.title=element_text(size=14),
        legend.text=element_text(size=12), legend.title=element_text(size=14))



#######################


# #Plot predicted marginal occupancy as a function of disturbance
# r <- range(cams_ucu_sf$elev)
# x1 <- seq(r[1],r[2],length.out=100)
# x_scale <- (x1-mean(cams_ucu_sf$elev))/sd(cams_ucu_sf$elev)
# 
# r2 <- range(cams_ucu_sf$dist_casa)
# x2 <- seq(r2[1],r2[2],length.out=100)
# x2_scale <- (x2-mean(cams_ucu_sf$dist_casa))/sd(cams_ucu_sf$dist_casa)
# 
# nd <- matrix(NA, 100, 2)
# nd <- data.frame(elev=x_scale, dist_casa= x2_scale)

##### conditional

Jaguar_Tropero_no <- predict(fit1, "state", 
                             species="Jaguar", 
                             cond='-Tropero',
                             newdata=nd)

Jaguar_Tropero_no$Species <- "Tropero ausente"

Jaguar_Tropero_si <- predict(fit1, "state", 
                             species="Jaguar", 
                             cond='Tropero',
                             newdata=nd)

Jaguar_Tropero_si $Species <- "Tropero presente"



# perros_pred <- predict(fit1, "state", species="perros", newdata=nd)
# perros_pred$Species <- "perros"

# ocelote_pred <- predict(fit2, "state", species="ocelote", newdata=nd)
# ocelote_pred$Species <- "ocelote"


plot_dat2 <- rbind(Jaguar_Tropero_si, Jaguar_Tropero_no)#, ocelote_pred)

ggplot(data=plot_dat2, aes(x=rep(x1,2), y=Predicted)) + # change to 3 sp and x2 to distance
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=Species), alpha=0.3) +
  geom_line(aes(col=Species)) +
  labs(x="per_tree_cov", y="Jaguar conditional occupancy") +
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.text=element_text(size=12), axis.title=element_text(size=14),
        legend.text=element_text(size=12), legend.title=element_text(size=14))

##########
# head(predict(fit1,'state',species='Jaguar',cond='Tropero'))
# lapply(residuals(fit1),head)

########################################################
coy_marginal <- predict(mod_hdens, type='state', species="coyote") # get coyote
marg_plot_dat <- rbind(redfox_marginal[1,], coy_marginal[1,])
marg_plot_dat$Species <- c("Red fox", "Coyote")
marg_plot_dat

plot(1:2, plot_dat1$Predicted, #ylim=c(0.1,0.4), 
     #xlim=c(0.5,2.5), pch=19, cex=1.5, xaxt='n', 
     xlab="", ylab="Marginal occupancy and 95% CI")
axis(1, at=1:2, labels=marg_plot_dat$Species)

# CIs
top <- 0.1
for (i in 1:2){
  segments(i, marg_plot_dat$lower[i], i, marg_plot_dat$upper[i])
  segments(i-top, marg_plot_dat$lower[i], i+top)
  segments(i-top, marg_plot_dat$upper[i], i+top)
}


######################### Conditional
Jaguar_Tropero <- predict(fit1, type="state", species="Jaguar", cond="Tropero")
head(Jaguar_Tropero)

Jaguar_No_Tropero <- predict(fit1, type="state", species="Jaguar", cond="-Tropero")
head(Jaguar_Tropero)


plot_data <- rbind(Jaguar_Tropero[1,], Jaguar_No_Tropero[1,])
plot_data$Coyote_status <- c("Present","Absent")
head(plot_data)

plot(1:2, plot_data$Predicted, ylim=c(0, 0.5), 
     xlim=c(0.5,2.5), pch=19, cex=1.5, xaxt='n', 
     xlab="Tropero status", ylab="Jaguar conditional occupancy and 95% CI")
axis(1, at=1:2, labels=plot_data$Coyote_status)

# CIs
top <- 0.1
for (i in 1:2){
  segments(i, plot_data$lower[i], i, plot_data$upper[i])
  segments(i-top, plot_data$lower[i], i+top)
  segments(i-top, plot_data$upper[i], i+top)
}

################################## Marginal
Jaguar_marginal <- predict(fit1, type="state", species="Jaguar")
head(Jaguar_marginal)

Tropero_marginal <- predict(fit1, type='state', species="Tropero") # get coyote
marg_plot_dat <- rbind(Jaguar_marginal[1,], Tropero_marginal[1,])
marg_plot_dat$Species <- c("Jaguar", "Tropero")
marg_plot_dat


plot(1:2, marg_plot_dat$Predicted, ylim=c(0.1,0.5), 
     xlim=c(0.5,2.5), pch=19, cex=1.5, xaxt='n', 
     xlab="", ylab="Marginal occupancy and 95% CI")
axis(1, at=1:2, labels=marg_plot_dat$Species)

# CIs
top <- 0.1
for (i in 1:2){
  segments(i, marg_plot_dat$lower[i], i, marg_plot_dat$upper[i])
  segments(i-top, marg_plot_dat$lower[i], i+top)
  segments(i-top, marg_plot_dat$upper[i], i+top)
}

fit1


