

#### read CSV

library(readr)
library(unmarked)
Jaguar_Colombia <- read_csv("C:/WCS_2024/camera_trap/data/CORREGIDAS/output/Jaguar_Colombia.csv")

y_col <- Jaguar_Colombia[,8:156]


#arrange detections observations

x4_bait <- cbind(as.data.frame(Jaguar_Colombia$bait), replicate(150, Jaguar_Colombia$bait))
x4_CamType <- cbind(as.data.frame(Jaguar_Colombia$CamType), replicate(150, Jaguar_Colombia$CamType))
x4_ObsCovs_list <- list(#bait= as.data.frame(x4_bait[,2:150]),
                        CamTypes= as.data.frame(x4_CamType[,2:150]))


# Make UMF object
umf <- unmarkedFrameOccu(y= y_col,
                         # siteCovs = as.data.frame(covs),
                         obsCovs = x4_ObsCovs_list)

# saveRDS(umf, "C:/CodigoR/WCS_2024/camera_trap/R/umf.rds")

plot(umf, main="Panthera onca")  



# fit unmarked models
fit_1 <- unmarked::occu(~1~1, data=umf) # ok!
fit_2 <- unmarked::occu(~factor(bait) ~1, data=umf) # It work!
fit_3 <- unmarked::occu(~factor(CamTypes) ~1, data=umf) # It work!
# fit_4 <- unmarked::occu(~1~scale(elevation), data=umf) # It work!
# fit_5 <- unmarked::occu(~1~factor(land_cover), data=umf) # Hessian problem!
# fit_6 <- unmarked::occu(~1~scale(per_tre_cover), data=umf)# It work!

# model names
# fit list for detection
fms1<-fitList("p(.) Ocu(.)"=            fit_1,
              "p(bait) Ocu(.)"=         fit_2, 
              "p(CamType) Ocu(.)"=      fit_3)#, 
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

pb <- parboot(fit_3, fitstats, nsim=300, report=1)
plot(pb, main="p(CamType) Ocu(.)")


# define number of iterations
itera=10000

# fit stan (Bayesian models) using the package ubms
fit_stan_1 <- stan_occu(~1~1, data=umf, chains=3, iter=itera, cores=3)
fit_stan_2 <- stan_occu(~factor(bait)~1, data=umf, chains=3, iter=itera, cores=3)
fit_stan_3 <- stan_occu(~factor(CamTypes)~1, data=umf, chains=3, iter=itera, cores=3)
fit_stan_4 <- stan_occu(~1~scale(elevation), data=umf, chains=3, iter=itera, cores=3)
fit_stan_5 <- stan_occu(~1~factor(land_cover), data=umf, chains=3, iter=itera, cores=3)
fit_stan_6 <- stan_occu(~1~scale(per_tre_cover), data=umf, chains=3, iter=itera, cores=3)
fit_stan_7 <- stan_occu(~1~scale(elevation) + factor(land_cover), data=umf, chains=3, iter=itera, cores=3)

# put name to the models
stan_mods <- fitList("p(.) Ocu(.)" =           fit_stan_1,
                     "p(bait) Ocu(.)"=         fit_stan_2,
                     "p(CamType) Ocu(.)"=      fit_stan_3,
                     "p(.) Ocu(elevation)"=    fit_stan_4,
                     "p(.) Ocu(land_cover)"=   fit_stan_5, 
                     "p(.) Ocu(per_tre_cover)"=fit_stan_6,
                     "p(.) Ocu(elev+land_cover)"=fit_stan_7
)

# model selection 
ms2 <- round(modSel(stan_mods), 3) #AGB_Spawn
ms2



(fit_top_gof <- gof(fit_stan_3, draws=100, quiet=TRUE))
plot(fit_top_gof)

(fit_stan_3)
plot_effects(fit_stan_2, "det") # Detection











