library(raster)
library(INLA)
library(maptools)
library(ggplot2)
library(cowplot)
library(rgeos)
library(SpatialEpi)
library(splancs) # for inout function

require(PBSmapping) 
library(maps)
library(mapdata)
library(maptools)

# 1. DATA INPUT ----
# load data and get environmental data for the models
Jelly <- read.csv("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/NorthSea/NorthSeaJellies_V3.csv", header = TRUE)
Jelly <- subset(Jelly, select = c("Haul_Lat", "Haul_Long","d13Cc", "dN15")) # need to check that I've got the right columns for the elements

# get points for sampling locations
x <- Jelly$Haul_Long
y <- Jelly$Haul_Lat
Jelly_xy <- cbind(x,y)
Jelly_pts <- SpatialPoints(Jelly_xy)

SST_ras<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/SprSum1516_SST.grd")
BT_ras<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/SprSum1516_BT.grd")
Sal_ras<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_Sal/SprSum1516_Sal.grd")
MLD_ras<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_MLD/SprSum1516_MLD.grd")
Depth_ras<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/Jellyfish_UKShelf/2017_ALL_JELLIES_UKSHELF/Environmental/etopo1.asc")
values(Depth_ras)[values(Depth_ras) > 0] = NA
Chl_ras<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/SprSum1516_CHL.grd")


#Worked out how to change resolution to 0.1 by 0.1 - do this in future:
#Make an empty raster so that resolution is 0.1 by 0.1 - use to resample env var rasters
blank<-raster(ncol=60, nrow=60, xmn=-4, xmx=8, ymn=50, ymx=62)

SST <- resample(SST_ras,blank)
CHL <- resample(Chl_ras,blank)
BT <- resample(BT_ras,blank)
MLD <- resample(MLD_ras,blank)
Sal <- resample(Sal_ras,blank)
Depth <- resample(Depth_ras,blank)

# put the rasters in a stack and name them - makes extracting data easier
env_data <- stack(SST, BT, Sal, MLD, Depth, CHL)
env_data <- scale(env_data) # standardising the values here so that they are standardised over the same mean and sd for fit and prediction
names(env_data) <- c("sst", "bt", "sal", "mld", "depth", "chl")

# extract data for each sample location
env_data_pts <- extract(env_data, Jelly_pts)


# merge to the main data - this will just merge the standardised data onto the main dataframe
Jelly <- cbind(Jelly, env_data_pts)
Jelly <- na.omit(Jelly)


# 2. CREATE THE MESH ----
# create the boundary for use in the mesh
XY_km <- as.matrix(latlong2grid(cbind(Jelly$Haul_Long, Jelly$Haul_Lat)))

data("wrld_simpl")

land <- subset(wrld_simpl, NAME %in% c("United Kingdom", "Ireland"))
land <- gSimplify(land, tol=0.3)
land<-aggregate(land)
plot(land)

# create a bounding box of the jellyfish samples
coords <- matrix(c(-4, 51.5,
                   -4, 62, 
                   8, 62, 
                   8, 51.5,
                   -4, 51.5), 
                 ncol=2, byrow=TRUE)

# rather than the bounding box, I've gone for a convex hull around the points and then a bit of a buffer
# you can change this back, I liked this because it limited the size of the mesh and prediction surface in a 
# sensible way
#ch <- chull(Jelly_xy)
#Jelly_bound <- Jelly_xy[c(ch, ch[1]), ] # closed polygon
Jelly_bound2<-coords

#outer <- SpatialPolygons(list(Polygons(list(Polygon(Jelly_bound)), ID=1)))
outer2 <- SpatialPolygons(list(Polygons(list(Polygon(Jelly_bound2)), ID=1)))
#outer <- gBuffer(outer, 5, byid=TRUE)
outer2 <- gBuffer(outer2, 5, byid=TRUE)

# subtract the land from outer limit
#sea <- gDifference(outer, land)
sea2 <- gDifference(outer2, land)# don't worry about the warning here about different proj4 strings, I've no idea why it's saying that because it's not true!
#plot(sea)
plot(sea2)

# create the mesh for use in the spatial models
#mesh_bound <- inla.mesh.2d(
 # max.edge = 0.5,
  #boundary = as.inla.mesh.segment(sea))

mesh_bound2 <- inla.mesh.2d(
  max.edge = 0.5,
  boundary = as.inla.mesh.segment(sea2))

plot(mesh_bound2, asp = 1)
points(x = Jelly$Haul_Long,  
       y = Jelly$Haul_Lat, 
       col = 2, 
       pch = 16, 
       cex = 0.5)


# 3. GENERATE PREDICTION DATA ----
# now we want to create the data to predict into - using the methods from https://www.stat.washington.edu/peter/591/INLA.html


# the aggregate line adjusts the resolution from 0.1 degree to 0.2 degree. Would recommend
# changing this to 1 for running on a more powerful computer to keep the
# original resolution. I ran the original testing on 0.5 (fact = 5) and it was very quick,
# but waaay too blocky
env_data_coarse <- aggregate(env_data, fact=1) 
plot(env_data_coarse, useRaster=T) # still reasonably fine
env_data_pred <- na.omit(as.data.frame(env_data_coarse, xy = TRUE))


# we also need to get rid of environmental data outside of the range of our
# observations (we don't want to extrapolate, gives weird results)
#sst_range <- subset(env_data_pred, sst <= 3 & sst >= -2)
sst_range <- env_data_pred
print(paste0("Limit by SST observed range: drop ", nrow(env_data_pred) - nrow(sst_range), " rows"))

depth_range <- subset(sst_range, depth <= 0.52 & depth >= -0.9)
#depth_range<-sst_range
print(paste0("Limit by Depth observed range: drop ", nrow(sst_range) - nrow(depth_range), " rows"))

chl_range <- subset(depth_range, chl <= 0.8 & chl >= -2)
#chl_range<-depth_range
print(paste0("Limit by Chl observed range: drop ", nrow(depth_range) - nrow(chl_range), " rows"))

#bt_range <- subset(chl_range, bt <=3 & bt >= -3)
bt_range<-chl_range
print(paste0("Limit by BT observed range: drop ", nrow(chl_range) - nrow(bt_range), " rows"))

#sal_range <- subset(bt_range, sal <=0.9 & sal >= -4)
sal_range <- bt_range
print(paste0("Limit by Sal observed range: drop ", nrow(bt_range) - nrow(sal_range), " rows"))

#mld_range <- subset(sal_range, mld <=3.5 & mld >= -1.5)
mld_range<- sal_range
print(paste0("Limit by Sal observed range: drop ", nrow(bt_range) - nrow(sal_range), " rows"))

env_data_pred <- mld_range

library(dplyr)
env_data_pred2<-subset(env_data_pred,env_data_pred$x>4.2 & env_data_pred$y>58)
env_data_pred3<-anti_join(env_data_pred, env_data_pred2, by=c('x', 'y'))

pred_locs <- cbind(env_data_pred3$x, env_data_pred3$y)


# now link the predicted locations to the mesh (and limit to the boundary we set in the mesh section)
pred_grid <- inla.mesh.projector(mesh_bound2, loc = pred_locs)
xy_in <- inout(pred_grid$loc, cbind(Jelly_bound2[, 1], Jelly_bound2[, 2])) # limits to boundary
sum(xy_in) # this is the number of points we are predicting into (it's reasonably high - 3195 at 0.2 deg res, so might take a bit of time)

# limit the locations and environmental data to those points within the boundary (denoted by xy_in)
pred_locs <- pred_locs[xy_in,]
colnames(pred_locs) <- c("x", "y")
env_data_pred <- env_data_pred3[xy_in,]

pred_pts <- SpatialPoints(pred_locs)
plot(pred_pts) # check we are plotting into sensible places


# 4. ASSOCIATE OBSERVATION AND PREDICTION DATA LOCATIONS WITH MESH VERTICES (A MATRIX)
A_b<-inla.spde.make.A(mesh_bound2, loc = XY_km)
spde_b<-inla.spde2.matern(mesh_bound2, alpha = 2)
wb.index <- inla.spde.make.index('w', n.spde = spde_b$n.spde)

# we need to calculate the A for the predictions too
A_pred <- pred_grid$proj$A[xy_in, ]

# sample size
N_obs <- nrow(Jelly)
N_pred <- nrow(pred_locs)
# Now we've got all the bits to go into the models, we can do the models:


# N ~ SST + Depth + Chl + Tdiff + f(Species) ----

# 1. Create the stack
Jelly$tdiff <- Jelly$sst - Jelly$bt # need to check this
env_data_pred$tdiff <- env_data_pred$sst - env_data_pred$bt

# fit stack
n_stack_fit <- inla.stack(
  tag = "Fit",
  data = list(y = Jelly$dN15),  
  A = list(1, 1, A_b),                  
  effects = list(   
    Intercept = rep(1, N_obs),
    X = Jelly[,c("sst", "bt", "tdiff", "mld", "sal", "chl", "depth")],
    w = wb.index))

stack_pred <- inla.stack(data = list(y = NA), 
                         A = list(1, 1, A_pred), tag = "Predicted", 
                         effects = list(
                           Intercept = rep(1, N_pred), 
                           X = env_data_pred[,c("sst", "bt", "tdiff", "mld", "sal", "chl", "depth")],
                           w = wb.index
                         ))

n_stack_all <- inla.stack(n_stack_fit, stack_pred)

# 3. No interactions model - no spatial 
N_ni_ns<- y ~ -1 + Intercept + sst + bt + mld + depth + chl + tdiff 

# No interactions model - spatial 
N_ni_s<- y ~ -1 + Intercept + sst + bt + mld + depth + chl + tdiff + f(w, model = spde_b)


# Interactions model - no spatial 
N_i_ns <- y ~ -1 + Intercept + sst + bt + sal + depth + chl + tdiff + sst:bt + 
  sst:sal + sst:mld + sst:depth + sst:chl + sst:tdiff + bt:sal + 
  bt:mld + bt:depth + bt:chl + bt:tdiff + sal:mld + sal:depth + 
  sal:chl + sal:tdiff + mld:depth + mld:chl + mld:tdiff + depth:chl + 
  depth:tdiff + chl:tdiff

# Interactions model - spatial 
N_i_s <- y ~ -1 + Intercept + sst + bt + sal + depth + chl + tdiff + sst:bt + 
  sst:sal + sst:mld + sst:depth + sst:chl + sst:tdiff + bt:sal + 
  bt:mld + bt:depth + bt:chl + bt:tdiff + sal:mld + sal:depth + 
  sal:chl + sal:tdiff + mld:depth + mld:chl + mld:tdiff + depth:chl + 
  depth:tdiff + chl:tdiff + f(w, model = spde_b)

N_ni_ns_m <- inla(N_ni_ns, 
                    family = "gaussian", 
                    data = inla.stack.data(n_stack_fit),
                    control.compute = list(dic = TRUE), 
                    control.predictor = list(A = inla.stack.A(n_stack_fit)))

#3.a. model outputs: 
summary(N_ni_ns_m)
Fit1 <- N_ni_ns_m$summary.fitted.values$mean[1:57]
E1   <- Jelly$dN15 - Fit1

plot(Fit1,Jelly$dN15)
cor.test(Fit1,Jelly$dN15)

#Homogeneity
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = Fit1, y = E1)
abline(h = 0, v = 0)

#Normality
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
hist(E1, breaks = 25)


# 4. Predict
n_spatial_pred <- inla(N_ni_ns, 
                       family = "gaussian", 
                       data = inla.stack.data(n_stack_all),
                       control.compute = list(dic = TRUE), 
                       control.predictor = list(A = inla.stack.A(n_stack_all)),  
                       quantiles = NULL,
                      control.results = list(return.marginals.random = FALSE, return.marginals.predictor = FALSE),
                       control.inla = list(int.strategy = "eb"))

# 5. Extract the indices of the prediction nodes and extract mean and sd of the response 
id_pred <- inla.stack.index(n_stack_all, "Predicted")$data
n_mean <- n_spatial_pred$summary.fitted.values$mean[id_pred]
n_sd <- n_spatial_pred$summary.fitted.values$sd[id_pred]
n_res <- data.frame(pred_locs, n_mean, n_var = n_sd^2)

# 7. Plot the results

n_mean_r<-subset(n_res, select = c("x", "y", "n_mean"))
coordinates(n_mean_r) <- ~x+y 
gridded(n_mean_r) <- TRUE
n_mean_raster<-raster(n_mean_r)

colfunc<-colorRampPalette(c("#a50026","#d73027","#f46d43","#fdae61","#fee090","#ffffbf","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695"))
colfunc(100)
plot(n_mean_raster,col=colfunc(100), axes=F, box=F, useRaster=T)
points(Jelly_xy,pch=16,cex=1,col="black")
map('worldHires',xlim=c(-4,9),ylim=c(50,62), col="white", fill=TRUE, add =TRUE)

n_var_r<-subset(n_res, select = c("x", "y", "n_var"))
coordinates(n_var_r) <- ~x+y 
gridded(n_var_r) <- TRUE
n_var_raster<-raster(n_var_r)

plot(n_var_raster,col=colfunc(100), axes=F, box=F, useRaster=T)
points(Jelly_xy,pch=16,cex=1,col="black")
map('worldHires',xlim=c(-4,9),ylim=c(48,62), col="white", fill=TRUE, add =TRUE)

# C ~ Sst + Chl + Species ----

# 1. Create the stack
Jelly$tdiff <- Jelly$sst - Jelly$bt # need to check this
env_data_pred$tdiff <- env_data_pred$sst - env_data_pred$bt

# fit stack
c_stack_fit <- inla.stack(
  tag = "Fit",
  data = list(y = Jelly$d13Cc),  
  A = list(1, 1, A_b),                  
  effects = list(   
    Intercept = rep(1, N_obs),
    X = Jelly[,c("sst", "bt", "tdiff", "mld", "sal", "chl", "depth")],
    w = wb.index))

stack_pred <- inla.stack(data = list(y = NA), 
                         A = list(1, 1, A_pred), tag = "Predicted", 
                         effects = list(
                           Intercept = rep(1, N_pred), 
                           X = env_data_pred[,c("sst", "bt", "tdiff", "mld", "sal", "chl", "depth")],
                           w = wb.index
                         ))

c_stack_all <- inla.stack(c_stack_fit, stack_pred)


# 3. 
#No intersactions no spatial
c_ni_ns<- y ~ -1 + Intercept + sst + bt + sal + depth + chl + tdiff

#No intersactions - spatial
c_ni_s<- y ~ -1 + Intercept + sst + bt + sal + depth + chl + tdiff + f(w, model = spde_b)

#interactions - no spatial
c_i_ns <- y ~ -1 + sst + bt + sal + chl + tdiff + sst:sal + 
  sst:mld + sst:depth + sst:chl + sst:tdiff + bt:sal + bt:mld + 
  bt:depth + bt:chl + sal:mld + sal:depth + sal:chl + sal:tdiff + 
  mld:depth + mld:chl + mld:tdiff + depth:chl + depth:tdiff + 
  chl:tdiff

#interactions - spatial
c_i_s <- y ~ -1 + sst + bt + sal + chl + tdiff + sst:sal + 
  sst:mld + sst:depth + sst:chl + sst:tdiff + bt:sal + bt:mld + 
  bt:depth + bt:chl + sal:mld + sal:depth + sal:chl + sal:tdiff + 
  mld:depth + mld:chl + mld:tdiff + depth:chl + depth:tdiff + 
  chl:tdiff + f(w, model = spde_b)

c_ni_ns<- inla(c_ni_ns, 
                    family = "gaussian", 
                    data = inla.stack.data(c_stack_fit),
                    control.compute = list(dic = TRUE), 
                    control.predictor = list(A = inla.stack.A(c_stack_fit)))

#3.a. model outputs: 
summary(c_ni_ns)
Fit1 <- c_ni_ns$summary.fitted.values$mean[1:57]
E1   <- Jelly$dN15 - Fit1

plot(Fit1,Jelly$d13Cc)
cor.test(Fit1,Jelly$d13Cc)

#Homogeneity
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = Fit1, y = E1)
abline(h = 0, v = 0)

#Normality
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
hist(E1, breaks = 25)

# 4. Predict
c_spatial_pred <- inla(c_ni_ns, 
                       family = "gaussian", 
                       data = inla.stack.data(c_stack_all),
                       control.compute = list(dic = TRUE), 
                       control.predictor = list(A = inla.stack.A(c_stack_all)),
                       quantiles = NULL, 
                       control.results = list(return.marginals.random = FALSE, return.marginals.predictor = FALSE), 
                       control.inla = list(int.strategy = "eb"))


# 5. Extract the indices of the prediction nodes and extract mean and sd of the response 
id_pred <- inla.stack.index(c_stack_all, "Predicted")$data
c_mean <- c_spatial_pred$summary.fitted.values$mean[id_pred]
c_sd <- c_spatial_pred$summary.fitted.values$sd[id_pred]
c_res <- data.frame(pred_locs, c_mean, c_var = c_sd^2)

# 7. Plot the results
c_mean_r<-subset(c_res, select = c("x", "y", "c_mean"))
coordinates(c_mean_r) <- ~x+y 
gridded(c_mean_r) <- TRUE
c_mean_raster<-raster(c_mean_r)

colfunc<-colorRampPalette(c("#a50026","#d73027","#f46d43","#fdae61","#fee090","#ffffbf","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695"))
colfunc(100)
plot(c_mean_raster,col=colfunc(100),axes=F, box=F, useRaster=T)
points(Jelly_xy,pch=16,cex=1,col="black")
map('worldHires',xlim=c(-4,9),ylim=c(48,62), col="white", fill=TRUE, add =TRUE)

c_var_r<-subset(c_res, select = c("x", "y", "c_var"))
coordinates(c_var_r) <- ~x+y 
gridded(c_var_r) <- TRUE
c_var_raster<-raster(c_var_r)

plot(c_var_raster,col=colfunc(100), axes=F, box=F, useRaster=T)
points(Jelly_xy,pch=16,cex=1,col="black")
map('worldHires',xlim=c(-4,9),ylim=c(48,62), col="white", fill=TRUE, add =TRUE)

#setwd("D:/KSG/INLA")
#writeRaster(c_mean_raster, "Template_raster",overwrite=TRUE)

#Save all rasters----
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/NorthSea/20180305_RASTERS")
writeRaster(c_mean_raster, "c_mean_raster_C_ni_ns2",overwrite=TRUE)
writeRaster(c_var_raster, "c_var_raster_C_ni_ns2",overwrite=TRUE)
writeRaster(n_mean_raster, "n_mean_raster_N_ni_ns2",overwrite=TRUE)
writeRaster(n_var_raster, "n_var_raster_N_ni_ns2",overwrite=TRUE)



