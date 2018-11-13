library(raster)
library(maptools)

require(PBSmapping) 
library(maps)
library(mapdata)
library(maptools)


setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/20180228_Model_selection_models/20180302_BEST_rasters")


# load data and get environmental data for the models
Jelly <- read.csv("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/UKdata_Corrected_SCN.csv")
Jelly <- subset(Jelly, select = c("Species", "Haul_Lat", "Haul_Long", "d34S", "d13Cc", "d15N")) # need to check that I've got the right columns for the elements

# get points for sampling locations
x <- Jelly$Haul_Long
y <- Jelly$Haul_Lat
Jelly_xy <- cbind(x,y)

colfunc<-colorRampPalette(c("#a50026","#d73027","#f46d43","#fdae61","#fee090","#ffffbf","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695"))
colfunc(100)
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Chapters/Thesis_Code_and_Data/Chapter 6/Isoscape_rasters")
S<-raster("UK_s_var_raster_S1_res02_depthlim_global_interactions")
plot(S,col=colfunc(100),axes=F, box=F, useRaster=T)
points(Jelly_xy,pch=16,cex=1,col="black")
map('worldHires',xlim=c(-13,9),ylim=c(47,64), col="grey91", fill=TRUE, add =TRUE)



#NS
Jelly <- read.csv("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/NorthSea/NorthSeaJellies_V3.csv", header = TRUE)
Jelly <- subset(Jelly, select = c("Haul_Lat", "Haul_Long","d13Cc", "dN15")) # need to check that I've got the right columns for the elements
# get points for sampling locations
x <- Jelly$Haul_Long
y <- Jelly$Haul_Lat
Jelly_xy <- cbind(x,y)

# read in isoscape and variance files
par(mfrow=c(2,2))

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/NorthSea/20180305_RASTERS")
jmodC<- raster("c_mean_raster_C_ni_ns2_global") 
jmodCvar<- raster("c_var_raster_C_ni_ns2_global") 

jmodN<- raster("n_mean_raster_N_ni_ns2_global") 
jmodNvar<- raster("n_var_raster_N_ni_ns2_global") 

colfunc<-colorRampPalette(c("#a50026","#d73027","#f46d43","#fdae61","#fee090","#ffffbf","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695"))
colfunc(100)
plot(jmodC,col=colfunc(100),axes=F, box=F, useRaster=T)
points(Jelly_xy,pch=16,cex=1,col="black")
map('worldHires',xlim=c(-4,9),ylim=c(48,62), col="grey91", fill=TRUE, add =TRUE)

plot(jmodCvar,col=colfunc(100), axes=F, box=F, useRaster=T)
points(Jelly_xy,pch=16,cex=1,col="black")
map('worldHires',xlim=c(-4,9),ylim=c(48,62), col="grey91", fill=TRUE, add =TRUE)

plot(jmodN,col=colfunc(100),axes=F, box=F, useRaster=T)
points(Jelly_xy,pch=16,cex=1,col="black")
map('worldHires',xlim=c(-4,9),ylim=c(48,62), col="grey91", fill=TRUE, add =TRUE)

plot(jmodNvar,col=colfunc(100), axes=F, box=F, useRaster=T)
points(Jelly_xy,pch=16,cex=1,col="black")
map('worldHires',xlim=c(-4,9),ylim=c(48,62), col="grey91", fill=TRUE, add =TRUE)

