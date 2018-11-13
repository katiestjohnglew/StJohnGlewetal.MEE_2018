
library(sp)
library(raster)
library(rgeos)
library(spatstat)
library(rgdal)     
library(maptools)
library(maps)
library(mapdata)

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/ices_areas")
shape <- readOGR(dsn = ".", layer = "ices_areas")

head(shape@data)

IVa<- shape[shape@data$ICES_area == "IVa",]
plot(IVa)

IVb<- shape[shape@data$ICES_area == "IVb",]
plot(IVb)

IVc<- shape[shape@data$ICES_area == "IVc",]
plot(IVc)

VIId<- shape[shape@data$ICES_area == "VIId",]
plot(VIId)

VIIe<- shape[shape@data$ICES_area == "VIIe",]
plot(VIIe)

VIIf<- shape[shape@data$ICES_area == "VIIf",]
plot(VIIf)

VIIg<- shape[shape@data$ICES_area == "VIIg",]
plot(VIIg)

VIIh<- shape[shape@data$ICES_area == "VIIh",]
plot(VIIh)

VIIa<- shape[shape@data$ICES_area == "VIIa",]
plot(VIIa)

UK<- rbind(IVa, IVb, IVc, VIIa, VIId, VIIe, VIIf, VIIh, VIIg)
plot(UK)

map('worldHires',xlim=c(-13,9),ylim=c(51,62), col="grey91", fill=TRUE, add =TRUE)
