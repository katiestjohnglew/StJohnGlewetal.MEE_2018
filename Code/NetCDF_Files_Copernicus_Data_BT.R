install.packages("chron")
library(chron)
library(RColorBrewer)
library(lattice)
install.packages("ncdf4")
library(ncdf4)
library(spnc)
library(raster)
install.packages("Thermimage")
library(Thermimage)
library(rgdal)

#ncpath <- "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044"
#ncname<-"metoffice_foam1_amm7_NWS_BED_b20150103_dm20150101"
#ncfname<-paste(ncpath,ncname,".nc",sep="")
#dname<-"sotemper"

#ncin <- nc_open("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Jan_15/metoffice_foam1_amm7_NWS_BED_b20150103_dm20150101.nc")
#ncin<-nc_open(ncfname)
#print(ncin)

Genious<-function(filename){
  myraster <- raster(paste("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Dec_16/", filename, sep=""))
  setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Dec_16")
  writeRaster(myraster, filename=filename, format="raster", overwrite=TRUE)
}

#Don't need this function - but have left it in incase anything useful in future 
Tea2<-function (filename) {     
  
  ncin <- nc_open(paste("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Dec_16/", filename, sep=""))
  
  lon<-ncvar_get(ncin,"lon")
  nlon<-dim(lon)
  head(lon)
  
  lat<-ncvar_get(ncin,"lat")
  nlat<-dim(lat)
  head(lat)
  
  print(c(nlon,nlat))
  
  time <- ncvar_get(ncin,"time")
  time
  
  tunits <- ncatt_get(ncin,"time","units")
  nt <- dim(time)
  nt
  
  bt_array<-ncvar_get(ncin,dname)
  dlname <- ncatt_get(ncin,dname,"long_name")
  dunits <- ncatt_get(ncin,dname,"units")
  fillvalue <- ncatt_get(ncin,dname,"_FillValue")
  dim(bt_array)
  
  title <- ncatt_get(ncin,0,"title")
  institution <- ncatt_get(ncin,0,"institution")
  datasource <- ncatt_get(ncin,0,"source")
  references <- ncatt_get(ncin,0,"references")
  history <- ncatt_get(ncin,0,"history")
  Conventions <- ncatt_get(ncin,0,"Conventions")
  
  nc_close(ncin)
  
  ls()
  
  bt_array[bt_array==fillvalue$value] <- NA
  
  #Get the top layer of data = surface salinity 
  
  #m<-1
  str(bt_array)
  #bt_slice<-bt_array[,,m]
  bt_slice<-bt_array
  image(bt_slice)
  image(lon,lat,bt_slice, col=rev(brewer.pal(10,"RdBu")))
  
  
  dim(bt_slice)
  x<-rotate90.matrix(bt_slice)
  projection(myraster) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  xmin(myraster) <- -20
  xmax(myraster) <- 15
  ymin(myraster) <- 40
  ymax(myraster) <- 65
  
  setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Dec_16")
  writeRaster(myraster, filename=filename, format="raster", overwrite=TRUE)
}



#Now repeat for each day in the month:


##########################################
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Jan_15")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Feb_15:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Feb_15")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Mar_15:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Mar_15")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Apr_15:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Apr_15")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#May_15:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/May_15")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Jun_15:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Jun_15")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Jul_15:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Jul_15")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Aug_15:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Aug_15")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Sep_15:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Sep_15")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Oct_15:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Oct_15")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Nov_15:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Nov_15")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Dec_15:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Dec_15")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Jan_16:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Jan_16")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Feb_16:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Feb_16")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Mar_16:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Mar_16")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Apr_16:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Apr_16")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#May_16:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/May_16")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Jun_16:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Jun_16")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Jul_16:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Jul_16")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Aug_16:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Aug_16")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Sep_16:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Sep_16")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Oct_16:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Oct_16")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Nov_16:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Nov_16")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}

#Dec_16:

setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/dataset-MetO-NWS-PHYS-dm-BED_1510585814044/Dec_16")

library(data.table)
library(plyr)

#list all files in working directory
files<-list.files(pattern=".nc")
files
n<-length(files)

filename <- files[[1]]
Genious(filename)

for (i in 1:length(files)){
  Genious(files[[i]])
}



########################################################################
#Read in a months worth of rasters and average

#Jan_15
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Jan_15")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Jan_15<-overlay(c.stack,fun=median)
image(Jan_15)

Jan_15.df=as.data.frame(rasterToPoints(Jan_15))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Jan_15, filename="Jan_15", format="raster", overwrite=TRUE)

#Feb_15
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Feb_15")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Feb_15<-overlay(c.stack,fun=median)
image(Feb_15)

Feb_15.df=as.data.frame(rasterToPoints(Feb_15))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Feb_15, filename="Feb_15", format="raster", overwrite=TRUE)

#Mar_15
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Mar_15")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Mar_15<-overlay(c.stack,fun=median)
image(Mar_15)

Mar_15.df=as.data.frame(rasterToPoints(Mar_15))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Mar_15, filename="Mar_15", format="raster", overwrite=TRUE)

#Apr_15
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Apr_15")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Apr_15<-overlay(c.stack,fun=median)
image(Apr_15)

Apr_15.df=as.data.frame(rasterToPoints(Apr_15))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Apr_15, filename="Apr_15", format="raster", overwrite=TRUE)

#May_15
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/May_15")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
May_15<-overlay(c.stack,fun=median)
image(May_15)

May_15.df=as.data.frame(rasterToPoints(May_15))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(May_15, filename="May_15", format="raster", overwrite=TRUE)


#Jun_15
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Jun_15")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Jun_15<-overlay(c.stack,fun=median)
image(Jun_15)

Jun_15.df=as.data.frame(rasterToPoints(Jun_15))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Jun_15, filename="Jun_15", format="raster", overwrite=TRUE)


#Jul_15
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Jul_15")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Jul_15<-overlay(c.stack,fun=median)
image(Jul_15)

Jul_15.df=as.data.frame(rasterToPoints(Jul_15))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Jul_15, filename="Jul_15", format="raster", overwrite=TRUE)

#Aug_15
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Aug_15")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Aug_15<-overlay(c.stack,fun=median)
image(Aug_15)

Aug_15.df=as.data.frame(rasterToPoints(Aug_15))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Aug_15, filename="Aug_15", format="raster", overwrite=TRUE)

#Sep_15
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Sep_15")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Sep_15<-overlay(c.stack,fun=median)
image(Sep_15)

Sep_15.df=as.data.frame(rasterToPoints(Sep_15))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Sep_15, filename="Sep_15", format="raster", overwrite=TRUE)

#Oct_15
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Oct_15")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Oct_15<-overlay(c.stack,fun=median)
image(Oct_15)

Oct_15.df=as.data.frame(rasterToPoints(Oct_15))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Oct_15, filename="Oct_15", format="raster", overwrite=TRUE)

#Nov_15
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Nov_15")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Nov_15<-overlay(c.stack,fun=median)
image(Nov_15)

Nov_15.df=as.data.frame(rasterToPoints(Nov_15))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Nov_15, filename="Nov_15", format="raster", overwrite=TRUE)

#Dec_15
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Dec_15")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Dec_15<-overlay(c.stack,fun=median)
image(Dec_15)

Dec_15.df=as.data.frame(rasterToPoints(Dec_15))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Dec_15, filename="Dec_15", format="raster", overwrite=TRUE)


#Jan_16
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Jan_16")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Jan_16<-overlay(c.stack,fun=median)
image(Jan_16)

Jan_16.df=as.data.frame(rasterToPoints(Jan_16))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Jan_16, filename="Jan_16", format="raster", overwrite=TRUE)

#Feb_16
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Feb_16")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Feb_16<-overlay(c.stack,fun=median)
image(Feb_16)

Feb_16.df=as.data.frame(rasterToPoints(Feb_16))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Feb_16, filename="Feb_16", format="raster", overwrite=TRUE)

#Mar_16
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Mar_16")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Mar_16<-overlay(c.stack,fun=median)
image(Mar_16)

Mar_16.df=as.data.frame(rasterToPoints(Mar_16))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Mar_16, filename="Mar_16", format="raster", overwrite=TRUE)

#Apr_16
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Apr_16")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Apr_16<-overlay(c.stack,fun=median)
image(Apr_16)

Apr_16.df=as.data.frame(rasterToPoints(Apr_16))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Apr_16, filename="Apr_16", format="raster", overwrite=TRUE)

#May_16
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/May_16")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
May_16<-overlay(c.stack,fun=median)
image(May_16)

May_16.df=as.data.frame(rasterToPoints(May_16))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(May_16, filename="May_16", format="raster", overwrite=TRUE)


#Jun_16
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Jun_16")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Jun_16<-overlay(c.stack,fun=median)
image(Jun_16)

Jun_16.df=as.data.frame(rasterToPoints(Jun_16))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Jun_16, filename="Jun_16", format="raster", overwrite=TRUE)


#Jul_16
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Jul_16")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Jul_16<-overlay(c.stack,fun=median)
image(Jul_16)

Jul_16.df=as.data.frame(rasterToPoints(Jul_16))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Jul_16, filename="Jul_16", format="raster", overwrite=TRUE)

#Aug_16
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Aug_16")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Aug_16<-overlay(c.stack,fun=median)
image(Aug_16)

Aug_16.df=as.data.frame(rasterToPoints(Aug_16))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Aug_16, filename="Aug_16", format="raster", overwrite=TRUE)

#Sep_16
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Sep_16")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Sep_16<-overlay(c.stack,fun=median)
image(Sep_16)

Sep_16.df=as.data.frame(rasterToPoints(Sep_16))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Sep_16, filename="Sep_16", format="raster", overwrite=TRUE)

#Oct_16
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Oct_16")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Oct_16<-overlay(c.stack,fun=median)
image(Oct_16)

Oct_16.df=as.data.frame(rasterToPoints(Oct_16))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Oct_16, filename="Oct_16", format="raster", overwrite=TRUE)

#Nov_16
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Nov_16")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Nov_16<-overlay(c.stack,fun=median)
image(Nov_16)

Nov_16.df=as.data.frame(rasterToPoints(Nov_16))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Nov_16, filename="Nov_16", format="raster", overwrite=TRUE)

#Dec_16
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT/Dec_16")
require(raster) 
current.list <- list.files(pattern =".grd", full.names=TRUE)
c.stack<- stack(current.list)
projection(c.stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
Dec_16<-overlay(c.stack,fun=median)
image(Dec_16)

Dec_16.df=as.data.frame(rasterToPoints(Dec_16))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/Rasters_BT")
writeRaster(Dec_16, filename="Dec_16", format="raster", overwrite=TRUE)

#####################################

#Then make a two year spring summer:


# Project Raster


SprSum1516<-overlay(Mar_15,Apr_15,May_15,Jun_15,Jul_15,Aug_15,Sep_15,Mar_16,Apr_16,May_16,Jun_16,Jul_16,Aug_16,Sep_16,fun=median)
writeRaster(SprSum1516, filename="SprSum1516_BT", format="raster", overwrite=TRUE)

plot(SprSum1516)
points(Jelly_xy,cex = .4)

