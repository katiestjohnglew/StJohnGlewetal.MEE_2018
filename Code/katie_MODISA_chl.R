library(devtools)
# if you don't have threddscrawler installed
install_github("BigelowLab/threddscrawler")
install_github("BigelowLab/obpgcrawler")
install_github("BigelowLab/spnc")


library(obpgcrawler)
library(spnc)
library(raster)
library(latticeExtra)

query <- obpg_query(top = 'https://oceandata.sci.gsfc.nasa.gov/opendap/catalog.xml',
                    platform = 'MODISA', 
                    product = 'L3SMI',
                    what = 'within',
                    date_filter = as.Date(c("2015-03-01", "2015-05-31"), format="%Y-%m-%d"),
                    greplargs = list(pattern='MO_CHL_chlor_a_4km', fixed = TRUE))

#Lizard coordinates
#lat <- -14.6787
#lon <- 145.4483
#Coordinates not on reef
lat <- 53
lon <- 0

i=1
q <- query[[i]]
#bb <- c(lon-1, lon+1, lat-1, lat+1)
chl <- SPNC(q$url, bb=NULL)
r <- chl$get_raster(what = 'chlor_a')

#p <- spplot(log10(r), main=paste0('Image=', format(chl$TIME)),
            #scales=list(draw=TRUE), auto.key=list(title="log10[chl]"))
#p <- p + layer(panel.points(lon, lat, pch=19, col=1, cex=2))
#print(p)

plot(r)
R<-r$layer_1

###  write raster files
dir = "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/CHL_2015_16"
setwd(dir)
writeRaster(R, filename="CHL_2016_01.grd",overwrite=TRUE)

###############################################
#Import all to make spring summer raster 



Mar15<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/CHL_2015_16/CHL_2015_03")
Apr15<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/CHL_2015_16/CHL_2015_04")
May15<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/CHL_2015_16/CHL_2015_05")
Jun15<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/CHL_2015_16/CHL_2015_06")
Jul15<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/CHL_2015_16/CHL_2015_07")
Aug15<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/CHL_2015_16/CHL_2015_08")
Sep15<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/CHL_2015_16/CHL_2015_09")
Mar16<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/CHL_2015_16/CHL_2016_03")
Apr16<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/CHL_2015_16/CHL_2016_04")
May16<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/CHL_2015_16/CHL_2016_05")
Jun16<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/CHL_2015_16/CHL_2016_06")
Jul16<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/CHL_2015_16/CHL_2016_07")
Aug16<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/CHL_2015_16/CHL_2016_08")
Sep16<-raster("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var/CHL_2015_16/CHL_2016_09")



#Then make a two year spring summer:
med_narm = function (x,...){median(x,na.rm=TRUE)}

SprSum1516_CHL<-do.call(overlay, c(Mar15,Apr15,May15,Jun15,Jul15,Aug15,Sep15,Mar16,Apr16,May16,Jun16,Jul16,Aug16,Sep16,fun=med_narm))
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/Env_Var")
writeRaster(SprSum1516_CHL, filename="SprSum1516_CHL", format="raster", overwrite=TRUE)

