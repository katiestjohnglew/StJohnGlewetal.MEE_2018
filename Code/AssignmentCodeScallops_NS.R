
library(gstat)
library(sp)
library(maps)
library(mapdata)
library(lattice)
library(raster)
library(mvnmle)

range01 <- function(x){(x/sum(x))}

rep=1
f=1

#IMAGE BASE AREA AND ADD MAP HERE
x.range <- c(-4,9)
y.range <- c(50,62)

# Define dimensions of study area
full<-extent(-4,9,50,62)
q=0.1
tplate<-raster(nrows=(120),ncols=(130),ext=(full),crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

grd <- expand.grid(x=seq(from=-4, to=8.9, by=q), y=seq(from=50, to=61.9, by=q) )
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE



# set file location
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/Jellyfish_UKShelf")


dataO<-read.csv("JenningsScallopDataAll.csv",header = TRUE, fill=TRUE)
### - note - have to remove col1 from auto saved testDat.

dataO<-dataO[dataO$Sea.Area=="North.sea",]
#2010 = lots od samples, 2001 - not many samples! 

dataO<-dataO[dataO$year=="2001",]


#length of the test data seriess 
ldat<-length(dataO$year)


#number of MC replicates
MCrep=1

# set vector for citical values
odds<-1/(c(0.01,0.05,seq(from=0.1, to=0.95, by=0.05),0.99))
#odds<-1/0.7

# set vector for lists of mean and SD distance, area and accuracy estimates
dist<-vector(length=ldat*length(odds))
oddsS<-vector(length=ldat*length(odds))
Al<-vector(length=ldat*length(odds))
cor<-vector(length=ldat*length(odds))
PropArea<-vector(length=ldat*length(odds))
ID<-vector(length=ldat*length(odds))
MC.count<-vector(length=ldat*length(odds))

names<-c("ID", "MC count","dist", "Al","PropArea", "cor","oddsS")
result<-matrix(nrow=0, ncol=10)
result<-as.data.frame(result)
colnames(result)<-names

# run mass correction on scallop data

#massmod<-lm(dataO$d15N~dataO$mass)
#masscor<-dataO$mass-mean(dataO$mass)
#corr<-masscor*coefficients(massmod)[2]
#dataO$cord15N<-dataO$d15N+corr

#massmodC<-lm(dataO$d13Cc~dataO$mass)
#masscorC<-dataO$mass-mean(dataO$mass)
#corrC<-masscorC*coefficients(massmodC)[2]
#dataO$cord13C<-dataO$d13Cc+corrC

# read in isoscape and variance files
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/NorthSea/20180305_RASTERS")
jmodC<- raster("c_mean_raster_C_ni_ns2_global") 
jmodCvar<- raster("c_var_raster_C_ni_ns2_global") 

jmodN<- raster("n_mean_raster_N_ni_ns2_global") 
jmodNvar<- raster("n_var_raster_N_ni_ns2_global") 

jmodC<-resample(jmodC,tplate)
jmodCvar<-resample(jmodCvar,tplate)
jmodN<-resample(jmodN,tplate)
jmodNvar<-resample(jmodNvar,tplate)


## function to find probabilites
## x=value to assign, m=mean vector, v=var vector, r=corr coefficient, ras=raster extent
pdraster<-function(x,m,v,r,ras){
  pd<-1/(2*pi*sqrt(v[,1])*sqrt(v[,2])*sqrt(1-r^2))*exp(-(1/(2*(1-r^2)))*
                                                         ((x[1]-m[,1])^2/v[,1]+(x[2]-m[,2])^2/v[,2]-(2*r*(x[1]-m[,1])*(x[2]-m[,2]))/
                                                            (sqrt(v[,1])*sqrt(v[,2]))))
  pdras<-setValues(ras,pd)
  return(pdras)
}

loops=1



#MC simulation of isotopic offset values
sample<-dataO
s1<-sample

dSN<-sample$d15N
dSC<-sample$d13Cc


# simulate distribution of trophic separations assign-jelly (initially 95% between 0.5 and 1.5 trophic levels)

Troph.diff<- 1
Troph.diff.SD<-0.5
Troph.diff.dist<-rnorm(10000, mean=Troph.diff, sd=Troph.diff.SD)

# simulate distribution of tissue offset assign-jelly (-2C -1N)

Tissue.offC<- -1
Tissue.offC.var<-0.25^2

Tissue.offN<- 1
Tissue.offN.var<-0.25^2


# set distributions for trophic fractionation in carbon and nitrogen isotopes

Troph.fract.N<-3.4
Troph.fract.N.SD<-0.5
Troph.fract.dist.N<-rnorm(10000, mean=Troph.fract.N, sd=Troph.fract.N.SD)

Troph.fract.C<-1
Troph.fract.C.SD<-0.5
Troph.fract.dist.C<-rnorm(10000, mean=Troph.fract.C, sd=Troph.fract.C.SD)



CalibErrorN<-(var(Troph.diff.dist*Troph.fract.dist.N)+Tissue.offN.var)
CalibErrorC<-(var(Troph.diff.dist*Troph.fract.dist.C)+Tissue.offC.var)


sam.lon<-s1$Lon
sam.lat<-s1$Lat

sam=cbind(sam.lon, sam.lat)

dSN<-sample$d15N
dSC<-sample$d13Cc


Tissue.diff.N<-(Troph.diff*Troph.fract.N)+Tissue.offN
Tissue.diff.C<-(Troph.diff*Troph.fract.C)+Tissue.offC

dSN.offset<-dSN+Tissue.diff.N
dSC.offset<-dSC+Tissue.diff.C

testN<-dSN.offset
testC<-dSC.offset

par(mfrow=c(1,1))
plot(x=jmodC, y=jmodN, xlim=c(-26, -12), ylim=c(5, 20), cex=0.02,col="grey")
points(x=testC, y=testN)




coordinates(sample)=c("Lon", "Lat")

par(mfrow=c(1,1))
tst<-as.matrix(rep(1, length(grd)))
testP<-raster(tst, template=tplate)
plot(testP, col="light grey", legend=FALSE)
map('worldHires',xlim=c(-4,9),ylim=c(50,62), col="white", fill=TRUE, add =TRUE)


cmu<-getValues(jmodC)
nmu<-getValues(jmodN)
mu<-cbind(cmu,nmu)



# run multi-variate normal estimate
mvn<-mlest(mu)

r=mvn$sigmahat[1,2]/(sqrt(mvn$sigmahat[1,1])*sqrt(mvn$sigmahat[2,2]))

##combined model and sampling uncertainty
var.c<-mvn$sigmahat[1,1]
var.n<-mvn$sigmahat[2,2]

#jelly measured

N.measJ<-0.2
C.measJ<-0.1


##repeat jellyfish variance from the troph.dist R code (1.02 for C and 1.02 for N)
rep.jelly.C.var<-1.02^2+C.measJ^2
rep.jelly.N.var<-1.02^2+N.measJ^2

#jelly variabilty - withi species
#paper values 
rep.jelly.C.var<-1.69^2+C.measJ^2
rep.jelly.N.var<-1.04^2+N.measJ^2

#std dev (in isotope delta per mille values) around sampling and between-individual variance (0.2 for measurement on scallop, 0.8 for between individual)

#scallop measured variance
N.meas<-0.2
C.meas<-0.2

#scallop ind variability 
N.ind<-0.7
C.ind<-0.2

var2.N<-N.meas^2+N.ind^2
var2.C<-C.meas^2+C.ind^2

#play with variability to see what happens 

cvar<-getValues(jmodCvar)+rep.jelly.C.var+var2.C+CalibErrorC

#cvar<-(cvar*10)

nvar<-getValues(jmodNvar)+rep.jelly.N.var+var2.N+CalibErrorN

#nvar<-(nvar*10)
vars<-cbind(cvar,nvar) #matrix like mu



#loop (MC) running MC simulations for each scallop
for(MC in 1:MCrep){
  
  
  x<-cbind(testC, testN)
  
  
  
  
  #loop (i) running each scallop separately
  for(i in 1:ldat){
    
    
    # Set the desired probability range for the assignment maps (the area that represents X% of the total probability of hypothesis of origin being true 	given the data)
    
    ct=1
    
    
    #RUN the MVN estimate
    
    ProbCN<-pdraster(x[i,],mu,vars,r,tplate)
    Prob<-getValues(ProbCN)
    ######Decide here what you ratio your odds against!
    OddsCN<-ProbCN/(1-ProbCN)
    
    OddsL<-min(getValues(OddsCN), na.rm=TRUE)
    OddsH<-max(getValues(OddsCN), na.rm=TRUE)
    
    OddsR.low<-OddsCN/OddsL
    OddsR.high<-OddsCN/OddsH
    
    #pick which Odds ratio you want here
    OddsS<-OddsR.high
    
    #save in correct grid format
    OddsSM<-getValues(OddsR.high)
    
    for(tt in 1:length(OddsSM)){
      if(is.na(OddsSM[tt]) == FALSE) {OddsSM[tt]<-OddsSM[tt]} else {OddsSM[tt]<-0} 
    }
    
    #loop to caulate precision and accuracy for each critical odds ratio
    for(critical in 1:length(odds)){
      crit<-odds[critical]
      
      # storage for summed areas and points at end
      maps2<-matrix(NA, nrow=15600, ncol=length(s1$Station))
      max<-matrix(NA, ncol=2, nrow=length(s1$Station))
      
      area<-OddsSM			
      for(c in 1:(length(OddsSM))){
        if(OddsSM[c]>= 1/crit) {area[c]=1} else {area[c]=0}
      }
      Marea<-setValues(tplate, area)
      
      areaM<-matrix(data=area, nrow=130, ncol=120, byrow=FALSE)
      areaM2<-matrix(data=NA, nrow=130, ncol=120)
      for(p in 1:120){
        areaM2[,p]<-areaM[,(120-p)+1]
      }
      
      areaB<-as.vector(areaM2)
      grd <- expand.grid(x=seq(from=-4, to=8.9, by=q), y=seq(from=50, to=61.9, by=q) )
      
      Likely<-cbind(grd, areaB)
      
      coordinates(Likely) <- ~ x+y
      gridded(Likely) <- TRUE
      
      #plot the areas adding to critical probability
      contour(Likely, nlevels=1, add=TRUE, labels="", col="black", lwd=0.1)
      
      max.val<-which.max(OddsSM)
      # Add points of maximum likelihood area
      spot<-xyFromCell(ProbCN,max.val)
      Mspot<-coordinates(spot)
      
      #plotting known and assigned
      points(Mspot, col="red", pch=19)
      Known<-cbind(as.numeric(sam.lon), as.numeric(sam.lat))
      coordinates(Known)
      points(Known, col="black", pch=21)
      
      
      #maps2[,i]=Yarea
      segments(Mspot[1,1], Mspot[1,2], Known[i,1], Known[i,2], lwd=0.25)
      
      
      Dlon<-(sam.lon[i]-Mspot[1])*64
      Dlat<-(sam.lat[i]-Mspot[2])*111
      dist[loops]<-sqrt((Dlon^2)+(Dlat^2))
      
      
      Al[loops]<-length(area[area==1])*(6.4*11.1)
      
      #is sample contained within area lIklely?
      Slat<-round(s1$Lat, digits=1)
      Slon<-round(s1$Lon, digits=1)
      
      
      # NEED to find how to identify which grid cell is sample then test if area[cell] = 1
      #if (area[Slon, Slat]=1) {cor[bt]=TRUE} else {cor[bt]=FALSE}
      val=which(areaB==1)
      grd2 <- expand.grid(x=seq(from=-4, to=8.9, by=q), y=seq(from=50, to=61.9, by=q) )
      
      
      zapx<-round(grd2[val,]$x, digits=1)
      zapy<-round(grd2[val,]$y, digits=1)
      
      
      grr<-cbind(zapx, zapy)
      ss<-cbind(Slon[i], Slat[i])
      cor[loops] = 0
      if ((ss[,1]%in%grr[,1] =="TRUE") & (ss[,2]%in%grr[,2]=="TRUE")) {cor[loops] = 1}
      
      
      totArea<-(length(cmu)-length(cmu[cmu=="NA"]))*(6.4*11.1)
      PropArea[loops]<-(Al[loops]/totArea)
      oddsS[loops]<-crit
      ID[loops]<-dataO$ID[i]
      MC.count[loops]<-MC
      loops=loops+1
      
      #end odds loop (crit) 
    }
    #end sample loop (i))
  }
  #end MC loop (MC) 
}



result<-as.data.frame(result)

result<-rbind(result, cbind(ID,MC.count, dist, Al, PropArea, cor, oddsS))


PropC<-round(sum(result$cor)/length(result$cor), digits=2)
xbar=as.integer(mean(result$dist))
xsig=as.integer(sd(dist))
xN=(length(dist))
AreaM=as.integer(mean(Al))

setEPS()
postscript("Fig 3.eps", height=5, width=8)


par(mfrow=c(1,3))


PropCz<-vector(length=length(odds)*MCrep)
ProbAz<-vector(length=length(odds)*MCrep)
OddsZ<-vector(length=length(odds)*MCrep)
count1=1
for (MCcount in 1:MCrep){
  resultA=result[result$MC.count==MCcount,]
  for (z in 1:length(odds)){
    ResZ<-resultA[resultA$oddsS==odds[z],]
    PropCz[count1]<-round(sum(ResZ$cor)/length(ResZ$cor), digits=2)
    ProbAz[count1]<-mean(ResZ$PropArea)	
    OddsZ[count1]<-odds[z]
    count1<-count1+1
  }
}


dev.off()

ResultsZ<-as.data.frame(cbind(ProbAz, PropCz, OddsZ))
colnames(ResultsZ)<-c("Area", "Correct", "Odds")
test<-17
threshold=odds[test]
1-1/odds[test]
#retrive mean accuracy and precision for odds ratios
Rel.Prec<-mean(ResultsZ$Area[ResultsZ$Odds==threshold],)
Rel.PrecSD<-sd(ResultsZ$Area[ResultsZ$Odds==threshold],)
Prec<-mean(ResultsZ$Area[ResultsZ$Odds==threshold],)*totArea
PrecSD<-sd(ResultsZ$Area[ResultsZ$Odds==threshold],)*totArea
diam.Prec<-2*(sqrt(Prec/pi))
diamPrecSD<-2*(sqrt(PrecSD/pi))
Acc<-mean(ResultsZ$Correct[ResultsZ$Odds==threshold],)
AccSD<-sd(ResultsZ$Correct[ResultsZ$Odds==threshold],)
Acc
Prec
Rel.Prec
diam.Prec

dist.calc<-result[result$oddsS==odds[test],]
dist.mean<-mean(dist.calc$dist)
dist.sd<-sd(dist.calc$dist)
dist.mean
dist.sd

par(mfrow=c(1,3))

plot(x=(1/result$oddsS), y=result$PropArea, xlab="Odds Ratio", ylab="Proportional Area", axes=TRUE, xlim=c(0,1), ylim=c(0,1), pch=21, cex=2, col="blue", cex.axis=1.5, cex.lab=1.75)
abline(a=1, b=-1, col="red")


plot(x=(1/OddsZ), y=PropCz, xlab="Odds Ratio", ylab="Proportion Correctly Assigned", axes=TRUE, xlim=c(0, 1), ylim=c(0,1), pch=19, cex=2, col="blue", cex.axis=1.5, cex.lab=1.75)
abline(a=1, b=-1, col="red")


plot(x=ProbAz, y=PropCz, xlab="Precision (Proportion total area)", ylab="Accuracy (Proportion correctly assigned)", xlim=c(0, 1), ylim=c(0,1), pch=19, cex=2, col="red", cex.axis=1.5, cex.lab=1.75)
abline(coef=c(0,1), col="red", lty=2)


ResultsZ.df<-as.data.frame(ResultsZ)
write.table(ResultsZ.df, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/NorthSea/ResultsZ_2010_INLA_lipcorV2.txt", sep="\t")
