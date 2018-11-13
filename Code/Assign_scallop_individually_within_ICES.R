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
x.range <- c(-13,8)
y.range <- c(48,62)

# Define dimensions of study area
full<-extent(-13,8,48,62)
q=0.1
tplate<-raster(nrows=(140),ncols=(210),ext=(full),crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

grd <- expand.grid(x=seq(from=-13, to=7.9, by=q), y=seq(from=48, to=61.9, by=q) )
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE

# set file location
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/Jellyfish_UKShelf")

dataO<-read.csv("JenningsScallopDataAll.csv",header = TRUE, fill=TRUE)
dataO<-dataO[dataO$year=="2001",]
dataO<-dataO[dataO$Ices=="VIId",]

unique(dataO$ID)

hdata<-dataO

sampleN<-hdata$d15N
sampleC<-hdata$d13Cc
#length of the test data seriess - here 18
ldat<-length(hdata$ID)

#number of MC replicates
MCrep=1

# storage for summed areas and points at end
mapX<-matrix(NA, nrow=29400, ncol=1)
max<-matrix(NA, ncol=2, nrow=1*MCrep)

crit=1.05

# read in isoscape and variance files
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/20180228_Model_selection_models/20180302_BEST_rasters")
jmodC<- raster("c_mean_raster_C2_res02_depthlim_mask") 
jmodCvar<- raster("c_var_raster_C2_res02_depthlim_mask") 

jmodN<- raster("n_mean_raster_N2_res02_depthlim_mask") 
jmodNvar<- raster("n_var_raster_N2_res02_depthlim_mask") 

jmodCvar[jmodCvar>0]<-2
jmodNvar[jmodNvar>0]<-4

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
N<-sampleN[22]
C<-sampleC[22]
dSN<-N
dSC<-C

# simulate distribution of trophic separations assign-jelly (initially 95% between 0.5 and 1.5 trophic levels)

Troph.diff<- 1
Troph.diff.SD<-0.5
Troph.diff.dist<-rnorm(10000, mean=Troph.diff, sd=Troph.diff.SD)

# simulate distribution of tissue offset assign-jelly (-2C -1N)

Tissue.offC<- -1.5
Tissue.offC.var<-0.25^2

Tissue.offN<- -0.5
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

Tissue.diff.N<-(Troph.diff*Troph.fract.N)+Tissue.offN
Tissue.diff.C<-(Troph.diff*Troph.fract.C)+Tissue.offC

dSN.offset<-dSN+Tissue.diff.N
dSC.offset<-dSC+Tissue.diff.C

testN<-dSN.offset
testC<-dSC.offset

par(mfrow=c(1,1))
plot(x=jmodC, y=jmodN, xlim=c(-26, -12), ylim=c(5, 20), cex=0.02,col="grey")
points(x=testC, y=testN)


par(mfrow=c(1,1))
tst<-as.matrix(rep(1, length(grd$x)))
testP<-raster(tst, template=tplate)
plot(testP, col="light grey", legend=FALSE)
map('worldHires',xlim=c(-20,15),ylim=c(50,62), col="white", fill=TRUE, add =TRUE)


cmu<-getValues(jmodC)
nmu<-getValues(jmodN)
mu<-cbind(cmu,nmu)



# run multi-variate normal estimate
mvn<-mlest(mu)

r=mvn$sigmahat[1,2]/(sqrt(mvn$sigmahat[1,1])*sqrt(mvn$sigmahat[2,2]))

##combined model and sampling uncertainty
var.c<-mvn$sigmahat[1,1]
var.n<-mvn$sigmahat[2,2]


N.measJ<-0.2
C.measJ<-0.1

rep.jelly.C.var<-1.08^2+C.measJ^2
rep.jelly.N.var<-1.65^2+N.measJ^2

#std dev (in isotope delta per mille values) around sampling and between-individual variance (0.2 for measurement on scallop, 0.8 for between individual)

N.meas<-0.2
C.meas<-0.2

N.ind<-0.7
C.ind<-0.2

var2.N<-N.meas^2+N.ind^2
var2.C<-C.meas^2+C.ind^2



cvar<-getValues(jmodCvar)+rep.jelly.C.var+var2.C+CalibErrorC
nvar<-getValues(jmodNvar)+rep.jelly.N.var+var2.N+CalibErrorN
vars<-cbind(cvar,nvar) #matrix like mu

ct=1

for(MC in 1:MCrep){
  
  
  x<-cbind(testC, testN)
  
  
  #i=1
  
  #loop (i) running each scallop separately
  #for(i in 1:ldat){
  
  
  # Set the desired probability range for the assignment maps (the area that represents X% of the total probability of hypothesis of origin being true   given the data)
  
  
  #RUN the MVN estimate
  
  ProbCN<-pdraster(x,mu,vars,r,tplate)
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
  
  area<-OddsSM      
  for(c in 1:(length(OddsSM))){
    if(OddsSM[c]>= 1/crit) {area[c]=1} else {area[c]=0}
  }
  Marea<-setValues(tplate, area)
  
  areaM<-matrix(data=area, nrow=210, ncol=140, byrow=FALSE)
  areaM2<-matrix(data=NA, nrow=210, ncol=140)
  for(p in 1:140){
    areaM2[,p]<-areaM[,(140-p)+1]
  }
  
  areaB<-as.vector(areaM2)
  grd <- expand.grid(x=seq(from=-13, to=7.9, by=q), y=seq(from=48, to=61.9, by=q) )
  
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
  
  test<-getValues(Marea)
  mapX[,ct]<-test
  
  mapX[,ct]=getValues(Marea)
  
  #max[i,1]<-jitter(Mspot[1], amount=0.3)
  #max[i,2]<-jitter(Mspot[2], amount=0.3)
  
  ct=ct+1  
  #end sample loop
}

max<-round(max, digits=1)

points(max, pch=19, col="purple")


# add the country outlines
map('worldHires',xlim=c(-13,8),ylim=c(48,62), col="white", fill=TRUE, add =TRUE)

all2<-vector(length=length(mapX[,1]))

for(x in 1:length(all2)){
  all2[x]<-sum(mapX[x,])
}

#end MC loop
#}


#plot 

A<-raster(mapX,template=tplate)
plot(testP, col="white", axes=FALSE, legend=FALSE)
plot(A, add=TRUE)
map('worldHires',xlim=c(-13,9),ylim=c(51,62), col="grey91", fill=TRUE, add =TRUE)
#title(main="Razor ID=44 SEC")

#See if assignment area overlaps with ICES area 
setwd("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/ICES_RASTERS")
VIId_r<-raster("VIId")

plot(VIId_r,add=TRUE,alpha=0.5)
