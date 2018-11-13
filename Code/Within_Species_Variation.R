#Trial2<-read.csv("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/Jellyfish_UKShelf/2017_ALL_JELLIES_UKSHELF/Code/Trial_Corrected.csv",header=T)

UK_data<-read.csv("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/UKdata_Corrected_SCN.csv",header=T)
Trial2<-UK_data


#Subset into Species 
Lion<-subset(Trial2,Trial2$Species=="Lion")
Bar<-subset(Trial2,Trial2$Species=="Barrel ")
Com<-subset(Trial2,Trial2$Species=="Compass ")
Mauve<-subset(Trial2,Trial2$Species=="Mauve")
Blue<-subset(Trial2,Trial2$Species=="Blue")
Cry<-subset(Trial2,Trial2$Species=="Crystal")
Moon<-subset(Trial2,Trial2$Species=="Moon")

#Find all Rows where the Location ID is duplicated (the stations where multiple samples of the same species occur)
Lion_d<-Lion$Location_ID[duplicated(Lion$Location_ID)]
Lion_d
length(Lion_d)
Lion_d <- as.character(Lion_d)

#Now for every Location ID which there is more than one sample for
#Work out the standard deviation between C values 
#Save in a table - have a list of standard deviation of C values of Lion's mane jellies 
myStDev_C_Lion <- vector(length=length(Lion_d), mode="numeric")
#for (i in 1:length(Lion_d)){
  #a<-subset(Lion,Lion$Location_ID==Lion_d[i])
 # myStDev_C_Lion[i]<-sd(a$d13Cc)
#}

#Non Corrected - but will be corrected most likely 
for (i in 1:length(Lion_d)){
  a<-subset(Lion,Lion$Location_ID==Lion_d[i])
  myStDev_C_Lion[i]<-sd(a$d13Cc)
}

myStDev_C_Lion
mean(myStDev_C_Lion,, na.rm=TRUE)

#Repeat for N
myStDev_N_Lion <- vector(length=length(Lion_d), mode="numeric")
for (i in 1:length(Lion_d)){
  a<-subset(Lion,Lion$Location_ID==Lion_d[i])
  myStDev_N_Lion[i]<-sd(a$d15N)
}

myStDev_N_Lion
mean(myStDev_N_Lion, na.rm=TRUE)

#Repeat for S
myStDev_S_Lion <- vector(length=length(Lion_d), mode="numeric")
for (i in 1:length(Lion_d)){
  a<-subset(Lion,Lion$Location_ID==Lion_d[i])
  myStDev_S_Lion[i]<-sd(a$d34S)
}

myStDev_S_Lion
mean(myStDev_S_Lion, na.rm=TRUE)

######################################################################

#Find all Rows where the Location ID is duplicated (the stations where multiple samples of the same species occur)
Bar_d<-Bar$Location_ID[duplicated(Bar$Location_ID)]
Bar_d
length(Bar_d)
Bar_d <- as.character(Bar_d)

#Now for every Location ID which there is more than one sample for
#Work out the standard deviation between C values 
#Save in a table - have a list of standard deviation of C values of Lion's mane jellies 
myStDev_C_Bar <- vector(length=length(Bar_d), mode="numeric")
#for (i in 1:length(Bar_d)){
 # a<-subset(Bar,Bar$Location_ID==Bar_d[i])
  #myStDev_C_Bar[i]<-sd(a$d13Cc)
#}

for (i in 1:length(Bar_d)){
  a<-subset(Bar,Bar$Location_ID==Bar_d[i])
  myStDev_C_Bar[i]<-sd(a$d13Cc)
}

myStDev_C_Bar
mean(myStDev_C_Bar, na.rm=TRUE)

#Repeat for N
myStDev_N_Bar <- vector(length=length(Bar_d), mode="numeric")
for (i in 1:length(Bar_d)){
  a<-subset(Bar,Bar$Location_ID==Bar_d[i])
  myStDev_N_Bar[i]<-sd(a$d15N)
}

myStDev_N_Bar
mean(myStDev_N_Bar, na.rm=TRUE)

#Repeat for S
myStDev_S_Bar <- vector(length=length(Bar_d), mode="numeric")
for (i in 1:length(Bar_d)){
  a<-subset(Bar,Bar$Location_ID==Bar_d[i])
  myStDev_S_Bar[i]<-sd(a$d34S)
}

myStDev_S_Bar
mean(myStDev_S_Bar, na.rm=TRUE)

###########################################

#Find all Rows where the Location ID is duplicated (the stations where multiple samples of the same species occur)
Com_d<-Com$Location_ID[duplicated(Com$Location_ID)]
Com_d
length(Com_d)
Com_d <- as.character(Com_d)

#Now for every Location ID which there is more than one sample for
#Work out the standard deviation between C values 
#Save in a table - have a list of standard deviation of C values of Lion's mane jellies 
myStDev_C_Com <- vector(length=length(Com_d), mode="numeric")
for (i in 1:length(Com_d)){
  a<-subset(Com,Com$Location_ID==Com_d[i])
  myStDev_C_Com[i]<-sd(a$d13Cc)
}

myStDev_C_Com
mean(myStDev_C_Com, na.rm=TRUE)

#Repeat for N
myStDev_N_Com <- vector(length=length(Com_d), mode="numeric")
for (i in 1:length(Com_d)){
  a<-subset(Com,Com$Location_ID==Com_d[i])
  myStDev_N_Com[i]<-sd(a$d15N)
}

myStDev_N_Com
mean(myStDev_N_Com, na.rm=TRUE)

#Repeat for S
myStDev_S_Com <- vector(length=length(Com_d), mode="numeric")
for (i in 1:length(Com_d)){
  a<-subset(Com,Com$Location_ID==Com_d[i])
  myStDev_S_Com[i]<-sd(a$d34S)
}

myStDev_S_Com
mean(myStDev_S_Com, na.rm=TRUE)

########################

#Find all Rows where the Location ID is duplicated (the stations where multiple samples of the same species occur)
Mauve_d<-Mauve$Location_ID[duplicated(Mauve$Location_ID)]
Mauve_d
length(Mauve_d)
Mauve_d <- as.character(Mauve_d)

#Now for every Location ID which there is more than one sample for
#Work out the standard deviation between C values 
#Save in a table - have a list of standard deviation of C values of Lion's mane jellies 
myStDev_C_Mauve <- vector(length=length(Mauve_d), mode="numeric")
for (i in 1:length(Mauve_d)){
  a<-subset(Mauve,Mauve$Location_ID==Mauve_d[i])
  myStDev_C_Mauve[i]<-sd(a$d13Cc)
}

myStDev_C_Mauve
mean(myStDev_C_Mauve, na.rm=TRUE)

#Repeat for N
myStDev_N_Mauve <- vector(length=length(Mauve_d), mode="numeric")
for (i in 1:length(Mauve_d)){
  a<-subset(Mauve,Mauve$Location_ID==Mauve_d[i])
  myStDev_N_Mauve[i]<-sd(a$d15N)
}

myStDev_N_Mauve
mean(myStDev_N_Mauve, na.rm=TRUE)

#Repeat for S
myStDev_S_Mauve <- vector(length=length(Mauve_d), mode="numeric")
for (i in 1:length(Mauve_d)){
  a<-subset(Mauve,Mauve$Location_ID==Mauve_d[i])
  myStDev_S_Mauve[i]<-sd(a$d34S)
}

myStDev_S_Mauve
mean(myStDev_S_Mauve, na.rm=TRUE)

#################################

#Find all Rows where the Location ID is duplicated (the stations where multiple samples of the same species occur)
Blue_d<-Blue$Location_ID[duplicated(Blue$Location_ID)]
Blue_d
length(Blue_d)
Blue_d <- as.character(Blue_d)

#Now for every Location ID which there is more than one sample for
#Work out the standard deviation between C values 
#Save in a table - have a list of standard deviation of C values of Lion's mane jellies 
myStDev_C_Blue <- vector(length=length(Blue_d), mode="numeric")
for (i in 1:length(Blue_d)){
  a<-subset(Blue,Blue$Location_ID==Blue_d[i])
  myStDev_C_Blue[i]<-sd(a$d13Cc)
}

myStDev_C_Blue
mean(myStDev_C_Blue, na.rm=TRUE)

#Repeat for N
myStDev_N_Blue <- vector(length=length(Blue_d), mode="numeric")
for (i in 1:length(Blue_d)){
  a<-subset(Blue,Blue$Location_ID==Blue_d[i])
  myStDev_N_Blue[i]<-sd(a$d15N)
}

myStDev_N_Blue
mean(myStDev_N_Blue, na.rm=TRUE)

#Repeat for S
myStDev_S_Blue <- vector(length=length(Blue_d), mode="numeric")
for (i in 1:length(Blue_d)){
  a<-subset(Blue,Blue$Location_ID==Blue_d[i])
  myStDev_S_Blue[i]<-sd(a$d34S)
}

myStDev_S_Blue
mean(myStDev_S_Blue, na.rm=TRUE)

#############################
#Find all Rows where the Location ID is duplicated (the stations where multiple samples of the same species occur)
Cry_d<-Cry$Location_ID[duplicated(Cry$Location_ID)]
Cry_d
length(Cry_d)
Cry_d <- as.character(Cry_d)


myStDev_C_Cry <- vector(length=length(Cry_d), mode="numeric")
for (i in 1:length(Cry_d)){
  a<-subset(Cry,Cry$Location_ID==Cry_d[i])
  myStDev_C_Cry[i]<-sd(a$d13Cc)
}

myStDev_C_Cry
mean(myStDev_C_Cry, na.rm=TRUE)

#Repeat for N
myStDev_N_Cry <- vector(length=length(Cry_d), mode="numeric")
for (i in 1:length(Cry_d)){
  a<-subset(Cry,Cry$Location_ID==Cry_d[i])
  myStDev_N_Cry[i]<-sd(a$d15N)
}

myStDev_N_Cry
mean(myStDev_N_Cry, na.rm=TRUE)

#Repeat for S
myStDev_S_Cry <- vector(length=length(Cry_d), mode="numeric")
for (i in 1:length(Cry_d)){
  a<-subset(Cry,Cry$Location_ID==Cry_d[i])
  myStDev_S_Cry[i]<-sd(a$d34S)
}

myStDev_S_Cry
mean(myStDev_S_Cry, na.rm=TRUE)

################################################
#Find all Rows where the Location ID is duplicated (the stations where multiple samples of the same species occur)
Moon_d<-Moon$Location_ID[duplicated(Moon$Location_ID)]
Moon_d
length(Moon_d)
Moon_d <- as.character(Moon_d)


myStDev_C_Moon <- vector(length=length(Moon_d), mode="numeric")
for (i in 1:length(Moon_d)){
  a<-subset(Moon,Moon$Location_ID==Moon_d[i])
  myStDev_C_Moon[i]<-sd(a$d13Cc)
}

myStDev_C_Moon
mean(myStDev_C_Moon, na.rm=TRUE)

#Repeat for N
myStDev_N_Moon <- vector(length=length(Moon_d), mode="numeric")
for (i in 1:length(Moon_d)){
  a<-subset(Moon,Moon$Location_ID==Moon_d[i])
  myStDev_N_Moon[i]<-sd(a$d15N)
}

myStDev_N_Moon
mean(myStDev_N_Moon, na.rm=TRUE)

#Repeat for S
myStDev_S_Moon <- vector(length=length(Moon_d), mode="numeric")
for (i in 1:length(Moon_d)){
  a<-subset(Moon,Moon$Location_ID==Moon_d[i])
  myStDev_S_Moon[i]<-sd(a$d34S)
}

myStDev_S_Moon
mean(myStDev_S_Moon, na.rm=TRUE)

#Put variation into tables:
#Lion:
CL<-as.data.frame(myStDev_C_Lion)
NL<-as.data.frame(myStDev_N_Lion)
SL<-as.data.frame(myStDev_S_Lion)
Lion_Var<-cbind(CL,NL,SL)

CB<-as.data.frame(myStDev_C_Bar)
NB<-as.data.frame(myStDev_N_Bar)
SB<-as.data.frame(myStDev_S_Bar)
Bar_Var<-cbind(CB,NB,SB)

CM<-as.data.frame(myStDev_C_Mauve)
NM<-as.data.frame(myStDev_N_Mauve)
SM<-as.data.frame(myStDev_S_Mauve)
Mauve_Var<-cbind(CM,NM,SM)

CC<-as.data.frame(myStDev_C_Com)
NC<-as.data.frame(myStDev_N_Com)
SC<-as.data.frame(myStDev_S_Com)
Com_Var<-cbind(CC,NC,SC)

CBl<-as.data.frame(myStDev_C_Blue)
NBl<-as.data.frame(myStDev_N_Blue)
SBl<-as.data.frame(myStDev_S_Blue)
Blue_Var<-cbind(CBl,NBl,SBl)

CCr<-as.data.frame(myStDev_C_Cry)
NCr<-as.data.frame(myStDev_N_Cry)
SCr<-as.data.frame(myStDev_S_Cry)
Cry_Var<-cbind(CCr,NCr,SCr)

CMo<-as.data.frame(myStDev_C_Moon)
NMo<-as.data.frame(myStDev_N_Moon)
SMo<-as.data.frame(myStDev_S_Moon)
Moon_Var<-cbind(CMo,NMo,SMo)

#Export data set as Each Species UK Shelf With Species Variation 


write.csv(Moon_Var, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Within_sp_var/Moon_Var.csv")
write.csv(Cry_Var, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Within_sp_var/Cry_Var.csv")
write.csv(Blue_Var, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Within_sp_var/Blue_Var.csv")
write.csv(Mauve_Var, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Within_sp_var/Mauve_Var.csv")
write.csv(Com_Var, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Within_sp_var/Com_Var.csv")
write.csv(Bar_Var, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Within_sp_var/Bar_Var.csv")
write.csv(Lion_Var, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Within_sp_var/Lion_Var.csv")
