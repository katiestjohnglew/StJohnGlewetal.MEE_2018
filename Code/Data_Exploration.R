
UKdata<-read.csv("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/UK_data_all.csv",header=T)
#Remember to combine West Ireland and West scotland as West 

#Plot C vs N plots to explore data
# Sulphur?

#Subset all data
NS<-subset(UKdata,UKdata$Region=="NS")
Celtic<-subset(UKdata,UKdata$Region=="Celtic")
Irish<-subset(UKdata,UKdata$Region=="Irish")
EChan<-subset(UKdata,UKdata$Region=="EChan")
West<-subset(UKdata,UKdata$Region=="West")

Lion<-subset(UKdata,UKdata$Species=="Lion")
Bar<-subset(UKdata,UKdata$Species=="Barrel ")
Com<-subset(UKdata,UKdata$Species=="Compass ")
Mauve<-subset(UKdata,UKdata$Species=="Mauve")
Blue<-subset(UKdata,UKdata$Species=="Blue")
Cry<-subset(UKdata,UKdata$Species=="Crystal")
Moon<-subset(UKdata,UKdata$Species=="Moon")


range(UKdata$EK_d13C, na.rm=TRUE)
range(UKdata$EK_d15N, na.rm=TRUE)
range(UKdata$EK_d34S, na.rm = TRUE)

#to identify points on a map/graph - click and press escape 
locator()

#Plot C vs N
#Look for outliers
plot(UKdata$EK_d13C,UKdata$EK_d15N)
plot(NS$EK_d13C,NS$EK_d15N)
plot(Celtic$EK_d13C,Celtic$EK_d15N)
plot(Irish$EK_d13C,Irish$EK_d15N)
plot(EChan$EK_d13C,EChan$EK_d15N)
plot(West$EK_d13C,West$EK_d15N)

plot(Lion$EK_d13C,Lion$EK_d15N)
plot(Bar$EK_d13C,Bar$EK_d15N)
plot(Com$EK_d13C,Com$EK_d15N)
plot(Mauve$EK_d13C,Mauve$EK_d15N)
plot(Blue$EK_d13C,Blue$EK_d15N)
plot(Cry$EK_d13C,Cry$EK_d15N)
plot(Moon$EK_d13C,Moon$EK_d15N)

#Not sure how best to do Sulphur 
plot(UKdata$EK_d34S)
plot(UKdata$EK_CS)
hist(UKdata$EK_CS)

#Are Data normal?
hist(UKdata$EK_d13C)
hist(UKdata$EK_d15N)
hist(UKdata$EK_d34S)

result <- shapiro.test(UKdata$EK_d13C)
result

#If P is <0.05 the data are not normal. 
#I think not being normal is probably fine for isotope data?
#Check this but I think its OK!

result <- shapiro.test(UKdata$EK_d15N)
result

result <- shapiro.test(UKdata$EK_d34S)
result

#CN ratio - lipid corrections?
#Linear regression of CN ratio to 13C 
plot(UKdata$EK_d13C,UKdata$EK_CN)
lm<-lm(UKdata$EK_d13C~UKdata$EK_CN,data=UKdata)
plot(lm)
summary(lm)

lm<-lm(NS$EK_d13C~NS$EK_CN,data=NS)
plot(lm)
plot(NS$EK_d13C,NS$EK_CN)
summary(lm)

lm<-lm(Celtic$EK_d13C~Celtic$EK_CN,data=Celtic)
plot(lm)
plot(Celtic$EK_d13C,Celtic$EK_CN)
summary(lm)

lm<-lm(Irish$EK_d13C~Irish$EK_CN,data=Irish)
plot(lm)
plot(Irish$EK_d13C,Irish$EK_CN)
summary(lm)

lm<-lm(EChan$EK_d13C~EChan$EK_CN,data=EChan)
plot(lm)
plot(EChan$EK_d13C,EChan$EK_CN)
summary(lm)

lm<-lm(West$EK_d13C~West$EK_CN,data=West)
plot(lm)
plot(West$EK_d13C,West$EK_CN)
summary(lm)

lm<-lm(Lion$EK_d13C~Lion$EK_CN,data=Lion)
plot(lm)
plot(Lion$EK_d13C,Lion$EK_CN)
summary(lm)

lm<-lm(Bar$EK_d13C~Bar$EK_CN,data=Bar)
plot(lm)
plot(Bar$EK_d13C,Bar$EK_CN)
summary(lm)

lm<-lm(Com$EK_d13C~Com$EK_CN,data=Com)
plot(lm)
plot(Com$EK_d13C,Com$EK_CN)
summary(lm)

lm<-lm(Mauve$EK_d13C~Mauve$EK_CN,data=Mauve)
plot(lm)
plot(Mauve$EK_d13C,Mauve$EK_CN)
summary(lm)

lm<-lm(Blue$EK_d13C~Blue$EK_CN,data=Blue)
plot(lm)
plot(Blue$EK_d13C,Blue$EK_CN)
summary(lm)

lm<-lm(Cry$EK_d13C~Cry$EK_CN,data=Cry)
plot(lm)
plot(Cry$EK_d13C,Cry$EK_CN)
summary(lm)

lm<-lm(Moon$EK_d13C~Moon$EK_CN,data=Moon)
plot(lm)
plot(Moon$EK_d13C~Moon$EK_CN)
summary(lm)

#Assume will need to lipid correct 
#Apply Lipid correction and add a column to dataO
lip<-lm(UKdata$EK_d13C~UKdata$EK_CN)
summary(lip)
plot(UKdata$EK_d13C~UKdata$EK_CN)

#using Kiljunen paper 
kil.L<-93/(1+((0.246*UKdata$EK_CN)-0.775)^-1)
UKdata$d13Cc<-UKdata$EK_d13C+7.018*(0.048+(3.9/(1+(287/kil.L))))

#Look at weight/diameter relationship 
#plot weights/diameters to see if any outliers

UKdata$Weight<-as.numeric(UKdata$Weight)
UKdata$Diameter<-as.numeric(UKdata$Diameter)
plot(UKdata$Weight,UKdata$Diameter)
D<-na.omit(UKdata$Diameter)
range(D)
length(D)
length(D$Species=="Barrel")
range(W)
length(W)
length(UKdata$Sample_ID)


#regressions:
#ALL:
#d13C
#Just do diameters as done have enough weights 

plot(UKdata$Diameter,UKdata$d13Cc)
mod<-lm(UKdata$d13Cc~UKdata$Diameter)
summary(mod)
abline(mod)

#d15N


plot(UKdata$Diameter,UKdata$EK_d15N)
mod<-lm(UKdata$EK_d15N~UKdata$Diameter)
summary(mod)
abline(mod)

#S

plot(UKdata$Diameter,UKdata$EK_d34S)
mod<-lm(UKdata$EK_d34S~UKdata$Diameter)
summary(mod)
abline(mod)

# need to do size correction:
# look at per species:

Lion<-subset(UKdata,UKdata$Species=="Lion")
Bar<-subset(UKdata,UKdata$Species=="Barrel ")
Com<-subset(UKdata,UKdata$Species=="Compass ")
Mauve<-subset(UKdata,UKdata$Species=="Mauve")
Blue<-subset(UKdata,UKdata$Species=="Blue")
Cry<-subset(UKdata,UKdata$Species=="Crystal")
Moon<-subset(UKdata,UKdata$Species=="Moon")

range(Lion$Diameter)
range(Bar$Diameter)
range(Com$Diameter, na.rm = TRUE)
range(Mauve$Diameter, na.rm = TRUE)
range(Blue$Diameter, na.rm = TRUE)
range(Cry$Diameter, na.rm = TRUE)
range(Moon$Diameter, na.rm = TRUE)

#Carbon:

plot(Lion$Diameter,Lion$d13Cc)
mod<-lm(Lion$d13Cc~Lion$Diameter)
summary(mod)
abline(mod)

plot(Bar$Diameter,Bar$d13Cc)
mod<-lm(Bar$d13Cc~Bar$Diameter)
summary(mod)
abline(mod)

plot(Com$Diameter,Com$d13Cc)
mod<-lm(Com$d13Cc~Com$Diameter)
summary(mod)
abline(mod)

plot(Mauve$Diameter,Mauve$d13Cc)
mod<-lm(Mauve$d13Cc~Mauve$Diameter)
summary(mod)
abline(mod)

plot(Blue$Diameter,Blue$d13Cc)
mod<-lm(Blue$d13Cc~Blue$Diameter)
summary(mod)
abline(mod)

plot(Cry$Diameter,Cry$d13Cc)
mod<-lm(Cry$d13Cc~Cry$Diameter)
summary(mod)
abline(mod)

plot(Moon$Diameter,Moon$d13Cc)
mod<-lm(Moon$d13Cc~Moon$Diameter)
summary(mod)
abline(mod)


#Nitrogen:

plot(Lion$Diameter,Lion$EK_d15N)
mod<-lm(Lion$EK_d15N~Lion$Diameter)
summary(mod)
abline(mod)

plot(Bar$Diameter,Bar$EK_d15N)
mod<-lm(Bar$EK_d15N~Bar$Diameter)
summary(mod)
abline(mod)

plot(Com$Diameter,Com$EK_d15N)
mod<-lm(Com$EK_d15N~Com$Diameter)
summary(mod)
abline(mod)

plot(Mauve$Diameter,Mauve$EK_d15N)
mod<-lm(Mauve$EK_d15N~Mauve$Diameter)
summary(mod)
abline(mod)

plot(Blue$Diameter,Blue$EK_d15N)
mod<-lm(Blue$EK_d15N~Blue$Diameter)
summary(mod)
abline(mod)

plot(Cry$Diameter,Cry$EK_d15N)
mod<-lm(Cry$EK_d15N~Cry$Diameter)
summary(mod)
abline(mod)

plot(Moon$Diameter,Moon$EK_d15N)
mod<-lm(Moon$EK_d15N~Moon$Diameter)
summary(mod)
abline(mod)

#Sulfur:

plot(Lion$Diameter,Lion$EK_d34S)
mod<-lm(Lion$EK_d34S~Lion$Diameter)
summary(mod)
abline(mod)

plot(Bar$Diameter,Bar$EK_d34S)
mod<-lm(Bar$EK_d34S~Bar$Diameter)
summary(mod)
abline(mod)

plot(Com$Diameter,Com$EK_d34S)
mod<-lm(Com$EK_d34S~Com$Diameter)
summary(mod)
abline(mod)

plot(Mauve$Diameter,Mauve$EK_d34S)
mod<-lm(Mauve$EK_d34S~Mauve$Diameter)
summary(mod)
abline(mod)

plot(Blue$Diameter,Blue$EK_d34S)
mod<-lm(Blue$EK_d34S~Blue$Diameter)
summary(mod)
abline(mod)

plot(Cry$Diameter,Cry$EK_d34S)
mod<-lm(Cry$EK_d34S~Cry$Diameter)
summary(mod)
abline(mod)

plot(Moon$Diameter,Moon$EK_d34S)
mod<-lm(Moon$EK_d34S~Moon$Diameter)
summary(mod)
abline(mod)

#feel I should not apply diameter correction - will be taken into account in species variability:
#Change column names so I dont get confused:
UKdata$d13Cc<-UKdata$d13Cc
UKdata$d15N<-UKdata$EK_d15N
UKdata$d34S<-UKdata$EK_d34S


#Apply Diameter correction:
#d13C:

#mod<-lm(UKdata$d13Cc~UKdata$Diameter)
#summary(mod)
#median(UKdata$Diameter, na.rm=TRUE)
#corrected = measured - (diameter-mean/median)*slope
#UKdata$d13Ccc<-UKdata$d13Cc-((UKdata$Diameter-8.5)*-0.03)

#d15N:

#mod<-lm(UKdata$EK_d15N~UKdata$Diameter)
#summary(mod)
#median(UKdata$Diameter)
#corrected = measured - (diameter-mean/median)*slope
#UKdata$d15Nc<-UKdata$EK_d15N-((UKdata$Diameter-8.5)*0.058)

#Do the same for N and Sulphur if need be. 
#diam<-lm(UKdata$EK_d34S~UKdata$Diameter)
#summary(diam)
#median(UKdata$Diameter)
#UKdata$d34Sc<-UKdata$EK_d34S-((UKdata$Diameter-8.5)*0.0247)



#Test for a date effect:

#Remember to change d15N and S to corrected columns if have size corrected
#Cant really do anything about this anyway as different date samples are in different areas 

#Export data set as "Corrected Data"


write.csv(UKdata, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/UKdata_Corrected_SCN.csv")
