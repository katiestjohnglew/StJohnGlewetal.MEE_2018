INLA_2001<-read.table("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/NorthSea/ResultsZ_2001_INLA_lipcorV2.txt", header=T)
INLA_2010<-read.table("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/NorthSea/ResultsZ_2010_INLA_lipcorV2.txt", header=T)
Krig_2001<-read.table("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/NorthSea/ResultsZ_2001_Krig_lipcor.txt", header=T)
Krig_2010<-read.table("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/NorthSea/ResultsZ_2010_Krig_lipcor.txt", header=T)

par(mfrow=c(2,2))

#Make figures to compare assignment results between original kriging isoscapes and new INLA isoscapes 
#2001

plot(x=(1/INLA_2001$Odds), y=INLA_2001$Correct, xlab="Odds Ratio", ylab="Proportion Correctly Assigned", axes=TRUE, xlim=c(0, 1), ylim=c(0,1), pch=19, cex=2, col="blue", cex.axis=1.5, cex.lab=1.75)
points(x=(1/Krig_2001$Odds), y=Krig_2001$Correct, pch=19, cex=2, col="black")
abline(a=1, b=-1, col="red")


plot(x=INLA_2001$Area, y=INLA_2001$Correct, xlab="Precision (Proportion total area)", ylab="Accuracy (Proportion correctly assigned)", xlim=c(0, 1), ylim=c(0,1), pch=19, cex=2, col="blue", cex.axis=1.5, cex.lab=1.75)
points(x=Krig_2001$Area, y=Krig_2001$Correct, pch=19, cex=2, col="black")
abline(coef=c(0,1), col="red", lty=2)

#2010

plot(x=(1/INLA_2010$Odds), y=INLA_2010$Correct, xlab="Odds Ratio", ylab="Proportion Correctly Assigned", axes=TRUE, xlim=c(0, 1), ylim=c(0,1), pch=19, cex=2, col="blue", cex.axis=1.5, cex.lab=1.75)
points(x=(1/Krig_2010$Odds), y=Krig_2010$Correct, pch=19, cex=2, col="black")
abline(a=1, b=-1, col="red")


plot(x=INLA_2010$Area, y=INLA_2010$Correct, xlab="Precision (Proportion total area)", ylab="Accuracy (Proportion correctly assigned)", xlim=c(0, 1), ylim=c(0,1), pch=19, cex=2, col="blue", cex.axis=1.5, cex.lab=1.75)
points(x=Krig_2010$Area, y=Krig_2010$Correct, pch=19, cex=2, col="black")
abline(coef=c(0,1), col="red", lty=2)


#UK shelf plots 
NS_01<-read.table("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/ResultsZ_INLA_UKShelf_2001_NSscallops_trial.txt", header=T)
EC_01<-read.table("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/ResultsZ_INLA_UKShelf_2001_EChanscallops_trial.txt", header=T)
IS_01<-read.table("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/ResultsZ_INLA_UKShelf_2001_Irishscallops_trial.txt", header=T)
NS_10<-read.table("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/ResultsZ_INLA_UKShelf_2010_NSscallops_trial.txt", header=T)
EC_10<-read.table("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/ResultsZ_INLA_UKShelf_2010_EChanscallops_trial.txt", header=T)
IS_10<-read.table("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/ResultsZ_INLA_UKShelf_2010_Irishscallops_trial.txt", header=T)

par(mfrow=c(1,3))
plot(x=NS_01$Area, y=NS_01$Correct, axes=TRUE, xlim=c(0, 1), ylim=c(0,1), pch=19, cex=1.5, col="black", cex.axis=1.5, cex.lab=1.75)
abline(coef=c(0,1), col="red", lty=2)
plot(x=EC_01$Area, y=EC_01$Correct, axes=TRUE, xlim=c(0, 1), ylim=c(0,1), pch=19, cex=1.5, col="black", cex.axis=1.5, cex.lab=1.75)
abline(coef=c(0,1), col="red", lty=2)
plot(x=IS_01$Area, y=IS_01$Correct, axes=TRUE, xlim=c(0, 1), ylim=c(0,1), pch=19, cex=1.5, col="black", cex.axis=1.5, cex.lab=1.75)
abline(coef=c(0,1), col="red", lty=2)
plot(x=NS_10$Area, y=NS_10$Correct, axes=TRUE, xlim=c(0, 1), ylim=c(0,1), pch=19, cex=1.5, col="black", cex.axis=1.5, cex.lab=1.75)
abline(coef=c(0,1), col="red", lty=2)
plot(x=EC_10$Area, y=EC_10$Correct, axes=TRUE, xlim=c(0, 1), ylim=c(0,1), pch=19, cex=1.5, col="black", cex.axis=1.5, cex.lab=1.75)
abline(coef=c(0,1), col="red", lty=2)
plot(x=IS_10$Area, y=IS_10$Correct, axes=TRUE, xlim=c(0, 1), ylim=c(0,1), pch=19, cex=1.5, col="black", cex.axis=1.5, cex.lab=1.75)
abline(coef=c(0,1), col="red", lty=2)
