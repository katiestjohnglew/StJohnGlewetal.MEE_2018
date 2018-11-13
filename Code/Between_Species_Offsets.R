#Trial2<-read.csv("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/Jellyfish_UKShelf/2017_ALL_JELLIES_UKSHELF/Code/Trial_Corrected.csv",header=T)

UK_data<-read.csv("/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/UKdata_Corrected_SCN.csv",header=T)
Trial2<-UK_data

library(plyr)

#Subset all data
NS<-subset(Trial2,Trial2$Region=="NS")
Celtic<-subset(Trial2,Trial2$Region=="Celtic")
Irish<-subset(Trial2,Trial2$Region=="Irish")
EChan<-subset(Trial2,Trial2$Region=="EChan")
West<-subset(Trial2,Trial2$Region=="West")

#NS - Carbon:

#Find all Rows where the Location ID is duplicated (the stations where multiple samples occur)
Stations<-NS$Location_ID[duplicated(NS$Location_ID)]
Stations
length(Stations)
Stations<- as.character(Stations)
Un_Stations<-unique(Stations)
Un_Stations

#[1] "SN1"  "SN6"  "SN7"  "SN8"  "SN2"  "SN20" "SN19" "SN11" "SN12" "SN13" "SN16" "SN27" "SN25" "SN26" "SN24" "SN32" "SN30"
#[18] "SN29" "SN28" "NN1"  "NN7"  "NN6"  "NN18" "NN4"  "NN39" "NN19" "NN23" "NN22" "NN37" "NN42" "NN41" "NN60" "NN43" "NN59"
#[35] "NN57" "NN58" "NN46" "NN49"

x<-unique(Stations)
Difference<-function(x){
for (i in 1:length(Stations)){
NN1<-subset(NS,NS$Location_ID==x[i])
NN1_L<-subset(NN1,NN1$Species=="Lion")
NN1_LC<-mean(NN1_L$d13Cc)
NN1_LN<-mean(NN1_L$d15N)
NN1_LS<-mean(NN1_L$d34S)

NN1_B<-subset(NN1,NN1$Species=="Barrel ")
NN1_BC<-mean(NN1_B$d13Cc)
NN1_BN<-mean(NN1_B$d15N)
NN1_BS<-mean(NN1_B$d34S)

NN1_C<-subset(NN1,NN1$Species=="Compass ")
NN1_CC<-mean(NN1_C$d13Cc)
NN1_CN<-mean(NN1_C$d15N)
NN1_CS<-mean(NN1_C$d34S)

NN1_M<-subset(NN1,NN1$Species=="Mauve")
NN1_MC<-mean(NN1_M$d13Cc)
NN1_MN<-mean(NN1_M$d15N)
NN1_MS<-mean(NN1_M$d34S)

NN1_Bl<-subset(NN1,NN1$Species=="Blue")
NN1_BlC<-mean(NN1_Bl$d13Cc)
NN1_BlN<-mean(NN1_Bl$d15N)
NN1_BlS<-mean(NN1_Bl$d34S)

NN1_Cr<-subset(NN1,NN1$Species=="Crystal")
NN1_CrC<-mean(NN1_Cr$d13Cc)
NN1_CrN<-mean(NN1_Cr$d15N)
NN1_CrS<-mean(NN1_Cr$d34S)

NN1_Mn<-subset(NN1,NN1$Species=="Moon")
NN1_MnC<-mean(NN1_Mn$d13Cc)
NN1_MnN<-mean(NN1_Mn$d15N)
NN1_MnS<-mean(NN1_Mn$d34S)

#Carbon Differences:

NN1_C_LB<-NN1_LC-NN1_BC
NN1_C_LC<-NN1_LC-NN1_CC
NN1_C_LM<-NN1_LC-NN1_MC
NN1_C_LBl<-NN1_LC-NN1_BlC
NN1_C_LCr<-NN1_LC-NN1_CrC
NN1_C_LMn<-NN1_LC-NN1_MnC
NN1_C_BC<-NN1_BC-NN1_CC
NN1_C_BM<-NN1_BC-NN1_MC
NN1_C_BBl<-NN1_BC-NN1_BlC
NN1_C_BCr<-NN1_BC-NN1_CrC
NN1_C_BMn<-NN1_BC-NN1_MnC
NN1_C_CM<-NN1_CC-NN1_MC
NN1_C_CBl<-NN1_CC-NN1_BlC
NN1_C_CCr<-NN1_CC-NN1_CrC
NN1_C_CMn<-NN1_CC-NN1_MnC
NN1_C_MBl<-NN1_MC-NN1_BlC
NN1_C_MCr<-NN1_MC-NN1_CrC
NN1_C_MMn<-NN1_MC-NN1_MnC
NN1_C_BlCr<-NN1_BlC-NN1_CrC
NN1_C_BlMn<-NN1_BlC-NN1_MnC
NN1_C_CrMn<-NN1_CrC-NN1_MnC



NN1_C<-c(NN1_C_LB,NN1_C_LC,NN1_C_LM,NN1_C_LBl,NN1_C_LCr,NN1_C_LMn,NN1_C_BC,NN1_C_BM,NN1_C_BBl,NN1_C_BCr,NN1_C_BMn,
         NN1_C_CM,NN1_C_CBl,NN1_C_CCr,NN1_C_CMn,NN1_C_MBl,NN1_C_MCr,NN1_C_MMn,NN1_C_BlCr,NN1_C_BlMn,
         NN1_C_CrMn)
NN1_C<-as.data.frame(NN1_C)
return(NN1_C)

}}

lapply(x,Difference)
Y<-ldply(x,Difference)

#[1] "SN1"  "SN6"  "SN7"  "SN8"  "SN2"  "SN20" "SN19" "SN11" "SN12" "SN13" "SN16" "SN27" "SN25" "SN26" "SN24" "SN32" "SN30"
#[18] "SN29" "SN28" "NN1"  "NN7"  "NN6"  "NN18" "NN4"  "NN39" "NN19" "NN23" "NN22" "NN37" "NN42" "NN41" "NN60" "NN43" "NN59"
#[35] "NN57" "NN58" "NN46" "NN49"

#Becareful of the order of stations!! 

SN1<-as.data.frame(Y[1:21,])
SN1
SN6<-as.data.frame(Y[22:42,])
SN6
SN7<-as.data.frame(Y[43:63,])
SN7
SN8<-as.data.frame(Y[64:84,])
SN8
SN2<-as.data.frame(Y[85:105,])
SN2
SN20<-as.data.frame(Y[106:126,])
SN20
SN19<-as.data.frame(Y[127:147,])
SN19
SN11<-as.data.frame(Y[148:168,])
SN11
SN12<-as.data.frame(Y[169:189,])
SN12
SN13<-as.data.frame(Y[190:210,])
SN13
SN16<-as.data.frame(Y[211:231,])
SN16
SN27<-as.data.frame(Y[232:252,])
SN27
SN25<-as.data.frame(Y[253:273,])
SN25
SN26<-as.data.frame(Y[274:294,])
SN26
SN24<-as.data.frame(Y[295:315,])
SN24
SN32<-as.data.frame(Y[316:336,])
SN32
SN30<-as.data.frame(Y[337:357,])
SN30
SN1<-as.data.frame(Y[358:378,])
SN1
SN7<-as.data.frame(Y[379:399,])
SN7
SN8<-as.data.frame(Y[400:420,])
SN8
SN19<-as.data.frame(Y[421:441,])
SN19
SN27<-as.data.frame(Y[442:462,])
SN27
SN30<-as.data.frame(Y[463:483,])
SN30
SN29<-as.data.frame(Y[484:504,])
SN29
SN28<-as.data.frame(Y[505:525,])
SN28
NN1<-as.data.frame(Y[526:546,])
NN1
NN7<-as.data.frame(Y[547:567,])
NN7
NN6<-as.data.frame(Y[568:588,])
NN6
NN18<-as.data.frame(Y[589:609,])
NN18
NN4<-as.data.frame(Y[610:630,])
NN4
NN39<-as.data.frame(Y[631:651,])
NN39
NN19<-as.data.frame(Y[652:672,])
NN19
NN23<-as.data.frame(Y[673:693,])
NN23
NN22<-as.data.frame(Y[694:714,])
NN22
NN37<-as.data.frame(Y[715:735,])
NN37
NN42<-as.data.frame(Y[736:756,])
NN42
NN41<-as.data.frame(Y[757:777,])
NN41
NN60<-as.data.frame(Y[778:798,])
NN60
NN43<-as.data.frame(Y[799:819,])
NN43
NN59<-as.data.frame(Y[820:840,])
NN59
NN57<-as.data.frame(Y[841:861,])
NN57
NN58<-as.data.frame(Y[862:882,])
NN58
NN46<-as.data.frame(Y[883:903,])
NN46
NN49<-as.data.frame(Y[904:924,])
NN49

NS_C<-cbind(SN1,SN6,SN7,SN8,SN2,SN20,SN19,SN11,SN12,SN13,SN16,SN27,SN25,SN26,SN24,SN32,SN30,
            SN29,SN28,NN1,NN7,NN6,NN18,NN4,NN39,NN19,NN23,NN22,NN37,NN42,NN41,NN60,NN43,NN59,
            NN57,NN58,NN46,NN49)

colnames(NS_C) <- c("SN1","SN6","SN7","SN8","SN2","SN20","SN19","SN11","SN12","SN13","SN16","SN27","SN25","SN26","SN24","SN32","SN30",
                    "SN29","SN28","NN1","NN7","NN6","NN18","NN4","NN39","NN19","NN23","NN22","NN37","NN42","NN41","NN60","NN43","NN59",
                    "NN57","NN58","NN46","NN49")

rownames(NS_C)<-c("L-B","L-C","L-M","L-Bl","L-Cr","L-Mn","B-C","B-M","B-Bl","B-Cr","B-Mn","C-M","C-Bl",
                 "C-Cr","C-Mn","M-Bl","M-Cr","M-Mn","Bl-Cr","Bl-Mn","Cr-Mn")

write.csv(NS_C, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Between_sp_var/Carbon_NorthSea_Species_Differences.csv")

################################################
#Nitrogen:
x<-unique(Stations)
Difference<-function(x){
  for (i in 1:length(Stations)){
    NN1<-subset(NS,NS$Location_ID==x[i])
    NN1_L<-subset(NN1,NN1$Species=="Lion")
    NN1_LC<-mean(NN1_L$d13Cc)
    NN1_LN<-mean(NN1_L$d15N)
    NN1_LS<-mean(NN1_L$d34S)
    
    NN1_B<-subset(NN1,NN1$Species=="Barrel ")
    NN1_BC<-mean(NN1_B$d13Cc)
    NN1_BN<-mean(NN1_B$d15N)
    NN1_BS<-mean(NN1_B$d34S)
    
    NN1_C<-subset(NN1,NN1$Species=="Compass ")
    NN1_CC<-mean(NN1_C$d13Cc)
    NN1_CN<-mean(NN1_C$d15N)
    NN1_CS<-mean(NN1_C$d34S)
    
    NN1_M<-subset(NN1,NN1$Species=="Mauve")
    NN1_MC<-mean(NN1_M$d13Cc)
    NN1_MN<-mean(NN1_M$d15N)
    NN1_MS<-mean(NN1_M$d34S)
    
    NN1_Bl<-subset(NN1,NN1$Species=="Blue")
    NN1_BlC<-mean(NN1_Bl$d13Cc)
    NN1_BlN<-mean(NN1_Bl$d15N)
    NN1_BlS<-mean(NN1_Bl$d34S)
    
    NN1_Cr<-subset(NN1,NN1$Species=="Crystal")
    NN1_CrC<-mean(NN1_Cr$d13Cc)
    NN1_CrN<-mean(NN1_Cr$d15N)
    NN1_CrS<-mean(NN1_Cr$d34S)
    
    NN1_Mn<-subset(NN1,NN1$Species=="Moon")
    NN1_MnC<-mean(NN1_Mn$d13Cc)
    NN1_MnN<-mean(NN1_Mn$d15N)
    NN1_MnS<-mean(NN1_Mn$d34S)
    
    
    #Nitrogen Differences:
    
    NN1_N_LB<-NN1_LN-NN1_BN
    NN1_N_LC<-NN1_LN-NN1_CN
    NN1_N_LM<-NN1_LN-NN1_MN
    NN1_N_LBl<-NN1_LN-NN1_BlN
    NN1_N_LCr<-NN1_LN-NN1_CrN
    NN1_N_LMn<-NN1_LN-NN1_MnN
    NN1_N_BC<-NN1_BN-NN1_CN
    NN1_N_BM<-NN1_BN-NN1_MN
    NN1_N_BBl<-NN1_BN-NN1_BlN
    NN1_N_BCr<-NN1_BN-NN1_CrN
    NN1_N_BMn<-NN1_BN-NN1_MnN
    NN1_N_CM<-NN1_CN-NN1_MN
    NN1_N_CBl<-NN1_CN-NN1_BlN
    NN1_N_CCr<-NN1_CN-NN1_CrN
    NN1_N_CMn<-NN1_CN-NN1_MnN
    NN1_N_MBl<-NN1_MN-NN1_BlN
    NN1_N_MCr<-NN1_MN-NN1_CrN
    NN1_N_MMn<-NN1_MN-NN1_MnN
    NN1_N_BlCr<-NN1_BlN-NN1_CrN
    NN1_N_BlMn<-NN1_BlN-NN1_MnN
    NN1_N_CrMn<-NN1_CrN-NN1_MnN
    
    NN1_N<-c(NN1_N_LB,NN1_N_LC,NN1_N_LM,NN1_N_LBl,NN1_N_LCr,NN1_N_LMn,NN1_N_BC,NN1_N_BM,NN1_N_BBl,NN1_N_BCr,NN1_N_BMn,
             NN1_N_CM,NN1_N_CBl,NN1_N_CCr,NN1_N_CMn,NN1_N_MBl,NN1_N_MCr,NN1_N_MMn,NN1_N_BlCr,NN1_N_BlMn,
             NN1_N_CrMn)
    NN1_N<-as.data.frame(NN1_N)
    return(NN1_N)
  }}

lapply(x,Difference)
Y<-ldply(x,Difference)

SN1<-as.data.frame(Y[1:21,])
SN1
SN6<-as.data.frame(Y[22:42,])
SN6
SN7<-as.data.frame(Y[43:63,])
SN7
SN8<-as.data.frame(Y[64:84,])
SN8
SN2<-as.data.frame(Y[85:105,])
SN2
SN20<-as.data.frame(Y[106:126,])
SN20
SN19<-as.data.frame(Y[127:147,])
SN19
SN11<-as.data.frame(Y[148:168,])
SN11
SN12<-as.data.frame(Y[169:189,])
SN12
SN13<-as.data.frame(Y[190:210,])
SN13
SN16<-as.data.frame(Y[211:231,])
SN16
SN27<-as.data.frame(Y[232:252,])
SN27
SN25<-as.data.frame(Y[253:273,])
SN25
SN26<-as.data.frame(Y[274:294,])
SN26
SN24<-as.data.frame(Y[295:315,])
SN24
SN32<-as.data.frame(Y[316:336,])
SN32
SN30<-as.data.frame(Y[337:357,])
SN30
SN1<-as.data.frame(Y[358:378,])
SN1
SN7<-as.data.frame(Y[379:399,])
SN7
SN8<-as.data.frame(Y[400:420,])
SN8
SN19<-as.data.frame(Y[421:441,])
SN19
SN27<-as.data.frame(Y[442:462,])
SN27
SN30<-as.data.frame(Y[463:483,])
SN30
SN29<-as.data.frame(Y[484:504,])
SN29
SN28<-as.data.frame(Y[505:525,])
SN28
NN1<-as.data.frame(Y[526:546,])
NN1
NN7<-as.data.frame(Y[547:567,])
NN7
NN6<-as.data.frame(Y[568:588,])
NN6
NN18<-as.data.frame(Y[589:609,])
NN18
NN4<-as.data.frame(Y[610:630,])
NN4
NN39<-as.data.frame(Y[631:651,])
NN39
NN19<-as.data.frame(Y[652:672,])
NN19
NN23<-as.data.frame(Y[673:693,])
NN23
NN22<-as.data.frame(Y[694:714,])
NN22
NN37<-as.data.frame(Y[715:735,])
NN37
NN42<-as.data.frame(Y[736:756,])
NN42
NN41<-as.data.frame(Y[757:777,])
NN41
NN60<-as.data.frame(Y[778:798,])
NN60
NN43<-as.data.frame(Y[799:819,])
NN43
NN59<-as.data.frame(Y[820:840,])
NN59
NN57<-as.data.frame(Y[841:861,])
NN57
NN58<-as.data.frame(Y[862:882,])
NN58
NN46<-as.data.frame(Y[883:903,])
NN46
NN49<-as.data.frame(Y[904:924,])
NN49

NS_N<-cbind(SN1,SN6,SN7,SN8,SN2,SN20,SN19,SN11,SN12,SN13,SN16,SN27,SN25,SN26,SN24,SN32,SN30,
            SN29,SN28,NN1,NN7,NN6,NN18,NN4,NN39,NN19,NN23,NN22,NN37,NN42,NN41,NN60,NN43,NN59,
            NN57,NN58,NN46,NN49)

colnames(NS_N) <- c("SN1","SN6","SN7","SN8","SN2","SN20","SN19","SN11","SN12","SN13","SN16","SN27","SN25","SN26","SN24","SN32","SN30",
                    "SN29","SN28","NN1","NN7","NN6","NN18","NN4","NN39","NN19","NN23","NN22","NN37","NN42","NN41","NN60","NN43","NN59",
                    "NN57","NN58","NN46","NN49")

rownames(NS_N)<-c("L-B","L-C","L-M","L-Bl","L-Cr","L-Mn","B-C","B-M","B-Bl","B-Cr","B-Mn","C-M","C-Bl",
                  "C-Cr","C-Mn","M-Bl","M-Cr","M-Mn","Bl-Cr","Bl-Mn","Cr-Mn")

write.csv(NS_N, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Between_sp_var/Nitrogen_NorthSea_Species_Differences.csv")

################################################
#Sulphur:
x<-unique(Stations)
Difference<-function(x){
  for (i in 1:length(Stations)){
    NN1<-subset(NS,NS$Location_ID==x[i])
    NN1_L<-subset(NN1,NN1$Species=="Lion")
    NN1_LC<-mean(NN1_L$d13Cc)
    NN1_LN<-mean(NN1_L$d15N)
    NN1_LS<-mean(NN1_L$d34S)
    
    NN1_B<-subset(NN1,NN1$Species=="Barrel ")
    NN1_BC<-mean(NN1_B$d13Cc)
    NN1_BN<-mean(NN1_B$d15N)
    NN1_BS<-mean(NN1_B$d34S)
    
    NN1_C<-subset(NN1,NN1$Species=="Compass ")
    NN1_CC<-mean(NN1_C$d13Cc)
    NN1_CN<-mean(NN1_C$d15N)
    NN1_CS<-mean(NN1_C$d34S)
    
    NN1_M<-subset(NN1,NN1$Species=="Mauve")
    NN1_MC<-mean(NN1_M$d13Cc)
    NN1_MN<-mean(NN1_M$d15N)
    NN1_MS<-mean(NN1_M$d34S)
    
    NN1_Bl<-subset(NN1,NN1$Species=="Blue")
    NN1_BlC<-mean(NN1_Bl$d13Cc)
    NN1_BlN<-mean(NN1_Bl$d15N)
    NN1_BlS<-mean(NN1_Bl$d34S)
    
    NN1_Cr<-subset(NN1,NN1$Species=="Crystal")
    NN1_CrC<-mean(NN1_Cr$d13Cc)
    NN1_CrN<-mean(NN1_Cr$d15N)
    NN1_CrS<-mean(NN1_Cr$d34S)
    
    NN1_Mn<-subset(NN1,NN1$Species=="Moon")
    NN1_MnC<-mean(NN1_Mn$d13Cc)
    NN1_MnN<-mean(NN1_Mn$d15N)
    NN1_MnS<-mean(NN1_Mn$d34S)
    
    
    #Sulphur Differences:
    
    NN1_S_LB<-NN1_LS-NN1_BS
    NN1_S_LC<-NN1_LS-NN1_CS
    NN1_S_LM<-NN1_LS-NN1_MS
    NN1_S_LBl<-NN1_LS-NN1_BlS
    NN1_S_LCr<-NN1_LS-NN1_CrS
    NN1_S_LMn<-NN1_LS-NN1_MnS
    NN1_S_BC<-NN1_BS-NN1_CS
    NN1_S_BM<-NN1_BS-NN1_MS
    NN1_S_BBl<-NN1_BS-NN1_BlS
    NN1_S_BCr<-NN1_BS-NN1_CrS
    NN1_S_BMn<-NN1_BS-NN1_MnS
    NN1_S_CM<-NN1_CS-NN1_MS
    NN1_S_CBl<-NN1_CS-NN1_BlS
    NN1_S_CCr<-NN1_CS-NN1_CrS
    NN1_S_CMn<-NN1_CS-NN1_MnS
    NN1_S_MBl<-NN1_MS-NN1_BlS
    NN1_S_MCr<-NN1_MS-NN1_CrS
    NN1_S_MMn<-NN1_MS-NN1_MnS
    NN1_S_BlCr<-NN1_BlS-NN1_CrS
    NN1_S_BlMn<-NN1_BlS-NN1_MnS
    NN1_S_CrMn<-NN1_CrS-NN1_MnS
    
    NN1_S<-c(NN1_S_LB,NN1_S_LC,NN1_S_LM,NN1_S_LBl,NN1_S_LCr,NN1_S_LMn,NN1_S_BC,NN1_S_BM,NN1_S_BBl,NN1_S_BCr,NN1_S_BMn,
             NN1_S_CM,NN1_S_CBl,NN1_S_CCr,NN1_S_CMn,NN1_S_MBl,NN1_S_MCr,NN1_S_MMn,NN1_S_BlCr,NN1_S_BlMn,
             NN1_S_CrMn)
    NN1_S<-as.data.frame(NN1_S)
    return(NN1_S)
  }}

lapply(x,Difference)
Y<-ldply(x,Difference)

SN1<-as.data.frame(Y[1:21,])
SN1
SN6<-as.data.frame(Y[22:42,])
SN6
SN7<-as.data.frame(Y[43:63,])
SN7
SN8<-as.data.frame(Y[64:84,])
SN8
SN2<-as.data.frame(Y[85:105,])
SN2
SN20<-as.data.frame(Y[106:126,])
SN20
SN19<-as.data.frame(Y[127:147,])
SN19
SN11<-as.data.frame(Y[148:168,])
SN11
SN12<-as.data.frame(Y[169:189,])
SN12
SN13<-as.data.frame(Y[190:210,])
SN13
SN16<-as.data.frame(Y[211:231,])
SN16
SN27<-as.data.frame(Y[232:252,])
SN27
SN25<-as.data.frame(Y[253:273,])
SN25
SN26<-as.data.frame(Y[274:294,])
SN26
SN24<-as.data.frame(Y[295:315,])
SN24
SN32<-as.data.frame(Y[316:336,])
SN32
SN30<-as.data.frame(Y[337:357,])
SN30
SN1<-as.data.frame(Y[358:378,])
SN1
SN7<-as.data.frame(Y[379:399,])
SN7
SN8<-as.data.frame(Y[400:420,])
SN8
SN19<-as.data.frame(Y[421:441,])
SN19
SN27<-as.data.frame(Y[442:462,])
SN27
SN30<-as.data.frame(Y[463:483,])
SN30
SN29<-as.data.frame(Y[484:504,])
SN29
SN28<-as.data.frame(Y[505:525,])
SN28
NN1<-as.data.frame(Y[526:546,])
NN1
NN7<-as.data.frame(Y[547:567,])
NN7
NN6<-as.data.frame(Y[568:588,])
NN6
NN18<-as.data.frame(Y[589:609,])
NN18
NN4<-as.data.frame(Y[610:630,])
NN4
NN39<-as.data.frame(Y[631:651,])
NN39
NN19<-as.data.frame(Y[652:672,])
NN19
NN23<-as.data.frame(Y[673:693,])
NN23
NN22<-as.data.frame(Y[694:714,])
NN22
NN37<-as.data.frame(Y[715:735,])
NN37
NN42<-as.data.frame(Y[736:756,])
NN42
NN41<-as.data.frame(Y[757:777,])
NN41
NN60<-as.data.frame(Y[778:798,])
NN60
NN43<-as.data.frame(Y[799:819,])
NN43
NN59<-as.data.frame(Y[820:840,])
NN59
NN57<-as.data.frame(Y[841:861,])
NN57
NN58<-as.data.frame(Y[862:882,])
NN58
NN46<-as.data.frame(Y[883:903,])
NN46
NN49<-as.data.frame(Y[904:924,])
NN49

NS_S<-cbind(SN1,SN6,SN7,SN8,SN2,SN20,SN19,SN11,SN12,SN13,SN16,SN27,SN25,SN26,SN24,SN32,SN30,
            SN29,SN28,NN1,NN7,NN6,NN18,NN4,NN39,NN19,NN23,NN22,NN37,NN42,NN41,NN60,NN43,NN59,
            NN57,NN58,NN46,NN49)

colnames(NS_S) <- c("SN1","SN6","SN7","SN8","SN2","SN20","SN19","SN11","SN12","SN13","SN16","SN27","SN25","SN26","SN24","SN32","SN30",
                    "SN29","SN28","NN1","NN7","NN6","NN18","NN4","NN39","NN19","NN23","NN22","NN37","NN42","NN41","NN60","NN43","NN59",
                    "NN57","NN58","NN46","NN49")

rownames(NS_S)<-c("L-B","L-C","L-M","L-Bl","L-Cr","L-Mn","B-C","B-M","B-Bl","B-Cr","B-Mn","C-M","C-Bl",
                  "C-Cr","C-Mn","M-Bl","M-Cr","M-Mn","Bl-Cr","Bl-Mn","Cr-Mn")

write.csv(NS_S, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Between_sp_var/Sulphur_NorthSea_Species_Differences.csv")

################################################################
###############################################################
#Irish - Carbon:

#Find all Rows where the Location ID is duplicated (the stations where multiple samples occur)
Stations<-Irish$Location_ID[duplicated(Irish$Location_ID)]
Stations
length(Stations)
Stations<- as.character(Stations)
Un_Stations<-unique(Stations)
Un_Stations

# "I52" "I10" "I47" "I16" "I43" "I27" "I21" "I19"

x<-unique(Stations)
Difference<-function(x){
  for (i in 1:length(Stations)){
    NN1<-subset(Irish,Irish$Location_ID==x[i])
    NN1_L<-subset(NN1,NN1$Species=="Lion")
    NN1_LC<-mean(NN1_L$d13Cc)
    NN1_LN<-mean(NN1_L$d15N)
    NN1_LS<-mean(NN1_L$d34S)
    
    NN1_B<-subset(NN1,NN1$Species=="Barrel ")
    NN1_BC<-mean(NN1_B$d13Cc)
    NN1_BN<-mean(NN1_B$d15N)
    NN1_BS<-mean(NN1_B$d34S)
    
    NN1_C<-subset(NN1,NN1$Species=="Compass ")
    NN1_CC<-mean(NN1_C$d13Cc)
    NN1_CN<-mean(NN1_C$d15N)
    NN1_CS<-mean(NN1_C$d34S)
    
    NN1_M<-subset(NN1,NN1$Species=="Mauve")
    NN1_MC<-mean(NN1_M$d13Cc)
    NN1_MN<-mean(NN1_M$d15N)
    NN1_MS<-mean(NN1_M$d34S)
    
    NN1_Bl<-subset(NN1,NN1$Species=="Blue")
    NN1_BlC<-mean(NN1_Bl$d13Cc)
    NN1_BlN<-mean(NN1_Bl$d15N)
    NN1_BlS<-mean(NN1_Bl$d34S)
    
    NN1_Cr<-subset(NN1,NN1$Species=="Crystal")
    NN1_CrC<-mean(NN1_Cr$d13Cc)
    NN1_CrN<-mean(NN1_Cr$d15N)
    NN1_CrS<-mean(NN1_Cr$d34S)
    
    NN1_Mn<-subset(NN1,NN1$Species=="Moon")
    NN1_MnC<-mean(NN1_Mn$d13Cc)
    NN1_MnN<-mean(NN1_Mn$d15N)
    NN1_MnS<-mean(NN1_Mn$d34S)
    
    #Carbon Differences:
    
    NN1_C_LB<-NN1_LC-NN1_BC
    NN1_C_LC<-NN1_LC-NN1_CC
    NN1_C_LM<-NN1_LC-NN1_MC
    NN1_C_LBl<-NN1_LC-NN1_BlC
    NN1_C_LCr<-NN1_LC-NN1_CrC
    NN1_C_LMn<-NN1_LC-NN1_MnC
    NN1_C_BC<-NN1_BC-NN1_CC
    NN1_C_BM<-NN1_BC-NN1_MC
    NN1_C_BBl<-NN1_BC-NN1_BlC
    NN1_C_BCr<-NN1_BC-NN1_CrC
    NN1_C_BMn<-NN1_BC-NN1_MnC
    NN1_C_CM<-NN1_CC-NN1_MC
    NN1_C_CBl<-NN1_CC-NN1_BlC
    NN1_C_CCr<-NN1_CC-NN1_CrC
    NN1_C_CMn<-NN1_CC-NN1_MnC
    NN1_C_MBl<-NN1_MC-NN1_BlC
    NN1_C_MCr<-NN1_MC-NN1_CrC
    NN1_C_MMn<-NN1_MC-NN1_MnC
    NN1_C_BlCr<-NN1_BlC-NN1_CrC
    NN1_C_BlMn<-NN1_BlC-NN1_MnC
    NN1_C_CrMn<-NN1_CrC-NN1_MnC
    
    NN1_C<-c(NN1_C_LB,NN1_C_LC,NN1_C_LM,NN1_C_LBl,NN1_C_LCr,NN1_C_LMn,NN1_C_BC,NN1_C_BM,NN1_C_BBl,NN1_C_BCr,NN1_C_BMn,
             NN1_C_CM,NN1_C_CBl,NN1_C_CCr,NN1_C_CMn,NN1_C_MBl,NN1_C_MCr,NN1_C_MMn,NN1_C_BlCr,NN1_C_BlMn,
             NN1_C_CrMn)
    NN1_C<-as.data.frame(NN1_C)
    return(NN1_C)
  }}

lapply(x,Difference)
Y<-ldply(x,Difference)

# "I52" "I10" "I47" "I16" "I43" "I27" "I21" "I19"

I52<-as.data.frame(Y[1:21,])
I52
I10<-as.data.frame(Y[22:42,])
I10
I47<-as.data.frame(Y[43:63,])
I47
I16<-as.data.frame(Y[64:84,])
I16
I43<-as.data.frame(Y[85:105,])
I43
I27<-as.data.frame(Y[106:126,])
I27
I21<-as.data.frame(Y[127:147,])
I21
I19<-as.data.frame(Y[148:168,])
I19

Irish_C<-cbind(I52,I10,I47,I16,I43,I27,I21,I19)

colnames(Irish_C) <- c("I52","I10","I47","I16","I43","I27","I21","I19")

rownames(Irish_C)<-c("L-B","L-C","L-M","L-Bl","L-Cr","L-Mn","B-C","B-M","B-Bl","B-Cr","B-Mn","C-M","C-Bl","C-Cr","C-Mn","M-Bl","M-Cr","M-Mn","Bl-Cr","Bl-Mn","Cr-Mn")

write.csv(Irish_C, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Between_sp_var/Carbon_Irish_Species_Differences.csv")

################################################
#Nitrogen:
x<-unique(Stations)
Difference<-function(x){
  for (i in 1:length(Stations)){
    NN1<-subset(Irish,Irish$Location_ID==x[i])
    NN1_L<-subset(NN1,NN1$Species=="Lion")
    NN1_LC<-mean(NN1_L$d13Cc)
    NN1_LN<-mean(NN1_L$d15N)
    NN1_LS<-mean(NN1_L$d34S)
    
    NN1_B<-subset(NN1,NN1$Species=="Barrel ")
    NN1_BC<-mean(NN1_B$d13Cc)
    NN1_BN<-mean(NN1_B$d15N)
    NN1_BS<-mean(NN1_B$d34S)
    
    NN1_C<-subset(NN1,NN1$Species=="Compass ")
    NN1_CC<-mean(NN1_C$d13Cc)
    NN1_CN<-mean(NN1_C$d15N)
    NN1_CS<-mean(NN1_C$d34S)
    
    NN1_M<-subset(NN1,NN1$Species=="Mauve")
    NN1_MC<-mean(NN1_M$d13Cc)
    NN1_MN<-mean(NN1_M$d15N)
    NN1_MS<-mean(NN1_M$d34S)
    
    NN1_Bl<-subset(NN1,NN1$Species=="Blue")
    NN1_BlC<-mean(NN1_Bl$d13Cc)
    NN1_BlN<-mean(NN1_Bl$d15N)
    NN1_BlS<-mean(NN1_Bl$d34S)
    
    NN1_Cr<-subset(NN1,NN1$Species=="Crystal")
    NN1_CrC<-mean(NN1_Cr$d13Cc)
    NN1_CrN<-mean(NN1_Cr$d15N)
    NN1_CrS<-mean(NN1_Cr$d34S)
    
    NN1_Mn<-subset(NN1,NN1$Species=="Moon")
    NN1_MnC<-mean(NN1_Mn$d13Cc)
    NN1_MnN<-mean(NN1_Mn$d15N)
    NN1_MnS<-mean(NN1_Mn$d34S)
    
    
    #Nitrogen Differences:
    
    NN1_N_LB<-NN1_LN-NN1_BN
    NN1_N_LC<-NN1_LN-NN1_CN
    NN1_N_LM<-NN1_LN-NN1_MN
    NN1_N_LBl<-NN1_LN-NN1_BlN
    NN1_N_LCr<-NN1_LN-NN1_CrN
    NN1_N_LMn<-NN1_LN-NN1_MnN
    NN1_N_BC<-NN1_BN-NN1_CN
    NN1_N_BM<-NN1_BN-NN1_MN
    NN1_N_BBl<-NN1_BN-NN1_BlN
    NN1_N_BCr<-NN1_BN-NN1_CrN
    NN1_N_BMn<-NN1_BN-NN1_MnN
    NN1_N_CM<-NN1_CN-NN1_MN
    NN1_N_CBl<-NN1_CN-NN1_BlN
    NN1_N_CCr<-NN1_CN-NN1_CrN
    NN1_N_CMn<-NN1_CN-NN1_MnN
    NN1_N_MBl<-NN1_MN-NN1_BlN
    NN1_N_MCr<-NN1_MN-NN1_CrN
    NN1_N_MMn<-NN1_MN-NN1_MnN
    NN1_N_BlCr<-NN1_BlN-NN1_CrN
    NN1_N_BlMn<-NN1_BlN-NN1_MnN
    NN1_N_CrMn<-NN1_CrN-NN1_MnN
    
    NN1_N<-c(NN1_N_LB,NN1_N_LC,NN1_N_LM,NN1_N_LBl,NN1_N_LCr,NN1_N_LMn,NN1_N_BC,NN1_N_BM,NN1_N_BBl,NN1_N_BCr,NN1_N_BMn,
             NN1_N_CM,NN1_N_CBl,NN1_N_CCr,NN1_N_CMn,NN1_N_MBl,NN1_N_MCr,NN1_N_MMn,NN1_N_BlCr,NN1_N_BlMn,
             NN1_N_CrMn)
    NN1_N<-as.data.frame(NN1_N)
    return(NN1_N)
  }}

lapply(x,Difference)
Y<-ldply(x,Difference)


I52<-as.data.frame(Y[1:21,])
I52
I10<-as.data.frame(Y[22:42,])
I10
I47<-as.data.frame(Y[43:63,])
I47
I16<-as.data.frame(Y[64:84,])
I16
I43<-as.data.frame(Y[85:105,])
I43
I27<-as.data.frame(Y[106:126,])
I27
I21<-as.data.frame(Y[127:147,])
I21
I19<-as.data.frame(Y[148:168,])
I19

Irish_N<-cbind(I52,I10,I47,I16,I43,I27,I21,I19)

colnames(Irish_N) <- c("I52","I10","I47","I16","I43","I27","I21","I19")

rownames(Irish_N)<-c("L-B","L-C","L-M","L-Bl","L-Cr","L-Mn","B-C","B-M","B-Bl","B-Cr","B-Mn","C-M","C-Bl","C-Cr","C-Mn","M-Bl","M-Cr","M-Mn","Bl-Cr","Bl-Mn","Cr-Mn")

write.csv(Irish_N, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Between_sp_var/Nitrogen_Irish_Species_Differences.csv")

################################################
#Sulphur:
x<-unique(Stations)
Difference<-function(x){
  for (i in 1:length(Stations)){
    NN1<-subset(Irish,Irish$Location_ID==x[i])
    NN1_L<-subset(NN1,NN1$Species=="Lion")
    NN1_LC<-mean(NN1_L$d13Cc)
    NN1_LN<-mean(NN1_L$d15N)
    NN1_LS<-mean(NN1_L$d34S)
    
    NN1_B<-subset(NN1,NN1$Species=="Barrel ")
    NN1_BC<-mean(NN1_B$d13Cc)
    NN1_BN<-mean(NN1_B$d15N)
    NN1_BS<-mean(NN1_B$d34S)
    
    NN1_C<-subset(NN1,NN1$Species=="Compass ")
    NN1_CC<-mean(NN1_C$d13Cc)
    NN1_CN<-mean(NN1_C$d15N)
    NN1_CS<-mean(NN1_C$d34S)
    
    NN1_M<-subset(NN1,NN1$Species=="Mauve")
    NN1_MC<-mean(NN1_M$d13Cc)
    NN1_MN<-mean(NN1_M$d15N)
    NN1_MS<-mean(NN1_M$d34S)
    
    NN1_Bl<-subset(NN1,NN1$Species=="Blue")
    NN1_BlC<-mean(NN1_Bl$d13Cc)
    NN1_BlN<-mean(NN1_Bl$d15N)
    NN1_BlS<-mean(NN1_Bl$d34S)
    
    NN1_Cr<-subset(NN1,NN1$Species=="Crystal")
    NN1_CrC<-mean(NN1_Cr$d13Cc)
    NN1_CrN<-mean(NN1_Cr$d15N)
    NN1_CrS<-mean(NN1_Cr$d34S)
    
    NN1_Mn<-subset(NN1,NN1$Species=="Moon")
    NN1_MnC<-mean(NN1_Mn$d13Cc)
    NN1_MnN<-mean(NN1_Mn$d15N)
    NN1_MnS<-mean(NN1_Mn$d34S)
    
    
    #Sulphur Differences:
    
    NN1_S_LB<-NN1_LS-NN1_BS
    NN1_S_LC<-NN1_LS-NN1_CS
    NN1_S_LM<-NN1_LS-NN1_MS
    NN1_S_LBl<-NN1_LS-NN1_BlS
    NN1_S_LCr<-NN1_LS-NN1_CrS
    NN1_S_LMn<-NN1_LS-NN1_MnS
    NN1_S_BC<-NN1_BS-NN1_CS
    NN1_S_BM<-NN1_BS-NN1_MS
    NN1_S_BBl<-NN1_BS-NN1_BlS
    NN1_S_BCr<-NN1_BS-NN1_CrS
    NN1_S_BMn<-NN1_BS-NN1_MnS
    NN1_S_CM<-NN1_CS-NN1_MS
    NN1_S_CBl<-NN1_CS-NN1_BlS
    NN1_S_CCr<-NN1_CS-NN1_CrS
    NN1_S_CMn<-NN1_CS-NN1_MnS
    NN1_S_MBl<-NN1_MS-NN1_BlS
    NN1_S_MCr<-NN1_MS-NN1_CrS
    NN1_S_MMn<-NN1_MS-NN1_MnS
    NN1_S_BlCr<-NN1_BlS-NN1_CrS
    NN1_S_BlMn<-NN1_BlS-NN1_MnS
    NN1_S_CrMn<-NN1_CrS-NN1_MnS
    
    NN1_S<-c(NN1_S_LB,NN1_S_LC,NN1_S_LM,NN1_S_LBl,NN1_S_LCr,NN1_S_LMn,NN1_S_BC,NN1_S_BM,NN1_S_BBl,NN1_S_BCr,NN1_S_BMn,
             NN1_S_CM,NN1_S_CBl,NN1_S_CCr,NN1_S_CMn,NN1_S_MBl,NN1_S_MCr,NN1_S_MMn,NN1_S_BlCr,NN1_S_BlMn,
             NN1_S_CrMn)
    NN1_S<-as.data.frame(NN1_S)
    return(NN1_S)
  }}

lapply(x,Difference)
Y<-ldply(x,Difference)

I52<-as.data.frame(Y[1:21,])
I52
I10<-as.data.frame(Y[22:42,])
I10
I47<-as.data.frame(Y[43:63,])
I47
I16<-as.data.frame(Y[64:84,])
I16
I43<-as.data.frame(Y[85:105,])
I43
I27<-as.data.frame(Y[106:126,])
I27
I21<-as.data.frame(Y[127:147,])
I21
I19<-as.data.frame(Y[148:168,])
I19

Irish_S<-cbind(I52,I10,I47,I16,I43,I27,I21,I19)

colnames(Irish_S) <- c("I52","I10","I47","I16","I43","I27","I21","I19")

rownames(Irish_S)<-c("L-B","L-C","L-M","L-Bl","L-Cr","L-Mn","B-C","B-M","B-Bl","B-Cr","B-Mn","C-M","C-Bl","C-Cr","C-Mn","M-Bl","M-Cr","M-Mn","Bl-Cr","Bl-Mn","Cr-Mn")


write.csv(Irish_S, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Between_sp_var/Sulphur_Irish_Species_Differences.csv")


###############################################################
#EChan - Carbon:

#Find all Rows where the Location ID is duplicated (the stations where multiple samples occur)
Stations<-EChan$Location_ID[duplicated(EChan$Location_ID)]
Stations
length(Stations)
Stations<- as.character(Stations)
Un_Stations<-unique(Stations)
Un_Stations

# "E109" "E98"  "E35"  "E46"  "E28"  "E36"  "E49"  "E30"  "E24"  "E41"  "E43"  "E44"  "E21"  "E6"   "E20"  "E2"   "E15" 

x<-unique(Stations)
Difference<-function(x){
  for (i in 1:length(Stations)){
    NN1<-subset(EChan,EChan$Location_ID==x[i])
    NN1_L<-subset(NN1,NN1$Species=="Lion")
    NN1_LC<-mean(NN1_L$d13Cc)
    NN1_LN<-mean(NN1_L$d15N)
    NN1_LS<-mean(NN1_L$d34S)
    
    NN1_B<-subset(NN1,NN1$Species=="Barrel ")
    NN1_BC<-mean(NN1_B$d13Cc)
    NN1_BN<-mean(NN1_B$d15N)
    NN1_BS<-mean(NN1_B$d34S)
    
    NN1_C<-subset(NN1,NN1$Species=="Compass ")
    NN1_CC<-mean(NN1_C$d13Cc)
    NN1_CN<-mean(NN1_C$d15N)
    NN1_CS<-mean(NN1_C$d34S)
    
    NN1_M<-subset(NN1,NN1$Species=="Mauve")
    NN1_MC<-mean(NN1_M$d13Cc)
    NN1_MN<-mean(NN1_M$d15N)
    NN1_MS<-mean(NN1_M$d34S)
    
    NN1_Bl<-subset(NN1,NN1$Species=="Blue")
    NN1_BlC<-mean(NN1_Bl$d13Cc)
    NN1_BlN<-mean(NN1_Bl$d15N)
    NN1_BlS<-mean(NN1_Bl$d34S)
    
    NN1_Cr<-subset(NN1,NN1$Species=="Crystal")
    NN1_CrC<-mean(NN1_Cr$d13Cc)
    NN1_CrN<-mean(NN1_Cr$d15N)
    NN1_CrS<-mean(NN1_Cr$d34S)
    
    NN1_Mn<-subset(NN1,NN1$Species=="Moon")
    NN1_MnC<-mean(NN1_Mn$d13Cc)
    NN1_MnN<-mean(NN1_Mn$d15N)
    NN1_MnS<-mean(NN1_Mn$d34S)
    
    #Carbon Differences:
    
    NN1_C_LB<-NN1_LC-NN1_BC
    NN1_C_LC<-NN1_LC-NN1_CC
    NN1_C_LM<-NN1_LC-NN1_MC
    NN1_C_LBl<-NN1_LC-NN1_BlC
    NN1_C_LCr<-NN1_LC-NN1_CrC
    NN1_C_LMn<-NN1_LC-NN1_MnC
    NN1_C_BC<-NN1_BC-NN1_CC
    NN1_C_BM<-NN1_BC-NN1_MC
    NN1_C_BBl<-NN1_BC-NN1_BlC
    NN1_C_BCr<-NN1_BC-NN1_CrC
    NN1_C_BMn<-NN1_BC-NN1_MnC
    NN1_C_CM<-NN1_CC-NN1_MC
    NN1_C_CBl<-NN1_CC-NN1_BlC
    NN1_C_CCr<-NN1_CC-NN1_CrC
    NN1_C_CMn<-NN1_CC-NN1_MnC
    NN1_C_MBl<-NN1_MC-NN1_BlC
    NN1_C_MCr<-NN1_MC-NN1_CrC
    NN1_C_MMn<-NN1_MC-NN1_MnC
    NN1_C_BlCr<-NN1_BlC-NN1_CrC
    NN1_C_BlMn<-NN1_BlC-NN1_MnC
    NN1_C_CrMn<-NN1_CrC-NN1_MnC
    
    NN1_C<-c(NN1_C_LB,NN1_C_LC,NN1_C_LM,NN1_C_LBl,NN1_C_LCr,NN1_C_LMn,NN1_C_BC,NN1_C_BM,NN1_C_BBl,NN1_C_BCr,NN1_C_BMn,
             NN1_C_CM,NN1_C_CBl,NN1_C_CCr,NN1_C_CMn,NN1_C_MBl,NN1_C_MCr,NN1_C_MMn,NN1_C_BlCr,NN1_C_BlMn,
             NN1_C_CrMn)
    NN1_C<-as.data.frame(NN1_C)
    return(NN1_C)
  }}

lapply(x,Difference)
Y<-ldply(x,Difference)

# "E109" "E98"  "E35"  "E46"  "E28"  "E36"  "E49"  "E30"  "E24"  "E41"  "E43"  "E44"  "E21"  "E6"   "E20"  "E2"   "E15" 

E109<-as.data.frame(Y[1:21,])
E109
E98<-as.data.frame(Y[22:42,])
E98
E35<-as.data.frame(Y[43:63,])
E35
E46<-as.data.frame(Y[64:84,])
E46
E28<-as.data.frame(Y[85:105,])
E28
E36<-as.data.frame(Y[106:126,])
E36
E49<-as.data.frame(Y[127:147,])
E49
E30<-as.data.frame(Y[148:168,])
E30
E24<-as.data.frame(Y[169:189,])
E24
E41<-as.data.frame(Y[190:210,])
E41
E43<-as.data.frame(Y[211:231,])
E43
E44<-as.data.frame(Y[232:252,])
E44
E21<-as.data.frame(Y[253:273,])
E21
E6<-as.data.frame(Y[274:294,])
E6
E20<-as.data.frame(Y[295:315,])
E20
E2<-as.data.frame(Y[316:336,])
E2
E15<-as.data.frame(Y[337:357,])
E15


EChan_C<-cbind(E109,E98,E35,E46,E28,E36,E49,E30,E24,E41,E43,E44,E21,E6,E20,E2,E15)

colnames(EChan_C) <- c("E109","E98","E35","E46","E28","E36","E49","E30","E24","E41","E43","E44","E21","E6","E20","E2","E15")

rownames(EChan_C)<-c("L-B","L-C","L-M","L-Bl","L-Cr","L-Mn","B-C","B-M","B-Bl","B-Cr","B-Mn","C-M","C-Bl","C-Cr","C-Mn","M-Bl","M-Cr","M-Mn","Bl-Cr","Bl-Mn","Cr-Mn")


write.csv(EChan_C, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Between_sp_var/Carbon_EChan_Species_Differences.csv")

################################################
#Nitrogen:
x<-unique(Stations)
Difference<-function(x){
  for (i in 1:length(Stations)){
    NN1<-subset(EChan,EChan$Location_ID==x[i])
    NN1_L<-subset(NN1,NN1$Species=="Lion")
    NN1_LC<-mean(NN1_L$d13Cc)
    NN1_LN<-mean(NN1_L$d15N)
    NN1_LS<-mean(NN1_L$d34S)
    
    NN1_B<-subset(NN1,NN1$Species=="Barrel ")
    NN1_BC<-mean(NN1_B$d13Cc)
    NN1_BN<-mean(NN1_B$d15N)
    NN1_BS<-mean(NN1_B$d34S)
    
    NN1_C<-subset(NN1,NN1$Species=="Compass ")
    NN1_CC<-mean(NN1_C$d13Cc)
    NN1_CN<-mean(NN1_C$d15N)
    NN1_CS<-mean(NN1_C$d34S)
    
    NN1_M<-subset(NN1,NN1$Species=="Mauve")
    NN1_MC<-mean(NN1_M$d13Cc)
    NN1_MN<-mean(NN1_M$d15N)
    NN1_MS<-mean(NN1_M$d34S)
    
    NN1_Bl<-subset(NN1,NN1$Species=="Blue")
    NN1_BlC<-mean(NN1_Bl$d13Cc)
    NN1_BlN<-mean(NN1_Bl$d15N)
    NN1_BlS<-mean(NN1_Bl$d34S)
    
    NN1_Cr<-subset(NN1,NN1$Species=="Crystal")
    NN1_CrC<-mean(NN1_Cr$d13Cc)
    NN1_CrN<-mean(NN1_Cr$d15N)
    NN1_CrS<-mean(NN1_Cr$d34S)
    
    NN1_Mn<-subset(NN1,NN1$Species=="Moon")
    NN1_MnC<-mean(NN1_Mn$d13Cc)
    NN1_MnN<-mean(NN1_Mn$d15N)
    NN1_MnS<-mean(NN1_Mn$d34S)
    
    
    #Nitrogen Differences:
    
    NN1_N_LB<-NN1_LN-NN1_BN
    NN1_N_LC<-NN1_LN-NN1_CN
    NN1_N_LM<-NN1_LN-NN1_MN
    NN1_N_LBl<-NN1_LN-NN1_BlN
    NN1_N_LCr<-NN1_LN-NN1_CrN
    NN1_N_LMn<-NN1_LN-NN1_MnN
    NN1_N_BC<-NN1_BN-NN1_CN
    NN1_N_BM<-NN1_BN-NN1_MN
    NN1_N_BBl<-NN1_BN-NN1_BlN
    NN1_N_BCr<-NN1_BN-NN1_CrN
    NN1_N_BMn<-NN1_BN-NN1_MnN
    NN1_N_CM<-NN1_CN-NN1_MN
    NN1_N_CBl<-NN1_CN-NN1_BlN
    NN1_N_CCr<-NN1_CN-NN1_CrN
    NN1_N_CMn<-NN1_CN-NN1_MnN
    NN1_N_MBl<-NN1_MN-NN1_BlN
    NN1_N_MCr<-NN1_MN-NN1_CrN
    NN1_N_MMn<-NN1_MN-NN1_MnN
    NN1_N_BlCr<-NN1_BlN-NN1_CrN
    NN1_N_BlMn<-NN1_BlN-NN1_MnN
    NN1_N_CrMn<-NN1_CrN-NN1_MnN
    
    NN1_N<-c(NN1_N_LB,NN1_N_LC,NN1_N_LM,NN1_N_LBl,NN1_N_LCr,NN1_N_LMn,NN1_N_BC,NN1_N_BM,NN1_N_BBl,NN1_N_BCr,NN1_N_BMn,
             NN1_N_CM,NN1_N_CBl,NN1_N_CCr,NN1_N_CMn,NN1_N_MBl,NN1_N_MCr,NN1_N_MMn,NN1_N_BlCr,NN1_N_BlMn,
             NN1_N_CrMn)
    NN1_N<-as.data.frame(NN1_N)
    return(NN1_N)
  }}

lapply(x,Difference)
Y<-ldply(x,Difference)

E109<-as.data.frame(Y[1:21,])
E109
E98<-as.data.frame(Y[22:42,])
E98
E35<-as.data.frame(Y[43:63,])
E35
E46<-as.data.frame(Y[64:84,])
E46
E28<-as.data.frame(Y[85:105,])
E28
E36<-as.data.frame(Y[106:126,])
E36
E49<-as.data.frame(Y[127:147,])
E49
E30<-as.data.frame(Y[148:168,])
E30
E24<-as.data.frame(Y[169:189,])
E24
E41<-as.data.frame(Y[190:210,])
E41
E43<-as.data.frame(Y[211:231,])
E43
E44<-as.data.frame(Y[232:252,])
E44
E21<-as.data.frame(Y[253:273,])
E21
E6<-as.data.frame(Y[274:294,])
E6
E20<-as.data.frame(Y[295:315,])
E20
E2<-as.data.frame(Y[316:336,])
E2
E15<-as.data.frame(Y[337:357,])
E15


EChan_N<-cbind(E109,E98,E35,E46,E28,E36,E49,E30,E24,E41,E43,E44,E21,E6,E20,E2,E15)

colnames(EChan_N) <- c("E109","E98","E35","E46","E28","E36","E49","E30","E24","E41","E43","E44","E21","E6","E20","E2","E15")

rownames(EChan_N)<-c("L-B","L-C","L-M","L-Bl","L-Cr","L-Mn","B-C","B-M","B-Bl","B-Cr","B-Mn","C-M","C-Bl","C-Cr","C-Mn","M-Bl","M-Cr","M-Mn","Bl-Cr","Bl-Mn","Cr-Mn")



write.csv(EChan_N, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Between_sp_var/Nitrogen_EChan_Species_Differences.xlsx")

################################################
#Sulphur:
x<-unique(Stations)
Difference<-function(x){
  for (i in 1:length(Stations)){
    NN1<-subset(EChan,EChan$Location_ID==x[i])
    NN1_L<-subset(NN1,NN1$Species=="Lion")
    NN1_LC<-mean(NN1_L$d13Cc)
    NN1_LN<-mean(NN1_L$d15N)
    NN1_LS<-mean(NN1_L$d34S)
    
    NN1_B<-subset(NN1,NN1$Species=="Barrel ")
    NN1_BC<-mean(NN1_B$d13Cc)
    NN1_BN<-mean(NN1_B$d15N)
    NN1_BS<-mean(NN1_B$d34S)
    
    NN1_C<-subset(NN1,NN1$Species=="Compass ")
    NN1_CC<-mean(NN1_C$d13Cc)
    NN1_CN<-mean(NN1_C$d15N)
    NN1_CS<-mean(NN1_C$d34S)
    
    NN1_M<-subset(NN1,NN1$Species=="Mauve")
    NN1_MC<-mean(NN1_M$d13Cc)
    NN1_MN<-mean(NN1_M$d15N)
    NN1_MS<-mean(NN1_M$d34S)
    
    NN1_Bl<-subset(NN1,NN1$Species=="Blue")
    NN1_BlC<-mean(NN1_Bl$d13Cc)
    NN1_BlN<-mean(NN1_Bl$d15N)
    NN1_BlS<-mean(NN1_Bl$d34S)
    
    NN1_Cr<-subset(NN1,NN1$Species=="Crystal")
    NN1_CrC<-mean(NN1_Cr$d13Cc)
    NN1_CrN<-mean(NN1_Cr$d15N)
    NN1_CrS<-mean(NN1_Cr$d34S)
    
    NN1_Mn<-subset(NN1,NN1$Species=="Moon")
    NN1_MnC<-mean(NN1_Mn$d13Cc)
    NN1_MnN<-mean(NN1_Mn$d15N)
    NN1_MnS<-mean(NN1_Mn$d34S)
    
    
    #Sulphur Differences:
    
    NN1_S_LB<-NN1_LS-NN1_BS
    NN1_S_LC<-NN1_LS-NN1_CS
    NN1_S_LM<-NN1_LS-NN1_MS
    NN1_S_LBl<-NN1_LS-NN1_BlS
    NN1_S_LCr<-NN1_LS-NN1_CrS
    NN1_S_LMn<-NN1_LS-NN1_MnS
    NN1_S_BC<-NN1_BS-NN1_CS
    NN1_S_BM<-NN1_BS-NN1_MS
    NN1_S_BBl<-NN1_BS-NN1_BlS
    NN1_S_BCr<-NN1_BS-NN1_CrS
    NN1_S_BMn<-NN1_BS-NN1_MnS
    NN1_S_CM<-NN1_CS-NN1_MS
    NN1_S_CBl<-NN1_CS-NN1_BlS
    NN1_S_CCr<-NN1_CS-NN1_CrS
    NN1_S_CMn<-NN1_CS-NN1_MnS
    NN1_S_MBl<-NN1_MS-NN1_BlS
    NN1_S_MCr<-NN1_MS-NN1_CrS
    NN1_S_MMn<-NN1_MS-NN1_MnS
    NN1_S_BlCr<-NN1_BlS-NN1_CrS
    NN1_S_BlMn<-NN1_BlS-NN1_MnS
    NN1_S_CrMn<-NN1_CrS-NN1_MnS
    
    NN1_S<-c(NN1_S_LB,NN1_S_LC,NN1_S_LM,NN1_S_LBl,NN1_S_LCr,NN1_S_LMn,NN1_S_BC,NN1_S_BM,NN1_S_BBl,NN1_S_BCr,NN1_S_BMn,
             NN1_S_CM,NN1_S_CBl,NN1_S_CCr,NN1_S_CMn,NN1_S_MBl,NN1_S_MCr,NN1_S_MMn,NN1_S_BlCr,NN1_S_BlMn,
             NN1_S_CrMn)
    NN1_S<-as.data.frame(NN1_S)
    return(NN1_S)
  }}

lapply(x,Difference)
Y<-ldply(x,Difference)

E109<-as.data.frame(Y[1:21,])
E109
E98<-as.data.frame(Y[22:42,])
E98
E35<-as.data.frame(Y[43:63,])
E35
E46<-as.data.frame(Y[64:84,])
E46
E28<-as.data.frame(Y[85:105,])
E28
E36<-as.data.frame(Y[106:126,])
E36
E49<-as.data.frame(Y[127:147,])
E49
E30<-as.data.frame(Y[148:168,])
E30
E24<-as.data.frame(Y[169:189,])
E24
E41<-as.data.frame(Y[190:210,])
E41
E43<-as.data.frame(Y[211:231,])
E43
E44<-as.data.frame(Y[232:252,])
E44
E21<-as.data.frame(Y[253:273,])
E21
E6<-as.data.frame(Y[274:294,])
E6
E20<-as.data.frame(Y[295:315,])
E20
E2<-as.data.frame(Y[316:336,])
E2
E15<-as.data.frame(Y[337:357,])
E15


EChan_S<-cbind(E109,E98,E35,E46,E28,E36,E49,E30,E24,E41,E43,E44,E21,E6,E20,E2,E15)

colnames(EChan_S) <- c("E109","E98","E35","E46","E28","E36","E49","E30","E24","E41","E43","E44","E21","E6","E20","E2","E15")

rownames(EChan_S)<-c("L-B","L-C","L-M","L-Bl","L-Cr","L-Mn","B-C","B-M","B-Bl","B-Cr","B-Mn","C-M","C-Bl","C-Cr","C-Mn","M-Bl","M-Cr","M-Mn","Bl-Cr","Bl-Mn","Cr-Mn")



write.csv(EChan_S, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Between_sp_var/Sulphur_EChan_Species_Differences.csv")

################################################################
###############################################################
#Celtic - Carbon:

#Find all Rows where the Location ID is duplicated (the stations where multiple samples occur)
Stations<-Celtic$Location_ID[duplicated(Celtic$Location_ID)]
Stations
length(Stations)
Stations<- as.character(Stations)
Un_Stations<-unique(Stations)
Un_Stations

#[1] "C150" "C19"  "C10"  "C1"   "C121" "C122" "C14"  "C17"  "C148" "C123" "C127" "C26"  "C125" "C124" "C130" "C33"  "C36" 
#[18] "C32"  "C38"  "C114" "C62"  "C72"  "C67"  "C49"  "C70"  "C68"  "C63"  "C65"  "C39"  "C85"  "C89"  "C61"  "C84"  "C40" 
#[35] "C139" "C57"  "C87"  "C31"  "C86"  "C51"  "C137" "C52"  "C48"  "C53"  "C108" "C111" "C30"  "C44"  "C109"

x<-unique(Stations)
Difference<-function(x){
  for (i in 1:length(Stations)){
    NN1<-subset(Celtic,Celtic$Location_ID==x[i])
    NN1_L<-subset(NN1,NN1$Species=="Lion")
    NN1_LC<-mean(NN1_L$d13Cc)
    NN1_LN<-mean(NN1_L$d15N)
    NN1_LS<-mean(NN1_L$d34S)
    
    NN1_B<-subset(NN1,NN1$Species=="Barrel ")
    NN1_BC<-mean(NN1_B$d13Cc)
    NN1_BN<-mean(NN1_B$d15N)
    NN1_BS<-mean(NN1_B$d34S)
    
    NN1_C<-subset(NN1,NN1$Species=="Compass ")
    NN1_CC<-mean(NN1_C$d13Cc)
    NN1_CN<-mean(NN1_C$d15N)
    NN1_CS<-mean(NN1_C$d34S)
    
    NN1_M<-subset(NN1,NN1$Species=="Mauve")
    NN1_MC<-mean(NN1_M$d13Cc)
    NN1_MN<-mean(NN1_M$d15N)
    NN1_MS<-mean(NN1_M$d34S)
    
    NN1_Bl<-subset(NN1,NN1$Species=="Blue")
    NN1_BlC<-mean(NN1_Bl$d13Cc)
    NN1_BlN<-mean(NN1_Bl$d15N)
    NN1_BlS<-mean(NN1_Bl$d34S)
    
    NN1_Cr<-subset(NN1,NN1$Species=="Crystal")
    NN1_CrC<-mean(NN1_Cr$d13Cc)
    NN1_CrN<-mean(NN1_Cr$d15N)
    NN1_CrS<-mean(NN1_Cr$d34S)
    
    NN1_Mn<-subset(NN1,NN1$Species=="Moon")
    NN1_MnC<-mean(NN1_Mn$d13Cc)
    NN1_MnN<-mean(NN1_Mn$d15N)
    NN1_MnS<-mean(NN1_Mn$d34S)
    
    #Carbon Differences:
    
    NN1_C_LB<-NN1_LC-NN1_BC
    NN1_C_LC<-NN1_LC-NN1_CC
    NN1_C_LM<-NN1_LC-NN1_MC
    NN1_C_LBl<-NN1_LC-NN1_BlC
    NN1_C_LCr<-NN1_LC-NN1_CrC
    NN1_C_LMn<-NN1_LC-NN1_MnC
    NN1_C_BC<-NN1_BC-NN1_CC
    NN1_C_BM<-NN1_BC-NN1_MC
    NN1_C_BBl<-NN1_BC-NN1_BlC
    NN1_C_BCr<-NN1_BC-NN1_CrC
    NN1_C_BMn<-NN1_BC-NN1_MnC
    NN1_C_CM<-NN1_CC-NN1_MC
    NN1_C_CBl<-NN1_CC-NN1_BlC
    NN1_C_CCr<-NN1_CC-NN1_CrC
    NN1_C_CMn<-NN1_CC-NN1_MnC
    NN1_C_MBl<-NN1_MC-NN1_BlC
    NN1_C_MCr<-NN1_MC-NN1_CrC
    NN1_C_MMn<-NN1_MC-NN1_MnC
    NN1_C_BlCr<-NN1_BlC-NN1_CrC
    NN1_C_BlMn<-NN1_BlC-NN1_MnC
    NN1_C_CrMn<-NN1_CrC-NN1_MnC
    
    NN1_C<-c(NN1_C_LB,NN1_C_LC,NN1_C_LM,NN1_C_LBl,NN1_C_LCr,NN1_C_LMn,NN1_C_BC,NN1_C_BM,NN1_C_BBl,NN1_C_BCr,NN1_C_BMn,
             NN1_C_CM,NN1_C_CBl,NN1_C_CCr,NN1_C_CMn,NN1_C_MBl,NN1_C_MCr,NN1_C_MMn,NN1_C_BlCr,NN1_C_BlMn,
             NN1_C_CrMn)
    NN1_C<-as.data.frame(NN1_C)
    return(NN1_C)
  }}

lapply(x,Difference)
Y<-ldply(x,Difference)

#[1] "C150" "C19"  "C10"  "C1"   "C121" "C122" "C14"  "C17"  "C148" "C123" "C127" "C26"  "C125" "C124" "C130" "C33"  "C36" 
#[18] "C32"  "C38"  "C114" "C62"  "C72"  "C67"  "C49"  "C70"  "C68"  "C63"  "C65"  "C39"  "C85"  "C89"  "C61"  "C84"  "C40" 
#[35] "C139" "C57"  "C87"  "C31"  "C86"  "C51"  "C137" "C52"  "C48"  "C53"  "C108" "C111" "C30"  "C44"  "C109"


C150<-as.data.frame(Y[1:21,])
C150
C19<-as.data.frame(Y[22:42,])
C19
C10<-as.data.frame(Y[43:63,])
C10
C1<-as.data.frame(Y[64:84,])
C1
C121<-as.data.frame(Y[85:105,])
C121
C122<-as.data.frame(Y[106:126,])
C122
C14<-as.data.frame(Y[127:147,])
C14
C17<-as.data.frame(Y[148:168,])
C17
C148<-as.data.frame(Y[169:189,])
C148
C123<-as.data.frame(Y[190:210,])
C123
C127<-as.data.frame(Y[211:231,])
C127
C26<-as.data.frame(Y[232:252,])
C26
C125<-as.data.frame(Y[253:273,])
C125
C124<-as.data.frame(Y[274:294,])
C124
C130<-as.data.frame(Y[295:315,])
C130
C33<-as.data.frame(Y[316:336,])
C33
C36<-as.data.frame(Y[337:357,])
C36
C32<-as.data.frame(Y[358:378,])
C32
C38<-as.data.frame(Y[379:399,])
C38
C114<-as.data.frame(Y[400:420,])
C114
C62<-as.data.frame(Y[421:441,])
C62
C72<-as.data.frame(Y[442:462,])
C72
C67<-as.data.frame(Y[463:483,])
C67
C49<-as.data.frame(Y[484:504,])
C49
C70<-as.data.frame(Y[505:525,])
C70
C68<-as.data.frame(Y[526:546,])
C68
C63<-as.data.frame(Y[547:567,])
C63
C65<-as.data.frame(Y[568:588,])
C65
C39<-as.data.frame(Y[589:609,])
C39
C85<-as.data.frame(Y[610:630,])
C85
C89<-as.data.frame(Y[631:651,])
C89
C61<-as.data.frame(Y[652:672,])
C61
C84<-as.data.frame(Y[673:693,])
C84
C40<-as.data.frame(Y[694:714,])
C40
C139<-as.data.frame(Y[715:735,])
C139
C57<-as.data.frame(Y[736:756,])
C57
C87<-as.data.frame(Y[757:777,])
C87
C31<-as.data.frame(Y[778:798,])
C31
C19<-as.data.frame(Y[799:819,])
C19
C10<-as.data.frame(Y[820:840,])
C10
C1<-as.data.frame(Y[841:861,])
C1
C14<-as.data.frame(Y[862:882,])
C14
C17<-as.data.frame(Y[883:903,])
C17
C68<-as.data.frame(Y[904:924,])
C68
C87<-as.data.frame(Y[925:945,])
C87
C86<-as.data.frame(Y[946:966,])
C86
C51<-as.data.frame(Y[967:987,])
C51
C137<-as.data.frame(Y[988:1008,])
C137
C52<-as.data.frame(Y[1009:1029,])
C52
C48<-as.data.frame(Y[1030:1050,])
C48
C53<-as.data.frame(Y[1051:1071,])
C53
C108<-as.data.frame(Y[1072:1092,])
C108
C111<-as.data.frame(Y[1093:1113,])
C111
C30<-as.data.frame(Y[1114:1134,])
C30
C44<-as.data.frame(Y[1135:1155,])
C44
C109<-as.data.frame(Y[1156:1176,])
C109
  
Celtic_C<-cbind(C150,C19,C10,C1,C121,C122,C14,C17,C148,C123,C127,C26,C125,C124,C130,C33,C36, 
                C32,C38,C114,C62,C72,C67,C49,C70,C68,C63,C65,C39,C85,C89,C61,C84,C40, 
                C139,C57,C87,C31,C86,C51,C137,C52,C48,C53,C108,C111,C30,C44,C109)

colnames(Celtic_C) <- c("C150","C19","C10","C1","C121","C122","C14","C17","C148","C123","C127","C26","C125","C124","C130","C33","C36", 
                        "C32","C38","C114","C62","C72","C67","C49","C70","C68","C63","C65","C39","C85","C89","C61","C84","C40", 
                         "C139","C57","C87","C31","C86","C51","C137","C52","C48","C53","C108","C111","C30","C44","C109")

rownames(Celtic_C)<-c("L-B","L-C","L-M","L-Bl","L-Cr","L-Mn","B-C","B-M","B-Bl","B-Cr","B-Mn","C-M","C-Bl","C-Cr","C-Mn","M-Bl","M-Cr","M-Mn","Bl-Cr","Bl-Mn","Cr-Mn")

write.csv(Celtic_C, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Between_sp_var/Carbon_Celtic_Species_Differences.csv")

################################################
#Nitrogen:
x<-unique(Stations)
Difference<-function(x){
  for (i in 1:length(Stations)){
    NN1<-subset(Celtic,Celtic$Location_ID==x[i])
    NN1_L<-subset(NN1,NN1$Species=="Lion")
    NN1_LC<-mean(NN1_L$d13Cc)
    NN1_LN<-mean(NN1_L$d15N)
    NN1_LS<-mean(NN1_L$d34S)
    
    NN1_B<-subset(NN1,NN1$Species=="Barrel ")
    NN1_BC<-mean(NN1_B$d13Cc)
    NN1_BN<-mean(NN1_B$d15N)
    NN1_BS<-mean(NN1_B$d34S)
    
    NN1_C<-subset(NN1,NN1$Species=="Compass ")
    NN1_CC<-mean(NN1_C$d13Cc)
    NN1_CN<-mean(NN1_C$d15N)
    NN1_CS<-mean(NN1_C$d34S)
    
    NN1_M<-subset(NN1,NN1$Species=="Mauve")
    NN1_MC<-mean(NN1_M$d13Cc)
    NN1_MN<-mean(NN1_M$d15N)
    NN1_MS<-mean(NN1_M$d34S)
    
    NN1_Bl<-subset(NN1,NN1$Species=="Blue")
    NN1_BlC<-mean(NN1_Bl$d13Cc)
    NN1_BlN<-mean(NN1_Bl$d15N)
    NN1_BlS<-mean(NN1_Bl$d34S)
    
    NN1_Cr<-subset(NN1,NN1$Species=="Crystal")
    NN1_CrC<-mean(NN1_Cr$d13Cc)
    NN1_CrN<-mean(NN1_Cr$d15N)
    NN1_CrS<-mean(NN1_Cr$d34S)
    
    NN1_Mn<-subset(NN1,NN1$Species=="Moon")
    NN1_MnC<-mean(NN1_Mn$d13Cc)
    NN1_MnN<-mean(NN1_Mn$d15N)
    NN1_MnS<-mean(NN1_Mn$d34S)
    
    
    #Nitrogen Differences:
    
    NN1_N_LB<-NN1_LN-NN1_BN
    NN1_N_LC<-NN1_LN-NN1_CN
    NN1_N_LM<-NN1_LN-NN1_MN
    NN1_N_LBl<-NN1_LN-NN1_BlN
    NN1_N_LCr<-NN1_LN-NN1_CrN
    NN1_N_LMn<-NN1_LN-NN1_MnN
    NN1_N_BC<-NN1_BN-NN1_CN
    NN1_N_BM<-NN1_BN-NN1_MN
    NN1_N_BBl<-NN1_BN-NN1_BlN
    NN1_N_BCr<-NN1_BN-NN1_CrN
    NN1_N_BMn<-NN1_BN-NN1_MnN
    NN1_N_CM<-NN1_CN-NN1_MN
    NN1_N_CBl<-NN1_CN-NN1_BlN
    NN1_N_CCr<-NN1_CN-NN1_CrN
    NN1_N_CMn<-NN1_CN-NN1_MnN
    NN1_N_MBl<-NN1_MN-NN1_BlN
    NN1_N_MCr<-NN1_MN-NN1_CrN
    NN1_N_MMn<-NN1_MN-NN1_MnN
    NN1_N_BlCr<-NN1_BlN-NN1_CrN
    NN1_N_BlMn<-NN1_BlN-NN1_MnN
    NN1_N_CrMn<-NN1_CrN-NN1_MnN
    
    NN1_N<-c(NN1_N_LB,NN1_N_LC,NN1_N_LM,NN1_N_LBl,NN1_N_LCr,NN1_N_LMn,NN1_N_BC,NN1_N_BM,NN1_N_BBl,NN1_N_BCr,NN1_N_BMn,
             NN1_N_CM,NN1_N_CBl,NN1_N_CCr,NN1_N_CMn,NN1_N_MBl,NN1_N_MCr,NN1_N_MMn,NN1_N_BlCr,NN1_N_BlMn,
             NN1_N_CrMn)
    NN1_N<-as.data.frame(NN1_N)
    return(NN1_N)
  }}

lapply(x,Difference)
Y<-ldply(x,Difference)

C150<-as.data.frame(Y[1:21,])
C150
C19<-as.data.frame(Y[22:42,])
C19
C10<-as.data.frame(Y[43:63,])
C10
C1<-as.data.frame(Y[64:84,])
C1
C121<-as.data.frame(Y[85:105,])
C121
C122<-as.data.frame(Y[106:126,])
C122
C14<-as.data.frame(Y[127:147,])
C14
C17<-as.data.frame(Y[148:168,])
C17
C148<-as.data.frame(Y[169:189,])
C148
C123<-as.data.frame(Y[190:210,])
C123
C127<-as.data.frame(Y[211:231,])
C127
C26<-as.data.frame(Y[232:252,])
C26
C125<-as.data.frame(Y[253:273,])
C125
C124<-as.data.frame(Y[274:294,])
C124
C130<-as.data.frame(Y[295:315,])
C130
C33<-as.data.frame(Y[316:336,])
C33
C36<-as.data.frame(Y[337:357,])
C36
C32<-as.data.frame(Y[358:378,])
C32
C38<-as.data.frame(Y[379:399,])
C38
C114<-as.data.frame(Y[400:420,])
C114
C62<-as.data.frame(Y[421:441,])
C62
C72<-as.data.frame(Y[442:462,])
C72
C67<-as.data.frame(Y[463:483,])
C67
C49<-as.data.frame(Y[484:504,])
C49
C70<-as.data.frame(Y[505:525,])
C70
C68<-as.data.frame(Y[526:546,])
C68
C63<-as.data.frame(Y[547:567,])
C63
C65<-as.data.frame(Y[568:588,])
C65
C39<-as.data.frame(Y[589:609,])
C39
C85<-as.data.frame(Y[610:630,])
C85
C89<-as.data.frame(Y[631:651,])
C89
C61<-as.data.frame(Y[652:672,])
C61
C84<-as.data.frame(Y[673:693,])
C84
C40<-as.data.frame(Y[694:714,])
C40
C139<-as.data.frame(Y[715:735,])
C139
C57<-as.data.frame(Y[736:756,])
C57
C87<-as.data.frame(Y[757:777,])
C87
C31<-as.data.frame(Y[778:798,])
C31
C19<-as.data.frame(Y[799:819,])
C19
C10<-as.data.frame(Y[820:840,])
C10
C1<-as.data.frame(Y[841:861,])
C1
C14<-as.data.frame(Y[862:882,])
C14
C17<-as.data.frame(Y[883:903,])
C17
C68<-as.data.frame(Y[904:924,])
C68
C87<-as.data.frame(Y[925:945,])
C87
C86<-as.data.frame(Y[946:966,])
C86
C51<-as.data.frame(Y[967:987,])
C51
C137<-as.data.frame(Y[988:1008,])
C137
C52<-as.data.frame(Y[1009:1029,])
C52
C48<-as.data.frame(Y[1030:1050,])
C48
C53<-as.data.frame(Y[1051:1071,])
C53
C108<-as.data.frame(Y[1072:1092,])
C108
C111<-as.data.frame(Y[1093:1113,])
C111
C30<-as.data.frame(Y[1114:1134,])
C30
C44<-as.data.frame(Y[1135:1155,])
C44
C109<-as.data.frame(Y[1156:1176,])
C109

Celtic_N<-cbind(C150,C19,C10,C1,C121,C122,C14,C17,C148,C123,C127,C26,C125,C124,C130,C33,C36, 
                C32,C38,C114,C62,C72,C67,C49,C70,C68,C63,C65,C39,C85,C89,C61,C84,C40, 
                C139,C57,C87,C31,C86,C51,C137,C52,C48,C53,C108,C111,C30,C44,C109)

colnames(Celtic_N) <- c("C150","C19","C10","C1","C121","C122","C14","C17","C148","C123","C127","C26","C125","C124","C130","C33","C36", 
                        "C32","C38","C114","C62","C72","C67","C49","C70","C68","C63","C65","C39","C85","C89","C61","C84","C40", 
                        "C139","C57","C87","C31","C86","C51","C137","C52","C48","C53","C108","C111","C30","C44","C109")

rownames(Celtic_N)<-c("L-B","L-C","L-M","L-Bl","L-Cr","L-Mn","B-C","B-M","B-Bl","B-Cr","B-Mn","C-M","C-Bl","C-Cr","C-Mn","M-Bl","M-Cr","M-Mn","Bl-Cr","Bl-Mn","Cr-Mn")


write.csv(Celtic_N, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Between_sp_var/Nitrogen_Celtic_Species_Differences.csv")

################################################
#Sulphur:
x<-unique(Stations)
Difference<-function(x){
  for (i in 1:length(Stations)){
    NN1<-subset(Celtic,Celtic$Location_ID==x[i])
    NN1_L<-subset(NN1,NN1$Species=="Lion")
    NN1_LC<-mean(NN1_L$d13Cc)
    NN1_LN<-mean(NN1_L$d15N)
    NN1_LS<-mean(NN1_L$d34S)
    
    NN1_B<-subset(NN1,NN1$Species=="Barrel ")
    NN1_BC<-mean(NN1_B$d13Cc)
    NN1_BN<-mean(NN1_B$d15N)
    NN1_BS<-mean(NN1_B$d34S)
    
    NN1_C<-subset(NN1,NN1$Species=="Compass ")
    NN1_CC<-mean(NN1_C$d13Cc)
    NN1_CN<-mean(NN1_C$d15N)
    NN1_CS<-mean(NN1_C$d34S)
    
    NN1_M<-subset(NN1,NN1$Species=="Mauve")
    NN1_MC<-mean(NN1_M$d13Cc)
    NN1_MN<-mean(NN1_M$d15N)
    NN1_MS<-mean(NN1_M$d34S)
    
    NN1_Bl<-subset(NN1,NN1$Species=="Blue")
    NN1_BlC<-mean(NN1_Bl$d13Cc)
    NN1_BlN<-mean(NN1_Bl$d15N)
    NN1_BlS<-mean(NN1_Bl$d34S)
    
    NN1_Cr<-subset(NN1,NN1$Species=="Crystal")
    NN1_CrC<-mean(NN1_Cr$d13Cc)
    NN1_CrN<-mean(NN1_Cr$d15N)
    NN1_CrS<-mean(NN1_Cr$d34S)
    
    NN1_Mn<-subset(NN1,NN1$Species=="Moon")
    NN1_MnC<-mean(NN1_Mn$d13Cc)
    NN1_MnN<-mean(NN1_Mn$d15N)
    NN1_MnS<-mean(NN1_Mn$d34S)
    
    
    #Sulphur Differences:
    
    NN1_S_LB<-NN1_LS-NN1_BS
    NN1_S_LC<-NN1_LS-NN1_CS
    NN1_S_LM<-NN1_LS-NN1_MS
    NN1_S_LBl<-NN1_LS-NN1_BlS
    NN1_S_LCr<-NN1_LS-NN1_CrS
    NN1_S_LMn<-NN1_LS-NN1_MnS
    NN1_S_BC<-NN1_BS-NN1_CS
    NN1_S_BM<-NN1_BS-NN1_MS
    NN1_S_BBl<-NN1_BS-NN1_BlS
    NN1_S_BCr<-NN1_BS-NN1_CrS
    NN1_S_BMn<-NN1_BS-NN1_MnS
    NN1_S_CM<-NN1_CS-NN1_MS
    NN1_S_CBl<-NN1_CS-NN1_BlS
    NN1_S_CCr<-NN1_CS-NN1_CrS
    NN1_S_CMn<-NN1_CS-NN1_MnS
    NN1_S_MBl<-NN1_MS-NN1_BlS
    NN1_S_MCr<-NN1_MS-NN1_CrS
    NN1_S_MMn<-NN1_MS-NN1_MnS
    NN1_S_BlCr<-NN1_BlS-NN1_CrS
    NN1_S_BlMn<-NN1_BlS-NN1_MnS
    NN1_S_CrMn<-NN1_CrS-NN1_MnS
    
    NN1_S<-c(NN1_S_LB,NN1_S_LC,NN1_S_LM,NN1_S_LBl,NN1_S_LCr,NN1_S_LMn,NN1_S_BC,NN1_S_BM,NN1_S_BBl,NN1_S_BCr,NN1_S_BMn,
             NN1_S_CM,NN1_S_CBl,NN1_S_CCr,NN1_S_CMn,NN1_S_MBl,NN1_S_MCr,NN1_S_MMn,NN1_S_BlCr,NN1_S_BlMn,
             NN1_S_CrMn)
    NN1_S<-as.data.frame(NN1_S)
    return(NN1_S)
  }}

lapply(x,Difference)
Y<-ldply(x,Difference)

C150<-as.data.frame(Y[1:21,])
C150
C19<-as.data.frame(Y[22:42,])
C19
C10<-as.data.frame(Y[43:63,])
C10
C1<-as.data.frame(Y[64:84,])
C1
C121<-as.data.frame(Y[85:105,])
C121
C122<-as.data.frame(Y[106:126,])
C122
C14<-as.data.frame(Y[127:147,])
C14
C17<-as.data.frame(Y[148:168,])
C17
C148<-as.data.frame(Y[169:189,])
C148
C123<-as.data.frame(Y[190:210,])
C123
C127<-as.data.frame(Y[211:231,])
C127
C26<-as.data.frame(Y[232:252,])
C26
C125<-as.data.frame(Y[253:273,])
C125
C124<-as.data.frame(Y[274:294,])
C124
C130<-as.data.frame(Y[295:315,])
C130
C33<-as.data.frame(Y[316:336,])
C33
C36<-as.data.frame(Y[337:357,])
C36
C32<-as.data.frame(Y[358:378,])
C32
C38<-as.data.frame(Y[379:399,])
C38
C114<-as.data.frame(Y[400:420,])
C114
C62<-as.data.frame(Y[421:441,])
C62
C72<-as.data.frame(Y[442:462,])
C72
C67<-as.data.frame(Y[463:483,])
C67
C49<-as.data.frame(Y[484:504,])
C49
C70<-as.data.frame(Y[505:525,])
C70
C68<-as.data.frame(Y[526:546,])
C68
C63<-as.data.frame(Y[547:567,])
C63
C65<-as.data.frame(Y[568:588,])
C65
C39<-as.data.frame(Y[589:609,])
C39
C85<-as.data.frame(Y[610:630,])
C85
C89<-as.data.frame(Y[631:651,])
C89
C61<-as.data.frame(Y[652:672,])
C61
C84<-as.data.frame(Y[673:693,])
C84
C40<-as.data.frame(Y[694:714,])
C40
C139<-as.data.frame(Y[715:735,])
C139
C57<-as.data.frame(Y[736:756,])
C57
C87<-as.data.frame(Y[757:777,])
C87
C31<-as.data.frame(Y[778:798,])
C31
C19<-as.data.frame(Y[799:819,])
C19
C10<-as.data.frame(Y[820:840,])
C10
C1<-as.data.frame(Y[841:861,])
C1
C14<-as.data.frame(Y[862:882,])
C14
C17<-as.data.frame(Y[883:903,])
C17
C68<-as.data.frame(Y[904:924,])
C68
C87<-as.data.frame(Y[925:945,])
C87
C86<-as.data.frame(Y[946:966,])
C86
C51<-as.data.frame(Y[967:987,])
C51
C137<-as.data.frame(Y[988:1008,])
C137
C52<-as.data.frame(Y[1009:1029,])
C52
C48<-as.data.frame(Y[1030:1050,])
C48
C53<-as.data.frame(Y[1051:1071,])
C53
C108<-as.data.frame(Y[1072:1092,])
C108
C111<-as.data.frame(Y[1093:1113,])
C111
C30<-as.data.frame(Y[1114:1134,])
C30
C44<-as.data.frame(Y[1135:1155,])
C44
C109<-as.data.frame(Y[1156:1176,])
C109

Celtic_S<-cbind(C150,C19,C10,C1,C121,C122,C14,C17,C148,C123,C127,C26,C125,C124,C130,C33,C36, 
                C32,C38,C114,C62,C72,C67,C49,C70,C68,C63,C65,C39,C85,C89,C61,C84,C40, 
                C139,C57,C87,C31,C86,C51,C137,C52,C48,C53,C108,C111,C30,C44,C109)

colnames(Celtic_S) <- c("C150","C19","C10","C1","C121","C122","C14","C17","C148","C123","C127","C26","C125","C124","C130","C33","C36", 
                        "C32","C38","C114","C62","C72","C67","C49","C70","C68","C63","C65","C39","C85","C89","C61","C84","C40", 
                        "C139","C57","C87","C31","C86","C51","C137","C52","C48","C53","C108","C111","C30","C44","C109")

rownames(Celtic_S)<-c("L-B","L-C","L-M","L-Bl","L-Cr","L-Mn","B-C","B-M","B-Bl","B-Cr","B-Mn","C-M","C-Bl","C-Cr","C-Mn","M-Bl","M-Cr","M-Mn","Bl-Cr","Bl-Mn","Cr-Mn")


write.csv(Celtic_S, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Between_sp_var/Sulphur_Celtic_Species_Differences.csv")

################################################################
###############################################################
#West - Carbon:

#Find all Rows where the Location ID is duplicated (the stations where multiple samples occur)
Stations<-West$Location_ID[duplicated(West$Location_ID)]
Stations
length(Stations)
Stations<- as.character(Stations)
Un_Stations<-unique(Stations)
Un_Stations

#  [1] "W7"   "W19"  "W20"  "W9"   "W22"  "W24"  "WS33" "WS31" "WS32" "WS27" "WS29" "WS52" "WS16" "WS51" "WS48" "WS46" "WS38"
#[18] "WS35" "WS41"

x<-unique(Stations)
Difference<-function(x){
  for (i in 1:length(Stations)){
    NN1<-subset(West,West$Location_ID==x[i])
    NN1_L<-subset(NN1,NN1$Species=="Lion")
    NN1_LC<-mean(NN1_L$d13Cc)
    NN1_LN<-mean(NN1_L$d15N)
    NN1_LS<-mean(NN1_L$d34S)
    
    NN1_B<-subset(NN1,NN1$Species=="Barrel ")
    NN1_BC<-mean(NN1_B$d13Cc)
    NN1_BN<-mean(NN1_B$d15N)
    NN1_BS<-mean(NN1_B$d34S)
    
    NN1_C<-subset(NN1,NN1$Species=="Compass ")
    NN1_CC<-mean(NN1_C$d13Cc)
    NN1_CN<-mean(NN1_C$d15N)
    NN1_CS<-mean(NN1_C$d34S)
    
    NN1_M<-subset(NN1,NN1$Species=="Mauve")
    NN1_MC<-mean(NN1_M$d13Cc)
    NN1_MN<-mean(NN1_M$d15N)
    NN1_MS<-mean(NN1_M$d34S)
    
    NN1_Bl<-subset(NN1,NN1$Species=="Blue")
    NN1_BlC<-mean(NN1_Bl$d13Cc)
    NN1_BlN<-mean(NN1_Bl$d15N)
    NN1_BlS<-mean(NN1_Bl$d34S)
    
    NN1_Cr<-subset(NN1,NN1$Species=="Crystal")
    NN1_CrC<-mean(NN1_Cr$d13Cc)
    NN1_CrN<-mean(NN1_Cr$d15N)
    NN1_CrS<-mean(NN1_Cr$d34S)
    
    NN1_Mn<-subset(NN1,NN1$Species=="Moon")
    NN1_MnC<-mean(NN1_Mn$d13Cc)
    NN1_MnN<-mean(NN1_Mn$d15N)
    NN1_MnS<-mean(NN1_Mn$d34S)
    
    #Carbon Differences:
    
    NN1_C_LB<-NN1_LC-NN1_BC
    NN1_C_LC<-NN1_LC-NN1_CC
    NN1_C_LM<-NN1_LC-NN1_MC
    NN1_C_LBl<-NN1_LC-NN1_BlC
    NN1_C_LCr<-NN1_LC-NN1_CrC
    NN1_C_LMn<-NN1_LC-NN1_MnC
    NN1_C_BC<-NN1_BC-NN1_CC
    NN1_C_BM<-NN1_BC-NN1_MC
    NN1_C_BBl<-NN1_BC-NN1_BlC
    NN1_C_BCr<-NN1_BC-NN1_CrC
    NN1_C_BMn<-NN1_BC-NN1_MnC
    NN1_C_CM<-NN1_CC-NN1_MC
    NN1_C_CBl<-NN1_CC-NN1_BlC
    NN1_C_CCr<-NN1_CC-NN1_CrC
    NN1_C_CMn<-NN1_CC-NN1_MnC
    NN1_C_MBl<-NN1_MC-NN1_BlC
    NN1_C_MCr<-NN1_MC-NN1_CrC
    NN1_C_MMn<-NN1_MC-NN1_MnC
    NN1_C_BlCr<-NN1_BlC-NN1_CrC
    NN1_C_BlMn<-NN1_BlC-NN1_MnC
    NN1_C_CrMn<-NN1_CrC-NN1_MnC
    
    NN1_C<-c(NN1_C_LB,NN1_C_LC,NN1_C_LM,NN1_C_LBl,NN1_C_LCr,NN1_C_LMn,NN1_C_BC,NN1_C_BM,NN1_C_BBl,NN1_C_BCr,NN1_C_BMn,
             NN1_C_CM,NN1_C_CBl,NN1_C_CCr,NN1_C_CMn,NN1_C_MBl,NN1_C_MCr,NN1_C_MMn,NN1_C_BlCr,NN1_C_BlMn,
             NN1_C_CrMn)
    NN1_C<-as.data.frame(NN1_C)
    return(NN1_C)
  }}

lapply(x,Difference)
Y<-ldply(x,Difference)

#  [1] "W7"   "W19"  "W20"  "W9"   "W22"  "W24"  "WS33" "WS31" "WS32" "WS27" "WS29" "WS52" "WS16" "WS51" "WS48" "WS46" "WS38"
#[18] "WS35" "WS41"

W7<-as.data.frame(Y[1:21,])
W7
W19<-as.data.frame(Y[22:42,])
W19
W20<-as.data.frame(Y[43:63,])
W20
W9<-as.data.frame(Y[64:84,])
W9
W22<-as.data.frame(Y[85:105,])
W22
W24<-as.data.frame(Y[106:126,])
W24
WS33<-as.data.frame(Y[127:147,])
WS33
WS31<-as.data.frame(Y[148:168,])
WS31
WS32<-as.data.frame(Y[169:189,])
WS32
WS27<-as.data.frame(Y[190:210,])
WS27
WS29<-as.data.frame(Y[211:231,])
WS29
WS52<-as.data.frame(Y[232:252,])
WS52
WS16<-as.data.frame(Y[253:273,])
WS16
WS51<-as.data.frame(Y[274:294,])
WS51
WS48<-as.data.frame(Y[295:315,])
WS48
WS46<-as.data.frame(Y[316:336,])
WS46
WS38<-as.data.frame(Y[337:357,])
WS38
WS35<-as.data.frame(Y[358:378,])
WS35
WS41<-as.data.frame(Y[379:399,])
WS41


West_C<-cbind(W7,W19,W20,W9,W22,W24,WS33,WS31,WS32,WS27,WS29,WS52,WS16,WS51,WS48,WS46,WS38,
              WS35,WS41)

colnames(West_C) <- c("W7","W19","W20","W9","W22","W24","WS33","WS31","WS32","WS27","WS29","WS52","WS16","WS51","WS48","WS46","WS38",
                      "WS35","WS41")

rownames(West_C)<-c("L-B","L-C","L-M","L-Bl","L-Cr","L-Mn","B-C","B-M","B-Bl","B-Cr","B-Mn","C-M","C-Bl","C-Cr","C-Mn","M-Bl","M-Cr","M-Mn","Bl-Cr","Bl-Mn","Cr-Mn")

write.csv(West_C, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Between_sp_var/Carbon_West_Species_Differences.csv")

################################################
#Nitrogen:
x<-unique(Stations)
Difference<-function(x){
  for (i in 1:length(Stations)){
    NN1<-subset(West,West$Location_ID==x[i])
    NN1_L<-subset(NN1,NN1$Species=="Lion")
    NN1_LC<-mean(NN1_L$d13Cc)
    NN1_LN<-mean(NN1_L$d15N)
    NN1_LS<-mean(NN1_L$d34S)
    
    NN1_B<-subset(NN1,NN1$Species=="Barrel ")
    NN1_BC<-mean(NN1_B$d13Cc)
    NN1_BN<-mean(NN1_B$d15N)
    NN1_BS<-mean(NN1_B$d34S)
    
    NN1_C<-subset(NN1,NN1$Species=="Compass ")
    NN1_CC<-mean(NN1_C$d13Cc)
    NN1_CN<-mean(NN1_C$d15N)
    NN1_CS<-mean(NN1_C$d34S)
    
    NN1_M<-subset(NN1,NN1$Species=="Mauve")
    NN1_MC<-mean(NN1_M$d13Cc)
    NN1_MN<-mean(NN1_M$d15N)
    NN1_MS<-mean(NN1_M$d34S)
    
    NN1_Bl<-subset(NN1,NN1$Species=="Blue")
    NN1_BlC<-mean(NN1_Bl$d13Cc)
    NN1_BlN<-mean(NN1_Bl$d15N)
    NN1_BlS<-mean(NN1_Bl$d34S)
    
    NN1_Cr<-subset(NN1,NN1$Species=="Crystal")
    NN1_CrC<-mean(NN1_Cr$d13Cc)
    NN1_CrN<-mean(NN1_Cr$d15N)
    NN1_CrS<-mean(NN1_Cr$d34S)
    
    NN1_Mn<-subset(NN1,NN1$Species=="Moon")
    NN1_MnC<-mean(NN1_Mn$d13Cc)
    NN1_MnN<-mean(NN1_Mn$d15N)
    NN1_MnS<-mean(NN1_Mn$d34S)
    
    
    #Nitrogen Differences:
    
    NN1_N_LB<-NN1_LN-NN1_BN
    NN1_N_LC<-NN1_LN-NN1_CN
    NN1_N_LM<-NN1_LN-NN1_MN
    NN1_N_LBl<-NN1_LN-NN1_BlN
    NN1_N_LCr<-NN1_LN-NN1_CrN
    NN1_N_LMn<-NN1_LN-NN1_MnN
    NN1_N_BC<-NN1_BN-NN1_CN
    NN1_N_BM<-NN1_BN-NN1_MN
    NN1_N_BBl<-NN1_BN-NN1_BlN
    NN1_N_BCr<-NN1_BN-NN1_CrN
    NN1_N_BMn<-NN1_BN-NN1_MnN
    NN1_N_CM<-NN1_CN-NN1_MN
    NN1_N_CBl<-NN1_CN-NN1_BlN
    NN1_N_CCr<-NN1_CN-NN1_CrN
    NN1_N_CMn<-NN1_CN-NN1_MnN
    NN1_N_MBl<-NN1_MN-NN1_BlN
    NN1_N_MCr<-NN1_MN-NN1_CrN
    NN1_N_MMn<-NN1_MN-NN1_MnN
    NN1_N_BlCr<-NN1_BlN-NN1_CrN
    NN1_N_BlMn<-NN1_BlN-NN1_MnN
    NN1_N_CrMn<-NN1_CrN-NN1_MnN
    
    NN1_N<-c(NN1_N_LB,NN1_N_LC,NN1_N_LM,NN1_N_LBl,NN1_N_LCr,NN1_N_LMn,NN1_N_BC,NN1_N_BM,NN1_N_BBl,NN1_N_BCr,NN1_N_BMn,
             NN1_N_CM,NN1_N_CBl,NN1_N_CCr,NN1_N_CMn,NN1_N_MBl,NN1_N_MCr,NN1_N_MMn,NN1_N_BlCr,NN1_N_BlMn,
             NN1_N_CrMn)
    NN1_N<-as.data.frame(NN1_N)
    return(NN1_N)
  }}

lapply(x,Difference)
Y<-ldply(x,Difference)

W7<-as.data.frame(Y[1:21,])
W7
W19<-as.data.frame(Y[22:42,])
W19
W20<-as.data.frame(Y[43:63,])
W20
W9<-as.data.frame(Y[64:84,])
W9
W22<-as.data.frame(Y[85:105,])
W22
W24<-as.data.frame(Y[106:126,])
W24
WS33<-as.data.frame(Y[127:147,])
WS33
WS31<-as.data.frame(Y[148:168,])
WS31
WS32<-as.data.frame(Y[169:189,])
WS32
WS27<-as.data.frame(Y[190:210,])
WS27
WS29<-as.data.frame(Y[211:231,])
WS29
WS52<-as.data.frame(Y[232:252,])
WS52
WS16<-as.data.frame(Y[253:273,])
WS16
WS51<-as.data.frame(Y[274:294,])
WS51
WS48<-as.data.frame(Y[295:315,])
WS48
WS46<-as.data.frame(Y[316:336,])
WS46
WS38<-as.data.frame(Y[337:357,])
WS38
WS35<-as.data.frame(Y[358:378,])
WS35
WS41<-as.data.frame(Y[379:399,])
WS41


West_N<-cbind(W7,W19,W20,W9,W22,W24,WS33,WS31,WS32,WS27,WS29,WS52,WS16,WS51,WS48,WS46,WS38,
              WS35,WS41)

colnames(West_N) <- c("W7","W19","W20","W9","W22","W24","WS33","WS31","WS32","WS27","WS29","WS52","WS16","WS51","WS48","WS46","WS38",
                      "WS35","WS41")

rownames(West_N)<-c("L-B","L-C","L-M","L-Bl","L-Cr","L-Mn","B-C","B-M","B-Bl","B-Cr","B-Mn","C-M","C-Bl","C-Cr","C-Mn","M-Bl","M-Cr","M-Mn","Bl-Cr","Bl-Mn","Cr-Mn")

write.csv(West_N, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Between_sp_var/Nitrogen_West_Species_Differences.csv")

################################################
#Sulphur:
x<-unique(Stations)
Difference<-function(x){
  for (i in 1:length(Stations)){
    NN1<-subset(West,West$Location_ID==x[i])
    NN1_L<-subset(NN1,NN1$Species=="Lion")
    NN1_LC<-mean(NN1_L$d13Cc)
    NN1_LN<-mean(NN1_L$d15N)
    NN1_LS<-mean(NN1_L$d34S)
    
    NN1_B<-subset(NN1,NN1$Species=="Barrel ")
    NN1_BC<-mean(NN1_B$d13Cc)
    NN1_BN<-mean(NN1_B$d15N)
    NN1_BS<-mean(NN1_B$d34S)
    
    NN1_C<-subset(NN1,NN1$Species=="Compass ")
    NN1_CC<-mean(NN1_C$d13Cc)
    NN1_CN<-mean(NN1_C$d15N)
    NN1_CS<-mean(NN1_C$d34S)
    
    NN1_M<-subset(NN1,NN1$Species=="Mauve")
    NN1_MC<-mean(NN1_M$d13Cc)
    NN1_MN<-mean(NN1_M$d15N)
    NN1_MS<-mean(NN1_M$d34S)
    
    NN1_Bl<-subset(NN1,NN1$Species=="Blue")
    NN1_BlC<-mean(NN1_Bl$d13Cc)
    NN1_BlN<-mean(NN1_Bl$d15N)
    NN1_BlS<-mean(NN1_Bl$d34S)
    
    NN1_Cr<-subset(NN1,NN1$Species=="Crystal")
    NN1_CrC<-mean(NN1_Cr$d13Cc)
    NN1_CrN<-mean(NN1_Cr$d15N)
    NN1_CrS<-mean(NN1_Cr$d34S)
    
    NN1_Mn<-subset(NN1,NN1$Species=="Moon")
    NN1_MnC<-mean(NN1_Mn$d13Cc)
    NN1_MnN<-mean(NN1_Mn$d15N)
    NN1_MnS<-mean(NN1_Mn$d34S)
    
    
    #Sulphur Differences:
    
    NN1_S_LB<-NN1_LS-NN1_BS
    NN1_S_LC<-NN1_LS-NN1_CS
    NN1_S_LM<-NN1_LS-NN1_MS
    NN1_S_LBl<-NN1_LS-NN1_BlS
    NN1_S_LCr<-NN1_LS-NN1_CrS
    NN1_S_LMn<-NN1_LS-NN1_MnS
    NN1_S_BC<-NN1_BS-NN1_CS
    NN1_S_BM<-NN1_BS-NN1_MS
    NN1_S_BBl<-NN1_BS-NN1_BlS
    NN1_S_BCr<-NN1_BS-NN1_CrS
    NN1_S_BMn<-NN1_BS-NN1_MnS
    NN1_S_CM<-NN1_CS-NN1_MS
    NN1_S_CBl<-NN1_CS-NN1_BlS
    NN1_S_CCr<-NN1_CS-NN1_CrS
    NN1_S_CMn<-NN1_CS-NN1_MnS
    NN1_S_MBl<-NN1_MS-NN1_BlS
    NN1_S_MCr<-NN1_MS-NN1_CrS
    NN1_S_MMn<-NN1_MS-NN1_MnS
    NN1_S_BlCr<-NN1_BlS-NN1_CrS
    NN1_S_BlMn<-NN1_BlS-NN1_MnS
    NN1_S_CrMn<-NN1_CrS-NN1_MnS
    
    NN1_S<-c(NN1_S_LB,NN1_S_LC,NN1_S_LM,NN1_S_LBl,NN1_S_LCr,NN1_S_LMn,NN1_S_BC,NN1_S_BM,NN1_S_BBl,NN1_S_BCr,NN1_S_BMn,
             NN1_S_CM,NN1_S_CBl,NN1_S_CCr,NN1_S_CMn,NN1_S_MBl,NN1_S_MCr,NN1_S_MMn,NN1_S_BlCr,NN1_S_BlMn,
             NN1_S_CrMn)
    NN1_S<-as.data.frame(NN1_S)
    return(NN1_S)
  }}

lapply(x,Difference)
Y<-ldply(x,Difference)

W7<-as.data.frame(Y[1:21,])
W7
W19<-as.data.frame(Y[22:42,])
W19
W20<-as.data.frame(Y[43:63,])
W20
W9<-as.data.frame(Y[64:84,])
W9
W22<-as.data.frame(Y[85:105,])
W22
W24<-as.data.frame(Y[106:126,])
W24
WS33<-as.data.frame(Y[127:147,])
WS33
WS31<-as.data.frame(Y[148:168,])
WS31
WS32<-as.data.frame(Y[169:189,])
WS32
WS27<-as.data.frame(Y[190:210,])
WS27
WS29<-as.data.frame(Y[211:231,])
WS29
WS52<-as.data.frame(Y[232:252,])
WS52
WS16<-as.data.frame(Y[253:273,])
WS16
WS51<-as.data.frame(Y[274:294,])
WS51
WS48<-as.data.frame(Y[295:315,])
WS48
WS46<-as.data.frame(Y[316:336,])
WS46
WS38<-as.data.frame(Y[337:357,])
WS38
WS35<-as.data.frame(Y[358:378,])
WS35
WS41<-as.data.frame(Y[379:399,])
WS41


West_S<-cbind(W7,W19,W20,W9,W22,W24,WS33,WS31,WS32,WS27,WS29,WS52,WS16,WS51,WS48,WS46,WS38,
              WS35,WS41)

colnames(West_S) <- c("W7","W19","W20","W9","W22","W24","WS33","WS31","WS32","WS27","WS29","WS52","WS16","WS51","WS48","WS46","WS38",
                      "WS35","WS41")

rownames(West_S)<-c("L-B","L-C","L-M","L-Bl","L-Cr","L-Mn","B-C","B-M","B-Bl","B-Cr","B-Mn","C-M","C-Bl","C-Cr","C-Mn","M-Bl","M-Cr","M-Mn","Bl-Cr","Bl-Mn","Cr-Mn")


write.csv(West_S, "/Users/ksjg1g08/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/MixedModelIsoscapes/UK_Shelf/Between_sp_var/Sulphur_West_Species_Differences.csv")

#Look at average differences between species 

LB<-cbind((West_C[1,]),(Celtic_C[1,]),(EChan_C[1,]),(NS_C[1,]),(Irish_C[1,]))
LB<-as.numeric(LB)
LB[is.nan(LB)] <- NA
LB<-na.omit(LB)
LB_average<-mean(LB)
LB_average
length(LB)

LC<-cbind((West_C[2,]),(Celtic_C[2,]),(EChan_C[2,]),(NS_C[2,]),(Irish_C[2,]))
LC<-as.numeric(LC)
LC[is.nan(LC)] <- NA
LC<-na.omit(LC)
LC_average<-mean(LC)
LC_average
length(LC)

LM<-cbind((West_C[3,]),(Celtic_C[3,]),(EChan_C[3,]),(NS_C[3,]),(Irish_C[3,]))
LM<-as.numeric(LM)
LM[is.nan(LM)] <- NA
LM<-na.omit(LM)
LM_average<-mean(LM)
LM_average

LBl<-cbind((West_C[4,]),(Celtic_C[4,]),(EChan_C[4,]),(NS_C[4,]),(Irish_C[4,]))
LBl<-as.numeric(LBl)
LBl[is.nan(LBl)] <- NA
LBl<-na.omit(LBl)
LBl_average<-mean(LBl)
LBl_average
length(LBl)

LCr<-cbind((West_C[5,]),(Celtic_C[5,]),(EChan_C[5,]),(NS_C[5,]),(Irish_C[5,]))
LCr<-as.numeric(LCr)
LCr[is.nan(LCr)] <- NA
LCr<-na.omit(LCr)
LCr_average<-mean(LCr)
LCr_average

LMn<-cbind((West_C[6,]),(Celtic_C[6,]),(EChan_C[6,]),(NS_C[6,]),(Irish_C[6,]))
LMn<-as.numeric(LMn)
LMn[is.nan(LMn)] <- NA
LMn<-na.omit(LMn)
LMn_average<-mean(LMn)
LMn_average

BC<-cbind((West_C[7,]),(Celtic_C[7,]),(EChan_C[7,]),(NS_C[7,]),(Irish_C[7,]))
BC<-as.numeric(BC)
BC[is.nan(BC)] <- NA
BC<-na.omit(BC)
BC_average<-mean(BC)
BC_average

BM<-cbind((West_C[8,]),(Celtic_C[8,]),(EChan_C[8,]),(NS_C[8,]),(Irish_C[8,]))
BM<-as.numeric(BM)
BM[is.nan(BM)] <- NA
BM<-na.omit(BM)
BM_average<-mean(BM)
BM_average

BBl<-cbind((West_C[9,]),(Celtic_C[9,]),(EChan_C[9,]),(NS_C[9,]),(Irish_C[9,]))
BBl<-as.numeric(BBl)
BBl[is.nan(BBl)] <- NA
BBl<-na.omit(BBl)
BBl_average<-mean(BBl)
BBl_average

BCr<-cbind((West_C[10,]),(Celtic_C[10,]),(EChan_C[10,]),(NS_C[10,]),(Irish_C[10,]))
BCr<-as.numeric(BCr)
BCr[is.nan(BCr)] <- NA
BCr<-na.omit(BCr)
BCr_average<-mean(BCr)
BCr_average

BMn<-cbind((West_C[11,]),(Celtic_C[11,]),(EChan_C[11,]),(NS_C[11,]),(Irish_C[11,]))
BMn<-as.numeric(BMn)
BMn[is.nan(BMn)] <- NA
BMn<-na.omit(BMn)
BMn_average<-mean(BMn)
BMn_average

CM<-cbind((West_C[12,]),(Celtic_C[12,]),(EChan_C[12,]),(NS_C[12,]),(Irish_C[12,]))
CM<-as.numeric(CM)
CM[is.nan(CM)] <- NA
CM<-na.omit(CM)
CM_average<-mean(CM)
CM_average

CBl<-cbind((West_C[13,]),(Celtic_C[13,]),(EChan_C[13,]),(NS_C[13,]),(Irish_C[13,]))
CBl<-as.numeric(CBl)
CBl[is.nan(CBl)] <- NA
CBl<-na.omit(CBl)
CBl_average<-mean(CBl)
CBl_average

CCr<-cbind((West_C[14,]),(Celtic_C[14,]),(EChan_C[14,]),(NS_C[14,]),(Irish_C[14,]))
CCr<-as.numeric(CCr)
CCr[is.nan(CCr)] <- NA
CCr<-na.omit(CCr)
CCr_average<-mean(CCr)
CCr_average

CMn<-cbind((West_C[15,]),(Celtic_C[15,]),(EChan_C[15,]),(NS_C[15,]),(Irish_C[15,]))
CMn<-as.numeric(CMn)
CMn[is.nan(CMn)] <- NA
CMn<-na.omit(CMn)
CMn_average<-mean(CMn)
CMn_average

MBl<-cbind((West_C[16,]),(Celtic_C[16,]),(EChan_C[16,]),(NS_C[16,]),(Irish_C[16,]))
MBl<-as.numeric(MBl)
MBl[is.nan(MBl)] <- NA
MBl<-na.omit(MBl)
MBl_average<-mean(MBl)
MBl_average

MCr<-cbind((West_C[17,]),(Celtic_C[17,]),(EChan_C[17,]),(NS_C[17,]),(Irish_C[17,]))
MCr<-as.numeric(MCr)
MCr[is.nan(MCr)] <- NA
MCr<-na.omit(MCr)
MCr_average<-mean(MCr)
MCr_average

MMn<-cbind((West_C[18,]),(Celtic_C[18,]),(EChan_C[18,]),(NS_C[18,]),(Irish_C[18,]))
MMn<-as.numeric(MMn)
MMn[is.nan(MMn)] <- NA
MMn<-na.omit(MMn)
MMn_average<-mean(MMn)
MMn_average

BlCr<-cbind((West_C[19,]),(Celtic_C[19,]),(EChan_C[19,]),(NS_C[19,]),(Irish_C[19,]))
BlCr<-as.numeric(BlCr)
BlCr[is.nan(BlCr)] <- NA
BlCr<-na.omit(BlCr)
BlCr_average<-mean(BlCr)
BlCr_average

BlMn<-cbind((West_C[20,]),(Celtic_C[20,]),(EChan_C[20,]),(NS_C[20,]),(Irish_C[20,]))
BlMn<-as.numeric(BlMn)
BlMn[is.nan(BlMn)] <- NA
BlMn<-na.omit(BlMn)
BlMn_average<-mean(BlMn)
BlMn_average

CrMn<-cbind((West_C[21,]),(Celtic_C[21,]),(EChan_C[21,]),(NS_C[21,]),(Irish_C[21,]))
CrMn<-as.numeric(CrMn)
CrMn[is.nan(CrMn)] <- NA
CrMn<-na.omit(CrMn)
CrMn_average<-mean(CrMn)
CrMn_average

##########################################
#Nitrogen:
#########################################


LB<-cbind((West_N[1,]),(Celtic_N[1,]),(EChan_N[1,]),(NS_N[1,]),(Irish_N[1,]))
LB<-as.numeric(LB)
LB[is.nan(LB)] <- NA
LB<-na.omit(LB)
LB_average<-mean(LB)
LB_average

LC<-cbind((West_N[2,]),(Celtic_N[2,]),(EChan_N[2,]),(NS_N[2,]),(Irish_N[2,]))
LC<-as.numeric(LC)
LC[is.nan(LC)] <- NA
LC<-na.omit(LC)
LC_average<-mean(LC)
LC_average

LM<-cbind((West_N[3,]),(Celtic_N[3,]),(EChan_N[3,]),(NS_N[3,]),(Irish_N[3,]))
LM<-as.numeric(LM)
LM[is.nan(LM)] <- NA
LM<-na.omit(LM)
LM_average<-mean(LM)
LM_average

LBl<-cbind((West_N[4,]),(Celtic_N[4,]),(EChan_N[4,]),(NS_N[4,]),(Irish_N[4,]))
LBl<-as.numeric(LBl)
LBl[is.nan(LBl)] <- NA
LBl<-na.omit(LBl)
LBl_average<-mean(LBl)
LBl_average

LCr<-cbind((West_N[5,]),(Celtic_N[5,]),(EChan_N[5,]),(NS_N[5,]),(Irish_N[5,]))
LCr<-as.numeric(LCr)
LCr[is.nan(LCr)] <- NA
LCr<-na.omit(LCr)
LCr_average<-mean(LCr)
LCr_average

LMn<-cbind((West_N[6,]),(Celtic_N[6,]),(EChan_N[6,]),(NS_N[6,]),(Irish_N[6,]))
LMn<-as.numeric(LMn)
LMn[is.nan(LMn)] <- NA
LMn<-na.omit(LMn)
LMn_average<-mean(LMn)
LMn_average

BC<-cbind((West_N[7,]),(Celtic_N[7,]),(EChan_N[7,]),(NS_N[7,]),(Irish_N[7,]))
BC<-as.numeric(BC)
BC[is.nan(BC)] <- NA
BC<-na.omit(BC)
BC_average<-mean(BC)
BC_average

BM<-cbind((West_N[8,]),(Celtic_N[8,]),(EChan_N[8,]),(NS_N[8,]),(Irish_N[8,]))
BM<-as.numeric(BM)
BM[is.nan(BM)] <- NA
BM<-na.omit(BM)
BM_average<-mean(BM)
BM_average

BBl<-cbind((West_N[9,]),(Celtic_N[9,]),(EChan_N[9,]),(NS_N[9,]),(Irish_N[9,]))
BBl<-as.numeric(BBl)
BBl[is.nan(BBl)] <- NA
BBl<-na.omit(BBl)
BBl_average<-mean(BBl)
BBl_average

BCr<-cbind((West_N[10,]),(Celtic_N[10,]),(EChan_N[10,]),(NS_N[10,]),(Irish_N[10,]))
BCr<-as.numeric(BCr)
BCr[is.nan(BCr)] <- NA
BCr<-na.omit(BCr)
BCr_average<-mean(BCr)
BCr_average

BMn<-cbind((West_N[11,]),(Celtic_N[11,]),(EChan_N[11,]),(NS_N[11,]),(Irish_N[11,]))
BMn<-as.numeric(BMn)
BMn[is.nan(BMn)] <- NA
BMn<-na.omit(BMn)
BMn_average<-mean(BMn)
BMn_average

CM<-cbind((West_N[12,]),(Celtic_N[12,]),(EChan_N[12,]),(NS_N[12,]),(Irish_N[12,]))
CM<-as.numeric(CM)
CM[is.nan(CM)] <- NA
CM<-na.omit(CM)
CM_average<-mean(CM)
CM_average

CBl<-cbind((West_N[13,]),(Celtic_N[13,]),(EChan_N[13,]),(NS_N[13,]),(Irish_N[13,]))
CBl<-as.numeric(CBl)
CBl[is.nan(CBl)] <- NA
CBl<-na.omit(CBl)
CBl_average<-mean(CBl)
CBl_average

CCr<-cbind((West_N[14,]),(Celtic_N[14,]),(EChan_N[14,]),(NS_N[14,]),(Irish_N[14,]))
CCr<-as.numeric(CCr)
CCr[is.nan(CCr)] <- NA
CCr<-na.omit(CCr)
CCr_average<-mean(CCr)
CCr_average

CMn<-cbind((West_N[15,]),(Celtic_N[15,]),(EChan_N[15,]),(NS_N[15,]),(Irish_N[15,]))
CMn<-as.numeric(CMn)
CMn[is.nan(CMn)] <- NA
CMn<-na.omit(CMn)
CMn_average<-mean(CMn)
CMn_average

MBl<-cbind((West_N[16,]),(Celtic_N[16,]),(EChan_N[16,]),(NS_N[16,]),(Irish_N[16,]))
MBl<-as.numeric(MBl)
MBl[is.nan(MBl)] <- NA
MBl<-na.omit(MBl)
MBl_average<-mean(MBl)
MBl_average

MCr<-cbind((West_N[17,]),(Celtic_N[17,]),(EChan_N[17,]),(NS_N[17,]),(Irish_N[17,]))
MCr<-as.numeric(MCr)
MCr[is.nan(MCr)] <- NA
MCr<-na.omit(MCr)
MCr_average<-mean(MCr)
MCr_average

MMn<-cbind((West_N[18,]),(Celtic_N[18,]),(EChan_N[18,]),(NS_N[18,]),(Irish_N[18,]))
MMn<-as.numeric(MMn)
MMn[is.nan(MMn)] <- NA
MMn<-na.omit(MMn)
MMn_average<-mean(MMn)
MMn_average

BlCr<-cbind((West_N[19,]),(Celtic_N[19,]),(EChan_N[19,]),(NS_N[19,]),(Irish_N[19,]))
BlCr<-as.numeric(BlCr)
BlCr[is.nan(BlCr)] <- NA
BlCr<-na.omit(BlCr)
BlCr_average<-mean(BlCr)
BlCr_average

BlMn<-cbind((West_N[20,]),(Celtic_N[20,]),(EChan_N[20,]),(NS_N[20,]),(Irish_N[20,]))
BlMn<-as.numeric(BlMn)
BlMn[is.nan(BlMn)] <- NA
BlMn<-na.omit(BlMn)
BlMn_average<-mean(BlMn)
BlMn_average

CrMn<-cbind((West_N[21,]),(Celtic_N[21,]),(EChan_N[21,]),(NS_N[21,]),(Irish_N[21,]))
CrMn<-as.numeric(CrMn)
CrMn[is.nan(CrMn)] <- NA
CrMn<-na.omit(CrMn)
CrMn_average<-mean(CrMn)
CrMn_average

##########################################
#Sulfur
#########################################


LB<-cbind((West_S[1,]),(Celtic_S[1,]),(EChan_S[1,]),(NS_S[1,]),(Irish_S[1,]))
LB<-as.numeric(LB)
LB[is.nan(LB)] <- NA
LB<-na.omit(LB)
LB_average<-mean(LB)
length(LB)
LB_average

LC<-cbind((West_S[2,]),(Celtic_S[2,]),(EChan_S[2,]),(NS_S[2,]),(Irish_S[2,]))
LC<-as.numeric(LC)
LC[is.nan(LC)] <- NA
LC<-na.omit(LC)
LC_average<-mean(LC)
length(LC)
LC_average

LM<-cbind((West_S[3,]),(Celtic_S[3,]),(EChan_S[3,]),(NS_S[3,]),(Irish_S[3,]))
LM<-as.numeric(LM)
LM[is.nan(LM)] <- NA
LM<-na.omit(LM)
LM_average<-mean(LM)
length(LM)
LM_average

LBl<-cbind((West_S[4,]),(Celtic_S[4,]),(EChan_S[4,]),(NS_S[4,]),(Irish_S[4,]))
LBl<-as.numeric(LBl)
LBl[is.nan(LBl)] <- NA
LBl<-na.omit(LBl)
LBl_average<-mean(LBl)
length(LBl)
LBl_average

LCr<-cbind((West_S[5,]),(Celtic_S[5,]),(EChan_S[5,]),(NS_S[5,]),(Irish_S[5,]))
LCr<-as.numeric(LCr)
LCr[is.nan(LCr)] <- NA
LCr<-na.omit(LCr)
LCr_average<-mean(LCr)
length(LCr)
LCr_average

LMn<-cbind((West_S[6,]),(Celtic_S[6,]),(EChan_S[6,]),(NS_S[6,]),(Irish_S[6,]))
LMn<-as.numeric(LMn)
LMn[is.nan(LMn)] <- NA
LMn<-na.omit(LMn)
LMn_average<-mean(LMn)
length(LMn)
LMn_average

BC<-cbind((West_S[7,]),(Celtic_S[7,]),(EChan_S[7,]),(NS_S[7,]),(Irish_S[7,]))
BC<-as.numeric(BC)
BC[is.nan(BC)] <- NA
BC<-na.omit(BC)
BC_average<-mean(BC)
length(BC)
BC_average

BM<-cbind((West_S[8,]),(Celtic_S[8,]),(EChan_S[8,]),(NS_S[8,]),(Irish_S[8,]))
BM<-as.numeric(BM)
BM[is.nan(BM)] <- NA
BM<-na.omit(BM)
BM_average<-mean(BM)
length(BM)
BM_average

BBl<-cbind((West_S[9,]),(Celtic_S[9,]),(EChan_S[9,]),(NS_S[9,]),(Irish_S[9,]))
BBl<-as.numeric(BBl)
BBl[is.nan(BBl)] <- NA
BBl<-na.omit(BBl)
BBl_average<-mean(BBl)
length(BBl)
BBl_average

BCr<-cbind((West_S[10,]),(Celtic_S[10,]),(EChan_S[10,]),(NS_S[10,]),(Irish_S[10,]))
BCr<-as.numeric(BCr)
BCr[is.nan(BCr)] <- NA
BCr<-na.omit(BCr)
BCr_average<-mean(BCr)
length(BCr)
BCr_average

BMn<-cbind((West_S[11,]),(Celtic_S[11,]),(EChan_S[11,]),(NS_S[11,]),(Irish_S[11,]))
BMn<-as.numeric(BMn)
BMn[is.nan(BMn)] <- NA
BMn<-na.omit(BMn)
BMn_average<-mean(BMn)
length(BMn)
BMn_average

CM<-cbind((West_S[12,]),(Celtic_S[12,]),(EChan_S[12,]),(NS_S[12,]),(Irish_S[12,]))
CM<-as.numeric(CM)
CM[is.nan(CM)] <- NA
CM<-na.omit(CM)
CM_average<-mean(CM)
length(CM)
CM_average

CBl<-cbind((West_S[13,]),(Celtic_S[13,]),(EChan_S[13,]),(NS_S[13,]),(Irish_S[13,]))
CBl<-as.numeric(CBl)
CBl[is.nan(CBl)] <- NA
CBl<-na.omit(CBl)
CBl_average<-mean(CBl)
length(CBl)
CBl_average

CCr<-cbind((West_S[14,]),(Celtic_S[14,]),(EChan_S[14,]),(NS_S[14,]),(Irish_S[14,]))
CCr<-as.numeric(CCr)
CCr[is.nan(CCr)] <- NA
CCr<-na.omit(CCr)
CCr_average<-mean(CCr)
length(CCr)
CCr_average

CMn<-cbind((West_S[15,]),(Celtic_S[15,]),(EChan_S[15,]),(NS_S[15,]),(Irish_S[15,]))
CMn<-as.numeric(CMn)
CMn[is.nan(CMn)] <- NA
CMn<-na.omit(CMn)
CMn_average<-mean(CMn)
length(CMn)
CMn_average

MBl<-cbind((West_S[16,]),(Celtic_S[16,]),(EChan_S[16,]),(NS_S[16,]),(Irish_S[16,]))
MBl<-as.numeric(MBl)
MBl[is.nan(MBl)] <- NA
MBl<-na.omit(MBl)
MBl_average<-mean(MBl)
length(MBl)
MBl_average

MCr<-cbind((West_S[17,]),(Celtic_S[17,]),(EChan_S[17,]),(NS_S[17,]),(Irish_S[17,]))
MCr<-as.numeric(MCr)
MCr[is.nan(MCr)] <- NA
MCr<-na.omit(MCr)
MCr_average<-mean(MCr)
length(MCr)
MCr_average

MMn<-cbind((West_S[18,]),(Celtic_S[18,]),(EChan_S[18,]),(NS_S[18,]),(Irish_S[18,]))
MMn<-as.numeric(MMn)
MMn[is.nan(MMn)] <- NA
MMn<-na.omit(MMn)
MMn_average<-mean(MMn)
length(MMn)
MMn_average

BlCr<-cbind((West_S[19,]),(Celtic_S[19,]),(EChan_S[19,]),(NS_S[19,]),(Irish_S[19,]))
BlCr<-as.numeric(BlCr)
BlCr[is.nan(BlCr)] <- NA
BlCr<-na.omit(BlCr)
BlCr_average<-mean(BlCr)
length(BlCr)
BlCr_average

BlMn<-cbind((West_S[20,]),(Celtic_S[20,]),(EChan_S[20,]),(NS_S[20,]),(Irish_S[20,]))
BlMn<-as.numeric(BlMn)
BlMn[is.nan(BlMn)] <- NA
BlMn<-na.omit(BlMn)
BlMn_average<-mean(BlMn)
length(BlMn)
BlMn_average

CrMn<-cbind((West_S[21,]),(Celtic_S[21,]),(EChan_S[21,]),(NS_S[21,]),(Irish_S[21,]))
CrMn<-as.numeric(CrMn)
CrMn[is.nan(CrMn)] <- NA
CrMn<-na.omit(CrMn)
CrMn_average<-mean(CrMn)
length(CrMn)
CrMn_average
