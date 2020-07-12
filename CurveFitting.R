#################################################################################
# Yildiz (2020)                       
# Estimating the full range of thermal conductivity dryout curve
# Géotechnique Letters 10(3)
# doi: "10.1680/jgele.20.00055"
# This script presents the model development given in the paper. 
#################################################################################setwd("C:/Users/yildiz/Desktop/LitData")
#-------------------------------------------------------------------------------#
# Importing data file 
#-------------------------------------------------------------------------------#
filename <- list.files("DataToTrain")
dataname <- unlist(strsplit(filename,".csv"))
filepath <- paste0("DataToTrain/",filename)
for(i in 1:length(dataname))
{
   assign(dataname[i],read.csv(filepath[i],header=T))
}
#-------------------------------------------------------------------------------#
# Entering the modified sigmoid curve equation
#-------------------------------------------------------------------------------#
yildiz <- function(d,a,k,c,b,x){
   (d+((a+k*(x-c))/(1+exp(-b*(x-c)))))
}
#-------------------------------------------------------------------------------#
# Converting data into volumetric water content (%)
#-------------------------------------------------------------------------------#
Wang_2030[,1] <- 100*Wang_2030[,1]
Wang_Fine[,1] <- 100*Wang_Fine[,1]
Lu_Soil1[,1] <- 100*Lu_Soil1[,1]
Lu_Soil2[,1] <- 100*Lu_Soil2[,1]
Lu_Soil3[,1] <- 100*Lu_Soil3[,1]
Lu_Soil4[,1] <- 100*Lu_Soil4[,1]
Lu_Soil5[,1] <- 100*Lu_Soil5[,1]
Lu_Soil6[,1] <- 100*Lu_Soil6[,1]
Lu_Soil7[,1] <- 100*Lu_Soil7[,1]
Lu_Soil8[,1] <- 100*Lu_Soil8[,1]
Lu_Soil9[,1] <- 100*Lu_Soil9[,1]
Lu_Soil10[,1] <- 100*Lu_Soil10[,1]
Lu_Soil11[,1] <- 100*Lu_Soil11[,1]
Likos_F75[,1] <- ((100*((Likos_F75[,1]*(0.41/(1-0.41)))/2.65))*(2.65/(1+(0.41/(1-0.41)))))
Likos_River[,1] <- ((100*((Likos_River[,1]*(0.383/(1-0.383)))/2.64))*(2.64/(1+(0.383/(1-0.383)))))
Likos_Concrete[,1] <- ((100*((Likos_Concrete[,1]*(0.35/(1-0.35)))/2.74))*(2.74/(1+(0.35/(1-0.35)))))
#-------------------------------------------------------------------------------#
# Performing curve fitting
#-------------------------------------------------------------------------------#
# Set 1
#-------------------------------------------------------------------------------#
y1 <- Wang_Fine[,2]
x1 <- Wang_Fine[,1]
y2 <- Wang_2030[,2]
x2 <- Wang_2030[,1]

fit_Wang_Fine <- nls(y1~yildiz(d,a,k,c,b,x1),start=list(a=2.2,b=1.5,c=3,d=0.3,k=0.03))
summary(fit_Wang_Fine)
fit_Wang_2030 <- nls(y2~yildiz(d,a,k,c,b,x2),start=list(a=2.2,b=1.5,c=3,d=0.3,k=0.03))
summary(fit_Wang_2030)
#-------------------------------------------------------------------------------#
# Set 2
#-------------------------------------------------------------------------------#
y3 <- Alrtimi_406[,2]
x3 <- Alrtimi_406[,1]
y4 <- Alrtimi_432[,2]
x4 <- Alrtimi_432[,1]
y5 <- Alrtimi_462[,2]
x5 <- Alrtimi_462[,1]
y6 <- Alrtimi_494[,2]
x6 <- Alrtimi_494[,1]

fit_Alrtimi_406 <- nls(y3~yildiz(d,a,k,c,b,x3),start=list(a=2.2,b=1.5,c=3,d=0.3,k=0.03))
summary(fit_Alrtimi_406)
fit_Alrtimi_432 <- nls(y4~yildiz(d,a,k,c,b,x4),start=list(a=2.2,b=1.5,c=3,d=0.3,k=0.03))
summary(fit_Alrtimi_432)
fit_Alrtimi_462 <- nls(y5~yildiz(d,a,k,c,b,x5),start=list(a=2.2,b=1.5,c=3,d=0.3,k=0.03))
summary(fit_Alrtimi_462)
fit_Alrtimi_494 <- nls(y6~yildiz(d,a,k,c,b,x6),start=list(a=2.2,b=1.5,c=3,d=0.3,k=0.03))
summary(fit_Alrtimi_494)
#-------------------------------------------------------------------------------#
# Set 3
#-------------------------------------------------------------------------------#
y7 <- Likos_Concrete[,2]
x7 <- Likos_Concrete[,1]
y8 <- Likos_F75[,2]
x8 <- Likos_F75[,1]
y9 <- Likos_River[,2]
x9 <- Likos_River[,1]

fit_Likos_Concrete <- nls(y7~yildiz(d,a,k,c,b,x7),start=list(a=2.2,b=1.5,c=3,d=0.3,k=0.03))
summary(fit_Likos_Concrete)
fit_Likos_F75 <- nls(y8~yildiz(d,a,k,c,b,x8),start=list(a=2.2,b=1.5,c=3,d=0.3,k=0.03))
summary(fit_Likos_F75)
fit_Likos_River <- nls(y9~yildiz(d,a,k,c,b,x9),start=list(a=2.2,b=1.5,c=3,d=0.3,k=0.03))
summary(fit_Likos_River)
#-------------------------------------------------------------------------------#
# Set 4
#-------------------------------------------------------------------------------#
y10 <- Lu_Soil3[,2]
x10 <- Lu_Soil3[,1]
y11 <- Lu_Soil4[,2]
x11 <- Lu_Soil4[,1]
y12 <- Lu_Soil5[,2]
x12 <- Lu_Soil5[,1]
y13 <- Lu_Soil7[,2]
x13 <- Lu_Soil7[,1]
y14 <- Lu_Soil8[,2]
x14 <- Lu_Soil8[,1]
y15 <- Lu_Soil9[,2]
x15 <- Lu_Soil9[,1]
y16 <- Lu_Soil10[,2]
x16 <- Lu_Soil10[,1]
y17 <- Lu_Soil11[,2]
x17 <- Lu_Soil11[,1]

fit_Lu_Soil3 <- nls(y10~yildiz(d,a,k,c,b,x10),start=list(a=1,b=0.5,c=3,d=0.3,k=0.03))
summary(fit_Lu_Soil3)
fit_Lu_Soil4 <- nls(y11~yildiz(d,a,k,c,b,x11),start=list(a=1,b=0.5,c=3,d=0.3,k=0.03))
summary(fit_Lu_Soil4)
fit_Lu_Soil5 <- nls(y12~yildiz(d,a,k,c,b,x12),start=list(a=0.5,b=0.5,c=9,d=0.3,k=0.03))
summary(fit_Lu_Soil5)
fit_Lu_Soil7 <- nls(y13~yildiz(d,a,k,c,b,x13),start=list(a=0.5,b=0.5,c=9,d=0.3,k=0.03))
summary(fit_Lu_Soil7)
fit_Lu_Soil8 <- nls(y14~yildiz(d,a,k,c,b,x14),start=list(a=0.5,b=0.5,c=9,d=0.3,k=0.03))
summary(fit_Lu_Soil8)
fit_Lu_Soil9 <- nls(y15~yildiz(d,a,k,c,b,x15),start=list(a=0.5,b=0.5,c=9,d=0.3,k=0.03))
summary(fit_Lu_Soil9)
fit_Lu_Soil10 <- nls(y16~yildiz(d,a,k,c,b,x16),start=list(a=0.8,b=0.5,c=12,d=0.3,k=0.03))
summary(fit_Lu_Soil10)
fit_Lu_Soil11 <- nls(y17~yildiz(d,a,k,c,b,x17),start=list(a=0.8,b=0.5,c=4,d=0.3,k=0.03))
summary(fit_Lu_Soil11)
#-------------------------------------------------------------------------------#
# Set 5
#-------------------------------------------------------------------------------#

Tarnawski$VoidRatio <- Tarnawski$Porosity/(1-Tarnawski$Porosity)
Tarnawski$Density <- Tarnawski$Gs/(1+Tarnawski$VoidRatio)

for(i in 1:nrow(Tarnawski))
{
   vwc <- (100*(((c(0,0.1,0.25,0.5,0.7,1)*Tarnawski$VoidRatio[i])/Tarnawski$Gs[i])*Tarnawski$Density[i]))
   therm <- unlist(c(Tarnawski[i,7:12]))
   data <- cbind(vwc,therm)
   assign(gsub("-","",as.character(Tarnawski$Sample[i])),data)
   rm(list=c("vwc","therm","data")) 
}

y18 <- NS05[,2]
x18 <- NS05[,1]
y19 <- PE02[,2]
x19 <- PE02[,1]
y20 <- NB05[,2]
x20 <- NB05[,1]
y21 <- QC02[,2]
x21 <- QC02[,1]
y22 <- ON01[,2]
x22 <- ON01[,1]
y23 <- ON02[,2]
x23 <- ON02[,1]
y24 <- ON05[,2]
x24 <- ON05[,1]
y25 <- ON06[,2]
x25 <- ON06[,1]
y26 <- MN01[,2]
x26 <- MN01[,1]

fit_NS05 <- nls(y18~yildiz(d,a,k,c,b,x18),start=list(a=0.8,b=0.5,c=4,d=0.3,k=0.03))
summary(fit_NS05)
fit_PE02 <- nls(y19~yildiz(d,a,k,c,b,x19),start=list(a=1.3,b=0.3,c=10,d=0.3,k=0.03))
summary(fit_PE02)
fit_NB05 <- nls(y20~yildiz(d,a,k,c,b,x20),start=list(a=0.6,b=0.2,c=8,d=0.3,k=0.03))
summary(fit_NB05)
fit_QC02 <- nls(y21~yildiz(d,a,k,c,b,x21),start=list(a=1.3,b=0.3,c=10,d=0.3,k=0.03))
summary(fit_QC02)
fit_ON01 <- nls(y22~yildiz(d,a,k,c,b,x22),start=list(a=1.3,b=0.3,c=10,d=0.3,k=0.03))
summary(fit_ON01)
fit_ON02 <- nls(y23~yildiz(d,a,k,c,b,x23),start=list(a=1.3,b=0.3,c=10,d=0.3,k=0.03))
summary(fit_ON02)
fit_ON05 <- nls(y24~yildiz(d,a,k,c,b,x24),start=list(a=1.3,b=0.3,c=10,d=0.3,k=0.03))
summary(fit_ON05)
fit_ON06 <- nls(y25~yildiz(d,a,k,c,b,x25),start=list(a=1.3,b=0.3,c=10,d=0.3,k=0.03))
summary(fit_ON06)
fit_MN01 <- nls(y26~yildiz(d,a,k,c,b,x26),start=list(a=1.3,b=0.3,c=10,d=0.3,k=0.03))
summary(fit_MN01)