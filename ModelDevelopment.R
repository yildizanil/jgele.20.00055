#################################################################################
# Yildiz (2020)                       
# Estimating the full range of thermal conductivity dryout curve
# Géotechnique Letters 10(3)
# doi: "10.1680/jgele.20.00055"
# This script presents the model development given in the paper. 
#################################################################################setwd("C:/Users/Anil/Desktop/LitData")
#-------------------------------------------------------------------------------#
# Importing data file. This is the same as Table 1 of the paper. 
#-------------------------------------------------------------------------------#
all <- read.csv("ModelDevelopment.csv")
#-------------------------------------------------------------------------------#
# Separating the data file according to sand content (%) 
#-------------------------------------------------------------------------------#
sandy <- all[which(all$Sand>90),]
others <- all[which(all$Sand<90),]
#-------------------------------------------------------------------------------#
# Equations for soils containing more than 90% sand-sized particles
#-------------------------------------------------------------------------------#
# Eq. 2 Fitting parameter "a" and "Cc"
#-------------------------------------------------------------------------------#
y2 <- sandy$a
x2 <- sandy$Cc
eq2 <- lm(y2~x2)
summary(eq2)
#-------------------------------------------------------------------------------#
# Eq. 3 Fitting parameter "b" and "Cc"
#-------------------------------------------------------------------------------#
y3 <- sandy$b
x3 <- sandy$Cc
#-------------------------------------------------------------------------------#
# Eq. 3 Checking for outliers as mean +- 2*standard deviation
#-------------------------------------------------------------------------------#
which(y3>mean(y3)+2*sd(y3))
which(y3<mean(y3)-2*sd(y3))
#-------------------------------------------------------------------------------#
# Eq. 3 Removing one outlier
#-------------------------------------------------------------------------------#
y3[which(y3>mean(y3)+2*sd(y3))] <- NA
x3[which(y3>mean(y3)+2*sd(y3))] <- NA
#-------------------------------------------------------------------------------#
# Eq. 3 final version of the equation
#-------------------------------------------------------------------------------#
eq3 <- nls(y3 ~ a*exp(x3)^b,start=list(a=0.08,b=-1))
summary(eq3)
#-------------------------------------------------------------------------------#
# Eq. 4 Fitting parameter "d" and density
#-------------------------------------------------------------------------------#
y4 <- sandy$d
x4 <- sandy$Density
eq4 <- lm(y4~x4)
summary(eq4)
#-------------------------------------------------------------------------------#
# Equations for soils containing less than 90% sand-sized particles
#-------------------------------------------------------------------------------#
# Eq. 5 Fitting parameter "a" and "n"
#-------------------------------------------------------------------------------#
y5 <- others$a
x5 <- others$Porosity
#-------------------------------------------------------------------------------#
# Eq. 5 Checking for outliers as mean +- 2*standard deviation
#-------------------------------------------------------------------------------#
which(y5>mean(y5)+2*sd(y5))
which(y5<mean(y5)-2*sd(y5))
#-------------------------------------------------------------------------------#
# Eq. 5 Removing one outlier
#-------------------------------------------------------------------------------#
y5[which(y5>mean(y5)+2*sd(y5))] <- NA
x5[which(y5>mean(y5)+2*sd(y5))] <- NA
#-------------------------------------------------------------------------------#
# Eq. 5 final version of the equation
#-------------------------------------------------------------------------------#
eq5 <- lm(y5~x5)
summary(eq5)
#-------------------------------------------------------------------------------#
# Eq. 6 Fitting parameter "b" and "S"
#-------------------------------------------------------------------------------#
y6 <- others$b
x6 <- others$Sand
eq6 <- lm(y6~x6)
summary(eq6)
#-------------------------------------------------------------------------------#
# Eq. 7 Fitting parameter "b" and "S"
#-------------------------------------------------------------------------------#
y7 <- others$d
x7 <- others$Sand
#-------------------------------------------------------------------------------#
# Eq. 7 Checking for outliers as mean +- 2*standard deviation
#-------------------------------------------------------------------------------#
which(y7>mean(y7)+2*sd(y7))
which(y7<mean(y7)-2*sd(y7))
#-------------------------------------------------------------------------------#
# Eq. 7 Removing one outlier
#-------------------------------------------------------------------------------#
y7[which(y7<mean(y7)-2*sd(y7))] <- NA
x7[which(y7<mean(y7)-2*sd(y7))] <- NA
#-------------------------------------------------------------------------------#
# Eq. 7 final version of the equation
#-------------------------------------------------------------------------------#
eq7 <- lm(y7~x7)
summary(eq7)
#-------------------------------------------------------------------------------#
# Equations for all soils
#-------------------------------------------------------------------------------#
# Eq. 8 Fitting parameter "c" and "S"
#-------------------------------------------------------------------------------#
y8 <- all$c
x8 <- all$Sand
eq8 <- lm(y8~x8)
summary(eq8)
#-------------------------------------------------------------------------------#
# Eq. 9 Fitting parameter "k" and "F"
#-------------------------------------------------------------------------------#
y9 <- all$k
x9 <- all$Fines
eq9 <- nls(y9 ~ a*exp(x9)^b,start=list(a=0.038,b=-0.015))
summary(eq9)