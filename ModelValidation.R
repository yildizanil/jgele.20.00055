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
filename <- list.files("DataToValidate")
dataname <- unlist(strsplit(filename,".csv"))
filepath <- paste0("DataToValidate/",filename)
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
# Choosing data with sand content less than 90%
#-------------------------------------------------------------------------------#
Tarnawski <- Tarnawski[which(Tarnawski$Sand<90),]
#-------------------------------------------------------------------------------#
# Converting data into volumetric water content (%)
#-------------------------------------------------------------------------------#
Tarnawski$VoidRatio <- Tarnawski$Porosity/(1-Tarnawski$Porosity)
Tarnawski$Density <- Tarnawski$Gs/(1+Tarnawski$VoidRatio)
#-------------------------------------------------------------------------------#
# Generating individual data files for each soil
#-------------------------------------------------------------------------------#
for(i in 1:nrow(Tarnawski))
{
   vwc <- (100*(((c(0,0.1,0.25,0.5,0.7,1)*Tarnawski$VoidRatio[i])/Tarnawski$Gs[i])*Tarnawski$Density[i]))
   therm <- unlist(c(Tarnawski[i,7:12]))
   data <- cbind(vwc,therm)
   assign(gsub("-","",as.character(Tarnawski$Sample[i])),data)
   rm(list=c("vwc","therm","data")) 
}
#-------------------------------------------------------------------------------#
# Estimating fitting parameters for each soil
#-------------------------------------------------------------------------------#
Tarnawski$a <- 1.72-2.01*Tarnawski$Porosity
Tarnawski$b <- 0.24+0.0032*Tarnawski$Sand
Tarnawski$c <- 10.94-0.073*Tarnawski$Sand
Tarnawski$d <- 0.00067*Tarnawski$Sand+0.149
Tarnawski$k <- 0.038*exp(-0.015*Tarnawski$Fines)
#-------------------------------------------------------------------------------#
# Defining colour and pch numbers for each soil
#-------------------------------------------------------------------------------#
col_list <- c(rep(8,19),rep(1,19))
pch_list <- c(seq(1,19,1),seq(1,19,1))
#-------------------------------------------------------------------------------#
# Figure 2 of the paper
#-------------------------------------------------------------------------------#
par(mar=c(2,2.25,0.25,0.25),mgp=c(0.1,0.1,0),ps=10,las=1,cex=1,cex.main=1,family="serif",pty="s")
plot(0,0,xlim=c(0,2.5),ylim=c(0,2.5),xlab=NA,ylab=NA,axes=F,pch="")
segments(x0=seq(0,2.5,0.25),y0=0,x1=seq(0,2.5,0.25),y1=2.5,col=rgb(0,0,0,0.1))
segments(x0=0,y0=seq(0,2.5,0.25),x1=2.5,y1=seq(0,2.5,0.25),col=rgb(0,0,0,0.1))
abline(0,1,lwd=1,lty=3)
axis(1,tck=0.02)
axis(2,tck=0.02)
box()
for(i in 1:nrow(Tarnawski))
{
   data <- as.data.frame(get(gsub("-","",as.character(Tarnawski$Sample[i]))))
   data_measured <- as.data.frame(get(gsub("-","",as.character(Tarnawski$Sample[i]))))[,2]
   data_predicted <- yildiz(Tarnawski$d[i],Tarnawski$a[i],Tarnawski$k[i],Tarnawski$c[i],
                            Tarnawski$b[i],data[,1])
   points(data_predicted~data_measured,col=col_list[i],pch=pch_list[i])
   rm(list="data")
   rm(list="data_measured")
   rm(list="data_predicted")
}
par(las=0)
mtext("Measured thermal conductivity, [W/mK]",side=1,line=1)
mtext("Predicted thermal conductivity, [W/mK]",side=2,line=1.25)
#-------------------------------------------------------------------------------#
# Calculating root mean square error
#-------------------------------------------------------------------------------#
rmse <- NA
for(i in 1:nrow(Tarnawski))
{
   data <- as.data.frame(get(gsub("-","",as.character(Tarnawski$Sample[i]))))
   data_measured <- as.data.frame(get(gsub("-","",as.character(Tarnawski$Sample[i]))))[,2]
   data_predicted <- yildiz(Tarnawski$d[i],Tarnawski$a[i],Tarnawski$k[i],Tarnawski$c[i],
                            Tarnawski$b[i],data[,1])
   
   rmse[i] <- sqrt((sum((data_predicted-data_measured)^2)/6))
   rm(list="data")
   rm(list="data_measured")
   rm(list="data_predicted")
}
summary(rmse)
