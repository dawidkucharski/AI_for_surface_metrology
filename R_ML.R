library(errors)
library(progress)
library(tictoc)
library(readr)
library(pracma)
library(readxl) 
library("dplyr")
library(tidyverse)
library(plyr)
library(fitdistrplus)
library(data.table)
library(caret)
library(ellipse)
#----------------------------Hommel----------------------------------------------------------------------------------------------------------------------------------
library(fitdistrplus)
# #ball_ront <- read.table("~/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/Hommel/Kula_ver2/ball_ront.txt", skip=2)
# #cylinder_ront <- read.table("~/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/Hommel/Walec/cylinder_ront.txt", skip=2)
# 
# #descdist(cylinder_ront$V2, discrete = FALSE,boot = 1000)
# #descdist(cylinder_ront$V3, discrete = FALSE,boot = 1000)
# #descdist(ball_ront$V2, discrete = FALSE,boot = 1000)
# #descdist(ball_ront$V3, discrete = FALSE,boot = 1000)
# 
# #fitW <- fitdist(ball_ront$V2, "weibull")
# #fitg <- fitdist(ball_ront$V2, "gamma")
# #fitln <- fitdist(ball_ront$V2, "lnorm")
# #fitn <- fitdist(ball_ront$V2, "norm")
# #summary(fitW)
# summary(fitg)
# summary(fitln)
# fix(denscomp)
# par(mfrow = c(1, 1))
# cdfcomp(list(fitW, fitg, fitln), legendtext=c("Weibull", "gamma", "lognormal"),fitlwd=c(2,3,3,3),xlab= (expression(RON[t]*" ["*mu*"m]")))
# denscomp(list(fitW, fitg, fitln,fitn), legendtext=c("Weibull", "gamma", "lognormal", "norm"),fitlwd=c(2,3,3,3),xlab= (expression(RON[t]*" ["*mu*"m]")))
# qqcomp(list(fitW, fitg, fitln,fitn), legendtext=c("Weibull", "gamma", "lognormal","norm"))
# ppcomp(list(fitW, fitg, fitln), legendtext=c("Weibull", "gamma", "lognormal"))
# gofstat(list(fitW, fitg, fitln, fitn), fitnames=c("Weibull", "gamma", "lognormal","norm"))
# ront<-mean(ball_ront$V2)
# ront
# u<-(sd(ball_ront$V2)/(sqrt(length(ball_ront$V2))))
# u
# 2*u
# shapiro.test(ball_ront$V2)

#----------------------------ball#1--------------------------------------
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
Hommel_ball <- read_table2("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/Hommel/Kula_ver2/ball_ront.txt", col_names = FALSE)


#fitW <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "weibull")
#fitg <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "gamma")
#fitln <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "lnorm")
#fitn <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "norm")
#summary(fitW)
#summary(fitg)
#summary(fitln)
#fix(denscomp)
#par(mfrow = c(1, 1))
#cdfcomp(list(fitW, fitg, fitln), legendtext=c("Weibull", "gamma", "lognormal"),fitlwd=c(2,3,3,3),xlab= (expression(RON[t]*" ["*mu*"m]")))
#denscomp(list(fitW, fitg, fitln,fitn), legendtext=c("Weibull", "gamma", "lognormal", "norm"),fitlwd=c(2,3,3,3),xlab= (expression(RON[t]*" ["*mu*"m]")))
#qqcomp(list(fitW, fitg, fitln,fitn), legendtext=c("Weibull", "gamma", "lognormal","norm"))
#ppcomp(list(fitW, fitg, fitln), legendtext=c("Weibull", "gamma", "lognormal"))
#gofstat(list(fitW, fitg, fitln, fitn), fitnames=c("Weibull", "gamma", "lognormal","norm"))
#ront<-mean(KulaNS_1$`odchyłka LSCI`*1000)
#u<-(sd(KulaNS_1$`odchyłka LSCI`)/(sqrt(length(KulaNS_1$`odchyłka LSCI`))))
#shapiro.test(KulaNS_1$`odchyłka LSCI`*1000)
#print(ront, digits=14)
#print(u, digits=1)



system_type<-"RoundScan";
Ra<-round(as.numeric(NA),digits=2);
Ra_uncert<-round(as.numeric(NA),digits=2);
Rz<-round(as.numeric(NA),digits=2);
Rz_uncert<-round(as.numeric(NA),digits=2);
material<-as.numeric(6);
standard<-as.numeric(1);
F<-as.numeric(1);


x <- c(abs(as.numeric(Hommel_ball$X3)))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.02 # linearity
bRep <- 0.02 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.05 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
#print(e)


RONt<-round(as.numeric(mean(boot.r)),digits=2);
RONt_uncert<-round(as.numeric(uncert),digits=2);
write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=TRUE,row.names=FALSE)
#----------------------------ball#2--------------------------------------


#fitW <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "weibull")
#fitg <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "gamma")
#fitln <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "lnorm")
#fitn <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "norm")
#summary(fitW)
#summary(fitg)
#summary(fitln)
#fix(denscomp)
#par(mfrow = c(1, 1))
#cdfcomp(list(fitW, fitg, fitln), legendtext=c("Weibull", "gamma", "lognormal"),fitlwd=c(2,3,3,3),xlab= (expression(RON[t]*" ["*mu*"m]")))
#denscomp(list(fitW, fitg, fitln,fitn), legendtext=c("Weibull", "gamma", "lognormal", "norm"),fitlwd=c(2,3,3,3),xlab= (expression(RON[t]*" ["*mu*"m]")))
#qqcomp(list(fitW, fitg, fitln,fitn), legendtext=c("Weibull", "gamma", "lognormal","norm"))
#ppcomp(list(fitW, fitg, fitln), legendtext=c("Weibull", "gamma", "lognormal"))
#gofstat(list(fitW, fitg, fitln, fitn), fitnames=c("Weibull", "gamma", "lognormal","norm"))
#ront<-mean(KulaNS_1$`odchyłka LSCI`*1000)
#u<-(sd(KulaNS_1$`odchyłka LSCI`)/(sqrt(length(KulaNS_1$`odchyłka LSCI`))))
#shapiro.test(KulaNS_1$`odchyłka LSCI`*1000)
#print(ront, digits=14)
#print(u, digits=1)


system_type<-"RoundScan";
Ra<-round(as.numeric(NA),digits=2);
Ra_uncert<-round(as.numeric(NA),digits=2);
Rz<-round(as.numeric(NA),digits=2);
Rz_uncert<-round(as.numeric(NA),digits=2);
material<-as.numeric(6);
standard<-as.numeric(1);
F<-as.numeric(0);


x <- c(abs(as.numeric(Hommel_ball$X2)))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.02 # linearity
bRep <- 0.02 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.05 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
#print(e)


RONt<-round(as.numeric(mean(boot.r)),digits=2);
RONt_uncert<-round(as.numeric(uncert),digits=2);
write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#----------------------------cylinder#1--------------------------------------

Hommel_cyl <- read_table2("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/Hommel/Walec/cylinder_ront.txt", col_names = FALSE)


#fitW <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "weibull")
#fitg <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "gamma")
#fitln <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "lnorm")
#fitn <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "norm")
#summary(fitW)
#summary(fitg)
#summary(fitln)
#fix(denscomp)
#par(mfrow = c(1, 1))
#cdfcomp(list(fitW, fitg, fitln), legendtext=c("Weibull", "gamma", "lognormal"),fitlwd=c(2,3,3,3),xlab= (expression(RON[t]*" ["*mu*"m]")))
#denscomp(list(fitW, fitg, fitln,fitn), legendtext=c("Weibull", "gamma", "lognormal", "norm"),fitlwd=c(2,3,3,3),xlab= (expression(RON[t]*" ["*mu*"m]")))
#qqcomp(list(fitW, fitg, fitln,fitn), legendtext=c("Weibull", "gamma", "lognormal","norm"))
#ppcomp(list(fitW, fitg, fitln), legendtext=c("Weibull", "gamma", "lognormal"))
#gofstat(list(fitW, fitg, fitln, fitn), fitnames=c("Weibull", "gamma", "lognormal","norm"))
#ront<-mean(KulaNS_1$`odchyłka LSCI`*1000)
#u<-(sd(KulaNS_1$`odchyłka LSCI`)/(sqrt(length(KulaNS_1$`odchyłka LSCI`))))
#shapiro.test(KulaNS_1$`odchyłka LSCI`*1000)
#print(ront, digits=14)
#print(u, digits=1)



system_type<-"RoundScan";
Ra<-round(as.numeric(NA),digits=2);
Ra_uncert<-round(as.numeric(NA),digits=2);
Rz<-round(as.numeric(NA),digits=2);
Rz_uncert<-round(as.numeric(NA),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);


x <- c(abs(as.numeric(Hommel_cyl$X3)))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.02 # linearity
bRep <- 0.02 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.05 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
#print(e)


RONt<-round(as.numeric(mean(boot.r)),digits=2);
RONt_uncert<-round(as.numeric(uncert),digits=2);
write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
#----------------------------cylinder#2--------------------------------------

#fitW <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "weibull")
#fitg <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "gamma")
#fitln <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "lnorm")
#fitn <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "norm")
#summary(fitW)
#summary(fitg)
#summary(fitln)
#fix(denscomp)
#par(mfrow = c(1, 1))
#cdfcomp(list(fitW, fitg, fitln), legendtext=c("Weibull", "gamma", "lognormal"),fitlwd=c(2,3,3,3),xlab= (expression(RON[t]*" ["*mu*"m]")))
#denscomp(list(fitW, fitg, fitln,fitn), legendtext=c("Weibull", "gamma", "lognormal", "norm"),fitlwd=c(2,3,3,3),xlab= (expression(RON[t]*" ["*mu*"m]")))
#qqcomp(list(fitW, fitg, fitln,fitn), legendtext=c("Weibull", "gamma", "lognormal","norm"))
#ppcomp(list(fitW, fitg, fitln), legendtext=c("Weibull", "gamma", "lognormal"))
#gofstat(list(fitW, fitg, fitln, fitn), fitnames=c("Weibull", "gamma", "lognormal","norm"))
#ront<-mean(KulaNS_1$`odchyłka LSCI`*1000)
#u<-(sd(KulaNS_1$`odchyłka LSCI`)/(sqrt(length(KulaNS_1$`odchyłka LSCI`))))
#shapiro.test(KulaNS_1$`odchyłka LSCI`*1000)
#print(ront, digits=14)
#print(u, digits=1)



system_type<-"RoundScan";
Ra<-round(as.numeric(NA),digits=2);
Ra_uncert<-round(as.numeric(NA),digits=2);
Rz<-round(as.numeric(NA),digits=2);
Rz_uncert<-round(as.numeric(NA),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(0);


x <- c(abs(as.numeric(Hommel_cyl$X2)))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.02 # linearity
bRep <- 0.02 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.05 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
#print(e)


RONt<-round(as.numeric(mean(boot.r)),digits=2);
RONt_uncert<-round(as.numeric(uncert),digits=2);
write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)




#--------------------------------KulaNS#1--------------------------------

#im_num = length(list.files("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/Kula_NS#1"))
#c_num = 1

#while (c_num <= im_num) { 
#  file = file.path("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/Kula_NS#1", 
                  # paste("", c_num, ".txt", sep=""))
  
  
 # kulaNS1 = read.csv2(file,header = FALSE, sep = ";", stringsAsFactors = FALSE) 
#  c_num = c_num+1  
#}

KulaNS1<-read_excel_allsheets("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/Kula_NS#1/Kula.xlsx")
KulaNS_1<- slice_head(KulaNS1$Arkusz1, n=50)  

#fitW <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "weibull")
#fitg <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "gamma")
#fitln <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "lnorm")
#fitn <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "norm")
#summary(fitW)
#summary(fitg)
#summary(fitln)
#fix(denscomp)
#par(mfrow = c(1, 1))
#cdfcomp(list(fitW, fitg, fitln), legendtext=c("Weibull", "gamma", "lognormal"),fitlwd=c(2,3,3,3),xlab= (expression(RON[t]*" ["*mu*"m]")))
#denscomp(list(fitW, fitg, fitln,fitn), legendtext=c("Weibull", "gamma", "lognormal", "norm"),fitlwd=c(2,3,3,3),xlab= (expression(RON[t]*" ["*mu*"m]")))
#qqcomp(list(fitW, fitg, fitln,fitn), legendtext=c("Weibull", "gamma", "lognormal","norm"))
#ppcomp(list(fitW, fitg, fitln), legendtext=c("Weibull", "gamma", "lognormal"))
#gofstat(list(fitW, fitg, fitln, fitn), fitnames=c("Weibull", "gamma", "lognormal","norm"))
#ront<-mean(KulaNS_1$`odchyłka LSCI`*1000)
#u<-(sd(KulaNS_1$`odchyłka LSCI`)/(sqrt(length(KulaNS_1$`odchyłka LSCI`))))
#shapiro.test(KulaNS_1$`odchyłka LSCI`*1000)
#print(ront, digits=14)
#print(u, digits=1)



system_type<-"CMM";
Ra<-round(as.numeric(NA),digits=2);
Ra_uncert<-round(as.numeric(NA),digits=2);
Rz<-round(as.numeric(NA),digits=2);
Rz_uncert<-round(as.numeric(NA),digits=2);
material<-as.numeric(6);
standard<-as.numeric(1);
F<-as.numeric(1);


x <- c(abs(KulaNS_1$`odchyłka LSCI`*1000))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.02 # linearity
bRep <- 0.02 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.05 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
#print(e)


RONt<-round(as.numeric(mean(boot.r)),digits=2);
RONt_uncert<-round(as.numeric(uncert),digits=2);
write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
#--------------------------------WalekNS#1--------------------------------
#im_num = length(list.files("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/Kula_NS#1"))
#c_num = 1

#while (c_num <= im_num) { 
#  file = file.path("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/Kula_NS#1", 
# paste("", c_num, ".txt", sep=""))


# kulaNS1 = read.csv2(file,header = FALSE, sep = ";", stringsAsFactors = FALSE) 
#  c_num = c_num+1  
#}

WalekNS1<-read_excel_allsheets("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/Walek_NS#1/walec.xlsx")
WalekNS_1<- slice_head(WalekNS1$Arkusz1, n=84)  

#fitW <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "weibull")
#fitg <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "gamma")
#fitln <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "lnorm")
#fitn <- fitdist(KulaNS_1$`odchyłka LSCI`*1000, "norm")
#summary(fitW)
#summary(fitg)
#summary(fitln)
#fix(denscomp)
#par(mfrow = c(1, 1))
#cdfcomp(list(fitW, fitg, fitln), legendtext=c("Weibull", "gamma", "lognormal"),fitlwd=c(2,3,3,3),xlab= (expression(RON[t]*" ["*mu*"m]")))
#denscomp(list(fitW, fitg, fitln,fitn), legendtext=c("Weibull", "gamma", "lognormal", "norm"),fitlwd=c(2,3,3,3),xlab= (expression(RON[t]*" ["*mu*"m]")))
#qqcomp(list(fitW, fitg, fitln,fitn), legendtext=c("Weibull", "gamma", "lognormal","norm"))
#ppcomp(list(fitW, fitg, fitln), legendtext=c("Weibull", "gamma", "lognormal"))
#gofstat(list(fitW, fitg, fitln, fitn), fitnames=c("Weibull", "gamma", "lognormal","norm"))
#ront<-mean(KulaNS_1$`odchyłka LSCI`*1000)
#u<-(sd(KulaNS_1$`odchyłka LSCI`)/(sqrt(length(KulaNS_1$`odchyłka LSCI`))))
#shapiro.test(KulaNS_1$`odchyłka LSCI`*1000)
#print(ront, digits=14)
#print(u, digits=1)



system_type<-"CMM";
Ra<-round(as.numeric(NA),digits=2);
Ra_uncert<-round(as.numeric(NA),digits=2);
Rz<-round(as.numeric(NA),digits=2);
Rz_uncert<-round(as.numeric(NA),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);


x <- c(abs(WalekNS_1$`Odch. LSCI`*1000))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.02 # linearity
bRep <- 0.02 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.05 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
#print(e)


RONt<-round(as.numeric(mean(boot.r)),digits=2);
RONt_uncert<-round(as.numeric(uncert),digits=2);
write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr6_chropC_TaylorHobson112_1534)------------------------

wzNr6_chropC_TaylorHobson112_1534<-read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#1/wzNr6_chropC_TaylorHobson112_1534/Powierzchnie_analiza_wyniki.csv")
#fitW <- fitdist(wzNr6_chropC_TaylorHobson112_1534$Ra*1000, "weibull")
#fitg <- fitdist(wzNr6_chropC_TaylorHobson112_1534$Ra*1000, "gamma")
#fitln <- fitdist(wzNr6_chropC_TaylorHobson112_1534$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr6_chropC_TaylorHobson112_1534$Ra*1000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))
#qqcomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma","norm"))
#cdfcomp(list(fitW, fitg, fitn), legendtext=c("Weibull", "gamma", "normal"))

#u<-(sd(wzNr6_chropC_TaylorHobson112_1534$Ra*1000)/(sqrt(length(wzNr6_chropC_TaylorHobson112_1534$Ra))))
#mean(wzNr6_chropC_TaylorHobson112_1534$Ra*1000)

x <- c(wzNr6_chropC_TaylorHobson112_1534$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr6_chropC_TaylorHobson112_1534$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 

RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
#------------------------------GUM_TaylorHobson(wzNr7_chropC_TaylorHobson_6530)------------------------

wzNr7_chropC_TaylorHobson_6530<-read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#1/wzNr7_chropC_TaylorHobson_6530/Powierzchnie_analiza_wyniki.csv")
#fitW <- fitdist(wzNr7_chropC_TaylorHobson_6530$Ra*10000, "weibull")
#fitg <- fitdist(wzNr7_chropC_TaylorHobson_6530$Ra*10000, "gamma")
#fitln <- fitdist(wzNr7_chropC_TaylorHobson_6530$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr7_chropC_TaylorHobson_6530$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr7_chropC_TaylorHobson_6530$Ra*1000)/(sqrt(length(wzNr7_chropC_TaylorHobson_6530$Ra))))
#mean(wzNr7_chropC_TaylorHobson_6530$Ra*1000)
#u
#2*u
#shapiro.test(wzNr7_chropC_TaylorHobson_6530$Ra)

x <- c(wzNr7_chropC_TaylorHobson_6530$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(5);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr7_chropC_TaylorHobson_6530$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
#------------------------------GUM_TaylorHobson(wzNr9_chropC_TaylorHobson_14861)------------------------

wzNr9_chropC_TaylorHobson_14861<-read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#1/wzNr9_chropC_TaylorHobson_14861/Powierzchnie_analiza_wyniki.csv")
#fitW <- fitdist(wzNr9_chropC_TaylorHobson_14861$Ra*10000, "weibull")
#fitg <- fitdist(wzNr9_chropC_TaylorHobson_14861$Ra*10000, "gamma")
#fitln <- fitdist(wzNr7_chropC_TaylorHobson_6530$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr9_chropC_TaylorHobson_14861$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr9_chropC_TaylorHobson_14861$Ra*1000)/(sqrt(length(wzNr9_chropC_TaylorHobson_14861$Ra))))
#mean(wzNr9_chropC_TaylorHobson_14861$Ra*1000)
#u
#2*u
#shapiro.test(wzNr9_chropC_TaylorHobson_14861$Ra)

x <- c(wzNr9_chropC_TaylorHobson_14861$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(5);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr9_chropC_TaylorHobson_14861$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr10_chropC_A-920)------

wzNr10_chropC_A920<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#2/wzNr10_chropC_A-920/wzNr10_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr10_chropC_A920$Ra*10000, "weibull")
#fitg <- fitdist(wzNr10_chropC_A920$Ra*10000, "gamma")
#fitln <- fitdist(wzNr7_chropC_TaylorHobson_6530$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr10_chropC_A920$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr10_chropC_A920$Ra*1000)/(sqrt(length(wzNr10_chropC_A920$Ra))))
#mean(wzNr10_chropC_A920$Ra*1000)
#u
#2*u
#shapiro.test(wzNr10_chropC_A920$Ra)
#hist(wzNr10_chropC_A920$Ra*1000,xlab=(expression(Ra*" ["*mu*"m]")))

x <- c(wzNr10_chropC_A920$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(5);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr10_chropC_A920$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
#------------------------------GUM_TaylorHobson(wzNr11_chropC_A-352)------

wzNr11_chropC_A352<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#2/wzNr11_chropC_A-352/wzNr11_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr11_chropC_A352$Ra*10000, "weibull")
#fitg <- fitdist(wzNr11_chropC_A352$Ra*10000, "gamma")
#fitln <- fitdist(wzNr11_chropC_A352$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr11_chropC_A352$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr11_chropC_A352$Ra*1000)/(sqrt(length(wzNr11_chropC_A352$Ra))))
#mean(wzNr11_chropC_A352$Ra*1000)
#u
#2*u
#shapiro.test(wzNr11_chropC_A352$Ra)
#hist(wzNr11_chropC_A352$Ra*1000,xlab=(expression(Ra*" ["*mu*"m]")))

x <- c(wzNr11_chropC_A352$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(5);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr11_chropC_A352$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr88tocz02_VP-031nr1)------

wzNr88tocz02_VP031nr1<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#2/wzNr88tocz02_VP-031nr1/wzNr88tocz02_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz02_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz02_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr7_chropC_TaylorHobson_6530$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz02_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz02_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz02_VP031nr1$Ra))))
#mean(wzNr88tocz02_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz02_VP031nr1$Ra)

x <- c(wzNr88tocz02_VP031nr1$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr88tocz02_VP031nr1$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr88tocz04_VP-031nr1)------

wzNr88tocz04_VP031nr1<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#2/wzNr88tocz04_VP-031nr1/wzNr88tocz04_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz04_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz04_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz04_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz04_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz04_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz04_VP031nr1$Ra))))
#mean(wzNr88tocz04_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz04_VP031nr1$Ra)

x <- c(wzNr88tocz04_VP031nr1$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr88tocz04_VP031nr1$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr88tocz08_VP-031nr1)------

wzNr88tocz08_VP031nr1<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#2/wzNr88tocz08_VP-031nr1/wzNr88tocz08_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz08_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz08_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz08_VP031nr1Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz08_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz08_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz08_VP031nr1$Ra))))
#mean(wzNr88tocz08_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz08_VP031nr1$Ra)

x <- c(wzNr88tocz08_VP031nr1$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr88tocz08_VP031nr1$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr88tocz16_VP-031nr1)------

wzNr88tocz16_VP031nr1<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#2/wzNr88tocz16_VP-031nr1/wzNr88tocz16_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz16_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz16_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz16_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz16_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz16_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz16_VP031nr1$Ra))))
#mean(wzNr88tocz16_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz16_VP031nr1$Ra)

x <- c(wzNr88tocz16_VP031nr1$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr88tocz16_VP031nr1$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr88tocz32_VP-031nr1)------

wzNr88tocz32_VP031nr1<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#2/wzNr88tocz32_VP-031nr1/wzNr88tocz32_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz32_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz32_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz32_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz32_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz32_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz32_VP031nr1$Ra))))
#mean(wzNr88tocz32_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz32_VP031nr1$Ra)

x <- c(wzNr88tocz32_VP031nr1$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr88tocz32_VP031nr1$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr88tocz64_VP-031nr1)------

wzNr88tocz64_VP031nr1<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#2/wzNr88tocz64_VP-031nr1/wzNr88tocz64_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr88tocz64_VP031nr1$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr88tocz64_VP031nr1$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
#------------------------------GUM_TaylorHobson(wzNr88scier04_VP-031nr1)---------

wzNr88scier04_VP031nr1<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#3/wzNr88scier04_VP-031nr1/wzNr88scier04_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr88scier04_VP031nr1$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr88scier04_VP031nr1$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr88scier08_VP-031nr1)---------

wzNr88scier08_VP031nr1<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#3/wzNr88scier08_VP-031nr1/wzNr88scier08_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr88scier08_VP031nr1$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr88scier08_VP031nr1$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr88tocz12,5_VP-031nr1)---------

wzNr88tocz12_5_VP031nr1<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#3/wzNr88tocz12,5_VP-031nr1/wzNr88tocz12,5_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr88tocz12_5_VP031nr1$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr88tocz12_5_VP031nr1$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr88tocz25_VP-031nr1)---------

wzNr88tocz25_VP031nr1<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#3/wzNr88tocz25_VP-031nr1/wzNr88tocz25_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr88tocz25_VP031nr1$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr88tocz25_VP031nr1$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr89szlif0,1_VP-031nr2)---------

wzNr89szlif0_1_VP031nr2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#3/wzNr89szlif0,1_VP-031nr2/wzNr89szlif0,1_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr89szlif0_1_VP031nr2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr89szlif0_1_VP031nr2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr89szlif0,1_VP-031nr2#2)---------

wzNr89szlif0_1_VP031nr2_2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#3/wzNr89szlif0,1_VP-031nr2/wzNr89szlif0,1_lr0,25_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr89szlif0_1_VP031nr2_2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr89szlif0_1_VP031nr2_2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr89szlif0,2_VP-031nr2)---------

wzNr89szlif0_2_VP031nr2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#3/wzNr89szlif0,2_VP-031nr2/wzNr89szlif0,2_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)
 
 
 
x <- c(wzNr89szlif0_2_VP031nr2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr89szlif0_2_VP031nr2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr89szlif0,05_VP-031nr2)---------

wzNr89szlif0_05_VP031nr2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#3/wzNr89szlif0,05_VP-031nr2/wzNr89szlif0,05_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr89szlif0_05_VP031nr2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr89szlif0_05_VP031nr2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr89szlif04_VP-031nr2)---------

wzNr89szlif04_VP031nr2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#3/wzNr89szlif04_VP-031nr2/wzNr89szlif04_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr89szlif04_VP031nr2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr89szlif04_VP031nr2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr89szlif08_VP-031nr2)---------

wzNr89szlif08_VP031nr2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#3/wzNr89szlif08_VP-031nr2/wzNr89szlif08_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr89szlif08_VP031nr2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr89szlif08_VP031nr2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);

Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr89szlif16_VP-031nr2)---------

wzNr89szlif16_VP031nr2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#3/wzNr89szlif16_VP-031nr2/wzNr89szlif16_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr89szlif16_VP031nr2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr89szlif16_VP031nr2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr89szlif32_VP-031nr2)---------

wzNr89szlif32_VP031nr2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#3/wzNr89szlif32_VP-031nr2/wzNr89szlif32_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr89szlif32_VP031nr2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr89szlif32_VP031nr2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr89sfinisz0,1_VP-031nr2)---------

wzNr89sfinisz0_1_VP031nr2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#4/wzNr89sfinisz0,1_VP-031nr2/wzNr89sfinisz0,1_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr89sfinisz0_1_VP031nr2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr89sfinisz0_1_VP031nr2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr89sfinisz0,05_VP-031nr2)---------

wzNr89sfinisz0_05_VP031nr2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#4/wzNr89sfinisz0,05_VP-031nr2/wzNr89sfinisz0,05_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr89sfinisz0_05_VP031nr2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr89sfinisz0_05_VP031nr2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr89sfinisz0,025_Dodatkowy0,80nmRozPion_VP-031nr2)---------

wzNr89sfinisz0_025_Dodatkowy0_80nmRozPion_VP031nr2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#4/wzNr89sfinisz0,025_Dodatkowy0,80nmRozPion_VP-031nr2/wzNr89sfinisz0,025_0,80nmRozPion_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr89sfinisz0_025_Dodatkowy0_80nmRozPion_VP031nr2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr89sfinisz0_025_Dodatkowy0_80nmRozPion_VP031nr2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr89sfinisz0,025_VP-031nr2)---------

wzNr89sfinisz0_025_VP031nr2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#4/wzNr89sfinisz0,025_VP-031nr2/wzNr89sfinisz0,025_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)
x <- c(wzNr89sfinisz0_025_VP031nr2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr89sfinisz0_025_VP031nr2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr91szlifCz0,1_VP-031nr4)---------

wzNr91szlifCz0_1_VP031nr4<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#5/wzNr91szlifCz0,1_VP-031nr4/wzNr91szlifCz0,1_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr91szlifCz0_1_VP031nr4$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr91szlifCz0_1_VP031nr4$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr91szlifCz0,2_VP-031nr4)---------

wzNr91szlifCz0_2_VP031nr4<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#5/wzNr91szlifCz0,2_VP-031nr4/wzNr91szlifCz0,2_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr91szlifCz0_2_VP031nr4$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr91szlifCz0_2_VP031nr4$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr91szlifCz0,4_VP-031nr4)---------

wzNr91szlifCz0_4_VP031nr4<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#5/wzNr91szlifCz0,4_VP-031nr4/wzNr91szlifCz0,4_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr91szlifCz0_4_VP031nr4$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr91szlifCz0_4_VP031nr4$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr91szlifCz0,8_VP-031nr4)---------

wzNr91szlifCz0_8_VP031nr4<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#5/wzNr91szlifCz0,8_VP-031nr4/wzNr91szlifCz0,8_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr91szlifCz0_8_VP031nr4$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr91szlifCz0_8_VP031nr4$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr91szlifCz1,6_VP-031nr4)---------

wzNr91szlifCz1_6_VP031nr4<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#5/wzNr91szlifCz1,6_VP-031nr4/wzNr91szlifCz1,6_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)
x <- c(wzNr91szlifCz1_6_VP031nr4$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr91szlifCz1_6_VP031nr4$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr91szlifCzPrz0,1_VP-031nr4)---------

wzNr91szlifCzPrz0_1_VP031nr4<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#6/wzNr91szlifCzPrz0,1_VP-031nr4/wzNr91szlifCzPrz0,1_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)
x <- c(wzNr91szlifCzPrz0_1_VP031nr4$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr91szlifCzPrz0_1_VP031nr4$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr91szlifCzPrz0,2_VP-031nr4)---------

wzNr91szlifCzPrz0_2_VP031nr4<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#6/wzNr91szlifCzPrz0,2_VP-031nr4/wzNr91szlifCzPrz0,2_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr91szlifCzPrz0_2_VP031nr4$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr91szlifCzPrz0_2_VP031nr4$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr91szlifCzPrz0,4_VP-031nr4)---------

wzNr91szlifCzPrz0_4_VP031nr4<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#6/wzNr91szlifCzPrz0,4_VP-031nr4/wzNr91szlifCzPrz0,4_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)
x <- c(wzNr91szlifCzPrz0_4_VP031nr4$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr91szlifCzPrz0_4_VP031nr4$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr91szlifCzPrz0,8_VP-031nr4)---------

wzNr91szlifCzPrz0_8_VP031nr4<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#6/wzNr91szlifCzPrz0,8_VP-031nr4/wzNr91szlifCzPrz0,8_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr91szlifCzPrz0_8_VP031nr4$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr91szlifCzPrz0_8_VP031nr4$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr91szlifCzPrz1,6_VP-031nr4)---------

wzNr91szlifCzPrz1_6_VP031nr4<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#6/wzNr91szlifCzPrz1,6_VP-031nr4/wzNr91szlifCzPrz1,6_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr91szlifCzPrz1_6_VP031nr4$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(wzNr91szlifCzPrz1_6_VP031nr4$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#----------------------------Tactile_profilometer_KG- STAL-V3-Toczenie----------------------------------------------------------------------------------------------------------------------------------
library(readxl)    
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
KG_for_R<-read_excel_allsheets("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/DK-TOPO/STATYSTYKA.xlsx")
 
x <- c(as.numeric(KG_for_R$`STAL-V3-Toczenie`$`Ra [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(as.numeric(KG_for_R$`STAL-V3-Toczenie`$`Rz [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#----------------------------Tactile_profilometer_STAL-V4-Frezowanie----------------------------------------------------------------------------------------------------------------------------------
x <- c(as.numeric(KG_for_R$`STAL-V4-Frezowanie`$`Ra [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(as.numeric(KG_for_R$`STAL-V4-Frezowanie`$`Rz [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#----------------------------Tactile_profilometer_STAL-V4-Struganie----------------------------------------------------------------------------------------------------------------------------------
x <- c(as.numeric(KG_for_R$`STAL-V4-Struganie`$`Ra [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(as.numeric(KG_for_R$`STAL-V4-Struganie`$`Rz [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#----------------------------Tactile_profilometer_STAL-V5-Frezowanie----------------------------------------------------------------------------------------------------------------------------------
x <- c(as.numeric(KG_for_R$`STAL-V5-Frezowanie`$`Ra [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(as.numeric(KG_for_R$`STAL-V5-Frezowanie`$`Rz [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#----------------------------Tactile_profilometer_STAL-V5-Struganie----------------------------------------------------------------------------------------------------------------------------------
x <- c(as.numeric(KG_for_R$`STAL-V5-Struganie`$`Ra [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(as.numeric(KG_for_R$`STAL-V5-Struganie`$`Rz [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#----------------------------Tactile_profilometer_STAL-V5-Toczenie----------------------------------------------------------------------------------------------------------------------------------
x <- c(as.numeric(KG_for_R$`STAL-V5-Toczenie`$`Ra [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(as.numeric(KG_for_R$`STAL-V5-Toczenie`$`Rz [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#----------------------------Tactile_profilometer_STAL-V5-Toczenie-W----------------------------------------------------------------------------------------------------------------------------------
x <- c(as.numeric(KG_for_R$`STAL-V5-Toczenie-W`$`Ra [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(as.numeric(KG_for_R$`STAL-V5-Toczenie-W`$`Rz [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#----------------------------Tactile_profilometer_STAL-V6-Frezowanie----------------------------------------------------------------------------------------------------------------------------------
x <- c(as.numeric(KG_for_R$`STAL-V6-Frezowanie`$`Ra [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(as.numeric(KG_for_R$`STAL-V6-Frezowanie`$`Rz [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#----------------------------Tactile_profilometer_STAL-V6-Struganie----------------------------------------------------------------------------------------------------------------------------------
x <- c(as.numeric(KG_for_R$`STAL-V6-Struganie`$`Ra [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(as.numeric(KG_for_R$`STAL-V6-Struganie`$`Rz [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#----------------------------Tactile_profilometer_STAL-V6-Szlifowanie----------------------------------------------------------------------------------------------------------------------------------
x <- c(as.numeric(KG_for_R$`STAL-V6-Szlifowanie`$`Ra [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(as.numeric(KG_for_R$`STAL-V6-Szlifowanie`$`Rz [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#----------------------------Tactile_profilometer_STAL-V6-Toczenie----------------------------------------------------------------------------------------------------------------------------------
x <- c(as.numeric(KG_for_R$`STAL-V6-Toczenie`$`Ra [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(as.numeric(KG_for_R$`STAL-V6-Toczenie`$`Rz [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#----------------------------Tactile_profilometer_STAL-V6-Toczenie-W----------------------------------------------------------------------------------------------------------------------------------
x <- c(as.numeric(KG_for_R$`STAL-V6-Toczenie-W`$`Ra [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(as.numeric(KG_for_R$`STAL-V6-Toczenie-W`$`Rz [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#----------------------------Tactile_profilometer_STAL-V7-Frezowanie----------------------------------------------------------------------------------------------------------------------------------
x <- c(as.numeric(KG_for_R$`STAL-V7-Frezowanie`$`Ra [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);


x <- c(as.numeric(KG_for_R$`STAL-V7-Frezowanie`$`Rz [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)


#----------------------------Tactile_profilometer_STAL-V7-Struganie----------------------------------------------------------------------------------------------------------------------------------
x <- c(as.numeric(KG_for_R$`STAL-V7-Struganie`$`Ra [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(as.numeric(KG_for_R$`STAL-V7-Struganie`$`Rz [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)


#----------------------------Tactile_profilometer_STAL-V7-Szlifowanie----------------------------------------------------------------------------------------------------------------------------------
x <- c(as.numeric(KG_for_R$`STAL-V7-Szlifowanie`$`Ra [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(as.numeric(KG_for_R$`STAL-V7-Szlifowanie`$`Rz [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)


#----------------------------Tactile_profilometer_STAL-V7-Toczenie-W----------------------------------------------------------------------------------------------------------------------------------
x <- c(as.numeric(KG_for_R$`STAL-V7-Toczenie-W`$`Ra [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(as.numeric(KG_for_R$`STAL-V7-Toczenie-W`$`Rz [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#----------------------------Tactile_profilometer_STAL-V8-Toczenie----------------------------------------------------------------------------------------------------------------------------------
x <- c(as.numeric(KG_for_R$`STAL-V8-Toczenie`$`Ra [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(as.numeric(KG_for_R$`STAL-V8-Toczenie`$`Rz [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#----------------------------Tactile_profilometer_STAL-V8-Toczenie-W----------------------------------------------------------------------------------------------------------------------------------
x <- c(as.numeric(KG_for_R$`STAL-V8-Toczenie-W`$`Ra [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

x <- c(as.numeric(KG_for_R$`STAL-V8-Toczenie-W`$`Rz [µm]`))

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)



#------------------------------GUM_TaylorHobson(wzNr92szlifObw0,1_VP-031nr5)---------
library(readr) # R package for csv files manipulation
wzNr92szlifObw0_1_VP031nr5<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#7/wzNr92szlifObw0,1_VP-031nr5/wzNr92szlifObw0,1_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)
 
x <- c(wzNr92szlifObw0_1_VP031nr5$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x # measurement function
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);

 
 
 
x <- c(wzNr92szlifObw0_1_VP031nr5$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);
 

system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)


#------------------------------GUM_TaylorHobson(wzNr92szlifObw0,1innyLc_VP-031nr5)---------

wzNr92szlifObw0_1innyLc_VP031nr5<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#7/wzNr92szlifObw0,1innyLc_VP-031nr5/wzNr92szlifObw0,1innyLc_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr92szlifObw0_1innyLc_VP031nr5$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr92szlifObw0_1innyLc_VP031nr5$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr92szlifObw0,2_VP-031nr5)---------

wzNr92szlifObw0_2_VP031nr5<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#7/wzNr92szlifObw0,2_VP-031nr5/wzNr92szlifObw0,2_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr92szlifObw0_2_VP031nr5$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr92szlifObw0_2_VP031nr5$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr92szlifObw0,4_VP-031nr5)---------

wzNr92szlifObw0_4_VP031nr5<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#7/wzNr92szlifObw0,4_VP-031nr5/wzNr92szlifObw0,4_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr92szlifObw0_4_VP031nr5$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr92szlifObw0_4_VP031nr5$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr92szlifObw0,8_VP-031nr5)---------

wzNr92szlifObw0_8_VP031nr5<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#7/wzNr92szlifObw0,8_VP-031nr5/wzNr92szlifObw0,8_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr92szlifObw0_8_VP031nr5$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr92szlifObw0_8_VP031nr5$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr92szlifObw1,6_VP-031nr5)---------

wzNr92szlifObw1_6_VP031nr5<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#7/wzNr92szlifObw1,6_VP-031nr5/wzNr92szlifObw1,6_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr92szlifObw1_6_VP031nr5$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr92szlifObw1_6_VP031nr5$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
toc()
#------------------------------GUM_TaylorHobson(wzNr92docier0,1_VP-031nr5)---------

wzNr92docier0_1_VP031nr5<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#8/wzNr92docier0,1_VP-031nr5/wzNr92docier0,1_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr92docier0_1_VP031nr5$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr92docier0_1_VP031nr5$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
toc()
#------------------------------GUM_TaylorHobson(wzNr92docier0,2_VP-031nr5)---------

wzNr92docier0_2_VP031nr5<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#8/wzNr92docier0,2_VP-031nr5/wzNr92docier0,2_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr92docier0_2_VP031nr5$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr92docier0_2_VP031nr5$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr92docier0,4_VP-031nr5)---------

wzNr92docier0_4_VP031nr5<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#8/wzNr92docier0,4_VP-031nr5/wzNr92docier0,4_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr92docier0_4_VP031nr5$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr92docier0_4_VP031nr5$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
toc()
#------------------------------GUM_TaylorHobson(wzNr92docier0,05_VP-031nr5)---------

wzNr92docier0_05_VP031nr5<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#8/wzNr92docier0,05_VP-031nr5/wzNr92docier0,05_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr92docier0_05_VP031nr5$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr92docier0_05_VP031nr5$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------GUM_TaylorHobson(wzNr92docier0,025_VP-031nr5)---------

wzNr92docier0_025_VP031nr5<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#8/wzNr92docier0,025_VP-031nr5/wzNr92docier0,025_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr92docier0_025_VP031nr5$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr92docier0_025_VP031nr5$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#------------------------------PK_TaylorHobson(stal_frez_kl5)---------
stal_frez_kl5<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/PK/Taylor Hobson I Pro/stal_frez_kl5/param.csv",show_col_types = FALSE, col_names = FALSE)
stal_frez_kl5_Ra = subset(stal_frez_kl5, X1 == "Ra") 
stal_frez_kl5_Rz = subset(stal_frez_kl5, X1 == "Rz")
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(stal_frez_kl5_Ra$X2)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(stal_frez_kl5_Rz$X2)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
#------------------------------PK_TaylorHobson(stal_frez_kl8)---------
stal_frez_kl8<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/PK/Taylor Hobson I Pro/stal_frez_kl8/param.csv",show_col_types = FALSE, col_names = FALSE)
stal_frez_kl8_Ra = subset(stal_frez_kl8, X1 == "Ra") 
stal_frez_kl8_Rz = subset(stal_frez_kl8, X1 == "Rz")
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(stal_frez_kl8_Ra$X2)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(stal_frez_kl8_Rz$X2)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
#------------------------------PK_TaylorHobson(stal_tocz_kl1)---------
stal_tocz_kl1<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/PK/Taylor Hobson I Pro/stal_tocz_kl1/param.csv",show_col_types = FALSE, col_names = FALSE)
stal_tocz_kl1_Ra = subset(stal_tocz_kl1, X1 == "Ra") 
stal_tocz_kl1_Rz = subset(stal_tocz_kl1, X1 == "Rz")
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(stal_tocz_kl1_Ra$X2)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(stal_tocz_kl1_Rz$X2)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
#------------------------------PK_TaylorHobson(stal_tocz_kl3)---------
stal_tocz_kl3<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/PK/Taylor Hobson I Pro/stal_tocz_kl3/param.csv",show_col_types = FALSE, col_names = FALSE)
stal_tocz_kl3_Ra = subset(stal_tocz_kl3, X1 == "Ra") 
stal_tocz_kl3_Rz = subset(stal_tocz_kl3, X1 == "Rz")
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(stal_tocz_kl3_Ra$X2)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(stal_tocz_kl3_Rz$X2)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
#------------------------------PK_TaylorHobson(stal_tocz_kl4)---------
stal_tocz_kl4<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/PK/Taylor Hobson I Pro/stal_tocz_kl4/param.csv",show_col_types = FALSE, col_names = FALSE)
stal_tocz_kl4_Ra = subset(stal_tocz_kl4, X1 == "Ra") 
stal_tocz_kl4_Rz = subset(stal_tocz_kl4, X1 == "Rz")
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(stal_tocz_kl4_Ra$X2)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(stal_tocz_kl4_Rz$X2)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
# ----------------------------------CCI_Ra_Rz--------------------------------
folder <- "/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/PW/CCI/"       
# path to folder that holds multiple .csv files 

file_list <- list.files(path=folder, pattern="*.csv")  
# create list of all .csv files in folder
for (i in 1:length(file_list)){ 
  Ra<-assign(file_list[i],  
             read.csv(paste(folder, file_list[i], sep=''), sep = ";")[1:1,14] 
  )
  if (Ra > 6) {
    Ra = Ra/1000;
  } 
  Rz<-assign(file_list[i],  
         read.csv(paste(folder, file_list[i], sep=''), sep = ";")[1:1,11] 
  )
if (Rz > 31) {
     Rz = Rz/1000;
}

  x <- Ra
  
  Nx <- length(x) # number of data points in x
  
  P <- 0.95 # confidence level
  R <- 10^5 # number of times to resample the data
  
  bLin <- 0.01 # linearity
  bRep <- 0.01 # repeatability
  bCal <- 0.005/2 # calibration error
  bProbe <- 0.01 # Probe error 
  
  
  boot.r <- numeric(R) # vector for r values
  for (i in 1:R) {
    boot.sample.x <- sample(x,size=Nx,replace=T)
    
    beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
    beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
    beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
    beta4x <- bProbe # Probe error 
    xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
    
    boot.r[i] <- xs 
  }
  #hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
  #mean(boot.r)
  quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
  uncert<-mean(boot.r)-quant[[1]]
  
  e<-set_errors(mean(boot.r), uncert)
  options(errors.notation = "plus-minus") 
  RONt_uncert<-round(as.numeric(NA),digits=2);
  
  
  system_type<-"CCI";
  Ra<-round(as.numeric(mean(boot.r)),digits=2);
  Ra_uncert<-round(as.numeric(uncert),digits=2);
  material<-as.numeric(1);
  standard<-as.numeric(1);
  F<-as.numeric(1);
  RONt<-round(as.numeric(NA),digits=2);
  
  
  x <- Rz
  
  Nx <- length(x) # number of data points in x
  
  P <- 0.95 # confidence level
  R <- 10^5  # number of times to resample the data
  
  bLin <- 0.01 # linearity
  bRep <- 0.01 # repeatability
  bCal <- 0.005/2 # calibration error
  bProbe <- 0.01 # Probe error 
  
  
  boot.r <- numeric(R) # vector for r values
  for (i in 1:R) {
    boot.sample.x <- sample(x,size=Nx,replace=T)
    
    beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
    beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
    beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
    beta4x <- bProbe # Probe error 
    xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
    
    boot.r[i] <- xs 
  }
  #hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
  #mean(boot.r)
  quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
  uncert<-mean(boot.r)-quant[[1]]
  
  e<-set_errors(mean(boot.r), uncert)
  options(errors.notation = "plus-minus") 
 
  Rz<-round(as.numeric(mean(boot.r)),digits=2);
  Rz_uncert<-round(as.numeric(uncert),digits=2);
  
  write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
 
} 
# ----------------------------------PGI_Ra_Rz--------------------------------
folder <- "/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/PW/PGI/"       
# path to folder that holds multiple .csv files 

file_list <- list.files(path=folder, pattern="*.csv")  

for (i in 1:length(file_list)){ 
  Ra<-assign(file_list[i],  
             read.csv(paste(folder, file_list[i], sep=''), sep = ";")[1:1,1] 
  )
  if (Ra > 6) {
    Ra = Ra/1000;
  } 
  Rz<-assign(file_list[i],  
             read.csv(paste(folder, file_list[i], sep=''), sep = ";")[1:1,2] 
  )
  if (Rz > 31) {
    Rz = Rz/1000;
  }

  x <- Ra
  
  Nx <- length(x) # number of data points in x
  
  P <- 0.95 # confidence level
  R <- 10^5 # number of times to resample the data
  
  bLin <- 0.01 # linearity
  bRep <- 0.01 # repeatability
  bCal <- 0.005/2 # calibration error
  bProbe <- 0.01 # Probe error 
  
  
  boot.r <- numeric(R) # vector for r values
  for (i in 1:R) {
    boot.sample.x <- sample(x,size=Nx,replace=T)
    
    beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
    beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
    beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
    beta4x <- bProbe # Probe error 
    xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
    
    boot.r[i] <- xs 
  }
  #hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
  #mean(boot.r)
  quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
  uncert<-mean(boot.r)-quant[[1]]
  
  e<-set_errors(mean(boot.r), uncert)
  options(errors.notation = "plus-minus") 
  RONt_uncert<-round(as.numeric(NA),digits=2);
  
  
  system_type<-"PGI";
  Ra<-round(as.numeric(mean(boot.r)),digits=2);
  Ra_uncert<-round(as.numeric(uncert),digits=2);
  material<-as.numeric(1);
  standard<-as.numeric(1);
  F<-as.numeric(1);
  RONt<-round(as.numeric(NA),digits=2);
  
  
  x <- Rz
  
  Nx <- length(x) # number of data points in x
  
  P <- 0.95 # confidence level
  R <- 10^5  # number of times to resample the data
  
  bLin <- 0.01 # linearity
  bRep <- 0.01 # repeatability
  bCal <- 0.005/2 # calibration error
  bProbe <- 0.01 # Probe error 
  
  
  boot.r <- numeric(R) # vector for r values
  for (i in 1:R) {
    boot.sample.x <- sample(x,size=Nx,replace=T)
    
    beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
    beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
    beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
    beta4x <- bProbe # Probe error 
    xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
    
    boot.r[i] <- xs 
  }
  #hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
  #mean(boot.r)
  quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
  uncert<-mean(boot.r)-quant[[1]]
  
  e<-set_errors(mean(boot.r), uncert)
  options(errors.notation = "plus-minus") 
  
  Rz<-round(as.numeric(mean(boot.r)),digits=2);
  Rz_uncert<-round(as.numeric(uncert),digits=2);
  
  write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
  
} 
#---------------------------GUM_TaylorHobson(wzNr73szlifCz_cieciePoprz0,1_VP1-4)-----
wzNr73szlifCz_cieciePoprz0_1_VP1_4<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#9/wzNr73szlifCz_cieciePoprz0,1_VP1-4/wzNr73szlifCz_cieciePoprz0,1_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr73szlifCz_cieciePoprz0_1_VP1_4$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr73szlifCz_cieciePoprz0_1_VP1_4$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
#----------------------------GUM_TaylorHobson(wzNr74szlifCz_cieciePoprz0,2_VP1-4)-------------------------------------
wzNr74szlifCz_cieciePoprz0_2_VP1_4<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#9/wzNr74szlifCz_cieciePoprz0,2_VP1-4/wzNr74szlifCz_cieciePoprz0,2_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr74szlifCz_cieciePoprz0_2_VP1_4$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr74szlifCz_cieciePoprz0_2_VP1_4$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
#---------------------------GUM_TaylorHobson(wzNr75szlifCz_cieciePoprz0,4_VP1-4)------------------------
wzNr75szlifCz_cieciePoprz0_4_VP1_4<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#9/wzNr75szlifCz_cieciePoprz0,4_VP1-4/wzNr75szlifCz_cieciePoprz0,4_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr75szlifCz_cieciePoprz0_4_VP1_4$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr75szlifCz_cieciePoprz0_4_VP1_4$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
#---------------------------GUM_TaylorHobson(wzNr76szlifCz_cieciePoprz0,8_VP1-4)------------------------
wzNr76szlifCz_cieciePoprz0_8_VP1_4<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#9/wzNr76szlifCz_cieciePoprz0,8_VP1-4/wzNr76szlifCz_cieciePoprz0,8_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr76szlifCz_cieciePoprz0_8_VP1_4$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr76szlifCz_cieciePoprz0_8_VP1_4$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)
#---------------------------GUM_TaylorHobson(wzNr77szlifCz_cieciePoprz1,6_VP1-4)------------------------
wzNr77szlifCz_cieciePoprz1_6_VP1_4<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#9/wzNr77szlifCz_cieciePoprz1,6_VP1-4/wzNr77szlifCz_cieciePoprz1,6_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr77szlifCz_cieciePoprz1_6_VP1_4$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr77szlifCz_cieciePoprz1_6_VP1_4$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr43frezRofrezCz1,6_VP1-2)------------------------
wzNr43frezRofrezCz1_6_VP1_2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr43frezRofrezCz1,6_VP1-2/wzNr43frezRofrezCz1,6_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr43frezRofrezCz1_6_VP1_2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr43frezRofrezCz1_6_VP1_2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr44frezRofrezCz3,2_VP1-2)------------------------
wzNr44frezRofrezCz3_2_VP1_2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr44frezRofrezCz3,2_VP1-2/wzNr44frezRofrezCz3,2_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr44frezRofrezCz3_2_VP1_2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr44frezRofrezCz3_2_VP1_2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr45frezRofrezCz6,3_VP1-2)------------------------
wzNr45frezRofrezCz6_3_VP1_2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr45frezRofrezCz6,3_VP1-2/wzNr45frezRofrezCz6,3_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr45frezRofrezCz6_3_VP1_2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr45frezRofrezCz6_3_VP1_2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)


#---------------------------GUM_TaylorHobson(wzNr46frezRofrezCz12,5_VP1-2)------------------------
wzNr46frezRofrezCz12_5_VP1_2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr46frezRofrezCz12,5_VP1-2/wzNr46frezRofrezCz12,5_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr46frezRofrezCz12_5_VP1_2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr46frezRofrezCz12_5_VP1_2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr47frezRofrezCz25_VP1-2)------------------------
wzNr47frezRofrezCz25_VP1_2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr47frezRofrezCz25_VP1-2/wzNr47frezRofrezCz25_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr47frezRofrezCz25_VP1_2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr47frezRofrezCz25_VP1_2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr48frezSkfrezCz1,6_VP1-2)------------------------
wzNr48frezSkfrezCz1_6_VP1_2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr48frezSkfrezCz1,6_VP1-2/wzNr48frezSkfrezCz1,6_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr48frezSkfrezCz1_6_VP1_2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr48frezSkfrezCz1_6_VP1_2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr49frezSkfrezCz3,2_VP1-2)------------------------
wzNr49frezSkfrezCz3_2_VP1_2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr49frezSkfrezCz3,2_VP1-2/wzNr49frezSkfrezCz3,2_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr49frezSkfrezCz3_2_VP1_2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr49frezSkfrezCz3_2_VP1_2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr50frezSkfrezCz6,3_VP1-2)------------------------
wzNr50frezSkfrezCz6_3_VP1_2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr50frezSkfrezCz6,3_VP1-2/wzNr50frezSkfrezCz6,3_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr50frezSkfrezCz6_3_VP1_2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr50frezSkfrezCz6_3_VP1_2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr50frezSkfrezCz6,3innyLc_VP1-2)------------------------
wzNr50frezSkfrezCz6_3innyLc_VP1_2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr50frezSkfrezCz6,3innyLc_VP1-2/wzNr50frezSkfrezCz6,3innyLc_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr50frezSkfrezCz6_3innyLc_VP1_2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr50frezSkfrezCz6_3innyLc_VP1_2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr51frezSkfrezCz12,5_VP1-2)------------------------
wzNr51frezSkfrezCz12_5_VP1_2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr51frezSkfrezCz12,5_VP1-2/wzNr51frezSkfrezCz12,5_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr51frezSkfrezCz12_5_VP1_2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr51frezSkfrezCz12_5_VP1_2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr52frezSkfrezCz25_VP1-2)------------------------
wzNr52frezSkfrezCz25_VP1_2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr52frezSkfrezCz25_VP1-2/wzNr52frezSkfrezCz25_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr52frezSkfrezCz25_VP1_2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr52frezSkfrezCz25_VP1_2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr53frezRofrezWalc1,6_VP1-2)------------------------
wzNr53frezRofrezWalc1_6_VP1_2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr53frezRofrezWalc1,6_VP1-2/wzNr53frezRofrezWalc1,6_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr53frezRofrezWalc1_6_VP1_2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr53frezRofrezWalc1_6_VP1_2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr54frezRofrezWalc3,2_VP1-2)------------------------
wzNr54frezRofrezWalc3_2_VP1_2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr54frezRofrezWalc3,2_VP1-2/wzNr54frezRofrezWalc3,2_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr54frezRofrezWalc3_2_VP1_2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr54frezRofrezWalc3_2_VP1_2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)


#---------------------------GUM_TaylorHobson(wzNr55frezRofrezWalc6,3_VP1-2)------------------------
wzNr55frezRofrezWalc6_3_VP1_2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr55frezRofrezWalc6,3_VP1-2/wzNr55frezRofrezWalc6,3_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr55frezRofrezWalc6_3_VP1_2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr55frezRofrezWalc6_3_VP1_2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr56frezRofrezWalc12,5_VP1-2)------------------------
wzNr56frezRofrezWalc12_5_VP1_2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr56frezRofrezWalc12,5_VP1-2/wzNr56frezRofrezWalc12,5_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr56frezRofrezWalc12_5_VP1_2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr56frezRofrezWalc12_5_VP1_2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr57frezRofrezWalc25_VP1-2)------------------------
wzNr57frezRofrezWalc25_VP1_2<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr57frezRofrezWalc25_VP1-2/wzNr57frezRofrezWalc25_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr57frezRofrezWalc25_VP1_2$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr57frezRofrezWalc25_VP1_2$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr90frezCz3,2innyLc_VP-031nr3)------------------------
wzNr90frezCz3_2innyLc_VP_031nr3<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr90frezCz3,2innyLc_VP-031nr3/wzNr90frezCz3,2innyLc_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr90frezCz3_2innyLc_VP_031nr3$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr90frezCz3_2innyLc_VP_031nr3$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr90frezCz6,3_VP-031nr3)------------------------
wzNr90frezCz6_3_VP_031nr3<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr90frezCz6,3_VP-031nr3/wzNr90frezCz6,3_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr90frezCz6_3_VP_031nr3$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr90frezCz6_3_VP_031nr3$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr90frezCz6,3innyLc_VP-031nr3)------------------------
wzNr90frezCz6_3innyLc_VP_031nr3<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr90frezCz6,3innyLc_VP-031nr3/wzNr90frezCz6,3innyLc_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr90frezCz6_3innyLc_VP_031nr3$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr90frezCz6_3innyLc_VP_031nr3$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr90frezCz12,5_VP-031nr3)------------------------
wzNr90frezCz12_5_VP_031nr3<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr90frezCz12,5_VP-031nr3/wzNr90frezCz12,5_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr90frezCz12_5_VP_031nr3$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr90frezCz12_5_VP_031nr3$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr90frezCz25_VP-031nr3)------------------------
wzNr90frezCz25_VP_031nr3<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr90frezCz25_VP-031nr3/wzNr90frezCz25_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr90frezCz25_VP_031nr3$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr90frezCz25_VP_031nr3$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr90frezCz25innyLc_VP-031nr3)------------------------
wzNr90frezCz25innyLc_VP_031nr3<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr90frezCz25innyLc_VP-031nr3/wzNr90frezCz25innyLc_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr90frezCz25innyLc_VP_031nr3$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr90frezCz25innyLc_VP_031nr3$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr90frezWalc1,6_VP-031nr3)------------------------
wzNr90frezWalc1_6_VP_031nr3<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr90frezWalc1,6_VP-031nr3/wzNr90frezWalc1,6_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr90frezWalc1_6_VP_031nr3$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr90frezWalc1_6_VP_031nr3$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr90frezWalc1,6innyLc_VP-031nr3)------------------------
wzNr90frezWalc1_6innyLc_VP_031nr3<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr90frezWalc1,6innyLc_VP-031nr3/wzNr90frezWalc1,6innyLc_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr90frezWalc1_6innyLc_VP_031nr3$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr90frezWalc1_6innyLc_VP_031nr3$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr90frezWalc3,2_VP-031nr3)------------------------
wzNr90frezWalc3_2_VP_031nr3<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr90frezWalc3,2_VP-031nr3/wzNr90frezWalc3,2_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr90frezWalc3_2_VP_031nr3$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr90frezWalc3_2_VP_031nr3$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr90frezWalc6,3_VP-031nr3)------------------------
wzNr90frezWalc6_3_VP_031nr3<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr90frezWalc6,3_VP-031nr3/wzNr90frezWalc6,3_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr90frezWalc6_3_VP_031nr3$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr90frezWalc6_3_VP_031nr3$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr90frezWalc12,5_VP-031nr3)------------------------
wzNr90frezWalc12_5_VP_031nr3<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr90frezWalc12,5_VP-031nr3/wzNr90frezWalc12,5_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr90frezWalc12_5_VP_031nr3$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr90frezWalc12_5_VP_031nr3$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr90frezWalc25_VP-031nr3)------------------------
wzNr90frezWalc25_VP_031nr3<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr90frezWalc25_VP-031nr3/wzNr90frezWalc25_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr90frezWalc25_VP_031nr3$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr90frezWalc25_VP_031nr3$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr93poler0,2_VP-031nr6)------------------------
wzNr93poler0_2_VP_031nr6<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr93poler0,2_VP-031nr6/wzNr93poler0,2_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr93poler0_2_VP_031nr6$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr93poler0_2_VP_031nr6$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr93szlifScierTasm0,4_VP-031nr6)------------------------
wzNr93szlifScierTasm0_4_VP_031nr6<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr93szlifScierTasm0,4_VP-031nr6/wzNr93szlifScierTasm0,4_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr93szlifScierTasm0_4_VP_031nr6$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr93szlifScierTasm0_4_VP_031nr6$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr93szlifScierTasm0,8_VP-031nr6)------------------------
wzNr93szlifScierTasm0_8_VP_031nr6<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr93szlifScierTasm0,8_VP-031nr6/wzNr93szlifScierTasm0,8_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr93szlifScierTasm0_8_VP_031nr6$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr93szlifScierTasm0_8_VP_031nr6$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

#---------------------------GUM_TaylorHobson(wzNr93szlifScierTasm1,6_VP-031nr6)------------------------
wzNr93szlifScierTasm1_6_VP_031nr6<-read_csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/GUM#10/wzNr93szlifScierTasm1,6_VP-031nr6/wzNr93szlifScierTasm1,6_Powierzchnie_analiza_wyniki.csv",show_col_types = FALSE)
#fitW <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "weibull")
#fitg <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "gamma")
#fitln <- fitdist(wzNr88tocz64_VP031nr1$Ra/10000, "lnorm")
#fitn <- fitdist(wzNr88tocz64_VP031nr1$Ra*10000, "norm")
#denscomp(list(fitW, fitg,fitn), legendtext=c("Weibull", "gamma", "norm"),fitlwd=c(1,3,3))

#u<-(sd(wzNr88tocz64_VP031nr1$Ra*1000)/(sqrt(length(wzNr88tocz64_VP031nr1$Ra))))
#mean(wzNr88tocz64_VP031nr1$Ra*1000)
#u
#2*u
#shapiro.test(wzNr88tocz64_VP031nr1$Ra)

x <- c(wzNr93szlifScierTasm1_6_VP_031nr6$Ra*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5 # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Ra*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Ra<-round(as.numeric(mean(boot.r)),digits=2);
Ra_uncert<-round(as.numeric(uncert),digits=2);
material<-as.numeric(1);
standard<-as.numeric(1);
F<-as.numeric(1);
RONt<-round(as.numeric(NA),digits=2);




x <- c(wzNr93szlifScierTasm1_6_VP_031nr6$Rz*1000)

Nx <- length(x) # number of data points in x

P <- 0.95 # confidence level
R <- 10^5  # number of times to resample the data

bLin <- 0.01 # linearity
bRep <- 0.01 # repeatability
bCal <- 0.005/2 # calibration error
bProbe <- 0.01 # Probe error 


boot.r <- numeric(R) # vector for r values
for (i in 1:R) {
  boot.sample.x <- sample(x,size=Nx,replace=T)
  
  beta1x <- rnorm(n=1,mean=0,sd=bLin) # linearity
  beta2x <- runif(n=1, min = 0, max = 0.1) # repeatability
  beta3x <- rnorm(n=1,mean=0,sd=bCal) # calibration
  beta4x <- bProbe # Probe error 
  xs <- mean(boot.sample.x)+beta1x+beta2x+beta3x+beta4x
  
  boot.r[i] <- xs 
}
hist(boot.r,xlab=(expression(Rz*" ["*mu*"m]")))
#mean(boot.r)
quant<-quantile(boot.r, probs = c((1-P)/2,(1+P)/2))
uncert<-mean(boot.r)-quant[[1]]

e<-set_errors(mean(boot.r), uncert)
options(errors.notation = "plus-minus") 
RONt_uncert<-round(as.numeric(NA),digits=2);


system_type<-"TP";
Rz<-round(as.numeric(mean(boot.r)),digits=2);
Rz_uncert<-round(as.numeric(uncert),digits=2);

write.table(data.frame(system_type, Ra, Ra_uncert, Rz, Rz_uncert, material, RONt, RONt_uncert, standard, F),file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input.txt", append=TRUE,sep=" ",col.names=FALSE,row.names=FALSE)

# #------------------------------MetrologyPackage--------------------------
# #sample area
# L<-5*2.54 #cm
# W<-8*2.54 #cm
# #sample mass
# m<-0.2543*1000 #mg
# #uncertainties
# L.u<-(1/16)*2.54 #cm (nearest 16th inch)
# W.u<-(1/16)*2.54 #cm
# m.u<-0.006*1000 #mg scale calibration data
# 
# require(metRology)
# d.set<-list(mass=m,Length=L,Width=W)
# d.set.u<-list(m.u,L.u,W.u)
# dent<-expression(7*(mass/(Length*Width)))
# uncert(obj=dent, x=d.set, u=d.set.u, method="GUM")
#----------------------------------------------------------------------------------
# #----------------------------------ML Algorithm_with_artif_data--------------------------------------------------------------------------------------
data_row <- data[sample(1:nrow(data)), ]     # Randomly reorder rows
data_row                                     # Print updated data
 library("readr")
 library(truncnorm)

##create data frame with 100 random integers between 1 and 50
 res <- as.data.frame(round(rtruncnorm(n = 1000,a = 1,b = 100,mean = 25,sd = 15) ,2)) 
# #define column names
 names(res) <- c('resolution')
 hist(res$resolution,labels = FALSE, main = '', 
      xlab=(expression(Resolution*" ["*mu*m*"]")),  cex.main=1.3, cex.lab=1.3,cex.axis=1.3)
# #create data frame with 1000 random integers between 10 and 30 with mean 12.2 and sd 4
 sigma <- as.data.frame(round(rtruncnorm(n = 1000,a = 10,b = 30,mean = 12.2,sd = 4) ,2)) 
# #define column names
 names(sigma) <- c('sigma')
 hist(sigma$sigma,labels = FALSE, main='',
 xlab=(expression(sigma*" ["*mu*m*"]")),  cex.main=1.3, cex.lab=1.3,cex.axis=1.3)
# 
# #view data frame
# #sigma
# 
 list <- c(1, 2, 3, 4)
# 
 number_of_variables<-4
# 
 materials<-expand.grid(data.frame(replicate(number_of_variables, list)))
# 
 str_mat<-c(t(materials) ) 
# 
 mat<-data.frame(str_mat)
# 
 mat <- as.numeric(str_mat)
# 
 material <- mat[1:1000]
# 
 hist(material,labels = FALSE, main = '',
      xlab='Material type',  cex.main=1.3, cex.lab=1.3,
     cex.axis=1.3,xaxt="n")
axis(1, at = seq(1, 4, by = 1), las=1,cex.axis=1.3)
# 
# 
 list <- c("CMM", "3D_Scan", "Microscope")
 number_of_variables<-5
 systems<-expand.grid(data.frame(replicate(number_of_variables, list)))
 str<-c(t(systems) ) 
 system<-data.frame(str)
 system <- as.factor(str)
 system_type <- system[1:1000]
 everything <-cbind(system_type, sigma, res, material)
 barplot(table(everything$system_type),
         xlab='System type', ylab='Frequency', cex.axis=1.3, cex.lab=1.3, cex.main=1.3, cex.sub=1.3,cex.names=1.3)
 library(caret)
# 
# # create a list of 80% of the rows in the original dataset we can use for training
 validation_index <- createDataPartition(everything$system_type, p=0.80, list=FALSE)
# # select 20% of the data for validation

 validation <- everything[-validation_index,]
# # use the remaining 80% of data to training and testing the models
 dataset <- everything[validation_index,]
# 
# # dimensions of dataset
# dim(dataset)
# 
# # list types for each attribute
 sapply(dataset, class)
# 
# # list the levels for the class
 levels(dataset$system_type)
# 
# # summarize the class distribution
 percentage <- prop.table(table(dataset$system_type)) * 100
 cbind(freq=table(dataset$dt), percentage=percentage)
# 
# # split input and output
 x <- dataset[,2:4]
 y <- dataset[,1]
# 
# # boxplot for each attribute on one image
 par(mfrow=c(1,3))
for(i in 1:3) {
   boxplot(x[,i], main=names(x)[i],notch = FALSE)
 }
# # scatterplot matrix
 featurePlot(x=x, y=y, plot="ellipse")
 
 # Customize upper panel
 upper.panel<-function(x, y){
   points(x,y, pch=19, col=c("red", "green3", "blue")[iris$Species])
   r <- round(cor(x, y), digits=2)
   txt <- paste0("R = ", r)
   usr <- par("usr"); on.exit(par(usr))
   par(usr = c(0, 1, 0, 1))
   text(0.5, 0.9, txt)
 }
 pairs(dataset[,1:4], lower.panel = NULL, 
       upper.panel = upper.panel)
 
 library(psych)
 pairs.panels(dataset[,1:4], 
              method = "pearson", # correlation method
              hist.col = "#00AFBB",
              density = TRUE,  # show density plots
              ellipses = TRUE # show correlation ellipses
 )

# # box and whisker plots for each attribute
 featurePlot(x=x, y=y, plot="box")
# # density plots for each attribute by class value
 scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)
# 
# # Run algorithms using 10-fold cross validation
 control <- trainControl(method="cv", number=10)
 metric <- "Accuracy"
# 
# # a) linear algorithms
 set.seed(7)
 fit.lda <- train(system_type~., data=dataset, method="lda", metric=metric, trControl=control, na.action=na.exclude)
 # b) nonlinear algorithms
 # CART
 set.seed(7)
 fit.cart <- train(system_type~., data=dataset, method="rpart", metric=metric, trControl=control)
 # kNN
 set.seed(7)
 fit.knn <- train(system_type~., data=dataset, method="knn", metric=metric, trControl=control)
 # c) advanced algorithms
 # SVM
 set.seed(7)
 fit.svm <- train(system_type~., data=dataset, method="svmRadial", metric=metric, trControl=control)
 
# # summarize accuracy of models
 results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm))
 #summary(results)
# 
 # compare accuracy of models
 dotplot(results)
# 
# # summarize Best Model
# print(fit.svm)
# 
# 
# 
# #estimate skill of SVM on the validation dataset
# predictions <- predict(fit.svm, validation)
# cm<-confusionMatrix(predictions, validation$system_type)
# overall <- cm$overall
# overall.accuracy <- round(overall['Accuracy'],2)
# 

# #----------------------------------ML Algorithm_with_exp_data--------------------------------------------------------------------------------------
# #data_row <- data[sample(1:nrow(data)), ]     # Randomly reorder rows
# #data_row                                     # Print updated data
 library("readr")
my_data <- read.table("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Projekty/NSMT/AI_GUM/Data/input_2.txt",header = TRUE, sep = " ",colClasses=c("factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
# Repeat All Rows
rows= c(1:nrow(my_data))
times = 9
input<-my_data[rep(rows, times),]

my_data <- input[sample(1:nrow(input)), ]
print(my_data)
length(my_data$system_type)

 library(truncnorm)

 library(caret)
# 
# # create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(my_data$system_type, p=0.70, list=FALSE)# 
# select 20% of the data for validation

validation <- my_data[-validation_index,]
# # use the remaining 80% of data to training and testing the models
dataset <- my_data[validation_index,]
# 
# # dimensions of dataset
length(dataset$system_type)
# 
# # list types for each attribute
 sapply(dataset, class)
# 
# # list the levels for the class
levels(dataset$system_type)
# 
# # summarize the class distribution
 percentage <- prop.table(table(dataset$system_type)) * 100
 cbind(freq=table(dataset$dt), percentage=percentage)
# 
# # split input and output
 x <- dataset[,2:10]
 y <- dataset[,1]
# 
# # boxplot for each attribute on one image
par(mfrow=c(1,1))
 #for(i in 1:10) {
#   boxplot(x[,i], main=names(x)[i],notch = TRUE)
 #}
# # scatterplot matrix
 featurePlot(x=dataset[,2:10], y=dataset[,1], plot = "box", 
             ## Pass in options to bwplot() 
             scales = list(y = list(relation="free"),
                           x = list(rot = 90)),  
             layout = c(2,1 ), 
             auto.key = list(columns = 2))

 
 # Customize upper panel
 upper.panel<-function(x, y){
   points(x,y, pch=19, col=c("red", "green3", "blue")[iris$Species])
   r <- round(cor(x, y), digits=2)
   txt <- paste0("R = ", r)
   usr <- par("usr"); on.exit(par(usr))
   par(usr = c(0, 1, 0, 1))
   text(0.5, 0.9, txt)
 }
 pairs(dataset[,1:8], lower.panel = NULL, 
       upper.panel = upper.panel)
 
 library(psych)
 pairs.panels(dataset[,1:6], 
              method = "pearson", # correlation method
              hist.col = "#00AFBB",
              density = TRUE,  # show density plots
              ellipses = TRUE # show correlation ellipses
 )
 

# # box and whisker plots for each attribute
 featurePlot(x=x, y=y, plot="box")
# # density plots for each attribute by class value
 scales <- list(x=list(relation="free"), y=list(relation="free"))
 featurePlot(x=x, y=y, plot="density", scales=scales)
# 
# # Run algorithms using 10-fold cross validation
 control <- trainControl(method="cv", number=10)
 metric <- "Accuracy"
# 
# # a) linear algorithms
# set.seed(7)
# fit.lda <- train(system_type~., data=dataset, method="lda", metric=metric, trControl=control, na.action=na.exclude)
# # b) nonlinear algorithms
# # CART
 set.seed(7)
 fit.cart <- train(system_type~., data=dataset, method="rpart", metric=metric, trControl=control)
# # kNN
 set.seed(7)
 fit.knn <- train(system_type~., data=dataset, method="knn", metric=metric, trControl=control)
# # c) advanced algorithms
# # SVM
 set.seed(7)
 fit.svm <- train(system_type~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# 
 # Random Forest
 set.seed(7)
 fit.rf <- train(system_type~., data=dataset, method="ranger", metric=metric, trControl=control)
 # train the LVQ model
 set.seed(7)
fit.Lvq <- train(system_type~., data=dataset, method="lvq", trControl=control)
 # train the GBM model
 set.seed(7)
fit.Gbm <- train(system_type~., data=dataset, method="gbm", trControl=control, verbose=FALSE)

 
 # # summarize accuracy of models
 results <- resamples(list(CART=fit.cart, KNN=fit.knn, SVM=fit.svm, RF= fit.rf, LVQ=fit.Lvq, GBM= fit.Gbm))
summary(results)
# 
# boxplots of results
bwplot(results)
# # compare accuracy of models
 #dotplot(results)
# 

dotplot(results, xlim = c(0.75, 1))[1]
 
 

# # summarize Best Model
 print(fit.rf)
# 
# 
# 
# #estimate skill of knn on the validation dataset
 predictions <- predict(fit.rf, validation)
 cm<-confusionMatrix(predictions, validation$system_type)
overall <- cm$overall
Accuracy<-round(overall['Accuracy'],5)
print(round(Accuracy*100,2))
Kappa <- round(overall['Kappa'],2)
Kappa
cm
 # 

# #-----------------Simple Neural Network implementation-----------------

library(tidyverse)
 library(neuralnet)
library(dplyr)
library(MASS)
my_data <- my_data %>% mutate_if(is.character, as.factor)
 summary(my_data)
 set.seed(245)
 data_rows <- floor(0.70 * nrow(my_data))
 train_indices <- sample(c(1:nrow(my_data)), data_rows)
 train_data <- my_data[train_indices,]
 test_data <- my_data[-train_indices,]
 
 model = neuralnet(
   system_type~Ra+Ra_uncert+Rz+Rz_uncert+material+RONt+RONt_uncert+standard+F,
   data=train_data,
   hidden=c(4,2),
   linear.output = FALSE,
  stepmax = 2000000
 )
 #import the function from Github
 library(devtools)
 source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
 
 #plot each model
 plot.nnet(model)

 
 #plot(model,rep = "best") # plot model
 
 
 pred <- predict(model, test_data)
 labels <- c("TP", "CCI", "PGI", "RoundScan", "CMM") 
 prediction_label <- data.frame(max.col(pred)) %>%      
   mutate(pred=labels[max.col.pred.]) %>% 
   dplyr::select(2) %>% 
   unlist() 
 
 table(test_data$system_type, prediction_label)
# 
 check = as.numeric(test_data$system_type) == max.col(pred)
 accuracy = (sum(check)/nrow(test_data))*100
 print(round(accuracy,2))

# 
# #--------------------------Convolutional Neural Network with Keras-------
library(keras)
library(tensorflow)
use_condaenv("r-tensorflow")
 c(c(x_train, y_train), c(x_test, y_test)) %<-% dataset_cifar10()
