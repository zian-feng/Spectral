
#Section I


# DEPENDENCIES
library(ggplot2)
library(dplyr)
library(hrbrthemes)


####### TIME SERIES 1

### PART 1 // SIMULATION

## GENERATING 5 SIGN WAVES

# A*cos(2*pi*w*t+phi)
pi=3.1415926
t<-seq(0,7*pi,1/45)
length(t)


f1=cos(2*pi*t/20 + 1.7*pi)
plot(t,f1, type="l")

f2=12*cos(2*pi*t)
plot(t,f2, type="l")

f3=15*cos(2*pi*t/7 + 0.3*pi)
plot(t,f3, type="l")

f4=18*cos(2*pi*t/9 + 0.1*pi)
plot(t,f4, type="l")

f5=19*cos(2*pi*t/10 + 1.2*pi)
plot(t,f5, type="l")

# linear / quadratic trend
y=2.7*t


# Random Noise 
set.seed(10)
N<- rnorm(length(t),0,7)

#time series 1
fx<-cos(2*pi*t/20 + 1.7*pi)+
  12*cos(2*pi*t)+
  15*cos(2*pi*t/7 + 0.3*pi)+
  18*cos(2*pi*t/9 + 0.1*pi)+
  19*cos(2*pi*t/10 + 1.2*pi)+N+y

plot(t,fx, type="l")


# creating a dataframe
mat<-matrix(c(t,f1,f2,f3,f4,f5,y,N),ncol = 8, byrow = FALSE)
dat<-as.data.frame(mat)
colnames(dat)<- c("t", "f1","f2","f3","f4","f5","y","Noise")
dat<-dat%>%mutate(fx = f1+f2+f3+f4+f5+N+y)
head(dat)

## PART 2 // PLOTS

#plots of sinusoidal waves
#F1
F1P<-dat%>%ggplot(aes(x=t, y=f1)) +
  geom_line( color="#69b3a2", size=1.3) + 
  ggtitle("Sinusoidal Wave 1")+
  xlab("time (seconds)") +
  ylab("f1") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
F1P

#F2
F2P<-dat%>%ggplot(aes(x=t, y=f2)) +
  geom_line( color="#69b3a2", size=1.3) + 
  ggtitle("Sinusoidal Wave 2")+
  xlab("time (seconds)") +
  ylab("f2") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
F2P

#F3
F3P<-dat%>%ggplot(aes(x=t, y=f3)) +
  geom_line( color="#69b3a2", size=1.3) + 
  ggtitle("Sinusoidal Wave 3")+
  xlab("time (seconds)") +
  ylab("f3") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
F3P

#F4
F4P<-dat%>%ggplot(aes(x=t, y=f4)) +
  geom_line( color="#69b3a2", size=1.3) + 
  ggtitle("Sinusoidal Wave 4")+
  xlab("time (seconds)") +
  ylab("f4") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
F4P

#F5
F5P<-dat%>%ggplot(aes(x=t, y=f5)) +
  geom_line( color="#69b3a2", size=1.3) + 
  ggtitle("Sinusoidal Wave 5")+
  xlab("time (seconds)") +
  ylab("f5") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
F5P

#y
Y<-dat%>%ggplot(aes(x=t, y=y)) +
  geom_line( color="#69b3a2", size=1.3) + 
  ggtitle("Linear Trend y=2.7t")+
  xlab("time (seconds)") +
  ylab("y") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
Y

#Noise
NP<-dat%>%ggplot(aes(x=t, y=N)) +
  geom_line( color="#69b3a2") + 
  ggtitle("Noise Level N(0,7)")+
  xlab("time (seconds)") +
  ylab("Noise") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
NP

#Plot of Time Series I
FXP<-dat%>%ggplot(aes(x=t, y=fx)) +
  geom_line( color="#69b3a2") + 
  ggtitle("Time Series I")+
  xlab("time (seconds)") +
  ylab("f(x)") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
FXP



####### TIME SERIES 2
pi=3.1415926
t<-seq(0,7*pi,1/45)
# ti<-seq(0,5*pi,1/30)
length(t)


g1=cos(2*pi*t/14 + 1.3*pi)
plot(t,g1, type="l")

g2=5*cos(2*pi*t/4 + 2.5*pi)
plot(t,g2, type="l")

g3=10*cos(2*pi*t/7 + 0.6*pi)
plot(t,g3, type="l")

g4=12*cos(2*pi*t/8 + 1.9*pi)
plot(t,g4, type="l")

g5=21*cos(2*pi*t)
plot(t,g5, type="l")

# linear / quadratic trend
y2=-3.8*t+1
plot(t,y2, type="l")

# Random Noise 
set.seed(10)
N2<- rnorm(length(t),0,9)

# second series

gx<-cos(2*pi*t/14 + 1.3*pi)+
  5*cos(2*pi*t/4 + 2.5*pi)+
  10*cos(2*pi*t/7 + 0.6*pi)+
  12*cos(2*pi*t/8 + 1.9*pi)+
  21*cos(2*pi*t)+N2+y2

plot(t,gx, type="l")

## creating dataframe

mat2<-matrix(c(t,g1,g2,g3,g4,g5,y2,N2),ncol = 8, byrow = FALSE)
dat2<-as.data.frame(mat2)
colnames(dat2)<- c("t", "g1","g2","g3","g4","g5","y","Noise")
dat2<-dat2%>%mutate(gx = g1+g2+g3+g4+g5+N2+y2)
head(dat2)


#Plotting 2
#G1P
G1P<-dat2%>%ggplot(aes(x=t, y=g1)) +
  geom_line( color="#DF3E6B", size=1.3) + 
  ggtitle("Sinusoidal Wave 1")+
  xlab("time (seconds)") +
  ylab("g1") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))

G2P<-dat2%>%ggplot(aes(x=t, y=g2)) +
  geom_line( color="#DF3E6B", size=1.3) + 
  ggtitle("Sinusoidal Wave 2")+
  xlab("time (seconds)") +
  ylab("g2") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))


G3P<-dat2%>%ggplot(aes(x=t, y=g3)) +
  geom_line( color="#DF3E6B", size=1.3) + 
  ggtitle("Sinusoidal Wave 3")+
  xlab("time (seconds)") +
  ylab("g3") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))


G4P<-dat2%>%ggplot(aes(x=t, y=g4)) +
  geom_line( color="#DF3E6B", size=1.3) + 
  ggtitle("Sinusoidal Wave 4")+
  xlab("time (seconds)") +
  ylab("g4") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))


G5P<-dat2%>%ggplot(aes(x=t, y=g5)) +
  geom_line( color="#DF3E6B", size=1.3) + 
  ggtitle("Sinusoidal Wave 5")+
  xlab("time (seconds)") +
  ylab("g5") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))

G1P
G2P
G3P
G4P
G5P

Y2<-dat2%>%ggplot(aes(x=t, y=y)) +
  geom_line( color="#DF3E6B", size=1.3) + 
  ggtitle("Linear Trend y=-3.8t+1")+
  xlab("time (seconds)") +
  ylab("y") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
Y2

#Noise
N2<-dat2%>%ggplot(aes(x=t, y=Noise)) +
  geom_line( color="#DF3E6B") + 
  ggtitle("Noise Level N(0,9)")+
  xlab("time (seconds)") +
  ylab("Noise") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
N2

#Plot of Time Series II
GXP<-dat2%>%ggplot(aes(x=t, y=gx)) +
  geom_line( color="#DF3E6B") + 
  ggtitle("Time Series II")+
  xlab("time (seconds)") +
  ylab("Y(t)") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
GXP
FXP



## ANALYSIS

# For Time Series 1
head(dat)

#Mean
n<-length(dat$fx)
Mx<-(1/n)*sum(dat$fx)
Mx

#Var

varx<-(1/n)*sum((dat$fx-Mx)^2)
varx

#quadratic norm & power of series
n
P<-(1/n)*sum((dat$fx)^2)
P


#auto-covariance
Cxx<-(1/(n-1))*sum((dat$fx[1:n-1] - Mx)*(dat$fx[2:n]-Mx))

Cxx0<- (1/n)*sum((dat$fx - Mx)^2)
Cxx0

#auto-correlation

Cxx/Cxx0


# For time series 2 y(t_i)

head(dat2)

#Mean
n2<-length(dat2$gx)
n2
sum(dat2$gx)
My<-(1/n2)*sum(dat2$gx)
My

#Variance


vary<- (1/n2)*sum((dat2$gx-My)^2)
vary

#power
n2
P_y<-(1/n2)*sum((dat2$gx)^2)
P_y

#auto-covariance
Cyy<-(1/(n2-1))*sum((dat2$gx[1:n2-1] - My)*(dat2$gx[2:n2]-My))
Cyy

#auto-correlation

Cyy0<- (1/n2)*sum((dat2$gx - My)^2)
Cyy0

Cyy/Cyy0


#Cross Covariance

Cxy<- (1/(n))*sum((dat$fx[1:n-1] - Mx)*(dat2$gx[2:n]-My))
Cxy

#Cross Correlation

Cxy/sqrt(Cxx0 * Cyy0)


#doubling the length of both series. 


d<-seq(7*pi,14*pi,1/45)

f12=cos(2*pi*d/20 + 1.7*pi)

f22=12*cos(2*pi*d)

f32=15*cos(2*pi*d/7 + 0.3*pi)

f42=18*cos(2*pi*d/9 + 0.1*pi)

f52=19*cos(2*pi*d/10 + 1.2*pi)

# linear / quadratic trend
y2=2.7*d


# Random Noise 
set.seed(10)
Nd<- rnorm(length(t),0,7)



#time series 1
fx2<-cos(2*pi*d/20 + 1.7*pi)+
  12*cos(2*pi*d)+
  15*cos(2*pi*d/7 + 0.3*pi)+
  18*cos(2*pi*d/9 + 0.1*pi)+
  19*cos(2*pi*d/10 + 1.2*pi)+Nd+y2

plot(d,fx2, type="l")

matxd<-matrix(c(d,f12,f22,f32,f42,f52,y2,Nd),ncol = 8, byrow = FALSE)
datxd<-as.data.frame(matxd)
colnames(datxd)<- c("t", "f1","f2","f3","f4","f5","y","Noise")
datxd<-datxd%>%mutate(fx = f12+f22+f32+f42+f52+Nd+y2)
head(datxd)

# binding rows of 2 dataframes

doublex<-bind_rows(dat,datxd)
plot(doublex$t, doublex$fx, type="l")





### doubling length time series 2
d<-seq(7*pi,14*pi,1/45)

g12=cos(2*pi*d/14 + 1.3*pi)

g22=5*cos(2*pi*d/4 + 2.5*pi)

g32=10*cos(2*pi*d/7 + 0.6*pi)

g42=12*cos(2*pi*d/8 + 1.9*pi)

g52=21*cos(2*pi*d)

# linear / quadratic trend
lt=-3.8*d+1
plot(d,y2, type="l")

# Random Noise 
set.seed(10)
Nd2<- rnorm(length(t),0,9)

gx<-cos(2*pi*d/14 + 1.3*pi)+
  5*cos(2*pi*d/4 + 2.5*pi)+
  10*cos(2*pi*d/7 + 0.6*pi)+
  12*cos(2*pi*d/8 + 1.9*pi)+
  21*cos(2*pi*d)+Nd2+lt

plot(d,gx, type="l")

## Dataframe of double length time series y

mat2d<-matrix(c(d,g12,g22,g32,g42,g52,lt,Nd2),ncol = 8, byrow = FALSE)
dat2d<-as.data.frame(mat2d)
colnames(dat2d)<- c("t", "g1","g2","g3","g4","g5","y","Noise")
dat2d<-dat2d%>%mutate(gx = g12+g22+g32+g42+g52+Nd2+lt)
head(dat2d)

doubley<-bind_rows(dat2,dat2d)
plot(doubley$t, doubley$gx, type="l")

###### Plots of Doubled Time Series:
X2D<-doublex%>%ggplot(aes(x=t, y=fx)) +
  geom_line( color="#69b3a2") + 
  ggtitle("Time Series I (Double Length)")+
  xlab("time (seconds)") +
  ylab("X(t)") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
X2D


Y2D<-doubley%>%ggplot(aes(x=t, y=gx)) +
  geom_line( color="#DF3E6B") + 
  ggtitle("Time Series II (Double Length)")+
  xlab("time (seconds)") +
  ylab("Y(t)") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
Y2D


# Analysis

# Mean

n<-length(doublex$t)
n
sum(doublex$fx)
Mx<-(1/n)*sum(doublex$fx)
Mx

My<-(1/n)*sum(doubley$gx)
My

# Var
sx<-(1/n)*sum((doublex$fx-Mx)^2)
sx

sy<-(1/n)*sum((doubley$gx-Mx)^2)
sy

#Power


PX2<-(1/n)*sum((doublex$fx)^2)
PY2<-(1/n)*sum((doubley$gx)^2)

PX2
PY2

# Auto Cov
#X(ti)
Gammaxx<-(1/(n-1))*sum((doublex$fx[1:n-1] - Mx)*(doublex$fx[2:n]-Mx))
Gammaxx

#Y(ti)
Gammayy<-(1/(n-1))*sum((doubley$gx[1:n-1] - My)*(doubley$gx[2:n]-My))
Gammayy

# Auto Cor

#X(ti)
Gammaxx/sx

#Y(ti)
Gammayy/sy


#Cross-COV Cross-Cor

Gammaxy<- (1/(n))*sum((doublex$fx[1:n-1] - Mx)*(doubley$gx[2:n]-My))
Gammaxy

#Cross Correlation

Gammaxy/(sqrt(sx * sy))


# Section II

#### DEPENDENCIES
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(TSA)

# read csv file
getwd()
main<-read.csv("main.csv")

#dropping rowname index
main<-(main[,2:18])
dim(main)
colnames(main)
head(main)


#indexing "main" dataframe for f(x) and g(x)

## full extended duration 1980 rows for f(x)
ffx<-main[ ,1:9]

## full extended duration 1980 rows for g(x)
fgx<-main[ ,c(1,10:17)]

## half duration first 990 rows for f(x) 
hfx<-main[1:990,1:9]

## half duration first 990 rows for g(x) 
hgx<-main[991:1980,c(1,10:17)]

# Use FFT or other Fourier algorithms to analyse the series simulated

#1. FOR ffx
fft(ffx$fx)/1980
tr_ffx<-(fft(ffx$fx)/1980)

#1b Periodogram
P1<-periodogram(tr_ffx,
                ylab=expression(paste("Power  (", lambda, ") ")), 
                xlab="Frequency (Hz)",
                main="Periodogram of Series 1 at Full Length")

#P1

#2. FOR fgx
fft(fgx$gx)/1980
tr_fgx<-(fft(fgx$gx)/1980)

#2b Periodogram
P2<-periodogram(tr_fgx,
                ylab=expression(paste("Power  (", lambda, ") ")), 
                xlab="Frequency (Hz)",
                main="Periodogram of Series 2 at Full Length")

#P2

#3. FOR hfx
fft(hfx$fx)/990
tr_hfx<-(fft(hfx$fx)/990)

#3b Periodogram
P3<-periodogram(tr_hfx,
                ylab=expression(paste("Power  (", lambda, ") ")), 
                xlab="Frequency (Hz)",
                main="Periodogram of Series 1 at Half Length")

#4. FOR hgx
fft(hgx$gx)/990
tr_hgx<-(fft(hgx$gx)/990)

#4b Periodogram
P4<-periodogram(tr_hgx,
                ylab=expression(paste("Power  (", lambda, ") ")), 
                xlab="Frequency (Hz)",
                main="Periodogram of Series 2 at Half Length")


#### PLOTS

P1
P2
P3
P4
#### 2 Removing the LT

ffx$fx4<-ffx$fx-ffx$LTx
fgx$gx4<-fgx$gx-fgx$LTy
hfx$fx4<-hfx$fx-hfx$LTx
hgx$gx4<-hgx$gx-hgx$LTy

# fft after removing the LT
fft(ffx$fx4)/1980
tr_ffx4<-(fft(ffx$fx4)/1980)

fft(fgx$gx4)/1980
tr_fgx4<-(fft(fgx$gx4)/1980)


fft(hfx$fx4)/990
tr_hfx4<-(fft(hfx$fx4)/990)

fft(hgx$gx4)/990
tr_hgx4<-(fft(hgx$gx4)/990)



#plots

L1<-periodogram(tr_ffx4,
                ylab=expression(paste("Power  (", lambda, ") ")), 
                xlab="Frequency (Hz)",
                main="Periodogram of Series 1 at Full Length w/o Linear Trend")

L2<-periodogram(tr_fgx4,
                ylab=expression(paste("Power  (", lambda, ") ")), 
                xlab="Frequency (Hz)",
                main="Periodogram of Series 2 at Full Length w/o Linear Trend")


L3<-periodogram(tr_hfx4,
                ylab=expression(paste("Power  (", lambda, ") ")), 
                xlab="Frequency (Hz)",
                main="Periodogram of Series 1 at Half Length w/o Linear Trend")


L4<-periodogram(tr_hgx4,
                ylab=expression(paste("Power  (", lambda, ") ")), 
                xlab="Frequency (Hz)",
                main="Periodogram of Series 2 at Half Length w/o Linear Trend")







#### 3 Removing the random noise

ffx$fx2<-ffx$fx-ffx$Nx
fgx$gx2<-fgx$gx-fgx$Ny
hfx$fx2<-hfx$fx-hfx$Nx
hgx$gx2<-hgx$gx-hgx$Ny


# fft after reducing noise
fft(ffx$fx2)/1980
tr_ffx2<-(fft(ffx$fx2)/1980)

fft(fgx$gx2)/1980
tr_fgx2<-(fft(fgx$gx2)/1980)


fft(hfx$fx2)/990
tr_hfx2<-(fft(hfx$fx2)/990)

fft(hgx$gx2)/990
tr_hgx2<-(fft(hgx$gx2)/990)


#Periodograms

P5<-periodogram(tr_ffx2,
                ylab=expression(paste("Power  (", lambda, ") ")), 
                xlab="Frequency (Hz)",
                main="Periodogram of Series 1 at Full Length w/o Noise")
P5

P6<-periodogram(tr_fgx2,
                ylab=expression(paste("Power  (", lambda, ") ")), 
                xlab="Frequency (Hz)",
                main="Periodogram of Series 2 at Full Length w/o Noise")
P6

P7<-periodogram(tr_hfx2,
                ylab=expression(paste("Power  (", lambda, ") ")), 
                xlab="Frequency (Hz)",
                main="Periodogram of Series 1 at Half Length w/o Noise")
P7

P8<-periodogram(tr_hgx2,
                ylab=expression(paste("Power  (", lambda, ") ")), 
                xlab="Frequency (Hz)",
                main="Periodogram of Series 2 at Half Length w/o Noise")
P8


# 4 doubling the noise level


ffx$fx3<-ffx$fx+ffx$Nx
fgx$gx3<-fgx$gx+fgx$Ny
hfx$fx3<-hfx$fx+hfx$Nx
hgx$gx3<-hgx$gx+hgx$Ny

# fft after doubling noise
fft(ffx$fx3)/1980
tr_ffx3<-(fft(ffx$fx3)/1980)

fft(fgx$gx3)/1980
tr_fgx3<-(fft(fgx$gx3)/1980)


fft(hfx$fx3)/990
tr_hfx3<-(fft(hfx$fx3)/990)

fft(hgx$gx3)/990
tr_hgx3<-(fft(hgx$gx3)/990)

#plots

P9<-periodogram(tr_ffx3,
                ylab=expression(paste("Power  (", lambda, ") ")), 
                xlab="Frequency (Hz)",
                main="Periodogram of Series 1 at Full Length w/ Double Noise")

P10<-periodogram(tr_fgx3,
                 ylab=expression(paste("Power  (", lambda, ") ")), 
                 xlab="Frequency (Hz)",
                 main="Periodogram of Series 2 at Full Length w/ Double Noise")


P11<-periodogram(tr_hfx3,
                 ylab=expression(paste("Power  (", lambda, ") ")), 
                 xlab="Frequency (Hz)",
                 main="Periodogram of Series 1 at Half Length w/ Double Noise")


P12<-periodogram(tr_hgx3,
                 ylab=expression(paste("Power  (", lambda, ") ")), 
                 xlab="Frequency (Hz)",
                 main="Periodogram of Series 2 at Half Length w/ Double Noise")


#Section III

#### DEPENDENCIES
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(TSA)
library(bspec)

# read csv file
getwd()
main<-read.csv("main.csv")

#dropping rowname index
main<-(main[,2:18])
dim(main)
colnames(main)
head(main)

#indexing "main" dataframe for f(x) and g(x) 

## full extended duration 1980 rows for f(x)
ffx<-main[ ,1:9]

## full extended duration 1980 rows for g(x)
fgx<-main[ ,c(1,10:17)]

## half duration first 990 rows for f(x) 
hfx<-main[1:990,1:9]

## half duration first 990 rows for g(x) 
hgx<-main[991:1980,c(1,10:17)]


#### FFT to obtain Fourier Series

N<-1980
delta<- 1/45

n<-N*delta


Cffx<-acf(ffx$fx, type = "covariance", plot=FALSE, lag.max =n )
class(Cffx)
attributes(Cffx)
cffx<-Cffx$acf
class(cffx)

tcffx<-fft(cffx)

P1<-spectrum(cffx, 
             xlab="Frequency (Hz)",
             ylab=expression(paste("Power  (", lambda^2, ") ")),
             main="Power Spectra of Time Series 1 (Extended)",
             col="blue",
             lwd=3,
)



Cfgx<-acf(fgx$gx, type = "covariance", plot=FALSE, lag.max=n )
cfgx<-Cfgx$acf
fft(cfgx)

tcfgx<-fft(cfgx)

P2<-spectrum(cfgx, 
             xlab="Frequency (Hz)",
             ylab=expression(paste("Power  (", lambda^2, ") ")),
             main="Power Spectra of Time Series 2 (Extended)",
             col="red",
             lwd=3,
)


N<-1980/2
delta<- 1/45

n<-N*delta


Chfx<-acf(hfx$fx, type = "covariance", plot=FALSE, lag.max =n )
chfx<-Chfx$acf

tchfx<-fft(chfx)

P3<-spectrum(chfx, 
             xlab="Frequency (Hz)",
             ylab=expression(paste("Power  (", lambda^2, ") ")),
             main="Power Spectra of Time Series 1 (Original)",
             col="blue",
             lwd=3,
)



N<-1980/2
delta<- 1/45

n<-N*delta


Chgx<-acf(hgx$gx, type = "covariance", plot=FALSE, lag.max =n )
chgx<-Chgx$acf

tchgx<-fft(chgx)

P4<-spectrum(chgx, 
             xlab="Frequency (Hz)",
             ylab=expression(paste("Power  (", lambda^2, ") ")),
             main="Power Spectra of Time Series 2 (Original)",
             col="red",
             lwd=3,
)




## PSD

N<-1980
delta<- 1/45

n<-N*delta


Pffx<-acf(ffx$fx, type = "correlation", plot=FALSE, lag.max =n )
class(Pffx)
attributes(Pffx)
pffx<-Pffx$acf
class(pffx)

tpffx<-fft(pffx)

PSD1<-spectrum(pffx, 
               xlab="Frequency (Hz)",
               ylab="Power",
               main="PSD of Time Series 1 (Extended)",
               col="blue",
               lwd=3,
)




Pfgx<-acf(fgx$gx, type = "correlation", plot=FALSE, lag.max=n )
pfgx<-Pfgx$acf
fft(pfgx)

tpfgx<-fft(pfgx)

PSD2<-spectrum(pfgx, 
               xlab="Frequency (Hz)",
               ylab="Power",
               main="PSD of Time Series 2 (Extended)",
               col="red",
               lwd=3,
)



N<-1980/2
delta<- 1/45

n<-N*delta


Phfx<-acf(hfx$fx, type = "correlation", plot=FALSE, lag.max =n )
phfx<-Phfx$acf

tphfx<-fft(phfx)

PSD3<-spectrum(phfx, 
               xlab="Frequency (Hz)",
               ylab="Power",
               main="PSD of Time Series 1 (Original)",
               col="blue",
               lwd=3,
)



N<-1980/2
delta<- 1/45

n<-N*delta


Phgx<-acf(hgx$gx, type = "correlation", plot=FALSE, lag.max =n )
phgx<-Phgx$acf

tphgx<-fft(phgx)

PSD4<-spectrum(phgx, 
               xlab="Frequency (Hz)",
               ylab="Power",
               main="PSD of Time Series 2 (Original)",
               col="red",
               log="yes",
               lwd=3,
)



######### 3
N<-1980
delta<- 1/45

tau<-N*delta

# PSD of extended series 1

PSD1$bandwidth

SNR1<-10*log(pffx)
SNR1

PSDfx<- spectrum(SNR1, 
                 xlab="Frequency (Hz)",
                 ylab="Power in dB",
                 main="PSD of Time Series 1 (Extended) in Decibals",
                 col="blue",
                 log="yes",
                 lwd=3,
)






snr(ffx$fx, PSD1)

# PSD1$
# 
# fx<-ffx$fx
# fxacf <- acf(fx,type="covariance", lag=tau) 
# ftfx <- fft(fxacf$acf)
# freqfx <- (1:nrow(ftfx))*N/nrow(ftfx) # 1000 hz
# Afx <- (Re(ftfx)^2 + Im(ftfx)^2)^.5 #amplitude is magnitude
# Pfx<-
# PSDfx <- cbind.data.frame(freqfx,Afx)
# ggplot(PSDfx, aes(x=freqfx,y=Afx)) + geom_smooth()

# PSD of extended series 2

PSD2$bandwidth

SNR2<-10*log(pfgx)
SNR2

PSDgx<- spectrum(SNR2, 
                 xlab="Frequency (Hz)",
                 ylab="Power in dB",
                 main="PSD of Time Series 2 (Extended) in Decibals",
                 col="red",
                 log="yes",
                 lwd=3,
)

# determine the bandwidth of the random noise (white noise) as well as its power (variance)

attributes(PSD1)

## Transformation to  SNR
# use the noise power level to transform the PSD into another form in which the y-axis will be signal-to-noise ratio (SNR) expressed in decibels (dB)


sfx<-PSD1$freq
sfy<-PSD1$spec




###### 4
#we will consider time series 1
head(ffx)

# adding 2 more waves with frequency that are higher than the Nyquist

# nyquist freq.
nyq<-(2*delta)^(-1)







#Section IV

#### DEPENDENCIES
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(seewave)
library(smoother)
library(clampSeg)

# read csv file
getwd()
main<-read.csv("main.csv")

#dropping rowname index
main<-(main[,2:18])
dim(main)
colnames(main)
head(main)

#indexing "main" dataframe for f(x) and g(x) 

## full extended duration 1980 rows for f(x)
ffx<-main[ ,1:9]

## full extended duration 1980 rows for g(x)
fgx<-main[ ,c(1,10:17)]

## half duration first 990 rows for f(x) 
hfx<-main[1:990,1:9]

## half duration first 990 rows for g(x) 
hgx<-main[991:1980,c(1,10:17)]


# PART 1 LOW PASS FILTER

# TIME SERIES 1 FULL LENGTH



LPF1<-ffilter(ffx$fx,f=45,to=1.05)
S1<-spectro(LPF1,f=45,wl=10)


P1D<-as.data.frame(matrix(c(1:length(LPF1),LPF1), ncol = 2, byrow = TRUE ))


P1<-P1D%>%ggplot(aes(x=1:length(LPF1), y=LPF1)) +
  geom_line( color="#69b3a2", size=1.3) + 
  ggtitle(expression("Low Pass Filter | Time Series I (Full Length)"))+
  xlab("time (s)") +
  ylab("x(t)") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
P1

# TIME SERIES 2 FULL LENGTH

LPF2<-ffilter(fgx$gx,f=45,to=1)
S2<-spectro(LPF2,f=45,wl=10)


P2D<-as.data.frame(matrix(c(1:length(LPF2),LPF2), ncol = 2, byrow = TRUE ))


P2<-P2D%>%ggplot(aes(x=1:length(LPF2), y=LPF2)) +
  geom_line( color="#DF3E6B", size=1.3) + 
  ggtitle(expression("Low Pass Filter | Time Series II (Full Length)"))+
  xlab("time (s)") +
  ylab("x(t)") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
P2


# PART 2 HIGH PASS FILTER

# TIME SERIES 1 FULL LENGTH

HPF1<-ffilter(ffx$fx,f=45,from=0.11)
SS1<-spectro(HPF1,f=45,wl=10)

P3D<-as.data.frame(matrix(c(1:length(HPF1),HPF1), ncol = 2, byrow = TRUE ))
P3<-P3D%>%ggplot(aes(x=1:length(HPF1), y=HPF1)) +
  geom_line( color="#69b3a2", size=1) + 
  ggtitle(expression("High Pass Filter | Time Series I (Full Length)"))+
  xlab("time (s)") +
  ylab("x(t)") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
P3
# TIME SERIES 2 FULL LENGTH

HPF2<-ffilter(fgx$gx,f=45,from=0.126)
SS2<-spectro(HPF2,f=45,wl=10)

P4D<-as.data.frame(matrix(c(1:length(HPF2),HPF2), ncol = 2, byrow = TRUE ))
P4<-P4D%>%ggplot(aes(x=1:length(HPF2), y=HPF1)) +
  geom_line( color="#DF3E6B", size=1) + 
  ggtitle(expression("High Pass Filter | Time Series II (Full Length)"))+
  xlab("time (s)") +
  ylab("x(t)") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
P4

# PART 3 BAND PASS FILTER

# filter out the two intermediate frequencies
# TIME SERIES 1 FULL LENGTH
BPF1<-ffilter(ffx$fx,f=45,from=1/9.5, to=1/6.5, bandpass = FALSE)
SSS1<-spectro(BPF1,f=45,wl=10)

P5D<-as.data.frame(matrix(c(1:length(BPF1),BPF1), ncol = 2, byrow = TRUE ))
P5<-P5D%>%ggplot(aes(x=1:length(BPF1), y=BPF1)) +
  geom_line( color="#69b3a2", size=1) + 
  ggtitle(expression("Band Pass Filter | Time Series I (Full Length)"))+
  xlab("time (s)") +
  ylab("x(t)") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
P5

# TIME SERIES 2 FULL LENGTH

BPF2<-ffilter(fgx$gx,f=45,from=1/9, to=1/5, bandpass = FALSE)
SSS2<-spectro(BPF2,f=45,wl=10)

P6D<-as.data.frame(matrix(c(1:length(BPF2),BPF2), ncol = 2, byrow = TRUE ))
P6<-P6D%>%ggplot(aes(x=1:length(BPF2), y=BPF2)) +
  geom_line( color="#DF3E6B", size=1) + 
  ggtitle(expression("Band Pass Filter | Time Series II (Full Length)"))+
  xlab("time (s)") +
  ylab("x(t)") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(panel.border = element_rect(color = "darkgrey",
                                    fill = NA,
                                    size = 2))
P6


#Section V

library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(gsignal)
library(ClamR)
library(surveillance)
library(tester)
library(IRISSeismic)
library(WaveletComp)

# Importing Data from assignment 1

# read csv file
getwd()
main<-read.csv("main.csv")

#dropping rowname index
main<-(main[,2:18])
dim(main)
colnames(main)
head(main)

# indexing "main" dataframe for f(x) and g(x) 

## half duration first 990 rows for f(x) 
hfx<-main[1:990,1:9]

## half duration first 990 rows for g(x) 
hgx<-main[991:1980,c(1,10:17)]


# Time Series 1 f(x)
head(hfx)

# Time Series 2 g(x)
head(hgx)


# Modification of original time series g(x)

pi=3.1415926
t<-seq(0,7*pi,1/45)
length(t)

# A*cos(2*pi*w*t+phi)

# Mod 1

# we will make changes to waves g_2 coincide precisely with one of the 
# frequencies of the first series f_4. 
# It is noted that one of the two amplitudes be much smaller than the other.

# original g2=5*cos(2*pi*t/4 + 2.5*pi) 

mg2= 5*cos(2*pi*t/9 + 2.5*pi)

# Mod 2

# Change g_4 with respect to f_5
# to have a amplitude that is 1/2 of f_5 and a frequency that is close


# original  g4=12*cos(2*pi*t/8 + 1.9*pi)

mg4= 9.5*cos(2*pi*t/9.5 + 1.9*pi)


# Wrangling
# modify g(x) data frame to add new values for g_2 and g_4 
# and new sum of g(x) values

head(hgx)


mods<-cbind(mg2,mg4)

colnames(hgx)

hgx<-hgx%>%select("t","g1","g3","g5","LTy","Ny")

hgx<-cbind(hgx,mods)

hgx<-hgx%>%select("t","g1","mg2","g3","mg4","g5","LTy","Ny")

hgx<-cbind(hgx,rowSums(hgx))

names(hgx)<-c("t","g1","g2","g3","g4","g5","LTy","Ny", "gx")

head(hgx)


# I - PSD

#### FFT to obtain Fourier Series

N<-990
delta<- 1/45
n<-N*delta

# PSD of f(x)

Phfx<-acf(hfx$fx, type = "correlation", plot=FALSE, lag.max =n )
class(Phfx)
attributes(Phfx)
phfx<-Phfx$acf
class(phfx)

tphfx<-fft(phfx)

PSD1<-spectrum(phfx, 
               xlab="Frequency (Hz)",
               ylab="Power",
               main="PSD of Time Series I",
               col="#69b3a2",
               lwd=3,
)


# PSD of g(x)
Phgx<-acf(hgx$gx, type = "correlation", plot=FALSE, lag.max =n )
class(Phgx)
attributes(Phgx)
phgx<-Phgx$acf
class(phgx)

tphgx<-fft(phgx)

PSD2<-spectrum(phgx, 
               xlab="Frequency (Hz)",
               ylab="Power",
               main="PSD of Time Series II",
               col="#DF3E6B",
               lwd=3,
)


PSD1
PSD2


#Further Wrangling
#create a matrix such that each column is a signal
hdx<-cbind(hfx,hgx)
inp<-hdx%>%select("t","fx","gx")
names(inp)<-c("window", "fx", "gx")
head(inp)
inp<-as.matrix(inp)


ts<-inp[1:nrow(inp),2:3]
ts

# II - Cross Power Spectrum

csd(
  inp,
  window = NextPow2(sqrt(NROW(inp))),
  overlap = 0.5,
  nfft = ifelse(is_scalar(window), window, length(window)),
  fs = 1,
  detrend = "none"
)

CPS<-crossSpectrum(ts, spans = NULL, kernel = NULL, taper = 0.1,
                   pad = 0, fast = TRUE,
                   demean = FALSE, detrend = TRUE,
                   na.action = stats::na.fail)

attributes(CPS)
head(CPS)


# Calculate the transfer function
transferFunction <- CPS$Pxy / CPS$Pxx
transferAmp <- Mod(transferFunction)
transferPhase <- pracma::mod(Arg(transferFunction) * 180/pi,360)

# 2 rows
layout(matrix(seq(2)))

# Plot
plot(1/CPS$freq,transferAmp,type='l',log='x',
     xlab="Period (sec)",
     main="Transfer Function Amplitude")

plot(1/CPS$freq,transferPhase,type='l',log='x',
     xlab="Period (sec)", ylab="degrees",
     main="Transfer Function Phase")



layout(1)

XPS<-spec.pgram(ts, spans = NULL, kernel=NULL, taper = 0,
                pad = 0, fast = TRUE, demean = TRUE, detrend = TRUE,
                plot = TRUE, na.action = na.fail, col=c("#69b3a2","#DF3E6B"),
                main="Cross Power Spectrum",
                ylab="Power Spectrum",
                xlab="Frequency")

spec.pgram(inp, spans = NULL, kernel=NULL, taper = 0,
           pad = 0, fast = TRUE, demean = FALSE, detrend = TRUE,
           plot = TRUE, na.action = na.fail)



# III CPSD

cpsd(  inp,
       window = NextPow2(sqrt(NROW(inp))),
       overlap = 0.5,
       nfft = ifelse(is_scalar(window), window, length(window)),
       fs = 1,
       detrend = "none")


XPSD<-pwelch(
  inp,
  window = NextPow2(sqrt(NROW(inp))),
  overlap = 0.5,
  nfft = if (is_scalar(window)) window else length(window),
  fs = 1,
  detrend = "none",
  range = if (is.numeric(x)) "half" else "whole"
)

plot(
  XPSD,
  xlab = NULL,
  ylab = "Power (Log Scale)",
  main = "Cross Power Spectral Density",
  plot.type = "cross-spectrum",
  yscale = "log")


plot(
  XPSD,
  xlab = NULL,
  ylab = NULL,
  main = NULL,
  plot.type = "spectrum",
  yscale = "log")


# IV Squared COH

plot(
  XPSD,
  xlab = NULL,
  ylab = NULL,
  main = "Ordinary Coherance",
  plot.type = "coherence",
  yscale = "log")


# V COMPLEX COH


attributes(XPSD)

CO<-XPSD$coh
co<-sqrt(CO)
head(co)
head(CO)

XPSD2<-XPSD
XPSD2$coh<-co

plot(
  XPSD2,
  xlab = NULL,
  ylab = "Coherency Magnitude",
  main = "Complex Coherance",
  plot.type = "coherence",
  yscale = "log")

head(XPSD$freq)
imp<-as.data.frame(inp)
dt<-1
lp<-2*dt
up<-floor(nrow(imp)/3)*dt

CH<-analyze.coherency(imp, my.pair = c("fx", "gx"), loess.span = 0.75, 
                      dt = 1, dj = 1/20, 
                      lowerPeriod = 2, 
                      upperPeriod = 330, 
                      window.type.t = 1, window.type.s = 1, 
                      window.size.t = 5, window.size.s = 1/4, 
                      make.pval = TRUE, method = "white.noise", params = NULL, 
                      n.sim = 100, 
                      date.format = NULL, date.tz = NULL, 
                      verbose = TRUE)

CH$Coherency

wc.image(CH$Coherency)

wc.image(CH, which.image="wc", timelab="Time", periodlab="Period", 
         main="Coherency", 
         legend.params=list(lab="Coherence Levels", lab.line=3.5, label.digits=3))


