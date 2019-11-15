# Install:

install.packages("classInt")
install.packages("raster")
install.packages("rgdal")
install.packages("dismo")
install.packages("XML")
install.packages("maps")
install.packages("sp")

# Load: ### this didn't work on GPEM comp so use below script to save to the local drive

library(classInt)
library(raster)
library(rgdal)
library(dismo)
library(XML)
library(maps)
library(sp)

install.packages("classInt", lib="C:/software/Rpackages")
library("classInt", lib.loc="C:/software/Rpackages")
install.packages("raster", lib="C:/software/Rpackages")
library("raster", lib.loc="C:/software/Rpackages")
install.packages("rgdal", lib="C:/software/Rpackages")
library("rgdal", lib.loc="C:/software/Rpackages")
install.packages("dismo", lib="C:/software/Rpackages")
library("dismo", lib.loc="C:/software/Rpackages")
install.packages("XML", lib="C:/software/Rpackages")
library("XML", lib.loc="C:/software/Rpackages")
install.packages("maps", lib="C:/software/Rpackages")
library("maps", lib.loc="C:/software/Rpackages")
install.packages("sp", lib="C:/software/Rpackages")
library("sp", lib.loc="C:/software/Rpackages")

home <- ("C://Users//uqjalla1//Downloads//wetransfer-244711");
setwd(home)

# create the colour matrix

colmat<-function(nquantiles=10, upperleft=rgb(0,150,235, maxColorValue=255), upperright=rgb(130,0,80, maxColorValue=255), bottomleft="grey", bottomright=rgb(255,230,15, maxColorValue=255), xlab="x label", ylab="y label"){
  my.data<-seq(0,1,.01)
  my.class<-classIntervals(my.data,n=nquantiles,style="quantile")
  my.pal.1<-findColours(my.class,c(upperleft,bottomleft))
  my.pal.2<-findColours(my.class,c(upperright, bottomright))
  col.matrix<-matrix(nrow = 101, ncol = 101, NA)
  for(i in 1:101){
    my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
    col.matrix[102-i,]<-findColours(my.class,my.col)}
  plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(0,1),ylim=c(0,1),frame.plot=F, xlab=xlab, ylab=ylab,cex.lab=1.3)
  for(i in 1:101){
    col.temp<-col.matrix[i-1,]
    points(my.data,rep((i-1)/100,101),pch=15,col=col.temp, cex=1)}
  seqs<-seq(0,100,(100/nquantiles))
  seqs[1]<-1
  col.matrix <- col.matrix[c(seqs), c(seqs)]}

# You can specify the number of quantiles, colors and labels of your color matrix. Example:

col.matrix <- colmat(nquantiles=10, upperleft="blue", upperright="red", bottomleft="black", bottomright="yellow", xlab="Bird Richness", ylab="Distance to coca plantation")


# But let's use this simple code. You can change "nquantiles" to generate color matrices with different color schemes. For example, change it to 4 to produce a 4x4 color scheme.

col.matrix <- colmat(nquantiles=10)
col.matrix <- colmat(nquantiles=10, upperleft="#2c7bb6", upperright="#abd9e9", bottomleft="#fc8d59", bottomright="#d7191c", xlab="Climate Change Vulnerability", ylab="Climate Change Relative Search Volume")


# Using the previous function we will both create and plot the color matrix. We'll get something like the following image. Save it, because we'll need it later.

### START COPYING HERE ###
bivariate.map<-function(rasterx, rastery, colormatrix=col.matrix, nquantiles=10){
  quanmean<-getValues(rasterx)
  temp <- data.frame(quanmean, quantile=rep(NA, length(quanmean)))
  brks <- with(temp, unique(quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles)))))
  r1 <- within(temp, quantile <- cut(quanmean, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
  quantr<-data.frame(r1[,2]) 
  quanvar<-getValues(rastery)
  temp <- data.frame(quanvar, quantile=rep(NA, length(quanvar)))
  brks <- with(temp, unique(quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles)))))
  r2 <- within(temp, quantile <- cut(quanvar, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
  quantr2<-data.frame(r2[,2])
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  col.matrix2<-colormatrix
  cn<-unique(colormatrix)
  for(i in 1:length(col.matrix2)){
    ifelse(is.na(col.matrix2[i]),col.matrix2[i]<-1,col.matrix2[i]<-which(col.matrix2[i]==cn)[1])}
  cols<-numeric(length(quantr[,1]))
  for(i in 1:length(quantr[,1])){
    a<-as.numeric.factor(quantr[i,1])
    b<-as.numeric.factor(quantr2[i,1])
    cols[i]<-as.numeric(col.matrix2[b,a])}
  r<-rasterx
  r[1:length(r)]<-cols
  return(r)}
### STOP COPYING AND PASTE INTO R ###

# Load the first raster for x axis e.g. ("GDP"):
raster.x<-raster("Country_GDP.tif")
raster.x
plot(raster.x)

# Load the second raster for y axis e.g. ("Armed_Actions")
raster.y<-raster("Country_Polity2.tif")
raster.y
plot(raster.y)

#to syncronize the extent and everything with other raster

install.packages("spatial.tools")
library(spatial.tools)
r1<-raster.y # your reference raster
r2<-raster.x # the one you want to sync with the reference
r3 <- spatial_sync_raster(r2,r1) # r3 will now have the save extents and everything as the reference raster
raster.x <- r3
# plot the first raster
my.colors = colorRampPalette(c("white","lightblue", "yellow","orangered", "red"))
plot(raster.x,frame.plot=F,axes=F,box=F,add=F,legend.width=1,legend.shrink=1,col=my.colors(255)) 
map(interior=T,add=T)

my.colors = colorRampPalette(c("white","lightblue", "yellow","orangered", "red"))
plot(raster.y,frame.plot=F,axes=F,box=F,add=F,legend.width=1,legend.shrink=1,col=my.colors(255)) 
map(interior=T,add=T)

bivmap<-bivariate.map(raster.x,raster.y, colormatrix=col.matrix, nquantiles=10)

plot(bivmap,frame.plot=F,axes=F,box=F,add=F,legend=F,col=as.vector(col.matrix))
map(interior=T,add=T)
