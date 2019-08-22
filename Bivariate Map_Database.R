####### ----------------------------------------------------------------------- ####### 
#
#     Bivariate Maps: "bivariate.map" 
#       
# ---------------------------------------------------------------------------
# bivariate.map.r
# Created by: Modified by: Nicki Shumway 22/08/19
#   http://rfunctions.blogspot.com/2015/03/bivariate-maps-bivariatemap-function.html
# Description: This R scipt which maos spatial data against eachother to create a bivarioute map 
# ---------------------------------------------------------------------------

### Packages

# Install:

install.packages("classInt")
install.packages("raster")
install.packages("rgdal")
install.packages("dismo")
install.packages("XML")
install.packages("maps")
install.packages("sp")

# Load:

library(classInt)
library(raster)
library(rgdal)
library(dismo)
library(XML)
library(maps)
library(sp)

### Colour matrix, this a function to create the ledgend and generates the colour map used to fill the map, you 

colmat<-function(nquantiles=2, upperleft=rgb(0,150,235, maxColorValue=255), upperright=rgb(130,0,80, maxColorValue=255), bottomleft="grey", bottomright=rgb(255,230,15, maxColorValue=255), xlab="x label", ylab="y label"){ 
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


# Here you can specify the number of quantiles, colors and labels of your color matrix. Examples:

col.matrix <- colmat(nquantiles=4, upperleft="#2c7bb6", upperright="#abd9e9", bottomleft="#fc8d59", bottomright="#d7191c", xlab="Climate Change Vulnerability", ylab="Climate Change Relative Search Volume")
col.matrix <- colmat(nquantiles=4, upperleft="#018571", upperright="#80cdc1", bottomleft="#dfc27d", bottomright="#a6611a", xlab="Climate Change Vulnerability", ylab="Climate Change Relative Search Volume")
col.matrix <- colmat(nquantiles=3, upperleft="#5e3c99", upperright="#fdb863", bottomleft="#b2abd2", bottomright="#e66101", xlab="Climate Change Vulnerability", ylab="Climate Change Relative Search Volume")
col.matrix <- colmat(nquantiles=3, upperleft="#2c7bb6", upperright="#fdb863", bottomleft="#abd9e9", bottomright="#d7191c", xlab="Climate Change Vulnerability", ylab="Climate Change Relative Search Volume")

# Using the previous function we will both create and plot the color matrix. We'll get something like the following image. Save it, because we'll need it later

### Bivariate Map Function

bivariate.map<-function(rasterx, rastery, colormatrix=col.matrix, nquantiles=3){ # make sure that "nquantiles=3" equalues the number of quantiles you use above
  quanmean<-getValues(rasterx)
  temp <- data.frame(quanmean, quantile=rep(NA, length(quanmean)))
  brks <- with(temp, unique(quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles)))))      
  r1 <- within(temp, quantile <- cut(quanmean, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
  quantr<-data.frame(r1[,2]) 
  quanvar<-getValues(rastery)
  temp <- data.frame(quanvar, quantile=rep(NA, length(quanvar)))
  brks <- with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
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

# Add raster data for example:
wd <- setwd("/Users/uqcarchi/Dropbox/PhD/PLC ES paper/Kappa/")
getwd()
list.files()
X1 <- raster("X1.tif")
X2 <- raster("X2.tif")

plot(X1) # view raster data
plot(X2)

# Create Bivariate map
bivmap <- bivariate.map(X1,X2, colormatrix=col.matrix, nquantiles=3)

plot(bivmap,frame.plot=F,axes=F,box=F,add=F,legend=T,col=as.vector(col.matrix))
map(interior=T,add=T)

# If you want to extract the Bivariate data for each location in your case study area

X3_shp <- readOGR(dsn="X3.shp", layer="X3")

# Extract raster values to polygons
X3_shp$ExtractData <- extract(bivmap, X3_shp, fun = mean) 
X3_shp$ExtractDataX1 <- extract(X1, X3_shp, fun = mean) 
X3_shp$ExtractDataX2 <- extract(X2, X3_shp, fun = mean) 

writePolyShape(X3_shp, "X3_shp")
write.csv(X3_shp, file="./X3_shp.csv")

