install.packages('caret', 'randomForest', 'rgdal', 'sf', 'raster', )
library(caret)
library(randomForest)
library(rgdal)
library(sf)
library(raster)

oct_12 <- stack("/cloud/project/Oct_12.tif")
may_19 <- stack("/cloud/project/May_19.tif")
june_10 <- stack("/cloud/project/June_10.tif")
june_18 <- stack("/cloud/project/June_18.tif")
plot(oct)
plotRGB(oct, r=3,g=2,b=1, scale=0.7, stretch="lin")

drStack <- stack("/cloud/project/May_19.tif",
                 "/cloud/project/June_10.tif",
                 "/cloud/project/June_18.tif",
                 "/cloud/project/Oct_12.tif")
plot(drStack)
plot(drStack[[16:20]])

lc <- st_read("/cloud/project/land_pts.shp")
head(lc)

plotRGB(oct, r=3,g=2,b=1, scale=0.7, stretch="lin")
plot(lc["landcover"], add=TRUE,
     pch=19,cex=0.5,
     pal= hcl.colors(3, palette = "Harmonic"))

legend("topleft",
       c("field", "path", "tree"),
       pch=19, pt.cex=0.5, 
       col=hcl.colors(3, palette = "Harmonic"), 
       bty="n")


trainPts <- subset(lc, lc$train=="train", drop=FALSE)

train <- extract(drStack, trainPts)

trainTable <- st_drop_geometry(trainPts)
trainDF <- na.omit(cbind(y=as.factor(trainTable[,3]), train))

tc <- trainControl(method = "repeatedcv", 
                   number = 10, 
                   repeats = 10) 

nbands <- 20 
rf.grid <- expand.grid(mtry=1:round(sqrt(nbands)))

set.seed(43)


rf_model <- caret::train(x = trainDF[,2:21],
                         y = as.factor(trainDF[,1]),
                         method = "rf",
                         metric="Accuracy", 
                         trainControl = tc, 
                         tuneGrid = rf.grid)
rf_model

rf_prediction <- raster::predict(drStack, rf_model )

plot(rf_prediction, col= hcl.colors(3, palette = "Harmonic"))

validPts <- subset(lc, lc$train=="valid", drop=FALSE)

valid_Table <- st_drop_geometry(validPts)

valid_rf <- extract(rf_prediction, validPts)

validDF_rf <- data.frame(y=valid_Table[,3], rf=valid_rf)

rf_errorM = confusionMatrix(as.factor(validDF_rf$rf),as.factor(validDF_rf$y))

colnames(rf_errorM$table) <- c("field","tree","path")
rownames(rf_errorM$table) <- c("field","tree","path")
rf_errorM

nnet.grid <- expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.001, to = 0.01, by = 0.001))

set.seed(18)
nnet_model <- caret::train(x = trainDF[,c(2:21)], y = as.factor(trainDF[,1]),
                           method = "nnet", metric="Accuracy", 
                           trainControl = tc, tuneGrid = nnet.grid,
                           trace=FALSE)
nnet_model

nn_prediction <- raster::predict(drStack, nnet_model)

plot(nn_prediction, col= hcl.colors(3, palette = "Harmonic"))

freq(nn_prediction)

freq(rf_prediction)

0.4*0.4*71019

0.4*0.4*71047

par(mfrow=c(1,2))
plot(nn_prediction, col= hcl.colors(3, palette = "Harmonic"),
     legend=FALSE, axes=FALSE, main="Neural network", box=FALSE)
legend("bottomleft", c("field","tree","path"),
       fill=hcl.colors(3, palette = "Harmonic") ,bty="n")

plot(rf_prediction, col= hcl.colors(3, palette = "Harmonic"),
     legend=FALSE, axes=FALSE, main="Random forest", box=FALSE)
legend("bottomleft", c("field","tree","path"),
       fill=hcl.colors(3, palette = "Harmonic") ,bty="n")

ndvi_may_19 <- (may_19[[2]]-may_19[[1]])/(may_19[[2]]+may_19[[1]])

ndvi_june_10 <- (june_10[[2]]-june_10[[1]])/(june_10[[2]]+june_10[[1]])

ndvi_june_18 <- (june_18[[2]]-june_18[[1]])/(june_18[[2]]+june_18[[1]])

ndvi_oct_12 <- (oct_12[[2]]-oct_12[[1]])/(oct_12[[2]]+oct_12[[1]])

par(mfrow=c(1,2))
plot(ndvi_may_19)
plot(ndvi_june_10)
plot(ndvi_june_18)
plot(ndvi_oct_12)

par(mfrow = c(1, 1))

ndviDiff <- ndvi_post - ndvi_pre
plot(ndviDiff)

nnet.grid.2 <- expand.grid(size = seq(from = 1, to = 15, by = 1),
                         decay = seq(from = 0.001, to = 0.01, by = 0.001))

set.seed(18)
nnet_model_2 <- caret::train(x = trainDF[,c(2:21)], y = as.factor(trainDF[,1]),
                           method = "nnet", metric="Accuracy", 
                           trainControl = tc, tuneGrid = nnet.grid.2,
                           trace=FALSE)
nnet_model_2

nn_prediction_2 <- raster::predict(drStack, nnet_model)

plot(nn_prediction_2, col= hcl.colors(3, palette = "Harmonic"))

freq(nn_prediction_2)

