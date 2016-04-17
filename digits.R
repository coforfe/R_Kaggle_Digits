
#------------------------------------------------------------
#Digits Recognizer - 42000 x 785 (train.csv) 
#------------------------------------------------------------

setwd("/Volumes/TOSHIBA EXT/Verbatin64/R-cosas/2016-01 - Kaggle/01_DigitsRecognizer")

#Library loading
library(readr)
library(data.table)
library(caret)
library(stringr)

#Data loading
datIn <- fread("train.csv")
datIn <- as.data.frame(datIn)
# First coloumn -> "label" contains the numbers to classify.
datIn$label <- as.factor(datIn$label)

#### ------------(2) STUDY SHARPENING IMAGES --------------#####
sharp_img <- function(x, v_sh) {
 #Sharp image pixels values <=150 -> 0.
 #datIn_s <- datIn[,2:ncol(datIn)] 
 datIn_s <- x[,2:ncol(x)] 
 datIn_s <- as.matrix(datIn_s)
 datIn_s[ datIn_s < v_sh ] <- 0
 datIn_s <- as.data.frame(datIn_s)
 datIn_s <- cbind.data.frame( label = x$label, datIn_s)
 return(datIn_s)
 #Result: Best result for "v_sh <75" but lower than no-sharpening.
}
#### -------------------------------------------------------
datIn_s50 <- sharp_img(datIn, 50) #High sharp
datIn_s75 <- sharp_img(datIn, 75)   #Low sharp
datIn_s150 <- sharp_img(datIn, 150) #High sharp
#Put together normal and sharpened datasets
datIn <- rbind.data.frame(datIn, datIn_s50, datIn_s75, datIn_s150)

#### ------------ (1) STUDY IMAGES ----------------#####
compare_plots <- function() {
# Some plotting to see how are the numbers...
# Plot some of the entries...
library(gridExtra)
for(i in 1:10) {
       #one <- datIn[i,]
       one <- datIn_s[i,]
       one_mat <- matrix(one[2:length(one)], nrow=28, ncol=28)
       gr_out <- levelplot(one_mat, main=paste(one$label), xlab="",ylab="" )
       print(gr_out)
} 

#----------------------------
# Plot some cases..
#http://stackoverflow.com/questions/16496210/rotate-a-matrix-in-r
val_idx <- 4 #number to plot
dat_four <- datIn[datIn$label==val_idx,]
#dat_four <- datIn_s[datIn_s$label==val_idx,]
hw_ma <- 6
dat4_red <- dat_four[sample(1:nrow(dat_four), hw_ma) , ]
gr_lst <- list()
gr_lst_r <- list()
for(i in 1:hw_ma) {
  one <- dat4_red[i,]
  one_mat <- matrix(one[2:length(one)], nrow=28, ncol=28, byrow=TRUE)
  #to rotate 90º -> m[nrow(m):1,ncol(m):1]
  one_mat_r <- t(one_mat[nrow(one_mat):1, ])
  gr_out <- levelplot(one_mat, main=paste(one$label), xlab="",ylab="", colorkey=FALSE )
  gr_out_r <- levelplot(one_mat_r, main=paste(one$label), xlab="",ylab="", colorkey=FALSE )
  gr_lst[[i]] <- gr_out
  gr_lst_r[[i]] <- gr_out_r
  #print(gr_out)
} 
#join lists
big_lst <- list()
val_r <- rep(1:hw_ma, each=2)
for(i in 1:length(val_r)) {
 if(i %% 2 ==0) {
   big_lst[[ i ]] <- gr_lst_r[[ val_r[i] ]]
 } else { big_lst[[ i ]] <- gr_lst[[ val_r[i] ]] }
}
#Compare original with rotated 90º
do.call(grid.arrange, c( big_lst, list(ncol=4) ) )
#----------------------------
} #compare_plots
#### ----------------------------------------------

# 000 001 002 003 ... 026 027
# 028 029 030 031 ... 054 055
# 056 057 058 059 ... 082 083
# |   |   |   |  ...  |   |
# 728 729 730 731 ... 754 755
# 756 757 758 759 ... 782 783 


#--------------- Feature Engineering ----------------
#Percentage of non cero by row, related to number?. General the whole image.
datIn$fenumce <- apply(datIn[,2:ncol(datIn)]==0, 1, sum)/(ncol(datIn)-1)

#--------------- Number of ceros/1 per cuadrant - each digit
#Number of 1 per quadrant.
#Vertical: First the extremes on the right - Then 14 units from there.
# First-Quadrant
  v_idx <- seq(0, 756, 28)
  v_idx_1q <- vector()
  for (i in 1:(length(v_idx) / 2)) {
    vtmp <- seq(v_idx[i], v_idx[i] + 14, 1)
    v_idx_1q <- c(v_idx_1q, vtmp)
  }
  nam_1q <- paste("pixel", v_idx_1q, sep = "")
  dat1q <- datIn[, (names(datIn) %in% nam_1q)]
  numce1q <- apply(dat1q == 0, 1, sum) / ncol(dat1q)
  
  # Third-Quadrant
  v_idx_3q <- vector()
  for (i in ((length(v_idx) / 2) + 1):length(v_idx)) {
    vtmp <- seq(v_idx[i], v_idx[i] + 14, 1)
    v_idx_3q <- c(v_idx_3q, vtmp)
  }
  nam_3q <- paste("pixel", v_idx_3q, sep = "")
  dat3q <- datIn[, (names(datIn) %in% nam_3q)]
  numce3q <- apply(dat3q == 0, 1, sum) / ncol(dat3q)
  
  #--------------------------------
  #Second Quadrant and 4th quadrant
  val24_ini <- seq(0, 27, 1)
  val24_end <- seq(756, 783, 1)
  v24_idx <- seq(val24_ini[14], val24_end[14], 28)
  # Second-quadrant
  v_idx_2q <- vector()
  for (i in 1:(length(v24_idx) / 2)) {
    vtmp <- seq(v24_idx[i], v24_idx[i] + 14, 1)
    v_idx_2q <- c(v_idx_2q, vtmp)
  }
  nam_2q <- paste("pixel", v_idx_2q, sep = "")
  dat2q <- datIn[, (names(datIn) %in% nam_2q)]
  numce2q <- apply(dat2q == 0, 1, sum) / ncol(dat2q)
  
  # Fourth-Quadrant
  v_idx_4q <- vector()
  for (i in ((length(v24_idx) / 2) + 1):length(v24_idx)) {
    vtmp <- seq(v24_idx[i], v24_idx[i] + 14, 1)
    v_idx_4q <- c(v_idx_4q, vtmp)
  }
  nam_4q <- paste("pixel", v_idx_4q, sep = "")
  dat4q <- datIn[, (names(datIn) %in% nam_4q)]
  numce4q <- apply(dat4q == 0, 1, sum) / ncol(dat4q)
  

#Center of mass
f_cm <- function(x) {
 if(sum(x[x>0]) > 0) {
   tmp <- x
   idx <- which(tmp > 0, arr.ind=TRUE)
   co1q <- names(idx)
   co1qr <- as.numeric(gsub("pixel","",co1q))
   y_coor <- findInterval(co1qr, v_idx)
   x_coor <- co1qr - v_idx[y_coor]
   xm <- sum(x_coor)/length(idx)
   ym <- sum(y_coor)/length(idx)
   return(c(xm,ym))
 } else { return(c(0,0)) }
}

coor1q <- as.data.frame(t(apply(as.matrix(dat1q), 1, f_cm )))
coor2q <- as.data.frame(t(apply(as.matrix(dat2q), 1, f_cm )))
coor3q <- as.data.frame(t(apply(as.matrix(dat3q), 1, f_cm )))
coor4q <- as.data.frame(t(apply(as.matrix(dat4q), 1, f_cm )))


# Clean
rm(v_idx, v24_idx)
rm(v_idx_1q, v_idx_2q, v_idx_3q, v_idx_4q)
rm(nam_1q, nam_2q, nam_3q, nam_4q)
rm(dat1q, dat2q, dat3q, dat4q)
rm(val24_ini, val24_end, vtmp, i)

# Extend datIn
# Number of ceros
datIn$fenumce1q <- numce1q
datIn$fenumce2q <- numce2q
datIn$fenumce3q <- numce3q
datIn$fenumce4q <- numce4q
# Center of mass per cuadrant
datIn$fecm1qx <- coor1q$V1
datIn$fecm1qy <- coor1q$V2
datIn$fecm2qx <- coor2q$V1
datIn$fecm2qy <- coor2q$V2
datIn$fecm3qx <- coor3q$V1
datIn$fecm3qy <- coor3q$V2
datIn$fecm4qx <- coor4q$V1
datIn$fecm4qy <- coor4q$V2
datIn$fecm12rx <- datIn$fecm1qx / datIn$fecm2qx
datIn$fecm13rx <- datIn$fecm1qx / datIn$fecm3qx
datIn$fecm14rx <- datIn$fecm1qx / datIn$fecm4qx
datIn$fecm23rx <- datIn$fecm2qx / datIn$fecm3qx
datIn$fecm24rx <- datIn$fecm2qx / datIn$fecm4qx
datIn$fecm34rx <- datIn$fecm3qx / datIn$fecm4qx
datIn$fecm12ry <- datIn$fecm1qy / datIn$fecm2qy
datIn$fecm13ry <- datIn$fecm1qy / datIn$fecm3qy
datIn$fecm14ry <- datIn$fecm1qy / datIn$fecm4qy
datIn$fecm23ry <- datIn$fecm2qy / datIn$fecm3qy
datIn$fecm24ry <- datIn$fecm2qy / datIn$fecm4qy
datIn$fecm34ry <- datIn$fecm3qy / datIn$fecm4qy
datIn$fecmgex <- sum(coor1q$V1, coor2q$V1, coor3q$V1, coor4q$V1)/4 
datIn$fecmgey <- sum(coor1q$V2, coor2q$V2, coor3q$V3, coor4q$V2)/4 

rm(coor1q, coor2q, coor3q, coor4q, numce1q, numce2q, numce3q, numce4q)

# -------- Just Feature Eng.
 # sub_col <- names(datIn)[str_detect(names(datIn),"fe")]
 # datIn <- datIn[, c("label", sub_col)]
# Result: Run a model "ranger" and Accuracy 
# with datIn + datIn_75s + datIn_150s + datIn200s -> 0.9081.


#--------------------------------------------------------
#--------------------------------------------------------
#-------------- READY TO MODEL
#--------------------------------------------------------
#--------------------------------------------------------
#Change set's size just to get something.
sizMod <- 1 * nrow(datIn) 
#sizMod <- 1 * nrow(datIn)
datSamp <- datIn[sample(1:nrow(datIn), sizMod) , ]
#rm(datIn);gc()

inTrain <- createDataPartition(datSamp$label, p = 0.70)[[1]]
trainDat <- datSamp[ inTrain, ]
testDat <- datSamp[ -inTrain, ]

library(doMC)
numCor <- parallel::detectCores() - 2; numCor
#numCor <- 2
registerDoMC(cores = numCor)

#---------------------------------
#---------------------- RANGER (randomForest)
#---------------------------------
a <- Sys.time();a
set.seed(5789)
#set.seed(457856+rnorm(1)*10000) 
#bootControl <- trainControl(method='boot',number=50, verboseIter=TRUE) 
#bootControl <- trainControl(method='oob',number=25, verboseIter=TRUE) 
bootControl <- trainControl( number=5, verboseIter=TRUE ) 

#rfGrid <- expand.grid(mtry=seq(43,47,1))
rfGrid <- expand.grid(mtry=45)

modFitrf <-  train(
  label ~ .,
  data = trainDat,
  trControl = bootControl,
  tuneGrid = rfGrid,
  method = "ranger",
  metric = "Accuracy",
  num.trees = 25,
  importance = 'impurity',
  respect.unordered.factors = TRUE,
  verbose = TRUE,
  classification = TRUE
)

modFitrf

predrf <- predict( modFitrf, newdata=testDat[,2:ncol(testDat)] )
#ConfusionMatrix
conMatrf <- confusionMatrix(testDat$label, predrf); conMatrf 
conMatrfdf <- as.data.frame(conMatrf$overall); rfAcc <- conMatrfdf[1,1]; rfAcc <- as.character(round(rfAcc*100,2))
b <- Sys.time();b; b-a   

if( nrow(rfGrid) < 2  )  { resampleHist(modFitrf) } else  
{ plot(modFitrf, as.table=T) }


#Best iteration
modBest <- modFitrf$bestTune; modBest
modBestc <- as.character(modBest)
#Execution time:
modFitrf$times$final[3]
#Samples
samp <- dim(modFitrf$resample)[1] ; samp
numvars <- ncol(trainDat); numvars

#Variable Importance
Imprf <- varImp( modFitrf, scale=F)
plot(Imprf, top=20)


#Save trainDat, testDat and Model objects.
save(
  trainDat, testDat, modFitrf,
  file=paste("ranger_",numvars,"vars_rf_n",samp,"_grid",modBestc,"_",rfAcc,"__.RData", sep="")
)


#---------------------------------------------------

# #Results - 20 - With Feature Engineering 
# datIn + datIn_70s + datIn_150s + *datIn_200s*
# Time difference of 1.491087 hours
# bootControl <- trainControl( number=5, verboseIter=TRUE ) 
# rfGrid <- expand.grid(mtry=45)
# num.trees = 250,
# Accuracy : 0.9962          
# 95% CI : (0.9957, 0.9967)


# #Results - 19 - With Feature Engineering 
# datIn + datIn_70s + datIn_150s + *datIn_200s*
# Time difference of 32.97288 mins
# bootControl <- trainControl( number=5, verboseIter=TRUE ) 
# rfGrid <- expand.grid(mtry=45)
# num.trees = 50,
# Accuracy : 0.9901         
# 95% CI : (0.9892, 0.991)


# #Results - 19 - With Feature Engineering - datIn + datIn_70s + datIn_150s
# Also included "ratios".
# Time difference of 3.552741 hours
# bootControl <- trainControl( number=15, verboseIter=TRUE ) 
# rfGrid <- expand.grid(mtry=45)
# num.trees = 300,
# Accuracy : 0.9909          
# 95% CI : (0.9899, 0.9918)


# #Results - 19 - With Feature Engineering - datIn + datIn_70s + datIn_150s
# c4.8xlarge - 0.40 $/hour
# Time difference of 18.09592 mins
# bootControl <- trainControl( number=15, verboseIter=TRUE ) 
# rfGrid <- expand.grid(mtry=45)
# num.trees = 1000,
# Accuracy : 0.9916          
# 95% CI : (0.9907, 0.9925)


# #Results - 19 - With Feature Engineering - datIn + datIn_70s + datIn_150s
# Time difference of 2.815219 hours
# inTrain <- createDataPartition(datSamp$label, p = 0.70)[[1]]
# bootControl <- trainControl( number=25, verboseIter=TRUE ) 
# rfGrid <- expand.grid(mtry=45)
# num.trees = 500,
# Time difference of 2.815219 hours
# Accuracy : 0.9928          
# 95% CI : (0.9918, 0.9936)




# #Results - 19 - With Feature Engineering - datIn + datIn_70s + datIn_150s
# Time difference of 27.86202 mins
#inTrain <- createDataPartition(datSamp$label, p = 0.70)[[1]]
# bootControl <- trainControl( number=5, verboseIter=TRUE ) 
# rfGrid <- expand.grid(mtry=45)
# num.trees = 250,
# Time difference of 27.86202 mins
# Accuracy : 0.9905          
# 95% CI : (0.9895, 0.9915)


# #Results - 19 - With Feature Engineering - datIn + datIn_70s + datIn_150s
# Time difference of 7.672186 mins
# inTrain <- createDataPartition(datSamp$label, p = 0.70)[[1]]
# bootControl <- trainControl( number=5, verboseIter=TRUE ) 
# rfGrid <- expand.grid(mtry=45)
# num.trees = 50,
# Accuracy : 0.9883          
# 95% CI : (0.9871, 0.9893)

# #Results - 18 - With Feature Engineering - With and without sharpened image.
# Time difference of 2.228411 hours
# bootControl <- trainControl( number=25, verboseIter=TRUE ) 
# rfGrid <- expand.grid(mtry=45)
# num.trees = 500,
# Accuracy : 0.9855        
# 95% CI : (0.984, 0.987)


#Results - 17 - With Feature Engineering - With and without sharpened image.
#Original dataset + dataset sharpened
# Time difference of 26.65356 mins
# inTrain <- createDataPartition(datSamp$label, p = 0.70)[[1]]
# bootControl <- trainControl( number=10, verboseIter=TRUE ) 
# rfGrid <- expand.grid(mtry=45)
# num.trees = 300,
# Accuracy : 0.9862          
# 95% CI : (0.9847, 0.9876)


#Results - 16 - With Feature Engineering
# Time difference of 1.636625 hours
# bootControl <- trainControl( number=25, verboseIter=TRUE ) 
# rfGrid <- expand.grid(mtry=45)
# inTrain <- createDataPartition(datSamp$label, p = 0.80)[[1]]
# num.trees = 1000,
# Accuracy : 0.967          
# 95% CI : (0.963, 0.9707)

#Results - 15 - With Feature Engineering
# Time difference of 16.88852 mins
# bootControl <- trainControl( number=15, verboseIter=TRUE ) 
# rfGrid <- expand.grid(mtry=45)
# Accuracy : 0.9623          
# 95% CI : (0.9588, 0.9655)


#Results - 14 - With Feature Engineering
# Time difference of 29.25112 mins
# bootControl <- trainControl( number=15, verboseIter=TRUE ) 
# rfGrid <- expand.grid(mtry=42)
# Train: 70 - ntrees: 300
# Accuracy : 0.9669        
# 95% CI : (0.9636, 0.97)


# # #Results - 13 - With Feature Engineering
# Time difference of 27.48336 mins
# bootControl <- trainControl( number=15, verboseIter=TRUE ) 
# rfGrid <- expand.grid(mtry=seq(32,42,10))
# The final value used for the model was mtry = 42. 
# Accuracy : 0.9664          
# 95% CI : (0.9631, 0.9695)

# # #Results - 12 - With Feature Engineering
# Train: 70 - ntrees: 100
# Time difference of 1.886488 mins
# rfGrid <- expand.grid(mtry=31)
# Accuracy : 0.9633          
# 95% CI : (0.9599, 0.9665)

# # #Results - 11 - With Feature Engineering
# Train: 70 - ntrees: 50
# Time difference of 5.864983 mins
# rfGrid <- expand.grid(mtry=seq(27,33,1))
# The final value used for the model was mtry = 31.
# Accuracy : 0.9613          
# 95% CI : (0.9578, 0.9646)

# # #Results - 10
# Train: 70 - ntrees: 1000
# Time difference of 1.517689 hours
# bootControl <- trainControl( number=25, verboseIter=TRUE )
# Accuracy  Kappa      Accuracy SD  Kappa SD   
# 0.96279   0.9586403  0.001374446  0.001527647
# Tuning parameter 'mtry' was held constant at a value of 45
# Accuracy : 0.9655          
# 95% CI : (0.9622, 0.9687)


# # #Results - 10
# Train: 70 - ntrees: 20
# Time difference of 3.804854 mins
# bootControl <- trainControl( number=25, verboseIter=TRUE ) 
# Accuracy : 0.9552          
# 95% CI : (0.9515, 0.9588)
# Accuracy   Kappa     Accuracy SD  Kappa SD   
# 0.9473879  0.941521  0.002262619  0.002512216
# Tuning parameter 'mtry' was held constant at a value of 45

# # #Results - 9 
# Train: 70 - ntrees: 100
# Time difference of 2.8288 mins
# bootControl <- trainControl( number=15, verboseIter=TRUE ) 
# rfGrid <- expand.grid(mtry=45)
# Accuracy : 0.9648          
# 95% CI : (0.9614, 0.9679)
# Accuracy   Kappa      Accuracy SD  Kappa SD   
# 0.9588135  0.9542199  0.001354036  0.001504298
# Tuning parameter 'mtry' was held constant at a value of 45


# # #Results - 8 
# Time difference of 35.0028 mins
# Train: 80 - ntrees: 500
# bootControl <- trainControl( number=15, verboseIter=TRUE ) 
# rfGrid <- expand.grid(mtry=45)
# Accuracy   Kappa      Accuracy SD  Kappa SD   
# 0.9634425  0.9593659  0.001546023  0.001718495
# Accuracy : 0.9651         
# 95% CI : (0.961, 0.9689)

# # #Results - 7 - Amazon c3.8xlarge
# Train: 80 - ntrees: 500
# bootControl <- trainControl( number=15, verboseIter=TRUE ) 
# 40 models each 2 replications - 15 times
# rfGrid <- expand.grid(mtry=seq(20,60,1))
# Time difference of 4.156706 hours
# The final value used for the model was mtry = 45. 
# Accuracy : 0.9622          
# 95% CI : (0.9579, 0.9662)

# # #Results - 6 - Amazon c3.8xlarge
# Time difference of 6.12057 mins
# Tuning parameter 'mtry' was held constant at a value of 35
# bootControl <- trainControl( number=15, verboseIter=TRUE ) 
# Train: 80 - ntrees: 500
# Accuracy : 0.9624          
# 95% CI : (0.9581, 0.9663)
# Accuracy   Kappa      Accuracy SD  Kappa SD   
# 0.9643902  0.9604182  0.001558485  0.001732455

# # #Results - 5
# Time difference of 28.86426 mins
# Train: 80 - ntrees: 500
# Tuning parameter 'mtry' was held constant at a value of 35
# bootControl <- trainControl( number=15, verboseIter=TRUE ) 
# Accuracy : 0.9615          
# 95% CI : (0.9572, 0.9655)
# Accuracy   Kappa      Accuracy SD  Kappa SD   
# 0.9643952  0.9604239  0.001395566  0.001551199

# # #Results - 4
# Time difference of 1.024916 hours
# Train: 80 - ntrees: 300
# bootControl <- trainControl( number=5, verboseIter=TRUE ) 
# rfGrid <- expand.grid(mtry=seq(30,40,1))
# The final value used for the model was mtry = 35. 
# Accuracy : 0.963           
# 95% CI : (0.9587, 0.9669)

# #Results - 3
# # Train: 80 - ntrees: 300
# Time difference of 23.07559 mins
# Tuning parameter 'mtry' was held constant at a value of 32
# Accuracy   Kappa      Accuracy SD  Kappa SD   
# 0.9633725  0.9592872  0.001705588  0.001894641
# Accuracy : 0.9664          
# 95% CI : (0.9623, 0.9702)

# #Results - 2
# Train: 70 - ntrees: 300
# Time difference of 19.07781 mins
# Tuning parameter 'mtry' was held constant at a value of 32
# Accuracy : 0.9642          
# 95% CI : (0.9608, 0.9674)

# #Results - 1
# Train: 70 - ntrees: 300
# Time difference of 44.72863 mins
# rfGrid <- expand.grid(mtry=seq(20,33,1))
# The final value used for the model was mtry = 32. 
# Accuracy : 0.966           
# 95% CI : (0.9627, 0.9691)



#--------------------------------- 
#---------------------- XGB (new version CARET)
#--------------------------------- 
set.seed(387)
a <- Sys.time();a
bootControl <- trainControl(number=5, verboseIter=TRUE) 

xgbGrid <- expand.grid(
  eta = 0.13,
  max_depth = 20,
  nrounds = 300,
  gamma = 0.501,
  colsample_bytree = 0.201,
  min_child_weight = 0.01
)
#nrounds, max_depth, eta, gamma, colsample_bytree, min_child_weight
#eta (0,1) - default: 0.3
#max_depth (1-Inf) - default: 6
#gamma (0-Inf) - default: 0
#min_child_weight (0-Inf) - default: 1
#colsample_bytree (0-1) - default:1


modFitxgb <-  train(
  label ~ .,
  # x = trainDat[, 1:(ncol(trainDat)-1)],
  # y = trainDat[, ncol(trainDat)], 
  data = trainDat,
  trControl = bootControl,
  tuneGrid = xgbGrid,
  metric = "Accuracy",
  method = "xgbTree",
  verbose = 1,
  num_class = 10
  #early.stop.round = 3
)

modFitxgb

predxgb <- predict( modFitxgb, newdata=testDat[,2:ncol(testDat)] )
#ConfusionMatrix
conMatxgb <- confusionMatrix(testDat$label, predxgb); conMatxgb 
conMatxgbdf <- as.data.frame(conMatxgb$overall); xgbAcc <- conMatxgbdf[1,1]; xgbAcc <- as.character(round(xgbAcc*100,2))
b <- Sys.time();b; b-a   

if( nrow(xgbGrid) < 2  )  { resampleHist(modFitxgb) } else  
{ plot(modFitxgb, as.table=T) }

# #Variable Importance
# Impxgb <- varImp( modFitxgb, scale=F)
# plot(Impxgb, top=20)

#Best iteration
modBest <- modFitxgb$bestTune; modBest
modBestc <- paste(modBest[1],modBest[2],modBest[3], sep="_")
#Execution time:
modFitxgb$times$final[3]
#Samples
samp <- dim(modFitxgb$resample)[1]


#Save trainDat, testDat and Model objects.
save(
  trainDat, testDat, modFitrf,
  file=paste("XGB_",numvars,"vars_n",samp,"_grid",modBestc,"_",xgbAcc,"__.RData", sep="")
)


#---------------------------------------------------


# #Results - 3 - With Feature Engineering - datIn + datIn_70s + datIn_150s
# Time difference of 4.658915 hours
# bootControl <- trainControl(number=5, verboseIter=TRUE) 
# nrounds max_depth  eta gamma colsample_bytree min_child_weight
# 1     300        20 0.13 0.501            0.201             0.01
# Accuracy : 0.9915          
# 95% CI : (0.9905, 0.9924)


# # #Results - 2
# Time difference of 1.250703 hours
# bootControl <- trainControl(number=5, verboseIter=TRUE) 
# nrounds max_depth  eta gamma colsample_bytree min_child_weight
# 1     300        20 0.13 0.501            0.201             0.01
# Accuracy : 0.9642          
# 95% CI : (0.9608, 0.9674)

# # #Results - 1
# Time difference of 1.732895 hours
# bootControl <- trainControl(number=5, verboseIter=TRUE) 
# nrounds max_depth    eta gamma colsample_bytree min_child_weight
# 1     300        71 0.1112 0.501            0.201             0.01
# Accuracy : 0.9646          
# 95% CI : (0.9612, 0.9678)


#---------------------------------------------------------------- 
#---------------------------------------------------------------- 
#---------------------- FILE TO UPLOAD
#---------------------------------------------------------------- 
#---------------------------------------------------------------- 
library(caret)
library(data.table)
library(stringr)
datTest <- fread("test.csv")
datTest <- as.data.frame(datTest)
#datTest_o <- datTest[,1:784]

#--------------------------------- 
#--------------------- Feature Engineering
#--------------------------------- 


#--------------- Feature Engineering ----------------
#Percentage of non cero by row, related to number?. General the whole image.
datTest$fenumce <- apply(datTest[,1:ncol(datTest)]==0, 1, sum)/(ncol(datTest)-1)

#--------------- Number of ceros/1 per cuadrant - each digit
#Number of 1 per quadrant.
#Vertical: First the extremes on the right - Then 14 units from there.
# First-Quadrant
v_idx <- seq(0, 756, 28)
v_idx_1q <- vector()
for (i in 1:(length(v_idx) / 2)) {
  vtmp <- seq(v_idx[i], v_idx[i] + 14, 1)
  v_idx_1q <- c(v_idx_1q, vtmp)
}
nam_1q <- paste("pixel", v_idx_1q, sep = "")
dat1q <- datTest[, (names(datTest) %in% nam_1q)]
numce1q <- apply(dat1q == 0, 1, sum) / ncol(dat1q)

# Third-Quadrant
v_idx_3q <- vector()
for (i in ((length(v_idx) / 2) + 1):length(v_idx)) {
  vtmp <- seq(v_idx[i], v_idx[i] + 14, 1)
  v_idx_3q <- c(v_idx_3q, vtmp)
}
nam_3q <- paste("pixel", v_idx_3q, sep = "")
dat3q <- datTest[, (names(datTest) %in% nam_3q)]
numce3q <- apply(dat3q == 0, 1, sum) / ncol(dat3q)

#--------------------------------
#Second Quadrant and 4th quadrant
val24_ini <- seq(0, 27, 1)
val24_end <- seq(756, 783, 1)
v24_idx <- seq(val24_ini[14], val24_end[14], 28)
# Second-quadrant
v_idx_2q <- vector()
for (i in 1:(length(v24_idx) / 2)) {
  vtmp <- seq(v24_idx[i], v24_idx[i] + 14, 1)
  v_idx_2q <- c(v_idx_2q, vtmp)
}
nam_2q <- paste("pixel", v_idx_2q, sep = "")
dat2q <- datTest[, (names(datTest) %in% nam_2q)]
numce2q <- apply(dat2q == 0, 1, sum) / ncol(dat2q)

# Fourth-Quadrant
v_idx_4q <- vector()
for (i in ((length(v24_idx) / 2) + 1):length(v24_idx)) {
  vtmp <- seq(v24_idx[i], v24_idx[i] + 14, 1)
  v_idx_4q <- c(v_idx_4q, vtmp)
}
nam_4q <- paste("pixel", v_idx_4q, sep = "")
dat4q <- datTest[, (names(datTest) %in% nam_4q)]
numce4q <- apply(dat4q == 0, 1, sum) / ncol(dat4q)


#Center of mass
f_cm <- function(x) {
  if(sum(x[x>0]) > 0) {
    tmp <- x
    idx <- which(tmp > 0, arr.ind=TRUE)
    co1q <- names(idx)
    co1qr <- as.numeric(gsub("pixel","",co1q))
    y_coor <- findInterval(co1qr, v_idx)
    x_coor <- co1qr - v_idx[y_coor]
    xm <- sum(x_coor)/length(idx)
    ym <- sum(y_coor)/length(idx)
    return(c(xm,ym))
  } else { return(c(0,0)) }
}

coor1q <- as.data.frame(t(apply(as.matrix(dat1q), 1, f_cm )))
coor2q <- as.data.frame(t(apply(as.matrix(dat2q), 1, f_cm )))
coor3q <- as.data.frame(t(apply(as.matrix(dat3q), 1, f_cm )))
coor4q <- as.data.frame(t(apply(as.matrix(dat4q), 1, f_cm )))


# Clean
rm(v_idx, v24_idx)
rm(v_idx_1q, v_idx_2q, v_idx_3q, v_idx_4q)
rm(nam_1q, nam_2q, nam_3q, nam_4q)
rm(dat1q, dat2q, dat3q, dat4q)
rm(val24_ini, val24_end, vtmp, i)

# Extend datTest
# Number of ceros
datTest$fenumce1q <- numce1q
datTest$fenumce2q <- numce2q
datTest$fenumce3q <- numce3q
datTest$fenumce4q <- numce4q
# Center of mass per cuadrant
datTest$fecm1qx <- coor1q$V1
datTest$fecm1qy <- coor1q$V2
datTest$fecm2qx <- coor2q$V1
datTest$fecm2qy <- coor2q$V2
datTest$fecm3qx <- coor3q$V1
datTest$fecm3qy <- coor3q$V2
datTest$fecm4qx <- coor4q$V1
datTest$fecm4qy <- coor4q$V2
datTest$fecm12rx <- datTest$fecm1qx / datTest$fecm2qx
datTest$fecm13rx <- datTest$fecm1qx / datTest$fecm3qx
datTest$fecm14rx <- datTest$fecm1qx / datTest$fecm4qx
datTest$fecm23rx <- datTest$fecm2qx / datTest$fecm3qx
datTest$fecm24rx <- datTest$fecm2qx / datTest$fecm4qx
datTest$fecm34rx <- datTest$fecm3qx / datTest$fecm4qx
datTest$fecm12ry <- datTest$fecm1qy / datTest$fecm2qy
datTest$fecm13ry <- datTest$fecm1qy / datTest$fecm3qy
datTest$fecm14ry <- datTest$fecm1qy / datTest$fecm4qy
datTest$fecm23ry <- datTest$fecm2qy / datTest$fecm3qy
datTest$fecm24ry <- datTest$fecm2qy / datTest$fecm4qy
datTest$fecm34ry <- datTest$fecm3qy / datTest$fecm4qy
datTest$fecmgex <- sum(coor1q$V1, coor2q$V1, coor3q$V1, coor4q$V1)/4 
datTest$fecmgey <- sum(coor1q$V2, coor2q$V2, coor3q$V3, coor4q$V2)/4 

rm(coor1q, coor2q, coor3q, coor4q, numce1q, numce2q, numce3q, numce4q)

load("ranger_785vars_rf_n25_grid45_96.55__.RData")
modFit <- modFitrf
modtype <-modFit$method
samptmp <- modFit$resample; samp <- length(unique(samptmp$Resample))
numvars <- length(modFit$coefnames)
timval <- str_replace_all(Sys.time(), " |:", "_")

pred_Digits <- as.numeric(as.vector(predict(modFit, newdata=datTest)))
toSubmit <- data.frame(ImageId=1:length(pred_Digits), Label=pred_Digits)

write.table(toSubmit, file=paste("Res_xxxx_", modtype,"_",numvars,"_n",samp,"_Acc_", timval,".csv",sep=""),sep=","
            , row.names=FALSE,col.names=TRUE, quote=FALSE)


#-----------------------------
#-----------------------------
#-----------------------------
#-----------------------------
# Graph Test Data
datTest_o <- datTest[,1:784]
library(gridExtra)
for(i in 1:10) {
  #one <- datIn[i,]
  one <- datTest_o[i,]
  one_mat <- matrix(one[1:length(one)], nrow=28, ncol=28)
  gr_out <- levelplot(one_mat, main=paste(one$label), xlab="",ylab="" )
  print(gr_out)
} 

#----------------------------
# Plot some cases..
#http://stackoverflow.com/questions/16496210/rotate-a-matrix-in-r
val_idx <- 4 #number to plot
dat_four <- datIn[datIn$label==val_idx,]
#dat_four <- datIn_s[datIn_s$label==val_idx,]
hw_ma <- 6
dat4_red <- dat_four[sample(1:nrow(dat_four), hw_ma) , ]
gr_lst <- list()
gr_lst_r <- list()
for(i in 1:hw_ma) {
  one <- dat4_red[i,]
  one_mat <- matrix(one[2:length(one)], nrow=28, ncol=28, byrow=TRUE)
  #to rotate 90º -> m[nrow(m):1,ncol(m):1]
  one_mat_r <- t(one_mat[nrow(one_mat):1, ])
  gr_out <- levelplot(one_mat, main=paste(one$label), xlab="",ylab="", colorkey=FALSE )
  gr_out_r <- levelplot(one_mat_r, main=paste(one$label), xlab="",ylab="", colorkey=FALSE )
  gr_lst[[i]] <- gr_out
  gr_lst_r[[i]] <- gr_out_r
  #print(gr_out)
} 
#join lists
big_lst <- list()
val_r <- rep(1:hw_ma, each=2)
for(i in 1:length(val_r)) {
  if(i %% 2 ==0) {
    big_lst[[ i ]] <- gr_lst_r[[ val_r[i] ]]
  } else { big_lst[[ i ]] <- gr_lst[[ val_r[i] ]] }
}
#Compare original with rotated 90º
do.call(grid.arrange, c( big_lst, list(ncol=4) ) )
