source("util/error_func.R")
source("util/date_helpers.R")
source("util/plot_helpers.R")
source("util/preprocess.R")
source("models/load/load_model.R")
source("models/load/temp/temp_model.R")
source("models/load/feature_helpers.R")

# clear workspace: rm(list = ls(all = TRUE))
path <- "data/load/train"

use.temp = FALSE
use.pca = FALSE
use.temp1 = FALSE
pred.traintemp = TRUE

last.dt <- getLastDt()
train.df <- createTrainDF(loadCSVs(path), getFirstDt(), getLastDt())

### PREPARE DATA ###
temp.df <- reduceToTempDF(train.df)
pca <- prcomp(temp.df[,-1], retx=TRUE, tol=0.2)
pc1 <- pca$x

if (use.temp1) temp.df <- temp.df[ ,c(1,2)]
avg.temp.series <- avgTempSeries(temp.df)
avg.temp.list.yearly <- listSeriesByYear(avg.temp.series, "empm")
avg.temp.yearly <- mergeSeriesByHour(avg.temp.list.yearly)
hash <- hashDtYear(avg.temp.series$TMS)
avg.temp <- cbind(avg.temp.series, HASH=hashDtYear(avg.temp.series$TMS))

pc.temp <- data.frame(TMS=avg.temp.series$TMS, MTEMP=pc1, HASH=hash)

load.df <- na.omit(reduceToLoadDF(train.df)) # remove first years of NA values 
hash <- hashDtYear(load.df$TMS)
load.df <- cbind(load.df, HASH=hash)

### DEFINE DATES & HORIZON ###
test.dt <- addHours(getLastDt(), 1)
test.dt <- subMonths(test.dt, 1)
load.train.dt <- subYears(test.dt, 5)
train.dt <- subYears(load.train.dt, 4)

train.horizon <- 9*12 
test.horizon <- 1

load.train.horizon <- train.horizon - 4*12

### PRECOMPUTE LOAD FEATURES ###
load.features <- createLoadFeatures(load.df, load.train.dt, load.train.horizon + test.horizon)
temp.features <- createTempFeatures(avg.temp, train.dt, train.horizon + test.horizon)

htype <- 2

flex.horizon <- 4*12
temp.load.train.dt <- train.dt
temp.load.pred.dt <- load.train.dt 
if(pred.traintemp) {
  index <- which(avg.temp$HASH==hashDtYear(load.train.dt))
  print(index)  
  avg.temp <- avg.temp[-c(index:nrow(avg.temp)), ]
  for (h in 1:(load.train.horizon+1)) {
    print(h)
    h <- load.train.horizon+2
    # increase training period with every month
    train.features <- getFeatures(temp.features, temp.load.train.dt, flex.horizon, 2)
    test.features <- getFeatures(temp.features, temp.load.pred.dt, test.horizon, 2)
    
    pred.stop.dt <- getStopDtByHorizon(temp.load.pred.dt, 1)
    test.dt.seq <- seq(temp.load.pred.dt, pred.stop.dt, by="hour")
    index1 <- which(temp.features$HASH==hashDtYear(test.dt.seq)[1], arr.ind=TRUE)
    index2 <- which(temp.features$HASH==hashDtYear(test.dt.seq)[length(test.dt.seq)], arr.ind=TRUE)
    
    temp.model <- trainTempModel(train.features, "s(DLAG, by=TOY, k=24) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)", temp.load.train.dt)
    test.fit <- predict.gam(temp.model, test.features[, -(1:2)])
    
    fit <- data.frame(TMS=test.dt.seq, MTEMP=test.fit, HASH=hashDtYear(test.dt.seq))
    #colnames(fit) <- colnames(avg.temp)
    target <- temp.features[index1:index2, 2]
    #index <- which(avg.temp$HASH == hashDtYear(temp.load.pred.dt), arr.ind = TRUE)  
    #if (length(index) != 0) avg.temp <- avg.temp[-c(index:nrow(avg.temp)),]
    pred.temp <- fit
    avg.temp <- rbind(avg.temp, pred.temp)
    # Update
    temp.load.pred.dt <- incrementDt(temp.load.pred.dt, 1, htype)
    flex.horizon <- flex.horizon + 1
  }
}

### TRAIN LOAD MODEL ### what temperature data to use?
load.train.features <- assembleFeatures(load.features, avg.temp, load.train.dt, load.train.horizon, htype)
train.result <- trainLoadModel(load.train.features, load.train.dt)
load.model <- train.result[["model"]]
residuals <- train.result[["residuals"]]
  
### PREDICT TEMPERATURE VS. USE REAL TEMP. ###
if (use.temp) {
  pred.temp <- avg.temp
} else {
  print("predicting temp")
  temp.train.horizon <- train.horizon
  pred.temp <- predictTemp(temp.features, test.dt, temp.train.horizon) # should return seq temp diff + 2 month prediction 
}

### TODO, use 365 day lag for temperature prediction
pred.temp <- avg.temp
### TEST LOAD MODEL ###
test.features <- assembleFeatures(load.features, pred.temp, test.dt, test.horizon, htype)
saveRDS(test.features, file="pred_features.R")
load.fit <- predict.gam(load.model, test.features[, -(1:2)]) # remove TMS and Y from features
err.measures <- pointErrorMeasures(test.features$Y, load.fit)

### QUANTILE PLOT & SUMMARY ###
pred.quantiles <- createPredQuantiles(load.fit, residuals)
plotPredictionQuantiles(test.features$TMS, test.features$Y, load.fit, pred.quantiles, 4,
                          paste0(test.dt, " + 1 month in hours"), "load in MW", "Plot of Percentiles")

summary(load.model)
print(err.measures)
print(pinball(pred.quantiles, test.features$Y))

#crossValidation(load.df, load.yearly, avg.temp.series, getFirstDt(), addHours(getLastDt(), 1), 12) 

# save quantiles to some file - ATM there must be bug in temp pred

# save( ... , ... , ...)