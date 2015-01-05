source("util/error_func.R")
source("util/date_helpers.R")
source("util/plot_helpers.R")
source("util/preprocess.R")
source("models/load/load_model.R")
source("models/load/temp/temp_model.R")
source("models/load/feature_helpers.R")

# clear workspace: rm(list = ls(all = TRUE))
path <- "data/load/train"

last.dt <- getLastDt()
train.df <- createTrainDF(loadCSVs(path), getFirstDt(), getLastDt())

### PREPARE DATA ###
temp.df <- reduceToTempDF(train.df)
avg.temp.series <- avgTempSeries(temp.df)
avg.temp.list.yearly <- listSeriesByYear(avg.temp.series, "empm")
avg.temp.yearly <- mergeSeriesByHour(avg.temp.list.yearly)
avg.temp <- cbind(avg.temp.series, HASH=hashDtYear(avg.temp.series$TMS))

load.df <- na.omit(reduceToLoadDF(train.df)) # remove first years of NA values 
hash = hashDtYear(load.df$TMS)
load.df <- cbind(load.df, HASH=hash)

### DEFINE DATES & HORIZON ###
start.dt <- addHours(getLastDt(), 1)
startearlier.dt <- subMonths(start.dt, 3)
train.horizon = 60
use.temp = FALSE

### PRECOMPUTE LOAD FEATURES ###
load.features <- createLoadFeatures(load.df, start.dt, 1)

### TRAIN LOAD MODEL ### what temperature data to use?
train.dt <- subMonths(start.dt, horizon)
train.features <- assembleFeatures(load.features, avg.temp, train.dt, train.horizon)
train.result <- trainLoadModel(train.features, train.dt, train.horizon)
load.model <- train.result[["model"]]
residuals <- train.result[["residuals"]]
  
### PREDICT TEMPERATURE VS. USE REAL TEMP. ###
if (use.temp) {
  pred.temp <- avg.temp
} else {
  pred.temp <- predictTemp(avg.temp, avg.temp.yearly, start.dt, 1) # should return seq temp diff + 2 month prediction 
}

### TEST LOAD MODEL ###
test.features <- assembleFeatures(load.features, pred.temp, start.dt, 1)
saveRDS(test.features, file="pred_features.R")
load.fit <- predict.gam(load.model, test.features[, -(1:2)]) # remove TMS and Y from features
err.measures <- pointErrorMeasures(test.features$Y, load.fit)

### QUANTILE PLOT & SUMMARY ###
pred.quantiles <- createPredQuantiles(load.fit, residuals)
plotPredictionQuantiles(test.features$TMS, test.features$Y, load.fit, pred.quantiles, 4,
                          paste0(start.dt, " + 1 month in hours"), "load in MW", "Plot of Percentiles")

summary(load.model)
print(err.measures)

#crossValidation(load.df, load.yearly, avg.temp.series, getFirstDt(), addHours(getLastDt(), 1), 12) 

# save quantiles to some file - ATM there must be bug in temp pred

# save( ... , ... , ...)