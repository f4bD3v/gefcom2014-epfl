source("util/error_func.R")
source("util/date_helpers.R")
source("util/plot_helpers.R")
source("util/preprocess.R")
source("models/load/load_model.R")
source("models/load/temp/temp_model.R")
source("models/load/feature_helpers.R")

dir.exists <- function(path) FALSE

# clear workspace: rm(list = ls(all = TRUE))
in.path <- "data/load/train"
out.path <- "data/load/test/CV"

today <- as.character(Sys.Date())
today.path <- paste(out.path, today, sep="/")
if (!dir.exists(today.path)) dir.create(today.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")

last.dt <- getLastDt()
train.df <- createTrainDF(loadCSVs(in.path), getFirstDt(), getLastDt())

### PREPARE DATA ###
temp.df <- reduceToTempDF(train.df)
avg.temp.series <- avgTempSeries(temp.df)
avg.temp.list.yearly <- listSeriesByYear(avg.temp.series, "empm")
avg.temp.yearly <- mergeSeriesByHour(avg.temp.list.yearly)
avg.temp <- cbind(avg.temp.series, HASH=hashDtYear(avg.temp.series$TMS))

load.df <- na.omit(reduceToLoadDF(train.df)) # remove first years of NA values 
hash = hashDtYear(load.df$TMS)
load.df <- cbind(load.df, HASH=hash)

use.temp = FALSE
pred.traintemp = TRUE

### DEFINE DATES & HORIZON ###

# test set
test.start.dt <- addYears(getFirstDt(), 10)
test.dt <- test.start.dt
test.stop.dt <- subHours(addMonth(test.dt), 1)
test.horizon <- 1 
test.len <- 6

# training set
train.start.dt <- addYears(getFirstDt(), 2)
print(train.start.dt)
train.dt <- train.start.dt
train.stop.dt <- subHours(test.dt, 1)
train.horizon <- 8*12 

load.train.start.dt <- subYears(test.start.dt, 5)
print(load.train.start.dt)
load.train.dt <- load.train.start.dt
load.train.horizon <- 5*12 #train.horizon - 5*12

test.path <- paste(today.path, as.character(as.Date(addMonths(test.dt, test.len))), sep="/")
if (!dir.exists(test.path)) dir.create(test.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")

out.path <- paste(test.path, as.character(train.horizon), as.character(load.train.horizon), sep="/")
if (!dir.exists(out.path)) dir.create(out.path, showWarnings = TRUE, recursive = TRUE, mode = "0777")

# gam models
temp.model.formulas <-list("WLAG52 + TOY + HOUR", 
                      "WLAG5 + WLAG52 + TOY + HOUR",
                      "WLAG52 + s(TOY, k=10) + s(HOUR, k=10)",
                      "s(WLAG5, k=24) + s(WLAG52, k=24) + s(TOY, k=10) + s(HOUR, k=10)",
                      "s(WLAG5, k=40) + s(WLAG52, k=10) + s(TOY, k=12) + s(HOUR, k=24)",
                      "s(WLAG5, TOY, k=40) + s(WLAG52, k=24) + s(TOY, k=12) + s(HOUR, k=24)",
                      "s(WLAG5, by=TOY, k=40) + s(WLAG52, k=24) + s(TOY, k=12) + s(HOUR, k=24)",
                      "s(WLAG5, by=MONTH, k=40) + s(WLAG52, k=24) + s(TOY, k=12) + s(HOUR, k=24)",
                      "s(WLAG52, k=24) + s(TOY, k=12) + s(HOUR, by=MONTH, k=24)")

temp.model.formulas <- list("s(WLAG52, k=24) + s(TOY, k=12) + s(HOUR, by=MONTH, k=24)",
                      "WLAG52 + s(TOY, k=10) + s(HOUR, k=10)",
                      "WLAG52 + TOY + HOUR")

temp.model.formulas <- list("s(WLAG5, TOY, k=52) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)")  

# with 5week lag, once without
load.model.formulas <- list("s(CTEMP, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(WLAG5, TOY, k=52) + s(WLAG52, k=24)",
                            "s(CTEMP, HOUR, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(WLAG5, TOY, k=52) + s(WLAG52, k=24)",
                            "s(CTEMP, HOUR, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(WLAG5, TOY, k=52) + s(WLAG52, k=24) + s(MTL7D, k=24) + s(MAXT24H, k=52) + s(MINT24H, k=52) + s(TM24H, k=52) + s(TM48H, k=52) + s(TM2H) + s(TM1H)")

#"DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=24) + s(WLAG5, k=24) + s(WLAG52, k=24)",
### CROSSVALIDATION ###

CV.res <- list()
temp.features <- createTempFeatures(avg.temp, train.dt, train.horizon + test.horizon)
load.features <- createLoadFeatures(load.df, train.dt, train.horizon + test.horizon)

plots.path <- paste(out.path, "loadCVplots.pdf", sep="/")
pdf(file=plots.path)
output.file <- paste(out.path, "loadCVlog.csv", sep="/")
for(k in 1:length(load.model.formulas)) {
  load.model.chr <- paste0("Load Model ", k, ": ", load.model.formulas[[k]])
  load.train.chr <- paste0("Load Model train horizon: ", load.train.horizon)
  appendToFile(load.model.chr, output.file)
  appendToFile(load.train.chr, output.file)
  
  for(i in 1:length(temp.model.formulas)) {
    temp.model.chr <- paste0("Temp Model ", i, ": ", temp.model.formulas[[i]])
    temp.train.chr <- paste0("Temp Model train horizon", train.horizon)
    appendToFile(temp.model.chr, output.file)
    appendToFile(temp.train.chr, output.file)
    temp.load.train.dt <- train.dt
    temp.load.pred.dt <- load.train.dt
    flex.horizon <- 3*12
    if(pred.traintemp) {
      avg.temp <- avg.temp[-(avg.temp$HASH==hashDtYear(load.train.dt):length(avg.temp)), ]
      for (i in 1:load.train.horizon) {
        # increase training period with every month
        train.features <- getFeatures(features, temp.load.train.dt, flex.horizon)
        test.features <- getFeatures(features, temp.load.pred.dt, test.horizon)
        if(model.formulas[[k]] != "mean") {
    
        }  
        temp.model <- trainTempModel(train.features, temp.model.formulas[[1]], temp.load.train.dt)
        test.fit <- predict.gam(temp.model, test.features[, -(1:2)])
        pred.stop.dt <- getStopDtByHorizon(temp.load.pred.dt, 1)
        test.dt.seq <- seq(temp.load.pred.dt, pred.stop.dt, by="hour")
        fit <- data.frame(TMS=test.dt.seq, MTEMP=test.fit, HASH=hashDtYear(test.dt.seq))
        #colnames(fit) <- colnames(avg.temp)
        index1 <- which(temp.features$HASH==hashDtYear(test.dt.seq)[1], arr.ind=TRUE)
        index2 <- which(temp.features$HASH==hashDtYear(test.dt.seq)[length(test.dt.seq)], arr.ind=TRUE)
        if(temp.model.formulas[[k]] != "mean") {
          target <- temp.features[index1:index2, "LAGM"]      
        }
        else {
          target <- temp.features[index1:index2, 2]
        }  
        index <- which(avg.temp$HASH == hashDtYear(temp.load.pred.dt), arr.ind = TRUE)  
        if (length(index) != 0) avg.temp <- avg.temp[-c(index:nrow(avg.temp)),]
        pred.temp <- fit
        avg.temp <- rbind(avg.temp, pred.temp)
        # Update
        temp.load.pred.dt <- addMonth(temp.load.pred.dt)
        flex.horizon <- flex.horizon + 1
      }
    }
    for (j in 1:test.len) {
      ### TEMPERATURE VALIDATION
      train.features <- getFeatures(temp.features, train.dt, train.horizon)
      test.features <- getFeatures(temp.features, test.dt, test.horizon)
      temp.model <- trainTempModel(train.features, temp.model.formulas[[i]], train.dt)
      test.fit <- predict.gam(temp.model, test.features[, -(1:2)])
      test.stop.dt <- getStopDtByHorizon(test.dt, 1)
      test.dt.seq <- seq(test.dt, test.stop.dt, by="hour")
      fit <- data.frame(TMS=test.dt.seq, MTEMP=test.fit, HASH=hashDtYear(test.dt.seq))
      #colnames(fit) <- colnames(avg.temp)
      index1 <- which(temp.features$HASH==hashDtYear(test.dt.seq)[1], arr.ind=TRUE)
      index2 <- which(temp.features$HASH==hashDtYear(test.dt.seq)[length(test.dt.seq)], arr.ind=TRUE)
      target <- temp.features[index1:index2, 2]
      plotTraining(test.dt.seq, target, fit$MTEMP, 0, xlabel=paste(as.character(test.dt), "1 month in hours", sep=" +"),
                   ylabel="Temperature in Fahrenheit", title=paste0("Temperature Model Validation, Model: ", temp.model.formulas[[i]]))
      
      if (test.dt < getLastDt()) {
        index <- which(avg.temp$HASH == hashDtYear(test.dt), arr.ind = TRUE)  
        if (length(index) != 0) avg.temp <- avg.temp[-c(index:nrow(avg.temp)),]
      }
      
      pred.temp <- fit
      if (use.temp) {
        pred.temp <- data.frame(TMS=test.dt.seq, MTEMP=target, HASH=hashDtYear(test.dt.seq))
      }
      avg.temp <- rbind(avg.temp, pred.temp)
      
      ### LOAD VALIDATION
      load.train.features <- assembleFeatures(load.features, avg.temp, load.train.dt, load.train.horizon)
      load.test.features <- assembleFeatures(load.features, avg.temp, test.dt, test.horizon)
      
      train.result <- trainLoadModelFormula(load.train.features, load.model.formulas[[k]], train.dt)
      load.model <- train.result[["model"]]  
      capture.output(summary(temp.model), file="load_models_CV.txt", append=TRUE)
      train.residuals <- train.result[["residuals"]]
      
      load.fit <- predict.gam(load.model, load.test.features[, -(1:2)])
      err.measures <- pointErrorMeasures(load.test.features$Y, load.fit)
      # add train error quantiles to point prediction
      test.quantiles <- createPredQuantiles(load.fit, train.residuals)
      ### create several plots, one for every week
      plotPredictionQuantiles(load.test.features$TMS, load.test.features$Y, load.fit, test.quantiles, 4,
                              paste0(test.dt, " + 1 month in hours"), "load in MW", "Plot of Percentiles")
      
      # use hash instead of TMS?
      tms.row <- cbind(TRAIN.TMS=as.character(train.dt), TEST.TMS=as.character(test.dt))
      err.row <- cbind(do.call(cbind.data.frame, err.measures), PINBALL=pinball(test.quantiles, load.test.features$Y))
      res.row <- cbind(tms.row, err.row)
      if (j == 1) res <- res.row else res <- rbind(res, res.row)
      
      # Update
      train.dt <- addMonth(train.dt)
      print(paste0("traindt",train.dt))
      test.dt <- addMonth(test.dt)
      print(paste0("testdt",test.dt))
      load.train.dt <- addMonth(load.train.dt)
      print(paste0("traindt",load.train.dt))
    }
    res.final.row <- cbind(TRAIN.TMS=as.character(test.start.dt), TEST.TMS=as.character(addMonth(subHours(test.dt, 1))), RMSE=mean(res$RMSE), MAE=mean(res$MAE), MAPE=mean(res$MAPE), PINBALL=mean(res$PINBALL))
    res <- rbind(res, res.final.row)
    row.names(res) <- c(c(1:test.len), "MODEL CV MEAN")
    appendTableToFile(res, output.file)
    
    train.dt <- train.start.dt
    test.dt <- test.start.dt
    load.train.dt <- load.train.start.dt
    
    CV.res[[k]] <- res
  }
  appendToFile("\n", output.file)
}
dev.off()