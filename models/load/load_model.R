require(mgcv)
require(nnet)
require(randomForest)

### CREATE LOAD FEATURES FOR $horizon TIME PERIOD ###
# - start.dt: datetime where to start creating features
# - htype: horizon type, default=2 --> months
createLoadFeatures <- function(load.df, start.dt, horizon, htype=2) {
  # get Datetime at end of horizon
  stop.dt <- getStopDtByHorizon(start.dt, horizon, htype)
  # if end of horizon exceeds end of dataset --> generate new tms & hash, add NAs to load
  if (stop.dt > last.dt) {
    cat("End of Horizon exceeded End of Dataset in createLoadFeatures()", sep="\n")
    dt.seq.target <- as.POSIXct(seq(from=start.dt, to=stop.dt, by="hour"), tz="EST")
    tms <- as.POSIXct(c(as.character(load.df$TMS), as.character(dt.seq.target)), tz="EST")
    load <- c(load.df$LOAD, rep(NA, length(dt.seq.target)))
    hash <- hashDtYear(tms)
  } else {
    load <- load.df$LOAD
    tms <- load.df$TMS
    hash <- load.df$HASH
  }
  
  # create daytypes 1-7, 8 for holiday
  daytype <- as.factor(createMaxDayFeatures(tms))
  hour <- createHourFeatures(tms)
  month <- createMonthFeatures(tms)
  
  #** CREATE TIME OF YEAR (TOY) FEATURE **#
  tms.year.list <- split(tms, year(ymd(as.Date(tms))))
  # pass years to function --> as.numeric(year) --> leap_year, if yes divide by specific number of hours in leap year or standard year
  num.hours <- 365*24
  list.of.seqs <- lapply(tms.year.list, function(x) { return(seq(0,length(x)/num.hours,length.out=length(x))) })
  time.of.year <- c(unlist(list.of.seqs))
  
  features <- data.frame(TMS=tms, LOAD=load, TOY=time.of.year, DAYT=daytype, MONTH=month, HOUR=hour, HASH=hash)
  return(features)
}


### GET TEMPERATURE FEATURES FOR LOAD PREDICTION ###
# - temp.df: the predicted or true temperature (either average over stations, that of first station or principal component of stations
# - start.dt: datetime from where to start 
# - horizon: feature length in months/weeks/years
getLoadTempFeatures <- function(temp.df, start.dt, horizon, htype=2) {
  cat(paste0("getting Load Temp Features: ",start.dt), sep="\n")
  cat(paste0("for htype: ", htypeToString(htype)), sep="\n")
  stop.dt <- getStopDtByHorizon(start.dt, horizon, htype)
  print(paste0("Stop Dt ", stop.dt))
  index.seq <- getSeqByDts(temp.df$HASH, start.dt, stop.dt)
  temp.series <- temp.df$MTEMP
  CTEMP <- temp.series[index.seq] 
  for (i in 1:length(index.seq)) {
    index <- index.seq[i]
    index.seq.last7d <- seq(index-168, index-1, 1)
    mean.last7d <- mean(temp.series[index.seq.last7d])
    index.seq.last24h <- seq(index-24, index-1, 1)
    max.last24h <- max(temp.series[index.seq.last24h])
    min.last24h <- min(temp.series[index.seq.last24h])
    temp_24h <- temp.series[index-24]
    temp_48h <- temp.series[index-48]
    temp_2h <- temp.series[index-2]
    temp_1h <- temp.series[index-1]
    # avg temp next 8 hours
    #index.seq.next8h <- seq(index+1, index+8, 1)
    #avg.temp.next8h <- mean(temp.series[index.seq.next8h])
    feature.row <- cbind(MTL7D=mean.last7d, MAXT24H=max.last24h, MINT24H=min.last24h, TM24H=temp_24h, TM48H=temp_48h, TM2H=temp_2h, TM1H=temp_1h)
    if (i == 1) features <- feature.row else features <- rbind(features, feature.row)
  }
  temp.features <- cbind(CTEMP, features)
  return(temp.features)
}

### GET PRECOMPUTED LOAD FEATURES FOR LOAD PREDICTION ###
# - feature.df: precomputed load features
# - start.dt:
# - lag.horizon: number of units of htype lag for assigning DLAG
# - horizon: ahead
# - htype: as before
getLoadFeatures <- function(load.features, start.dt, lag.horizon, horizon, htype=2) {
  cat(paste0("getting Load Features: ",start.dt), sep="\n")
  stop.dt <- getStopDtByHorizon(start.dt, horizon, htype)
  print(paste0("Stop Dt ", stop.dt))
  dt.seq.target <- seq(from=start.dt, to=stop.dt, by="hour")
  nrows <- nrow(load.features)

  # if end of horizon exceeds end of dataset --> generate new tms & hash, add NAs to load
  if (stop.dt > last.dt) {
    target <- rep(NA, length(dt.seq.target)) 
    index.seq.target <- seq(nrows-length(dt.seq.target)+1, nrows, 1)
  } else {
    index.seq.target <- getSeqByDts(load.features$HASH, start.dt, stop.dt)
    target <- load.features$LOAD[index.seq.target]
  }
  
  offset <- 0
  ### this may become a problem, if htype and lag.horizon are not related
  if(htype == 0) offset <- lag.horizon * 24 else if(htype == 1) offset <- lag.horizon * 7 * 24 else offset <- 5 * 7 * 24
  cat(paste0("DLAG offset: ", offset, " hours; htype: ", htypeToString(htype)), sep="\n")

  days.lag.seq <- index.seq.target - offset
  weeks52.lag.seq <- index.seq.target - (52*7*24)
  
  days.lag <- load.features$LOAD[days.lag.seq]
  weeks52.lag <- load.features$LOAD[weeks52.lag.seq]
  
  time.of.year <- load.features$TOY[index.seq.target] 
  daytype <- load.features$DAYT[index.seq.target] 
  hour <- load.features$HOUR[index.seq.target]
  month <- load.features$MONTH[index.seq.target]

  return(list(TMS=dt.seq.target, Y=target, DLAG=days.lag, WLAG52=weeks52.lag, TOY=time.of.year, DAYT=daytype, MONTH=month, HOUR=hour))
}  

assembleLoadFeatures <- function(load.features, avg.temp, start.dt, lag.horizon, horizon, htype) {
  # offset 0 instead of 4 bc load df starts at 2001 with NAs
  temp.features <- getLoadTempFeatures(avg.temp, start.dt, horizon, htype)
  load.feature.list <- getLoadFeatures(load.features, start.dt, lag.horizon, horizon, htype)
  load.features <- do.call(cbind.data.frame, load.feature.list)
  feature.df <- cbind(load.features, temp.features)
  return(feature.df)
}

# model.vars: colnames[-1]
#TODO: hard code formula, because some of the parameters should be grouped with smoothing functions
getLoadFormula <- function(target.var, model.vars) {
  # This creates the appropriate string:
  # "y ~ x1 + x2 + x3"
  str.formula <- paste(target.var, paste(model.vars, collapse=") + s("), sep=" ~ ")
  str.formula <- paste0(target.var, " ~ ",
                        "s(CTEMP, HOUR, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24)")
                        #"s(CTEMP, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(WLAG5, TOY, k=52) + s(WLAG52, k=24)")
                        #s(CTEMP, k=24) +
                        #"DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(WLAG52, k=24)")# + ",
                        #"s(MTL7D) + s(MAXT24H) + s(MINT24H) + s(TM24H) + s(TM48H) + s(TM2H) + s(TM1H)")
                        #s(CTEMP, HOUR, k=24) + 
  print(str.formula)
  # y ~ x1 + x2 + x3
  return(as.formula(str.formula))
}

trainLoadModelFormula <- function(feature.df, formula, train.dt) {
  formula <- paste0("Y ~ ", formula)
  load.model <- gam(as.formula(formula), family=gaussian(), data=feature.df)
  residuals <- feature.df$Y - load.model$fitted.values
  plotTraining(feature.df$TMS, feature.df$Y, load.model$fitted.values, mean(feature.df$Y)+residuals, xlabel=paste(as.character(train.dt), "1 months in hours", sep=" +"),
               ylabel="Load in MW", title="Load Model Training")
  return(list(model=load.model, residuals=residuals))
}

trainLoadModelFormulaLM <- function(feature.df, formula, train.dt) {
  formula <- paste0("Y ~ ", formula)
  load.model <- lm(as.formula(formula), family=gaussian(), data=feature.df)
  residuals <- feature.df$Y - load.model$fitted.values
  return(list(model=load.model, residuals=residuals))
}

trainLoadModelFormulaGAM <- function(feature.df, formula, train.dt, gamma) {
  formula <- paste0("Y ~ ", formula)
  if(gamma) {
	load.model <- gam(as.formula(formula), family=Gamma(), data=feature.df)
  } else {
	load.model <- gam(as.formula(formula), family=gaussian(), data=feature.df)
  }
  residuals <- feature.df$Y - load.model$fitted.values
  return(list(model=load.model, residuals=residuals))
}

trainLoadModelFormulaNN <- function(feature.df, formula, train.dt, hidden.units) {
  formula <- paste0("Y ~ ", formula)
  load.model <- nnet(formula=as.formula(formula), data=feature.df, maxit=750, decay=1e-3, size=hidden.units, linout=T)
#maxit=1000, decay=0.001, trace=F
  residuals <- feature.df$Y - as.vector(predict(load.model, data=feature.df)) #load.model$fitted.values
  return(list(model=load.model, residuals=residuals))
}

trainLoadModelFormulaRF <- function(feature.df, formula, train.dt, ntrees) {
  formula <- paste0("Y ~ ", formula)
  load.model <- randomForest(as.formula(formula), data=feature.df, nodesize=20, ntree=ntrees, importance=T)
  residuals <- feature.df$Y - predict(load.model, data=feature.df) #load.model$predicted
  return(list(model=load.model, residuals=residuals))
}

trainLoadModel <- function(feature.df, train.dt) {
  colnames <- colnames(feature.df)[-(1:2)]
  load.model <- gam(getLoadFormula("Y", colnames), family=gaussian(), data=feature.df)
  saveRDS(feature.df, file="trainData.rds")
  residuals <- feature.df$Y - load.model$fitted.values
  #plotTraining(feature.df$TMS, feature.df$Y, load.model$fitted.values, mean(feature.df$Y)+residuals, xlabel=paste(as.character(train.dt), "1 months in hours", sep=" +"),
  #             ylabel="Load in MW", title="Load Model Training")
  return(list(model=load.model, residuals=residuals))
}

validateLoadModel <- function(trained.model, load.features, avg.temp, valid.dt, horizon) {
  pred.feature.df <- assembleFeatures(load.features, avg.temp, valid.dt, horizon)
  colnames <- colnames(feature.df)[-(1:2)]
  model.validation <- predict.gam(trained.model, pred.feature.df[, -(1:2)])
  valid.Y <- pred.feature.df$Y
  valid.fit <- model.validation$fitted.values
  pred.quantiles <- createPredQuantiles(pred.fit, train.result[["residuals"]])
  plotPredictionQuantiles(pred.feature.df$TMS, valid.Y, valid.fit, pred.quantiles, 4,
                          paste0(start.dt, " + 1 month in hours"), "load in MW", "Plot of Percentiles")
  err.measures <- errorMeasures(valid.Y, valid.fit)
  return(list(validation=model.validation, point.errs=err.measures))
}

createPredQuantiles <- function(pred.fit, residuals) {
  # add prediction to residual quantile values
  quantiles=seq(0.01, 0.99, by=0.01)
  train.res.quantiles = quantile(residuals, probs=quantiles, na.rm=TRUE)
  # compute pinball loss function
  pred.quantiles = matrix(data=NA, nrow=length(pred.fit), ncol=99)
  for (q in 1:99){
    pred.quantiles[,q] = pred.fit + (train.res.quantiles[q])
  }
  return(pred.quantiles)
}

### TODO - always train on 2006-2010 period, same for crossvalidation
# Have dataframes ready & loaded and call these functions from "main.R" script
predictLoad <- function(load, avg.temp, avg.temp.yearly, start.dt, horizon, use.temp) {
  # have to call function for preprocessing of temp
  load.features <- createLoadFeatures(load, start.dt, 1)
  print(paste0("predictLoad",start.dt))
  # train model
  train.dt <- subMonths(start.dt, horizon)
  
  train.result <- trainLoadModel(load.features, avg.temp, train.dt, horizon)
  load.model <- train.result[["model"]]
  #gam.check(load.model)
  
  if (use.temp) {
    pred.temp.df <- avg.temp
  } else {
    print("doing this")
    pred.temp.df <- predictTemp(avg.temp, avg.temp.yearly, start.dt, 1) # should return seq temp diff + 2 month prediction 
  }
  # predict with model
  pred.feature.df <- assembleFeatures(load.features, pred.temp.df, start.dt, 1)
  saveRDS(pred.feature.df, file="pred_features.R")
  # otherwise (no flags set): an array of predictions is returned
  pred.fit <- predict.gam(load.model, pred.feature.df[, -(1:2)])
  err.measures <- pointErrorMeasures(pred.feature.df$Y, pred.fit)
  # add train error quantiles to point prediction
  pred.quantiles <- createPredQuantiles(pred.fit, train.result[["residuals"]])
  ### create several plots, one for every week
  plotPredictionQuantiles(pred.feature.df$TMS, pred.feature.df$Y, pred.fit, pred.quantiles, 4,
                          paste0(start.dt, " + 1 month in hours"), "load in MW", "Plot of Percentiles")
  return(list(quantiles=pred.quantiles, model=load.model, errors=err.measures))
}

### TODO
# - train for whole year (datastructures for 12 months)
# - validate on X horizon months in rolling fashion
# PARAMS:
# - avg.temp.by.hour: dataframe with average temperature over all weather stations by year and hour
# - horizon in months
# - use all years except first and last
crossValidation <- function(load, avg.temp, first.dt, last.dt, horizon) {
  load.features <- createLoadFeatures(load, start.dt, horizon)
  
  lub.last.dt <- ymd_hms(last.dt, tz="EST")
  lub.first.dt <- ymd_hms(first.dt, tz="EST")
  last.train.dt <- getCVStopDtByHorizon(last.dt, horizon)
  first.train.dt <- addMonth(addYears(lub.first.dt, 4))
  
  train.seq <- seq(from=first.train.dt, to=last.train.dt, by="month")
  # create validation sequence depending on train ts
  train.dt <- sample(train.seq, size=1)
  valid.dt <- getStopDtByHorizon(train.dt, horizon)
  train.dt.vec <- character(horizon)
  valid.dt.vec <- train.dt.vec
  rmse.vec <- numeric(horizon)
  mae.vec <- rmse.vec
  mape.vec <- rmse.vec
  for (i in 1:horizon) {
    # apply subDays(..., 1) to stop.dt in features ??? 
    load.model <- trainLoadModel(load.features, avg.temp, train.dt, horizon)
    valid.list <- validateLoadModel(load.features, load.yearly, avg.temp, valid.dt, horizon)
    train.dt <- addMonth(train.dt)
    valid.dt <- addMonth(train.dt)
    train.chr <- as.character(train.dt)
    valid.chr <- as.character(valid.dt)
    train.dt.vec[i] <- train.chr
    valid.dt.vec[i] <- valid.chr
    rmse_vec[i] <- valid.list[["point.errs"]][["RMSE"]]
    mae_vec[i] <- valid.list[["point.errs"]][["MAE"]]
    mape_vec[i] <- valid.list[["point.errs"]][["MAPE"]]
  }
  train.dt.vec[i+1] <- "valid.average"
  valid.dt.vec[i+1] <- "over whole period"
  rmse_vec[i+1] <- mean(rmse_vec)
  mae_vec[i+1] <- mean(mae_vec)
  mape_vec[i+1] <- mean(mape_vec)

  results.df <- data.frame(TRAINTMS=train.dt.vec, VALIDTMS=valid.dt.vec, RMSE=rmse.vec, MAE=mae.vec, MAPE=mape.vec, stringsAsFactors = FALSE)
  write.table(results.df, paste(paste("models/load/CV/", "results", sep=""), "csv", sep="."), quote=FALSE, row.names=FALSE, sep=",")
  # save variables
  
}
