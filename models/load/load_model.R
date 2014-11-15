require(mgcv)

# ATM passed temp features yearyl
getTempFeatures <- function(avg.temp.df, train.dt, horizon) {
  stop.dt <- getStopDtByHorizon(train.dt, horizon)
  index.seq <- calcSeqByIndex(nrow(avg.temp.df), getColIndex(avg.temp.df$HASH, train.dt, stop.dt))
  print(index.seq)
  print(avg.temp.df[1, ])
  data.df <- avg.temp.df$MTEMP
  CTEMP <- data.df[index.seq] ### FEED TEMPERATURE OF PREVIOUS MONTH, previous year (365*24)
  for (i in 1:length(index.seq)) {
    index <- index.seq[i]
    print(index)
    #index.seq.last7d <- seq(index-720, index-1, 1)
    #mean.last7d <- mean(data.df[index.seq.last7d])
    index.seq.last24h <- seq(index-24, index-1, 1)
    max.last24h <- max(data.df[index.seq.last24h])
    min.last24h <- min(data.df[index.seq.last24h])
    temp_24h <- data.df[index-24]
    temp_48h <- data.df[index-48]
    temp_2h <- data.df[index-2]
    temp_1h <- data.df[index-1]
    # avg temp next 8 hours
    ### TODO: Always predict temperature for next 2 months!
    #index.seq.next8h <- seq(index+1, index+8, 1)
    #avg.temp.next8h <- mean(data.df[index.seq.next8h])
    #MTL7D=mean.last7d, 
    feature.row <- cbind(MAXT24H=max.last24h, MINT24H=min.last24h, TM24H=temp_24h, TM48H=temp_48h, TM2H=temp_2h, TM1H=temp_1h)
    if (i == 1) features <- feature.row else features <- rbind(features, feature.row)
  }
  temp.features <- cbind(CTEMP, features)
  return(temp.features)
}

createLoadFeatures <- function(load.df, start.dt, horizon) {
  stop.dt <- getStopDtByHorizon(start.dt, horizon)
  if (stop.dt > getLastDt()) {
    dt.seq.target <- as.POSIXct(seq(from=start.dt, to=stop.dt, by="hour"), tz="EST")
    tms <- as.POSIXct(c(as.character(load.df$TMS), as.character(dt.seq.target)), tz="EST")
    load <- c(load.df$LOAD, rep(NA, length(dt.seq.target)))
    hash <- hashDtYear(tms)
  } else {
    load <- load.df$LOAD
    tms <- load.df$TMS
    hash <- load.df$HASH
  }
  
  daytype <- as.factor(createMaxDayFeatures(tms))
  #month <- createMonthFeatures(tms)
  hour <- createHourFeatures(tms)
  
  tms.year.list <- split(tms, year(ymd(as.Date(tms))))
  # pass years to funciton --> as.numeric(year) --> leap_year , if yes divide by specific number of hours in leap year or standard year
  num.hours <- 365*24
  list.of.seqs <- lapply(tms.year.list, function(x) { return(seq(0,length(x)/num.hours,length.out=length(x))) })#, names(tms.year.list))
  time.of.year <- c(unlist(list.of.seqs))
  write.table(time.of.year, paste(paste("models/load/", "timeofyear", sep=""), "csv", sep="."), quote=FALSE, row.names=FALSE, sep=",")
  
  features <- data.frame(TMS=tms, LOAD=load, TOY=time.of.year, DAYT=daytype, HOUR=hour, HASH=hash)
  return(features)
}

getLoadFeatures <- function(data.df, start.dt, horizon, htype) {
  print(paste0("getLoadFeatures",start.dt))
  
  stop.dt <- getStopDtByHorizon(start.dt, horizon, htype)
  dt.seq.target <- seq(from=start.dt, to=stop.dt, by="hour")
  
  nrows <- nrow(data.df)
  if (stop.dt > getLastDt()) {
    target <- rep(NA, length(dt.seq.target)) 
    index.seq.target <- seq(nrows-length(dt.seq.target)+1, nrows, 1)
  } else {
    index.seq.target <- calcSeqByIndex(nrows, getColIndex(data.df$HASH, start.dt, stop.dt))
    target <- data.df$LOAD[index.seq.target]
    print(target)
  }
  
  offset <- 0
  if(htype == 0) offset <- horizon * 24 else if(htype == 1) offset <- horizon * 7 * 24 else offset <- 5 * 7 * 24
  days.lag.seq <- index.seq.target - offset
  
  weeks52.lag.seq <- index.seq.target - (52*7*24)
  
  days.lag <- data.df$LOAD[days.lag.seq]
  weeks52.lag <- data.df$LOAD[weeks52.lag.seq]
  
  time.of.year <- data.df$TOY[index.seq.target] 
  daytype <- data.df$DAYT[index.seq.target] 
  hour <- data.df$HOUR[index.seq.target] 

  return(list(TMS=dt.seq.target, Y=target, DLAG=days.lag, WLAG52=weeks52.lag, TOY=time.of.year, DAYT=daytype, HOUR=hour))
}  

assembleFeatures <- function(load.features, avg.temp, start.dt, horizon, htype) {
  # ofset 0 instead of 4 bc load df starts at 2001 with NAs
  temp.features <- getTempFeatures(avg.temp, start.dt, horizon)
  load.feature.list <- getLoadFeatures(load.features, start.dt, horizon, htype)
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
  saveRDS(feature.df, file="trainData.rds")
  residuals <- feature.df$Y - load.model$fitted.values
  plotTraining(feature.df$TMS, feature.df$Y, load.model$fitted.values, mean(feature.df$Y)+residuals, xlabel=paste(as.character(train.dt), "1 months in hours", sep=" +"),
               ylabel="Load in MW", title="Load Model Training")
  return(list(model=load.model, residuals=residuals))
}

trainLoadModel <- function(feature.df, train.dt) {
  colnames <- colnames(feature.df)[-(1:2)]
  load.model <- gam(getLoadFormula("Y", colnames), family=gaussian(), data=feature.df)
  saveRDS(feature.df, file="trainData.rds")
  residuals <- feature.df$Y - load.model$fitted.values
  plotTraining(feature.df$TMS, feature.df$Y, load.model$fitted.values, mean(feature.df$Y)+residuals, xlabel=paste(as.character(train.dt), "1 months in hours", sep=" +"),
               ylabel="Load in MW", title="Load Model Training")
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