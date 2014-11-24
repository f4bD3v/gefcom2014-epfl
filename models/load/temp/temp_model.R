if(!exists("errorMeasures", mode="function")) source("util/error_func.R")

### TODO: use getDirectFeatures & getCalcFeatures from load.model
getPredSeq <- function(temp_df, dt, horz) {
  index_dt <- replaceYear(dt, "2012")
  index <- which(temp_df$TMS==index_ts, arr.ind=TRUE)
  index_seq = seq.int(index+1, index+horz, 1)
  return(index_seq)
}

createTempFeatures <- function(avg.temp, start.dt, horizon, htype=2) {
  stop.dt <- getStopDtByHorizon(start.dt, horizon, htype)
  if (stop.dt > getLastDt()) {
    dt.seq.target <- as.POSIXct(seq(from=start.dt, to=stop.dt, by="hour"), tz="EST")
    tms <- as.POSIXct(c(as.character(avg.temp$TMS), as.character(dt.seq.target)), tz="EST")
    mtemp <- c(avg.temp$MTEMP, rep(NA, length(dt.seq.target)))
    hash <- hashDtYear(tms)
  } else {
    mtemp <- avg.temp$MTEMP
    tms <- avg.temp$TMS
    hash <- avg.temp$HASH
  }
  
  first.index <- which(hash==hashDtYear(start.dt))
  last.index <- length(hash) #which(hash==hashDtYear(stop.dt))
  mean <- rep(NA, (first.index-1))
  mdiff <- mean
  lagmin <- mean 
  lagmax <- mean 
  lagsd <- mean
  print(first.index)
  print(last.index)
  for(i in last.index:first.index) {
    lag.indices <- seq.int(i,1, -(365*24))[-1]  
    lags <- avg.temp[lag.indices, 2]
    mean <- c(mean, mean(lags))
    lagmax <- c(lagmax, max(lags))
    lagmin <- c(lagmin, min(lags))
    lagsd <- c(lagsd, sd(lags))
    mdiff <- c(mdiff, mean(diff(lags)))
  }
  
  print(length(mtemp))
  print(length(tms))
  
  hour <- createHourFeatures(tms) #as.factor(createHourFeatures(tms))
  month <- as.factor(createMonthFeatures(tms))
  
  tms.year.list <- split(tms, year(ymd(as.Date(tms))))
  
  # pass years to funciton --> as.numeric(year) --> leap_year , if yes divide by specific number of hours in leap year or standard year
  num.hours <- 365*24
  list.of.seqs <- lapply(tms.year.list, function(x) { return(seq(0,length(x)/num.hours,length.out=length(x))) })#, names(tms.year.list))
  time.of.year <- c(unlist(list.of.seqs))
  print(length(time.of.year))
  
  features <- data.frame(TMS=tms, MTEMP=mtemp, TOY=time.of.year, HOUR=hour, MONTH=month, LAGM=mean, LAGMD=mdiff, LAGMAX=lagmax, LAGMIN=lagmin, LAGSD=lagsd, HASH=hash)
  return(features)
}

getFeatures <- function(feature.df, start.dt, horizon, htype) {
  stop.dt <- getStopDtByHorizon(start.dt, horizon, htype)
  dt.seq.target <- seq(from=start.dt, to=stop.dt, by="hour")
  
  nrows <- nrow(feature.df)
  if (stop.dt > getLastDt()) {
    target <- rep(NA, length(dt.seq.target)) 
    index.seq.target <- seq(nrows-length(dt.seq.target)+1, nrows, 1)
  } else {
    index.seq.target <- calcSeqByIndex(nrows, getColIndex(feature.df$HASH, start.dt, stop.dt))
    target <- feature.df$MTEMP[index.seq.target]
  }
  
  #index.seq.days7.before <- calcSeqByIndex(nrows, getColIndex(feature.df$HASH, subDays(start.dt, 7), subHours(start.dt, 1)))
  #days7.before <- feature.df$MTEMP[index.seq.days7.before]
  
  if(htype == 0) offset <- horizon*24 else if(htype == 1) offset <- horizon*7*24 else offset <- 5*7*24

  days.lag.seq <- index.seq.target - offset
  weeks52.lag.seq <- index.seq.target - (365*24)
  
  days.lag <- feature.df$MTEMP[days.lag.seq]
  weeks52.lag <- feature.df$MTEMP[weeks52.lag.seq]
  
  time.of.year <- feature.df$TOY[index.seq.target] 
  hour <- feature.df$HOUR[index.seq.target] 
  month <- feature.df$MONTH[index.seq.target] 
  
  mean <- feature.df$LAGM[index.seq.target]
  lagmax <- feature.df$LAGMAX[index.seq.target]
  lagmin <- feature.df$LAGMIN[index.seq.target]
  lagsd <- feature.df$LAGSD[index.seq.target]
  mdiff <- feature.df$LAGMD[index.seq.target]

  #MIN7DB=rep(min(days7.before), length(index.seq.target))
  #MAX7DB=rep(max(days7.before), length(index.seq.target))
  feature.list <- list(TMS=dt.seq.target, Y=target, DLAG=days.lag, WLAG52=weeks52.lag, TOY=time.of.year, MONTH=month, HOUR=hour, LAGM=mean, LAGMD=mdiff, LAGMAX=lagmax, LAGMIN=lagmin, LAGSD=lagsd)
  
  features <- do.call(cbind.data.frame, feature.list)
  return(features)
 }  

assembleTempFeatures <- function(avg.temp, avg.temp.yearly, start.dt, horizon) {
  ### MAKE SURE NOT TO EXCEED LAST DAY OF MONTH !!! OTHERWISE NA IS PRODUCED
  #if(nchar(as.character(start.dt)) == 10) {
  #  start.dt <- as.POSIXct(addTms(start.dt), "Y:"
  #}
  print(paste0("assembleTempFeatures",start.dt))
  direct.features <- getDirectFeatures(avg.temp, start.dt, horizon)
  # offset 0, bc load df starts at 2001 with NAs
  calc.features <- getRowFeatures(avg.temp.yearly, start.dt, horizon, 0)
  # drop date and time features
  direct.features.df <- do.call(cbind.data.frame, direct.features)
  calc.features.df <- do.call(cbind.data.frame, calc.features)
  feature.df <- cbind(direct.features.df, calc.features.df)
  return(feature.df)
}

#TODO: hard code formula, because some of the parameters should be grouped with smoothing functions
### TODO: devise different formulas to be tested in crossvalidation

# use real temp
# average over 5 years

# lm
getTempLmFormula <- function(target.var, model.vars) {
  # This creates the appropriate string:
  # "y ~ x1 + x2 + x3"
  str.formula <- paste(target.var, paste(model.vars, collapse=" + ", sep=" ~ "))
  #str.formula <- paste0(target.var, " ~ ", "RMEAN + RMAX + RMIN + LASTY + HOUR + MONTH + HMEAN + HMIN + HMAX")
  #str.formula <- paste0(target.var, " ~ ", "RMEAN + LASTY + HOUR + MONTH + HMEAN + HINC")
  str.formula <- paste0(target.var, " ~ ", "RMEAN + HMEAN + LASTY + HOUR + MONTH")
  # y ~ x1 + x2 + x3
  return(as.formula(str.formula))
}


getTempFormula <- function(target.var, model.vars) {
  # This creates the appropriate string:
  # "y ~ x1 + x2 + x3"
  str.formula <- paste(target.var, paste(model.vars, collapse=" + "), sep=" ~ ")
  print(str.formula)
  # y ~ x1 + x2 + x3
  return(as.formula(str.formula))
}

trainTempLmModel <- function(avg.temp, avg.temp.yearly, train.dt, horizon) {
  feature.df <- assembleTempFeatures(avg.temp, avg.temp.yearly, train.dt, horizon)
  colnames <- colnames(feature.df)[-(1:2)]
  temp.model <- lm(getTempLmFormula("Y", colnames), data=feature.df)
  saveRDS(temp.model, file="tempModel.rds")
  residuals <- feature.df$Y - temp.model$fitted.values
  plotTraining(feature.df$TMS, feature.df$Y, temp.model$fitted.values, mean(feature.df$Y)+residuals, xlabel=paste(as.character(train.dt), "2 months in hours", sep=" +"),
               ylabel="Temperature in Fahrenheit", title="Temperature Model Training")
  return(temp.model)
}

trainTempModel <- function(feature.df, formula, train.dt) {
  colnames <- colnames(feature.df)[-(1:2)]
  #getTempFormula("Y", colnames),
  print(colnames)
  formula <- paste0("Y ~ ", formula)
  print(formula)
  temp.model <- gam(as.formula(formula), family=gaussian(), data=feature.df)
  residuals <- feature.df$Y - temp.model$fitted.values
  #plotTraining(feature.df$TMS, feature.df$Y, temp.model$fitted.values, mean(feature.df$Y)+residuals, xlabel=paste(as.character(train.dt), "1 month in hours", sep=" +"),
  #             ylabel="Temperature in Fahrenheit", title="Temperature Model Training")
  return(temp.model)
}

validateTempModel <- function(trained.model, pred.feature.df, test.dt, model.formula) {
  valid.Y <- pred.feature.df$Y
  valid.fit <- predict.gam(trained.model, pred.feature.df[, -1])
  err.measures <- pointErrorMeasures(valid.Y, valid.fit)
  plotTraining(pred.feature.df$TMS, pred.feature.df$Y, valid.fit, 0, xlabel=paste(as.character(test.dt), "1 month in hours", sep=" +"),
               ylabel="Temperature in Fahrenheit", title=paste0("Temperature Model Validation, Model: ", model.formula))
  return(list(fit=valid.fit, point.errs=err.measures))
}

predictTemp <- function(temp.features, start.dt, horizon, htype) {
  train.dt <- subMonths(start.dt, horizon)
  train.features <- getFeatures(temp.features, train.dt, horizon, htype)
  temp.model <- trainTempModel(train.features, "s(DLAG, by=TOY, k=24) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)", train.dt)
  # predict with model
  # TODO: pass date directly at later stage
  pred.feature.df <- getFeatures(temp.features, start.dt, 1, 2)
  # otherwise (no flags set): an array of predictions is returned
  pred.fit <- predict.gam(temp.model, pred.feature.df[, -(1:2)])
  plotValidation(pred.fit, "", "" , "Plot of Temperature Predicition for two monts")
  stop.dt <- getStopDtByHorizon(start.dt, 1, 2)
  pred.seq <- seq(start.dt, stop.dt, by="hour")
  fit <- data.frame(TMS=pred.seq, MTEMP=pred.fit, HASH=hashDtYear(pred.seq))
  #colnames(fit) <- colnames(avg.temp)
  index1 <- which(avg.temp$HASH==hashDtYear(pred.seq)[1], arr.ind=TRUE)
  index2 <- which(avg.temp$HASH==hashDtYear(pred.seq)[length(pred.seq)], arr.ind=TRUE)
  target <- avg.temp[index1:index2, 2]
  plotTraining(pred.seq, target, fit[,2], 0, " ", " ", " ")
  if (start.dt < getLastDt()) {
    index <- which(avg.temp$HASH == hashDtYear(start.dt), arr.ind = TRUE)  
    avg.temp <- avg.temp[-c(index:nrow(avg.temp)),]
  }
  temp.df <- rbind(avg.temp, fit)
  saveRDS(temp.df, file="temp_fit.rds")
  return(temp.df)
}
