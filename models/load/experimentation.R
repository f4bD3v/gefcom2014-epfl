require(xtsExtra)
require(forecast)
require(timeDate)
require(stats)
require(manipulate) # interactive plots
require(lubridate)
require(reshape)
require(reshape2)
require(meboot)
require(matrixStats) # for rowSds
require(TSdist)
require(stringr)
#require(lm) # linear methods
require(MASS)
require(arm) # plot with confidence intervals
require(splines) # needed for estimating regression splines

cat("\014") # to clear console
source("util/error_func.R")

first_dt <- as.POSIXct("01/01/2001 01:00", format="%m/%d/%Y %H:%M", tz="EST")
last_dt <- as.POSIXct("07/01/2011 00:00", format="%m/%d/%Y %H:%M", tz="EST")

holidays_2005 <- c("12/26/2005", "11/24/2005", "11/11/2005", "10/10/2005", "09/05/2005", "07/04/2005", "05/30/2005", "02/21/2005", "01/17/2005", "12/31/2004")
holidays_2006 <- c("12/25/2006", "11/23/2006", "11/10/2006", "10/09/2006", "09/04/2006", "07/04/2006", "05/29/2006", "02/20/2006", "01/16/2006", "01/02/2006")
holidays_2007 <- c("12/25/2007", "11/22/2007", "11/12/2007", "10/08/2007", "09/03/2007", "07/04/2007", "05/28/2007", "02/19/2007", "01/15/2007", "01/01/2007")
holidays_2008 <- c("12/25/2008", "11/27/2008", "11/11/2008", "10/13/2008", "09/01/2008", "07/04/2008", "05/26/2008", "02/18/2008", "01/21/2008", "01/01/2008")
holidays_2009 <- c("12/25/2009", "11/26/2009", "11/11/2009", "10/12/2009", "09/07/2009", "07/03/2009", "05/25/2009", "02/16/2009", "01/19/2009", "01/01/2009")
holidays_2010 <- c("12/24/2010", "11/25/2010", "11/11/2010", "10/11/2010", "09/06/2010", "07/05/2010", "05/31/2010", "02/15/2010", "01/18/2010", "01/01/2010")
holidays_2011 <- c("12/26/2011", "11/24/2011", "11/11/2011", "10/10/2011", "09/05/2011", "07/04/2011", "05/30/2011", "02/21/2011", "01/17/2011", "12/31/2010")
holidays <- c(holidays_2005, holidays_2006, holidays_2007, holidays_2008, holidays_2009, holidays_2010, holidays_2011)
holidays <- as.Date(holidays, format="%m/%d/%Y")

loadCSVs <- function(path) {
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  assign('train_data', do.call(rbind, tables))
  return(train_data)
}

#merge.all <- function(bywhat, mylist) {
  # by - column which to join by
  # all - append all the non-matching cases
#  return (Reduce(function(x,y) { merge(x, y, by=bywhat, all=TRUE)}, mylist))
#}

createTrainDF <- function(train_data, first_dt, last_dt) {
  datetime_seq <- seq(from=first_dt, to=last_dt, by="hour")
  #datetime_seq[1]
  #datetime_seq[length(date_seq)]
  train_data$TIMESTAMP <- datetime_seq
  return(train_data)
}

reduceToTempDF <- function(train_data) {
  drops <- c("ZONEID","LOAD")
  temp_data <- train_data[!(names(train_data) %in% drops)]
  return(temp_data)
}

reduceToLoadDF <- function(train_data) {
  temp_data <- train_data[, c("TIMESTAMP", "LOAD")]  
  return(temp_data)
}

listTempSeries <- function(temp_data) {
  temp_list <- list()
  for(i in 1:(ncol(temp_data)-1)) {
    col <- paste("w",i, sep="")
    temp_list[[i]] <- temp_data[c("TIMESTAMP", col)]
  }
  return(temp_list)
}

listSeriesByYear <- function(temp_list, first_dt, last_dt) {  
  first_date <- as.character(as.Date(first_dt))
  last_date <- as.character(as.Date(last_dt))

  temp_by_year <- list()
  for(i in 1:length(temp_list)) {
    temp_year_list <- split(temp_list[[i]], year(ymd(as.Date(temp_list[[i]]$TIMESTAMP))))
    
    for(j in 1:length(temp_year_list)) {
      t_col <- paste(paste(paste("t", i, sep=""), "_", sep=""), j, sep="")
      ze_names <- c("TIMESTAMP", t_col)
      colnames(temp_year_list[[j]]) <- ze_names
      curr_df <- unique(temp_year_list[[j]])
      # remove year from timestamp to allow for merge on month-day time
      temp_year_list[[j]][, "TIMESTAMP"] <- substr(curr_df[, "TIMESTAMP"], 6, 19)
    }
    temp_by_year[[i]] <- temp_year_list
  }
  return(temp_by_year)
}

loadSeriesByYear <- function(load_series) {
  load_year_list <- split(load_series, year(ymd(as.Date(load_series$TIMESTAMP))))
  names_list <- names(load_year_list)
  for(i in 1:length(load_year_list)) {
    load_col <- paste("LOAD", names_list[i], sep="_")
    ze_names <- c("TIMESTAMP", load_col)  
    colnames(load_year_list[[i]]) <- ze_names
    load_year_list[[i]][, "TIMESTAMP"] <- substr(load_year_list[[i]][, "TIMESTAMP"], 6, 19)
  }
  load_df <- Reduce(merge.all, load_year_list)
  tms <- load_df[,"TIMESTAMP"]
  # add leap year for plotting
  tms <- paste("2012-", tms, sep="")
  # format chr as POSIXct
  datets <- as.POSIXct(tms, format="%Y-%m-%d %H:%M:%S")
  load_df <- load_df[,-1]
  load_df <- transform(load_df, TMS=datets, MEAN=rowMeans(load_df, na.rm = TRUE), SD=apply(load_df, 1, sd, na.rm = TRUE))
  return(load_df)
}

merge.all <- function(x, y) {  merge(x, y, all=TRUE, by="TIMESTAMP") }

mergeSeriesByHour <- function(temp_by_year) {
  merged_by_hour <- list()
  for(i in 1:length(temp_by_year)) {
    merged_by_hour[[i]] <- Reduce(merge.all, temp_by_year[[i]])
  }
  return(merged_by_hour)
}

listMTempByHour <- function(merged_by_hour) {
  mtemp_by_hour <- list()
  for(i in 1:length(merged_by_hour)) {
    curr_df <- merged_by_hour[[i]]
    tms <- curr_df[,"TIMESTAMP"]
    # add leap year for plotting
    tms <- paste("2012-", tms, sep="")
    # format chr as POSIXct
    datets <- as.POSIXct(tms, format="%Y-%m-%d %H:%M:%S")
    curr_df <- curr_df[,-1]
    mtemp_by_hour[[i]] <- transform(curr_df, TMS=datets, MEAN=rowMeans(curr_df[,-1], na.rm = TRUE), SD=apply(curr_df[,-1], 1, sd, na.rm = TRUE))
  }
  return(mtemp_by_hour)
}

first_dt <- as.POSIXct("01/01/2001 01:00", format="%m/%d/%Y %H:%M", tz="EST")
last_dt <- as.POSIXct("07/01/2011 00:00", format="%m/%d/%Y %H:%M", tz="EST")

beforelast_dt <- as.POSIXct("06/01/2011 00:00", format="%m/%d/%Y %H:%M", tz="EST")

### LOAD CSV data % ADJUST DATES for sequence ###
path <- "data/load/train"
train_df <- createTrainDF(loadCSVs(path), first_dt, last_dt)

### FOR PLOTTING ###

# How to convert Fahrenheit temperatures to Celsius:
# -Subtract 32 degrees to adjust for the offset in the Fahrenheit scale.
# -Multiply the result by 5/9.
createTempHistos <- function(temp_series, filepath) {
  for(i in 1:length(temp_series)) {
    currt_series <- temp_series[[i]][,2]
    celsius_series <- (currt_series-32)*(5/9)
    png(filename=paste(filepath, paste(i, "png", sep="."), sep="/w"))
    hist(celsius_series, col="grey", freq=FALSE, breaks=10, xlab="Temperature in Degrees Celsius", main=paste("Histogram Density of Values for Temperature Series w",i,sep=""))
    # to keep seeing the plot use: dev.copy(png,'myplot.png')
    dev.off()
  }
}

### PLOT LOAD ###
load_series <- na.omit(reduceToLoadDF(train_df))
hist(load_series[,2], col="grey", freq=FALSE,  xlab="Load in MW", main="Histogram Density of Load Values")
plot_data <- xts(load_series[,2], load_series[,1])
axTicksByTime(plot_data, ticks.on="Year")
plot(plot_data, major.ticks="Year", minor.ticks=FALSE, main="Historical load series for prediction purposes")

load_by_year <- loadSeriesByYear(load_series)

# TEMPERATURE
temp_series <- listTempSeries(reduceToTempDF(train_df))
createTempHistos(temp_series, "plots/hist")

merged_by_hour <- mergeSeriesByHour(listSeriesByYear(temp_series, first_dt, last_dt))
tempStats_by_hour <- listMTempByHour(merged_by_hour)
sapply(1:length(tempStats_by_hour), function(x) write.table(tempStats_by_hour[[x]], paste(paste("data/load/train/temp/temp",x, sep="_"), ".csv", sep=".")))

path1 <- paste(path, "/temp", sep="")
tempStats_by_hour <- loadCSVs(path1)

mtemp_one <- tempStats_by_hour[[1]]
  
#plot(mtemp_test[,2][0:2190] ~ datets[0:2190], type="l")

mtemp_daily_test <- aggregate(mtemp_one$m_temp,by=list(substr(mtemp_one$tms, 1, 10)), FUN=mean)

### FOR CROSS-CORRELATION ###

temp_series_df <- Reduce(merge.all, temp_series)
#ccf(temp_series_df[,2], temp_series_df[,3])

### TEMPERATURE FORECASTING/SAMPLING ###

addMeanDiff <- function(tempStats_by_hour, i, last_dt, div) {
  currt_series <- tempStats_by_hour[[i]]
  last_ts <- paste("2012-", substr(last_dt, 6, 19), sep="")
  index <- which(currt_series$TMS==last_ts, arr.ind=TRUE)
  # split by index by removing rows, 1:24*30
  relev_series <- currt_series[-(1:index-1),]
  relev_series <- relev_series[1:721, ]
  dm <- diff(relev_series[, "MEAN"], lag = 1, differences = 1)
  rand_mean_pred <- diffinv(dm, xi=relev_series[1,"MEAN"])
  len <- nrow(relev_series)-1
  sd <- relev_series[1:len, "SD"]
  sdma <- rollmean(sd, k=168)/div
  for(i in 1:len) {
    noise <- rnorm(1, 0, sd=sdma[i])
    rand_mean_pred[i] <- rand_mean_pred[i]+noise
  }
  horizon <- 720
  ly2 <- currt_series[-(1:index),]
  ly1 <- currt_series[-(1:(index-horizon)),]
  df2 <- data.frame(LY=ly2[1:horizon, 1], LM=rep(0,horizon), MEAN_RAND_PRED=rand_mean_pred[-1])
  df1 <- data.frame(LY=ly1[1:horizon, 1], LM=ly1[1:horizon, "MEAN"], MEAN_RAND_PRED=rep(0,horizon))
  return(rbind(df1, df2))
}

## ME BOOTSTRAP NOT USEFUL HERE, TOO LITTLE VARIATION
createReplicaYears <- function() {}

# APPLY MOVING AVERAGE TO THE STANDARD DEVIATION, k=24 for DAILY, k=168 for WEEKLY
#d1 <- rollmean(data$SD, k=168)/10

# divisor for standard deviation, range: 10-15
matchSeries <- function(tempStats_by_hour, i, curr_ts, last_ts, div) {
  temp_df <- tempStats_by_hour[[i]]
  if(nchar(as.character(curr_ts)) == 10) {
    curr_ts <- ymd_hms(paste(curr_ts, "00:00:00", sep=" "), tz="EST")
  } else {
    curr_ts <- ymd_hms(curr_ts, tz="EST")
  }
  check_ts <- ymd_hms(curr_ts, tz="EST") %m+% months(1)
  if(check_ts >= last_ts || year(check_ts) < "2005") {
    return(-1)
  }
  index_ts <- paste("2012-", substr(curr_ts, 6, 19), sep="") 
  index <- which(temp_df$TMS==index_ts, arr.ind=TRUE)
  horizon <- 720
  if(index < horizon) {
    print("Matching algorithm not mature enough to treat dates between the years.")
    return(-1)
  }
  relev_df <- temp_df[-(1:(index-168)), ]
  # Loop over last ten years of data for this temperature series i
  to_match <- relev_df[1:168, 11]
  track_j <- 1
  track_d <- 9999
  for(j in 1:(ncol(currt_series)-4)) {
    curry <- relev_df[1:168, j]
    #dist <- tsDistances(to_match, curry, distance="dissimapprox")
    # increment distance when more than 10 degrees fahrenheit of difference
    edr  <- tsDistances(to_match, curry, distance="edr", epsilon=10)
    #dist2 <- tsDistances(to_match, curry, distance="pearsoncorrelation")
    if(edr < track_d) track_j <- j
  }
  to_plot1 <- relev_df[1:(168), c(track_j, 11)]
  d <- diff(relev_df[168:(168+horizon), track_j], lag=1)
  sd <- relev_df[168:(168+horizon), "SD"]
  sdma <- rollmean(sd, k=168)/div # 672 for 4 weeks
  add_temp <- diffinv(d, xi=relev_df[168, 11])
  for(i in 1:length(sdma)) {
    noise <- rnorm(1, 0, sd=sdma[i])
    add_temp[i] <- add_temp[i]+noise
  }
  ### SAVE HIST  
  #hist(celsius_series, col="grey", freq=FALSE, xlab="Temperature in Degrees Celsius", main=paste("Histogram Density of Values for Temperature Series w",i,sep=""))
  to_plot2 <- cbind(relev_df[169:(168+horizon), track_j], add_temp[-1])
  return(list(to_plot1, to_plot2))
}

# GENERATE DATE SEQUENCE BETWEEN FIRST AND LAST, RANDOMLY PICK ELEMENT AND THROW IT TO FUNC

createPredDF <- function(tempStats_by_hour, i, curr_ts, last_ts) {
  temp_df <- tempStats_by_hour[[i]]
  horizon <- 720
  print(paste("Current Timestamp for creating Prediction DF", curr_ts, sep=" "))

  ### MAKE SURE NOT TO EXCEED LAST DAY OF MONTH !!! OTHERWISE NA IS PRODUCED
  check_ts <- curr_ts %m+% months(1)
  print(paste("Threshold Timestamp for creating Prediction DF", check_ts, sep=" "))
  if(check_ts > last_ts || year(check_ts) < 2005) {
    return(-1)
  }
  if(nchar(as.character(curr_ts)) == 10) {
    curr_ts <- paste(curr_ts, "00:00:00 EST", sep=" ")
  }
  print(paste("Current Timestamp for creating Prediction DF", curr_ts, sep=" "))
  index_ts <- paste("2012", str_sub(curr_ts, start=5, end=-1), sep="")
  index <- which(temp_df$TMS==index_ts, arr.ind=TRUE)
  print(temp_df$TMS[index])
  print(paste("DF index:", index, sep=" "))
  # translate year to col
  curr_ts <- ymd_hms(curr_ts, tz="EST")
  y <- as.numeric(str_sub(year(curr_ts), start=-1))
  if(year(curr_ts) > 2009) {
    y <- as.numeric(str_sub(year(curr_ts), start=-2))
  }
  print(paste("Year:", y, sep=" "))
  index_seq = seq.int(index+1, index+horizon, 1)
  hist_temp_df <- temp_df[index_seq, 1:(y-1)]
  # get daily mean over the last years
  hist_mean <- apply(hist_temp_df, 1, mean, na.rm = TRUE)
  # get yearly increase over the last years
  sub_df <- temp_df[index_seq, 1:(y-1)]
  diff_df <- apply(sub_df, 1, diff, na.rm = TRUE)
  inc_mean <- apply(diff_df, 2, mean, na.rm = TRUE)
  last_year <- temp_df[index_seq, (y-1)]
  # next: expontential smoothing or average of recent temperature values (7 days or 3 months?) 
  days <- 7
  range <- 7*24
  # easier solution ! if(month(curr_ts) == 1 && day(curr_ts))
  if(index < horizon) {
    recent_temp <- rbind(temp_df[1:index, y], temp_df[nrow(temp_df)-((horizon-1-index):0) , (y-1)])
  } else {
    recent_temp <- temp_df[(index-range):index, y]
  }
  recent_mean <- mean(recent_temp)
  ret_df <- data.frame(Y=temp_df[index_seq,y], REC=rep(recent_mean, horizon), LYD=last_year, HIS=hist_mean, INC=inc_mean)
  #ret_df <- data.frame(do.call(cbind, list(sub_df[,y] ,recent_mean, hist_mean, inc_mean)))
  return(ret_df)
}
  
createLoadPredDF <- function(load_by_hour, temp_pred, curr_ts, last_ts) {
    print(load_by_hour)
    horizon <- 720 
    print(paste("Current Timestamp for creating LOAD Prediction DF", curr_ts, sep=" "))
    if(nchar(as.character(curr_ts)) == 10) {
      curr_ts <- ymd_hms(paste(curr_ts, "00:00:00", sep=" "), tz="EST")
    } else {
      curr_ts <- ymd_hms(curr_ts, tz="EST")
    }
    print(paste("Current Timestamp for creating LOAD Prediction DF", curr_ts, sep=" "))
    ### MAKE SURE NOT TO EXCEED LAST DAY OF MONTH !!! OTHERWISE NA IS PRODUCED
    check_ts <- curr_ts %m+% months(1)
    print(paste("do this:", last_ts, sep=" "))
    print(paste("Threshold Timestamp for creating Prediction DF", check_ts, sep=" "))
    if(check_ts > last_ts || year(check_ts) < 2005) {
      return(-1)
    }
    print(curr_ts)
    index_ts <- paste("2012", str_sub(curr_ts, start=5, end=-1), sep="")
    index <- which(load_by_hour$TMS==index_ts, arr.ind=TRUE)
    print(paste("DF index:", index, sep=" "))
    # translate year to col
    y <- as.numeric(str_sub(year(curr_ts), start=-1))
    if(year(curr_ts) > 2009) {
      y <- as.numeric(str_sub(year(curr_ts), start=-2))
    }
    print(paste("Year:", y, sep=" "))
    y <- y-4
    index_seq = seq.int(index+1, index+horizon, 1)
    hist_load_df <- load_by_hour[index_seq, 1:(y-1)]
    # get daily mean over the last years
    hist_mean <- apply(hist_load_df, 1, mean, na.rm = TRUE)
    # get yearly increase over the last years
    sub_df <- load_by_hour[index_seq, 1:(y-1)]
    diff_df <- apply(sub_df, 1, diff, na.rm = TRUE)
    inc_mean <- apply(diff_df, 2, mean, na.rm = TRUE)
    last_year <- load_by_hour[index_seq, (y-1)]
    # next: expontential smoothing or average of recent temperature values (7 days or 3 months?) 
    days <- 7
    range <- 7*24
    # easier solution ! if(month(curr_ts) == 1 && day(curr_ts))
    if(index < horizon) {
      recent_load <- rbind(load_by_hour[1:index, y], temp_df[nrow(load_by_hour)-((horizon-1-index):0) , (y-1)])
    } else {
      recent_load <- load_by_hour[(index-range):index, y]
    }
    recent_mean <- mean(recent_load)
    ## in future: compute SD extra
    std <- load_by_year[index_seq, "SD"]
    ret_df <- data.frame(Y=load_by_hour[index_seq, y], REC=rep(recent_mean, horizon), LYD=last_year, HIS=hist_mean, INC=inc_mean, SD=std, TPRED=temp_pred)
    #ret_df <- data.frame(do.call(cbind, list(sub_df[,y] ,recent_mean, hist_mean, inc_mean)))
    return(ret_df)
}
  
errorMeasures <- function(target, fit) {
  rmse <- sqrt( mean((target-fit)^2 , na.rm = TRUE) )
  mae <- mean( abs(target-fit), na.rm = TRUE )
  mape <- mean( abs(target-fit)/target, na.rm = TRUE )
  return(list(RMSE=rmse, MAE=mae, MAPE=mape))
}

# create random matrix: myframe <- data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))

predictLoad <- function(load_by_hour, tempStats_by_hour, last_ts) {
  lub_last_ts <- ymd(last_ts, tz="EST")
  train_ts <- lub_last_ts - years(1)# + hours(1)
  temp_train_df <- createPredDF(tempStats_by_hour, 1, train_ts, lub_last_ts)
  temp_model <- lm(formula=Y~REC+LYD+HIS+INC, data=temp_train_df) #, lambda=0.1) #, lambda=seq(0,0.05, 0.001)) #no subset
  ### Train Target
  temp_train <- temp_train_df[, "Y"]
  last_ts <- lub_last_ts + years(1)
  print(paste("do this:", last_ts, sep=" "))
  load_train_df <- createLoadPredDF(load_by_hour, temp_train, train_ts, lub_last_ts)
  # SD
  load_model <- lm(formula=Y~REC+LYD+HIS+INC+TPRED, data=load_train_df) #, lambda=0.1) #, lambda=seq(0,0.05, 0.001)) #no subset
  temp_pred_df <- createPredDF(tempStats_by_hour, 1, lub_last_ts, last_ts)
  temp_pred <- predict(temp_model, temp_pred_df[, -1], level=0.95, interval="prediction")
  print(head(temp_pred))
  plot(temp_pred_df[, "Y"], type="l", col="blue")
  lines(temp_pred[, "fit"], type="l", col="red")
  err <- errorMeasures(temp_pred_df[, "Y"], temp_pred[, "fit"])
  print(err[["MAPE"]])
  load_pred_df <- createLoadPredDF(load_by_hour, temp_pred[, "fit"], lub_last_ts, last_ts)
  load_pred <- predict(load_model, load_pred_df[, -1], level=0.95, interval="prediction")
  print(load_pred)
  plot(load_pred_df[, "Y"], type="l", col="blue")
  lines(load_pred[, "fit"], type="l", col="red")
  err <- errorMeasures(load_pred_df[, "Y"], load_pred[, "fit"])
  print(err)
  horizon <- length(load_pred[, "fit"])
  qf <- matrix(0, nrow=99, ncol=horizon)
  m <- load_pred[, "fit"]
  s <- (load_pred[,"upr"]-load_pred[,"lwr"])/1.96/2
  for(h in 1:horizon)
  qf[,h] <- qnorm((1:99)/100, m[h], s[h])
  qf <- t(qf)
  pinball <- pinball(qf, load_pred_df[, "Y"])
  print(pinball)
  return(qf)
}

qf <- predictLoad(load_by_year, tempStats_by_hour, addHours(beforelast_dt, 1))

# write.table(qf, file = "L9submission_raw.csv", sep = ",") 
# 
# # use ME Bootstrap here at a later stage
# # http://stats.stackexchange.com/questions/17932/calculating-forecast-error-with-time-series-cross-validation
# crossValidation <- function(tempStats_by_hour, first_ts, last_ts) {
#   lub_last_ts <- ymd(last_ts, tz="EST")
#   lub_first_ts <- ymd_hms(first_ts, tz="EST")
#   last_CV_ts <- lub_last_ts - years(1)
#   first_CV_ts <- lub_first_ts + years(3)
#   # to allow for validation the training data should stop one year before last_dt
#   train_seq <- seq(from=first_CV_ts, to=last_CV_ts, by="month")
#   valid_seq <- seq(from=last_CV_ts, to=(lub_last_ts-months(1)), by="month")
#   # about 5 years with 4 seasons -> 20 CVs
#   n <- 20 
#   m <- 4
#   train_ts_vec <- character(n*m)
#   valid_ts_vec <- train_ts_vec
#   rmse_vec <- numeric(n*m)
#   mae_vec <- rmse_vec
#   for(i in 1:n) {
#     train_ts <- sample(train_seq, size=1)
#     print(train_ts)
#     thresh_ts <- ymd_hms(paste(last_CV_ts, "00:00:00", sep=" "), tz="EST")
#     print(paste("Training Timestamp:", train_ts, sep=" "))
#     ### TRAIN
#     pred_df <- createPredDF(tempStats_by_hour, 1, train_ts, thresh_ts)
#     if(length(pred_df) == 1) 
#       next        
#     Y=pred_df[,1]
#     REC=pred_df[,2]
#     LYD=pred_df[,3]
#     HIS=pred_df[,4]
#     INC=pred_df[,5]
#     reg_model <- lm(formula=Y~REC+LYD+HIS+INC) #, lambda=0.1) #, lambda=seq(0,0.05, 0.001)) #no subset
#     
#     # HOW TO CONTROL REGRESSION NOISE MODEL? ADD NOISE MANUALLY
#     
#     ### VALIDATE
#     for(j in 1:m) {
#       valid_ts <- sample(valid_seq, size=1)
#       print(paste("Validation timestamp:", valid_ts, sep=" "))
#       valid_df <- createPredDF(tempStats_by_hour, 1, valid_ts, last_ts)
#       # REMOVE Y from valid df to make SURE IT IS A PREDICTION
#       data <- valid_df[, -1]
#       pred <- predict(reg_model, data, level=0.95, interval="prediction")
#       coef(reg_model)
#       err_measures <- errorMeasures(valid_df$Y, pred[, "fit"])
#       train_chr <- as.character(train_ts)
#       valid_chr <- as.character(valid_ts)
#       index <- 4*i+j-4
#       train_ts_vec[index] <- train_chr
#       valid_ts_vec[index] <- valid_chr
#       rmse_vec[index] <- err_measures[["RMSE"]]
#       mae_vec[index] <- err_measures[["MAE"]]
#       print(pred[, "fit"])
#       png(filename=paste("models/load/temp/plots", paste(paste(train_chr, valid_chr, sep="_"), "png", sep="."), sep="/"))
#       plot(pred[, "fit"], type="l", col="red", xlab="Hours", ylab="Temperature in Fahrenheit", main=paste("Validation period starting from", valid_chr, sep=" "))
#       lines(valid_df$Y, type="l", col="blue")
#       dev.off()
#     }
#     # print score    
#   }
#   results_df <- data.frame(TRAINTMS=train_ts_vec, VALIDTMS=valid_ts_vec, RMSE=rmse_vec, MAE=mae_vec, stringsAsFactors = FALSE)
#   write.table(results_df, paste(paste("models/load/temp/", "results", sep=""), "csv", sep="."), quote=FALSE, row.names=FALSE, sep=",")
# }
# 
# ### LOOPING FUNCTION CALLS
# ### Use get to retrieve an object given a character string containing its name. 
# ### You can also use getFunction, the specific version to retrieve functions
# 
# crossValidation(tempStats_by_hour, first_dt, last_dt)
# 
# 
# # plot results of matchSeries
# #res <- matchSeries(tempStats_by_hour, 1, last_dt)
# # plot(c(res[[1]][,1], res[[2]][,1]), type="l", col="blue")
# # lines(c(rep(0,168), res[[2]][,2]), type="l", col="red")
# # lines(res[[1]][,2], type="l", col="green")
# 
# 
# #plot_temp <- xts(merged_by_year[[1]][,2], datets)
# 
