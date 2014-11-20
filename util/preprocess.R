loadCSVs <- function(path) {
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  assign('train_data', do.call(rbind, tables))
  return(train_data)
}

reduceToLoadDF <- function(train_data) {
  load.data <- train_data[, c("TIMESTAMP", "LOAD")]  
  colnames(load.data) <- c("TMS", "LOAD")
  return(load.data)
}

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

listTempSeries <- function(temp_data) {
  temp_list <- list()
  for(i in 1:(ncol(temp_data)-1)) {
    col <- paste("w",i, sep="")
    temp_list[[i]] <- temp_data[c("TMS", col)]
  }
  return(temp_list)
}

avgTempSeries <- function(temp_data) {
  mean_temp <- data.frame(TMS=temp_data$TIMESTAMP, MTEMP=rowMeans(temp_data[, -1]))
  return(mean_temp)
}

splitByYear <- function(temp_data) {
  temp_year_list <- split(temp_data, year(ymd(as.Date(temp_data$TMS))))
}

nameColsByYear <- function(temp_data, i, j) {
  # check for string
  t_col <- paste(paste(paste("t", i, sep=""), "_", sep=""), j, sep="")
  new_names <- c(colnames(temp_data)[1], t_col)
  colnames(temp_data) <- new_names
  temp_data <- unique(temp_data)
  return(temp_data)
}

# Use this function to create list of average temperature by year
listSeriesByYear <- function(temp_data, i) {
  temp_year_list <- splitByYear(temp_data)
  for(j in 1:length(temp_year_list)) {
      curr_df <- temp_year_list[[j]]
      curr_df <- nameColsByYear(curr_df, i, j)
      # remove year from timestamp to allow for merge on month-day time
      # REPLACE YEAR IN TIMESTAMP
      tms <- replaceYear(curr_df$TMS, "2012")
      curr_df$TMS = tms
      temp_year_list[[j]] <- curr_df
  }
  return(temp_year_list)
}

listSeriesListByYear <- function(temp_list, first_dt, last_dt) {  
  first_date <- as.character(as.Date(first_dt))
  last_date <- as.character(as.Date(last_dt))
  
  temp_by_year <- list()
  for(i in 1:length(temp_list)) {
    temp_year_list <- splitByYear(temp_list[[i]])
    temp_by_year[[i]] <- listSeriesByYear(temp_year_list, i)
  }
  return(temp_by_year)
}

merge.all <- function(x, y) { merge(x, y, all=TRUE, by="TMS") }

mergeSeriesListByHour <- function(temp_by_year) {
  merged_by_hour <- list()  
  for(i in 1:length(temp_by_year)) {
    merged_by_hour[[i]] <- mergeSeriesByHour(temp_by_year[[i]])
  }
  return(merged_by_hour)
}

mergeSeriesByHour <- function(temp_list_by_year) {
  merged_by_hour <- Reduce(merge.all, temp_list_by_year)
  tms <- as.POSIXct(merged_by_hour$TMS, format="%Y-%m-%d %H:%M:%S", tz="EST")
  #tms <- merged_by_hour$TMS
  curr_df <- merged_by_hour[, -1]
  merged_by_hour <- transform(curr_df, TMS=removeYear(tms), HASH=hashDt(tms), MEAN=rowMeans(curr_df, na.rm = TRUE), SD=apply(curr_df, 1, sd, na.rm = TRUE))
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

appendToFile <- function(chr, fn) {
  write(chr, file=fn, append=TRUE)
}

appendTableToFile <- function(df, fn) {
  write.table(df, file=fn, quote=FALSE, col.names=NA, append=TRUE, sep=",")
}
