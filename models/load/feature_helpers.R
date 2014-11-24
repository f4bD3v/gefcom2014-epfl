#if(!exists("addMonth", mode="function")) source("util/date_helpers.R")

# comment/uncomment multiple lines: command + shift + c
# getSeqList <- function(data.df, start.dt, horizon) {
#   stop.dt <- getDtByHorizon(start.dt)
#   #dt.seq.target <- seq(from=start.dt, to=stop.dt, by="hour")
#   index.seq.target <- getIndex(data.df$TMS, start.dt, stop.dt)
#   #dt.seq.recent <- seq(from=subDays(start.dt, 7), to=subDays(start.dt, 1), by="hour")
#   index.seq.recent <- getIndex(data.df$TMS, subDays(start.dt, 7), subDays(stop.dt, 1))
#   #lasty.start.dt <- subYear(start.dt)
#   #lasty.stop.dt <- subYear(stop.dt)
#   #dt.seq.lasty <- seq(from=lasty.start.dt, to=lasty.stop.dt, by="hour")
#   index.seq.lasty <- getIndex(data.df$TMS, subYear(start.dt), subYear(stop.dt))  
#   return(list(target=dt.seq.target, lasty=dt.seq.lasty, recent=dt.seq.recent))
# }

### TODO: check if issues with timestamps
getDirectFeatures <- function(data.df, start.dt, horizon) {
  data <- data.df[, 2]
  stop.dt <- getStopDtByHorizon(start.dt, horizon)
  dt.seq.target <- seq(from=start.dt, to=stop.dt, by="hour")
  dt.seq.recent <- seq(from=subDays(start.dt, 7), to=subHours(start.dt, 1), by="hour")
  lasty.start.dt <- subYear(start.dt)
  lasty.stop.dt <- subYear(stop.dt)
  dt.seq.lasty <- seq(from=lasty.start.dt, to=lasty.stop.dt, by="hour")
  
  #list.of.seq <- getSeqList(data.df, start.dt, horizon)
  nrows <- nrow(data.df)
  if (stop.dt > getLastDt()) {
    target <- rep(NA, length(dt.seq.target)) 
  } else {
    index.seq.target <- calcSeqByIndex(nrows, getColIndex(data.df$HASH, start.dt, stop.dt))
    target <- data[index.seq.target]
  }
  
  index.seq.recent <- calcSeqByIndex(nrows, getColIndex(data.df$HASH, subDays(start.dt, 7), subHours(start.dt, 1)))
  recent <- data[index.seq.recent]
  index.seq.lasty <- calcSeqByIndex(nrows, getColIndex(data.df$HASH, subYear(start.dt), subYear(stop.dt)))
  lasty <- data[index.seq.lasty]
  
  # TODO: use Mean, Max, Min of last week
  daytype <- createMaxDayFeatures(dt.seq.target)
  month <- createMonthFeatures(dt.seq.target)
  hour <- createHourFeatures(dt.seq.target)
  return(list(TMS=dt.seq.target, Y=target, RMEAN=mean(recent), RMAX=max(recent), RMIN=min(recent), LASTY=lasty, DAYT=daytype, MONTH=month, HOUR=hour))
}  

### TODO: what if period during the year: 6 MONTHS? CODED but not debugged
getRowFeatures <- function(data.yearly, start.dt, horizon, offset) {
  print(paste0("getCalcFeatures",start.dt))
  stop.dt <- getStopDtByHorizon(start.dt, horizon)
  index <- getIndex(data.yearly$HASH, start.dt, stop.dt, horizon)
  year <- extractYear(stop.dt)
  # Load starting from 2005: y=9 --> j=5
  j <- year - offset 
  # Remove timestamp column from DF
  data.df <- data.yearly
  if(!leap_year(year)) {
    data.df <- data.df[-c(1417:1440),]
  }
  mean <- calcRowMeans(data.df, index, j)
  min <- calcRowMin(data.df, index, j)
  max <- calcRowMax(data.df, index, j)
  mean.inc <- calcRowAvgInc(data.df, index, j)
  ### TODO: check if current year a leap year leap_year(years)

  return(list(HMEAN=mean, HMIN=min, HMAX=max, HINC=mean.inc))
}

## TODO: change data type from list?
calcRowMeans <- function(data.df, index, y) {
  index.seqs <- calcSeqByIndex(nrow(data.df), index)
  if (typeof(index.seqs) == "list") {
    # 2 DIFFERENT CASES
    m1 <- apply(data.df[index.seqs[[1]], 1:(y-1)], 1, mean, na.rm = TRUE)
    m2 <- apply(data.df[index.seqs[[2]], 1:y], 1, mean, na.rm = TRUE)
    m <- cbind(t(m1), t(m2))[,]
    return(m)
    #return(rbind(m1,m2))
  } 
  m <- apply(data.df[index.seqs, 1:(y-1)], 1, mean, na.rm = TRUE)
  return(m)
}

calcRowMax <- function(data.df, index, y) {
  index.seqs <- calcSeqByIndex(nrow(data.df), index)
  if (typeof(index.seqs) == "list") {
    max1 <- apply(data.df[index.seqs[[1]], 1:(y-1)], 1, max, na.rm = TRUE)
    max2 <- apply(data.df[index.seqs[[2]], 1:y], 1, max, na.rm = TRUE)
    max <- cbind(t(max1), t(max2))[,]
    return(max)
    #return(rbind(max1, max2))
  }
  max <- apply(data.df[index.seqs, 1:(y-1)], 1, max, na.rm = TRUE)
  return(max)
} 

calcRowMin <- function(data.df, index, y) {
  index.seqs <- calcSeqByIndex(nrow(data.df), index)
  if (typeof(index.seqs) == "list") {
    min1 <- apply(data.df[index.seqs[[1]], 1:(y-1)], 1, min, na.rm = TRUE)
    min2 <- apply(data.df[index.seqs[[2]], 1:y], 1, min, na.rm = TRUE)
    min <- cbind(t(min1), t(min2))[,]
    return(min)
    #return(rbind(min1, min2))
  }
  min <- apply(data.df[index.seqs, 1:(y-1)], 1, min, na.rm = TRUE)
  return(min)
}

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

calcRowAvgInc <- function(data.df, index, y) {
  index.seqs <- calcSeqByIndex(nrow(data.df), index)  
  if (typeof(index.seqs) == "list") {
    df.part1 <- data.df[index.seqs[[1]], 1:(y-1)]
    df.part2 <- data.df[index.seqs[[2]], 1:y]
    df.part1.diff <- frameDiff(df.part1, "row")
    mean.inc1 <- frameMean(df.part1.diff, "col")
    mean.inc1[is.nan(mean.inc1)] <- 0
    df.part2.diff <- frameDiff(df.part2, "row")
    mean.inc2 <- frameMean(df.part2.diff, "col")
    # replace NaN (row) values due to only one leap year with 0
    mean.inc2[is.nan(mean.inc2)] <- 0
    mean.inc <- cbind(t(mean.inc1), t(mean.inc2))[,]
    return(mean.inc)
    #return(rbind(mean.inc1, mean.inc2))
  }
  df <- data.df[index.seqs, 1:(y-1)]
  df.diff <- frameDiff(df, "row")
  mean.inc <- frameMean(df.diff, "col")
  return(mean.inc)
}

frameMean <- function(df, dim) {
  if(dim == "row") {
    df_m <- apply(df, 1, mean, na.rm = TRUE)
  } else if(dim == "col") {
    df_m <- apply(df, 2, mean, na.rm = TRUE)  
  } else {
    print("Wrong dimension passed to func frameMean!")
  }
  return(df_m)
}

frameDiff <- function(df, dim) {
  if(dim == "row") {
    df_d <- apply(df, 1, diff, na.rm = TRUE)
  } else if(dim == "col") {
    df_d <- apply(df, 2, diff, na.rm = TRUE)  
  } else {
    print("Wrong dimension passed to func frameDiff!")
  }
  return(df_d)
}

## FUNDAMENTAL FLAW, ERROR, BUG --> QUICKFIX
# PASS HORIZON
getIndex <- function(tms, start.dt, stop.dt, horizon) {
  #print(start.dt)
  index1.dt <- hashDt(start.dt) #replaceYear(start.dt, "2012")
  index1 <- which(tms==index1.dt, arr.ind=TRUE)
  index2.dt <- hashDt(stop.dt) #replaceYear(stop.dt, "2012")
  #print(stop.dt)
  index2 <- which(tms==index2.dt, arr.ind=TRUE)
  if (year(stop.dt) == year(start.dt)) {
    print("same year")
    index2.dt <- hashDt(stop.dt) #replaceYear(stop.dt, "2012")
    index2 <- which(tms==index2.dt, arr.ind=TRUE)
    print(tms[index1:index2])
    index <- list("one", index1, index2)
    return(index)
  }
  index <- list("two", index1, index2)
  return(index)
}

getColIndex <- function(tms, start.dt, stop.dt) {
  #print(stop.dt)
  index1 <- which(tms==hashDtYear(start.dt), arr.ind=TRUE)
  #print(index1)
  index2 <- which(tms==hashDtYear(stop.dt), arr.ind=TRUE)
  return(list(index1, index2))
}

# ONLY ALLOWS TRAINING PERIODS OF A YEAR
calcSeqByIndex <- function(nrows, index) {
  print(paste0("calcSeqByIndex", index))
  #print(index)
  if (typeof(index) == "list") {
    if (is.numeric(index[[1]])) {
      # NO LIST --> get direct features
      return(seq.int(index[[1]], index[[2]], 1))
    } else if(index[[1]] == "one") {
      # NO LIST - same y
      return(seq.int(index[[2]], index[[3]], 1))  
    } else if(index[[1]] == "two") {
      index.seq1 <- seq.int(index[[2]], nrows, 1)
      index.seq2 <- seq.int(1, index[[3]], 1)
      # LIST - y, y-1
      return(list(index.seq1, index.seq2))
    }
  } else {
    print("Index not of type list: Something went wrong!")
  }
}
