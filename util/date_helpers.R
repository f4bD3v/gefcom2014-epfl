require(timeDate)
require(lubridate)
require(stringr)

require(plyr) # for func mapvalues

incrementDt <- function(dt, units, htype) {
  if(htype == 0) {
    incr.dt <- addDays(dt, units)
  } else if (htype == 1) {
    incr.dt <- addWeeks(dt, units)
  } else {
    incr.dt <- addMonths(dt, units)  
  }
  return(incr.dt)
}  

getFirstDt <- function() {
  first.dt <- as.POSIXct("01/01/2001 01:00", format="%m/%d/%Y %H:%M", tz="EST")
  return(first.dt)
}

getBeforeLastDt <- function() {
  return(subMonth(getLastDt()))
}

getLastDt <- function() {
  last.dt <- as.POSIXct("10/01/2011 00:00", format="%m/%d/%Y %H:%M", tz="EST")
  return(last.dt)
}

addTms <- function(dt) {
  ndt <- paste(dt, "00:00:00 EST", sep=" ") # EST
  return(ndt)
}

removeYear <- function(dt, y) {
  # take last part of datetime and paste year before
  #ndt <- as.POSIXct(paste(y, str_sub(dt, start=5, end=-1), sep=""), format="%m/%d/%Y %H:%M", tz="EST")
  #ndt <- paste(y, str_sub(dt, start=5, end=-1), sep="")
  ndt <- str_sub(dt, start=5, end=-1)
  return(ndt)
}

replaceYear <- function(dt, y) {
  # take last part of datetime and paste year before
  #ndt <- as.POSIXct(paste(y, str_sub(dt, start=5, end=-1), sep=""), format="%m/%d/%Y %H:%M", tz="EST")
  ndt <- paste(y, str_sub(dt, start=5, end=-1), sep="")
  return(ndt)
}

hashDtYear <- function(dt) {
  hash <- (((year(dt)-2000)*100+month(dt))*100+day(dt))*100+hour(dt)
  return(hash)  
}

hashDt <- function(dt) {
  hash <- ((month(dt))*100+day(dt))*100+hour(dt)
  return(hash)  
}

extractYear <- function(dt) {
  y <- as.numeric(str_sub(year(dt), start=-1))
  if(year(dt) > 2009)
    y <- as.numeric(str_sub(year(dt), start=-2))
  return(y)
}

getUSHolidays <- function(dt) {
  holidays_2005 <- c("12/26/2005", "11/24/2005", "11/11/2005", "10/10/2005", "09/05/2005", "07/04/2005", "05/30/2005", "02/21/2005", "01/17/2005", "12/31/2004")
  holidays_2006 <- c("12/25/2006", "11/23/2006", "11/10/2006", "10/09/2006", "09/04/2006", "07/04/2006", "05/29/2006", "02/20/2006", "01/16/2006", "01/02/2006")
  holidays_2007 <- c("12/25/2007", "11/22/2007", "11/12/2007", "10/08/2007", "09/03/2007", "07/04/2007", "05/28/2007", "02/19/2007", "01/15/2007", "01/01/2007")
  holidays_2008 <- c("12/25/2008", "11/27/2008", "11/11/2008", "10/13/2008", "09/01/2008", "07/04/2008", "05/26/2008", "02/18/2008", "01/21/2008", "01/01/2008")
  holidays_2009 <- c("12/25/2009", "11/26/2009", "11/11/2009", "10/12/2009", "09/07/2009", "07/03/2009", "05/25/2009", "02/16/2009", "01/19/2009", "01/01/2009")
  holidays_2010 <- c("12/24/2010", "11/25/2010", "11/11/2010", "10/11/2010", "09/06/2010", "07/05/2010", "05/31/2010", "02/15/2010", "01/18/2010", "01/01/2010")
  holidays_2011 <- c("12/26/2011", "11/24/2011", "11/11/2011", "10/10/2011", "09/05/2011", "07/04/2011", "05/30/2011", "02/21/2011", "01/17/2011", "12/31/2010")
  holidays <- c(holidays_2005, holidays_2006, holidays_2007, holidays_2008, holidays_2009, holidays_2010, holidays_2011)
  holidays <- as.Date(holidays, format="%m/%d/%Y")
  return(as.character(holidays))
}

getStopDtByHorizon <- function(start.dt, units, htype=2) {
  incr.dt <- incrementDt(start.dt, units, htype)
  stop.dt <- subHours(incr.dt, 1)
  return(stop.dt)
} 

addDays <- function(dt, n) {
  ndt <- dt + days(n)  
  return(ndt)
}

subDays <- function(dt, n) {
  ndt <- dt - days(n)
  return(ndt)
}

addWeeks <- function(dt, n) {
  ndt <- dt + weeks(n)
  return(ndt)
}

addMonth <- function(dt) {
  ndt <- dt %m+% months(1)
  return(ndt)
}

addMonths <- function(dt, n) {
  ndt <- dt %m+% months(n)
  return(ndt)
}

subMonth <- function(dt) {
  ndt <- dt %m-% months(1)
  return(ndt)
}

subMonths <- function(dt, n) {
  ndt <- dt %m-% months(n)
  return(ndt)
}

addYear <- function(dt) {
  ndt <- dt + years(1)
  return(ndt)
}

addYears <- function(dt, n) {
  ndt <- dt + years(n)
  return(ndt)
}

subYear <- function(dt) {
  ndt <- dt - years(1)
  return(ndt)
}

subYears <- function(dt, n) {
  ndt <- dt - years(n)
  return(ndt)
}

addHours <- function(dt, n) {
  ndt <- dt + hours(n)
  return(ndt)
}

subHours <- function(dt, n) {
  ndt <- dt - hours(n)
  return(ndt)
}

### LOAD specific

extractDaytype <- function(x, y) {
  #print(grep(as.character(as.Date(x)), y, fixed=TRUE))
  dtc <- as.character(as.Date(x, tz="EST"))
  if(dtc %in% y) {
    return("Holiday")
  } else {
    return(weekdays(x))
  }
}

## DONE
# if holiday is on a Thursday --> treat Friday as holiday as well
# if holiday is on a Tuesay --> treat Monday as a holiday as well
# offset: -1 for mondays, +1 for fridays
reclassifyBy <- function(day.types, daytype, offset) {
  indices <- which(day.types == "Holiday")
  indices.vec <- split(indices, ceiling(seq_along(indices)/24))
  if (length(indices.vec) != 0) {
    for(i in 1:length(indices.vec)) {
      if (daytype == "Friday") {
        index <- max(unlist(indices.vec[[i]]))
        rel.indices <- which(day.types[index+offset] == daytype) 
      } else {
        index <- min(unlist(indices.vec[[i]]))
        rel.indices <- which(day.types[index+offset] == daytype)  
      }
      day.types[index+rel.indices] <- "Holiday"
    }
  }
  return(day.types)
}

reclassifyGapDays <- function(day.types) {
  day.types <- reclassifyBy(day.types, "Friday", 1:24)
  day.types <- reclassifyBy(day.types, "Monday", -(1:24))
  return(day.types)
}

mapDayFeatures <- function(dt.vec, days, nums) {
  day.types <- unlist(lapply(dt.vec, extractDaytype, getUSHolidays()))
  #day.types.reclass <- reclassifyGapDays(day.types)
  day.types.reclass <- day.types
  day.features <- mapvalues(day.types.reclass, from=days, to=nums)
  return(as.numeric(day.features))
}

createMaxDayFeatures <- function(dt.vec) {
  days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Holiday")
  nums <- c(2, 3, 4, 5, 6, 7, 1, 8)
  return(mapDayFeatures(dt.vec, days, nums))
}

createMinDayFeatures <- function(dt.vec) {
  days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Holiday")
  nums <- c(2, 2, 2, 2, 2, 1, 1, 3)
  return(mapDayFeatures(dt.vec))
}

createDayFeatures <- function(dt.vec) {
  days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Holiday")
  nums <- c(2, 3, 3, 3, 4, 5, 1, 6)
  return(mapDayFeatures(dt.vec))
}

createMonthFeatures <- function(dt.vec) {
  return(month(dt.vec))
}

createHourFeatures <- function(dt.vec) {
  return(hour(dt.vec))
}
