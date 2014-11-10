parseDates<-function(timestamps) {
  
  timestamps <- as.character(timestamps)
  
  ###
  d_new <- t_stamps
  # not ambiguous when nchar is 6 - e.g. 112014
  d_new <- ifelse(nchar(d_new)==6,
                  paste0("0", substr(d_new,1,1), "0", substr(d_new,2,nchar(d_new))), d_new)
  # now not ambiguous when nchar is 7 and it doesn't begin with a "1" - e.g. 2112014, 9122014
  d_new <- ifelse(nchar(d_new)==7 & substr(d_new,1,1) != "1",
                  paste0("0",d_new), d_new)
  # now guess a leading zero and parse - e.g. 09112014, but problem with 01112014, could be 11012014
  # --> 01112014
  d_new <- ifelse(nchar(d_new)==7, paste0("0",d_new), d_new)
  d_try <- as.POSIXct(d_new, "%m%d%Y %H:%m")
  ###
  
  # now only days in October, November, and December might be wrong (see above)
  bad <- cumsum(c(1L,as.integer(diff(d_try)))-1L) < 0L # we have a sequence of dates... 
  # put the leading zero in the day, but remember "bad" rows have an
  # extra leading zero, so make sure to skip it
  d_try2 <- ifelse(bad,paste0(substr(d_new,2,3),"0", substr(d_new,4,nchar(d_new))), d_new)
  # convert to Date, POSIXlt, whatever and do a happy dance
  dates <- as.POSIXct(d_try2, "%m%d%Y %H:%m")
  
  return(dates)
}