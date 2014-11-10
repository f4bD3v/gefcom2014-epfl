reduceToLoadDF <- function(train_data) {
  load.data <- train_data[, c("TIMESTAMP", "LOAD")]  
  colnames(load.data) <- c("TMS", "LOAD")
  return(load.data)
}

loadSeriesByYear <- function(load_series) {
  load_year_list <- split(load_series, year(ymd(as.Date(load_series$TMS))))
  names_list <- names(load_year_list)
  for(i in 1:length(load_year_list)) {
    load_col <- paste("LOAD", names_list[i], sep="_")
    ze_names <- c("TMS", load_col)  
    colnames(load_year_list[[i]]) <- ze_names
    load_year_list[[i]][, "TMS"] <- substr(load_year_list[[i]][, "TMS"], 6, 19)
  }
  load_df <- Reduce(merge.all, load_year_list)
  tms <- load_df[,"TMS"]
  # add leap year for plotting
  tms <- paste("2012-", tms, sep="")
  # format chr as POSIXct
  datets <- as.POSIXct(tms, format="%Y-%m-%d %H:%M:%S")
  load_df <- load_df[,-1]
  load_df <- transform(load_df, TMS=datets, MEAN=rowMeans(load_df, na.rm = TRUE), SD=apply(load_df, 1, sd, na.rm = TRUE))
  return(load_df)
}