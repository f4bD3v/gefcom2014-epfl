calcPosition <- function(leaderboard, row.index, score) {
  col.index <- 1
  for(j in 1:ncol(leaderboard)) {
    board.entry <- leaderboard[row.index, j]
    if(board.entry >= score) {
      print(board.entry)
      col.index <- j
      return(col.index)
    }
  }
  return(ncol(leaderboard))
}

getPredictionType(htype, units) {
  if(htype == 2) {
    return(paste0(units, "m"))
  } else if(htype == 1) {
    return(paste0(units, "w"))
  } else {
    return(paste0(units, "d"))
  }
}

htypeToString(htype) {
  if(htype == 2) {
    return("months")
  } else if(htype == 1) {
    return("weeks")
  } else {
    return("days")
  }
}

aschr <- function(date) {
  return(as.character(date))
}


