frameMean <- function(df, dim) {
  if(dim == "row") {
    df_m <- apply(df, 1, mean, na.rm = TRUE)
  } else if(dim == "col") {
    df_m <- apply(df, 2, mean, na.rm = TRUE)  
  } else {
    print("Wrong dimension passed to func frameMean!")
  }
}

frameDiff <- function(df, dim) {
  if(dim == "row") {
    df_m <- apply(df, 1, diff, na.rm = TRUE)
  } else if(dim == "col") {
    df_m <- apply(df, 2, diff, na.rm = TRUE)  
  } else {
    print("Wrong dimension passed to func frameDiff!")
  }
}