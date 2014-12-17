calcPosition <- function(leaderboard, row.index, score) {
  col.index <- 1
  for(j in 1:ncol(leaderboard)) {
    board.entry <- leaderboard[row.index, j]
    if(board.entry >= score) {
      col.index <- j
      return(col.index)
    }
  }
  return(ncol(leaderboard))
}

getPredictionType <- function(htype, units) {
  if(htype == 2) {
    return(paste0(units, "m"))
  } else if(htype == 1) {
    return(paste0(units, "w"))
  } else {
    return(paste0(units, "d"))
  }
}

htypeToString <- function(htype) {
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

createDir <- function(folder.path) {
	if (!dir.exists(folder.path)) dir.create(folder.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}

tempPrediction <- function(k, j, model.path, tempf.path, features, model.formula, train.start, pred.start, lag.horizon, train.horizon, pred.horizon, htype, gamma) {
	train.features <- getFeatures(features, train.start, lag.horizon, train.horizon, htype)
	test.features <- getFeatures(features, pred.start, lag.horizon, pred.horizon, htype)

	train.features.fn <- paste0(paste("train-temp-features", "instance", j, pred.type, sep="_"), ".rds")
	saveRDS(train.features, file=paste(tempf.path, train.features.fn, sep="/"), compress=TRUE)
	test.features.fn <- paste0(paste("test-temp-features", "instance", j, pred.type, sep="_"), ".rds")
	saveRDS(test.features, file=paste(tempf.path, test.features.fn, sep="/"), compress=TRUE)

	pred.stop <- getStopDtByHorizon(pred.start, pred.horizon, htype)
	test.dt.seq <- seq(pred.start, pred.stop, by="hour")
	first <- hashDtYear(test.dt.seq)[1]
	second <- hashDtYear(test.dt.seq)[length(test.dt.seq)]
	index1 <- which(features$HASH==first, arr.ind=TRUE)
	index2 <- which(features$HASH==second, arr.ind=TRUE)

	#** CREATE & SAVE LOAD MODEL **#
	if(model.formula == "mean") {
		fit <- temp.features[index1:index2, "LAGM"]      
	} else {
		if(pred.method == "LM") {
			train.result <- trainTempModelFormulaLM(train.features[,-1], model.formula, train.start)
			temp.model <- train.result[["model"]]  
			fit <- predict.lm(temp.model, test.features[, -(1:2)])
		} else if(pred.method == "GAM") {
			train.result <- trainTempModelFormulaGAM(train.features[,-1], model.formula, train.start, gamma)
			temp.model <- train.result[["model"]]  
			fit <- predict.gam(temp.model, test.features[, -(1:2)])
		} else if(pred.method == "NN") {
			train.result <- trainTempModelFormulaNN(train.features[,-1], model.formula, train.start, hidden)
			temp.model <- train.result[["model"]]  
			fit <- predict(temp.model, test.features[, -(1:2)])
		} else if(pred.method == "RF") {
			train.result <- trainTempModelFormulaRF(train.features[,-1], model.formula, train.start, ntrees)
			temp.model <- train.result[["model"]]  
			fit <- predict(temp.model, test.features[, -(1:2)])
		}
		target <- temp.features[index1:index2, 2]
		temp.model.fn <- paste(model.path, paste0(paste("model", k, "instance", j, pred.type, sep="_"), ".txt"), sep="/")
		capture.output(summary(temp.model), file=temp.model.fn)
	}
	train.residuals <- train.result[["residuals"]]
	test.quantiles <- createPredQuantiles(fit, train.residuals)
		
	fit <- data.frame(TMS=test.dt.seq, FIT=fit, TARGET=target, HASH=hashDtYear(test.dt.seq))
	return(list(fit, test.quantiles))
}
