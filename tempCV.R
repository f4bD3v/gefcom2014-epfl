library("getopt")

spec <- matrix(
	c('predMethod', 'pm', 1, 'character', # temperature prediction method
	'predTrain', 'pt', 1, 'logical',# 'predict temperature for training period',
	'formula', 'f', 1, 'integer', 
	'gamma', 'g', 2, 'logical',
	'hiddenUnits', 'hu', 2, 'integer',# 'number of units in single hidden layer (nnet package)',
	'ntrees', 'nt', 2, 'integer',
	'PCA', 'p', 2, 'logical', # use principal component of weather stations as opposed to average
	'station', 's', 2, 'integer'), # use specific weather station as opposed to principal component or average
byrow=TRUE, nrow=8, ncol=4);
spec.dim=dim(spec)
spec.opt.long=spec[,1]
spec.opt.short=spec[,2]
spec.opt.type=spec[,3]

opt <- getopt(spec)

if (is.null(opt$predTrain) || is.null(opt$predMethod)) {
	#get the script name (only works when invoked with Rscript).
	self = commandArgs()[1];
	#print a friendly message and exit with a non-zero error code
	cat(paste("Usage: ", self, "-(-predMethod|pm) <(LM|GAM|NN|RF)> [-(-hidden|hu) <(0-9)+>] [-(-ntree|nt) <(0-9)+>] \n",sep=""));
	stop("Required argument missing")
}

source("config.R")
source("util/method-formulas.R")
source("util/error_func.R")
source("util/date_helpers.R")
source("util/preprocess.R")
source("models/load/load_model.R")
source("models/load/temp/temp_model.R")
source("models/load/feature_helpers.R")
source("util/CV_helpers.R")

# TODO: change this part
global.htype <- 2
htype <- 2
units <- 1
pred.type <- getPredictionType(htype, units)
#

#** TEMP **#
method <- opt$predMethod
method.option <- "NONE" 
# fill through filesystem

formula <- "NONE"
if(!is.null(opt$formula)) {
	formula <- opt$formula
}
gamma <- FALSE
if(!is.null(opt$gamma)) {
	gamma <- opt$gamma
	method.option <- "gamma"
}
ntrees <- -1
if(!is.null(opt$ntrees)) {
	ntrees <- opt$ntrees	
	method.option <- underscoreJoin("ntrees", ntrees)
}
hidden.units <- -1
if(!is.null(opt$hiddenUnits)) {
	hidden.units <- opt$hiddenUnits
	method.option <- underscoreJoin("hidden-units", hidden.units)
}
maxit <- 1000
if(!is.null(opt$tempMaxit)) {
	maxit <- opt$tempMaxit
}
pred.train <- FALSE
if(!is.null(opt$predTrain)) {
	pred.train <- opt$predTrain
}
PCA <- FALSE
if(!is.null(opt$PCA)) {
	PCA <- TRUE
	#method <- underscoreJoin(method, "PCA")
}
station <- FALSE
if(!is.null(opt$station)) {
	station <- opt$station
	#method <- underscoreJoin(method, station)
}

units <- 1 

### LOAD LEADERBOARD ###
leaderboard.path <- "data/load/competition-stats/leaderboard.csv"
firstpos_benchmark.path <- "data/load/competition-stats/firstpos_benchmark.csv"
leaderboard <- read.csv(leaderboard.path, header=TRUE, row.names=1, sep=",")
firstpos_benchmark <- read.csv(firstpos_benchmark.path, header=TRUE, sep=",")
#########################

in.path <- "data/load/train"
out.path <- "output/test/CV"
analysis.path <- "data/analysis"

dir.exists <- function(path) FALSE

### PREPARE DATA ###
train.df.path <- pathJoin(analysis.path, "train-df.rds")
if(file.exists(train.df.path)) {
    train.df <- readRDS(train.df.path)
} else {
    train.df <- createTrainDF(loadCSVs(in.path), first.dt, last.dt)
	saveRDS(train.df, file=train.df.path, compress=TRUE)
}

#** TEMPERATURE **#
temp.df <- reduceToTempDF(train.df)
temp.df.path <- pathJoin(analysis.path, "temp-df.rds")
if(file.exists(temp.df.path)) {
    temp.df <- readRDS(temp.df.path)
} else {
    temp.df <- reduceToTempDF(train.df)
	saveRDS(temp.df, file=temp.df.path, compress=TRUE)
}
# Use average temperature over 25 stations
avg.series.path <- pathJoin(analysis.path, "temp-stations-avg-df.rds")
if(file.exists(avg.series.path)) {
    temp.series <- readRDS(avg.series.path)
} else {
    temp.series <- avgTempSeries(temp.df)
	saveRDS(temp.series, file=avg.series.path, compress=TRUE)
}

# Use specific station 
if (station) {
    station.series <- temp.df[ ,c(1,station+1)]
    colnames(station.series) <- c("TMS", "MTEMP")
    temp.series <- station.series 
}
# Use PCA - get first principal component
temp.pca.path <- pathJoin(analysis.path, "temp-stations-pca-df.rds")
if(PCA) {
	pca <- prcomp(temp.df[, -1], retx=TRUE, tol=0.2)
	pc1 <- pca$x
	colnames(pc1) <- "MTEMP"
	saveRDS(pc1, file=temp.pca.path, compress=TRUE)
	temp.series <- data.frame(TMS=temp.series$TMS, MTEMP=pc1)
}

# hash timestamps for easy comparison operations
hash <- hashDtYear(temp.series$TMS)
temp <- cbind(temp.series, HASH=hash)


### DEFINE DATES & HORIZON ###

#** TEST DATES **#
test.month.len <- 13

test.stop.dt <- last.dt 
test.start.dt <- subMonths(addHours(test.stop.dt, 1), test.month.len)
test.dt <- test.start.dt
last.test.dt <- test.dt
cat(paste0("Test Start Dt: ", test.start.dt), sep="\n")
cat(paste0("Test Stop Dt: ", test.stop.dt), sep="\n")
cat("\n")

htype <- 2
test.horizon <- 1

pred.type <- getPredictionType(htype, test.horizon)
#** TEMP TRAINING DATES **#
temp.train.year.len <- 7

temp.train.stop.dt <- subHour(test.start.dt)
temp.train.start.dt <- subYears(test.start.dt, temp.train.year.len)

cat(paste0("Train Start Dt: ", temp.train.start.dt), sep="\n")
cat(paste0("Train Stop Dt: ", temp.train.stop.dt), sep="\n")
cat("\n")
temp.train.dt <- temp.train.start.dt

temp.train.month.len <- temp.train.year.len * 12

#** LOAD TRAINING SET **#
load.train.year.len <- 4

load.train.stop.dt <- temp.train.stop.dt
load.train.start.dt <- subYears(test.start.dt, load.train.year.len)
load.train.dt <- load.train.start.dt
last.load.train.dt <- load.train.dt
cat(paste0("Load Train Start Dt: ", load.train.dt), sep="\n")
cat("\n")

load.train.month.len <- load.train.year.len * 12
################################################


### CREATE LOAD FOLDER STRUCTURE ###
base.path <- createBaseFolder(out.path, test.start.dt, test.stop.dt)
method.path <- createMethodFolder(base.path, "temp", method, method.option, formula, PCA, station) 
print(paste0("method.path", method.path))


if(!grepl("(MEAN|TRUE)", method)) {
	model.instances.path <- createFolder(method.path, "temp-model-instances")
	plot.path <- createFolder(method.path, "plots")
	scores.path <- createFolder(method.path, "scores")

	#** COPY PLOT SCRIPTS **#
	file.copy(from='plotTempResults.R', to=plot.path)
	file.copy(from='util/plot_helpers.R', to=plot.path)
	file.copy(from='config.R', to=plot.path)
}
features.path <- createFolder(method.path, "features")

### CREATE TEMPERATURE FEATRES ###
features.fn <- extensionJoin("full-temp-features", "rds")
curr.features.path <- pathJoin(features.path, features.fn)
if(file.exists(curr.features.path)) {
	temp.features <- readRDS(curr.features.path)
} else {
	temp.features <- createTempFeatures(temp, temp.train.dt, temp.train.month.len + test.month.len)
	saveRDS(temp.features, file=curr.features.path, compress=TRUE)
}

if(method == "TRUE") {
	print('exiting')
	saveRDS(temp, pathJoin(method.path, "true-temperature.rds"))
	stop("Created and saved temperature")
	quit("Created and saved temperature")
} else if(method == "MEAN") {
	print('exiting')
	# if PCA, temp.features should be of applied PCA, so there should not be any problems
	index <- which(temp$HASH==hashDtYear(load.train.start.dt))
	cut.temp <-temp[-c(index:nrow(temp)), ]
	temp <- temp.features[index:nrow(temp), c("TMS", "LAGM", "HASH")]
	colnames(temp) <- c("TMS", "MTEMP", "HASH")
	temp <- rbind(cut.temp, temp)
	saveRDS(temp, pathJoin(method.path, "hist-mean-pred-temperature.rds"))
	stop("Created and saved mean temperature prediction")
	quit("Created and saved mean temperature prediction")
}
#** CREATE LOOP VARS MONTH AND WEEK PREDICTIONS
loop.vars = list(c(2,1),
			c(1,1),
  			c(1,2),
  			c(1,3),
  			c(1,4))

str.htype <- htypeToString(htype)

if(method.option != "NONE") {
	weekly.scores.fn <- extensionJoin(paste("weekly", "temp-fit-scores", method, method.option, paste0("formula", formula), sep="_"), "csv")
	monthly.scores.fn <- extensionJoin(paste("monthly", "temp-fit-scores", method, method.option, paste0("formula", formula), sep="_"), "csv")
} else {
	weekly.scores.fn <- extensionJoin(paste("weekly", "temp-fit-scores", method, paste0("formula", formula), sep="_"), "csv")
	monthly.scores.fn <- extensionJoin(paste("monthly", "temp-fit-scores", method, paste0("formula", formula), sep="_"), "csv")
}
weekly.scores.path <- pathJoin(scores.path, weekly.scores.fn)
monthly.scores.path <- pathJoin(scores.path, monthly.scores.fn)
print(weekly.scores.path)
print(monthly.scores.path)

#** CREATE LIST OF RESULT TABLES
res <- list()

#########################
### CROSS VALIDATION ###
########################
PINBALL <- c()
#-------------------------------
train.start <- temp.train.start.dt
print(train.start)
pred.start <- test.start.dt

# if pred train len, change start dt and run len
pred.run.len <- test.month.len
train.len <- temp.train.month.len
if(pred.train) {
	pred.start <- load.train.start.dt
	pred.run.len <- load.train.month.len + test.month.len
	train.len <- temp.train.month.len - load.train.month.len
}
train.stop <- subHours(addMonths(train.start), 1)

#-- PRINT temp MODEL TYPE TO FILE
method.formula <- temp.methods.formulas[[method]][[formula]]
writeTempHeader(weekly.scores.path, method, method.formula, "weekly", train.start, train.stop, train.len, str.htype) 
writeTempHeader(monthly.scores.path, method, method.formula, "monthly", train.start, train.stop, train.len, str.htype)

#pred.run.len <- 2
temp.res <- list()

for(j in 1:pred.run.len) {
	for (h in 1:length(loop.vars)) { 
		var.set <- loop.vars[[h]]
		htype = var.set[1]
		pred.horizon = 1
		lag = var.set[2]

		# features, train.start, pred.start, lag.horizon, train.len, pred.horizon, htype)
		print(paste0("addedmonths ", addMonths(train.start, train.len)))
		
		train.features <- getFeatures(temp.features, train.start, lag, train.len, htype)
		test.features <- getFeatures(temp.features, pred.start, lag, pred.horizon, htype)

		train.features.fn <- paste0(paste("train-temp-features", "instance", j, pred.type, sep="_"), ".rds")
		saveRDS(train.features, file=pathJoin(features.path, train.features.fn), compress=TRUE)
		test.features.fn <- paste0(paste("test-temp-features", "instance", j, pred.type, sep="_"), ".rds")
		saveRDS(test.features, file=pathJoin(features.path, test.features.fn), compress=TRUE)

		pred.stop <- getStopDtByHorizon(pred.start, pred.horizon, htype)
		test.dt.seq <- seq(pred.start, pred.stop, by="hour")
		first <- hashDtYear(test.dt.seq)[1]
		second <- hashDtYear(test.dt.seq)[length(test.dt.seq)]
		index1 <- which(temp.features$HASH==first, arr.ind=TRUE)
		index2 <- which(temp.features$HASH==second, arr.ind=TRUE)

		#** CREATE & SAVE LOAD MODEL **#
		if(method == "LM") {
			train.result <- trainTempModelFormulaLM(train.features[,-1], method.formula, train.start)
			temp.model <- train.result[["model"]]  
			fit <- predict.lm(temp.model, test.features[, -(1:2)])
		} else if(method == "GAM") {
			train.result <- trainTempModelFormulaGAM(train.features[,-1], method.formula, train.start, gamma)
			temp.model <- train.result[["model"]]  
			fit <- predict.gam(temp.model, test.features[, -(1:2)])
		} else if(method == "NN") {
			print(method.formula)
			print(hidden.units)
			train.result <- trainTempModelFormulaNN(train.features[,-1], method.formula, train.start, hidden.units)
			temp.model <- train.result[["model"]]  
			fit <- predict(temp.model, test.features[, -(1:2)])
		} else if(method == "RF") {
			print(method.formula)
			print(ntrees)
			train.result <- trainTempModelFormulaRF(train.features[,-1], method.formula, train.start, ntrees)
			temp.model <- train.result[["model"]]  
			fit <- predict(temp.model, test.features[, -(1:2)])
		}
		target <- temp.features[index1:index2, 2]
		if(h==1) {
			temp.model.fn <- pathJoin(model.instances.path, extensionJoin(paste("model", "instance", j, "monthly", pred.type, sep="_"), "txt"))
		} else {
			temp.model.fn <- pathJoin(model.instances.path, extensionJoin(paste("model", "instance", j, "weekly", pred.type, sep="_"), "txt"))
		}
		capture.output(summary(temp.model), file=temp.model.fn)

		train.residuals <- train.result[["residuals"]]
		test.quantiles <- createPredQuantiles(fit, train.residuals)
			
		fit <- data.frame(TMS=test.dt.seq, FIT=fit, TARGET=target, HASH=hashDtYear(test.dt.seq))
		pred.temp <- fit 

		temp.res.row <- pred.temp
		if(h==1) { 
			if (j == 1) monthly.temp.res <- temp.res.row else monthly.temp.res <- rbind(monthly.temp.res, temp.res.row)
			first.rest <- 4*7*24+1
			restl <- FALSE
			if(first.rest < nrow(temp.res.row)) {
				restl <- TRUE
				rest.days <- c(first.rest:nrow(temp.res.row))
				rest <- temp.res.row[rest.days,]
			}
		} else {
			if (j == 1 && h==2) weekly.temp.res <- temp.res.row else weekly.temp.res <- rbind(weekly.temp.res, temp.res.row)
			if (h==5 && restl == TRUE) weekly.temp.res <- rbind(weekly.temp.res, rest)
		}
		
		# TO SAVE OR NOT TO SAVE HERE?
		#attr(temp.res, 'htype') <- htypeToString(htype)
		#attr(temp.res, 'horizon') <- pred.horizon
		# TODO - save to method folder with pred.type
		#res.fn <- paste0(paste(pred.type, "results", "model", k, "instance", j, sep="_"), ".rds")
		#saveRDS(temp.res.row, file=paste(subfolder.path, res.fn, sep="/"), compress=TRUE)

		## What about train.start increment?
		#pred.start <- incrementDt(pred.start, pred.horizon, htype)
		#cat(paste0("Train Start Dt for Temp Prediction: ", train.start), sep="\n")
		#cat(paste0("Test Start Dt for Temp Prediction: ", pred.start), sep="\n")
		#cat("\n")

		if(h==1) {
			chunks <- 4
		} else {
			chunks <- pred.horizon
		}
		
		err.scores <- pointErrorMeasures(pred.temp$TARGET, pred.temp$FIT)
		err.pinball <- pinball(test.quantiles, pred.temp$TARGET)

		len.fit <- length(temp.res.row$FIT)
		#** CALC ERROR FOR DAYS AFTER W4 **#
		if(h==1) {
			rest.days <- c((4*7*24):len.fit)
			if(length(rest.days) == 1) {
				rest.err.scores <- list(RMSE=0,MAE=0,MAPE=0)
				rest.err.pinball <- 0
			} else {
				rest.err.scores <- pointErrorMeasures(pred.temp$TARGET[rest.days], pred.temp$FIT[rest.days])
				rest.err.pinball <- pinball(test.quantiles[rest.days,], pred.temp$TARGET[rest.days])
			}
		}

		tms.row <- cbind(TRAIN.TMS=as.character(train.start), TEST.TMS=as.character(pred.start))
		err.row <- cbind(do.call(cbind.data.frame, err.scores), PINBALL=err.pinball)
		res.row <- cbind(tms.row, err.row)
		cat("h ", h, sep="\n")

		#** RECORD ERROR FOR DAYS AFTER W4 **#
		if (h==1) {
			if (j == 1) res[[h]] <- res.row else res[[h]] <- rbind(res[[h]], res.row)
			rest.pred.start <- incrementDt(pred.start, 4, 1)
			tms.row <- cbind(TRAIN.TMS=as.character(train.start), TEST.TMS=as.character(rest.pred.start))
			err.row <- cbind(do.call(cbind.data.frame, rest.err.scores), PINBALL=rest.err.pinball)
			res.row <- cbind(tms.row, err.row)
			cat("length loop.vars", length(loop.vars), sep="\n")
			hp <- h + 1
			if (j == 1) res[[hp]] <- res.row else res[[hp]] <- rbind(res[[hp]], res.row)
		} else {
			hp <- h + 1
			if (j == 1) res[[hp]] <- res.row else res[[hp]] <- rbind(res[[hp]], res.row)
		}

		#** UPDATE DATES **# 
		if(h==1) {
			last.pred.start <- pred.start
			last.train.start <- train.start
		}

		# FLEX HORIZON ?
		if(h > 1 && lag != 4) {
			# do not increment
			if(pred.train && j >= load.train.month.len) train.start <- incrementDt(train.start, 1, htype)
			pred.start <- incrementDt(pred.start, 1, htype) 
		} else if (lag == 4) {
			# do not increment
			if(pred.train && j >= load.train.month.len) train.start <- incrementDt(last.train.start, 1, 2)
			pred.start <- incrementDt(last.pred.start, 1, 2) 
		}

		if(h==1 && pred.train && j < load.train.month.len) {
			train.len <- train.len + 1
		}

		cat(paste0("Temp Train Dt: ", train.start), sep="\n")
		cat(paste0("Temp Test Dt: ", pred.start), sep="\n")
		cat("\n")
		#******************#
	}
}

temp.res[[1]] <- monthly.temp.res
temp.res[[2]] <- weekly.temp.res

date.period <- paste0(as.character(as.Date(test.start.dt)), "-", as.character(as.Date(test.stop.dt)))

score.board <- list()
for(h in 1:(length(loop.vars)+1)) {
	res.last.row <- cbind(TRAIN.TMS=as.character(load.train.start.dt), TEST.TMS=as.character(last.test.dt), RMSE=mean(res[[h]]$RMSE), MAE=mean(res[[h]]$MAE), MAPE=mean(res[[h]]$MAPE), PINBALL=mean(res[[h]]$PINBALL))
	score.board[[h]] <- rbind(res[[h]], res.last.row)
	row.names(score.board[[h]]) <- c(c(1:pred.run.len), "MODEL CV MEAN")
	if (h==1) {
		if(method.option != "NONE") {
			scores.fn <- extensionJoin(paste("predtrain", "temp-fit-scores", "1m", date.period, method, method.option, paste0("formula", formula), "monthly", sep="_"), "rds")
		} else {
			scores.fn <- extensionJoin(paste("predtrain", "temp-fit-scores", "1m", date.period, method, paste0("formula", formula), "monthly", sep="_"), "rds")
		}
  		appendTableToFile(score.board[[h]], monthly.scores.path)
  		cat("\n", file=monthly.scores.path, append = TRUE)
	} else if (h==2) {
		if(method.option != "NONE") {
			scores.fn <- extensionJoin(paste("predtrain", "temp-fit-scores", "5wrest", date.period, method, method.option, paste0("formula", formula), "monthly", sep="_"), "rds")
		} else {
			scores.fn <- extensionJoin(paste("predtrain", "temp-fit-scores", "5wrest", date.period, method, paste0("formula", formula), "monthly", sep="_"), "rds")
		}
  		appendTableToFile(score.board[[h]], monthly.scores.path)
  		cat("\n", file=weekly.scores.path, append = TRUE)
	} else {
		var.set <- loop.vars[[h-1]]
		htype = var.set[1]
		test.horizon = var.set[2]
		if(method.option != "NONE") {
			scores.fn <- extensionJoin(paste("predtrain", "temp-fit-scores", getPredictionType(htype, test.horizon), date.period, method, method.option, paste0("formula", formula), "weekly", sep="_"), "rds")
		} else {
			scores.fn <- extensionJoin(paste("predtrain", "temp-fit-scores", getPredictionType(htype, test.horizon), date.period, method, paste0("formula", formula), "weekly", sep="_"), "rds")
		}
  		appendTableToFile(score.board[[h]], weekly.scores.path)
  		cat("\n", file=weekly.scores.path, append = TRUE)
	}
   	saveRDS(score.board[[h]], file=paste(scores.path, scores.fn, sep="/"), compress=TRUE)
}


if(pred.train) {
	htype <- 2
	pred.horizon = 1
	pred.type <- getPredictionType(htype, pred.horizon)

	temp.res.h <- data.frame(temp.res[[1]])

	attr(temp.res.h, 'htype') <- htypeToString(htype)
	attr(temp.res.h, 'horizon') <- temp.train.month.len + test.month.len 
	attr(temp.res.h, 'train.len') <- temp.train.month.len - load.train.month.len
	attr(temp.res.h, 'train.start') <- temp.train.start.dt
	attr(temp.res.h, 'train.stop') <- subHours(load.train.start.dt, 1)
	attr(temp.res.h, 'test.start') <- load.train.start.dt
	attr(temp.res.h, 'test.stop') <- test.stop.dt
	attr(temp.res.h, 'option') <- method.option
	attr(temp.res.h, 'formula') <- method.formula
	attr(temp.res.h, 'formula_index') <- formula
	attr(temp.res.h, 'pred_interval') <- "monthly"

	if(method.option != "NONE") {
		temp.res.fn <- extensionJoin(paste("predtrain", "temp-fit", date.period, method, method.option, paste0("formula", formula), "monthly", sep="_"), "rds")
	} else {
		temp.res.fn <- extensionJoin(paste("predtrain", "temp-fit", date.period, method, paste0("formula", formula), "monthly", sep="_"), "rds")
	}
	saveRDS(temp.res.h, file=paste(method.path, temp.res.fn, sep="/"), compress=TRUE)

	first.index <- which(temp.res.h$HASH==hashDtYear(test.start.dt), arr.ind=TRUE)
	temp.res.h <- temp.res.h[first.index:nrow(temp.res.h), ]

	attr(temp.res.h, 'horizon') <- test.month.len 
	attr(temp.res.h, 'train.len') <- temp.train.month.len
	attr(temp.res.h, 'train.stop') <- temp.train.stop.dt
	attr(temp.res.h, 'test.start') <- test.start.dt
	if(method.option != "NONE") {
		temp.res.fn <- extensionJoin(paste("temp-fit", date.period, method, method.option, paste0("formula", formula), "monthly", sep="_"), "rds")
	} else {
		temp.res.fn <- extensionJoin(paste("temp-fit", date.period, method, paste0("formula", formula), "monthly", sep="_"), "rds")
	}
	saveRDS(temp.res.h, file=paste(method.path, temp.res.fn, sep="/"), compress=TRUE)


	temp.res.h2 <- data.frame(temp.res[[2]])

	attr(temp.res.h, 'horizon') <- temp.train.month.len + test.month.len 
	attr(temp.res.h, 'train.len') <- temp.train.month.len - load.train.month.len
	attr(temp.res.h, 'train.start') <- temp.train.start.dt
	attr(temp.res.h, 'train.stop') <- subHours(load.train.start.dt, 1)
	attr(temp.res.h, 'test.start') <- load.train.start.dt
	attr(temp.res.h, 'pred_interval') <- "weekly"
	if(method.option != "NONE") {
		temp.res.fn <- extensionJoin(paste("predtrain", "temp-fit", date.period, method, method.option, paste0("formula", formula), "weekly", sep="_"), "rds")
	} else {
		temp.res.fn <- extensionJoin(paste("predtrain", "temp-fit", date.period, method, paste0("formula", formula), "weekly", sep="_"), "rds")
	}
	saveRDS(temp.res.h2, file=paste(method.path, temp.res.fn, sep="/"), compress=TRUE)

	first.index <- which(temp.res.h2$HASH==hashDtYear(test.start.dt), arr.ind=TRUE)
	temp.res.h2 <- temp.res.h2[first.index:nrow(temp.res.h2), ]

	attr(temp.res.h, 'horizon') <- test.month.len 
	attr(temp.res.h, 'train.len') <- temp.train.month.len
	attr(temp.res.h, 'train.stop') <- temp.train.stop.dt
	attr(temp.res.h, 'test.start') <- test.start.dt
	if(method.option != "NONE") {
		temp.res.fn <- extensionJoin(paste("temp-fit", date.period, method, method.option, paste0("formula", formula), "weekly", sep="_"), "rds")
	} else {
		temp.res.fn <- extensionJoin(paste("temp-fit", date.period, method, paste0("formula", formula), "weekly", sep="_"), "rds")
	}
	saveRDS(temp.res.h2, file=paste(method.path, temp.res.fn, sep="/"), compress=TRUE)
} else {
	htype <- 2
	pred.horizon = 1
	pred.type <- getPredictionType(htype, pred.horizon)

	temp.res.h <- data.frame(temp.res[[1]])

	attr(temp.res.h, 'htype') <- htypeToString(htype)
	attr(temp.res.h, 'horizon') <- test.month.len 
	if(method.option != "NONE") {
		temp.res.fn <- extensionJoin(paste("predtrain", "temp-fit", date.period, method, method.option, paste0("formula", formula), "weekly", sep="_"), "rds")
	} else {
		temp.res.fn <- extensionJoin(paste("predtrain", "temp-fit", date.period, method, paste0("formula", formula), "weekly", sep="_"), "rds")
	}
	saveRDS(temp.res.h2, file=paste(method.path, temp.res.fn, sep="/"), compress=TRUE)

	temp.res.h2 <- data.frame(temp.res[[2]])

	attr(temp.res.h2, 'htype') <- htypeToString(htype)
	attr(temp.res.h2, 'horizon') <- test.month.len 
	if(method.option != "NONE") {
		temp.res.fn <- extensionJoin(paste("temp-fit", method, method.option, paste0("formula", formula), "weekly", sep="_"), "rds")
	} else {
		temp.res.fn <- extensionJoin(paste("temp-fit", method, paste0("formula", formula), "weekly", sep="_"), "rds")
	}
	saveRDS(temp.res.h2, file=paste(method.path, temp.res.fn, sep="/"), compress=TRUE)
}

