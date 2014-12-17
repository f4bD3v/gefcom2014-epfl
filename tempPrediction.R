library("getopt")

spec <- matrix(
	c('predMethod', 'pm', 1, 'character',# 'the prediction method to be used (LM, GAM, NN, RF)',
	'predTrainTemp', 'ptt', 1, 'logical',# 'predict temperature for training period',
	'model', 'm', 2, 'integer',# 'load model',
	'gamma', 'g', 2, 'logical',# 'number of trees in RF'
	'hidden', 'hu', 2, 'integer',# 'number of units in single hidden layer (nnet package)',
	'ntree', 'nt', 2, 'integer'),# 'number of trees in RF'
byrow=TRUE, nrow=6, ncol=4);
spec.dim=dim(spec)
spec.opt.long=spec[,1]
spec.opt.short=spec[,2]
spec.opt.type=spec[,3]

opt <- getopt(spec)

if (is.null(opt$predTrainTemp) || is.null(opt$predMethod)) {
	#get the script name (only works when invoked with Rscript).
	self = commandArgs()[1];
	#print a friendly message and exit with a non-zero error code
	cat(paste("Usage: ", self, "-(-predMethod|pm) <(LM|GAM|NN|RF)> [-(-hidden|hu) <(0-9)+>] [-(-ntree|nt) <(0-9)+>] \n",sep=""));
	stop("Required argument missing")
}

source("config.R")
source("util/error_func.R")
source("util/date_helpers.R")
source("util/plot_helpers.R")
source("util/preprocess.R")
source("models/load/load_model.R")
source("models/load/temp/temp_model.R")
source("models/load/feature_helpers.R")
source("util/CV_helpers.R")
source("util/model-formulas.R")

use.temp1 <- 0
pred.traintemp <- opt$predTrainTemp
use.pca <- 0 
use.temp1 <- 0
units <- 1 
pred.method <- opt$predMethod
hidden <- opt$hidden
ntrees <- opt$ntree

if(is.null(opt$model)) {
	k <- 1
} else {
	k <- opt$model
}

gamma <- FALSE
if(!is.null(opt$gamma)) {
	gamma <- opt$gamma
}

### LOAD LEADERBOARD ###
leaderboard.path <- "data/load/competition-stats/leaderboard.csv"
firstpos_benchmark.path <- "data/load/competition-stats/firstpos_benchmark.csv"
leaderboard <- read.csv(leaderboard.path, header=TRUE, row.names=1, sep=",")
firstpos_benchmark <- read.csv(firstpos_benchmark.path, header=TRUE, sep=",")
#########################

in.path <- "data/load/train"
out.path <- "output/test/CV"

dir.exists <- function(path) FALSE

### PREPARE DATA ###
train.df <- createTrainDF(loadCSVs(in.path), first.dt, last.dt)

#** TEMPERATURE **#
temp.df <- reduceToTempDF(train.df)
# Use average temperature over 25 stations
avg.temp.series <- avgTempSeries(temp.df)

# Use first series
if (use.temp1) {
    temp1.series <- temp.df[ ,c(1,2)]
    colnames(temp1.series) <- c("TMS", "MTEMP")
    avg.temp.series <- temp1.series
}

# hash timestamps for easy comparison operations
hash <- hashDtYear(avg.temp.series$TMS)
avg.temp <- cbind(avg.temp.series, HASH=hash)


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


### CREATE FOLDER STRUCTURE ###
#if(pred.traintemp) folder <- paste0(today, "_train_pred") else if(use.pca) folder <- paste0(today, "_pca") else if(use.temp1) folder <- paste0(today, "_temp1")
test.first <- format(test.start.dt, "%d-%m-%Y")
test.last <- format(test.stop.dt, "%d-%m-%Y")

# NO TODAY FOLDER
# - period model type folder
folder <- paste(aschr(test.first), aschr(test.last), pred.type, sep="_")
folder.path <- paste(out.path, folder, sep="/")
createDir(folder.path)

folder.path <- paste(folder.path, "temp", sep="/")
createDir(folder.path)

# MOVE dates inside the folders
# write load model choices to file 
tempf.path <- paste(folder.path, "features", sep="/")
createDir(tempf.path)

# same for temperature?
if(pred.method == "GAM") {
	subfolder.path <- paste(folder.path, "gam", sep="/")
	createDir(subfolder.path)
	if(gamma) {
		print('creating gamma dir')
		subfolder.path <- paste(subfolder.path, "gamma", sep="/")
		createDir(subfolder.path)
	}
} else if(pred.method == "LM") {
	subfolder.path <- paste(folder.path, "lm", sep="/")
	createDir(subfolder.path)
} else if(pred.method == "NN") {
	subfolder.path <- paste(folder.path, "nnet", sep="/")
	createDir(subfolder.path)
	subfolder.path <- paste(subfolder.path, paste("hidden-units", hidden, sep="_"), sep="/")
	createDir(subfolder.path)
} else if(pred.method == "RF") {
	subfolder.path <- paste(folder.path, "randomforest", sep="/")
	createDir(subfolder.path)
	subfolder.path <- paste(subfolder.path, paste("ntrees", ntrees, sep="_"), sep="/")
	createDir(subfolder.path)
}

# - model folder
subfolder <- "models"
model.path <- paste(subfolder.path, subfolder, sep="/")
createDir(model.path)

scores.subfolder.path <- paste(subfolder.path, 'scores', sep="/")
createDir(scores.subfolder.path)

#** COPY PLOT SCRIPTS **#
file.copy(from='util/plot_helpers.R', to=subfolder.path)
file.copy(from='plotTempResults.R', to=subfolder.path)

str.htype <- htypeToString(htype)

#* csv
fn.csv <- paste0(paste("results", pred.type, sep="_"), ".csv")
output.file <- paste(subfolder.path, fn.csv, sep="/")


### CREATE TEMPERATURE FEATRES ###
temp.features <- createTempFeatures(avg.temp, temp.train.dt, temp.train.month.len + test.month.len)
temp.features.fn <- paste0("all-temp-features", ".rds")
saveRDS(temp.features, file=paste(tempf.path, temp.features.fn, sep="/"), compress=TRUE)

saveRDS(temp.features, file='all-temp-features.rds', compress=TRUE)


#** CREATE LOOP VARS MONTH AND WEEK PREDICTIONS
loop.vars = list(c(2,1),
			c(1,1),
  			c(1,2),
  			c(1,3),
  			c(1,4))

#** CREATE LIST OF RESULT TABLES
res <- list()

#-- PRINT temp MODEL TYPE TO FILE
# WHAT TO DO WITH temp.model.formulas?
if(pred.method == "GAM") {
	temp.model.formula <- gam.temp.models[[k]]
	temp.model.chr <- paste0("GAM temp Model ", k, ": ", temp.model.formula)
} else if(pred.method == "LM") {
	temp.model.formula <- lm.temp.models[[1]]
	temp.model.chr <- paste0("LM temp Model: ", temp.model.formula)
} else if(pred.method == "NN") {
	temp.model.formula <- nnet.temp.models[[1]]
	temp.model.chr <- paste0("NNET temp Model: ", temp.model.formula)
} else if(pred.method == "RF") {
	temp.model.formula <- randomforest.temp.models[[1]]
	temp.model.chr <- paste0("RandomForest temp Model: ", temp.model.formula)
}
temp.train.chr <- paste0("Temp Model Training Length: ", temp.train.month.len, " ", str.htype)
writeToFile(temp.model.chr, output.file)
appendToFile(temp.train.chr, output.file)
cat("\n", file = output.file, append = TRUE)


#########################
### CROSS VALIDATION ###
########################
PINBALL <- c()
#-- PRINT TEMP MODEL TYPE TO FILE
temp.model.chr <- paste0("Temp Model : ", temp.model.formula)
temp.train.chr <- paste0("Temp Model Training Length: ", temp.train.month.len, " ", str.htype)
appendToFile(temp.model.chr, output.file)
appendToFile(temp.train.chr, output.file)
cat("\n", file = output.file, append = TRUE)
#-------------------------------

#### TODO: CHECK IF THERE IS PROBLEM WITH AVG.TEMP IN LOOP

# SAVE TRUE TEMPERATURE SERIES (take from features)
# CREATE PRED TEMPERATURE
# CREATE PREDICTED TRAIN TEMPERATURE

# need for piece of code than runs in for loop --> saves pred temperature
# same piece of code that runs in for loop --> creates pred train temperature before, merges and saves

train.start <- temp.train.start.dt
print(train.start)
pred.start <- test.start.dt
if(pred.traintemp) {
	pred.start <- load.train.start.dt
}

# if pred train len, change start dt and run len
pred.run.len <- test.month.len
if(pred.traintemp) {
	pred.run.len <- load.train.month.len + test.month.len
}

#pred.run.len <- 2

train.len <- temp.train.month.len
if(pred.traintemp) {
	train.len <- temp.train.month.len - load.train.month.len
}

temp.res <- list()

for(j in 1:pred.run.len) {
	for (h in 1:length(loop.vars)) { 
		var.set <- loop.vars[[h]]
		htype = var.set[1]
		pred.horizon = 1
		lag = var.set[2]

		# features, train.start, pred.start, lag.horizon, train.len, pred.horizon, htype)
		print(paste0("addedmonths ", addMonths(train.start, train.len)))
		pred <- tempPrediction(k, j, model.path, tempf.path, temp.features, temp.model.formula, train.start, pred.start, lag, train.len, pred.horizon, htype, gamma)
		pred.temp <- pred[[1]]
		test.quantiles <- pred[[2]]

		temp.res.row <- pred.temp
		if(h==1) { 
			if (j == 1) temp.res[[h]] <- temp.res.row else temp.res[[h]] <- rbind(temp.res[[h]], temp.res.row)
			first.rest <- 4*7*24+1
			rest <- FALSE
			if(first.rest < nrow(temp.res.row)) {
				rest <- TRUE
				rest.days <- c(first.rest:nrow(temp.res.row))
				rest <- temp.res.row[rest.days,]
			}
		} else {
			if (j == 1 && h==2) temp.res[[2]] <- temp.res.row else temp.res[[2]] <- rbind(temp.res[[2]], temp.res.row)
			if (h==5 && rest == TRUE) temp.res[[2]] <- rbind(temp.res[[2]], rest)
			print(head(temp.res[[2]]))
			print(tail(temp.res[[2]]))
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
			if(pred.traintemp && j >= load.train.month.len) train.start <- incrementDt(train.start, 1, htype)
			pred.start <- incrementDt(pred.start, 1, htype) 
		} else if (lag == 4) {
			# do not increment
			if(pred.traintemp && j >= load.train.month.len) train.start <- incrementDt(last.train.start, 1, 2)
			pred.start <- incrementDt(last.pred.start, 1, 2) 
		}

		if(h==1 && pred.traintemp && j < load.train.month.len) {
			train.len <- train.len + 1
		}

		cat(paste0("Temp Train Dt: ", train.start), sep="\n")
		cat(paste0("Temp Test Dt: ", pred.start), sep="\n")
		cat("\n")
		#******************#
	}

	print(head(temp.res[[2]]))
	print(tail(temp.res[[2]]))
}

print(head(temp.res[[2]]))

score.board <- list()
for(h in 1:(length(loop.vars)+1)) {
	res.last.row <- cbind(TRAIN.TMS=as.character(load.train.start.dt), TEST.TMS=as.character(last.test.dt), RMSE=mean(res[[h]]$RMSE), MAE=mean(res[[h]]$MAE), MAPE=mean(res[[h]]$MAPE), PINBALL=mean(res[[h]]$PINBALL))
	score.board[[h]] <- rbind(res[[h]], res.last.row)
	row.names(score.board[[h]]) <- c(c(1:pred.run.len), "MODEL CV MEAN")
  	appendTableToFile(score.board[[h]], output.file)
  	cat("\n", file = output.file, append = TRUE)
	if (h==1) {
		scores.fn <- paste0(paste("model", k, "scores", "1m", sep="_"), ".rds")
	} else if (h==2) {
		scores.fn <- paste0(paste("model", k, "scores", "5wrest", sep="_"), ".rds")
	} else {
		var.set <- loop.vars[[h-1]]
		htype = var.set[1]
		test.horizon = var.set[2]
		scores.fn <- paste0(paste("model", k, "scores", getPredictionType(htype, test.horizon), sep="_"), ".rds")
	}
   	saveRDS(score.board[[h]], file=paste(scores.subfolder.path, scores.fn, sep="/"), compress=TRUE)
}

if(pred.traintemp) {
	htype <- 2
	pred.horizon = 1
	pred.type <- getPredictionType(htype, pred.horizon)

	temp.res.h <- data.frame(temp.res[[1]])

	attr(temp.res.h, 'htype') <- htypeToString(htype)
	attr(temp.res.h, 'horizon') <- temp.train.month.len + test.month.len 
	temp.res.fn <- paste0(paste("predtrain", paste0("model", k), "monthly", "temp-fit-target", sep="_"), ".rds")
	saveRDS(temp.res.h, file=paste(subfolder.path, temp.res.fn, sep="/"), compress=TRUE)

	first.index <- which(temp.res.h$HASH==hashDtYear(test.start.dt), arr.ind=TRUE)
	temp.res.h <- temp.res.h[first.index:nrow(temp.res.h), ]

	attr(temp.res.h, 'htype') <- htypeToString(htype)
	attr(temp.res.h, 'horizon') <- test.month.len 
	temp.res.fn <- paste0(paste(paste0("model", k), "monthly", "temp-fit-target", sep="_"), ".rds")
	saveRDS(temp.res.h, file=paste(subfolder.path, temp.res.fn, sep="/"), compress=TRUE)

	temp.res.h2 <- data.frame(temp.res[[2]])

	attr(temp.res.h2, 'htype') <- htypeToString(htype)
	attr(temp.res.h2, 'horizon') <- temp.train.month.len + test.month.len 
	temp.res.fn <- paste0(paste("predtrain", paste0("model", k), "weekly", "temp-fit-target", sep="_"), ".rds")
	saveRDS(temp.res.h2, file=paste(subfolder.path, temp.res.fn, sep="/"), compress=TRUE)

	first.index <- which(temp.res.h$HASH==hashDtYear(test.start.dt), arr.ind=TRUE)
	temp.res.h2 <- temp.res.h2[first.index:nrow(temp.res.h2), ]

	attr(temp.res.h2, 'htype') <- htypeToString(htype)
	attr(temp.res.h2, 'horizon') <- test.month.len 
	temp.res.fn <- paste0(paste(paste0("model", k), "weekly", "temp-fit-target", sep="_"), ".rds")
	saveRDS(temp.res.h2, file=paste(subfolder.path, temp.res.fn, sep="/"), compress=TRUE)
} else {
	htype <- 2
	pred.horizon = 1
	pred.type <- getPredictionType(htype, pred.horizon)

	temp.res.h <- data.frame(temp.res[[1]])

	attr(temp.res.h, 'htype') <- htypeToString(htype)
	attr(temp.res.h, 'horizon') <- test.month.len 
	temp.res.fn <- paste0(paste(paste0("model", k), "monthly", "temp-fit-target", sep="_"), ".rds")
	saveRDS(temp.res.h, file=paste(subfolder.path, temp.res.fn, sep="/"), compress=TRUE)

	temp.res.h2 <- data.frame(temp.res[[2]])

	attr(temp.res.h2, 'htype') <- htypeToString(htype)
	attr(temp.res.h2, 'horizon') <- test.month.len 
	temp.res.fn <- paste0(paste(paste0("model", k), "weekly", "temp-fit-target", sep="_"), ".rds")
	saveRDS(temp.res.h2, file=paste(subfolder.path, temp.res.fn, sep="/"), compress=TRUE)
}

