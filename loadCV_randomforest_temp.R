# USEFUL TUTORIAL ON USING R SCRIPTS
# http://tgmstat.wordpress.com/2014/05/21/r-scripts/
library("getopt")

# Specification matrix has 4 (5) columns:
# - col1: long flag name
# - col2: short flag name
# - col3: type of argument to follow flag (0 = no argument, 1 = required argument, 2 = optional argument.)
# - col4: data type to which the flag argument shall be cast (logical, integer, double, complex, character)
# - col5: to add a brief description of the purpose of the option
spec <- matrix(
	c('predMethod', 'pm', 1, 'character',# 'the prediction method to be used (LM, GAM, NN, RF)',
	'tempModel', 'tm', 1, 'integer',
	'htype', 'ht', 1, 'integer',# 'the horizon type for which to predict',
	'units', 'un', 1, 'integer',# 'the number of units of htype to predict ahead',
	'gamma', 'g', 2, 'logical',# 'number of trees in RF'
	'model', 'm', 2, 'integer',# 'load model',
	'hidden', 'hu', 2, 'integer',# 'number of units in single hidden layer (nnet package)',
	'ntree', 'nt', 2, 'integer'),# 'number of trees in RF'
byrow=TRUE, nrow=8, ncol=4);
spec.dim=dim(spec)
spec.opt.long=spec[,1]
spec.opt.short=spec[,2]
spec.opt.type=spec[,3]
# set decay and maxit?

# REMOVE PRED.TEMP, always use true temperature, predTrainTemp and predTemp

opt <- getopt(spec)

if (is.null(opt$predMethod)) {
	#get the script name (only works when invoked with Rscript).
	self = commandArgs()[1];
	#print a friendly message and exit with a non-zero error code
	cat(paste("Usage: ", self, " -(-loadModel|lm) <(1-3)> -(-predTemp|pt) <0/1> -(-htype|ht) <(0-2)> -(-units|un) <(0-9)+> -(-predMethod|pm) <(LM|GAM|NN|RF)> [-(-hidden|hu) <(0-9)+>] [-(-ntree|nt) <(0-9)+>] \n",sep=""));
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

gamma <- FALSE
if(!is.null(opt$gamma)) {
	gamma <- opt$gamma
}

temp.model <- 1 
if(!is.null(opt$tempModel)) {
	temp.model <- opt$tempModel
}

k <- 1
if(!is.null(opt$model)) {
	k <- as.numeric(opt$model)
}

### call: Rscript loadCV.R --args [#model]
in.path <- "data/load/train"
out.path <- "output/test/CV"

dir.exists <- function(path) FALSE

use.pca <- 0 
use.temp1 <- 0
global.htype <- as.numeric(opt$htype)
htype <- global.htype
units <- opt$units 
pred.method <- opt$predMethod
hidden <- opt$hidden
ntrees <- opt$ntree

pred.type <- getPredictionType(htype, units)

### LOAD LEADERBOARD ###
leaderboard.path <- "data/load/competition-stats/leaderboard.csv"
firstpos_benchmark.path <- "data/load/competition-stats/firstpos_benchmark.csv"
leaderboard <- read.csv(leaderboard.path, header=TRUE, row.names=1, sep=",")
firstpos_benchmark <- read.csv(firstpos_benchmark.path, header=TRUE, sep=",")
#########################


### PREPARE DATA ###
train.df <- createTrainDF(loadCSVs(in.path), first.dt, last.dt)
train.df.fn <- paste0("train-df", ".rds")
saveRDS(train.df, file=train.df.fn, compress=TRUE)

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

#** TEMP PCA **#
pca <- prcomp(temp.df[,-1], retx=TRUE, tol=0.2)
pc1 <- pca$x
colnames(pc1) <- "MTEMP"
pc.temp <- data.frame(TMS=avg.temp.series$TMS, MTEMP=pc1, HASH=hash)
if(use.pca) avg.temp <- pc.temp

#** LOAD **#
load.df <- reduceToLoadDF(train.df)
load.df.fn <- paste0("load-df", ".rds")
saveRDS(load.df, file=load.df.fn, compress=TRUE)

hash <- hashDtYear(load.df$TMS)
load.df <- cbind(load.df, HASH=hash)
load.df.fn <- paste0("load-df-hash", ".rds")
saveRDS(load.df, file=load.df.fn, compress=TRUE)

start.index <- which(load.df$HASH == hashDtYear(addYears(first.dt, 4)))
load.df <- load.df[start.index:nrow(load.df),]
#load.df <- na.omit(load.df) # remove first years of NA values 
load.df.fn <- paste0("load-df-omit", ".rds")
saveRDS(load.df, file=load.df.fn, compress=TRUE)
#####################


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

test.horizon <- units

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
folder.path <- out.path

#---

test.first <- format(test.start.dt, "%d-%m-%Y")
test.last <- format(test.stop.dt, "%d-%m-%Y")

# - period model type folder
folder  <- paste(aschr(test.first), aschr(test.last), pred.type, sep="_")
folder.path <- paste(folder.path, folder, sep="/")
createDir(folder.path)

subfolder.path <- paste(folder.path, "load", sep="/")
createDir(subfolder.path)

# write load model choices to file 
loadf.subfolder.path <- paste(subfolder.path, "features", sep="/")
if (!dir.exists(loadf.subfolder.path)) dir.create(loadf.subfolder.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")

# same for temperature?
if(pred.method == "GAM") {
	subfolder.path <- paste(subfolder.path, "gam", sep="/")
	if (!dir.exists(subfolder.path)) dir.create(subfolder.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
	if(gamma) {
		subfolder.path <- paste(subfolder.path, "gamma", sep="/")
		if (!dir.exists(subfolder.path)) dir.create(subfolder.path, showWarnings = TRUE, recursive = FALSE, mode = "01000")
	}
} else if(pred.method == "LM") {
	subfolder.path <- paste(subfolder.path, "lm", sep="/")
	if (!dir.exists(subfolder.path)) dir.create(subfolder.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
} else if(pred.method == "NN") {
	subfolder.path <- paste(subfolder.path, "nnet", sep="/")
	if (!dir.exists(subfolder.path)) dir.create(subfolder.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
	subfolder.path <- paste(subfolder.path, paste("hidden-units", hidden, sep="_"), sep="/")
	if (!dir.exists(subfolder.path)) dir.create(subfolder.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
} else if(pred.method == "RF") {
	subfolder.path <- paste(subfolder.path, "randomforest", sep="/")
	if (!dir.exists(subfolder.path)) dir.create(subfolder.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
	subfolder.path <- paste(subfolder.path, paste("ntrees", ntrees, sep="_"), sep="/")
	if (!dir.exists(subfolder.path)) dir.create(subfolder.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}

# - model folder
subfolder <- "models"
model.path <- paste(subfolder.path, subfolder, sep="/")
if (!dir.exists(model.path)) dir.create(model.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")

#** COPY PLOT SCRIPTS **#
file.copy(from='plotLoadResults.R', to=subfolder.path)
file.copy(from='util/plot_helpers.R', to=subfolder.path)

#--- FILES
str.htype <- htypeToString(htype)

# Where to put length of train and test?
########################################

#** TEMPERATURE MODELS **#
# limit number of models - to decrease amount of computation
#####################################


### CREATE FEATURES FOR WHOLE CV ###
#** createLoadFeatures: 
# - load.df: dataframe of tms, load, hash
# - start.dt: start of load training
# - horizon: length of features in htype respective units (until test.stop.dt)
# - htype=2
load.features <- createLoadFeatures(load.df, load.train.dt, load.train.month.len + test.month.len)
load.features.fn <- paste0("all-load-features", ".rds")
saveRDS(load.features, file=paste(loadf.subfolder.path, load.features.fn, sep="/"), compress=TRUE)

#** CREATE LOOP VARS MONTH AND WEEK PREDICTIONS
loop.vars = list(c(2,1),
			c(1,1),
  			c(1,2),
  			c(1,3),
  			c(1,4))

#** CREATE LIST OF RESULT TABLES
res <- list()


#-- PRINT LOAD MODEL TYPE TO FILE
if(pred.method == "GAM") {
	load.model.formula <- gam.load.models[[k]]
	load.model.chr <- paste0("GAM Load Model ", k, ": ", load.model.formula)
} else if(pred.method == "LM") {
	load.model.formula <- lm.load.models[[1]]
	load.model.chr <- paste0("LM Load Model: ", load.model.formula)
} else if(pred.method == "NN") {
	load.model.formula <- nnet.load.models[[1]]
	load.model.chr <- paste0("NNET Load Model: ", load.model.formula)
} else if(pred.method == "RF") {
	load.model.formula <- randomforest.load.models[[1]]
	load.model.chr <- paste0("RandomForest Load Model: ", load.model.formula)
}
temp.model.formulas <- randomforest.temp.models

temp.path <- paste(folder.path, "temp", sep="/")
temp.method.path <- paste(temp.path, "randomforest", sep="/")
dirs <- list.dirs(temp.method.path, full.names = TRUE, recursive=FALSE)
print(dirs)

temp.methods <- list()
temp.methods.files <- list()
output.files <- list()
for(i in 1:length(dirs)) {
	dirc <- dirs[[i]]
	temp.files <- dir(dirc, pattern = '^predtrain(.*)temp-fit-target', full.names = TRUE)
	pattern <- paste0("model", temp.model)
	temp.methods.files[[i]] <- temp.files[grepl(pattern, temp.files)]
	parts <- strsplit(dirc, "/")[[1]]
	method <- parts[length(parts)]
	temp.methods[[i]] <- method
	fn.csv <- paste0(paste("temp-method", "nnet", method, "scores", pred.type, sep="_"), ".csv")
	output.file <- paste(subfolder.path, fn.csv, sep="/")
	output.files[[i]] <- output.file
}
print(output.files)
print(temp.methods.files)

index <- which(avg.temp$HASH==hashDtYear(load.train.start.dt))
cut.temp <- avg.temp[-c(index:nrow(avg.temp)), ]

test.month.len <- 2
# TRUE TEMPERATURE AS ADDITIONAL MODEL

#########################
### CROSS VALIDATION ###
########################
for(m in 1:length(temp.methods)) {
	load.train.chr <- paste0("Load Model Training Length: ", load.train.month.len, " ", str.htype)
	writeToFile(load.model.chr, output.files[[m]])
	appendToFile(load.train.chr, output.files[[m]])
	cat("\n", file = output.files[[m]], append = TRUE)
	#-----------------------------

	for(i in 1:2) {
		PINBALL <- c()
		POSITIONS <- c()
		if(i==1) {
			sinterval <- "monthly"
			interval <- "m"
		} else {
			sinterval <- "weekly"
			interval <- "w"
		}
		temp.model.file <- temp.methods.files[[m]][grepl(sinterval, temp.methods.files[[m]])]
		print(temp.model.file)
		pred.temp <- readRDS(temp.model.file)
		print(nrow(pred.temp))
		print(head(pred.temp))
		print(tail(pred.temp))
		print(nrow(cut.temp))
		print(head(cut.temp))
		print(tail(cut.temp))
		print(nrow(avg.temp))
		print(head(avg.temp))
		print(tail(avg.temp))

		pred.temp <- pred.temp[,c(1,2,4)]
		colnames(pred.temp) <- c("TMS", "MTEMP", "HASH")
		avg.temp <- rbind(cut.temp, pred.temp)
		rownames(avg.temp) <- NULL

		#-- PRINT TEMP MODEL TYPE TO FILE
		temp.model.chr <- paste(interval, "Temp Model", temp.model, temp.methods[[m]], ": ", temp.model.formulas[[temp.model]], sep=" ")
		# temp train month len stays the same and is defined by config
		temp.train.chr <- paste0("Temp Model Training Length: ", temp.train.month.len, " ", str.htype)
		appendToFile(temp.model.chr, output.files[[m]])
		appendToFile(temp.train.chr, output.files[[m]])
		cat("\n", file = output.files[[m]], append = TRUE)
		#-------------------------------

		### SELECT TEMPERATURE FROM MODEL FORMULA INDEX
		# cut off avg.temp and merge with predicted temp to new avg.temp

		# write function to get from method to folder
		# write function to parse pred.type and model

		#### TODO: CHECK IF THERE IS PROBLEM WITH AVG.TEMP IN LOOP
		test.month.len <- 2

		### CROSS VALIDATION ###
		for (j in 1:test.month.len) {
			# LOOP: CREATE MONTHLY LOAD PREDICTION (DLAG = 35), CREATE PREDICTION FOR FIRST WEEK (DLAG = 7), SECOND WEEK (DLAG = 14), THIRD WEEK (DLAG = 21), FOURTH WEEK (DLAG = 28)
			for (h in 1:length(loop.vars)) { 
				var.set <- loop.vars[[h]]
				htype = var.set[1]
				test.horizon = 1
				lag = var.set[2]

				pred.type <- getPredictionType(htype, test.horizon)

				print(tail(avg.temp))
				print(load.train.month.len)
				#** GET FEATURES FOR CURRENT TRAIN AND TEST PERIODS **#
				# - lag for training changes
				load.train.features <- assembleLoadFeatures(load.features, avg.temp, load.train.dt, lag, load.train.month.len, 2)
				# - htype for test horizon changes
				load.test.features <- assembleLoadFeatures(load.features, avg.temp, test.dt, lag, test.horizon, htype)

				load.train.features.fn <- paste0(paste("train-load-features", "instance", j, pred.type, sep="_"), ".rds")
				saveRDS(load.train.features, file=paste(loadf.subfolder.path, load.train.features.fn, sep="/"), compress=TRUE)
				load.test.features.fn <- paste0(paste("test-load-features", "instance", j, pred.type, sep="_"), ".rds")
				saveRDS(load.test.features, file=paste(loadf.subfolder.path, load.test.features.fn, sep="/"), compress=TRUE)
				
				#** CREATE & SAVE LOAD MODEL **#
				if(pred.method == "LM") {
					train.result <- trainLoadModelFormulaLM(load.train.features[,-1], load.model.formula, load.train.dt)
				} else if(pred.method == "GAM") {
					train.result <- trainLoadModelFormulaGAM(load.train.features[,-1], load.model.formula, load.train.dt, gamma)
				} else if(pred.method == "NN") {
					train.result <- trainLoadModelFormulaNN(load.train.features[,-1], load.model.formula, load.train.dt, hidden)
				} else if(pred.method == "RF") {
					train.result <- trainLoadModelFormulaRF(load.train.features[,-1], load.model.formula, load.train.dt, ntrees)
				}
				load.model <- train.result[["model"]]  
				load.model.fn <- paste(model.path, paste0(paste("temp-model", temp.model, temp.methods[[m]], interval, "instance", j, pred.type, sep="_"), ".txt"), sep="/")
				capture.output(summary(load.model), file=load.model.fn)
				
				#** CREATE PREDICTION **#
				if(pred.method == "LM") {
					load.fit <- predict.lm(load.model, load.test.features[, -(1:2)])
					len.fit <- length(load.fit)
				} else if(pred.method == "GAM") {
					load.fit <- predict.gam(load.model, load.test.features[, -(1:2)])
					len.fit <- nrow(load.fit)
				} else if(pred.method == "NN") {
					load.fit <- as.vector(predict(load.model, load.test.features[, -(1:2)]))
					len.fit <- length(load.fit)
				} else if(pred.method == "RF") {
					load.fit <- predict(load.model, load.test.features[, -(1:2)])
					len.fit <- length(load.fit)
				}

				print(head(load.fit))

				#** USE TRAINING RESIDUALS TO COMPUTE PREDICTION QUANTILES **#
				train.residuals <- train.result[["residuals"]]
				print(head(train.residuals))
				test.quantiles <- createPredQuantiles(load.fit, train.residuals)

				#** SAVE RESULTS **#
				# - only save monthly prediction
				if(h==1) {
					load.res.row <- cbind(TMS=as.character(load.test.features$TMS), FIT=load.fit, TARGET=load.test.features$Y)
					if(j==1) load.res <- load.res.row else load.res <- rbind(load.res, load.res.row)

					attr(load.res.row, 'htype') <- htypeToString(htype)
					attr(load.res.row, 'horizon') <- test.horizon 
					load.res.row.fn <- paste0(paste("temp-model", temp.model, temp.methods[[m]], interval, pred.type, "?", "load-fit-target", "instance", j, sep="_"), ".rds")
					saveRDS(load.res.row, file=paste(subfolder.path, load.res.row.fn, sep="/"), compress=TRUE)
				}

				if(h==1) {
					chunks <- 4
				} else {
					chunks <- test.horizon
				}
				
				err.scores <- pointErrorMeasures(load.test.features$Y, load.fit)
				err.pinball <- pinball(test.quantiles, load.test.features$Y)

				#** CALC ERROR FOR DAYS AFTER W4 **#
				if(h==1) {
					rest.days <- c((4*7*24):len.fit)
					if(length(rest.days) == 1) {
						rest.err.scores <- list(RMSE=0,MAE=0,MAPE=0)
						rest.err.pinball <- 0
					} else {
						rest.err.scores <- pointErrorMeasures(load.test.features$Y[rest.days], load.fit[rest.days])
						rest.err.pinball <- pinball(test.quantiles[rest.days,], load.test.features$Y[rest.days])
					}
				}

				#** CALC LEADERBOARD POSITION FOR GIVEN MONTH **#
				if(h==1) {
					position <- calcPosition(leaderboard, j, err.pinball)
					POSITIONS <- c(POSITIONS, position)
				}

				tms.row <- cbind(LOAD.TRAIN.TMS=as.character(load.train.dt), TEST.TMS=as.character(test.dt))
				err.row <- cbind(do.call(cbind.data.frame, err.scores), PINBALL=err.pinball)
				res.row <- cbind(tms.row, err.row)
				cat("h ", h, sep="\n")

				#** RECORD ERROR FOR DAYS AFTER W4 **#
				if (h==1) {
					if (j == 1) res[[h]] <- res.row else res[[h]] <- rbind(res[[h]], res.row)
					rest.test.dt <- incrementDt(test.dt, 4, 1)
					tms.row <- cbind(LOAD.TRAIN.TMS=as.character(load.train.dt), TEST.TMS=as.character(rest.test.dt))
					err.row <- cbind(do.call(cbind.data.frame, rest.err.scores), PINBALL=rest.err.pinball)
					res.row <- cbind(tms.row, err.row)
					cat("length loop.vars", length(loop.vars), sep="\n")
					hp <- h + 1
					if (j == 1) res[[hp]] <- res.row else res[[hp]] <- rbind(res[[hp]], res.row)
				} else {
					hp <- h + 1
					if (j == 1) res[[hp]] <- res.row else res[[hp]] <- rbind(res[[hp]], res.row)
				}
				print(res[[hp]])

				#** UPDATE DATES **# 
				if(h==1) {
					last.test.dt <- test.dt
					last.load.train.dt <- load.train.dt
				}

				if(h > 1 && lag != 4) {
					load.train.dt <- incrementDt(load.train.dt, 1, htype)
					test.dt <- incrementDt(test.dt, 1, htype) 
				} else if (lag == 4) {
					load.train.dt <- incrementDt(last.load.train.dt, 1, 2)
					test.dt <- incrementDt(last.test.dt, 1, 2) 
				}
				cat(paste0("Load Train Dt: ", load.train.dt), sep="\n")
				cat(paste0("Load Test Dt: ", test.dt), sep="\n")
				cat("\n")
				#******************#
			}
		}

		attr(load.res, 'htype') <- htypeToString(htype)
		attr(load.res, 'horizon') <- test.month.len 
		load.res.fn <- paste0(paste("temp-model", temp.model, temp.methods[[m]], interval, test.month.len, "all", pred.type, "?", "fit-target", "instance", h, sep="_"), ".rds")
		saveRDS(load.res, file=paste(subfolder.path, load.res.fn, sep="/"), compress=TRUE)


		score.board <- list()
		for(h in 1:(length(loop.vars)+1)) {
			res.last.row <- cbind(LOAD.TRAIN.TMS=as.character(load.train.start.dt), TEST.TMS=as.character(last.test.dt), RMSE=mean(res[[h]]$RMSE), MAE=mean(res[[h]]$MAE), MAPE=mean(res[[h]]$MAPE), PINBALL=mean(res[[h]]$PINBALL))
			score.board[[h]] <- rbind(res[[h]], res.last.row)
			row.names(score.board[[h]]) <- c(c(1:test.month.len), "MODEL CV MEAN")
		}

		position.board <- cbind(MAPE=res[[1]]$MAPE, PINBALL=res[[1]]$PINBALL, POSITION=POSITIONS, PINBALL_FIRSTPOS=firstpos_benchmark[1:test.month.len, ])
		dates <- res[[1]][1:(nrow(res[[1]])), c(1,2)]
		position.board <- cbind(dates, position.board)
		row.names(position.board) <- c(1:test.month.len)#, "MODEL CV MEAN")

		### LATER: DO STATS AND RECOMPUTE 
		for(h in 1:(length(loop.vars)+1)) {
			appendTableToFile(score.board[[h]], output.files[[m]])
			cat("\n", file = output.files[[m]], append = TRUE)
			scores.subfolder.path <- paste(subfolder.path, "scores", sep="/")
			if (!dir.exists(scores.subfolder.path)) dir.create(scores.subfolder.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
			if (h==1) {
				scores.fn <- paste0(paste("tempm", temp.model, temp.methods[[m]], interval, "scores", "1m", sep="_"), ".rds")

				col <- position.board[1:test.month.len, 1:6]
				comp <- col
			} else if (h==2) {
				scores.fn <- paste0(paste("tempm", temp.model, temp.methods[[m]], interval, "scores", "5wrest", sep="_"), ".rds")
			} else {
				var.set <- loop.vars[[h-1]]
				htype = var.set[1]
				test.horizon = var.set[2]
				scores.fn <- paste0(paste("tempm", temp.model, temp.methods[[m]], interval, "scores", getPredictionType(htype, test.horizon), sep="_"), ".rds")

				sb <- score.board[[h]]
				col <- sb$MAPE[1:test.month.len]
				col <- as.numeric(col)
				prev.coln <- colnames(comp)
				comp <- cbind(comp, col)
				colnames(comp) <- c(prev.coln, c(paste0("MAPE_w", h-2)))
				col <- sb$PINBALL[1:test.month.len]
				col <- as.numeric(col)
				prev.coln <- colnames(comp)
				comp <- cbind(comp, col)
				colnames(comp) <- c(prev.coln, c(paste0("PINBALL_w", h-2)))

			}
			### SAVE SCORES TO RDS FILES ###
			saveRDS(score.board[[h]], file=paste(scores.subfolder.path, scores.fn, sep="/"), compress=TRUE)
		}
		appendTableToFile(position.board, output.files[[m]])
		cat("\n", file = output.files[[m]], append = TRUE)
		
		colnames(comp) <- c("TRAIN.TMS", "TEST.TMS", "MAPE_1m", "PINBALL_1m", "POS_1m", "PINBALL_#1", "MAPE_1w", "PINBALL_1w", "MAPE_2w", "PINBALL_2w", "MAPE_3w", "PINBALL_3w", "MAPE_4w", "PINBALL_4w")
		#comp <- data.frame(TRAIN.TMS=comp[,1], TEST.TMS=comp[,2], PINBALL_1m=as.numeric(comp[,3]), POS_1m=as.numeric(comp[,4]), PINBALL_1w=as.numeric(comp[,5]), PINBALL_2w=as.numeric(comp[,6]), PINBALL_3w=as.numeric(comp[,7]), PINBALL_4w=as.numeric(comp[,8]))
		#comp <- data.frame(TRAIN.TMS=comp[,1], TEST.TMS=comp[,2], MAPE_1m=comp[,3], PINBALL_1m=comp[,4], POS_1m=comp[,5], PINBALL_FIRSTPOS=comp[,6], PINBALL_1w=comp[,7], PINBALL_2w=comp[,8], PINBALL_3w=comp[,9], PINBALL_4w=comp[,10])
		#comp[,3:8] <- sapply(comp[, 3:8], as.numeric)

		combined.score <- c()
		combined.pos <- c()
		#ul <- unname(unlist(comp))
		#print(ul)
		scores <- rowMeans(comp[, 5:8, drop=FALSE], na.rm=TRUE)
		#scores <- apply(comp[,5:8, drop=FALSE], 1, mean, na.rm=TRUE)	
		for(h in 1:test.month.len) {
			score <- scores[h]
			combined.score <- c(combined.score, score)
			combined.pos <- c(combined.pos, calcPosition(leaderboard, h, score))
		}
		comparison.board <- cbind(comp, PINBALL_wAVG=combined.score, POS_wAVG=combined.pos)
		appendTableToFile(comparison.board, output.files[[m]])
		cat("\n", file = output.files[[m]], append = TRUE)

		comparison.board.fn <- paste0(paste("comparison_board_tempm", temp.model, temp.methods[[m]], interval, sep="_"), ".rds")
		saveRDS(comparison.board, file=paste(scores.subfolder.path, comparison.board.fn, sep="/"), compress=TRUE)
		
		### IMPORTANT: RESET DATES ###
		temp.train.dt <- temp.train.start.dt
		load.train.dt <- load.train.start.dt
		test.dt <- test.start.dt

		htype <- global.htype
		test.horizon <- units
		###############

		cat("\n", file = output.files[[m]], append = TRUE)
		cat("\n", file = output.files[[m]], append = TRUE)
	}
}
