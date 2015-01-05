library("getopt")

# Specification matrix has 4 (5) columns:
# - col1: long flag name
# - col2: short flag name
# - col3: type of argument to follow flag (0 = no argument, 1 = required argument, 2 = optional argument.)
# - col4: data type to which the flag argument shall be cast (logical, integer, double, complex, character)
# - col5: to add a brief description of the purpose of the option
spec <- matrix(
	c('loadPredMethod', 'lpm', 1, 'character',# 'the prediction method to be used (LM, GAM, NN, RF)',
	'loadFormula', 'lf', 2, 'integer',# load formula for GAM & LM
	'loadGamma', 'lg', 2, 'logical',# 'use gamma distribution for GAM method
	'loadHiddenUnits', 'lhu', 2, 'integer',# 'number of units in single hidden layer (nnet package)',
	'loadNtrees', 'lnt', 2, 'integer',# 'number of trees in RF'
	'tempPredMethod', 'tpm', 1, 'character', # temperature prediction method
	'tempFormula', 'tf', 2, 'integer', 
	'tempGamma', 'tg', 2, 'logical',
	'tempHiddenUnits', 'thu', 2, 'integer',# 'number of units in single hidden layer (nnet package)',
	'tempNtrees', 'tnt', 2, 'integer',
	'tempPCA', 'tp', 2, 'logical', # use principal component of weather stations as opposed to average
	'tempStation', 'ts1', 2, 'integer'), # use specific weather station as opposed to principal component or average
byrow=TRUE, nrow=12, ncol=4);
spec.dim=dim(spec)
spec.opt.long=spec[,1]
spec.opt.short=spec[,2]
spec.opt.type=spec[,3]

opt <- getopt(spec)

if(is.null(opt$loadPredMethod)) {
	#get the script name (only works when invoked with Rscript).
	self = commandArgs()[1];
	#print a friendly message and exit with a non-zero error code
	cat(paste("Usage: ", self, " -(-loadModel|lm) <(1-3)> -(-predTemp|pt) <0/1> -(-htype|ht) <(0-2)> -(-units|un) <(0-9)+> -(-predMethod|pm) <(LM|GAM|NN|RF)> [-(-hidden|hu) <(0-9)+>] [-(-ntree|nt) <(0-9)+>] \n",sep=""));
	stop("Required argument missing")
}

dir.exists <- function(path) FALSE

### ASSIGN SCRIPT VARIABLES ###
#** LOAD **#
load.method <- opt$loadPredMethod

load.method.option <- "NONE"
load.gamma <- FALSE
if(!is.null(opt$loadGamma)) {
	load.gamma <- opt$loadGamma
	load.method.option <- "gamma"
}
load.ntrees <- 50
if(!is.null(opt$loadNtrees)) {
	load.ntrees <- opt$loadNtrees
	load.method.option <- paste0("ntrees_", load.ntrees)
}
load.hidden.units <- 30
if(!is.null(opt$loadHiddenUnits)) {
	load.hidden.units <- opt$loadHiddenUnits
	load.method.option <- paste0("hidden-units_", load.hidden.units)
}
load.formula <- "NONE"
if(!is.null(opt$loadFormula)) {
	load.formula <- opt$loadFormula
}


#** TEMP **#
temp.method <- opt$tempPredMethod
temp.method.option <- "NONE"
temp.method.options <- list()
# fill through filesystem

temp.formula <- "NONE"
if(!is.null(opt$tempFormula)) {
	temp.formula <- opt$tempFormula
}
temp.gamma <- FALSE
if(!is.null(opt$tempGamma)) {
	temp.gamma <- opt$tempGamma
	temp.method.option <- "gamma"
}
temp.ntrees <- -1
if(!is.null(opt$tempNtrees)) {
	temp.ntrees <- opt$tempNtrees	
	temp.method.option <- underscoreJoin("ntrees", temp.ntrees)
}
temp.hidden.units <- -1
if(!is.null(opt$tempHiddenUnits)) {
	temp.hidden.units <- opt$tempHiddenUnits
	temp.method.option <- underscoreJoin("hidden-units", temp.hidden.units)
}
temp.maxit <- 1000
if(!is.null(opt$tempMaxit)) {
	temp.maxit <- opt$tempMaxit
}
temp.PCA <- FALSE
if(!is.null(opt$tempPCA)) {
	temp.PCA <- TRUE
	temp.method <- underscoreJoin(temp.method, "PCA")
}
temp.station <- FALSE
if(!is.null(opt$tempStation)) {
	temp.station <- opt$tempStation
	temp.method <- underscoreJoin(temp.method, paste0("Station", temp.station))
}

if(grepl('(RF|NN)', temp.method)) {
	temp.method.option <- "TRUE"
}
### SOURCE DEPENDENCIES ###
source("config.R")
source("util/error_func.R")
source("util/date_helpers.R")
source("util/plot_helpers.R")
source("util/preprocess.R")
source("models/load/load_model.R")
source("models/load/temp/temp_model.R")
source("models/load/feature_helpers.R")
source("util/CV_helpers.R")
source("util/method-formulas.R")
###########################

# TODO: change this part
global.htype <- 2
htype <- 2
units <- 1
pred.type <- getPredictionType(htype, units)

str.htype <- htypeToString(htype)
#


### READ LEADERBOARD ###
leaderboard.path <- "data/load/competition-stats/leaderboard.csv"
firstpos_benchmark.path <- "data/load/competition-stats/firstpos_benchmark.csv"
leaderboard <- read.csv(leaderboard.path, header=TRUE, row.names=1, sep=",")
firstpos_benchmark <- read.csv(firstpos_benchmark.path, header=TRUE, sep=",")
#########################

in.path <- "data/load/train"
out.path <- "output/test/CV"

# TODO: move to tempPrediction.R
### PREPARE DATA ###
train.df <- createTrainDF(loadCSVs(in.path), first.dt, last.dt)
# TODO: make sure CSVs are read in the right order
train.df.fn <- paste0("train-df", ".rds")
saveRDS(train.df, file=train.df.fn, compress=TRUE)

#** LOAD **#
load.df <- reduceToLoadDF(train.df)
# TODO: make sure CSVs are read in the right order
load.df.fn <- paste0("load-df", ".rds")
saveRDS(load.df, file=load.df.fn, compress=TRUE)

hash <- hashDtYear(load.df$TMS)
load.df <- cbind(load.df, HASH=hash)
load.df.fn <- paste0("load-df-hash", ".rds")
saveRDS(load.df, file=load.df.fn, compress=TRUE)

# remove rows with NA load entries
start.index <- which(load.df$HASH == hashDtYear(addYears(first.dt, 4)))
load.df <- load.df[start.index:nrow(load.df),]
load.df.fn <- paste0("load-df-omit", ".rds")
saveRDS(load.df, file=load.df.fn, compress=TRUE)
#####################

# TODO: move definition of dates to config

### CREATE LOAD FOLDER STRUCTURE ###
base.path <- createBaseFolder(out.path, test.start.dt, test.stop.dt)

#load.features.path <- createFolder(base.path, "load-features")
# Deals with options etc.
load.method.path <- createMethodFolder(base.path, "load", load.method, load.method.option, load.formula, temp.PCA, temp.station)
load.model.instances.path <- createFolder(load.method.path, "load-model-instances")
load.plot.path <- createFolder(load.method.path, "plots")
load.scores.path <- createFolder(load.method.path, "scores")
load.fits.path <- createFolder(load.method.path, "fits")

load.features.path <- createFolder(load.method.path, "load-features")

#** COPY PLOT SCRIPTS **#
file.copy(from='plotLoadResults.R', to=load.plot.path)
file.copy(from='util/plot_helpers.R', to=load.plot.path)
file.copy(from='config.R', to=load.plot.path)


### SAVE FULL LOAD FEATURES FOR VERIFICATION ###
features.fn <- extensionJoin("full-load-features", "rds")
curr.features.path <- pathJoin(load.features.path, features.fn)
if(file.exists(curr.features.path)) {
	load.features <- readRDS(curr.features.path)
} else {
	load.features <- createLoadFeatures(load.df, load.train.dt, load.train.month.len + test.month.len)
	saveRDS(load.features, file=curr.features.path, compress=TRUE)
}


## FETCH TEMP PATHS ##
temp.path <- pathJoin(base.path, "temp")
temp.method.path <- pathJoin(temp.path, temp.method)

no.temp.formula <- FALSE
if(grepl("(MEAN|TRUE)", temp.method)) {
	intervals <- list(temp.method)
	no.temp.formula <- TRUE
} else {
	intervals <- list("monthly", "weekly")
}

true.path <- pathJoin(temp.path, "TRUE")
temp <- readRDS(pathJoin(true.path, "true-temperature.rds"))
index <- which(temp$HASH==hashDtYear(load.train.start.dt))
cut.temp <-temp[-c(index:nrow(temp)), ]

#* FIND OPTION PATHS *#
# get list for paths -> get list for formulas
list.of.lists <- getTempMethodPaths(temp.method.path, temp.method, temp.method.option, no.temp.formula)
temp.method.options.paths <- list.of.lists[[1]] 
temp.method.options <- list.of.lists[[2]] 
# hash table for formulas would be most convenient solution
temp.method.formulas.paths <- list.of.lists[[3]] 
temp.method.formulas <- list.of.lists[[4]] 
#if(temp.method.option != "NONE") {
#	temp.method.options <- list(temp.method.option)
#}

load.method.formula <- load.methods.formulas[[load.method]][[load.formula]]
print(temp.method.options)

for(i in 1:length(temp.method.options)) {
	curr.temp.method.option <- temp.method.options[[i]]
	print(curr.temp.method.option)
	temp.formulas <- temp.method.formulas[[curr.temp.method.option]]
	temp.formula.paths <- temp.method.formulas.paths[[curr.temp.method.option]]
	for(j in 1:length(temp.formulas)) {
		temp.formula <- temp.formulas[[j]]
		temp.formula.path <- temp.formula.paths[[j]]
		for(p in 1:length(intervals)) {
			print(curr.temp.method.option)
			print(temp.formula.path)
			load.temp.scores.path <- pathJoin(load.scores.path, temp.method)
			load.temp.fits.path <- pathJoin(load.fits.path, temp.method)
			createDir(load.temp.scores.path)
			createDir(load.temp.fits.path)
			print(load.temp.scores.path)
			print(load.temp.fits.path)
			res <- list()
			
			PINBALL <- c()
			POSITIONS <- c()
			if(grepl("(GAM|LM)", curr.temp.method.option)) {
				weekly.scores.fn <- extensionJoin(paste("weekly", "load-fit-scores", load.method, "?", curr.temp.method.option, paste0("formula", j), intervals[[p]], sep="_"), "csv")
				monthly.scores.fn <- extensionJoin(paste("monthly", "load-fit-scores", load.method, "?", curr.temp.method.option, paste0("formula", j), intervals[[p]], sep="_"), "csv")
			} else if(no.temp.formula) {
				weekly.scores.fn <- extensionJoin(paste("weekly", "load-fit-scores", load.method, "?", temp.method, sep="_"), "csv")
				monthly.scores.fn <- extensionJoin(paste("monthly", "load-fit-scores", load.method, "?", temp.method, sep="_"), "csv")
			} else {
				weekly.scores.fn <- extensionJoin(paste("weekly", "load-fit-scores", load.method, "?", temp.method, curr.temp.method.option, paste0("formula", j), intervals[[p]], sep="_"), "csv")
				monthly.scores.fn <- extensionJoin(paste("monthly", "load-fit-scores", load.method, "?", temp.method, curr.temp.method.option, paste0("formula", j), intervals[[p]], sep="_"), "csv")
			}
			weekly.scores.path <- pathJoin(load.temp.scores.path, weekly.scores.fn)
			monthly.scores.path <- pathJoin(load.temp.scores.path, monthly.scores.fn)

			writeLoadHeader(weekly.scores.path, load.method, load.method.formula, "weekly", load.train.month.len, str.htype, load.train.start.dt)
			writeLoadHeader(monthly.scores.path, load.method, load.method.formula, "monthly", load.train.month.len, str.htype, load.train.start.dt)

			print(temp.formula.path)
			if(no.temp.formula) {
				pattern <- temp.method
				temp.method.file <- dir(temp.method.path, pattern='\\.rds', full.names = TRUE)
			} else {
				temp.files <- dir(temp.formula.path, pattern='^predtrain(.*)\\.rds', full.names = TRUE)
				pattern <- paste0("formula", j, "_", intervals[[p]])
				temp.method.file <- temp.files[grepl(pattern, temp.files)]
			}
			print(temp.method.file)
			pred.temp <- readRDS(temp.method.file)
			temp.train.len <- attr(pred.temp, "train.len")
			print(temp.train.len)
			
			writeLoadTempHeader(weekly.scores.path, no.temp.formula, temp.method, temp.formula, pred.temp, intervals[[p]])
			writeLoadTempHeader(monthly.scores.path, no.temp.formula, temp.method, temp.formula, pred.temp, intervals[[p]])

			if(grepl("(MEAN|TRUE)", temp.method)) {
				temp <- pred.temp
			} else {
				pred.temp <- pred.temp[, c(1,2,4)]
				colnames(pred.temp) <- c("TMS", "MTEMP", "HASH")
				temp <- rbind(cut.temp, pred.temp)
			}
			rownames(temp) <- NULL
			

			date.period <- paste0(as.character(as.Date(test.start.dt)), "-", as.character(as.Date(test.stop.dt)))
			print(date.period)
			if(grepl("(GAM|LM)", curr.temp.method.option)) {
				all.weekly.fits.fn <- extensionJoin(paste("weekly", "load-fit-series", load.method, date.period, "all", "?", "temp-model", temp.method, paste0("formula", j), intervals[[p]], sep="_"), "rds")
				all.monthly.fits.fn <- extensionJoin(paste("monthly", "load-fit-series", load.method, date.period, "all", "?", "temp-model", temp.method, paste0("formula", j), intervals[[p]], sep="_"), "rds")
			} else if(no.temp.formula) {
				all.weekly.fits.fn <- extensionJoin(paste("weekly", "load-fit-series", load.method, date.period, "all", "?", "temp-model", temp.method, sep="_"), "rds")
				all.monthly.fits.fn <- extensionJoin(paste("monthly", "load-fit-series", load.method, date.period, "all", "?", "temp-model", temp.method, sep="_"), "rds")
			} else {
				all.weekly.fits.fn <- extensionJoin(paste("weekly", "load-fit-series", load.method, date.period, "all", "?", "temp-model", temp.method, curr.temp.method.option, paste0("formula", j), intervals[[p]], sep="_"), "rds")
				all.monthly.fits.fn <- extensionJoin(paste("monthly", "load-fit-series", load.method, date.period, "all","?", "temp-model", temp.method, curr.temp.method.option, paste0("formula", j), intervals[[p]], sep="_"), "rds")
			}
			all.weekly.path <- pathJoin(load.temp.fits.path, all.weekly.fits.fn)
			all.monthly.path <- pathJoin(load.temp.fits.path, all.monthly.fits.fn)

			loop.vars = list(c(2,1),
						c(1,1),
						c(1,2),
						c(1,3),
						c(1,4))

			for (ml in 1:test.month.len) {
				# LOOP: CREATE MONTHLY LOAD PREDICTION (DLAG = 35), CREATE PREDICTION FOR FIRST WEEK (DLAG = 7), SECOND WEEK (DLAG = 14), THIRD WEEK (DLAG = 21), FOURTH WEEK (DLAG = 28)
				for (h in 1:length(loop.vars)) { 
					var.set <- loop.vars[[h]]
					htype = var.set[1]
					test.horizon = 1
					lag = var.set[2]

					pred.type <- getPredictionType(htype, test.horizon)

					print(tail(temp))
					print(load.train.month.len)
					#** GET FEATURES FOR CURRENT TRAIN AND TEST PERIODS **#
					# - lag for training changes
					load.train.features <- assembleLoadFeatures(load.features, temp, load.train.dt, lag, load.train.month.len, 2)
					# - htype for test horizon changes
					load.test.features <- assembleLoadFeatures(load.features, temp, test.dt, lag, test.horizon, htype)

					train.features.fn <- extensionJoin(paste("train-load-features", paste0("start", as.character(as.Date(load.train.dt))), "instance", ml, sep="_"), "rds")
					train.features.path <- pathJoin(load.features.path, train.features.fn)
					saveRDS(load.train.features, file=train.features.path, compress = TRUE)

					test.features.fn <- extensionJoin(paste("test-load-features", paste0("start", as.character(as.Date(test.dt))), "instance", ml, sep="_"), "rds")
					test.features.path <- pathJoin(load.features.path, test.features.fn)
					saveRDS(load.test.features, file=test.features.path, compress = TRUE)
					
					#** CREATE & SAVE LOAD MODEL **#
					if(load.method == "LM") {
						train.result <- trainLoadModelFormulaLM(load.train.features[,-1], load.method.formula, load.train.dt)
					} else if(load.method == "GAM") {
						train.result <- trainLoadModelFormulaGAM(load.train.features[,-1], load.method.formula, load.train.dt, load.gamma)
					} else if(load.method == "NN") {
						train.result <- trainLoadModelFormulaNN(load.train.features[,-1], load.method.formula, load.train.dt, load.hidden.units)
					} else if(load.method == "RF") {
						train.result <- trainLoadModelFormulaRF(load.train.features[,-1], load.method.formula, load.train.dt, load.ntrees)
					}
					load.model <- train.result[["model"]]  
					if(h==1) {
						if(grepl("(GAM|LM)", curr.temp.method.option)) {
							load.model.fn <- extensionJoin(paste("monthly", "model", load.method, "?", temp.method, paste0("formula", j), intervals[[p]], pred.type, "instance", ml, sep="_"), "txt")
						} else if(no.temp.formula) {
							load.model.fn <- extensionJoin(paste("monthly", "model", load.method, "?", temp.method, intervals[[p]], pred.type, "instance", ml, sep="_"), "txt")
						} else {
							load.model.fn <- extensionJoin(paste("monthly", "model", load.method, "?", temp.method, curr.temp.method.option, paste0("formula", j), intervals[[p]], pred.type, "instance", ml, sep="_"), "txt")
						}
					} else {
						if(grepl("(GAM|LM)", curr.temp.method.option)) {
							load.model.fn <- extensionJoin(paste("weekly", "model", load.method, "?", temp.method, paste0("formula", j), intervals[[p]], pred.type, "instance", ml, sep="_"), "txt")
						} else if(no.temp.formula) {
							load.model.fn <- extensionJoin(paste("weekly", "model", load.method, "?", temp.method, intervals[[p]], pred.type, "instance", ml, sep="_"), "txt")
						} else {
							load.model.fn <- extensionJoin(paste("weekly", "model", load.method, "?", temp.method, curr.temp.method.option, paste0("formula", j), intervals[[p]], pred.type, "instance", ml, sep="_"), "txt")
						}
					}
					capture.output(summary(load.model), file=pathJoin(load.model.instances.path,load.model.fn))
					
					#** CREATE PREDICTION **#
					if(load.method == "LM") {
						load.fit <- predict.lm(load.model, load.test.features[, -(1:2)])
						len.fit <- length(load.fit)
					} else if(load.method == "GAM") {
						load.fit <- predict.gam(load.model, load.test.features[, -(1:2)])
						len.fit <- nrow(load.fit)
					} else if(load.method == "NN") {
						load.fit <- as.vector(predict(load.model, load.test.features[, -(1:2)]))
						len.fit <- length(load.fit)
					} else if(load.method == "RF") {
						load.fit <- predict(load.model, load.test.features[, -(1:2)])
						len.fit <- length(load.fit)
					}

					#** USE TRAINING RESIDUALS TO COMPUTE PREDICTION QUANTILES **#
					train.residuals <- train.result[["residuals"]]
					test.quantiles <- createPredQuantiles(load.fit, train.residuals)

					if(grepl("(GAM|LM)", curr.temp.method.option)) {
						monthly.res.fn <- extensionJoin(paste("monthly", "load-fit-series", load.method, as.Date(test.dt), "?", "temp-model", temp.method, paste0("formula", j), intervals[[p]], "instance", ml, sep="_"), "rds")
					} else if(no.temp.formula) {
						monthly.res.fn <- extensionJoin(paste("monthly", "load-fit-series", load.method, as.Date(test.dt), "?", "temp-model", temp.method, "instance", ml, sep="_"), "rds")
					} else {
						monthly.res.fn <- extensionJoin(paste("monthly", "load-fit-series", load.method, as.Date(test.dt),"?", "temp-model", temp.method, curr.temp.method.option, paste0("formula", j), intervals[[p]], "instance", ml, sep="_"), "rds")
					}
					monthly.path <- pathJoin(load.temp.fits.path, monthly.res.fn)


					#** SAVE RESULTS **#
					# - save monthly prediction
					load.res.row <- cbind(TMS=as.character(load.test.features$TMS), FIT=load.fit, TARGET=load.test.features$Y)
					if(h==1) {
						if(ml==1) monthly.load.res <- load.res.row else monthly.load.res <- rbind(monthly.load.res, load.res.row)
						first.rest <- 4*7*24+1
						restl <- FALSE
						if(first.rest < nrow(load.res.row)) {
							restl <- TRUE
							rest.days <- c(first.rest:nrow(load.res.row))
							rest <- load.res.row[rest.days,]
						}
						attr(load.res.row, 'htype') <- htypeToString(htype)
						attr(load.res.row, 'horizon') <- test.horizon 
						attr(load.res.row, 'method') <- load.method
						if(load.method.option != "NONE") {
							attr(load.res.row, 'option') <- load.method.option
						}	
						attr(load.res.row, 'formula') <- load.method.formula
						saveRDS(load.res.row, file=monthly.path, compress=TRUE)
					# - save weekly prediction
					} else {
						if (ml == 1 && h==2) weekly.load.res <- load.res.row else weekly.load.res <- rbind(weekly.load.res, load.res.row)
						if (h==5 && restl == TRUE) weekly.load.res <- rbind(weekly.load.res, rest)
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
						if (ml == 1) res[[h]] <- res.row else res[[h]] <- rbind(res[[h]], res.row)
						rest.test.dt <- incrementDt(test.dt, 4, 1)
						tms.row <- cbind(LOAD.TRAIN.TMS=as.character(load.train.dt), TEST.TMS=as.character(rest.test.dt))
						err.row <- cbind(do.call(cbind.data.frame, rest.err.scores), PINBALL=rest.err.pinball)
						res.row <- cbind(tms.row, err.row)
						cat("length loop.vars", length(loop.vars), sep="\n")
						hp <- h + 1
						if (ml== 1) res[[hp]] <- res.row else res[[hp]] <- rbind(res[[hp]], res.row)
					} else {
						hp <- h + 1
						if (ml == 1) res[[hp]] <- res.row else res[[hp]] <- rbind(res[[hp]], res.row)
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
			print(res)

			## TODO: save options as well
			attr(weekly.load.res, 'htype') <- htypeToString(htype)
			attr(weekly.load.res, 'horizon') <- test.month.len 
			attr(weekly.load.res, 'load_prediction_interval') <- "weekly"
			attr(weekly.load.res, 'load_method') <- load.method
			attr(weekly.load.res, 'load_method_option') <- load.method.option
			attr(weekly.load.res, 'load_train_len') <- load.train.month.len
			attr(weekly.load.res, 'load_formula') <- load.method.formula
			attr(weekly.load.res, 'load_formula_index') <- load.formula
			attr(weekly.load.res, 'temp_method') <- temp.method
			attr(weekly.load.res, 'temp_method_option') <- curr.temp.method.option 
			attr(weekly.load.res, 'temp_method_formula') <- temp.formula
			attr(weekly.load.res, 'temp_method_formula_index') <- j 
			attr(weekly.load.res, 'temp_prediction_interval') <- intervals[[p]]
			attr(weekly.load.res, 'temp_train_len') <- temp.train.len
			saveRDS(weekly.load.res, file=all.weekly.path, compress=TRUE)

			attr(monthly.load.res, 'htype') <- htypeToString(htype)
			attr(monthly.load.res, 'horizon') <- test.month.len 
			attr(monthly.load.res, 'load_prediction_interval') <- "monthly"
			attr(monthly.load.res, 'load_method') <- load.method
			attr(monthly.load.res, 'load_method_option') <- load.method.option
			attr(monthly.load.res, 'load_train_len') <- load.train.month.len
			attr(monthly.load.res, 'load_formula') <- load.method.formula
			attr(monthly.load.res, 'load_formula_index') <- load.formula
			attr(monthly.load.res, 'temp_method') <- temp.method
			attr(monthly.load.res, 'temp_method_option') <- curr.temp.method.option 
			attr(monthly.load.res, 'temp_method_formula') <- temp.formula 
			attr(monthly.load.res, 'temp_method_formula_index') <- j 
			attr(monthly.load.res, 'temp_prediction_interval') <- intervals[[p]]
			attr(monthly.load.res, 'temp_train_len') <- temp.train.len
			saveRDS(monthly.load.res, file=all.monthly.path, compress=TRUE)

			score.board <- list()
			for(h in 1:(length(loop.vars)+1)) {
				res.last.row <- cbind(LOAD.TRAIN.TMS=as.character(load.train.start.dt), TEST.TMS=as.character(last.test.dt), RMSE=mean(res[[h]]$RMSE), MAE=mean(res[[h]]$MAE), MAPE=mean(res[[h]]$MAPE), PINBALL=mean(res[[h]]$PINBALL))
				score.board[[h]] <- rbind(res[[h]], res.last.row)
				row.names(score.board[[h]]) <- c(c(1:test.month.len), "CV MEAN")
			}

			position.board <- cbind(MAPE=res[[1]]$MAPE, PINBALL=res[[1]]$PINBALL, POSITION=POSITIONS, PINBALL_FIRSTPOS=firstpos_benchmark[1:test.month.len, ])
			dates <- res[[1]][1:(nrow(res[[1]])), c(1,2)]
			position.board <- cbind(dates, position.board)
			row.names(position.board) <- c(1:test.month.len)#, "MODEL CV MEAN")

			### LATER: DO STATS AND RECOMPUTE 
			for(h in 1:(length(loop.vars)+1)) {
				appendTableToFile(score.board[[h]], weekly.scores.path)
				appendTableToFile(score.board[[h]], monthly.scores.path)
				cat("\n", file = weekly.scores.path, append = TRUE)
				cat("\n", file = monthly.scores.path, append = TRUE)
				if (h==1) {
					if(grepl("(GAM|LM)", curr.temp.method.option)) {
						scores.fn <- extensionJoin(paste("weekly", "load-fit-scores", "1m", load.method, date.period, "all", "?", "temp-model", temp.method, paste0("formula", j), intervals[[p]], sep="_"), "rds")
					} else if(no.temp.formula) {
						scores.fn <- extensionJoin(paste("weekly", "load-fit-scores", "1m", load.method, date.period, "all", "?", "temp-model", temp.method, sep="_"), "rds")
					} else {
						scores.fn <- extensionJoin(paste("weekly", "load-fit-scores", "1m", load.method, date.period, "all", "?", "temp-model", temp.method, curr.temp.method.option, paste0("formula", j), intervals[[p]], sep="_"), "rds")
					}					
					col <- position.board[1:test.month.len, 1:6]
					comp <- col
				} else if (h==2) {
					if(grepl("(GAM|LM)", curr.temp.method.option)) {
						scores.fn <- extensionJoin(paste("weekly", "load-fit-scores", "5wrest", load.method, date.period, "all", "?", "temp-model", temp.method, paste0("formula", j), intervals[[p]], sep="_"), "rds")
					} else if(no.temp.formula) {
						scores.fn <- extensionJoin(paste("weekly", "load-fit-scores", "5wrest", load.method, date.period, "all", "?", "temp-model", temp.method, sep="_"), "rds")
					} else {
						scores.fn <- extensionJoin(paste("weekly", "load-fit-scores", "5wrest", load.method, date.period, "all", "?", "temp-model", temp.method, curr.temp.method.option, paste0("formula", j), intervals[[p]], sep="_"), "rds")
					}	
				} else {
					var.set <- loop.vars[[h-1]]
					htype = var.set[1]
					test.horizon = var.set[2]
					if(grepl("(GAM|LM)", curr.temp.method.option)) {
						scores.fn <- extensionJoin(paste("weekly", "load-fit-scores", getPredictionType(htype, test.horizon), load.method, date.period, "all", "?", "temp-model", temp.method, paste0("formula", j), intervals[[p]], sep="_"), "rds")
					} else if(no.temp.formula) {
						scores.fn <- extensionJoin(paste("weekly", "load-fit-scores", getPredictionType(htype, test.horizon), load.method, date.period, "all", "?", "temp-model", temp.method, sep="_"), "rds")
					} else {
						scores.fn <- extensionJoin(paste("weekly", "load-fit-scores", getPredictionType(htype, test.horizon), load.method, date.period, "all", "?", "temp-model", temp.method, curr.temp.method.option, paste0("formula", j), intervals[[p]], sep="_"), "rds")
					}	
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
				saveRDS(score.board[[h]], file=paste(load.temp.scores.path, scores.fn, sep="/"), compress=TRUE)
			}
			appendTableToFile(position.board, weekly.scores.path)
			cat("\n", file = weekly.scores.path, append = TRUE)
			appendTableToFile(position.board, monthly.scores.path)
			cat("\n", file = monthly.scores.path, append = TRUE)
			
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
			means.row <- colMeans(comparison.board[, 3:ncol(comparison.board), drop=FALSE], na.rm=TRUE)
			print(means.row)
			comparison.means.row <- cbind(TRAIN.TMS=as.character(load.train.start.dt), TEST.TMS=as.character(last.test.dt), t(data.frame(means.row)))
			print(comparison.means.row)
			colnames(comparison.means.row) <- c("TRAIN.TMS", "TEST.TMS", "MAPE_1m", "PINBALL_1m", "POS_1m", "PINBALL_#1", "MAPE_1w", "PINBALL_1w", "MAPE_2w", "PINBALL_2w", "MAPE_3w", "PINBALL_3w", "MAPE_4w", "PINBALL_4w", "PINBALL_wAVG", "POS_wAVG")
			print(comparison.means.row)
			comparison.board <- rbind(comparison.board, comparison.means.row)
			print(comparison.board)
			row.names(comparison.board) <- c(c(1:test.month.len), "CV MEAN")
			print(comparison.board)

			attr(comparison.board, 'htype') <- htypeToString(htype)
			attr(comparison.board, 'horizon') <- test.month.len 
			attr(comparison.board, 'load_method') <- load.method
			attr(comparison.board, 'load_method_option') <- load.method.option
			attr(comparison.board, 'load_formula') <- load.method.formula
			attr(comparison.board, 'load_formula_index') <- load.formula
			attr(comparison.board, 'load_train_len') <- load.train.month.len
			attr(comparison.board, 'temp_method') <- temp.method
			attr(comparison.board, 'temp_method_option') <- curr.temp.method.option 
			attr(comparison.board, 'temp_method_formula') <- temp.formula
			attr(comparison.board, 'temp_method_formula_index') <- j 
			attr(comparison.board, 'temp_prediction_interval') <- intervals[[p]]
			attr(comparison.board, 'temp_train_len') <- temp.train.len

			if(grepl("(GAM|LM)", curr.temp.method.option)) {
				comp.board.fn <- extensionJoin(paste("comparison_board", load.method, "?", curr.temp.method.option, paste0("formula", j), intervals[[p]], sep="_"), "rds")
			} else if(no.temp.formula) {
				comp.board.fn <- extensionJoin(paste("comparison_board", load.method, "?", temp.method, sep="_"), "rds")
			} else {
				comp.board.fn <- extensionJoin(paste("comparison_board", load.method, "?", temp.method, curr.temp.method.option, paste0("formula", j), intervals[[p]], sep="_"), "rds")
			}
			saveRDS(comparison.board, file=pathJoin(load.temp.scores.path, comp.board.fn), compress=TRUE)
			
			### IMPORTANT: RESET DATES ###
			temp.train.dt <- temp.train.start.dt
			load.train.dt <- load.train.start.dt
			test.dt <- test.start.dt

			htype <- global.htype
			test.horizon <- units
			###############

			cat("\n", file = weekly.scores.path, append = TRUE)
			cat("\n", file = weekly.scores.path, append = TRUE)
			cat("\n", file = monthly.scores.path, append = TRUE)
			cat("\n", file = monthly.scores.path, append = TRUE)
		}
	}
}
