source("/home/fbrix/gefcom2014-epfl/util/plot_helpers.R")
source("/home/fbrix/gefcom2014-epfl/config.R")
require("lubridate")
# load all .rds files in directory

dir.exists <- function(path) FALSE

plotRDS <- function(df, htype, horizon) {
	tms <- df[, 1]
	fit <- df[, 2]
	target <- df[, 3]
	residuals <- fit - target 
	chunks <- 4*horizon

	start.date <- tms[1]
	end.date <- tms[nrow(res.df)]

	#** plotPredictionResiduals
	# - tms, fit, target, residuals,
	# - chunks
	# - xlabel, ylabel, title
	# xlabel = paste(start.date, "-", end.date, sep=" ")
	plotPredictionResiduals(tms, target, fit, residuals, chunks, "Time in Hours", "Utility Load in MW", "Load Prediction", c(40, 320))
}

path <- '.'
files <- dir(path, pattern = '\\.rds', full.names = TRUE)
files <- files[order(nchar(files), files)]

for(rds.file in files) {
	res.df <-readRDS(rds.file)
	htype <- attr(res.df, 'htype')
	horizon <-attr(res.df, 'horizon')
	print(horizon)

 	tms <- as.POSIXct(res.df[, 1], origin="1970-01-01", tz="EST")
	start.date <- as.Date(tms[1])
	end.date <- as.Date(tms[nrow(res.df)])

	res.df <- data.frame(TMS=tms, FIT=res.df[,2], TARGET=res.df[,3])

	prefix <- substring(rds.file, 3, nchar(rds.file))
	parts <- strsplit(prefix, "\\?")[[1]]
	prefix <- parts[1]

	if(grepl('all', rds.file)) {
		fn.pdf <- paste0(paste("all-plots", prefix, start.date, end.date, sep="_"), ".pdf")
		pdf(file=fn.pdf, width=8, height=11)
		par(mfrow=c(3,4))

		# split up by month again
		plotRDS(res.df, htype, 13)	

		#call aggregate plotting
		dev.off()

	} else {
		fn.pdf <- paste0(paste("plot", prefix, start.date, end.date, sep="_"), ".pdf")
		pdf(file=fn.pdf, width=8, height=11)
		par(mfrow=c(2,2))

		plotRDS(res.df, htype, horizon)	

		dev.off()
		# call standard plotting
	}
}


