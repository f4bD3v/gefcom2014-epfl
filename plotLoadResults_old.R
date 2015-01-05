source("util/plot_helpers.R")
# load all .rds files in directory

dir.exists <- function(path) FALSE

plotRDS <- function(df, htype, horizon) {
	fit <- df[, 2]
	target <- df$TARGET
	residuals <- fit - target 
	chunks <- 4
	title <- '' 

	start.date <- df$TMS[1]
	end.date <- df$TMS[nrow(df)]

	#** plotPredictionResiduals
	# - tms, fit, target, residuals,
	# - chunks
	# - xlabel, ylabel, title
	plotPredictionResiduals(df$TMS, fit, target, residuals, paste(start.date, "-", end.date, sep=" "), "Utility Load in MW", "Prediction")
}

path <- '.'
files <- dir(path, pattern = '\\.rds', full.names = TRUE)
print(files)
files <- files[order(nchar(files), files)]
print(files)

for(rds.file in files) {
	res.df <-readRDS(rds.file)
	htype <- attr(res.df, 'htype')
	horizon <-attr(res.df, 'horizon')

	start.date <- res.df$TMS[1]
	end.date <- res.df$TMS[nrow(res.df)]

	prefix <- substring(rds.file, 1, 10)
	print(prefix)

	if(grepl(rds.file, 'all')) {
		fn.pdf <- paste0(paste("all-plots", prefix, start.date, end.date, sep="_"), ".pdf")
		pdf(file=fn.pdf, width=8, height=11)
		par(mfrow=c(3,4))
		#for(i in 1:horizon) {
		#	plotRDS(res.df, htype, horizon)	
		#}
		# call aggregate plotting
	} else {
		fn.pdf <- paste0(paste("plot", prefix, start.date, end.date, sep="_"), ".pdf")
		pdf(file=fn.pdf, width=8, height=11)
		par(mfrow=c(2,2))

		plotRDS(res.df, htype, horizon)	
		# call standard plotting
	}

	dev.off()
}


