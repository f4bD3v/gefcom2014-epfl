source("~/github_repos/gefcom2014-epfl/util/plot_helpers.R")
source("~/github_repos/gefcom2014-epfl/config.R")
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
	end.date <- tms[nrow(df)]

	#** plotPredictionResiduals
	# - tms, fit, target, residuals,
	# - chunks
	# - xlabel, ylabel, title
	# xlabel = paste(start.date, "-", end.date, sep=" ")
	plotPredictionResiduals(tms, target, fit, residuals, chunks, "Time in Hours", "Hourly Temperature in Fahrenheit", "Temperature Prediction", c(0, 120))
}


plotProcessing <- function(dir.path, files) {
	print(dir.path)
	setwd(dir.path)
	#print(getwd())
	for(rds.file in files) {
		#rds.file <- substring(rds.file, 3, nchar(rds.file))
		#rds.file <- gsub('?', '\?', rds.file)
		#rds.file <- paste0("./", rds.file)
		print(rds.file)
		path <- paste(getwd(), rds.file, sep="/")
		print(path)

		#res.df <-readRDS(system.file("help", rds.file, package="MASS"))
		res.df <- readRDS(path)
		#print(res.df)
		htype <- attr(res.df, 'htype')
		horizon <-attr(res.df, 'horizon')
		print(horizon)

		tms <- as.POSIXct(res.df[, 1], origin="1970-01-01", tz="EST")
		start.date <- as.Date(tms[1])
		end.date <- as.Date(tms[nrow(res.df)])

		res.df <- data.frame(TMS=tms, FIT=as.numeric(res.df[,2]), TARGET=as.numeric(res.df[,3]))
		#res.df <- data.matrix(res.df, rownames.force=NA)
		#prefix <- substring(rds.file, 3, nchar(rds.file))
		#parts <- strsplit(path, "\\?")[[1]]
		#part1 <- parts[1]
		#prefix <- parts[1]
		#parts <- strsplit(part1, "/")[[1]]
		#part1 <- parts[1]
		#part2 <- parts[2]
		#part3 <- parts[3]

		#prefix <- substring(rds.file, 3, nchar(rds.file))
		parts <- strsplit(rds.file, "\\.")[[1]]
		filename <- parts[1]

		if(grepl('all', rds.file)) {
			#fn.pdf <- paste0(paste(part1, part2, paste("all-plots", part3, start.date, end.date, sep="_"), sep="/"), ".pdf")
			fn.pdf <- paste0("./plots/", paste("all-plots", filename, sep="_"), ".pdf")
			print(fn.pdf)
			pdf(file=fn.pdf, width=8, height=11)
			par(mfrow=c(3,4))

			# split up by month again
			plotRDS(res.df, htype, 13)	

			#call aggregate plotting
			dev.off()

		} else {
			#fn.pdf <- paste0(paste(part1, part2, paste("plot", part3, start.date, end.date, sep="_"), sep="/"), ".pdf")
			fn.pdf <- paste0("./plots/", paste("plot", filename, sep="_"), ".pdf")
			pdf(file=fn.pdf, width=8, height=11)
			par(mfrow=c(2,2))

			plotRDS(res.df, htype, horizon)	

			dev.off()
			# call standard plotting
		}
	}
}

plotFromFormulaPath <- function(form.dir, method, option="NONE") {
	parts <- strsplit(form.dir, "/")[[1]]
	formula <- parts[[length(parts)]]
    dir.create(paste0(form.dir, "/plots"), showWarnings = TRUE, recursive = FALSE)
	files <- dir(form.dir, pattern = '\\.rds', full.names = FALSE)
	files <- files[order(nchar(files), files)]
	plotProcessing(form.dir, files)
	if(option == "NONE") {
		setwd("../../")
	} else {
		setwd("../../../")
	}
}

path <- '.'
dirs <- list.dirs(path, full.names = TRUE, recursive=FALSE)
for(meth.dir in dirs) {
	print(meth.dir)
	parts <- strsplit(meth.dir, "/")[[1]]
	method <- parts[[length(parts)]]
	opt.dirs <- list.dirs(meth.dir, full.names = TRUE, recursive=FALSE)
    if(grepl("(TRUE|MEAN)", method)) {
        next
    }
	for(opt.dir in opt.dirs) {
		if(grepl('formula', opt.dir)) {
			print(opt.dir)
			plotFromFormulaPath(opt.dir, method)
		} else {
		#option
			parts <- strsplit(opt.dir, "/")[[1]]
			option <- parts[[length(parts)]]
			form.dirs <- list.dirs(opt.dir, full.names = TRUE, recursive=FALSE)
            print(form.dirs)
            for(form.dir in form.dirs) {
                if(grepl('formula', form.dir)) {
                    plotFromFormulaPath(form.dir, method, option)
                }
            }
		}
	}
}

