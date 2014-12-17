#require('WriteXLS')
require('XLConnect')
require('methods')

write_list <-function(my_list, list.names, wb_name = 'method-comparison.xlsx') {    
	wb <- loadWorkbook(wb_name, create = TRUE)
	createSheet(wb, list.names)
	writeWorksheet(wb, my_list, list.names,header=FALSE)
	saveWorkbook(wb)
}

path <- '.'
dirs <- list.dirs(path, full.names = TRUE, recursive=FALSE)
print(dirs)

data.frames <- list()
data.names <- list()
count <- 1
for(dir in dirs) {
	#dir <- substring(dir, 3, nchar(dir))
	# gam - gamma later
	print(dirs)
	if(grepl('gam', dir) || grepl('lm', dir)) {
		scores.path <- paste(dir, "scores", sep="/")
		print(scores.path)
		files <- dir(scores.path, pattern = 'comparison', full.names = TRUE)
		files <- files[order(nchar(files), files)]
		for(file in files) {
			data.frames[[count]] <- readRDS(file)
			parts <- strsplit(file, "/")
			print(parts)
			#### WRONG ####
			fne <- parts[[length(parts)]]
			fn <- strsplit(fne, "\\.")[1]
			data.names[[count]] <- paste(gsub('/', '_', gsub('\\.', '', dir)), fn, sep="/")
			count <- count + 1
		}
	} else if(grepl('nnet', dir) || grepl('randomforest', dir)) {
		sub.dirs <- list.dirs(dir, full.names = TRUE, recursive=FALSE)
		for(sub.dir in sub.dirs) {
			scores.path <- paste(sub.dir, "scores", sep="/")
			files <- dir(scores.path, pattern = 'comparison', full.names = TRUE)
			files <- files[order(nchar(files), files)]
			for(file in files) {
				data.frames[[count]] <- readRDS(file)
				parts <- strsplit(file, "/")
				fne <- parts[[length(parts)]]
				fn <- strsplit(fne, "\\.")[1]
				data.names[[count]] <- gsub('/', '_', gsub('\\.', '', sub.dir))
				count <- count + 1
			}
		}
	}
}

print(data.names)
write_list(data.frames, data.names)
#WriteXLS(data.frames, "method-comparison.XLS", perl = perl)
