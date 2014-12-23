#require('WriteXLS')
require('XLConnect')
require('methods')
# http://stackoverflow.com/questions/17623697/how-can-i-automate-data-frame-naming-in-r

write_list <-function(df.list, list.names, wb_name = 'method-comparison.xlsx') {    
	wb <- loadWorkbook(wb_name, create = TRUE)
	for(i in 1:length(list.names)) {
		createSheet(wb, list.names[[i]])
		writeWorksheet(wb, data=df.list[[i]], sheet=i, header=TRUE)
		saveWorkbook(wb)
	}
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
	if(grepl('gam', dir) || grepl('lm', dir)) {
		# add an extra layer for the model folders
		sub.dirs <- list.dirs(dir, full.names = TRUE, recursive=FALSE)
		for(sub.dir in sub.dirs) {
			scores.path <- paste(dir, "scores", sep="/")
			files <- dir(scores.path, pattern = 'comparison', full.names = TRUE)
			files <- files[order(nchar(files), files)]
			for(file in files) {
				data.frames[[count]] <- readRDS(file)
				parts <- strsplit(file, "/")[[1]]
				fne <- parts[[length(parts)]]
				fn <- strsplit(fne, "\\.")[[1]][1]
				fn <- gsub('comparison_board_', '', fn)
				fn <- gsub('hidden-units', 'hu', fn)
				label <- paste(gsub('/', '_', gsub('\\./', '', sub.dir)), fn, sep="_")
				print(label)
				# remove board tempm
				data.names[[count]] <- label
				count <- count + 1
			}
		}
	} else if(grepl('nnet', dir) || grepl('randomforest', dir)) {
		sub.dirs <- list.dirs(dir, full.names = TRUE, recursive=FALSE)
		for(sub.dir in sub.dirs) {
			scores.path <- paste(sub.dir, "scores", sep="/")
			files <- dir(scores.path, pattern = 'comparison', full.names = TRUE)
			files <- files[order(nchar(files), files)]
			for(file in files) {
				data.frames[[count]] <- readRDS(file)
				parts <- strsplit(file, "/")[[1]]
				fne <- parts[[length(parts)]]
				fn <- strsplit(fne, "\\.")[[1]][1]
				fn <- gsub('comparison_board_', '', fn)
				fn <- gsub('hidden-units', 'hu', fn)
				sub.dir <- gsub('hidden-units', 'hu', sub.dir)
				sub.dir <- gsub('randomforest', 'rf', sub.dir)
				label <- paste(gsub('/', '_', gsub('\\./', '', sub.dir)), fn, sep="_")
				print(label)
				data.names[[count]] <- label
				count <- count + 1
			}
		}
	}
}

print(data.names[10:20])
write_list(data.frames, data.names)
#WriteXLS(data.frames, "method-comparison.XLS", perl = perl)
