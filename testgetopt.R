library('getopt')

opt = getopt(c(
    'realtemp', 'rt', 0, "logical",
    'predtemp', 'pt', 1, "logical",
    'temp1', 't1', 0, "logical",
    'pca', 'pc', 0, "logical",
    'htype', 'ht', 2, "logical",
    'units', 'u', 1, "integer"
));

if (!is.null(opt$realtemp) ) {
    #get the script name (only works when invoked with Rscript).
    self = commandArgs()[1];
    #print a friendly message and exit with a non-zero error code
    cat(paste("Usage: ",self," [-[vh]] [-[-mean|m] <mean>] [-[-sd|s] <sd>] [-[-count|c] <count>]\n",sep=""));
    q(status=1);
}

print(opt$temp1)
