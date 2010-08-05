
#################################################################################################
# DATA.FRAME TO ASCII FILE
#################################################################################################
#tdata is the dataframe that has y, x and columns for the data to be output IS OUT
#filenames is a vector of the output file names. if null, column names will be used
#outdir is the output directory... default is to use the current working directory
#gz is a boolean to define if the output are supposed to be gzip compressed or not
dataframe2asc = function(tdata,filenames=NULL,outdir=getwd(),gz=FALSE) {
	
	#check values
	if (is.null(filenames)) {
		filenames = colnames(tdata)[3:length(tdata)] #if no variable names defined, create them
	} else {
		if (length(filenames)!=length(3:length(tdata))) stop('variable names must be the same length as the files vector')
		filenames = as.character(filenames)
	}	
	for (ii in 3:(length(tdata))) { #cycle through each of the files
		lats=unique(tdata[,1]);lats=sort(lats);
		longs=unique(tdata[,2]);longs=sort(longs)
		cellsize = min(c(diff(lats),diff(longs)))  # set cell size
		nc=ceiling((max(lats)-min(lats))/cellsize)+1; nr=ceiling((max(longs)-min(longs))/cellsize)+1
		out.asc=as.asc(matrix(NA,nr=nr,nc=nc),xll=min(longs),yll=min(lats),cellsize=cellsize)
		out.asc = put.data(tdata[,c(2:1,ii)],out.asc)
		write.asc(out.asc,paste(outdir,'/',filenames[ii-2],sep=''),gz=gz)  #put the name and extention
	}
}
