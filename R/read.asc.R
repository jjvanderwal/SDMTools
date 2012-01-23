read.asc <-
function (file, gz=FALSE) {
  #confirm ascii grid file name is specified
	if (gz) {
		if (substr(file, nchar(file) - 2, nchar(file)) != ".gz") stop("not a valid .gz file")
	} else {
		if (substr(file, nchar(file) - 3, nchar(file)) != ".asc") stop("not a valid .asc file")
	}
	
	#read in the header
	if (gz) { zz <- gzfile(file, "r") } else { zz <- file(file, "r") }
		nc <- scan(zz,what=list('',''),nlines=1,quiet=TRUE); nc <- as.numeric(nc[[2]][1])#number of columns
		nl <- scan(zz,what=list('',''),nlines=1,quiet=TRUE); nl <- as.numeric(nl[[2]][1])#number of rows
		xll <- scan(zz,what=list('',''),nlines=1,quiet=TRUE); #lower left corner
		yll <- scan(zz,what=list('',''),nlines=1,quiet=TRUE); #lower left corner
		cs <- scan(zz,what=list('',''),nlines=1,quiet=TRUE); cs <- as.numeric(cs[[2]][1])#cell size
		nas <- scan(zz,what=list('',''),nlines=1,quiet=TRUE); nas <- as.numeric(nas[[2]][1])#nodata value
	#close the link to the file
	close(zz)
	
	#ensure xll & yll are centers of the cells
	if ((xll[[1]][1] == "xllcenter") | (xll[[1]][1] == "XLLCENTER")) { xll=as.numeric(xll[[2]][1]) } else { xll=as.numeric(xll[[2]][1])+ cs/2 }
	if ((yll[[1]][1] == "yllcenter") | (xll[[1]][1] == "YLLCENTER")) { yll=as.numeric(yll[[2]][1]) } else { yll=as.numeric(yll[[2]][1])+ cs/2 }

	#read in the data skipping the first six header rows
	if (gz) { zz <- gzfile(file, "r"); output <- scan(zz,nmax=nl*nc,skip=6,quiet = TRUE); close(zz);
	} else { output <- scan(file,nmax=nl*nc,skip=6, quiet = TRUE) }

	#convert no data to NA
	output[output == nas] <- NA
	#convert data to matrix
	output <- matrix(c(as.matrix(output)), ncol = nl, nrow = nc)
	output <- output[, ncol(output):1]
	#define the attributes
	attr(output, "xll") <- xll
	attr(output, "yll") <- yll
	attr(output, "cellsize") <- cs
	attr(output, "type") <- 'numeric'
	class(output) <- "asc"
	#return the file
	return(output)
}

read.asc.gz <-
function (file) {
	return(read.asc(file, gz=TRUE))
}

