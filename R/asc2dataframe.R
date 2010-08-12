##########################################################################################################
#ASC2DATAFRAME FUNCTION
##########################################################################################################
#files is simply a vector of file names
#varnames is a vector of names for the output columns .. must be the same length as files
#gz is a boolean defining weather or not the files are gzipped ascii grid files
asc2dataframe = function(filenames,varnames=NULL,gz=FALSE) {
	#check values
	if (is.null(varnames)) {
		varnames = paste('var',1:length(filenames),sep='.') #if no variable names defined, create them
	} else {
		if (length(varnames)!=length(filenames)) stop('variable names must be the same length as the files vector')
		varnames = as.character(varnames)
	}
	out = NULL #define the output
	for (ii in 1:length(filenames)) { #cycle through each of the files
		tfile = filenames[ii]; varname = varnames[ii] #define the file and variable name
		cat('working with',tfile,'...\n')
		if (!file.exists(tfile)) { warning(paste(tfile,'does not exist and was not used')); next } #check if the file exists, if not move to the next file
		tasc = read.asc(tfile,gz=gz) #read in the ascii grid file
		if (is.null(out)) { #if out is still null, populate the row/col/x,y data
			out = as.data.frame(which(is.finite(tasc),arr.ind=T)) #get row column info for actual data
			out$y = getXYcoords(tasc)$y[out$col]#extract the longitudes
			out$x = getXYcoords(tasc)$x[out$row] #extract the latitudes
		}
		out[varname] = tasc[cbind(out$row,out$col)] #append the actual data
	}
	if (is.null(out)) { #if out is still null
		warning('no data was extracted...'); return(NA)
	} else { #if out has some data
		out = na.omit(out) #remove any missing data
		out$row = out$col = NULL #remove the row/col info
		attr(out,'filenames') = list(filenames,names=varnames) #set an atrtribute relating filenames with variable names
		#names(attr(out,'filenames')) = varnames #define the names of the attribute list
		return(out)
	}
}

