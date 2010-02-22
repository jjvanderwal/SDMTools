write.asc.gz <-
function (x, file) {
  #confirm asc object and file named appropriately
  if (!inherits(x, "asc")) {stop("Non convenient data")}
  if (substr(file, nchar(file) - 3, nchar(file)) != ".asc") {file <- paste(file, ".asc", sep = "")}
  #add the gz extention
  file <- paste(file, ".gz", sep = "")
  #create the file  
  file.create(file)
  #open a connection to file
  zz <- gzfile(file, "w")
    #write the header info
    cat("ncols         ",nrow(x),'\n',sep = "",file=zz)
    cat("nrows         ",ncol(x),'\n',sep = "",file=zz)
    cat("xllcorner     ",attr(x,"xll")-attr(x,"cellsize")/2,'\n',sep = "",file=zz)
    cat("yllcorner     ",attr(x,"yll")-attr(x,"cellsize")/2,'\n',sep = "",file=zz)
    cat("cellsize      ", attr(x, "cellsize"),'\n',sep = "",file=zz)
    cat("NODATA_value  ", -9999,'\n',sep = "",file=zz)
    #prep and write the data
    x[is.na(x)] <- -9999 #change na values
    x <- x[, ncol(x):1] #reorder
    x <- rbind(x, rep("\n", ncol(x))) #add new line character
    cat(x,file=zz)
  #close the connection to the file
  close(zz)
}

