read.asc <-
function (file) {
  #confirm ascii grid file name is specified
  if (substr(file, nchar(file) - 3, nchar(file)) != ".asc") {stop("not a valid .asc file")}
  #read in the header
  zz <- file(file, "r")
    #number of columns
    nc <- readLines(zz, 1); nc <- strsplit(nc, " "); nc <- as.numeric(nc[[1]][length(nc[[1]])])
    #number of rows
    nl <- readLines(zz, 1); nl <- strsplit(nl, " "); nl <- as.numeric(nl[[1]][length(nl[[1]])])
    #lower left corner
    xll <- readLines(zz, 1); xll <- strsplit(xll, " ")
    yll <- readLines(zz, 1); yll <- strsplit(yll, " ")
    #cell size
    cs <- readLines(zz, 1); cs <- strsplit(cs, " "); cs <- as.numeric(cs[[1]][length(cs[[1]])])
    #nodata value
    nas <- readLines(zz, 1); nas <- strsplit(nas, " "); nas <- as.numeric(nas[[1]][length(nas[[1]])])
  #close the link to the file
  close(zz)
  
  #ensure xll & yll are corners
  if ((xll[[1]][1] == "xllcenter") | (xll[[1]][1] == "XLLCENTER")) {xll=as.numeric(xll[[1]][length(xll[[1]])])+ cs/2} else {xll=as.numeric(xll[[1]][length(xll[[1]])])}
  if ((yll[[1]][1] == "yllcenter") | (xll[[1]][1] == "YLLCENTER")) {yll=as.numeric(yll[[1]][length(yll[[1]])])+ cs/2} else {yll=as.numeric(yll[[1]][length(yll[[1]])])}

  #read in the data skipping the first six header rows
  output <- scan(file,skip=6, quiet = TRUE)

  #convert no data to NA
  output[output == nas] <- NA
  #convert data to matrix
  output <- matrix(c(as.matrix(output)), ncol = nl)
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

