write.asc2 <-
function (x, file, sigdig = 0, gz=FALSE) {
	###confirm asc object and file named appropriately
	if (!inherits(x, "asc")) stop("Non convenient data")
	if (substr(file, nchar(file) - 3, nchar(file)) != ".asc") file <- paste(file, ".asc", sep = "")
	###write out the data
	tt = .Call('writeascdata', nrow(x) , ncol(x) , as.character(attr(x,"xll")-attr(x,"cellsize")/2) ,
		as.character(attr(x,"yll")-attr(x,"cellsize")/2) , as.character(attr(x, "cellsize")) , x , file , sigdig)
	if (gz) {
		require(R.utils)
		gzip(file)
	}
}

write.asc2.gz <-
function (x, file, sigdig = 0) {
	write.asc(x, file, sigdig=sigdig, gz=TRUE)
}
