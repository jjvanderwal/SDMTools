
.onLoad <- function(lib, pkg)  {
	
	pkg.info <- utils::packageDescription('SDMTools') 
	packageStartupMessage(paste("SDMTools ", pkg.info[["Version"]], " (", pkg.info["Date"], ")", sep=""))

	return(invisible(0))
}
