# CREATE A FUNCTION FOR THE LEGEND GRADIENT
legend.gradient = function(pnts,cols=heat.colors(100),limits=c(0,1), title='Legend', ...){
	pnts = try(as.matrix(pnts),silent=T)
	if(!is.matrix(pnts)) stop("you must have a 4x2 matrix")
	if(dim(pnts)[1]!=4 || dim (pnts)[2]!=2) stop ("Matrix must have dimensions of 4 rows and 2 columms")
	if(length(cols)<2) stop("You must have 2 or more colors")
	#break up the min and max into a number of values == length(cols)
	yvals = seq(min(pnts[, 2]), max(pnts[, 2]), length=length(cols)+1)
	#cycle through each of the yvals and create polygons
	for (i in 1:length(cols)){  #create the polygon for that color
		polygon(x=pnts[,1],y=c(yvals[i],yvals[i],yvals[i+1],yvals[i+1]),col=cols[i],border=F)
	}
	#add the text
	text(max(pnts[,1]),min(pnts[,2]),labels=limits[1],pos=4,...)
	text(max(pnts[,1]),max(pnts[,2]),labels=limits[2],pos=4,...)
	text(min(pnts[,1]),max(pnts[,2]),labels=title,adj=c(0,-1),...)
}
