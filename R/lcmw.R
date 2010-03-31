lcmw <-
function(mat,mw,mnc) {
	#check input for class for returning info
	if (class(mat) == 'asc') { 
		attrib = attributes(mat)
	} else if (any(class(mat) %in% 'RasterLayer')) {
		attrib = mat; mat = asc.from.raster(mat)
	} else if (any(class(mat) == 'SpatialGridDataFrame')) {
		attrib = mat; mat = asc.from.sp(mat)
	} else {
		attrib = attributes(mat)
	}
	#buffer edges by full number of distance cells
	#define the shifts in mat to account for a moving window...
	vals = expand.grid(Y=-mnc:mnc,X=-mnc:mnc) #define all shifts
	vals$cost = mw[(mnc+1)+cbind(vals$Y,vals$X)];vals=na.omit(vals) #extract the cost of associated with the move
	nrow.vals = nrow(vals)
	#cycle through and get the output
	if (nrow.vals <5000) {
		return(.Call("movewindow",mat,as.integer(vals$X),as.integer(vals$Y),as.numeric(vals$cost)))
	} else {
		num.subsets = nrow.vals%/%2000
		#run the first set of 2000
		tmin = 1; tmax = 2000
		#print a status
		cat('0%...')
		#create the first part of the moving window
		out = .Call("movewindow",mat,as.integer(vals$X[tmin:tmax]),as.integer(vals$Y[tmin:tmax]),as.numeric(vals$cost[tmin:tmax]))
		#cycle through the remaining data
		for (i in 1:num.subsets){
			if (i<num.subsets){
				tmin = i*2000+1; tmax = (i+1)*2000
			} else {
				tmin = i*2000+1; tmax = nrow.vals
			}
			cat(round(tmin/nrow.vals*100,1),'%...',sep='')
			out2 = .Call("movewindow",mat,as.integer(vals$X[tmin:tmax]),as.integer(vals$Y[tmin:tmax]),as.numeric(vals$cost[tmin:tmax]))
			out = .Call("getmin",out,out2)
			if (dim(out)[1] != dim(mat)[1] | dim(out)[2] != dim(mat)[2]) print('error in dimensions...check output')
		}
		cat('done\n')
	}
	#reset the attributes of the input
	if (any(class(attrib) %in% 'RasterLayer')) {
		attrib = setValues(attrib, as.vector(t(t(unclass(out))[dim(out)[2]:1,]))); return(attrib)
	} else if (any(class(attrib) == 'SpatialGridDataFrame')) {
		attrib@data[1] = as.vector(unclass(out)[,dim(out)[2]:1]); return(attrib)
	} else {
		attributes(out) = attrib; return(out)
	}
}

