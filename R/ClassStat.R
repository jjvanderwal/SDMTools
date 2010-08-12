#calculate class statistics
#mat is a matrix of data representing a raster of uniquely identified patches (from ConnCompLabel)
#cell size is a single value representing the width/height of cell edges (assuming square cells and distance is in m)
#bkgd is the background value for which statistics will not be calculated

ClassStat  = function(mat,cellsize=1,bkgd=NA,latlon=FALSE) {
	##method to calculate shape index or aggregation indexes
	#a = area of the patch in number of cells
	#p is the perimeter in number of edges
	#g is the number of 'internal' edges (single count)
	aggregation.index = function(a,g) {
		n = trunc(sqrt(a))
		m = a - n^2
		if (m==0) maxg = 2*n*(n-1)
		if (m<=n) maxg = 2*n*(n-1)+2*m-1
		if (m>n) maxg = 2*n*(n-1)+2*m-2
		minp=rep(0,length(m))
		for (ii in 1:length(m)){
			if (m[ii]==0) minp[ii] = 4*n[ii]
			if (n[ii]^2<a[ii] & a[ii]<=n[ii]*(1+n[ii])) minp[ii] = 4 * n[ii] + 2
			if (a[ii] > n[ii]*(1+n[ii])) minp[ii] = 4 * n[ii] + 4
		}
		return((g/maxg)*100)
	}
	shape.index = function(a,p) {
		n = trunc(sqrt(a))
		m = a - n^2
		minp=rep(0,length(m))
		for (ii in 1:length(m)){
			if (m[ii]==0) minp[ii] = 4*n[ii]
			if (n[ii]^2<a[ii] & a[ii]<=n[ii]*(1+n[ii])) minp[ii] = 4 * n[ii] + 2
			if (a[ii] > n[ii]*(1+n[ii])) minp[ii] = 4 * n[ii] + 4
		}
		return(p/minp)
	}

	######
	#check if raster from sp or raster package and convert if necessary
	if (any(class(mat) %in% 'RasterLayer')) mat = asc.from.raster(mat)
	if (any(class(mat) == 'SpatialGridDataFrame')) mat = asc.from.sp(mat)
	#check to ensure matrix
	mat = try(as.matrix(mat))
	if (!is.matrix(mat)) stop('objects must be a matrix')
	#get the uniqu classes of data
	classes = as.numeric(na.omit(unique(as.vector(mat))));classes = classes[order(classes)]
	#omit the background value
	if (!is.na(bkgd)) classes = classes[-which(classes==bkgd)]
	#out is the final object to be returned
	out = NULL
	#cycle through each of the classes
	for (cl in classes){
		#create a reclassed matrix
		mat2 = mat; mat2 = mat * 0; mat2[which(mat==cl)] = 1
		#get the patch info for the class
		out.patch = PatchStat(ConnCompLabel(mat2),cellsize=cellsize,latlon=latlon);rm(mat2)
		#define a couple constants
		L.cell = sum(out.patch$n.cell) #n cells in landscape
		L.area = sum(out.patch$area) #full area of landscape
		#remove the background patch (id = 0)
		if (0 %in% out.patch$patchID) out.patch = out.patch[-which(out.patch$patchID==0),]		
		#create a temporary variable to store output & calculate patch stats
		tout = list(class=cl)
		tout$n.patches = nrow(out.patch)
		tout$total.area = sum(out.patch$area)
		tout$prop.landscape = tout$total.area / L.area
		tout$patch.density = tout$n.patches / L.area
		tout$total.edge = sum(out.patch$perimeter)
		tout$edge.density = tout$total.edge / L.area
		tout$landscape.shape.index = shape.index(sum(out.patch$n.cell),sum(out.patch$n.edges.perimeter))
		tout$largest.patch.index = max(out.patch$area) / L.area
		tout$mean.patch.area = mean(out.patch$area)
		tout$sd.patch.area = sd(out.patch$area)
		tout$min.patch.area = min(out.patch$area)
		tout$max.patch.area = max(out.patch$area)
		tout$perimeter.area.frac.dim = 2 / (((tout$n.patches*sum(log(out.patch$perimeter)+log(out.patch$area)))-(tout$total.edge*tout$total.area))/(tout$n.patches*sum(log(out.patch$perimeter^2))-tout$total.edge^2))
		tout$mean.perim.area.ratio = mean(out.patch$perim.area.ratio)
		tout$sd.perim.area.ratio = sd(out.patch$perim.area.ratio)
		tout$min.perim.area.ratio = min(out.patch$perim.area.ratio)
		tout$max.perim.area.ratio = max(out.patch$perim.area.ratio)
		tout$mean.shape.index = mean(out.patch$shape.index,na.rm=T)
		tout$sd.shape.index = sd(out.patch$shape.index,na.rm=T)
		tout$min.shape.index = min(out.patch$shape.index,na.rm=T)
		tout$max.shape.index = max(out.patch$shape.index,na.rm=T)
		tout$mean.frac.dim.index = mean(out.patch$frac.dim.index,na.rm=T)
		tout$sd.frac.dim.index = sd(out.patch$frac.dim.index,na.rm=T)
		tout$min.frac.dim.index = min(out.patch$frac.dim.index,na.rm=T)
		tout$max.frac.dim.index = max(out.patch$frac.dim.index,na.rm=T)	
		tout$total.core.area = sum(out.patch$core.area)
		tout$prop.landscape.core = tout$total.core.area / L.area
		tout$mean.patch.core.area = mean(out.patch$core.area)
		tout$sd.patch.core.area = sd(out.patch$core.area)
		tout$min.patch.core.area = min(out.patch$core.area)
		tout$max.patch.core.area = max(out.patch$core.area)
		tout$prop.like.adjacencies = sum(out.patch$n.edges.internal) / sum(out.patch$n.edges.internal+out.patch$n.edges.perimeter*2)
		tout$aggregation.index = aggregation.index(sum(out.patch$n.cell),sum(out.patch$n.edges.internal)/2)
		tout$lanscape.division.index = 1-sum((out.patch$n.cell / L.cell)^2)
		tout$splitting.index = L.area / sum(out.patch$area^2)
		tout$effective.mesh.size = sum(out.patch$area^2) / L.area 
		tout$patch.cohesion.index = ((1-(sum(out.patch$n.edges.internal)/sum(out.patch$n.edges.internal*sqrt(out.patch$n.cell))) )*((1-1/sqrt(L.cell))/10))*100
		
		#store in out 
		out = rbind(out,as.data.frame(tout))
	}
	return(out)
}
