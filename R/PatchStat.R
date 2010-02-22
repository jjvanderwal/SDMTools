### patch statistics
#mat is a matrix of data representing a raster of uniquely identified patches (from ConnCompLabel)
#cell size is a single value representing the width/height of cell edges (assuming square cells and distance is in m)

PatchStat <- 
function(mat,cellsize=1)	{
	#check to ensure matrix
	mat = try(as.matrix(mat))
	if (!is.matrix(mat)) stop('objects must be a matrix')
	#get the unique patch ID's
	ID.vals = as.numeric(na.omit(unique(as.vector(mat))));ID.vals = ID.vals[order(ID.vals)]
	#extract the base patch info
	out = as.data.frame(.Call('PS',mat,ID.vals))
	names(out) = c('patchID','n.cell','n.core.cell','n.edges.perimeter','n.edges.internal')
	#calculate other stats
	out$area = out$n.cell * cellsize^2
	out$core.area = out$n.core.cell * cellsize^2
	out$perimeter = out$n.edges.perimeter * cellsize
	out$perim.area.ratio = out$perimeter / out$area 
	out$shape.index = shape.index(out$n.cell,out$n.edges.perimeter)
	out$frac.dim.index = (2 * log(0.25 * out$perimeter)) / log(out$area)
	out$core.area.index = out$core.area / out$area
	return(out)
}
