### connected components labelling
#mat is a binary matrix of data with NA, 0 for background and 1 for 'patch' to be classified

ConnCompLabel <- 
function(mat)	{
	#get the input attributes
	tattrib = attributes(mat)
	#check to ensure matrix
	mat = try(as.matrix(mat))
	if (!is.matrix(mat)) stop('objects must be a matrix')
	#run the connected component labelling
	out = .Call('ccl',mat)
	#reset the attributes of the input
	attributes(out) = tattrib
	return(out)
}