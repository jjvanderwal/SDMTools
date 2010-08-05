# this estimates the averate of a varity of directions using vector averaging
circular.averaging = function(direction,deg=TRUE) {
	n=length(direction) #get the length of direction vector
	out = vector.averaging(direction=direction,distance=rep(1,n),deg=deg)
	return(out$direction)
}
