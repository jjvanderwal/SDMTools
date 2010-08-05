vector.averaging = function(direction,distance,deg=TRUE) {
	if (deg) direction = direction*pi/180 #convert to radians
	n=length(direction) #get the length of direction vector
	if (any(is.na(direction))) { #ensure no NA data
		warning('NAs in data'); pos = which(is.na(direction)); direction = direction[-pos]; distance = distance[-pos] 
	} else {
		sinr <- sum(sin(direction))
		cosr <- sum(cos(direction))
		if (sqrt((sinr^2 + cosr^2))/n > .Machine$double.eps) {
			Ve = sum(distance*sin(direction))/n
			Vn = sum(distance*cos(direction))/n
			UV = sqrt(Ve^2 + Vn^2)
			AV = atan(Ve/Vn)
			if (deg) AV = AV *180/pi #convert back to degrees
			return(list(distance=UV,direction=AV))
		} else {
			return(list(distance=NA,direction=NA))
		}
	}
}
