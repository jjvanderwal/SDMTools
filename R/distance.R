#given a location from and to in lat lon, return the distance based on algorithm selected
#distance is the distance in m
distance = function(lat1, lon1=NULL, lat2=NULL, lon2=NULL, bearing=FALSE) {
	if (is.data.frame(lat1) | is.matrix(lat1)) { #if input is matrix or data.frame... break it out to individual vectors
		lat1 = as.matrix(lat1); if (ncol(lat1)!=4) stop('incorrect lat/lon inputs... must be matrix with 4 columns or 4 vectors')
		lon2=lat1[,4]; lat2=lat1[,3]; lon1=lat1[,2]; lat1=lat1[,1] #break out individual columns
	} else if (!is.null(lat2) & !is.null(lon1) & !is.null(lon2)) {
		if (!all(c(length(lat2),length(lon1),length(lon2))==length(lat1))) stop('inputs must all be of same length')
	} else { stop('inappropriate inputs... see helpfile') }
	if (any(c(lon1,lon2) < -180) | any(c(lon1,lon2) > 180)) stop('lon must be decimal degrees between -180 & 180')
	if (any(c(lat1,lat2) < -90) | any(c(lat1,lat2) > 90)) stop('lat must be decimal degrees between -90 & 90')
	#cycle through and output the new data
	out = data.frame(lat1=lat1,lon1=lon1,lat2=lat2,lon2=lon2)
	out$distance = round(.Call('dist',out$lat1,out$lon1,out$lat2,out$lon2),2) #round to the nearest mm
	if (bearing) { #if requested, calculate bearing
		lat1=lat1*pi/180;lat2=lat2*pi/180;lon1=lon1*pi/180;lon2=lon2*pi/180 #convert to radians
		brng = atan2(sin(lon2-lon1)*cos(lat2),cos(lat1)*sin(lat2)-sin(lat1)*cos(lat2)*cos(lon1-lon2)) #estimate bearing
		out$bearing = ((brng*180/pi)+360)%%360 #convert to bearing in degrees
	}
	#return the output
	return(out)
}
