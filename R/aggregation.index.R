##method to calculate shape index or aggregation indexes
#a = area of the patch in number of cells
#p is the perimeter in number of edges
#g is the number of 'internal' edges (single count)

aggregation.index <- 
function(a,g) {
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
