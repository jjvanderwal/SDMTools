#calculate weighted mean
wt.mean <- function(x,wt) {
	s = which(is.finite(x*wt)); wt = wt[s]; x = x[s] #remove NA info
	return( sum(wt * x)/sum(wt) ) #return the mean
}

#calculate the unbiased weighted mean variance based on GNU Scentific Library 
#equation at http://www.gnu.org/software/gsl/manual/html_node/Weighted-Samples.html
#this equation was found at http://en.wikipedia.org/wiki/Weighted_mean#Weighted_sample_variance
wt.var <- function(x,wt) {
	s = which(is.finite(x + wt)); wt = wt[s]; x = x[s] #remove NA info
	xbar = wt.mean(x,wt) #get the weighted mean
	return( sum(wt *(x-xbar)^2)*(sum(wt)/(sum(wt)^2-sum(wt^2))) ) #return the variance
} 
wt.sd <- function(x,wt) { 
	return( sqrt(wt.var(x,wt)) ) #return the standard deviation
} 
