#function to calculate 5 measures of accuracy for presence/absence or presence/psuedo-absence data

#obs is the observed values ... 0 for absence & 1 for presence
#pred are predicted probabilities from 0 to 1
#threshold can be:
#  -a single value representing a single threshold between 0 & 1
#  -a vector of threshold values between 0 & 1
#  -an integer value representing the number of equal interval threshold values between 0 & 1

#NAs are automatically removed

accuracy <- function(obs,pred,threshold=0.5){
	#input checks
	if (length(obs)!=length(pred)) stop('this requires the same number of observed & predicted values')
	
	#deal with NAs
	if (length(which(is.na(c(obs,pred))))>0) {
		na = union(which(is.na(obs)),which(is.na(pred)))
		warning(length(na),' data points removed due to missing data')
		obs = obs[-na]; pred = pred[-na]
	}

	#define the n's and do checks
	n = length(obs); if (length(which(obs %in% c(0,1)))!=n) stop('observed values must be 0 or 1') #ensure observed are values 0 or 1

	# check / setup the threshold values
	if (length(threshold)==1 & threshold[1]<=1 & threshold[1]>=0) { 
		thresholds = threshold
	} else if (length(threshold)==1 & threshold[1]>1) {
		thresholds = seq(0,1,length=threshold)
	} else if (length(threshold)>1 & max(threshold)<=1 & min(threshold)>=0) {
		thresholds = threshold
	} else { stop('inappropriate threshold values used as input. See help file.') }
	
	#cycle through each of the helpfiles
	out = NULL
	for (threshold in thresholds) {
		
	}

}

