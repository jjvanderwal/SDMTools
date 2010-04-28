#create a confusion matrix of data given a threshold

#obs is the observed values ... 0 for absence & 1 for presence
#pred are predicted probabilities from 0 to 1
#threshold can be a single value representing a single threshold between 0 & 1

#NAs are automatically removed

confusion.matrix <- function(obs,pred,threshold=0.5){
	#input checks
	if (length(obs)!=length(pred)) stop('this requires the same number of observed & predicted values')
	if (!(length(threshold)==1 & threshold[1]<=1 & threshold[1]>=0)) stop('inappropriate threshold value... must be a single value between 0 & 1.')
	n = length(obs); if (length(which(obs %in% c(0,1,NA)))!=n) stop('observed values must be 0 or 1') #ensure observed are values 0 or 1
	
	#deal with NAs
	if (length(which(is.na(c(obs,pred))))>0) {
		na = union(which(is.na(obs)),which(is.na(pred)))
		warning(length(na),' data points removed due to missing data')
		obs = obs[-na]; pred = pred[-na]
	}
	#apply the threshold to the prediction
	if (threshold==0) {
		pred[which(pred>threshold)] = 1; pred[which(pred<=threshold)] = 0
	} else {
		pred[which(pred>=threshold)] = 1; pred[which(pred<threshold)] = 0
	}
	#return the confusion matrix
	mat = table(pred=factor(pred,levels=c(0,1)),obs=factor(obs,levels=c(0,1)))
	attr(mat,'class') = 'confusion.matrix'
	return(mat)
}

