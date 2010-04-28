#function to calculate the AUC of the ROC approximated with a Mann-Whitney U statistic

#obs is the observed values ... 0 for absence & 1 for presence
#pred are predicted probabilities from 0 to 1

#NAs are automatically removed

auc <- function(obs,pred){
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
	n1 = length(which(obs==1)); n0 = length(which(obs==0))
	if (n1==0 || n1==n) return( NaN ) #if all observed 1's or 0's return NaN

	###calc AUC
	pred0 = pred[which(obs==0)]
	pred1 = pred[which(obs==1)]
	ranks = rank(pred,ties.method='average')#define ranks
	ranks0 = ranks[which(obs==0)]
	ranks1 = ranks[which(obs==1)]
	U = n0*n1 + (n0*(n0+1))/2 - sum(ranks0) #calc U stat
	AUC = U/(n0*n1) #estimate AUC
	if (AUC<.5) AUC = 1-AUC
	
	#return the auc value
	return(AUC)
}

