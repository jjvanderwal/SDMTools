#calculate the ommision rate, sensitivity, specificity, proporion correctly identified

#mat is a confusion matrix of class confusion.matrix

#NAs are automatically removed

omission <- function(mat){
	#input checks
	if (attr(mat,'class')!='confusion.matrix') stop('mat must be of class confusion.matrix')
	#return the value
	return(mat[1,2]/sum(mat[,2]))
}

sensitivity = function(mat) { 
	#input checks
	if (attr(mat,'class')!='confusion.matrix') stop('mat must be of class confusion.matrix')
	#return the value
	return(mat[2,2]/sum(mat[,2])) 
}

specificity = function(mat) { 
	#input checks
	if (attr(mat,'class')!='confusion.matrix') stop('mat must be of class confusion.matrix')
	#return the value
	return(mat[1,1]/sum(mat[,1])) 
}

prop.correct = function(mat) { 
	#input checks
	if (attr(mat,'class')!='confusion.matrix') stop('mat must be of class confusion.matrix')
	#return the value
	return(sum(diag(mat))/sum(mat)) 
}


	