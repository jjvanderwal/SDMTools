#calculate Kappa statistic

#mat is a confusion matrix of class confusion.matrix

#NAs are automatically removed

Kappa <- function(mat){
	#input checks
	if (attr(mat,'class')!='confusion.matrix') stop('mat must be of class confusion.matrix')
	#calculate Kappa
	n<-sum(mat)
	colsums<-apply(mat,2,sum)
	rowsums<-apply(mat,1,sum)
	t1 = sum(diag(mat))/n; 	t2 = sum(rowsums*colsums)/(n^2)
	#return the value
	return((t1-t2)/(1-t2))
}
