\name{optim.thresh}
\alias{optim.thresh}

\title{ Estimation of Optimal Threshold Values }

\description{
	\code{optim.thresh} estimates optimal threshold values given eight methods. \cr \cr 
	\bold{Note:} this method will exclude any missing data.
}

\usage{ optim.thresh(obs,pred,threshold) }

\arguments{
	\item{obs}{a vector of observed values which must be 0 for absences and 1 for occurrences}
	\item{pred}{a vector of the same length as \code{obs} representing the predicted values. Values must be between 0 & 1 representing a likelihood.}
	\item{threshold}{a single integer value representing the number of equal interval threshold values between 0 & 1}
}

\value{
Returns a list of the optimal thresholds for the different methods. If the list item is a single value, that is the optimal threshold but if two values are reported for the method, this represents the range in thresholds that are equal for that threshold selection method. \cr \cr 
The returned list includes the single or range in thresholds selected using the following methods:
	\item{min.occurence.prediction}{is the minimum prediction for the occurrence (presence) records} 
	\item{mean.occurence.prediction}{is the mean prediction for the occurrence (presence) records}
	\item{'10.percent.omission'}{is the threshold value or range in values that	excludes approx. 10 percent of the occurrence records} 
	\item{'sensitivity=specificity'}{is the threshold value or range in values where sensitivity is equal to sensitivity} 
	\item{'max.sensitivity+specificity'}{is the threshold value or range in values that	maximizes sensitivity plus specificity} 
	\item{maxKappa}{is the threshold value or range in values with the maximum Kappa statistic}
	\item{max.prop.correct}{is the threshold value or range in values with the maximum proportion of presence and absence records correctly identified} 
	\item{min.ROC.plot.distance}{is the threshold value or range in values where the ROC curve is closest to point (0,1) (or perfect fit)} 
}

\author{Jeremy VanDerWal \email{jjvanderwal@gmail.com}}

\seealso{ \code{\link{accuracy}}, \code{\link{auc}}, \code{\link{Kappa}}, \code{\link{omission}}, \code{\link{sensitivity}}, \code{\link{specificity}}, \code{\link{prop.correct}}, \code{\link{confusion.matrix}} }

\examples{

#create some data
obs = c(sample(c(0,1),20,replace=TRUE),NA); obs = obs[order(obs)]
pred = runif(length(obs),0,1); pred = pred[order(pred)]

#calculate the optimal thresholds
optim.thresh(obs,pred)

}
