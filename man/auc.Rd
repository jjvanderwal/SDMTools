\name{auc}
\alias{auc}

\title{ Area Under the Curve of the Reciever Operating Curve }

\description{
	\code{auc} estimates the AUC of the ROC using a Mann-Whitney U statistic. \cr \cr
	\bold{Note:} this method will exclude any missing data.
}

\usage{ auc(obs,pred) }

\arguments{
	\item{obs}{a vector of observed values which must be 0 for absences and 1 for occurrences}
	\item{pred}{a vector of the same length as \code{obs} representing the predicted values. Values must be between 0 & 1 representing a likelihood.}
}

\value{ Returns a single value represting the AUC value. }

\author{ Jeremy VanDerWal \email{jjvanderwal@gmail.com} }

\seealso{ \code{\link{Kappa}}, \code{\link{omission}}, \code{\link{sensitivity}}, \code{\link{specificity}}, \code{\link{prop.correct}}, \code{\link{confusion.matrix}}, \code{\link{accuracy}} }

\examples{

#create some data
obs = c(sample(c(0,1),20,replace=TRUE),NA)
pred = runif(length(obs),0,1)

#calculate AUC from the random data
auc(obs,pred)

#calculate an example 'perfect' AUC
obs = obs[order(obs)]
pred = pred[order(pred)]
auc(obs,pred)

}
