\name{omission}
\alias{omission}
\alias{sensitivity}
\alias{specificity}
\alias{prop.correct}

\title{ Measures of Accuracy }

\description{ Estimates different measures of accurracy given a confusion matrix. }

\usage{
omission(mat)
sensitivity(mat)
specificity(mat)
prop.correct(mat)
}

\arguments{ \item{mat}{a confusion matrix of class 'confusion.matrix' from \code{confusion.matrix}} }

\value{
	returns single values representing the:
		\item{ommission}{the ommission rate as a proportion of true occurrences misidentified given the defined threshold value}
		\item{sensitivity}{the sensitivity given the defined threshold value}
		\item{specificity}{the specificity given the defined threshold value}
		\item{prop.correct}{the proportion of the presence and absence records correctly identified given the defined threshold value}
}

\author{Jeremy VanDerWal \email{jjvanderwal@gmail.com}} 

\seealso{ \code{\link{auc}}, \code{\link{Kappa}}, \code{\link{confusion.matrix}}, \code{\link{accuracy}} }

\examples{

#create some data
obs = c(sample(c(0,1),20,replace=TRUE),NA); obs = obs[order(obs)]
pred = runif(length(obs),0,1); pred = pred[order(pred)]

#calculate the confusion matrix
mat = confusion.matrix(obs,pred,threshold=0.5)

#calculate the accuracy measures
omission(mat)
sensitivity(mat)
specificity(mat)
prop.correct(mat)

}
