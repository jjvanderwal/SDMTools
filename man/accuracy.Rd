\name{accuracy}
\alias{accuracy}
\title{Measures of Model Accuracy}
\usage{
accuracy(obs, pred, threshold = 0.5)
}
\arguments{
  \item{obs}{a vector of observed values which must be 0
  for absences and 1 for occurrences}

  \item{pred}{a vector of the same length as \code{obs}
  representing the predicted values.  Values must be
  between 0 & 1 prepresenting a likelihood.}

  \item{threshold}{this can be: \cr a) a single value
  representing a single threshold between 0 & 1; \cr b) a
  vector of threshold values between 0 & 1; OR \cr c) an
  integer value representing the number of equal interval
  threshold values between 0 & 1}
}
\value{
a data.frame with seven columns: \item{threshold}{the
threshold values representing each row of data}
\item{AUC}{the AUC given the defined threshold value}
\item{ommission.rate}{the ommission rate as a proportion of
true occurrences misidentified given the defined threshold
value} \item{sensitivity}{the sensitivity given the defined
threshold value} \item{specificity}{the specificity given
the defined threshold value} \item{prop.correct}{the
proportion of the presence and absence records correctly
identified given the defined threshold value}
\item{Kappa}{the Kappa statistic of the model given the
defined threshold value}
}
\description{
\code{accuracy} estimates six measures of accuracy for
presence-absence or presence-psuedoabsence data. These
include AUC, ommission rates, sensitivity, specificity,
proportion correctly identified and Kappa. \cr \cr
\bold{Note:} this method will exclude any missing data.
}
\examples{
#create some data
obs = c(sample(c(0,1),20,replace=TRUE),NA); obs = obs[order(obs)]
pred = runif(length(obs),0,1); pred = pred[order(pred)]

#calculate accuracy of the model with a single threshold value
accuracy(obs,pred,threshold=0.5)

#calculate accuracy given several defined thresholds
accuracy(obs,pred,threshold=c(0.33,0.5,0.66))

#calculate accuracy given a number of equal interval thresholds
accuracy(obs,pred,threshold=20)
}
\author{
Jeremy VanDerWal \email{jjvanderwal@gmail.com}
}
\seealso{
\code{\link{auc}}, \code{\link{Kappa}},
\code{\link{omission}}, \code{\link{sensitivity}},
\code{\link{specificity}}, \code{\link{prop.correct}},
\code{\link{confusion.matrix}}
}

