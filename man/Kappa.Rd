\name{Kappa}
\alias{Kappa}

\title{ Kappa Statistic }
\description{
\code{Kappa} estimates the Kappa statistic for model accuracy.
}
\usage{
Kappa(mat)
}

\arguments{
  \item{mat}{a confusion matrix of class 'confusion.matrix' from \code{confusion.matrix}}
}
\value{
returns a single value represting the Kappa statistic.
}
\author{Jeremy VanDerWal \email{jjvanderwal@gmail.com}}
\examples{

#create some data
obs = c(sample(c(0,1),20,replace=TRUE),NA); obs = obs[order(obs)]
pred = runif(length(obs),0,1); pred = pred[order(pred)]

#calculate the confusion matrix
mat = confusion.matrix(obs,pred,threshold=0.5)

#calculate the Kappa statistic
Kappa(mat)

}
