\name{wt.mean}
\alias{wt.mean}
\alias{wt.var}
\alias{wt.sd}

\title{ Weighted mean, variance and standard deviation calculations }

\description{
	\code{wt.mean} calculates the mean given a weighting of the values. \cr \cr
	\code{wt.var} is the unbiased variance of the weighted mean calculation using equations of GNU Scentific Library (\url{http://www.gnu.org/software/gsl/manual/html_node/Weighted-Samples.htmland}.\cr\cr
	\code{wt.sd} is the standard deviation of the weighted mean calculated as the sqrt of \code{wt.var}. \cr \cr 
	\bold{Note:} NA data is automatically ommitted from analysis.
}

\usage{
wt.mean(x,wt)
wt.var(x,wt)
wt.sd(x,wt)
}

\arguments{
	\item{x}{is a vector of numerical data.}
	\item{wt}{is a vector of equal length to \code{x} representing the weights.)}
}

\value{ returns a single value from analysis requested. }

\author{ Jeremy VanDerWal \email{jjvanderwal@gmail.com} }

\examples{

#define simple data
x = 1:25 # set of numbers
wt = runif(25) #some arbitrary weights

#display means & variances (unweighted and then weighted)
mean(x); wt.mean(x,wt)
var(x); wt.var(x,wt)
sd(x); wt.sd(x,wt)

}
