\name{ConnCompLabel}
\Rdversion{1.1}
\alias{ConnCompLabel}

\title{ Connected components labelling }

\description{
\code{ConnCompLabel} is a 1 pass implementation of connected components 
labelling. Here it is applied to identify disjunt patches of a distribution.
}

\usage{
ConnCompLabel(mat)
}
\arguments{
  \item{mat}{is a binary matrix of data with 0 representing background and 
  1 representing environment of interest. NA values are acceptable}
}

\value{
a matrix of the same dim of \item{mat} in which unique components (individual 
patches) are numbered 1:n with 0 remaining background value.
}

\author{Jeremy VanDerWal \email{jjvanderwal@gmail.com}}

\references{
Chang, F., C.-J. Chen, and C.-J. Lu. 2004. A linear-time component-labeling 
algorithm using contour tracing technique. Comput. Vis. Image Underst. 
93:206-220.
}

\examples{

#define a simple binary matrix
tmat = { matrix(c(	0,0,0,1,0,0,1,1,0,1,
					0,0,1,0,1,0,0,0,0,0,
					0,1,NA,1,0,1,0,0,0,1,
					1,0,1,1,1,0,1,0,0,1,
					0,1,0,1,0,1,0,0,0,1,
					0,0,1,0,1,0,0,1,1,0,
					1,0,0,1,0,0,1,0,0,1,
					0,1,0,0,0,1,0,0,0,1,
					0,0,1,1,1,0,0,0,0,1,
					1,1,1,0,0,0,0,0,0,1),nr=10,byrow=T) }
#do the connected component labelling
ccl.mat = ConnCompLabel(tmat)
ccl.mat
image(t(ccl.mat[10:1,]),col=c('grey',rainbow(length(unique(ccl.mat))-1)))

}
