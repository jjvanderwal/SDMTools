\name{circular.averaging}
\alias{circular.averaging}
\alias{vector.averaging}
\title{Circular Averaging based on Vector Averaging}
\usage{
circular.averaging(direction, deg = TRUE)

vector.averaging(direction, distance, deg = TRUE)
}
\arguments{
  \item{direction}{a vector of directions given in degrees
  (0 - 360) if \code{deg}==TRUE or in radians if
  \code{deg}==FALSE}

  \item{distance}{a vector of distances associated with
  each direction}

  \item{deg}{a boolean object defining if \code{direction}
  is in degrees (TRUE) or radians (FALSE)}
}
\value{
\code{circular.averaging} returns the average direction
while \code{vector.averaging} returns a list with 2
elements distance & direction
}
\description{
\code{circular.averaging} calculates the average direction
(0 - 360) given a vector of directions.\cr\cr
\code{vector.averaging} calculates the average distance and
direction given a vector of directions and a vector of
distances.
}
\details{
functions return NA if the average distance or direction is
not valid... e.g., when averaging directions of 0 & 180
degrees, the result could theoretically be 90 or 270 but is
practically neither.
}
\examples{
#EXAMPLE circular.averaging
circular.averaging(c(0,90,180,270)) #result is NA
circular.averaging(c(70,82,96,110,119,259))

#EXAMPLE vector.averaging
vector.averaging(c(10,20,70,78,108), distance=10)
vector.averaging(c(159,220,258,273,310),distance=runif(5))
}
\author{
Jeremy VanDerWal \email{jjvanderwal@gmail.com} & Lorena
Falconi \email{lorefalconi@gmail.com}
}

