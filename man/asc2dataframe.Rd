\name{asc2dataframe}
\Rdversion{1.1}
\alias{asc2dataframe}
\alias{dataframe2asc}

\title{ Ascii Grid Files to Dataframe and Dataframe to Ascii Grid Files }
\description{ 
\code{asc2dataframe} converts a list of Esri ascii grid formatted files to a data.frame consisting of only locations with data.\cr\cr 
\code{dataframe2asc} converts a data.frame or matrix with spatial data to Esri ascii grid formatted files. 
}
\usage{
asc2dataframe(filenames, varnames = NULL, gz = FALSE)
dataframe2asc(tdata, filenames = NULL, outdir = getwd(), gz = FALSE)
}

\arguments{
  \item{filenames}{is a vector of file names}
  \item{varnames}{is a vector of names for the output columns, and must be the same length as files}
  \item{tdata}{is the data.frame which has y, x coordinates (OR lat,lon) and columns for the data to be output (MUST be in that order)}
  \item{outdir}{is the output directory, the default is the current working directory}
  \item{gz}{boolean defining if the ascii grid files are gzip compressed}
}

\details{ 
asc2dataframe:  The ascii grid files can be read in gzip compress format.
The dataframe returned contains the X and Y coordinate columns followed by columns of data.

dataframe2asc: If filenames is null, column names will be used. The data.frame has to 
contain the Y and X coordinates and the data as columns. The ascii grid files can be 
created as gzip compress format and would be saved in the outdir.
}
\value{ 
	\item{asc2dataframe }{Returns a dataframe with XY coordinates and the data of each ascii grid files, as columns.}
	\item{dataframe2asc }{ Returns an asc grid file for each data column within the data.frame.}
}
\author{ Lorena Falconi \email{lorefalconi@gmail.com} }
\examples{
#Create 2 ascii files
y=seq(10,50,0.5)
x=seq(140,180,0.5)
cellsize=0.5
data1=sample(160,140)
data2=sample(158,140)
out1.asc=as.asc(matrix(data1,nc=y, nr=x), xll=min(x), yll=min(y), cellsize=cellsize)
out2.asc=as.asc(matrix(data2,nc=y, nr=x), xll=min(x), yll=min(y), cellsize=cellsize)
#write the ascii files to the work directory
write.asc(out1.asc, 'out1.asc')
write.asc(out2.asc, 'out2.asc')
#list the ascii files
ascfiles=c('out1.asc', 'out2.asc')
#generate a dataframe from the ascii files
tdata=asc2dataframe(ascfiles)
tdata

#remove the files
unlink('out1.asc'); unlink('out2.asc')

#convert the dataframe tdata to ascii grid files
dataframe2asc(tdata)

#remove the files
unlink('var.1.asc'); unlink('var.2.asc')
}
