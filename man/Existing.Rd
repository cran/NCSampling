\name{Existing}
\alias{Existing}
\title{
Pre-existing plot neighbours
}
\description{
Determines the plots which are close, in the auxiliary space, to the pre-existing plots.
}
\usage{
Existing(popfile, nrefs, desvars, draw.plot)
}
\arguments{
  \item{popfile}{
   dataframe containg information on all plots in the population file.
}
  \item{nrefs}{
   vector containing the number of reference plots in each stratum.
}
  \item{desvars}{
   vector containing the names of the design variables.
}
  \item{draw.plot}{
   logical variable - should a bar graph be drawn to show the number of neighbours for each pre-existing plot?
}
}
\value{
A list with components:-
	\item{Nx}{
  	vector containing the number of neighbours to existing plots in each stratum.
}
	\item{Ng}{
  	vector containing the number of target plots in each stratum. 
}
	\item{popfile}{
  	dataframe containing the original population file with neighbours to pre-existing plots separately identified.
}
}
\author{
G Melville.
}
\seealso{
\code{\link{NC.sample}}.
}
\examples{
## Existing(popfile, nrefs, desvars, draw.plot=T) 
}
