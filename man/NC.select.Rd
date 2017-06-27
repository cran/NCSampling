\name{NC.select}
\alias{NC.select}
\title{
Nearest Centroid (NC) Plot Selection
}
\description{
Select the reference plots closest, in the auxiliary space, to the target plot centroids. Not intended to be called directly.
}
\usage{
NC.select(popfile, nrefs, desvars, centroids)
}
\arguments{
  \item{popfile}{
   dataframe containing information on all plots in the stratum.
}
  \item{nrefs}{
   vector containing the number of reference plots in the stratum.
}
  \item{desvars}{
   vector containing the names of the design variables.
}
  \item{centroids}{
   dataframe containing the centroids for the stratum.
}
}
\value{
A list with components:-
  \item{refs }{
   dataframe containing reference plots}
  \item{exist }{
   dataframe containing pre-existing plots}
  \item{targs }{
   dataframe containing target plots}
}
\author{
G. Melville
}
\seealso{
\code{\link{NC.sample}}.
}
\examples{
## NC.select(popfile, nrefs, desvars, centroids) 
}

