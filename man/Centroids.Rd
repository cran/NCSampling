\name{Centroids}
\alias{Centroids}
\title{
Calculate centroids
}
\description{
Separates a single stratum of the population file into n clusters and finds the centroid of each cluster, where n is the sample size. Not intended to be called directly.
}
\usage{
Centroids(popfile, nrefs, desvars, ctype, imax, nst)
}
\arguments{
  \item{popfile}{
   population file - dataframe containing information relating to all plots in the stratum.
}
  \item{nrefs}{
   scalar defining the number of reference plots - required sample size for the stratum.
}
  \item{desvars}{
   character vector containing the names of the design variables.
}
  \item{ctype}{
   clustering type - either k-means ('km') or Ward's D2 ('WD').
}
  \item{imax}{
   maximum number of iterations when calling the k-means clustering procedure.
}
  \item{nst}{
   number of random initial centroid sets when calling the k-means clustering procedure.
}
}
\details{
The virtual plots are partitioned so as to minimise the sums of squares of distances from plots to cluster centroids. This is done by using a multivariate clustering procedure such as k-means clustering (Hartigan & Wong, 1979) or Ward's D2 clustering (Murtagh & Legendre, 2013), using standardized design variables and a Euclidean distance metric.
}
\value{
\item{centroids }{
   dataframe containing centroids.
}
\item{cmns }{
   dataframe containing centroid means.
}
}
\references{
Hartigan & Wong (1979) Algorithm AS 136: a K-means clustering algorithm. Applied Statistics 28, 100-108, DOI:10.2307/2346830.
  
Murtagh, M & Legendre, P. (2014) Ward's hierarchical agglomerative clustering method: which algorithms implement Ward's criterion? Journal of Classification, 31, 274-295, DOI: 10.1007/s00357-014-9161-z.
}
\author{
G Melville
}
\seealso{
\code{\link{Existing}, \link{NC.sample} and kmeans.
}
}
\examples{
## Centroids(popfile, nrefs, desvars, ctype='km', imax=200, nst=20) 
}

