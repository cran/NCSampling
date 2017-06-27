\name{NC.sample}
\alias{NC.sample}
\title{
Nearest Centroid (NC) Sample
}
\description{
Selects NC sample in multiple strata.
}
\usage{
NC.sample(popfile, nrefs, desvars, ctype, imax, nst)
}
\arguments{
  \item{popfile}{
   dataframe containing information on all plots in the population.
}
  \item{nrefs}{
   vector containing the sample size of each stratum.
}
  \item{desvars}{
   vector containing the names of the design variables.
}
  \item{ctype}{
   clustering type - either k-means ('km') or Wards D ('WD').
}
  \item{imax}{
   maximum number of iterations for the k-means procedure.
}
  \item{nst}{
   number of initial random sets of cluster means for the k-means procedure.
}
}
\details{
In each stratum the population of virtual plots is segregated into n clusters where n is the stratum sample size (number of reference plots).
The virtual plots are partitioned so as to minimise the sums of squares of distances from plots to cluster centroids. This is achieved by using a multivariate clustering procedure such as k-means clustering (Hartigan & Wong, 1979) or Ward's D clustering (Murtagh & Legendre, 2013), using standardized design variables and a Euclidean distance metric. Following determination of the cluster centroids, the virtual plot, in the candidate set, closest to each centroid is selected as a reference plot. 
}
\value{
A list with components:-
\item{popfile }{
   population file - dataframe, as above, with reference plots designated as 'R'
}
\item{cmns }{
   centroid means
}
}
\references{
G. Melville & C. Stone. (2016) Optimising nearest neighbour information - a simple, efficient sampling strategy for forestry plot imputation using remotely sensed data. Australian Forestry, 79:3, 217:228, DOI: 10.1080/00049158.2016.1218265.

Hartigan & Wong (1979) Algorithm AS 136: a K-means clustering algorithm. Applied Statistics 28, 100-108, DOI:10.2307/2346830.

Murtagh, M & Legendre, P. (2013) Ward's hierarchical agglomerative clustering method: Which algorithms implement Ward's criterion? Journal of Classification.
}
\author{
G. Melville
}
\seealso{
See also \code{\link{NC.sample}}.
}
\examples{
## NC.sample(popfile, nrefs, desvars, ctype='km', imax=200, nst=20) 
}

