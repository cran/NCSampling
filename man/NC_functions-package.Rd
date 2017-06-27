\name{NCSampling-package}
\alias{NCSampling-package}
\alias{NCSampling}
\docType{package}
\title{
Nearest Centroid (NC) Sampling
}
\description{
Suite of functions to perform NC sampling. Used by forestry practitioners to select reference plots for imputation using remotely sensed data, for example aerial laser scanning (ALS) data. 
}
\details{
\tabular{ll}{
Package: NCSampling \cr
Type: Package\cr
Version: 1.0\cr
Date: 2017-06-26\cr
License: GPL-2\cr

  \cr

Depending on the application, the functions are usually called in the following order:- \cr
   Check.pop - check population file for errors \cr 
   Alloc - allocate sample numbers to strata \cr 
   Existing - determine the virtual plots, in the target set, which are neighbours to pre-existing plots \cr 
   Alloc - re-allocate sample numbers to strata, taking into account pre-existing plots and their neighbours \cr 
   NC.sample - select reference plots from the candidate set, using the internal functions Centroids and NC.select. \cr 
   Spatial.plot - display the virtual plots, including the NC sample plots, as an x-y graph.  \cr 
   DesVar - calculate approximate design variances for each stratum and for the whole population.   
}
}
\author{
G Melville
Maintainer: <gavin.melville@dpi.nsw.gov.au>
}
\references{
G. Melville & C. Stone. (2016) Optimising nearest neighbour information - a simple, efficient sampling strategy for forestry plot imputation using remotely sensed data. Australian Forestry, 79:3, 217:228, DOI: 10.1080/00049158.2016.1218265.
}
\keyword{ package }

