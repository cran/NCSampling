\name{DVar}
\alias{DVar}
\title{
Design variances for single stratum.
}
\description{
For a single stratum approximate design variances are calculated. Not intended to be called directly.
}
\usage{
DVar(popfile, nrefs, yvars, desvars, kvalue, B=1000)
}
\arguments{
  \item{popfile}{
   dataframe containing information on stratum of interest.
}
  \item{nrefs}{
   scalar containing the sample size of the stratum.
}
  \item{yvars}{
   character vector containing the name of each variable of interest (dependent variable) for which design variances are required.
}
  \item{desvars}{
   character vector containing the names of the design variables.
}
  \item{kvalue}{
   scalar specifying the value of k for the k-nn imputation.
}
  \item{B}{
   number of re-samples used to calculate the design variances.
}
}
\value{
A dataframe containing the design variances for the stratum of interest. Data used to calculate these are also returned.
}
\author{
G. Melville
}
\seealso{
\code{\link{NC.sample}}, \code{\link{DesVar}}.
}
\examples{
## DesVar(popfile, nrefs, yvars, kvalue, desvars, B=1000) 
}

