\name{DesVar}
\alias{DesVar}
\title{
Design variances for NC sample.
}
\description{
For each stratum ,and for the population as a whole, approximate design variances are calculated.
}
\usage{
DesVar(popfile, nrefs, desvars, yvars, kvalue, B=1000, zvars=NULL, 

training=NULL, xvars=NULL, pool=F)
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
  \item{yvars}{
   character vector containing the name of each variable of interest (dependent variable) for which design variances are required.
}
  \item{kvalue}{
   scalar specifying the value of k for the k-nn imputation.
}
  \item{B}{
   number of re-samples used to calculate the design variances.
}
  \item{zvars}{
   character vector containing the name/s of the predictor variables.
}
  \item{training}{
   dataframe containing the data needed to determine the predictor variable. Must contain the necessary yvars and xvars. If missing, predictor variables are supplied by the user (zvars)
}
  \item{xvars}{
   character vector containing the name/s of the predictor variables.
}
  \item{pool}{
   logical value - should strata be pooled prior to fitting regression model?
}
}
\details{
Approximate design variances are calculated using a re-sampling procedure in conjunction with a predictor variable. The predictor variable can be user-supplied or determined by the program using random forest regression based on a set of training data. The regression model can be fitted separately for each strata (pool=F), the default, or based on pooled training data with stratum included in the regression model as a factor.
}
\value{
A dataframe containing the design variances for each stratum and for the whole population.
}
\author{
G. Melville
}
\seealso{
\code{\link{NC.sample}}.
}
\examples{
## DesVar(popfile, nrefs, desvars, yvars, B=1000, zvars=NULL, 
##   training=NULL, xvars=NULL, pool=F) 
}

