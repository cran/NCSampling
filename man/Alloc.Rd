\name{Alloc}
\alias{Alloc}
\title{
Allocation
}
\description{
Allocate sample among several strata, using proportional allocation.
Inputs population file and total sample size.
Outputs sample sizes for each stratum
}
\usage{
Alloc(popfile, ntotal)
}
\arguments{
  \item{popfile}{
   dataframe containing population data - as a minimum there must be columns named 'PID' (plot identifier), 'Strata' and 'plot_type'.
}
  \item{ntotal}{
   total sample size  - required number of reference plots for all strata combined.
}
}
\details{
Performs a proportional allocation, by calculating the required sample size for each stratum (i) using the formula n_i = n * N_i / N, where n is the sample size (number of reference plots) and N is the number of target plots.
}
\value{
A vector of sample sizes, one for each stratum in the population file.
}
\author{
G. Melville
}
\seealso{
\code{\link{Existing}} and \code{\link{NC.sample}}.
}
\examples{
popfile<-data.frame(PID=1:20, Strata=rep(c('A', 'B'),c(12,8)),
 	plot_type=rep('B',20))
tot.samp<-6
Alloc(popfile, tot.samp)
}

