\name{Spatial.plot}
\alias{Spatial.plot}
\title{
Spatial Plot
}
\description{
Spatial (x-y) graph of candidate plots, target plots, pre-existing plots, reference plots and neighbours to pre-existing plots.
}
\usage{
Spatial.plot(popfile, sampfile)
}
\arguments{
  \item{popfile}{
   dataframe containing information on all plots in the population prior to the sample.
}
  \item{sampfile}{
   dataframe containing information on all plots in the population after the sample.
}
}
\value{
Draws an x-y plot showing the location of different plots in each stratum.
}
\author{
G. Melville
}
\seealso{
See also \code{\link{NC.sample}}.
}
\examples{
## Spatial.plot(popfile, sampfile)
}

