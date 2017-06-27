\name{R.sample1}
\alias{R.sample1}
\title{
Random sample.
}
\description{
Selects random sample in a single stratum.
}
\usage{
R.sample1(popfile, nrefs)
}
\arguments{
  \item{popfile}{
   dataframe containing information on all plots in the stratum.
}
  \item{nrefs}{
   vector containing the required sample size of the stratum.
}
}
\details{
A random sample of virtual plots is selected from the candidate set in the stratum of interest. 
}
\value{
A list with components:-
\item{popfile }{
 population file - dataframe, as above, with plot type of reference plots set to 'R'
}
}
\author{
G. Melville
}
\seealso{
\code{\link{NC.sample}}.
}
\examples{
## R.sample1(popfile, nrefs)
}

