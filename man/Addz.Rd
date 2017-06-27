\name{Addz}
\alias{Addz}
\title{
Addz
}
\description{
Add variable/s to the population file which are good predictors of the variables/s of interest
}
\usage{
Addz(popfile, training, yvars, xvars, pool)
}
\arguments{
  \item{popfile}{
  dataframe containing population data - as a minimum there must be columns named 'PID' (plot identifier), 'Strata' and 'plot_type'.
}
  \item{training}{
  dataframe containing training data. Must contain auxiliary variables and variable/s of interest.
}
  \item{yvars}{
   vector containing the name of each variable of interest (dependent variable).
}
  \item{xvars}{
   vector containing the names of the auxiliary variables.}
  \item{pool}{
   logical value - should the training data be pooled across strata prior to fitting the regression model?}
}
\details{
The predictor variable for the each variable of interest (dependent variable) is obtained by performing random forest regression on the training data using the designated auxiliary variables. The training data can be pooled across strata (pool=T), or fitted separately within each strata (the default). Not normally called directly.
}
\value{
A list with components:-
\item{popfile }{population file - data frame, as above, with predictor variable/s added to the file
}
\item{r.sqared }{dataframe containing the R-squared values obtained from the random forest regression/s
}
}
\references{
Random forest regression is performed using the randomForest package. 
}
\author{
G. Melville
}
\seealso{
\code{\link{DesVar}, randomForest}.
}
\examples{
## Addz(popfile, training, yvars, xvars)
}

