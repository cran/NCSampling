\name{training}
\alias{training}
\docType{data}
\title{
Nundle State Forest LiDAR data
}
\description{
Contains LiDAR data for 200 plots from two strata acquired by over-flying the Nundle State Forest (SF), NSW, Australia in 2011}
\usage{data(training)}
\format{
  A data frame with 200 observations on the following 10 variables.
  \describe{
    \item{\code{OV}}{ a numeric vector containing LiDAR occupied volume }
    \item{\code{height}}{ numeric vector containing LiDAR heights }
    \item{\code{cc}}{ a numeric vector containing LiDAR canopy cover }
    \item{\code{pstk}}{ a numeric vector containing LiDAR stocking rate }
    \item{\code{var}}{ a numeric vector containing LiDAR height variances }
    \item{\code{x}}{ a numeric vector containing x-coordinates }
    \item{\code{y}}{ a numeric vector containing y-coordinates }
    \item{\code{Strata}}{a factor with levels \code{O} \code{Y}}
    \item{\code{PID}}{ numeric vector containing unique plot IDs }
    \item{\code{plot_type}}{a factor with levels \code{B} \code{C} \code{T}}
  }
}
\details{
The LiDAR variables were calculated as outlined in Turner et al. (2011).
}
\source{
Forestry Corporation of NSW }
\references{
Melville G, Stone C, Turner R (2015). Application of LiDAR data to maximize the efficiency of inventory plots in softwood plantations. New Zealand Journal of Forestry Science, 45:9,1-16. doi:10.1186/s40490-015-0038-7. 

Stone C, Penman T, Turner R (2011). Determining an optimal model for processing lidar data at the plot level: results for a Pinus radiata plantation in New SouthWales, Australia. New Zealand Journal of Forestry Science, 41, 191-205.

Turner R, Kathuria A, Stone C (2011). Building a case for lidar-derived structure stratification for Australian softwood plantations. In Proceedings of the SilviLaser 2011 conference, Hobart, Tasmania, Australia.
}
\examples{
data(training)
}
\keyword{datasets}
