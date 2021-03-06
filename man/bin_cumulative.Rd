% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binomial.R
\name{bin_cumulative}
\alias{bin_cumulative}
\title{bin_cumulative}
\usage{
bin_cumulative(trials, prob)
}
\arguments{
\item{trials}{as number of trials}

\item{prob}{as probability number between 0 and 1}
}
\value{
data.frame as class 'bincum'
}
\description{
finds the cumulative probability that sums to 1
}
\examples{
bin_cumulative(trials = 5, prob = 0.5)
plotting binomial cumulative distribution
dis2 <- bin_cumulative(trials = 5, prob = 0.5)
plot(dis2)
#shows a graph of distribution
}
