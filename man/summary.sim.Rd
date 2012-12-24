\name{summary.sim}
\alias{summary.sim}
\title{Zombie Outbreak simulation summary}
\usage{
  summary.sim(res)
}
\arguments{
  \item{res}{the result of a simulation run}
}
\value{
  a vector with the proportion of Zs (pZs) in and the mean
  infection rate (mir) of the population.
}
\description{
  Summarizes the results of a simulation
}
\details{
  This function summarizes the results of a simulation by
  first computing the median infection rate and the
  proportion of Zs in the population, and then returning
  the mean values over all days. Additionally, the first
  day where more than one Z occured and the last day a H
  was observed, are returned.
}
\author{
  Christoph Waldhauser
}

