\name{summary.sim}
\alias{summary.sim}
\title{Zombie Outbreak simulation summary}
\usage{
  \method{summary}{sim} (object, ...)
}
\arguments{
  \item{object}{the result of a simulation run}

  \item{...}{currently ignored}
}
\value{
  a vector with the proportion of Zs (pZs) in and the mean
  infection rate (mir) of the population, along with the
  number of the first day a second Z was observed (fZd) and
  the last day a H was observed (lHd). Finally, the days
  the proportion of Zs exceeds percentiles (10 percent
  steps).
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
  was observed, are returned. Then also the distribution of
  the proportion of Zs in the population (in 10% steps) is
  returned.
}
\author{
  Christoph Waldhauser
}

