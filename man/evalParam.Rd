\name{evalParam}
\alias{evalParam}
\title{Bootstrapping simulation results with possibly varying
parameters.}
\usage{
  evalParam(param = "virulence", range = c(0.1, 0.5, 1),
    replications = 5, start.pop = NULL, days = 500, cure,
    sim.params = NULL)
}
\arguments{
  \item{param}{The name of the parameter that is to be
  evaluated}

  \item{range}{The values this parameter is to take
  (vector)}

  \item{replications}{How many times simulations for each
  parameter should be run.}

  \item{start.pop}{The initial population. If NULL, the
  default, then a standard population of size 100 with one
  Z will be created.}

  \item{days}{The number of days each simulation should
  span}

  \item{cure}{The cure to be used in the simulation}

  \item{sim.params}{A parameter list used to specify the
  (other) parameters. If the parameter specified under
  param is contained within that list, this entry will be
  ignored.}
}
\value{
  A list, one entry per parameter value, containing the
  bootstrap results of the proportion of Zs in the
  population (pZs), the mean infection rate (mir), the
  first day a second Z was observed (fZd) and the last day
  a H was observed (lHd) are returned.
}
\description{
  Evaluating the influence of a parameter by bootstrapping
  multiple simulation runs.
}
\details{
  This function is used to assess the influence of
  parameters used to specify the simulation of C-Virus
  outbreaks. To this end, one parameter can be varied, by
  specifying the value it should take. Each evaluation is
  repeated a number of times (bootstrapping) to get an idea
  of the distribution of the results.
}
\examples{
\dontrun{
## Test the effect of different settings for m.cont
res <- evalParam(param="m.cont", range=c(0.15, 0.75), cure=rep(0, times=10))
}
}
\author{
  Christoph Waldhauser
}

