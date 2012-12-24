\name{runSim}
\alias{runSim}
\title{Simulation of C-Virus Zombies}
\usage{
  runSim(start.pop, days, cure, verbose = TRUE,
    params = list(virulence = 1, m.cont = 0.15, sd.cont = 0.05, ill.effect = 1.5, tol = 0.05))
}
\arguments{
  \item{start.pop}{the initial population}

  \item{days}{the number of days the simulation should
  cover (not walltime days, hopefully)}

  \item{cure}{a cure specification. See cure.}

  \item{verbose}{if TRUE, diagnostic messages will be
  output.}

  \item{params}{a named list of parameters controlling the
  simulation. See Details.}
}
\value{
  a data frame with dimensions [days, nrow(start.pop)]
}
\description{
  Simulating the outbreak of a C-Virus induced zombie
  infestation.
}
\details{
  The simulation framework repeatedly calls the disease
  progression and cure functions on a population for a
  given number of days. This then returns a data frame with
  individuals as columns and days in the rows.

  The parameters controlling the simulation can be set
  using the params argument. Currently, the parameters of
  progression.C() (virulence, m.cont, sd.cont) and the
  ill.effect and tol parameters of treat() can be set this
  way.
}
\author{
  Christoph Waldhauser
}

