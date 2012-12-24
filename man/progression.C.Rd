\name{progression.C}
\alias{progression.C}
\title{C-Virus progression}
\usage{
  progression.C(t0, virulence = 1, m.cont = 0.15,
    sd.cont = 0.05, treshold = 0.75)
}
\arguments{
  \item{t0}{The statuses of all individuals in a population
  at the beginning of a day}

  \item{virulence}{The probability of Zs attacking Hs}

  \item{m.cont}{Mean of contagiousness; by how much is the
  status of the victim of an attack increased}

  \item{sd.cont}{SD of contagiousness}

  \item{treshold}{Individuals infected this much (status),
  are able to spread the disease}
}
\value{
  The statuses of all individuals in a population after all
  interactions have taken place.
}
\description{
  Simulation of C-Virus progression from one day to the
  next
}
\details{
  The key concept in this function is virulence. Here it is
  conceived as the probability of a Z infecting a H. It
  takes the status of all people in a population, and
  computes their new statuses at the end of the day.

  A C-Virus knows an infinite number of infection states.
  However, not every individual infected can in turn pass
  its infection on to others. Only once a certain treshold
  has been passed, an individual becomes infective itself.

  When an infectious carrier meets another indivduals,
  there is a chance (virulence) of them of giving some of
  their infection on to the other. How much the status of
  the other one is being increased is a normally
  distributed random variable, with a mean and SD of
  m.cont, SD.cont. This somewhat relates to the strength of
  a zombie bite, ie how much health a zombie bite drains
  from its victim.

  [NB: optionality not yet implemented] To accurately
  simulate the spread of news, not only Zs influence Hs
  status. Also, the status of each non-attacked H regresses
  towards the mean of the H population, and the status of
  each non-attacked Z towards the mean of the Z population.
}
\examples{
## create a population size 10
t0 <- newPop("c", size=20)

## have it experience one day of C-Virus spreading
t1 <- progression.C(t0)
}
\author{
  Christoph Waldhauser
}

