\name{progression.D}
\alias{progression.D}
\title{D-Virus progression}
\usage{
  progression.D(t0, virulence = 0.2, verbose = FALSE)
}
\arguments{
  \item{t0}{the statuses of all individuals in a population
  at the beginning of a day}

  \item{virulence}{the probability of Zs attacking Hs}

  \item{verbose}{if TRUE, print out diagnostic messages}
}
\value{
  the statuses of all individuals in a population after all
  interactions have taken place.
}
\description{
  Simulation of D-Virus progression from one day to the
  next
}
\details{
  The key concept in this function is virulence. Here it is
  conceived as the probability of a Z infecting a H. It
  takes the status of all people in a population, and
  computes their new statuses at the end of the day.

  A D-Virus knows only two states of infection: healthy and
  infected. The probability of Hs being turned by a Z
  depends on the number of Zs and the virulence of the
  virus. In case of H being turned into a Z, H's status is
  changed from FALSE to TRUE.
}
\author{
  Christoph Waldhauser
}

