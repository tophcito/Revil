\name{cure}
\alias{cure}
\title{Curing a population from C-Virus}
\usage{
  cure(population, cure, ill.effect = 1.5, tol = 0.05)
}
\arguments{
  \item{population}{the population to cure}

  \item{cure}{the cure specification (see Details).}

  \item{ill.effect}{if a cure is too strong, the
  individuals status will be increased by it's original
  status times this factor.}

  \item{tol}{strength needs to be in a tol-neighbourhood of
  status, for the treatment to work; handed down to
  treat().}
}
\value{
  the population after the cure has been applied.
}
\description{
  Applies a cure to an entire population.
}
\details{
  A cure is a set of treatments together with the
  probability of each treatment being used. At the moment,
  each cure needs to contain ten different treatments.
  Here, they are being used to treat all those Hs that are
  infected less then 50%, ie have a status of 0.5. Each
  treatment is targeted at a specific infection severity.
  The targets are spread out equally on the range 0.05:0.5.

  The specification of the cure is just a vector of
  probabilities, that each treatment is going to be used.
  If no cure is to be applied, all elements of the vector
  should be 0.

  At a later state, the fixed extent of ten treatments per
  cure will be lifted, so that an arbitrary number (below a
  fixed maximum) of treatments can form a cure. But this is
  not yet implemented.
}
\author{
  Christoph Waldhauser
}

