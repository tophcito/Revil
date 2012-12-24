\name{treat}
\alias{treat}
\title{Treating an individual}
\usage{
  treat(status, strength, ill.effect = 1.5, tol = 0.05)
}
\arguments{
  \item{status}{infection status of the individual}

  \item{strength}{strength of the treatment}

  \item{ill.effect}{if a cure is too strong, the
  individuals status will be multiplied with this factor.}

  \item{tol}{strength needs to be in a tol-neighbourhood of
  status, for the treatment to work.}
}
\value{
  the new status of the individual, after treatment has
  been applied.
}
\description{
  Treats an individual with a treatment
}
\details{
  Treatments are specific to their level of infection. A
  treatment for 80% infected Zs is ineffective for Zs
  infected more strongly. If this treatment is applied to
  individuals infected far less, it will even increase
  their infection status by a factor of ill.effect. Exactly
  how specific a treatment is, is controlled with the tol
  parameter. If a treatment is effective, it will heal (set
  status to 0) the individual.
}
\author{
  Christoph Waldhauser
}

