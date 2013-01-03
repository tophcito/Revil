\name{newPop}
\alias{newPop}
\title{Create a new population}
\usage{
  newPop(type = c("c", "d"), size = 100, z.prop = 1/size,
    h.spread = c(0, 0), z.spread = c(1, 1))
}
\arguments{
  \item{type}{either "c" or "d", creating a continous or
  discrete population}

  \item{size}{the intended size of the population}

  \item{z.prop}{the proportion of Zs in the population}

  \item{h.spread}{the range of the uniform distribution
  from which the healthy part of the population is being
  generated. The default value of c(0,0) indicates that no
  healthy humans are infected at all. This parameter is
  ignored, if type="d".}

  \item{z.spread}{the range of the uniform distribution
  from which the Z part of the population is being
  generated. The default value of c(1,1) indicates that all
  zombies are infected with the maximum possible infection
  status of 1. This parameter is ignored, if type="d".}
}
\value{
  a population
}
\description{
  Creates an initial population of the specified size with
  a certain number of Zs in it.
}
\details{
  newPop() creates a population suitable to be used in
  conjunction with the disease spreading functions. A
  created population will be of class pop. It can contain
  either a population for the continous or the discrete
  viral infection models.

  In reality, this is only a data frame with a single
  column, status, and associated class methods.
}
\examples{
t0 <- newPop("c")
t1 <- newPop("d", size=50, z.prop=0.5)
}
\author{
  Christoph Waldhauser
}

