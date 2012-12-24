\name{newPop}
\alias{newPop}
\title{Create a new population}
\usage{
  newPop(type = c("c", "d"), size = 100, z.prop = 1/size)
}
\arguments{
  \item{type}{either "c" or "d", creating a continous or
  discrete population}

  \item{size}{the intended size of the population}

  \item{z.prop}{the proportion of Zs in the population}
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

