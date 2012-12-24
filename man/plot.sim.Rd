\name{plot.sim}
\alias{plot.sim}
\title{Kaplan-Meier curves for C-Virus median infection rates}
\usage{
  \method{plot}{sim} (x, y = NULL, Zs.only = FALSE, ...)
}
\arguments{
  \item{x}{the result of a simulation}

  \item{y}{not implemented}

  \item{Zs.only}{if TRUE, limit analysis to the Z part of
  the population only}

  \item{...}{currently ignored}
}
\value{
  nothing
}
\description{
  Plots Kaplan-Meier curves of the zombie proportion of the
  population and the median infectiousness.
}
\details{
  To create visual representations of simulations, classial
  Kaplan-Meier curves are produced. They contain the
  proportion of Zs in the population (red) and the median
  infectiousness of the general population. The Q_{0.05}
  and Q_{0.95} percentiles are plotted as black, dotted
  lines.

  If selected, these curves are produced either for the
  entire population (Hs and Zs) or just the Zs.
}
\examples{
t0 <- newPop("c", size=25)
res <- runSim(t0, days=10, cure=rep(0, times=10))
plot(res)
}
\author{
  Christoph Waldhauser
}

