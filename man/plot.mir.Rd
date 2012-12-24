\name{plot.mir}
\alias{plot.mir}
\title{Kaplan-Meier curves for C-Virus median infection rates}
\usage{
  plot.mir(res, Zs.only = FALSE)
}
\arguments{
  \item{res}{the result data frame of a simulation}

  \item{Zs.only}{if TRUE, limit analysis to the Z part of
  the population only}
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
\author{
  Christoph Waldhauser
}

