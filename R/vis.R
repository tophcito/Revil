##' Plots Kaplan-Meier curves of the zombie proportion of the
##' population and the median infectiousness.
##'
##' To create visual representations of simulations, classial
##' Kaplan-Meier curves are produced. They contain the proportion of
##' Zs in the population (red) and the median infectiousness of the
##' general population. The Q_{0.05} and Q_{0.95} percentiles are
##' plotted as black, dotted lines.
##'
##' If selected, these curves are produced either for the entire
##' population (Hs and Zs) or just the Zs.
##' @title Kaplan-Meier curves for C-Virus median infection rates
##' @param res the result data frame of a simulation
##' @param Zs.only if TRUE, limit analysis to the Z part of the
##'        population only
##' @return nothing
##' @author Christoph Waldhauser
plot.mir <- function(res, Zs.only=FALSE) {
  if (Zs.only) {
    res <- apply(res, 1, function(day) {
      res <- day[day>0.75]
    })
  }
  plot(apply(res, 1, quantile, probs=0.5), type="l", xlab="Day",
       ylab="Median Infection", col="green", ylim=c(0,1))
  if (!Zs.only) {
    lines(x=1:nrow(res),
          y=apply(res, 1, function(x) sum(x>0.75)/length(x)),
          col="red") 
  }
  lines(x=1:nrow(res), y=apply(res, 1, quantile, probs=0.95), lty=3)
  lines(x=1:nrow(res), y=apply(res, 1, quantile, probs=0.05), lty=3)
}
