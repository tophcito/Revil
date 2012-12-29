##' Simulating the outbreak of a C-Virus induced zombie infestation.
##'
##' The simulation framework repeatedly calls the disease progression
##' and cure functions on a population for a given number of
##' days. This then returns a data frame with individuals as columns
##' and days in the rows.
##'
##' The parameters controlling the simulation can be set using the
##' params argument. Currently, the parameters of progression.C()
##' (virulence, m.cont, sd.cont) and the ill.effect and tol parameters
##' of treat() can be set this way.
##' @title Simulation of C-Virus Zombies
##' @param start.pop the initial population
##' @param days the number of days the simulation should cover (not
##'             walltime days, hopefully)
##' @param cure a cure specification. See cure.
##' @param verbose if TRUE, diagnostic messages will be output.
##' @param params a named list of parameters controlling the
##'               simulation. See Details.
##' @return a data frame with dimensions [days, nrow(start.pop)]
##' @author Christoph Waldhauser
##' @export 
##' @examples
##' ## create a population
##' t0 <- newPop("c", size=20)
##'
##' ## run a simulation over 15 days, with no cure
##' res <- runSim(t0, 15, cure=rep(0, times=10))
runSim <- function(start.pop, days, cure, verbose=TRUE,
                   params=NULL) {
  ## running a simulation
  ## start.pop: the starting population
  ## days: the number of days to simulate
  ## cure: the mixture of ten treatments
  p <-
    list(virulence=ifelse(is.null(params[["virulence"]]), 1, params[["virulence"]]),
         m.cont=ifelse(is.null(params[["m.cont"]]), 0.15, params[["m.cont"]]),
         sd.cont=ifelse(is.null(params[["sd.cont"]]), 0.05, params[["sd.cont"]]),
         ill.effect=ifelse(is.null(params[["ill.effect"]]), 1.5, params[["ill.effect"]]),
         tol=ifelse(is.null(params[["tol"]]), 0.05, params[["tol"]]))
  
  active.t <- start.pop
  res <- as.data.frame(vector("list", length=0))
  if (verbose) message(paste("Running for", days, "days: "),
                       appendLF=FALSE)
  
  for (i in 1:days) {
    if ((i%%100)==0) {
      if (verbose) message(i/100, appendLF=FALSE)
    } else {
      if (verbose) message(".", appendLF=FALSE)
    }
    active.t <- progression.C(active.t, virulence=p[["virulence"]],
                              m.cont=p[["m.cont"]],
                              sd.cont=p[["sd.cont"]])
    active.t <- cure(active.t, cure, ill.effect=p[["ill.effect"]],
                     tol=p[["tol"]]) 
    res <- rbind(res, active.t[,"status"])
  }
  if (verbose) message(" ")
  class(res) <- c("sim", "data.frame")
  return(res)
}

##' Summarizes the results of a simulation
##'
##' This function summarizes the results of a simulation by first
##' computing the median infection rate and the proportion of Zs in
##' the population, and then returning the mean values over all
##' days. Additionally, the first day where more than one Z occured
##' and the last day a H was observed, are returned. Then also the
##' distribution of the proportion of Zs in the population (in 10%
##' steps) is returned.
##' @title Zombie Outbreak simulation summary
##' @method summary sim
##' @param object the result of a simulation run
##' @param ... currently ignored
##' @return a vector with the proportion of Zs (pZs) in and the mean
##' infection rate (mir) of the population, along with the number of
##' the first day a second Z was observed (fZd) and the last day a H
##' was observed (lHd). Finally, the days the proportion of Zs exceeds
##' percentiles (10 percent steps).
##' @author Christoph Waldhauser
##' @export 
summary.sim <- function(object, ...) {
  ## summarises the results of a simulation
  res <- as.data.frame(object)
  pZs <- apply(res, 1,
                    function(day) sum(day>0.75)/length(day))
  mir <- apply(res, 1,
                    function(day) quantile(day, prob=0.5))

  pZdist <- cbind(seq(0, 1, by=0.1), match(0:10,
                              findInterval(pZs, seq(0.1, 1, by=0.1))))
  nZdist <- cbind(c(2, length(pZs)),
                  match(1:2, findInterval(pZs*ncol(object), c(2, ncol(object)))))
  fZd <- nZdist[1,2]
  lHd <- nZdist[2,2]-1
  res <- list(mean(pZs), mean(mir), as.integer(fZd), as.integer(lHd), pZdist)
  names(res) <- c("pZs", "mir", "fZd", "lHd", "pZdist")
  
  return(res)
}


##' Evaluating the influence of a parameter by bootstrapping multiple
##' simulation runs.
##'
##' This function is used to assess the influence of parameters used
##' to specify the simulation of C-Virus outbreaks. To this end, one
##' parameter can be varied, by specifying the value it should
##' take. Each evaluation is repeated a number of times
##' (bootstrapping) to get an idea of the distribution of the results.
##' @title Bootstrapping simulation results with possibly varying
##' parameters.
##' @param param The name of the parameter that is to be evaluated
##' @param range The values this parameter is to take (vector)
##' @param replications How many times simulations for each parameter
##'                     should be run.
##' @param start.pop The initial population. If NULL, the default,
##'                  then a standard population of size 100 with one Z
##'                  will be created.
##' @param days The number of days each simulation should span
##' @param cure The cure to be used in the simulation
##' @param sim.params A parameter list used to specify the (other)
##'                   parameters. If the parameter specified under
##'                   param is contained within that list, this entry
##'                   will be ignored.
##' @return A list, one entry per parameter value, containing the
##' bootstrap results of the proportion of Zs in the population (pZs),
##' the mean infection rate (mir), the first day a second Z was
##' observed (fZd) and the last day a H was observed (lHd) are
##' returned.
##' @author Christoph Waldhauser
##' @export 
##' @examples \dontrun{
##' ## Test the effect of different settings for m.cont
##' res <- evalParam(param="m.cont", range=c(0.15, 0.75), cure=rep(0, times=10))
##' }
evalParam <- function(param="virulence", range=c(0.1, 0.5, 1),
                      replications=5, start.pop=NULL, days=500, cure,
                      sim.params=NULL) {
  if (is.null(start.pop)) {
    start.pop <- newPop("c", size=100)
  }
  param.iter <- as.list(range)
  this.params <- sim.params
  if (is.null(sim.params)) this.params <- as.list(this.params)

  res <- lapply(param.iter, function(iter) {
    this.params[[param]] <- iter
    this.res <- data.frame(nPs=NULL, mir=NULL, fZd=NULL, lHd=NULL)

    for (i in 1:replications) {
      sim.res <- runSim(start.pop, days, cure, verbose=FALSE,
                        params=this.params)
      this.res <- rbind(this.res, unlist(summary.sim(sim.res)[1:4]))
    }
    colnames(this.res) <- c("pZs", "mir", "fZd", "lHd")
    return(this.res)
  })
  names(res) <- range
  return(res)
}

