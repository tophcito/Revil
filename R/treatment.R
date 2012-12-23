##' Treats an individual with a treatment
##'
##' Treatments are specific to their level of infection. A treatment
##' for 80% infected Zs is ineffective for Zs infected more
##' strongly. If this treatment is applied to individuals infected far
##' less, it will even increase their infection status by a factor of
##' ill.effect. Exactly how specific a treatment is, is controlled
##' with the tol parameter. If a treatment is effective, it will heal
##' (set status to 0) the individual.
##' 
##' @title Treating an individual
##' @param status infection status of the individual
##' @param strength strength of the treatment
##' @param ill.effect if a cure is too strong, the individuals status
##'        will be multiplied with this factor.
##' @param tol strength needs to be in a tol-neighbourhood of status,
##'            for the treatment to work.
##' @return the new status of the individual, after treatment has been
##'         applied.
##' @author Christoph Waldhauser
treat <- function(status, strength, ill.effect=1.5, tol=0.05) {
  ## treats an individual with a cure of a certain strength
  ## status: status of the individual
  ## strength: strength of the applied treatment
  ## tol: the specificity of the treatment. if small, then status
  ##      needs to be close to strength for cure to have positive
  ##      effect 
  ## Value: new status of individual

  ## if strength is close to status, status is reduced, else increased
  new.status <- status
  diff <- abs(status-strength)
  if (diff < tol) {
    new.status <- 0
  } else if (status < strength) {
    new.status <- min(new.status*ill.effect, 1)
  }
  return(new.status)
}


##' Applies a cure to an entire population. 
##'
##' A cure is a set of treatments together with the probability of
##' each treatment being used. At the moment, each cure needs to
##' contain ten different treatments. Here, they are being used to
##' treat all those Hs that are infected less then 50%, ie have a
##' status of 0.5. Each treatment is targeted at a specific infection
##' severity. The targets are spread out equally on the range
##' 0.05:0.5.
##'
##' The specification of the cure is just a vector of probabilities,
##' that each treatment is going to be used. If no cure is to be
##' applied, all elements of the vector should be 0.
##'
##' At a later state, the fixed extent of ten treatments per cure will
##' be lifted, so that an arbitrary number (below a fixed maximum) of
##' treatments can form a cure. But this is not yet implemented.
##' @title Curing a population from C-Virus
##' @param population the population to cure
##' @param cure the cure specification (see Details).
##' @param ill.effect if a cure is too strong, the individuals status
##'        will be increased by it's original status times this factor.
##' @param tol strength needs to be in a tol-neighbourhood of status,
##'            for the treatment to work; handed down to treat().
##' @return the population after the cure has been applied.
##' @author Christoph Waldhauser
cure <- function(population, cure, ill.effect=1.5, tol=0.05) {
  ## selects a treament according to cure,
  ## applies it to every individual in the population
  ## population: the statuses of the population
  ## cure: for now, a vector of length 10, given the probability of
  ##          a counter-measure being applied
  ## Value: the population statuses, after the cure has been applied
  stat <- population[,"status"]
  if (any(cure!=0)) {
  ## assumption: Zs > 0.75 cannot be treated (if tol==0.25)
    selected.treatments <- data.frame(status=stat,
                                      treatment=sample(seq(0.05, 0.5,
                                        by=0.05), length(stat),
                                        replace=TRUE, 
                                        prob=cure))
                                    
    new.pop <- apply(selected.treatments, 1, function(i) {
      res <- treat(i["status"], i["treatment"], ill.effect, tol)
      return(res)
    })
  } else {
    new.pop <- stat
  }
  return(data.frame(status=new.pop))
}
