##' Simulation of D-Virus progression from one day to the next
##'
##' The key concept in this function is virulence. Here it is
##' conceived as the probability of a Z infecting a H. It takes the
##' status of all people in a population, and computes their new
##' statuses at the end of the day.
##'
##' A D-Virus knows only two states of infection: healthy and
##' infected. The probability of Hs being turned by a Z depends on the
##' number of Zs and the virulence of the virus. In case of H being
##' turned into a Z, H's status is changed from FALSE to TRUE.
##' 
##' @title D-Virus progression
##' @param t0 the statuses of all individuals in a population at the
##'           beginning of a day
##' @param virulence the probability of Zs attacking Hs
##' @param verbose if TRUE, print out diagnostic messages
##' @return the statuses of all individuals in a population after all
##'         interactions have taken place.
##' @author Christoph Waldhauser
progression.D <- function(t0, virulence=0.2, verbose=FALSE) {
  nZombies <- sum(t0[,"status"])
  prob <- virulence*(nZombies/nrow(t0))
  if (verbose) message(paste("Prob:", prob))
  t1 <- t0
  t1[!t0[,"status"],"status"] <- sample(c(TRUE, FALSE), size=nrow(t0)-nZombies,
                                        prob=c(prob, 1-prob),
                                        replace=TRUE)
  return(t1)
}


##' Simulation of C-Virus progression from one day to the next
##'
##' The key concept in this function is virulence. Here it is
##' conceived as the probability of a Z infecting a H. It takes the
##' status of all people in a population, and computes their new
##' statuses at the end of the day.
##'
##' A C-Virus knows an infinite number of infection states. However,
##' not every individual infected can in turn pass its infection on to
##' others. Only once a certain treshold has been passed, an
##' individual becomes infective itself.
##'
##' When an infectious carrier meets another indivduals, there is a
##' chance (virulence) of them of giving some of their infection on to
##' the other. How much the status of the other one is being increased
##' is a normally distributed random variable, with a mean and SD of
##' m.cont, SD.cont. This somewhat relates to the strength of a zombie
##' bite, ie how much health a zombie bite drains from its victim.
##'
##' [NB: optionality not yet implemented] To accurately simulate the
##' spread of news, not only Zs influence Hs status. Also, the status
##' of each non-attacked H regresses towards the mean of the H
##' population, and the status of each non-attacked Z towards the mean
##' of the Z population.
##' 
##' @title C-Virus progression
##' @param t0 The statuses of all individuals in a population at the
##'           beginning of a day
##' @param virulence The probability of Zs attacking Hs
##' @param m.cont Mean of contagiousness; by how much is the status of
##'               the victim of an attack increased
##' @param sd.cont SD of contagiousness 
##' @param treshold Individuals infected this much (status), are able
##'                 to spread the disease
##' @return The statuses of all individuals in a population after all
##'         interactions have taken place.
##' @author Christoph Waldhauser
progression.C <- function(t0, virulence=1, m.cont=0.15,
                             sd.cont=0.05, treshold=0.75) {
  t1 <- t0
  isZombie <- t0[,"status"]>treshold
  human.mean <- mean(t0[!isZombie, "status"])
  zombie.mean <- mean(t0[isZombie, "status"])
  ## probability of human being attacked
  probH <- virulence*(sum(isZombie)/nrow(t0))
  ## probability of zombie being attacked
  probZ <- virulence

  ## attacked humans
  isAttackedH <- !isZombie & sample(c(TRUE, FALSE),
                                    size=length(!isZombie),
                                    prob=c(probH, 1-probH),
                                    replace=TRUE)
  t1[isAttackedH, "status"] <- min(t0[isAttackedH, "status"] +
                                   rnorm(sum(isAttackedH), m.cont,
                                         sd.cont), 1)
  
  ## nonAttacked humans
  isNonAttackedH <- !isZombie & !isAttackedH
  t1[isNonAttackedH, "status"] <- t0[isNonAttackedH, "status"] +
    (human.mean - t0[isNonAttackedH, "status"])/2
  
  ## attacked zombies
  isAttackedZ <- isZombie & sample(c(TRUE, FALSE),
                                   size=length(isZombie),
                                   prob=c(probZ, 1-probZ),
                                   replace=TRUE)
  t1[isAttackedZ, "status"] <- min(t0[isAttackedZ, "status"] +
                                   rnorm(sum(isAttackedZ), m.cont,
                                         sd.cont), 1)
  
  ## nonAttacked zombies
  isNonAttackedZ <- isZombie & !isAttackedZ
  t1[isNonAttackedZ, "status"] <- t0[isNonAttackedZ, "status"] +
    (zombie.mean - t0[isNonAttackedZ, "status"])/2
    return(t1)
}
