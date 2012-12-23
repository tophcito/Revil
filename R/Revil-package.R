##' Revil
##'
##' This code simulates the outbreak of zombie infestation caused by a
##' C-Virus. A C-Virus is a disease that is not like a classical
##' disease knowing only a dichotmous state of
##' infected--not-infected. Instead, infectedness is conceptualized on
##' a continous scale. It therefore is much more akin to political
##' attitudes like xenophobia or fascism.

##' In addition, simulation is also available for a D-Virus, that
##' knows only two states of infection.
##'
##' The following description will cover
##' \itemize{
##'   \item Data structure definition
##'   \item Disease progression
##'   \item Treatments
##'   \item Simulation framework
##'   \item Visualization
##' }
##'
##' @section Data structure definition:
##' The data structure at the foundation of the simulator is a simple,
##' one-column data frame. Each line corresponds to an individual, and
##' the data field contains that person's status. In case of a simple
##' D-Virus infection, TRUE or FALSE (TRUE being infected). In case of
##' a more complex C-Virus outbreak, status is numeric, in the range
##' of 0:1, given the proportion of a person being infected. So 0
##' means totally healty, 1 totally infected.
##' 
##' @section Disease progression:
##' The disease progression functions simulate how the disease spreads
##' through the population. There are two functions, one for the C-
##' and one for the D-Virus. The key concept in both functions is
##' virulence. Here it is conceived as the probability of a Z
##' infecting a H. It takes the status of all people in a population,
##' and computes their new statuses at the end of the day.

##' @section Treatments:
##' The following functions apply treatments of a C-Virus outbreak to
##' a population. Here, two concepts are introduced. A cure is a set
##' of different treatments that have the potential to reduce the
##' infection in an individual. However, these treatments are specific
##' to their level of infection. A treatment for 80% infected Zs is
##' ineffective for Zs infected more strongly. If this treatment is
##' applied to individuals infected far less, it will even increase
##' their infection status.
##' 
##' The function treat() applies a treatment to an individual, while
##' the function cure() distributes the treatments of the set (cure)
##' to the population.
##' 
##' @section Simulation framework:
##' The simulation framework repeatedly calls the disease progression
##' and cure functions on a population for a given number of
##' days. This then returns a data frame with individuals as columns
##' and days in the rows.
##'
##' @section Visualization:
##' To create visual representations of simulations, classial
##' Kaplan-Meier curves are produced. They contain the proportion of
##' Zs in the population (red) and the median infectiousness of the
##' general population. If selected, these curves are produced either
##' for the entire population (Hs and Zs) or just the Zs.
##' @name Revil
##' @docType package
NULL
