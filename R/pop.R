##' Creates an initial population of the specified size with a certain
##' number of Zs in it.
##'
##' newPop() creates a population suitable to be used in conjunction
##' with the disease spreading functions. A created population will be
##' of class pop. It can contain either a population for the continous
##' or the discrete viral infection models.
##'
##' In reality, this is only a data frame with a single column,
##' status, and associated class methods.
##' @title Create a new population
##' @param type either "c" or "d", creating a continous or discrete
##'        population
##' @param size the intended size of the population
##' @param z.prop the proportion of Zs in the population
##' @return a population 
##' @author Christoph Waldhauser
##' @export 
##' @examples
##' t0 <- newPop("c")
##' t1 <- newPop("d", size=50, z.prop=0.5) 
newPop <- function(type=c("c", "d"), size=100, z.prop=1/size) {
  size <- round(size)
  if (size < 1) stop("Size needs to be a positive integer")
  if (size > 5000) warning("You specified a very large population")
  if (z.prop<0 | z.prop >1) stop("Parameter z.prop needs to be in 0:1")
  nZ <- z.prop * size
  nH <- size - nZ
  status <- c(rep(0, times=nH), rep(1, times=nZ))
  if (type=="c") {
    pop <- data.frame(status=status)
    class.string <- c("c", "pop", "data.frame")
  } else if (type=="d") {
    pop <- data.frame(status=as.logical(status))
    class.string <- c("d", "pop", "data.frame")
  } else {
    stop("Type needs to be 'c' for continous or 'd' for dichotmous virus models.")
  }
  class(pop) <- class.string
  return(pop)
}


##' Produces a summary of a population
##'
##' Fields returned are type, size, amount of Hs and Zs and the mean
##' infection rate.
##' @title Summarize a population
##' @method summary pop
##' @param object population
##' @param ... currently ignored
##' @return a list with summary information
##' @author Christoph Waldhauser
##' @export
summary.pop <- function(object, ...) {
  if (!any(class(object) == "pop"))
    stop("Parameter object needs to be of class 'pop'.")
  status <- object$status
  size <- length(status)
  type <- ifelse(is.logical(status), "D", "C")
  nH <- sum(as.numeric(status)==0)
  nZ <- sum(as.numeric(status)==1)
  mir <- mean(status)
  res <- list(size=size,
              type=type,
              nH=nH,
              nZ=nZ,
              mir=mir)
  class(res) <- "summary.pop"
  return(res)
}

##' Print the summary of a population
##'
##' just pretty formatting
##' @title Print population summary
##' @param object the summary to print
##' @return returns the object in the call invisibly
##' @author Christoph Waldhauser
print.summary.pop <- function(object) {
  cat(paste(object$type, "-Virus infected population\n", sep=""))
  cat(paste("Size: ", object$size,
            " (", object$nH, " Hs, ",
            object$nZ, " Zs)\n", sep=""))
  cat(paste("Mean infection rate:", round(object$mir, 3), "\n"))
  invisible(object)
}
  
    


