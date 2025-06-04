#' Hunting Function
#'
#' @param t time (days)
#' @param pop atatype list initial conditions; list with two values prey=number of prey and pred=number of predictor
#' @param pars datatype list  coefficient in Lotka-Voltera pars$rprey, pars$alpha, pars$eff, par$pmort
#'  \emph{rprey} is growth rate of prey population;
#'  \emph{eff} is the rate of ingestion of prey by predators
#'  \emph{alpha} is a interaction coefficient (higher values greater interaction
#'  \emph{pmort}  mortality rate of predictor population
#'  \emph{K}
#'  \emph{hunt_rate}
#'  \emph{min_prey_hunt}
#'
#' @return hunting returns a list containing the following components
#' \describe{
#' \item{dprey}{rate of change of prey populutation}
#' \item{dpred}{rate of change of preditor populutation}
#' }
#' 
#' @export
#'
#' @examples
#' 

hunting <- function(t, pop, pars) {
  with(as.list(c(pars, pop)), {
    
    # Determine the actual number of prey hunted
    hunt_effort <- if (prey >= min_prey_hunt) hunt_rate else 0
    hunted <- min(hunt_effort, prey)  # Don't hunt more than exist
    
    # Differential equations with hunting subtracted from prey
    dprey <- rprey * (1 - prey / K) * prey - alpha * prey * pred - hunted
    dpred <- eff * alpha * prey * pred - pmort * pred
    
    return(list(c(dprey, dpred)))
  })
}
