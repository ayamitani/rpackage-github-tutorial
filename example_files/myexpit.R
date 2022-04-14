#' @title inverse logistic link function
#' @description a function that returns the inverse of the logistic link function
#' @param x numeric vector
#' @return z
#' @author Aya Mitani
#' @examples
#' myexpit(2)
#' @export

myexpit <- function(x){
  y <- exp(x) / (1 + exp(x))
  z <- log(y)
  return(z)
}
