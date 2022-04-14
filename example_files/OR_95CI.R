#' @title odds ratio function
#' @description a function that returns OR (95\% CI) for each variable as character string
#' @param coef numeric vector of parameter estimates
#' @param se numeric vector of standard error estimates
#' @param siglevel significance level
#' @param roundto number of decimal places to display
#' @return character string of OR (95\% CI)
#' @author Aya Mitani
#' @examples
#' m1 <- glm(y ~ x1 + x2, family = binomial("logit"), data = toydata)
#' m1coef <- summary(m1)$coef
#' OR_95CI(m1coef[,1], m1coef[,2], 0.05, 2)
#' @export


OR_95CI <- function(coef, se, siglevel, roundto){
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall=roundto), " (", format(round(ORlcl, roundto), nsmall=roundto), ", ", format(round(ORucl, roundto), nsmall=roundto), ")")
  return(ORresult)
}
