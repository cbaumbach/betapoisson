#' Probability function of Beta-Poisson mixture distribution
#'
#' @details We have the following formulas for expected value and
#'     variance (see references below):
#'
#'     \deqn{E(X) = E(lambda) = \frac{a}{a+b}}{E(X) = E(lambda) = a/(a+b)}
#'     \deqn{V(X) = E(lambda) + V(lambda) = \frac{a}{a+b} + \frac{ab}{(a+b)^2(a+b+1)}}{V(X) = E(lambda) + V(lambda) = a/(a+b) + (ab)/((a+b)^2(a+b+1))}
#'
#' @references See pages 43 ff. in Jan Grandell, Mixed Poisson
#'     Processes, CRC Press, 1997 and also Dimitris Karlis and Evdokia
#'     Xekalaki, Mixed Poisson Distributions, International
#'     Statistical Review (2005), *73*, 1, 35-58
#'
#' @export
dbetapoisson <- function(x, a, b, lambda = 1) {
    missing <- is.na(x)
    x[missing] <- 0
    negative <- x < 0
    x[negative] <- 0
    log_numerator <- x * log(lambda) + lbeta(x + a, b) +
        Re(fAsianOptions::kummerM(-lambda, a + x, a + b + x, lnchf = 1))
    log_denominator <- lfactorial(x) + lbeta(a, b)
    result <- exp(log_numerator - log_denominator)
    result[missing] <- NA
    result[negative] <- 0
    result
}
