#' Probability function of Beta-Poisson mixture distribution
#'
#' @param x vector of (non-negative integer) quantiles
#' @param a,b parameters of mixing Beta distribution
#' @param lambda Poisson mean before thinning (see Details)
#'
#' @details The Beta-Poisson distribution with parameters \eqn{a},
#'     \eqn{b}, and \eqn{lambda}, can be understood as a random
#'     variable constructed by independent \eqn{p}-thinning (see
#'     Definition 2.4 in reference 1) from a Poisson random variable
#'     with mean \eqn{lambda} where \eqn{p} itself is a random
#'     variable following a Beta distribution with parameters \eqn{a}
#'     and \eqn{b}.
#'
#'     The expected value and variance of \eqn{X} can be written as
#'     follows (see reference 2):
#'
#'     \deqn{E(X) = E(lambda) = \frac{a}{a+b}}{E(X) = E(lambda) = a/(a+b)}
#'     \deqn{V(X) = E(lambda) + V(lambda) = \frac{a}{a+b} + \frac{ab}{(a+b)^2(a+b+1)}}{V(X) = E(lambda) + V(lambda) = a/(a+b) + (ab)/((a+b)^2(a+b+1))}
#'
#'     where \eqn{lambda} has a Beta distribution with parameters
#'     \eqn{a} and \eqn{b}.
#'
#' @references
#' \enumerate{
#'   \item Jan Grandell, Mixed Poisson Processes, CRC Press, 1997,
#'       pages 43 ff.
#'   \item Dimitris Karlis and Evdokia Xekalaki, Mixed Poisson
#'       Distributions, International Statistical Review (2005), *73*,
#'       1, 35-58
#' }
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
