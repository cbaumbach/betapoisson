#' Probability function of Beta-Poisson mixture distribution
#'
#' @references See pages 43 ff. in Jan Grandell, Mixed Poisson
#'     Processes, CRC Press, 1997 and also Dimitris Karlis and Evdokia
#'     Xekalaki, Mixed Poisson Distributions, International
#'     Statistical Review (2005), *73*, 1, 35-58
#'
#' @export
dbetapoisson <- function(x, a, b, lambda = 1) {
    negative <- x < 0
    x[negative] <- 0
    log_numerator <- x * log(lambda) + lbeta(x + a, b) +
        Re(fAsianOptions::kummerM(-lambda, a + x, a + b + x, lnchf = 1))
    log_denominator <- lfactorial(x) + lbeta(a, b)
    result <- exp(log_numerator - log_denominator)
    result[negative] <- 0
    result
}
