expected_value <- function(density, a, b, lambda = 1) {
    find_sum(function(x) x * density(x, a, b, lambda), 0L, find_upper_limit(a, b, lambda))
}

variance <- function(density, a, b, lambda = 1) {
    mean <- find_mean(a, b, lambda)
    find_sum(function(x) (x - mean)^2 * density(x, a, b, lambda), 0L, find_upper_limit(a, b, lambda))
}

skewness <- function(density, a, b) {
    mean <- find_mean(a, b)
    variance <- find_variance(a, b)
    find_sum(function(x) (x - mean)^3 / variance^(3/2) * density(x, a, b), 0, find_upper_limit(a, b))
}

kurtosis <- function(density, a, b) {
    mean <- find_mean(a, b)
    variance <- find_variance(a, b)
    find_sum(function(x) (x - mean)^4 / variance^2 * density(x, a, b), 0, find_upper_limit(a, b))
}

find_mean <- function(a, b, lambda = 1) {
    moment_of_x(1, a, b, lambda)
}

find_variance <- function(a, b, lambda = 1) {
    moment_of_x(2, a, b, lambda) - moment_of_x(1, a, b, lambda)^2
}

find_skewness <- function(a, b) {
    mean <- find_mean(a, b)
    variance <- find_variance(a, b)
    (moment_of_x(3, a, b)
        - 3 * mean * moment_of_x(2, a, b)
        + 3 * mean^2 * moment_of_x(1, a, b)
        - mean^3) / variance^(3/2)
}

find_kurtosis <- function(a, b) {
    mean <- find_mean(a, b)
    variance <- find_variance(a, b)
    (moment_of_x(4, a, b)
        - 4 * mean * moment_of_x(3, a, b)
        + 6 * mean^2 * moment_of_x(2, a, b)
        - 4 * mean^3 * moment_of_x(1, a, b)
        + mean^4) / variance^2
}

find_sum <- function(f, from, to) {
    sum(vapply(from:to, f, double(1)))
}

find_upper_limit <- function(a, b, lambda = 1) {
    as.integer(qpois(-35, lambda = lambda * a/(a+b), lower.tail = FALSE, log.p = TRUE)) + 1L
}

# The moments of X can be computed using a recursive formula:
#
#     E(X^r) = sum_{j=1}^r S(r, j) E(lambda^j)
#
# where S(r, j) are the Stirling numbers of the second kind (see
# reference 2).

moment_of_x <- function(k, a, b, lambda) {
    stirling_numbers <- rbind(
        c(1, NA, NA, NA),
        c(1,  1, NA, NA),
        c(1,  3,  1, NA),
        c(1,  7,  6,  1))
    sum(sapply(1:k, moment_of_lambda, a, b, lambda) * stirling_numbers[k, 1:k])
}

# The moments of lambda, which has a Beta distribution with parameters
# a and b, can also be obtained via a recursive formula:
#
#     E(lambda^j) = (a+j-1)/(a+b+j-1) E(lambda^j-1)

moment_of_lambda <- function(k, a, b, lambda) {
    if (k == 1)
        return(lambda * a / (a + b))
    lambda * (a + k - 1) / (a + b + k - 1) * moment_of_lambda(k - 1, a, b, lambda)
}
