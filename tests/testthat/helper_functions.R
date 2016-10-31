expected_value <- function(density, a, b) {
    find_sum(function(x) x * density(x, a, b), 0L, find_upper_limit(a, b))
}

find_sum <- function(f, from, to) {
    sum(vapply(from:to, f, double(1)))
}

find_upper_limit <- function(a, b) {
    as.integer(qpois(-30, lambda = a/(a+b), lower.tail = FALSE, log.p = TRUE))
}
