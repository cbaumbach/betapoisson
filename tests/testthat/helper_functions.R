expected_value <- function(density, a, b) {
    find_sum(function(x) x * density(x, a = a, b = b), from = 0, to = 10)
}

find_sum <- function(f, from, to) {
    sum(vapply(from:to, f, double(1)))
}
