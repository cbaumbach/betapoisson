source("helper_functions.R")

context("dbetapoisson")

test_that("values below 0 have probability 0", {
    expect_equal(dbetapoisson(-1, a = 2, b = 3), 0)
})

test_that("NAs result in NAs", {
    expect_equal(dbetapoisson(NA, a = 2, b = 3), NA_real_)
})

test_that("probabilities are >= 0 for values >= 0", {
    expect_gte(min(dbetapoisson(1:10, a = 2, b = 3)), 0)
})

test_that("a = 2 and b = 3 results in correct E(X)", {
    expect_equal(expected_value(dbetapoisson, a = 2, b = 3), find_mean(a = 2, b = 3))
})

test_that("a = 2 and b = 3 results in correct V(X)", {
    expect_equal(variance(dbetapoisson, a = 2, b = 3), find_variance(a = 2, b = 3))
})

test_that("a = 2 and b = 3 results in correct skewness", {
    expect_equal(skewness(dbetapoisson, a = 2, b = 3), find_skewness(a = 2, b = 3))
})

test_that("a = 2 and b = 3 results in correct kurtosis", {
    expect_equal(kurtosis(dbetapoisson, a = 2, b = 3), find_kurtosis(a = 2, b = 3))
})
