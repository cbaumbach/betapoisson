source("helper_functions.R")

context("dbetapoisson")

test_that("values below 0 have probability 0", {
    expect_equal(dbetapoisson(-1, a = 2, b = 3), 0)
})

test_that("probabilities are >= 0 for values >= 0", {
    expect_gte(min(dbetapoisson(1:10, a = 2, b = 3)), 0)
})

test_that("a = 2 and b = 3 results in E(X) = 2/5", {
    expect_equal(expected_value(dbetapoisson, a = 2, b = 3), 2/5)
})
