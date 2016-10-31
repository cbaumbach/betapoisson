context("dbetapoisson")

test_that("values below 0 have probability 0", {
    expect_equal(dbetapoisson(-1, a = 2, b = 3), 0)
})
