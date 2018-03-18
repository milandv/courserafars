library(dplyr)
library(maps)

context("Test inputs")

test_that("make_filename returns correct output", {
    expect_is(make_filename(2013),"character")
})

