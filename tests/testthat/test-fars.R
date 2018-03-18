library(dplyr)
library(maps)

setwd(system.file("extdata", package = "courserafars"))
context("Test Basic Functionality")

test_that("Year summarizing works", {
    expect_is(fars_summarize_years(c(2013,2014)),"tbl_df")
    expect_error(fars_summarize_years('bob'))
})

