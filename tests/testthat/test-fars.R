library(readr)

context("File operations")

# Test \code{make_filename() }
test_that("File name correct", {
  expect_that(make_filename(1), equals("accident_1.csv.bz2"))
})


# Test \code{fars_read() }, giving a subset of real data
test_that("File read properly", {
  expect_that(fars_read("no_File.csv.bz2"), throws_error() )
  tst_frame <- fars_read(file.path( "accident_2013.csv.bz2"))
  expect_that(length(tst_frame), equals(50))
  expect_that(nrow(tst_frame), equals(750))
})
