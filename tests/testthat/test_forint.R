library(mR)

test_that("forint() returns string", {
  expect_match(forint(42), "42 Ft")
  expect_error(forint("42"), "Assertion on 'x' failed: Must be of type 'number', not 'character'.")
})