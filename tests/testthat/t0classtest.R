context("xnpv")

test_that("t0_classtest", {
  xnpv(0.05, cashflows)
  expect_that(t0, is_a("date"))
})
