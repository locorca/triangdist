test_that("dtriang validations and math work", {
  expect_error(dtriang(0.5, min = 5, max = 1, mode = 2))
  expect_error(dtriang(0.5, min = 0, max = 1, mode = 2))

  expect_equal(dtriang(1, 0, 2, 1), 1)

  expect_equal(dtriang(3, 0, 2, 1), 0)
})

test_that("ptriang cumulative logic works", {
  expect_equal(ptriang(0, 0, 2, 1), 0)
  expect_equal(ptriang(1, 0, 2, 1), 0.5)
  expect_equal(ptriang(2, 0, 2, 1), 1)
})

test_that("qtriang and rtriang consistency", {
  expect_equal(qtriang(0.5, 0, 2, 1), 1)
  expect_error(qtriang(1.1, 0, 1, 0.5))

  set.seed(123)
  sample <- rtriang(100, 0, 1, 0.5)
  expect_length(sample, 100)
  expect_true(all(sample >= 0 & sample <= 1))
})
