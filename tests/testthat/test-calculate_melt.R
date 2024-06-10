# calculate_melt() --------------------------------------------------------
test_that("calculate_melt()", {
  expect_equal(calculate_melt(-10, melt_factor = 0.5), 0)
  expect_true(calculate_melt(10, melt_factor = 0.5) > 0)
  expect_equal(calculate_melt(10, melt_factor = 0.5), 5)
  expect_equal(calculate_melt(10, melt_factor = 0.2), 2)
  expect_error(calculate_melt(10, melt_factor = -10))
  #expect_error(calculate_melt(10, melt_factor = 10))
})

