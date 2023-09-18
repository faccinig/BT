test_that("lag works", {
  expect_equal(lag(c(1,2)), c(NA, 1))
  expect_equal(lag(c(NA,2)), c(NA_real_, NA_real_))
  })

test_that("crossover works", {
  expect_equal(crossover(vec1 = 1:2, vec2 =  2:1), c(FALSE, TRUE))
  expect_equal(crossover(vec1 = c(2,2), vec2 =  c(1,1), c(FALSE, FALSE)))
  expect_equal(crossover(vec1 = c(1,1), vec2 =  c(2,2), c(FALSE, FALSE)))
  expect_equal(crossover(vec1 = 2:1, vec2 =  1:2), c(FALSE, FALSE))
  expect_equal(crossover(vec1 = c(NA,1), vec2 =  c(2,2), c(FALSE, FALSE)))
  expect_equal(crossover(vec1 = c(NA,2), vec2 =  c(1,1), c(FALSE, FALSE)))
  expect_equal(crossover(vec1 = c(NA,2), vec2 =  c(NA,1), c(FALSE, FALSE)))
  })
