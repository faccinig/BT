
test_that("can create orders", {
  bt <- Backtest$new()

  bt$buy()
  expect_equal(length(bt$get_orders()), 1L)

  bt$buy_start(100)
  expect_equal(length(bt$get_orders()), 2L)

  bt$buy_stop(100)
  expect_equal(length(bt$get_orders()), 3L)

  bt$sell()
  expect_equal(length(bt$get_orders()), 4L)

  bt$sell_start(100)
  expect_equal(length(bt$get_orders()), 5L)

  bt$sell_stop(100)
  expect_equal(length(bt$get_orders()), 6L)
})

test_that("can cancel single order", {
  bt <- Backtest$new()

  bt$buy()
  bt$buy()
  bt$buy()
  bt$cancel_order(bt$get_orders()[[2L]])
  expect_equal(length(bt$get_orders()), 2L)
  orders_idx <- vapply(bt$get_orders(), \(o) o$order_idx, FUN.VALUE = 1L)
  expect_equal(orders_idx, c(1L,3L))
})
