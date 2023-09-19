new_trade <- function(trade_bar,
                      bar,
                      order_bar,
                      type,
                      side,
                      size,
                      price,
                      trade_cost,
                      ...) {
  structure(
    list(
      trade_bar = trade_bar,
      bar = bar,
      order_bar = order_bar,
      type = type,
      side = side,
      size = size,
      price = price,
      trade_cost = trade_cost,
      ...
    ),
    class = c(type, "trade")
  )
}

print.trade <- function(x, ...) {
  cat_glue("
  Trade:
    Trade Idx: {x$trade_bar}
    Type: {x$type}
    Side: {x$side}
    Size: {x$size}
    Price:{x$price}
  "
  )
  invisible(x)
}

# trade <- new_trade(idx = 156L,
#                  100L,
#                  side = BUY,
#                  type = MARKET,
#                  price = 12.50)
