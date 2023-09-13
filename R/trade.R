new_trade <- function(trade_idx,
                      bar,
                      order_idx,
                      type,
                      side,
                      size,
                      price,
                      trade_cost,
                      ...) {
  structure(
    list(
      trade_idx = trade_idx,
      bar = bar,
      order_idx = order_idx,
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
    Trade Idx: {x$trade_idx}
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
