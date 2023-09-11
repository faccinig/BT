new_trade <- function(idx, data_idx, symbol, size, side, type, price, trade_cost, order_idx, ...) {
  structure(
    list(
      idx = idx,
      data_idx = data_idx,
      order_idx = order_idx,
      symbol = symbol,
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
  cat(
    glue("
  Trade:
    Idx: {x$idx}
    Symbol: {x$symbol}
    Type: {x$type}
    Side: {x$side}
    Size: {x$size}
    Price:{x$price}
         "),
  "\n"
  )
  invisible(x)
}

# trade <- new_trade(idx = 156L,
#                  "teste", 100L,
#                  side = BUY,
#                  type = MARKET,
#                  price = 12.50)
