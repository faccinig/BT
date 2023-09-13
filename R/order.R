new_order <- function(order_idx, date_idx, type, side, size, ...) {
  # stopifnot(
  #   "`symbol` must be a character!" = is.character(symbol),
  #   "`symbol` must be a single value!" = length(symbol) == 1L,
  #   "`size` must be a numeric!" = is.numeric(size),
  #   "`size` must be a single value!" = length(size) == 1L,
  #   "`side` must be a single value!" = length(side) == 1L,
  #   "`side` must be a 'buy' or 'sell'!" = side %in% c("buy", "sell"),
  #   "`idx` must be a numeric!" = is.numeric(idx),
  #   "`idx` must be a single value!" = length(idx) == 1L,
  #   "`type` must be a single value!" = length(type) == 1L,
  #   "`type` must be one of 'market'!" = type %in% c("market")
  # )

  structure(
    list(
      order_idx = order_idx,
      date_idx = date_idx,
      type = type,
      # symbol = symbol,
      size = size,
      side = side,
      ...
    ),
    class = c(type, "order")
  )
}


print.order <- function(x, ...) {
  cat_glue("
  Order:
    Order Idx: {x$order_idx}
    Type: {x$type}
    Side: {x$side}
    Size: {x$size}
  ")
  invisible(x)
}

# order <- new_order(symbol = "EMBR3",
#                    side = "buy",
#                    size = 100L,
#                    type = "market",
#                    idx = 14L)


