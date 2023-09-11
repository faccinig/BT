# Strategy ----
Strategy <- R6::R6Class(
  "Strategy",
  ## public ----
  public = list(
    data = NULL,
    bt = NULL,
    current_idx = NA_integer_,
    order_idx = 0L,
    initialize = function() {},
    next_bar = function(idx) {
      # esta função deve ser implementada pelo usuário
      stop("Not implemented!")
    },
    set_bt = function(bt) {
      self$bt = bt
      self
    }
    clean_orders = function() {
      self$orders <- list()
      self
    },
    buy = function(symbol, size = 100L) {
      self$bt$add_order(
        symbol = symbol,
        size = size,
        side = BUY,
        type = MARKET
      )
    },
    sell = function(symbol, size = 100L) {
      self$bt$add_order(
        symbol = symbol,
        size = -size,
        side = SELL,
        type = MARKET
      )
    },
    buy_start = function(symbol, size = 100L, start_price) {
      self$bt$add_order(
        symbol = symbol,
        size = size,
        side = BUY,
        type = START,
        start_price = start_price
      )
    },
    sell_start = function(symbol, size = 100L, start_price) {
      self$bt$add_order(
        symbol = symbol,
        size = -size,
        side = SELL,
        type = START,
        start_price = start_price
      )
    },
    buy_stop = function(symbol, size = 100L, stop_price) {
      self$bt$add_order(
        symbol = symbol,
        size = size,
        side = BUY,
        type = STOP,
        stop_price = stop_price
      )
    },
    sell_stop = function(symbol, size = 100L, stop_price) {
      self$bt$add_order(
        symbol = symbol,
        size = -size,
        side = SELL,
        type = STOP,
        stop_price = stop_price
      )
    }
  ),
  ## private ----
  private = list(

  )
)

