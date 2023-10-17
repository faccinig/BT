#' @export
Broker <- R6::R6Class(
  "Broker",
  # public ----
  public = list(
    cash = 0,
    position = 0L,
    open_trade = NULL,
    trades = list(),
    initialize = function(cash = 1e5, fee = 0, commission = 0) {
      private$initial_capital = cash
      private$fee <- fee
      private$commision <- commission
    },
    renew = function(bt) {
      self$position <- 0L
      self$cash = private$initial_capital
      self$open_trade <- NULL
      numeric_vec <- rep(NA_real_, nrow(private$bt$data))
      private$data <- data.frame(cash = numeric_vec,
                                 asset = numeric_vec)

      self
    },
    on_bar = function(current_bar) {
      private$data$cash[current_bar] <- self$cash
      private$data$asset[current_bar] <- self$asset
      self
    },
    fill_order = function(orders) {

      filled <- switch (order$type,
                        'market' = TRUE,
                        'limit' = self$limit_filled(order),
                        'stop' = self$stop_filled(order)
                        )
      if (!filled) return(NULL)

      price <- switch (order$type,
                       'market' = self$market_price(order),
                       'limit' = self$limit_price(order),
                       'stop' = self$stop_price(order)
                       )

      cost <- self$trade_cost(price, size = abs(order$size))

      trade <- new_trade(
        trade_idx = private$next_trade(),
        bar = private$bt$current_bar,
        order_idx = order$order_idx,
        symbol = order$symbol,
        type = order$type,
        side = order$side,
        size = order$size,
        price = price,
        trade_cost = cost,
        order = order
        )
      self$trades <- append(self$trades, list(trade))

    },
    start_filled = function(order) {
      if (order$side == BUY) {
        high <- self$bt$high()
        return(order$start_price <= high)
      } else {
        low <- self$bt$low()
        return(order$start_price >= low)
      }
    },
    stop_filled = function(order) {
      if (order$side == BUY) {
        low <- self$bt$low()
        return(order$stop_price >= low)
      } else {
        high <- self$bt$high()
        return(order$stop_price <= high)
      }
    },
    market_price = function(order) {
        # Para market order o preço de exercício será o preço de abertura
        # TODO: avaliar método mais realista
      self$bt$open()
    },
    stop_price = function(order) {

      open <- self$bt$open()
      if (order$side == BUY) {
        if (open <= order$stop_price) {
          return(open)
        } else {
          return(order$stop_price)
        }

      } else {
        if (open >= order$stop_price) {
          return(open)
        } else {
          return(order$stop_price)
        }
      }
    },
    start_price = function(order) {
      open <- self$bt$open()
      if (order$side == BUY) {
        if (open >= order$start_price) {
          return(open)
        } else {
          return(order$start_price)
        }

      } else {
        if (open <= order$start_price) {
          return(open)
        } else {
          return(order$start_price)
        }
      }
    },
    trade_cost = function(price, size) {
      fee <- self$fee
      commission <- price * size * self$commission
      fee + commission
    }
  ),
  # private----
  private = list(
    bt = NULL,
    initial_capital = 0,
    fee = 0,
    commision = 0

  )
)
