Backtest <- R6::R6Class(
  "Backtest",
  # public ----
  public = list(
    current_bar = 0L,
    orders = list(),
    trades = list(),
    data = NULL,
    reporter = NULL,
    begin = function(...) {
      stop("`begin` method must be created by user!")
    },
    on_bar = function(current_bar) {
      stop("`on_bar` method must be created by user!")
    },
    initialize = function(reporter = Console_reporter()) {
      self$set_reporter(reporter)
    },
    set_data = function(data) {
      # talvez esta função deva se tornar um active binding
      check_vector(data, name = "date", is_of_type = \(x) inherits(x, "Date"), type = "Date")
      check_vector(data, name = "open", is_of_type = is.numeric, type = "numeric")
      check_vector(data, name = "high", is_of_type = is.numeric, type = "numeric")
      check_vector(data, name = "low", is_of_type = is.numeric, type = "numeric")
      check_vector(data, name = "close", is_of_type = is.numeric, type = "numeric")
      self$data <- data
      invisible(self)
    },
    set_reporter = function(reporter) {
      # testar se é da classe reporter
      self$reporter <- reporter
      invisible(self)
    },
    set_trade_cost = function(cost_fun) {
      stopifnot(
        "`cost_fun` must be a function!" = is.function(cost_fun)
      )
      self$trade_cost <- cost_fun
      invisible(self)
    },
    # TODO: método para definir as variáveis:
    #     * begin_at = NULL,
    #     * end_at = NULL,
    #     * initial_cash = 100000,
    #     * from_start = TRUE,
    run = function(...) {
      private$renew()
      self$begin(...)
      # avalia se todos os requisitos para rodar são atendidos
      # Possui:
      #   * data
      # Se reporter não for definido usar "ReporterSimple"(avaliar somente estatísticas básicas)
      # iniciar reporter
      # avança para o primeira data sem NA
      private$find_initial_bar()
      # roda o backtest
      private$backtest()
      self
    },
    buy = function(size = 100L) {
      private$add_order(
        size = size,
        side = BUY,
        type = MARKET
      )
    },
    sell = function(size = 100L) {
      private$add_order(
        size = -size,
        side = SELL,
        type = MARKET
      )
    },
    buy_start = function(size = 100L, start_price) {
      private$add_order(
        size = size,
        side = BUY,
        type = START,
        start_price = start_price
      )
    },
    sell_start = function(size = 100L, start_price) {
      private$add_order(
        size = -size,
        side = SELL,
        type = START,
        start_price = start_price
      )
    },
    buy_stop = function(size = 100L, stop_price) {
      private$add_order(
        size = size,
        side = BUY,
        type = STOP,
        stop_price = stop_price
      )
    },
    sell_stop = function(size = 100L, stop_price) {
      private$add_order(
        size = -size,
        side = SELL,
        type = STOP,
        stop_price = stop_price
      )
    },
    last_line = function() {self$data[self$current_idx, ]},
    open = function() {self$data$open[self$current_idx]},
    high = function() {self$data$high[self$current_idx]},
    low = function() {self$data$low[self$current_idx]},
    close = function() {self$data$close[self$current_idx]}

  ),
  # private----
  private = list(
    order_idx = 0L,
    trade_idx = 0L,
    renew = function() {
      private$order_idx <- 0L
      private$trade_idx <- 0L
      self$current_bar <- 0L
      self$orders <- list()
      self$trades <- list()
      self
    },
    add_order = function(symbol, size, side, type, ...) {
      order <- new_order(order_idx = private$next_order(),
                         bar = self$current_bar,
                         symbol = symbol,
                         size = size,
                         side = side,
                         type = type,
                         ...)
      private$orders <- append(self$orders, list(order))
      invisible(self)
    },
    next_trade = function() {
      private$trade_idx <- private$trade_idx + 1L
      private$trade_idx
    },
    next_order = function() {
      private$order_idx <- private$order_idx + 1L
      private$order_idx
    },
    next_bar = function() {
      self$current_bar <- self$current_bar + 1L
      self$current_bar
    },
    find_initial_bar = function() {
      max_NAs <- map_int(self$data,
                         function(.x) {
                           w <- which(is.na(.x))
                           if (length(w) == 0L) return(0L)
                           max(w)
                         })
      self$current_bar <- max(max_NAs) + 1L
      self$current_bar
    },
    backtest = function() {
      last_bar <- nrow(self$data)
      while (private$next_bar() <= last_bar) {
        private$fill_orders()
        self$on_bar()
      }
      self
    },
    fill_orders = function() {
      orders <- private$orders
      if (length(orders) == 0L) return(self)

      new_trades <- Reduce(function(lst, order) {
        trade <- self$broker$fill_order(order)
        if (is.null(trade)) return(lst)

        append(lst, list(trade))
      },
      x = orders,
      init = list())

      if (length(new_trades) > 0L) {
        removed_orders <- map_int(new_trades, \(t) t$order_idx)
        self$orders <- remove(orders, \(o) o$order_idx %in% removed_orders)
        self$trades <- append(self$trades, new_trades)
      }

      self
    }
  )
)


# Fill order -------------------------------------------------------------------
# Não vai funcionar como desejado - o R6 só usa as funções definidas no pacote
# não podendo ser substituidas pelo usuário

fill_order <- function(ord, bt) {
  UseMethod("make_trade")
}

fill_order.market <- function(order, bt) {
  line <- bt$last_line()
  # Para market order o preço de exercício será o preço de abertura
  # TODO: avaliar método mais realista
  price <- line$open
  new_trade(
    idx = bt$next_trade(),
    data_idx = bt$current_idx,
    order_idx = order$idx,
    symbol = order$symbol,
    type = order$type,
    side = order$side,
    size = order$size,
    price = price,
    trade_cost = bt$trade_cost(order = order, price = price)
  )
}

