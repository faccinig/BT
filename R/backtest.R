Backtest <- R6::R6Class(
  "Backtest",
  # public ----
  public = list(
    current_idx = 0L,
    order_idx = 0L,
    trade_idx = 0L,
    data = NULL,
    orders = list(),
    trades = list(),
    strategy = NULL,
    reporter = NULL,
    # trade_cost = function(price = 0, order = NULL) {0}
    cash = NA_real_,
    initial_cash = NA_real_,
    next_trade = function() {
      self$trade_idx <- trade_idx + 1L
      self$trade_idx
    },
    next_order = function() {
      self$order_idx <- self$order_idx + 1L
      self$order_idx
    },
    set_data = function(data) {
      # TODO: testar se o df possui requisitos mínimos
      # Possui:
      #   * date
      #   * OHLC
      check_vector(data, name = "date", is_of_type = \(x) inherits(x, "Date"), type = "Date")
      check_vector(data, name = "open", is_of_type = is.numeric, type = "numeric")
      check_vector(data, name = "high", is_of_type = is.numeric, type = "numeric")
      check_vector(data, name = "low", is_of_type = is.numeric, type = "numeric")
      check_vector(data, name = "close", is_of_type = is.numeric, type = "numeric")
      self$data <- data
      invisible(self)
    },
    set_strategy = function(strategy) {
      # testar se é da classe strategy
      self$strategy <- strategy
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
      # avalia se todos os requisitos para rodar são atendidos
      # Possui:
      #   * strategy
      #   * data
      # Se reporter não for definido usar "ReporterSimple"(avaliar somente estatísticas básicas)
      # iniciar reporter
      # iniciar strategy
      # avança para o primeira data sem NA
      private$find_begin()
      # roda o backtest
      private$run()
    },
    add_order = function(symbol, size, side, type, ...) {
      order <- new_order(idx = private$next_order(),
                         date_idx = self$current_idx,
                         symbol = symbol,
                         size = size,
                         side = side,
                         type = type,
                         ...)
      self$orders <- append(self$orders, list(order))
      invisible(self)
    },
    last_line = function() {self$data[self$current_idx, ]}
  ),
  # private----
  private = list(
    next_bar = function() {
      self$current_idx <- self$current_idx + 1L
      self
    },
    fill_orders = function() {
      orders <- self$strategy$orders
      if (length(orders) == 0L) return(self)

      new_trades <- Reduce(function(lst, order) {
        trade <- fill_order(order, bt = self)
        if (is.null(trade)) return(lst)
        self$portifolio$add_trade(trade)
        append(lst, list(trade))
      },
      x = orders,
      init = list())

      self$trades <- append(self$trades, new_trades)
      self
    },
    find_begin = function() {
      self$current_idx <- 1L

      while (TRUE) {
        self$reporter$on_prenext(self$current_idx)
        any_na <- any(map_lgl(self$data, function(x) is.na(x[self$current_idx])))
        if (!any_na) break
        private$next_bar()
      }
    },
    run = function() {
      nro_bars <- nrow(self$data)
      while (self$current_idx <= nro_bars) {
        private$fill_orders()
        self$strategy$next_bar(self$current_idx)
        self$reporter$on_next(self$current_idx)
        private$next_bar()
      }
    }
  )
)


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

