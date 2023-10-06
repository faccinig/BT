#' @export
Backtest <- R6::R6Class(
  "Backtest",
  # public ----
  public = list(
    current_bar = 0L,
    data = NULL,
    broker = NULL,
    begin = function(...) {
      stop("`begin` method must be created by user!")
    },
    on_bar = function(current_bar) {
      stop("`on_bar` method must be created by user!")
    },
    initialize = function(broker = NULL) {
      if (is.null(broker)) broker <- Broker$new()
      self$set_broker(broker)
      self
    },
    set_data = function(data) {
      # talvez esta função deva se tornar um active binding
      check_vector(data, name = "date", is_of_type = \(x) inherits(x, "Date"), type = "Date")
      check_vector(data, name = "open", is_of_type = is.numeric, type = "numeric")
      check_vector(data, name = "high", is_of_type = is.numeric, type = "numeric")
      check_vector(data, name = "low", is_of_type = is.numeric, type = "numeric")
      check_vector(data, name = "close", is_of_type = is.numeric, type = "numeric")
      self$data <- data
      self
    },
    set_broker = function(broker) {
      stopifnot(
        "`broker` must be of `Brocker` class!" = inherits(broker, "Broker")
      )
      self$broker <- broker
      self
    },
    # TODO: método para definir as variáveis:
    #     * begin_at = NULL,
    #     * end_at = NULL
    run = function(...) {
      private$renew()
      self$begin(...)
      # avalia se todos os requisitos para rodar são atendidos
      # Possui:
      #   * data
      # identifica primeira barra sem NA
      private$find_initial_bar()
      # roda o backtest
      private$backtest()
      self
    },
    buy = function(size = 100L, ...) {
      private$add_order(
        size = size,
        side = BUY,
        type = MARKET,
        ...
      )
    },
    sell = function(size = 100L, ...) {
      private$add_order(
        size = -size,
        side = SELL,
        type = MARKET,
        ...
      )
    },
    buy_start = function(start_price, size = 100L, ...) {
      private$add_order(
        size = size,
        side = BUY,
        type = START,
        start_price = start_price,
        ...
      )
    },
    sell_limit = function(limit_price, size = 100L, ...) {
      private$add_order(
        size = -size,
        side = SELL,
        type = START,
        start_price = start_price,
        ...
      )
    },
    buy_stop = function(stop_price, size = 100L, ...) {
      private$add_order(
        size = size,
        side = BUY,
        type = STOP,
        stop_price = stop_price,
        ...
      )
    },
    sell_stop = function(stop_price, size = 100L, ...) {
      private$add_order(
        size = -size,
        side = SELL,
        type = STOP,
        stop_price = stop_price,
        ...
      )
    },
    cancel_order = function(order) {
      private$orders <- remove_one(private$orders,
                                   \(o) o$order_idx == order$order_idx)
      self
    },
    get_orders = function() {
      private$orders
    },
    get_open_trade = function() {
      self$broker$open_trade
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
    orders = list(),
    renew = function() {
      private$order_idx <- 0L
      self$current_bar <- 0L
      private$orders <- list()
      self$broker$renew()
      self
    },
    add_order = function(size, side, type, ...) {
      order <- new_order(order_idx = private$next_order(),
                         bar = self$current_bar,
                         size = size,
                         side = side,
                         type = type,
                         ...)
      private$orders <- append(private$orders, list(order))
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
        self$on_bar(self$current_bar)
        self$broker$on_bar(self$current_bar)
      }
      self
    },
    fill_orders = function() {
      orders <- private$orders
      if (length(orders) == 0L) return(self)

      private$orders <- Reduce(function(lst, order) {
        filled <- self$broker$fill_order(order)
        if (filled) return(lst)

        append(lst, list(order))
      },
      x = orders,
      init = list())

      self
    }
  )
)

