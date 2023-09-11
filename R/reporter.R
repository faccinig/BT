# Reporter ----
Reporter <- R6::R6Class(
  "Reporter",
  public = list(
    data = NULL,
    strategy = NULL,
    current_idx = NA,
    date = function() {
      self$data$date[self$current_idx]
    },
    last_bar = function() {
      self$data[self$current_idx,]
    },
    set_data = function(data) {
      self$data <- data
      self
    },
    set_strategy = function(strategy) {
      self$strategy <- strategy
      self
    },
    on_begining = function() {self},
    on_end = function() {self},
    on_prenext = function(current_idx) {
      self$current_idx <- current_idx
      self
    },
    on_next_bar = function(current_idx) {
      self$current_idx <- current_idx
      self
    }
    on_trade = function(trade) {self},
  )
)


# List_reporter ----
List_reporter <- R6::R6Class(
  "List_reporter",
  inherit = Reporter,
  public = list(
    reporters = list(),
    add_reporter = function(reporter) {
      self$reporters <- append(self$reporters, list(reporter))
      self
    },
    set_data = function(data) {
      lapply(self$reporters, function(r) r$set_data(data))
      self
    },
    on_begining = function() {
      lapply(self$reporters, function(r) r$on_begining())
      self
    },
    on_end = function() {
      lapply(self$reporters, function(r) r$on_end())
      self
    },
    on_next = function() {
      lapply(self$reporters, function(r) r$on_end())
      self
    },
    on_next_bar = function(current_idx) {
      lapply(self$reporters, function(r) r$on_next_bar(current_idx))
      self
    }
    on_trade = function(trade) {
      lapply(self$reporters, function(r) r$on_trade(trade))
      self
    },
  )
)

# Console_reporter ----
Console_reporter <- R6::R6Class(
  "Console_reporter",
  inherit = Reporter,
  public = list(
    on_trade = function(trade) {
      dt <- self$data$date[trade$idx]
      trade$size
      cat(glue("{dt} TRADE {trade$symbol} {trade$side} {trade$type} Price: {trade$price} Size: {trade$size}"),"\n")
    }
  )
)

