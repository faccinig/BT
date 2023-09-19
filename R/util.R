BUY <- "buy"
SELL <- "sell"
MARKET <- "market"
START <- "start"
STOP <- "stop"



map_lgl <- function(x, fun, ...) {
  vapply(x, fun, FUN.VALUE = logical(1), ...)
}

map_int <- function(x, fun, ...) {
  vapply(x, fun, FUN.VALUE = integer(1), ...)
}

discard <- function(x, .p, ...) {
  discarted <- map_lgl(x, .p, ...)
  x[!discarted]
}

check_vector <- function(df, name, is_of_type, type) {
  if (!name %in% names(df)) {
    stop(glue("There's no column of name '{name}' in data!"))
  }
  if (!is_of_type(df[[name]])) {
    stop(glue("Column {name} is not of type '{type}'!"))
  }
}

cat_glue <- function(msg, .envir = parent.frame()) {
  cat(glue(msg, .envir = .envir), "\n")
}

# remove_one <- function(lst, .p) {
#   for (i in seq_along(lst)) {
#     if (.p(lst[[i]])) {
#       lst <- lst[-i]
#       return(lst)
#     }
#   }
#   stop("No item was found!")
# }

remove <- function(lst, .p, ...) {
  Reduce(
    function(a, b) {
      if (.p(b, ...)) return(a)
      append(a, list(b))
    },
    x = lst,
    init = list()
  )
}






