# The MIT License (MIT)
#
# Copyright (c) [2013-2015] [RStudio]
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# LICENSE above applies to this source file only
# Reason for copy, paste and mod:
# http://stackoverflow.com/questions/20515358/rcmd-check-unexported-objects-imported-by-calls#comment30669909_20515358
# https://stat.ethz.ch/pipermail/r-devel/2013-August/thread.html#67180

#' @importFrom dplyr translate_sql_q build_sql ident query
build_query <- function(x, limit = NULL, is_percent = NULL) {
  assertthat::assert_that(is.null(limit) || assertthat::is.number(limit))
  translate <- function(expr, ...) {
    translate_sql_q(expr, tbl = x, env = NULL, ...)
  }

  if (x$summarise) {
    # Summarising, so SELECT needs to contain grouping variables
    select <- c(x$group_by, x$select)
    select <- select[!duplicated(select)]

    select_sql <- translate(select)
    vars <- auto_names(select)

    group_by_sql <- translate(x$group_by)
    order_by_sql <- translate(x$order_by)
  } else {
    # Not in summarise, so assume functions are window functions
    select_sql <- translate(x$select, window = uses_window_fun(x$select, x))
    vars <- auto_names(x$select)

    # Don't use group_by - grouping affects window functions only
    group_by_sql <- NULL

    # If the user requested ordering, ensuring group_by is included.
    # Otherwise don't, because that may make queries substantially slower
    if (!is.null(x$order_by) && !is.null(x$group_by)) {
      order_by_sql <- translate(c(x$group_by, x$order_by))
    } else {
      order_by_sql <- translate(x$order_by)
    }
  }

  # If ordering, then TOP should be specified, if it isn't already,
  # to ensure query works when query is part of a subquery. See #49
  if (!is.null(order_by_sql) && is.null(limit)) {
    limit <- 100
    is_percent <- TRUE
  }

  if (!uses_window_fun(x$where, x)) {
    from_sql <- x$from
    where_sql <- translate(x$where)
  } else {
    # window functions in WHERE need to be performed in subquery
    where <- translate_window_where(x$where, x, con = x$src$con)
    base_query <- update(x,
      group_by = NULL,
      where = NULL,
      select = c(x$select, where$comp))$query

    from_sql <- build_sql("(", base_query$sql, ") AS ", ident(unique_name()),
      con = x$src$con)
    where_sql <- translate(where$expr)
  }

  sql <- sql_select(x$src$con, from = from_sql, select = select_sql,
    where = where_sql, order_by = order_by_sql, group_by = group_by_sql,
    limit = limit, is_percent = is_percent)
  query(x$src$con, sql, vars)
}

uses_window_fun <- function(x, tbl) {
  if (is.null(x)) return(FALSE)
  if (is.list(x)) {
    calls <- unlist(lapply(x, all_calls))
  } else {
    calls <- all_calls(x)
  }
  win_f <- ls(envir = src_translate_env(tbl)$window)
  any(calls %in% win_f)
}

all_calls <- function(x) {
  if (!is.call(x)) return(NULL)
  fname <- as.character(x[[1]])
  unique(c(fname, unlist(lapply(x[-1], all_calls), use.names = FALSE)))
}

translate_window_where <- function(expr, tbl, con = NULL) {
  # Simplest base case: atomic vector or name ---------------------------------
  if (is.atomic(expr) || is.name(expr)) {
    return(list(expr = expr, comp = list()))
  }

  # Other base case is an aggregation function --------------------------------
  variant <- src_translate_env(tbl)
  agg_f <- ls(envir = variant$window)

  if (is.call(expr) && as.character(expr[[1]]) %in% agg_f) {
    name <- unique_name()
    sql <- translate_sql_q(list(expr), tbl, env = NULL, window = TRUE)

    return(list(expr = as.name(name), comp = setNames(list(sql), name)))
  }

  # Recursive cases: list and all other functions -----------------------------

  if (is.list(expr)) {
    args <- lapply(expr, translate_window_where, tbl = tbl, con = con)

    env <- sql_env(call, variant, con = con)
    sql <- lapply(lapply(args, "[[", "expr"), eval, env = env)
  } else {
    args <- lapply(expr[-1], translate_window_where, tbl = tbl, con = con)

    call <- as.call(c(expr[[1]], lapply(args, "[[", "expr")))
    env <- sql_env(call, variant, con = con)
    sql <- eval(call, envir = env)
  }

  comps <- unlist(lapply(args, "[[", "comp"), recursive = FALSE)

  list(expr = sql, comp = comps)
}

sql_env <- function(expr, variant, con, window = FALSE,
  strict = getOption("dplyr.strict_sql")) {
  stopifnot(is.sql_variant(variant))

  # Default for unknown functions
  if (!strict) {
    unknown <- setdiff(all_calls(expr), names(variant))
    default_env <- ceply(unknown, default_op, parent = emptyenv())
  } else {
    default_env <- new.env(parent = emptyenv())
  }


  # Known R -> SQL functions
  special_calls <- copy_env(variant$scalar, parent = default_env)
  if (!window) {
    special_calls2 <- copy_env(variant$aggregate, parent = special_calls)
  } else {
    special_calls2 <- copy_env(variant$window, parent = special_calls)
  }

  # Existing symbols in expression
  names <- all_names(expr)
  name_env <- ceply(names, function(x) escape(ident(x), con = con),
    parent = special_calls2)

  # Known sql expressions
  symbol_env <- copy_env(base_symbols, parent = name_env)
  symbol_env
}

is.sql_variant <- function(x) inherits(x, "sql_variant")

ceply <- function(x, f, ..., parent = parent.frame()) {
  if (length(x) == 0) return(new.env(parent = parent))
  l <- lapply(x, f, ...)
  names(l) <- x
  list2env(l, parent = parent)
}

copy_env <- function(from, to = NULL, parent = parent.env(from)) {
  list2env(as.list(from), envir = to, parent = parent)
}

sql_vector <- function (x, parens = NA, collapse = " ", con = NULL) {
  if (is.na(parens)) {
    parens <- length(x) > 1L
  }
  x <- names_to_as(x, con = con)
  x <- paste(x, collapse = collapse)
  if (parens) x <- paste0("(", x, ")")
  sql(x)
}

#' @importFrom dplyr sql_escape_ident
names_to_as <- function (x, con = NULL) {
  names <- names2(x)
  as <- ifelse(names == '', '',
    paste0(' AS ', sql_escape_ident(con, names)))
  paste0(x, as)
}

names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

only_has_names <- function(x, nms) {
  all(names(x) %in% nms)
}

"%||%" <- function (x, y) if (is.null(x)) y else x

# From random_table_name
random_ident_name <- function (n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}

#' @importFrom dplyr db_create_index
db_create_indexes <- function(con, table, indexes = NULL, ...) {
  if (is.null(indexes)) return()
  assertthat::assert_that(is.list(indexes))
  for(index in indexes) {
    db_create_index(con, table, index, ...)
  }
}

#' @importFrom dplyr tbl_vars
#' @importFrom utils capture.output
common_by <- function(by = NULL, x, y) {
  if (is.list(by)) return(by)

  if (!is.null(by)) {
    x <- names(by) %||% by
    y <- unname(by)

    # If x partially named, assume unnamed are the same in both tables
    x[x == ""] <- y[x == ""]

    return(list(x = x, y = y))
  }

  by <- intersect(tbl_vars(x), tbl_vars(y))
  if (length(by) == 0) {
    stop("No common variables. Please specify `by` param.", call. = FALSE)
  }
  message("Joining by: ", capture.output(dput(by)))

  list(
    x = by,
    y = by
  )
}

auto_names <- function(x) {
  nms <- names2(x)
  missing <- nms == ""
  if (all(!missing)) return(nms)

  deparse2 <- function(x) paste(deparse(x, 500L), collapse = "")
  defaults <- vapply(x[missing], deparse2, character(1), USE.NAMES = FALSE)

  nms[missing] <- defaults
  nms
}

unique_names <- function(x_names, y_names, by, x_suffix = ".x", y_suffix = ".y") {
  # See: https://github.com/hadley/dplyr/issues/709
  common <- intersect(x_names, y_names)
  if (length(common) == 0) return(NULL)

  x_match <- match(common, x_names)
  x_new <- x_names
  x_new[x_match] <- paste0(x_names[x_match], x_suffix)

  y_match <- match(common, y_names)
  y_new <- y_names
  y_new[y_match] <- paste0(y_names[y_match], y_suffix)

  list(x = setNames(x_new, x_names), y = setNames(y_new, y_names))
}

unique_name <- local({
  i <- 0

  function() {
    i <<- i + 1
    paste0("_W", i)
  }
})

#' @importFrom dplyr same_src
auto_copy <- function(x, y, copy = FALSE, ...) {
  if (same_src(x, y)) return(y)

  if (!copy) {
    stop("x and y don't share the same src. Set copy = TRUE to copy y into ",
      "x's source (this may be time consuming).", call. = FALSE)
  }

  UseMethod("auto_copy")
}

compact <- function(x) Filter(Negate(is.null), x)


base_symbols <- dplyr::sql_translator(
  pi = dplyr::sql("PI()"),
  `*` = dplyr::sql("*"),
  `NULL` = dplyr::sql("NULL")
)

all_names <- function(x) {
  if (is.name(x)) return(as.character(x))
  if (!is.call(x)) return(NULL)

  unique(unlist(lapply(x[-1], all_names), use.names = FALSE))
}

#' @importFrom dplyr sql_infix
default_op <- function(x) {
  assertthat::assert_that(assertthat::is.string(x))
  infix <- c("::", "$", "@", "^", "*", "/", "+", "-", ">", ">=", "<", "<=",
    "==", "!=", "!", "&", "&&", "|", "||", "~", "<-", "<<-")

  if (x %in% infix) {
    sql_infix(x)
  } else if (grepl("^%.*%$", x)) {
    x <- substr(x, 2, nchar(x) - 1)
    sql_infix(x)
  } else {
    sql_prefix(x)
  }
}


