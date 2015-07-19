# # Sourced and modified from dplyr v0.4.1
# # LICENSE: MIT
# # YEAR: 2013-2014
# # COPYRIGHT HOLDER: RStudio
# # https://github.com/hadley/dplyr/releases/tag/v0.4.1
# #
# # Reason for copy, paste and mod:
# # http://stackoverflow.com/questions/20515358/rcmd-check-unexported-objects-imported-by-calls#comment30669909_20515358
# # https://stat.ethz.ch/pipermail/r-devel/2013-August/thread.html#67180

sql_vector <- function (x, parens = NA, collapse = " ", con = NULL) {
  if (is.na(parens)) {
    parens <- length(x) > 1L
  }
  x <- names_to_as(x, con = con)
  x <- paste(x, collapse = collapse)
  if (parens) x <- paste0("(", x, ")")
  dplyr::sql(x)
}

names_to_as <- function (x, con = NULL) {
  names <- names2(x)
  as <- ifelse(names == '', '',
    paste0(' AS ', dplyr::sql_escape_ident(con, names)))
  paste0(x, as)
}

names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

"%||%" <- function (x, y) if (is.null(x)) y else x

# From random_table_name
random_ident_name <- function (n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}

db_create_indexes <- function(con, table, indexes = NULL, ...) {
  if (is.null(indexes)) return()
  assertthat::assert_that(is.list(indexes))
  for(index in indexes) {
    db_create_index(con, table, index, ...)
  }
}

# all_calls <- function(x) {
#   if (!is.call(x)) return(NULL)
#
#   fname <- as.character(x[[1]])
#   unique(c(fname, unlist(lapply(x[-1], all_calls), use.names = FALSE)))
# }
#
# all_names <- function(x) {
#   if (is.name(x)) return(as.character(x))
#   if (!is.call(x)) return(NULL)
#
#   unique(unlist(lapply(x[-1], all_names), use.names = FALSE))
# }
#
# auto_copy <- function(x, y, copy = FALSE, ...) {
#   if (dplyr::same_src(x, y)) return(y)
#
#   if (!copy) {
#     stop("x and y don't share the same src. Set copy = TRUE to copy y into ",
#       "x's source (this may be time consuming).", call. = FALSE)
#   }
#
#   UseMethod("auto_copy")
# }
#
# auto_names <- function(x) {
#   nms <- names2(x)
#   missing <- nms == ""
#   if (all(!missing)) return(nms)
#
#   deparse2 <- function(x) paste(deparse(x, 500L), collapse = "")
#   defaults <- vapply(x[missing], deparse2, character(1), USE.NAMES = FALSE)
#
#   nms[missing] <- defaults
#   nms
# }
#
# base_symbols <- dplyr::sql_translator(
#   pi = dplyr::sql("PI()"),
#   `*` = dplyr::sql("*"),
#   `NULL` = dplyr::sql("NULL")
# )
#
# build_query <- function(x, limit = NULL) {
#   assertthat::assert_that(is.null(limit) ||
#       (is.numeric(limit) && length(limit) == 1))
#   translate <- function(expr, ...) {
#     dplyr::translate_sql_q(expr, tbl = x, env = NULL, ...)
#   }
#
#   if (x$summarise) {
#     # Summarising, so SELECT needs to contain grouping variables
#     select <- c(x$group_by, x$select)
#     select <- select[!duplicated(select)]
#
#     select_sql <- translate(select)
#     vars <- auto_names(select)
#
#     group_by_sql <- translate(x$group_by)
#     order_by_sql <- translate(x$order_by)
#   } else {
#     # Not in summarise, so assume functions are window functions
#     select_sql <- translate(x$select, window = uses_window_fun(x$select, x))
#     vars <- auto_names(x$select)
#
#     # Don't use group_by - grouping affects window functions only
#     group_by_sql <- NULL
#
#     # If the user requested ordering, ensuring group_by is included
#     # Otherwise don't, because that may make queries substantially slower
#     if (!is.null(x$order_by) && !is.null(x$group_by)) {
#       order_by_sql <- translate(c(x$group_by, x$order_by))
#     } else {
#       order_by_sql <- translate(x$order_by)
#     }
#   }
#
#   if (!uses_window_fun(x$where, x)) {
#     from_sql <- x$from
#     where_sql <- translate(x$where)
#   } else {
#     # window functions in WHERE need to be performed in subquery
#     where <- translate_window_where(x$where, x, con = x$src$con)
#     base_query <- update(x,
#       group_by = NULL,
#       where = NULL,
#       select = c(x$select, where$comp))$query
#
#     from_sql <- dplyr::build_sql("(", base_query$sql, ") AS ",
#       dplyr::ident(unique_name()), con = x$src$con)
#     where_sql <- translate(where$expr)
#   }
#
#
#   sql <- dplyr::sql_select(x$src$con, from = from_sql, select = select_sql,
#     where = where_sql, order_by = order_by_sql, group_by = group_by_sql,
#     limit = limit)
#   dplyr::query(x$src$con, sql, vars)
# }
#
# ceply <- function(x, f, ..., parent = parent.frame()) {
#   if (length(x) == 0) return(new.env(parent = parent))
#   l <- lapply(x, f, ...)
#   names(l) <- x
#   list2env(l, parent = parent)
# }
#
# common_by <- function(by = NULL, x, y) {
#   if (is.list(by)) return(by)
#
#   if (!is.null(by)) {
#     x <- names(by) %||% by
#     y <- unname(by)
#
#     # If x partially named, assume unnamed are the same in both tables
#     x[x == ""] <- y[x == ""]
#
#     return(list(x = x, y = y))
#   }
#
#   by <- intersect(dplyr::tbl_vars(x), dplyr::tbl_vars(y))
#   if (length(by) == 0) {
#     stop("No common variables. Please specify `by` param.", call. = FALSE)
#   }
#   message("Joining by: ", capture.output(dput(by)))
#
#   list(
#     x = by,
#     y = by
#   )
# }
#
# compact <- function(x) Filter(Negate(is.null), x)
#
# copy_env <- function(from, to = NULL, parent = parent.env(from)) {
#   list2env(as.list(from), envir = to, parent = parent)
# }
#
# default_op <- function(x) {
#   assertthat::assert_that(assertthat::is.string(x))
#   infix <- c("::", "$", "@", "^", "*", "/", "+", "-", ">", ">=", "<", "<=",
#     "==", "!=", "!", "&", "&&", "|", "||", "~", "<-", "<<-")
#
#   if (x %in% infix) {
#     dplyr::sql_infix(x)
#   } else if (grepl("^%.*%$", x)) {
#     x <- substr(x, 2, nchar(x) - 1)
#     dplyr::sql_infix(x)
#   } else {
#     dplyr::sql_prefix(x)
#   }
# }
#
# is.sql_variant <- function(x) inherits(x, "sql_variant")
#
# names2 <- function(x) {
#   names(x) %||% rep("", length(x))
# }
#
# names_to_as <- function(x, con = NULL) {
#   names <- names2(x)
#   as <- ifelse(names == '', '',
#     paste0(' AS ', dplyr::sql_escape_ident(con, names)))
#
#   paste0(x, as)
# }
#
# only_has_names <- function(x, nms) {
#   all(names(x) %in% nms)
# }
#
# random_table_name <- function(n = 10) {
#   paste0(sample(letters, n, replace = TRUE), collapse = "")
# }
#
# sql_env <- function(expr, variant, con, window = FALSE,
#   strict = getOption("dplyr.strict_sql")) {
#   stopifnot(is.sql_variant(variant))
#
#   # Default for unknown functions
#   if (!strict) {
#     unknown <- setdiff(all_calls(expr), names(variant))
#     default_env <- ceply(unknown, default_op, parent = emptyenv())
#   } else {
#     default_env <- new.env(parent = emptyenv())
#   }
#
#
#   # Known R -> SQL functions
#   special_calls <- copy_env(variant$scalar, parent = default_env)
#   if (!window) {
#     special_calls2 <- copy_env(variant$aggregate, parent = special_calls)
#   } else {
#     special_calls2 <- copy_env(variant$window, parent = special_calls)
#   }
#
#   # Existing symbols in expression
#   names <- all_names(expr)
#   name_env <- ceply(names,
#     function(x) dplyr::escape(dplyr::ident(x), con = con),
#     parent = special_calls2)
#
#   # Known sql expressions
#   symbol_env <- copy_env(base_symbols, parent = name_env)
#   symbol_env
# }
#
# sql_vector <- function(x, parens = NA, collapse = " ", con = NULL) {
#   if (is.na(parens)) {
#     parens <- length(x) > 1L
#   }
#
#   x <- names_to_as(x, con = con)
#   x <- paste(x, collapse = collapse)
#   if (parens) x <- paste0("(", x, ")")
#   dplyr::sql(x)
# }
#
# translate_window_where <- function(expr, tbl, con = NULL) {
#   # Simplest base case: atomic vector or name ---------------------------------
#   if (is.atomic(expr) || is.name(expr)) {
#     return(list(
#       expr = expr,
#       comp = list()
#     ))
#   }
#
#   # Other base case is an aggregation function --------------------------------
#   variant <- src_translate_env(tbl)
#   agg_f <- ls(envir = variant$window)
#
#   if (is.call(expr) && as.character(expr[[1]]) %in% agg_f) {
#     name <- unique_name()
#     sql <- dplyr::translate_sql_q(list(expr), tbl, env = NULL, window = TRUE)
#
#     return(list(
#       expr = as.name(name),
#       comp = setNames(list(sql), name)
#     ))
#   }
#
#   # Recursive cases: list and all other functions -----------------------------
#
#   if (is.list(expr)) {
#     args <- lapply(expr, translate_window_where, tbl = tbl, con = con)
#
#     env <- sql_env(call, variant, con = con)
#     sql <- lapply(lapply(args, "[[", "expr"), eval, env = env)
#   } else {
#     args <- lapply(expr[-1], translate_window_where, tbl = tbl, con = con)
#
#     call <- as.call(c(expr[[1]], lapply(args, "[[", "expr")))
#     env <- sql_env(call, variant, con = con)
#     sql <- eval(call, envir = env)
#   }
#
#   comps <- unlist(lapply(args, "[[", "comp"), recursive = FALSE)
#
#   list(
#     expr = sql,
#     comp = comps
#   )
# }
#
# unique_name <- local({
#   i <- 0
#
#   function() {
#     i <<- i + 1
#     paste0("_W", i)
#   }
# })
#
# unique_names <- function(x_names, y_names, by, x_suffix = ".x", y_suffix = ".y") {
#   # See: https://github.com/hadley/dplyr/issues/709
#   common <- intersect(x_names, y_names)
#   if (length(common) == 0) return(NULL)
#
#   x_match <- match(common, x_names)
#   x_new <- x_names
#   x_new[x_match] <- paste0(x_names[x_match], x_suffix)
#
#   y_match <- match(common, y_names)
#   y_new <- y_names
#   y_new[y_match] <- paste0(y_names[y_match], y_suffix)
#
#   list(x = setNames(x_new, x_names), y = setNames(y_new, y_names))
# }
#
# uses_window_fun <- function(x, tbl) {
#   if (is.null(x)) return(FALSE)
#   if (is.list(x)) {
#     calls <- unlist(lapply(x, all_calls))
#   } else {
#     calls <- all_calls(x)
#   }
#
#   win_f <- ls(envir = src_translate_env(tbl)$window)
#   any(calls %in% win_f)
# }
#
# update.tbl_sql <- function(object, ...) {
#   args <- list(...)
#   assertthat::assert_that(only_has_names(args,
#     c("select", "where", "group_by", "order_by", "summarise")))
#
#   for (nm in names(args)) {
#     object[[nm]] <- args[[nm]]
#   }
#
#   # Figure out variables
#   if (is.null(object$select)) {
#     var_names <- dplyr::db_query_fields(object$src$con, object$from)
#     vars <- lapply(var_names, as.name)
#     object$select <- vars
#   }
#
#   object$query <- build_query(object)
#   object
# }
#
