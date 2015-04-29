#' @importFrom dplyr sql_select
#' @export
sql_select.SQLServerConnection <- function(con, select, from, where = NULL,
  group_by = NULL, having = NULL, order_by = NULL, top = NULL,
  offset = NULL, fetch = NULL, ...) {

  out <- vector("list", 9)
  names(out) <- c("select", "from", "where", "group_by", "having", "order_by",
    "top", "offset", "fetch")

  if (!is.null(top)) {
    assertthat::assert_that(is.integer(top), length(top) == 1L)
    top <- paste0("TOP ", top)
  }

  assertthat::assert_that(is.character(select), length(select) > 0L)
  out$select <- dplyr::build_sql("SELECT ", top,
    dplyr::escape(select, collapse = ", ", con = con))
  assertthat::assert_that(is.character(from), length(from) == 1L)
  out$from <- dplyr::build_sql("FROM ", from, con = con)

  if (length(where) > 0L) {
    assertthat::assert_that(is.character(where))
    out$where <- dplyr::build_sql("WHERE ",
      dplyr::escape(where, collapse = " AND ", con = con))
  }

  if (!is.null(group_by)) {
    assertthat::assert_that(is.character(group_by), length(group_by) > 0L)
    out$group_by <- dplyr::build_sql("GROUP BY ",
      dplyr::escape(group_by, collapse = ", ", con = con))
  }

  if (!is.null(having)) {
    assertthat::assert_that(is.character(having), length(having) == 1L)
    out$having <- dplyr::build_sql("HAVING ",
      dplyr::escape(having, collapse = ", ", con = con))
  }

  if (is.null(order_by)) {
    if (!is.null(top)) {
      # MS best practice: use ORDER BY clause when using TOP clause
      # This is the only way to predictably indicate which rows are affected by
      # TOP.
      # Source: http://msdn.microsoft.com/en-us/library/ms189463.aspx
      order_by <- dplyr::build_sql("ORDER BY ",
        dplyr::escape(order_by, collapse = ", ", con = con))
    }
  } else {
    assertthat::assert_that(is.character(order_by), length(order_by) > 0L)
    out$order_by <- dplyr::build_sql("ORDER BY ",
      dplyr::escape(order_by, collapse = ", ", con = con))
  }

  if (!is.null(offset)) {
    # OFFSET/FETCH supported for SQL Server 2012 and higher (i.e. 11 and higher)
    # when using ORDER BY
    # http://stackoverflow.com/questions/187998/row-offset-in-sql-server
    # MSSQL versioning: http://support2.microsoft.com/kb/321185
    # OFFSET/FETCH: http://msdn.microsoft.com/en-us/library/ms188385(v=sql.110).aspx
    assertthat::assert_that(!is.null(order_by), dbGetInfo(con)$db.version >= 11,
      is.integer(offset), length(offset) == 1L)
    out$offset <- dplyr::build_sql("OFFSET ", offset, con = con)
  }

  if (!is.null(fetch)) {
    # SQL Server 2012 + equivalent of LIMIT is FETCH (used with OFFSET)
    # out$offset will be non-NULL if it is set and SQL Server dependency is met.
    assertthat::assert_that(!is.null(out$offset), is.integer(fetch),
      length(fetch) == 1L)
    out$fetch <- dplyr::build_sql("FETCH NEXT ", fetch, " ONLY", con = con)
  }

  dplyr::escape(unname(compact(out)), collapse = "\n", parens = FALSE, con = con)
}

# #' @importFrom dplyr sql_join
# #' @export
# sql_join.SQLServerConnection <- function(con, x, y, type = "inner",
#   by = NULL, ...) {
#   join <- switch(type,
#     left = dplyr::sql("LEFT"),
#     inner = dplyr::sql("INNER"),
#     right = dplyr::sql("RIGHT"),
#     full = dplyr::sql("FULL"),
#     stop("Unknown join type:", type, call. = FALSE)
#   )
#
#   by <- common_by(by, x, y)
#
#   # Ensure tables have unique names
#   x_names <- auto_names(x$select)
#   y_names <- auto_names(y$select)
#   uniques <- unique_names(x_names, y_names, by$x[by$x == by$y])
#
#   if (is.null(uniques)) {
#     sel_vars <- c(x_names, y_names)
#   } else {
#     x <- update(x, select = setNames(x$select, uniques$x))
#     y <- update(y, select = setNames(y$select, uniques$y))
#
#     by$x <- unname(uniques$x[by$x])
#     by$y <- unname(uniques$y[by$y])
#
#     sel_vars <- unique(c(uniques$x, uniques$y))
#   }
#
#   xname <- unique_name()
#   yname <- unique_name()
#   on <- sql_vector(paste0(
#     paste0(dplyr::sql_escape_ident(con, xname), ".",
#       dplyr::sql_escape_ident(con, by$x)),
#     " = ",
#     paste0(dplyr::sql_escape_ident(con, yname), ".",
#       dplyr::sql_escape_ident(con, by$y)),
#     collapse = " AND "), parens = TRUE)
#   cond <- dplyr::build_sql("ON ", on, con = con)
#
#   from <- dplyr::build_sql(
#     'SELECT * FROM ',
#     dplyr::sql_subquery(con, x$query$sql, xname), "\n\n",
#     join, " JOIN \n\n" ,
#     dplyr::sql_subquery(con, y$query$sql, yname), "\n\n",
#     cond, con = con
#   )
#   attr(from, "vars") <- lapply(sel_vars, as.name)
#
#   from
# }
#

build_query <- function (x, top = NULL) {
  assertthat::assert_that(is.null(top) || (is.numeric(top) && length(top) == 1))
  translate <- function (expr, ...) {
    dplyr::translate_sql_q(expr, tbl = x, env = NULL, ...)
  }
  if (x$summarise) {
    # Summarising, so SELECT needs to contain grouping variables
    select <- c(x$group_by, x$select)
    select <- select[!duplicated(select)]

    select_sql <- translate(select)
    vars <- dplyr:::auto_names(select)

    group_by_sql <- translate(x$group_by)
    order_by_sql <- translate(x$order_by)
  } else {
    # Not in summarise, so assume functions are window functions
    select_sql <- translate(x$select, window = uses_window_fun(x$select, x))
    vars <- dplyr:::auto_names(x$select)

    # Don't use group_by - grouping affects window functions only
    group_by_sql <- NULL

    # If the user requested ordering, ensuring group_by is included
    # Otherwise don't, because that may make queries substantially slower
    if (!is.null(x$order_by) && !is.null(x$group_by)) {
      order_by_sql <- translate(c(x$group_by, x$order_by))
    } else{
      order_by_sql <- translate(x$order_by)
    }
  }

  if (!uses_window_fun(x$where, x)) {
    from_sql <- x$from
    where_sql <- translate(x$where)
  } else {
    # window functions in WHERE need to be performed in subquery
    where <- translate_window_where(x$where, x, con = x$src$con)
    base_query <- update(x, group_by = NULL, where = NULL,
      select = c(x$select, where$comp))$query

    from_sql <- dplyr::build_sql("(", base_query$sql, ") AS ",
      dplyr::ident(unique_name()), con = x$src$con)
    where_sql <- translate(where$expr)
  }

  sql <- sql_select(x$src$con, from = from_sql, select = select_sql,
    where = where_sql, order_by = order_by_sql, group_by = group_by_sql,
    top = top)
  dplyr::query(x$src$con, sql, vars)
}

