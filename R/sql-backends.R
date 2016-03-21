#' @importFrom dplyr sql_select
#' @export
sql_select.SQLServerConnection <- function (con, select, from, where = NULL,
  group_by = NULL, having = NULL, order_by = NULL, limit = NULL,
  offset = NULL, fetch = NULL, is_percent = NULL, into = NULL, ...) {

  # REFERENCES --------------------------------------------------------------
  # 2000 : https://technet.microsoft.com/en-us/library/aa259187(v=sql.80).aspx
  # 2005+: https://msdn.microsoft.com/en-us/library/ms189499(v=sql.90).aspx


  # SETUP -------------------------------------------------------------------

  out <- vector("list", 10)
  names(out) <- c("select", "from", "where", "group_by", "having",
    "order_by", "limit", "offset", "fetch", "into")

  # SELECT ------------------------------------------------------------------

  assertthat::assert_that(is.character(select), length(select) > 0L)

  if (!is.null(limit)) {
    limit <- mssql_top(con, limit, is_percent)
  }

  out$select <- build_sql("SELECT ", limit, " ",
    escape(select, collapse = ", ", con = con), con = con)

  # INTO --------------------------------------------------------------------

  if (!is.null(into)) {
    out$into <- build_sql("INTO ", into, con = con)
  }

  # FROM --------------------------------------------------------------------

  assertthat::assert_that(assertthat::is.string(from))
  out$from <- build_sql("FROM ", from, con = con)

  # WHERE -------------------------------------------------------------------

  if (length(where) > 0L) {
    assertthat::assert_that(is.character(where))
    out$where <- build_sql("WHERE ",
      escape(where, collapse = " AND ", con = con))
  }

  # GROUP BY ----------------------------------------------------------------

  if (!is.null(group_by)) {
    assertthat::assert_that(is.character(group_by), length(group_by) > 0L)
    out$group_by <- build_sql("GROUP BY ",
      escape(group_by, collapse = ", ", con = con))
  }

  # HAVING ------------------------------------------------------------------

  if (!is.null(having)) {
    assertthat::assert_that(assertthat::is.string(having))
    out$having <- build_sql("HAVING ",
      escape(having, collapse = ", ", con = con))
  }

  # ORDER BY ----------------------------------------------------------------

  if (!is.null(order_by)) {
    assertthat::assert_that(is.character(order_by), length(order_by) > 0L)
    out$order_by <- build_sql("ORDER BY ",
      escape(order_by, collapse = ", ", con = con))
  }

  # Offset
  if (!is.null(offset)) {
    # OFFSET/FETCH supported for SQL Server 2012 and higher (i.e. 11 and higher)
    # when using ORDER BY
    # http://stackoverflow.com/questions/187998/row-offset-in-sql-server
    # MSSQL versioning: http://support2.microsoft.com/kb/321185
    # OFFSET/FETCH: http://msdn.microsoft.com/en-us/library/ms188385(v=sql.110).aspx
    assertthat::assert_that(!is.null(order_by),
      dbGetInfo(con)$db.version >= 11, is.integer(offset), offset >= 0)
    out$offset <- build_sql("OFFSET ", offset, con = con)
  }

  # Fetch
  if (!is.null(fetch)) {
    # SQL Server 2012 + equivalent of LIMIT is FETCH (used with OFFSET)
    # offset will be non-NULL if it is set and SQL Server dependency is met.
    assertthat::assert_that(!is.null(offset), dbGetInfo(con)$db.version >= 11,
      is.integer(fetch), fetch >= 0)
    out$fetch <- build_sql("FETCH ", fetch, " ONLY", con = con)
  }

  # Resulting SELECT --------------------------------------------------------

  escape(unname(compact(out)), collapse = "\n", parens = FALSE, con = con)
}

#' @importFrom dplyr sql_join sql sql_escape_ident
#' @importFrom stats setNames
#' @export
sql_join.SQLServerConnection <- function(con, x, y, type = "inner",
  by = NULL, ...) {
  join <- switch(type,
    left = sql("LEFT"),
    inner = sql("INNER"),
    right = sql("RIGHT"),
    full = sql("FULL"),
    stop("Unknown join type:", type, call. = FALSE)
  )

  by <- common_by(by, x, y)

  # Ensure tables have unique names
  x_names <- auto_names(x$select)
  y_names <- auto_names(y$select)
  uniques <- unique_names(x_names, y_names, by$x[by$x == by$y])

  if (is.null(uniques)) {
    sel_vars <- c(x_names, y_names)
  } else {
    x <- update(x, select = setNames(x$select, uniques$x))
    y <- update(y, select = setNames(y$select, uniques$y))

    by$x <- unname(uniques$x[by$x])
    by$y <- unname(uniques$y[by$y])

    sel_vars <- unique(c(uniques$x, uniques$y))
  }

  xname <- unique_name()
  yname <- unique_name()
  on <- sql_vector(paste0(
    paste0(sql_escape_ident(con, xname), ".",
      sql_escape_ident(con, by$x)), " = ",
    paste0(sql_escape_ident(con, yname), ".",
      sql_escape_ident(con, by$y)),
    collapse = " AND "), parens = TRUE)
  cond <- build_sql("ON ", on, con = con)

  from <- build_sql(
    'SELECT * FROM ',
    sql_subquery(con, x$query$sql, xname), "\n\n",
    join, " JOIN \n\n" ,
    sql_subquery(con, y$query$sql, yname), "\n\n",
    cond, con = con
  )
  attr(from, "vars") <- lapply(sel_vars, as.name)

  from
}

#' @importFrom dplyr escape
mssql_top <- function (con, n, is_percent = NULL) {
  # https://technet.microsoft.com/en-us/library/aa259187(v=sql.80).aspx
  # https://msdn.microsoft.com/en-us/library/ms189463(v=sql.90).aspx
  assertthat::assert_that(assertthat::is.number(n), n >= 0)
  n <- as.integer(n)
  is_mssql_2000 <- dbGetInfo(con)$db.version == 8
  if (is.null(is_percent) || !isTRUE(is_percent)) {
    if (!is_mssql_2000) n <- escape(n, parens = TRUE)
    return(build_sql("TOP ", n))
  } else {
    # Assume TOP n PERCENT. n must already be >= 0
    assertthat::assert_that(n <= 100)
    if (!is_mssql_2000) n <- escape(n, parens = TRUE)
    return(build_sql("TOP ", n, " PERCENT"))
  }
}
