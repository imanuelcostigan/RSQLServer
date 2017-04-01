#' @importFrom dplyr sql_select
#' @importFrom dbplyr sql
#' @export

sql_select.SQLServerConnection <- function (con, select, from, where = NULL,
  group_by = NULL, having = NULL, order_by = NULL, limit = NULL, distinct = FALSE,
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

  if (length(limit) > 0L || length(order_by) > 0L) {
    # If ordering, then TOP should be specified, if it isn't already,
    # to ensure query works when query is part of a subquery. See #49
    # Code via Nick Kennedy:
    # https://github.com/imanuelcostigan/RSQLServer/pull/129#commitcomment-20230748
    if (length(limit) == 0L) {
      limit <- mssql_top(con, 100, TRUE)
    } else {
      limit <- mssql_top(con, limit, is_percent %||% FALSE)
    }
  }

  assertthat::assert_that(is.character(select), length(select) > 0L)
  out$select <- build_sql("SELECT ",
    if (distinct) sql("DISTINCT "), limit, " ",
    escape(select, collapse = ", ", con = con), con = con)

  # INTO --------------------------------------------------------------------

  if (length(into) > 0L) {
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

  if (length(group_by) > 0L) {
    assertthat::assert_that(is.character(group_by), length(group_by) > 0L)
    out$group_by <- build_sql("GROUP BY ",
      escape(group_by, collapse = ", ", con = con))
  }

  # HAVING ------------------------------------------------------------------

  if (length(having) > 0L) {
    assertthat::assert_that(assertthat::is.string(having))
    out$having <- build_sql("HAVING ",
      escape(having, collapse = ", ", con = con))
  }

  # ORDER BY ----------------------------------------------------------------

  if (length(order_by) > 0L) {
    assertthat::assert_that(is.character(order_by), length(order_by) > 0L)
    out$order_by <- build_sql("ORDER BY ",
      escape(order_by, collapse = ", ", con = con))
  }

  # Offset
  if (length(offset) > 0L) {
    # OFFSET/FETCH supported for SQL Server 2012 and higher (i.e. 11 and higher)
    # when using ORDER BY
    # http://stackoverflow.com/questions/187998/row-offset-in-sql-server
    # MSSQL versioning: http://support2.microsoft.com/kb/321185
    # OFFSET/FETCH: http://msdn.microsoft.com/en-us/library/ms188385(v=sql.110).aspx
    assertthat::assert_that(length(order_by) > 0L,
      dbGetInfo(con)$db.version >= 11, is.integer(offset), offset >= 0)
    out$offset <- build_sql("OFFSET ", offset, con = con)
  }

  # Fetch
  if (length(fetch) > 0L) {
    # SQL Server 2012 + equivalent of LIMIT is FETCH (used with OFFSET)
    # offset will be non-NULL if it is set and SQL Server dependency is met.
    assertthat::assert_that(length(offset) > 0L, dbGetInfo(con)$db.version >= 11,
      is.integer(fetch), fetch >= 0)
    out$fetch <- build_sql("FETCH ", fetch, " ONLY", con = con)
  }

  # Resulting SELECT --------------------------------------------------------

  escape(unname(compact(out)), collapse = "\n", parens = FALSE, con = con)
}


#' @importFrom dbplyr escape
mssql_top <- function (con, n, is_percent = NULL) {
  # https://technet.microsoft.com/en-us/library/aa259187(v=sql.80).aspx
  # https://msdn.microsoft.com/en-us/library/ms189463(v=sql.90).aspx
  assertthat::assert_that(assertthat::is.number(n), n >= 0)
  n <- as.integer(n)
  is_mssql_2000 <- dbGetInfo(con)$db.version[1,1] == 8
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

## Math (scalar) functions - no change across versions based on eyeballing:
# MSSQL 2000: https://technet.microsoft.com/en-us/library/aa258862(v=sql.80).aspx
# MSSQL 2005: https://technet.microsoft.com/en-us/library/ms177516(v=sql.90).aspx
# MSSQL 2008: https://technet.microsoft.com/en-us/library/ms177516(v=sql.100).aspx
# MSSQL 2008(r2): https://technet.microsoft.com/en-us/library/ms177516(v=sql.105).aspx
# MSSQL 2012: https://technet.microsoft.com/en-us/library/ms177516(v=sql.110).aspx

## Aggregate functions
# MSSQL 2005: https://technet.microsoft.com/en-US/library/ms173454(v=sql.90).aspx
# MSSQL 2008: https://technet.microsoft.com/en-US/library/ms173454(v=sql.100).aspx
# MSSQL 2008r2*: https://technet.microsoft.com/en-US/library/ms173454(v=sql.100).aspx
# MSSQL 2012*: https://technet.microsoft.com/en-US/library/ms173454(v=sql.110).aspx
# MSSQL 2014: https://technet.microsoft.com/en-US/library/ms173454(v=sql.120).aspx
#' @importFrom dplyr sql_translate_env
#' @importFrom dbplyr base_agg base_scalar sql_prefix base_win sql_variant sql_translator
#' @export
sql_translate_env.SQLServerConnection <- function (con) {
  sql_variant(
    scalar = sql_translator(.parent = base_scalar,
      # http://sqlserverplanet.com/tsql/format-string-to-date
      as.POSIXct = function(x) build_sql("CAST(", x, " AS DATETIME)"),
      # DATE data type only available since SQL Server 2008
      as.Date = function (x) build_sql("CAST(", x, " AS DATE)"),
      as.numeric = function(x) build_sql("CAST(", x, " AS FLOAT)"),
      as.character = function(x) build_sql("CAST(", x, " AS NVARCHAR(4000))")
    ),
    aggregate = sql_translator(.parent = base_agg,
      n = function() sql("COUNT(*)"),
      mean = sql_prefix('AVG'),
      sd = sql_prefix("STDEV"),
      sdp = sql_prefix("STDEVP"),
      varp = sql_prefix("VARP")
    ),
    window = base_win
  )
}
