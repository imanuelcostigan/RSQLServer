#' @export
src_sqlserver <- function (server, ...)
{
  con <- dbConnect(SQLServer(), server = server, ...)
  info <- dbGetInfo(con)
  dplyr::src_sql("sqlserver", con = con, info = info, ...)
}

#' @export
src_desc.src_sqlserver <- function (x)
{
  info <- x$info
  paste0(info$db.product.name, ' version ', info$db.version, " [", info$user, "]")
}

#' @export
src_translate_env.src_sqlserver <- function (x)
{
  dplyr::sql_variant(
    dplyr::base_scalar,
    dplyr::sql_translator(.parent = dplyr::base_agg,
      n = function() dplyr::sql("COUNT(*)"),
      mean = dplyr::sql_prefix('AVG'),
      sd =  dplyr::sql_prefix("STDEV")
    ),
    dplyr::base_win
  )
}

#' @export
tbl.src_sqlserver <- function (src, from, ...)
  dplyr::tbl_sql("sqlserver", src = src, from = from, ...)

#' @export
head.tbl_sqlserver <- function(x, n = 6L, ...) {
  assertthat::assert_that(length(n) == 1, n > 0L)
  dplyr:::build_query(x)$fetch(n)
}

# DB backend methods ------------------------------------------------------------------

db_query_fields.SQLServerConnection <- function (con, sql, ...)
  dbListFields(con, sql)

db_query_rows.SQLServerConnection <- function(con, sql, ...) {
  info <- dbGetInfo(con)
  info$tbls[info$tbls == sql, ][['Records']]
}

db_save_query.SQLServerConnection <- function (con, sql, name,
  temporary = TRUE, ...) {
  stop ("Temporary tables are not supported.")
}


db_explain.SQLServerConnection <- function (con, sql, ...)
{
  stop ('SQL Server does not provide an explain statement.', call. = FALSE)
  # Though may be possible to use SHOWPLAN
  # http://msdn.microsoft.com/en-us/library/ms187735.aspx
}

# SQL backend methods --------------------------------------------------------------
#' @export
sql_select.SQLServerConnection <- function(con, select, from, where = NULL,
  group_by = NULL, having = NULL, order_by = NULL, limit = NULL,
  offset = NULL, top = NULL, ...) {

  out <- vector("list", 8)
  names(out) <- c("select", "from", "where", "group_by", "having", "order_by",
    "offset", "limit")
  browser()
  assertthat::assert_that(is.character(select), length(select) > 0L)
  top_clause <- if (!is.null(top)) paste0("TOP ", top)
  out$select <- dplyr::build_sql("SELECT ", top_clause,
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

  if (!is.null(order_by)) {
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

  if (!is.null(limit)) {
    # SQL Server 2012 + equivalent of LIMIT is FETCH (used with OFFSET)
    # out$offset will be non-NULL if it is set and SQL Server dependency is met.
    assertthat::assert_that(!is.null(out$offset), is.integer(limit),
      length(limit) == 1L)
    out$limit <- dplyr::build_sql("FETCH NEXT ", limit, " ONLY", con = con)
  }
  dplyr::escape(unname(dplyr:::compact(out)), collapse = "\n", parens = FALSE,
    con = con)
}

