#' @export
src_sqlserver <- function (server, ...)
{
  if (!require("RSQLServer"))
    stop("RSQLServer package required to connect to SQL Server", call. = FALSE)
  con <- dbConnect(SQLServer(), server = server, ...)
  info <- dbGetInfo(con)
  src_sql("sqlserver", con, info = info,
    disco = db_disconnector(con, "sqlserver"))
}

#' @export
src_desc.src_sqlserver <- function (x)
{
  info <- x$info
  paste0(info$dbname, ' version ', info$db.version, " [", info$user, "]")
}

#' @export
src_translate_env.src_sqlserver <- function (x)
{
  sql_variant(
    base_scalar,
    sql_translator(.parent = base_agg,
      n = function() sql("COUNT(*)"),
      mean = sql_prefix('AVG'),
      sd =  sql_prefix("STDEV")
    ),
    base_win
  )
}

#' @export
tbl.src_sqlserver <- function (src, from, ...)
{
  tbl_sql("sqlserver", src = src, from = from, ...)
}

#' @export
head.tbl_sqlserver <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)
  build_query(x)$fetch(n)
}


# DB backend methods ------------------------------------------------------------------
#' @export
db_list_tables.SQLServerConnection <- function (con)
  DBI::dbListTables(con)

#' @export
db_has_table.SQLServerConnection <- function (con, table)
  table %in% db_list_tables(con)

#' @export
db_query_fields.SQLServerConnection <- function (con, sql, ...)
{
  rs <- dbSendQuery(con, paste0("SELECT * FROM ", sql))
  on.exit(dbClearResult(rs))
  names(fetch(rs, 1L))
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

  assert_that(is.character(select), length(select) > 0L)
  top_clause <- if (!is.null(top)) paste0("TOP ", top)
  out$select <- build_sql("SELECT ", top_clause,
    escape(select, collapse = ", ", con = con))
  assert_that(is.character(from), length(from) == 1L)
  out$from <- build_sql("FROM ", from, con = con)

  if (length(where) > 0L) {
    assert_that(is.character(where))
    out$where <- build_sql("WHERE ",
      escape(where, collapse = " AND ", con = con))
  }

  if (!is.null(group_by)) {
    assert_that(is.character(group_by), length(group_by) > 0L)
    out$group_by <- build_sql("GROUP BY ",
      escape(group_by, collapse = ", ", con = con))
  }

  if (!is.null(having)) {
    assert_that(is.character(having), length(having) == 1L)
    out$having <- build_sql("HAVING ",
      escape(having, collapse = ", ", con = con))
  }

  if (!is.null(order_by)) {
    assert_that(is.character(order_by), length(order_by) > 0L)
    out$order_by <- build_sql("ORDER BY ",
      escape(order_by, collapse = ", ", con = con))
  }

  if (!is.null(offset)) {
    # OFFSET/FETCH supported for SQL Server 2012 and higher (i.e. 11 and higher)
    # when using ORDER BY
    # http://stackoverflow.com/questions/187998/row-offset-in-sql-server
    # MSSQL versioning: http://support2.microsoft.com/kb/321185
    # OFFSET/FETCH: http://msdn.microsoft.com/en-us/library/ms188385(v=sql.110).aspx
    assert_that(!is.null(order_by), dbGetInfo(con)$db.version >= 11,
      is.integer(offset), length(offset) == 1L)
    out$offset <- build_sql("OFFSET ", offset, con = con)
  }

  if (!is.null(limit)) {
    # SQL Server 2012 + equivalent of LIMIT is FETCH (used with OFFSET)
    # out$offset will be non-NULL if it is set and SQL Server dependency is met.
    assert_that(!is.null(out$offset), is.integer(limit), length(limit) == 1L)
    out$limit <- build_sql("FETCH NEXT ", limit, " ONLY", con = con)
  }
  escape(unname(compact(out)), collapse = "\n", parens = FALSE, con = con)
}
