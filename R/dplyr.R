#' Connect to SQLServer or Sybase
#'
#' Use \code{src_sqlserver} to connect to an existing SQL Server or Sybase
#' database, and \code{tbl} to connect to tables within that database.
#'
#' @template sqlserver-parameters
#' @return a dplyr SQL based src with subclass \code{sqlserver}
#' @examples
#' \dontrun{
#' library(dplyr)
#' # Connection basics ---------------------------------------------------------
#' # To connect to TEST database, assumed to be specified in your ~/sql.yaml
#' # file (see \code{\link{have_test_server}}), first create a src:
#' my_src <- src_sqlserver("TEST")
#' # Then reference a tbl within that src
#' my_tbl <- tbl(my_src, "my_table")
#' # Methods -------------------------------------------------------------------
#' # You can then inspect table and perform actions on it
#' dim(my_tbl)
#' colnames(my_tbl)
#' head(my_tbl)
#' # Data manipulation verbs ---------------------------------------------------
#' filter(my_tbl, this.field == "that.value")
#' select(my_tbl, from.this.field:to.that.field)
#' arrange(my_tbl, this.field)
#' mutate(my_tbl, squared.field = field ^ 2)
#' # Group by operations -------------------------------------------------------
#' by_field <- group_by(my_tbl, field)
#' group_size(by_field)
#' by_field %>% summarise(ave = mean(numeric.field))
#' # See dplyr documentation for further information on data operations
#' }
#' @export
src_sqlserver <- function (server, file = NULL, database = "",
  type = "sqlserver", port = "", properties = list()) {
  con <- dbConnect(SQLServer(), server, file, database , type, port, properties)
  info <- dbGetInfo(con)
  dplyr::src_sql("sqlserver", con, info = info)
}

#' @importFrom dplyr src_desc
#' @export
src_desc.src_sqlserver <- function (x) {
  info <- x$info
  paste0(info$db.product.name, ' version ', info$db.version, " [", info$user, "]")
}

#' @importFrom dplyr tbl
#' @export
tbl.src_sqlserver <- function (src, from, ...) {
  dplyr::tbl_sql("sqlserver", src = src, from = from, ...)
}

# DBI backend methods ------------------------------------------------------------------

#' @importFrom dplyr db_list_tables
#' @export
db_list_tables.SQLServerConnection <- function (con) {
  dbListTables(con)
}

#' @importFrom dplyr db_has_table
#' @export
db_has_table.SQLServerConnection <- function (con, table) {
  table %in% db_list_tables(con)
}

#
#
# #' @importFrom dplyr src_translate_env
# #' @export
# src_translate_env.src_sqlserver <- function (x) {
#   dplyr::sql_variant(
#     dplyr::base_scalar,
#     dplyr::sql_translator(.parent = dplyr::base_agg,
#       n = function() dplyr::sql("COUNT(*)"),
#       mean = dplyr::sql_prefix('AVG'),
#       sd = dplyr::sql_prefix("STDEV")
#     ),
#     dplyr::base_win
#   )
# }
#
# #' @export
# head.tbl_sqlserver <- function (x, n = 6L, ...) {
#   assertthat::assert_that(length(n) == 1, n > 0L)
#   build_query_ss(x)$fetch(n)
# }
#
# #' @importFrom dplyr compute
# #' @export
# compute.tbl_sqlserver <- function (x, name = random_table_name(),
#   temporary = TRUE, ...) {
#   name <- paste0(if (temporary) dplyr::sql("#"), name)
#   db_save_query(x$src$con, x$query$sql, name = name, temporary = temporary)
#   update(dplyr::tbl(x$src, name), group_by = dplyr::groups(x))
# }
#
# #' @importFrom dplyr intersect
# #' @export
# intersect.tbl_sqlserver <- function(x, y, copy = FALSE, ...) {
#   # SQL Server 2000 does not support INTERSECT or EXCEPT
#   assertthat::assert_that(x$src$info$db.version > 8, y$src$info$db.version > 8)
#   y <- auto_copy(x, y, copy)
#   sql <- dplyr::sql_set_op(x$src$con, x, y, "INTERSECT")
#   update(tbl(x$src, sql), group_by = dplyr::groups(x))
# }
#
# #' @importFrom dplyr setdiff
# #' @export
# setdiff.tbl_sqlserver <- function(x, y, copy = FALSE, ...) {
#   # SQL Server 2000 does not support INTERSECT or EXCEPT
#   assertthat::assert_that(x$src$info$db.version > 8, y$src$info$db.version > 8)
#   y <- auto_copy(x, y, copy)
#   sql <- dplyr::sql_set_op(x$src$con, x, y, "EXCEPT")
#   update(tbl(x$src, sql), group_by = dplyr::groups(x))
# }
#
#
# #' @importFrom dplyr db_query_fields
# #' @export
# db_query_fields.SQLServerConnection <- function (con, sql, ...) {
#   rs <- dbSendQuery(con, paste0("SELECT * FROM ", sql, " WHERE 0=1"))
#   on.exit(dbClearResult(rs))
#   names(fetch(rs, 1L))
# }
#
# #' @importFrom dplyr db_query_rows
# #' @export
# db_query_rows.SQLServerConnection <- function(con, sql, ...) {
#   qry <- dplyr::build_sql(sql, con = con)
#   dbSendQuery(con, qry)
#   qry <- dplyr::build_sql("SELECT @@ROWCOUNT")
#   as.integer(dbGetQuery(con, qry))
# }
#
# #' @importFrom dplyr db_save_query
# #' @export
# db_save_query.SQLServerConnection <- function (con, sql, name, temporary = TRUE,
#   ...) {
#   tt_sql <- dplyr::build_sql("SELECT * ", "INTO ", dplyr::ident(name),
#     " FROM (", sql, ") AS MASTER")
#   js <- J(con@jc, "createStatement")
#   J(js, "execute", as.character(tt_sql)[1])
#   name
# }
#
# #' @importFrom dplyr db_explain
# #' @export
# db_explain.SQLServerConnection <- function (con, sql, ...) {
#   message('SQL Server does not provide an EXPLAIN statement.')
#   # Though may be possible to use SHOWPLAN
#   # http://msdn.microsoft.com/en-us/library/ms187735.aspx
#   # Maybe use same strategy as db_query_rows
# }
#
# # SQL backend methods --------------------------------------------------------------
#
# #' @importFrom dplyr sql_select
# #' @export
# sql_select.SQLServerConnection <- function(con, select, from, where = NULL,
#   group_by = NULL, having = NULL, order_by = NULL, top = NULL,
#   offset = NULL, fetch = NULL, ...) {
#
#   out <- vector("list", 9)
#   names(out) <- c("select", "from", "where", "group_by", "having", "order_by",
#     "top", "offset", "fetch")
#
#   if (!is.null(top)) {
#     assertthat::assert_that(is.integer(top), length(top) == 1L)
#     top <- paste0("TOP ", top)
#     if (is.null(order_by))
#     {
#       # MS best practice: use ORDER BY clause when using TOP clause
#       # This is the only way to predictably indicate which rows are affected by
#       # TOP.
#       # Source: http://msdn.microsoft.com/en-us/library/ms189463.aspx
#       order_by <- dplyr::build_sql("ORDER BY ",
#         dplyr::escape(order_by, collapse = ", ", con = con))
#     }
#   }
#   assertthat::assert_that(is.character(select), length(select) > 0L)
#   out$select <- dplyr::build_sql("SELECT ", top,
#     dplyr::escape(select, collapse = ", ", con = con))
#   assertthat::assert_that(is.character(from), length(from) == 1L)
#   out$from <- dplyr::build_sql("FROM ", from, con = con)
#
#   if (length(where) > 0L) {
#     assertthat::assert_that(is.character(where))
#     out$where <- dplyr::build_sql("WHERE ",
#       dplyr::escape(where, collapse = " AND ", con = con))
#   }
#
#   if (!is.null(group_by)) {
#     assertthat::assert_that(is.character(group_by), length(group_by) > 0L)
#     out$group_by <- dplyr::build_sql("GROUP BY ",
#       dplyr::escape(group_by, collapse = ", ", con = con))
#   }
#
#   if (!is.null(having)) {
#     assertthat::assert_that(is.character(having), length(having) == 1L)
#     out$having <- dplyr::build_sql("HAVING ",
#       dplyr::escape(having, collapse = ", ", con = con))
#   }
#
#   if (!is.null(order_by)) {
#     assertthat::assert_that(is.character(order_by), length(order_by) > 0L)
#     out$order_by <- dplyr::build_sql("ORDER BY ",
#       dplyr::escape(order_by, collapse = ", ", con = con))
#   }
#
#   if (!is.null(offset)) {
#     # OFFSET/FETCH supported for SQL Server 2012 and higher (i.e. 11 and higher)
#     # when using ORDER BY
#     # http://stackoverflow.com/questions/187998/row-offset-in-sql-server
#     # MSSQL versioning: http://support2.microsoft.com/kb/321185
#     # OFFSET/FETCH: http://msdn.microsoft.com/en-us/library/ms188385(v=sql.110).aspx
#     assertthat::assert_that(!is.null(order_by), dbGetInfo(con)$db.version >= 11,
#       is.integer(offset), length(offset) == 1L)
#     out$offset <- dplyr::build_sql("OFFSET ", offset, con = con)
#   }
#
#   if (!is.null(fetch)) {
#     # SQL Server 2012 + equivalent of LIMIT is FETCH (used with OFFSET)
#     # out$offset will be non-NULL if it is set and SQL Server dependency is met.
#     assertthat::assert_that(!is.null(out$offset), is.integer(fetch),
#       length(fetch) == 1L)
#     out$fetch <- dplyr::build_sql("FETCH NEXT ", fetch, " ONLY", con = con)
#   }
#   dplyr::escape(unname(compact(out)), collapse = "\n", parens = FALSE,
#     con = con)
# }
#
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
# build_query_ss <- function (x, top = NULL) {
#   assertthat::assert_that(is.null(top) || (is.numeric(top) && length(top) == 1))
#   translate <- function (expr, ...)
#     dplyr::translate_sql_q(expr, tbl = x, env = NULL, ...)
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
#     select_sql <- translate(x$select,
#       window = uses_window_fun(x$select, x))
#     vars <- auto_names(x$select)
#
#     # Don't use group_by - grouping affects window functions only
#     group_by_sql <- NULL
#
#     # If the user requested ordering, ensuring group_by is included
#     # Otherwise don't, because that may make queries substantially slower
#     if (!is.null(x$order_by) && !is.null(x$group_by))
#       order_by_sql <- translate(c(x$group_by, x$order_by))
#     else
#       order_by_sql <- translate(x$order_by)
#   }
#
#   if (!uses_window_fun(x$where, x)) {
#     from_sql <- x$from
#     where_sql <- translate(x$where)
#   } else {
#     # window functions in WHERE need to be performed in subquery
#     where <- translate_window_where(x$where, x, con = x$src$con)
#     base_query <- update(x, group_by = NULL, where = NULL,
#       select = c(x$select, where$comp))$query
#
#     from_sql <- dplyr::build_sql("(", base_query$sql, ") AS ",
#       dplyr::ident(unique_name()), con = x$src$con)
#     where_sql <- translate(where$expr)
#   }
#
#   sql <- sql_select(x$src$con, from = from_sql, select = select_sql,
#     where = where_sql, order_by = order_by_sql, group_by = group_by_sql,
#     top = top)
#   dplyr::query(x$src$con, sql, vars)
# }
#
