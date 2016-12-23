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
#' @importFrom dplyr src_sql
#' @export
src_sqlserver <- function (server, file = NULL, database = "",
  type = "sqlserver", port = "", properties = list()) {
  con <- dbConnect(SQLServer(), server, file, database , type, port, properties)
  info <- dbGetInfo(con)
  src_sql("sqlserver", con, info = info)
}

#' @importFrom dplyr src_desc
#' @export
src_desc.src_sqlserver <- function (x) {
  info <- x$info
  paste0("SQLServer ", info$db.version, " [", info$username, "@",
    info$host, ":", info$port, "/", info$dbname, "]")
}

#' @importFrom dplyr tbl tbl_sql
#' @export
tbl.src_sqlserver <- function (src, from, ...) {
  tbl_sql("sqlserver", src = src, from = from, ...)
}

#' @importFrom dplyr copy_to db_data_type con_acquire con_release
#' @export

copy_to.src_sqlserver <- function (dest, df, name = NULL, types = NULL,
  temporary = TRUE, unique_indexes = NULL, indexes = NULL, ...) {
  # Modified version of dplyr method:
  # https://github.com/hadley/dplyr/blob/36687792b349bfeca24c69177cfb74b7fee341c6/R/tbl-sql.r#L329
  # Modification necessary because temporary tables in SQL Server are
  # prefixed by `#` and so db_insert_into() needs to pick up this name from
  # db_create_table() whereas this isn't necessary in dplyr method.
  assertthat::assert_that(is.data.frame(df),
    is.null(name) || assertthat::is.string(name),
    assertthat::is.flag(temporary))

  name <- name %||% deparse(substitute(df))
  class(df) <- "data.frame" # avoid S4 dispatch problem in dbSendPreparedQuery

  con <- con_acquire(dest)
  tryCatch({
    if (isTRUE(db_has_table(con, name))) {
      stop("Table ", name, " already exists.", call. = FALSE)
    }
    types <- types %||% db_data_type(con, df)
    names(types) <- names(df)
    name <- db_create_table(con, name, types, temporary = temporary)
    db_insert_into(con, name, df, temporary = temporary)
    db_create_indexes(con, name, unique_indexes, unique = TRUE)
    db_create_indexes(con, name, indexes, unique = FALSE)
    # SQL Server doesn't have ANALYZE TABLE support so this part of
    # copy_to.src_sql has been dropped
  }, finally = {
    con_release(dest, con)
  })

  tbl(dest, name)
}


#' @importFrom dplyr compute op_vars select_ sql_render tbl group_by_ groups
#' @importFrom dplyr db_create_indexes %>%
#' @export
compute.tbl_sqlserver <- function(x, name = random_table_name(), temporary = TRUE,
  unique_indexes = list(), indexes = list(), ...) {

  # Based on dplyr:
  # https://github.com/hadley/dplyr/blob/284b91d7357cd3c184843bf9206d19cc8c0cd0e3/R/tbl-sql.r#L364
  # Modified because db_save_query returns a temp table name which must be used
  # by subsequent method calls.

  if (!is.list(indexes)) {
    indexes <- as.list(indexes)
  }
  if (!is.list(unique_indexes)) {
    unique_indexes <- as.list(unique_indexes)
  }

  con <- con_acquire(x$src)
  tryCatch({
    vars <- op_vars(x)
    assertthat::assert_that(all(unlist(indexes) %in% vars))
    assertthat::assert_that(all(unlist(unique_indexes) %in% vars))
    x_aliased <- select_(x, .dots = vars) # avoids problems with SQLite quoting (#1754)
    name <- db_save_query(con, sql_render(x_aliased),
      name = name, temporary = temporary)
    db_create_indexes(con, name, unique_indexes, unique = TRUE)
    db_create_indexes(con, name, indexes, unique = FALSE)
  }, finally = {
    con_release(x$src, con)
  })

  tbl(x$src, name) %>% group_by_(.dots = groups(x))
}
