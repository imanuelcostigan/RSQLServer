#' @include dbi-classes.R
NULL

# Drivers ----------------------------------------------------------------

#' SQLServerDriver class and methods
#'
#' \code{SQLServer()} creates a \code{SQLServerDriver} object and is based on
#' the jTDS driver while \code{dbConnect()} provides a convenient interface to
#' connecting to a SQL Server database using this driver.
#'
#' @references
#' \href{http://jtds.sourceforge.net/doc/net/sourceforge/jtds/jdbc/Driver.html}{jTDS API doc for Driver class}
#' @examples
#' \dontrun{
#' SQLServer()
#' }
#' @rdname SQLServer
#' @export

SQLServer <- function () {
  new("SQLServerDriver", jdrv = start_driver())
}


#' @param drv An objected of class \code{\linkS4class{SQLServerDriver}}, or an
#'   existing \code{\linkS4class{SQLServerConnection}}. If a connection, the
#'   connection will be cloned.
#' @template sqlserver-parameters
#' @return \code{SQLServer()} returns an object of class
#'   \code{SQLServerDriver}; \code{dbConnect()} returns a
#'   \code{\linkS4class{SQLServerConnection}} object.
#' @examples
#' # View sql.yaml file bundled in package
#' file <- system.file("extdata", "sql.yaml", package = "RSQLServer")
#' readLines(file)
#' # Connect using ~/sql.yaml file
#' \dontrun{
#' if (have_test_server()) {
#'  dbConnect(RSQLServer::SQLServer(), "TEST")
#' }
#' # Example where ~/sql.yaml does not exist or where the server
#' # is not in the YAML file.
#' dbConnect(RSQLServer::SQLServer(), server="11.1.111.11", port=1434,
#'    properties=list(useNTLMv2="true", domain="myco", user="me",
#'      password="asecret"))
#' }
#' @rdname SQLServer
#' @export

setMethod('dbConnect', "SQLServerDriver",
  definition = function (drv, server, file = NULL, database = NULL,
    type = NULL, port = NULL, properties = NULL) {

    # Set default values for arguments
    file <- file %||% file.path(Sys.getenv("HOME"), "sql.yaml")
    database <- database %||% ""
    type <- type %||% "sqlserver"
    port <- port %||% ""
    properties <- properties %||% list()

    # Use sql.yaml file if file is not missing. If so, then the paramaters
    # type, port and connection properties will be ignored and the
    # information in sql.yaml will be used instead.
    if (file.exists(file)) {
      sd <- get_server_details(server, file)
    } else {
      sd <- NULL
    }
    # Server details must include type and port otherwise get_server_file fails
    if (!is.null(sd)) {
      server <- sd$server
      sd$server <- NULL
      type <- sd$type
      sd$type <- NULL
      port <- sd$port
      sd$port <- NULL
      properties <- sd
    }
    url <- jtds_url(server, type, port, database, properties)
    new("SQLServerConnection", jc = new_connection(drv, url))
  }
)

#' @rdname SQLServerDriver-class
#' @export

setMethod('dbGetInfo', 'SQLServerDriver', definition = function (dbObj, ...) {
  list(name = 'RSQLServer (jTDS)',
    # jTDS is a JDBC 3.0 driver. This can be determined by calling the
    # getDriverVersion() method of the JtdsDatabaseMetaData class. But
    # this method isn't defined for Driver class - so hard coded.
    driver.version = numeric_version("3.0"),
    client.version = driver_version(dbObj),
    # Max connection defined server side rather than by driver.
    max.connections = NA)
})

#' @rdname SQLServerDriver-class
#' @export
setMethod("dbUnloadDriver", "SQLServerDriver", function(drv, ...) TRUE)

#' @rdname SQLServerDriver-class
#' @export
setMethod("dbIsValid", "SQLServerDriver", function(dbObj, ...) TRUE)


# DBI methods inherited from DBI
# dbDriver()
# show()

# Connections ------------------------------------------------------------

#' @rdname SQLServerConnection-class
#' @export

setMethod('dbGetInfo', 'SQLServerConnection',
  definition = function (dbObj, ...) {
    list(
      username   = connection_info(dbObj, "username"),
      host       = connection_info(dbObj, "host"),
      port       = connection_info(dbObj, "port"),
      dbname     = connection_info(dbObj, "dbname"),
      db.version = connection_info(dbObj, "db.version")
    )
  }
)

#' @rdname SQLServerConnection-class
#' @export

setMethod('dbIsValid', 'SQLServerConnection', function (dbObj, ...) {
  !rJava::.jcall(dbObj@jc, "Z", "isClosed")
})

#' @rdname SQLServerConnection-class
#' @export

setMethod("dbDisconnect", "SQLServerConnection", function (conn, ...) {
  if (!dbIsValid(conn))  {
    warning("The connection has already been closed")
    FALSE
  } else {
    close_connection(conn)
    TRUE
  }
})

#' @rdname SQLServerConnection-class
#' @export

setMethod("dbSendQuery", c("SQLServerConnection", "character"),
  def = function (conn, statement, params = NULL, ...) {
    # Notes:
    # 1. Unlike RJDBC, this method does **not** support executing stored procs
    # or precompiled statements as these do not appear to be explicitly
    # supported by any of the rstats-db backends.
    # 2. This method is only responsible for sending SELECT statements which have
    # to return ResultSet objects. To execute data definition or manipulation
    # commands such as CREATE TABLE or UPDATE, use dbExecute instead.
    assertthat::assert_that(assertthat::is.string(statement))
    ps <- create_prepared_statement(conn, statement)
    catch_exception(ps, "Unable to create prepared statement ", statement)
    pre_res <- new("SQLServerPreResult", stat = ps)
    # If parameterised and don't have params supplied, create a skeleton
    # Result otherwise, create full thing
    if (is_parameterised(ps) && is.null(params)) {
      return(pre_res)
    } else {
      if (is_parameterised(ps) && !is.null(params)) {
        dbBind(pre_res, params)
      }
      jr <- execute_query(pre_res@stat)
      catch_exception(jr, "Unable to retrieve result set for ", statement)
      md <- rs_metadata(jr, FALSE)
      catch_exception(md, "Unable to retrieve result set meta data for ",
        statement, " in dbSendQuery")
      return(new("SQLServerResult", pre_res, jr = jr, md = md))
    }
})

#' @rdname SQLServerConnection-class
#' @export

setMethod("dbSendStatement", c("SQLServerConnection", "character"),
  function(conn, statement, params = NULL, ...) {
    assertthat::assert_that(assertthat::is.string(statement),
      is.null(params) || is.list(params))
    if(!is.null(params)) {
      s <- create_prepared_statement(conn, statement)
      catch_exception(s, "Unable to create prepared statement ", statement)
      on.exit(close_statement(s))
      ps_bind_all(params, s)
      res <- execute_update(s, check = TRUE)
    } else {
      s <- create_statement(conn)
      catch_exception(s, "Unable to create statement ", statement)
      on.exit(close_statement(s))
      res <- execute_update(s, statement, check = TRUE)
    }
    res <- dbSendQuery(conn, paste("SELECT", res, "AS ROWS_AFFECTED"))
    new("SQLServerUpdateResult", res)
})

#' @rdname SQLServerConnection-class
#' @export

dbSendUpdate <- function (conn, statement, ...) {
  .Deprecated("dbExecute")
  dbExecute(conn, statement)
}

#' @rdname SQLServerConnection-class
#' @export
setMethod("dbReadTable", c("SQLServerConnection", "character"),
  function(conn, name, ...) {
    sql <- paste("SELECT * FROM", dbQuoteIdentifier(conn, name))
    dbGetQuery(conn, sql)
})

#' @rdname SQLServerConnection-class
#' @export
setMethod("dbDataType", c("SQLServerConnection", "ANY"),
  def = function (dbObj, obj, ...) {
    # GOING FROM R data type to SQL Server data type
    # http://jtds.sourceforge.net/typemap.html
    # https://msdn.microsoft.com/en-us/library/ms187752.aspx
    # https://msdn.microsoft.com/en-us/library/ms187752(v=sql.90).aspx

    if (is.factor(obj)) return(char_type(obj, dbObj))
    if (inherits(obj, "POSIXct")) return("DATETIME")
    if (inherits(obj, "Date")) return(date_type(obj, dbObj))

    switch(typeof(obj),
      logical = "BIT",
      integer = "INT",
      double = "FLOAT",
      raw = binary_type(obj, dbObj),
      character = char_type(obj, dbObj),
      list = binary_type(obj, dbObj),
      stop("Unsupported type", call. = FALSE)
    )
  }
)

#' @rdname SQLServerConnection-class
#' @export
setMethod("dbListTables", "SQLServerConnection",
  function(conn, pattern = "%", ...) {
  # Modified from RJDBC:
  # https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R#L161
  md <- rJava::.jcall(conn@jc, "Ljava/sql/DatabaseMetaData;", "getMetaData",
    check = FALSE)
  catch_exception(md, "Unable to retrieve JDBC database metadata")
  # Create arguments for call to getTables
  jns <- rJava::.jnull("java/lang/String")
  table_types <- rJava::.jarray(c("TABLE", "VIEW"))
  rs <- rJava::.jcall(md, "Ljava/sql/ResultSet;", "getTables",
    jns, jns, pattern, table_types, check = FALSE)
  catch_exception(rs, "Unable to retrieve JDBC tables list")
  on.exit(rJava::.jcall(rs, "V", "close"))
  tbls <- character()
  while (rJava::.jcall(rs, "Z", "next")) {
    schema <- rJava::.jcall(rs, "S", "getString", "TABLE_SCHEM")
    sys_schemas <- c("sys", "INFORMATION_SCHEMA")
    if (!(schema %in% sys_schemas)) {
      tbls <- c(tbls, rJava::.jcall(rs, "S", "getString", "TABLE_NAME"))
    }
  }
  tbls
})

#' @rdname SQLServerConnection-class
#' @export
setMethod("dbExistsTable", "SQLServerConnection", function (conn, name, ...) {
  length(dbListTables(conn, name)) > 0
})

#' @rdname SQLServerConnection-class
#' @export
setMethod("dbRemoveTable", "SQLServerConnection", function (conn, name, ...) {
  assertthat::is.number(dbExecute(conn,
    paste0("DROP TABLE ", dbQuoteIdentifier(conn, name))))
})

#' @rdname SQLServerConnection-class
#' @export
setMethod("dbBegin", "SQLServerConnection", function (conn, ...) {
  rJava::.jcall(conn@jc, "V", "setAutoCommit", FALSE)
  TRUE
})

#' @rdname SQLServerConnection-class
#' @export
setMethod("dbCommit", "SQLServerConnection", function (conn, ...) {
  rJava::.jcall(conn@jc, "V", "commit")
  TRUE
})

#' @rdname SQLServerConnection-class
#' @export
setMethod("dbRollback", "SQLServerConnection", function (conn, ...) {
  rJava::.jcall(conn@jc, "V", "rollback")
  TRUE
})


#' @rdname SQLServerConnection-class
#' @export
setMethod("dbWriteTable", "SQLServerConnection",
  function (conn, name, value, overwrite = TRUE, append = FALSE) {

    assertthat::assert_that(is.data.frame(value), ncol(value) > 0,
      !(overwrite && append))

    dbBegin(conn)
    on.exit(dbRollback(conn))

    found <- dbExistsTable(conn, name)
    temp <- grepl("^#", name)

    if (found && !append && !overwrite) {
      stop("The table ", name, " exists but you are not overwriting or ",
        "appending to this table.", call. = FALSE)
    }

    if (!found && append) {
      stop("The table ", name, " does not exist but you are trying to ",
        "append data to it.")
    }

    if (temp && append) {
      stop("Appending to a temporary table is unsupported.")
    }

    name <- dbQuoteIdentifier(conn, name)

    # NB: if table "name" does not exist, having "overwrite" set to TRUE does
    # not cause problems, so no need for error handling in this case.
    # Let server backend handle case when temp table exists but overwriting it

    if ((found || temp) && overwrite) {
      dbRemoveTable(conn, name)
    }

    if (!found || temp || overwrite) {
      dbExecute(conn, sqlCreateTable(conn, name, value))
    }

    if (nrow(value) > 0) {
      sql <- sqlAppendTableTemplate(conn, name, value)
      for (j in seq_along(value[[1]])) {
        dbExecute(conn, sql, params = as.list(value[j, ]))
      }
    }

    # Overwrite on.exist(dbRollback(conn)) call above as now guaranteed
    # commit success
    on.exit(NULL)
    dbCommit(conn)
    TRUE
})

setMethod("dbListFields", "SQLServerConnection", function(conn, name, ...) {
  # Using MSFT recommendation linked here:
  # https://github.com/imanuelcostigan/RSQLServer/issues/23
  qry <- paste0("SELECT TOP 0 * FROM ", dbQuoteIdentifier(conn, name))
  rs <- dbSendQuery(conn, qry)
  on.exit(dbClearResult(rs))
  jdbcColumnNames(rs@md)
})

setMethod("dbListResults", "SQLServerConnection", function(conn, ...) {
  # https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R#L150
  warning("JDBC does not maintain a list of active results.")
  NULL
})

setMethod("dbGetException", "SQLServerConnection", def = function(conn, ...) {
  # https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R#L143
  # Function to be deprecated from DBI: https://github.com/rstats-db/DBI/issues/51
  list()
})


# Inherited from DBI:
# show()
# dbQuoteString()
# dbQuoteIdentifier()


# Results ----------------------------------------------------------------

#' @rdname SQLServerResult-class
#' @export
setMethod ('dbIsValid', 'SQLServerResult', function (dbObj) {
  !rJava::.jcall(dbObj@jr, "Z", "isClosed")
})

#' @rdname SQLServerResult-class
#' @export
setMethod("dbFetch", c("SQLServerPreResult", "numeric"),
  def = function (res, n, ...) {
    fetch(res, n, ...)
})

#' @rdname SQLServerResult-class
#' @export
setMethod("fetch", c("SQLServerPreResult", "numeric"),
  function(res, n, ...) {
    jr <- execute_query(res@stat)
    catch_exception(jr, "Unable to retrieve result set for ", res@stat)
    md <- rs_metadata(jr, FALSE)
    catch_exception(md, "Unable to retrieve result set meta data for ",
      res@stat, " in dbSendQuery")
    fetch(new("SQLServerResult", res, jr = jr, md = md), n)
})

#' @rdname SQLServerResult-class
#' @export
setMethod("dbBind", "SQLServerPreResult", function(res, params, ...) {
  rs_bind_all(params, res)
})

#' @rdname SQLServerResult-class
#' @export
setMethod("dbFetch", c("SQLServerResult", "numeric"),
  def = function (res, n, ...) {
    fetch(res, n, ...)
})

#' @rdname SQLServerResult-class
#' @export
setMethod("fetch", c("SQLServerResult", "numeric"),
  function(res, n = -1, block = 2048L, ...) {
    # Based on:
    # https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R#L287
    assertthat::assert_that(assertthat::is.count(block))

    ###### Initialise JVM side cache of results
    rp <- res@pull
    rJava::.jaddClassPath(pull_class_path())
    if (rJava::is.jnull(rp)) {
      rp <- rJava::.jnew("com/github/RSQLServer/MSSQLResultPull",
        rJava::.jcast(res@jr, "java/sql/ResultSet"))
      catch_exception(rp, "cannot instantiate MSSQLResultPull helper class")
    }

    ###### Build list that will store data and be coerced into data frame
    # Field type integers are defined in MSSQLResultPull class
    # constant ints CT_STRING, CT_NUMERIC and CT_INT where:
    # 0L - string
    # 1L - double
    # 2L - integer
    # 3L - date
    # 4L - datetime/timestamp
    ctypes <- rJava::.jcall(rp, "[I", "mapColumns")
    cnames <- rJava::.jcall(rp, "[S", "columnNames")
    ctypes_r <- rp_to_r_type_map(ctypes)
    out <- create_empty_lst(ctypes_r, cnames)

    ###### Fetch into cache and pull from cache into R
    if (n < 0L) { ## infinite pull
      # stride - set capacity of cache
      # block - set fetch size which gives driver hint as to # rows to be fetched
      stride <- 32768L  ## start fairly small to support tiny queries and increase later
      while ((nrec <- rJava::.jcall(rp, "I", "fetch", stride, block)) > 0L) {
        out <- fetch_rp(rp, out, ctypes)
        if (nrec < stride) break
        stride <- 524288L # 512k
      }
    }
    if (n > 0L) {
      nrec <- rJava::.jcall(rp, "I", "fetch", as.integer(n), block)
      out <- fetch_rp(rp, out, ctypes)
    }
    # n = 0 is taken care of by creation of `out` list variable above.
    if (length(out[[1]]) > 0) {
      out <- purrr::map_if(out, ctypes == 3L, as.Date,
        format = "%Y-%m-%d")
      out <- purrr::map_if(out, ctypes == 4L, as.POSIXct,
        tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")
      out <- purrr::map_if(out, ctypes == 5L, as.logical)
    }
    # as.data.frame is expensive - create it on the fly from the list
    attr(out, "row.names") <- c(NA_integer_, length(out[[1]]))
    class(out) <- "data.frame"
    out
})

#' @rdname SQLServerResult-class
#' @importFrom dplyr bind_rows data_frame
#' @export
setMethod("dbColumnInfo", "SQLServerResult", def = function (res, ...) {
  # Inspired by RJDBC method for JDBCResult
  # https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R
  cols <- rJava::.jcall(res@md, "I", "getColumnCount")
  if (cols < 1L) {
    df <- data_frame(field.name = character(),
                     field.type = character(),
                     data.type = character())
  } else {
    df <- dplyr::bind_rows(
      lapply(seq_len(cols), function(i) {
        list(field.name = rJava::.jcall(res@md, "S", "getColumnName", i),
             field.type = rJava::.jcall(res@md, "S", "getColumnTypeName", i),
             data.type = jdbcToRType(rJava::.jcall(res@md, "I", "getColumnType", i)))
      })
    )
  }
  df
})

#' @rdname SQLServerResult-class
#' @export
setMethod("dbHasCompleted", "SQLServerResult", def = function (res, ...) {
  # Need to override RJDBC method as it always returns TRUE
  rJava::.jcall(res@jr, "Z", "isAfterLast")
})

#' @rdname SQLServerResult-class
#' @export
setMethod("dbClearResult", "SQLServerResult", function (res, ...) {
  # Need to overwrite RJDBC supplied method to pass DBItest. Needs to throw
  # warning if calling this method on cleared resultset
  if (!dbIsValid(res)) {
    warning("ResultSet has already been cleared", call. = FALSE)
  } else {
    rJava::.jcall(res@jr, "V", "close")
  }
  if (rJava::.jcall(res@stat, "Z", "isClosed")) {
    warning("Statement has already been cleared", call. = FALSE)
  } else {
    close_statement(res@stat)
  }
  TRUE
})

#' @rdname SQLServerResult-class
#' @export
setMethod("dbGetStatement", "SQLServerResult", function(res, ...) {
  rJava::.jcall(res@stat, "S", "toString")
})

#' @rdname SQLServerResult-class
#' @export
setMethod("dbGetRowCount", "SQLServerResult", function(res, ...) {
  rJava::.jcall(res@jr, "I", "getRow")
})

#' @rdname SQLServerResult-class
#' @export
setMethod("dbGetRowsAffected", "SQLServerResult", function(res, ...) {
  rJava::.jcall(res@jr, "I", "getFetchSize")
})

#' @rdname SQLServerResult-class
#' @export
setMethod("dbGetRowsAffected", "SQLServerUpdateResult", function(res, ...) {
  fetch(res, -1)$ROWS_AFFECTED
})

#' @rdname SQLServerResult-class
#' @export
setMethod("dbHasCompleted", "SQLServerResult", function(res, ...) {
  # http://docs.oracle.com/javase/7/docs/api/java/sql/ResultSet.html#isAfterLast()
  # If res is empty (row = 0), isAfterLast() will return FALSE, but
  # dbGetRowCount will return 0L.
  rJava::.jcall(res@jr, "Z", "isAfterLast") || dbGetRowCount(res) == 0L
})

# Inherited from DBI:
# show()
# dbGetInfo()
