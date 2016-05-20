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
  rJava::.jinit(jdbc_class_path())
  drv <- rJava::.jnew("net.sourceforge.jtds.jdbc.Driver", check = FALSE)
  rJava::.jcheck(TRUE)
  if (rJava::is.jnull(drv)) drv <- rJava::.jnull()
  new("SQLServerDriver", jdrv = drv)
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
    jc <- rJava::.jcall(drv@jdrv, "Ljava/sql/Connection;", "connect", url,
      rJava::.jnew('java/util/Properties'))
    new("SQLServerConnection", jc = jc)
  }
)

#' @rdname SQLServerDriver-class
#' @export

setMethod('dbGetInfo', 'SQLServerDriver', definition = function (dbObj, ...) {
  list(name = 'RSQLServer (jTDS)',
    # jTDS is a JDBC 3.0 driver. This can be determined by calling the
    # getDriverVersion() method of the JtdsDatabaseMetaData class. But
    # this method isn't defined for Driver class - so hard coded.
    driver.version = "3.0",
    client.version = rJava::.jcall(dbObj@jdrv, "S", "getVersion"),
    # Max connection defined server side rather than by driver.
    max.connections = NA)
})

#' @export
#' @rdname SQLServerDriver-class
setMethod("dbUnloadDriver", "SQLServerDriver", function(drv, ...) TRUE)

# DBI methods inherited from DBI
# dbDriver()
# show()

# Connections ------------------------------------------------------------

#' @rdname SQLServerConnection-class
#' @export

setMethod('dbGetInfo', 'SQLServerConnection',
  definition = function (dbObj, ...) {
    list(
      username = rJava::.jfield(dbObj@jc, "S", "user"),
      host = rJava::.jfield(dbObj@jc, "S", "serverName"),
      port = rJava::.jfield(dbObj@jc, "I", "portNumber"),
      dbname = rJava::.jfield(dbObj@jc, "S", "currentDatabase"),
      db.version = rJava::.jfield(dbObj@jc, "S", "databaseProductVersion")
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
  if (rJava::.jcall(conn@jc, "Z", "isClosed"))  {
    warning("The connection has already been closed")
    FALSE
  } else {
    rJava::.jcall(conn@jc, "V", "close")
    TRUE
  }
})


#' @rdname SQLServerConnection-class
#' @export

setMethod("dbSendQuery", c("SQLServerConnection", "character"),
  def = function (conn, statement, ...) {
    # Notes:
    # 1. Unlike RJDBC, this method does **not** support executing stored procs
    # or precompiled statements as these do not appear to be explicitly
    # supported by any of the rstats-db backends.
    # 2. This method is only responsible for sending SELECT statements which have
    # to return ResultSet objects. To execute data definition or manipulation
    # commands such as CREATE TABLE or UPDATE, use dbExecute instead.
    assertthat::assert_that(assertthat::is.string(statement),
      grepl("^SELECT", statement))
    stat <- rJava::.jcall(conn@jc, "Ljava/sql/Statement;", "createStatement")
    jdbc_exception(stat, "Unable to create simple JDBC statement ", statement)
    jr <- rJava::.jcall(stat, "Ljava/sql/ResultSet;", "executeQuery",
      statement, check = FALSE)
    jdbc_exception(jr, "Unable to retrieve JDBC result set for ", statement)
    md <- rJava::.jcall(jr, "Ljava/sql/ResultSetMetaData;", "getMetaData",
      check = FALSE)
    jdbc_exception(md, "Unable to retrieve JDBC result set meta data for ",
      statement, " in dbSendQuery")
    new("JDBCResult", jr, md, stat, pull = rJava::.jnull())
})

#' Execute non-query SQL commands
#'
#' Methods for this generic will execute non-query (i.e. non SELECT) SQL
#' commands like \code{CREATE TABLE} or \code{UPDATE}. The \code{dbSendUpdate}
#' method which was based on the same method in the \code{RJDBC} package will
#' be deprecated.
#'
#' @param conn a connection inheriting from \code{DBIConnection}
#' @param statement a string denoting the SQL command
#' @return logical value representing whether or not the command has been
#' executed successfully.
#' @examples
#' \dontrun{
#' con <- dbConnect(RSQLServer::SQLServer(), "TEST")
#' dbExecute(con, "CREATE TABLE test (a integer)")
#' }
#' @export

setGeneric("dbExecute", function(conn, statement, ...) {
  standardGeneric("dbExecute")
})

#' @rdname dbExecute
#' @export

setMethod("dbExecute", c("SQLServerConnection", "character"),
  def = function (conn, statement, ...) {
    # Modified from RJDBC
    # https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R#L108
    # See comments to dbSendQuery. dbExecute doesn't support calling stored
    # procedures that do not return results.
    assertthat::assert_that(assertthat::is.string(statement),
      !grepl("^SELECT", statement))
    stat <- rJava::.jcall(conn@jc, "Ljava/sql/Statement;", "createStatement")
    jdbc_exception(stat, "Unable to create JDBC statement ", statement)
    # In theory following is not necesary since 's' will go away and be
    # collected, but apparently it may be too late for Oracle (ORA-01000)
    on.exit(rJava::.jcall(stat, "V", "close"))
    rJava::.jcall(stat, "I", "executeUpdate", statement, check = FALSE)
    x <- rJava::.jgetEx(TRUE)
    if (!rJava::is.jnull(x)) {
      stop("execute JDBC update query failed in dbExecute (",
        rJava::.jcall(x, "S", "getMessage"), ")")
    }
  }
)

#' @rdname dbExecute
#' @export
dbSendUpdate <- function (conn, statement, ...) {
  .Deprecated("dbExecute")
  dbExecute(conn, statement)
}

#' @rdname SQLServerConnection-class
#' @export
setMethod("dbDataType", c("SQLServerConnection", "ANY"),
  def = function (dbObj, obj, ...) {
    # GOING FROM R data type to SQL Server data type
    # http://jtds.sourceforge.net/typemap.html
    # https://msdn.microsoft.com/en-us/library/ms187752.a spx
    # https://msdn.microsoft.com/en-us/library/ms187752(v=sql.90).aspx

    #### Helper functions
    char_type <- function (x) {
      n <- max(nchar(as.character(x)))
      if (n <= 8000) {
        paste0("VARCHAR(", n, ")")
      } else {
        "TEXT"
      }
    }

    binary_type <- function (x) {
      # SQL Server 2000 does not support varbinary(max) type.
      if (dbGetInfo(dbObj)$db.version < 9) {
        # https://technet.microsoft.com/en-us/library/aa225972(v=sql.80).aspx
        "VARBINARY(8000)"
      } else {
        "VARBINARY(MAX)"
      }
    }

    date_type <- function (x) {
      if (dbGetInfo(dbObj)$db.version < 10) {
        # DATE available in >= SQL Server 2008 (>= v.10)
        "DATETIME"
      } else {
        "DATE"
      }
    }
    ####

    if (is.factor(obj)) return(char_type(obj))
    if (inherits(obj, "POSIXct")) return("DATETIME")
    if (inherits(obj, "Date")) return(date_type(obj))

    switch(typeof(obj),
      logical = "BIT",
      integer = "INT",
      double = "FLOAT",
      character = char_type(obj),
      list = binary_type(obj),
      stop("Unsupported type", call. = FALSE)
    )
  }
)

#' @rdname SQLServerConnection-class
#' @export
setMethod("dbListTables", "SQLServerConnection", function(conn, ...) {
  # Modified from RJDBC:
  # https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R#L161
  md <- rJava::.jcall(conn@jc, "Ljava/sql/DatabaseMetaData;", "getMetaData",
    check = FALSE)
  jdbc_exception(md, "Unable to retrieve JDBC database metadata")
  # Create arguments for call to getTables
  jns <- rJava::.jnull("java/lang/String")
  table_types <- rJava::.jarray(c("TABLE", "VIEW"))
  rs <- rJava::.jcall(md, "Ljava/sql/ResultSet;", "getTables",
    jns, jns, jns, table_types, check = FALSE)
  jdbc_exception(rs, "Unable to retrieve JDBC tables list")
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
  all(name %in% dbListTables(conn))
})

#' @rdname SQLServerConnection-class
#' @export
setMethod("dbWriteTable", "SQLServerConnection",
  function (conn, name, value, overwrite = TRUE, append = FALSE) {

    # Based on RJDBC method:
    # https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R#L242
    # However require `value` to be a data frame. No coercion will take place
    assertthat::assert_that(is.data.frame(value), ncol(value) > 0,
      !(overwrite && append))

    # Capture whether auto-commit is enabled on connection
    # If it is, disable until this function exists, after which it should be
    # enabled again. Should be disabled to ensure all SQL statements are
    # committed together rather than separately.
    # http://docs.oracle.com/javase/tutorial/jdbc/basics/transactions.html
    ac <- rJava::.jcall(conn@jc, "Z", "getAutoCommit")

    # Check whether table already exists and needs to be dropped.
    overwrite <- isTRUE(as.logical(overwrite))
    append <- if (overwrite) FALSE else isTRUE(as.logical(append))
    if (dbExistsTable(conn, name)) {
      msg <- paste0("Table '", name, "' already exists")
      if (overwrite) dbRemoveTable(conn, name) else if (!append) stop(msg)
    } else if (append) {
      stop("Cannot append to a non-existing table '", name, "'")
    }
    # Set auto-commit state on exit if necessary.
    if (ac) {
      rJava::.jcall(conn@jc, "V", "setAutoCommit", FALSE)
      on.exit(rJava::.jcall(conn@jc, "V", "setAutoCommit", ac))
    }
    # Create / append new table
    fts <- vapply(value, dbDataType, "character", dbObj=conn, USE.NAMES = FALSE)
    fdef <- paste(ident(names(value)), fts, collapse = ', ')
    qname <- ident(name)
    if (!append) {
      ct <- paste("CREATE TABLE ", qname, " (", fdef, ")", sep= '')
      dbExecute(conn, ct)
    }
    if (length(value[[1]])) {
      # Use Prepared Statement.
      inss <- paste("INSERT INTO ", qname, " VALUES(",
        paste(rep("?", length(value)), collapse = ','), ")", sep = '')
      for (j in 1:length(value[[1]])) {
        dbExecute(conn, inss, list = as.list(value[j, ]))
      }
    }
    if (ac) dbCommit(conn)
  })


# Inherited from DBI:
# show()
# dbQuoteString()
# dbQuoteIdentifier()

# DBI methods that inherit from RJDBC:
# dbGetException()
# dbListResults()
# dbListFields()
# dbListTables()
# dbReadTable()
# dbExistsTable()
# dbRemoveTable()
# dbCommit()
# dbRollback()

# Results ----------------------------------------------------------------

#' @rdname SQLServerResult-class
#' @export
setMethod ('dbIsValid', 'SQLServerResult', function (dbObj) {
  rJava::.jcall(dbObj@jr, "Z", "isClosed")
})

#' @rdname SQLServerResult-class
#' @export
setMethod("fetch", c("SQLServerResult", "numeric"),
  def = function (res, n, ...) {
    # Needed because dplyr's Query class calls the S4 fetch method when it calls
    # its R6 fetch method. See:
    # https://github.com/hadley/dplyr/blob/db2f59ce3a0732c81a4fde2b60b06c048eaf1291/R/query.r#L44
    df <- callNextMethod()

    ####
    # RJDBC translates SQL Server fields to numeric and character vectors only.
    # This means that for eg, date fields types are represented by character
    # vectors. A bit of post-processing will be good. At some point should
    # file a bug report to RJDBC about this.
    ####
    # Assume that RJDBC doesn't change column order in fetching result
    # First find JDBC column types and turn them into R types
    rcts <- jdbcToRType(jdbcColumnTypes(res@md))
    # Check which columns need conversion
    df_cts <- vapply(df, class, "character", USE.NAMES = FALSE)
    to_convert <- rcts != df_cts
    # Conversion time
    if (any(to_convert)) {
      cnames <- colnames(df)
      names(rcts) <- cnames
      for (cname in cnames[to_convert]) {
        # special case for bit columns,
        # which become character vectors of "0" and "1"
        if (rcts[cname] == "logical") {
          df[, cname] <- as.logical(as.numeric(df[, cname]))
        } else {
          f <- paste0("as.", unname(rcts[cname]))
          df[, cname] <- eval(call(f, df[, cname]))
        }
      }
    }
    df
})

#' @rdname SQLServerResult-class
#' @export
setMethod("dbFetch", c("SQLServerResult", "numeric"), function(res, n, ...) {
  # Per DBI documentation:
  # "fetch is provided for compatibility with older DBI clients - for all new
  # code you are strongly encouraged to use dbFetch."
  # RJDBC does not currently have a dbFetch method.
  fetch(res, n, ...)
})

#' @rdname SQLServerResult-class
#' @export
setMethod("dbGetInfo", "SQLServerResult", def = function (dbObj, ...) {
  list(statement = dbObj@stat,
    row.count = rJava::.jcall(dbObj@jr, "I", "getRow"),
    rows.affected = rJava::.jcall(dbObj@jr, "I", "getFetchSize"),
    # http://docs.oracle.com/javase/7/docs/api/java/sql/ResultSet.html#isAfterLast()
    has.completed = rJava::.jcall(dbObj@jr, "Z", "isAfterLast"),
    # No JDBC method is available that determines whether statement is a
    # SELECT
    is.select = NA)
})

#' @rdname SQLServerResult-class
#' @importFrom dplyr data_frame
#' @export
setMethod("dbColumnInfo", "SQLServerResult", def = function (res, ...) {
  # Inspired by RJDBC method for JDBCResult
  # https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R
  cols <- rJava::.jcall(res@md, "I", "getColumnCount")
  df <- data_frame(field.name = character(),
    field.type = character(),
    data.type = character())
  if (cols < 1) return(df)
  for (i in 1:cols) {
    df$field.name[i] <- rJava::.jcall(res@md, "S", "getColumnName", i)
    df$field.type[i] <- rJava::.jcall(res@md, "S", "getColumnTypeName", i)
    ct <- rJava::.jcall(res@md, "I", "getColumnType", i)
    df$data.type[i] <- jdbcToRType(ct)
  }
  df
})

#' @rdname SQLServerResult-class
#' @export
setMethod("dbHasCompleted", "SQLServerResult", def = function (res, ...) {
  # Need to override RJDBC method as it always returns TRUE
  dbGetInfo(res)$has.completed
})

#' @rdname SQLServerResult-class
#' @export
setMethod("dbClearResult", "SQLServerResult", function (res, ...) {
  # Need to overwrite RJDBC supplied method to pass DBItest. Needs to throw
  # warning if calling this method on cleared resultset
  if (rJava::.jcall(res@jr, "Z", "isClosed")) {
    warning("ResultSet has already been cleared", call. = FALSE)
  } else {
    rJava::.jcall(res@jr, "V", "close")
  }
  if (rJava::.jcall(res@stat, "Z", "isClosed")) {
    warning("Statement has already been cleared", call. = FALSE)
  } else {
    rJava::.jcall(res@stat, "V", "close")
  }
  TRUE
})

# Inherited from DBI:
# show()
# dbGetStatement()
# dbGetRowsAffected()
# dbGetRowCount()
#
# Inherited from RJDBC:
# fetch()
# dbGetInfo()

# Other ----------------------------------------------------------------

jdbc_exception <- function (object, ...) {
  # Based on RJDBC .verify.JDBC.result()
  # https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R#L18
  if (rJava::is.jnull(object)) {
    x <- rJava::.jgetEx(TRUE)
    if (rJava::is.jnull(x))
      stop(...)
    else
      stop(..., " (", rJava::.jcall(x, "S", "getMessage"), ")")
  }
}
