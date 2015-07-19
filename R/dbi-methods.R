#' @include dbi-classes.R
NULL

# Drivers ----------------------------------------------------------------

#' @param  dbObj a \code{\linkS4class{SQLServerDriver}} object
#' @param ... Other arguments to methods.
#' @rdname SQLServerDriver-class
#' @export

setMethod('dbGetInfo', 'SQLServerDriver', definition = function (dbObj, ...) {
  list(name = 'RSQLServer (jTDS)',
    driver.version = rJava::.jcall(dbObj@jdrv, "S", "getVersion"))
})

#' @param  object a \code{\linkS4class{SQLServerDriver}} object
#' @rdname SQLServerDriver-class
#' @export

setMethod("show", "SQLServerDriver", definition = function (object) {
  cat("<SQLServerDriver>\n")
})

#' Connect to/disconnect from a SQL Server database.
#'
#' @param drv An objected of class \code{\linkS4class{SQLServerDriver}}, or an
#' existing \code{\linkS4class{SQLServerConnection}}. If a connection,
#' the connection will be cloned.
#' @template sqlserver-parameters
#' @return a \code{\linkS4class{SQLServerConnection}}
#' @examples
#' # View sql.yaml file bundled in package
#' file <- system.file("extdata", "sql.yaml", package = "RSQLServer")
#' readLines(file)
#' # Connect using ~/sql.yaml file
#' if (have_test_server()) {
#'  dbConnect(RSQLServer::SQLServer(), "TEST")
#' }
#' # Don't use file argument:
#' \dontrun{
#' dbConnect(RSQLServer::SQLServer(), server="11.1.111.11", port=1434,
#'    properties=list(useNTLMv2="true", domain="myco", user="me",
#'      password="asecret"))
#' }
#' @rdname SQLServer
#' @export

setMethod('dbConnect', "SQLServerDriver",
  definition = function (drv, server, file = NULL, database = "",
    type = "sqlserver", port = "", properties = list()) {
    # Use sql.yaml file if file is not missing. Note this will then ignore
    # the paramaters type, port and connection properties will be ignored and the
    # information in sql.yaml will be used instead
    sd <- get_server_details(server, file)
    if (!identical(sd, list())) {
      # Server details must include type and port otherwise get_server_file
      # fails
      server <- sd$server
      sd$server <- NULL
      type <- sd$type
      sd$type <- NULL
      port <- sd$port
      sd$port <- NULL
      properties <- sd
    }
    url <- jtds_url(server, type, port, database, properties)
    properties <- rJava::.jnew('java/util/Properties')
    jc <- rJava::.jcall(drv@jdrv, "Ljava/sql/Connection;", "connect", url,
      properties)
    new("SQLServerConnection", jc = jc, identifier.quote = drv@identifier.quote)
  }
)

# DBI methods inherited from DBI
# dbDriver()
#
# DBI methods inherited from RJDBC
# dbUnloadDriver()


# Connections ------------------------------------------------------------

#' @rdname SQLServerConnection-class
#' @export

setMethod('dbGetInfo', 'SQLServerConnection',
  definition = function (dbObj, ...) {
    meta <- rJava::.jcall(dbObj@jc, "Ljava/sql/DatabaseMetaData;",
      "getMetaData")
    list(db.product.name = rJava::.jcall(meta, "S", "getDatabaseProductName"),
      db.version = rJava::.jcall(meta, "I", "getDatabaseMajorVersion"),
      user = rJava::.jcall(meta, "S","getUserName"))
  }
)

#' @param  object a \code{\linkS4class{SQLServerConnection}} object
#' @rdname SQLServerConnection-class
#' @export

setMethod("show", "SQLServerConnection", definition = function (object) {
  info <- dbGetInfo(object)
  cat("<SQLServerConnection>\n")
  cat(info$db.product.name, " ", info$db.version, "\n", sep = "")
  if (!dbIsValid(object)) {
    cat("  DISCONNECTED\n")
  }
})

#' @rdname SQLServerConnection-class
#' @export

setMethod('dbIsValid', 'SQLServerConnection', function (dbObj, ...) {
  !rJava::.jcall(dbObj@jc, "Z", "isClosed")
})

# Inherited from DBI:
# dbQuoteString()
# dbQuoteIdentifier()

#' Send query to SQL Server
#'
#' This is basically a copy of RJDBC's \code{\link[RJDBC:JDBCConnection-methods]{dbSendQuery}}
#' method for JDBCConnection, except that this returns a
#' \code{\linkS4class{SQLServerResult}} rather than a JDBCResult.
#'
#' @param statement SQL statement to execute
#' @param ... additional arguments to prepared statement substituted for "?"
#' @param list undocumented
#' @return a \code{\linkS4class{SQLServerResult}} object
#' @rdname SQLServerConnection-class
#' @export

setMethod("dbSendQuery", c("SQLServerConnection", "character"),
  def = function (conn, statement, ..., list=NULL) {
    statement <- as.character(statement)[1L]
    ## if the statement starts with {call or {?= call then we use CallableStatement
    if (isTRUE(as.logical(grepl("^\\{(call|\\?= *call)", statement)))) {
      s <- rJava::.jcall(conn@jc, "Ljava/sql/CallableStatement;", "prepareCall",
        statement, check=FALSE)
      .verify.JDBC.result(s, "Unable to execute JDBC callable statement ",
        statement)
      if (length(list(...)))
        .fillStatementParameters(s, list(...))
      if (!is.null(list))
        .fillStatementParameters(s, list)
      r <- rJava::.jcall(s, "Ljava/sql/ResultSet;", "executeQuery", check=FALSE)
      .verify.JDBC.result(r, "Unable to retrieve JDBC result set for ",
        statement)
    } else if (length(list(...)) || length(list)) {
      ## use prepared statements if there are additional arguments
      s <- rJava::.jcall(conn@jc, "Ljava/sql/PreparedStatement;",
        "prepareStatement", statement, check=FALSE)
      .verify.JDBC.result(s, "Unable to execute JDBC prepared statement ",
        statement)
      if (length(list(...)))
        .fillStatementParameters(s, list(...))
      if (!is.null(list))
        .fillStatementParameters(s, list)
      r <- rJava::.jcall(s, "Ljava/sql/ResultSet;", "executeQuery", check=FALSE)
      .verify.JDBC.result(r, "Unable to retrieve JDBC result set for ",
        statement)
    } else {
      ## otherwise use a simple statement some DBs fail with the above)
      s <- rJava::.jcall(conn@jc, "Ljava/sql/Statement;", "createStatement")
      .verify.JDBC.result(s, "Unable to create simple JDBC statement ",
        statement)
      r <- rJava::.jcall(s, "Ljava/sql/ResultSet;", "executeQuery",
        as.character(statement)[1], check=FALSE)
      .verify.JDBC.result(r, "Unable to retrieve JDBC result set for ",
        statement)
    }
    md <- rJava::.jcall(r, "Ljava/sql/ResultSetMetaData;", "getMetaData",
      check=FALSE)
    .verify.JDBC.result(md, "Unable to retrieve JDBC result set meta data for ",
      statement, " in dbSendQuery")
    new("SQLServerResult", jr=r, md=md, stat=s, pull=rJava::.jnull())
  }
)

#' @rdname SQLServerConnection-class
#' @export
setMethod("dbGetQuery", c("SQLServerConnection", "character"),
  def = function (conn, statement, ...) {
    # Copied from RJDBC:
    # https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R#L136
    # Necessary because otherwise dbGetQuery will call RJDBC's fetch function
    # which isn't very granular (see RJDBC #22)
    res <- dbSendQuery(conn, statement, ...)
    on.exit(rJava::.jcall(res@stat, "V", "close"))
    dbFetch(res, -1)
})

#' @param obj An R object whose SQL type we want to determine
#' @rdname SQLServerConnection-class
#' @export
setMethod("dbDataType", c("SQLServerConnection", "ANY"),
  def = function (dbObj, obj, ...) {
    # RJDBC method is too crude. See:
    # https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R
    # Based on db_data_type.MySQLConnection from dplyr
    # https://msdn.microsoft.com/en-us/library/ms187752(v=sql.90).aspx
    char_type <- function (x) {
      n <- max(nchar(as.character(x)))
      if (n <= 8000) {
        paste0("varchar(", n, ")")
      } else {
        "text"
      }
    }
    switch(class(obj)[1],
      logical = "bit",
      integer = "int",
      numeric = "float",
      factor =  char_type(obj),
      character = char_type(obj),
      # SQL Server does not have a date data type without time corresponding
      # to R's Date class
      Date = "datetime",
      POSIXct = "datetime",
      raw = "binary",
      stop("Unknown class ", paste(class(obj), collapse = "/"), call. = FALSE)
    )
  }
)

# Modified from RJDBC
# https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R#L63
.fillStatementParameters <- function(s, l) {
  for (i in 1:length(l)) {
    v <- l[[i]]
    if (is.na(v)) { # map NAs to NULLs (courtesy of Axel Klenk)
      sqlType <- rToJdbcType(class(v))
      rJava::.jcall(s, "V", "setNull", i, as.integer(sqlType))
    } else if (is.integer(v)) {
      rJava::.jcall(s, "V", "setInt", i, v[1])
    } else if (is.numeric(v)) {
      rJava::.jcall(s, "V", "setDouble", i, as.double(v)[1])
    } else if (is.logical(v)) {
      rJava::.jcall(s, "V", "setBoolean", i, as.logical(v)[1])
    } else if (lubridate::is.Date(v)) {
      # as.POSIXlt sets time to UTC whereas as.POSIXct sets time to local
      # timezone. The tz argument is ignored when a Date is passed to either
      # function
      milliseconds <- as.numeric(as.POSIXlt(v)[1]) * 1000
      vdate <- rJava::.jnew("java/sql/Date", rJava::.jlong(milliseconds))
      rJava::.jcall(s, "V", "setDate", i, vdate)
    } else if (lubridate::is.POSIXct(v)) {
      # as.integer converts POSIXct to seconds since epoch. Timestamp
      # constructor needs milliseconds so multiply by 1000
      # http://docs.oracle.com/javase/7/docs/api/java/sql/Timestamp.html
      milliseconds <- as.numeric(as.POSIXct(v)[1]) * 1000
      vtimestamp <- rJava::.jnew("java/sql/Timestamp",
        rJava::.jlong(milliseconds))
      rJava::.jcall(s, "V", "setTimestamp", i, vtimestamp)
    } else if (is.raw(v)) {
      rJava::.jcall(s, "V", "setByte", i, rJava::.jbyte(as.raw(v)[1]))
    } else {
      rJava::.jcall(s, "V", "setString", i, as.character(v)[1])
    }
  }
}

#' @rdname SQLServerConnection-class
#' @importMethodsFrom RJDBC dbSendUpdate
#' @export

setMethod("dbSendUpdate", c("SQLServerConnection", "character"),
  def = function (conn, statement, ..., list = NULL) {
    # Modified from RJDBC
    # https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R#L108
    statement <- as.character(statement)[1L]
    ## if the statement starts with {call or {?= call then we use CallableStatement
    if (isTRUE(as.logical(grepl("^\\{(call|\\?= *call)", statement)))) {
      s <- rJava::.jcall(conn@jc, "Ljava/sql/CallableStatement;", "prepareCall",
        statement, check=FALSE)
      .verify.JDBC.result(s, "Unable to execute JDBC callable statement ",
        statement)
      on.exit(rJava::.jcall(s, "V", "close")) # same as ORA issue below and #4
      if (length(list(...))) .fillStatementParameters(s, list(...))
      if (!is.null(list)) .fillStatementParameters(s, list)
      r <- rJava::.jcall(s, "Ljava/sql/ResultSet;", "executeQuery", check=FALSE)
      .verify.JDBC.result(r, "Unable to retrieve JDBC result set for ", statement)
    } else if (length(list(...)) || length(list)) {
      ## use prepared statements if there are additional arguments
      s <- rJava::.jcall(conn@jc, "Ljava/sql/PreparedStatement;",
        "prepareStatement", statement, check=FALSE)
      .verify.JDBC.result(s, "Unable to execute JDBC prepared statement ",
        statement)
      on.exit(rJava::.jcall(s, "V", "close"))
      # this will fix issue #4 and http://stackoverflow.com/q/21603660/2161065
      if (length(list(...))) .fillStatementParameters(s, list(...))
      if (!is.null(list)) .fillStatementParameters(s, list)
      rJava::.jcall(s, "I", "executeUpdate", check=FALSE)
    } else {
      s <- rJava::.jcall(conn@jc, "Ljava/sql/Statement;", "createStatement")
      .verify.JDBC.result(s, "Unable to create JDBC statement ", statement)
      on.exit(rJava::.jcall(s, "V", "close"))
      # in theory this is not necesary since 's' will go away and be collected, but appearently it may be too late for Oracle (ORA-01000)
      rJava::.jcall(s, "I", "executeUpdate", as.character(statement)[1],
        check=FALSE)
    }
    x <- rJava::.jgetEx(TRUE)
    if (!rJava::is.jnull(x)) {
      stop("execute JDBC update query failed in dbSendUpdate (",
        rJava::.jcall(x, "S", "getMessage"), ")")
    }
  }
)

setMethod("dbWriteTable", "SQLServerConnection",
  function (conn, name, value, overwrite = TRUE, append = FALSE) {
    # Based on RJDBC method:
    # https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R#L242
    # However require `value` to be a data frame. No coercion will take place
    assertthat::assert_that(is.data.frame(value), ncol(value) > 0)
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
    fdef <- paste(dplyr::ident(names(value)), fts, collapse = ', ')
    qname <- dplyr::ident(name)
    if (!append) {
      ct <- paste("CREATE TABLE ", qname, " (", fdef, ")", sep= '')
      dbSendUpdate(conn, ct)
    }
    if (length(value[[1]])) {
      # Use Prepared Statement.
      inss <- paste("INSERT INTO ", qname, " VALUES(",
        paste(rep("?", length(value)), collapse = ','), ")", sep = '')
      for (j in 1:length(value[[1]])) {
        dbSendUpdate(conn, inss, list = as.list(value[j, ]))
      }
    }
    if (ac) dbCommit(conn)
})

# Modified from RJDBC:
# https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R#L161
setMethod("dbListTables", "SQLServerConnection", function(conn, ...) {
  md <- rJava::.jcall(conn@jc, "Ljava/sql/DatabaseMetaData;", "getMetaData",
    check = FALSE)
  .verify.JDBC.result(md, "Unable to retrieve JDBC database metadata")
  # Create arguments for call to getTables
  jns <- rJava::.jnull("java/lang/String")
  table_types <- rJava::.jarray(c("TABLE", "VIEW"))
  rs <- rJava::.jcall(md, "Ljava/sql/ResultSet;", "getTables",
    jns, jns, jns, table_types, check = FALSE)
  .verify.JDBC.result(rs, "Unable to retrieve JDBC tables list")
  on.exit(rJava::.jcall(rs, "V", "close"))
  tbls <- character()
  while (rJava::.jcall(rs, "Z", "next")) {
    tbls <- c(tbls, rJava::.jcall(rs, "S", "getString", "TABLE_NAME"))
  }
  tbls
})

setMethod("dbExistsTable", "SQLServerConnection", function (conn, name, ...) {
  all(name %in% dbListTables(conn))
})

# DBI methods that inherit from RJDBC:
# dbDisconnect()
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

#' @param dbObj An object inheriting from \code{\linkS4class{SQLServerResult}}
#' @rdname SQLServerResult-class
#' @export
setMethod ('dbIsValid', 'SQLServerResult', function (dbObj) {
  rJava::.jcall(dbObj@jr, "Z", "isClosed")
})

#' @param res an object inheriting from \code{\linkS4class{SQLServerResult}}
#' @param n  If n is -1 then the current implementation fetches 32k rows first
#' and then (if not sufficient) continues with chunks of 512k rows, appending
#' them. If the size of the result set is known in advance, it is most efficient
#' to set n to that size.
#' @param ... other arguments passed to method
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
        f <- paste0("as.", unname(rcts[cname]))
        df[, cname] <- eval(call(f, df[, cname]))
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
#' @export
setMethod("dbColumnInfo", "SQLServerResult", def = function (res, ...) {
  # Inspired by RJDBC method for JDBCResult
  # https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R
  cols <- rJava::.jcall(res@md, "I", "getColumnCount")
  df <- dplyr::data_frame(field.name = character(),
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


# Inherited from DBI:
# show()
# dbGetStatement()
# dbGetRowsAffected()
# dbGetRowCount()
#
# Inherited from RJDBC:
# fetch()
# dbClearResult()
# dbGetInfo()

# Other ----------------------------------------------------------------

# Copied from RJDBC:
# https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R#L18

.verify.JDBC.result <- function (result, ...) {
  if (rJava::is.jnull(result)) {
    x <- rJava::.jgetEx(TRUE)
    if (rJava::is.jnull(x))
      stop(...)
    else
      stop(..., " (", rJava::.jcall(x, "S", "getMessage"), ")")
  }
}
