start_driver <- function() {
  rJava::.jaddClassPath(jtds_class_path())
  rJava::.jnew("net.sourceforge.jtds.jdbc.Driver")
}

driver_version <- function(driver, check = TRUE) {
  rJava::.jcall(driver@jdrv, "S", "getVersion", check = check)
}

new_connection <- function(driver, url, properties = NULL, check = TRUE) {
  properties <- properties %||%  rJava::.jnew('java/util/Properties')
  rJava::.jcall(driver@jdrv, "Ljava/sql/Connection;", "connect", url,
    properties, check = check)
}

close_connection <- function (conn, check = TRUE) {
  rJava::.jcall(conn@jc, "V", "close", check = check)
}

connection_info <- function (conn, info) {
  switch(info,
    username = rJava::.jfield(conn@jc, "S", "user"),
    host = rJava::.jfield(conn@jc, "S", "serverName"),
    port = rJava::.jfield(conn@jc, "I", "portNumber"),
    dbname = rJava::.jfield(conn@jc, "S", "currentDatabase"),
    db.version = numeric_version(rJava::.jfield(conn@jc, "S",
      "databaseProductVersion")))
}

create_statement <- function(conn, check = FALSE) {
  rJava::.jcall(conn@jc, "Ljava/sql/Statement;", "createStatement", check = check)
}

create_prepared_statement <- function(conn, statement, check = FALSE) {
  rJava::.jcall(conn@jc, "Ljava/sql/PreparedStatement;", "prepareStatement",
    statement, check = check)
}

close_statement <- function(statement, check = TRUE) {
  rJava::.jcall(statement, "V", "close", check = check)
}

execute_query <- function(statement, string = NULL, check = FALSE) {
  if (is.null(string)) {
    # A prepared statement
    rJava::.jcall(statement, "Ljava/sql/ResultSet;", "executeQuery",
      check = check)
  } else {
    rJava::.jcall(statement, "Ljava/sql/ResultSet;", "executeQuery",
      string, check = check)
  }
}

execute_update <- function(statement, string = NULL, check = FALSE) {
  if (is.null(string)) {
    rJava::.jcall(statement, "I", "executeUpdate", check = check)
  } else {
    rJava::.jcall(statement, "I", "executeUpdate", string, check = check)
  }
}

rs_metadata <- function(res, check = FALSE) {
  rJava::.jcall(res, "Ljava/sql/ResultSetMetaData;", "getMetaData",
    check = check)
}

catch_exception <- function (object, ...) {
  # Based on RJDBC .verify.JDBC.result()
  # https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R#L18
  if (rJava::is.jnull(object)) {
    x <- rJava::.jgetEx(TRUE)
    if (rJava::is.jnull(x))
      stop(..., call. = FALSE)
    else
      stop(..., ": ", rJava::.jcall(x, "S", "getMessage"), call. = FALSE)
  }
}
