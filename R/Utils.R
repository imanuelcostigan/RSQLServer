msft_url <- function(server = NULL, port = NULL, instance = NULL, properties = NULL) {
  # General form:
  # jdbc:sqlserver://[serverName[\instanceName][:portNumber]][;property=value[;property=value]]
  # Source: https://docs.microsoft.com/en-us/sql/connect/jdbc/building-the-connection-url
  if (is.null(server) && is.null(properties)) {
    return(NA)
  }
  # You can build URL completely as a set of properties. So lets do this to make
  # the code easier to read
  # https://docs.microsoft.com/en-us/sql/connect/jdbc/setting-the-connection-properties
  url <- "jdbc:sqlserver://"
  plist <- list(
    serverName = server,
    portNumber = port,
    instanceName = instance)
  plist <- purrr::compact(c(plist, properties))
  pchar <- paste(names(plist), unlist(plist, use.names = FALSE), sep = "=")
  pstr <- paste0(pchar, collapse = ";")
  paste0(url, paste0(";", pstr))
}

#' Get server details from YAML file
#'
#' The \code{sql.yaml} file in a user's \code{HOME} directory can store server
#' details and login credentials (in plain text). The YAML file format is
#' documented in this package's \code{README} file, while an example is provided
#' in \code{extdata/sql.yaml} (see example). At a high level, each server should
#' be documented in its own associative array with each aspect of the server
#' documented in an associative array.
#'
#' @param server corresponds to the server name key in the YAML \code{file} and
#' should be a string.
#' @param file defaults to \code{NULL} which means that it will use
#' \code{$HOME/sql.yaml}.
#' @return a named list of \code{server} details if this is specified in the
#' \code{file}. \code{NULL} is returned if the \code{file} does not contain the
#' \code{server} key
#' @examples
#' # See link below
#' \dontrun{
#' aw <- dbConnect(RSQLServer::SQLServer(), server = "mhknbn2kdz.database.windows.net",
#'  database = 'AdventureWorks2012',
#'  properties = list(user = "sqlfamily", password = "sqlf@@m1ly"))
#' dbListTables(aw)
#' }
#' @seealso
#' \href{https://github.com/imanuelcostigan/RSQLServer/blob/master/README.md}{RSQLServer README}
#' \href{https://github.com/yaml/yaml}{YAML}
#' \href{http://sqlblog.com/blogs/jamie_thomson/archive/2012/03/27/adventureworks2012-now-available-to-all-on-sql-azure.aspx}{Example SQL Server instance}
#' @keywords internal

get_server_details <- function (server, file = NULL) {
  assertthat::assert_that(assertthat::is.string(server))
  yaml::yaml.load_file(file)[[server]]
}

#' Checks availability of TEST server
#'
#' SQL Server details can be specified in a \code{~/sql.yaml} file.
#' To be able to run examples and some tests in the package, it is necessary for
#' there to be a valid server with name TEST in this file.
#'
#' @param type specifies whether the server type is \code{"sqlserver"} (default)
#' or \code{"sybase"}
#' @return boolean \code{TRUE} if TEST server details are available. Otherwise,
#' \code{FALSE}
#' @examples
#' have_test_server()
#' @seealso \code{\link{get_server_details}} \code{\link{dbConnect,SQLServerDriver-method}}
#' @export

have_test_server <- function (type = 'sqlserver') {
  yaml_file <- file.path(Sys.getenv("HOME"), "sql.yaml")
  if (file.exists(yaml_file)) {
    server_details <- yaml::yaml.load_file(yaml_file)
    res <- assertthat::has_name(server_details, "TEST")
    return(isTRUE(res & identical(server_details[["TEST"]]$type, type)))
  } else {
    return(FALSE)
  }
}

msft_class_path <- function () {
  file.path(system.file('java', package = 'RSQLServer'),
    'mssql-jdbc-6.1.7.jre8.preview.jar')
}

pull_class_path <- function () {
  file.path(system.file('java', package = 'RSQLServer'), "MSSQLRequestPull.jar")
}

# SQL types --------------------------------------------------------------

as_is_type <- function(x, con) {
  class(x) <- class(x)[-1]
  dbDataType(con, x)
}

data_frame_data_type <- function(x, con) {
  vapply(x, dbDataType, FUN.VALUE = character(1), dbObj = con, USE.NAMES = TRUE)
}

# Needed to keep track of number of rows that have been fetched for
# dbGetRowCount as JDBC ResultSet class's getRow() method returns 0 when the
# fetch is completed.
RowCounter <- setRefClass(
  Class   = "RowCounter",
  fields  = list(count = "integer"),
  methods = list(
    add = function(increment) {
      count <<- count + increment
    },
    show = function() {
      cat("<RowCounter>:", count, "\n")
    }
  )
)

