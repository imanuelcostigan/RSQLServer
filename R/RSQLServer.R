#' @import DBI methods
NULL

#' RSQLServer
#'
#' Implements an R Database Interface (DBI) for SQL Server. Sybase may work,
#' but to date has not been tested. You will need to have installed the
#' \href{https://www.java.com/en/download/index.jsp}{Java Runtime Environment}.
#'
#' If you intend to use integrated security (Windows Authentication) to
#' authenticate your server session, you will need to download \href{http://sourceforge.net/projects/jtds/files/}{jTDS}
#' and copy the native single sign on library (\code{ntlmauth.dll}) to any location
#' on your system's \code{PATH} (e.g. \code{Sys.getenv("PATH")}). Full
#' installation instructions are available in the \code{README.SSO}
#' file in the jTDS download bundle. This functionality is unreliable in
#' my testing, but it could just be my setup. I would recommend that that
#' you fully specify your server and login credentials in the \code{~/sql.yaml}
#' file rather than using the single sign on library. See the example provided:
#' \code{system.file("extdata", "sql.yaml", package = "RSQLServer")}
#'
#' @docType package
#' @name RSQLServer
#' @importFrom purrr %||%
NULL
