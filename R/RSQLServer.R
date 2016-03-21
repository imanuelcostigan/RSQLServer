#' @importFrom methods setClass setMethod show new
#' @import DBI
NULL

#' RSQLServer
#'
#' Wrapper around RJDBC DBI applied specifically to SQL Server. Sybase may work,
#' but to date hasn't been tested. You will need the
#' \href{https://www.java.com/en/download/index.jsp}{Java Runtime Environment}.
#'
#' If you intend to use integrated security (Windows Authentication) to
#' authenticate your server session, you will need to download \href{http://sourceforge.net/projects/jtds/files/}{jTDS}
#' and copy the native single sign on library (\code{ntlmauth.dll}) to any location
#' on your system's \code{PATH} (e.g. \code{Sys.getenv("PATH")}). Full
#' installation instructions are available in the \code{README.SSO}
#' file in the jTDS download bundle. This functionality seems a little flaky in
#' my testing, but it could just be my setup. In any case, it is preferable that
#' you fully specify your server and login credentials in the \code{~/sql.yaml}
#' file. See the example provided:
#' \code{system.file("extdata", "sql.yaml", package = "RSQLServer")}
#'
#' @docType package
#' @name RSQLServer
NULL
