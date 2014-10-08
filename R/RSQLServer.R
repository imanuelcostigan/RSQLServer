#' @import methods DBI rJava RJDBC
NULL

#' RSQLServer
#'
#' Wrapper around RJDBC DBI applied specifically to SQL Server. You will need the
#' \href{https://www.java.com/en/download/index.jsp}{Java Runtime Environment}.
#'
#' If you intend to use integrated security (Windows Authentication) to
#' authenticate your server session, you will need to download \href{http://sourceforge.net/projects/jtds/files/}{jTDS}
#' and copy the native single sign on library (\code{ntlmauth.dll}) to any file
#' on your system's \code{PATH} (e.g. \code{Sys.getenv("PATH")}). This
#' functionality seems a little flaky in my testing, but it could just be my
#' setup. Full installation instructions are available in the \code{README.SSO}
#' file in the jTDS download bundle.
#'
#' @docType package
#' @name RSQLServer
NULL
