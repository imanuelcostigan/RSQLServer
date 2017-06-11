# Following instructions:
# http://cran.r-project.org/web/packages/helloJavaWorld/vignettes/helloJavaWorld.pdf

.onLoad <- function (libname, pkgname) {
  res <- rJava::.jpackage(pkgname, lib.loc = libname)
  # Per "Writing R Extensions"
  if (java_run_time_version() < 1.8) {
    stop("Java >= 8 is needed for this package but not available", call. = FALSE)
  }
  res
}

java_run_time_version <- function() {
  # From "Writing R Extensions"
  jv <- rJava::.jcall("java/lang/System", "S", "getProperty",
    "java.runtime.version")
  numeric_version(paste0(strsplit(jv, "[.]")[[1L]][1:2], collapse = "."))
}
