# Following instructions:
# http://cran.r-project.org/web/packages/helloJavaWorld/vignettes/helloJavaWorld.pdf

.onLoad <- function (libname, pkgname)
  .jpackage(pkgname, lib.loc = libname)
