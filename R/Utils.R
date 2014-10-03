jre_version <- function ()
{
  key <- readRegistry('SOFTWARE\\JavaSoft\\Java Runtime Environment\\')
  as.numeric(key$CurrentVersion)
}

jdbc_root_path <- function ()
  system.file('java', package = 'RSQLServer')

jdbc_class_path <- function ()
{
  if (jre_version() <= 1.5)
    file.path(jdbc_root_path(), 'sqljdbc.jar')
  else
    file.path(jdbc_root_path(), 'sqljdbc4.jar')
}
