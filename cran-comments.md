## Test environments

* local OS X install, R 3.2.4
* Win-builder R 3.2.4 and 3.3.0 alpha

## R CMD check results

There were no ERRORs, WARNINGs but two NOTEs (but only via win-builder):

First spelling issues related to package names and legitimate abbreviations:

> Possibly mis-spelled words in DESCRIPTION:
>   Backend (3:60)
>   DBI (3:41, 19:44, 21:58)
>   JDBC (18:42)
>   RJDBC (19:26, 21:23, 22:61)
>   RJDBC's (24:39)
>   SQL (3:8, 3:56, 18:51, 25:54)
>   SQLServerConnection (20:22)
>   SQLServerDriver (20:5)
>   SQLServerRsult (20:44)
>   backend (25:58)
>   dbConnect (23:18)
>   dbWriteTable (24:57)
>   dplyr (3:50, 25:73)
>   jTDS (18:27)

Second complaints about `dplyr`, a package name, not being title case in my 
package's Title field. 

> The Title field should be in title case, current version then in title case:
> 'SQL Server R Database Interface (DBI) and dplyr SQL Backend'
> 'SQL Server R Database Interface (DBI) and Dplyr SQL Backend'

I don't think either of these aspects of the note should prevent submission.

## Downstream dependencies

This package has no downstream dependencies.
