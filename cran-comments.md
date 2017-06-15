
RESUBMISSION AFTER ENQUOTING 'dplyr' in DESCRIPTION title

## Background

RSQLServer was archived by CRAN after dplyr v0.4 irredeemably broke the dplyr SQL Server backend provided by this package. This submission is to get RSQLServer back onto CRAN following the release of v0.7 allowed us to correct these issues.

## Test environments

* R-3.3.3, 3.4.0, r72772 on Ubuntu 12.04 hosted by Travis CI
* R-3.4.0 run locally on macOS 
* R-3.4.0 on Windows hosted by Appveyor

## R CMD check results

1 WARNING and 1 NOTE

WARNING:

* Getting an error locally regarding README conversion due to inability to retrieve a Travis CI badge. This is not an issue that occurs on Travis / Appveyor CI platforms and shouldn't prevent submission proceeding

NOTE regarding CRAN feasibility:

* "X-CRAN-Comment: Archived on 2016-12-01 as check problems were not corrected despite reminders" (see above explanation)
* There were a number of spelling exceptions highlighted. However these represented package or application names.
* Asking for the first letter of the dplyr package name to be capitalised in the Title field which shouldn't prevent submission proceeding.


## Downstream dependencies

This package has no downstream dependencies.
