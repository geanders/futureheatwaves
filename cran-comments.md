## Test environments
* local OS X install, R 3.3.0
* ubuntu 12.04 (on travis-ci), R 3.2.4
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs. 

## Reverse dependencies

This package has no reverse dependencies.

## Other points

* This release is to create a stable release to prevent two errors reported by CRAN that were caused by crashes. The errors were caused by tests of a function that called a function created using the Rcpp package, which was linked to an error in a call to `coerceToReal`. The old version of the package passed a value coerced to an integer using `as.integer` to the C++ function; in the current code, we have changed to code to coerce the value to numeric using `as.numeric` before we pass it to the C++ function. All checks on OS X, Ubuntu, and Windows are now passing without any crashes or errors. 
* One function in this package writes out files to the user's computer. The function requires the user's explicit permission before doing this by printing a warning message through a prompt and asking for permission before proceeding to write the files. The exact warning is: "Warning: This function will write new files to your computer in the [user-selected] directory of your computer. If that directory already exists, running this function will write over it. Do you want to continue? (y / n):". The function will abort if the user does not enter "y" or "yes" at this prompt.
