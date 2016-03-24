## Test environments
* local OS X install, R 3.2.4
* ubuntu 12.04 (on travis-ci), R 3.2.4
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was one NOTE:

* This package is a new submission.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.

## Other points

* One function in this package writes out files to the user's computer. The function requires the user's explicit permission before doing this by printing a warning message through a prompt and asking for permission before proceeding to write the files. The exact warning is: "Warning: This function will write new files to your computer in the [user-selected] directory of your computer. If that directory already exists, running this function will write over it. Do you want to continue? (y / n):". The function will abort if the user does not enter "y" or "yes" at this prompt.
