## Test environments
* local OS X install, R 3.2.3
* ubuntu 12.04 (on travis-ci), R 3.2.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.

## Other points

- One function in this package writes out files to the user's computer. The function requires the user's explicit permission before doing this by printing a warning message through a prompt and requiring the user's explicit permission before proceeding to write the files.
- The package has several functions that process comma-separated files. Therefore, I have included some example data for function examples and the vignette as comma-separated files in the `inst/extdata` directory rather than saving these example datasets as `.rda` files in the `data` directory.
