# futureheatwaves 1.0.2.9000

* Add a function to create interactive leaflet maps to explore relationship
between study cities and associated climate model grid points for each 
climate model. 
* Change so if user selects "n" when prompted to allow `gen_hw_set` to write files locally, the function returns `NULL` rather than an error, along with a message explaining the result.
* Improve memory management for some of the functions. Some functions write to global objects. In previous versions, the functions also returned what they were writing to the global objects. However, this is unnecessary, so these functions have been changed to return `NULL` and so use less memory.
* Change to use `fread` from `data.table` instead of `read.csv` to read in climate data from csv.

# futureheatwaves 1.0.2

* Add code so that package can process climate projections that include Feb. 29 or Feb. 30 (it defaults the start day and month of those heat waves to be Mar. 1). (Note: If there is a heatwave that starts on Feb. 29 in a non-leap year and only lasts two days, you may get an error. I think this will be a very rare case, but please email the package maintainer if you experience this problem.)

# futureheatwaves 1.0.1

* Fixed a bug in the C++ function to identify heatwaves. Originally, this function passed two integer vectors into `NumericVector` Rcpp classes. This occassionally caused a crash on some operating systems related to a call to `coerceToReal`. The function now passes these two integer vectors into `IntegerVector` Rcpp classes. 
* Added a `NEWS.md` file to track changes to the package.



