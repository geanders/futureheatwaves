# futureheatwaves 1.0.1

* Fixed a bug in the C++ function to identify heatwaves. Originally, this function passed two integer vectors into `NumericVector` Rcpp classes. This occassionally caused a crash on some operating systems related to a call to `coerceToReal`. The function now passes these two integer vectors into `IntegerVector` Rcpp classes. 
* Added a `NEWS.md` file to track changes to the package.



