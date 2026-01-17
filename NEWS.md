# derivmkts 0.2.6.9000 (development)

* Need ability to turn random number seed restoration on and off in the functions that do this. Should be false by default (not yet implemented)

# derivmkts 0.2.5.1

* In documentation, change "\\$" to "\$" per CRAN request (escape not necessary)

# derivmkts 0.2.5

* Saving and restoring random number seed should now work properly in
  `simprice`, `quincunx`, and Asian Monte Carlo functions 
* `simprice` now returns `asset` and `trial` as factors
* Vignette revisions
* Configurable tolerance for zero finding implied price and volatility
  functions
* Configurable `upper` and `lower` bounds in `bscallimps` and
  `bsputimps`
* Configurable `highvol` and `lowvol` bounds for `bscallimpvol` and
  `bsputimpvol`
* Compute elast (greeks.R) for negative premium positions

# derivmkts 0.2.4

* New function `simprice`, which produces simulated lognormal price
  paths, with or without jumps.
  * Paths can be split into subperiods, e.g. to value Asian options
  * By supplying a covariance matrix, simprice will return multiple
    correlated price paths.

* In `greeks`, specifying `long=TRUE` now also implies `complete=TRUE`
  (#3)

* The `mertonjump` function now returns a dataframe, has a `complete`
  option and handles vectorized `lambda`

# derivmkts 0.2.3

* greeks `tidy` option renamed to `complete`

* Added options to `greeks`: 
  * `complete` if `TRUE`, return all inputs and greeks for each case
* greeks `tidy` option renamed to `complete`. By default, this returns
  wide form results
  * `long` (if `complete=TRUE`, return long form output)
  * `initcaps`: capitalize "Delta", "Gamma", etc.

* Breaking change:
  * option value is now returned as "premium" rather than "price";
    the term "price" is ambiguous (e.g. a futures price is 100 but
    the value of the contract --- the premium --- is 0)

* Fixed greeks `elast` calculation for barrier options --- would return Inf when
 close to out barrier (fixelast branch)

* added dependency on `testthat`

# derivmkts 0.2.2.1

Primarily a maintenance release with one new feature (tidy output)

* Added `tidy` parameter to `greeks` function to return output in wide
  tidy format. This is `FALSE` by default, for compatability.

* Fix: if a parameter in the function passed to greeks uses the
  index "i", the eval step in Greeks fails (because the eval loop
  also uses "i"). The index variable is now `z91k25`

* Fix: spurious "break" in implied.R
   
# derivmkts 0.2.2

* Functions for compound options (call on call, call on put, etc.)

* Binomplot: Option for log y axis

* New vignette discussing alternative ways to write vectorized
  functions

# derivmkts 0.2.1

* Binomopt

	* Fixed: default dn=1.5 in binomopt and binomplot
	* Added "returnprice" parameter to binomplot

* greeks
	* Simplification of Greeks discussion in README.Rmd
	* Greeks function ignores theta when appropriate (perpetual options)

* callperpetual and putperpetual functions added to barriers.R

* Asian options 

	* Added individual geomavgpricecall, geomavgpriceput,
	  geomavgstrikecall, and geomavgstrikeput functions.
	* Fixed greeks functionality for asian options
	

# derivmkts 0.2.0

* First CRAN release

* Completed vignette

# derivmkts 0.1.3.9000

* added simple bond functions (yield, pv, duration, convexity)

* fixed problem with vectorization in barrier options 

# derivmkts 0.1.2.9000

* Added Asian pricing files

# derivmkts 0.1.1.9000

* Greeks (delta, gamma, theta, added to binomial output)

* With returntrees=TRUE, returns replicating portfolio components
  ($bond and $delta)

# derivmkts 0.1.0.9000

* Added binomial pricing via `binomopt` and plotting of the binomial
  tree with `binomplot`

# derivmkts 0.0.0.9000

First version. Includes:

* basic Black-Scholes functions and Greeks

* barrier pricing

* implied volatility

* quincunx (Galton board) function to illustrate the central limit
  theorem


