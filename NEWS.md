# derivmkts 0.2.3

* greeks `tidy` option renamed to `complete`

* Added options to `greeks`: 
  * `complete` if `TRUE`, return all inputs and greeks for each case 
* greeks `tidy` option renamed to `complete`. By default, this returns wide form results
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


