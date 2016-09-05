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


