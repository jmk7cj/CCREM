* ---------------------------------------------------------------------------- *
* set working directory, import and view data
* ---------------------------------------------------------------------------- *
cd "/Users/joekush/Desktop/"
import delimited "cleaned_data.csv", clear
browse
* ---------------------------------------------------------------------------- *



* ---------------------------------------------------------------------------- *
* Model 0: Naive Approach Ignoring Changes in Group Membership (Pure 3-level Clustering)
* ---------------------------------------------------------------------------- *
* fit model
mixed math time female ses public suspension platform ///
|| school: , covariance(identity) ///
|| student: time, cov(unstructured) ///
reml variance
* ---------------------------------------------------------------------------- *



* ---------------------------------------------------------------------------- *
* Model 1: Acute Effects CCREM
* ---------------------------------------------------------------------------- *
* fit model
mixed math time female ses public suspension platform ///
|| _all: R.school, nocons covariance(identity) ///
|| student: time, cov(unstructured) ///
reml variance
* ---------------------------------------------------------------------------- *



* ---------------------------------------------------------------------------- *
* Model 2: Cumulative Effects CCREM
* ---------------------------------------------------------------------------- *
* to construct school Z matrix of cumulative effects, first create dummy
* indicators for each school
tabulate school, generate(d)

* for cumulative weights, fill downward consecutively
* (must run both commands [local] and [forvalues] simultaneously)
local n_schools = r(r)
forvalues x = 1/`n_schools' {
	gen w`x' = d`x'
	bysort student: replace w`x' = w`x'[_n-1] if w`x'==0 & w`x'[_n-1]==1
}

* fit model
mixed math time female ses public suspension platform ///
|| _all: w*, nocons covariance(identity) ///
|| student: time, cov(unstructured) ///
reml variance
* ---------------------------------------------------------------------------- *



* ---------------------------------------------------------------------------- *
* Model 3: Cumulative Effects CCREM with Standardized Weights
* ---------------------------------------------------------------------------- *
* keep same weights from previous Model 2, but ensure weights are proportional
* such that weights sum to 1 for each row of the weight matrix
egen tot_w = rowtotal(w*)
foreach x of varlist w* {
	replace `x' = `x' / tot_w
}

* fit model
mixed math time female ses public suspension platform ///
|| _all: w*, nocons covariance(identity) ///
|| student: time, cov(unstructured) ///
reml variance
* ---------------------------------------------------------------------------- *



* ---------------------------------------------------------------------------- *
* Model 4: Acute Effects CCREM with Autoregressive Group Effects
* ---------------------------------------------------------------------------- *

* not able to implement

* ---------------------------------------------------------------------------- *



* ---------------------------------------------------------------------------- *
* Model 5: Acute Effects CCREM with Compound Symmetric Group Effects
* ---------------------------------------------------------------------------- *

* not able to implement

* ---------------------------------------------------------------------------- *



* ---------------------------------------------------------------------------- *
* Model 6: Acute Effects CCREM with Unstructured Group Effects
* ---------------------------------------------------------------------------- *

* not able to implement

* ---------------------------------------------------------------------------- *
