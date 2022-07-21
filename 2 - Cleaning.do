*Keep Canadian and US Firms
keep if fic == "CAN" | fic == "USA"

*Encoding Country of Incorporation
encode fic, gen(fic_n)
drop fic
rename fic_n fic

*Industry Classification
sort gvkey date
/// Agriculture, Forestry, Fishing
gen industry = "Agriculture, Forestry, Fishing" if ///
	strpos(sic,"0") == 1
/// Mining
replace industry = "Mining" if ///
	strpos(sic,"10") == 1 | /// 
	strpos(sic,"11") == 1 | /// 
	strpos(sic,"12") == 1 | /// 
	strpos(sic,"13") == 1 | /// 
	strpos(sic,"14") == 1
/// Construction
replace industry = "Construction" if ///
	strpos(sic,"15") == 1 | ///
	strpos(sic,"16") == 1| ///
	strpos(sic,"17") == 1
/// Manufacturing
replace industry = "Manufacturing" if ///
	strpos(sic,"2") == 1 | ///
	strpos(sic,"3") == 1 
/// Transportation and Utilities
replace industry = "Transportation and Utilities" if ///
	strpos(sic,"4") == 1
/// Wholesale Trade
replace industry = "Wholesale Trade" if ///
	strpos(sic,"50") == 1 | ///
	strpos(sic,"51") == 1
/// Retail Trade
replace industry = "Retail Trade" if ///
	strpos(sic,"52") == 1 | ///
	strpos(sic,"53") == 1 | ///
	strpos(sic,"54") == 1 | ///
	strpos(sic,"55") == 1 | ///
	strpos(sic,"56") == 1 | ///
	strpos(sic,"57") == 1 | ///
	strpos(sic,"58") == 1 | ///
	strpos(sic,"59") == 1 
/// Finance, Insurance, Real Estate
replace industry = "Finance, Insurance, Real Estate" if ///
	strpos(sic,"6") == 1
/// Services
replace industry = "Services" if ///
	strpos(sic,"7") == 1 | ///
	strpos(sic,"8") == 1
/// Public Administration 
replace industry = "Public Administration" if ///
	strpos(sic,"9") == 1
	
*Encoding Industry
encode sic, gen(sic_n)
drop sic
rename sic_n sic

encode naics, gen(naics_n)
drop naics
rename naics_n naics

encode industry, gen(industry_n)
drop industry
rename industry_n industry

*Dropping by Industry
drop if industry == 3 | industry == 6

*Imputation of Missing Values with Linear Interpolation
foreach var of varlist ppentq ppegtq saleq atq ltq actq lctq req piq xintq mkvaltq xrdq niq wcapq cheq dlttq dlcq niq oibdpq txty price revtq capxy prccq cshoq {
    replace `var' = (`var'[_n-1] + `var'[_n+1])/2 if `var'[_n] == . & gvkey[_n-1] == gvkey[_n] & gvkey[_n] == gvkey[_n+1]
}

*Dropping Missing and Non-Positive Values
drop if ppentq == . | ppentq < 0
*drop if ppegtq == . | ppegtq < 0
drop if saleq == . | saleq < 0
drop if atq == . | atq < 0
drop if emp < 0

*Firm Age Classification
replace ipodate = qofd(ipodate)
format ipodate %tq
sort gvkey date

gen firmage = .
replace firmage = 1 if ipodate == . | ipodate != . & ipodate > date
replace firmage = date - ipodate if ipodate != . & ipodate <= date
foreach gvkey in gvkey {
	replace firmage = firmage[_n-1] + 1 if gvkey[_n] == gvkey[_n-1] & ipodate == .
	replace firmage = firmage[_n-1] + 1 if gvkey[_n] == gvkey[_n-1] & ipodate != . & ipodate > date
}

*Drop Young Firm-Quarter Observations
drop if firmage < 40

*Creating Eletion Year Dummy Variables
/// Canada
gen canelectionyear = 0
replace canelectionyear = 1 if fyearq == 1962 | fyearq == 1963 | fyearq == 1965 | fyearq == 1968 | fyearq == 1972 | fyearq == 1974 | fyearq == 1979 | fyearq == 1980 | fyearq == 1984 | fyearq == 1988 | fyearq == 1993 | fyearq == 1997 | fyearq == 2000 | fyearq == 2004 | fyearq == 2006 | fyearq == 2008 | fyearq == 2011 | fyearq == 2015 | fyearq == 2019 | fyearq == 2021

/// US
gen uselectionyear = 0
replace uselectionyear = 1 if fyearq == 1964 | fyearq == 1968 | fyearq == 1972 | fyearq == 1976 | fyearq == 1980 | fyearq == 1984 | fyearq == 1988 | fyearq == 1992 | fyearq == 1996 | fyearq == 2000 | fyearq == 2004 | fyearq == 2008 | fyearq == 2012 | fyearq == 2016 | fyearq == 2020

gen electionyear = 0
replace electionyear = canelectionyear if fic == 1
replace electionyear = uselectionyear if fic == 2

*Investment Rate
sort gvkey date
gen investment = log(ppentq[_n]) - log(ppentq[_n-1]) if gvkey[_n] == gvkey[_n-1]

*Productivity Growth
foreach var of varlist revtq emp ppentq firmage capxy {
    gen log_`var' = log(`var')
}
rename log_revtq y
rename log_emp l
rename log_ppentq k
rename log_firmage a
rename investment inv

/// OLS
reg y k l a, r
predict productivity_ols, resid
replace productivity_ols = exp(productivity_ols)

/// Fixed Effects
xtreg y k l a, fe
predict productivity_fe, e
replace productivity_fe = exp(productivity_fe)

/// Olley-Pakes (1996) Method
prodest y, method(op) free(l) state(k) proxy(inv) poly(3) reps(5) attrition
predict productivity_op, residuals exponential

/// Calculating Productivity Growth 
sort gvkey date
gen productivitygrowth_ols = (productivity_ols[_n] - productivity_ols[_n-1])/productivity_ols[_n-1]

gen productivitygrowth_fe = (productivity_fe[_n] - productivity_fe[_n-1])/productivity_fe[_n-1]

gen productivitygrowth_op = (productivity_op[_n] - productivity_op[_n-1])/productivity_op[_n-1]

rename productivitygrowth_op productivitygrowth

*Bankruptcy
gen x1 = ((actq-lctq)/atq)
gen x2 = (req/atq)
gen x3 = ((niq + xintq + txty)/atq)
gen x4 = ((prccq*cshoq)/ltq)
gen x5 = (saleq/atq)

foreach var of varlist x1 x2 x3 x4 x5 {
    egen `var'_low = pctile(`var'), p(0.5)
	egen `var'_high = pctile(`var'), p(99.5)
	drop if `var' < `var'_low & `var' != .
	drop if `var' > `var'_high & `var' != .
	drop `var'_low `var'_high  
}

gen bankruptcy = 1.2*x1 + 1.4*x2 + 3.3*x3 + 0.6*x4 + 1.0*x5

*Accounting for Changes in Organizational Structure
sort gvkey date
rename inv investment
gen attq_growth = log(atq[_n]) - log(atq[_n-1]) if gvkey[_n] == gvkey[_n-1]
gen salesgrowth = log(saleq[_n]) - log(saleq[_n-1]) if gvkey[_n] == gvkey[_n-1]

foreach var of varlist attq_growth investment salesgrowth productivitygrowth {
    egen `var'_low = pctile(`var'), p(0.5)
	egen `var'_high = pctile(`var'), p(99.5)
	drop if `var' < `var'_low & `var' != .
	drop if `var' > `var'_high & `var' != .
	drop `var'_low `var'_high 
}

*Winsorization on Key Financial Variables
sort gvkey date
gen liquidity = cheq/atq
gen debt = (dlttq + dlcq)
gen leverage = debt/atq
gen cashflow = oibdpq / ppentq
gen tobinsq = (atq + (cshoq*prccq) - ceqq)/atq

foreach var of varlist liquidity leverage cashflow tobinsq {
    egen `var'_low = pctile(`var'), p(0.5)
	egen `var'_high = pctile(`var'), p(99.5)
	drop if `var' < `var'_low & `var' != .
	drop if `var' > `var'_high & `var' != .
	drop `var'_low `var'_high 
}

*Removing Observations with Extreme Values
/// Leverage
drop if leverage > 10 & leverage != . 

/// Asset Ratio
gen assetratio = actq/atq
drop if assetratio > 10 & assetratio != .
drop if assetratio < -10 & assetratio != .

/// Sales Growth
drop if salesgrowth > 1 & salesgrowth != .
drop if salesgrowth < -1 & salesgrowth != .

*Constructing Stock Return Volatility
sort gvkey date
gen stockreturn = (price[_n] - price[_n-1])/price[_n-1] if gvkey[_n] == gvkey[_n-1]
egen stockreturn_low = pctile(stockreturn), p(0.5)
egen stockreturn_high = pctile(stockreturn), p(99.5)
drop if stockreturn < stockreturn_low & stockreturn != .
drop if stockreturn > stockreturn_high & stockreturn != .
drop stockreturn_low stockreturn_high
bys date: egen srv = sd(stockreturn)

/// Adjustment for Fiscal Quarter Date
bys gvkey (date): gen srv_lead = f.srv
drop srv
rename srv_lead srv

*Trim Sample Period by Date
drop if date < tq(1980q1)
drop if date > tq(2022q1)

*Drop Small Panels
*sort gvkey date
*bys gvkey (date): gen n_obs = _N
*drop if n_obs < 40