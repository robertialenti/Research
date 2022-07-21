*Preliminaries
use "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\Quarterly Compustat.dta", clear
set scheme s1mono
sort gvkey fyearq fqtr

*Configure Date
gen date = qofd(datadate)
format date %tq
duplicates report gvkey date
duplicates tag gvkey date, gen(isdup)
drop if isdup >= 1
drop isdup

gen quarter = quarter(dofq(date))
drop fqtr
rename quarter fqtr

*Generate Unique Identifier
gen Q = "Q"
egen new_date = concat(fyearq Q fqtr)
egen identifier = concat(gvkey new_date)

duplicates report identifier
duplicates tag identifier, gen(isdup)
drop if isdup >= 1
drop isdup

*Employment
sort gvkey date
merge 1:1 identifier using "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\employment.dta"
drop if _merge == 2
drop _merge

*Inflation Adjustment
/// Canada
sort gvkey date
merge m:m date using "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\candeflator.dta"
drop if _merge == 2
drop _merge
rename deflator candeflator
replace candeflator = candeflator / 100
foreach var of varlist saleq ppentq ppegtq atq ltq actq lctq req piq xintq mkvaltq xrdq niq wcapq cheq dlttq dlcq niq oibdpq txty revtq capxy {
    replace `var' = `var' / candeflator if fic == "CAN"
}

/// US
sort gvkey date
merge m:m date using "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\usdeflator.dta"
drop if _merge == 2
drop _merge
rename deflator usdeflator
replace usdeflator = usdeflator / 100
foreach var of varlist saleq ppentq ppegtq atq ltq actq lctq req piq xintq mkvaltq xrdq niq wcapq cheq dlttq dlcq niq oibdpq txty revtq capxy {
    replace `var' = `var' / usdeflator if fic == "USA"
}

*Securities Data
sort gvkey date
merge 1:1 identifier using "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\securities.dta"
drop if _merge == 2
drop _merge

*Aggregate Uncertainty Measures	
/// Canada RSMV
sort gvkey date
merge m:m date using "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\canrsmv.dta"
drop if _merge == 2
drop _merge

/// US RSMV
sort gvkey date
merge m:m date using "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\usrsmv.dta"
drop if _merge == 2
drop _merge

gen rsmv = .
replace rsmv = canrsmv if fic == "CAN"
replace rsmv = usrsmv if fic == "USA"

/// VIX
sort gvkey date
merge m:m date using "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\vix.dta"
drop if _merge == 2
drop _merge

/// CAN EPU
sort gvkey date
merge m:m date using "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\canepu.dta"
drop if _merge == 2
drop _merge

/// US EPU
sort gvkey date
merge m:m date using "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\usepu.dta"
drop if _merge == 2
drop _merge

gen epu = .
replace epu = canepu if fic == "CAN"
replace epu = usepu if fic == "USA"

*Macroeconomic Variables
/// Bank of Canada Bank Rate
sort gvkey date
merge m:m date using "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\canbankrate.dta"
drop if _merge == 2
drop _merge

/// Federal Funds Rate
sort gvkey date
merge m:m date using "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\usfedfundrate.dta"
drop if _merge == 2
drop _merge

gen bankrate = .
replace bankrate = canbankrate if fic == "CAN"
replace bankrate = usfedfundrate if fic == "USA"
foreach var of varlist canbankrate usfedfundrate bankrate {
	replace `var' = `var' / 100
}

/// Stock Market Index
sort gvkey date
merge m:m date using "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\stockmarketindex.dta"
drop if _merge == 2
drop _merge
gen stockmarket = .
replace stockmarket = tsx if fic == "CAN"
replace stockmarket = sp500 if fic == "USA"
gen stockmarket_dlog = .
replace stockmarket_dlog = tsx_dlog if fic == "CAN"
replace stockmarket_dlog = sp500_dlog if fic == "USA"

*Configure Dataset as Panel
destring gvkey, generate(gvkey_n)
drop gvkey
rename gvkey_n gvkey

duplicates report gvkey date
duplicates tag gvkey date, gen(isdup)
drop if isdup >= 1
drop isdup

xtset gvkey date
sort gvkey date