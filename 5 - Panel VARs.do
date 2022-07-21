*Aggregating Canadian Data
use "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\CANVAR.dta", clear
keep date
save "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\CANVAR.dta", replace

use "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\dataset.dta", clear
keep if fic == 1
foreach var of varlist srv investment investment_std productivitygrowth productivitygrowth_std bankruptcy bankruptcy_std tobinsq salesgrowth cashflow leverage {
	bys date: asgen mean_`var' = `var', weight(atq) 
}
bysort date: keep if _n == _N 

/// VAR Dataset
merge 1:1 date using "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\CANVAR.dta"
sort date
save "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\CANVAR.dta", replace
use "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\CANVAR.dta", clear

/// Renaming New Variables
foreach var of varlist srv investment investment_std productivitygrowth productivitygrowth_std bankruptcy bankruptcy_std tobinsq salesgrowth cashflow leverage {
	drop `var'
	rename mean_`var' `var'
}

/// Configure as Panel
tsset date

/// Difference Key Variables
foreach var of varlist canrsmv usrsmv vix canepu usepu srv tobinsq salesgrowth cashflow leverage {
	gen `var'_d = d.`var'
}

keep canrsmv canrsmv_d usrsmv usrsmv_d vix vix_d canepu canepu_d usepu usepu_d srv srv_d investment investment_std productivitygrowth productivitygrowth_std bankruptcy bankruptcy_std canbankrate tobinsq tobinsq_d salesgrowth salesgrowth_d cashflow cashflow_d leverage leverage_d tsx tsx_log tsx_dlog canelectionyear fqtr date

save "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\CANVAR.dta", replace

*Aggregating US Data
use "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\USVAR.dta", clear
keep date
save "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\USVAR.dta", replace

use "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\dataset.dta", clear
keep if fic == 2
foreach var of varlist srv investment investment_std productivitygrowth productivitygrowth_std bankruptcy bankruptcy_std tobinsq salesgrowth cashflow leverage {
	bys date: asgen mean_`var' = `var', weight(atq) 
}
bysort date: keep if _n == _N

/// VAR Dataset
merge 1:1 date using "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\USVAR.dta"
sort date
save "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\USVAR.dta", replace
use "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\USVAR.dta", clear

/// Renaming New Variables
foreach var of varlist srv investment investment_std productivitygrowth productivitygrowth_std bankruptcy bankruptcy_std tobinsq salesgrowth cashflow leverage {
	drop `var'
	rename mean_`var' `var'
}

/// Configure as Panel
tsset date

/// Difference Key Variables
foreach var of varlist canrsmv usrsmv vix canepu usepu srv tobinsq salesgrowth cashflow leverage {
	gen `var'_d = d.`var'
}

keep canrsmv canrsmv_d usrsmv usrsmv_d vix vix_d canepu canepu_d usepu usepu_d srv srv_d investment investment_std productivitygrowth productivitygrowth_std bankruptcy bankruptcy_std usfedfundrate tobinsq tobinsq_d salesgrowth salesgrowth_d cashflow cashflow_d leverage sp500 sp500_log sp500_dlog uselectionyear fqtr date

save "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\USVAR.dta", replace