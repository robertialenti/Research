*Number of Firms and Firm-Year Observations
by gvkey, sort: gen nvals = _n == 1
count if nvals & fic == 1
count if nvals & fic == 2

*Descriptive Statistics 
/// Constructing Weights by Total Assets
bysort gvkey (date): egen avgatq = mean(atq)

/// Firm-Level Variables
foreach var of varlist firmage atq salesgrowth liquidity cashflow leverage tobinsq investment productivitygrowth bankruptcy  {
	bys fic: sum `var' [aweight = avgatq], d
}

/// Uncertainty Measures
foreach var of varlist canrsmv usrsmv vix canepu usepu srv {
    qui sum `var' if date == tq(2013q1)
	qui scalar `var'_100 = r(mean)
    qui gen `var'_index = (`var'/`var'_100)*100
}

foreach var of varlist canrsmv usrsmv vix canepu usepu srv {
	sum `var'_index, d
}

/// Macroeconomic Variables
foreach var of varlist canbankrate usfedfundrate tsx sp500 {
	sum `var', d
}

*Sectoral Distribution
bys fic: tab industry

*Correlation Matrix
bys fic: corr investment productivitygrowth bankruptcy canrsmv usrsmv vix canepu usepu srv

*Graphing Firm-Level Variables 
foreach var of varlist investment productivitygrowth bankruptcy {
    bysort gvkey (date): gen `var'_MA4 = (`var'[_n-4] + `var'[_n-3] + `var'[_n-2] + `var'[_n-1])/4
}

/// Investment Rate
replace investment_MA4 = investment_MA4 * 100
preserve 
collapse investment_MA4 [aweight = avgatq], by(date fic)
graph twoway ///
(line investment_MA4 date if fic == 1, legend(label(1 "Canada")) lcolor(cranberry)) ///
(line investment_MA4 date if fic == 2, legend(label(2 "US")) lcolor(edkblue) ///
title("") ///
xtitle("") ///
ytitle("Investment Growth Rate (%)") ///
legend(cols(2) region(lstyle(none))))
restore

/// Productivity Growth
replace productivitygrowth_MA4 = productivitygrowth_MA4 * 100
preserve 
collapse productivitygrowth_MA4 [aweight = avgatq], by(date fic)
graph twoway ///
(line productivitygrowth_MA4 date if fic == 1, legend(label(1 "Canada")) lcolor(cranberry)) ///
(line productivitygrowth_MA4 date if fic == 2, legend(label(2 "US")) lcolor(edkblue) ///
title("") ///
xtitle("") ///
ytitle("Productivity Growth Rate (%)") ///
legend(cols(2) region(lstyle(none)))) 
restore

/// Bankruptcy
preserve
collapse bankruptcy_MA4 [aweight = avgatq], by(date fic)
graph twoway ///
(line bankruptcy_MA4 date if fic == 1 & tin(2000q1, 2022q1), legend(label(1 "Canada")) lcolor(cranberry)) ///
(line bankruptcy_MA4 date if fic == 2 & tin(2000q1, 2022q1), legend(label(2 "US")) lcolor(edkblue) /// 
title("") ///
xtitle("") ///
ytitle("Altman Z-Score") ///
legend(cols(2) region(lstyle(none))))
restore

/// Indexing Uncertainty Measures
foreach var of varlist canrsmv usrsmv vix canepu usepu srv {
    bysort gvkey (date): gen `var'_MA4 = (`var'[_n-4] + `var'[_n-3] + `var'[_n-2] + `var'[_n-1])/4
    qui sum `var'_MA4 if date == tq(2013q1)
	qui scalar `var'_MA4_100 = r(mean)
    qui gen `var'_index_MA4 = (`var'_MA4/`var'_MA4_100)*100
	qui drop `var'_MA4
}

/// Uncertainty Measures 
gen upper = 400
preserve
collapse canrsmv_index_MA4 usrsmv_index_MA4 vix_index_MA4 canepu_index_MA4 usepu_index_MA4 srv_index_MA4 upper [aweight = avgatq], by(date)
local barcall upper date if inrange(date, tq(1981q4), tq(1982q4)) | inrange(date, tq(1990q4), tq(1991q1)) | inrange(date, tq(2001q2), tq(2001q4)) | inrange(date, tq(2008q1), tq(2009q2)) | inrange(date, tq(2020q1), tq(2020q2)), bcolor(gs14) base(0) plotregion(margin(0.25 0.25 0.25 0.25))
graph twoway ///
(bar `barcall') ///
(line canrsmv_index_MA4 date, legend(label(2 "CAN RSMV")) lcolor(red)) ///
(line usrsmv_index_MA4 date, legend(label(3 "US RSMV")) lcolor(blue)) ///
(line vix_index_MA4 date, legend(label(4 "VIX")) lcolor(green)) ///
(line canepu_index_MA4 date, legend(label(5 "CAN EPU")) lcolor(orange)) ///
(line usepu_index_MA4 date, legend(label(6 "US EPU")) lcolor(pink)) ///
(line srv_index_MA4 date, legend(label(7 "SRV")) lcolor(black) ///
title("") ///
xtitle("") ///
ytitle("") ///
legend(order(2 3 4 5 6 7) cols(3) region(lstyle(none)))) 
restore
drop upper