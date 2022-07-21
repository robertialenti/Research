*Logarithmic Transformations
replace canrsmv = canrsmv*1000
replace usrsmv = usrsmv*1000
foreach var of varlist atq firmage stockmarket {
	gen `var'_log = log(`var')
}

*Constructing Lagged Variables
foreach var of varlist atq_log firmage_log salesgrowth investment productivitygrowth bankruptcy cashflow leverage tobinsq {
	bys gvkey (date): gen `var'_l = l.`var'
}

*Constructing and Standardizing Interaction Independent Variables
foreach var of varlist canrsmv usrsmv vix canepu usepu srv {
   qui gen `var'_atq = `var' * atq_log_l
   qui egen `var'_std_atq = std(`var'_atq)
   
   qui gen `var'_firmage = `var' * firmage_log_l
   qui egen `var'_std_firmage = std(`var'_firmage)
}

*Standardizing Variables
foreach var of varlist canrsmv usrsmv vix canepu usepu srv atq_log_l firmage_log_l salesgrowth_l cashflow_l leverage_l tobinsq_l bankrate stockmarket stockmarket_log stockmarket_dlog investment investment_l productivitygrowth productivitygrowth_l bankruptcy bankruptcy_l {
   qui egen `var'_std = std(`var') 
}

*Constructing Interaction Fixed Effects Variables
gen datecountry = date*fic
gen dateindustry = date*industry

*Panel Regressions by Country
sort gvkey date
/// Investment Rate
/// Baseline
gen it = 1
matrix investment_matrix = J(6,6,.)
forvalues i = 1/2 {
	 foreach var of varlist canrsmv_std usrsmv_std vix_std canepu_std usepu_std srv_std {
		qui xtreg investment_std `var' `var'_atq `var'_firmage atq_log_l_std firmage_log_l_std salesgrowth_l_std i.fqtr i.dateindustry [aweight = avgatq_`i'] if fic == `i', fe vce(cluster gvkey)
		qui scalar coeff = e(b)[1,1]
		qui matrix define results = r(table)
		qui scalar se1 = r(table)[5,1]
		qui scalar se2 = r(table)[6,1]
		if `i' == 1 qui matrix investment_matrix[it,`i'] = coeff
		if `i' == 1 qui matrix investment_matrix[it,`i' + 1] = se1
		if `i' == 1 qui matrix investment_matrix[it,`i' + 2] = se2
		if `i' == 2 qui matrix investment_matrix[it,`i' + 2] = coeff
		if `i' == 2 qui matrix investment_matrix[it,`i' + 3] = se1
		if `i' == 2 qui matrix investment_matrix[it,`i' + 4] = se2
		disp it
		qui replace it = it + 1
		qui replace it = 1 if it == 7
		}	
}
matrix list investment_matrix
drop it

/// Augmented
gen it = 1
matrix investment_matrix = J(6,6,.)
forvalues i = 1/2 {
	 foreach var of varlist canrsmv_std usrsmv_std vix_std canepu_std usepu_std srv_std {
		qui xtreg investment_std `var' `var'_atq `var'_firmage atq_log_l_std firmage_log_l_std salesgrowth_l_std cashflow_l_std leverage_l_std tobinsq_l_std bankrate_std stockmarket_dlog_std electionyear i.fqtr i.dateindustry [aweight = avgatq_`i'] if fic ==  `i', fe vce(cluster gvkey)
		qui scalar coeff = e(b)[1,1]
		matrix define results = r(table)
		qui scalar se1 = r(table)[5,1]
		qui scalar se2 = r(table)[6,1]
		if `i' == 1 qui matrix investment_matrix[it,`i'] = coeff
		if `i' == 1 qui matrix investment_matrix[it,`i' + 1] = se1
		if `i' == 1 qui matrix investment_matrix[it,`i' + 2] = se2
		if `i' == 2 qui matrix investment_matrix[it,`i' + 2] = coeff
		if `i' == 2 qui matrix investment_matrix[it,`i' + 3] = se1
		if `i' == 2 qui matrix investment_matrix[it,`i' + 4] = se2
		disp it
		qui replace it = it + 1
		qui replace it = 1 if it == 7
}	
}
matrix list investment_matrix
drop it

/// Productivity Growth
/// Baseline
gen it = 1
matrix productivity_matrix = J(6,6,.)
forvalues i = 1/2 {
	 foreach var of varlist canrsmv_std usrsmv_std vix_std canepu_std usepu_std srv_std {
		qui xtreg productivitygrowth_std `var' `var'_atq `var'_firmage atq_log_l_std firmage_log_l_std salesgrowth_l_std i.fqtr i.dateindustry [aweight = avgatq_`i'] if fic == `i', fe vce(cluster gvkey)
		qui scalar coeff = e(b)[1,1]
		qui matrix define results = r(table)
		qui scalar se1 = r(table)[5,1]
		qui scalar se2 = r(table)[6,1]
		if `i' == 1 qui matrix productivity_matrix[it,`i'] = coeff
		if `i' == 1 qui matrix productivity_matrix[it,`i' + 1] = se1
		if `i' == 1 qui matrix productivity_matrix[it,`i' + 2] = se2
		if `i' == 2 qui matrix productivity_matrix[it,`i' + 2] = coeff
		if `i' == 2 qui matrix productivity_matrix[it,`i' + 3] = se1
		if `i' == 2 qui matrix productivity_matrix[it,`i' + 4] = se2
		disp it
		qui replace it = it + 1
		qui replace it = 1 if it == 7
		}	
}
matrix list productivity_matrix
drop it

/// Augmented
gen it = 1
matrix productivity_matrix = J(6,6,.)
forvalues i = 1/2 {
	 foreach var of varlist canrsmv_std usrsmv_std vix_std canepu_std usepu_std srv_std {
		qui xtreg productivitygrowth_std `var' `var'_atq `var'_firmage atq_log_l_std firmage_log_l_std salesgrowth_l_std cashflow_l_std leverage_l_std tobinsq_l_std bankrate_std stockmarket_dlog_std electionyear i.fqtr i.dateindustry [aweight = avgatq_`i'] if fic ==  `i', fe vce(cluster gvkey)
		qui scalar coeff = e(b)[1,1]
		qui matrix define results = r(table)
		qui scalar se1 = r(table)[5,1]
		qui scalar se2 = r(table)[6,1]
		if `i' == 1 qui matrix productivity_matrix[it,`i'] = coeff
		if `i' == 1 qui matrix productivity_matrix[it,`i' + 1] = se1
		if `i' == 1 qui matrix productivity_matrix[it,`i' + 2] = se2
		if `i' == 2 qui matrix productivity_matrix[it,`i' + 2] = coeff
		if `i' == 2 qui matrix productivity_matrix[it,`i' + 3] = se1
		if `i' == 2 qui matrix productivity_matrix[it,`i' + 4] = se2
		disp it
		qui replace it = it + 1
		qui replace it = 1 if it == 7
		}	
}
matrix list productivity_matrix
drop it

/// Bankruptcy Risk
/// Baseline
gen it = 1
matrix bankruptcy_matrix = J(6,6,.)
forvalues i = 1/2 {
	 foreach var of varlist canrsmv_std usrsmv_std vix_std canepu_std usepu_std srv_std {
		qui xtreg bankruptcy_std `var' `var'_atq `var'_firmage atq_log_l_std firmage_log_l_std salesgrowth_l_std i.fqtr i.date [aweight = avgatq_`i'] if fic ==  `i', fe vce(cluster gvkey)
		qui scalar coeff = e(b)[1,1]
		qui matrix define results = r(table)
		qui scalar se1 = r(table)[5,1]
		qui scalar se2 = r(table)[6,1]
		if `i' == 1 qui matrix bankruptcy_matrix[it,`i'] = coeff
		if `i' == 1 qui matrix bankruptcy_matrix[it,`i' + 1] = se1
		if `i' == 1 qui matrix bankruptcy_matrix[it,`i' + 2] = se2
		if `i' == 2 qui matrix bankruptcy_matrix[it,`i' + 2] = coeff
		if `i' == 2 qui matrix bankruptcy_matrix[it,`i' + 3] = se1
		if `i' == 2 qui matrix bankruptcy_matrix[it,`i' + 4] = se2
		disp it
		qui replace it = it + 1
		qui replace it = 1 if it == 7
		}	
}
matrix list bankruptcy_matrix
drop it

/// Augmented
gen it = 1
matrix bankruptcy_matrix = J(6,6,.)
forvalues i = 1/2 {
	 foreach var of varlist canrsmv_std usrsmv_std vix_std canepu_std usepu_std srv_std {
		qui xtreg bankruptcy_std `var' `var'_atq `var'_firmage atq_log_l_std firmage_log_l_std salesgrowth_l_std cashflow_l_std leverage_l_std tobinsq_l_std bankrate_std stockmarket_dlog_std electionyear i.fqtr i.dateindustry [aweight = avgatq_`i'] if fic ==  `i', fe vce(cluster gvkey)
		qui scalar coeff = e(b)[1,1]
		qui matrix define results = r(table)
		qui scalar se1 = r(table)[5,1]
		qui scalar se2 = r(table)[6,1]
		if `i' == 1 qui matrix bankruptcy_matrix[it,`i'] = coeff
		if `i' == 1 qui matrix bankruptcy_matrix[it,`i' + 1] = se1
		if `i' == 1 qui matrix bankruptcy_matrix[it,`i' + 2] = se2
		if `i' == 2 qui matrix bankruptcy_matrix[it,`i' + 2] = coeff
		if `i' == 2 qui matrix bankruptcy_matrix[it,`i' + 3] = se1
		if `i' == 2 qui matrix bankruptcy_matrix[it,`i' + 4] = se2
		disp it
		qui replace it = it + 1
		qui replace it = 1 if it == 7
		}	
}
matrix list bankruptcy_matrix
drop it

save "C:\Users\Robert\OneDrive\Desktop\Bobby\School\MA Economics\Research\dataset.dta", replace