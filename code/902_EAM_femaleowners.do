/**********************************************************************
 Project:    Spring Paper 2026
 Purpose:    Read Excels with female ownership 1998 - 2021
 Author:     IQ
 Created:    2026-02-17
 Updated:    2026-02-17
 Stata Ver:  Stata 18
**********************************************************************/

*==============================*
* 0. Preamble & Housekeeping   *
*==============================*

* Root directory
global ROOT "C:\Users\k_the\main\01_ECMIC_paper"
cd ${ROOT}

version 18.0
clear all
macro drop _all
capture log close _all
set more off
set linesize 255
set scheme s2color
set varabbrev off
set seed 123456
set dp comma

local packages estout reghdfe ivreg2

foreach pkg of local packages {
    capture which `pkg'
    if _rc {
        display "Installing `pkg'..."
        ssc install `pkg', replace
    }
}

*==============================*
* 2021
*==============================*
import excel "data\raw\EAM_anex\Anexos_EAM_desagregacion_variables_2021.xlsx", sheet("4.5") allstring clear
drop if C==""
keep A C D G J M
rename A ciiu
rename D total_owners
keep if C=="Total Grupo"
drop C
destring total_owners G J M, replace
egen women_owners=rowtotal(G J M)
drop G J M
compress
gen ciiu2=substr(ciiu,1,2)
collapse (sum) total_owners women_owners, by(ciiu2)
gen year=2021
drop if ciiu2=="TO"
compress
save "data\proc\EAM_own_2021.dta", replace

*==============================*
* 2020 & 2019
*==============================*
forvalues y=2019/2020 {
	import excel "data\raw\EAM_anex\Anexos_EAM_desagregacion_variables_`y'.xls", sheet("4.5") allstring clear
	drop if C==""
	keep A C D G J M
	rename A ciiu
	rename D total_owners
	keep if C=="TOTAL GRUPO"
	drop C
	destring total_owners G J M, replace
	egen women_owners=rowtotal(G J M)
	drop G J M
	compress
	gen ciiu2=substr(ciiu,1,2)
	collapse (sum) total_owners women_owners, by(ciiu2)
	gen year=`y'
	drop if ciiu2=="TO"
	compress
	save "data\proc\EAM_own_`y'.dta", replace
}
*==============================*
* 2018 - 2015
*==============================*
forvalues y=2015/2018 {
	import excel "data\raw\EAM_anex\Anexos_EAM_desagregacion_variables_`y'.xls", sheet("4.5") allstring clear
	drop if C==""
	keep A C D G J M
	rename A ciiu
	rename D total_owners
	keep if C=="TOTAL "
	drop if ciiu=="TOTAL NACIONAL "
	drop C
	destring total_owners G J M, replace
	egen women_owners=rowtotal(G J M)
	drop G J M
	compress
	gen ciiu2=substr(ciiu,1,2)
	collapse (sum) total_owners women_owners, by(ciiu2)
	gen year=`y'
	compress
	save "data\proc\EAM_own_`y'.dta", replace
}

*==============================*
* 2014 & 2013
*==============================*
forvalues y=13/14 {
	import excel "data\raw\EAM_anex\Anexos EAM 20`y'\c4_5_`y'.xls", sheet("c4_5_`y'_con reserva") allstring clear
	drop if C==""
	keep A C D G J M
	rename A ciiu
	rename D total_owners
	keep if C=="TOTAL"
	drop if ciiu=="TOTAL NACIONAL "
	drop C
	destring total_owners G J M, replace
	egen women_owners=rowtotal(G J M)
	drop G J M
	compress
	gen ciiu2=substr(ciiu,1,2)
	collapse (sum) total_owners women_owners, by(ciiu2)
	gen year=20`y'
	drop if ciiu2=="TO"	
	compress
	save "data\proc\EAM_own_20`y'.dta", replace
}

*==============================*
* 2012
*==============================*
	local y=12
	import excel "data\raw\EAM_anex\Anexos EAM 20`y'\c4_5_`y'_con reserva.xlsx", sheet("c4_5_`y'_con reserva") allstring clear
	drop if C==""
	keep A C D G J M
	rename A ciiu
	rename D total_owners
	keep if C=="TOTAL "
	drop C
	destring total_owners G J M, replace force
	egen women_owners=rowtotal(G J M)
	drop G J M
	compress
	gen ciiu2=substr(ciiu,1,2)
	collapse (sum) total_owners women_owners, by(ciiu2)
	gen year=20`y'
	drop if ciiu2=="TO"
	compress
	save "data\proc\EAM_own_20`y'.dta", replace

*==============================*
* Append All
*==============================*
use "data\proc\EAM_own_2021.dta", clear
forvalues y=2013/2020 {
	append using "data\proc\EAM_own_`y'.dta"
	compress
}
sort year ciiu2
order year ciiu2
gen share_femaleown=(women_owners/total_owners)*100
compress
save "data\proc\EAM_own.dta", replace
	
*==============================*
* END OF DOFILE                *
*==============================*