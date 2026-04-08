/**********************************************************************
 Project:    Spring Paper 2026
 Purpose:    Merge CompeteLAC and FemaleOwnership
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
* MERGE                *
*==============================*

use "data\proc\industry_markdown_WBUwTA.dta", clear
tostring industry_id, gen(ciiu2)
keep year ciiu2 md_mean md_p50
keep if year>=2013
merge 1:1 year ciiu2 using "data\proc\EAM_own.dta"
sort year ciiu2
order year ciiu2
compress
save "data\proc\industry_markdown_femown.dta", replace

*==============================*
* REG GRAPH
*==============================*
ssc install schemepack, replace
set scheme swift_red

use "data\proc\industry_markdown_femown.dta", clear
drop if md_mean==.  | share_femaleown==.

replace md_mean=1/md_mean
twoway ///
    (scatter md_mean share_femaleown, ///
        mcolor(navy%80) ///
        msymbol(circle)) ///
    (lfitci md_mean share_femaleown, ///
        lcolor(maroon) ///
        fcolor(maroon%20)), ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    xlabel(, format(%9.1f)) ///
    ylabel(, format(%9.1f)) ///
    xtitle("Share of Female Ownership") ///
    ytitle("Mean markdown") ///
    legend(off)

twoway ///
    (scatter md_mean share_femaleown if year==2013, mcolor(%80)) ///
    (scatter md_mean share_femaleown if year==2014, mcolor(%80)) ///
    (scatter md_mean share_femaleown if year==2015, mcolor(%80)) ///
    (scatter md_mean share_femaleown if year==2016, mcolor(%80)) ///
    (scatter md_mean share_femaleown if year==2017, mcolor(%80)) ///
    (scatter md_mean share_femaleown if year==2018, mcolor(%80)) ///
    (scatter md_mean share_femaleown if year==2019, mcolor(%80)) ///
    (scatter md_mean share_femaleown if year==2020, mcolor(%80)) ///
    (scatter md_mean share_femaleown if year==2021, mcolor(%80)) ///
    (lfitci md_mean share_femaleown, ///
        lcolor(black) ///
        fcolor(gs12%30)), ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    xlabel(, format(%9.1f)) ///
    ylabel(, format(%9.1f)) ///
    xtitle("Share of Female Ownership") ///
    ytitle("Mean markdown") ///
    legend(order(1 "2013" 2 "2014" 3 "2015" 4 "2016" 5 "2017" 6 "2018" 7 "2019" 8 "2020" 9 "2021")) ///
	note("Source: Author's calculation using EAM and CompeteLAC." "Note: Markdown with Labor as input, Firm Wage Bill-weighted, method Translog Adjusted")
graph export "figures\industry_markdown_femownshare.png", as(png) name("Graph") replace		

twoway ///
    (scatter md_mean share_femaleown if year<=2015, mcolor(%80)) ///
    (scatter md_mean share_femaleown if year>=2016 & year<=2019, mcolor(%80)) ///
    (scatter md_mean share_femaleown if year>=2020, mcolor(%80)) ///
    (lfitci md_mean share_femaleown, ///
        lcolor(black) ///
        fcolor(gs12%30)), ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    xlabel(, format(%9.1f)) ///
    ylabel(, format(%9.1f)) ///
    xtitle("Share of Female Ownership") ///
    ytitle("Mean markdown") scheme(white_tableau) ///
    legend(order(1 "2013-2015" 2 "2016-2019" 3 "2020-2021")) ///
	note("Source: Author's calculation using EAM and CompeteLAC." "Note: Markdown with Labor as input, Firm Wage Bill-weighted, method Translog Adjusted")
	
	

collapse (mean) md_mean share_femaleown, by(ciiu2)
		
twoway ///
    (scatter md_mean share_femaleown, mcolor(%80)) ///
    (lfitci md_mean share_femaleown, ///
        lcolor(black) ///
        fcolor(gs12%30)), ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    xlabel(, format(%9.1f)) ///
    ylabel(, format(%9.1f)) ///
    ytitle("Mean markdown") ///
    legend(off) scheme(swift_red)
		
*==============================*
* END OF DOFILE                *
*==============================*