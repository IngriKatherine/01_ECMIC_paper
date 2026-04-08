/**********************************************************************
 Project:    Spring Paper 2026
 Purpose:    Uploads all markdown files and creates a single database in Stata
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
* Load Markdowns Data
*==============================*
clear
tempfile master
save `master', emptyok

* Folder containing CSV files
local folder "${ROOT}\data\raw\industry_markdown"

* Get list of CSV files
local files : dir "`folder'" files "*.csv"

foreach f of local files {

    di "Processing file: `f'"

    import delimited "`folder'/`f'", clear stringcols(_all)

    * Create variable with filename (without extension)
    gen source_file = "`f'"

    * Append to master
    append using `master'
    save `master', replace
}

*==============================*
* Clean and save
*==============================*

* Load final appended dataset
use `master', clear
compress
save "data\proc\industry_markdown_all.dta", replace

use "data\proc\industry_markdown_all.dta", clear
keep if pflaborinput=="Labor"
keep if weight=="Wage Bill"
keep if method=="Translog Adjusted"
drop country unit pflaborinput weight method value
destring year, replace
gen industry_id=substr(source_file, 4,2)
destring industry_id, replace
drop source_file
order year industry_id family
sort year industry_id
destring mean sd p1 p5 p10 p25 p50 p75 p90 p95 p99, replace

foreach var of varlist mean sd p1 p5 p10 p25 p50 p75 p90 p95 p99 {
	rename `var' md_`var'
}
compress
save "data\proc\industry_markdown_WBUwTA.dta", replace

*==============================*
* END OF DOFILE                *
*==============================*