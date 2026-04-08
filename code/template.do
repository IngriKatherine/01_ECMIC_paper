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

version 18.0
clear all
macro drop _all
capture log close _all
set more off
set linesize 255
set scheme s2color
set varabbrev off
set seed 123456

* Prevent accidental overwrites
set dp comma

*==============================*
* 1. Directory Structure       *
*==============================*

* Root directory (edit once)
global ROOT    "C:/path/to/project"

* Subfolders
global DATA    "$ROOT/data"
global RAW     "$DATA/raw"
global CLEAN   "$DATA/clean"
global OUTPUT  "$ROOT/output"
global FIG     "$OUTPUT/figures"
global TABLE   "$OUTPUT/tables"
global LOG     "$ROOT/logs"
global DO      "$ROOT/do"

*==============================*
* 2. Logging                   *
*==============================*

log using "$LOG/main.log", replace text

display "Do-file started on `c(current_date)' at `c(current_time)'"
display "User: `c(username)'"
display "Stata version: `c(stata_version)'"

*==============================*
* 3. User-Written Packages     *
*==============================*

* Example: check and install required packages
local packages estout reghdfe ivreg2

foreach pkg of local packages {
    capture which `pkg'
    if _rc {
        display "Installing `pkg'..."
        ssc install `pkg', replace
    }
}

*==============================*
* 4. Global Options            *
*==============================*

* Numeric display
set cformat %9.3f
set pformat %9.3f
set sformat %9.3f

* Memory (older Stata versions only)
* set maxvar 20000

*==============================*
* 5. Load Data                 *
*==============================*

* Example:
* use "$RAW/mydata.dta", clear

*==============================*
* 6. Data Cleaning             *
*==============================*

* <Cleaning steps go here>

*==============================*
* 7. Analysis                  *
*==============================*

* <Main analysis goes here>

*==============================*
* 8. Output                    *
*==============================*

* Example:
* esttab using "$TABLE/results.tex", replace

*==============================*
* 9. Wrap-Up                   *
*==============================*

display "Do-file completed successfully."
display "End time: `c(current_time)'"

log close
exit
