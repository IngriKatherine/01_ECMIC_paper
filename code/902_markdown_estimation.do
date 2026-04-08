******************************************************************	
******************************************************************
			
			************************************************************************
			* Substituions (Strategy 3 - S3)
			************************************************************************
			* This method is similar to Method 1. However, if the elasticity is not available at the corresponding level, 
			* use the elasticity from the nearest higher level of aggregation where it is available.			
			
			if ${do_trimm_beta} == 1 {
				
				foreach method in DLW ACF CS CAL1 CAL2 CAL3 DLW_adj ACF_adj {
					
					foreach var in k l m {
						
						if `sector' == 4 {
							
							* Display where you are at!
							dis in red "`method' - `var' - `sector' - `samples'"

							* Creates a new column (source) with empty strings to record the source of the values.
							gen source_`var'_`method'_4 = ""														

							* Marks the source as "4dig" for values that were originally in beta_`var'_`method'_4.
							gen tmp = 1 if missing(beta_`var'_`method'_4)
							replace source_`var'_`method'_4 = "4dig" if tmp != 1
							drop tmp 
							
							* Replace missing values in beta_`var'_`method'_4 with values from beta_`var'_`method'_3.
							gen tmp = 1 if missing(beta_`var'_`method'_4)		
							replace beta_`var'_`method'_4 = beta_`var'_`method'_3 if tmp == 1
							replace source_`var'_`method'_4 = "3dig" if missing(source_`var'_`method'_4) & tmp == 1
							drop tmp
							
							* Replace missing values in beta_`var'_`method'_4 with values from beta_`var'_`method'_2.
							gen tmp = 1 if missing(beta_`var'_`method'_4)
							replace beta_`var'_`method'_4 = beta_`var'_`method'_2 if tmp == 1
							replace source_`var'_`method'_4 = "2dig" if missing(source_`var'_`method'_4) & tmp == 1
							drop tmp

						}
					}
				}
			}	
						
			*=============================
			dis in red "Input expenditure shares"
			*=============================

			* Without the adjustment
			foreach method in DLW ACF CS CAL1 CAL2 CAL3 {

				* Capital input share (WITHOUT error adjustment)	
				gen alpha_k_`method'_`sector' = KEXP/GO

				* Labor input share	(WITHOUT error adjustment)
				gen alpha_l_`method'_`sector' = WB/GO		

				* Intermediate Input Share (WITHOUT error adjustment)
				gen alpha_m_`method'_`sector' = M/GO

			}
						
			* With the adjustment
			foreach method in DLW ACF {

				* Capital input share (WITH error adjustment)
				gen alpha_k_`method'_adj_`sector' = KEXP/(GO/exp(epsilon_`method'_`sector'))		

				* Labor input share	(WITH error adjustment)
 				gen alpha_l_`method'_adj_`sector' = WB/(GO/exp(epsilon_`method'_`sector'))

				* Intermediate Input Share (WITH error adjustment)
				gen alpha_m_`method'_adj_`sector' = M/(GO/exp(epsilon_`method'_`sector'))

			}			
			
			*=============================
			dis in red "Winsorizing input expenditure shares - Alphas" 
			*=============================
			* Note that we winsorize the alphas AFTER the elasticities estimation!

			if ${do_winsor_alpha} == 1 {
				foreach method in DLW ACF CS CAL1 CAL2 CAL3 DLW_adj ACF_adj {
					foreach var in k l m {
			
						* Winsorization (ALL METHODS)
						gen alpha_`var'_`method'_raw_`sector' = alpha_`var'_`method'_`sector'
						gstats winsor alpha_`var'_`method'_`sector' ${WEIGHT_REG}, cuts(${alpha_low} ${alpha_upp}) trim suffix(_trim) 
						
						* Trimming
						replace alpha_`var'_`method'_`sector' = alpha_`var'_`method'_`sector'_trim
						drop alpha_`var'_`method'_`sector'_trim
												
					}
				}
			}
			
			*=============================
			dis in red "Markups and Markdowns - Computation"
			*=============================

			foreach method in DLW ACF CS CAL1 CAL2 CAL3 DLW_adj ACF_adj {

				* Markup calculation (ALL METHODS)
				gen mu_m_`method'_`sector' = (beta_m_`method'_`sector'/alpha_m_`method'_`sector')

				* Markdown calculation (ALL METHODS)
				gen nu_l_`method'_`sector' = (beta_l_`method'_`sector'/alpha_l_`method'_`sector') * (1/mu_m_`method'_`sector')

			}

			*=============================
			dis in red "Markups and Markdowns - Winsorization"
			*=============================

			if ${do_winsor_mu_nu} == 1 {
				
				foreach method in DLW ACF CS CAL1 CAL2 CAL3 DLW_adj ACF_adj {

					* Markups winsorization (without error adjustment)
					gen mu_m_`method'_raw_`sector' = mu_m_`method'_`sector'
					gstats winsor mu_m_`method'_`sector' ${WEIGHT_REG}, cuts(${mu_nu_low} ${mu_nu_upp}) trim 
					
					* Trimming
					replace mu_m_`method'_`sector' = mu_m_`method'_`sector'_tr
					drop mu_m_`method'_`sector'_tr

					* Markdowns calculation (without error adjustment)
					gen nu_l_`method'_raw_`sector' = nu_l_`method'_`sector'	
					gstats winsor nu_l_`method'_`sector' ${WEIGHT_REG}, cuts(${mu_nu_low} ${mu_nu_upp}) trim 
					
					* Trimming
					replace nu_l_`method'_`sector' = nu_l_`method'_`sector'_tr
					drop nu_l_`method'_`sector'_tr

				}
			}
			
			* Create an indicator if all information is available
			foreach method in DLW ACF CS CAL1 CAL2 CAL3 DLW_adj ACF_adj {

				gen sample_mu_m_`method'_`sector' = 1 if mu_m_`method'_`sector' ~= . & beta_m_`method'_`sector' ~= . & alpha_m_`method'_`sector' ~= .
				gen sample_nu_l_`method'_`sector' = 1 if mu_m_`method'_`sector' ~= . & nu_l_`method'_`sector' ~= . & beta_l_`method'_`sector' ~= . & alpha_l_`method'_`sector' ~= .			
						
				tabstat beta_m_`method'_`sector' alpha_m_`method'_`sector' mu_m_`method'_`sector' if sample_mu_m_`method'_`sector' == 1, columns(statistics) statistics(N p25 p50 mean min max) varwidth(24) format(%9.3f)				
				tabstat beta_l_`method'_`sector' alpha_l_`method'_`sector' nu_l_`method'_`sector' mu_m_`method'_`sector' if sample_nu_l_`method'_`sector' == 1, columns(statistics) statistics(N p25 p50 mean min max) varwidth(24) format(%9.3f)				

			}

			*=============================
			dis in red "Firm-Level Results"
			*=============================
			save "$start/input/${CountryName}_MARKETPOWER_`samples'_`sector'DIGIT_SECTOR_${countryISO}_S3_${labor_input}.dta", replace   		
	}
}

***********************************************************
**# Close Log File
***********************************************************
* This section closes the currently open log file, ensuring that all the output
* generated during the execution of the script is properly saved and the log file
* is closed cleanly.
***********************************************************
dis in red "Log close"

* Close the log file
log close
