/* Replication materials for analkysis in
	Yordanova, N., Ershova, A., Khokhlova, A., Obaid Ul-Islam, S. and Glavaš, G. 
	(2025) "Council Responsiveness to Public Opinion: Negotiating European Integration", 
	European Union Politics.
	
This script relies on  the short, act level data. 
 */


// cd "\DIR"
use "Data_linkage_analysis.dta", clear
set scheme burd




/****
- Power & salience weighted avevarge support across MS at t trilogue --- support_trilogue_salience_ms_vot
- EU wide public support at t trilogue--- eu_support_trilogue
- Support in pivot state on policy dimension at t trilogue --- raw_eu_support_trilogue_policy_d 
- Power weighted support at t trilogue --- support_trilogue_ms_vote_weighte
*/



* Online Appendix, Table 9: Descriptive statistics for linkage analysis
estpost tabstat cn_euaction_exp_all support_trilogue_salience_ms_vot eu_salience_trilogue ///
						  com_euaction_exp_all   ep_polarization_proposal  amending ///
						  n_comm_consulted , c(stat) stat( mean sd min max n)
						  
esttab using "descriptive_short.tex", replace ///
 cells("mean(fmt(%6.2fc)) sd(fmt(%6.2fc)) min max count(fmt(%6.0fc))") nonumber ///
  nomtitle nonote noobs label booktabs ///
  collabels("Mean" "SD" "Min" "Max" "N")
  
* Online Appendix, Figure 3: DV1 distribution
hist cn_euaction_exp_all, xtitle("DV1: Council support for EU authority expansion")
graph save Graph "DV_EUexpansion_hist.gph", replace
graph export "DV_EUexpansion_hist.png", as(png) replace


corr support_trilogue_salience_ms_vot  eu_support_trilogue raw_eu_support_trilogue_policy_d support_trilogue_ms_vote_weighte  
pwcorr support_trilogue_salience_ms_vot  eu_support_trilogue raw_eu_support_trilogue_policy_d support_trilogue_ms_vote_weighte , sig

	 
	 
	 
**************************************											  
** Models for Table 11 in Appendix 6.1
**************************************

eststo m1: fracreg logit  cn_euaction_exp_all com_euaction_exp_all ///
						  support_trilogue_salience_ms_vot // * Power & salience weighted av PO across MS						  
//Calculate observation-specific log-likelihoods for Clarke test
predict xb_salience_power
gen lnf_salience_power = cn_euaction_exp_all*log(xb_salience_power) + (1-cn_euaction_exp_all)*log(1-xb_salience_power)
	
*  Figure 1
margins , at(support_trilogue_salience_ms_vot=(0 (.1) 1)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
			plot(level(95) recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) ///
			 xtitle("Power and salience weighted average public support for EU action at t trilogue" )  ///
			 ytitle("Predicted Council support for EU authority expansion")title("" , color(white) ) ///
			 addplot(hist support_trilogue_salience_ms_vot, xlabel(0(0.1)1) freq bfcolor(none) blcolor(gs10)  ///
			 yaxis(2) yscale(alt axis(2)) below legend(off)) saving(file3, replace)  ///
			 legend(off ) )			 
graph export "short_raw_m1_po.png", as(png) replace
	
	
	
eststo m2: fracreg logit  cn_euaction_exp_all com_euaction_exp_all ///
						  eu_support_trilogue if e(sample) // *EU wide PO
//Calculate observation-specific log-likelihoods for Clarke test
predict xb_eu
gen lnf_eu = cn_euaction_exp_all*log(xb_eu) + (1-cn_euaction_exp_all)*log(1-xb_eu)	
	
	
	

* Pivot on policy dimention	
eststo m3: fracreg logit  cn_euaction_exp_all com_euaction_exp_all ///
						  raw_eu_support_trilogue_policy_d 	  if e(sample) // * PO in pivot state on policy dim			
predict xb_pivotPolicy
gen lnf_pivotPolicy = cn_euaction_exp_all*log(xb_pivotPolicy) + (1-cn_euaction_exp_all)*log(1-xb_pivotPolicy)
	
	
	
	
eststo m4: fracreg logit  cn_euaction_exp_all com_euaction_exp_all ///
						  support_trilogue_ms_vote_weighte  	if e(sample) // *Power weighted 
//Calculate observation-specific log-likelihoods for Clarke test
predict xb_power
gen lnf_power = cn_euaction_exp_all*log(xb_salience_power) + (1-cn_euaction_exp_all)*log(1-xb_salience_power)
	
									  

*** Online Appendix, Table 11
estout  m2 m3 m4 m1    ,cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) /// 
  stats(r2 N, fmt(%9.3f %9.0g))  legend  ///
    label varlabels(_cons Constant) ///
varwidth(45)	

						  

// Clarke tests
signtest lnf_salience_power = lnf_eu  if e(sample) == 1
signtest lnf_salience_power = lnf_pivotPolicy if e(sample) == 1
signtest lnf_salience_power = lnf_power  if e(sample) == 1
signtest lnf_pivotPolicy = lnf_eu  if e(sample) == 1
signtest lnf_pivotPolicy = lnf_power  if e(sample) == 1
signtest lnf_power = lnf_eu  if e(sample) == 1

drop  xb_salience_power lnf_salience_power _est_m2 xb_eu lnf_eu xb_pivotPolicy lnf_pivotPolicy _est_m4 xb_power lnf_power  xb_salience lnf_salience _est_m3 _est_m1  
			
	

			
			
			
**************************************											  
** Models for Table 12 in Appendix 6.2
**************************************
eststo m1c: fracreg logit  cn_euaction_exp_all ///
						  c.support_trilogue_salience_ms_vot  ///
						  eu_salience_trilogue ///
						  com_euaction_exp_all ///
						  ep_polarization_proposal ///
						  amending ///
						  n_comm_consulted  
						  
eststo m1a: fracreg logit  cn_euaction_exp_all ///
						   support_trilogue_salience_ms_vot  if e(sample)
						   
eststo m1b: fracreg logit  cn_euaction_exp_all ///
						   support_trilogue_salience_ms_vot  ///
						  com_euaction_exp_all  if e(sample)

eststo m1c: fracreg logit  cn_euaction_exp_all ///
						  c.support_trilogue_salience_ms_vot  ///
						  eu_salience_trilogue ///
						  com_euaction_exp_all ///
						  ep_polarization_proposal ///
						  amending ///
						  n_comm_consulted    if e(sample)	  					  
						  
eststo m1d: fracreg logit  cn_euaction_exp_all ///
						  c.support_trilogue_salience_ms_vot  ///
						  eu_salience_trilogue ///
						  com_euaction_exp_all ///
						  ep_polarization_proposal ///
						  amending ///
						  n_comm_consulted ///
						  i.tristartyear    if e(sample)
						  
eststo m1e: fracreg logit  cn_euaction_exp_all ///
						  c.support_trilogue_salience_ms_vot  ///
						  eu_salience_trilogue ///
						  com_euaction_exp_all ///
						  ep_polarization_proposal ///
						  amending ///
						  n_comm_consulted ///
  						  i.capcode ///
						  i.tristartyear   if e(sample) 


estout  m1a m1b m1c m1d m1e  ,cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) /// 
  stats(N, fmt(%9.3f %9.0g))   drop(*.tristartyear *capcode) legend  ///
  label varlabels(_cons Constant) ///
varwidth(45)			 
				

*** Online Appendix, Table 12
 esttab   m1a m1b m1c m1d m1e    using ///
"Table_short_appendix.tex" ,replace  f  ///
 b(2) p(2) nomtitle label star(* 0.10 ** 0.05 *** 0.01)  drop(*.tristartyear *capcode)	///
 booktabs alignment(D{.}{.}{-1})	


 
 
 
 
 
 
 
 
 
 
 
 
 
  

		  



