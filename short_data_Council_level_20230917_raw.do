/* 
Replication materials for the paper "Council Responsiveness to Public Opinion: 
	Negotiating European Integration" by Yordanova, N.; Ershova, A. and Khokhlova, A. 
	
This is the script that relies on the short, aggregated act level  data and raw (not imputed) PO values

Written by Nikoleta Yordanova on 11 Sept 2023.

Last updated 6 October 2023) */


// cd "\DIR"
cd "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper"


* import delimited "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Council_data_main_24082023.csv", delimiter(comma) clear 
import delimited "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\20230912_council_short_raw_sd.csv", delimiter(comma) clear 

// import "20230912_council_short_raw_sd.csv", delimiter(comma) clear 


*ssc install scheme-burd
set scheme burd

* gen numeric for ep term
gen ep_term_pr_num= 1 if  ep_term_proposal== "EP6"
replace ep_term_pr_num= 2 if  ep_term_proposal== "EP7_1"
replace ep_term_pr_num= 3 if  ep_term_proposal== "EP7_2"
replace ep_term_pr_num= 4 if  ep_term_proposal== "EP8"
replace ep_term_pr_num= 5 if  ep_term_proposal== "EP9"

*foreach var of varlist * {
*cap replace `var' = "" if `var'=="NA"
*}

* generate the difference in the public support from tri to proposal
gen eu_support_change = eu_support_trilogue- eu_support_proposal
* support with the imputed values
gen eu_change_support = eu_support_trilogue- eu_support_proposal

*generate chage in salience variable as a diff between salience at trilogue  & salience at proposal
gen eu_salience_change= eu_salience_trilogue- eu_salience_proposal
* imputed values for salience
gen eu_salience_change_imp= eu_salience_trilogue- eu_salience_proposal	 

* gen indicator for ll consulted committees 
gen full_consulted_coms= n_comm_consulted+n_comm_consulted_former

* gen indicator for the instrument 
gen leg_inst_num =1 if leg_instr=="Decision"
replace leg_inst_num =2 if leg_instr=="Directive"
replace leg_inst_num =3 if leg_instr=="Regulation"

gen num_competence_years = tristartyear - competence_year

* New DV change in pro EU authority expansion between text of COM and text of CN 
*gen diff_cn_com_euaction_exp_all = cn_euaction_exp_all - com_euaction_exp_all


rename v68 support_trilogue_policy_pos_raw
rename v67 support_trilogue_eu_pos_wei_raw

save "short_raw.dta", replace

*  ms_minority = ((N MS in which PO t trilogue < 0.5 if mean(MS PO)>=0.5 ) + (N MS in which PO t trilogue >0.5 if mean(MS PO)<=0.5 ) ) / ( N MS )



cd "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper"
use "short_raw.dta", clear
set scheme burd



* Descriptive statistics

 *********************************************decriptive
 
 

 *********************************************decriptive
 
  
  estpost tabstat cn_euaction_exp_all eu_support_trilogue eu_salience_trilogue ///
						  com_euaction_exp_all   ep_polarization_proposal  amending ///
						  n_comm_consulted , c(stat) stat( mean sd min max n)
 
 
  esttab using "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Latex tables\descriptive_short.tex", replace ///
 cells("mean(fmt(%6.2fc)) sd(fmt(%6.2fc)) min max count(fmt(%6.0fc))") nonumber ///
  nomtitle nonote noobs label booktabs ///
  collabels("Mean" "SD" "Min" "Max" "N")
  

	
** TO DO -- get in var on the public opinion in the MS of the MS holding the Presidency at t trilogue
* gen policy position of country of the presidency 
*gen presid_gov_policy = gov_policy_positionstriimp if country_str  == country_presid_nego_start & presidency_dummy==1
*bysort cod: egen test=min(presid_gov_policy)
*drop presid_gov_policy
*rename test presid_gov_policy

hist cn_euaction_exp_all, xtitle("DV1: Propensity of Council to support EU authority expansion")
graph save Graph "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Figures_short\DV_EUexpansion_hist.gph", replace
graph export "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Figures_short\DV_EUexpansion_hist.png", as(png) replace



corr cn_euaction_exp_all eu_support_trilogue support_trilogue_policy_position ///
		support_trilogue_eu_position_wei  support_trilogue_salience_weight ///
		support_trilogue_salience_ms_vot  ///
		raw_eu_support_trilogue_eu_dimen raw_eu_support_trilogue_policy_d ///
		 support_trilogue_ms_vote_weighte

		

corr cn_euaction_exp_all eu_support_trilogue support_trilogue_policy_position ///
		support_trilogue_eu_position_wei  support_trilogue_salience_weight ///
		support_trilogue_salience_ms_vot  ///
		raw_eu_support_trilogue_eu_dimen raw_eu_support_trilogue_policy_d ///
     support_trilogue_ms_vote_weighte if ms_minority_sd>0.1

	 
	 
	 
	 
	 
	 
**************
*** MODELS ***
**************


/* MODELS

I BARGAINING/COOPERATIVE models:

1) Consensus model:
IV 1: Average public support (not weighted) across the MS -- EU-wide PO
*eu_support_trilogue
*/
					  
eststo m1a: fracreg logit  cn_euaction_exp_all ///
						  c.eu_support_trilogue  
						  *[pw =cn_meaning_weight]  if ms_minority_sd>0.10
						   
eststo m1b: fracreg logit  cn_euaction_exp_all ///
						  c.eu_support_trilogue  ///
						  com_euaction_exp_all
						  *[pw =cn_meaning_weight]  if ms_minority_sd>0.10
****Public support****
 margins , at(eu_support_trilogue=(0 (.1) 1)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
			plot(level(95) recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) ///
			 xtitle("EU-wide level of public support for EU authority at t trilogue" )  ///
			 ytitle("Classified Council pro-EU authority expansion position")title("" , color(white) ) addplot(hist eu_support_trilogue, xlabel(0(0.1)1) freq bfcolor(none) blcolor(gs10)  ///
			 yaxis(2) yscale(alt axis(2)) below legend(off)) saving(file3, replace)  ///
			 legend(off ) )
			 
graph export "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Figures_short\short_raw_m1b_po.png", as(png) replace


eststo m1c: fracreg logit  cn_euaction_exp_all ///
						  c.eu_support_trilogue  ///
						  eu_salience_trilogue ///
						  com_euaction_exp_all ///
						  ep_polarization_proposal ///
						  amending ///
						  n_comm_consulted  
						  *[pw =cn_meaning_weight]  if ms_minority_sd>0.10

****Public support****
 margins , at(eu_support_trilogue=(0 (.1) 1)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
			plot(level(95) recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) ///
			 xtitle("EU-wide level of public support for EU authority at t trilogue" )  ///
			 ytitle("Classified Council pro-EU authority expansion position")title("" , color(white) ) addplot(hist eu_support_trilogue, xlabel(0(0.1)1) freq bfcolor(none) blcolor(gs10)  ///
			 yaxis(2) yscale(alt axis(2)) below legend(off)) saving(file3, replace)  ///
			 legend(off ) )
			 
graph export "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Figures_short\short_raw_m1c_po.png", as(png) replace


					  
*** as latex--> Main text table
 esttab   m1a  m1b   using ///
"C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Latex tables\Table_short_main.tex" ,replace  f  ///
 b(2) p(2) nomtitle label star(* 0.10 ** 0.05 *** 0.01)  	///
 booktabs alignment(D{.}{.}{-1})	
 
					
eststo m1d: fracreg logit  cn_euaction_exp_all ///
						  c.eu_support_trilogue  ///
						  eu_salience_trilogue ///
						  com_euaction_exp_all ///
						  ep_polarization_proposal ///
						  amending ///
						  n_comm_consulted ///
						  i.capcode if e(sample)
						  
eststo m1: fracreg logit  cn_euaction_exp_all ///
						  c.eu_support_trilogue  ///
						  eu_salience_trilogue ///
						  com_euaction_exp_all ///
						  ep_polarization_proposal ///
						  amending ///
						  n_comm_consulted ///
						  i.tristartyear  ///
						  i.capcode if e(sample)
						  
						  

estout  m1a m1b m1c m1d m1  ,cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) /// 
  stats(N, fmt(%9.3f %9.0g))   drop(*.tristartyear *capcode) legend  ///
  label varlabels(_cons Constant) ///
varwidth(45)			 
				

*** as latex --> for Appendix
 esttab   m1a m1b m1c m1d m1    using ///
"C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Latex tables\Table_short_appendix.tex" ,replace  f  ///
 b(2) p(2) nomtitle label star(* 0.10 ** 0.05 *** 0.01)  drop(*.tristartyear *capcode)	///
 booktabs alignment(D{.}{.}{-1})	




 
 
 
 
 
 
** Rerun for Clarke test

eststo m5: fracreg logit  cn_euaction_exp_all ///
						  c.support_trilogue_ms_vote_weighte ///
						  eu_salience_trilogue ///
						  com_euaction_exp_all ///
						  ep_polarization_proposal ///
						  amending ///
						  n_comm_consulted 
						  /// 						  * [pw =cn_meaning_weight] ///
						  *if ms_minority_sd>0.10
						  
						  
eststo m1: fracreg logit  cn_euaction_exp_all ///
						  eu_salience_trilogue ///
						  com_euaction_exp_all ///
						  ep_polarization_proposal ///
						  amending ///
						  n_comm_consulted ///
  						  c.eu_support_trilogue  ///
						  if e(sample)
						  * capcode!=6 & capcode!=10 & capcode!=23 
						  ///
						  *[pw =cn_meaning_weight]  if ms_minority_sd>0.10
						  *if e(sample) 
						  

//Calculate observation-specific log-likelihoods for Clarke test
predict xb_eu
gen lnf_eu = cn_euaction_exp_all*log(xb_eu) + (1-cn_euaction_exp_all)*log(1-xb_eu)
						  


			 
						  
/* 2) Europhile coalition model:
IV2: Weighted average public support across MS,  where MS Govt's EU positions (or, POLICY position --> so two separate IVs, respectively below) serve as the weights. To create the weights, GOVt's EU (policy) positions need to be normalized between 0 and 1 --- or some such), (edited) 
*** Public support for EU policy action across EU as a weighted average of Public support across MS, with MS Govt's EU (policy action) positions as weights
* 2a) support_trilogue_eu_position_wei 
* support_trilogue_policy_position
*/

eststo m2a: fracreg logit  cn_euaction_exp_all ///
						  c.support_trilogue_policy_pos_raw  ///
						  eu_salience_trilogue ///
						  com_euaction_exp_all ///
						  ep_polarization_proposal ///
						  amending ///
						  n_comm_consulted ///
						  if e(sample)

//Calculate observation-specific log-likelihoods for Clarke test
predict xb_govPolicyW
gen lnf_govPolicyW = cn_euaction_exp_all*log(xb_govPolicyW) + (1-cn_euaction_exp_all)*log(1-xb_govPolicyW)

						  

margins , at(support_trilogue_policy_pos_raw=(0 (.1) 1)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
			plot(level(90) recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) ///
			 xtitle("Weigted Av public support for EU authority at t trilogue across MS" "(Gov EU position weights)")  ///
			 ytitle("Classified Council pro-EU authority expansion position")title("" , color(white)) ///
			 legend(off ) )						  



eststo m2b: fracreg logit  cn_euaction_exp_all ///
						  c.support_trilogue_eu_pos_wei_raw  ///
						  eu_salience_trilogue ///
						  com_euaction_exp_all ///
						  ep_polarization_proposal ///
						  amending ///
						  n_comm_consulted if e(sample)

//Calculate observation-specific log-likelihoods for Clarke test
predict xb_govEUW
gen lnf_govEUW = cn_euaction_exp_all*log(xb_govEUW) + (1-cn_euaction_exp_all)*log(1-xb_govEUW)


margins , at(support_trilogue_eu_pos_wei_raw=(0 (.1) 1)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
			plot(level(90) recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) ///
			 xtitle("Weigted Av public support for EU authority at t trilogue across MS" "(Gov POLICY position weights)")  ///
			 ytitle("Classified Council pro-EU authority expansion position")title("" , color(white)) ///
			 legend(off ) )	
/* 3) salience model:
IV3a: Weighted average public support across MS, where Salience levels in MS serve as the Weights (edited) 
*** Public support for EU policy action as a weighted average of Public support across MS, with public salience in MS as weights
*support_trilogue_salience_weight

IV3b: Weighted average public support across MS, where Salience levels in MS as well as MS voting weights serve as the Weights (edited) 
*** Public support for EU policy action as a weighted average of Public support across MS, with public salience in MS and MS voting powers as weights 
*support_trilogue_salience_ms_vot
*/


eststo m3a: fracreg logit  cn_euaction_exp_all ///
						  c.support_trilogue_salience_weight  ///
						  eu_salience_trilogue ///
						  com_euaction_exp_all ///
						  ep_polarization_proposal ///
						  amending ///
						  n_comm_consulted  if e(sample)

//Calculate observation-specific log-likelihoods for Clarke test
predict xb_salience
gen lnf_salience = cn_euaction_exp_all*log(xb_salience) + (1-cn_euaction_exp_all)*log(1-xb_salience)

margins , at(support_trilogue_salience_weight=(0 (.1) 1)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
			plot(level(90) recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) ///
			 xtitle("Weigted Av public support for EU authority at t trilogue across MS" "(Piblic salience in MS weights)")  ///
			 ytitle("Classified Council pro-EU authority expansion position")title("" , color(white)) ///
			 legend(off ) )	

			 
eststo m3b: fracreg logit  cn_euaction_exp_all ///
						  c.support_trilogue_salience_ms_vot  ///
						  eu_salience_trilogue ///
						  com_euaction_exp_all ///
						  ep_polarization_proposal ///
						  amending ///
						  n_comm_consulted if e(sample)
//Calculate observation-specific log-likelihoods for Clarke test
predict xb_salience_power
gen lnf_salience_power = cn_euaction_exp_all*log(xb_salience_power) + (1-cn_euaction_exp_all)*log(1-xb_salience_power)

margins , at(support_trilogue_salience_ms_vot=(0 (.1) 1)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
			plot(level(90) recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) ///
			 xtitle("Weigted Av public support for EU authority at t trilogue across MS" "(Piblic salience in MS & MS voting weights)")  ///
			 ytitle("Classified Council pro-EU authority expansion position")title("" , color(white)) ///
			 legend(off ) )	

			 /* 
PROCEDURAL/NON-COOPERATIVE models:




4) Pivot wins
IV4: Public support in the Council pivotal state for EU authority expansion (the most Europhile or the Eurosceptic states)   /// respectively for Council pivot on policy dimension (edited) 

*** Public support for EU policy action in PIVOT MS in CN on EU (policy) dimension  at t of trilogue
* raw_eu_support_trilogue_eu_dimen
* raw_eu_support_trilogue_policy_d 
*/
//
eststo m4a: fracreg logit  cn_euaction_exp_all ///
						  c.raw_eu_support_trilogue_eu_dimen  ///
						  eu_salience_trilogue ///
						  com_euaction_exp_all ///
						  ep_polarization_proposal ///
						  amending ///
						  n_comm_consulted ///
						   if e(sample)

//Calculate observation-specific log-likelihoods for Clarke test
predict xb_pivotEU
gen lnf_pivotEU = cn_euaction_exp_all*log(xb_pivotEU) + (1-cn_euaction_exp_all)*log(1-xb_pivotEU)


margins , at(raw_eu_support_trilogue_eu_dimen=(0 (.1) 1)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
			plot(level(90) recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) ///
			 xtitle("Public support for EU authority at t trilogue in pivot MS on EU dim")  ///
			 ytitle("Classified Council pro-EU authority expansion position")title("" , color(white)) ///
			 legend(off ) )	
//
//			 
// eststo m4b: fracreg logit  cn_euaction_exp_all ///
// 						  c.raw_eu_support_trilogue_policy_d  ///
// 						  eu_salience_trilogue ///
// 						  com_euaction_exp_all ///
// 						  ep_polarization_proposal ///
// 						  amending ///
// 						  n_comm_consulted ///
// 						  if e(sample)

// //Calculate observation-specific log-likelihoods for Clarke test
// predict xb_pivotPolicy
// gen lnf_pivotPolicy = cn_euaction_exp_all*log(xb_pivotPolicy) + (1-cn_euaction_exp_all)*log(1-xb_pivotPolicy)


// margins , at(raw_eu_support_trilogue_policy_d=(0 (.1) 1)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
// 			plot(level(90) recast(line) plotopts(lwidth(vthin)) ///
// 			 recastci(rarea) ciopts(fcolor(%20) ///
// 			 lcolor(none) lwidth(none) lpattern(dot)) ///
// 			 xtitle("Public support for EU authority at t trilogue in pivot MS on POLICY dim")  ///
// 			 ytitle("Classified Council pro-EU authority expansion position")title("" , color(white)) ///
// 			 addplot(hist eu_support_trilogue, xlabel(0(0.1)1) freq bfcolor(none) blcolor(gs10)  ///
// 			 yaxis(2) yscale(alt axis(2)) below legend(off)) saving(file3, replace)  ///
// 			 legend(off ) )	

			 
			 
/* 5) Weighted average wins
IV5: Weighted average public support in the Council, where only MS voting powers serve as weights (edited) 

*** Public support for EU policy action as a weighted average of Public support across MS, with MS voting powers as weights
*support_trilogue_ms_vote_weighte
*/


eststo m5: fracreg logit  cn_euaction_exp_all ///
						  c.support_trilogue_ms_vote_weighte  ///
						  eu_salience_trilogue ///
						  com_euaction_exp_all ///
						  ep_polarization_proposal ///
						  amending ///
						  n_comm_consulted ///
						  if e(sample)

//Calculate observation-specific log-likelihoods for Clarke test
predict xb_power
gen lnf_power = cn_euaction_exp_all*log(xb_power) + (1-cn_euaction_exp_all)*log(1-xb_power)

margins , at(support_trilogue_ms_vote_weighte=(0 (.1) 1)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
			plot(level(90) recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) ///
			 xtitle("Weigted Av public support for EU authority at t trilogue across MS" "(MS voting weights)")  ///
			 ytitle("Classified Council pro-EU authority expansion position")title("" , color(white)) ///
			 addplot(hist eu_support_trilogue, xlabel(0(0.1)1) freq bfcolor(none) blcolor(gs10)  ///
			 yaxis(2) yscale(alt axis(2)) below legend(off)) saving(file3, replace)  ///
			 legend(off ) )	
			 
		 
estout  m1 m2a m2b m3a m3b m5   ,cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) /// 
  stats(N, fmt(%9.3f %9.0g))  legend  ///
    label varlabels(_cons Constant) ///
varwidth(45)	


estout m1 m4a m5 m2b m3a m3b   ,cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) /// 
  stats(N, fmt(%9.3f %9.0g))  legend  ///
    label varlabels(_cons Constant) ///
varwidth(45)	


*** as latex --> for Appendix 2
 esttab   m1 m4a m5 m2b m3a m3b    using ///
"C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Latex tables\Table_short_appendix2.tex" ,replace  f  ///
 b(2) p(2) nomtitle label star(* 0.10 ** 0.05 *** 0.01) 	///
 booktabs alignment(D{.}{.}{-1})	



sum 	    eu_support_trilogue /// H1
			support_trilogue_policy_pos_raw /// 2a
			support_trilogue_eu_pos_wei_raw /// 2b   ---->> !!! Only 174 observations while all others have 299 or 300 !!
			support_trilogue_salience_weight /// 3a
			support_trilogue_salience_ms_vot /// 3b
			raw_eu_support_trilogue_eu_dimen /// 4a
			support_trilogue_ms_vote_weighte 
			*if e(sample)	
			*H5


//   drop(*.tristartyear *capcode) 
		 
				
*** as latex
* esttab   m1    using ///
*"C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Latex tables\Table_short.tex" ,replace  f  ///
* b(2) p(2) nomtitle label star(* 0.10 ** 0.05 *** 0.01)  drop(*.tristartyear *capcode)	///
* booktabs alignment(D{.}{.}{-1})	
 					  

//Perform Clarke tests
// Ho: median of lnf_eu - lnf_govPolicyW  = 0 
signtest lnf_eu = lnf_govPolicyW if e(sample) == 1

*2b
signtest lnf_eu = lnf_govEUW if e(sample) == 1

*3a
signtest lnf_eu = lnf_salience if e(sample) == 1

*3b
signtest lnf_eu = lnf_salience_power if e(sample) == 1

*4a
signtest lnf_eu = lnf_pivotEU if e(sample) == 1
//4b
// signtest lnf_eu = lnf_pivotPolicy if e(sample) == 1
//


*5
signtest lnf_eu = lnf_power if e(sample) == 1

drop  _est_m1 xb_eu lnf_eu _est_m2a xb_govPolicyW lnf_govPolicyW _est_m2b xb_govEUW lnf_govEUW _est_m3a xb_salience lnf_salience _est_m3b xb_salience_power lnf_salience_power _est_m4a xb_pivotEU lnf_pivotEU _est_m5 xb_power lnf_power
			
			
//Investigate whether differences in log-likelihoods are mesokurtic
/* "Figures A3.1 to A3.6 plot kernel density estimates of the observation-specific differences
in the log-likelihoods between the specifications of the territorial model and
the standard model using the full as well as the sample of conflict issues. I also include
a normal density for comparison. This demonstrates that the distribution of the
log-likelihood differences is clearly leptokurtic for all model comparisons. In this
case, the Clarke test is asymptotically more efficient than the alternative Vuong test
(Vuong 1989)." (Wratil 2019 AJPS, p. 38 of suplementary material)*/ 
gen lnf_diff1 = lnf_eu - lnf_govPolicyW
kdensity lnf_diff1 if e(sample), normal scheme(lean2) title("") xtitle("Differences in log-likelihoods") // the distribution of the log-likelihood differences is leptokurtic (having greater kurtosis than the normal distribution; more concentrated about the mean).
*graph export "Figure_A3_1.png", replace height(1500) 					  

gen lnf_diff2 = lnf_eu - lnf_govEUW
kdensity lnf_diff2 if e(sample), normal scheme(lean2) title("") xtitle("Differences in log-likelihoods") 			  

gen lnf_diff3 = lnf_eu - lnf_salience
kdensity lnf_diff3 if e(sample), normal scheme(lean2) title("") xtitle("Differences in log-likelihoods") 			  

gen lnf_diff4 = lnf_eu - lnf_salience_power
kdensity lnf_diff4 if e(sample), normal scheme(lean2) title("") xtitle("Differences in log-likelihoods") 			  

// gen lnf_diff5 = lnf_eu - lnf_pivotPolicy
// kdensity lnf_diff5 if e(sample), normal scheme(lean2) title("") xtitle("Differences in log-likelihoods") 			  

gen lnf_diff6 = lnf_eu - lnf_power
kdensity lnf_diff6 if e(sample), normal scheme(lean2) title("") xtitle("Differences in log-likelihoods") 			  

		  

				  
					  
						  
*** Possible extra controls: leg_inst, 
*	  c.cn_weighttri (needed given MS weights?)
*	  i.leg_inst  (endogenous?)


*** Extra thing to check
* Role of presidency -- need 3 vars: 1) position on EU of presidency gov; 2) position on policy of presidency gov; 3) public opinion in the presidency MS

//Create measure of conflicting opinion ala Wratil and use those with conflict_perc>.15 --> needs to be done using the long data
gen opinion_bin = .
replace opinion_bin = 1 if Opinion >= 0.5
replace opinion_bin = 0 if Opinion < 0.5
replace opinion_bin = . if Opinion == .

egen sum_change = sum(opinion_bin), by(question_id)
egen obs = count(opinion_bin), by(question_id)
gen conflicts = obs - sum_change
replace conflicts = sum_change if sum_change < conflicts
gen conflict_perc = conflicts/obs


/*
Hi both, for the following procedures we don’t have a date for the first trilogue while imputed PO indicators are present
2008/0227(COD)
2010/0212(COD)
2010/0255(COD)
2010/0271(COD)
2010/0325(COD)
2010/0362(COD)
2011/0231(COD)
2011/0361(COD)
2011/0380(COD)
2012/0039(COD)
2014/0096(COD)
2017/0226(COD)
2018/0041(COD)
I suggest we do not include these procedures to the sample as NAs in PO this case
 do not represent the absence of PO indicator in CAP in a year (here we can use imputations), 
 but not knowing what is an actual year and which year’s PO to use (so we might have PO indicator) —
 hence, using imputed PO is misleading (edited) 

keep if cod!="2008/0227(COD)" & cod!="2010/0212(COD)" & cod!="2010/0255(COD)" & cod!="2010/0271(COD)" & cod!="2010/0325(COD)" & cod!="2010/0362(COD)" & cod!="2011/0231(COD)" & cod!="2011/0361(COD)" & cod!="2011/0380(COD)" & cod!="2012/0039(COD)" & cod!="2014/0096(COD)" & cod!="2017/0226(COD)" & cod!="2018/0041(COD)" 
*/



************************
* OLS regression analysis insread of logit fracreg logit
************************

 
 
** Rerun for Clarke test

eststo m5: reg  cn_euaction_exp_all ///
						  c.support_trilogue_ms_vote_weighte ///
						  com_euaction_exp_all 						 
						 
						  /// 						  * [pw =cn_meaning_weight] ///
						  *if ms_minority_sd>0.10
						  
						  
eststo m1: reg  cn_euaction_exp_all ///
						  eu_salience_trilogue   ///
						  com_euaction_exp_all 				
						  
			 
						  
/* 2) Europhile coalition model:
IV2: Weighted average public support across MS,  where MS Govt's EU positions (or, POLICY position --> so two separate IVs, respectively below) serve as the weights. To create the weights, GOVt's EU (policy) positions need to be normalized between 0 and 1 --- or some such), (edited) 
*** Public support for EU policy action across EU as a weighted average of Public support across MS, with MS Govt's EU (policy action) positions as weights
* 2a) support_trilogue_eu_position_wei 
* support_trilogue_policy_position
*/

eststo m2a: reg cn_euaction_exp_all ///
						  c.support_trilogue_policy_pos_raw  ///
						  com_euaction_exp_all 						  


// eststo m2b: reg  cn_euaction_exp_all ///
// 						  c.support_trilogue_eu_pos_wei_raw  ///
// 						  com_euaction_exp_all 
/* 3) salience model:
IV3a: Weighted average public support across MS, where Salience levels in MS serve as the Weights (edited) 
*** Public support for EU policy action as a weighted average of Public support across MS, with public salience in MS as weights
*support_trilogue_salience_weight */




eststo m3a: reg  cn_euaction_exp_all ///
						  c.support_trilogue_salience_weight  ///
						  com_euaction_exp_all 


/* IV3b: Weighted average public support across MS, where Salience levels in MS as well as MS voting weights serve as the Weights (edited) 
*** Public support for EU policy action as a weighted average of Public support across MS, with public salience in MS and MS voting powers as weights 
*support_trilogue_salience_ms_vot
*/		 

eststo m3b: reg  cn_euaction_exp_all ///
						  c.support_trilogue_salience_ms_vot  ///
						  com_euaction_exp_all 

			 /* 
PROCEDURAL/NON-COOPERATIVE models:

4) Pivot wins
IV4: Public support in the Council pivotal state for EU authority expansion (the most Europhile or the Eurosceptic states)   /// respectively for Council pivot on policy dimension (edited) 

*** Public support for EU policy action in PIVOT MS in CN on EU (policy) dimension  at t of trilogue
* m4a: raw_eu_support_trilogue_eu_dimen
* m4b: raw_eu_support_trilogue_policy_d 
*/
//
eststo m4a: reg  cn_euaction_exp_all ///
						  c.raw_eu_support_trilogue_eu_dimen  ///
						  com_euaction_exp_all 

	 
/* 5) Weighted average wins
IV5: Weighted average public support in the Council, where only MS voting powers serve as weights (edited) 

*** Public support for EU policy action as a weighted average of Public support across MS, with MS voting powers as weights
*support_trilogue_ms_vote_weighte
*/


eststo m5: reg  cn_euaction_exp_all ///
						  c.support_trilogue_ms_vote_weighte  ///
						  com_euaction_exp_all 



* m1=EUwide m4a=EU pivot m5=powerWeightedPO m3a=salienceWeightedPO m3b=Power&SalienceWeightePO
estout m1 m4a  m5 m3a m3b   ,cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) /// 
  stats(r2 N, fmt(%9.3f %9.0g))  legend  ///
    label varlabels(_cons Constant) ///
varwidth(45)	
