/* Replication materials for the paper "Council Responsiveness to Public Opinion: 
	Negotiating European Integration" by Yordanova, N.; Ershova, A. and Khokhlova, A. 

This is the script that relies on disaggregated data with the country level variance.

Written by Nikoleta Yordanova on 8 September 2023.

Last updated on 6 October 2023) */


// cd "\DIR"
cd "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper"
*import delimited "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Council_long_data_092023.csv", delimiter(comma) clear 

import delimited "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\041023_Council_long.csv", delimiter(comma) clear 
// import delimited "041023_Council_long.csv", delimiter(comma) clear 



* Destringing not needed in this version
* destring *, ignore("NA") replace 

*ssc install scheme-burd
set scheme burd

* 
rename congruence_support responsiveness


* gen numeric for ep term
gen ep_term_pr_num= 1 if  ep_term_proposal== "EP6"
replace ep_term_pr_num= 2 if  ep_term_proposal== "EP7_1"
replace ep_term_pr_num= 3 if  ep_term_proposal== "EP7_2"
replace ep_term_pr_num= 4 if  ep_term_proposal== "EP8"
replace ep_term_pr_num= 5 if  ep_term_proposal== "EP9"


* generate the difference in the public support from tri to proposal
*gen support_change = support_trilogue- support_proposals
gen change_support= support_trilogue - support_proposals

* generate the difference in the public opposition from tri to proposal
gen opposition_change= opposition_trilogue- opposition_proposals

*generate chage in salince variable as a diff between salience at trilogue  & salience at proposal
gen salience_change= salience_trilogue- salience_proposals

* gen indicator for ll consulted committees
gen full_consulted_coms= n_comm_consulted+ n_comm_consulted_former

* gen indicator for the instrument
gen leg_inst_num =1 if leg_instr=="Decision"
replace leg_inst_num =2 if leg_instr=="Directive"
replace leg_inst_num =3 if leg_instr=="Regulation"


* gen absolute distance to pivot on POLICY dimension
* gen abs_dist_to_pivot_policy = abs(gov_policy_positionstri - pivot_policy_position_proeu) if gov_policy_positionstri!=. & pivot_policy_position_proeu!=.

*gen distance to pivot on POLICY dimension   // higher values mean gov is more pro policy integration that the pivot on the policy dim
* gen dist_to_pivot_policy = gov_policy_positionstri - pivot_policy_position_proeu  if gov_policy_positionstri!=. & pivot_policy_position_proeu!=. // higher values mean gov is more pro policy integration that the pivot on the policy dim

* gen absolute distance to pivot on eu dimension
gen abs_dist_to_pivot_eu =  abs(goveu_triloguelowe - pivot_eu_position) if goveu_triloguelowe!=. & pivot_eu_position!=.

  // higher values mean gov is more pro eu that the pivot on the policy dim

* gen  distance to pivot on EU dimension // higher values mean gov is more pro EU integration that the pivot
*gen dist_to_pivot_eu =  goveu_triloguelowe - pivot_eu_position  if goveu_triloguelowe!=. &  pivot_eu_position!=. // higher values mean gov is more pro eu that the pivot on the policy dim



gen num_competence_years = tristartyear - competence_year

* drop act that had no trilogue
drop if    cn_euaction_exp_all==. 

* gen POLICY position of country of the presidency 
gen presid_gov_policy = gov_policy_positionstri if country_str  == country_presid_nego_start & presidency_dummy==1
bysort cod: egen test=min(presid_gov_policy)  // assign presid gov policy position to all observations in a cod 
drop presid_gov_policy
rename test presid_gov_policy

gen abs_dist_gov_presid_policy = abs(gov_policy_positionstri-presid_gov_policy) if gov_policy_positionstri !=. & presid_gov_policy!=.

* gen EU position of country of the presidency 
gen presid_gov_eu = goveu_triloguelowe if country_str  == country_presid_nego_start
bysort cod: egen test=min(presid_gov_eu)
drop presid_gov_eu
rename test presid_gov_eu

gen abs_dist_presid_gov_eu = abs(goveu_triloguelowe-presid_gov_eu) if goveu_triloguelowe!=. & presid_gov_eu!=.


* gen absolute distance of Gov from CN mean on POLICY
*gen abs_dist_gov_cn_policy1 = abs(gov_policy_positionstri-cm_wmean_policy_imp) if gov_policy_positionstri!=. & cm_wmean_policy!=. // cm_wmean_policy_imp with imputed value
gen abs_dist_gov_cn_policy = abs(gov_policy_positionstri-cn_wmean_policy_tri_raw) if gov_policy_positionstri!=. & cn_wmean_policy_tri_raw!=. 

* gen absolute distance of Gov from CN mean on EU dim 
*gen abs_dist_gov_cn_eu1 = abs(goveu_triloguelowe-cn_wmean_eu_imp) if goveu_triloguelowe!=. & cn_wmean_eu_imp!=. // cn_wmean_eu_imp with imputed value
gen abs_dist_gov_cn_eu = abs(goveu_triloguelowe-cn_mean_eu_raw_tri) if goveu_triloguelowe!=. & cn_mean_eu_raw_tri!=. 



* Number of days between proposal and first trilogue 
generate  proposal_date2=date( proposal_date,"YMD")
generate  tristartdate2 =date( tristartdate,"YMD")
gen days_prop_to_trilogue = tristartdate2 - proposal_date2

*Gen unity_trilogue as inverse of polarisation
gen unity_trilogue = (polarization_trilogue-1)*-1


* Proposal before Lisbon -- but there are many more such in our data -- we just focused on those adopted after Lisbon, so don't drop
*drop if cod=="2007/0229(COD)"

save "long_raw.dta", replace











set scheme burd
cd "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper"
use "long_raw.dta", clear



* Descriptive statistics

 *********************************************decriptive

 ** DV distribution 
  graph bar (count), over(responsiveness_03)  title("Responsiveness of the Council to shifts in public" "support for EU action in member states")
 graph export "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Figures_long\DV2_responsiveness03_bar.png", as(png) replace

 
  estpost tabstat responsiveness_03 salience_trilogue unity_trilogue  ///
				   days_prop_to_trilogue , c(stat) stat( mean sd min max n)
  		          *  c.abs_dist_gov_cn_policy  abs_dist_gov_cn_eu ///

  esttab using "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Latex tables\descriptive_long.tex", replace ///
 cells("mean(fmt(%6.2fc)) sd(fmt(%6.2fc)) min max count(fmt(%6.0fc))") nonumber ///
  nomtitle nonote noobs label booktabs ///
  collabels("Mean" "SD" "Min" "Max" "N")
 
  
  

sum 		  responsiveness_03 salience_trilogue  polarization_trilogue  gov_policy_positionstri ///
			     abs_dist_gov_cn_policy  goveu_triloguelowe  ///
			    abs_dist_gov_cn_eu same_support_shift03
misstable sum responsiveness_03 salience_trilogue  polarization_trilogue  gov_policy_positionstri  ///
			     abs_dist_gov_cn_policy  goveu_triloguelowe   ///
			    abs_dist_gov_cn_eu same_support_shift03 // check that I use non-imputed vars with missings


corr  		  responsiveness_03 salience_trilogue  polarization_trilogue  gov_policy_positionstri ///
			     abs_dist_gov_cn_policy  goveu_triloguelowe  ///
			    abs_dist_gov_cn_eu

			  
			  
			  
*** MODELS ***

/* We include year and policy area fixed effects to eliminate the possibility that any relationship 
we observe between country-level effects (e.g. of country-level public opinion, 
polarisation or salience) is driven by EU-wide effects in given policies or years(e.g. of EU-wide public opinion, 
polarisation or salience). Fixed effects for policy and year remove all act-level variation, 
which could be explained by EU-wide mean measures.

We also include country fixed effects to control for any other unobserved country level variables that can
explain the responsiveness of the Council to public opinion shifts (e.g. voting power,pivot position, relation to presidency).
*/





*** DV: responsiveness_03

* With no interactions

eststo m1: 		   logit  responsiveness_03 ///
						  c.salience_trilogue ///
						  unity_trilogue  ///
						  days_prop_to_trilogue ///
						  i.tristartyear ///
						  i.country_num ///
						  i.capcode 
						  *  c.abs_dist_gov_cn_policy  abs_dist_gov_cn_eu ///

estat class
* Correctly classified: 65.2   Null model: 67.5


margins , at(unity_trilogue=(0 (.1) 1)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
			plot(level(95) recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) ///
			 xtitle("Public unity on POLICY" )  ///
			 ytitle("Probability of Council responsiveness" "to shifts in public support for EU action") title("" , color(white)) ///
			 addplot(hist polarization_trilogue if e(sample), xlabel(0(.1)1) freq bfcolor(none) blcolor(gs10)  ///
			 yaxis(2) yscale(alt axis(2)) below legend(off)) saving(file3, replace)  ///
			 legend(off ) )			 
graph export "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Figures_long\long_raw_m1_unity.png", as(png) replace


margins , at(salience_trilogue=(0 (.1) 1)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
			plot(level(95) recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) ///
			 xtitle("Public salience on POLICY" )  ///
			 ytitle("Probability of Council responsiveness" "to shifts in public support for EU action") title("" , color(white)) ///
			 addplot(hist polarization_trilogue if e(sample), xlabel(0(.1)1) freq bfcolor(none) blcolor(gs10)  ///
			 yaxis(2) yscale(alt axis(2)) below legend(off)) saving(file3, replace)  ///
			 legend(off ) )	
graph export "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Figures_long\long_raw_m1_salience.png", as(png) replace
//
// margins , at(abs_dist_gov_cn_policy=(0 (1) 12)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
// 			plot(level(95) recast(line) plotopts(lwidth(vthin)) ///
// 			 recastci(rarea) ciopts(fcolor(%20) ///
// 			 lcolor(none) lwidth(none) lpattern(dot)) ///
// 			 xtitle("Distance of Gov position to weighted average Council position on POLICY" )  ///
// 			 ytitle("Responsiveness of the Council position on COM proposal" "to shifts in public support for EU action")  title("" , color(white)) ///
// 			 addplot(hist abs_dist_gov_cn_policy if e(sample), xlabel(0 (1) 12) freq bfcolor(none) blcolor(gs10)  ///
// 			 yaxis(2) yscale(alt axis(2)) below legend(off)) saving(file3, replace)  ///
// 			 legend(off ) )	
// graph export "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Figures_long\long_raw_m1_govDistPolicyCNmean.png", as(png) replace
//
//


* With interaction of Salience*Policy
			  
eststo m2: 	logit  responsiveness_03 ///
				   c.salience_trilogue##c.unity_trilogue  ///
				   days_prop_to_trilogue ///
				   i.capcode ///
				   i.country_num ///
				   i.tristartyear if e(sample) 
				   *  c.abs_dist_gov_cn_policy  abs_dist_gov_cn_eu ///

estat class
* Correctly classified:    Null model: 67.6



margins , dydx(unity_trilogue) at(salience_trilogue=(0(.2)1)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
			 plot(yline(0) level(95) recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) ///
			 xtitle("Public salience" )  ///
			 ytitle("Average marginal effect of public unity") title("" , color(white)) ///
			 addplot(hist salience_trilogue if e(sample), xlabel(0(.1)1) freq bfcolor(none) blcolor(gs10)  ///
			 yaxis(2) yscale(alt axis(2)) below legend(off)) saving(file3, replace)  ///
			 legend(off))
graph export "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Figures_long\m2_AME_unity_at_salience.png", as(png) replace			 

		 
			 
margins , dydx(salience_trilogue) at(unity_trilogue=(0(.2)1)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
			 plot(yline(0) level(95) recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) ///
			 xtitle("Public unity" )  ///
			 ytitle("Average marginal effect of public salience") title("" , color(white)) ///
			 addplot(hist unity_trilogue if e(sample), xlabel(0(.1)1) freq bfcolor(none) blcolor(gs10)  ///
			 yaxis(2) yscale(alt axis(2)) below legend(off)) saving(file3, replace)  ///
			 legend(off)  )
graph export "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Figures_long\m2_AME_salience_at_unity.png", as(png) replace			 
			 
			 
margins, at(unity_trilogue=(0 (.2) 1) salience_trilogue=(0.2, 0.4, 0.6, 0.8)) ///
				 plot(level(95)  recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) xtitle("Public unity in member state" )  ///
			 ytitle("Probability of Council responsiveness" "to shifts in public support for EU action")  title("" , color(white)) ///
			 legend(pos(5) ring(0) order(1 "Salience=0.2" 2 "Salience=0.4" 3 "Salience=0.6" 4 "Salience=1.0")))
marginsplot, by(salience_trilogue, label("Salience=0.2" "Salience=0.4" "Salience=0.6" "Salience=0.8")) recast(line) plotopts(lwidth(vthin))  ///
		 recastci(rarea) ciopts(fcolor(%20)  lcolor(none) lwidth(none)  ///
		  lpattern(dot))    legend(off) xtitle("Public unity in member state" )	///	
		  ytitle("Probability of Council responsiveness" "to shifts in public support for EU action")  byopts(title("", color(white)))
graph export "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Figures_long\m2_unity_at_salience.png", as(png) replace			 

		  
		  
margins, at(salience_trilogue=(0 (.2) 1) unity_trilogue=(0.2, 0.4, 0.6, 0.8)) ///
				 plot(level(95)  recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) xtitle("Public salience in member state" )  ///
			 ytitle("Probability of Council responsiveness" "to shifts in public support for EU action")  title("" , color(white)) ///
			 legend(pos(5) ring(0) order(1 "Unity=0.2" 2 "Unity=0.4" 3 "Unity=0.6" 4 "Unity=0.8")))
marginsplot, by(unity_trilogue, label("Unity=0.2" "Unity=0.4" "Unity=0.6" "Unity=0.8")) recast(line) plotopts(lwidth(vthin))  ///
		 recastci(rarea) ciopts(fcolor(%20)  lcolor(none) lwidth(none)  ///
		  lpattern(dot))    legend(off) xtitle("Public salience in member state" )	///	
		  ytitle("Probability of Council responsiveness" "to shifts in public support for EU action")  byopts(title("", color(white)))

graph export "C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Figures_long\m2_salience_at_unity.png", as(png) replace			 


estout  m1 m2 ,cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) /// 
  stats( N, fmt(%9.3f %9.0g))   drop(*.country_num *.tristartyear *.capcode) legend  ///
  label varlabels(_cons Constant) ///
varwidth(45)	

*** as latex
 esttab   m1 m2 using ///
"C:\Users\yordanovan1\OneDrive - Universiteit Leiden\Norface\Leveraging politicization_Council paper\data\Council_paper\Latex tables\Table_congruence_rawdata.tex" ,replace  f  ///
 b(2) p(2) nomtitle label star(* 0.10 ** 0.05 *** 0.01)  drop(*.country_num *.tristartyear *.capcode)	///
 booktabs alignment(D{.}{.}{-1})	

 
 
 
 
 
 
 
 
 
 
 **********************************
 ***** ROBUSTNESS CHECKS **********
  **********************************

 *********  1) DV measured differently: 
 
 * Models with different threholds in DV measures of responsiveness
 ** for detecting positive or negative change 
 ** in leg. text position AND public support b/n t prpposal and t trilogue
 
 
*** DV: responsiveness_01 -- at least 0.01 move on PO and COM-CN text measures

eststo m3: 		   logit  responsiveness_01 ///
						  c.salience_trilogue ///
						  unity_trilogue  ///
						  days_prop_to_trilogue ///
						  i.tristartyear ///
						  i.country_num ///
						  i.capcode 
				          *  c.abs_dist_gov_cn_policy  abs_dist_gov_cn_eu ///
estat class

 
eststo m4: 	logit  responsiveness_01 ///
				   c.salience_trilogue##c.unity_trilogue  ///
				   abs_dist_gov_cn_policy abs_dist_gov_cn_eu ///
				   days_prop_to_trilogue ///
				   i.capcode ///
				   i.country_num ///
				   i.tristartyear if e(sample) 
estat class




*** DV: responsiveness_02 -- at least 0.02 move on PO and COM-CN text measures

eststo m5: 		   logit  responsiveness_02 ///
						  c.salience_trilogue ///
						  unity_trilogue  ///
						  days_prop_to_trilogue ///
						  i.tristartyear ///
						  i.country_num ///
						  i.capcode 
				          *  c.abs_dist_gov_cn_policy  abs_dist_gov_cn_eu /// 
estat class
		  
eststo m6: 	logit  responsiveness_02 ///
				   c.salience_trilogue##c.unity_trilogue  ///
				   abs_dist_gov_cn_policy abs_dist_gov_cn_eu ///
				   days_prop_to_trilogue ///
				   i.capcode ///
				   i.country_num ///
				   i.tristartyear if e(sample) 
estat class




*** DV: responsiveness -- with any (>0) move on PO and COM-CN text measures

eststo m7: 		   logit  responsiveness ///
						  c.salience_trilogue ///
						  unity_trilogue  ///
						  days_prop_to_trilogue ///
						  i.tristartyear ///
						  i.country_num ///
						  i.capcode 
				          *  c.abs_dist_gov_cn_policy  abs_dist_gov_cn_eu /// 
estat class
			  
eststo m8: 	logit  responsiveness ///
				   c.salience_trilogue##c.unity_trilogue  ///
				   days_prop_to_trilogue ///
				   i.capcode ///
				   i.country_num ///
				   i.tristartyear if e(sample) 
				          *  c.abs_dist_gov_cn_policy  abs_dist_gov_cn_eu /// 
estat class


estout  m1 m2 m3 m4 m5 m6 m7 m8 ,cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) /// 
  stats( N, fmt(%9.3f %9.0g))   drop(*.country_num *.tristartyear *.capcode) legend  ///
  label varlabels(_cons Constant) ///
varwidth(45)	








 
 
 
 
 
 
 
 
 
 
  *********  2) IV control -- distance of Gov to Presidency on policy 
 
 *** These models may be interesting only if we don't include country fixed effects, 
 *** as these FEs capture the policy distance of gov to presidency effects 
 ** (vary only on policy and MS level -- and we have FEs for both)
 

 ** Effects of abs distance to Presidency on Policy and/or EU dim
 eststo m9: 	logit  responsiveness_03 ///
				   c.salience_trilogue##c.unity_trilogue  ///
				   abs_dist_gov_presid_policy ///
				   days_prop_to_trilogue ///
				   i.capcode ///
				   i.country_num ///
				   i.tristartyear if e(sample) 

margins , at(abs_dist_gov_presid_policy=(0 (1) 8)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
			plot(level(95) recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) ///
			 xtitle("Distance of Gov position to pivot Govt on POLICY dimension" )  ///
			 ytitle("Probability of Council responsiveness" "to shifts in public support for EU action")  title("" , color(white)) ///
			 addplot(hist abs_dist_gov_cn_policy if e(sample), xlabel(0 (1) 12) freq bfcolor(none) blcolor(gs10)  ///
			 yaxis(2) yscale(alt axis(2)) below legend(off)) saving(file3, replace)  ///
			 legend(off ) )	
			 
eststo m10: 	logit  responsiveness_03 ///
				   c.salience_trilogue##c.unity_trilogue  ///
				   abs_dist_presid_gov_eu ///
				   days_prop_to_trilogue ///
				   i.capcode ///
				   i.country_num ///
				   i.tristartyear if e(sample) 



					  
					  
	
	
	
	
	
	
	
	
	
	
	
	
	
					  
*** WON't be using these models from here on with alternative (less intuitive DVs)...
	
				  
***** II. MODELS with congr_cn_cap_mspo
* Congr. b/n MS imp support relative to av.EU and Council COD position relative to CN average  in CAP across year
** +effect of salience
** +effect of polarisation -- counterintuitive!! 
** +effect of Gov EU position 					  
					  


eststo m3: 		   logit  congr_cn_cap_mspo ///
						  c.abs_dist_gov_cn_policy   ///
						  c.salience_trilogue ///
						  polarization_trilogue  ///
						  i.tristartyear ///
						  i.country_num ///
						  i.capcode if e(sample)
			  

eststo m6:  logit  congr_cn_cap_mspo ///
						  c.abs_dist_gov_cn_eu  ///
						  c.salience_trilogue ///
						  polarization_trilogue  ///
						  i.tristartyear ///
						  i.country_num ///
 						  i.capcode if e(sample)

estout  m1 m2 m3  m4 m5 m6 ,cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) /// 
  stats( N, fmt(%9.3f %9.0g))   drop(*.country_num *.tristartyear *.capcode) legend  ///
  label varlabels(_cons Constant) ///
varwidth(45)		
	
	
	
	
	
	

				  
***** III. MODELS with congr_cn_year_mspo
* Congr. b/n MS imp support relative to av.EU and Council COD position relative to CN average  in YEAR across CAP
** +effect of salience
** +effect of Gov EU position 

eststo m3: 		   logit  congr_cn_year_mspo ///
						  c.abs_dist_gov_cn_policy   ///
						  c.salience_trilogue ///
						  polarization_trilogue  ///
						  i.tristartyear ///
						  i.country_num ///
						  i.capcode if e(sample)
			  
eststo m6:  logit  congr_cn_cap_mspo ///
						  c.abs_dist_gov_cn_eu  ///
						  c.salience_trilogue ///
						  polarization_trilogue  ///
						  i.tristartyear ///
						  i.country_num ///
 						  i.capcode if e(sample)

estout  m1 m2 m3  m4 m5 m6 ,cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) /// 
  stats( N, fmt(%9.3f %9.0g))   drop(*.country_num *.tristartyear *.capcode) legend  ///
  label varlabels(_cons Constant) ///
varwidth(45)		
	







***** IV. MODELS with congr_cn_year_mspo
* Congr. b/n MS imp support relative to av.EU and Council COD position relative to CN average in CAP-YEAR
** +effect of salience
** +effect of Gov EU position 

						  

eststo m3: 		   logit  congr_cn_year_mspo ///
						  c.abs_dist_gov_cn_policy   ///
						  c.salience_trilogue ///
						  polarization_trilogue  ///
						  i.tristartyear ///
						  i.country_num ///
						  i.capcode if e(sample)
			  

eststo m6:  logit  congr_cn_year_mspo ///
						  c.abs_dist_gov_cn_eu  ///
						  c.salience_trilogue ///
						  polarization_trilogue  ///
						  i.tristartyear ///
						  i.country_num ///
 						  i.capcode if e(sample)

estout  m1 m2 m3  m4 m5 m6 ,cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) /// 
  stats( N, fmt(%9.3f %9.0g))   drop(*.country_num *.tristartyear *.capcode) legend  ///
  label varlabels(_cons Constant) ///
varwidth(45)	









