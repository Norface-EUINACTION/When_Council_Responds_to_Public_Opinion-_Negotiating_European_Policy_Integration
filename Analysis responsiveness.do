/* Replication materials for analkysis in
	Yordanova, N., Ershova, A., Khokhlova, A., Obaid Ul-Islam, S. and Glavaš, G. 
	(2025) "Council Responsiveness to Public Opinion: Negotiating European Integration", 
	European Union Politics.

This script relies on the long, country*act level data.
*/


// cd "\DIR"
set scheme burd
use "Data_responsiveness_analysis.dta", clear



******************************
*** DESCRIPTIVE STATISTICS ***
******************************


 ** Online Appendix, Figure 5: DV2 distribution 
 graph bar (count), over(responsiveness_03)  title("Responsiveness of the Council to shifts in public" "support for EU action in member states")
 graph export "DV2_responsiveness03_bar.png", as(png) replace


 * Online Appendix, Table 10: Descriptive statistics for responsiveness analysis	  
estpost tabstat responsiveness_03 salience_trilogue unity_trilogue  ///
				   months_prop_to_trilogue , c(stat) stat( mean sd min max n)
  		          *  c.abs_dist_gov_cn_policy  abs_dist_gov_cn_eu ///

esttab using "descriptive_long.tex", replace ///
 cells("mean(fmt(%6.2fc)) sd(fmt(%6.2fc)) min max count(fmt(%6.0fc))") nonumber ///
  nomtitle nonote noobs label booktabs ///
  collabels("Mean" "SD" "Min" "Max" "N")
 
 		  
			  
			  
**************
*** MODELS ***
**************

* Table 2, Model 1
eststo m1: 		   logit  responsiveness_03 ///
						  c.salience_trilogue ///
						  unity_trilogue  ///
						  months_prop_to_trilogue ///
						  i.tristartyear ///
						  i.country_num ///
						  i.capcode 

estat class
estat ic // to get AIC

* Figure 2
margins , at(salience_trilogue=(0 (.1) 1)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
			plot(level(95) recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) ///
			 xtitle("Public salience on POLICY" )  ///
			 ytitle("Probability of Council responsiveness" "to shifts in public support for EU action") title("" , color(white)) ///
			 addplot(hist salience_trilogue if e(sample), xlabel(0(.1)1) freq bfcolor(none) blcolor(gs10)  ///
			 yaxis(2) yscale(alt axis(2)) below legend(off)) saving(file3, replace)  ///
			 legend(off ) )	
graph export "long_raw_m1_salience.png", as(png) replace


* Figure 3
margins , at(unity_trilogue=(0 (.1) 1)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
			plot(level(95) recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) ///
			 xtitle("Public unity on POLICY" )  ///
			 ytitle("Probability of Council responsiveness" "to shifts in public support for EU action") title("" , color(white)) ///
			 addplot(hist unity_trilogue if e(sample), xlabel(0(.1)1) freq bfcolor(none) blcolor(gs10)  ///
			 yaxis(2) yscale(alt axis(2)) below legend(off)) saving(file3, replace)  ///
			 legend(off ) )			 
graph export "long_raw_m1_unity.png", as(png) replace


* Table 2, Model 2		  
eststo m2: 	logit  responsiveness_03 ///
				   c.salience_trilogue##c.unity_trilogue  ///
				   months_prop_to_trilogue ///
				   i.capcode ///
				   i.country_num ///
				   i.tristartyear if e(sample) 

estat class
estat ic // to get AIC


* Figure 4a
margins , dydx(unity_trilogue) at(salience_trilogue=(0(.2)1)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
			 plot(yline(0) level(95) recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) ///
			 xtitle("Public salience" )  ///
			 ytitle("Average marginal effect of public unity") title("" , color(white)) ///
			 addplot(hist salience_trilogue if e(sample), xlabel(0(.1)1) freq bfcolor(none) blcolor(gs10)  ///
			 yaxis(2) yscale(alt axis(2)) below legend(off)) saving(file3, replace)  ///
			 legend(off))
graph export "m2_AME_unity_at_salience.png", as(png) replace			 

		 
* Figure 4b			 
margins , dydx(salience_trilogue) at(unity_trilogue=(0(.2)1)) cformat(%9.2f) pformat(%5.3f) sformat(%8.3f) ///
			 plot(yline(0) level(95) recast(line) plotopts(lwidth(vthin)) ///
			 recastci(rarea) ciopts(fcolor(%20) ///
			 lcolor(none) lwidth(none) lpattern(dot)) ///
			 xtitle("Public unity" )  ///
			 ytitle("Average marginal effect of public salience") title("" , color(white)) ///
			 addplot(hist unity_trilogue if e(sample), xlabel(0(.1)1) freq bfcolor(none) blcolor(gs10)  ///
			 yaxis(2) yscale(alt axis(2)) below legend(off)) saving(file3, replace)  ///
			 legend(off)  )
graph export "m2_AME_salience_at_unity.png", as(png) replace			 

			 
* Online Appendix, Figure 6	  
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

graph export "m2_salience_at_unity.png", as(png) replace	


* Online Appendix, Figure 7
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
graph export "m2_unity_at_salience.png", as(png) replace			 
		 

* Table 2
estout  m1 m2 ,cells(b (star fmt(%9.3f)) p(par))  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) /// 
  stats( N, fmt(%9.3f %9.0g))   drop(*.country_num *.tristartyear *.capcode) legend  ///
  label varlabels(_cons Constant) ///
varwidth(45)	

*** as latex
 esttab   m1 m2 using ///
"Table_congruence_rawdata.tex" ,replace  f  ///
 b(2) p(2) nomtitle label star(* 0.10 ** 0.05 *** 0.01)  drop(*.country_num *.tristartyear *.capcode)	///
 booktabs alignment(D{.}{.}{-1})	



