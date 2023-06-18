clear all

cd ~\income_damage_2023
insheet using data\socieconomic\GDPpc.csv ,clear

*****************************************
*      panel regression for GDPpc       *
*****************************************

*Variable generation
rename area_ID gdlcode1
encode country, gen(country1)
xtset gdlcode1 year
gen lngdp = log(gdp)
gen dgdp = d.lngdp
gen dT = d.tem
gen dP = d.pre
gen bins  = floor(year/5)
egen gdp_region_mean = mean(gdp),by(gdlcode1 bins)
egen gdp_median = median(gdp_region_mean)
gen poor = (gdp_region_mean <= gdp_median) // generate dummy varibale for heterogeneity analysis

*Descriptive statistics
//descriptive for Panel B
preserve
drop if missing(dgdp, tem,pre,tem_sd,pre_sd)
summ dgdp tem pre tem_sd pre_sd
duplicates drop gdlcode1, force
summ gdlcode1
duplicates drop country1, force
summ country1
restore
//descriptive for Panel B-C
preserve
drop if missing(dgdp, tem,pre, tem_sd,pre_sd,pop,edu)
summ dgdp tem pre tem_sd pre_sd pop edu
duplicates drop gdlcode1, force
summ gdlcode1
duplicates drop country1, force
summ country1
restore

*pre-regression tests
preserve
drop if missing(dgdp, tem,pre, tem_sd,pre_sd)
xtbalance,range(1991 2015)
local tests llc ips ht
local vars dgdp tem pre tem_sd pre_sd dT dP
foreach var of loc vars  {
	foreach test of loc tests {
		xtunitroot `test' `var',trend demean //unit root tests (Table S3)
	}
}
restore

*mian regression
//This study-type model (Table S4)
reghdfe dgdp c.dT c.dT#c.tem  c.dP c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd edu pop,absorb(i.year gdlcode1##c.year) vce( cluster country1)
estimates store r1, title("DTW_type")
margins, dydx(tem) at(tem=(-1(1)31) tem_sd = 0 pre_sd = 0) atmeans post level(90) 
margins, dydx(pre) at(pre=(0(0.1)6) tem_sd = 0 pre_sd = 0) atmeans post level(90) 

//BHM-type model (Table S4)
reghdfe dgdp c.tem##c.tem c.pre##c.pre edu pop,absorb(i.year gdlcode1##c.year) vce( cluster country1)
estimates store r2, title("BHM_type")
margins, dydx(tem) at(tem=(-1(1)31)) atmeans post level(90) 
margins, dydx(pre) at(pre=(0(0.1)6)) atmeans post level(90) 

//KAL-type regression (Table S4)
reghdfe dgdp c.dT c.dT#c.tem  c.dP c.dP#c.pre  c.tem##c.tem c.pre##c.pre  edu pop,absorb(i.year gdlcode1##c.year) vce( cluster country1)
estimates store r3, title("KAL_type")
margins, dydx(tem) at(tem=(-1(1)31)) atmeans post level(90) 
margins, dydx(pre) at(pre=(0(0.1)6)) atmeans post level(90) 

esttab r1 r2 r3 using TableS4.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)


*Interaction effects analysis (Table S5)
local levs tem pre
local vars tem_sd pre_sd 
local i 3
foreach lev of loc levs  {
	foreach var of loc vars {
		reghdfe dgdp c.dT c.dT#c.tem  c.dP  c.dP#c.pre  c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd (c.`lev'##c.`lev')#(c.`var'##c.`var') edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
		test c.`lev'#c.`var' c.`lev'#c.`var'#c.`var' c.`lev'#c.`lev'#c.`var' c.`lev'#c.`lev'#c.`var'#c.`var'
		local i: di %01.0f `i'+1
	    estimates store r`i', title("Inter_`lev'_`var'")
	}
}
esttab r4 r5 r6 r7 using TableS5.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)


*Heterogeneity analysis
//Heterogeneity analysis of level and variation effects (Table S6)
reghdfe dgdp poor#(c.dT c.dT#c.tem  c.dP  c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd ) edu pop,absorb(i.year gdlcode1##c.year) vce( cluster country1)
estimates store r8, title("Hetero_LV")
margins, dydx(tem) over(poor) at(tem=(-1(1)31) pre_sd=0 tem_sd=0 ) atmeans post level(90)
margins, dydx(pre) over(poor) at(pre=(0(0.1)6) pre_sd=0 tem_sd=0 ) atmeans post level(90)

esttab r8 using TableS6.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)

//Heterogeneity analysis of interaction effects between T and AST (Table S7)
reghdfe dgdp c.dT c.dT#c.tem  c.dP  c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd  poor#((c.tem_sd##c.tem_sd)#(c.tem##c.tem)) edu pop,absorb(i.year gdlcode1##c.year) vce( cluster country1)
estimates store r9, title("Hetero_T_AST")
margins, dydx(tem_sd) over(poor) at(tem_sd=(-4(0.1)4) pre_sd=0 tem=30 ) atmeans post level(90)

//Heterogeneity analysis of interaction effects between P and ASP (Table S7)
reghdfe dgdp c.dT c.dT#c.tem  c.dP  c.dP#c.pre  c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd poor#((c.pre_sd##c.pre_sd)#(c.pre##c.pre))   edu pop,absorb(i.year gdlcode1##c.year) vce( cluster country1)
estimates store r10, title("Hetero_P_ASP")
margins, dydx(pre_sd) over(poor) at(pre_sd=(-4(0.1)4) pre=4 tem_sd=0 ) atmeans post level(90)

esttab r9 r10 using TableS7.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)


*Post-estimation tests
qui reghdfe dgdp dT c.dT#c.tem dP c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd edu  pop,noabsorb vce(cluster country1) res(re)
estimates store r11, title("Post_noabsorb")
xtcdf re //Cross-section dependency test (Table S9)
xtqptest re, order(1) force //First-order serial correlation test (Table S10)
xtqptest re, order(2) force //Second-order serial correlation test (Table S10)

qui reghdfe dgdp dT c.dT#c.tem dP c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd edu  pop,absorb(i.year gdlcode1##c.year) vce(cluster country1) res(ref)
estimates store r12, title("Post_absorb")
xtcdf ref //Cross-section dependency test
xtqptest ref, order(1) force //First-order serial correlation test
xtqptest ref, order(2) force //Second-order serial correlation test

qui reghdfe dgdp l.dgdp  dT c.dT#c.tem dP c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd edu  pop,absorb(i.year gdlcode1##c.year) vce(cluster country1) res(refl)
estimates store r13, title("Post_absorb_lag")
xtqptest refl, order(1) force //First-order serial correlation test
xtqptest refl, order(2) force //Second-order serial correlation test

esttab r11 r12 r13 using TableS11.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f) //(Table S11)


*Robustness checks
//Results Comparison (Table S16)
preserve
keep if kalkuhl == 1
reghdfe dgdp  c.dT c.dT#c.tem  c.dP c.dP#c.pre c.tem##c.tem c.pre##c.pre edu pop,absorb(i.year gdlcode1##c.year) vce( cluster country1)
estimates store r14, title("Results_Comparison")
restore

esttab r14 using TableS16.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)

// Replace CRU by ERA5 (Table S17)
preserve
replace dT = d.tem_era
replace dP = d.pre_era
reghdfe dgdp c.dT c.dT#c.tem_era  c.dP c.dP#c.pre_era c.tem_era##c.tem_era c.pre_era##c.pre_era c.asp_tem##c.asp_tem c.asp_pre##c.asp_pre pop  edu ,absorb(i.year gdlcode1##c.year) vce( cluster country1)
estimates store r15, title("DTW_type_ERA")
restore

esttab r15 using TableS17.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)


// Replace ASP by WASP (Table S18)
reghdfe dgdp c.dT c.dT#c.tem  c.dP c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_wsd##c.pre_wsd edu pop,absorb(i.year gdlcode1##c.year) vce( cluster country1)
estimates store r16, title("DTW_type_WASP")

esttab r16 using TableS18.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)

*plot
//effect of annual mean temperature (Figure 2a)
qui reghdfe dgdp c.dT c.dT#c.tem  c.dP c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd edu pop,absorb(i.year gdlcode1##c.year) vce( cluster country1)
qui margins, at(tem=(-1(1)31) tem_sd = 0 pre_sd = 0) atmeans post level(90)
preserve
parmest, norestore level(90)
egen  x = seq(), from(-1) to(31)
drop parm
twoway ///
(rarea min max x, fcolor( "0 175 80%30") lcolor(%0) ) ///
(line est x , lcolor("0 175 80") )  ///
 , xline(18,lwidth(thin) lcolor("0 175 80") lpattern(dash)) ///
xlabel(0(5)31, labsize(small)) ///
ylabel(-0.4(0.1)0.3, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Change in ln(GDP per capital), size(small)) xtitle(Annual mean temperature(℃), size(small)) graphregion(color(white))
graph save Graph "figure\GDP_T.png",replace
restore

//margins effect of annual mean temperature (Figure 2c)
qui reghdfe dgdp c.dT c.dT#c.tem  c.dP c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd edu pop,absorb(i.year gdlcode1##c.year) vce( cluster country1)
qui margins, dydx(tem) at(tem=(-1(1)31) tem_sd = 0 pre_sd=0 ) atmeans post level(90)
preserve
parmest, norestore level(90)
egen  x = seq(), from(-1) to(31)
drop parm
twoway ///
(rarea min max x, fcolor( "0 175 80%30") lcolor(%0) ) ///
(line est x , lcolor("0 175 80") )  ///
 ,  xlabel(0(5)31, labsize(small)) ///
ylabel(-0.06(0.01)0.03, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Margin effect on ln(GDP per capital) change, size(small)) xtitle(Annual mean temperature(℃), size(small)) graphregion(color(white))
 graph save Graph "figure\GDP_marg_T.png",replace
restore

//effect of annual total precipitation (Figure 2b)
qui reghdfe dgdp c.dT c.dT#c.tem  c.dP c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd edu pop,absorb(i.year gdlcode1##c.year) vce( cluster country1)
qui margins, at(pre=(0(0.1)6) tem_sd=0 pre_sd=0) atmeans post level(90)
preserve
parmest, norestore level(90)
egen  x = seq(), from(0) to(60)
replace x = x/10
drop parm
twoway ///
(rarea min max x, fcolor( "0 175 80%30") lcolor(%0) ) ///
(line est x , lcolor("0 175 80") )  ///
 , xline(5.2,lwidth(thin) lcolor("0 175 80") lpattern(dash)) ///
xlabel(0(1)6, labsize(small)) ///
ylabel(-0.2(0.1)0.2, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Change in ln(GDP per capital), size(small)) xtitle(Annual total precipitation(m), size(small)) graphregion(color(white))
graph save Graph "figure\GDP_P.png",replace
restore
 
//margins effect of annual total precipitation (Figure 2d)
qui reghdfe dgdp c.dT c.dT#c.tem  c.dP c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd edu pop,absorb(i.year gdlcode1##c.year) vce( cluster country1)
qui margins,dydx(pre) at(pre=(0(0.1)6) tem_sd=0 pre_sd=0) atmeans post level(90)
preserve
parmest, norestore level(90)
egen  x = seq(), from(0) to(60)
replace x = x/10
drop parm
twoway ///
(rarea min max x, fcolor( "0 175 80%30") lcolor(%0) ) ///
(line est x , lcolor("0 175 80") )  ///
 , xlabel(0(1)6, labsize(small)) ///
ylabel(-0.1(0.05)0.1, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Margin effect on ln(GDP per capital) change, size(small)) xtitle(Annual total precipitation(m), size(small)) graphregion(color(white))
 graph save Graph "figure\GDP_marg_P.png",replace
restore

//Heterogeneity analysis for the effects of temperature, temperature change, and temperature variability (Figure 4a, it has been further enhanced through Adobe Illustration)
qui reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre  pop edu,absorb(i.year gdlcode1##c.year) vce( cluster country1)
qui margins,dydx(dT) at( tem_sd=0 pre_sd = 0) atmeans post level(90)
estimates store m1
qui reghdfe dgdp poor#c.(c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd  c.dT c.dT#c.tem c.dP c.dP#c.pre ) edu pop,absorb(i.year gdlcode1##c.year) vce( cluster country1)
qui margins,dydx(dT) at(poor =(0 1) tem_sd=0 pre_sd = 0) atmeans post level(90)
estimates store m2
coefplot ///
(r1, keep(c.tem#c.tem c.tem_sd#c.tem_sd ) mcolor("0 175 80%60") ciopts(color("0 175 80") lwidth(medthick)) ms(O) ) ///
(m1, mcolor("0 175 80%60") ciopts(color("0 175 80") lwidth(medthick)) ms(O) ) /// 
(r8, keep(0.poor#c.tem#c.tem 0.poor#c.tem_sd#c.tem_sd) baselevels level(90) mcolor("0 175 80")  ciopts(color("0 175 80") lwidth(medthick)) ms(Dh)) ///
(r8, keep(1.poor#c.tem#c.tem 1.poor#c.tem_sd#c.tem_sd) mcolor("0 175 80") ciopts(color("0 175 80") lwidth(medthick)) ms(Sh)) ///
(m2, keep(1._at) baselevels level(90) mcolor("0 175 80")  ciopts(color("0 175 80") lwidth(medthick)) ms(Dh)) ///
(m2, keep(2._at) baselevels level(90) mcolor("0 175 80")  ciopts(color("0 175 80") lwidth(medthick)) ms(Sh)) ///
,xline(0,lwidth(thin) lcolor(black) lpattern(dash))  xlabel(-0.004(0.002)0.012, labsize(small) nogrid) 
graph save Graph  "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDP_figure4a.png", replace

//Heterogeneity analysis for the effects of precipitation, precipitation change, and precipitation variability (Figure 4b, it has been further enhanced through Adobe Illustration)
qui reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre  pop edu,absorb(i.year gdlcode1##c.year) vce( cluster country1)
qui margins,dydx(dP) at( tem_sd=0 pre_sd = 0) atmeans post level(90)
estimates store m3
qui reghdfe dgdp poor#c.(c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd  c.dT c.dT#c.tem c.dP c.dP#c.pre ) edu pop,absorb(i.year gdlcode1##c.year) vce( cluster country1)
qui margins,dydx(dP) at(poor = (0 1) tem_sd=0 pre_sd = 0) atmeans post level(90)
estimates store m4
coefplot ///
(r1, keep(c.pre#c.pre c.pre_sd#c.pre_sd ) mcolor("0 175 80%60") ciopts(color("0 175 80") lwidth(medthick)) ms(O) ) /// 
(m3, mcolor("0 175 80%60") ciopts(color("0 175 80") lwidth(medthick)) ms(O) ) /// 
(r8, keep(0.poor#c.pre#c.pre 0.poor#c.pre_sd#c.pre_sd) baselevels level(90) mcolor("0 175 80")  ciopts(color("0 175 80") lwidth(medthick)) ms(Dh)) ///
(r8, keep(1.poor#c.pre#c.pre 1.poor#c.pre_sd#c.pre_sd) mcolor("0 175 80") ciopts(color("0 175 80") lwidth(medthick)) ms(Sh)) ///
(m4, keep(1._at) baselevels level(90) mcolor("0 175 80")  ciopts(color("0 175 80") lwidth(medthick)) ms(Dh)) ///
(m4, keep(2._at) baselevels level(90) mcolor("0 175 80")  ciopts(color("0 175 80") lwidth(medthick)) ms(Sh)) ///
,xline(0,lwidth(thin) lcolor(black) lpattern(dash))  xlabel(-0.025(0.005)0.015, labsize(small) nogrid) 
graph save Graph  "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDP_figure4b.png", replace

//Heterogeneity analysis for the interaction effect of temperature (Figure 4c)
qui reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre poor#((c.tem_sd##c.tem_sd)#(c.tem##c.tem))   pop edu,absorb(i.year gdlcode1##c.year) vce( cluster country1)
qui margins, over(poor) at(tem_sd=(-4(0.2)4) pre_sd=0 tem=10) atmeans post level(90)
preserve
parmest, norestore level(90)
egen  x = seq(), from(-20) to(20) block(2)
replace x = x/5
split parm, p("." "#" )
rename parm3 poor
destring poor, replace
drop parm*
gen est_poor = estimate if poor == 1
gen min_poor = min90 if poor == 1
gen max_poor = max90 if poor == 1
gen est_rich = estimate if poor == 0
gen min_rich = min90 if poor == 0
gen max_rich = max90 if poor == 0
twoway ///
(rarea min_rich max_rich x if poor ==0 , fcolor( "136 168 0%30") lcolor(white) msize(small )) ///
(rarea min_poor max_poor x if poor ==1 , fcolor("0 180 250%30") lcolor(white) msize(small )) ///
(line est_rich x if poor ==0 , lcolor("136 168 0" ) ) ///
(line est_poor x if poor ==1 , lcolor("0 180 250") ) ///
, xlabel(-4(1)4)  ylabel(-0.1(0.1)0.1,nogrid) ///
 yline(0,lwidth(thin) lcolor(black)) ///
 legend(lab(1 "Rich CI-90%") lab(2 "Poor CI-90%") lab(3 "Rich regions") lab(4 "Poor regions")) xtitle(AST) ytitle(GDP per capital growth) graphregion(color(white))
 graph save Graph "figure\GDP_inter_T10.png",replace
restore

//Heterogeneity analysis for the interaction effect of precipitation (Figure 4e)
qui reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre poor#((c.pre_sd##c.pre_sd)#(c.pre##c.pre))   pop edu,absorb(i.year gdlcode1##c.year) vce( cluster country1)
margins, over(poor) at(pre_sd=(-4(0.2)4) pre= 1.2 tem_sd=0 ) atmeans post level(90)
preserve
parmest, norestore level(90)
egen  x = seq(), from(-20) to(20) block(2)
replace x = x/5
split parm, p("." "#" )
rename parm3 poor
destring poor, replace
drop parm*
gen est_poor = estimate if poor == 1
gen min_poor = min90 if poor == 1
gen max_poor = max90 if poor == 1
gen est_rich = estimate if poor == 0
gen min_rich = min90 if poor == 0
gen max_rich = max90 if poor == 0
twoway ///
(rarea min_rich max_rich x if poor ==0 , fcolor( "136 168 0%30") lcolor(white) msize(small )) ///
(rarea min_poor max_poor x if poor ==1 , fcolor("0 180 250%30") lcolor(white) msize(small )) ///
(line est_rich x if poor ==0 , lcolor("136 168 0" ) ) ///
(line est_poor x if poor ==1 , lcolor("0 180 250") ) ///
, xlabel(-4(1)4)  ylabel(-0.1(0.1)0.1,nogrid) ///
 yline(0,lwidth(thin) lcolor(black)) ///
 legend(lab(1 "Rich CI-90%") lab(2 "Poor CI-90%") lab(3 "Rich regions") lab(4 "Poor regions")) xtitle(ASP) ytitle(GDP per capital growth) graphregion(color(white))
graph save Graph "figure\GDP_inter_P1.2.png",replace
restore