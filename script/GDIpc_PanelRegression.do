clear all

cd E:\06博士论文\002出国交流\02cliamte_economy\global\income_damage_2023
insheet using data\socieconomic\GDIpc.csv ,clear

*****************************************
*      panel regression for GDIpc       *
*****************************************

*Variable generation
encode Country, gen(country1)
encode gdlcode, gen(gdlcode1)
xtset gdlcode1 year 
drop if total == 2 //remove national mean data
gen dgdi = d.gdi
gen dT = d.tem
gen dP = d.pre
gen bins  = floor(year/5)
egen gdi_region_mean = mean(gdi),by(gdlcode1 bins)
egen gdi_median = median(gdi_region_mean)
gen poor = (gdi_region_mean <= gdi_median) // generate dummy varibale for heterogeneity analysis


*Descriptive statistics
//descriptive for Panel A (Table S1)
preserve
drop if missing(dgdi, tem,pre,tem_sd,pre_sd)
summ dgdi tem pre tem_sd pre_sd
duplicates drop gdlcode1, force
summ gdlcode1
duplicates drop country1, force
summ country1
restore
//descriptive for Panel C (Table S1)
preserve
drop if missing(pop, edu)
summ pop edu
duplicates drop gdlcode1, force
summ gdlcode1
duplicates drop country1, force
summ country1
restore
//descriptive for Panel A-C (Table S2)
preserve
drop if missing(dgdi, tem,pre, tem_sd,pre_sd,pop,edu)
drop if year >2015
summ dgdi tem pre tem_sd pre_sd pop edu
duplicates drop gdlcode1, force
summ gdlcode1
duplicates drop country1, force
summ country1
restore

*pre-regression tests
preserve
drop if missing(dgdi, tem,pre, tem_sd,pre_sd,pop,edu)
drop if year >2015
xtserial dgdi tem pre tem_sd pre_sd dT dP,output //serial correlation test (Table S3)
xtbalance,range(1991 2015)
local tests llc ips ht
local vars dgdi tem pre tem_sd pre_sd dT dP
foreach var of loc vars  {
	xtcdf `var' // cross-section test (Table S4)
	foreach test of loc tests {
		xtunitroot `test' `var',trend demean //unit root tests (Table S5)
	}
}
restore

keep if year <=2015
*mian regression
//This study-type model (Table S6)
reghdfe dgdi c.dT c.dT#c.tem  c.dP c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r1, title("DTW-type")
esttab r1 using r1.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)
margins, dydx(tem) at(tem=(-1(1)31) tem_sd = 0 pre_sd = 0) atmeans post level(90) 
margins, dydx(pre) at(pre=(0(0.1)6) tem_sd = 0 pre_sd = 0) atmeans post level(90) 

//BHM-type model (Table S6)
reghdfe dgdi c.tem##c.tem c.pre##c.pre edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r2, title("BHM-type")
margins, dydx(tem) at(tem=(-1(1)31)) atmeans post level(90) 
margins, dydx(pre) at(pre=(0(0.1)6)) atmeans post level(90) 

//KAL-type regression (Table S6)
reghdfe dgdi c.dT c.dT#c.tem  c.dP c.dP#c.pre  c.tem##c.tem c.pre##c.pre  edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r3, title("KAL-type")
margins, dydx(tem) at(tem=(-1(1)31)) atmeans post level(90) 
margins, dydx(pre) at(pre=(0(0.1)6)) atmeans post level(90) 

*Interaction effects analysis (Table S7)
local levs tem pre
local vars tem_sd pre_sd 
local i 3
foreach lev of loc levs  {
	foreach var of loc vars {
		reghdfe dgdi c.dT c.dT#c.tem  c.dP  c.dP#c.pre  c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd (c.`lev'##c.`lev')#(c.`var'##c.`var') edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
		test c.`lev'#c.`var' c.`lev'#c.`var'#c.`var' c.`lev'#c.`lev'#c.`var' c.`lev'#c.`lev'#c.`var'#c.`var'
		local i: di %01.0f `i'+1
	    estimates store r`i'
	}
}

*Heterogeneity analysis
//Heterogeneity analysis of level and variation effects (Table S8)
reghdfe dgdi poor#(c.dT c.dT#c.tem  c.dP  c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd ) edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r8, title("Hetero_LV")
esttab r8 using r8.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)
margins, dydx(tem) over(poor) at(tem=(-1(1)31) pre_sd=0 tem_sd=0 ) atmeans post level(90)
margins, dydx(pre) over(poor) at(pre=(0(0.1)6) pre_sd=0 tem_sd=0 ) atmeans post level(90)

//Heterogeneity analysis of interaction effects between T and AST (Table S9)
reghdfe dgdi c.dT c.dT#c.tem  c.dP  c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd  poor#((c.tem_sd##c.tem_sd)#(c.tem##c.tem)) edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r9, title("Hetero_IE_T")
margins, dydx(tem_sd) over(poor) at(tem_sd=(-4(0.1)4) pre_sd=0 tem=30 ) atmeans post level(90)

//Heterogeneity analysis of interaction effects between P and ASP (Table S9)
reghdfe dgdi c.dT c.dT#c.tem  c.dP  c.dP#c.pre  c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd poor#((c.pre_sd##c.pre_sd)#(c.pre##c.pre))   edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r10, title("Hetero_IE_P")
margins, dydx(pre_sd) over(poor) at(pre_sd=(-4(0.1)4) pre=4 tem_sd=0 ) atmeans post level(90)

*Robustness checks
// Replace ASP by WASP (Table S13)
reghdfe dgdi c.dT c.dT#c.tem  c.dP c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_wsd##c.pre_wsd edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r11, title("KAL-type")

// Replace CRU by ERA5 (Table S12)
replace dT = d.tem_era
replace dP = d.pre_era
reghdfe dgdi c.dT c.dT#c.tem_era  c.dP c.dP#c.pre_era c.tem_era##c.tem_era c.pre_era##c.pre_era c.asp_tem##c.asp_tem c.asp_pre##c.asp_pre pop  edu ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r11, title("KAL-type")


*plot
//effect of annual mean temperature (Figure 2a)
qui reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre pop edu ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins, at(tem=(-1(1)31) tem_sd = 0 pre_sd = 0) atmeans post level(90)
preserve
parmest, norestore level(90)
egen  x = seq(), from(-1) to(31)
drop parm
twoway ///
(rarea min max x, fcolor( "190 50 190%30") lcolor(%0) ) ///
(line est x , lcolor("190 50 190") )  ///
 , xline(10,lwidth(thin) lcolor("190 50 190") lpattern(dash)) ///
xlabel(0(5)31, labsize(small)) ///
ylabel(-0.4(0.1)0.3, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Change in ln(GDI per capital), size(small)) xtitle(Annual average temperature(℃), size(small)) graphregion(color(white))
graph save Graph "figure\GDI_T.png",replace
restore

//margins effect of annual mean temperature (Figure 2c)
preserve
reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre pop edu ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins, dydx(tem) at(tem=(-1(1)31) tem_sd = 0 pre_sd=0 ) atmeans post level(90)
parmest, norestore level(90)
egen  x = seq(), from(-1) to(31)
drop parm
twoway ///
(rarea min max x, fcolor( "190 50 190%30") lcolor(%0) ) ///
(line est x , lcolor("190 50 190") )  ///
 , xlabel(0(5)31, labsize(small)) ///
ylabel(-0.06(0.01)0.03, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Margin effect on GDI per capital change, size(small)) xtitle(Annual average temperature(℃), size(small)) graphregion(color(white))
 graph save Graph "figure\GDI_marg_T.png",replace
restore

//effect of annual total precipitation (Figure 2b)
preserve
reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre pop edu ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins, at(pre=(0(0.1)6) tem_sd=0 pre_sd=0) atmeans post level(95)
parmest, norestore level(90)
egen  x = seq(), from(0) to(60)
replace x = x/10
drop parm
twoway ///
(rarea min max x, fcolor( "190 50 190%30") lcolor(%0) ) ///
(line est x , lcolor("190 50 190") )  ///
 , xline(2.5,lwidth(thin) lcolor("190 50 190") lpattern(dash)) ///
xlabel(0(1)6, labsize(small)) ///
ylabel(-0.2(0.1)0.2, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Change in ln(GDI per capital), size(small)) xtitle(Annual average precipitation(cm), size(small)) graphregion(color(white))
graph save Graph "figure\GDI_P.png",replace
restore
 
//margins effect of annual total precipitation (Figure 2c)
preserve
reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre pop edu ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins,dydx(pre) at(pre=(0(0.1)6) tem_sd=0 pre_sd=0) atmeans post level(95)
parmest, norestore level(90)
egen  x = seq(), from(0) to(60)
replace x = x/10
drop parm
twoway ///
(rarea min max x, fcolor( "190 50 190%30") lcolor(%0) ) ///
(line est x , lcolor("190 50 190") )  ///
 , xlabel(0(1)6, labsize(small)) ///
ylabel(-0.1(0.05)0.1, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Margin effect on GDI per capital change, size(small)) xtitle(Annual average precipitation(cm), size(small)) graphregion(color(white))
 graph save Graph "figure\GDI_marg_P.png",replace
restore

//Figure 4c
qui reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre poor#((c.tem_sd##c.tem_sd)#(c.tem##c.tem))   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins, over(poor) at(tem_sd=(-4(0.2)4) pre_sd=0) atmeans post level(90)
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
(rarea min_rich max_rich x if poor ==0 , fcolor( "154 4 44%60") lcolor(white) msize(small )) ///
(rarea min_poor max_poor x if poor ==1 , fcolor("131 80 200%60") lcolor(white) msize(small )) ///
(line est_rich x if poor ==0 , lcolor("154 4 44" ) ) ///
(line est_poor x if poor ==1 , lcolor("131 80 200") ) ///
, xlabel(-4(1)4)  ylabel(-0.1(0.1)0.3,nogrid) ///
 yline(0,lwidth(thin) lcolor(black)) ///
 legend(lab(1 "Rich CI-90%") lab(2 "Poor CI-90%") lab(3 "Rich regions") lab(4 "Poor regions")) xtitle(Annual average temperature(℃)) ytitle(Change in ln(GDI per capital)) graphregion(color(white))
 graph save Graph "figure\GDI_moder_T1",replace
restore










forvalues i = -4(1)4 {
	reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.tem_sd##c.tem_sd)#(c.tem##c.tem)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
    margins,at(tem=(0(1)31) tem_sd=`i' pre_sd=0) atmeans post level(90)
	marginsplot
	local sname: di %01.0f `i'+4
	estimates store mf`sname'
}
esttab mf* using mf_tem_result.csv,cells("b se") nogap nonumber replace 





reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre poor#((c.pre_sd##c.pre_sd)#(c.pre##c.pre))   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
test 0.poor#c.pre_sd#c.pre 0.poor#c.pre_sd#c.pre#c.pre 0.poor#c.pre_sd#c.pre_sd#c.pre 0.poor#c.pre_sd#c.pre_sd#c.pre#c.pre
margins, over(poor) at(pre_sd=(-4(0.2)4) pre= 5 tem_sd=0 ) atmeans post level(90)
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
(rarea min_rich max_rich x if poor ==0 , fcolor( "154 4 44%60") lcolor(white) msize(small )) ///
(rarea min_poor max_poor x if poor ==1 , fcolor("131 80 200%60") lcolor(white) msize(small )) ///
(line est_rich x if poor ==0 , lcolor("154 4 44" ) ) ///
(line est_poor x if poor ==1 , lcolor("131 80 200") ) ///
, xlabel(-4(0)4)  ylabel(-0.2(0.1)0.5,nogrid) ///
 yline(0,lwidth(thin) lcolor(black)) ///
 legend(lab(1 "Rich CI-90%") lab(2 "Poor CI-90%") lab(3 "Rich regions") lab(4 "Poor regions")) xtitle(Annual average temperature(℃)) ytitle(Change in ln(GDI per capital)) graphregion(color(white))

preserve
reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.tem_sd##c.tem_sd)#(c.tem##c.tem)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins,at(tem=(0(1)31) tem_sd=0 pre_sd=0) atmeans post level(90)
parmest, norestore level(90)
egen  x = seq(), from(-1) to(31)
drop parm
twoway ///
(rarea min max x, fcolor( "190 50 190%30") lcolor(%0) ) ///
(line est x , lcolor("190 50 190") )  ///
 , ///
xlabel(0(5)31, labsize(small)) ///
ylabel(-0.4(0.1)0.3, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Change in ln(GDI per capital), size(small)) xtitle(Annual average temperature(℃), size(small)) graphregion(color(white))
 graph save Graph "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDI_moder_T",replace
restore


reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.tem_sd##c.tem_sd)#(c.tem##c.tem)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins,at(tem_sd=(-4(0.2)4) tem=19 pre_sd=0) atmeans post level(90)
preserve
parmest, norestore level(90)
egen  x = seq(), from(-20) to(20)
replace x = x/5
drop parm
twoway ///
(rarea min max x, fcolor( "190 50 190%30") lcolor(%0) ) ///
(line est x , lcolor("190 50 190") )  ///
 , ///
xlabel(-4(2)4, labsize(small)) ///
ylabel(-0.4(0.1)0.3, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Change in ln(GDI per capital), size(small)) xtitle(Annual average temperature(℃), size(small)) graphregion(color(white))
 graph save Graph "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDI_moder_T",replace
restore

forvalues i = 0(5)30 {
	reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.tem_sd##c.tem_sd)#(c.tem##c.tem)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
    margins,at(tem_sd=(-4(0.2)4) tem=`i' pre_sd=0) atmeans post level(90)
	marginsplot
	local sname: di %01.0f `i'+4
	estimates store mfts`sname'
}
esttab mfts* using mf_tem_sd_result.csv,cells("b se") nogap nonumber replace 

preserve
reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.tem_sd##c.tem_sd)#(c.tem##c.tem)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins, at(tem_sd=(-4(0.2)4) tem=0 pre_sd=0) atmeans post level(90)
parmest, norestore level(90)
egen  x = seq(), from(-20) to(20)
replace x = x/5
drop parm
twoway ///
(rarea min max x, fcolor( "190 50 190%30") lcolor(%0) ) ///
(line estimate x , lcolor("190 50 190") )  ///
 , xlabel(-4(1)4, labsize(small)) ///
ylabel(-0.4(0.05)0.3, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle( Effect of temperature on GDI per capital change, size(small)) xtitle(Variation of temperature, size(small)) graphregion(color(white))
 graph save Graph "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDI_moder_T",replace
restore

forvalues i = -4(1)4 {
	reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.pre_sd##c.pre_sd)#(c.pre##c.pre)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
    margins,at(pre=(0(0.1)6) pre_sd=`i' tem_sd=0) atmeans post level(90)
	marginsplot
	local sname: di %01.0f `i'+4
	estimates store mf`sname'
}
esttab mf* using mf_result.csv,cells("b se") nogap nonumber replace 

preserve
reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.pre_sd##c.pre_sd)#(c.pre##c.pre)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins, at(pre=(0(0.1)6) tem_sd=0 pre_sd=0) atmeans post level(90)
parmest, norestore level(90)
egen  x = seq(), from(0) to(60)
replace x = x/10
drop parm
twoway ///
(rarea min max x, fcolor( "190 50 190%30") lcolor(%0) ) ///
(line est x , lcolor("190 50 190") )  ///
 ,  ///
xlabel(0(1)6, labsize(small)) ///
ylabel(-0.2(0.1)0.7, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Change in ln(GDI per capital), size(small)) xtitle(Annual average precipitation(cm), size(small)) graphregion(color(white))
graph save Graph "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDI_marg_P",replace
restore



forvalues i = 0(1)6 {
	reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.pre_sd##c.pre_sd)#(c.pre##c.pre)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
    margins,at(pre_sd=(-4(0.2)4) pre=`i' tem_sd=0) atmeans post level(90)
	marginsplot
	local sname: di %01.0f `i'+4
	estimates store mfps`sname'
}
esttab mfps* using mf_pre_sd_result.csv,cells("b se") nogap nonumber replace 

preserve
reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.pre_sd##c.pre_sd)#(c.pre##c.pre)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins, at(pre_sd=(-4(0.2)4) pre=0 pre_sd=0) atmeans post level(90)
parmest, norestore level(90)
egen  x = seq(), from(-20) to(20)
replace x = x/5
drop parm
twoway ///
(rarea min max x, fcolor( "190 50 190%30") lcolor(%0) ) ///
(line estimate x , lcolor("190 50 190") )  ///
 , xlabel(-4(1)4, labsize(small)) ///
ylabel(-0.4(0.05)0.3, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle( Effect of temperature on GDI per capital change, size(small)) xtitle(Variation of temperature, size(small)) graphregion(color(white))
 graph save Graph "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDI_moder_T",replace
restore

forvalues i = -4(1)4 {
	reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.pre_sd##c.pre_sd)#(c.tem##c.tem)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
    margins,at(tem=(0(1)31) pre_sd=`i' tem_sd=0) atmeans post level(90)
	marginsplot
	local sname: di %01.0f `i'+4
	estimates store mftps`sname'
}
esttab mftps* using mf_tem_presd_result.csv,cells("b se") nogap nonumber replace 

preserve
reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.tem_sd##c.tem_sd)#(c.tem##c.tem)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins,at(tem=(0(1)31) tem_sd=0 pre_sd=4) atmeans post level(90)
parmest, norestore level(90)
egen  x = seq(), from(-1) to(31)
drop parm
twoway ///
(rarea min max x, fcolor( "190 50 190%30") lcolor(%0) ) ///
(line est x , lcolor("190 50 190") )  ///
 , ///
xlabel(0(5)31, labsize(small)) ///
ylabel(-0.4(0.1)0.3, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Change in ln(GDI per capital), size(small)) xtitle(Annual average temperature(℃), size(small)) graphregion(color(white))
 graph save Graph "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDI_moder_T",replace
restore

preserve
reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.pre_sd##c.pre_sd)#(c.tem##c.tem)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins, at(pre_sd=(-4(0.2)4) tem=30 pre_sd=0) atmeans post level(90)
parmest, norestore level(90)
egen  x = seq(), from(-20) to(20)
replace x = x/5
drop parm
twoway ///
(rarea min max x, fcolor( "190 50 190%30") lcolor(%0) ) ///
(line estimate x , lcolor("190 50 190") )  ///
 , xlabel(-4(1)4, labsize(small)) ///
ylabel(-0.4(0.05)0.3, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle( Effect of temperature on GDI per capital change, size(small)) xtitle(Variation of temperature, size(small)) graphregion(color(white))
 graph save Graph "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDI_moder_T",replace
restore


forvalues i = -4(1)4 {
	reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.tem_sd##c.tem_sd)#(c.pre##c.pre)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
    margins,at(pre=(0(0.1)6) tem_sd=`i' pre_sd=0) atmeans post level(90)
	marginsplot
	local sname: di %01.0f `i'+4
	estimates store mfpts`sname'
}
esttab mfpts* using mf_pre_temsd_result.csv,cells("b se") nogap nonumber replace 


//margins effect of precipitation on the change of economy
preserve
reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.pre_sd##c.pre_sd)#(c.pre##c.pre)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins, at(pre_sd=(-4(0.2)4)  tem_sd=0) atmeans post level(95)
parmest, norestore level(90)
egen  x = seq(), from(-20) to(20)
replace x = x/5
drop parm
twoway ///
(rarea min max x, fcolor( "190 50 190%30") lcolor(%0) ) ///
(line estimate x , lcolor("190 50 190") )  ///
 , xlabel(-4(1)4, labsize(small)) ///
ylabel(-0.1(0.05)0.1, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Margin effect of precipitation on GDI per capital change, size(small)) xtitle(Variation of precipitation, size(small)) graphregion(color(white))
 graph save Graph "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDI_marg_effect_P",replace
restore


***heterogeneity analysis bwetween poor and rich regions***
gen bins  = floor(year/5)
egen gdi_region_mean = mean(gdi),by(gdlcode1 bins)
egen gdi_median = median(gdi_region_mean)
gen poor = (gdi_region_mean <= gdi_median)

reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd  c.dT c.dT#c.tem c.dP c.dP#c.pre  edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store h1
reghdfe dgdi poor#c.(c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd  c.dT c.dT#c.tem c.dP c.dP#c.pre ) edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store h2

reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre  pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins,dydx(dT) at( tem_sd=0 pre_sd = 0) atmeans post level(90)
marginsplot
estimates store h3

reghdfe dgdi c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre  pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins,dydx(dP) at( tem_sd=0 pre_sd = 0) atmeans post level(90)
estimates store h4
reghdfe dgdi poor#c.(c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd  c.dT c.dT#c.tem c.dP c.dP#c.pre ) edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins,dydx(dT) at(poor = (0 1) tem_sd=0 pre_sd = 0) atmeans post level(90)
estimates store h5
reghdfe dgdi poor#c.(c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd  c.dT c.dT#c.tem c.dP c.dP#c.pre ) edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins,dydx(dP) at(poor = (0 1) tem_sd=0 pre_sd = 0) atmeans post level(90)
estimates store h6

coefplot ///
(h1, keep(c.pre#c.pre c.pre_sd#c.pre_sd c.dP#c.pre) mcolor("190 50 190%60") ciopts(color("190 50 190") lwidth(medthick)) ms(O) ) /// 
(h4, mcolor("190 50 190%60") ciopts(color("190 50 190") lwidth(medthick)) ms(O) ) /// 
(h2, keep(0.poor#c.pre#c.pre 0.poor#c.pre_sd#c.pre_sd 0.poor#c.dP#c.pre) baselevels level(90) mcolor("190 50 190")  ciopts(color("190 50 190") lwidth(medthick)) ms(Dh)) ///
(h2, keep(1.poor#c.pre#c.pre 1.poor#c.pre_sd#c.pre_sd 1.poor#c.dP#c.pre) mcolor("190 50 190") ciopts(color("190 50 190") lwidth(medthick)) ms(Sh)) ///
(h6,  baselevels level(90) mcolor("190 50 190")  ciopts(color("190 50 190") lwidth(medthick)) ms(Dh)) ///
,xline(0,lwidth(thin) lcolor(black) lpattern(dash))  xlabel(-0.025(0.005)0.035, labsize(small) nogrid) 

coefplot ///
(h1, keep(c.tem#c.tem c.tem_sd#c.tem_sd ) mcolor("190 50 190%60") ciopts(color("190 50 190") lwidth(medthick)) ms(O) ) ///
(h3, mcolor("190 50 190%60") ciopts(color("190 50 190") lwidth(medthick)) ms(O) ) /// 
(h2, keep(0.poor#c.tem#c.tem 0.poor#c.tem_sd#c.tem_sd) baselevels level(90) mcolor("190 50 190")  ciopts(color("190 50 190") lwidth(medthick)) ms(Dh)) ///
(h2, keep(1.poor#c.tem#c.tem 1.poor#c.tem_sd#c.tem_sd) mcolor("190 50 190") ciopts(color("190 50 190") lwidth(medthick)) ms(Sh)) ///
(h5,  baselevels level(90) mcolor("190 50 190")  ciopts(color("190 50 190") lwidth(medthick)) ms(Dh)) ///
,xline(0,lwidth(thin) lcolor(black) lpattern(dash))  xlabel(-0.004(0.002)0.016, labsize(small) nogrid) 

graph save Graph  "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDI_hetero", replace




//hist_figure for gdi under different temperatures
preserve 
gen bins  = floor(tem*10/5)*5/10
egen gdi_total  =sum(gdi)
egen gdi_by = sum(gdi), by(bins)
gen gdi_dist = gdi_by/gdi_total
duplicates drop bins-gdi_dist, force
twoway ///
(bar gdi_dist bins  if bins>=0 & bins<=31, barwidth(0.5) fcolor( bluishgray)  ) , ///
yscale(off) ylabel(,nogrid) ytitle("") ///
 xtitle(Annual average temperature(℃), size(small)) ///
 t2title(Global distribution of GDI, size(small)) ///
 xlabel(0(5)31, labsize(small)) ///
 graphregion(color(white)) ///
 aspectratio(0.1) ysize(1)
graph save Graph  "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDI_hist_T", replace
restore

//hist_figure for gdi under different precipitations
preserve 
gen bins  = floor(pre*10)/10
egen gdi_total  =sum(gdi)
egen gdi_by = sum(gdi), by(bins)
gen gdi_dist = gdi_by/gdi_total
duplicates drop bins-gdi_dist, force
twoway ///
(bar gdi_dist bins if bins >=0 & bins <=6, barwidth(0.1) fcolor( bluishgray) ) , ///
yscale(off) ylabel(,nogrid)  ///
 xtitle(Annual average precipitation(cm), size(small)) ///
 t2title(Global distribution of GDI, size(small)) ///
 xlabel(0(0.5)6, labsize(small)) ///
 graphregion(color(white)) ///
 aspectratio(0.1) ysize(1)
graph save Graph  "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDI_hist_P", replace
restore

//hist_figure for temperature variation
preserve 
gen bins  = floor(tem_sd*10)/10
egen tem_sd_total  =count(tem_sd)
egen tem_sd_by = count(tem_sd), by(bins)
gen tem_sd_dist = tem_sd_by/tem_sd_total
duplicates drop bins-tem_sd_dist, force
twoway ///
(bar tem_sd_dist bins if bins>=-4 & bins<=4,barwidth(0.1) fcolor( bluishgray) ) , ///
yscale(off) ylabel(,nogrid) ytitle("") ///
 xtitle(Monthly temperature deviations, size(small)) ///
 xlabel(-4(0.5)4, labsize(small)) ///
 graphregion(color(white)) ///
 aspectratio(0.1) ysize(1)
graph save Graph  "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDI_hist_TV", replace
restore

//hist_figure for precipitation variation
preserve 
gen bins  = floor(pre_sd*10)/10
egen wasp_total  =count(pre_sd)
egen wasp_by = count(pre_sd), by(bins)
gen wasp_dist = wasp_by/wasp_total
duplicates drop bins-wasp_dist, force
twoway ///
(bar wasp_dist bins if bins>=-4 & bins<=4,barwidth(0.1) fcolor( bluishgray) ) , ///
yscale(off) ylabel(,nogrid) ytitle("") ///
 xtitle(Monthly precipitation deviations, size(small)) ///
 xlabel(-4(0.5)4, labsize(small)) ///
 graphregion(color(white)) ///
 aspectratio(0.1) ysize(1)
graph save Graph  "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDI_hist_PV", replace
restore

