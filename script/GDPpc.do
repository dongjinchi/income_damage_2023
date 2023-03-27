clear all

cd E:\06博士论文\002出国交流\02cliamte_economy\global\income_damage_2023\data\socieconomic
insheet using GDPpc.csv ,clear

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
drop if missing(dgdp, tem,pre, tem_sd,pre_sd,pop,edu)
xtserial dgdp tem pre tem_sd pre_sd dT dP,output //serial correlation test
xtbalance,range(1991 2015)
local tests llc ips ht
local vars dgdp tem pre tem_sd pre_sd dT dP
foreach var of loc vars  {
	xtcdf `var' // cross-section test
	foreach test of loc tests {
		xtunitroot `test' `var',trend demean //unit root tests
	}
}
restore

*mian regression
//This study-type model
reghdfe dgdp c.dT c.dT#c.tem  c.dP c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd   edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r1, title("DTW-type")
esttab r1 using r1.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)
margins, dydx(tem) at(tem=(-1(1)31) tem_sd = 0 pre_sd = 0) atmeans post level(90) 
margins, dydx(pre) at(pre=(0(0.1)6) tem_sd = 0 pre_sd = 0) atmeans post level(90) 

//BHM-type model
reghdfe dgdp c.tem##c.tem c.pre##c.pre edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r2, title("BHM-type")
esttab r2 using r2.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)
margins, dydx(tem) at(tem=(-1(1)31)) atmeans post level(90) 
margins, dydx(pre) at(pre=(0(0.1)6)) atmeans post level(90) 

//KAL-type regression
reghdfe dgdp c.dT c.dT#c.tem  c.dP c.dP#c.pre  c.tem##c.tem c.pre##c.pre  edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r3, title("KAL-type")
esttab r3 using r3.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)
margins, dydx(tem) at(tem=(-1(1)31)) atmeans post level(90) 
margins, dydx(pre) at(pre=(0(0.1)6)) atmeans post level(90) 

*Interaction effects analysis
//Interaction between T and AST
reghdfe dgdp c.dT c.dT#c.tem  c.dP  c.dP#c.pre  c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd (c.tem_sd##c.tem_sd)#(c.tem##c.tem)   edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
test c.tem_sd#c.tem c.tem_sd#c.tem#c.tem c.tem_sd#c.tem_sd#c.tem c.tem_sd#c.tem_sd#c.tem#c.tem
estimates store r4, title("T_AST")
esttab r4 using r4.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)

//Interaction between T and ASP
reghdfe dgdp c.dT c.dT#c.tem  c.dP  c.dP#c.pre  c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd (c.pre_sd##c.pre_sd)#(c.tem##c.tem)   edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
test c.pre_sd#c.tem c.pre_sd#c.tem#c.tem c.pre_sd#c.pre_sd#c.tem c.pre_sd#c.pre_sd#c.tem#c.tem
estimates store r5, title("T_ASP")
esttab r5 using r5.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)

//Interaction between P and AST
reghdfe dgdp c.dT c.dT#c.tem  c.dP  c.dP#c.pre  c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd (c.tem_sd##c.tem_sd)#(c.pre##c.pre)   edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
test c.tem_sd#c.pre c.tem_sd#c.pre#c.pre c.tem_sd#c.tem_sd#c.pre c.tem_sd#c.tem_sd#c.pre#c.pre
estimates store r6, title("P_AST")
esttab r6 using r6.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)

//Interaction between P and ASP
reghdfe dgdp c.dT c.dT#c.tem  c.dP  c.dP#c.pre  c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd (c.pre_sd##c.pre_sd)#(c.pre##c.pre)   edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
test c.pre_sd#c.pre c.pre_sd#c.pre#c.pre c.pre_sd#c.pre_sd#c.pre c.pre_sd#c.pre_sd#c.pre#c.pre
estimates store r7, title("P_ASP")
esttab r7 using r7.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)

*Heterogeneity analysis
//Heterogeneity analysis of level and variation effects
reghdfe dgdp poor#(c.dT c.dT#c.tem  c.dP  c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd ) edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r8, title("Hetero_LV")
esttab r8 using r8.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)
margins, dydx(tem) over(poor) at(tem=(-1(1)31) pre_sd=0 tem_sd=0 ) atmeans post level(90)
margins, dydx(pre) over(poor) at(pre=(0(0.1)6) pre_sd=0 tem_sd=0 ) atmeans post level(90)

//Heterogeneity analysis of interaction effects
reghdfe dgdp c.dT c.dT#c.tem  c.dP  c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd  poor#((c.tem_sd##c.tem_sd)#(c.tem##c.tem)) edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r9, title("Hetero_IE_T")
esttab r9 using r9.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)
margins, dydx(tem_sd) over(poor) at(tem_sd=(-4(0.1)4) pre_sd=0 tem=19 ) atmeans post level(90)


reghdfe dgdp c.dT c.dT#c.tem  c.dP  c.dP#c.pre  c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd poor#((c.pre_sd##c.pre_sd)#(c.pre##c.pre))   edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r10, title("Hetero_IE_P")
esttab r10 using r10.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)
margins, dydx(pre_sd) over(poor) at(pre_sd=(-4(0.1)4) pre=5.0 tem_sd=0 ) atmeans post level(90)




forvalues i = -4(1)4 {
	reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.tem_sd##c.tem_sd)#(c.tem##c.tem)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
    margins,at(tem=(0(1)31) tem_sd=`i' pre_sd=0) atmeans post level(90)
	marginsplot
	local sname: di %01.0f `i'+4
	estimates store mf`sname'
}
esttab mf* using mf_tem_result.csv,cells("b se") nogap nonumber replace 


reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.tem_sd##c.tem_sd)#(c.tem##c.tem)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins, at(tem=(-1(1)31) tem_sd = -4 pre_sd = 0) atmeans post level(90) 
preserve
parmest, norestore level(90)
egen  x = seq(), from(-1) to(31)
drop parm
twoway ///
(rarea min max x, fcolor( "0 175 80%30") lcolor(%0) ) ///
(line est x , lcolor("0 175 80") )  ///
 , ///
xlabel(0(5)31, labsize(small)) ///
ylabel(-0.3(0.1)0.2, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Change in ln(GDI per capital), size(small)) xtitle(Annual average temperature(℃), size(small)) graphregion(color(white))

reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.tem_sd##c.tem_sd)#(c.tem##c.tem)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins,at(tem_sd=(-4(0.2)4)  tem = 30 pre_sd=0) atmeans post level(90)
preserve
parmest, norestore level(90)
egen  x = seq(), from(-20) to(20)
replace x = x/5
drop parm
twoway ///
(rarea min max x, fcolor( "190 50 190%30") lcolor(%0) ) ///
(line estimate x , lcolor("190 50 190") )  ///
 , xlabel(-4(1)4, labsize(small)) ///
ylabel(-0.3(0.1)0.2, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Margin effect of precipitation on GDI per capital change, size(small)) xtitle(Variation of precipitation, size(small)) graphregion(color(white))
 

forvalues i = -4(1)4 {
	reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.pre_sd##c.pre_sd)#(c.pre##c.pre)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
    margins,at(pre=(0(0.1)6) pre_sd=`i' tem_sd=0) atmeans post level(90)
	marginsplot
	local sname: di %01.0f `i'+4
	estimates store mf`sname'
}
esttab mf* using mf_pre_result.csv,cells("b se") nogap nonumber replace 

reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.pre_sd##c.pre_sd)#(c.pre##c.pre)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins, at(pre=(0(0.1)6) pre_sd = 0 tem_sd = 0) atmeans post level(90) 
preserve
parmest, norestore level(90)
egen  x = seq(), from(0) to(60)
replace x = x/10
drop parm
twoway ///
(rarea min max x, fcolor( "0 175 80%30") lcolor(%0) ) ///
(line est x , lcolor("0 175 80") )  ///
 , ///
xlabel(0(1)6, labsize(small)) ///
ylabel(-0.1(0.1)1.2, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Change in ln(GDI per capital), size(small)) xtitle(Annual average temperature(℃), size(small)) graphregion(color(white))

reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.pre_sd##c.pre_sd)#(c.pre##c.pre)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
test c.pre_sd#c.pre c.pre_sd#c.pre#c.pre c.pre_sd#c.pre_sd#c.pre c.pre_sd#c.pre_sd#c.pre#c.pre
margins, at(pre_sd=(-4(0.2)4) pre = 1.2 tem_sd = 0) atmeans post level(90) 
preserve
parmest, norestore level(90)
egen  x = seq(), from(-20) to(20)
replace x = x/5
drop parm
twoway ///
(rarea min max x, fcolor( "0 175 80%30") lcolor(%0) ) ///
(line est x , lcolor("0 175 80") )  ///
 , ///
xlabel(-4(1)4, labsize(small)) ///
ylabel(-0.1(0.1)0.8, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Change in ln(GDI per capital), size(small)) xtitle(Annual average temperature(℃), size(small)) graphregion(color(white))


***long difference regression***
local deltaT = 5

gen N_usd = gdp

sum year
gen y_max = 2015

gen p = floor(year / `deltaT')
sum p
gen p_max = r(max)

gen YY = year - y_max +`deltaT'*p_max -1
gen period = floor(YY/`deltaT')

collapse(mean) gdp tem pre tem_sd pre_sd edu pop ///
   (first) country1 ///
   (count) N_usd  ///
   , by(gdlcode1 period)

keep if N_usd>= floor(`deltaT'/2)

sum period
gen T_max = r(max)

gen year = period
xtset gdlcode1 year 

gen dT = d.tem
gen dP = d.pre
gen T1 = l.tem
gen P1 = l.pre
gen T3 = l3.tem
gen P3 = l3.pre
gen lngdp = log(gdp)
gen dgdp = d.lngdp

reghdfe dgdp c.dT c.dT#c.T1  c.dP c.dP#c.P1 c.T1##c.T1 c.P1##c.P1 c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd   edu pop if year == T_max,absorb(i.country1 ) vce( cluster country1)
estimates store r11, title("Hetero_IE_P")
esttab r11 using r11.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)
margins, at(T=(0(1)31) pre_sd=0 tem_sd=0 ) atmeans post level(90)
 
*Robustness checks
//
keep if kw_data == 1
collapse(mean) dgdp tem pre tem_sd pre_sd ///
   (first) country1 kw_data ///
   , by(iso_code year)
xtset country1 year 
gen dT = d.tem
gen dP = d.pre
reghdfe dgdp c.tem##c.tem c.pre##c.pre  ,absorb(i.country1 i.year i.country1#c.year) vce( cluster country1)
estimates store r12, title("KAL-type")
esttab r12 using r12.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)
//
reghdfe dgdp c.dT c.dT#c.tem  c.dP c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_wsd##c.pre_wsd   edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r13, title("KAL-type")
esttab r13 using r13.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)
margins, dydx(pre) at(pre=(0(0.1)6)) atmeans post level(90) 

//
reghdfe dgdp c.dT c.dT#c.tem  c.dP c.dP#c.pre c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd   edu pop [aweight = pop],absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r13, title("KAL-type")
esttab r13 using r13.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)

//
replace dT = d.tem_era
replace dP = d.pre_era
reghdfe dgdp c.dT c.dT#c.tem_era  c.dP c.dP#c.pre_era c.tem_era##c.tem_era c.pre_era##c.pre_era c.asp_tem##c.asp_tem c.asp_pre##c.asp_pre pop  edu ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r14, title("KAL-type")
esttab r14 using r14.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)
margins, dydx(tem) at(tem=(-1(1)31)) atmeans post level(90) 
margins, dydx(pre) at(pre=(0(0.1)6)) atmeans post level(90) 

*Descriptive statistics
//descriptive for Panel B
preserve
drop if missing(dgdp, tem_era,pre_era,wasp_tem,wasp_pre)
summ dgdp tem_era pre_era wasp_tem wasp_pre
duplicates drop gdlcode1, force
summ gdlcode1
duplicates drop country1, force
summ country1
restore
//descriptive for Panel C
preserve
drop if missing(gdp, tem,pre, tem_sd,pre_sd,pop,edu)
summ dgdp tem pre tem_sd pre_sd pop life edu
duplicates drop gdlcode1, force
summ gdlcode1
duplicates drop country1, force
summ country1
restore


*pre-regression tests
preserve
gen T_sqr = tem_era*tem_era
gen P_sqr = pre_era*pre_era
gen AST_sqr = asp_tem*asp_tem
gen ASP_sqr = asp_pre*asp_pre
gen tp = tem_era*pre_era
gen astp = asp_tem*asp_pre
gen dtt = dT*tem_era
gen dpp = dP*pre_era
gen dastt = dAST*asp_tem
gen daspp = dASP*asp_pre
gen tast = tem_era*asp_tem
gen tasp = tem_era*asp_pre
gen past = pre_era*asp_tem
gen pasp = pre_era*asp_pre
xtserial dgdp tem_era T_sqr pre_era P_sqr asp_tem AST_sqr asp_pre ASP_sqr tp astp dt dtt dp dpp dAST dastt dASP daspp tast tasp past pasp,output //serial correlation test

drop if missing(dgdp, tem_era,pre_era, asp_tem,asp_pre, pop,life, edu)
xtbalance,range(1991 2019)
local tests llc ips ht
local vars dgdp tem_era pre_era wasp_tem wasp_pre dT dP edu life pop
foreach var of loc vars  {
	xtcdf `var' // cross-section test
	foreach test of loc tests {
		xtunitroot `test' `var',trend demean //unit root tests
	}
}
restore

*BHM-type regression 
reghdfe dgdp c.tem##c.tem c.pre##c.pre pop edu ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r0, title("KAL-type")
esttab r0 using r0.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)

*KAL-type regression
reghdfe dgdp c.tem##c.tem c.pre##c.pre c.dT c.dT#c.tem c.dP  c.dP#c.pre pop edu ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r1, title("KAL-type")
esttab r1 using r1.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)
reghdfe dgdp c.tem##c.tem c.pre##c.pre c.l(0/1).dT c.l(0/1).dT#c.tem  c.l(0/1).dP c.l(0/1).dP#c.pre ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)


*this study-type regression
reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre pop edu ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r2, title("KAL-type")
esttab r2 using r2.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)
reghdfe dgdp c.tem##c.tem c.pre##c.pre c.l(0/1).dT c.l(0/1).dT#c.tem  c.l(0/1).dP c.l(0/1).dP#c.pre  c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)


*this study-type regression-weighted by pop
reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre pop edu [aweight=pop],absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r3, title("KAL-type")
esttab r3 using r3.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)

*moderating effects-T*P
reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre  c.tem#c.pre  pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r4, title("KAL-type")
esttab r4 using r4.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)

*moderating effects-T*(P2+P)
reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre  c.tem#(c.pre##c.pre)  pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r5, title("KAL-type")
esttab r5 using r5.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)


*moderating effects-ASP*(P2+P)
reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre poor#(c.pre_sd##c.pre_sd)#(c.pre##c.pre)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
test 0.poor#c.pre_sd#c.pre 0.poor#c.pre_sd#c.pre#c.pre 0.poor#c.pre_sd#c.pre_sd#c.pre 0.poor#c.pre_sd#c.pre_sd#c.pre#c.pre
estimates store r10, title("KAL-type")
esttab r10 using r10.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)

reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre poor#(c.tem_sd##c.tem_sd)#(c.tem##c.tem)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
test 0.poor#c.tem_sd#c.tem 0.poor#c.tem_sd#c.tem#c.tem 0.poor#c.tem_sd#c.tem_sd#c.tem 0.poor#c.tem_sd#c.tem_sd#c.tem#c.tem
estimates store r11, title("KAL-type")
esttab r11 using r11.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)


reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.tem_sd##c.tem_sd)#(c.pre##c.pre)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r12, title("KAL-type")
test c.tem_sd#c.pre c.tem_sd#c.pre#c.pre c.tem_sd#c.tem_sd#c.pre c.tem_sd#c.tem_sd#c.pre#c.pre
esttab r12 using r12.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)

reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.pre_sd##c.pre_sd)#(c.tem##c.tem)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r13, title("KAL-type")
test c.pre_sd#c.tem c.pre_sd#c.tem#c.tem c.pre_sd#c.pre_sd#c.tem c.pre_sd#c.pre_sd#c.tem#c.tem
esttab r13 using r13.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)



gen bins  = floor(year/5)
egen gdp_region_mean = mean(gdp),by(gdlcode1 bins)
egen gdp_median = median(gdp_region_mean)
gen poor = (gdp_region_mean <= gdp_median)

reghdfe dgdp poor#(c.tem##c.tem c.pre##c.pre) c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre pop edu ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r11, title("KAL-type")
esttab r11 using r11.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)

*Robust check
*replace weather database from CRU to ERA
replace dT = d.tem_era
replace dP = d.pre_era
*BHM-type regression 
reghdfe dgdp c.tem_era##c.tem_era c.pre_era##c.pre_era pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r1, title("BHM-type")

*KAL-type regression 
reghdfe dgdp c.tem_era##c.tem_era c.pre_era##c.pre_era c.dT c.dT#c.tem_era  c.dP  c.dP#c.pre_era pop edu ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r2, title("KAL-type")

egen minyear = min(year),by(gdlcode1)
gen trend = year-minyear
*this study regression type
reghdfe dgdp c.tem_era##c.tem_era c.pre_era##c.pre_era c.asp_tem##c.asp_tem c.asp_pre##c.asp_pre  c.dT c.dT#c.tem_era c.dP c.dP#c.pre_era  edu [aweight = pop],absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store r3, title("DRS-type-regions")

reghdfe dgdp c.tem_era##c.tem_era c.pre_era##c.pre_era c.asp_tem##c.asp_tem c.asp_pre##c.asp_pre  c.dT c.dT#c.tem_era c.dP c.dP#c.pre_era  c.tem_era#(c.pre_era##c.pre_era)  pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)

reghdfe dgdp c.tem_era##c.tem_era c.pre_era##c.pre_era c.asp_tem##c.asp_tem c.asp_pre##c.asp_pre  c.dT c.dT#c.tem_era c.dP c.dP#c.pre_era  c.tem_era#(c.asp_tem##c.asp_tem)  pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)

reghdfe dgdp c.tem_era##c.tem_era c.pre_era##c.pre_era c.asp_tem##c.asp_tem c.asp_pre##c.asp_pre  c.dT c.dT#c.tem_era c.dP c.dP#c.pre_era  c.tem_era#(c.asp_pre##c.asp_pre)  pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)

*plot for figures
//margins for temperature
preserve
reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre pop edu ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins, at(tem=(-1(1)31) tem_sd = 0 pre_sd = 0) atmeans post level(90) 
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
 legend(off)  ytitle(Change in ln(GDI per capital), size(small)) xtitle(Annual average temperature(℃), size(small)) graphregion(color(white))
graph save Graph "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDP_marg_T",replace
restore

//margins effect of temperature
preserve
reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre pop edu ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins, dydx(tem) at(tem=(-1(1)31) tem_sd = 0 pre_sd=0 ) atmeans post level(90)
parmest, norestore level(90)
egen  x = seq(), from(-1) to(31)
drop parm
twoway ///
(rarea min max x, fcolor( "0 175 80%30") lcolor(%0) ) ///
(line est x , lcolor("0 175 80") )  ///
 , xlabel(0(5)31, labsize(small)) ///
ylabel(-0.06(0.01)0.03, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Margin effect on GDI per capital change, size(small)) xtitle(Annual average temperature(℃), size(small)) graphregion(color(white))
 graph save Graph "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDP_marg_effect_T",replace
restore

//margins for precipitation
preserve
reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre pop edu ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins, at(pre=(0(0.1)6) tem_sd=0 pre_sd=0) atmeans post level(95)
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
 legend(off)  ytitle(Change in ln(GDI per capital), size(small)) xtitle(Annual average precipitation(m), size(small)) graphregion(color(white))
graph save Graph "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDP_marg_P",replace
restore
 
//margins effect of precipitation on the change of economy
preserve
reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre pop edu ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins,dydx(pre) at(pre=(0(0.1)6) tem_sd=0 pre_sd=0) atmeans post level(95)
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
 legend(off)  ytitle(Margin effect on GDI per capital change, size(small)) xtitle(Annual average precipitation(m), size(small)) graphregion(color(white))
 graph save Graph "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDP_marg_effect_P",replace
restore

***heterogeneity analysis bwetween poor and rich regions***
gen bins  = floor(year/5)
egen gdp_region_mean = mean(gdp),by(gdlcode1 bins)
egen gdp_median = median(gdp_region_mean)
gen poor = (gdp_region_mean <= gdp_median)

reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd  c.dT c.dT#c.tem c.dP c.dP#c.pre  edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store h1
reghdfe dgdp poor#c.(c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd  c.dT c.dT#c.tem c.dP c.dP#c.pre ) edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
estimates store h2

reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre  pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins,dydx(dT) at( tem = 19 tem_sd=0 pre_sd = 0) atmeans post level(90)
marginsplot
estimates store h3

reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre  pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins,dydx(dT) at( pre= 1.2 tem_sd=0 pre_sd = 0) atmeans post level(90)
estimates store h4
reghdfe dgdp poor#c.(c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd  c.dT c.dT#c.tem c.dP c.dP#c.pre ) edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins,dydx(dT) at(poor = (0 1) tem = 19 tem_sd=0 pre_sd = 0) atmeans post level(90)
estimates store h5
reghdfe dgdp poor#c.(c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd  c.dT c.dT#c.tem c.dP c.dP#c.pre ) edu pop,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins,dydx(dT) at(poor = (0 1) pre=1.2 tem_sd=0 pre_sd = 0) atmeans post level(90)
estimates store h6

coefplot ///
(h1, keep(c.pre#c.pre c.pre_sd#c.pre_sd c.dP#c.pre) mcolor("0 175 80%60") ciopts(color("0 175 80") lwidth(medthick)) ms(O) ) /// 
(h4, mcolor("0 175 80%60") ciopts(color("0 175 80") lwidth(medthick)) ms(O) ) /// 
(h2, keep(0.poor#c.pre#c.pre 0.poor#c.pre_sd#c.pre_sd 0.poor#c.dP#c.pre) baselevels level(90) mcolor("0 175 80")  ciopts(color("0 175 80") lwidth(medthick)) ms(Dh)) ///
(h2, keep(1.poor#c.pre#c.pre 1.poor#c.pre_sd#c.pre_sd 1.poor#c.dP#c.pre) mcolor("0 175 80") ciopts(color("0 175 80") lwidth(medthick)) ms(Sh)) ///
(h6,  baselevels level(90) mcolor("0 175 80")  ciopts(color("0 175 80") lwidth(medthick)) ms(Dh)) ///
,xline(0,lwidth(thin) lcolor(black) lpattern(dash))  xlabel(-0.02(0.005)0.015, labsize(small) nogrid) 

coefplot ///
(h1, keep(c.tem#c.tem c.tem_sd#c.tem_sd ) mcolor("0 175 80%60") ciopts(color("0 175 80") lwidth(medthick)) ms(O) ) ///
(h3, mcolor("0 175 80%60") ciopts(color("0 175 80") lwidth(medthick)) ms(O) ) /// 
(h2, keep(0.poor#c.tem#c.tem 0.poor#c.tem_sd#c.tem_sd) baselevels level(90) mcolor("0 175 80")  ciopts(color("0 175 80") lwidth(medthick)) ms(Dh)) ///
(h2, keep(1.poor#c.tem#c.tem 1.poor#c.tem_sd#c.tem_sd) mcolor("0 175 80") ciopts(color("0 175 80") lwidth(medthick)) ms(Sh)) ///
(h5,  baselevels level(90) mcolor("0 175 80")  ciopts(color("0 175 80") lwidth(medthick)) ms(Dh)) ///
,xline(0,lwidth(thin) lcolor(black) lpattern(dash))  xlabel(-0.004(0.002)0.012, labsize(small) nogrid) 


graph save Graph  "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDI_hetero", replace

preserve
reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.tem#(c.tem_sd##c.tem_sd) c.dT c.dT#c.tem  c.dP  c.dP#c.pre pop edu ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins,dydx(tem) at(tem_sd=(-4(0.2)4)  pre_sd=0) atmeans post level(90)
parmest, norestore level(90)
egen  x = seq(), from(-20) to(20)
replace x = x/5
drop parm
twoway ///
(rarea min max x, fcolor( "190 50 190%30") lcolor(%0) ) ///
(line estimate x , lcolor("190 50 190") )  ///
 , xlabel(-4(1)4, labsize(small)) ///
ylabel(-0.04(0.005)0.02, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off)  ytitle(Margin effect of precipitation on GDI per capital change, size(small)) xtitle(Variation of precipitation, size(small)) graphregion(color(white))
 graph save Graph "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDI_moder_T",replace
restore


preserve
reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.pre#(c.pre_sd##c.pre_sd) c.dT c.dT#c.tem  c.dP  c.dP#c.pre pop edu ,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins,dydx(pre) at(pre_sd=(-4(0.2)4)  tem_sd=0) atmeans post level(95)
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

forvalues i = -4(1)4 {
	reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.pre_sd##c.pre_sd)#(c.pre##c.pre)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
    margins,at(pre=(0(0.1)6) pre_sd=`i' tem_sd=0) atmeans post level(90)
	marginsplot
	local sname: di %01.0f `i'+4
	estimates store mf`sname'
}
esttab mf* using mf_pre_result.csv,cells("b se") nogap nonumber replace 

reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre poor#((c.pre_sd##c.pre_sd)#(c.pre##c.pre))   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins, over(poor) at(pre_sd=(-4(0.2)4) pre= 5.0 tem_sd=0) atmeans post level(90)
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
, xlabel(-4(1)4)  ylabel(-0.2(0.1)0.8,nogrid) ///
 yline(0,lwidth(thin) lcolor(black)) ///
 legend(lab(1 "Rich CI-90%") lab(2 "Poor CI-90%") lab(3 "Rich regions") lab(4 "Poor regions")) xtitle(Annual average temperature(℃)) ytitle(Change in ln(GDI per capital)) graphregion(color(white))

forvalues i = 0(1)6 {
	reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.pre_sd##c.pre_sd)#(c.pre##c.pre)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
    margins,at(pre_sd=(-4(0.2)4) pre=`i' tem_sd=0) atmeans post level(90)
	marginsplot
	local sname: di %01.0f `i'+4
	estimates store mf`sname'
}
esttab mf* using mf_pre_sd_result.csv,cells("b se") nogap nonumber replace 

forvalues i = -4(1)4 {
	reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.tem_sd##c.tem_sd)#(c.tem##c.tem)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
    margins,at(tem=(0(1)31) tem_sd=`i' pre_sd=0) atmeans post level(90)
	marginsplot
	local sname: di %01.0f `i'+4
	estimates store mf`sname'
}
esttab mf* using mf_tem_result.csv,cells("b se") nogap nonumber replace 

reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre poor#((c.tem_sd##c.tem_sd)#(c.tem##c.tem))   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
margins, over(poor) at(tem_sd=(-4(0.2)4) tem= 30 pre_sd=0) atmeans post level(90)
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
, xlabel(-4(1)4)  ylabel(-0.2(0.1)0.2,nogrid) ///
 yline(0,lwidth(thin) lcolor(black)) ///
 legend(lab(1 "Rich CI-90%") lab(2 "Poor CI-90%") lab(3 "Rich regions") lab(4 "Poor regions")) xtitle(Annual average temperature(℃)) ytitle(Change in ln(GDI per capital)) graphregion(color(white))

restore


forvalues i = 0(1)31 {
	reghdfe dgdp c.tem##c.tem c.pre##c.pre c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd c.dT c.dT#c.tem  c.dP  c.dP#c.pre (c.tem_sd##c.tem_sd)#(c.tem##c.tem)   pop edu,absorb(i.gdlcode1 i.year i.gdlcode1#c.year) vce( cluster country1)
    margins,at(tem_sd=(-4(0.2)4) tem=`i' pre_sd=0) atmeans post level(90)
	marginsplot
	local sname: di %01.0f `i'+4
	estimates store mf`sname'
}
esttab mf* using mf_tem_sd_result.csv,cells("b se") nogap nonumber replace 



//hist_figure for gdp under different temperatures
preserve 
gen bins  = floor(tem*10/5)*5/10
egen gdp_total  =sum(gdp)
egen gdp_by = sum(gdp), by(bins)
gen gdp_dist = gdp_by/gdp_total
duplicates drop bins-gdp_dist, force
twoway ///
(bar gdp_dist bins  if bins>=0 & bins<=31, barwidth(0.5) fcolor( bluishgray)  ) , ///
yscale(off) ylabel(,nogrid) ytitle("") ///
 xtitle(Annual average temperature(℃), size(small)) ///
 t2title(Global distribution of GDP, size(small)) ///
 xlabel(0(5)31, labsize(small)) ///
 graphregion(color(white)) ///
 aspectratio(0.1) ysize(1)
graph save Graph  "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDP_hist_T", replace
restore

//hist_figure for gdp under different precipitations
preserve 
gen bins  = floor(pre*10)/10
egen gdp_total  =sum(gdp)
egen gdp_by = sum(gdp), by(bins)
gen gdp_dist = gdp_by/gdp_total
duplicates drop bins-gdp_dist, force
twoway ///
(bar gdp_dist bins if bins >=0 & bins <=6, barwidth(0.1) fcolor( bluishgray) ) , ///
yscale(off) ylabel(,nogrid)  ///
 xtitle(Annual average precipitation(m), size(small)) ///
 t2title(Global distribution of GDP, size(small)) ///
 xlabel(0(0.5)6, labsize(small)) ///
 graphregion(color(white)) ///
 aspectratio(0.1) ysize(1)
graph save Graph  "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDP_hist_P", replace
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
graph save Graph  "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDP_hist_TV", replace
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
graph save Graph  "E:\06博士论文\002出国交流\02cliamte_economy\global\plot\GDP_hist_PV", replace
restore

