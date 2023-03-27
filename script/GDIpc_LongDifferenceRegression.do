clear all

cd E:\06博士论文\002出国交流\02cliamte_economy\global\income_damage_2023\data\socieconomic
insheet using GDIpc.csv ,clear

***************************************************
*      Long Difference Regression for GDIpc       *
***************************************************

*Variable generation
encode Country, gen(country1)
encode gdlcode, gen(gdlcode1)
xtset gdlcode1 year 
drop if total == 2 //remove national mean data
gen dgdi = d.gdi
keep if year <=2015
local deltaT = 5

gen N_usd = gdi

sum year
gen y_max = 2015

gen p = floor(year / `deltaT')
sum p
gen p_max = r(max)

gen YY = year - y_max +`deltaT'*p_max -1
gen period = floor(YY/`deltaT')

collapse(mean) gdi tem pre tem_sd pre_sd edu pop ///
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
gen dgdi = d.gdi

forvalues i = 1(1)4{
	gen T`i' = l`i'.tem
    gen P`i' = l`i'.pre
	reghdfe dgdi c.dT c.dT#c.T`i'  c.dP c.dP#c.P`i' c.T`i'##c.T`i' c.P`i'##c.P`i' c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd   edu pop if year == T_max,absorb(i.country1 ) vce( cluster country1)
	estimates store ldr`i', title("LongDiff")
    esttab ldr`i' using ldr`i'.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)
}

