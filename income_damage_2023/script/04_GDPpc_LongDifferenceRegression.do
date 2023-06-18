clear all

cd ~\income_damage_2023
insheet using data\socieconomic\GDPpc.csv ,clear

***************************************************
*      Long Difference Regression for GDIpc       *
***************************************************

*Variable generation
rename area_ID gdlcode1
encode country, gen(country1)
xtset gdlcode1 year 
gen dgdp = d.gdp
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
gen lngdp = log(gdp)
gen dgdp = d.lngdp

forvalues i = 1(1)4{
	gen T`i' = l`i'.tem
    gen P`i' = l`i'.pre
	reghdfe dgdp c.dT c.dT#c.T`i'  c.dP c.dP#c.P`i' c.T`i'##c.T`i' c.P`i'##c.P`i' c.tem_sd##c.tem_sd c.pre_sd##c.pre_sd   edu pop if year == T_max,absorb(i.country1 ) vce( cluster country1)
	estimates store ldr`i', title("LongDiff")
}
esttab ldr* using GDP_ldr.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)
