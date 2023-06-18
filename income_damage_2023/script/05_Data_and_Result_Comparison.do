clear all

cd ~\income_damage_2023
insheet using data\socieconomic\Data_and_Result_Comparison.csv ,clear
*****************************************
*      Data and Result Comparison       *
*****************************************
encode country, gen(country1)
xtset country1 year
gen lngdp = log(gdp)
gen dgdp = d.lngdp
gen lgdi = log(gdi)
gen dgdi = d.lgdi
gen dT = d.tem
gen dP = d.pre


*Data Comparison
preserve
drop if missing(gdp_b, gdp, gdp_k)
//Regression analysis (Table S13)
reghdfe gdp_b dgdp, absorb(i.country1 i.year i.country1#c.year)
reghdfe gdp_k gdp_b, absorb(i.country1 i.year i.country1#c.year)
reghdfe gdp_k dgdp, absorb(i.country1 i.year i.country1#c.year)
//Correlation analysis (Table S14)
pwcorr gdp_b gdp_k dgdp, sig star (.05) print(.05)
spearman gdp_b gdp_k dgdp,pw star(0.01)
restore


*Results Comparison (Table S15)
drop if missing(gdp_b, dgdp, dgdi)
//World Bank database
reghdfe gdp_b c.tem##c.tem c.pre##c.pre pop edu,absorb(i.year country1##c.year ) vce( cluster country1)
estimates store r1, title("Country")
//Kummu Database-GDP
reghdfe dgdp c.tem##c.tem c.pre##c.pre pop edu,absorb(i.year country1##c.year ) vce( cluster country1)
estimates store r2, title("Country")
//Kalkuhl Database
reghdfe gdp_k c.tem##c.tem c.pre##c.pre pop edu,absorb(i.year country1##c.year ) vce( cluster country1)
estimates store r3, title("Country")
//Kummu Database-GDP-subset
preserve
drop if missing(gdp_k)
reghdfe dgdp c.tem##c.tem c.pre##c.pre pop edu,absorb(i.year country1##c.year ) vce( cluster country1)
estimates store r4, title("Country")
restore
//Kummu Database-GDI
reghdfe dgdi c.tem##c.tem c.pre##c.pre pop edu,absorb(i.year country1##c.year ) vce( cluster country1)
estimates store r5, title("Country")

esttab r1 r2 r3 r4 r5 using TableS15.rtf, star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) ar2 aic(%10.4f) bic(%10.4f)