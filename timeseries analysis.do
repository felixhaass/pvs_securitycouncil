* cd "D:\Felix\Uni\Aufsätze für Fachzeitschriften\Verantwortung in UN Resolutionen - PVS\Time Series Analysis\dataset"
use ./Data/nettoanstieg.dta, clear

*set up time series analysis
tsset year, yearly


**Generate Interventions
*Agenda for Peace at 1992
gen afppulse=0
replace afppulse=1 if year==1992
gen afpabrupt=0
replace afpabrupt=1 if year>1992
gen undummy =1
btscs afppulse year undummy, g(timesinceafp)
replace timesinceafp=. if afpabrupt==0
replace timesinceafp= timesinceafp+1
replace timesinceafp=0 if timesinceafp==.
gen afpgradual=timesinceafp
replace afpgradual=10 if timesinceafp>9
replace afpgradual=afpgradual/10

*R2P at 2005
gen r2ppulse=0
replace r2ppulse=1 if year==2005
gen r2pabrupt=0
replace r2pabrupt=1 if year>2005
btscs r2ppulse year undummy, g(timesincer2p)
replace timesincer2p=. if r2pabrupt==0
replace timesincer2p= timesincer2p+1
replace timesincer2p=0 if timesincer2p==.
gen r2pgradual=timesincer2p
replace r2pgradual=10 if timesincer2p>9
replace r2pgradual=r2pgradual/10

*gen easier to interpret importance score
gen term_relevance_res2 = term_relevance_res*100

*Intervention Analysis Agenda for Peace
ssc install itsa
ssc install actest
*testing for autocorrelation
actest term_relevance_res2 if year<1992, lags(10)
ac term_relevance_res2 if year<1992
pac term_relevance_res2 if year<1992
itsa term_relevance_res2, single trperiod(1992) lag(1) posttrend fig 
actest, lags(10)
drop _*
*robustnesstest with second order autocorrelation
itsa term_relevance_res2, single trperiod(1992) lag(2) posttrend fig 
drop _*

*Interention Analysis R2P
*testing for autocorrelation
actest term_relevance_res2 if year<2005, lags(10)
ac term_relevance_res2 if year<2005
pac term_relevance_res2 if year<2005
itsa term_relevance_res2, single trperiod(2005) lag(2) posttrend fig 
actest, lags(10)
drop _*

*Intervention Analysis Agenda for Peace & R2P
itsa term_relevance_res2, single trperiod(1992 2005) lag(1) posttrend fig 
actest, lags(10)
drop _*
*Robustnesstests with higher order autocorrelation
itsa term_relevance_res2, single trperiod(1992 2005) lag(2) posttrend fig 
actest, lags(10)
drop _*
itsa term_relevance_res2, single trperiod(1992 2005) lag(3) posttrend fig 
actest, lags(10)
drop _*

*Generate Table 2
itsa term_relevance_res2, single trperiod(1992 2005) lag(1) posttrend fig 
eststo
drop _*

*Generate data for plotting Figure 4 in R
itsa term_relevance_res2, single trperiod(1992 2005) lag(1) posttrend fig 
save "nettoanstieg fitted values.dta"


***Appendix
*Placebotests
*Agenda for Peace
itsa term_relevance_res2, single trperiod(1991) lag(1) posttrend 
eststo
drop _*
itsa term_relevance_res2, single trperiod(1990) lag(1) posttrend 
eststo
drop _*
*R2P
itsa term_relevance_res2, single trperiod(2004) lag(1) posttrend 
eststo
drop _*
itsa term_relevance_res2, single trperiod(2003) lag(1) posttrend 
eststo
drop _*
esttab, se b(%10.3f) label mtitles star(* 0.10 ** 0.05)
est clear

*Analysis with differenced dependent variable
sort year
gen dterm_relevance_res2 = D.term_relevance_res2
*Agenda for Peace
actest dterm_relevance_res2 if year<1992, lags(10)
ac dterm_relevance_res2 if year<1992
pac dterm_relevance_res2 if year<1992
itsa dterm_relevance_res2, single trperiod(1992) lag(0) posttrend fig 
actest, lags(10)
drop _*
itsa dterm_relevance_res2, single trperiod(1992) lag(1) posttrend fig 
drop _*
*R2P
actest dterm_relevance_res2 if year<2005, lags(10)
ac dterm_relevance_res2 if year<2005
pac dterm_relevance_res2 if year<2005
itsa dterm_relevance_res2, single trperiod(2005) lag(1) posttrend fig 
actest, lags(10)
drop _*
*multiple interventions
itsa dterm_relevance_res2, single trperiod(1992 2005) lag(1) posttrend fig 






itsa term_relevance_res2, single trperiod(2005) lag(1) posttrend fig 
actest, lags(10)
drop _*
itsa term_relevance_res2, single trperiod(1992 2005) lag(3) posttrend fig 
actest, lags(10)
drop _*


sort year
gen dterm_relevance_res2 = D.term_relevance_res2
itsa dterm_relevance_res2, single trperiod(1992) lag(1) posttrend fig 
actest, lags(6)
drop _*



*Format & generate date variables
replace date= date(date_string, "YMD") if date==.
format %td date
gen yq = yq(year, quarter) 
format yq %tq
save resonew.dta, replace

*collapse dateset, i.e. frequencies and means per year
use resonew, clear
collapse (count) myresanalysis if myresanalysis !=0 , by(year)
rename myresanalysis countrwrpy

save resoyearswithresp.dta, replace
use resonew, clear
gen reslength2 = reslength
gen myresanalysis2 = myresanalysis
collapse (mean) myresanalysis (sum) myresanalysis2 (mean) reslength (sum) reslength2 (count) resolution, by(year)
rename reslength2 sumreslength
rename reslength meanreslength
rename myresanalysis2 sumrespo
rename myresanalysis meanrespo
rename resolution resocount

*set up time series analysis
tsset year, yearly
tsfill
tsset year, yearly
save resonew.dta, replace

***count of resos with respo per year
merge 1:1 year using "resoyearswithresp.dta"
drop _m
replace countrwrpy= 0 if countrwrpy==.
gen proprwrpy = countrwrpy/resocount
save resonew.dta, replace

**gen respo by resolength
gen respoperlength2 = meanrespo /meanreslength
gen respoperlength = sumrespo /sumreslength
replace respoperlength=0 if respoperlength==.

*Figures
*Abbildung 1a: Number of Resolutions per year
twoway (tsline resocount), tlabel(#8)
*Mean resocount before and after the cold war
sum resocount if year<1990
sum resocount if year>1989
*Abbildung 1b: Average length of resolutions per year
twoway (tsline meanreslength), tlabel(#8)
*Abbildung 2a: Häufigkeit von Verantwortungszuschreibungen
twoway (tsline sumrespo), tlabel(#8)
*Abbildung 2b: Durchschnittliche Anzahl von Verantwortung pro Resolution
twoway (tsline meanrespo), tlabel(#8)
*Abbildung 2c: Anteil der resos die mindestens einmal verantwortung zuschreiben
twoway (tsline proprwrpy), tlabel(#8)
*Abbildung 2d: Durchschnittliche Anzahl von Verantwortung pro Wort
twoway (tsline respoperlength), tlabel(#8)
sum respoperlength if year<1990
sum respoperlength if year>1989

gen respoperlength3 = log(respoperlength+1)
replace respoperlength3 = 0 if  respoperlength3== .


gen new2 = respoperlength*resocount


**Generate Interventions
*Agenda for Peace at 1992
gen afppulse=0
replace afppulse=1 if year==1992
gen afpabrupt=0
replace afpabrupt=1 if year>1992
gen undummy =1
btscs afppulse year undummy, g(timesinceafp)
replace timesinceafp=. if afpabrupt==0
replace timesinceafp= timesinceafp+1
replace timesinceafp=0 if timesinceafp==.
gen afpgradual=timesinceafp
replace afpgradual=10 if timesinceafp>9
replace afpgradual=afpgradual/10

*R2P at 2005
gen r2ppulse=0
replace r2ppulse=1 if year==2005
gen r2pabrupt=0
replace r2pabrupt=1 if year>2005
btscs r2ppulse year undummy, g(timesincer2p)
replace timesincer2p=. if r2pabrupt==0
replace timesincer2p= timesincer2p+1
replace timesincer2p=0 if timesincer2p==.
gen r2pgradual=timesincer2p
replace r2pgradual=10 if timesincer2p>9
replace r2pgradual=r2pgradual/10
save resonew.dta, replace

***Intervention Analysis
**Interrupted Time Series Analysis
*Durchschnittliche Anzahl von Verantwortung pro Resolution
*Agenda for Peace
ssc install itsa
ssc install actest
itsa meanrespo, single trperiod(1992) lag(1) posttrend fig 
actest, lags(6)
drop _*
itsa meanrespo, single trperiod(1992) lag(2) posttrend fig 
actest, lags(6)
drop _*
*Placebotests
itsa meanrespo, single trperiod(1991) lag(2) posttrend fig 
actest, lags(6)
drop _*
itsa meanrespo, single trperiod(1990) lag(2) posttrend fig 
actest, lags(6)
drop _*
itsa meanrespo, single trperiod(1989) lag(2) posttrend fig 
actest, lags(6)
drop _*
*differencing = Veränderung in der Durchschnittliche Anzahl von Verantwortung pro Resolution
gen dmeanrespo = D.meanrespo
itsa dmeanrespo, single trperiod(1992) lag(1) posttrend fig 
actest, lags(6)
drop _*
*Placebotests
itsa dmeanrespo, single trperiod(1991) lag(1) posttrend fig 
actest, lags(6)
drop _*
itsa dmeanrespo, single trperiod(1990) lag(1) posttrend fig 
actest, lags(6)
drop _*
itsa dmeanrespo, single trperiod(1989) lag(1) posttrend fig 
actest, lags(6)
drop _*

*Anteil der resos die mindestens einmal verantwortung zuschreiben
itsa proprwrpy, single trperiod(1992) lag(1) posttrend fig 
actest, lags(6)
drop _*
*Placebotests
itsa proprwrpy, single trperiod(1991) lag(1) posttrend fig 
actest, lags(6)
drop _*
itsa proprwrpy, single trperiod(1990) lag(1) posttrend fig 
actest, lags(6)
drop _*
itsa proprwrpy, single trperiod(1989) lag(1) posttrend fig 
actest, lags(6)
drop _*
*differencing = Veränderung im Anteil der resos die mindestens einmal verantwortung zuschreiben
gen dproprwrpy = D.proprwrpy
itsa dproprwrpy, single trperiod(1992) lag(1) posttrend fig 
actest, lags(6)
drop _*

*Durchschnittliche Anzahl von Verantwortung pro Wort
twoway (tsline respoperlength), tlabel(#8)
itsa respoperlength, single trperiod(1992) lag(1) posttrend fig 
actest, lags(6)
drop _*
*Placebotests
itsa respoperlength, single trperiod(1991) lag(1) posttrend fig 
actest, lags(6)
drop _*
itsa respoperlength, single trperiod(1990) lag(1) posttrend fig 
actest, lags(6)
drop _*
itsa respoperlength, single trperiod(1989) lag(1) posttrend fig 
actest, lags(6)
drop _*
*differencing = Veränderung in der Durchschnittliche Anzahl von Verantwortung pro Wort
gen drespoperlength = D.respoperlength
itsa drespoperlength, single trperiod(1992) lag(1) posttrend fig 
actest, lags(6)
drop _*





***Code Reste
*ARMA models

*Autocorrelation und Partial- Autocorrelation Grafiken
*choosing q (moving average order)
*if correlation is substantial and outside the confidence intervall, use respective lag
ac meanrespo
ac dmeanrespo
ac proprwrpy
ac dproprwrpy
ac respoperlength
ac drespoperlength


*choosing p (autoregressive order)
*if correlation is substantial and outside the confidence intervall, use respective lag
pac meanrespo
pac dmeanrespo
pac proprwrpy
pac dproprwrpy
pac respoperlength
pac drespoperlength


itsa meanrespo, single trperiod(2005) lag(1) posttrend fig 
actest, lags(6)
drop _*
itsa meanrespo, single trperiod(2005) lag(2) posttrend fig 
actest, lags(6)
drop _*
itsa dmeanrespo, single trperiod(2005) lag(1) posttrend fig 
actest, lags(6)
drop _*
itsa meanrespo, single trperiod(1992 2005) lag(2) posttrend fig 
actest, lags(6)
drop _*






****Abbildungen zu Konjunktur und Wandel von Verantwortung
*Anzahl der Resolutionen
twoway (tsline resocount), tlabel(#8)
*Durchschnittliche Länge von Resolutionen
twoway (tsline meanreslength), tlabel(#8)
*Häufigkeit von Verantwortung in Resolutionen
twoway (tsline sumrespo), tlabel(#8)
*Durchschnittliche Häufigkeit von Verantwortung in Resolutionen
twoway (tsline meanrespo), tlabel(#8)
sort year
twoway (tsline D.meanrespo), tlabel(#8)

*Verantwortung/durchschnittliche Dokumentlänge
twoway (tsline respoperlength), tlabel(#8)

*Autocorrelation und Partial- Autocorrelation Grafiken
*choosing q (moving average order)
*if correlation is substantial and outside the confidence intervall, use respective lag
ac meanrespo
ac D.meanrespo
ac respoperlength

*choosing p (autoregressive order)
*if correlation is substantial and outside the confidence intervall, use respective lag
pac meanrespo
pac D.meanrespo
pac respoperlength

***ITSA
itsa meanrespo, single trperiod(1992) lag(1) posttrend fig 
actest, lags(6)
drop _*
itsa meanrespo, single trperiod(1992) lag(2) posttrend fig 
actest, lags(6)
drop _*
gen dmeanrespo = D.meanrespo
itsa dmeanrespo, single trperiod(1992) lag(1) posttrend fig 
actest, lags(6)
drop _*
itsa meanrespo, single trperiod(2005) lag(1) posttrend fig 
actest, lags(6)
drop _*
itsa meanrespo, single trperiod(2005) lag(2) posttrend fig 
actest, lags(6)
drop _*
itsa dmeanrespo, single trperiod(2005) lag(1) posttrend fig 
actest, lags(6)
drop _*
itsa meanrespo, single trperiod(1992 2005) lag(2) posttrend fig 
actest, lags(6)
drop _*


****Replicate with Arima Intervention Models
gen t=_n
gen tafpabrupt = t*afpabrupt
arima meanrespo afpabrupt t tafpabrupt, arima(1,0,3)
arima meanrespo afpabrupt t tafpabrupt, arima(1,0,1)
arima dmeanrespo afpabrupt t tafpabrupt, arima(1,0,3)
arima dmeanrespo afpabrupt t tafpabrupt, arima(1,0,1)
