******************************************************************************
******************************************************************************
*Meta-analysis MORPEP Energy Prices
******************************************************************************
******************************************************************************
clear all
*global setdir "C:\Nextcloud\project-vwl4makro\09_Forschung\Drittmittel\DZ2022\WP2_Meta_energy\estimation"
global setdir "C:\software\nextcloud\project-vwl4makro\09_Forschung\Drittmittel\DZ2022\WP2_Meta_energy\estimation"
global setdiroutput "C:\software\nextcloud\project-vwl4makro\09_Forschung\Drittmittel\DZ2022\WP2_Meta_energy\estimation\output"
cd "$setdir"
set more off
use metaenergy_final

cd "$setdiroutput"

 *run first time to install all required packages
/* 
net install winsor2 
ssc install winsor2
ssc install ranktest
ssc install ivreg2
ssc install estout
ssc install outreg2
ssc install boottest
ssc install ivvif, replace
ssc install matmap 
*/


*****************
*winsorising of outliers
*****************

*our baseline is a 2-98 percentile winsorization 
winsor2 coeffc sec nobs, replace cuts(2 98) 

*recalculate other metrics after winsorisation
replace prec= 1/sec
replace tstatc = coeffc/sec
separate prec, by(tstatc<-1.96) veryshortlabel 
replace sec2 = sec^2
replace prec2 = prec^2
replace invobs = 1/nobs


*create logarithm of number of citations per study
gen logcit = log(1+ ncitat)
label var logcit "log citations"


******************************
*generate median elasticity per study
bysort paperid: egen idmax = max(pobsid)
gen last=0
replace last = 1 if idmax==pobsid
bysort paperid: egen coeffmed = median(coeffc)
bysort paperid: egen precmed = median(prec)
bysort paperid: egen tmed = median(tstatc)
bysort paperid: egen perstudy = count(coeffc)
gen invperstudy = 1/perstudy
egen coeffmedfull = median(coeffc)
gen cmeddiff = abs(coeffc-coeffmed)
bysort paperid: egen cmeddiffmin = min(cmeddiff)
gen cmeddum = 1 if cmeddiff==cmeddiffmin
egen papergrp = group(paperid)
bysort paperid: egen cmedfirst = min(pobsid) if cmeddum == 1
gen cmeddumunique = cmeddum if cmedfirst == pobsid

******************************************************************************
*SUMMARY STATISTICS
******************************************************************************

*Table 1 - main variables
local xcatm "i.energysourcec i.energyusec i.OECDc i.sectorc"
local xdumm "pricechanged"
local xcontm "coeffc sec"

qui: table (var) (horizond), statistic(mean `xcontm' `xdumm') ///
statistic(sd `xcontm') ///                     
statistic(fvpercent `xcatm') ///  
statistic(fvfrequency `xcatm') ///  
nototal //nformat(%6.2f mean sd) nformat(%6.1f fvpercent) sformat("%s%%" fvpercent)
*collect levelsof result
qui: collect recode result fvpercent   = mean-share ///
                      fvfrequency = SD-obs ///
                      mean        = mean-share ///
                      sd          = SD-obs 

qui: collect layout (var) (horizond#result[mean-share SD-obs])
collect style row stack, nobinder
collect style cell border_block, border(right, pattern(nil)) 
collect style cell var[`xcontm']#result[mean-share], nformat(%9.2f)
collect style cell var[`xcontm']#result[SD-obs], nformat(%9.2f) sformat("(%s)")
collect style cell var[`xdumm']#result[mean-share], nformat(%5.4f) sformat("%s%%")
collect style cell var[`xcatm']#result[mean-share], nformat(%9.2f) sformat("%s%%")
collect style cell var[`xcatm']#result[SD-obs], nformat(%9.0fc)
collect label dim horizond "Time Horizon", modify
collect preview
collect export "descstatm.tex", tableonly  replace 


*Table A1 - auxiliary controls
local xcata "i.datadimc i.preferc"
local xduma "macrod datafreqd estdynd estsysd identifd inccontrd xpricecontd topjourd byproduct qconcernd"
local xconta "nobs avyearcd logcit"
qui: table (var) (horizond), statistic(mean `xconta' `xduma') ///
statistic(sd `xconta') ///                     
statistic(min `xconta') ///                     
statistic(max `xconta') ///                     
statistic(fvpercent `xcata') ///  
statistic(fvfrequency `xcata') ///  
nototal //nformat(%6.2f mean sd) nformat(%6.1f fvpercent) sformat("%s%%" fvpercent)
*collect levelsof result
qui: collect recode result fvpercent   = mean-share ///
                      fvfrequency = SD-obs ///
                      mean        = mean-share ///
                      sd          = SD-obs ///
					  min		  = min ///
					  max		  = max
					  
qui: collect layout (var) (horizond#result[mean-share SD-obs min max])
collect style row stack, nobinder
collect style cell border_block, border(right, pattern(nil)) 
collect style cell var[`xconta']#result[mean-share], nformat(%9.2f)
collect style cell var[`xconta']#result[SD-obs], nformat(%9.2f) sformat("(%s)")
collect style cell var[`xconta']#result[min], nformat(%9.2f) sformat("(%s)")
collect style cell var[`xconta']#result[max], nformat(%9.2f) sformat("(%s)")
collect style cell var[`xduma']#result[mean-share], nformat(%5.4f) sformat("%s%%")
collect style cell var[`xcata']#result[mean-share], nformat(%9.2f) sformat("%s%%")
collect style cell var[`xcata']#result[SD-obs], nformat(%9.0fc)
collect label dim horizond "Time Horizon", modify
collect preview
collect export "descstata.tex", tableonly  replace 

*/


*****************************************************************************************
*PLOTS - HISTOGRAMS
****************************************************************************************
*Figure A2
*histo SR LR
twoway (hist coeffc if horizond==0, freq bin(30) gap(10) lcolor(navy) fcolor(navy) legend(label(1 "Short run"))) (hist coeffc if horizond==1, freq bin(30)  gap(40) fcolor(cranberry) lcolor(cranberry) legend(label(2 "Long run")))
graph export "histSRLR.pdf", replace

*histo Energysourcemain
twoway (hist coeffc if energysourcem==1, freq bin(30) gap(10) lcolor(navy) fcolor(navy) legend(label(1 "Electricity"))) (hist coeffc if energysourcem==2, freq bin(30)  gap(40) fcolor(cranberry) lcolor(cranberry) legend(label(2 "Natural Gas")))   (hist coeffc if energysourcem==3, freq bin(30) gap(60) fcolor(gs12) lcolor(gs12) legend(label(3 "Other Mixed"))) 
graph export "histEsourcemain.pdf", replace

*histo Energysourcerare
twoway  (hist coeffc if energysourcec==2, freq bin(30) gap(10) fcolor(brown) lcolor(brown) legend(label(1 "Coal"))) (hist coeffc if energysourcec==5, freq bin(30) gap(40) fcolor(khaki) lcolor(khaki) legend(label(2 "Heat Oil")))   (hist coeffc if energysourcec==4, freq bin(30) gap(60) fcolor(midblue) lcolor(midblue) legend(label(3 "LPG")))   (hist coeffc if energysourcec==6, freq bin(30) gap(80) fcolor(gs12) lcolor(gs12) legend(label(4 "Mixed"))) 
graph export "histEsourcerare.pdf", replace

*histo Energyusec
twoway (hist coeffc if energyusec==1, freq bin(30) gap(10) lcolor(navy) fcolor(navy) legend(label(1 "Heating"))) (hist coeffc if energyusec==2, freq bin(30)  gap(40) fcolor(cranberry) lcolor(cranberry) legend(label(2 "Cooling")))   (hist coeffc if energyusec==3, freq bin(30) gap(60) fcolor(gs12) lcolor(gs12) legend(label(3 "Mixed"))) 
graph export "histEuse.pdf", replace

*histo sectorc
twoway (hist coeffc if sectorc==1, freq bin(30) gap(10) lcolor(navy) fcolor(navy) legend(label(1 "Residential"))) (hist coeffc if sectorc==2, freq bin(30)  gap(40) fcolor(cranberry) lcolor(cranberry) legend(label(2 "Business"))) (hist coeffc if sectorc==3, freq bin(30) gap(60) fcolor(gs12) lcolor(gs12) legend(label(3 "Mixed"))) 
graph export "histsector.pdf", replace

*histo OECD non mixed
twoway (hist coeffc if OECDc==1, freq bin(30) gap(10) lcolor(navy) fcolor(navy) legend(label(1 "OECD"))) (hist coeffc if OECDc==2, freq bin(30) gap(40) fcolor(cranberry) legend(label(2 "nonOECD"))) (hist coeffc if OECDc==3, freq bin(30) gap(60) fcolor(gs12) lcolor(gs12) legend(label(3 "Mixed")))
graph export "histOECD.pdf", replace


*/



*****************************************************************************************
*FUNNEL PLOTS - PUBLICATION BIAS
*****************************************************************************************
*Figure 3

* Short run 
su coeffc if horizond==0
local arm = r(mean)
scatter prec coeffc if horizond==0 & abs(tstatc)>=1.96, msize(*.5) msymbol(Oh) mcolor(navy) legend(pos(11) ring(0) col(1) label(1 "sig5%")) || scatter prec coeffc if horizond==0 & abs(tstatc)<1.96, msize(*.5) msymbol(O) mcolor(gs12) legend(label(2 "insig5%")) xtitle(Elasticity short run) ytitle(1/SE) xline(`arm')
graph export "funnel_SR.pdf", as(pdf) replace

* Long run 
su coeffc if horizond==1
local arm = r(mean)
scatter prec coeffc if horizond==1 & abs(tstatc)>=1.96, msize(*.5) msymbol(Oh) mcolor(navy) legend(pos(11) ring(0) col(1) label(1 "sig5%")) || scatter prec coeffc if horizond==1 & abs(tstatc)<1.96, msize(*.5) msymbol(O) mcolor(gs12) legend(label(2 "insig5%")) xtitle(Elasticity long run) ytitle(1/SE) xline(`arm')
graph export "funnel_LR.pdf", as(pdf) replace



*Figure B.6
*Funnels with highlighting for inferior and qualconcerns
* Short run 
su coeffc if horizond==0
local arm = r(mean)
scatter prec coeffc if horizond==0 & abs(tstatc)>=1.96 & preferc!=0 & qconcernd != 1, msize(*.5) msymbol(Oh) mcolor(navy) legend(pos(11) ring(0) col(1) label(1 "sig5%")) || scatter prec coeffc if horizond==0 & abs(tstatc)<1.96 & preferc!=0 & qconcernd != 1, msize(*.5) msymbol(O) mcolor(gs12) legend(label(2 "insig5%")) || scatter prec coeffc if horizond==0 & preferc==0, msize(*.5) msymbol(X) mcolor(red) legend(label(3 "inferior")) || scatter prec coeffc if horizond==0 & qconcernd!=0, msize(*.5) msymbol(+) mcolor(green) legend(label(4 "quality concern")) xtitle(Elasticity shortrun) ytitle(1/SE) xline(`arm')
graph export "funnel_SR_qual.pdf", as(pdf) replace

* Long run 
su coeffc if horizond==1
local arm = r(mean)
scatter prec coeffc if horizond==1 & abs(tstatc)>=1.96 & preferc!=0 & qconcernd != 1, msize(*.5) msymbol(Oh) mcolor(navy) legend(pos(11) ring(0) col(1) label(1 "sig5%")) || scatter prec coeffc if horizond==1 & abs(tstatc)<1.96 & preferc!=0 & qconcernd != 1, msize(*.5) msymbol(O) mcolor(gs12) legend(label(2 "insig5%")) || scatter prec coeffc if horizond==1 & preferc==0, msize(*.5) msymbol(X) mcolor(red) legend(label(3 "inferior")) || scatter prec coeffc if horizond==1 & qconcernd!=0, msize(*.5) msymbol(+) mcolor(green) legend(label(4 "quality concern")) xtitle(Elasticity longrun) ytitle(1/SE) xline(`arm')
graph export "funnel_LR_qual.pdf", as(pdf) replace

*/

******************************************************************************
*FUNNEL ASYMMETRY TESTS - Publication bias
******************************************************************************
*Table 2: linear tests
eststo clear
xtset paperid pobsid

*********** FAT short-run 
qui: eststo UA: ivreg2 coeffc if horizond==0, cluster (paperid) 
qui: eststo UWLS: ivreg2 tstatc prec if horizond==0, noconstant cluster (paperid)
qui: eststo OLS: ivreg2 coeffc sec if horizond==0, cluster (paperid)
qui: eststo FE: xtreg coeffc sec if horizond==0, fe cluster (paperid)
qui: eststo PET: ivreg2 coeffc sec [aweight=prec2] if horizond==0, cluster (paperid)
qui: eststo PEESE: ivreg2 coeffc sec2 [aweight=prec2] if horizond==0, cluster (paperid)
qui: eststo Median: ivreg2 coeffc sec [aweight=prec2] if horizond==0 & cmeddumunique==1

esttab using "pblin_SR.html", se ar2 replace mtitles label title(Linear publication bias tests, SR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab using "pblin_SR.tex", se ar2 booktabs replace compress title (Linear publication bias tests, SR \label{tab:pblinSR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
eststo clear

************ FAT long-run 
qui: eststo UA: ivreg2 coeffc if horizond==1, cluster (paperid) 
qui: eststo UWLS: ivreg2 tstatc prec if horizond==1, noconstant cluster (paperid)
qui: eststo OLS: ivreg2 coeffc sec if horizond==1, cluster (paperid)
qui: eststo FE: xtreg coeffc sec if horizond==1, fe cluster (paperid)
qui: eststo PET: ivreg2 coeffc sec [aweight=prec2] if horizond==1, cluster (paperid)
qui: eststo PEESE: ivreg2 coeffc sec2 [aweight=prec2] if horizond==1, cluster (paperid)
qui: eststo Median: ivreg2 coeffc sec [aweight=prec2] if horizond==1 & cmeddumunique==1

esttab using "pblin_LR.html", se ar2 replace mtitles label title(Linear publication bias tests, LR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab using "pblin_LR.tex", se ar2 booktabs replace compress title (Linear publication bias tests, LR \label{tab:pblinLR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
eststo clear

*/

* Figure 4: Uncorrected and corrected elasticities for subsamples, short run and long run 
* regression output
*********** FAT short-run subsamples
qui: eststo UA_elec: ivreg2 coeffc if horizond==0 & energysourcec==1, cluster (paperid) 
qui: eststo PEESE_elec: ivreg2 coeffc sec2 [aweight=prec2] if horizond==0 & energysourcec==1, cluster (paperid)
qui: eststo UA_ngas: ivreg2 coeffc if horizond==0 & energysourcec==3, cluster (paperid) 
qui: eststo PEESE_ngas: ivreg2 coeffc sec2 [aweight=prec2] if horizond==0 & energysourcec==3, cluster (paperid)

qui: eststo UA_OECD: ivreg2 coeffc if horizond==0 & OECDc==1, cluster (paperid) 
qui: eststo PEESE_OECD: ivreg2 coeffc sec2 [aweight=prec2] if horizond==0 & OECDc==1, cluster (paperid)
qui: eststo UA_nonOECD: ivreg2 coeffc if horizond==0 & OECDc==2, cluster (paperid) 
qui: eststo PEESE_nonOECD: ivreg2 coeffc sec2 [aweight=prec2] if horizond==0 & OECDc==2, cluster (paperid)

qui: eststo UA_res: ivreg2 coeffc if horizond==0 & sectorc==1, cluster (paperid) 
qui: eststo PEESE_res: ivreg2 coeffc sec2 [aweight=prec2] if horizond==0 & sectorc==1, cluster (paperid)
qui: eststo UA_bus: ivreg2 coeffc if horizond==0 & sectorc==2, cluster (paperid) 
qui: eststo PEESE_bus: ivreg2 coeffc sec2 [aweight=prec2] if horizond==0 & sectorc==2, cluster (paperid)

qui: eststo UA_jtop: ivreg2 coeffc if horizond==0 & topjourd==1, cluster (paperid) 
qui: eststo PEESE_jtop: ivreg2 coeffc sec2 [aweight=prec2] if horizond==0 & topjourd==1, cluster (paperid)
qui: eststo UA_jran: ivreg2 coeffc if horizond==0 & topjourd==0, cluster (paperid) 
qui: eststo PEESE_jran: ivreg2 coeffc sec2 [aweight=prec2] if horizond==0 & topjourd==0, cluster (paperid)

esttab using "pbsub_SR.html", se ar2 replace mtitles label title(Uncorrected and corrected elasticities for subsamples, SR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab using "pbsub_SR.tex", se ar2 booktabs replace compress title (Uncorrected and corrected elasticities for subsamples, SR \label{tab:pbsubSR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
eststo clear


*********** FAT long-run subsamples
qui: eststo UA_elec: ivreg2 coeffc if horizond==1 & energysourcec==1, cluster (paperid) 
qui: eststo PEESE_elec: ivreg2 coeffc sec2 [aweight=prec2] if horizond==1 & energysourcec==1, cluster (paperid)
qui: eststo UA_ngas: ivreg2 coeffc if horizond==1 & energysourcec==3, cluster (paperid) 
qui: eststo PEESE_ngas: ivreg2 coeffc sec2 [aweight=prec2] if horizond==1 & energysourcec==3, cluster (paperid)

qui: eststo UA_OECD: ivreg2 coeffc if horizond==1 & OECDc==1, cluster (paperid) 
qui: eststo PEESE_OECD: ivreg2 coeffc sec2 [aweight=prec2] if horizond==1 & OECDc==1, cluster (paperid)
qui: eststo UA_nonOECD: ivreg2 coeffc if horizond==1 & OECDc==2, cluster (paperid) 
qui: eststo PEESE_nonOECD: ivreg2 coeffc sec2 [aweight=prec2] if horizond==1 & OECDc==2, cluster (paperid)

qui: eststo UA_res: ivreg2 coeffc if horizond==1 & sectorc==1, cluster (paperid) 
qui: eststo PEESE_res: ivreg2 coeffc sec2 [aweight=prec2] if horizond==1 & sectorc==1, cluster (paperid)
qui: eststo UA_bus: ivreg2 coeffc if horizond==1 & sectorc==2, cluster (paperid) 
qui: eststo PEESE_bus: ivreg2 coeffc sec2 [aweight=prec2] if horizond==1 & sectorc==2, cluster (paperid)

qui: eststo UA_jtop: ivreg2 coeffc if horizond==1 & topjourd==1, cluster (paperid) 
qui: eststo PEESE_jtop: ivreg2 coeffc sec2 [aweight=prec2] if horizond==1 & topjourd==1, cluster (paperid)
qui: eststo UA_jran: ivreg2 coeffc if horizond==1 & topjourd==0, cluster (paperid) 
qui: eststo PEESE_jran: ivreg2 coeffc sec2 [aweight=prec2] if horizond==1 & topjourd==0, cluster (paperid)

esttab using "pbsub_LR.html", se ar2 replace mtitles label title(Uncorrected and corrected elasticities for subsamples, LR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab using "pbsub_LR.tex", se ar2 booktabs replace compress title (Uncorrected and corrected elasticities for subsamples, LR \label{tab:pbsubSR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
eststo clear

*/

*************************
*Table B.3: winsorisation robustness
*note: reload dataset to undo baseline 2-98 winsorization

preserve
cd "$setdir"
set more off
use metaenergy_final, clear
cd "$setdiroutput"
 
qui: eststo SR0100: ivreg2 coeffc sec2 [aweight=prec2] if horizond==0, cluster (paperid)
qui: eststo LR0100: ivreg2 coeffc sec2 [aweight=prec2] if horizond==1, cluster (paperid)

winsor2 coeffc sec nobs, replace cuts(1 99)
replace sec2 = sec^2
replace prec= 1/sec
replace prec2 = prec^2
qui: eststo SR199: ivreg2 coeffc sec2 [aweight=prec2] if horizond==0, cluster (paperid)
qui: eststo LR199: ivreg2 coeffc sec2 [aweight=prec2] if horizond==1, cluster (paperid)

winsor2 coeffc sec nobs, replace cuts(2 98)
replace sec2 = sec^2
replace prec= 1/sec
replace prec2 = prec^2
qui: eststo SR298: ivreg2 coeffc sec2 [aweight=prec2] if horizond==0, cluster (paperid)
qui: eststo LR298: ivreg2 coeffc sec2 [aweight=prec2] if horizond==1, cluster (paperid)

winsor2 coeffc sec nobs, replace cuts(3 97) 
replace sec2 = sec^2
replace prec= 1/sec
replace prec2 = prec^2
qui: eststo SR397: ivreg2 coeffc sec2 [aweight=prec2] if horizond==0, cluster (paperid)
qui: eststo LR397: ivreg2 coeffc sec2 [aweight=prec2] if horizond==1, cluster (paperid)

winsor2 coeffc sec nobs, replace cuts(4 96) 
replace sec2 = sec^2
replace prec= 1/sec
replace prec2 = prec^2
qui: eststo SR496: ivreg2 coeffc sec2 [aweight=prec2] if horizond==0, cluster (paperid)
qui: eststo LR496: ivreg2 coeffc sec2 [aweight=prec2] if horizond==1, cluster (paperid)

winsor2 coeffc sec nobs, replace cuts(5 95) 
replace sec2 = sec^2
replace prec= 1/sec
replace prec2 = prec^2
qui: eststo SR595: ivreg2 coeffc sec2 [aweight=prec2] if horizond==0, cluster (paperid)
qui: eststo LR595: ivreg2 coeffc sec2 [aweight=prec2] if horizond==1, cluster (paperid)

esttab SR* using "winsor_SR.html", se ar2 replace mtitles label title(PEESE test for different winsorization, SR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab SR* using "winsor_SR.tex", se ar2 booktabs replace compress title (PEESE test for different winsorization, SR \label{tab:winsorSR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
esttab LR* using "winsor_LR.html", se ar2 replace mtitles label title(PEESE test for different winsorization, LR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab LR* using "winsor_LR.tex", se ar2 booktabs replace compress title (PEESE test for different winsorization, LR \label{tab:winsorLR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
eststo clear

restore
*/


*Table B.4: linear tests without inferior estimates and quality concerns
eststo clear
preserve
drop if preferc==0 | qconcernd==1
*********** FAT short-run 
qui: eststo UA: ivreg2 coeffc if horizond==0, cluster (paperid) 
qui: eststo UWLS: ivreg2 tstatc prec if horizond==0, noconstant cluster (paperid)
qui: eststo OLS: ivreg2 coeffc sec if horizond==0, cluster (paperid)
qui: eststo FE: xtreg coeffc sec if horizond==0, fe cluster (paperid)
qui: eststo PET: ivreg2 coeffc sec [aweight=prec2] if horizond==0, cluster (paperid)
qui: eststo PEESE: ivreg2 coeffc sec2 [aweight=prec2] if horizond==0, cluster (paperid)
qui: eststo Median: ivreg2 coeffc sec [aweight=prec2] if horizond==0 & cmeddumunique==1

esttab using "pblinqual_SR.html", se ar2 replace mtitles label title(Linear publication bias tests without inferior estimates and quality concerns, SR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab using "pblinqual_SR.tex", se ar2 booktabs replace compress title (Linear publication bias tests without inferior estimates and quality concerns, SR \label{tab:pblinSR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
eststo clear

************ FAT long-run 
qui: eststo UA: ivreg2 coeffc if horizond==1, cluster (paperid) 
qui: eststo UWLS: ivreg2 tstatc prec if horizond==1, noconstant cluster (paperid)
qui: eststo OLS: ivreg2 coeffc sec if horizond==1, cluster (paperid)
qui: eststo FE: xtreg coeffc sec if horizond==1, fe cluster (paperid)
qui: eststo PET: ivreg2 coeffc sec [aweight=prec2] if horizond==1, cluster (paperid)
qui: eststo PEESE: ivreg2 coeffc sec2 [aweight=prec2] if horizond==1, cluster (paperid)
qui: eststo Median: ivreg2 coeffc sec [aweight=prec2] if horizond==1 & cmeddumunique==1

esttab using "pblinqual_LR.html", se ar2 replace mtitles label title(Linear publication bias tests without inferior estimates and quality concerns, LR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab using "pblinqual_LR.tex", se ar2 booktabs replace compress title (Linear publication bias tests without inferior estimates and quality concerns, LR \label{tab:pblinLR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
eststo clear
restore
*/


*********************************
*Table 3: Nonlinear Tests. 
*Note: Furukawa Stem method not included. See here https://github.com/Chishio318/stem-based_method
*Note: Uncomment "drop if preferc==0 | qconcernd==1" twice for Table B.5 Nonlinear underlying effects: without inferior estimates and quality concerns
*Andrews Kasy 2019 AER non-parametric pubbias exercise
preserve
*drop if preferc==0 | qconcernd==1 //uncomment for Table B.5 results
drop if coeffc==.
drop if sec==.
drop if se_orig==. & t_orig==. & (p_val_calc==0.001 | p_val_calc==0.01 | p_val_calc==0.05 | p_val_calc==0.1 | p_val_calc==0.5) //drop all artifically generated pvals according to stars or cutoffs, as they would distort the threshold estimates

*safe data for AK test in the web application. Then upload them at https://maxkasy.github.io/home/metastudy/
*or use the code with R https://github.com/maxkasy/MetaStudiesApp
export delimited coeffc sec using "dataAK_sr.csv" if horizond==0, novarnames replace
export delimited coeffc sec using "dataAK_lr.csv" if horizond==1, novarnames replace
restore

***********************
*WAAP Ioannidis et al. 2017, EJ 
preserve
*drop if preferc==0 | qconcernd==1 //uncomment for Table B.5 results
summarize coeffc [aweight=prec2]
qui: reg tstatc prec, noconstant
scalar waapse = abs(_b[prec])
qui: eststo WAAPsr: ivreg2 tstatc prec if sec<waapse/2.8 & horizond==0, noconstant cluster(paperid)
qui: eststo WAAPlr: ivreg2 tstatc prec if sec<waapse/2.8 & horizond==1, noconstant cluster(paperid)

***********************
*MAIVE* Irsova Z., Bom P. R. D., Havranek T., and H. Rachinger (2025): "Spurious Precision in Meta-Analysis of Observational Research." Nature Communications, forthcoming. 
qui: eststo MAIVEsr: ivreg2 coeffc (sec2 = invobs) [aweight=prec2] if horizond==0, cluster (paperid) first savefirst savefprefix(firstcwsr)
qui: eststo MAIVElr: ivreg2 coeffc (sec2 = invobs) [aweight=prec2] if horizond==1, cluster (paperid) first savefirst savefprefix(firstcwlr)

*use Top10, and 1st stage MAIVEcwsr as placeholder for AK test and Furukawa
esttab WAAPsr MAIVEsr using "pbnonlin_SR.html", se ar2 scalars(widstat) replace mtitles label title(Nonlinear Pubbias Tests, SR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab WAAPsr MAIVEsr using "pbnonlin_SR.tex", se ar2 scalars(widstat) booktabs replace compress title(Nonlinear Pubbias Tests, SR \label{tab:pbnonlin_SR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)

esttab WAAPlr MAIVElr using "pbnonlin_LR.html", se ar2 scalars(widstat) replace mtitles label title(Nonlinear Pubbias Tests, LR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab WAAPlr MAIVElr using "pbnonlin_LR.tex", se ar2 scalars(widstat) booktabs replace compress title(Nonlinear Pubbias Tests, LR \label{tab:pbnonlin_SR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)

eststo clear
restore

*/

***********************************
*Multivariate meta regression (MMR)
***********************************
eststo clear
*xtset paperid pobsid

*Table 4 and C.1: Multivariate meta regression, baseline and robustness checks with further controls

local xbase "ib0.horizond ib1.energysourcem ib3.energyusec ib1.pricechanged ib1.OECDc ib1.sectorc" 
local xpars "ib0.horizond ib1.energysourcem ib1.OECDc ib1.pricechanged"
local xdata "avyearcd macrod ib3.datadimc datafreqd" 
local xest "estdynd estsysd identifd inccontrd xpricecontd"  
local xpub "topjourd logcit byproduct ib1.preferc" 

qui: eststo base: ivreg2 coeffc sec2 `xbase' [aweight=prec2], cluster (paperid)
qui: eststo pars: ivreg2 coeffc sec2 `xpars' [aweight=prec2], cluster (paperid)
qui: eststo data: ivreg2 coeffc sec2 `xbase' `xdata' [aweight=prec2], cluster (paperid)
qui: eststo estim: ivreg2 coeffc sec2 `xbase' `xest' [aweight=prec2], cluster (paperid)
qui: eststo public: ivreg2 coeffc sec2 `xbase' `xpub' [aweight=prec2], cluster (paperid)
qui: eststo all: ivreg2 coeffc sec2 `xbase' `xdata' `xest' `xpub' [aweight=prec2], cluster (paperid)

esttab using "mmrcontrols.tex", se ar2 booktabs replace compress title(Multivariate meta regression, baseline and robustness checks with further controls \label{tab:mmrcontrols}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
esttab using "mmrcontrols.html", se ar2 replace  mtitles label title(ultivariate meta regression, baseline and robustness checks with further controls) addnote("Standard errors in parentheses and clustered at the study level.") nostar nobaselevels 
eststo clear

*********************************************************
*Table 5: Multivariate meta regression, group-wise samples

qui: eststo SR: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if horizond==0, cluster (paperid)
qui: eststo LR: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if horizond==1, cluster (paperid)
qui: eststo elect: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if energysourcec==1, cluster (paperid)
qui: eststo natgas: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if energysourcec==3, cluster (paperid) // produces warning due to low number of natgas + cooling observations. But results do not change much if cooling variable is dropped.
qui: eststo resi: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if sectorc==1, cluster (paperid) 
qui: eststo busi: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if sectorc==2, cluster (paperid)  // produces warning due to low number of business + energysource mix observations. But results do not change much if energysource variable is dropped.

esttab using "mmrgroup.tex", se ar2 booktabs replace compress title(Multivariate meta regression, group-wise samples \label{tab:mmrgroup}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
esttab using "mmrgroup.html", se ar2 replace  mtitles label title(Multivariate meta regression, group-wise samples) addnote("Standard errors in parentheses and clustered at the study level.") nostar nobaselevels
eststo clear


*********************************************************
***Table C.2: Multivariate meta regression, subsamples with quality criteria

qui: eststo qual: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if preferc!=0 & qconcernd!=1, cluster (paperid)
qui: eststo pref: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if preferc==2, cluster (paperid)
qui: eststo icpcontr: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if inccontrd==1 & xpricecontd==1, cluster (paperid)
qui: eststo ident: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if identifd==1, cluster (paperid)

esttab using "mmrsub.tex", se ar2 booktabs replace compress title(Multivariate meta regression, subsamples with quality criteria \label{tab:mmrsub}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
esttab using "mmrsub.html", se ar2 replace  mtitles label title(Multivariate meta regression, subsamples with quality criteria) addnote("Standard errors in parentheses and clustered at the study level.") nostar nobaselevels
eststo clear

*/



**************************
*Best practice predictions
**************************
* drop low quality estimates
drop if qconcernd == 1
drop if preferc == 0 

*derived from necessary baseline variables plus BMA
local xbase "ib1.energysourcem ib3.energyusec ib1.pricechanged ib1.OECDc ib1.sectorc" 
local xqual "topjourd logcit byproduct ib1.preferc identifd inccontrd xpricecontd"  
local xbma "avyearcd macrod ib3.datadimc datafreqd estdynd estsysd" 
local xbest "`xbase' `xqual' `xbma'" 
local xcat "energysourcem energyusec OECDc sectorc datadimc preferc pricechanged macrod datafreqd estdynd estsysd identifd inccontrd xpricecontd topjourd byproduct qconcernd"

foreach h in 0 1 {
     local hname = ""
     if `h' == 0 {
     local hname = "SR"
     }
     else {
     local hname = "LR"
     }
foreach x of local xcat {
qui: ta `x' if horizond == `h', matcell(mat_`x')
local nobs=r(N)
display `nobs'
local count = 0
display "`x'"
levelsof `x', local(levels) 
    foreach l of local levels {	
 	local count = `count' + 1
    local category = "`x'" + "`l'"
	display "`category'"
    local s`category'`hname' = mat_`x'[`count',1]/`nobs'
	display `count'
}
}
su logcit
local slogcit`hname' = r(max)
local spreferc2`hname' = 1
}
macro list

matrix bestprac = J(9,6,.)
matrix rownames bestprac = bestpractice electricity naturalgas heating cooling OECD nonOECD residential business 
matrix colnames bestprac = SRpe SRub SRlb LRpe LRub LRlb


*short run linear predictions
qui: eststo bestsr: ivreg2 coeffc sec2 `xbest' [aweight=prec2] if horizond==0, cluster (paperid)
qui: lincom _cons + 0*sec2 + `senergysourcem2SR'*2.energysourcem + `senergysourcem3SR'*3.energysourcem + `senergyusec1SR'*1.energyusec + `senergyusec2SR'*2.energyusec + `spricechanged1SR'*1.pricechanged + `sOECDc2SR'*2.OECDc + `sOECDc3SR'*3.OECDc + `ssectorc2SR'*2.sectorc + `ssectorc2SR'*3.sectorc + 0*avyearcd + `smacrod1SR'*macrod + `sdatafreqd1SR'*datafreqd + `sestdynd1SR'*estdynd + `sestsysd1SR'*estsysd + 1*identifd + 1*inccontrd + 1*xpricecontd + 1*topjourd + `slogcitSR'*logcit + 0*byproduct + `spreferc2SR'*2.preferc
matrix bestprac[1,1] = r(estimate)
matrix bestprac[1,2] = r(ub)
matrix bestprac[1,3] = r(lb)

**elec
qui: lincom _cons + 0*sec2 + 0*2.energysourcem + 0*3.energysourcem + `senergyusec1SR'*1.energyusec + `senergyusec2SR'*2.energyusec + `spricechanged1SR'*1.pricechanged + `sOECDc2SR'*2.OECDc + `sOECDc3SR'*3.OECDc + `ssectorc2SR'*2.sectorc + `ssectorc2SR'*3.sectorc + 0*avyearcd + `smacrod1SR'*macrod + `sdatafreqd1SR'*datafreqd + `sestdynd1SR'*estdynd + `sestsysd1SR'*estsysd + 1*identifd + 1*inccontrd + 1*xpricecontd + 1*topjourd + `slogcitSR'*logcit + 0*byproduct + `spreferc2SR'*2.preferc
matrix bestprac[2,1] = r(estimate)
matrix bestprac[2,2] = r(ub)
matrix bestprac[2,3] = r(lb)

**natgas
qui: lincom _cons + 0*sec2 + 1*2.energysourcem + 0*3.energysourcem + `senergyusec1SR'*1.energyusec + `senergyusec2SR'*2.energyusec + `spricechanged1SR'*1.pricechanged + `sOECDc2SR'*2.OECDc + `sOECDc3SR'*3.OECDc + `ssectorc2SR'*2.sectorc + `ssectorc2SR'*3.sectorc + 0*avyearcd + `smacrod1SR'*macrod + `sdatafreqd1SR'*datafreqd + `sestdynd1SR'*estdynd + `sestsysd1SR'*estsysd + 1*identifd + 1*inccontrd + 1*xpricecontd + 1*topjourd + `slogcitSR'*logcit + 0*byproduct + `spreferc2SR'*2.preferc
matrix bestprac[3,1] = r(estimate)
matrix bestprac[3,2] = r(ub)
matrix bestprac[3,3] = r(lb)

**heat
qui: lincom _cons + 0*sec2 + `senergysourcem2SR'*2.energysourcem + `senergysourcem3SR'*3.energysourcem + 1*1.energyusec + 0*2.energyusec + `spricechanged1SR'*1.pricechanged + `sOECDc2SR'*2.OECDc + `sOECDc3SR'*3.OECDc + `ssectorc2SR'*2.sectorc + `ssectorc2SR'*3.sectorc + 0*avyearcd + `smacrod1SR'*macrod + `sdatafreqd1SR'*datafreqd + `sestdynd1SR'*estdynd + `sestsysd1SR'*estsysd + 1*identifd + 1*inccontrd + 1*xpricecontd + 1*topjourd + `slogcitSR'*logcit + 0*byproduct + `spreferc2SR'*2.preferc
matrix bestprac[4,1] = r(estimate)
matrix bestprac[4,2] = r(ub)
matrix bestprac[4,3] = r(lb)

**cool
qui: lincom _cons + 0*sec2 + `senergysourcem2SR'*2.energysourcem + `senergysourcem3SR'*3.energysourcem + 0*1.energyusec + 1*2.energyusec + `spricechanged1SR'*1.pricechanged + `sOECDc2SR'*2.OECDc + `sOECDc3SR'*3.OECDc + `ssectorc2SR'*2.sectorc + `ssectorc2SR'*3.sectorc + 0*avyearcd + `smacrod1SR'*macrod + `sdatafreqd1SR'*datafreqd + `sestdynd1SR'*estdynd + `sestsysd1SR'*estsysd + 1*identifd + 1*inccontrd + 1*xpricecontd + 1*topjourd + `slogcitSR'*logcit + 0*byproduct + `spreferc2SR'*2.preferc
matrix bestprac[5,1] = r(estimate)
matrix bestprac[5,2] = r(ub)
matrix bestprac[5,3] = r(lb)

**OECD
qui: lincom _cons + 0*sec2 + `senergysourcem2SR'*2.energysourcem + `senergysourcem3SR'*3.energysourcem + `senergyusec1SR'*1.energyusec + `senergyusec2SR'*2.energyusec + `spricechanged1SR'*1.pricechanged + 0*2.OECDc + 0*3.OECDc + `ssectorc2SR'*2.sectorc + `ssectorc2SR'*3.sectorc + 0*avyearcd + `smacrod1SR'*macrod + `sdatafreqd1SR'*datafreqd + `sestdynd1SR'*estdynd + `sestsysd1SR'*estsysd + 1*identifd + 1*inccontrd + 1*xpricecontd + 1*topjourd + `slogcitSR'*logcit + 0*byproduct + `spreferc2SR'*2.preferc
matrix bestprac[6,1] = r(estimate)
matrix bestprac[6,2] = r(ub)
matrix bestprac[6,3] = r(lb)

**nonOECD
qui: lincom _cons + 0*sec2 + `senergysourcem2SR'*2.energysourcem + `senergysourcem3SR'*3.energysourcem + `senergyusec1SR'*1.energyusec + `senergyusec2SR'*2.energyusec + `spricechanged1SR'*1.pricechanged + 1*2.OECDc + 0*3.OECDc + `ssectorc2SR'*2.sectorc + `ssectorc2SR'*3.sectorc + 0*avyearcd + `smacrod1SR'*macrod + `sdatafreqd1SR'*datafreqd + `sestdynd1SR'*estdynd + `sestsysd1SR'*estsysd + 1*identifd + 1*inccontrd + 1*xpricecontd + 1*topjourd + `slogcitSR'*logcit + 0*byproduct + `spreferc2SR'*2.preferc
matrix bestprac[7,1] = r(estimate)
matrix bestprac[7,2] = r(ub)
matrix bestprac[7,3] = r(lb)

**residential
qui: lincom _cons + 0*sec2 + `senergysourcem2SR'*2.energysourcem + `senergysourcem3SR'*3.energysourcem + `senergyusec1SR'*1.energyusec + `senergyusec2SR'*2.energyusec + `spricechanged1SR'*1.pricechanged + `sOECDc2SR'*2.OECDc + `sOECDc3SR'*3.OECDc + 0*2.sectorc + 0*3.sectorc + 0*avyearcd + `smacrod1SR'*macrod + `sdatafreqd1SR'*datafreqd + `sestdynd1SR'*estdynd + `sestsysd1SR'*estsysd + 1*identifd + 1*inccontrd + 1*xpricecontd + 1*topjourd + `slogcitSR'*logcit + 0*byproduct + `spreferc2SR'*2.preferc
matrix bestprac[8,1] = r(estimate)
matrix bestprac[8,2] = r(ub)
matrix bestprac[8,3] = r(lb)

**business
qui: lincom _cons + 0*sec2 + `senergysourcem2SR'*2.energysourcem + `senergysourcem3SR'*3.energysourcem + `senergyusec1SR'*1.energyusec + `senergyusec2SR'*2.energyusec + `spricechanged1SR'*1.pricechanged + `sOECDc2SR'*2.OECDc + `sOECDc3SR'*3.OECDc + 1*2.sectorc + 0*3.sectorc + 0*avyearcd + `smacrod1SR'*macrod + `sdatafreqd1SR'*datafreqd + `sestdynd1SR'*estdynd + `sestsysd1SR'*estsysd + 1*identifd + 1*inccontrd + 1*xpricecontd + 1*topjourd + `slogcitSR'*logcit + 0*byproduct + `spreferc2SR'*2.preferc
matrix bestprac[9,1] = r(estimate)
matrix bestprac[9,2] = r(ub)
matrix bestprac[9,3] = r(lb)


*Long run linear predictions 
qui: eststo bestlr: ivreg2 coeffc sec2 `xbest' [aweight=prec2] if horizond==1, cluster (paperid) //`xbase' `xdata' `xest' `xpub' 
qui: lincom _cons + 0*sec2 + `senergysourcem2LR'*2.energysourcem + `senergysourcem3LR'*3.energysourcem + `senergyusec1LR'*1.energyusec + `senergyusec2LR'*2.energyusec + `spricechanged1LR'*1.pricechanged + `sOECDc2LR'*2.OECDc + `sOECDc3LR'*3.OECDc + `ssectorc2LR'*2.sectorc + `ssectorc2LR'*3.sectorc + 0*avyearcd + `smacrod1LR'*macrod + `sdatafreqd1LR'*datafreqd + `sestdynd1LR'*estdynd + `sestsysd1LR'*estsysd + 1*identifd + 1*inccontrd + 1*xpricecontd + 1*topjourd + `slogcitLR'*logcit + 0*byproduct + `spreferc2LR'*2.preferc
matrix bestprac[1,4] = r(estimate)
matrix bestprac[1,5] = r(ub)
matrix bestprac[1,6] = r(lb)

**elec
qui: lincom _cons + 0*sec2 + 0*2.energysourcem + 0*3.energysourcem + `senergyusec1LR'*1.energyusec + `senergyusec2LR'*2.energyusec + `spricechanged1LR'*1.pricechanged + `sOECDc2LR'*2.OECDc + `sOECDc3LR'*3.OECDc + `ssectorc2LR'*2.sectorc + `ssectorc2LR'*3.sectorc + 0*avyearcd + `smacrod1LR'*macrod + `sdatafreqd1LR'*datafreqd + `sestdynd1LR'*estdynd + `sestsysd1LR'*estsysd + 1*identifd + 1*inccontrd + 1*xpricecontd + 1*topjourd + `slogcitLR'*logcit + 0*byproduct + `spreferc2LR'*2.preferc
matrix bestprac[2,4] = r(estimate)
matrix bestprac[2,5] = r(ub)
matrix bestprac[2,6] = r(lb)


**natgas
qui: lincom _cons + 0*sec2 + 1*2.energysourcem + 0*3.energysourcem + `senergyusec1LR'*1.energyusec + `senergyusec2LR'*2.energyusec + `spricechanged1LR'*1.pricechanged + `sOECDc2LR'*2.OECDc + `sOECDc3LR'*3.OECDc + `ssectorc2LR'*2.sectorc + `ssectorc2LR'*3.sectorc + 0*avyearcd + `smacrod1LR'*macrod + `sdatafreqd1LR'*datafreqd + `sestdynd1LR'*estdynd + `sestsysd1LR'*estsysd + 1*identifd + 1*inccontrd + 1*xpricecontd + 1*topjourd + `slogcitLR'*logcit + 0*byproduct + `spreferc2LR'*2.preferc
matrix bestprac[3,4] = r(estimate)
matrix bestprac[3,5] = r(ub)
matrix bestprac[3,6] = r(lb)

**heat
qui: lincom _cons + 0*sec2 + `senergysourcem2LR'*2.energysourcem + `senergysourcem3LR'*3.energysourcem + 1*1.energyusec + 0*2.energyusec + `spricechanged1LR'*1.pricechanged + `sOECDc2LR'*2.OECDc + `sOECDc3LR'*3.OECDc + `ssectorc2LR'*2.sectorc + `ssectorc2LR'*3.sectorc + 0*avyearcd + `smacrod1LR'*macrod + `sdatafreqd1LR'*datafreqd + `sestdynd1LR'*estdynd + `sestsysd1LR'*estsysd + 1*identifd + 1*inccontrd + 1*xpricecontd + 1*topjourd + `slogcitLR'*logcit + 0*byproduct + `spreferc2LR'*2.preferc
matrix bestprac[4,4] = r(estimate)
matrix bestprac[4,5] = r(ub)
matrix bestprac[4,6] = r(lb)


**cool
qui: lincom _cons + 0*sec2 + `senergysourcem2LR'*2.energysourcem + `senergysourcem3LR'*3.energysourcem + 0*1.energyusec + 1*2.energyusec + `spricechanged1LR'*1.pricechanged + `sOECDc2LR'*2.OECDc + `sOECDc3LR'*3.OECDc + `ssectorc2LR'*2.sectorc + `ssectorc2LR'*3.sectorc + 0*avyearcd + `smacrod1LR'*macrod + `sdatafreqd1LR'*datafreqd + `sestdynd1LR'*estdynd + `sestsysd1LR'*estsysd + 1*identifd + 1*inccontrd + 1*xpricecontd + 1*topjourd + `slogcitLR'*logcit + 0*byproduct + `spreferc2LR'*2.preferc
matrix bestprac[5,4] = r(estimate)
matrix bestprac[5,5] = r(ub)
matrix bestprac[5,6] = r(lb)



**OECD
qui: lincom _cons + 0*sec2 + `senergysourcem2LR'*2.energysourcem + `senergysourcem3LR'*3.energysourcem + `senergyusec1LR'*1.energyusec + `senergyusec2LR'*2.energyusec + `spricechanged1LR'*1.pricechanged + 0*2.OECDc + 0*3.OECDc + `ssectorc2LR'*2.sectorc + `ssectorc2LR'*3.sectorc + 0*avyearcd + `smacrod1LR'*macrod + `sdatafreqd1LR'*datafreqd + `sestdynd1LR'*estdynd + `sestsysd1LR'*estsysd + 1*identifd + 1*inccontrd + 1*xpricecontd + 1*topjourd + `slogcitLR'*logcit + 0*byproduct + `spreferc2LR'*2.preferc
matrix bestprac[6,4] = r(estimate)
matrix bestprac[6,5] = r(ub)
matrix bestprac[6,6] = r(lb)

**nonOECD
qui: lincom _cons + 0*sec2 + `senergysourcem2LR'*2.energysourcem + `senergysourcem3LR'*3.energysourcem + `senergyusec1LR'*1.energyusec + `senergyusec2LR'*2.energyusec + `spricechanged1LR'*1.pricechanged + 1*2.OECDc + 0*3.OECDc + `ssectorc2LR'*2.sectorc + `ssectorc2LR'*3.sectorc + 0*avyearcd + `smacrod1LR'*macrod + `sdatafreqd1LR'*datafreqd + `sestdynd1LR'*estdynd + `sestsysd1LR'*estsysd + 1*identifd + 1*inccontrd + 1*xpricecontd + 1*topjourd + `slogcitLR'*logcit + 0*byproduct + `spreferc2LR'*2.preferc
matrix bestprac[7,4] = r(estimate)
matrix bestprac[7,5] = r(ub)
matrix bestprac[7,6] = r(lb)

**residential
qui: lincom _cons + 0*sec2 + `senergysourcem2LR'*2.energysourcem + `senergysourcem3LR'*3.energysourcem + `senergyusec1LR'*1.energyusec + `senergyusec2LR'*2.energyusec + `spricechanged1LR'*1.pricechanged + `sOECDc2LR'*2.OECDc + `sOECDc3LR'*3.OECDc + 0*2.sectorc + 0*3.sectorc + 0*avyearcd + `smacrod1LR'*macrod + `sdatafreqd1LR'*datafreqd + `sestdynd1LR'*estdynd + `sestsysd1LR'*estsysd + 1*identifd + 1*inccontrd + 1*xpricecontd + 1*topjourd + `slogcitLR'*logcit + 0*byproduct + `spreferc2LR'*2.preferc
matrix bestprac[8,4] = r(estimate)
matrix bestprac[8,5] = r(ub)
matrix bestprac[8,6] = r(lb)

**business 
qui: lincom _cons + 0*sec2 + `senergysourcem2LR'*2.energysourcem + `senergysourcem3LR'*3.energysourcem + `senergyusec1LR'*1.energyusec + `senergyusec2LR'*2.energyusec + `spricechanged1LR'*1.pricechanged + `sOECDc2LR'*2.OECDc + `sOECDc3LR'*3.OECDc + 1*2.sectorc + 0*3.sectorc + 0*avyearcd + `smacrod1LR'*macrod + `sdatafreqd1LR'*datafreqd + `sestdynd1LR'*estdynd + `sestsysd1LR'*estsysd + 1*identifd + 1*inccontrd + 1*xpricecontd + 1*topjourd + `slogcitLR'*logcit + 0*byproduct + `spreferc2LR'*2.preferc
matrix bestprac[9,4] = r(estimate)
matrix bestprac[9,5] = r(ub)
matrix bestprac[9,6] = r(lb)

matrix list bestprac
matmap bestprac bestprac, map(round(@, 0.001)) 
esttab matrix(bestprac) using bestprac.tex, tex replace

*/