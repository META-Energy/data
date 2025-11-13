******************************************************************************
******************************************************************************
*Meta-analysis MORPEP Energy Prices
******************************************************************************
******************************************************************************
clear all
set more off
use metaenergy_final

cd "output" //directs to output folder. In case you use your own folder structure, you need to create the folder first or direct to another subfolder if wanted. 

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

*Table 1 - summary stats of main variables
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
collect style cell var[`xcatm']#result[mean-share], nformat(%9.2f) sformat("%s%%")
collect style cell var[`xcatm']#result[SD-obs], nformat(%9.0fc)
collect style cell var[`xdumm']#result[mean-share], nformat(%5.4f) sformat("%s%%")
collect label dim horizond "Time Horizon", modify
collect preview
collect export "Tab1_dstatmain.tex", tableonly  replace 
collect export "Tab1_dstatmain.html", tableonly  replace 


*Table A1 - summary stats of auxiliary variables
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
collect export "TabA1_dstataux.tex", tableonly  replace 
collect export "TabA1_dstataux.html", tableonly  replace 

*/


*****************************************************************************************
*PLOTS - HISTOGRAMS
****************************************************************************************
*Figure A2
*histo SR LR
twoway (hist coeffc if horizond==0, freq bin(30) gap(10) lcolor(navy) fcolor(navy) legend(label(1 "Short run"))) (hist coeffc if horizond==1, freq bin(30)  gap(40) fcolor(cranberry) lcolor(cranberry) legend(label(2 "Long run")))
graph export "FigA2_histSRLR.pdf", replace

*histo Energysourcemain
twoway (hist coeffc if energysourcem==1, freq bin(30) gap(10) lcolor(navy) fcolor(navy) legend(label(1 "Electricity"))) (hist coeffc if energysourcem==2, freq bin(30)  gap(40) fcolor(cranberry) lcolor(cranberry) legend(label(2 "Natural Gas")))   (hist coeffc if energysourcem==3, freq bin(30) gap(60) fcolor(gs12) lcolor(gs12) legend(label(3 "Other Mixed"))) 
graph export "FigA2_histEsourcemain.pdf", replace

*histo Energysourcerare
twoway  (hist coeffc if energysourcec==2, freq bin(30) gap(10) fcolor(brown) lcolor(brown) legend(label(1 "Coal"))) (hist coeffc if energysourcec==5, freq bin(30) gap(40) fcolor(khaki) lcolor(khaki) legend(label(2 "Heat Oil")))   (hist coeffc if energysourcec==4, freq bin(30) gap(60) fcolor(midblue) lcolor(midblue) legend(label(3 "LPG")))   (hist coeffc if energysourcec==6, freq bin(30) gap(80) fcolor(gs12) lcolor(gs12) legend(label(4 "Mixed"))) 
graph export "FigA2_histEsourcerare.pdf", replace

*histo Energyusec
twoway (hist coeffc if energyusec==1, freq bin(30) gap(10) lcolor(navy) fcolor(navy) legend(label(1 "Heating"))) (hist coeffc if energyusec==2, freq bin(30)  gap(40) fcolor(cranberry) lcolor(cranberry) legend(label(2 "Cooling")))   (hist coeffc if energyusec==3, freq bin(30) gap(60) fcolor(gs12) lcolor(gs12) legend(label(3 "Mixed"))) 
graph export "FigA2_histEuse.pdf", replace

*histo sectorc
twoway (hist coeffc if sectorc==1, freq bin(30) gap(10) lcolor(navy) fcolor(navy) legend(label(1 "Residential"))) (hist coeffc if sectorc==2, freq bin(30)  gap(40) fcolor(cranberry) lcolor(cranberry) legend(label(2 "Business"))) (hist coeffc if sectorc==3, freq bin(30) gap(60) fcolor(gs12) lcolor(gs12) legend(label(3 "Mixed"))) 
graph export "FigA2_histsector.pdf", replace

*histo OECD non mixed
twoway (hist coeffc if OECDc==1, freq bin(30) gap(10) lcolor(navy) fcolor(navy) legend(label(1 "OECD"))) (hist coeffc if OECDc==2, freq bin(30) gap(40) fcolor(cranberry) legend(label(2 "nonOECD"))) (hist coeffc if OECDc==3, freq bin(30) gap(60) fcolor(gs12) lcolor(gs12) legend(label(3 "Mixed")))
graph export "FigA2_histOECD.pdf", replace


*/



*****************************************************************************************
*FUNNEL PLOTS - PUBLICATION BIAS
*****************************************************************************************
*Figure 3

* Short run 
su coeffc if horizond==0
local arm = r(mean)
scatter prec coeffc if horizond==0 & abs(tstatc)>=1.96, msize(*.5) msymbol(Oh) mcolor(navy) legend(pos(11) ring(0) col(1) label(1 "sig5%")) || scatter prec coeffc if horizond==0 & abs(tstatc)<1.96, msize(*.5) msymbol(O) mcolor(gs12) legend(label(2 "insig5%")) xtitle(Elasticity short run) ytitle(1/SE) xline(`arm')
graph export "Fig3_funnel_SR.pdf", as(pdf) replace

* Long run 
su coeffc if horizond==1
local arm = r(mean)
scatter prec coeffc if horizond==1 & abs(tstatc)>=1.96, msize(*.5) msymbol(Oh) mcolor(navy) legend(pos(11) ring(0) col(1) label(1 "sig5%")) || scatter prec coeffc if horizond==1 & abs(tstatc)<1.96, msize(*.5) msymbol(O) mcolor(gs12) legend(label(2 "insig5%")) xtitle(Elasticity long run) ytitle(1/SE) xline(`arm')
graph export "Fig3_funnel_LR.pdf", as(pdf) replace



*Figure B.6
*Funnels with highlighting for inferior and qualconcerns
* Short run 
su coeffc if horizond==0
local arm = r(mean)
scatter prec coeffc if horizond==0 & abs(tstatc)>=1.96 & preferc!=0 & qconcernd != 1, msize(*.5) msymbol(Oh) mcolor(navy) legend(pos(11) ring(0) col(1) label(1 "sig5%")) || scatter prec coeffc if horizond==0 & abs(tstatc)<1.96 & preferc!=0 & qconcernd != 1, msize(*.5) msymbol(O) mcolor(gs12) legend(label(2 "insig5%")) || scatter prec coeffc if horizond==0 & preferc==0, msize(*.5) msymbol(X) mcolor(red) legend(label(3 "inferior")) || scatter prec coeffc if horizond==0 & qconcernd!=0, msize(*.5) msymbol(+) mcolor(green) legend(label(4 "quality concern")) xtitle(Elasticity shortrun) ytitle(1/SE) xline(`arm')
graph export "FigB6_funnel_SR_qual.pdf", as(pdf) replace

* Long run 
su coeffc if horizond==1
local arm = r(mean)
scatter prec coeffc if horizond==1 & abs(tstatc)>=1.96 & preferc!=0 & qconcernd != 1, msize(*.5) msymbol(Oh) mcolor(navy) legend(pos(11) ring(0) col(1) label(1 "sig5%")) || scatter prec coeffc if horizond==1 & abs(tstatc)<1.96 & preferc!=0 & qconcernd != 1, msize(*.5) msymbol(O) mcolor(gs12) legend(label(2 "insig5%")) || scatter prec coeffc if horizond==1 & preferc==0, msize(*.5) msymbol(X) mcolor(red) legend(label(3 "inferior")) || scatter prec coeffc if horizond==1 & qconcernd!=0, msize(*.5) msymbol(+) mcolor(green) legend(label(4 "quality concern")) xtitle(Elasticity longrun) ytitle(1/SE) xline(`arm')
graph export "FigB6_funnel_LR_qual.pdf", as(pdf) replace

*/

******************************************************************************
*FUNNEL ASYMMETRY TESTS - Publication bias
******************************************************************************
*Table 2: linear tests
eststo clear
xtset paperid pobsid

*********** FAT short-run 
qui: eststo UA: ivreg2 coeffc if horizond==0, cluster (paperid) 
qui: eststo UWLS: ivreg2 coeffc [aweight=prec2] if horizond==0, cluster (paperid)
qui: eststo OLS: ivreg2 coeffc sec if horizond==0, cluster (paperid)
qui: eststo FE: xtreg coeffc sec if horizond==0, fe cluster (paperid)
qui: eststo PET: ivreg2 coeffc sec [aweight=prec2] if horizond==0, cluster (paperid)
qui: eststo PEESE: ivreg2 coeffc sec2 [aweight=prec2] if horizond==0, cluster (paperid)
qui: eststo Median: ivreg2 coeffc sec [aweight=prec2] if horizond==0 & cmeddumunique==1

esttab using "Tab2_pblin_SR.html", se ar2 replace mtitles label title(Linear publication bias tests, SR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab using "Tab2_pblin_SR.tex", se ar2 booktabs replace compress title (Linear publication bias tests, SR \label{tab:pblinSR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
eststo clear

************ FAT long-run 
qui: eststo UA: ivreg2 coeffc if horizond==1, cluster (paperid) 
qui: eststo UWLS: ivreg2 coeffc [aweight=prec2] if horizond==1, cluster (paperid)
qui: eststo OLS: ivreg2 coeffc sec if horizond==1, cluster (paperid)
qui: eststo FE: xtreg coeffc sec if horizond==1, fe cluster (paperid)
qui: eststo PET: ivreg2 coeffc sec [aweight=prec2] if horizond==1, cluster (paperid)
qui: eststo PEESE: ivreg2 coeffc sec2 [aweight=prec2] if horizond==1, cluster (paperid)
qui: eststo Median: ivreg2 coeffc sec [aweight=prec2] if horizond==1 & cmeddumunique==1

esttab using "Tab2_pblin_LR.html", se ar2 replace mtitles label title(Linear publication bias tests, LR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab using "Tab2_pblin_LR.tex", se ar2 booktabs replace compress title (Linear publication bias tests, LR \label{tab:pblinLR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
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

esttab using "Fig4_pbsub_SR.html", se ar2 replace mtitles label title(Uncorrected and corrected elasticities for subsamples, SR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab using "Fig4__pbsub_SR.tex", se ar2 booktabs replace compress title (Uncorrected and corrected elasticities for subsamples, SR \label{tab:pbsubSR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
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

esttab using "Fig4_pbsub_LR.html", se ar2 replace mtitles label title(Uncorrected and corrected elasticities for subsamples, LR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab using "Fig4_pbsub_LR.tex", se ar2 booktabs replace compress title (Uncorrected and corrected elasticities for subsamples, LR \label{tab:pbsubSR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
eststo clear

*/

*************************
*Table B.3: winsorisation robustness
*note: reload dataset to undo baseline 2-98 winsorization

preserve
cd ..
use metaenergy_final, clear
cd "output"
 
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

esttab SR* using "TabB3_winsor_SR.html", se ar2 replace mtitles label title(PEESE test for different winsorization, SR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab SR* using "TabB3_winsor_SR.tex", se ar2 booktabs replace compress title (PEESE test for different winsorization, SR \label{tab:winsorSR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
esttab LR* using "TabB3_winsor_LR.html", se ar2 replace mtitles label title(PEESE test for different winsorization, LR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab LR* using "TabB3_winsor_LR.tex", se ar2 booktabs replace compress title (PEESE test for different winsorization, LR \label{tab:winsorLR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
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

esttab using "TabB4_pblinqual_SR.html", se ar2 replace mtitles label title(Linear publication bias tests without inferior estimates and quality concerns, SR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab using "TabB4_pblinqual_SR.tex", se ar2 booktabs replace compress title (Linear publication bias tests without inferior estimates and quality concerns, SR \label{tab:pblinSR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
eststo clear

************ FAT long-run 
qui: eststo UA: ivreg2 coeffc if horizond==1, cluster (paperid) 
qui: eststo UWLS: ivreg2 tstatc prec if horizond==1, noconstant cluster (paperid)
qui: eststo OLS: ivreg2 coeffc sec if horizond==1, cluster (paperid)
qui: eststo FE: xtreg coeffc sec if horizond==1, fe cluster (paperid)
qui: eststo PET: ivreg2 coeffc sec [aweight=prec2] if horizond==1, cluster (paperid)
qui: eststo PEESE: ivreg2 coeffc sec2 [aweight=prec2] if horizond==1, cluster (paperid)
qui: eststo Median: ivreg2 coeffc sec [aweight=prec2] if horizond==1 & cmeddumunique==1

esttab using "TabB4_pblinqual_LR.html", se ar2 replace mtitles label title(Linear publication bias tests without inferior estimates and quality concerns, LR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab using "TabB4_pblinqual_LR.tex", se ar2 booktabs replace compress title (Linear publication bias tests without inferior estimates and quality concerns, LR \label{tab:pblinLR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
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

*safe data for AK test that can be used in the web application. Then upload them at https://maxkasy.github.io/home/metastudy/
*or use the code with R https://github.com/maxkasy/MetaStudiesApp
export delimited coeffc sec using "Tab3_dataAK_sr.csv" if horizond==0, novarnames replace
export delimited coeffc sec using "Tab3_dataAK_lr.csv" if horizond==1, novarnames replace
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

esttab WAAPsr MAIVEsr using "Tab3_pbnonlin_SR.html", se ar2 scalars(widstat) replace mtitles label title(Nonlinear Pubbias Tests, SR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab WAAPsr MAIVEsr using "Tab3_pbnonlin_SR.tex", se ar2 scalars(widstat) booktabs replace compress title(Nonlinear Pubbias Tests, SR \label{tab:pbnonlin_SR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)

esttab WAAPlr MAIVElr using "Tab3_pbnonlin_LR.html", se ar2 scalars(widstat) replace mtitles label title(Nonlinear Pubbias Tests, LR) addnote("Standard errors in parentheses and clustered at the study level.") nostar
esttab WAAPlr MAIVElr using "Tab3_pbnonlin_LR.tex", se ar2 scalars(widstat) booktabs replace compress title(Nonlinear Pubbias Tests, LR \label{tab:pbnonlin_SR}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)

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

esttab using "Tab4_mmrcontrols.tex", se ar2 booktabs replace compress title(Multivariate meta regression, baseline and robustness checks with further controls \label{tab:mmrcontrols}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
esttab using "Tab4_mmrcontrols.html", se ar2 replace  mtitles label title(ultivariate meta regression, baseline and robustness checks with further controls) addnote("Standard errors in parentheses and clustered at the study level.") nostar nobaselevels 
eststo clear

*********************************************************
*Table 5: Multivariate meta regression, group-wise samples

qui: eststo SR: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if horizond==0, cluster (paperid)
qui: eststo LR: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if horizond==1, cluster (paperid)
qui: eststo elect: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if energysourcec==1, cluster (paperid)
qui: eststo natgas: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if energysourcec==3, cluster (paperid) // produces warning due to low number of natgas + cooling observations. But results do not change much if cooling variable is dropped.
qui: eststo resi: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if sectorc==1, cluster (paperid) 
qui: eststo busi: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if sectorc==2, cluster (paperid)  // produces warning due to low number of business + energysource mix observations. But results do not change much if energysource variable is dropped.

esttab using "Tab5_mmrgroup.tex", se ar2 booktabs replace compress title(Multivariate meta regression, group-wise samples \label{tab:mmrgroup}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
esttab using "Tab5_mmrgroup.html", se ar2 replace  mtitles label title(Multivariate meta regression, group-wise samples) addnote("Standard errors in parentheses and clustered at the study level.") nostar nobaselevels
eststo clear


*********************************************************
***Table C.2: Multivariate meta regression, subsamples with quality criteria

qui: eststo qual: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if preferc!=0 & qconcernd!=1, cluster (paperid)
qui: eststo pref: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if preferc==2, cluster (paperid)
qui: eststo icpcontr: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if inccontrd==1 & xpricecontd==1, cluster (paperid)
qui: eststo ident: ivreg2 coeffc sec2 `xbase' [aweight=prec2] if identifd==1, cluster (paperid)

esttab using "TabC2_mmrsub.tex", se ar2 booktabs replace compress title(Multivariate meta regression, subsamples with quality criteria \label{tab:mmrsub}) mtitles addnote("Standard errors in parentheses and clustered at the study level.") nostar label nonumber nogaps width(1\hsize)
esttab using "TabC2_mmrsub.html", se ar2 replace  mtitles label title(Multivariate meta regression, subsamples with quality criteria) addnote("Standard errors in parentheses and clustered at the study level.") nostar nobaselevels
eststo clear

*/



**************************
*Table 6. Best practice predictions
**************************
* drop low quality estimates
drop if qconcernd == 1
drop if preferc == 0

*derived from necessary baseline variables plus BMA
local xbase "ib1.energysourcem ib3.energyusec ib1.pricechanged ib1.OECDc ib1.sectorc"
local xqual "topjourd logcit byproduct ib1.preferc identifd inccontrd xpricecontd"
local xbma  "avyearcd macrod ib3.datadimc datafreqd estdynd estsysd"
local xbest "`xbase' `xqual' `xbma'"

*predetermine the matrix that will contain the results 
matrix bestprac = J(9,6,.)
matrix rownames bestprac = all electricity naturalgas heating cooling OECD nonOECD residential business
matrix colnames bestprac = SRpe SRub SRlb LRpe LRub LRlb

*define the scenarios
local scenarios "all elec natgas heat cool OECD nonOECD residential business"

* loop over SR and LR
foreach h in 0 1 {
    local hname = cond(`h'==0, "SR", "LR")
    local col0  = cond(`h'==0, 1, 4)
    local col1  = `col0' + 1
    local col2  = `col0' + 2

    * estimate model
    qui eststo best`=lower("`hname'")': ///
        ivreg2 coeffc sec2 `xbest' [aweight=prec2] if horizond==`h', cluster(paperid)

    * Best-Practice predictions for small se, average year, identification approach, income + xprice controls, top journal, no byproduct, preferred estimate, maximum number of citations.
	qui su logcit
    local slogcit`hname' = r(max)
	local AT_common "sec2=0 avyearcd=0 identifd=1 inccontrd=1 xpricecontd=1 topjourd=1 byproduct=0 preferc=(2) logcit=`slogcit`hname''"

    * run scenarios and fill the matrix
    local r = 0
    foreach s of local scenarios {
        local ++r
        local AT_extra ""   // overrides for each scenario

        if      "`s'"=="elec"       local AT_extra "energysourcem=(1)"
        else if "`s'"=="natgas"     local AT_extra "energysourcem=(2)"
        else if "`s'"=="heat"       local AT_extra "energyusec=(1)"
        else if "`s'"=="cool"       local AT_extra "energyusec=(2)"
        else if "`s'"=="OECD"       local AT_extra "OECDc=(1)"
        else if "`s'"=="nonOECD"    local AT_extra "OECDc=(2)"
        else if "`s'"=="residential" local AT_extra "sectorc=(1)"
        else if "`s'"=="business"    local AT_extra "sectorc=(2)"
 
        qui margins, predict(xb) asobserved at(`AT_common' `AT_extra')
        matrix T = r(table)   // rows: b se z p ll ul

        matrix bestprac[`r',`col0']   = T[1,1]   // point estimate
        matrix bestprac[`r',`col1']   = T[6,1]   // upper bound 95% CI
        matrix bestprac[`r',`col2']   = T[5,1]   // lower bound 95% CI
    }
}

matrix list bestprac
matmap bestprac bestprac, map(round(@, 0.001))
esttab matrix(bestprac, fmt(%9.3f)) using "Tab6_bestprac.tex", tex replace
esttab matrix(bestprac, fmt(%9.3f)) using "Tab6_bestprac.html", html replace

cd ..

