use "C:\Users\bhem\Nextcloud2\project-vwl4makro\09_Forschung\Drittmittel\DZ2022\WP2_Meta_energy\documentation\Meta_Lit_wo_dublicates_27543
> .dta" 

preserve
* drop sources that are not journal articles and that are published before 1980
* note: We kept sources where the publication year was missing and samples that information later on
keep if itemtype == "journalArticle" & excludeYear != 1

* retrieve sources with missing or incomplete abstracts
keep if missAbs > 0

export delimited key publicationyear author title doi url abstractnote using "C:\Users\bhem\Nextcloud2\project-vwl4makro\09_Forschung\Drit
> tmittel\DZ2022\WP2_Meta_energy\documentation\Meta_miss_abstracts.csv", replace

restore



