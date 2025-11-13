# Abstract screening

Using [`ASReview_sample_drawing.smcl`](ASReview_sample_drawing.smcl), we first drew a [1% random sample](1_percent_random_sample/1_Percent_random_sample_from_final_Bib.csv) from the 22,023 entries of the [unlabelled (unscreened) full sample of studies](Meta_bib_final.csv). We then conducted abstract screening using [ASReview](https://asreview.nl/) on this 1 % sample to pilot the selection process and to estimate the number of potentially relevant entries in the full sample for the calibration of our stopping rule (see [Sec. *Stopping rules for screening and data extraction* of our pre-registration](https://osf.io/zdche) for details of the abstract screening and [Sec. *Selection of studies, method of extracting data / Abstract Screening Phase*](https://osf.io/zdche) for the stopping rules). In addition to the piloting, the labbeled 1% sample also provided the prior information for the active learning-based abstract screening of the full sample. The results of this initial screening are stored as [asreview_dataset_mp-energy-1-random-sample_update.csv](1_percent_random_sample/asreview_dataset_mp-energy-1-random-sample_update.csv).

We then merged the 1% screened and labelled entries with the remaining entries from the full sample as [`META-EP_full_sample_partially_labeled.csv`](META-EP_full_sample_partially_labeled.csv). This file was then independently used by two screeners for the abstract screening process.

The identical start projects for abstract screening are:

- [`abstract-screening-morpep-energy.asreview`](ASReview Start Projects/AS_BM/abstract-screening-morpep-energy.asreview) for screener 1
- [`abstrac-screening-morpep-energy.asreview`](ASReview Start Projects/AS_FP/abstrac-screening-morpep-energy.asreview) for screener 2

After the abstract screening finished, the final files were stored in the respective folder as: 

- [`abstract-screening-morpep-energy-2023-03-19.asreview`](ASReview Start Projects/AS_BM/abstract-screening-morpep-energy-2023-03-19.asreview) for screener 1
- [`abstrac-screening-morpep-energy-2023-04-04.asreview`](ASReview Start Projects/AS_FP/abstrac-screening-morpep-energy-2023-04-04.asreview) for screener 2

The `.asreview` project files can be opened in [ASReview](https://asreview.nl/) to directly inspect the screening results from within the software. 

## Analyses of screening

Using [`merging_screener_data.R`](/database_search/processed/post_abstract_screening/Merged_data/merging_screener_data.R), we did some consistency checks and analysis of the screening as well as preparations for the retrieval of full-text PDFs and full text screening of the potentially eligible studies.

### Screener agreement and overlap

- 3303 entries (excluding the prior sample) were screened by either screener 1 or 2 (or both). 1480 were included by screener 1. 1662 were included by screener 2.
- 1210 entries were included by both screeners.
- 889 entries were excluded by both screeners.
- 201 entries were included by screener 1 but excluded by screener 2.
- 371 entries were included by screener 2 but excluded by screener 1.
- 2671 entries were screened by both screeners. Of these, 1411, i.e. 52.8 %, were included by Screener 1 and 1581, i.e. 59.2 %, were included by Screener 2.
- 411 entries were screened exclusively by Screener 1. Of these, 69 were included, i.e. 16.8 %.
- 221 entries were screened exclusively by Screener 2. Of these, 81 were included, i.e. 36.7 %.
- Total number of relevant entries after abstract screening (including 17 from the prior sample): 1949

### Randomization and packages creation for full text download

-   Order of entries has been randomized
-   Data is split into a number `.xslx` [files](/database_search/processed/post_abstract_screening/Packages_for_full_text_download) with max of 100 entries in each file. These files contain only information relevant for retrieving PDFs and empty columns for notes regarding availability. They were used for the retrieval of full text PDFs by student assistants.
