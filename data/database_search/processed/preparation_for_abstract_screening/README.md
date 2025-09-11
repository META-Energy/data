# Preparing data for title and abstract screening

Here we outline the steps which were conducted on the data from the initial database search to prepare our machine-learning supported title and abstract screening process. We also store intermediate data files which were created during preparation.

## 1. De-duplication

After our initial database search, we merged the results from EconLit and Google Scholar and removed duplicates using the bibliographic software Zotero (version: 6.0.21) and the Zotero Duplicates Merger add-on (version: v1.1.5). We took care to preserve the existing complete abstracts from EconLit when merging the duplicates. See here for the merged and duplicate-free results in .csv (`Meta_Bibliothek_EconLit_GS_ohne_Dub.csv`) and `.dta` format (`Meta_Lit_wo_dublicates_27543.dta`).

- Entries after merging of EconLit and Google Scholar results: 30,858
- Entries after removal of duplicates: 27,543

## 2. Application of eligibility criteria and Retrieving missing abstracts

We applied the inclusion and exclusion criteria as in [Sec. *Eligibility: Inclusion and Exclusion Criteria*](...) of our pre-registration on the duplicate-free results which yielded 22,434 results. 
We then exported those 5,101 entries that have no abstract or only an incomplete abstract (Google Scholar only extracts abbreviated abstracts and both Google Scholar and EconLit sometimes have missing abstracts). See here for the `.do` (`extracting_abstracts.do`) file to retrieve those entries and the resulting `.csv` (`Meta_miss_abstracts.csv`) file. 

A student assistant and a co-author collected those abstracts from the websites of bibliographic entries where possible, based on the URLs or using Google Scholar when only the title was available. The resulting collection of 4,690 additions with then complete abstracts were re-integrated using Zotero. Resulting duplicates were removed using the Zotero Duplicates Merger add-on (version: v1.1.5). We took care to preserve the entry with existing complete abstract when merging the duplicates. See here for the final dataset in `.csv` (`Meta_bib_final.csv`) and `.dta` (`Bib_1980_noBook_allAbstracts.dta`).

**The resulting literature database contains 22,023 entries and was then screened for eligiblilty based on title and abstract.**
