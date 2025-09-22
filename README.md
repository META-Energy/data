> [!NOTE]
>
> # CO2 pricing and the elasticity of heating and cooling energy demand
>
> ## About:
>  
> This repository contains raw and processed data, replication files and documentation for our **meta-study on the price elasticity of heating and cooling energy demand**. Below, we provide guidance on our research process and the documentation of our data collection, data processing and transformation, and the scripts for the data analyses.
> 
> ## Pre-registration:
>
> The pre-registration and pre-analysis plan for our meta-study can be found at [https://osf.io/zdche](https://osf.io/zdche).
>
> ## Research papers:
> 
> Gechert, S., Mey, B., Prante, F., & Schäfer, T. (2025). **The Price Elasticity of Heating and Cooling Energy Demand**. [**[OSF Preprint]**](https://osf.io/preprints/osf/4sjy5) [**[Replication files]**](analyses/replication_main_paper).
>
> **Abstract**: We create a large meta-dataset of price elasticities of energy demand for heating and cooling in buildings, comprising close to 5000 price elasticity estimates including study and observation characteristics from more than 400 primary studies. We find robust and strong signs of p-hacking and publication bias with insignificant or positive elasticities being underrepresented. Correcting for this bias, the price elasticities range from -0.05 to -0.2 for the short run and from -0.1 to -0.3 for the long run. This holds for all relevant fossil fuels and electricity, poor and rich countries, residential and business usage, and aggregate and survey data.
>
> ## Project:
> 
> This repository is part of the research project [Monetary Policy and Energy Prices](https://www.tu-chemnitz.de/wirtschaft/vwl4/Makrooekonomie/MORPEP.php.en) funded by the [European Macro Policy Network (EMPN)](https://empn.eu/).

---

## Data collection

### Literature data base search

We conducted a comprehensive search for literature that econometrically estimates energy price elasticities. We detailed our search strategy for relevant literature in [Sec. *Search Strategy* of our pre-registration](https://osf.io/zdche). We used the [EconLit](https://www.aeaweb.org/econlit/) and the [Google Scholar](https://scholar.google.com/) databases for our search of primary studies. Due to differences in their search behaviour,[^1] we decided to use one comprehensive query for EconLit and multiple simpler search queries for Google Scholar.

- The raw results and procedural details of our EconLit search can be accessed [here](data/database_search/raw/EconLit_search).

- The raw results and procedural details of our Google Scholar search can be accessed [here](data/database_search/raw/Google_Scholar_search).

[^1]: Check Sec. Search Strategy of the [pre-registration](https://osf.io/zdche) for details.

This yielded 23,948 bibliographic entries from our EconLit search and 6,910 bibliographic entries from our Google scholar search. After de-duplication, checks for the availability of abstracts and the application of eligibility criteria (see [here](data/database_search/processed/preparation_for_abstract_screening) for detailed documentation of these steps and related files), our consolidated dataset of primary studies with available abstracts totalled at 22,023 entries.

### Exclusion criteria

The eligibility of studies will be assessed in two steps. In the first step, clearly ineligible studies are excluded based on title and abstract (abstract screening phase). In the second step, the remaining studies will undergo full text screening (full-text  screening phase).
In our PAP (https://osf.io/zdche) we had already specified several exclusion criteria in advance. During the full-text screening phase, we expanded these criteria based on insights gained in the process and subsequently assigned them to the following nine categories of exclusion criteria :

1.	Master’s thesis: The study is a Master’s thesis and does not meet our quality requirements.

2.	Meta-analysis: The study is a meta-analysis or a systematic literature review.

3.	No own price elasticities are estimated: The study does not estimate own-price elasticities. For example, it reports elasticities from other papers, or only cross-price elasticities or other metrics (e.g., expenditure shares, budget shares).

4.	No full-text available: Full access to the study is not available.

5.	No inference citeria given: The paper reports a price elasticity but does not provide the required standard error, or it cannot be calculated due to missing information.

6.	Wrong energy source: The study focuses on an irrelevant energy source or application (e.g., the transport sector).

7.	Duplicate: The study focuses on an irrelevant energy source or application (e.g., the transport sector).

8.	Not written in English; The study is not written in English.

9.	High-frequency data: Studies using data with a frequency higher than monthly (e.g., intra-month data, daily data, intra-day data, hourly, or intra-hour data) are excluded.


### Abstract screening

This dataset of 22,023 studies then entered into the artificial intelligence-supported abstract screening to exclude clearly ineligible studies according to our eligibility criteria as defined in [Sec. *Selection of studies, method of extracting data*](https://osf.io/zdche) of our pre-registration. The title and abstract screening was conducted independently by two researchers using [ASReview](https://asreview.nl/). [Sec.*Abstract Screening Phase*](https://osf.io/zdche) of our pre-registration presents the details of the abstract screening process. [Sec. *Stopping rules for screening and data extraction*](https://osf.io/zdche) of our pre-registration defines the stopping rules for the abstract screening phase. See [here](data/database_search/processed/abstract_screening) for further documentation and related files of the abstract screening.

We then conducted some validity tests as well as agreement and overlap analysis on the merged abstract screening data of both screeners. We then randomized the order of the potentially relevant studies and prepared files to assist and document the full text download. See [here](/data/database_search/processed/post_abstract_screening/Merged_data/merging_testing_pre_download.html) for the documentation and [here](/data/database_search/processed/post_abstract_screening/Merged_data/merging_screener_data.R) for the R code for these steps.

#### Final duplicate check

After the title and abstract screening, a total of 1,949 potentially relevant studies remained. These were randomly split into 20 packages   for a final round of duplicate checks. During these checks we aimed to find matches where we have two or more different versions of a paper in our data set. For example, a working paper version and a journal version of the same paper. In such cases we only kept the most recent and published version. Thus, we made sure that we do not double code studies.

### Full text screening

#### PDF retrieval 

...

#### Full text assessment and coding

Studies were assessed based on a set of predefined eligibility criteria, which are described in detail in the [pre-analysis plan](https://osf.io/zdche). Furthermore, at this stage we classified the reasons for final study exclusion into nine categories.
If a study fulfilled all inclusion criteria, it was retained for data extraction. Cases of uncertainty during the screening or extraction process were discussed regularly within the research group to ensure consistent interpretation and application of the inclusion criteria. As part of this process, a set of variables was extracted for each price elasticity estimate, including information on the energy source, the sector, the estimation method, and the time horizon of the elasticity, among others.
Due to time constraints, it was not possible to screen all potentially relevant studies. The screening phase took place between May 2023 and February 2024. During this period, a total of 1,341 studies were screened, of which 920 were excluded based on the eligibility criteria. An overview of the studies that were excluded based on the predefined exclusion criteria can be found in [study_sets_ALL.xlsx](data/database_search/processed/post_abstract_screening/study_sets_ALL.xlsx). 421 studies fulfilled our eligibility criteria and provided a total of 4,974 elasticity estimates used in the final dataset.

#### Quality concerns 

Elasticities were flagged with a quality concern if we had reason to question the overall study quality or the reported estimates. These concerns can be grouped into six categories:

1)	Unclear or problematic research design, e.g.  expenditure share equations, translog cost functions.

2)	Inconsistent results or reporting, e.g.  the study reports negative standard errors, regression not given, reported t-statistics with implausible or wrong signs, coefficients in the table inconsistent with those discussed in the text.

3)	Implausible values in model validation, e.g. extremely high R², implausibly high precision, strong autocorrelation and heteroscedasticity issues.

4)	Authors question their own results, e.g. Authors state that reported elasticities might be biased or unstable. or note that results are difficult to interpret due to endogeneity, or they caution that their own estimates should be interpreted with care.

5)	Data limitations, e.g. authors point to restricted data availability in their analysis, which limits the interpretation of the reported estimates.

6)	Issues with language, clarity, or formatting, e.g. severe writing errors or formatting mistakes, badly written methodology sections with missing or contradictory explanations, or the study is a master's thesis.

#### Final check

After completion of the full dataset, a random quality check was conducted: at least 10\% of the extracted entries were independently reviewed by a second researcher to ensure coding accuracy and consistency.

---

## Data analyses

Folders with replication files for the data analyses in our research papers are linked [above](https://github.com/META-Energy/data?tab=readme-ov-file#research-papers).
