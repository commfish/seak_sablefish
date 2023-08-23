# Northern Southeast Inside Waters (NSEI) sablefish (*Anoplopoma fimbria*) stock assessment

Please direct any questions to: 
Phil Joy (philip.joy@alaska.gov, ummjane@gmail.com) or Rhea Ehresmann (rhea.ehresmann@alaska.gov)

Phil Joy took over this position and assessment in 2022 from Jane Sullivan, now at NOAA.
This fork will contain all assessments of NSEI sablefish from 2022 until otherwise stated.
Jane's orginal model and code has been left unaltered as she left it in 2021.  It is in 
the branch labelled "seak_sablefish_thru2021_original_JS".  
The 2022 assessment changed little from the 2021 assessment as done by Jane.

Last updated: August 2023

## Reports
2023 forecast: [git word document](https://github.com/commfish/seak_sablefish/blob/master/2023/text/RIR.1J.2023.XX_NSEI_Final.docx).  Please note that as of August 2023 the ADF&G publication department is undergoing revisions and publications have been temporarily suspended.  Please site the github site for this report until official publications are made available. 

2022 forecast: http://www.adfg.alaska.gov/FedAidPDFs/RIR.1J.2022.19.pdf

2021 forecast: http://www.adfg.alaska.gov/FedAidPDFs/RIR.1J.2021.13.pdf

2020 forecast: http://www.adfg.alaska.gov/FedAidPDFs/RIR.5J.2020.05.pdf.

2019 forecast: http://www.adfg.alaska.gov/FedAidPDFs/RIR.5J.2019.03.pdf.

The 2017, 2018, and 2019 forecasts were developed as reproducible research projects in `RMarkdown` and found under `text/`(https://github.com/commfish/seak_sablefish/tree/master/text).

## Fishery development and history 

*History for NSEI and SSEI fisheries modified from a recent Board of Fish Report*

State managed fisheries currently occur in NSEI (Chatham Strait and Frederick Sound) and SSEI (Clarence Strait and adjacent waters of Dixon Entrance). Sablefish have been harvested in the internal waters of Southeast Alaska since the early 1900s. Prior to the 1940s, sablefish were primarily landed as bycatch in the halibut fishery (Bracken 1983). Halibut longline gear was modified in the late 1940s to target sablefish. Until the 1970s, harvest levels fluctuated widely due to low price and better opportunities in other fisheries. Pot gear was first introduced in 1970 in the Clarence Strait and Dixon Entrance areas and the pot fishery accounted for 33% of the total harvest in the early 1970s. In 1981, the NSEI fishery was restricted to longline gear only, but pot gear was still allowed in the SSEI Subdistrict.

Sablefish have been historically managed with limitations on fishing seasons and harvest levels. Season limitations were first imposed in 1945 for the NSEI management area and in 1982 for the SSEI management area (Bracken 1983). Fishing seasons continued to be shortened in both areas as effort escalated in the 1970s and 1980s. Guideline Harvest Regulations (GHR) based on historic catches were established for both areas in 1980. In 1985, a limited entry program was implemented for both the NSEI and SSEI sablefish fisheries. However, the number of vessels and overall operating efficiency of the longline fleet increased significantly after the limited entry program was implemented. In order to stay within GHRs, the department continued to reduce the number of fishing days in both areas. In the NSEI area, the number of fishing days fell from 76 days in 1980 to one day in 1987. One-day openings continued in the NSEI area through 1993. In 1993, the NSEI fleet harvested 3,640,000 dressed lb, which was 2,140,000 dressed lb over the upper bound of the GHR (1,500,000 dressed lb). In an effort to improve management, the board adopted an equal quota share (EQS) system for the NSEI area in 1994.

![alt text](https://github.com/commfish/seak_sablefish/blob/master/readme/NSEI_map.jpg)
![alt text](https://github.com/commfish/seak_sablefish/blob/master/readme/fishery_harvest.jpg)

## Stock assessment

The Alaska Department of Fish and Game (ADFG) conducts an annual mark-recapture pot survey in NSEI in May that has served as the basis for stock assessment and management since 2006 (Stahl and Holum 2010). Tags are recaptured in the ADFG longline survey in July and the longline fishery in August (Beder and Stahl 2016). A time-stratified Chapmanized Petersen model is used to estimate abundance in the Bayesian open source software `JAGS 4.3.0` (Chapman 1951, Sullivan and Williams 2018, Depaoli 2016). 

Prior to 2020 the abundance estimate was partitioned into age classes and biomass estimates using age composition and weight-at-age data collected during the longline survey and fishery. A yield-per-recruit (YPR) model was used to estimate F_50%, the fishing mortality rate that reduces female spawning stock biomass to 50% of unfished levels, using the `optim()` function in the statistical software `R` (R Core Team 2018). ADFG defines the maximum Acceptable Biological Catch (ABC) as F_maxABC = F_50% for the NSEI sablefish stock (Dressel 2009). 

Several factors motivated the development of a statistical catch-at-age (SCAA) model, which was implemented for management of the 2020 NSEI sablefish fishery. The current ADFG framework relies heavily on the mark-recapture experiment, which is vulnerable to future budget cuts. The mark-recapture estimate provides a single snapshot in time and therefore results in high inter-annual variability in abundance and biomass estimates. Consequently, we are unable to fully integrate the available data sources, explore historical trends, or adequately assess stock status or harvest strategies. ADFG collects a significant amount of data in the NSEI through multiple surveys, logbooks, and port sampling. Moving to a new modeling framework will allow us to better utilize these data and will make management more resilient to potential budget cuts. Finally, strong recruitment from the 2014 and 2016 year classes were reported in the Federal assessment, prompting questions about how to treat the uncertainty in recruitment for State management (Hanselman et al. 2017, Sullivan and Williams 2018). A statistical catch-at-age model coded in Template Model Builder (`TMB`) will allow more flexibility in exploring recruitment using random effects (Kristensen 2016 et al. 2016).

The annual schedule for stock assessments, survey, fishery, and data processing are as follows:

![alt text](https://github.com/commfish/seak_sablefish/blob/master/readme/assessment_timeline.jpg)

## Data

The SCAA model uses a combination of catch, longline survey and fishery CPUE, mark-recapture abundance estimates, length, and age data. Fisheries-independent data and inputs to the SCAA model are made available under [`data/`](https://github.com/commfish/seak_sablefish/tree/master/data) of this repository. Fisheries-dependent data are not made public to protect fishermen and processor confidentiality, but may be obtained through a formal data request to ADFG. Data are available for the following years:

![alt text](https://github.com/commfish/seak_sablefish/blob/master/readme/available_data.jpg)

The following product recovery rates for sablefish were used:

Ice and slime assumed to be 2% of total weight.

| Delivery code | Description                    | Rate |
|---------------|--------------------------------|-----:|
| 1             | Whole/round                    |    1 |
| 4             | Gutted, head on                | 0.89 |
| 7             | Headed and gutted, Western cut | 0.68 |
| 9 (08 in IFDB)| Headed and gutted, Eastern cut | 0.63 |

Here is a summary of project codes in the data:

| New (Zander) | Old (IFDB) | Description                    |
|--------------|------------|-------------------------------:|
| 601          | 01         | Clarence Sablefish LL Survey   |
| 602          | 02         | Commercial Longline Trip       |
| 603          | 03         | Chatham Sablefish LL Survey    |
| 604          | 04         | Commercial Jig Trip            |
| 605          | 05         | Longline Survey (NMFS survey)  |
| 606          | 06         | Jig Survey                     |
| 607          | 07         | Atypical Sample (unknown gear) |
| 608          | 08         | Atypical Longline Sample       |
| 609          | 09         | Atypical Jig Sample            |
| 610          | 10         | Clarence Sablefish Pot Survey  |
| 611          | 11         | Chatham Sablefish Pot Survey   |
| 612          | 12         | Sitka Harbor Sablefish Survey  |
| 613          | 13         | Kodiak Trawl Sablefish Survey  |
| 614          | 14         | 1979 NSEI Crab Survey          |
| 615          | 15         | IPHC Annual Survey             |
| 616          | 16         | NMFS Coop Tagging Survey       |
| 617          | 17         | Commercial Pot Trip            |
| 618          | 18         | Lingcod Stock Assessment       |
| 619          | 19         | Black Rockfish Stock Assessment|
| 620          | 20         | Commercial Troll               |
| 621          | 21         | Commercial Halibut Longline    |
| 622          | 22         | Atypical Trawl Sample          |
| 623          | 23         | Canadian Commercial Longline   |
| 624          | 24         | Canadian Commercial Pot        |
| 625          | 25         | Canadian Commercial Trawl      |
| 626          | 26         | Canadian Scientific Survey     |
| 627          | 27         | Subsistence/Personal Use       |
| 628          | 28         | Sport-caught Sample            |


## Code

The analyses underpinning the current stock assessment are found in the folder labelled with the most recent year.  Typically the assessment occurs between January and April and thus the most recent year will contain code and work that is in progress during that time frame.  The helper and function files are contained in their own folder [`r_helper/`](https://github.com/commfish/seak_sablefish/tree/master/r_helper).  The rest of the scripts necessary to run the assessment are contained in the appropriate year folder, as well as the legacy folder, and are described below.  The scripts should be run in the order listed.  

![alt text](https://github.com/commfish/seak_sablefish/blob/master/readme/steps_to_run_assessment.jpg)

**Description of R scripts:**  
1.  [`r_helper/helper.r`](https://github.com/commfish/seak_sablefish/blob/master/r_helper/helper.R): Sourced by most other R scripts in this project, includes libraries/dependecies and ggplot themes;
2.  [`r_helper/functions.r`](https://github.com/commfish/seak_sablefish/blob/master/r_helper/functions.R):  Sourced by most other R scripts in this project, includes user-defined functions; 
3.  `YEAR/r/fishery_cpue_fr_OceanAK_ftx_lb_dat.R`: Instructions on pulling fish ticket and logbook data from OceanAK, cleaning it and merging the two data sources for CPUE calculation; 
4.  `YEAR/r/0_querynclean_data.r`: Descriptions of the data, `SQL` queries, and subsequent manipulations to clean raw data.  This script is no longer used as the data is pulled from OceanAK database;
5.  `YEAR/r/0_clean_data.R`: Descriptions of data and manipulations to clean raw data for analysis; 
6.  `YEAR/r/fishery_catch.R`: calculate and graphics for fishery catch data; 
7.  `YEAR/r/fishery_ll_cpue.R`: calculate and standardize longline fishery cpue;
8.  `YEAR/r/fishery_pot_cpue.R`: calculate and standardize pot fishery cpue; 
9.  `YEAR/r/llsurvey_cpue.r`: ADFG longline survey CPUE analysis and a preliminary steps towards a CPUE standardization;
10.  `YEAR/r/biological.r`: analysis of fishery and longline survey data, including modeling of growth, length-weight allometry, maturity, and sex ratios, as well as compilation of age and length compositions for stock assessment;
11.  `YEAR/r/mark_recapture.r`: clean release and recapture data, evaluate assumptions for mark-recapture experiment, and conduct analysis and model selection for the mark-recapture analysis; mark-recapture abundance estimated using the Bayesian software `JAGS`;
12.  `YEAR/r/scaa_dataprep.r`: compilation of catch, indices of relative and absolute abundace, age and length comps, biological data, and fishery retention probabilities for use in the SCAA model; also includes conversion tables for age-length-weight, which appears in an appendix in the 2020 assessment;
13.  `YEAR/r/scaa.r`: run SCAA model, generate output, results, and figures for assessment; also includes prelim work to run the SCAA as a Bayesian model;
14.  `YEAR/r/tune_comps.r`: estimate effective samples sizes for age/length comps using McAllister and Ianelli (1997) with harmonic mean; implemented for assessment in 2023;
15.  `YEAR/r/retrospective.r`: retrospective analysis to evaluate performance of SCAA model;
17.  `YEAR/r/marking_survey_analysis.r`: sensitivity analysis of marking survey/abundance estimate on SCAA results; impact of moving to a bi- or triennial stock assessment; appeared in 2020 forecast, does not need to be rerun annually;
18.  `YEAR/r/ypr.r`: run YPR stock assessment by partitioning mark-recapture abundance estimate into sex and age classes, estimating F50 YPR model, and calculating ABC;
19.  `YEAR/r/ageing_error_matrix.r`: old code from Kray Van Kirk (previous biometrician) that may be useful when developing an updated ageing error matrix.

The `.cpp file` for the SCAA model is found in `tmb/` folder in each YEAR folder.

## Session Info

Last updated: May 2023
```
devtools::session_info()
─ Session info ────────────────────────────────────────────────────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.2.1 (2022-06-23 ucrt)
 os       Windows 10 x64 (build 19044)
 system   x86_64, mingw32
 ui       RStudio
 language (EN)
 collate  English_United States.utf8
 ctype    English_United States.utf8
 tz       America/Anchorage
 date     2023-01-18
 rstudio  2022.07.2+576 Spotted Wakerobin (desktop)
 pandoc   NA

─ Packages ────────────────────────────────────────────────────────────────────────────────────────────────────────────────
 package      * version date (UTC) lib source
 assertthat     0.2.1   2019-03-21 [1] CRAN (R 4.2.1)
 cachem         1.0.6   2021-08-19 [1] CRAN (R 4.2.1)
 callr          3.7.2   2022-08-22 [1] CRAN (R 4.2.1)
 cli            3.3.0   2022-04-25 [1] CRAN (R 4.2.1)
 colorspace     2.0-3   2022-02-21 [1] CRAN (R 4.2.1)
 crayon         1.5.2   2022-09-29 [1] CRAN (R 4.2.1)
 DBI            1.1.3   2022-06-18 [1] CRAN (R 4.2.1)
 devtools       2.4.4   2022-07-20 [1] CRAN (R 4.2.1)
 digest         0.6.29  2021-12-01 [1] CRAN (R 4.2.1)
 dplyr          1.0.10  2022-09-01 [1] CRAN (R 4.2.1)
 ellipsis       0.3.2   2021-04-29 [1] CRAN (R 4.2.1)
 fansi          1.0.3   2022-03-24 [1] CRAN (R 4.2.1)
 fastmap        1.1.0   2021-01-25 [1] CRAN (R 4.2.1)
 fs             1.5.2   2021-12-08 [1] CRAN (R 4.2.1)
 generics       0.1.3   2022-07-05 [1] CRAN (R 4.2.1)
 GGally         2.1.2   2021-06-21 [1] CRAN (R 4.2.1)
 ggplot2        3.3.6   2022-05-03 [1] CRAN (R 4.2.1)
 ggrepel        0.9.1   2021-01-15 [1] CRAN (R 4.2.1)
 glue           1.6.2   2022-02-24 [1] CRAN (R 4.2.1)
 gtable         0.3.1   2022-09-01 [1] CRAN (R 4.2.1)
 htmltools      0.5.3   2022-07-18 [1] CRAN (R 4.2.1)
 htmlwidgets    1.5.4   2021-09-08 [1] CRAN (R 4.2.1)
 httpuv         1.6.6   2022-09-08 [1] CRAN (R 4.2.1)
 later          1.3.0   2021-08-18 [1] CRAN (R 4.2.1)
 lifecycle      1.0.2   2022-09-09 [1] CRAN (R 4.2.1)
 magrittr       2.0.3   2022-03-30 [1] CRAN (R 4.2.1)
 memoise        2.0.1   2021-11-26 [1] CRAN (R 4.2.1)
 mime           0.12    2021-09-28 [1] CRAN (R 4.2.0)
 miniUI         0.1.1.1 2018-05-18 [1] CRAN (R 4.2.1)
 munsell        0.5.0   2018-06-12 [1] CRAN (R 4.2.1)
 pillar         1.8.1   2022-08-19 [1] CRAN (R 4.2.1)
 pkgbuild       1.3.1   2021-12-20 [1] CRAN (R 4.2.1)
 pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.2.1)
 pkgload        1.3.0   2022-06-27 [1] CRAN (R 4.2.1)
 plyr           1.8.7   2022-03-24 [1] CRAN (R 4.2.1)
 prettyunits    1.1.1   2020-01-24 [1] CRAN (R 4.2.1)
 processx       3.7.0   2022-07-07 [1] CRAN (R 4.2.1)
 profvis        0.3.7   2020-11-02 [1] CRAN (R 4.2.1)
 promises       1.2.0.1 2021-02-11 [1] CRAN (R 4.2.1)
 ps             1.7.1   2022-06-18 [1] CRAN (R 4.2.1)
 purrr          0.3.4   2020-04-17 [1] CRAN (R 4.2.1)
 R6             2.5.1   2021-08-19 [1] CRAN (R 4.2.1)
 RColorBrewer   1.1-3   2022-04-03 [1] CRAN (R 4.2.0)
 Rcpp           1.0.9   2022-07-08 [1] CRAN (R 4.2.1)
 remotes        2.4.2   2021-11-30 [1] CRAN (R 4.2.1)
 reshape        0.8.9   2022-04-12 [1] CRAN (R 4.2.1)
 reshape2       1.4.4   2020-04-09 [1] CRAN (R 4.2.1)
 rlang          1.0.6   2022-09-24 [1] CRAN (R 4.2.1)
 rstudioapi     0.14    2022-08-22 [1] CRAN (R 4.2.1)
 scales         1.2.1   2022-08-20 [1] CRAN (R 4.2.1)
 sessioninfo    1.2.2   2021-12-06 [1] CRAN (R 4.2.1)
 shiny          1.7.2   2022-07-19 [1] CRAN (R 4.2.1)
 stringi        1.7.8   2022-07-11 [1] CRAN (R 4.2.1)
 stringr        1.4.1   2022-08-20 [1] CRAN (R 4.2.1)
 tibble         3.1.8   2022-07-22 [1] CRAN (R 4.2.1)
 tidyselect     1.1.2   2022-02-21 [1] CRAN (R 4.2.1)
 urlchecker     1.0.1   2021-11-30 [1] CRAN (R 4.2.1)
 usethis        2.1.6   2022-05-25 [1] CRAN (R 4.2.1)
 utf8           1.2.2   2021-07-24 [1] CRAN (R 4.2.1)
 vctrs          0.4.2   2022-09-29 [1] CRAN (R 4.2.1)
 xtable         1.8-4   2019-04-21 [1] CRAN (R 4.2.1)

```
