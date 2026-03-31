# Northern Southeast Inside Waters (NSEI) sablefish (*Anoplopoma fimbria*) stock assessment

*Fishery data needed for the assessment but not available from this repository is available on the Sitka Groundfish drive.* ***M:/SABLEFISH/CHATHAM/SCAA_Fishery_Data_for_new_bio_2024.***

## Assessment authors:

* 2019-2021: Jane Sullivan, now with NOAA. Development of the original SCAA model that replaced the SPR model used previously in this assessment. Original model and code archived in branch *"seak_sablefish_thru2021_original_JS"*
* 2022-2024: Phil Joy, now with ADF&G Sport Fish Division. SCAA development including selectivity development, random effects and fishery CPUE standardization.
* 2026-current: Aaron Lambert, ADF&G Commercial Fish Division. SCAA code updates and model exploration.

Last updated: March 2026

## Reports
2025 forecast: https://www.adfg.alaska.gov/FedAidPDFs/RIR.1J.2025.23.pdf

2024 forecast: https://www.adfg.alaska.gov/FedAidPDFs/RIR.1J.2025.03.pdf

2023 forecast: https://www.adfg.alaska.gov/FedAidPDFs/RIR.1J.2025.02.pdf

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

The NSEI sablefish stock assessment transitioned to a statistical catch-at-age (SCAA) model in 2020 to better integrate the wealth of data collected by ADF&G and reduce reliance on a single abundance index. The previous framework relied heavily on the mark–recapture experiment, which provides a single snapshot of abundance in time, resulting in high inter-annual variability in abundance and biomass estimates, and is vulnerable to future budget cuts. This limited our ability to fully integrate available data sources, explore historical trends, or adequately assess stock status and harvest strategies. ADF&G collects a substantial amount of data in the NSEI through multiple surveys, logbooks, and port sampling, and the SCAA framework allows us to better utilize these data while making management more resilient to potential budget cuts. Additionally, strong recruitment from the 2013–2022 year classes prompted questions about how to characterize and respond to recruitment uncertainty in State management. The SCAA model, coded in Template Model Builder (TMB; Kristensen et al. 2016), provides flexibility to explore recruitment dynamics and has been progressively refined to reduce reliance on fixed values derived from the federal assessment and to be more responsive to NSEI-specific data.

The annual schedule for stock assessments, survey, fishery, and data processing are as follows:

![alt text](https://github.com/commfish/seak_sablefish/blob/master/readme/assessment_timeline.jpg)

## Data

The SCAA model uses a combination of catch, longline survey and fishery CPUE, mark-recapture abundance estimates, length, and age data. Fisheries-independent data and inputs to the SCAA model are made available under [`data/`](https://github.com/commfish/seak_sablefish/tree/master/data) of this repository. Fisheries-dependent data are not made public to protect fishermen and processor confidentiality, but may be obtained through a formal data request to ADFG. Data are available for the following years:

![alt text](https://github.com/commfish/seak_sablefish/blob/master/readme/available_data.png)

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

**Description of R scripts:** Follow the order of these scripts to run the assessment...  
1.  [`r_helper/helper.r`](https://github.com/commfish/seak_sablefish/blob/master/r_helper/helper.R): Sourced by most other R scripts in this project, includes libraries/dependecies and ggplot themes;
2.  [`r_helper/functions.r`](https://github.com/commfish/seak_sablefish/blob/master/r_helper/functions.R):  Sourced by most other R scripts in this project, includes user-defined functions; 
3.  `2023/r/fishery_cpue_fr_OceanAK_ftx_lb_dat.R`: *Deprecated* Instructions on pulling fish ticket and logbook data from OceanAK, cleaning it and merging the two data sources for CPUE calculation for the 2023 assessment. *This has been replaced in 2024 by the next script listed;* 
4.  `YEAR/r/fishery_cpue_prep.R`: This replaces the previous script used in 2023. This script has instructions for getting the cpue data from the new OceanAK output and formatting it for calculating cpue indices. As of 2024 this is a WORK IN PROGRESS and the next assessment author should pay special attention to this script as well as the associated scipts for calculating and standarizing cpue fishery_ll_cpue.R and fishery_pot_cpue.R.
5.  `YEAR/r/0_querynclean_data.r`: *Deprecated* Descriptions of the data, `SQL` queries, and subsequent manipulations to clean raw data.  *This script is no longer used as the data is pulled from OceanAK database;*
6.  `YEAR/r/0_clean_data.R`: Descriptions of data and manipulations to clean raw data for analysis; 
7.  `YEAR/r/fishery_catch.R`: calculate and graphics for fishery catch data; 
8.  `YEAR/r/fishery_ll_cpue.R`: calculate and standardize longline fishery cpue;
9.  `YEAR/r/fishery_pot_cpue.R`: calculate and standardize pot fishery cpue; 
10.  `YEAR/r/llsurvey_cpue.r`: ADFG longline survey CPUE analysis and a preliminary steps towards a CPUE standardization;
11.  `YEAR/r/biological.r`: analysis of fishery and longline survey data, including modeling of growth, length-weight allometry, maturity, and sex ratios, as well as compilation of age and length compositions for stock assessment;
12.  `YEAR/r/mark_recapture.r`: clean release and recapture data, evaluate assumptions for mark-recapture experiment, and conduct analysis and model selection for the mark-recapture analysis; mark-recapture abundance estimated using the Bayesian software `JAGS`. *Note*: Only run in assessment years that marked fish;
13.  `YEAR/r/scaa_dataprep.r`: compilation of catch, indices of relative and absolute abundace, age and length comps, biological data, and fishery retention probabilities for use in the SCAA model; also includes conversion tables for age-length-weight, which appears in an appendix in the 2020 assessment;
14.  `YEAR/r/scaa.r`: run SCAA model, generate output, results, and figures for assessment; also includes prelim work to run the SCAA as a Bayesian model;
15.  `YEAR/r/tune_comps.r`: estimate effective samples sizes for age/length comps using McAllister and Ianelli (1997) with harmonic mean. After running scaa.r use this script to tune the model. implemented for assessment in 2023;
16.  `YEAR/r/retrospective.r`: retrospective analysis to evaluate performance of SCAA model;
17.  `YEAR/r/marking_survey_analysis.r`: sensitivity analysis of marking survey/abundance estimate on SCAA results; impact of moving to a bi- or triennial stock assessment; appeared in 2020 forecast, does not need to be rerun annually;
18.  `YEAR/r/ypr.r`: run YPR stock assessment by partitioning mark-recapture abundance estimate into sex and age classes, estimating F50 YPR model, and calculating ABC. *Not run since 2020*;
19.  `YEAR/r/ageing_error_matrix.r`: old code from Kray Van Kirk (previous biometrician) that may be useful when developing an updated ageing error matrix.
20.  `2024/r/survey_gear_experiments.r`: This code examines differences in the catch composition of longline and pot gear in experimental comparison performed in Clarence (SSEI) and Chatham (NSEI) straights in 2022 and 2023. The 2022 study compared slinky and conical pots during the Chatham marking survey. The 2023 studies in both locations compared slinky pots to longline gear in side by side sets. All data is stored in the 2024 year folder.

The `.cpp file` for the SCAA model is found in `tmb/` folder in each YEAR folder.

## Session Info

Last updated: March 2023
```
devtools::session_info()
─ Session info ─────────────────────────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.5.2 (2025-10-31 ucrt)
 os       Windows 11 x64 (build 26200)
 system   x86_64, mingw32
 ui       RStudio
 language (EN)
 collate  English_United States.utf8
 ctype    English_United States.utf8
 tz       America/Anchorage
 date     2026-03-31
 rstudio  2025.09.2+418 Cucumberleaf Sunflower (desktop)
 pandoc   NA

─ Packages ─────────────────────────────────────────────────────────────────────────────────────
 ! package           * version  date (UTC) lib source
   backports           1.5.0    2024-05-23 [1] CRAN (R 4.5.2)
   broom             * 1.0.11   2025-12-04 [1] CRAN (R 4.5.2)
   cachem              1.1.0    2024-05-16 [1] CRAN (R 4.5.2)
   captioner         * 2.2.3    2015-07-16 [1] CRAN (R 4.5.2)
   cli                 3.6.5    2025-04-23 [1] CRAN (R 4.5.2)
   cowplot           * 1.2.0    2025-07-07 [1] CRAN (R 4.5.2)
   data.table        * 1.18.2.1 2026-01-27 [1] CRAN (R 4.5.2)
   devtools            2.4.6    2025-10-03 [1] CRAN (R 4.5.2)
   digest              0.6.39   2025-11-19 [1] CRAN (R 4.5.2)
   dplyr             * 1.1.4    2023-11-17 [1] CRAN (R 4.5.2)
   ellipsis            0.3.2    2021-04-29 [1] CRAN (R 4.5.2)
   evaluate            1.0.5    2025-08-27 [1] CRAN (R 4.5.2)
   farver              2.1.2    2024-05-13 [1] CRAN (R 4.5.2)
   fastmap             1.2.0    2024-05-15 [1] CRAN (R 4.5.2)
   fontBitstreamVera   0.1.1    2017-02-01 [1] CRAN (R 4.5.2)
   fontLiberation      0.1.0    2016-10-15 [1] CRAN (R 4.5.2)
   fontquiver          0.2.1    2017-02-01 [1] CRAN (R 4.5.2)
   forcats           * 1.0.1    2025-09-25 [1] CRAN (R 4.5.2)
   fs                  1.6.7    2026-03-06 [1] CRAN (R 4.5.2)
   gdtools             0.4.4    2025-10-06 [1] CRAN (R 4.5.2)
   generics            0.1.4    2025-05-09 [1] CRAN (R 4.5.2)
   ggformula         * 1.0.0    2025-10-06 [1] CRAN (R 4.5.2)
   ggiraph             0.9.2    2025-10-07 [1] CRAN (R 4.5.2)
   ggplot2           * 4.0.2    2026-02-03 [1] CRAN (R 4.5.2)
   ggridges          * 0.5.7    2025-08-27 [1] CRAN (R 4.5.2)
   ggthemes          * 5.2.0    2025-11-30 [1] CRAN (R 4.5.2)
   glue                1.8.0    2024-09-30 [1] CRAN (R 4.5.2)
   gridExtra         * 2.3      2017-09-09 [1] CRAN (R 4.5.2)
   gtable              0.3.6    2024-10-25 [1] CRAN (R 4.5.2)
   haven               2.5.5    2025-05-30 [1] CRAN (R 4.5.2)
   hms                 1.1.4    2025-10-17 [1] CRAN (R 4.5.2)
   htmltools           0.5.9    2025-12-04 [1] CRAN (R 4.5.2)
   htmlwidgets         1.6.4    2023-12-06 [1] CRAN (R 4.5.2)
   knitr             * 1.51     2025-12-20 [1] CRAN (R 4.5.2)
   labelled            2.16.0   2025-10-22 [1] CRAN (R 4.5.2)
   lattice           * 0.22-7   2025-04-02 [2] CRAN (R 4.5.2)
   lifecycle           1.0.5    2026-01-08 [1] CRAN (R 4.5.2)
   lubridate         * 1.9.4    2024-12-08 [1] CRAN (R 4.5.2)
   magrittr            2.0.4    2025-09-12 [1] CRAN (R 4.5.2)
   MASS                7.3-65   2025-02-28 [2] CRAN (R 4.5.2)
   Matrix            * 1.7-4    2025-08-28 [2] CRAN (R 4.5.2)
   memoise             2.0.1    2021-11-26 [1] CRAN (R 4.5.2)
   mgcv              * 1.9-4    2025-11-07 [1] CRAN (R 4.5.2)
   mosaic            * 1.9.2    2025-07-30 [1] CRAN (R 4.5.2)
   mosaicCore          0.9.5    2025-07-30 [1] CRAN (R 4.5.2)
   mosaicData        * 0.20.4   2023-11-05 [1] CRAN (R 4.5.2)
   nlme              * 3.1-168  2025-03-31 [2] CRAN (R 4.5.2)
   otel                0.2.0    2025-08-29 [1] CRAN (R 4.5.2)
   padr              * 0.6.3    2024-11-21 [1] CRAN (R 4.5.2)
   pillar              1.11.1   2025-09-17 [1] CRAN (R 4.5.2)
   pkgbuild            1.4.8    2025-05-26 [1] CRAN (R 4.5.2)
   pkgconfig           2.0.3    2019-09-22 [1] CRAN (R 4.5.2)
   pkgload             1.5.0    2026-02-03 [1] CRAN (R 4.5.2)
   purrr             * 1.2.0    2025-11-04 [1] CRAN (R 4.5.2)
   R6                  2.6.1    2025-02-15 [1] CRAN (R 4.5.2)
   RColorBrewer        1.1-3    2022-04-03 [1] CRAN (R 4.5.2)
   Rcpp                1.1.0    2025-07-02 [1] CRAN (R 4.5.2)
   readr             * 2.1.6    2025-11-14 [1] CRAN (R 4.5.2)
   remotes             2.5.0    2024-03-17 [1] CRAN (R 4.5.2)
   rlang               1.1.6    2025-04-11 [1] CRAN (R 4.5.2)
   rstudioapi          0.17.1   2024-10-22 [1] CRAN (R 4.5.2)
   S7                  0.2.1    2025-11-14 [1] CRAN (R 4.5.2)
   scales              1.4.0    2025-04-24 [1] CRAN (R 4.5.2)
   sessioninfo         1.2.3    2025-02-05 [1] CRAN (R 4.5.2)
   stringi             1.8.7    2025-03-27 [1] CRAN (R 4.5.2)
   stringr           * 1.6.0    2025-11-04 [1] CRAN (R 4.5.2)
   systemfonts         1.3.1    2025-10-01 [1] CRAN (R 4.5.2)
   tibble            * 3.3.0    2025-06-08 [1] CRAN (R 4.5.2)
   tidyr             * 1.3.2    2025-12-19 [1] CRAN (R 4.5.2)
   tidyselect          1.2.1    2024-03-11 [1] CRAN (R 4.5.2)
   tidyverse         * 2.0.0    2023-02-22 [1] CRAN (R 4.5.2)
   timechange          0.3.0    2024-01-18 [1] CRAN (R 4.5.2)
 D TMB               * 1.9.19   2025-12-15 [1] CRAN (R 4.5.2)
   tzdb                0.5.0    2025-03-15 [1] CRAN (R 4.5.2)
   usethis             3.2.1    2025-09-06 [1] CRAN (R 4.5.2)
   vctrs               0.6.5    2023-12-01 [1] CRAN (R 4.5.2)
   withr               3.0.2    2024-10-28 [1] CRAN (R 4.5.2)
   xfun                0.55     2025-12-16 [1] CRAN (R 4.5.2)
   zoo               * 1.8-15   2025-12-15 [1] CRAN (R 4.5.2)

```
