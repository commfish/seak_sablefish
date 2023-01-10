# Northern Southeast Inside Waters (NSEI) sablefish (*Anoplopoma fimbria*) stock assessment

Please direct any questions to: 
Phil Joy (philip.joy@alaska.gov, ummjane@gmail.com) or Rhea Ehresmann (rhea.ehresmann@alaska.gov)

Phil Joy took over this position and assessment in 2022 from Jane Sullivan, now at NOAA.
This fork will contain all assessments of NSEI sablefish from 2022 until otherwise stated.
Jane's orginal model and code has been left unaltered as she left it in 2021.  
The 2022 assessment changed little from the 2021 assessment as done by Jane.

Last updated: January 2023

## Reports
2022 forecast: http://www.adfg.alaska.gov/FedAidPDFs/RIR.1J.2022.19.pdf

2021 forecast: http://www.adfg.alaska.gov/FedAidPDFs/RIR.1J.2021.13.pdf

2020 forecast: http://www.adfg.alaska.gov/FedAidPDFs/RIR.5J.2020.05.pdf.

2019 forecast: http://www.adfg.alaska.gov/FedAidPDFs/RIR.5J.2019.03.pdf.

The 2017, 2018, and 2019 forecasts were developed as reproducible research projects in `RMarkdown` and found under `text/`(https://github.com/commfish/seak_sablefish/tree/master/text).

## Fishery development and history 

*History for NSEI and SSEI fisheries modified from a recent Board of Fish Report*

State managed fisheries currently occur in NSEI (Chatham Strait and Frederick Sound) and SSEI (Clarence Strait and adjacent waters of Dixon Entrance). Sablefish have been harvested in the internal waters of Southeast Alaska since the early 1900s. Prior to the 1940s, sablefish were primarily landed as bycatch in the halibut fishery (Bracken 1983). Halibut longline gear was modified in the late 1940s to target sablefish. Until the 1970s, harvest levels fluctuated widely due to low price and better opportunities in other fisheries. Pot gear was first introduced in 1970 in the Clarence Strait and Dixon Entrance areas and the pot fishery accounted for 33% of the total harvest in the early 1970s. In 1981, the NSEI fishery was restricted to longline gear only, but pot gear was still allowed in the SSEI Subdistrict.

Sablefish have been historically managed with limitations on fishing seasons and harvest levels. Season limitations were first imposed in 1945 for the NSEI management area and in 1982 for the SSEI management area (Bracken 1983). Fishing seasons continued to be shortened in both areas as effort escalated in the 1970s and 1980s. Guideline Harvest Regulations (GHR) based on historic catches were established for both areas in 1980. In 1985, a limited entry program was implemented for both the NSEI and SSEI sablefish fisheries. However, the number of vessels and overall operating efficiency of the longline fleet increased significantly after the limited entry program was implemented. In order to stay within GHRs, the department continued to reduce the number of fishing days in both areas. In the NSEI area, the number of fishing days fell from 76 days in 1980 to one day in 1987. One-day openings continued in the NSEI area through 1993. In 1993, the NSEI fleet harvested 3,640,000 dressed lb, which was 2,140,000 dressed lb over the upper bound of the GHR (1,500,000 dressed lb). In an effort to improve management, the board adopted an equal quota share (EQS) system for the NSEI area in 1994.

![alt text](https://github.com/commfish/seak_sablefish/blob/master/figures/readme/NSEI_map.jpg)
![alt text](https://github.com/commfish/seak_sablefish/blob/master/figures/readme/fishery_harvest.jpg)

## Stock assessment

The Alaska Department of Fish and Game (ADFG) conducts an annual mark-recapture pot survey in NSEI in May that has served as the basis for stock assessment and management since 2006 (Stahl and Holum 2010). Tags are recaptured in the ADFG longline survey in July and the longline fishery in August (Beder and Stahl 2016). A time-stratified Chapmanized Petersen model is used to estimate abundance in the Bayesian open source software `JAGS 4.3.0` (Chapman 1951, Sullivan and Williams 2018, Depaoli 2016). 

Prior to 2020 the abundance estimate was partitioned into age classes and biomass estimates using age composition and weight-at-age data collected during the longline survey and fishery. A yield-per-recruit (YPR) model was used to estimate F_50%, the fishing mortality rate that reduces female spawning stock biomass to 50% of unfished levels, using the `optim()` function in the statistical software `R` (R Core Team 2018). ADFG defines the maximum Acceptable Biological Catch (ABC) as F_maxABC = F_50% for the NSEI sablefish stock (Dressel 2009). 

Several factors motivated the development of a statistical catch-at-age (SCAA) model, which was implemented for management of the 2020 NSEI sablefish fishery. The current ADFG framework relies heavily on the mark-recapture experiment, which is vulnerable to future budget cuts. The mark-recapture estimate provides a single snapshot in time and therefore results in high inter-annual variability in abundance and biomass estimates. Consequently, we are unable to fully integrate the available data sources, explore historical trends, or adequately assess stock status or harvest strategies. ADFG collects a significant amount of data in the NSEI through multiple surveys, logbooks, and port sampling. Moving to a new modeling framework will allow us to better utilize these data and will make management more resilient to potential budget cuts. Finally, strong recruitment from the 2014 and 2016 year classes were reported in the Federal assessment, prompting questions about how to treat the uncertainty in recruitment for State management (Hanselman et al. 2017, Sullivan and Williams 2018). A statistical catch-at-age model coded in Template Model Builder (`TMB`) will allow more flexibility in exploring recruitment using random effects (Kristensen 2016 et al. 2016).

The annual schedule for stock assessments, survey, fishery, and data processing are as follows:

![alt text](https://github.com/commfish/seak_sablefish/blob/master/figures/readme/assessment_timeline.jpg)

## Data

The SCAA model uses a combination of catch, longline survey and fishery CPUE, mark-recapture abundance estimates, length, and age data. Fisheries-independent data and inputs to the SCAA model are made available under [`data/`](https://github.com/commfish/seak_sablefish/tree/master/data) of this repository. Fisheries-dependent data are not made public to protect fishermen and processor confidentiality, but may be obtained through a formal data request to ADFG. Data are available for the following years:

![alt text](https://github.com/commfish/seak_sablefish/blob/master/figures/readme/available_data.jpg)

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

The analyses underpinning the current stock assessment are found in [`r/`](https://github.com/commfish/seak_sablefish/tree/master/r) of this repository. In order to reproduce the assessment results, the scripts should be run in the following order:

![alt text](https://github.com/commfish/seak_sablefish/blob/master/figures/readme/steps_to_run_assessment.jpg)

**Description of R scripts:**
1.  [`r/helper.r`](https://github.com/commfish/seak_sablefish/blob/master/r/helper.R): Sourced by most other R scripts in this project, includes libraries/dependecies and ggplot themes;
2.  [`r/functions.r`](https://github.com/commfish/seak_sablefish/blob/master/r/functions.R):  Sourced by most other R scripts in this project, includes user-defined functions; 
3.  [`r/0_querynclean_data.r`](https://github.com/commfish/seak_sablefish/blob/master/r/0_querynclean_data.R): Descriptions of the data, `SQL` queries, and subsequent manipulations to clean raw data;
4.  [`llsurvey_cpue.r`](https://github.com/commfish/seak_sablefish/blob/master/r/llsurvey_cpue.R): ADFG longline survey CPUE analysis and a preliminary steps towards a CPUE standardization;
5.  [`fishery_catch_cpue.r`](https://github.com/commfish/seak_sablefish/blob/master/r/fishery_catch_cpue.R): summarize harvest (1985-present) and fishery CPUE and a preliminary CPUE standardization anaylsis using a generalized additive model;
6.  [`biological.r`](https://github.com/commfish/seak_sablefish/blob/master/r/biological.R): analysis of fishery and longline survey data, including modeling of growth, length-weight allometry, maturity, and sex ratios, as well as compilation of age and length compositions for stock assessment;
7.  [`mark_recapture.r`](https://github.com/commfish/seak_sablefish/blob/master/r/mark_recapture.R): clean release and recapture data, evaluate assumptions for mark-recapture experiment, and conduct analysis and model selection for the mark-recapture analysis; mark-recapture abundance estimated using the Bayesian software `JAGS`;
8.  [`scaa_dataprep.r`](https://github.com/commfish/seak_sablefish/blob/master/r/scaa_dataprep.R): compilation of catch, indices of relative and absolute abundace, age and length comps, biological data, and fishery retention probabilities for use in the SCAA model; also includes conversion tables for age-length-weight, which appears in an appendix in the 2020 assessment;
9.  [`scaa.r`](https://github.com/commfish/seak_sablefish/blob/master/r/scaa_dataprep.R): run SCAA model, generate output, results, and figures for assessment; also includes prelim work to run the SCAA as a Bayesian model;
10.  [`tune_comps.r`](https://github.com/commfish/seak_sablefish/blob/master/r/tune_comps.R): prelim work to estimate effective samples sizes for age/length comps using McAllister and Ianelli (1997) with harmonic mean; not currently implemented for assessment;
11.  [`retrospective.r`](https://github.com/commfish/seak_sablefish/blob/master/r/retrospective.R): retrospective analysis to evaluate performance of SCAA model;
12.  [`marking_survey_analysis.r`](https://github.com/commfish/seak_sablefish/blob/master/r/marking_survey_analysis.R): sensitivity analysis of marking survey/abundance estimate on SCAA results; impact of moving to a bi- or triennial stock assessment; appeared in 2020 forecast, does not need to be rerun annually;
13.  [`ypr.r`](https://github.com/commfish/seak_sablefish/blob/master/r/ypr.R): run YPR stock assessment by partitioning mark-recapture abundance estimate into sex and age classes, estimating F50 YPR model, and calculating ABC;
14.  [`ageing_error_matrix.r`](https://github.com/commfish/seak_sablefish/blob/master/r/ageing_error_matrix.R): old code from Kray Van Kirk (previous biometrician) that may be useful when developing an updated ageing error matrix.

The [`.cpp file`](https://github.com/commfish/seak_sablefish/blob/master/tmb/scaa_mod.cpp) for the SCAA model is found in `tmb/` folder.

## Session Info

Last updated: June 2020
```
- Session info ---------------------------------------------------------------------------------------------------------------------------------------------------
 setting  value                       
 version  R version 3.6.3 (2020-02-29)
 os       Windows >= 8 x64            
 system   x86_64, mingw32             
 ui       RStudio                     
 language (EN)                        
 collate  English_United States.1252  
 ctype    English_United States.1252  
 tz       America/Anchorage           
 date     2020-06-05                  

- Packages -------------------------------------------------------------------------------------------------------------------------------------------------------
 ! package     * version  date       lib source        
   assertthat    0.2.1    2019-03-21 [1] CRAN (R 3.6.3)
   backports     1.1.6    2020-04-05 [1] CRAN (R 3.6.3)
   broom       * 0.5.6    2020-04-20 [1] CRAN (R 3.6.3)
   callr         3.4.3    2020-03-28 [1] CRAN (R 3.6.3)
   captioner   * 2.2.3    2015-07-16 [1] CRAN (R 3.6.3)
   cellranger    1.1.0    2016-07-27 [1] CRAN (R 3.6.3)
   cli           2.0.2    2020-02-28 [1] CRAN (R 3.6.3)
   coda          0.19-3   2019-07-05 [1] CRAN (R 3.6.3)
   colorspace    1.4-1    2019-03-18 [1] CRAN (R 3.6.1)
   cowplot     * 1.0.0    2019-07-11 [1] CRAN (R 3.6.3)
   crayon        1.3.4    2017-09-16 [1] CRAN (R 3.6.3)
   crosstalk     1.1.0.1  2020-03-13 [1] CRAN (R 3.6.3)
   data.table  * 1.12.8   2019-12-09 [1] CRAN (R 3.6.3)
   DBI         * 1.1.0    2019-12-15 [1] CRAN (R 3.6.3)
   dbplyr        1.4.3    2020-04-19 [1] CRAN (R 3.6.3)
   desc          1.2.0    2018-05-01 [1] CRAN (R 3.6.3)
   devtools      2.3.0    2020-04-10 [1] CRAN (R 3.6.3)
   digest        0.6.25   2020-02-23 [1] CRAN (R 3.6.3)
   dplyr       * 0.8.5    2020-03-07 [1] CRAN (R 3.6.3)
   ellipsis      0.3.0    2019-09-20 [1] CRAN (R 3.6.3)
   fansi         0.4.1    2020-01-08 [1] CRAN (R 3.6.3)
   farver        2.0.3    2020-01-16 [1] CRAN (R 3.6.3)
   forcats     * 0.5.0    2020-03-01 [1] CRAN (R 3.6.3)
   fs            1.4.1    2020-04-04 [1] CRAN (R 3.6.3)
   generics      0.0.2    2018-11-29 [1] CRAN (R 3.6.3)
   ggdendro      0.1-20   2016-04-27 [1] CRAN (R 3.6.3)
   ggforce       0.3.1    2019-08-20 [1] CRAN (R 3.6.3)
   ggformula   * 0.9.4    2020-03-04 [1] CRAN (R 3.6.3)
   ggplot2     * 3.3.0    2020-03-05 [1] CRAN (R 3.6.3)
   ggrepel       0.8.2    2020-03-08 [1] CRAN (R 3.6.3)
   ggridges    * 0.5.2    2020-01-12 [1] CRAN (R 3.6.3)
   ggstance    * 0.3.4    2020-04-02 [1] CRAN (R 3.6.3)
   ggthemes    * 4.2.0    2019-05-13 [1] CRAN (R 3.6.3)
   glue          1.4.0    2020-04-03 [1] CRAN (R 3.6.3)
   gridExtra   * 2.3      2017-09-09 [1] CRAN (R 3.6.3)
   gtable        0.3.0    2019-03-25 [1] CRAN (R 3.6.3)
   haven         2.2.0    2019-11-08 [1] CRAN (R 3.6.3)
   hms           0.5.3    2020-01-08 [1] CRAN (R 3.6.3)
   htmltools     0.4.0    2019-10-04 [1] CRAN (R 3.6.3)
   htmlwidgets   1.5.1    2019-10-08 [1] CRAN (R 3.6.3)
   httr          1.4.1    2019-08-05 [1] CRAN (R 3.6.3)
   jsonlite      1.6.1    2020-02-02 [1] CRAN (R 3.6.3)
   knitr       * 1.28     2020-02-06 [1] CRAN (R 3.6.3)
   lattice     * 0.20-38  2018-11-04 [2] CRAN (R 3.6.3)
   lazyeval      0.2.2    2019-03-15 [1] CRAN (R 3.6.3)
   leaflet       2.0.3    2019-11-16 [1] CRAN (R 3.6.3)
   lifecycle     0.2.0    2020-03-06 [1] CRAN (R 3.6.3)
   lubridate   * 1.7.8    2020-04-06 [1] CRAN (R 3.6.3)
   magrittr      1.5      2014-11-22 [1] CRAN (R 3.6.3)
   MASS          7.3-51.5 2019-12-20 [2] CRAN (R 3.6.3)
   Matrix      * 1.2-18   2019-11-27 [2] CRAN (R 3.6.3)
   memoise       1.1.0    2017-04-21 [1] CRAN (R 3.6.3)
   mgcv        * 1.8-31   2019-11-09 [2] CRAN (R 3.6.3)
   modelr        0.1.7    2020-04-30 [1] CRAN (R 3.6.3)
   mosaic      * 1.7.0    2020-05-18 [1] CRAN (R 3.6.3)
   mosaicCore    0.6.0    2018-06-24 [1] CRAN (R 3.6.3)
   mosaicData  * 0.18.0   2020-05-15 [1] CRAN (R 3.6.3)
   munsell       0.5.0    2018-06-12 [1] CRAN (R 3.6.3)
   nlme        * 3.1-144  2020-02-06 [2] CRAN (R 3.6.3)
   padr        * 0.5.2    2020-05-12 [1] CRAN (R 3.6.3)
   pillar        1.4.3    2019-12-20 [1] CRAN (R 3.6.3)
   pkgbuild      1.0.7    2020-04-25 [1] CRAN (R 3.6.3)
   pkgconfig     2.0.3    2019-09-22 [1] CRAN (R 3.6.3)
   pkgload       1.0.2    2018-10-29 [1] CRAN (R 3.6.3)
   plyr          1.8.6    2020-03-03 [1] CRAN (R 3.6.3)
   polyclip      1.10-0   2019-03-14 [1] CRAN (R 3.6.0)
   prettyunits   1.1.1    2020-01-24 [1] CRAN (R 3.6.3)
   processx      3.4.2    2020-02-09 [1] CRAN (R 3.6.3)
   ps            1.3.2    2020-02-13 [1] CRAN (R 3.6.3)
   purrr       * 0.3.4    2020-04-17 [1] CRAN (R 3.6.3)
   R6            2.4.1    2019-11-12 [1] CRAN (R 3.6.3)
   Rcpp          1.0.4.6  2020-04-09 [1] CRAN (R 3.6.3)
   readr       * 1.3.1    2018-12-21 [1] CRAN (R 3.6.3)
   readxl        1.3.1    2019-03-13 [1] CRAN (R 3.6.3)
   remotes       2.1.1    2020-02-15 [1] CRAN (R 3.6.3)
   reprex        0.3.0    2019-05-16 [1] CRAN (R 3.6.3)
   rlang         0.4.5    2020-03-01 [1] CRAN (R 3.6.3)
   ROracle     * 1.3-1    2016-10-26 [1] CRAN (R 3.6.3)
   rprojroot     1.3-2    2018-01-03 [1] CRAN (R 3.6.3)
   rstudioapi    0.11     2020-02-07 [1] CRAN (R 3.6.3)
   rvest         0.3.5    2019-11-08 [1] CRAN (R 3.6.3)
   scales        1.1.0    2019-11-18 [1] CRAN (R 3.6.3)
   sessioninfo   1.1.1    2018-11-05 [1] CRAN (R 3.6.3)
   stringi       1.4.6    2020-02-17 [1] CRAN (R 3.6.2)
   stringr     * 1.4.0    2019-02-10 [1] CRAN (R 3.6.3)
   testthat      2.3.2    2020-03-02 [1] CRAN (R 3.6.3)
   tibble      * 3.0.1    2020-04-20 [1] CRAN (R 3.6.3)
   tidyr       * 1.0.2    2020-01-24 [1] CRAN (R 3.6.3)
   tidyselect    1.0.0    2020-01-27 [1] CRAN (R 3.6.3)
   tidyverse   * 1.3.0    2019-11-21 [1] CRAN (R 3.6.3)
 D TMB         * 1.7.16   2020-01-15 [1] CRAN (R 3.6.3)
   tweenr        1.0.1    2018-12-14 [1] CRAN (R 3.6.3)
   usethis       1.6.1    2020-04-29 [1] CRAN (R 3.6.3)
   vctrs         0.2.4    2020-03-10 [1] CRAN (R 3.6.3)
   withr         2.2.0    2020-04-20 [1] CRAN (R 3.6.3)
   xfun          0.13     2020-04-13 [1] CRAN (R 3.6.3)
   xml2          1.3.2    2020-04-23 [1] CRAN (R 3.6.3)
   yaml          2.2.1    2020-02-01 [1] CRAN (R 3.6.3)
   zoo         * 1.8-8    2020-05-02 [1] CRAN (R 3.6.3)

[1] C:/Users/jysullivan/Documents/R/win-library/3.6
[2] C:/Program Files/R/R-3.6.3/library
```
