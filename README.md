# Northern Southeast Inside Waters (NSEI) sablefish (*Anoplopoma fimbria*) stock assessment

Please direct any questions to: 
Jane Sullivan (jane.sullivan1@alaska.gov), Ben Williams (ben.williams@alaska.gov), or Andrew Olson (andrew.olson@alaska.gov)

## Fishery development and history 

*History for NSEI and SSEI fisheries modified from a recent Board of Fish Report*

State managed fisheries currently occur in NSEI (Chatham Strait and Frederick Sound) and SSEI (Clarence Strait and adjacent waters of Dixon Entrance). Sablefish have been harvested in the internal waters of Southeast Alaska since the early 1900s. Prior to the 1940s, sablefish were primarily landed as bycatch in the halibut fishery (Bracken 1983). Halibut longline gear was modified in the late 1940s to target sablefish. Until the 1970s, harvest levels fluctuated widely due to low price and better opportunities in other fisheries. Pot gear was first introduced in 1970 in the Clarence Strait and Dixon Entrance areas and the pot fishery accounted for 33% of the total harvest in the early 1970s. In 1981, the NSEI fishery was restricted to longline gear only, but pot gear was still allowed in the SSEI Subdistrict.

Sablefish have been historically managed with limitations on fishing seasons and harvest levels. Season limitations were first imposed in 1945 for the NSEI management area and in 1982 for the SSEI management area (Bracken 1983). Fishing seasons continued to be shortened in both areas as effort escalated in the 1970s and 1980s. Guideline Harvest Regulations (GHR) based on historic catches were established for both areas in 1980. In 1985, a limited entry program was implemented for both the NSEI and SSEI sablefish fisheries. However, the number of vessels and overall operating efficiency of the longline fleet increased significantly after the limited entry program was implemented. In order to stay within GHRs, the department continued to reduce the number of fishing days in both areas. In the NSEI area, the number of fishing days fell from 76 days in 1980 to one day in 1987. One-day openings continued in the NSEI area through 1993. In 1993, the NSEI fleet harvested 3,640,000 dressed lb, which was 2,140,000 dressed lb over the upper bound of the GHR (1,500,000 dressed lb). In an effort to improve management, the board adopted an equal quota share (EQS) system for the NSEI area in 1994.

![alt text](https://github.com/commfish/seak_sablefish/blob/master/figures/readme/NSEI_map.jpg)
![alt text](https://github.com/commfish/seak_sablefish/blob/master/figures/readme/fishery_harvest.jpg)

## Stock assessment

Currently the Alaska Department of Fish and Game (ADF&G) conducts an annual mark-recapture pot survey in May that serves as the basis for stock assessment and managment (Stahl and Holum 2010). Tags are recaptured in the ADF&G longline survey in July and the longline fishery in August (Beder and Stahl 2016). A time-stratified Chapmanized Petersen model is used to estimate abundance in the Bayesian open source software `JAGS 4.3.0` (Chapman 1951, Sullivan and Williams 2018, Depaoli 2016). The abundance estimate is then partitioned into age classes and biomass estimates using age composition and weight-at-age data collected during the longline survey and fishery. A yield-per-recruit model is used to estimate <a href="https://www.codecogs.com/eqnedit.php?latex=$F_{50\%}$" target="_blank"><img src="https://latex.codecogs.com/gif.latex?$F_{50\%}$" title="$F_{50\%}$" /></a> using the `optim()` function in the statistical software `R` (R Core Team 2018). ADF&G has defined Acceptable Biological Catch (ABC) as <a href="https://www.codecogs.com/eqnedit.php?latex=$F_{ABC}$=$F_{50\%}$" target="_blank"><img src="https://latex.codecogs.com/gif.latex?$F_{ABC}$=$F_{50\%}$" title="$F_{ABC}$=$F_{50\%}$" /></a> for the NSEI sablefish stock (Dressel 2009). 

Several factors motivated the development of a new statistical catch-at-age model. The current ADF&G framework relies heavily on the mark-recapture experiment, which may be vulnerable to future budget cuts. Further the mark-recapture estimate provides a single snapshot in time and therefore results in high inter-annual variability in abundance and biomass estimates. Consequently, we are unable to fully integrate the available data sources, explore historical trends, or adequately assess stock status or harvest strategies. ADF&G collects a significant amount of data in the NSEI through multiple surveys, logbooks, and port sampling. Moving to a new modeling framework will allow us to better utilize these data and will make management more resilient to potential budget cuts. In addition, the current assessment relies on Federal estimates of selectivity and does not estimate recruitment for the stock. If there are differences in availability, gear selectivity, or stock dynamics in NSEI, we are unable to detect them. Finally, strong recruitment from the 2014 and possibly 2013 and 2015 year classes were reported in the Federal assessment, prompting questions about how to treat the uncertainty in recruitment for State management (Hanselman et al. 2017, Sullivan and Williams 2018). A statistical catch-at-age model coded in Template Model Builder (`TMB`) will allow more flexibility in exploring recruitment using random effects (Kasper et al. 2016).

## Data

Fisheries-independent data and inputs to the `TMB` model are made available under [`data/`](https://github.com/commfish/seak_sablefish/tree/master/data) of this repository. These include biological data and indices of effort. Fisheries-dependent data are not made public to protect fishermen and processor confidentiality, but may be obtained through a formal data request to ADF&G. All descriptions of the data, `SQL` queries, and subsequent manipulations to the data are found in [`r/0_querynclean_data.r`](https://github.com/commfish/seak_sablefish/blob/master/r/0_querynclean_data.R).

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

1.  [`fishery_catch_cpue.r`](https://github.com/commfish/seak_sablefish/blob/master/r/fishery_catch_cpue.R): summarize harvest and fishery CPUE and a preliminary CPUE standardization anaylsis using a generalized additive model;
2.  [`llsurvey_cpue.r`](https://github.com/commfish/seak_sablefish/blob/master/r/llsurvey_cpue.R): standardization and summary of ADF&G longline survey in NSEI;
3.  [`biological.r`](https://github.com/commfish/seak_sablefish/blob/master/r/biological.R): analysis of fishery, longline, and pot survey data, including growth and maturity modeling, age and length compositions, and preliminary code for age-length keys;
4.  [`mark_recapture.r`](https://github.com/commfish/seak_sablefish/blob/master/r/mark_recapture.R): summary of tag data and model selection for the mark-recapture analysis, which was conducted using the Bayesian software `JAGS`;
5.  [`ypr.r`](https://github.com/commfish/seak_sablefish/blob/master/r/ypr.R): yield-per-recruit analysis and ABC calculation.

Data preparation and visualization of model inputs to the preliminary `TMB` statistical catch-at-age model are found in [`tmb_dataprep.r`](https://github.com/commfish/seak_sablefish/blob/master/r/tmb_datprep.R).  The [`.cpp file`](https://github.com/commfish/seak_sablefish/blob/master/tmb/mod.cpp) and [`R` script](https://github.com/commfish/seak_sablefish/blob/master/tmb/run_mod.R) to run the model are found in the `tmb/` folder.

## Reports

The 2018 and 2019 NSEI stock assessments and preliminary statistical catch-at-age model were developed as reproducible research projects in `RMarkdown` and found under `text/`(https://github.com/commfish/seak_sablefish/tree/master/text).

The 2019 assessment is also available as a Regional Information Report (http://www.adfg.alaska.gov/FedAidPDFs/RIR.5J.2019.03.pdf)

## Session Info

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
