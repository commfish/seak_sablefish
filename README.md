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

Fisheries-independent data and inputs to the `TMB` model are made available under [`data/`](../tree/master/data) of this repository. These include biological data and indices of effort. Fisheries-dependent data are not made public to protect fishermen and processor confidentiality, but may be obtained through a formal data request to ADF&G. All `SQL` queries used to obtain data are found in [`r/0_querynclean_data.r`](../blob/master/r/0_querynclean_data.R).

The following product recovery rates for sablefish were used:

Ice and slime assumed to be 2% of total weight.

| Delivery code | Description                    | Rate |
|---------------|--------------------------------|-----:|
| 1             | Whole/round                    |    1 |
| 4             | Gutted, head on                | 0.89 |
| 7             | Headed and gutted, Western cut | 0.68 |
| 9 (08 in IFDB)| Headed and gutted, Eastern cut | 0.63 |

