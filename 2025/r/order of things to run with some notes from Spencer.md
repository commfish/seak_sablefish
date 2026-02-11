## Notes from Spencer regarding running the code

The analyses underpinning the current stock assessment are found in the folder labelled with the most recent year. Typically the assessment occurs between January and April and thus the most recent year will contain code and work that is in progress during that time frame. The helper and function files are contained in their own folder [`r_helper/`](https://github.com/commfish/seak_sablefish/tree/master/r_helper). The rest of the scripts necessary to run the assessment are contained in the appropriate year folder, as well as the legacy folder, and are described below. The scripts should be run in the order listed.

![alt text](https://github.com/commfish/seak_sablefish/blob/master/readme/steps_to_run_assessment.jpg)

**Description of R scripts:** Follow the order of these scripts to run the assessment...\
1. [`r_helper/helper.r`](https://github.com/commfish/seak_sablefish/blob/master/r_helper/helper.R): Sourced by most other R scripts in this project, includes libraries/dependecies and ggplot themes;

2.  [`r_helper/functions.r`](https://github.com/commfish/seak_sablefish/blob/master/r_helper/functions.R): Sourced by most other R scripts in this project, includes user-defined functions;

3.  `2023/r/fishery_cpue_fr_OceanAK_ftx_lb_dat.R`: *Deprecated* Instructions on pulling fish ticket and logbook data from OceanAK, cleaning it and merging the two data sources for CPUE calculation for the 2023 assessment. *This has been replaced in 2024 by the next script listed;*

4.  `YEAR/r/fishery_cpue_prep.R`: This replaces the previous script used in 2023. This script has instructions for getting the cpue data from the new OceanAK output and formatting it for calculating cpue indices. As of 2024 this is a WORK IN PROGRESS and the next assessment author should pay special attention to this script as well as the associated scipts for calculating and standarizing cpue fishery_ll_cpue.R and fishery_pot_cpue.R.

5.  `YEAR/r/0_querynclean_data.r`: *Deprecated* Descriptions of the data, `SQL` queries, and subsequent manipulations to clean raw data. *This script is no longer used as the data is pulled from OceanAK database;*

6.  `YEAR/r/0_clean_data.R`: Descriptions of data and manipulations to clean raw data for analysis;

Make sure the downloaded files are saved in the indicated format; otherwise, the dates will be incorrectly formatted and things will be weird.

7.  `YEAR/r/fishery_catch.R`: calculate and graphics for fishery catch data;

8.  `YEAR/r/fishery_ll_cpue.R`: calculate and standardize longline fishery cpue;

9.  `YEAR/r/fishery_pot_cpue.R`: calculate and standardize pot fishery cpue;

10. `YEAR/r/llsurvey_cpue.r`: ADFG longline survey CPUE analysis and a preliminary steps towards a CPUE standardization;

Most up-to-date file is "updated_2025_AGR goes on a tangent"

11. `YEAR/r/biological.r`: analysis of fishery and longline survey data, including modeling of growth, length-weight allometry, maturity, and sex ratios, as well as compilation of age and length compositions for stock assessment;

12. `YEAR/r/mark_recapture.r`: clean release and recapture data, evaluate assumptions for mark-recapture experiment, and conduct analysis and model selection for the mark-recapture analysis; mark-recapture abundance estimated using the Bayesian software `JAGS`;

13. `YEAR/r/scaa_dataprep.r`: compilation of catch, indices of relative and absolute abundace, age and length comps, biological data, and fishery retention probabilities for use in the SCAA model; also includes conversion tables for age-length-weight, which appears in an appendix in the 2020 assessment;

14. `YEAR/r/scaa.r`: run SCAA model, generate output, results, and figures for assessment; also includes prelim work to run the SCAA as a Bayesian model;

Run this all the way through using 'TUNED_VER\<-NA' and 'VER \<- "v23_3f_3s_2016"' (if using the same version as was used in the 2025 assessment). Then run tune_comps.R. THEN RERUN SCAA.R with the following changes: 'TUNED_VER \<- "v23_3f_3s_2016"' and 'VER \<- "v23_3f_3s_2016_TUNED"'. There is a way to avoid running the full script twice if you want to figure that out.

The final output file that was used in the 2025 assessment was 'scaa_brps_2024_v23_3f_3s_2016_TUNED'

15. `YEAR/r/tune_comps.r`: estimate effective samples sizes for age/length comps using McAllister and Ianelli (1997) with harmonic mean. After running scaa.r use this script to tune the model. implemented for assessment in 2023;

16. `YEAR/r/retrospective.r`: retrospective analysis to evaluate performance of SCAA model;

17. `YEAR/r/marking_survey_analysis.r`: sensitivity analysis of marking survey/abundance estimate on SCAA results; impact of moving to a bi- or triennial stock assessment; appeared in 2020 forecast, does not need to be rerun annually;

18. `YEAR/r/ypr.r`: run YPR stock assessment by partitioning mark-recapture abundance estimate into sex and age classes, estimating F50 YPR model, and calculating ABC. *Not run since 2020*;

19. `YEAR/r/ageing_error_matrix.r`: old code from Kray Van Kirk (previous biometrician) that may be useful when developing an updated ageing error matrix.

20. `2024/r/survey_gear_experiments.r`: This code examines differences in the catch composition of longline and pot gear in experimental comparison performed in Clarence (SSEI) and Chatham (NSEI) straights in 2022 and 2023. The 2022 study compared slinky and conical pots during the Chatham marking survey. The 2023 studies in both locations compared slinky pots to longline gear in side by side sets. All data is stored in the 2024 year folder.

The `.cpp file` for the SCAA model is found in `tmb/` folder in each YEAR folder.

When it comes time for the industry meeting, additional necessary figures can be generated using the code in 'indmtg_2025.R'.
