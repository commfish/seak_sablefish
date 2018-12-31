# TO DO LIST FOR AGE-STRUCTURED MODEL IN TMB

## Data
[X] Move to a larger plus group (probably best to use 42+ to maintain consistency with the feds)

[X]	Use predicted values from a weight-based von Bertalanffy model instead of mean weight-at-age (especially important for older ages which are poorly represented in the data)

[ ]	**Work with groundfish biologists to assess the utility of the 1-hr soak time survey (1988-1996). If we decide to use this as an index obtain raw data**

[ ]	**Work with goundfish biologists to determine if we should account for a change in selectivity or catchability when the fishery changed from open access to limited entry or any other points since 1980**

[ ]	**Work with groundfish biologists to obtain the raw mark-recapture data from 2003 and 2004** 

[ ]	Use data to estimate variances and effective sample sizes for the various abundance indices and age compositions. Currently the sigmas for the abundance indices and effective sample sizes for the age comps are assumed constant values, which is likely unrealistic
     
     [X] Mark-recapture
     
     [ ] Fishery cpue
     
     [ ] Survey cpue
     
     [ ] Age comps
      
[ ]	Evaluate the current ageing error matrix and include it in the model


## Model

[ ] The model is not getting the scale correct 

[ ] Fs are tiny and biomasses enormous – moving to a prior on survey q may help resolve this (perhaps the q for the mark-recapture estimates, which one hopes are close to 1)

[ ] The age-2 recruits seem far too stable – that may be a consequence of the weighting or just the enormous last values. The standard errors are very large and that may reflect that the data are uninformative about scale.

[ ] Mark-recap q may be poorly estimated and the main source of information on scale. If so there should be a direct tradeoff between q, M (if estimated), and mean recruitment. The gradient structure will show the parameter correlation, as would posterior correlation in MCMC samples 

[ ] Weighting factors – Andre suspects that the weighting on the rec_devs is too high and on the F_devs too low – one wants the model to estimate annual recruitments and also fit the catches

[ ] Refine the implementation of phasing, starting values, and weighting for the various likelihood components.

[X] Consideration should be given to time-varying selectivity given there are multiple fisheries (that are aggregated)

[X] Test alternative models to determine if the same selectivity for the two surveys is appropriate 

[ ] Develop the TMB code to estimate biological reference points within the model

[ ] Include assumed discard mortality

[ ] Estimate recruitment and fishing mortality deviations as random effects 

[ ] Explore the benefits of a sex-structured model to deal with the fact that at-age females are larger than males

[ ] Conduct projections to evaluate stock status and future spawning stock size

[ ] Develop code to conduct a retrospective analysis

[ ] Develop priors and move towards a Bayesian implementation of this assessment

## Text and figures

[X] Put the report in R markdown

[X] Fix typos in the equations (e.g. not “i” on the RHS for the recruitment model)

[X] Do not use E for predicted CPUE because it can be confused with effort

[ ] Include the assumed standard errors on plots that show fitted values to aid in the evaluation of model fit

[X] Use standardized instead of raw residuals 

[X] Include zeros on the plots to help place the trends in context
