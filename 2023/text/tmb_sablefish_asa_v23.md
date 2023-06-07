---
title: "Northern Southeast Inside Subdistrict Sablefish (*Anoplopoma fimbria*) Management Plan and Stock Assessment for 2023"
date: "June 07, 2023"
author: 
 - Phil Joy^[Alaska Department of Fish and Game, Commercial Fisheries Division, Juneau, Alaska]
 - Rhea Ehresmann^[Alaska Department of Fish and Game, Commercial Fisheries Division, Sitka, Alaska]
  
output:
 # bookdown::pdf_document2:
 # bookdown::word_document2:
 # bookdown::html_document2:
  word_document: 
 # html_document:
    fig_caption: yes
    toc: yes
    number_sections: false
 #   theme: cerulean
#    highlight: tango
#   reference_docx: word-styles-reference.docx
#  reference_docx: RIR.1J.2023.XX_NSEI DRAFT.docx
    keep_md: true
#    df_print: kable
#    documentclass:
#  radix::radix_article: default

header-includes: 
 \usepackage{float}
 \floatplacement{figure}{H}
 \usepackage{fontspec}
 \setmainfont{Times New Roman}
always_allow_html: yes
mainfont: "Times New Roman"

---
<style type="text/css">
  body{
    font-family: Times New Roman;
  font-size: 12pt;
}

.math{
font-family: Times New Roman;
  font-size: small;
}

figcaption {
  text-align: left;
}

figure {
  text-align: left;
}
</style>
---





# ABSTRACT 

This report provides an overview of the stock assessment, harvest strategy, and regulations effective for the 2023 Northern Southeast Inside (NSEI) sablefish (*Anoplopoma fimbria*) commercial fishery. The NSEI sablefish commercial fishery is scheduled to open August 15 and close November 15 and open to both longline and pot gear. The 2023 NSEI sablefish commercial fishery annual harvest objective is 1,233,633 round pounds and is based on decrements from an acceptable biological catch of 1,573,109 round pounds. The annual harvest objective is allocated to 73 limited entry Commercial Fisheries Entry Commission C61A permits through an equal quota share (EQS) system, resulting in a 2023 EQS of 16,899 round pounds for each permit holder.

Key words: 	sablefish, black cod, *Anoplopoma fimbria*, stock assessment, annual harvest objective, AHO, catch per unit effort, CPUE, Northern Southeast, Chatham Strait, NSEI, mark–recapture, tagging

<br>

#  INTRODUCTION

The Alaska Department of Fish and Game (ADF&G) evaluates stock status and establishes the Northern Southeast Inside (NSEI) acceptable biological catch (ABC) and subsequent annual harvest objective (AHO). The NSEI Subdistrict management area (Figure \@ref(fig:nseimap)) consists of all waters as defined in 5 AAC 28.105(a)(2).  

The recommended 2023 allowable biological catch (ABC) is 1,573,109 round pounds ($F_{ABC}$ = 0.063), a 9% increase from the 2022 ABC (Table \@ref(tab:catchtab) and \@ref(tab:brps)). After making decrements for sablefish mortalities in other fisheries, **the 2023 NSEI Subdistrict commercial sablefish fishery annual harvest objective (AHO) is <span style="color: red;">1,395,868</span> round pounds** (Table \@ref(tab:catchtab) and \@ref(tab:decrements)). There are 73 valid Commercial Fisheries Entry Commission (CFEC) permits for 2023, which is the same number as 2022. **The individual equal quota share (EQS) is <span style="color: red;">19,121</span> round pounds**, a <span style="color: red;">13%</span> increase from the 2022 EQS of 16,899 round pounds (Table \@ref(tab:catchtab)). 

Several advancements to the stock assessment and statistical catch-at-age (SCAA) model were implemented for the 2023 NSEI sablefish assessment that improved the model’s ability to capture the dynamics of the stock.  These advancements are:

1.	Fishery CPUE was fully standardized to correct for variability in fishing methods and practices (i.e, hook size, fishing depth, length of sets, location, etc.) to better detect underlying trends that reflect the abundance of fish available to the fishery.  This involved recalculating fishery CPUE from the re-entered logbook data that was completed in 2020. 
2.	Fishery selectivity in the SCAA was updated to the fixed values estimated in the federal sablefish fishery assessment (Goethel et al. 2022).  Selectivity in the time period prior to implementation of the IFQ fishery in 1995 changed significantly from the last assessment such that the curve is substantially less steep indicating that fewer, smaller fish were being selected for in the pre-IFQ fishery than previously estimated.  This has the effect of increasing the apparent size of the population during that time period and thus reducing stock status today.   The model is now capable of estimating fishery selectivity within the model, but the estimated selectivity curves are suspect and further work needs to be done before this version of the model is implemented. 
3.	Survey selectivity was switched from the fixed values borrowed from the federal domestic longline survey values to being freely estimated in the SCAA model, thus being a more accurate reflection of the NSEI longline survey.  This involved adding a second time block to reflect the switch from an unstandardized survey prior to 2000 and the fully standardized fishery that began in 2000. 
4.	The recruitment process is now modelled using random effects which allows for the estimation of variability, $\sigma_R$.  Prior to this assessment, $\sigma_R$ had been fixed at the assumed federal assessment value of 1.2.  
5.	The data weighting of the model was changed to reflect best practices in SCAA modelling.  This involved tuning the age and length compositional data to adjust the effective sample sizes using McAllister and Ianelli (1997) and removing the fixed weights that had been applied to the abundance indices (mark-recapture estimates, longline survey CPUE and longline fishery CPUE).  The variance of the longline survey was changed from assumed values to the true estimates.  The fishery CPUE and mark-recapture variance was kept at the inflated and fixed values to allow for the extra uncertainty in these indices owing to the unrecorded releases of fish that are permitted in the fishery and unquantified biases in the mark-recapture project.  

With these changes, the recommended 2023 ABC is 1,573,109 round lb ($F_{ABC}$ = 0.063), a 9% increase from the 2022 ABC.  The ABC was calculated as an average of the base model and the new model (v23) to balance the clear increase in biomass with the uncertainty about stock status evident in comparing the base and v23 models.  The increase in the ABC is attributed to the continued growth and maturation of the strong recruitment events since 2015, highlighted by recruitment in 2018 (the 2016 year class) which is the highest recruitment since 1979.   The dominant 2016 year class is now 50% mature and will comprise 27% of the biomass.  All three abundance estimates are elevated from recent years with the highest abundance estimate on record from the mark-recapture project, the third sustained year of high CPUE in the longline survey and increasing CPUE in the longline fishery.  However, the lower stock status estimated in the new model results in less of an increase than was present in using the 2022 model (the base model).  The recommended ABC is thus an average of the recommended ABC from the base model used in past assessments and the new model v23 used in this assessment.  Fishery catch and ex-vessel value remain depressed from historical levels, but have increased since 2022 as the 2013-2018 year classes reach marketable sizes and are being landed and retained in the fishery (Figure \@ref(fig:catch)). Though recent high catch rates of small sablefish across multiple geographic areas signal increasing trends for sablefish stocks (Goethel et al. 2022), the department maintains a precautionary approach to setting harvest limits. Estimates from the 2022 stock assessment suggest sablefish spawning stock biomass remains at suppressed levels compared to the 1980s and 1990s. 

The ABC determination process uses a statistical catch-at-age model, which was first implemented in 2020. The model reduces the reliance on the annual mark-recapture project to estimate recruitment, abundance, and spawning stock biomass of NSEI sablefish by integrating multiple indices of abundance and biological data (e.g., catch, mark-recapture abundance estimates, longline survey and fishery CPUE, longline survey length and age compositions). As in previous years, maximum ABC is defined by $F_{50}$, the fishing mortality rate that reduces spawning biomass to 50% of equilibrium unfished levels.

The process leading to the determination of the ABC, AHO, and EQS includes compiling fishery and survey data, running the stock assessment, and accounting for additional sources of mortality through decrements. Although the ABC is determined prior to the AHO and EQS, this report is organized to make management-related information accessible to stakeholders and improve documentation of the assessment process by organizing this report into the following sections:

1.	2023 Sablefish Management Plan: details the decrements process leading to the AHO and EQS and effective regulations for the 2023 NSEI fishery.
2.	2022 Sablefish Stock Assessment and 2023 ABC Determination: highlights stock assessment data inputs, methods, results, and subsequent analyses that informed the recommended ABC. 

<br>

# 2023 SABLEFISH MANAGEMENT PLAN

## ANNUAL HARVEST OBJECTIVE DETERMINATION

The 2023 AHO was determined by making the following decrements from the recommended ABC (1,573,109 round pounds, Tables 2 and 3):

  * estimated sablefish bycatch mortality in the commercial Pacific halibut fishery,
  * ADF&G longline survey removals,
  * sport fishery guided and unguided harvest,
  * mortality from fishery deadloss, and 
  * subsistence and personal use harvest.
  

### Bycatch mortality in the halibut fishery

Sablefish caught in NSEI during the Pacific halibut individual fishing quota fishery prior to the sablefish fishery season opening (August 15) must be released; however, because not all are expected to survive, bycatch mortality is estimated. Prior to 2003, a 50% bycatch morality rate was applied as bycatch sablefish were permitted to be retained as bait. In 2003, the Alaska Board of Fisheries disallowed retaining bycatch sablefish for bait, and a 25% bycatch mortality rate was assumed for all sablefish caught and released due to the larger hook size in the Pacific halibut fishery. Released sablefish bycatch is calculated as the product of the 3-year average of the sablefish to Pacific halibut ratio from the International Pacific Halibut Commission (IPHC) annual survey and the 3-year average of the Pacific halibut catch in areas greater than 99 fathoms in NSEI.

### ADF&G longline survey removals

In 2022, no NSEI permit holders will participate in the NSEI longline survey due to budgetary instability and deficit given the low prices of sablefish in 2020 and 2021 (Tables 3 and 4). The survey removal decrement was determined by averaging the survey total harvest from the previous 3 years. Permit holders will likely resume survey participation in 2023. 

### Sport fish harvest (guided and unguided)

Sablefish sport fish preliminary harvest and release mortality from the guided and unguided sectors are estimated utilizing charter logbooks and the statewide harvest survey (Romberg et al. 2017). Estimates of harvested and released fish are based on the total number of fish and converted to weight using a 3-year average of fish sampled from the guided and unguided sectors. A 10% release mortality rate is applied to the sport fishery; this was based on the 11.7% estimated in Stachura et al. (2012) and modified to account for difference in gear type (rod and reel versus longline) and handling time. 

### Mortality from fishery deadloss

Deadloss mortality in the directed sablefish fishery was estimated by applying the percentage of dead sablefish (i.e., recorded as predated by sand fleas, sharks, hooking injury, or other cause of mortality) caught on the NSEI longline survey using the recent 3-year average, 0.85% (2019–2021), to the NSEI sablefish commercial AHO. 

### Personal use and subsistence harvest

A total of 772 personal use and subsistence sablefish permits were issued in 2021. Annual subsistence and personal use harvest of sablefish is estimated from these permits by adding the total number of retained sablefish reported to the proportion of released sablefish reported after applying a 16% discard mortality rate to released sablefish (Gilroy and Stewart 2013). The Pacific halibut fishery is assumed a reasonable proxy for sablefish because the fisheries utilize similar gear and frequently the same vessels and crew participate in both fisheries. Moreover, both species are considered hardy and do not experience barotrauma. The 2021 longline survey average weight (5.1 lb) was applied to this harvest to obtain a decrement total. 
In 2015, personal use harvest was limited to an annual limit of 50 fish per household. Since 2018, participants of the personal use fishery have been allowed to use pot gear with no more than 2 pots per permit and a maximum of 8 pots per vessel when 4 or more permit holders are on board the same vessel. Use of pot gear has continued to increase with 68% of permit holders fishing pots. 

<br>

## REGULATIONS

### 2022 Board of Fisheries Decisions

In March 2022, the Alaska Board of Fisheries adopted new regulations that will be enacted prior to or during the fishing season for the NSEI sablefish commercial fishery. An advisory announcement will be issued at a later date with more information. These new regulations include:

  * Full retention requirements and landing requirements using hook-and-line and pot gear for all species of rockfish including thornyhead rockfish.
  * Allowing pot gear as a legal gear type in addition to longline gear for the C61A permits, which is contingent upon the approval process through CFEC. 
  * Pots must have at least two circular escape rings, with a minimum inside diameter of three and three-fourths inches, installed on opposing vertical or sloping walls of the pot
  
### Registration and logbook requirements

Fishermen must register prior to fishing [5 AAC 28.106 (b)] and keep a logbook during the fishery. Completed logbook pages must be attached to the ADF&G copy of the fish ticket at the time of delivery. Confidential envelopes for logbook pages may be requested when registering. 

Permit holders will receive a personal quota share (PQS) tracking form at the time of registration. This form is used to record the total round weight landed for each delivery. Each permit holder must, upon request, provide the buyer with the total round weight of sablefish the permit holder has landed to date. The department requests that a copy of the completed PQS tracking form is included with the final fish ticket of the season for that permit.

Logbooks must include, by set, the date and time gear is set and retrieved, specific location of harvest by latitude and longitude for start and ending positions, hook spacing, amount of gear (number of hooks and skates) used, depth of set, estimated number or weight of the target species, and the estimated number or weight of bycatch by species. Permit holders must indicate for each set if the target species was sablefish or Pacific halibut and if there was any gear lost. A permit holder must retain all visibly injured or dead sablefish. Sablefish that are not visibly injured or dead may be released unharmed, and the permit holder must record in the logbook, by set, the number of live sablefish released [5 AAC 28.170(f)]. Permit holders must record release reason (e.g., fish are small) and whether their personal quota share has been met. 

### Tagged sablefish

Fishermen are requested to watch for tagged sablefish, record tag number(s), and attach tags directly in the logbook with the corresponding set information. All tags returned will receive a reward. Tag rewards include a t-shirt and entry into an annual drawing for one \$1,000, two \$500, and four $250 cash rewards. To qualify for entry in the annual drawing, ADF&G requires the following information: the tag, set location (latitude and longitude), date of capture of the fish, and the name and address of the person recovering the tag.

### Sablefish possession and landing requirements

In the NSEI Subdistrict, the holder of a CFEC permit for sablefish may not retain more sablefish from the directed fishery than the annual amount of sablefish EQS specified by the department [5 AAC 28.170 (f)]. However, if a permit holder’s harvest exceeds the EQS for that year, by not more than 5%, ADF&G shall reduce the permit holder's EQS for the following year by the amount of the overage. If a permit holder's harvest exceeds the permit holder's EQS by more than 5%, the proceeds from the sale of the overage in excess of 5% shall be surrendered to the state and the permit holder may be prosecuted under AS 16.05.723 [5 AAC 28.170 (j)]. If a permit holder’s harvest is less than the permit holder’s EQS established for the year, ADF&G shall increase the permit holder’s PQS only for the following year by the amount of the underage that does not exceed 5% of the EQS [5 AAC 28.170 (k)]. For the 2022 fishing season, 5% of the annual EQS is 845 round pounds.

### Fish tocket requirements

Landed weights must be recorded on a fish ticket at the time of delivery. If a fisherman delivers fish in the round, the total round weight delivered must be recorded on the fish ticket. If a fisherman delivers dressed fish, the fish ticket must include the total landed dressed weight as well as the round weight equivalent, determined by using the standard 0.63 recovery rate. There is a 2% allowance for ice and slime when unrinsed whole iced sablefish are weighed. A fish ticket must be completed prior to the resumption of fishing and each permit holder must retain, on board their vessel, copies of all NSEI sablefish tickets from the current season and their updated PQS tracking form. When delivering fish out of state, a completed fish ticket must be submitted to ADF&G prior to transporting fish out of Alaska.

### Bycatch allowances for other species

Full retention and reporting of rockfish *Sebastes*, excluding thornyhead rockfish *Sebastolobus*, is required for internal waters (5 AAC 28.171). The full retention regulation does not apply to thornyhead rockfish at the time of publication but will when the new regulations become effective in 2022. The allowable bycatch that may be legally landed and sold on an NSEI sablefish permit is based on round weight of sablefish and bycatch species or species group on board the vessel:

  * All rockfish, including thornyheads: 15% in aggregate, of which 1% may be demersal shelf rockfish (DSR), which includes yelloweye, quillback, canary, tiger, copper, China, and rosethorn rockfish
  * Lingcod: 0%
  * Pacific cod: 20%
  * Spiny dogfish: 35%
  * Other groundfish: 20%
  
All rockfish retained in excess of allowable bycatch limits shall be reported as bycatch overage on an ADF&G fish ticket. All proceeds from the sale of excess rockfish bycatch shall be surrendered to the state. Excess rockfish retained due to full retention requirements may be retained for personal use; however, the pounds must be documented as overage on the fish ticket. 

A CFEC permit holder fishing for groundfish must retain all Pacific cod when the directed fishery for Pacific cod is open and up to the maximum retainable bycatch amount (20%) of Pacific cod when a directed fishery for Pacific cod is closed [5 AAC 28.070 (e)]. Pacific cod taken in excess of the bycatch limit in areas open to directed fishing for Pacific cod may be landed on a CFEC miscellaneous saltwater finfish permit designated for the gear that was used. Fishermen with halibut Individual Fishing Quota (IFQ) in regulatory area 2C and a CFEC halibut permit card must retain all halibut over 32 inches in length, up to the amount of their IFQ.

### Sablefish  live market

The holder of a CFEC or interim use permit for sablefish may possess live sablefish for delivery as live product except that, upon request of a local representative of the department or law enforcement, a permit holder must present sablefish for inspection and allow biological samples to be taken [5 AAC 28.170 (l)]. 

### Prohibitions

The operator of a fishing vessel may not take sablefish in the NSEI area with sablefish from another area on board. Also, the operator of a vessel taking sablefish in the NSEI area shall unload those sablefish before taking sablefish in another area [5 AAC 28.170(a) and (b)]. 

A vessel, or person onboard a vessel, from which commercial, subsistence, or personal use longline fishing gear was used to take fish in the NSEI or SSEI Subdistricts during the 72-hour period immediately before the start of the commercial sablefish fishery in that subdistrict, or from which that gear will be used during the 24-hour period immediately after the closure of the commercial sablefish fishery in that subdistrict, may not participate in the taking of sablefish in that subdistrict during that open sablefish fishing period. A vessel, or a person onboard a vessel, who has harvested and sold their personal quota share before the final day of the sablefish season in that subdistrict is exempt from the prohibition on fishing longline gear during the 24-hour period immediately following the closure of the sablefish fishery in that subdistrict. In addition, a vessel or a person on board a vessel commercial fishing for sablefish in the NSEI Subdistrict may not operate subsistence or personal use longline gear for groundfish from that vessel until all sablefish harvested in the commercial fishery are offloaded from the vessel. 

<br>

# 2022 SABLEFISH STOCK ASSESSMENT AND 2023 RECOMMENDED ABC DETERMINATION

Sablefish are a highly migratory, long-lived species broadly distributed in the North Pacific Ocean. Although research to date suggests that sablefish comprise a single, panmictic population, they are managed as separate stocks in Alaska state and federal waters, British Columbia, and in state and federal waters off the U.S. west coast. After three decades of declining or suppressed spawning stock biomass in the North Pacific, persistent high catch rates of small sablefish in recent years across multiple surveys and fisheries signal strong recruitment and increasing trends for the stock (Goethel et al. 2021). 

Despite these positive population trends, we continue to recommend a precautionary approach to setting harvest limits. The target fishing mortality rate of $F_{50}$, that defines maximum ABC is based on female spawning stock biomass and does not take into account the relative economic value of sablefish. Because sablefish begin contributing to the spawning biomass as young as age-3, ABCs can increase quickly even if average fish size is small. These small sablefish are worth significantly less per pound, making them subject to high release rates in NSEI where fishery releases are legal. Taken together, steep annual increases in ABCs in response to large recruitment events can result in low fishery value, and the unobserved fishery releases introduce an uncertain source of mortality into the stock assessment. As the 2013-2018 year classes mature these strong recruitment events are beginning to translate into higher catches and ex-vessel value evident in 2022 (Figure \@ref(fig:catch)).  CPUE in the fishery has increased as more of these fish are landed, which tracks with increased CPUE in the longline survey and the high estimates of abundance from the mark-recapture project.  As fish from these strong year classes grow they are more likely to be retained and sold.  Similarly, as these fish mature they are increasing the size of the spawning biomass.  

In response to concerns about release practices, we introduced a “max 15% change” management procedure in 2020 that constrains the recommended ABC to a 15% annual maximum change. This management procedure was well-received during two stakeholder and industry meetings in April 2020 and 2021 and appears to be supported by the fleet. The “max 15% change” management procedure has been shown to increase fishery stability, maximize catch, and successfully achieve biological goals in long-term simulations conducted by IPHC (https://www.iphc.int/uploads/pdf/srb/srb014/ppt/iphc-2019-srb014-08-p.pdf). The current NSEI harvest policy continues to define maximum permissible ABCs at $F_{50}$, and recommended ABCs will be constrained to a maximum 15% change between years.

In 2020, we implemented an integrated statistical catch-at-age (SCAA) model for the NSEI stock assessment, which had been in development for several years (Sullivan et al. 2020). The SCAA model is structured similarly to the federal sablefish model (Goethel et al. 2022) and allows for the estimation of recruitment, spawning stock biomass, and abundance. This model was used again in 2023 with several modifications that loosened reliance on fixed values derived from the federal assessment and makes the model more responsive to NSEI specific data. 

The SCAA model results in a maximum permissible ABC of 1,573,109 round lb at a target fully selected fishing mortality of $F_{50}$ (Table \@ref(tab:brps)). This is a 129,795 lb increase (9%) from the 2022 ABC of 1,443,314 round lb. Under the max 15% change management procedure, the recommended 2023 ABC remains the same as the maximum permissible ABC. To account for legal releases of small sablefish in NSEI, fixed retention probabilities and an assumed discard mortality of 16% were incorporated directly into the SCAA model following Sullivan et al. (2019).  The mortality from fishery releases under $F_{50}$ is estimated to be 69,522 lb (79,711 in the base model) and is incorporated directly into the max ABC calculation. See section titled “ABC Recommendations” for more information.  

The following are notable results from the SCAA model and reflect potential conservation or assessment concerns for this stock:

1. <span style="color: red;">Slinky pots:</span>  This was the first year where slinky pots were allowed in the NSEI sablefish fishery.  Participation was limited in 2022 and did not impact this assessment.  A large increase in pot usage is anticipated in 2023 and will likely affect the next assessment in 2024.  

2. Stock status (i.e, the proportional relationship of the stock relative to it’s virgin state) remains uncertain and sensitive to data weighting methodology and fishery selectivity values that remain fixed to values from the federal assessment. The department manages the NSEI fishery for $F_{50}$ (the fishing mortality that results in a SPR (spawner per recruit) of 50%) and changes in model structure and assumptions results in changes to where the population is relative to this target (Table \@ref(tab:modcomp) and Figure \@ref(fig:Fsprcomp)).   Updates to the model instituted in this assessment decreased the reliance on subjective weighting of the data sources but retains a degree of subjectivity in the amount of variance ascribed to the three indices of abundance.  The model relies on variance terms for the mark-recapture abundance estimate and fishery CPUE that are inflated above those calculated from the data and changes to those terms results in different conclusions about where the population is relative to the management target.  Furthermore, updating fishery selectivity to the most recent estimates available from the federal model affects estimates of stock status and is thus a source of concern given likely differences in fishery selectivity between the federal and NSEI fisheries.  Although this assessment demonstrates a 9% increase in the ABC from last year, permutations to the variance terms associated with those indices can swing that increase to as low as 3% (Table \@ref(tab:modcomp)).  The trend in the stock is unambiguous, but managers should be wary of the uncertainty inherent in the current operating model.  As the data weighting for this model continues to evolve to be in line with best practices a goal remains to remove subjective assignment of variances or weighting and allow the model to estimate variance beyond that calculated from the data.  Initial steps were taken to address these concerns in 2023 but require more work before they are adopted into the operating model. 

3.  Fit of the model to the abundance indices remains poor and reliant on the inflated variance terms assigned to fishery CPUE and mark-recapture estimates of abundance.  In particular, the abundance estimates derived from the mark-recapture assumption that have underpinned the NSEI sablefish assessment since 2005 and provides scale to the population now appears to underestimate abundance relative to the model estimates.  There is tension between the other data sources (age and length compositions) that forces the aforementioned data weighting to keep the model tethered to those abundance estimates.  A thorough review of the mark-recapture experiment to identify and correct biases in the estimate remains a priority for this project.  Bias correction may result in better fit to the model both by correcting estimates and modifying the modeling prior (penalized likelihood) describing the relationship between actual abundance and the mark-recapture estimate (currently assumed to be a 1:1 ratio).  

4.  Fixing fishery selectivity to values estimated in the federal assessment remains a principal weakness in this model and assessment.  Efforts were made in this year’s assessment to estimate fishery selectivity in the model, but thus far the model has failed to converge and it was necessary to leave fishery selectivity fixed for this assessment.   The selectivity curve for the derby (pre-IFQ) fishery changed substantially since the last assessment owing to a general lack of data for the pre-IFQ fishery in the federal assessment.  The model and stock-status estimates remain sensitive to these selectivity values and developing the model to estimate fishery selectivity in the NSEI fishery remains a high priority going forward.

5. The fit of the model to the age data has improved relative to past assessments and is the result of the model tuning that resulted in higher estimates of effective sample size than those used in past assessments (a conservative estimate derived from the square root of the raw sample size).  The fit is still not satisfactory and likely is the result of fixed selectivity values for the fishery.  Better estimated selectivity curves remain a priority for addressing the fit of age data. 

6. Similarly, while the fit to length data is also improved owing to that model tuning methods now used, it is still far from desirable with a consistent pattern in the residuals whereby mid-size fish are underestimated and larger fish are overestimated in the model.  In conjunction with the retrospective results this suggests that the model may be underestimating large recruitment events.  Better estimation of selectivity in both the fishery and the survey will be necessary to improve the fit of length data.

7.	Recruitment of the 2013-2018 year classes was substantial and above the long term average.  These strong year classes are driving the increase in biomass that has occurred over the last several years.  These recruitment events are in line with what is seen in the federal assessment, although the increase evident in the NSEI population is not as steep as that seen in the federal assessment. These fish are still not fully mature or fully grown and thus biomass is likely to continue increasing over the next several yers as these fish grow and mature into the population.  However, they are likely still less than optimal size from a price standpoint and may still be subject to high release rates.  

8.	Retrospectives patterns in the model are satisfactory.  The model demonstrates a slightly positive bias in spawning biomass of 5% indicating that the model tends to overestimate spawning biomass.  The bias in recruitment is also low on average, although individual years can be quite biased (up to 200%).  In general, the model overestimates recruitment during low recruitment periods and underestimates recruitment during periods of high recruitment.  Given the strong evidence that the population has experienced a recruitment boom over the last several years it is likely that the size of those year classes is somewhat underestimated and the population will see continued growth for several more years. 

<br>

## CHANGES TO THE NSEI ASSESSMENT RELATIVE TO 2021

Updates to the stock assessment are listed here:

1.	Fishery CPUE was fully standardized to control for variability in fishing methods and practices over time (i.e, hook size, fishing depth, length of sets, location, etc.) to better detect underlying trends that reflect the abundance of fish available to the fishery.  This involved recalculating fishery CPUE from the re-entered logbook data that was completed in 2020.  In 2020, the ADF&G Southeast Groundfish Project biologists invested considerable staff time and resources into standardizing the full time series of available logbook data, which should improve the long-term quality and interpretation of this index. In particular, consistent methods for identifying target species by trip and set efforts were developed, which was previously conducted manually.  <span style="color: red;">METHODS FROM RHEA!!!</span>

2.	Fishery selectivity in the SCAA was updated to the fixed values estimated in the federal sablefish fishery (Goethel et al. 2022).  

3.	Survey selectivity was switched from being fixed to the values estimated in the federal domestic longline survey to being freely estimated in the SCAA model, thus being a more accurate reflection of the NSEI longline survey.  This involved modelling selectivity in two time blocks reflecting the survey before and after it became fully standardized in 2000. 

4.	The recruitment process is now modelled using random effects which allows for the estimation of variability, $\sigma_R$.  Prior to this assessment, $\sigma_R$ had been fixed at the assumed federal assessment value of 1.2.  

5.	The data weighting of the model was changed to reflect best practices in SCAA modelling.  This involved tuning the age and length compositional data to adjust the effective sample sizes using McAllister and Ianelli (1997) methodology and removing the fixed weights that had been applied to the abundance indices (mark-recapture estimates, longline survey CPUE and longline fishery CPUE).  The variance of the longline survey was changed from assumed values to the true estimates of variance.  The fishery CPUE and mark-recapture variances were kept at the inflated and fixed values to allow for the extra uncertainty in these indices owing to the unrecorded releases of fish that are permitted in the fishery.  

We made no additional changes to the SCAA model structure or assumptions, estimation of biological reference points, or population dynamics equations. We used status quo methods to update estimates of weight-at-age, maturity-at-age, catch, survey CPUE, mark-recapture abundance, and age/length compositions. For detailed technical information on the SCAA model and data preparation, please see Sullivan et al. (2020) or visit the GitHub repository for this project: https://github.com/commfish/seak_sablefish. 

## MODEL STRUCTURE

The integrated statistical catch-at-age (SCAA) model presented here was coded in TMB, an R library that leverages C/C++ functionality to calculate first and second order derivatives and was inspired by a similar C/C++ templating software ADMB (Kristensen et al. 2016; Fournier et al. 2012). The TMB code replicates or makes refinements to methods used in a previous ADMB based, age-structured model for the NSEI sablefish stock (Mueter 2010) that was based on code from an older federal assessment of sablefish that has also been adapted for several Alaska rockfish stocks (Kimura 1990; Sigler 1999). The model can be run as either a single-sex or sex-structured model; however, data inputs are only shown for the sex-structured model. Variable definitions for all equations used in the statistical catch-at-age model can be found in Table \@ref(tab:vardefns). Uncertainty in parameters are currently estimated using a maximum likelihood approach.

## DATA INPUTS

The data used as inputs to the SCAA model biological data, catch, abundance, and composition (Figure 4) are found here: https://github.com/commfish/seak_sablefish/tree/master/data/ tmb_inputs).

### Weight-at-age

Data from the 2002–2022 longline fishery and 1997–2022 ADF&G longline surveys were used to obtain fishery and survey weight-at-age used in the SCAA model. A weight-based von Bertalanffy growth model was fit to weight-at-age data: 

$$
\begin{equation}
\ln(w_a)=\ln W_{\infty}+\beta \cdot \ln (1-\exp (a-t_0)) \\
(\#eq:weightvonb)
\end{equation}
$$

where $w_a$ is weight at a given age (lb), $W_{\infty}$ is the mean asymptotic weight (lb), $\beta$ is the power in the allometric equation, *k* relates to the rate at which $W_{\infty}$ is reached, and $t_0$ is the theoretical age at weight zero (years).  Residuals $\epsilon$ were assumed lognormally distributed to account for increasing variability by age, and the variance of these residuals ($\sigma_R$) was estimated. Models were fit separately for each sex and data source using maximum likelihood and the mle() function in R.

The federal assessment uses survey weight-at-age exclusively to fit to catch and effort indices (Hanselman et al. 2018). However, because discarding is permitted in the NSEI fishery, there are large differences in survey and fishery weight-at-age, especially at younger ages (Figure 11A). Consequently, fishery weight-at-age was fit to landed catch biomass, whereas survey weight-at-age was used to estimate exploitable biomass, spawning biomass, and other quantities of interest in the model.

### Maturity-at-age

Maturity data from the 1997–2022 ADF&G longline surveys were used to fit a maturity ogive for female sablefish using logistic regression and the glm() function in R. Maturity-at-length data for this time period were more abundant than maturity-at-age data and appeared to provide the best estimates of maturity; therefore, maturity curves were fit using maturity-at-length data.

Predicted maturity-at-length was transformed to maturity-at-age using fitted values from a length-based von Bertalanffy growth curve fit to survey data. The length at 50% maturity is 61.2 cm; the $k_{mat}$ (the slope at the length at 50% maturity) is 0.38; and the age at 50% maturity is 5.9 years (Figure \@ref(fig:bioinputs)). Predicted proportions maturity-at-age were used as inputs to the SCAA model and in the calculation of spawning stock biomass.

Annual fits of maturity, though not explicitly used in the SCAA model, can provide insight into changes in the population or cohort-specific dynamics. Of note, the fit to maturity data in the last four years suggest that fish matured at younger ages and smaller sizes compared to previous years (Figure \@ref(fig:deltamat)). It is possible that earlier maturation can be linked to warm environmental conditions in the North Pacific since 2014, or to density-dependent effects driven by the large recruitment events in recent years. Trends in maturity and growth should be monitored in future assessments.  

### Catch

Catch data from 1975 to 2022 include harvest in the directed sablefish longline and pot fishery, ADF&G longline survey removals, and sablefish retained in other fisheries like the individual fishing quota halibut longline fishery (Figure \@ref(fig:catch) and \@ref(fig:abdind)A). Catch estimates from 1975 to 1984 were obtained from Carlile et al. (2002) and 1985–present catch was obtained from fish tickets. Catch was estimated in the SCAA model assuming a lognormal distribution with a fixed log standard deviation of 0.05. Changes in the management structure during this period included a move to limited entry in 1985 and the EQS program in 1994 (Olson et al. 2017). Additional sources of mortality that are not currently included in this model include sport, subsistence and personal use harvest, estimated bycatch mortality in the halibut fishery, and estimated deadloss including mortality from sand fleas, sharks, and whales. Currently these additional sources of mortality are accounted for in the decrements process (see the section titled “Annual Harvest Objective Determination” for more information).

### Fishery CPUE

Fishery CPUE, defined as retained lb per hook, was used as an index of abundance from 1980 to 2022 (Figure \@ref(fig:abdind)B). Fishery CPUE was estimated in the SCAA model assuming a lognormal distribution with a fixed log standard deviation of 0.1 for the historical data from dockside interviews (1980–1996; Carlile et al. 2002) and 0.08 for the contemporary logbook data (1997–present).

<span style="color: red;">In 2020, ADF&G reviewed and re-entered logbook data to standardize how trip and set targets were identified.  In past years this was done ad-hoc on an annual basis and methods were not documented leading to confusion with the retirement and turnover of staff.  This project established guidelines for identifying trip and set target based on yadda, yadda, yadda. Trips that had CONDITION were used to identify trip targets while sets that had CONDITION were used to identify set targets. Only sets and trips targetting sablefish were used to calculate fishery CPUE values for use in the assessment. </span>

Fishery CPUE since 1997 was fully standardized in this year’s assessment to account for shift in fishing practices and vessel participation over time.  Standardization accounts for variability in hook size, hook spacing, fishing depth, soak time, statistical area (fishing location), fishing vessel (as a random effect), julian day, and set length.  CPUE was estimated as the predicted value from generalized additive models (GAM) fitting CPUE to these variables using the mgcv package in R and the gamma smoothing feature.  Standardization resulted in slight changes in the overall time series from past assessments, but the standardized values do a better job of catpuring the increase in biomass that has occured in recent years (Figure \@ref(fig:fcpuestand)).  Standardized fishery CPUE in 2022 was at it’s highest value since 2000 (Figure \@ref(fig:abdind)B), although it remains below the high catch rates seen in the 1980s and early 1990s (Figure \@ref(fig:abdind)B and Figure \@ref(fig:fcpuestand)). 

Because discarding sablefish is legal in the NSEI fishery, estimating fishery selectivity within the model is not currently possible. To address this issue, the federal selectivity curve is used in the model, which is estimated assuming 100% mandatory retention. A sex- and age-specific retention probability, coupled with a fixed discard mortality rate, are used to estimate mortality from fishery releases. Future research will be aimed at better understanding discarding behavior in the NSEI fishery as it relates to economic and biological factors. 

### Survey CPUE

Longline survey CPUE in numbers per hook was used as an index of abundance from 1997 to 2022 (Figure \@ref(fig:abdind)C). This index was assumed to be log-normally distributed, with a fixed log  standard deviation derived from the data. The 1988–1996 longline surveys used a shorter soak time of 1 hr instead of the current 3–11 hr (Carlile et al. 2002; Dressel 2009). These data were omitted because the 1 hr soak time was likely too short to provide an accurate measure of relative abundance (Sigler 1993).

Survey CPUE has remained substantially above the long term mean since 2020 with minimal variation over the last three years (Figure \@ref(fig:abdind)C).  

### Mark-recapture abundance

Currently, ADF&G conducts an annual mark–recapture survey that serves as the basis for stock assessment and management (Green et al. 2016; Stahl and Holum 2010). Fish are tagged during a pot survey in May and June, with recaptures occurring in the ADF&G longline survey in late July or early August and the longline fishery from August through November (Beder and Stahl 2016). 

The mark–recapture abundance estimates provide an index of exploitable abundance for years when a marking survey occurred (2003–2010, 2012, 2013, 2015, 2017–2020; 2022; Figure \@ref(fig:abdind)D). This index was assumed to be lognormally distributed with a fixed log standard deviation of 0.05. The mark–recapture abundance index increased from 3.01 to 3.14 million fish (+4.3%) between 2020 and 2022 and is the highest estimate since 2005 (Figure \@ref(fig:abdind)D). 

The 2022 marking survey released 8,654 tagged fish (Table \@ref(tab:mr)). Following methods in past assessments, we accounted for tags recovered outside of the NSEI or period of recapture, natural and fishing mortality, and differences in the size of fish captured in the pot survey and the longline fishery (Appendix A in Sullivan et al. 2019). A summary of data used in the mark–recapture models is in Table \@ref(tab:mr). 

Mark–recapture abundance estimates were obtained using a time-stratified Petersen mark–recapture model implemented in the Bayesian software JAGS 4.3.0 (Depaoli et al. 2016). For any given time period 𝑖, the number of tagged fish in Chatham Strait (*K*) and subsequent abundance (*N*) were modeled as:

$$
\begin{equation}
K_{i} = \left\{ \begin{array}{ll}
(K_0 - D_0)*exp(-M*t_i) &i = 1\\
(K_{i-1} - k_{i-1} - D_{i-1})*exp(-M*t_i) & i > 1
\end{array}\right.
(\#eq:mr1)
\end{equation}
$$

and 

$$
\begin{equation}
N_{i} = \left\{ \begin{array}{ll}
N_i *exp(-M*t_i) &i = 1\\
(N_{i-1} - C_{i-1})*exp(-M*t_i) & i > 1
\end{array}\right.
(\#eq:mr2)
\end{equation}
$$
where $K_0$ is number of tags released in the ADF&G pot survey, $D_0$ is the number of tagged fish that are not available to either the ADF&G longline survey or to the fishery (tags recovered in halibut fishery or outside of Chatham Strait), *𝑀* is assumed natural mortality of 0.10 (Johnson and Quinn 1988), *k* is the number of marked fish recovered, and *C* is the total catch or number of sablefish removed. $N_1$ was assumed to follow a normal distribution with an uninformed prior (precision = $1×10^{-12}$) centered on past assessments’ forecast of abundance.

The probability that a sablefish caught in a given time period is marked *p_i* is informed by the ratio of marks in the population to the total population at that time $K_i/N_i$. Each $p_i$ is assumed to follow  beta prior distribution $p_i$ = $\beta$($\alpha$, $\beta$), where $\alpha$ = ($K_i/N_i$ ) ∗ *x*, $\beta$ = (*1 − $K_i/N_i$ *)/*x*, and a large *x* indicates confidence in $K_i/N_i$. Because $N_i$ was previously assumed to follow vague normal prior, $p_i$ was assigned an informed prior by setting *x* equal to 10,000. 

In each time period, the likelihood of recapturing *k* marked sablefish given *n* sampled fish follows a binomial distribution, where

$$
\begin{equation}
Pr(k|n,p) = {n \choose k}p^k(1-p)^{n-k}
(\#eq:mr3)
\end{equation}
$$
Additional information on mark–recapture modeling, alternative models considered, and model selection methodology is detailed in Appendix A of Sullivan et al. (2019).

The mark-recapture experiment likely overestimates precision and is biased to some degree given that there are currently no diagnostics that examine differences in capture probability based on fish size and/ or location.  Furthermore, the project relies on marked fish being returned by fishermen and the accounting done at processing plants by ADF&G staff and tag returns from industry seldom agree.  A thorough re-evaluation of this project remains a priority both to detect and potentially correct biases in the estimates and produce more accurate estimates of uncertainty in the estimate.  

### Age compositions

Fishery age compositions from the 2002–2022 longline fishery and survey age compositions from the 1997–2022 longline surveys (Figure \@ref(fig:agecomps)) were included in the model. The plus group age was updated from 42 to 31 in 2020 to maintain consistency with the federal assessment. Sample sizes were deemed insufficient to fit age compositions by sex, so age data have been aggregated for both the survey and fishery. The McAllister and Ianelli (1997) method of tuning composition data by iteratively reweighting the sample size has been applied to the SCAA model and were implemented in this assessment.

Currently no NSEI-specific ageing error matrix exists. Until this has been fully developed and reviewed, the federal sablefish ageing error matrix has been made available to the State of Alaska (D. Hanselman, Fisheries Research Biologist, NOAA, Juneau, personal communication, April 2019; Hanselman et al. 2018; Heifetz et al. 1999). The ageing error matrix ($\Omega_{a',a}$) is the proportion observed at age *a* given the true age ${a}^{'}$. Ageing error matrices are critical for correcting observed age compositions and estimating recruitment (Fournier and Archibald 1982). Future research should include the development of an ageing error matrix for NSEI in conjunction with the ADF&G Age Determination Unit. 

### Length compositions

Sex-structured length data from the 2002–2022 longline fishery and 1997–2022 ADF&G longline surveys (Figure \@ref(fig:lencomp)) were summarized using the federal conventions for length compositions (Hanselman et al. 2018). The federal assessment uses 2 cm length bins ranging from 41 to 99 cm. Fish less than 41 cm (*$l_0$*) were omitted from the analysis, and fish greater than 99 cm were aggregated into the 99 cm length bin (*l+*). Effective sample sizes were estimated using the McAllister and Ianelli (1997) method of tuning composition data by iteratively reweighting the sample size.

Length distributions in the fishery have dramatically different patterns than the survey (Figures \@ref(fig:lencomp) and \@ref(fig:compcomps)), with few lengths in the fishery less than 60 cm. Full retention is not a requirement in state waters and the length differences between the survey and fishery are attributed to fishery releases of small fish. Because of the bias introduced by allowing fish to be released in the fishery, fishery age and length compositions tend to be poorly fit by the model. 

Finally, the selective harvest of larger-bodied fish results in large differences between survey and fishery size-at-age. Until an age-length key is developed for NSEI, the federal age-length keys ($\Lambda_{a,l,k}$) will be used to fit both survey and fishery length compositions (D. Hanselman, Fisheries Research Biologist, NOAA, Juneau, personal communication, April 2019; Hanselman et al. 2018; Echave et al. 2012; Figure 18). Ultimately, separate age-length keys should be developed for each data source to account for the differences in survey and fishery size-at-age.

### Retention probability

The release of healthy (i.e., not dead, sand flea bitten, etc.) sablefish is allowed in state waters. To model the discarding behavior in the NSEI fishery, processor grade and price per pound data were used to inform retention probabilities-at-size (Figure \@ref(fig:retention)). Based on conversations with groundfish port sampling staff and fishermen, the lower bound of the Grade 2/3 (3.1 round lb) was assigned a 10% retention probability, the lower bound of the Grade 3/4 (4.9 round lb) was assigned a 50% retention probability, and everything greater than 8 round lb was assigned a 100% retention probability (A. Olson, Groundfish project leader, ADF&G, personal communication, July 2018). Remaining retention probabilities were interpolated between these fixed values. Weight-based retention probabilities were translated to sex and age using the longline survey sex- and weight-based von Bertalanffy growth curves (Figure \@ref(fig:bioinputs)A).

## MODEL PARAMETERS

### Natural mortality

Natural mortality *M* was assumed constant over time and age and fixed at 0.10 (Johnson and Quinn 1988). Code infrastructure has been developed to estimate *M* using a prior as is done in the federal assessment, but this methodology will not be implemented until prior distributions can be thoroughly analyzed.

### Discard mortality

Stachura et al. (2012) estimated discard mortality *D* of sablefish to be 11.7% using release–recapture data from a longline survey in Southeast Alaska. It is likely that discard mortality in a fishery is higher due to careful fish handling on survey vessels during tagging experiments. Therefore, the discard mortality rate from the Pacific halibut fishery, *D*=16%, was used (Gilroy and Stewart 2013). The Pacific halibut fishery is assumed a reasonable proxy for sablefish because the fisheries utilize similar gear and frequently the same vessels and crew participate in both fisheries. Moreover, both species are considered hardy and do not experience barotrauma.

### Selectivity

The longline fishery and survey are assumed to follow a logistic selectivity pattern. The current parameterization of the logistic curves uses *$s_{50}$* and *$\delta$*, which represent the ages at which 50% of fish are selected by the gear (*$s_{50}$*) and the shape or slope of the logistic curve (*$\delta$*). Selectivity-at-age (*$s_a$*) for this parameterization is defined as

$$
\begin{equation}
s_a=\frac{1}{1+\mbox{exp}(-k(a-s_{50}))}.
(\#eq:sel2)
\end{equation}
$$

Selectivity is fit separately for the longline fishery (*fsh*) and survey (*srv*). There is flexibility to define discrete time blocks for both fishery and survey selectivity.

Currently, fishery selectivity is fixed in the model using federal selectivity values for the derby (pre-EQS) and contemporary fishery (EQS) (Goethel et al. 2022; Figure \@ref(fig:slx)). Estimating selectivity is challenging when accounting for fishery releases because no age or length data are available on the released fish. Further research is needed to better characterize how discarding behavior has changed over time and if discarding was common pre-EQS.

Selectivity in the longline survey is now estimated in the model using two time blocks representing the unstandardized survey (pre-2000) and the fully standardized survey that began in 2000.  

### Catchability

Currently 5 parameters for catchability are estimated: 2 for fishery catchability (pre-EQS and EQS) ln(*$q_{fsh}$*), 2 for the ADF&G longline survey ln(*$q_{srv}$*), and 1 for the mark–recapture abundance index ln(*$q_{MR}$*).

### Recruitment and initial numbers-at-age

The numbers-at-age matrix *N* is parameterized with mean log-recruitment *$\mu_R$*, 48 (*T*) log recruitment deviations *$\tau$*, mean log initial numbers-at-age *$\mu_N$*, and 28 (*A* − 2) deviations from mean log initial numbers-at-age *$\psi$*. The parameter that governs the variability in *$\tau$* and *$\psi$*, ln(*$\sigma_R$*), is estimated within the model using random effects.

### Fishing mortality

There is 1 parameter estimated for mean log-fishing mortality, *$\mu_F$*, and 48 (*T*) log-fishing mortality deviations *$\phi$*.

## POPULATION DYNAMICS

The population dynamics of this model are governed by the following state dynamics equations, where the number of sablefish $N$ in year $t=1$, age $a$, and sex $k$ are defined as

$$
\begin{equation}
N_{1,a,k} = \left\{ \begin{array}{ll}
0.5\cdot\mbox{exp}(\mu_R-M(a-a_0)+\psi_a) &a_{0}<a<a_{+}\\
0.5\cdot\mbox{exp}(\mu_R-M(a_{+}-1))/(1-\mbox{exp}(-M)) &a=a_{+}
\end{array}\right.
(\#eq:Nmat1)
\end{equation}
$$

Recruitment to age-2 in all years and the remaining projected $N$ matrix is defined as

$$
\begin{equation}
N_{t,a,k} = \left\{ \begin{array}{ll}
0.5\cdot\mbox{exp}(\mu_R+\tau_t) &a=a_0\\
0.5\cdot N_{t-1,a-1,k}\mbox{exp}(Z_{t-1,a-1,k}) &a_{0}<a<a_{+}\\
0.5\cdot N_{t-1,a-1,k}\mbox{exp}(Z_{t-1,a-1,k})+N_{t-1,a,k}\mbox{exp}(Z_{t-1,a,k}) &a=a_{+}
\end{array}\right.
(\#eq:Nmat2)
\end{equation}
$$

where the total instantaneous mortality, $Z_{t,a,k}$, is the sum of natural mortality $M$ and fishing mortality $F_{t,a,k}$. Sex ratios are assumed 50/50 at time of recruitment, thus any changes in sex ratios in the population over time are the result of sex-specific, fully selected fishing mortality.
Total annual fishing mortality *$F_t$* is defined as

$$
\begin{equation}
F_t=\mbox{exp}(\mu_F+\phi_t).
(\#eq:fmort1)
\end{equation}
$$

Fishing mortality is modeled as a function of fishery selectivity $s_{t,a,k}$, retention probability $R_{a,k}$ (the age-specific probability of being landed given being caught, Figure \@ref(fig:retention)), and discard mortality $D$:

$$
\begin{equation}
F_{t,a,k}=s_{t,a,k}^{fsh}(R_{a,k}+D(1-R_{a,k}))F_t.
(\#eq:fmort2)
\end{equation}
$$

## PREDICTED VALUES

Predicted fishery CPUE (kg per hook) in year $t$, $\hat{I_t}^{fsh}$ Is defined as a function of fishery catchability $q_{fsh}$ and biomass available to the fishery:

$$
\begin{equation}
\hat{I_t}^{fsh}=q_{fsh}\sum_{k=1}^{2}\sum_{a=a_0}^{a+}w_{a,k}^{srv} \cdot s_{t,a,k}^{fsh} \cdot N_{t,a,k} \cdot S^{fsh},
(\#eq:predfshcpue)
\end{equation}
$$

where $w_{a,k}^{srv}$ is mean weight-at-age by sex in the longline survey.  Survival ($S^{srv}$) to the beginning of the fishery in August is defined as 

$$
\begin{equation}
S_{t,a,k}^{fsh}=exp(\frac{8}{12}(M+F_{t,a,k}))
(\#eq:bigS)
\end{equation}
$$

Survival equations include natural and fishing mortality because the model assumes continuous fishing mortality.

Predicted longline survey CPUE (numbers per hook) in year $t$, ($\hat{I_t}^{srv}$) is defined as a function survey catchability $q^{srv}$, abundance available to the survey, and survival to the beginning of the survey in July ($S^{srv}$):

$$
\begin{equation}
\hat{I_t}^{srv}=q_{srv}\sum_{k=1}^{2}\sum_{a=a_0}^{a+}s_{t,a,k}^{srv} \cdot N_{t,a,k} \cdot S^{srv}.
(\#eq:predsrvcpue)
\end{equation}
$$

Predicted mark-recapture abundance in year $t$ ($\hat{I_t}^{MR}$) is defined as a function of mark-recapture catchability $q^{MR}$, abundance available to the fishery, and survival to the beginning of the NSEI fishery in August ($S_{t,a,k}^{fsh}$):

$$
\begin{equation}
\hat{I_t}^{MR}=q_{MR}\sum_{k=1}^{2}\sum_{a=a_0}^{a+}s_{t,a,k}^{fsh} \cdot N_{t,a,k} \cdot S_{t,a,k}^{fsh}.
(\#eq:predmr)
\end{equation}
$$

Spawning biomass $SSB$ is calculated as

$$
\begin{equation}
SSB=\sum_{a=a_0}^{a+} w_{a,f}^{srv} \cdot N_{t,a,f} \cdot S_{t,a,k}^{spawn} \cdot p_a,
(\#eq:ssb)
\end{equation}
$$

where $w_{a,f}^{srv}$ is mean weight-at-age of females in the longline survey, $S^{spawn}$ is the fraction of individuals surviving to spawn in February, and $p_a$ is the proportion of mature females-at-age. In the single sex model, proportion of females at age in the survey $r_a$ is used to get the female portion of the $N$ matrix. 

Predicted survey age compositions (sexes combined) are computed as

$$
\begin{equation}
\hat{P}_{t,a}^{srv}=\Omega_{a',a}\frac{\sum_{k=1}^{2}N_{t,a,k} \cdot s_{a,k}^{srv}}{\sum_{k=1}^{2}\sum_{a=a_0}^{a+} N_{t,a,k} \cdot s_{a,k}^{srv}},
(\#eq:predsrvage)
\end{equation}
$$

where $\Omega_{a',a}$ is the ageing error matrix.  Predicted fishery age compositions (sexes combined) are computed as

$$
\begin{equation}
\hat{P}_{t,a}^{fsh}=\Omega_{a',a}\frac{\sum_{k=1}^{2}C_{t,a,k}}{\sum_{k=1}^{2}\sum_{a=a_0}^{a+} C_{t,a,k}},
(\#eq:predfshage)
\end{equation}
$$

where $\hat{C}_{t,a,k}$ is the predicted landed catch in numbers-at-age by sex derived from a modified Baranov catch equation:

$$
\begin{equation}
\hat{C}_{t,a,k}=N_{t,a,k}\frac{R_{a,k}F_{t,a,k}}{Z_{t,a,k}}(1-\mbox{exp}(-Z_{t,a,k})),
(\#eq:landed)
\end{equation}
$$

where $R_{a,k}$ is the assumed probability of retention by age and sex (Figure \@ref(fig:retention)).

Predicted landed catch in biomass $\hat{Y}$ is calculated as the product of fishery weight-at-age, $w_{a,k}^{fsh}$, and landed catch in numbers-at-age:

$$
\begin{equation}
\hat{Y}_t=\sum_{k=1}^{2}\sum_{a=a_0}^{a+} w_{a,k}^{fsh} \cdot \hat{C}_{t,a,k}.
(\#eq:yield)
\end{equation}
$$

The predicted biomass of discarded sablefish estimated to die ($W_t$) with an assumed discard mortality ($D$) of 0.16 is

$$
\begin{equation}
\hat{W}_t= \sum_{k=1}^{2}\sum_{a=a_0}^{a+}w_{a,k}^{srv}N_{t,a,k}\frac{D (1-R_{a,k})F_{t,a,k}}{Z_{t,a,k}}(1-\mbox{exp}(-Z_{t,a,k})).
(\#eq:wastage)
\end{equation}
$$

Predicted survey length compositions are calculated using the sex-specific age-length keys ($\Lambda_{a,l,k}$), such that

$$
\begin{equation}
\hat{P}_{t,l,k}^{srv}=\Lambda_{a,l,k}\frac{N_{t,a,k} \cdot s_{a,k}^{srv}}{\sum_{a=a_0}^{a+} N_{t,a,k} \cdot s_{a,k}^{srv}}.
(\#eq:predsrvlen)
\end{equation}
$$

Predicted fishery length compositions are calculated as 

$$
\begin{equation}
\hat{P}_{t,l,k}^{fsh}=\Lambda{a,l,k}\frac{\hat{C}_{t,a,k}}{\sum_{a=a_0}^{a+} \hat{C}_{t,a,k}}.
(\#eq:predfshlen)
\end{equation}
$$

## BIOLOGICAL REFERENCE POINTS

Biological reference points for NSEI sablefish were developed for the SCAA model following the federal assessment ADMB code (D. Hanselman, Fisheries Research Biologist, NOAA, Juneau, personal communication, April 2019). They are based on spawning potential ratio (SPR), or the average fecundity of a recruit over its lifetime divided by the average fecundity of a recruit over its lifetime when the stock is unfished. Spawning stock biomass is used as a proxy for fecundity, which assumes that weight-at-age and fecundity-at-age are proportionally related.

The theoretical numbers-at-age per recruit ($N_a^{SPR}$) under the current harvest policy $F_{50}$ (the fishing mortality that results in a SPR of 50%) is initialized with 1, then populated assuming the most recent year’s values (*T*) for female fishery selectivity-at-age and estimated $F_{50}$:

$$
\begin{equation}
N_{a}^{SPR50} = \left\{ \begin{array}{lll}
1 &a = a_{0}\\
N_{a-1}^{SPR50}exp(-M-F_{50}s^{fsh}_{a-1,fem}) &a_{0} < a < a_{+})\\
N_{a-1}^{SPR50}exp(-M-F_{50}s^{fsh}_{a-1,fem}) +  N_{a}^{SPR50}exp(-M-F_{50}s^{fsh}_{T,a,fem}) &a=a_{+}
\end{array}\right.
(\#eq:Nspr50)
\end{equation}
$$
The $N_a^{SPR}$ under unfished conditions (relating to an SPR of 100%) collapses to 

$$
\begin{equation}
N_{a}^{SPR100} = \left\{ \begin{array}{lll}
1 &a = a_{0}\\
N_{a-1}^{SPR100}exp(-M) &a_{0} < a < a_{+})\\
N_{a-1}^{SPR100}exp(-M) +  N_{a}^{SPR100}exp(-M) &a=a_{+}
\end{array}\right.
(\#eq:Nspr100)
\end{equation}
$$
The spawning biomass per recruit (${SPBR}_{SPR}$) under fished (e.g., *SPR* = 50%) and unfished (*SPR* = 100%) condition is

$$
\begin{equation}
SPBR_{SPR}={\sum_{a=a_0}^{a+}}w_{a,f}^{srv}*N_a^{spr}*S_{T,a,f}^{spawn}*p_a.
(\#eq:SPBRSPR)
\end{equation}
$$
Equilibrium recruitment is assumed to be equal to the geometric mean of the full estimated recruitment time series such that

$$
\begin{equation}
\dot{R}=({\prod_{t=1}^{T}exp(\mu_R+\tau_t)})^{\frac{1}{T}}.
(\#eq:SPBRSPR)
\end{equation}
$$
This assumption differs from the federal model, which assumes the arithmetic mean instead of the geometric mean. The geometric mean is a more appropriate measure of central tendency because sablefish recruitment is best described by a multiplicative function. Using the arithmetic mean in this case results in an equilibrium value for recruitment that is biased high.

Assuming a 50/50 sex ratio for recruitment, equilibrium female spawning biomass (${SB}_{SPR}$) under fished and unfished conditions is calculated as

$$
\begin{equation}
SB_{SPR}=0.5*\dot{R}*SPBR_{SPR}
(\#eq:SBSPR)
\end{equation}
$$
The SPR-based fishing mortality rate of $F_{50}$ is estimated using penalized likelihood, where

$$
\begin{equation}
lnL(SPR) = 100({\frac{SBPR_{50}}{SBPR_{100}}-0.50})^2
(\#eq:lnSPR)
\end{equation}
$$
In addition to $F_{50}$, $F_{35}$, $F_{40}$, $F_{60}$ and $F_{70}$ are estimated fro comparison.

The maximum permissible ABC is calculated as the difference between the predicted landed proportion of the catch ($\hat{Y}_{T+1}$) and the estimated mortality from releases ($\hat{W}_{T+1}$) under $F_{50}$ using forecasted estimates of abundance ($N_{T+1}$). Equation details for $\hat{Y}_{T+1}$ and $\hat{W}_{T+1}$ are detailed in the section of this report titled “Predicted Values.”

## LIKELIHOOD COMPONENTS

The objective function, or the total negative log-likelihood to be minimized, included the sum of the following likelihood components $L$ which received individual weights $\lambda$:.  
<ol>
  <li>  Landed catch biomass ($Y$) was modeled using a lognormal likelihood where $\sigma_Y$ was assumed to be 0.05:  $$
\begin{equation}
\mbox{ln}L(Y)=\lambda_Y\frac{1}{2\sigma_Y^2}\sum_{t=1}^{T}\Big(\mbox{ln}(Y_t+c)-\mbox{ln}(\hat{Y}_t+c)\Big)^2 ,
(\#eq:catchlike)
\end{equation} 
$$ 
where $\lambda_Y$ = 1.0 and $c$ is a small constant set at 0.0001 to allow approximately zero catches in log-space. 

  <li> Fishery CPUE, survey CPUE, and the mark-recapture abundance index were modeled using lognormal likelihoods, where $\sigma_I$ was assumed to be 0.08 for the fishery and survey CPUEs and 0.5 for the mark-recapture abundance index: 

$$
\begin{equation}
\mbox{ln}L(I)=\lambda_I\frac{1}{2\sigma_I^2}\sum_{t=1}^{T_I}\Big(\mbox{ln}(I_t+c)-\mbox{ln}(\hat{I}_t+c)\Big)^2 ,
(\#eq:indexlike)
\end{equation} 
$$ 
where $T_I$ is the number of years of data for each index and $\lambda_I$ is set to 1.0.  

  <li> Fishery and survey age compositions were modeled using the multinomial likelihood ($P^{age}$), where inital effective sample size $\omega_t$ was calculated as the square root of the total sample size in year $t$: 

$$
\begin{equation}
\mbox{ln}L(P^{age})=\lambda_{P^{age}}\sum_{t=1}^{T_P^{age}} - \omega_t \sum_{a=a_0}^{a+} (P_{t,a}+c)\cdot\mbox{ln}(\hat{P}_{t,a}+c),
(\#eq:agemult)
\end{equation} 
$$
where $T_P^{age}$ is the number of years of data for each age composition, $\lambda_{P^{age}}$ is set to 1.0, and $c$ prevents the composition from being 0 in the likelihood calculation. The effective sample size used in the final model was estimated using iteritive re-weighting methods described by McAllister and Ianelli (1997).  
 
The Dirichlet-multinomial likelihood is also available in the SCAA code, which derives effective sample size through the estimation of an additional parameter $\theta$ using the Dirichlet-multinomial likelihood (Thorson et al. 2017): 

$$
\begin{equation}
\mbox{ln}L(P^{age})=\sum_{t=1}^{T_P^{age}} -\Gamma(n_t+1)-\sum_{a=a_0}^{a+}\Gamma(n_t P_{t,a}+1)+\Gamma(n_t\theta)-\Gamma(n_t+\theta n_t)+\sum_{a=a_0}^{a+}\Big[\Gamma(n_tP_{t,a}+\theta n_t \hat{P}_{t,a})-\Gamma(\theta n_t \hat{P}_{t,a})\Big],
(\#eq:agedirich)
\end{equation} 
$$
where $n$ is the input sample size. The relationship between $n$, $\theta$, and $\omega$ is 

$$
\begin{equation}
\omega_t = \frac{1+\theta n_t}{1+\theta}.
(\#eq:effn)
\end{equation} 
$$
Further exploration is needed to implement the Dirichlet-multinomial as efforts on this assessment failed to reach convergence when the Dirichlet-multinomial was implemented.  As such only results for the multinomial likelihood tuned using McAllister and Ianelli (1997) are presented in the current assessment. 
  <li> Fishery and survey length compositions by sex are modeled using the multinomial likelihood ($P^{len}$), where initial effective sample size $\omega_t$ is calculated as the square root of the total sample size in year $t$: 
$$
\begin{equation}
\mbox{ln}L(P^{len})=\lambda_{P^{len}}\sum_{k=1}^{2}\sum_{t=1}^{T_P^{len}} - \omega_t \sum_{l=l_0}^{l+} (P_{t,l}+c)\cdot\mbox{ln}(\hat{P}_{t,l}+c).
(\#eq:lenmult)
\end{equation} 
$$
$T_P^{len}$ is the number of years of data for each length composition and $\lambda_{Plen}$ is set to 1.0.  The Dirichlet-multinomial likelihood is also available for length compositions but failed to converge for this assessment.  As such the multinomial likelihoods tuned using McAllister and Ianelli (1997) are used in this assessment. 

 <li> Annual log-fishing mortality deviations ($\phi_t$) are included with a penalized lognormal likelihood, where 
$$
\begin{equation}
\mbox{ln}L(\phi)=\lambda_{\phi}\sum_{t=1}^{T}\phi_t^2,
(\#eq:fmortlike)
\end{equation} 
$$
where $\lambda_{\phi}$=0.1. 

 <li> Recruitment deviations ($\tau_t$) are modeled using random effects such that 
$$
\begin{equation}
\mbox{ln}L(\tau)=\lambda_{\tau}\sum_{i=1}^{T+A-2}\mbox{ln}(\sigma_R)+\frac{(\tau_i-0.5\sigma_R^2)^2}{2\sigma_R}.
(\#eq:randomrec)
\end{equation} 
$$
where -0.5$\sigma^2$ is a bias correction needed to obtain the expected value (mean) instead of the median, and $\lambda^{\tau}$ is fixed to 2.0. The initial numbers-at-age deviations $\psi_a$ are implemented in the same way as recruitment deviations and are governed by the same $\sigma_R$ . Unlike ADMB, TMB allows fast implementation of nonlinear random effects models by estimating the marginal likelihood of the fixed effects via the Laplace approximation and estimating the random effects using empirical Bayes methods (Kristensen et al. 2016). 
</ol>

### Priors

Because the mark–recapture abundance index scales the exploitable population, a normal prior is imposed on $q_{MR}$ of 1.0 with a standard deviation of 0.1. Vague priors are assigned to fishery and survey *q*. Future work on this model should include the development of priors for fishery and survey *q*.

<br>

## MODEL RESULTS

A total of 146 parameters were estimated in the SCAA model, which converged with a maximum gradient component less than 0.001 (Table \@ref(tab:keyparams)). The objective function value (negative log likelihood) was 1799 (Table \@ref(tab:likesum)). The model fits catch, survey CPUE, and pre-EQS fishery CPUE reasonably well in most years (Figure \@ref(fig:predabdind)). Contemporary fishery CPUE (EQS) does not fit well, with long runs of positive or negative residuals (Figure \@ref(fig:predabdind)B). The model performs poorly during the period directly following the implementation of EQS in 1994 for all indices, including catch (Figure \@ref(fig:predabdind)).  Additionally, the fit to the mark-recapture abundance estimates have worsened with the model estimating higher abundance than indicated the mark-recapture project in earlier years, although it fits well in recent years (Figure \@ref(fig:predabdind)D).  

Further consideration should be given to which abundance indices should be used in the model. For example, because releasing fish is legal in NSEI and past logbook data have not required released fish to be recorded, fishery CPUE may not be a suitable index of abundance. Starting in 2019, fishermen were required to provide an estimated number of released sablefish by set; however, there is no record of length or weight of these releases. 

The mark-recapture estimate of abundance is also likely biased to some degree and overestimates precision.  The project relies on tag returns from the fishery and tag accounting rarely matches the count of fin clips at processor plants performed by ADF&G staff.  Under and/or over reporting of tag recoveries likely biases the results to some degree and the bias may be different from year to year depending on retention incentives.  Furthermore, the removal of tags by fisherman prior to exam by ADF&G staff prevents the ability to identify and correct for tag loss.  Lastly, the current mark-recapture analysis does not correct for size or geographic differences in capture probabilities, which will bias results to some degree.  Examining these sources of biases remains a priority.  

Finally, variability in catch, survey and fishery CPUE indices, and the mark–recapture abundance estimate was assumed. Future enhancements could include estimating this variability using available data and allowing the SCAA model to estimate extra variance based on the fit to the entire data set. 

Derived indices of age-2 recruitment, female spawning stock biomass, and exploitable abundance and biomass (i.e., available to the fishery) suggest that this stock has been in a period of low productivity since the mid-1990s but has experienced a surge of recruitment in recent years, highlighted by the strong 2016-year class (Figure \@ref(fig:derivedts)). Recruitment trends are comparable with federal values, and estimates of spawning stock biomass, exploitable biomass, and exploitable abundance, including large recruitment events (Goethel et al. 2022; Sullivan et al. 2019). Although recruitment has been strong in recent years and biomass is clearly expanding as these fish grow and mature, the population remains below historical levels evident in the early 90’s.  And while the dominance of the younger age classes is the result of these strong recruitment events, the lack of older sablefish, which can live into their 90s, remains concerning given the likely outside contribution these older fish make to the spawning population.  

Fits to the age composition data is improved from past assessments, however still fails to capture all of the variability in the data (Figure \@ref(fig:fshage) and \@ref(fig:srvage)).  Although the model fits the general shape of the age compositions in most years, there are poor residual patterns (Figure \@ref(fig:residage)). Additionally, the model appears to underestimate fits to the plus group ages, which should be explored in future assessments. 

Fits to the length composition data also remain poor and suffer from poor residual patterns signifying that the model is underestimating smaller, mid-size classes and overestimating larger and the smallest size classes (Figures \@ref(fig:malefshlen), \@ref(fig:femalefshlen), \@ref(fig:malesrvlen), \@ref(fig:femalesrvlen) and \@ref(fig:residlen)).  Like the age compositions, the model predicts the general shape of the length compositions for both the survey and fishery in most years. Despite this, there are also poor residual patterns in the length compositions, and the model is not predicting the small individuals observed in the survey in recent years.

The lack of fit to the age and length composition data likely results from restrictions of fishery and survey selectivity in the model.  Survey selectivity is now estimated in the model, which appears to have improved model fit.  However, survey selectivity is modeled in two time blocks and allowing time-varying survey selectivity may further improve fit to the data.  Fishery selectivity is further restricted as the values are fixed to the federal model values owing to an inability in the model to estimate it.  Because no data on fishery releases exist, it may not be possible to estimate fishery selectivity that fit to the composition data. Stock assessments that account for discarded catch frequently have observer data and will overcome this challenge through the estimation of a separate selectivity curve for discarded catch (e.g., Zheng and Siddeek 2018). Methods to improve fits to fishery composition data should be developed in future assessments, including modeling changes in retention probability over time using price per pound and catch composition data.  It may also be possible to loosen reliance on the federal curves by placing prior around the selectivity parameters rather than fixing those values.  

Changes made to the operating model resulted in lower estimates of stock status although the overall trajectory of the stock remains the same.  Tuning the model to estimate the effective sample sizes or the age and length composition data placed more weight on the composition data and had the effect of increasing the biomass estimates (Table \@ref(tab:modcomp)).  Updating the selectivity curves to the most recent values in the federal assessment resulted in lower biomass estimate, although still above the base model using the old selectivity estimates.  The updates made for model v23 that include estimating survey selectivity within the model and estimating recruitment deviations using random effects resulted in lower biomass estimates.  The population still appears to be increasing, however the fishery appears closer to the management target of $SPR_{50}$ than estimated by the base model. 

Estimation of recruitment deviations using random effects produced much lower values of $\sigma_R$ than had been fixed to the federal model value of 1.2 (Table \@ref(tab:keyparams)).  The federal value is noticeably higher than that estimated for other Alaska groundfish stocks (Lynch et al. 2018; Hanselman et al. 2019) whereas the estimate from model v23 was much more in line with other Alaska groundfish at 0.52.   
Despite challenges to fitting the data, the model demonstrates good retrospective patterns.  Retrospective patterns are defined as “systematic changes to estimates of population size, or other assessment model-derived quantities, that occur as additional years of data are added to, or removed from, a stock assessment” (Hutado-Ferro et al. 2015).  They cause over- or underestimation of stock size, which can lead to flawed harvest recommendations or management advice. A positive retrospective pattern or bias can result in overestimation of stock biomass, which if persistent over many years, will result in the realized fishing mortality rate exceeding the target harvest policy (i.e., overfishing). Alternatively, a persistent negative retrospective pattern or bias will translate into foregone yields and fishing opportunity.  

### Retrospective analysis

Following recommendations from the North Pacific Fishery Management Council’s Groundfish Plan Team (Hanselman et al. 2013), a retrospective analysis was performed by dropping the last 10 years of data (i.e., “peels”), plotting spawning biomass, fishing mortality, and recruitment time series for each model run, and plotting the relative changes in reference to the terminal model (2022).  Mohn’s $\rho$ was calculated for spawning biomass, fishing mortality and recruitment such that 

$$
\begin{equation}
\text{Mohn's } \rho=\sum_{p=1}^{P}\frac{X_{Y-p,p}-X_{Y-p,0}}{Y_{Y-p,0}}/P
(\#eq:mohns)
\end{equation} 
$$
where *Y* is the last year in the full time series, *p* is the number of years at the end of the peeled data series, and *X* denotes the estimate of the quantity of interest (i.e., spawning biomass, fishing mortality or recruitment)(Mohn 1999; Hanselman et al. 2013).  

Model v23 demonstrates a small, positive bias in spawning biomass (Mohn’s $\rho$ = 0.05; Figure \@ref(fig:mohnsbiom)) and a slight negative bias in fishing mortality (Mohn’s $\rho$ = -0.03) that are well within the acceptable range for a long-lived groundfish species.  There is a larger positive bias in Age-2 recruits (Mohn’s $\rho$ = 0.10, Figure \@ref(fig:mohnsrec)), however, individual years may over or underestimate recruitment by up to 200%.  It should be noted that the model tends to overestimate recruitment when recruitment is low and underestimate recruitment when recruitment is high.  That is to say, in recent years that have shown clear signs of high recruitment, the model tends to underestimate those year classes. 

### Marking survey sensitivity analysis

The mark–recapture project has formed the foundation of sablefish management in NSEI since 2005 and the abundance estimate provides a snapshot of the exploitable abundance in NSEI (Figure \@ref(fig:abdind)D; Dressel 2009).  There are numerous shortcomings to the mark-recapture project which are detailed elsewhere in this report and the abundance estimates certainly overestimate precision and are likely biased to some unknown degree that likely varies in direction and strength over the course of the time series.  Due to budget constraints the mark-recapture project does not occur every year and uncertainty with future funding was part of the impetus for adopting the SCAA which is less reliant on yearly abundance estimates (Sullivan et al. 2020).  With the adoption of the SCAA model, an initial analysis was performed to determine the effects of performing the mark-recapture project every other, or every third year, and the model was found to perform adequately under those circumstances (Sullivan et al. 2020).  

There continues to be interest in possibly abandoning the mark-recapture project all together owing to it’s expense and the amount of staff time required to enact the project.  In this year’s assessment we examined simpler scenarios than examined by Sullivan et al. (2020) and simply dropped the last 5 and 10 year’s of mark-recapture data from the model to determine how ABC’s and spawning biomass would compare to the full data set with model v23 (Table \@ref(tab:modcomp)).  If there had been no mark-recapture project in the last 5 years, the maximum ABC and the estimated age-2 biomass would be 0.9% higher.  Had there been no mark-recapture project in the last 10 years, the maximum ABC would have been 12.1% higher and the estimated age-2 biomass would be 10.4% higher.  

These results, combined with Sullivan et al’s (2020) analysis, continue to demonstrate that this assessment will produce consistent results when the mark-recapture project is not performed every year.  However, it is important to note that the other indices of abundance, survey CPUE and fishery CPUE, fail to provide any scale to the population and the mark-recapture abundance estimate is the only data source that anchors the model to an estimate of true abundance.  If the mark-recapture project was completely abandoned, the assessment would not likely deprecate in the first several years, however, over time the estimates of biomass and associated biological reference points are likely to drift away from what the true biomass might be.  While it remains important to revisit the mark-recapture analysis to estimate and potentially correct biases in the abundance estimates, it is also important to recognize this data as a key piece of information for the assessment if time, staffing and funding remain available.  

## ABC RECOMMENDATIONS

The recommended ABC for 2023 is derived from an average of the recommended ABC from the base model and model v23.  Regardless of model choice, the population continues to expand with the growth and maturation of the 2013-2019 year classes.  Model v23 shows the population to be much closer to SB50 than does the base model (Figure \@ref(fig:Fsprcomp)) and using this model would result in an increase in the ABC of 3% from last year.  Given that the population is increasing, and the population is forecast to continue increasing in the next several years (albeit, at a slowing rate) we felt that in fairness to the fleet that averaging the two models was appropriate.  This may result in a small to negligible change in the ABC in the 2024 assessment as model development continues and model v23 becomes the base model for the next assessment.   

Model v23 results in a maximum permissible ABC (max ABC) of 1,486,406 round lbs at the target fully selected fishing mortality of $F_{50}$ (Table \@ref(tab:brps)).  This is a 43,092 round lb increase (3%) from the 2022 ABC of 1,443,314 round lbs.  The base model produces a max ABC of 1,873,598 round lbs (30% higher than last year’s max ABC) which under the max 15% change would have resulted in a recommended ABC of 1,659,811 round lbs (or a 15% increase).  Balancing model v23 with the base model and averaging the recommended ABC from the two models results in a recommended ABC of 1,573,109 round lbs, of a 9% increase from last year’s ABC.  Mortality from fishery releases under $F_{50}$, assuming fixed retention probabilities and a discard mortality of 0.16 is estimated to be 69,522 lbs in model v23 and 79,711 in the base model, which was included in the max ABC calculation (Table \@ref(tab:brps) and \@ref(tab:decrements)).  

While there is uncertainty in the absolute estimate of sablefish biomass in the NSEI, the population is undoubtedly increasing as the 2013-2018 year classes continue to grow and mature.  This trend is likely to continue over the next several years as these fish become fully mature and reach maximum size.  While this is certainly a good sign, it is important to note that the population remains below historical levels and that there is still a lack of older fish in the population.  Older females likely contribute disproportionally to the spawning output in the population and it remains desirable to maintain fishing pressure that allows the younger age classes to grow and mature.  

# FUTURE WORK AND RECOMENDATIONS

These tasks are viewed as the next steps in developing the SCAA:

<ol>
  <li> It is expected that participation in the pot fishery in 2023 will increase dramatically as it has in the SSEI and the federal fishery where pots have been legal for several years.  This will need to be monitored closely to see how catch rates and fish size varies between the longline and pot fisheries.  This issue will involve significant model development and will be of primary concern as the fleet changes fishing practices.
  <li> Develop methods to estimate fishery selectivity as this will make the model less dependent on federal values and the assumption that selectivity in the federal fishery mirrors that in the NSEI fishery.  Initial efforts to do this failed to produce converged numbers and reasonable estimates of selectivity.  Exploring the use of priors on the selectivity parameters, based on the federal estimates, may be an option.  Exploring time varying selectivity in both the fishery and the survey may also provide options that could improve the fit of age and length data.  
  <li> Review the mark-recapture analysis for two primary reasons: 
    <ul>
      <li> Determine if less biased estimates of abundance can be produced and by modelling size and geographic differences in capture probabilities, and
      <li> Determine the level of bias in the abundance estimates by comparing recapture rates between the longline survey and the fishery
    </ul>
  <li> Continue to develop proper data weighting for the model by 
    <ul>
      <li> using estimated uncertainty in the indices and allowing the model to estimate extra-uncertainty parameters, and
      <li> continuing to develop the Dirichlet data weighting of the age and length composition data.
    </ul>
  <li> Implement the SCAA model in a Bayesian framework. Preliminary work has been done using
the R library tmbstan (Monnahan and Kristensen 2018). The process is currently very slow;
the next steps include optimizing the NUTS algorithm using methods detailed in the
supplementary material of Monnahan and Kristensen (2018).
</ol>


# ACKNOWLEDGEMENTS

Many thanks to ADF&G Region I Groundfish Project staff who have collected NSEI sablefish data, maintained documentation, and worked to improve the conservation and management of this unique fishery. Additionally, we would like to thank the Age Determination Unit staff, including Kevin McNeel, Chris Hinds, and Catherine Mattson, who provide age data in a timely manner for stock assessments. We are grateful to Region I analyst/programmers, Karl Wood and Justin Daily, who provide database support and application development. Thans to Jane Sullivan, the original author of this model, who continues to offer insight and advice in developing this assessment.  We also wish to thank Curry Cunningham and his graduate student Matt Cheng with the University of Alaska Fairbanks, College of Fisheries and Ocean Science for their advice in developing this model. Finally, we are thankful to the NOAA scientists for their continued collaborations and sharing their sablefish knowledge.


 
# REFERENCES

Akaike, H. 1974. A new look at the statistical model identification. IEEE Transactions on Automatic Control 19:716–723.

Beder, A., J. Stahl. 2016. Northern Southeast Inside Commercial Sablefish Fishery and Survey Activities in Southeast Alaska, 2015. Alaska Department of Fish and Game, Fishery Management Report No. 15-27, Anchorage, Alaska.

Carlile, D. W., Richardson, B., Cartwright, M., and O'Connell, V.M. 2002. Southeast Alaska sablefish stock assessment activities 1988–2001, Alaska Department of Fish and Game, Division of Commercial Fisheries Juneau, Alaska.

Chapman, D. G. 1951. Some properties of the hypergeometric distribution with applications to zoological census. University of California Publications in Statistics 1:131–160. 

Depaoli, S., James P. Clifton, and Patrice R. Cobb. 2016. Just Another Gibbs Sampler (JAGS) Flexible Software for MCMC Implementation. Journal of Educational and Behavioral Statistics 41.6: 628-649.

Dressel, S.C. 2009. 2006 Northern Southeast Inside sablefish stock assessment and 2007 forecast and quota. Alaska Department of Fish and Game, Fishery Data Series No. 09-50, Anchorage, Alaska.

Echave, K. B., D. H. Hanselman, M. D. Adkison, M. F. Sigler. 2012. Inter-decadal changes in sablefish, Anoplopoma fimbria, growth in the northeast Pacific Ocean. Fish. Bull. 210:361-374.

Fournier, D. and C. P. Archibald. 1982. A general theory for analyzing catch at age data. Can. J. Fish. Aq. Sci. 39: 1195-1207.

Fournier, D. A., H. J. Skaug, J. Ancheta, J. Ianelli, A. Magnusson, M.N. Maunder, A. Nielsen, and J. Sibert. 2012. AD Model Builder: using automatic differentiation for statistical inference of highly parameterized complex nonlinear models. Optim. Methods Softw. 27, 233-249.

Francis, R. I. C. C., 2011. Data weighting in statistical fisheries stock assessment models. Can. J. Fish. Aquat. Sci. 68, 1124–1138.

Goethel, D. R., C. J. Rodgveller, K. B. Echave, S. K. Shotmwell, K. A. Siwicke, D. Hanselman, P. W. Malecha, M. Cheng, M. Williams, K. Omori, and C. R. Lunsford.  2022.  Chapter 3: Assessment of the sablefish stock in Alaska. In: Stock assessment and fishery evaluation report for the groundfish resources of the GOA and BS/AI as projected for 2023. North Pacific Fishery Management Council, 605 W 4th Ave, Suite 306 Anchorage, AK 99501.

Hanselman, D. H., C. J. Rodgveller, K. H. Fenske, S. K. Shotwell, K. B. Echave, P. W. Malecha, and C. R. Lunsford. 2018. Chapter 3: Assessment of the sablefish stock in Alaska. In: Stock assessment and fishery evaluation report for the groundfish resources of the GOA and BS/AI as projected for 2019. North Pacific Fishery Management Council, 605 W 4th Ave, Suite 306 Anchorage, AK 99501.

Hanselman, D. H., C. J. Rodgveller, C. R. Lunsford, and K. H Fenske. 2017. Chapter 3: Assessment of the sablefish stock in Alaska. In: Stock assessment and fishery evaluation report for the groundfish resources of the GOA and BS/AI as projected for 2018. North Pacific Fishery Management Council, 605 W 4th Ave, Suite 306 Anchorage, AK 99501.

Heifetz, J., D. Anderl, N.E. Maloney, and T.L. Rutecki. 1999. Age validation and analysis of ageing error from marked and recaptured sablefish, Anoplopoma fimbria. Fish. Bull. 97: 256-263.

Johnson, S. L., and T. J. Quinn II. 1988. Catch-Age Analysis with Auxiliary Information of sablefish in the Gulf of Alaska. Contract report to National Marine Fisheries Service, Auke Bay, Alaska. 79 pp. Center for Fisheries and Ocean Sciences, University of Alaska, Juneau, Alaska.

Kimura, D. K. 1990. Approaches to age-structured separable sequential population analysis. Can. J. Fish. Aquat. Sci. 47: 2364-2374.

Kristensen, K., A. Nielsen, C. W. Berg, H. Skaug, B. M. Bell. 2016. TMB: Automatic Differentiation and Laplace Approximation. Journal of Statistical Software, 70(5), 1-21.<doi:10.18637/jss.v070.i05>.

McAllister, M. K., Ianelli, J. N., 1997. Bayesian stock assessment using catch-age data and the sampling: importance resampling algorithm. Can. J. Fish. Aquat. Sci. 54,284–300.

Mueter, F. 2010. Evaluation of stock assessment and modeling options to assess sablefish population levels and status in the Northern Southeast Inside (NSEI) management area. Alaska Department of Fish and Game, Special Publication No. 10-01, Anchorage, Alaska.

Sigler, M. F. 1993. Stock assessment and management of sablefish Anoplopoma fimbria in the Gulf of Alaska. PhD Dissertation. University of Washington. 188 pp.

Sigler, M. F., 1999. Estimation of sablefish, Anoplopoma fimbria, abundance off Alaska with an age-structured population model. Fishery Bulletin, 97: 591-603.

Sigler, M. F., C. R. Lunsford, J. T. Fujioka, and S. A. Lowe. 2002. Alaska sablefish assessment for 2003. In Stock assessment and fishery evaluation report for the groundfish fisheries of the Bering Sea and Aleutian Islands. pp. 449-514. North Pacific Fishery Management Council, 605 W 4th Avenue, Suite 306, Anchorage, AK 99510.

Sullivan, J., B. Williams, and A. Olson. 2018. 2018 NSEI Sablefish Assessment. State of Alaska, Department of Fish and Game, Division of Commercial Fisheries Memorandum. June 20, 2018.

Thorson, J. T., Johnson, K. F., Methot, R. D., & Taylor, I. G. 2017. Model-based estimates of effective sample size in stock assessment models using the Dirichlet-multinomial distribution. Fisheries Research, 192, 84-93.

Williams, B., and K. Van Kirk. 2017. 2017 NSEI Sablefish Assessment. State of Alaska, Department of Fish and Game, Division of Commercial Fisheries Memorandum. March 16, 2017.

Wood, S. N. 2011. Fast stable restricted maximum likelihood and marginal likelihood estimation of semiparametric generalized linear models. Journal of the Royal Statistical Society (B) 73(1):3-36. 

\newpage
# TABLES

<br><br><br>

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Annual harvest objective (round lb), equal quota share (round lb), reported harvest (round lb), exvessel value, numberof permits, and effort (dats) for the directed commercial Northern Southeast Inside (NSEI) Subdistrict sablefish fishery, 1985-2022.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Year </th>
   <th style="text-align:center;"> Annual harvest objective </th>
   <th style="text-align:center;"> Equal quota share </th>
   <th style="text-align:center;"> Harvest </th>
   <th style="text-align:center;"> Exvessel value (mil) </th>
   <th style="text-align:center;"> No. of permits </th>
   <th style="text-align:center;"> No. of days </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1985 </td>
   <td style="text-align:center;"> 2,380,952 </td>
   <td style="text-align:center;"> NA </td>
   <td style="text-align:center;"> 2,951,056 </td>
   <td style="text-align:center;"> $2 </td>
   <td style="text-align:center;"> 105 </td>
   <td style="text-align:center;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1986 </td>
   <td style="text-align:center;"> 2,380,952 </td>
   <td style="text-align:center;"> NA </td>
   <td style="text-align:center;"> 3,874,269 </td>
   <td style="text-align:center;"> $2.9 </td>
   <td style="text-align:center;"> 138 </td>
   <td style="text-align:center;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1987 </td>
   <td style="text-align:center;"> 2,380,952 </td>
   <td style="text-align:center;"> NA </td>
   <td style="text-align:center;"> 3,861,546 </td>
   <td style="text-align:center;"> $3.4 </td>
   <td style="text-align:center;"> 158 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1988 </td>
   <td style="text-align:center;"> 2,380,952 </td>
   <td style="text-align:center;"> NA </td>
   <td style="text-align:center;"> 4,196,601 </td>
   <td style="text-align:center;"> $4.4 </td>
   <td style="text-align:center;"> 149 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1989 </td>
   <td style="text-align:center;"> 2,380,952 </td>
   <td style="text-align:center;"> NA </td>
   <td style="text-align:center;"> 3,767,518 </td>
   <td style="text-align:center;"> $3.5 </td>
   <td style="text-align:center;"> 151 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1990 </td>
   <td style="text-align:center;"> 2,380,952 </td>
   <td style="text-align:center;"> NA </td>
   <td style="text-align:center;"> 3,254,262 </td>
   <td style="text-align:center;"> $3.1 </td>
   <td style="text-align:center;"> 120 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1991 </td>
   <td style="text-align:center;"> 2,380,952 </td>
   <td style="text-align:center;"> NA </td>
   <td style="text-align:center;"> 3,955,189 </td>
   <td style="text-align:center;"> $5.5 </td>
   <td style="text-align:center;"> 127 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1992 </td>
   <td style="text-align:center;"> 2,380,952 </td>
   <td style="text-align:center;"> NA </td>
   <td style="text-align:center;"> 4,267,781 </td>
   <td style="text-align:center;"> $5.4 </td>
   <td style="text-align:center;"> 115 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1993 </td>
   <td style="text-align:center;"> 2,380,952 </td>
   <td style="text-align:center;"> NA </td>
   <td style="text-align:center;"> 5,795,974 </td>
   <td style="text-align:center;"> $6.6 </td>
   <td style="text-align:center;"> 120 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1994 </td>
   <td style="text-align:center;"> 4,761,905 </td>
   <td style="text-align:center;"> 38,889 </td>
   <td style="text-align:center;"> 4,708,584 </td>
   <td style="text-align:center;"> $8.1 </td>
   <td style="text-align:center;"> 121 </td>
   <td style="text-align:center;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1995 </td>
   <td style="text-align:center;"> 4,761,905 </td>
   <td style="text-align:center;"> 38,889 </td>
   <td style="text-align:center;"> 4,543,272 </td>
   <td style="text-align:center;"> $9 </td>
   <td style="text-align:center;"> 121 </td>
   <td style="text-align:center;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1996 </td>
   <td style="text-align:center;"> 4,761,905 </td>
   <td style="text-align:center;"> 38,889 </td>
   <td style="text-align:center;"> 4,676,032 </td>
   <td style="text-align:center;"> $10.1 </td>
   <td style="text-align:center;"> 122 </td>
   <td style="text-align:center;"> 61 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1997 </td>
   <td style="text-align:center;"> 4,800,000 </td>
   <td style="text-align:center;"> 39,300 </td>
   <td style="text-align:center;"> 4,752,285 </td>
   <td style="text-align:center;"> $12.2 </td>
   <td style="text-align:center;"> 122 </td>
   <td style="text-align:center;"> 76 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1998 </td>
   <td style="text-align:center;"> 4,800,000 </td>
   <td style="text-align:center;"> 41,700 </td>
   <td style="text-align:center;"> 4,689,713 </td>
   <td style="text-align:center;"> $7.4 </td>
   <td style="text-align:center;"> 116 </td>
   <td style="text-align:center;"> 76 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1999 </td>
   <td style="text-align:center;"> 3,120,000 </td>
   <td style="text-align:center;"> 28,000 </td>
   <td style="text-align:center;"> 3,043,272 </td>
   <td style="text-align:center;"> $6.5 </td>
   <td style="text-align:center;"> 112 </td>
   <td style="text-align:center;"> 76 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2000 </td>
   <td style="text-align:center;"> 3,120,000 </td>
   <td style="text-align:center;"> 28,600 </td>
   <td style="text-align:center;"> 3,081,797 </td>
   <td style="text-align:center;"> $8.6 </td>
   <td style="text-align:center;"> 111 </td>
   <td style="text-align:center;"> 76 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2001 </td>
   <td style="text-align:center;"> 2,184,000 </td>
   <td style="text-align:center;"> 19,600 </td>
   <td style="text-align:center;"> 2,142,619 </td>
   <td style="text-align:center;"> $4.6 </td>
   <td style="text-align:center;"> 111 </td>
   <td style="text-align:center;"> 76 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2002 </td>
   <td style="text-align:center;"> 2,005,000 </td>
   <td style="text-align:center;"> 18,400 </td>
   <td style="text-align:center;"> 2,009,379 </td>
   <td style="text-align:center;"> $5.3 </td>
   <td style="text-align:center;"> 109 </td>
   <td style="text-align:center;"> 76 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2003 </td>
   <td style="text-align:center;"> 2,005,000 </td>
   <td style="text-align:center;"> 18,565 </td>
   <td style="text-align:center;"> 2,003,083 </td>
   <td style="text-align:center;"> $4.8 </td>
   <td style="text-align:center;"> 108 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2004 </td>
   <td style="text-align:center;"> 2,245,000 </td>
   <td style="text-align:center;"> 20,787 </td>
   <td style="text-align:center;"> 2,230,396 </td>
   <td style="text-align:center;"> $4.6 </td>
   <td style="text-align:center;"> 108 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2005 </td>
   <td style="text-align:center;"> 2,053,000 </td>
   <td style="text-align:center;"> 19,400 </td>
   <td style="text-align:center;"> 2,027,187 </td>
   <td style="text-align:center;"> $5 </td>
   <td style="text-align:center;"> 106 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2006 </td>
   <td style="text-align:center;"> 2,053,000 </td>
   <td style="text-align:center;"> 19,550 </td>
   <td style="text-align:center;"> 2,031,227 </td>
   <td style="text-align:center;"> $5.1 </td>
   <td style="text-align:center;"> 105 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2007 </td>
   <td style="text-align:center;"> 1,488,000 </td>
   <td style="text-align:center;"> 14,500 </td>
   <td style="text-align:center;"> 1,501,483 </td>
   <td style="text-align:center;"> $3.7 </td>
   <td style="text-align:center;"> 103 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2008 </td>
   <td style="text-align:center;"> 1,508,000 </td>
   <td style="text-align:center;"> 15,710 </td>
   <td style="text-align:center;"> 1,513,043 </td>
   <td style="text-align:center;"> $4.4 </td>
   <td style="text-align:center;"> 96 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2009 </td>
   <td style="text-align:center;"> 1,071,000 </td>
   <td style="text-align:center;"> 12,170 </td>
   <td style="text-align:center;"> 1,069,217 </td>
   <td style="text-align:center;"> $3.3 </td>
   <td style="text-align:center;"> 88 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2010 </td>
   <td style="text-align:center;"> 1,063,000 </td>
   <td style="text-align:center;"> 12,218 </td>
   <td style="text-align:center;"> 1,054,279 </td>
   <td style="text-align:center;"> $3.8 </td>
   <td style="text-align:center;"> 87 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2011 </td>
   <td style="text-align:center;"> 880,000 </td>
   <td style="text-align:center;"> 10,602 </td>
   <td style="text-align:center;"> 882,777 </td>
   <td style="text-align:center;"> $4.4 </td>
   <td style="text-align:center;"> 83 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012 </td>
   <td style="text-align:center;"> 975,000 </td>
   <td style="text-align:center;"> 12,342 </td>
   <td style="text-align:center;"> 969,775 </td>
   <td style="text-align:center;"> $3.9 </td>
   <td style="text-align:center;"> 79 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2013 </td>
   <td style="text-align:center;"> 1,002,162 </td>
   <td style="text-align:center;"> 12,848 </td>
   <td style="text-align:center;"> 972,740 </td>
   <td style="text-align:center;"> $2.6 </td>
   <td style="text-align:center;"> 78 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2014 </td>
   <td style="text-align:center;"> 745,774 </td>
   <td style="text-align:center;"> 9,561 </td>
   <td style="text-align:center;"> 773,534 </td>
   <td style="text-align:center;"> $2.7 </td>
   <td style="text-align:center;"> 78 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:center;"> 786,748 </td>
   <td style="text-align:center;"> 10,087 </td>
   <td style="text-align:center;"> 781,702 </td>
   <td style="text-align:center;"> $3.1 </td>
   <td style="text-align:center;"> 78 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2016 </td>
   <td style="text-align:center;"> 650,754 </td>
   <td style="text-align:center;"> 8,343 </td>
   <td style="text-align:center;"> 646,329 </td>
   <td style="text-align:center;"> $2.8 </td>
   <td style="text-align:center;"> 78 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017 </td>
   <td style="text-align:center;"> 720,250 </td>
   <td style="text-align:center;"> 9,234 </td>
   <td style="text-align:center;"> 714,404 </td>
   <td style="text-align:center;"> $3.6 </td>
   <td style="text-align:center;"> 78 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018 </td>
   <td style="text-align:center;"> 855,416 </td>
   <td style="text-align:center;"> 10,967 </td>
   <td style="text-align:center;"> 855,600 </td>
   <td style="text-align:center;"> $4.2 </td>
   <td style="text-align:center;"> 78 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019 </td>
   <td style="text-align:center;"> 920,093 </td>
   <td style="text-align:center;"> 11,796 </td>
   <td style="text-align:center;"> 909,341 </td>
   <td style="text-align:center;"> $4 </td>
   <td style="text-align:center;"> 78 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020 </td>
   <td style="text-align:center;"> 1,108,003 </td>
   <td style="text-align:center;"> 14,773 </td>
   <td style="text-align:center;"> 1,101,091 </td>
   <td style="text-align:center;"> $3.1 </td>
   <td style="text-align:center;"> 75 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021 </td>
   <td style="text-align:center;"> 1,137,867 </td>
   <td style="text-align:center;"> 15,587 </td>
   <td style="text-align:center;"> 1,083,363 </td>
   <td style="text-align:center;"> $2.8 </td>
   <td style="text-align:center;"> 73 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022 </td>
   <td style="text-align:center;"> 1,233,633 </td>
   <td style="text-align:center;"> 16,899 </td>
   <td style="text-align:center;"> 1,182,518 </td>
   <td style="text-align:center;"> $3.6 </td>
   <td style="text-align:center;"> 71 </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023 </td>
   <td style="text-align:center;"> 1,300,000 </td>
   <td style="text-align:center;"> 17,000 </td>
   <td style="text-align:center;"> NA </td>
   <td style="text-align:center;"> $NA </td>
   <td style="text-align:center;"> NA </td>
   <td style="text-align:center;"> 93 </td>
  </tr>
</tbody>
</table>
<br><br><br>

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Summary of key assessment results used to inform management in 2022 and 2023.  The table includes the estimates of projected biomass (sablefish aged 2 years and above) and female spawning stock biomass, estimated biological reference points of unfished female spawning biomass ($SB_{100}$), female spawning bioass at 50% of unfished levels ($SB_{50}$), and the maximum target fishing mortality of *$F_{50}$*.  Additional values include the maximum permissible Acceptable Biological Catch (max ABC) defined by *$F_{50}$*, the estimates of mortality from fishery releases that would result under max ABC and a discard mortality rate of 016, and the recommended ABC under the max 15% change management proceedure</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Quantity/Status </th>
   <th style="text-align:center;"> 2022 </th>
   <th style="text-align:center;"> 2023 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Projected age-2 biomass (lb) </td>
   <td style="text-align:center;"> 51,885,665 </td>
   <td style="text-align:center;"> 51,975,426 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Projected female spawning biomass (lb) </td>
   <td style="text-align:center;"> 19,714,244 </td>
   <td style="text-align:center;"> 19,836,111 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Unfished female spawning biomass ($SB_{100}$, lb) </td>
   <td style="text-align:center;"> 28,995,917 </td>
   <td style="text-align:center;"> 28,434,171 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Equilibrium female spawning biomass ($SB_{50}$, lb) </td>
   <td style="text-align:center;"> 14,497,958 </td>
   <td style="text-align:center;"> 14,217,085 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> max $F_{ABC}$ = $F_{50}$ </td>
   <td style="text-align:center;"> 0.062 </td>
   <td style="text-align:center;"> 0.059 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Recommended $F_{ABC}$ </td>
   <td style="text-align:center;"> 0.056 </td>
   <td style="text-align:center;"> 0.059 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mortality from fishery discards under max ABC (lb) </td>
   <td style="text-align:center;"> 72,190 </td>
   <td style="text-align:center;"> 69,522 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Max ABC (lb) </td>
   <td style="text-align:center;"> 1,595,932 </td>
   <td style="text-align:center;"> 1,486,406 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Recommended ABC (lb) </td>
   <td style="text-align:center;"> 1,443,314 </td>
   <td style="text-align:center;"> 1,573,109 </td>
  </tr>
</tbody>
</table>
<br><br><br>

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Decrement types and amounts, 2017-2023.  Estimated catch in round pounds of sablefish.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Year </th>
   <th style="text-align:right;"> 2018 </th>
   <th style="text-align:right;"> 2019 </th>
   <th style="text-align:right;"> 2020 </th>
   <th style="text-align:right;"> 2021 </th>
   <th style="text-align:right;"> 2022 </th>
   <th style="text-align:right;"> 2023 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Acceptable biological catch </td>
   <td style="text-align:right;"> 965,354 </td>
   <td style="text-align:right;"> 1,058,037 </td>
   <td style="text-align:right;"> 1,216,743 </td>
   <td style="text-align:right;"> 1,255,056 </td>
   <td style="text-align:right;"> 1,443,314 </td>
   <td style="text-align:right;"> 1,573,109 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Decrement Type (round lb) </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bycatch mortality in halibut fishery$^a$ </td>
   <td style="text-align:right;"> 19,583 </td>
   <td style="text-align:right;"> 18,434 </td>
   <td style="text-align:right;"> 16,207 </td>
   <td style="text-align:right;"> 38,124 </td>
   <td style="text-align:right;"> 35,406 </td>
   <td style="text-align:right;"> 35,445 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ADF&amp;G longline survey removal decrement (excluding catch retained by permit holders for their equal quota share$)^a$ </td>
   <td style="text-align:right;"> 15,875 </td>
   <td style="text-align:right;"> 26,260 </td>
   <td style="text-align:right;"> 24,698 </td>
   <td style="text-align:right;"> 42,499 </td>
   <td style="text-align:right;"> 95,502 </td>
   <td style="text-align:right;"> 75,636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Guided sport fish harvest$^b$ </td>
   <td style="text-align:right;"> 41,179 </td>
   <td style="text-align:right;"> 33,135 </td>
   <td style="text-align:right;"> 35,004 </td>
   <td style="text-align:right;"> 753 </td>
   <td style="text-align:right;"> 33,990 </td>
   <td style="text-align:right;"> 34,395 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Unguided sport fish harvest$^b$ </td>
   <td style="text-align:right;"> 5,872 </td>
   <td style="text-align:right;"> 11,340 </td>
   <td style="text-align:right;"> 5,280 </td>
   <td style="text-align:right;"> 5,631 </td>
   <td style="text-align:right;"> 9,846 </td>
   <td style="text-align:right;"> 2,655 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mortality from fishery deadloss$^a$ </td>
   <td style="text-align:right;"> 5,699 </td>
   <td style="text-align:right;"> 8,046 </td>
   <td style="text-align:right;"> 9,729 </td>
   <td style="text-align:right;"> 10,888 </td>
   <td style="text-align:right;"> 11,085 </td>
   <td style="text-align:right;"> 9,467 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mortality from fishery releases$^a$ </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> 19,142 </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Subsistence and personal use harvest$^b$ </td>
   <td style="text-align:right;"> 21,730 </td>
   <td style="text-align:right;"> 21,587 </td>
   <td style="text-align:right;"> 17,821 </td>
   <td style="text-align:right;"> 19,295 </td>
   <td style="text-align:right;"> 23,852 </td>
   <td style="text-align:right;"> 18,643 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Total decrements </td>
   <td style="text-align:right;"> 109,938 </td>
   <td style="text-align:right;"> 137,944 </td>
   <td style="text-align:right;"> 108,740 </td>
   <td style="text-align:right;"> 117,189 </td>
   <td style="text-align:right;"> 209,681 </td>
   <td style="text-align:right;"> 177,241 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Annual harvest objective </td>
   <td style="text-align:right;"> 855,416 </td>
   <td style="text-align:right;"> 920,093 </td>
   <td style="text-align:right;"> 1,108,003 </td>
   <td style="text-align:right;"> 1,137,867 </td>
   <td style="text-align:right;"> 1,233,633 </td>
   <td style="text-align:right;"> 1,395,868 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Permit holders </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 75 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 73 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Equal quota share </td>
   <td style="text-align:right;"> 10,967 </td>
   <td style="text-align:right;"> 11,796 </td>
   <td style="text-align:right;"> 14,773 </td>
   <td style="text-align:right;"> 15,587 </td>
   <td style="text-align:right;"> 16,899 </td>
   <td style="text-align:right;"> 19,121 </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>a</sup> Projected estimate of mortality for the current season.</td>
</tr>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup>b</sup> Estimate of mortality that occurred during the previous season and is applied as decrement for the current season.</td>
</tr>
</tfoot>
</table>
<br><br><br>

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>A comparison for biological reference points from candidate models in the 2023 assessment.  The base model refers to the model used in the prior assessment.  The 'tuned' model refers to the base model with age and length compositioned tuned via McAllister and Ianelli (1997) methodology.  Progression from that model adds the updated fishery selectivity curves from the federal assessment while v23 model modifications are described in this document.  The v23 no MR 5 and 10 refer to models where mark-recapture abundance estimates were dropped in the last 5 and last 10 years, respectivly,</caption>
 <thead>
  <tr>
   <th style="text-align:left;">  </th>
   <th style="text-align:right;"> Base </th>
   <th style="text-align:right;"> Tuned base </th>
   <th style="text-align:right;"> Tuned base w/ new selectivity </th>
   <th style="text-align:right;"> v23 </th>
   <th style="text-align:right;"> v23 no MR in last 5 yrs </th>
   <th style="text-align:right;"> v23 no MR in last 10 yrs </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Number of parameters: </td>
   <td style="text-align:right;"> 136 </td>
   <td style="text-align:right;"> 136 </td>
   <td style="text-align:right;"> 136 </td>
   <td style="text-align:right;"> 146 </td>
   <td style="text-align:right;"> 146 </td>
   <td style="text-align:right;"> 146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Negative log likelihood: </td>
   <td style="text-align:right;"> 2,396 </td>
   <td style="text-align:right;"> 2,396 </td>
   <td style="text-align:right;"> 6,919 </td>
   <td style="text-align:right;"> 1,799 </td>
   <td style="text-align:right;"> 1,791 </td>
   <td style="text-align:right;"> 1,747 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Maximum gradient component: </td>
   <td style="text-align:right;"> 1.83e-06 </td>
   <td style="text-align:right;"> 1.40e-10 </td>
   <td style="text-align:right;"> 6.66e-12 </td>
   <td style="text-align:right;"> 3.32e-12 </td>
   <td style="text-align:right;"> 1.59e-07 </td>
   <td style="text-align:right;"> 1.54e-11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Projected age-2 biomass: </td>
   <td style="text-align:right;"> 61,145,122 </td>
   <td style="text-align:right;"> 71,385,368 </td>
   <td style="text-align:right;"> 66,676,302 </td>
   <td style="text-align:right;"> 51,975,427 </td>
   <td style="text-align:right;"> 52,424,435 </td>
   <td style="text-align:right;"> 57,371,915 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Projected female spawning biomass: </td>
   <td style="text-align:right;"> 23,441,266 </td>
   <td style="text-align:right;"> 27,128,399 </td>
   <td style="text-align:right;"> 26,271,709 </td>
   <td style="text-align:right;"> 19,836,112 </td>
   <td style="text-align:right;"> 20,020,673 </td>
   <td style="text-align:right;"> 22,208,439 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Unfished equilibrium female spawning biomass (SPR = 100): </td>
   <td style="text-align:right;"> 30,866,389 </td>
   <td style="text-align:right;"> 32,930,727 </td>
   <td style="text-align:right;"> 33,033,358 </td>
   <td style="text-align:right;"> 28,434,171 </td>
   <td style="text-align:right;"> 28,527,598 </td>
   <td style="text-align:right;"> 30,309,066 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Equilibrium female spawning biomass under$F_{50}$ (SPR = 50): </td>
   <td style="text-align:right;"> 15,433,194 </td>
   <td style="text-align:right;"> 16,465,363 </td>
   <td style="text-align:right;"> 16,516,679 </td>
   <td style="text-align:right;"> 14,217,086 </td>
   <td style="text-align:right;"> 14,263,799 </td>
   <td style="text-align:right;"> 15,154,533 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Max ABC: </td>
   <td style="text-align:right;"> 1,873,598 </td>
   <td style="text-align:right;"> 2,152,761 </td>
   <td style="text-align:right;"> 1,983,085 </td>
   <td style="text-align:right;"> 1,486,406 </td>
   <td style="text-align:right;"> 1,499,490 </td>
   <td style="text-align:right;"> 1,666,358 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Recommended ABC: </td>
   <td style="text-align:right;"> 1,659,811 </td>
   <td style="text-align:right;"> 1,659,811 </td>
   <td style="text-align:right;"> 1,659,811 </td>
   <td style="text-align:right;"> 1,486,406 </td>
   <td style="text-align:right;"> 1,499,490 </td>
   <td style="text-align:right;"> 1,659,811 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mortality from fishery discards under max ABC: </td>
   <td style="text-align:right;"> 79,711 </td>
   <td style="text-align:right;"> 91,383 </td>
   <td style="text-align:right;"> 82,775 </td>
   <td style="text-align:right;"> 69,522 </td>
   <td style="text-align:right;"> 70,182 </td>
   <td style="text-align:right;"> 75,426 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> max $F_{ABC}$ = $F_{50}$: </td>
   <td style="text-align:right;"> 0.063 </td>
   <td style="text-align:right;"> 0.0626 </td>
   <td style="text-align:right;"> 0.059 </td>
   <td style="text-align:right;"> 0.0591 </td>
   <td style="text-align:right;"> 0.0591 </td>
   <td style="text-align:right;"> 0.059 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F under recommended ABC: </td>
   <td style="text-align:right;"> 0.056 </td>
   <td style="text-align:right;"> 0.0483 </td>
   <td style="text-align:right;"> 0.049 </td>
   <td style="text-align:right;"> 0.0591 </td>
   <td style="text-align:right;"> 0.0591 </td>
   <td style="text-align:right;"> 0.0588 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;">  </td>
  </tr>
</tbody>
</table>
<br><br><br>

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Variable definitions for the statistical catch-at-age model.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:left;"> Definition </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="12"><td colspan="2" style="border-bottom: 1px solid;"><strong>$\textit{Indexing and model dimensions}$</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $T$ </td>
   <td style="text-align:left;"> Number of years in the model </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $t$ </td>
   <td style="text-align:left;"> Index for year in model equations </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $A$ </td>
   <td style="text-align:left;"> Number of ages in the model </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $a$ </td>
   <td style="text-align:left;"> Index for age in model equations </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $a_0$ </td>
   <td style="text-align:left;"> Recruitment age (age-2) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $a_{+}$ </td>
   <td style="text-align:left;"> Plus group age (age-31) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $l$ </td>
   <td style="text-align:left;"> Index for length bin in model equations </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $l_0$ </td>
   <td style="text-align:left;"> Recruitment length bin (41 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $l_{+}$ </td>
   <td style="text-align:left;"> Plus group length bin (99 cm) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $fsh$ </td>
   <td style="text-align:left;"> NSEI longline fishery </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $srv$ </td>
   <td style="text-align:left;"> ADF\&amp;G longline survey </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $MR$ </td>
   <td style="text-align:left;"> Mark-recapture abundance </td>
  </tr>
  <tr grouplength="17"><td colspan="2" style="border-bottom: 1px solid;"><strong>$\textit{Parameters}$</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $M$ </td>
   <td style="text-align:left;"> Instantaneous natural mortality </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $F$ </td>
   <td style="text-align:left;"> Instantaneous fishing mortality </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $Z$ </td>
   <td style="text-align:left;"> Total instantaneous mortality </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $S$ </td>
   <td style="text-align:left;"> Total annual survival </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $D$ </td>
   <td style="text-align:left;"> Discard mortality </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $s_{50}$ </td>
   <td style="text-align:left;"> Age at which 50\% of individuals are selected to the gear </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $s_{95}$ </td>
   <td style="text-align:left;"> Age at which 95\% of individuals are selected to the gear </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $\delta$ </td>
   <td style="text-align:left;"> Slope parameter in the logistic selectivity curve </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $q$ </td>
   <td style="text-align:left;"> Catchability </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $\mu_R$ </td>
   <td style="text-align:left;"> Mean log recruitment </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $\tau_t$ </td>
   <td style="text-align:left;"> Log recruitment deviations </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $\mu_N$ </td>
   <td style="text-align:left;"> Mean log initial numbers-at-age </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $\psi_a$ </td>
   <td style="text-align:left;"> Log deviations of initial numbers-at-age </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $\sigma_R$ </td>
   <td style="text-align:left;"> Variability in recruitment and initial numbers-at-age </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $\mu_F$ </td>
   <td style="text-align:left;"> Mean log fishing mortality </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $\phi_t$ </td>
   <td style="text-align:left;"> Log fishing mortality deviations </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $\theta$ </td>
   <td style="text-align:left;"> Dirichlet-multinomial parameter related to effective sample size </td>
  </tr>
  <tr grouplength="19"><td colspan="2" style="border-bottom: 1px solid;"><strong>$\textit{Data and predicted variables}$</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $w_a$ </td>
   <td style="text-align:left;"> Weight-at-age </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $p_a$ </td>
   <td style="text-align:left;"> Proportion mature-at-age </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $r_a$ </td>
   <td style="text-align:left;"> Proportion female-at-age </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $R$ </td>
   <td style="text-align:left;"> Retention probability </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $s_a$ </td>
   <td style="text-align:left;"> Selectivity-at-age </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $\Omega_{a',a}$ </td>
   <td style="text-align:left;"> Ageing error matrix (proportion observed at age given the true age $a'$) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $\Lambda_{a,l,k}$ </td>
   <td style="text-align:left;"> Age-length key (proportion in length bin given age and sex) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $N$ </td>
   <td style="text-align:left;"> Numbers-at-age </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $C$ </td>
   <td style="text-align:left;"> Landed catch in numbers-at-age </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $I$, $\hat{I}$ </td>
   <td style="text-align:left;"> Indices of abundance, $\hat{I}$ are predicted values </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $P_a$, $\hat{P}_a$ </td>
   <td style="text-align:left;"> Age compositions, $\hat{P}_a$ are predicted values </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $P_l$, $\hat{P}_l$ </td>
   <td style="text-align:left;"> Length compositions, $\hat{P}_l$ are predicted values </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $Y$, $\hat{Y}$ </td>
   <td style="text-align:left;"> Landed catch biomass, $\hat{Y}$ are predicted values </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $\hat{W}$ </td>
   <td style="text-align:left;"> Estimated mortality from discards (biomass) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $\lambda$ </td>
   <td style="text-align:left;"> Weight for likelihood component </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $L$ </td>
   <td style="text-align:left;"> Likelihood </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $\omega$ </td>
   <td style="text-align:left;"> Effective sample size for age and length compositions </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $n$ </td>
   <td style="text-align:left;"> Input sample size for Dirichlet-multinomial likelihood </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> $c$ </td>
   <td style="text-align:left;"> Small constant (0.00001) </td>
  </tr>
</tbody>
</table>

<br><br><br>

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>A summary of data inputs to the mark-recapture models, including total individuals tagged (*K*), the total number of tags remaining once size selectivity is accounted for ($K_0$), tags not available to the longline survey or fishery (captured in ther fisheries or outside Chatham, $D_0$), recaptured individuals in the lpongline survey and fishery ($k_{srv}$ and $k_{fsh}$), number of sampled individuals in the longline survey and fishery ($n_{srv}$ and $n_{fsh}$), tags not available to the fishery (captured outside Chatham or in other fisheries during the survey, $D_{srv}$, and tags recaptred in other fisheries or outside Chatham during the fishery ($D_{fsh}$) for years with a tagging survey, 2005-2023.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Year </th>
   <th style="text-align:right;"> $K$ </th>
   <th style="text-align:right;"> $K_0$ </th>
   <th style="text-align:right;"> $D_0$ </th>
   <th style="text-align:right;"> $k_{srv}$ </th>
   <th style="text-align:right;"> $n_{srv}$ </th>
   <th style="text-align:right;"> $D_{srv}$ </th>
   <th style="text-align:right;"> $k_{fsh}$ </th>
   <th style="text-align:right;"> $n_{fsh}$ </th>
   <th style="text-align:right;"> $D_{fsh}$ </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2005 </td>
   <td style="text-align:right;"> 7,118 </td>
   <td style="text-align:right;"> 7,118 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 104 </td>
   <td style="text-align:right;"> 690 </td>
   <td style="text-align:right;"> 180,999 </td>
   <td style="text-align:right;"> 189 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2006 </td>
   <td style="text-align:right;"> 5,325 </td>
   <td style="text-align:right;"> 5,325 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 503 </td>
   <td style="text-align:right;"> 203,878 </td>
   <td style="text-align:right;"> 123 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2007 </td>
   <td style="text-align:right;"> 6,158 </td>
   <td style="text-align:right;"> 6,055 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 43 </td>
   <td style="text-align:right;"> 335 </td>
   <td style="text-align:right;"> 150,729 </td>
   <td style="text-align:right;"> 77 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2008 </td>
   <td style="text-align:right;"> 5,450 </td>
   <td style="text-align:right;"> 5,412 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 15,319 </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 431 </td>
   <td style="text-align:right;"> 156,313 </td>
   <td style="text-align:right;"> 104 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2009 </td>
   <td style="text-align:right;"> 7,071 </td>
   <td style="text-align:right;"> 7,054 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 285 </td>
   <td style="text-align:right;"> 105,709 </td>
   <td style="text-align:right;"> 92 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2010 </td>
   <td style="text-align:right;"> 7,443 </td>
   <td style="text-align:right;"> 7,307 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 14,765 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 331 </td>
   <td style="text-align:right;"> 106,201 </td>
   <td style="text-align:right;"> 38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012 </td>
   <td style="text-align:right;"> 7,582 </td>
   <td style="text-align:right;"> 7,548 </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 380 </td>
   <td style="text-align:right;"> 97,134 </td>
   <td style="text-align:right;"> 72 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2013 </td>
   <td style="text-align:right;"> 7,961 </td>
   <td style="text-align:right;"> 7,921 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 89 </td>
   <td style="text-align:right;"> 374 </td>
   <td style="text-align:right;"> 99,286 </td>
   <td style="text-align:right;"> 113 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:right;"> 6,862 </td>
   <td style="text-align:right;"> 6,765 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 242 </td>
   <td style="text-align:right;"> 70,273 </td>
   <td style="text-align:right;"> 49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017 </td>
   <td style="text-align:right;"> 7,096 </td>
   <td style="text-align:right;"> 6,933 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 197 </td>
   <td style="text-align:right;"> 60,409 </td>
   <td style="text-align:right;"> 11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018 </td>
   <td style="text-align:right;"> 9,678 </td>
   <td style="text-align:right;"> 9,160 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 77 </td>
   <td style="text-align:right;"> 183 </td>
   <td style="text-align:right;"> 65,940 </td>
   <td style="text-align:right;"> 142 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019 </td>
   <td style="text-align:right;"> 11,094 </td>
   <td style="text-align:right;"> 10,208 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 201 </td>
   <td style="text-align:right;"> 71,044 </td>
   <td style="text-align:right;"> 122 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020 </td>
   <td style="text-align:right;"> 7,916 </td>
   <td style="text-align:right;"> 7,824 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 75 </td>
   <td style="text-align:right;"> 240 </td>
   <td style="text-align:right;"> 103,190 </td>
   <td style="text-align:right;"> 129 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022 </td>
   <td style="text-align:right;"> 8,654 </td>
   <td style="text-align:right;"> 8,638 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 22,745 </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 334 </td>
   <td style="text-align:right;"> 162,074 </td>
   <td style="text-align:right;"> 233 </td>
  </tr>
</tbody>
</table>

\newpage
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Assumed selectivity parameters for the fishery before the Equal Quota Share program started in 1994 (pre-EQS) and the fishery since the implementation of EQS for females (black points) and males (grey triangles). These parameters estimates were borrowed from the Federal stock assessment, where the Federal derby fishery and IFQ fishery were assumed to represent pre-EQS and EQS NSEI fisheries.</caption>
 <thead>
<tr>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="1"></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Male</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Female</div></th>
</tr>
  <tr>
   <th style="text-align:left;">  </th>
   <th style="text-align:left;"> $s_{50}$ </th>
   <th style="text-align:left;"> $\delta_{50}$ </th>
   <th style="text-align:left;"> $s_{50}$ </th>
   <th style="text-align:left;"> $\delta_{50}$ </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Pre-EQS Fishery </td>
   <td style="text-align:left;"> 7.27 </td>
   <td style="text-align:left;"> 0.49 </td>
   <td style="text-align:left;"> 3.82 </td>
   <td style="text-align:left;"> 0.49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EQS Fishery </td>
   <td style="text-align:left;"> 4.29 </td>
   <td style="text-align:left;"> 0.90 </td>
   <td style="text-align:left;"> 3.34 </td>
   <td style="text-align:left;"> 1.76 </td>
  </tr>
</tbody>
</table>
\newpage

<br><br><br>

\newpage
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Negative likelihood values and percent of each component to the total likelihood. The data likelihood is the sum of all likelihood contributions from data. The difference between the total likelihood and the data likelihood is the contribution of penalized likelihoods, including recruitment and fishing mortality.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Likelihood component </th>
   <th style="text-align:center;"> $NLL$ </th>
   <th style="text-align:center;"> \% of $NLL$ </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Catch </td>
   <td style="text-align:center;"> 17.6 </td>
   <td style="text-align:center;"> 1.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Fishery CPUE </td>
   <td style="text-align:center;"> 178.9 </td>
   <td style="text-align:center;"> 9.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Survey CPUE </td>
   <td style="text-align:center;"> 107.7 </td>
   <td style="text-align:center;"> 6.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mark-recapture abundance </td>
   <td style="text-align:center;"> 84.9 </td>
   <td style="text-align:center;"> 4.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Fishery ages </td>
   <td style="text-align:center;"> 228.9 </td>
   <td style="text-align:center;"> 12.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Survey ages </td>
   <td style="text-align:center;"> 274.0 </td>
   <td style="text-align:center;"> 15.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Fishery lengths </td>
   <td style="text-align:center;"> 368.3 </td>
   <td style="text-align:center;"> 20.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Survey lengths </td>
   <td style="text-align:center;"> 539.8 </td>
   <td style="text-align:center;"> 30.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Data likelihood </td>
   <td style="text-align:center;"> 1800.1 </td>
   <td style="text-align:center;"> 100.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Fishing mortality penalty </td>
   <td style="text-align:center;"> 1.4 </td>
   <td style="text-align:center;"> 0.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Recruitment likelihood </td>
   <td style="text-align:center;"> -11.7 </td>
   <td style="text-align:center;"> -0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SPR penalty </td>
   <td style="text-align:center;"> 0.0 </td>
   <td style="text-align:center;"> 0.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sum of catchability priors </td>
   <td style="text-align:center;"> 9.1 </td>
   <td style="text-align:center;"> 0.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Total likelihood </td>
   <td style="text-align:center;"> 1798.9 </td>
   <td style="text-align:center;"> 99.9 </td>
  </tr>
</tbody>
</table>
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Parameter estimates from the statistical catch-at-age model. Estimates of recruitment, initial numbers-at-age, and fishing mortality deviations were excluded for brevity.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Parameter </th>
   <th style="text-align:center;"> Estimate </th>
   <th style="text-align:center;"> Lower 95% CI </th>
   <th style="text-align:center;"> Upper 95% CI </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Survey male selectivity pre-2000, $\mbox{s}_{50}$ </td>
   <td style="text-align:center;"> 6.237 </td>
   <td style="text-align:center;"> 4.161 </td>
   <td style="text-align:center;"> 10.307 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Survey male selectivity 2000-2022, $\mbox{s}_{50}$ </td>
   <td style="text-align:center;"> 5.042 </td>
   <td style="text-align:center;"> 4.511 </td>
   <td style="text-align:center;"> 5.685 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Survey male selectivity pre-2000, $\delta$ </td>
   <td style="text-align:center;"> 0.562 </td>
   <td style="text-align:center;"> 0.243 </td>
   <td style="text-align:center;"> 1.300 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Survey male selectivity 2000-2022, $\delta$ </td>
   <td style="text-align:center;"> 0.802 </td>
   <td style="text-align:center;"> 0.613 </td>
   <td style="text-align:center;"> 1.050 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Survey female selectivity pre-2000, $\mbox{s}_{50}$ </td>
   <td style="text-align:center;"> 3.896 </td>
   <td style="text-align:center;"> 3.261 </td>
   <td style="text-align:center;"> 4.849 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Survey female selectivity 2000-2022, $\mbox{s}_{50}$ </td>
   <td style="text-align:center;"> 3.697 </td>
   <td style="text-align:center;"> 3.493 </td>
   <td style="text-align:center;"> 3.928 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Survey female selectivity pre-2000, $\delta$ </td>
   <td style="text-align:center;"> 1.525 </td>
   <td style="text-align:center;"> 0.732 </td>
   <td style="text-align:center;"> 3.177 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Survey female selectivity 2000-2022, $\delta$ </td>
   <td style="text-align:center;"> 2.348 </td>
   <td style="text-align:center;"> 1.649 </td>
   <td style="text-align:center;"> 3.345 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pre-EQS catchability, $\mbox{ln}(q_{fsh,pre-EQS})$ </td>
   <td style="text-align:center;"> -17.670 </td>
   <td style="text-align:center;"> -17.751 </td>
   <td style="text-align:center;"> -17.589 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EQS catchability, $\mbox{ln}(q_{fsh,EQS})$ </td>
   <td style="text-align:center;"> -17.243 </td>
   <td style="text-align:center;"> -17.292 </td>
   <td style="text-align:center;"> -17.193 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Survey catchability pre-2000, $\mbox{ln}(q_{srv})$ </td>
   <td style="text-align:center;"> -16.880 </td>
   <td style="text-align:center;"> -17.003 </td>
   <td style="text-align:center;"> -16.758 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Survey catchability 2000-2022, $\mbox{ln}(q_{srv})$ </td>
   <td style="text-align:center;"> -16.718 </td>
   <td style="text-align:center;"> -16.777 </td>
   <td style="text-align:center;"> -16.658 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mark-recapture catchability, $\mbox{ln}(q_{MR})$ </td>
   <td style="text-align:center;"> -0.043 </td>
   <td style="text-align:center;"> -0.062 </td>
   <td style="text-align:center;"> -0.024 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mean recruitment, $\mu_R$ </td>
   <td style="text-align:center;"> 799,173 </td>
   <td style="text-align:center;"> 669,879 </td>
   <td style="text-align:center;"> 953,423 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mean initial numbers-at-age, $\mu_N$ </td>
   <td style="text-align:center;"> 1,020,153 </td>
   <td style="text-align:center;"> 776,952 </td>
   <td style="text-align:center;"> 1,339,481 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Variability in recruitment and initial numbers-at-age (random effects parameter), $\sigma_R$ </td>
   <td style="text-align:center;"> 0.521 </td>
   <td style="text-align:center;"> 0.439 </td>
   <td style="text-align:center;"> 0.618 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mean fishing mortality, $\mu_F$ </td>
   <td style="text-align:center;"> 0.056 </td>
   <td style="text-align:center;"> 0.030 </td>
   <td style="text-align:center;"> 0.106 </td>
  </tr>
</tbody>
</table>
<br><br><br>

\newpage
# FIGURES

<br><br><br>

![Northern Southeast Inside (NSEI) and Southern Southeast Inside (SSEI) Subdistricts including restricted waters of Glacier Bay National Park and Preserve and Annette Islands Reserve.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/Legacy_code_pre2023/figures/nsei_ssei_map.jpg){width=50%}
<br><br><br>

![Catch, landings by port, and ex-vessel value for Northern Southeast Inside (NSEI) Subdistrict commercial sablefish 1985-2022.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/catch_exvesselvalue_2022v3.png){width=50%}
<br><br><br>

![Estimated catch in the NSEI fishery from 2000 - 2022 and the relationship to $F_{40}$, $F_{50}$ and $F_{60}$ (Fspr), the fishing mortality that results in a spawner-per-recruit (SPR) of 40, 50 and 60% of the population's virgin state, in the base model and model v23.  Note that model v23, which makes fewer assumptions based on the federal assessment and is moredpendent on NSEI data, estimates the population as muc closer to the ADF&G management target of $F_{50}$.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/tmb/base_vs_v23_catch_Fspr.png)
<br><br><br>

![A summary of the available data sources in NSEI by year.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/sable_data_yprmodel_2022.png){width=50%}
<br><br><br>

![Biological inputs to the statistical catch-at-age model, including: (A) von Bertalanffy growth model predictions of weight-at-age (kg) by sex from the longline fishery (black) and ADFG longline survey (grey); (B) proportion mature at age for females estimated from the longline survey with the age at 50% maturity ($a_{50}$=6.4 yr); and (C) proportion female in the longline survey, where the curve is the fitted line from a generalized additive model +/- 2 standard error.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/tmb/bio_dat_2022.png){width=50%}
<br><br><br>

![Changes in length- and maturity-at-age over time in the NSEI sablefish population.  There is a general trend of fish maturing at younger ages and smaller sizes.  The model uses an average of all years for the assessment.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/maturity_atage_byyear_srvfem_2022.png){width=50%}![Changes in length- and maturity-at-age over time in the NSEI sablefish population.  There is a general trend of fish maturing at younger ages and smaller sizes.  The model uses an average of all years for the assessment.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/maturity_atlength_byyear_srvfem_2022.png){width=50%}
<br><br><br>

![Indices of catch and abundance with the assumed error distribution, including: (A) harvest (round mt), (B) fishery catch per unit effort in round kg per hook, (C) survey catch per unit effort in number of fish per hook, and (D) mark-recapture abundance estimates in millions. The dashed vertical line in 1994 mark the transition to the Equal Quota Share program.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/tmb/abd_indices_2022V2.png){width=50%}
<br><br><br>

![CPUE in the NSEI longline sablefish fishery in round lbs per hook.  The nominal values (blue) represent values from past assessments and the fully standardized values represent the values used in this assessment.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/ll_cpue_fullstand_1980_2022.png){width=50%}
<br><br><br>

![Proportions-at-age for in the NSEI longline fishery (2002-2022) and ADFG longline survey (1997-2022).](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/tmb/agecomps_2022.png){width=50%}
<br><br><br>

![Longline fishery and survey length distributions by sex from 1997-2022.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/raw_length_comps.png){width=50%}
<br><br><br>

![A comparison of the mean length and age in the longline fishery and longline survey since 1997 for male and female sablefish in the NSEI district.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/compare_comp_summaries.png){width=50%}
<br><br><br>


![The probability of retaining a fish as a function of weight (left), sex, and age (right).](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/tmb/retention_prob_2022.png)
<br><br><br>

![Fixed age-based selectivity curves for the fishery before the Equal Quota Share program started in 1994 (pre-EQS), the fishery since the implementation of EQS, and the estimated ADFG longline survey for females (black points) and males (grey triangles) before and after the standardization of the survey in 2000. Fishery selectivity parameter estimates were borrowed from the Federal stock assessment for the derby fishery (pre-EQS) and IFQ fishery (EQS), while the survey selectivity parameters are estimated within the model.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/tmb/Final2_v23_new_2slx/selectivity_2022.png){width=50%}
<br><br><br>

![Fits to indices of catch and abundance with the assumed error distribution shown as shaded grey polygons. Input data are shown as grey points and model fits are shown in black. Indices include (A) harvest (round mt); (B) fishery catch per unit effort in round kg per hook with separate selectivity and catchability time periods before and after the implementation of the Equal Quota Share program in 1994; (C) survey catch per unit effort in number of fish per hook; and (D) mark-recapture abundance estimates in millions. Solid and dashed lines in panel D reflect years for which data were and were not available, respectively.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/tmb/Final2_v23_new_2slx/pred_abd_indices_imperial_2022.png){width=50%}
<br><br><br>

![Fits to fishery age compositions, 2002-2022. Observed and predicted proportions-at-age shown as grey bars and black lines, respectively.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/tmb/Final2_v23_new_2slx/Fishery_agecomps_barplot.png){width=75%}
<br><br><br>

![Fits to survey age compositions, 1997-2022. Observed and predicted proportions-at-age shown as grey bars and black lines, respectively.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/tmb/Final2_v23_new_2slx/Survey_agecomps_barplot.png){width=75%}
<br><br><br>

![Standardized residuals of fits to fishery (2002-2022) and survey (1997-2022) age compositions. Size of residual scales to point size. Black points represent negative residuals (observed < predicted); white points represent positive residuals (observed > predicted).](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/tmb/Final2_v23_new_2slx/agecomps_residplot.png){width=50%}
<br><br><br>

![Fits to male fishery length compositions, 2002-2022. Observed and predicted proportions-at-age shown as grey bars and black lines, respectively.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/tmb/Final2_v23_new_2slx/Fishery_Male_lencomps_barplot.png){width=75%}
<br><br><br>

![Fits to female fishery length compositions, 2002-2022. Observed and predicted proportions-at-age shown as grey bars and black lines, respectively.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/tmb/Final2_v23_new_2slx/Fishery_Female_lencomps_barplot.png){width=75%}
<br><br><br>

![Fits to male survey length compositions, 1997-2022. Observed and predicted proportions-at-age shown as grey bars and black lines, respectively.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/tmb/Final2_v23_new_2slx/Survey_Male_lencomps_barplot.png){width=75%}
<br><br><br>

![Fits to female survey length compositions, 1997-2022. Observed and predicted proportions-at-age shown as grey bars and black lines, respectively.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/tmb/Final2_v23_new_2slx/Survey_Female_lencomps_barplot.png){width=75%}
<br><br><br>

![Standardized residuals of fits to fishery (2002-2022) and survey (1997-2022) length compositions for males and females. Size of residual scales to point size. Black points represent negative residuals (observed < predicted); white points represent positive residuals (observed > predicted).](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/tmb/Final2_v23_new_2slx/lencomps_residplot.png){width=50%}
<br><br><br>

![Model predictions of (A) age-2 recruitment (millions), (B) female spawning stack biomass (million lb), (C) exploitable abundance (millions), and (D) exploitable biomass (million lb).](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/tmb/Final2_v23_new_2slx/derived_ts_imperial_2022.png){width=50%}
<br><br><br>

![Model-estimated fishing mortality rate (top) and realized harvest rate (bottom), defined as the ratio of total predicted catch to exploitable biomass. Total predicted catch is the sum of landed catch and discarded biomass assumed to die post-release.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/figures/tmb/Final2_v23_new_2slx/fishing_mort.png){width=50%}
<br><br><br>

![Mohn's $\rho$ and retrospective peels of sablefish spawning biomass for the last 9 years.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/output/tmb/retrospective_v23_new_2slx/retrospective_spawn_biom_2022.png){width=50%}
<br><br><br>

![Mohn's $\rho$ and retrospective peels of sablefish recruitment for the last 9 years.](C:/Users/pjjoy/Documents/Groundfish Biometrics/Sablefish/seak_sablefish/2023/output/tmb/retrospective_v23_new_2slx/retrospective_recruitment_2022.png){width=50%}
<br><br><br>
<br><br><br>
<br><br><br>
<br><br><br>


#```{r ageerror, fig.cap= paste0("Ageing error matrix used in the model, showing the probability of #observing an age given the true #age (Heifetz et al. 1999).")}

#knitr::include_graphics(paste0("../figures/tmb/ageing_error.png"))
#```



#```{r srvlen, fig.cap= paste0("Longline survey length distributions by sex from 2012-",YEAR,". Vertical #bars represent the data nd the black line shows the length composition from the most recent year #superimposed on previous years.")}

#knitr::include_graphics(paste0(figures_dir,"/llsrv_lencomps_2012_",YEAR,".png"), rel_path=FALSE)
#```

#```{r agelenkey, fig.cap= paste0("Age-length key used in the model, with the relative size of the #bubbles reflecting the probability #that a fish of a given age falls within a certain length bin (Echave #et al. 2012). The probabilities sum to 1 across each age.")}

#knitr::include_graphics(paste0("../figures/tmb/age_length_key.png"))
#```

#```{r residabdind, fig.cap= paste0("Standardized residuals of fits to indices of catch and abundance, #including: (A) harvest, (B) #fishery catch per unit effort, (C) survey catch per unit effort, and (D) #mark-recapture (MR) abundance.")}

#knitr::include_graphics(paste0(tmb_mod_figures_dir,"/selectivity_",YEAR,".png"))
#knitr::include_graphics(paste0("../figures/tmb/presid_abd_indices.png"))
#```
