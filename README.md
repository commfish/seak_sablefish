## Code for processing Southeast Alaska sablefish data for population assessments  

Please direct any questions to: 
Jane Sullivan jane.sullivan1@alaska.gov
or
Ben Williams ben.williams@alaska.gov


# DATA SOURCES

## Commercial fishery data 
1. Source: Longline and Pot (a relatively new fishery)
2. Catch and effort:
	- Nistorical cpue/catch data separate? (lbs/hook, 1980-1996, Dave Carlile?)
	- Logbook, Fish tickets
	- Target fishery
	- Whole (or round) lbs by trip and by set
		* Source code that matches logbook and fish ticket data to get weight-by-set?
	- Gear (e.g. hook or pot size/type, number of hooks/pots per skate/set, spacing)
	- Vessel info: Vessel name, ADFG number
3. Biological: Sex, length (type?), weight (type?), age, maturity status
4. Recapture data from tagged and released fish 
	- From state and federal fisheries 
	- Where and how are these data are stored?
5. Other: 
	- Dates: fishing activity and delivery dates
	- Area: Management area, stat area, lat/lon
	- Depth: Start of set, end of set, or mean (fathom or meters?)
	- I also have columns 'G_STAT_AREA_GROUP', 'SAMPLE_TYPE_CODE', and 'SPECIMEN_NO', but I don't know what these mean.

## Sport and personal use/subsistence catch (currently there are no queries for this)
1. Source: Sport, charter, and personal use/subsistence
2. Catch and effort:
	- Charter logbooks
	- Port sampling
	- Sportfish or subsistence harvest reports (?)
3. Biological: Lengths from port sampling
4. Other: 
	- Dates: fishing activity
	- Area: Management area, stat area
	- Potential for recaptures?

## ADFG Longline Survey
1. Source: Longline survey (OceanAK, except for ages?)
2. Catch and effort:
	- Whole (or round) lbs by trip and by set (sablefish per hook)
	- Gear (e.g. hook size/type, spacing, hook condition, number of sets)
	- Vessel info
3. Biological: Sex, length (type?), weight (type?), age, maturity status
4. Recapture data from tagged and released fish 
5. Other: 
	- Times/Dates: fishing activity
	- Area: Management area, stat area, lat/lon
		* Stations not consistent year to year for LL survey
	- Depth: Start of set, end of set, or mean (fathom or meters?)
	- I also have columns 'Subset No' and 'Subset Condition', but I don't know what these mean.

## ADFG Pot Survey
1. Source: Pot survey (OceanAK?)
2. Catch and effort:
	- Number caught
	- Number release
3. Biological: Sex, length (type?), weight (type?), age, maturity status
	- ASL data only available for 2009
4. Other: 
	- Times/Dates: fishing activity
	- Area: Management area, stat area, lat/lon
		* For both marked and recaptured
	- Depth
	- I also have columns 'Subset No' and 'Subset Condition', but I don't know what these mean.

## Marked fish tagged and released in the pot survey for recapture in the survey and fishery  

## Product recovery rates for sablefish

Ice and slime assumed to be 2% of total weight.

| Delivery code | Description                    | Rate |
|---------------|--------------------------------|-----:|
| 1             | Whole/round                    |    1 |
| 4             | Gutted, head on                | 0.89 |
| 7             | Headed and gutted, Western cut | 0.68 |
| 9             | Headed and gutted, Eastern cut | 0.63 |

# PAST QUERIES (from Kray Van Kirk February 2017)

## Commercial fishery queries

### annual_catch

Total annual commercial longline catch in metric tons for ASA

```sql
# ALEX QUERY CRITERIA  
Groundfish Fish Tickets 
   Base Table	out_g_cat_ticket																			
   Select Clause	*																				
   Where Clause	year In (present) AND species_code = 710
   AND g_management_area = NSEI
   AND g_cfec_fishery_group_code = C									
```


### legacy fishery cpue

Values from Dave Carlile, pounds per hook. 1980 - 1996. 
Logbook data for these years NOT entered into IFDB; manually constructed.

**NO QUERY OR OTHER DOCUMENTATION**

### cpue_fsh

Used to calculate commercial longline fishery CPUE
Output from Scott Johnson following the execution of Martina's script to define set target. 1997 - present

**NO QUERY OR OTHER DOCUMENTATION**


### fish_bio_data

Need an explanation of G_STAT_AREA_GROUP (e.g. NSEI 22, NSEI 15), SPECIMEN_NO, SAMPLE_TYPE_CODE, WEIGHT_KILOGRAMS (round? ice/slime? some of these are really heavy, e.g. 17 kg)
NOTE: when running, always make sure that all data have been read and uploaded into IFDB by the ADU

**FLAG** incomplete query?

```sql
# ALEX QUERY CRITERIA
	year BETWEEN 1980 AND 2015 AND species_code = '710' AND 
	g_management_area_code = 'NSEI' AND project_code = '02'
```
 
## Longline survey queries

### cpue_srv

Used to calculate Chatham longline survey CPUE
**FLAG** Need depth? # of sets?

```sql
# ALEX QUERY CRITERIA
SURVEY >> LONGLINE SURVEY - CATCH AND HOOK ACCOUNTING
    Base Table	out_g_sur_longline_hooks_catch																				
    Select Clause	*																				
    Where Clause	project_code = '03' AND year IN (1988:present)																				
    Group By Clause																					
    Order By Clause	year, project_code, trip_no,effort_no,subset_no																				
```

### srv_bio_data

**FLAG** - What does Effort No mean in this context? What day and depth were these collected? Date collected?

```sql
# ALEX QUERY CRITERIA - old, use OceanAK
BIOLOGICAL DATA >> Age Sex Size Sampled at Sea
    Base Table	out_g_bio_effort_age_sex_size							
    Select Clause *							
    Where Clause year BETWEEN 1988 AND 2015 AND 
    	species_code = '710' AND 
    	project_code = '03'			

# OceanAK QUERY CRITERIA
# Commercial Fisheries / Region I / Groundfish / Age-Sex-Size Sampled at Sea
Criteria	Year is between 1988 AND Present AND 
    	Species = 'Sablefish' AND 
   		Project = 'Chatham Sablefish LL Survey'		
```


## Pot survey 

### POT_LENGTH_FREQUENCY_15 (and 04_13)

Lengths of fish marked on the pot survey.
04_13 are legacy values between 2004 - 2013 - split for 2015+ due Excel limitation on row numbers; likely corrected with the implementation of ZANDER instead of ALEX

```sql
# ALEX QUERY CRITERIA
BIOLOGICAL DATA >> Age-Sex-Size Tagged at Sea
	Base Table	out_g_bio_eff_age_sex_size_tag							
    Select Clause *							
    Where Clause year = 2015+ AND
    	species_code = '710' AND
    	gear_code = '91' AND
    	project_code = '11'							
```


### pot_sex

Subsample of those fish on the pot marking survey sacrificed to obtain sex data. Note that this table is distinct from POT_LENGTH above, as this one selects from Age-Sex-Size *Sampled* at Sea, not *Tagged* pot_sex and srv_bio_data could potentially be joined into a single call, wait for ZANDER?

This query currently only has age-sex-size data in 2009.

```sql
#ALEX QUERY CRITERIA - legacy, keep
BIOLOGICAL DATA >> Age-Sex-Size Sampled at Sea
    Base Table	out_g_bio_eff_age_sex_size							
    Select Clause *							
    Where Clause year BETWEEN 2009 AND 2015 AND
    	species_code = '710' AND
    	gear_code = '91' AND
    	project_code = '11'							
```

### tagged_recovery_lengths
Used only to show differences between tagged retentions and non-tagged retentions in the commercial fishery; not used for any formal calcs
**FLAG** Are these all the tag recoveries?

```sql
#ALEX QUERY CRITERIA
BIOLOGICAL DATA >> Tag Release and Recovery for Mapping
    Base Table	out_g_bio_effort_rel_rec_map							
    Select Clause *							
    Where Clause release_year IN (2015,2013,2012,2011,2010,
    	2009,2008,2007,2006,2005, 2004,2003) AND 
    	species_code = '710'	
```

## Other

### sablefish_ADU_age
Used to calculate the aging-error matrix for the ASA. Spreadsheet from Kevin McNeel at the ADF&G Age Determination Lab. These data are not currently stored in useable format in IFDB, so Kevin prepares it for us. This isn't necessarily something that needs to be updated and run every single year, but don't let more than one year go between updates. 

**NO QUERY OR OTHER DOCUMENTATION**


