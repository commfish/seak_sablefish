/* Queries for Chatham sablefish assessment
 year = 2020 (most recent year of data)

This file was developed by J. Sullivan on 20210121 and edited by Justin Daily
to facilitate the handoff of the assessment to the future groundfish 
biometrician. These queries were adapted from the r/0_querynclean_data.R,
which was developed to query the data directly from the database. 
Hopefully when the new biometrician is onboarded they can work with
Paul Caldwell to get hooked up to the database. In the meantime, work with 
r/0_clean_data.R

There are 9 queries that need to be executed to perform this assessment.
I've included general documentation, and after each query there are 
instructions about how the data should be exported (as csv files),
what they should be named, and where they should be saved in the 
seak_sablefish project directory.

Note that the Fishery CPUE (#2) will likely not run because of the new app 
developed last year and changes to trip target designation methods (more 
documentation below)

Questions can be directed to jane.sullivan@noaa.gov 907-738-3311*/

-- ZANDER/IFDB

-- 1. Fishery harvest:
select	year, adfg_no, trip_no, vessel_name, port_code, gear,
		catch_date, sell_date, harvest_code, harvest, g_stat_area,
		g_management_area_code, species_code, pounds, round_pounds,
		delivery_code, g_cfec_fishery_group, g_cfec_fishery
from	out_g_cat_ticket
where	species_code = '710' and
		harvest_code NOT IN ('43', '42') and
		g_cfec_fishery = 'C61A' and
		g_management_area_code = 'NSEI' and year = 2020

-- Name and save file as follows in the seak_sablefish project directory: 
-- data/fishery/raw_data/nseiharvest_ifdb_2020.csv	

-- 2. Fishery CPUE: 

 /* !!!This query will likely not run, 
 and @JustinD feel free to skip it (documentaiton below). */

 /* Current documentation: In 2020, Rhea Ehresmann and GF project staff worked with Karl Wood
 to create a new cpue data input application. As part, they redefined how set targets are
 defined. This means Kallenburger's scripts that match fish ticket poundage data to 
 logbook data are likely depricated and will need a revamp. If this is the case, my 
 recommendation is to work with Justin Daily and the new biometrician to develop new methods
 (ideally in R). In the mean time, the statistical catch-at-age model will accept NULLS for 
 this data component. This part of the model likelihood receives a relatively low weight,
 so I anticipate this will have little influence on model results. */

/* Past documentation: Kamala Carroll pulls the IFDB view out_g_log_longline_c_e,
 populates new columns effort_target_species_code and effort_comments, and sends to Scott
Johnson. Scott runs a series of Martina Kallenburger's sql scripts
(https://github.com/commfish/seak_sablefish/issues/7) that match fish
tickets pounds back to set based on Kamala's set target designation based on
some undocumented methodology (proportional to numbers/pounds in logbook?). It
lives in a view called sablefish_cpue_final_view in scottj's IFDB schema,
though he has made a public synonym for it. This output doesn't contain
information on sets designated as halibut targets. Note that it is missing
effort_no's (effort_no's = individual sets).*/

select	year, project_code, trip_no, adfg_no, longline_system_code, sell_date, 
        hook_size, hook_spacing, number_of_skates, number_of_hooks,
        average_depth_meters, g_management_area_code, g_stat_area, trip_target, set_target,
        effort_no, sable_lbs_per_set, time_set, time_hauled, 
        start_latitude_decimal_degrees, start_longitude_decimal_degree
from    SABLEFISH_CPUE_FINAL_VIEW
where   g_management_area_code = 'NSEI' and year = 2020

-- Name and save file as follows in the seak_sablefish project directory: 
-- data/fishery/raw_data/fishery_cpue_2020.csv	

-- 3. Fishery biological data (e.g. sex, age, length, weight, maturity)

select  year, project_code, trip_no, adfg_no, vessel_name, sell_date, g_stat_area,
        g_management_area_code, sample_type, species_code, length_type_code, 
        length_type, length_millimeters / 10 as length, weight_kilograms,
        age, age_readability_code, age_readability, sex_code, 
        maturity_code, maturity, gear_code, gear

from    out_g_bio_age_sex_size

where   species_code = '710' and
        project_code in ('02', '17') and
        age_readability_code in ('01', '02', '03') and
        g_management_area_code = 'NSEI'and year = 2020

/* Name and save file as follows in the seak_sablefish project directory: 
 data/fishery/raw_data/fishery_bio_2020.csv	*/

-- 4. Longline survey cpue

/* There are two longline survey CPUE views: For CPUE you want
 output.out_g_sur_longline_hook_acc_bi. out_g_sur_longline_hook_acc_bi sums the
 total number of sablefish while out_g_sur_longline_catch_bi splits out by
 retained, lost, discard, etc. For the 2017 assessment (2018 forecast) I used
 the correct view for CPUE calculation but the incorrect one in the
 mark-recapture models 3 and 4. These were not used for management, and the
 code has been corrected for the 2018 assessments. The other view is used for
 mark-recapture purposes because all retained fish are checked for marks/tags.*/

/* 2020-01-28: Worked with Rhea Ehresman to improve data clean up base on database
 comments. Invalidate: skates with >= 12 invalid hooks (should be done already), large
 snarls, clothelined skates, control for bycatch, sleeper sharks. This was an ongoing effort,
 so it's worth pulling the full data set. The file is saved as "v2" because the data
 are different than past years, which were not QAQCed as thoroughly, but that can and
 probably should be changed in the future. Once these data are stable, you can modify the 
 query and R code to only pull the current year of data and merge it to past years. */

select  *

from    output.out_g_sur_longline_hook_acc_bi

where   project_code in ('603', '03')

/* Name and save file as follows in the seak_sablefish project directory: 
 data/survey/raw_data/llsrv_cpue_v2_1985_2020.csv	*/

-- 5. Longline survey catch 

/* !!!!! @JustinD or whoever is running these queries, I don't know what the name of 
the schema for the discard_status table, 
so if there isn't a public synonym, you may have to modify the query */

/* There is no countback for each fish on the longline survey to check for marks.
 Only tags are pulled. However, prior to 2019, it was assumed that all fish
 were checked (only discard status "01" for retained fish) and this is the view
 that was used. Jane S found out about this false assumption during the 2019 longline
 survey.*/

select  c.year, c.project_code, c.trip_no, c.target_species_code, c.adfg_no, c.vessel_name, 
        c.time_first_buoy_onboard, c.number_of_stations, c.hooks_per_set, c.hook_size, 
        c.hook_spacing_inches, c.sample_freq, c.last_skate_sampled, c.effort_no, c.station_no, c.species_code, 
        c.g_stat_area as stat, c.number_hooks, c.bare, c.bait, c.invalid, c.hagfish_slime, c.unknown, 
        c.numbers, c.discard_status_code, d.discard_status, c.subset_condition_code

from    output.out_g_sur_longline_catch_bi c

join	ifdb.discard_status d on c.discard_status_code = d.discard_status_code

where   c.species_code = '710' and
		c.project_code in ('603', '03')

/* Name and save file as follows in the seak_sablefish project directory: 
 data/survey/raw_data/llsrv_by_condition_1988_2020.csv	*/

-- 6. Longline survey biological data (e.g. sex, age, length, weight, maturity)

select  year, project_code, trip_no, target_species_code, adfg_no, vessel_name, 
        time_first_buoy_onboard, number_of_stations, hooks_per_set, hook_size, 
        hook_spacing_inches, sample_freq, last_skate_sampled, effort_no, station_no, species_code, 
        g_stat_area as stat, start_latitude_decimal_degrees as start_lat,
        start_longitude_decimal_degree as start_lon, end_latitude_decimal_degrees as end_lat,
        end_longitude_decimal_degrees as end_lon, avg_depth_fathoms * 1.8288 as depth_meters, 
        length_millimeters / 10 as length, weight_kilograms as weight, 
        age, age_type_code, age_readability_code, sex_code, maturity_code, otolith_condition_code

from    output.out_g_sur_longline_specimen

where   species_code = '710' and
        age_readability_code in ('01', '02', '03') and
        project_code in ('603', '03') and 
        year = 2020

/* Name and save file as follows in the seak_sablefish project directory: 
 data/survey/raw_data/llsrv_bio_2020.csv	*/

 -- 7. Pot survey biological

/* The pot survey is a mark-recapture survey. Limitted bio data exists. Use this
data for two purposes: 1) the bio data, and 2) determining the number of marks
deployed into the fishery in a given year and which batch_no the tags are
from.

This query currently only has age-sex-size data in 2009. There's also
an IFDB view called out_g_bio_eff_age_sex_size_tag. Asked S. Johnson
2018-02-16 what the difference between the two views is. He said "These two
views are almost identical. out_g_bio_eff_age_sex_size_tag is created by the
following SQL: 'CREATE OR REPLACE VIEW OUT_G_BIO_EFF_AGE_SEX_SIZE_TAG AS
select *  from out_g_bio_effort_age_sex_size where length(tag_no) > 2'. This
omits the "T-" entries which are fish that were sampled but not tagged. The _tag 
view only includes fish that were tagged.

The out_g_bio_effort_age_sex_size view has all the biological samples for the
pot surveys, and includes fish that were not tagged (tag_no = 'T-').

Updated query 20200124 to include project code = 66, the experimental code
used for the 2019 (and 2020) escape ring studies*/

select  year, project_code, trip_no, target_species_code, adfg_no, vessel_name,
        time_first_buoy_onboard, effort_no, station_no, species_code, 
        g_stat_area as stat, management_area, start_latitude_decimal_degrees as start_lat,
        start_longitude_decimal_degree as start_lon, end_latitude_decimal_degrees as end_lat,
        end_longitude_decimal_degrees as end_lon, avg_depth_fathoms * 1.8288 as depth_meters, 
        length_millimeters / 10 as length, weight_kilograms as weight, 
        age, age_type_code, age_readability_code, sex_code, maturity_code, 
        tag_no, tag_batch_no, discard_status, release_condition_code
  
from    out_g_bio_effort_age_sex_size

where   species_code = '710' and
        project_code in ('11', '611', '66') and year = 2020

/* Name and save file as follows in the seak_sablefish project directory: 
 data/survey/raw_data/potsrv_bio_2020.csv	*/

-- 8. Tag releases (from pot marking survey)

/*!!! @JustinD, just fyi I was previously left joining these in R. 
I think I got the SQL query right, but have no way of checking it. */

/* The out_g_bio_eff_age_sex_size_tag view is almost the same as the
 out_g_bio_effort_age_sex_size view, except it only stores tagged fish from the
 pot marking survey. Use this to get the number of fish marked, double check
 the release condition codes, get length frequencies of tagged fish, and cross
 reference the batch_no's with fish recovered outside of the directed fishery.
 Note that each year has a unique tag_batch_no

 Updated query 20200124 to include project code = 66, the experimental code
 used for the 2019 (and 2020) escape ring studies*/

select 	r.year, r.project_code, r.trip_no, r.effort_no, r.species_code, 
        r.management_area, r.length_millimeters / 10 as length, 
        r.tag_no, r.tag_batch_no, r.discard_status, r.release_condition_code, 
        r.comments, e.time_second_anchor_overboard, e.stat

from    out_g_bio_eff_age_sex_size_tag r

left join (
	select distinct year, trip_no, effort_no, time_second_anchor_overboard, species_code, 
          			g_stat_area as stat

	from	out_g_sur_pot) e

on 		r.year = e.year and r.trip_no = e.trip_no and r.effort_no = e.effort_no and 
			r.species_code = e.species_code

where   r.species_code = '710' and project_code in ('11', '611', '66') and r.year = 2020

/* Name and save file as follows in the seak_sablefish project directory: 
 data/survey/raw_data/tag_releases_2020.csv	*/

-- 9. Tag recoveries 

/* This is the batch report that Mike Vaughn does (how we determine how many tags
 lost/not available to the directed NSEI sablefish fishery). Match up
 batch_no's to the tag_releases. Also includes recapture lengths (careful to
 only use sampler lengths)*/

select	tag_no, tag_batch_no, tag_event_code, tag_event, year, project_code, 
        trip_no, species_code, landing_date, catch_date, g_management_area_code, 
        g_stat_area, g_stat_area_group, vessel_type, length_millimeters / 10 as length, measurer_type, 
        information_source, tag_returned_by_type, comments

from    out_g_bio_tag_recovery

where   species_code = '710' and year = 2020

/* Name and save file as follows in the seak_sablefish project directory: 
 data/fishery/raw_data/tag_recoveries_2020.csv	*/
