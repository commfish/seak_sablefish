/*

----------------------------------------------------------------------
Sablefish Proportion By Set
----------------------------------------------------------------------

NOTES:
	- This is Step 2 of the process outlined in:
	  "update_target_species_mk.sql"
	- Each step is described in a comment just before it executes.
	- This script must be run as user "SCOTTJ". 
		- Going to create workaround for this soon...
	- REMEMBER TO CHANGE RELEVANT YEARS BEFORE RUNNING!
	- Queries 1-17 generate sablefish-centric tables.
	- Queries 18-46 generate halibut-centric tables.
	- Queries 47-54 combine this data into a common format.
	- Justin edited this script to save a step at step 4, 21, and 37. 
	  (see comments in the script for details)
	- It is entirely possible that this could be done with temporary
	  tables, which would clean up the database significantly, but I'm
	  not sure if we should, do we need to be able to see each step 
	  once it's done, possibly for debugging purposes?
	- Renamed tables 1-9 to 01-09 so that they would show up in the 
	  correct order in SQL Developer.
	- This should probably be rewritten to use the new Zander versions
	  of the tables now that logbooks have been moved out of Alex.

*/


-- gets logbook data for sablefish

drop table scottj.out_g_log_longline_prop_01;

create table scottj.out_g_log_longline_prop_01 as
SELECT
    t.year,
    t.project_code,
    t.trip_no,
    e.effort_no,
    c.species_code,
    c.numbers AS sable_numbers,
    c.pounds  AS sable_log_pounds,
    e.g_stat_area
FROM
    data.g_log_longline_trip   t,
    data.g_log_longline_effort e,
    data.g_log_longline_catch  c
WHERE
    c.species_code = '710'
    AND t.year BETWEEN 1997 AND 2021
    AND t.id = e.trip_id (+)
    AND e.id = c.effort_id (+)
ORDER BY
    year,
    project_code,
    trip_no,
    effort_no,
    species_code;

-- gets fish ticket data for sablefish

drop table scottj.out_g_log_longline_prop_02;

create table scottj.out_g_log_longline_prop_02 as
select  year,
		trip_no,
		adfg_no,
		g_stat_area,
		sum(round_pounds)as sable_tkt_round_lbs,
		species_code
from ifdb.g_cat_ticket_join
where year between 1997 and 2005
and gear_code in ('06','61')
and species_code ='710'
and trip_no is not null
and harvest_code != '43'
and delivery_code !='98'
group by year,
         trip_no,
         adfg_no,
         g_stat_area,
         trip_no,
         species_code
union
select  year,
		trip_no,
		adfg_no,
		g_stat_area,
		sum(round_pounds)as sable_tkt_round_lbs,
		species_code
from ifdb.g_cat_ticket_join
where year >= 2006
and gear_code in ('06','61')
and species_code ='710'
and trip_no is not null
and harvest_code != '43'
and disposition_code !='98'
group by year,
         trip_no,
         adfg_no,
         g_stat_area,
         trip_no,
         species_code;

-- gets sablefish logbook data and fish ticket data together

drop table scottj.out_g_log_longline_prop_03;

create table scottj.out_g_log_longline_prop_03 as
select
 a.year,
 a.project_code,
 b.adfg_no,
 a.trip_no,
 a.g_stat_area,
 a.effort_no,
 a.sable_numbers,
 a.sable_log_pounds,
 b.sable_tkt_round_lbs,
 a.species_code
from scottj.out_g_log_longline_prop_01 a,
     scottj.out_g_log_longline_prop_02 b
where a.year = b.year
and   a.trip_no = b.trip_no
and   a.g_stat_area = b.g_stat_area;

-- concatenation to create unique code to find sablefish
-- stat. areas with catch reported in both numbers and pounds

drop table scottj.out_g_log_longline_prop_05;

create table scottj.out_g_log_longline_prop_05 as
select year||trip_no||g_stat_area as effort_code,
	year,
	trip_no,
	g_stat_area,
    sable_numbers,
 	sable_log_pounds,
 	sable_tkt_round_lbs,
 	species_code
from scottj.out_g_log_longline_prop_03 
order by year, trip_no, g_stat_area;

-- gets sablefish rows with pounds

drop table scottj.out_g_log_longline_prop_06;

create table scottj.out_g_log_longline_prop_06 as
select distinct(effort_code) as effort_code,
'pounds' as type,
	year,
	trip_no,
	g_stat_area,
    sable_tkt_round_lbs,
    species_code
from scottj.out_g_log_longline_prop_05 
where sable_log_pounds is not null;

-- gets sablefish rows with numbers

drop table scottj.out_g_log_longline_prop_07;

create table scottj.out_g_log_longline_prop_07 as
select distinct(effort_code) as effort_code,
'numbers' as type,
	year,
	trip_no,
	g_stat_area,
    sable_tkt_round_lbs,
    species_code
from scottj.out_g_log_longline_prop_05 
where sable_numbers is not null;

drop table scottj.out_g_log_longline_prop_08;

create table scottj.out_g_log_longline_prop_08 as
select * from scottj.out_g_log_longline_prop_06 
union
select * from scottj.out_g_log_longline_prop_07;

-- count = 2 are the trips with stat. areas with both sablefish numbers and pounds reported

drop table scottj.out_g_log_longline_prop_09;

create table scottj.out_g_log_longline_prop_09 as
select count(distinct type) as count,
	year,
	trip_no,
	g_stat_area,
    species_code
from scottj.out_g_log_longline_prop_08 
group by effort_code,
		year,
		trip_no,
		g_stat_area,
    	species_code;

-- gets stat. areas with both sablefish numbers and pounds reported

drop table scottj.out_g_log_longline_prop_10;

create table scottj.out_g_log_longline_prop_10 as
select year,
       trip_no,
       g_stat_area,
       species_code
from scottj.out_g_log_longline_prop_09 
where count = '2';

-- drops stat. areas with both numbers and pounds reported from sablefish logbook data

drop table scottj.out_g_log_longline_prop_11;

create table scottj.out_g_log_longline_prop_11 as
select
	year,
	project_code,
	adfg_no,
 	trip_no,
 	g_stat_area,
 	effort_no,
 	sable_numbers,
 	sable_log_pounds,
 	sable_tkt_round_lbs,
 	species_code
from scottj.out_g_log_longline_prop_03 a
where a.species_code = '710'
	and not exists
	(select b.*
	  from scottj.out_g_log_longline_prop_10 b
	  where   a.year = b.year
	  	and   a.trip_no = b.trip_no
	  	and   a.g_stat_area = b.g_stat_area);

--gets total sablefish logbook pounds and numbers by stat. area

drop table scottj.out_g_log_longline_prop_12;

create table scottj.out_g_log_longline_prop_12 as
 select
 year,
 project_code,
 adfg_no,
 trip_no,
 g_stat_area,
 sum(sable_log_pounds)as sable_log_total_pounds,
 sum(sable_numbers)as sable_total_numbers,
 sable_tkt_round_lbs,
 species_code
from scottj.out_g_log_longline_prop_11
 group by
 	year,
 	project_code,
 	adfg_no,
 	trip_no,
 	g_stat_area,
 	sable_tkt_round_lbs,
 	species_code;

-- gets sablefish numbers and pounds together

drop table scottj.out_g_log_longline_prop_13;

create table scottj.out_g_log_longline_prop_13 as
select
 a.year,
 a.project_code,
 a.adfg_no,
 a.trip_no,
 a.g_stat_area,
 a.effort_no,
 a.sable_log_pounds,
 b.sable_log_total_pounds,
 a.sable_numbers,
 b.sable_total_numbers,
 a.sable_tkt_round_lbs,
 a.species_code
from scottj.out_g_log_longline_prop_11 a,
     scottj.out_g_log_longline_prop_12 b
where a.year = b.year
and   a.trip_no = b.trip_no
and   a.g_stat_area = b.g_stat_area;

-- calculates proportion of sablefish logbook pounds and numbers per set (effort number)

drop table scottj.out_g_log_longline_prop_14;

create table scottj.out_g_log_longline_prop_14 as
select
 year,
 project_code,
 adfg_no,
 trip_no,
 g_stat_area,
 effort_no,
 sable_log_pounds,
 sable_log_total_pounds,
 (sable_log_pounds/sable_log_total_pounds)as sable_log_pounds_prop,
 sable_numbers,
 sable_total_numbers,
 (sable_numbers/sable_total_numbers) as sable_log_numbers_prop,
 sable_tkt_round_lbs,
 species_code
from scottj.out_g_log_longline_prop_13;

-- does the math for fish ticket proportion for sets with pounds

drop table scottj.out_g_log_longline_prop_15;

create table scottj.out_g_log_longline_prop_15 as
select
 year,
 project_code,
 adfg_no,
 trip_no,
 g_stat_area,
 effort_no,
 sable_log_pounds,
 sable_log_total_pounds,
 sable_log_pounds_prop,
 0 as sable_numbers,
 0 as sable_total_numbers,
 0 as sable_log_numbers_prop,
 sable_tkt_round_lbs,
 (sable_tkt_round_lbs * sable_log_pounds_prop) as sable_pounds_per_set,
 species_code
from scottj.out_g_log_longline_prop_14
where sable_log_pounds is not null;

-- does the math for fish ticket proportion for sets with numbers

drop table scottj.out_g_log_longline_prop_16;

create table scottj.out_g_log_longline_prop_16 as
select
 year,
 project_code,
 adfg_no,
 trip_no,
 g_stat_area,
 effort_no,
 0 as sable_log_pounds,
 0 as sable_log_total_pounds,
 0 as sable_log_pounds_prop,
 sable_numbers,
 sable_total_numbers,
 sable_log_numbers_prop,
 sable_tkt_round_lbs,
 (sable_tkt_round_lbs * sable_log_numbers_prop) as sable_pounds_per_set,
 species_code
 from scottj.out_g_log_longline_prop_14
 where sable_numbers is not null;

drop table scottj.out_g_log_longline_prop_17;
create table scottj.out_g_log_longline_prop_17 as
select * from scottj.out_g_log_longline_prop_15
union
select * from scottj.out_g_log_longline_prop_16;

-- gets logbook data for halibut where year >= 1997

drop table scottj.out_g_log_longline_prop_18;

create table scottj.out_g_log_longline_prop_18 as
SELECT
    t.year,
    t.project_code,
    t.trip_no,
    e.effort_no,
    c.species_code,
    c.numbers as halibut_numbers,
    c.pounds as halibut_log_pounds,
    e.g_stat_area
FROM
    data.g_log_longline_trip   t,
    data.g_log_longline_effort e,
    data.g_log_longline_catch  c
WHERE
    c.species_code = '200'
    AND t.year BETWEEN 1997 AND 2021
    AND t.id = e.trip_id (+)
    AND e.id = c.effort_id (+)
ORDER BY
    year,
    project_code,
    trip_no,
    effort_no,
    species_code;

-- gets fish ticket data for halibut

drop table scottj.out_g_log_longline_prop_19;

create table scottj.out_g_log_longline_prop_19 as
select year,
    trip_no,
    adfg_no,
	g_stat_area,
	sum(round_pounds)as halibut_round_lbs,
	species_code
from ifdb.g_cat_ticket_join
where year between 1997 and 2021
and species_code ='200'
and gear_code in ('06','61')
and trip_no is not null
group by year,
         trip_no,
         adfg_no,
         g_stat_area,
         species_code;

-- gets halibut logbook data and fish ticket data together

drop table scottj.out_g_log_longline_prop_20;

create table scottj.out_g_log_longline_prop_20 as
select
 a.year,
 a.project_code,
 b.adfg_no,
 a.trip_no,
 a.g_stat_area,
 a.effort_no,
 a.halibut_numbers,
 a.halibut_log_pounds,
 b.halibut_round_lbs,
 a.species_code
from scottj.out_g_log_longline_prop_18 a,
     scottj.out_g_log_longline_prop_19 b
where a.year = b.year
and   a.trip_no = b.trip_no
and   a.g_stat_area = b.g_stat_area;

-- concatenation to create unique code to find halibut
-- stat. areas with catch reported in both numbers and pounds
-- in halibut logs with tickets

drop table scottj.out_g_log_longline_prop_22;

create table scottj.out_g_log_longline_prop_22 as
select year||trip_no||g_stat_area as effort_code,
	year,
	trip_no,
	g_stat_area,
    halibut_numbers,
 	halibut_log_pounds,
 	halibut_round_lbs,
 	species_code
from scottj.out_g_log_longline_prop_20
order by year, trip_no, g_stat_area;

-- gets halibut rows with pounds in logs with tickets

drop table scottj.out_g_log_longline_prop_23;

create table scottj.out_g_log_longline_prop_23 as
select distinct(effort_code) as effort_code,
'pounds' as type,
	year,
	trip_no,
	g_stat_area,
    halibut_round_lbs,
    species_code
from scottj.out_g_log_longline_prop_22
where halibut_log_pounds is not null;

-- gets halibut rows with numbers in logs with tickets

drop table scottj.out_g_log_longline_prop_24;

create table scottj.out_g_log_longline_prop_24 as
select distinct(effort_code) as effort_code,
'numbers' as type,
	year,
	trip_no,
	g_stat_area,
    halibut_round_lbs,
    species_code
from scottj.out_g_log_longline_prop_22
where halibut_numbers is not null;

-- gets rows with pounds and numbers together

drop table scottj.out_g_log_longline_prop_25;

create table scottj.out_g_log_longline_prop_25 as
select * from scottj.out_g_log_longline_prop_23
union
select * from scottj.out_g_log_longline_prop_24;

-- count = 2 are the trips with stat. areas with both halibut numbers and pounds reported in logs with tickets

drop table scottj.out_g_log_longline_prop_26;

create table scottj.out_g_log_longline_prop_26 as
select count(distinct type) as count,
	year,
	trip_no,
	g_stat_area,
    species_code
from scottj.out_g_log_longline_prop_25
group by effort_code,
		YEAR,
		TRIP_NO,
		g_stat_area,
    	species_code
order by count desc;

-- gets stat. areas with both halibut numbers and pounds reported
-- in logs with tickets

drop table scottj.out_g_log_longline_prop_27;

create table scottj.out_g_log_longline_prop_27 as
select year,
       trip_no,
       g_stat_area,
       species_code
from scottj.out_g_log_longline_prop_26
where count = '2';


-- drops stat. areas with both numbers and pounds of halibut
-- reported in logs with tickets

drop table scottj.out_g_log_longline_prop_28;

create table scottj.out_g_log_longline_prop_28 as
select
	year,
	project_code,
	adfg_no,
 	trip_no,
 	g_stat_area,
 	effort_no,
 	halibut_numbers,
 	halibut_log_pounds,
 	halibut_round_lbs,
 	species_code
from scottj.out_g_log_longline_prop_20 a
where a.species_code = '200'
	and not exists
	(select b.*
	  from scottj.out_g_log_longline_prop_27 b
	  where   a.year = b.year
	  	and   a.trip_no = b.trip_no
	  	and   a.g_stat_area = b.g_stat_area);

-- gets total halibut logbook pounds and numbers by stat. area
-- for logs with tickets

drop table scottj.out_g_log_longline_prop_29;

create table scottj.out_g_log_longline_prop_29 as
 select
 year,
 project_code,
 adfg_no,
 trip_no,
 g_stat_area,
 sum(halibut_log_pounds)as halibut_log_total_pounds,
 sum(halibut_numbers)as halibut_total_numbers,
 halibut_round_lbs,
 species_code
from scottj.out_g_log_longline_prop_28
 group by
 	year,
 	project_code,
 	adfg_no,
 	trip_no,
 	g_stat_area,
 	halibut_round_lbs,
 	species_code;

-- gets halibut numbers and pounds together for logs with tickets

drop table scottj.out_g_log_longline_prop_30;

create table scottj.out_g_log_longline_prop_30 as
select
 a.year,
 a.project_code,
 a.adfg_no,
 a.trip_no,
 a.g_stat_area,
 a.effort_no,
 a.halibut_log_pounds,
 b.halibut_log_total_pounds,
 a.halibut_numbers,
 b.halibut_total_numbers,
 a.halibut_round_lbs,
 a.species_code
from scottj.out_g_log_longline_prop_28 a,
     scottj.out_g_log_longline_prop_29 b
where a.year = b.year
and   a.trip_no = b.trip_no
and   a.g_stat_area = b.g_stat_area;

-- calculates proportion of halibut logbook pounds and numbers
-- per set (effort number) for logs with tickets

drop table scottj.out_g_log_longline_prop_31;

create table scottj.out_g_log_longline_prop_31 as
select
 year,
 project_code,
 adfg_no,
 trip_no,
 g_stat_area,
 effort_no,
 halibut_log_pounds,
 halibut_log_total_pounds,
 (halibut_log_pounds/halibut_log_total_pounds)as halibut_log_pounds_prop,
 halibut_numbers,
 halibut_total_numbers,
 (halibut_numbers/halibut_total_numbers) as halibut_log_numbers_prop,
 halibut_round_lbs,
 species_code
from scottj.out_g_log_longline_prop_30;

-- does the math for proportion for sets with pounds
-- for logs with tickets

drop table scottj.out_g_log_longline_prop_32;

create table scottj.out_g_log_longline_prop_32 as
select
 year,
 project_code,
 adfg_no,
 trip_no,
 g_stat_area,
 effort_no,
 halibut_log_pounds,
 halibut_log_total_pounds,
 halibut_log_pounds_prop,
 0 as halibut_numbers,
 0 as halibut_total_numbers,
 0 as halibut_log_numbers_prop,
 halibut_round_lbs,
 (halibut_round_lbs * halibut_log_pounds_prop) as halibut_pounds_per_set,
 species_code
from scottj.out_g_log_longline_prop_31
where halibut_log_pounds is not null;

-- does the math for proportion for sets with numbers for logs with tickets

drop table scottj.out_g_log_longline_prop_33;

create table scottj.out_g_log_longline_prop_33 as
select
 year,
 project_code,
 adfg_no,
 trip_no,
 g_stat_area,
 effort_no,
 0 as halibut_log_pounds,
 0 as halibut_log_total_pounds,
 0 as halibut_log_pounds_prop,
 halibut_numbers,
 halibut_total_numbers,
 halibut_log_numbers_prop,
 halibut_round_lbs,
 (halibut_round_lbs * halibut_log_numbers_prop) as halibut_pounds_per_set,
 species_code
 from scottj.out_g_log_longline_prop_31
 where halibut_numbers is not null;

-- gets pound and numbers proportions together for logs with tickets

drop table scottj.out_g_log_longline_prop_34;

create table scottj.out_g_log_longline_prop_34 as
select * from scottj.out_g_log_longline_prop_32
union
select * from scottj.out_g_log_longline_prop_33;

-- gets halibut logbook rows that don't have associated fish ticket data

drop table scottj.out_g_log_longline_prop_35;

create table scottj.out_g_log_longline_prop_35 as
select * from scottj.out_g_log_longline_prop_18 a
where not exists
 (select b.*
  from scottj.out_g_log_longline_prop_20 b
  where a.year = b.year
  and a.project_code = b.project_code
  and a.trip_no = b.trip_no
  and a.effort_no = b.effort_no
  )
  order by a.year, a.trip_no, a.effort_no;

-- gets halibut logbook data and IPHC avrg. weight data together
-- for logs without tickets

drop table scottj.out_g_log_longline_prop_36;

create table scottj.out_g_log_longline_prop_36 as
select
 a.year,
 a.project_code,
 b.adfg_no,
 a.trip_no,
 a.g_stat_area,
 a.effort_no,
 a.halibut_numbers,
 a.halibut_log_pounds,
(a.halibut_numbers * c.avrg_weight_lbs) as halibut_round_lbs,
 a.species_code
from scottj.out_g_log_longline_prop_35 a,
	 IFDB.g_log_longline_trip_vessel b,
         scottj.g_iphc_halibut_data c
where a.year between 1997 and 2021
and a.halibut_log_pounds is null
and a.year = c.year(+)
and a.year = b.year
and a.trip_no = b.trip_no
union
select
 a.year,
 a.project_code,
 --b.adfg_no,
 '' as adfg_no,
 a.trip_no,
 a.g_stat_area,
 a.effort_no,
 a.halibut_numbers,
 a.halibut_log_pounds,
 a.halibut_log_pounds as halibut_round_lbs,
 a.species_code
from scottj.out_g_log_longline_prop_35 a
where a.year between 1997 and 2021
and a.halibut_log_pounds is not null;

-- concatenation to create unique code to find halibut
-- stat. areas with catch reported in both numbers and pounds
-- in halibut logs without tickets

drop table scottj.out_g_log_longline_prop_38;

create table scottj.out_g_log_longline_prop_38 as
select year||trip_no||g_stat_area as effort_code,
	year,
	trip_no,
	g_stat_area,
    halibut_numbers,
 	halibut_log_pounds,
 	halibut_round_lbs,
 	species_code
from scottj.out_g_log_longline_prop_36
order by year, trip_no, g_stat_area;

-- gets halibut rows with pounds in logs without tickets

drop table scottj.out_g_log_longline_prop_39;

create table scottj.out_g_log_longline_prop_39 as
select distinct(effort_code) as effort_code,
       'pounds' as type,
	year,
	trip_no,
	g_stat_area,
    halibut_round_lbs,
    species_code
from scottj.out_g_log_longline_prop_38
where halibut_log_pounds is not null;

-- gets halibut rows with numbers in logs without tickets

drop table scottj.out_g_log_longline_prop_40;

create table scottj.out_g_log_longline_prop_40 as
select distinct(effort_code) as effort_code,
'numbers' as type,
	year,
	trip_no,
	g_stat_area,
    halibut_round_lbs,
    species_code
from scottj.out_g_log_longline_prop_38
where halibut_numbers is not null;

-- gets rows with pounds and numbers together for logs without tickets

drop table scottj.out_g_log_longline_prop_41;

create table scottj.out_g_log_longline_prop_41 as
select * from scottj.out_g_log_longline_prop_39
union
select * from scottj.out_g_log_longline_prop_40;

-- count = 2 are the trips with stat. areas with both halibut numbers
-- and pounds reported in logs without tickets

drop table scottj.out_g_log_longline_prop_42;

create table scottj.out_g_log_longline_prop_42 as
select count(distinct type) as count,
	year,
	trip_no,
	g_stat_area,
    species_code
from scottj.out_g_log_longline_prop_41
group by effort_code,
		YEAR,
		trip_no,
		g_stat_area,
    	species_code
order by count desc;

-- gets stat. areas with both halibut numbers and pounds reported
-- in logs without tickets

drop table scottj.out_g_log_longline_prop_43;

create table scottj.out_g_log_longline_prop_43 as
select year,
       trip_no,
       g_stat_area,
       species_code
from scottj.out_g_log_longline_prop_42
where count = '2';

-- drops stat. areas with both numbers and pounds of halibut
-- reported in logs with tickets

drop table scottj.out_g_log_longline_prop_44;

create table scottj.out_g_log_longline_prop_44 as
select
	year,
	project_code,
	adfg_no,
 	trip_no,
 	g_stat_area,
 	effort_no,
 	halibut_numbers,
 	halibut_log_pounds,
 	halibut_round_lbs,
 	species_code
from scottj.out_g_log_longline_prop_36 a
where a.species_code = '200'
	and not exists
	(select b.*
	  from scottj.out_g_log_longline_prop_43 b
	  where   a.year = b.year
	  	and   a.trip_no = b.trip_no
	  	and   a.g_stat_area = b.g_stat_area);

-- set up logs without ticket data for union with logs with ticket data

drop table scottj.out_g_log_longline_prop_45;

create table scottj.out_g_log_longline_prop_45 as
select
 year,
 project_code,
 adfg_no,
 trip_no,
 g_stat_area,
 effort_no,
 halibut_log_pounds,
 0 as halibut_log_total_pounds,
 0 as halibut_log_pounds_prop,
 halibut_numbers,
 0 as halibut_total_numbers,
 0 as halibut_log_numbers_prop,
 halibut_round_lbs,
 halibut_round_lbs as halibut_pounds_per_set,
 species_code
from scottj.out_g_log_longline_prop_44;

-- gets logs without ticket data and logs with ticket data together

drop table scottj.out_g_log_longline_prop_46;

create table scottj.out_g_log_longline_prop_46 as
select * from scottj.out_g_log_longline_prop_34
union
select * from scottj.out_g_log_longline_prop_45;

-- gets sablefish and halibut data together

drop table scottj.out_g_log_longline_prop_47;
create table scottj.out_g_log_longline_prop_47 as
select
 a.year,
 a.project_code,
 a.adfg_no,
 a.trip_no,
 a.g_stat_area,
 a.effort_no,
 a.sable_log_pounds as sable_log_lbs,
 a.sable_log_total_pounds as sable_log_total_lbs,
 substr(a.sable_log_pounds_prop,1,3) as sable_log_lbs_prop,
 a.sable_numbers,
 a.sable_total_numbers,
 substr(a.sable_log_numbers_prop,1,3) as sable_log_numbers_prop,
 trunc(a.sable_tkt_round_lbs,2) as sable_tkt_round_lbs,
 trunc(a.sable_pounds_per_set,2)as sable_lbs_per_set,
 b.halibut_log_pounds as halibut_log_lbs,
 b.halibut_log_total_pounds as halibut_log_total_lbs,
 substr(b.halibut_log_pounds_prop,1,3) as halibut_log_lbs_prop,
 b.halibut_numbers,
 b.halibut_total_numbers,
 substr(b.halibut_log_numbers_prop,1,3) as halibut_log_numbers_prop,
 trunc(b.halibut_round_lbs,2) as halibut_round_lbs,
 trunc(b.halibut_pounds_per_set,2) as halibut_lbs_per_set
from scottj.out_g_log_longline_prop_17 a,
     scottj.out_g_log_longline_prop_46 b
where a.year = b.year(+)
and a.trip_no = b.trip_no(+)
and a.g_stat_area = b.g_stat_area(+)
and a.effort_no = b.effort_no(+)
order by a.effort_no;

-- Gets all the sablefish and  halibut columns together and assigns sort order
-- Note: take out vessel name comment when vessel table problem is resolved

drop table scottj.out_g_log_longline_prop_48;

CREATE TABLE scottj.out_g_log_longline_prop_48
    AS
        SELECT
            a.year,
            a.project_code,
            a.trip_no,
            a.adfg_no,
            c.longline_system_code                  AS longline_system,
            c.date_left_port,
            d.sell_date,
            c.longline_system_code,
            y.hook_size,
            y.hook_spacing,
            y.hooks_per_skate,
            x.number_of_skates,
            x.number_of_hooks,
            ( e.avg_depth_fathoms * 1.828799 )      AS average_depth_meters,
            f.g_management_area_code,
            a.g_stat_area,
            c.target_species_code_1                 AS trip_target,
            g.target_species_code                   AS set_target,
            a.effort_no,
            a.sable_log_lbs,
            a.sable_numbers,
            a.sable_tkt_round_lbs,
            a.sable_lbs_per_set,
            a.halibut_log_lbs,
            a.halibut_numbers,
            a.halibut_round_lbs,
            a.halibut_lbs_per_set,
            g.target_species_comment                AS set_comment,
            c.comments                              AS trip_comment,
            2                                       AS sort_order
        FROM
            scottj.out_g_log_longline_prop_47            a
            ,data.g_log_longline_trip             c
            ,ifdb.g_log_longline_trip_date        d
            ,data.g_log_longline_effort           e
            ,data.g_log_longline_eff_gear_cfg     x
            ,data.g_log_longline_trip_gear_cfg    y
            ,ifdb.g_stat_area_load                f
            ,ifdb.g_log_longline_effort_mk        g
        WHERE
                a.year = c.year
            AND a.project_code = c.project_code
            AND a.trip_no = c.trip_no
			
            AND a.year = d.year
            AND a.project_code = '6'||d.project_code
            AND a.trip_no = d.trip_no
			
            AND c.id = e.trip_id
            AND a.effort_no = e.effort_no
			
            AND a.g_stat_area = f.g_stat_area (+)
			
            AND a.year = g.year
            AND a.project_code = '6'||g.project_code
            AND a.trip_no = g.trip_no
            AND a.effort_no = g.effort_no
			
            AND e.id = x.effort_id
            AND x.trip_gear_config_id = y.trip_gear_config_id
        ORDER BY
            a.year,
            a.trip_no,
            a.effort_no;

-- calculates number of hooks if not entered in IFDB

drop table scottj.out_g_log_longline_prop_49;

create table scottj.out_g_log_longline_prop_49 as
select
 year,
 project_code,
 trip_no,
 adfg_no,
--vessel_name,
 longline_system,
 date_left_port,
 sell_date,
 longline_system_code,
 hook_size,
 hook_spacing,
 hooks_per_skate,
 number_of_skates,
 (hooks_per_skate * number_of_skates) as number_of_hooks,
 average_depth_meters,
 g_management_area_code,
 g_stat_area,
 trip_target,
 set_target,
 effort_no,
 sable_log_lbs,
 sable_numbers,
 sable_tkt_round_lbs,
 sable_lbs_per_set,
 halibut_log_lbs,
 halibut_numbers,
 halibut_round_lbs,
 halibut_lbs_per_set,
 set_comment,
 trip_comment,
 sort_order
from scottj.out_g_log_longline_prop_48
where number_of_hooks is null;

-- gets all rows together with their hook data

drop table scottj.out_g_log_longline_prop_50;

create table scottj.out_g_log_longline_prop_50 as
select * from scottj.out_g_log_longline_prop_48
where number_of_hooks is not null
union
select * from scottj.out_g_log_longline_prop_49;

-- finds trips with sablefish target sets that don't have a row in the catch table

drop table scottj.out_g_log_longline_prop_51;

create table scottj.out_g_log_longline_prop_51 as
select a.year, a.trip_no, a.effort_no
from IFDB.G_LOG_LONGLINE_EFFORT_MK a
where target_species_code ='710'
and not exists (
    select * from data.G_LOG_LONGLINE_CATCH b
    where b.effort_id in (
        select e.id from data.g_log_longline_effort e
        where e.effort_no = a.effort_no
        and e.trip_id in (
            select id from data.g_log_longline_trip t
            where t.year = a.year
            and t.project_code = '6'||a.project_code
            and t.trip_no = a.trip_no
        )
    )
);

-- drops trips that don't have catch data

drop table scottj.out_g_log_longline_prop_52;

create table scottj.out_g_log_longline_prop_52 as
select *
from scottj.out_g_log_longline_prop_50 a
where not exists
(select *
from scottj.out_g_log_longline_prop_51 b
where a.year = b.year
and a.trip_no = b.trip_no
);

-- report metadata & data

drop table scottj.out_g_log_longline_prop_53;

create table scottj.out_g_log_longline_prop_53 as
select
  null as year,
  null as project_code,
  null as trip_no,
  null as adfg_no,
--null as vessel_name,
  null as longline_system,
  null as date_left_port,
  null as sell_date,
  null as longline_system_code,
  null as hook_size,
  null as hook_spacing,
  null as hooks_per_skate,
  null as number_of_skates,
  null as number_of_hooks,
  null as average_depth_meters,
  null as g_management_area_code,
  null as g_stat_area,
  null as trip_target,
  null as set_target,
  null as effort_no,
  null as sable_log_lbs,
  null as sable_numbers,
  null as sable_tkt_round_lbs,
  null as sable_lbs_per_set,
  null as halibut_log_lbs,
  null as halibut_numbers,
  null as halibut_round_lbs,
  null as halibut_lbs_per_set,
  null as set_comment,
 'Metadata: This report does not include statistical areas with sets that were reported in a mix of numbers and pounds. The trip target species was assigned by ADFG Groundfish staff. The set target species was noted in the logbook by the fisherman.' as trip_comment,
 1 as sort_order
from dual
union
select * from scottj.out_g_log_longline_prop_52;

-- final query for longline harvest by set; does sort

drop table scottj.out_g_log_longline_prop_54;

create table scottj.out_g_log_longline_prop_54 as
select
  year,
  project_code,
  trip_no,
  adfg_no,
 -- vessel_name,
  longline_system,
  date_left_port,
  sell_date,
  longline_system_code,
  hook_size,
  hook_spacing,
  hooks_per_skate,
  number_of_skates,
  number_of_hooks,
  average_depth_meters,
  g_management_area_code,
  g_stat_area,
  trip_target,
  set_target,
  effort_no,
  sable_log_lbs,
  sable_numbers,
  sable_tkt_round_lbs,
  sable_lbs_per_set,
  halibut_log_lbs,
  halibut_numbers,
  halibut_round_lbs,
  halibut_lbs_per_set,
  set_comment,
  trip_comment
from scottj.out_g_log_longline_prop_53
order by sort_order,
        year,
        trip_no,
        effort_no;

COMMIT;

-- execute this command in SQL Developer before running final query
-- this sets the date format properly to include times

ALTER SESSION SET nls_date_format = 'dd-mon-yyyy hh24:mi:ss';

----------------------------------------------------------------------
-- FINAL OUTPUT QUERY
----------------------------------------------------------------------

-- run this and export to a spreadsheet and email to Phil Joy
SELECT
    s.*,
    e.time_set,
    e.time_hauled,
    e.start_latitude_decimal_degrees,
    e.start_longitude_decimal_degree
FROM
    scottj.out_g_log_longline_prop_54 s,
    data.g_log_longline_effort        e,
    data.g_log_longline_trip          t
WHERE
    s.year = 2021
    AND s.set_target = '710'
    AND s.year = t.year (+)
    AND s.project_code = t.project_code (+)
    AND s.trip_no = t.trip_no (+)
    AND s.effort_no = e.effort_no (+)
    AND t.id = e.trip_id (+)
ORDER BY
    s.year,
    s.project_code,
    s.trip_no,
    s.effort_no,
    s.sable_log_lbs,
    s.sable_numbers;
