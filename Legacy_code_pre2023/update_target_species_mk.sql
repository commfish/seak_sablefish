/*

----------------------------------------------------------------------
Update Target Species by Set - 2022 Version
----------------------------------------------------------------------

NOTES: 
	- This process takes place in IFDB.
	- 'mk' refers to Martina Kallenberger
	- All relevant programmer files are located at:
	  S:\Programming\zander\g_log_longline\target species by set - new
	  
CHANGELOG:
	- 11/15/2018: This file was created.
	- 01/27/2022: Since step 1 now automatically gets effort-level 
	  target species codes and comments, steps 2-5 of the original 
	  script became obsolete, and have been removed. Original script
	  moved to archive folder "target species by set - old" (2018-2021 version)
	
PROCESS:

Step 1: Move logbook data to mk's table (g_log_longline_effort_mk) by 
running this script. Change year on line 67 if necessary.
*/
INSERT INTO ifdb.g_log_longline_effort_mk (
    year,
    project_code,
    trip_no,
    effort_no,
    time_set,
    time_hauled,
    start_latitude_decimal_degrees,
    start_longitude_decimal_degree,
    end_latitude_decimal_degrees,
    end_longitude_decimal_degrees,
    g_stat_area,
    avg_depth_fathoms,
    number_of_skates,
    number_of_hooks,
    target_species_code,
    target_species_comment
)
    SELECT
        t.year,
        substr(t.project_code,2,2) as project_code,
        t.trip_no,
        g.effort_no,
        g.time_set,
        g.time_hauled,
        g.start_latitude_decimal_degrees,
        g.start_longitude_decimal_degree,
        g.end_latitude_decimal_degrees,
        g.end_longitude_decimal_degrees,
        g.g_stat_area,
        g.avg_depth_fathoms,
        c.number_of_hooks,
        c.number_of_skates,
        case when g.target_species_code_1 is not null then g.target_species_code_1
        else t.target_species_code_1
        end target_species_code,
        g.comments target_species_comment
    FROM
        data.g_log_longline_trip t,
        data.g_log_longline_effort g,
        DATA.g_log_longline_eff_gear_cfg c
-- change the line below if you need a different year!
    WHERE
            t.year = 2021 
            and t.id = g.trip_id
            and c.effort_id = g.id
            AND NOT EXISTS (
            SELECT
                m.*
            FROM
                ifdb.g_log_longline_effort_mk m
            WHERE
                    t.year = m.year AND t.trip_no = m.trip_no AND g.effort_no = m.effort_no
        );

COMMIT;

/*

Step 2: Run script "sablefish_proportion_by_set" to get the final
output. You'll need to connect to IFDB as user "scottj" to run it. At 
the end of this SQL script, there's a query for producing the final 
data that gets sent to Jane Sullivan in a spreadsheet.

*/