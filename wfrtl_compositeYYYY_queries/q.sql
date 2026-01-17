CREATE TEMP TABLE xref_1 AS 
    SELECT
        wof_id
            AS "WOF_ID",
        name
            AS "NAME",
        adm0name
            AS "ADM0NAME",
        adm1name
            AS "ADM1NAME",
        longitude
            AS "LONGITUDE",
        latitude
            AS "LATITUDE",
        geom
            AS geometry
    FROM 
        populated_places_expanded
    GROUP BY 
        wof_id,
        name,
        adm0name,
        adm1name,
        longitude,
        latitude,
        geom
    ORDER BY 
        wof_id;


CREATE TEMP TABLE xref_2 AS
    SELECT
        id,
        "Density"
            AS density,
        (TO_DATE(SUBSTRING("Start" FROM 1 FOR 7), 'YYYYDDD') + i)::date 
            AS day
        FROM public.hms_smokes{{YEAR}},
        generate_series(
            0,
            (TO_DATE(SUBSTRING("End" FROM 1 FOR 7), 'YYYYDDD') - TO_DATE(SUBSTRING("Start" FROM 1 FOR 7), 'YYYYDDD'))::int
        ) 
            AS i
    ORDER BY
        id;


CREATE TEMP TABLE xref_3 AS
    SELECT
        id,
        MAX(
            CASE COALESCE(density, '')
                WHEN 'Heavy'  THEN 3
                WHEN 'Medium' THEN 2
                WHEN 'Light'  THEN 1
                ELSE 0
            END
        )
            AS rank,
        day
    FROM 
        xref_2
    GROUP BY 
        id,
        day
    ORDER BY 
        id;


CREATE TEMP TABLE xref_4 AS
    SELECT
        x3.id,
        x3.rank,
        x3.day,
        s.geometry
    FROM xref_3 x3
    JOIN public.hms_smokes{{YEAR}} s
        ON x3.id = s.id
    ORDER BY
        x3.id;


CREATE TEMP TABLE xref_5 AS
    SELECT
        x1."WOF_ID",
        x1."NAME",
        x1."ADM0NAME",
        x1."ADM1NAME",
        x1."LONGITUDE",
        x1."LATITUDE",
        MAX(x4.rank) 
            AS rank,
        x4.day
    FROM xref_1 x1
    LEFT JOIN xref_4 x4
        ON ST_Intersects(x1.geometry, x4.geometry)
    GROUP BY
        x1."WOF_ID",
        x1."NAME",
        x1."ADM0NAME",
        x1."ADM1NAME",
        x1."LONGITUDE",
        x1."LATITUDE",
        x4.day;


CREATE TEMP TABLE xref_6 AS
    SELECT
        "WOF_ID",
        "NAME",
        "ADM0NAME",
        "ADM1NAME",
        "LONGITUDE",
        "LATITUDE",
        
        COUNT(CASE WHEN rank = 1 THEN 1 END)
            AS "Smoke_days_light_point",
        
        COUNT(CASE WHEN rank = 2 THEN 1 END)
            AS "Smoke_days_medium_point",

        COUNT(CASE WHEN rank = 3 THEN 1 END)
            AS "Smoke_days_heavy_point",

        COUNT(CASE WHEN rank = 0 THEN 1 END)
            AS "Smoke_days_undefined_point"
    FROM xref_5
    GROUP BY
        "WOF_ID",
        "NAME",
        "ADM0NAME",
        "ADM1NAME",
        "LONGITUDE",
        "LATITUDE"
    ORDER BY
        "WOF_ID";


CREATE TEMP TABLE out AS
    SELECT
        "WOF_ID",
        "NAME",
        "ADM0NAME",
        "ADM1NAME",
        "LONGITUDE",
        "LATITUDE",
        "Smoke_days_light_point",
        "Smoke_days_medium_point",
        "Smoke_days_heavy_point",
        "Smoke_days_undefined_point",
        ("Smoke_days_light_point" 
        + "Smoke_days_medium_point" 
        + "Smoke_days_heavy_point" 
        + "Smoke_days_undefined_point") 
            AS "Smoke_days_aggregate_point"
    FROM xref_6
    ORDER BY 
        "WOF_ID";

COPY out TO '/Users/williamchuter-davies/Downloads/wfrtl_composite{{YEAR}}.csv' WITH (FORMAT csv, HEADER true);