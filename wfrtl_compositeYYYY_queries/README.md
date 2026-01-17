This folder contains the queries to reproduce the wfrtl_compositeYYYY.csv files

.sh: Executes batch process for years 2005 - 2025.
```sh
#!/bin/bash

for year in $(seq 2005 2025); do
    echo -n "Processing year $year … "

    sed "s/{{YEAR}}/$year/g" q.sql | psql -d spatial -o "wfrtl_composite${year}.csv"

    echo "done."
done
```

Query 1: Builds XREF_1 dataset
```sql
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
```

Query 2: Builds XREF_2 dataset
```sql
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
```

Query 3: Builds XREF_3 dataset (depends upon XREF_2)
```sql
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
```

Query 4: Builds XREF_4 dataset (depends upon XREF_3)
```sql
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
```

Query 5: Builds XREF_5 dataset (depends upon XREF_1, XREF_4)
```sql
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
```

Query 6: Builds XREF_6 dataset (depends upon XREF_5)
```sql
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
```

Query 7: Builds wfrtl_compositeYYYY.csv (depends upon XREF_6)
```sql
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
```