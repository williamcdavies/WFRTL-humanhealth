How to use:

Prerequisite:
- PostgreSQL must be installed and available

For additional info on installing PostgreSQL see https://www.postgresql.org/download/.

- A database with name=spatial

For additional info on creating databases see https://www.postgresql.org/docs/current/sql-createdatabase.html.


1. Restore `spatial.dump`:

```sh
pg_restore --no-owner -j 8 -d spatial dumpdir
```

For additional info on database restoration see https://www.postgresql.org/docs/current/app-pgrestore.html.


2. Enter the postgres interactive terminal

```sh
psql spatial
```


3. Execute the following queries in order:

```sql
CREATE TEMP TABLE xref AS (
    SELECT
        p.id
            AS p_id,
        s.id
            AS s_id,
        s."Year"
            AS year,
        SUBSTRING(s."Start" FROM 1 FOR 7)
            AS start,
        SUBSTRING(s."End" FROM 1 FOR 7)
            AS end,
        s."Density"
            AS density
    FROM public.populated_places p
    JOIN public.hms_smokes s
        ON ST_Intersects(
            p.geometry,
            s.geometry
        )
);
```

```sql
CREATE TEMP TABLE xref_expanded AS (
    SELECT
        p_id,
        s_id,
        year,
        (TO_DATE(start, 'YYYYDDD') + i)::date 
            AS day,
        density
    FROM xref,
    generate_series(
        0,
        (TO_DATE("end", 'YYYYDDD') - TO_DATE(start, 'YYYYDDD'))::int
    ) AS i
);
```

```sql
CREATE TEMP TABLE xref_ranked AS (
    SELECT
        p_id,
        year,
        day,
        MAX(
            CASE COALESCE(density, '')
                WHEN 'Heavy'  THEN 3
                WHEN 'Medium' THEN 2
                WHEN 'Light'  THEN 1
                ELSE 0
            END
        )
            AS density_rank
    FROM xref_expanded
    GROUP BY p_id, year, day
);
```

```sql
CREATE TEMP TABLE out AS (
    SELECT
        day
            AS date,
        COUNT(CASE WHEN density_rank = 1 THEN 1 END) 
            AS l,
        COUNT(CASE WHEN density_rank = 2 THEN 1 END) 
            AS m,
        COUNT(CASE WHEN density_rank = 3 THEN 1 END) 
            AS h,
        COUNT(CASE WHEN density_rank = 0 THEN 1 END) 
            AS na,
        COUNT(*) AS sum
    FROM xref_ranked
    GROUP BY date
    ORDER BY date
);
```

These queries can also be found in `q.sql`


4. Export `out`

```sql
COPY public.out TO '/some/unique/path/out.csv' CSV HEADER;
```

For additional info on COPY see https://www.postgresql.org/docs/current/sql-copy.html.